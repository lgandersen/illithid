-module(illithid_engine_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("include/illithid.hrl").

-export([all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_suite/1,
         end_per_suite/1]).

-export([
         create_jail/1,
         create_jail_and_wait_on_finish/1,
         destroy_jail/1,
         create_layer_with_run_instruction/1,
         create_layer_with_copy_instruction/1,
         test_image_builder/1
        ]).


all() -> [
          create_jail,
          create_jail_and_wait_on_finish,
          %%destroy_jail,
          create_layer_with_run_instruction,
          create_layer_with_copy_instruction,
          test_image_builder
         ].


init_per_suite(Config) ->
    lager:start(),
    os:cmd("../../../default/bin/illithid clean"),
    Config.


end_per_suite(_Config) ->
    os:cmd("../../../default/bin/illithid clean"),
    ok.


init_per_testcase(TestCase, Config) ->
    Name = atom_to_list(TestCase),
    Dataset = ?ZROOT ++ "/" ++ Name,
    0 = illithid_engine_zfs:clone(?BASEJAIL_IMAGE_LOCATION, Dataset),

    Jail = #jail{
              path= "/" ++ Dataset, %% Atm illithid_engine_jail is relying on the fact the clone created for a jail is automatically mount into this path. Should be more generic.
              parameters=["mount.devfs", "ip4.addr=10.13.37.3", "exec.stop=\"/bin/sh /etc/rc.shutdown\""] % "mount.devfs", <-- using mount.devfs requires additional unmounting or that zfs destroy -f is used
             },
    [{jail, Jail}, {dataset, Dataset} | Config].


end_per_testcase(start_layer_builder, _Config) ->
    ok;

end_per_testcase(_TestCase, Config) ->
    Dataset = ?config(dataset, Config),
    0 = illithid_engine_zfs:destroy(Dataset).


create_jail(Config) ->
    Jail = ?config(jail, Config),
    {ok, _Pid} = illithid_engine_jail:start_jail([Jail#jail { command = "/bin/ls", command_args=["/"] }]),
    timer:sleep(1000), % start_jail is async so we'll have to wait for the gen_server to finish.
    illithid_engine_jail:umount_devfs(Jail#jail.path),
    ok.


create_jail_and_wait_on_finish(Config) ->
    Jail = ?config(jail, Config),
    {ok, {exit_status, _N}} = illithid_engine_jail:start_and_finish_jail([Jail#jail { command = "/bin/ls", command_args=["/"] }]),
    illithid_engine_jail:umount_devfs(Jail#jail.path),
    ok.


destroy_jail(Config) ->
    Jail = ?config(jail, Config),
    {ok, _Pid} = illithid_engine_jail:start_jail([Jail#jail { command = "/bin/sh", command_args=["etc/rc"] }]),
    timer:sleep(1000),
    illithid_engine_jail:destroy(Jail),
    timer:sleep(15000), % It takes some time to close down a fullblown and recently created jail!
    illithid_engine_jail:umount_devfs(Jail#jail.path),
    ok.


create_layer_with_run_instruction(_Config) ->
    illithid_engine_layer:start_link(),
    Context = "./",
    ParentlayerId = base,
    Instruction = {run, "/bin/sh", ["-c", "echo 'lol' > /root/test.txt"]},

    {ok, #layer{ location = LayerLocation }} = illithid_engine_layer:create_layer(Context, Instruction, ParentlayerId),

    [Path | _Rest] = string:split(LayerLocation, "@"),
    {ok, <<"lol\n">>} = file:read_file("/" ++ Path ++ "/root/test.txt"),
    illithid_engine_zfs:destroy(LayerLocation),
    ok.


create_layer_with_copy_instruction(_Config) ->
    illithid_engine_layer:start_link(),
    Context = create_test_context("test_context"),
    ParentlayerId = base,
    Instruction = {copy, ["test.txt", "/root/"]},

    {ok, #layer{ location = LayerLocation }} = illithid_engine_layer:create_layer(Context, Instruction, ParentlayerId),

    [Path | _Rest] = string:split(LayerLocation, "@"),
    {ok, <<"lol\n">>} = file:read_file("/" ++ Path ++ "/root/test.txt"),
    illithid_engine_zfs:destroy(LayerLocation),
    ok.


test_image_builder(_Config) ->
    illithid_engine_layer:start_link(),
    illithid_engine_image:start_link(),
    Context = create_test_context("test_image_builder_copy"),
    Instructions = [
                    {from, base},
                    {copy, ["test.txt", "/root/"]},                             % Layer1
                    {run, "/bin/sh", ["-c", "echo 'lol1' > /root/test_1.txt"]}, % Layer2
                    {run, "/bin/sh", ["-c", "echo 'lol2' > /root/test_2.txt"]}  % Layer3
                    ],
    {ok, #image { layers = [
                            #layer { location = LayerLocation3 },
                            #layer { location = LayerLocation2 },
                            #layer { location = LayerLocation1 }
                           ]
                } } = illithid_engine_image:create_image(Instructions, Context),

    [Path1 | _Rest] = string:split(LayerLocation1, "@"),
    {ok, <<"lol\n">>} = file:read_file("/" ++ Path1 ++ "/root/test.txt"),
    {error, enoent} = file:read_file("/" ++ Path1 ++ "/root/test_1.txt"),
    {error, enoent} = file:read_file("/" ++ Path1 ++ "/root/test_2.txt"),

    [Path2 | _Rest] = string:split(LayerLocation2, "@"),
    {ok, <<"lol\n">>} = file:read_file("/" ++ Path2 ++ "/root/test.txt"),
    {ok, <<"lol1\n">>} = file:read_file("/" ++ Path2 ++ "/root/test_1.txt"),
    {error, enoent} = file:read_file("/" ++ Path2 ++ "/root/test_2.txt"),

    [Path3 | _Rest] = string:split(LayerLocation3, "@"),
    {ok, <<"lol\n">>} = file:read_file("/" ++ Path3 ++ "/root/test.txt"),
    {ok, <<"lol1\n">>} = file:read_file("/" ++ Path3 ++ "/root/test_1.txt"),
    {ok, <<"lol2\n">>} = file:read_file("/" ++ Path3 ++ "/root/test_2.txt").


create_test_context(Name) ->
    ContextDataset = ?ZROOT ++ "/" ++ Name,
    ContextRoot = "/" ++ ContextDataset,
    illithid_engine_zfs:create(ContextDataset),
    [] = os:cmd("echo 'lol' > " ++ ContextRoot ++ "/test.txt"),
    ContextRoot.
