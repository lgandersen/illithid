-module(mindflayer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("mindflayer.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([create_jail/1, create_jail_and_wait_on_finish/1, destroy_jail/1, create_layer/1, start_image_builder/1]).


all() -> [
          create_jail,
          create_jail_and_wait_on_finish,
          %destroy_jail,
          create_layer,
          start_image_builder
         ].


init_per_testcase(TestCase, Config) ->
    Name = atom_to_list(TestCase),
    Dataset = ?ZROOT ++ "/" ++ Name,
    0 = mindflayer_zfs:clone(?BASEJAIL_IMAGE_LOCATION, Dataset),

    Jail = #jail{
              path= "/" ++ Dataset, %% Atm mindflayer_jail is relying on the fact the clone created for a jail is automatically mount into this path. Should be more generic.
              parameters=["mount.devfs", "ip4.addr=10.13.37.3", "exec.stop=\"/bin/sh /etc/rc.shutdown\""] % "mount.devfs", <-- using mount.devfs requires additional unmounting or that zfs destroy -f is used
             },
    [{jail, Jail}, {dataset, Dataset} | Config].


end_per_testcase(start_layer_builder, _Config) ->
    ok;

end_per_testcase(_TestCase, Config) ->
    Dataset = ?config(dataset, Config),
    0 = mindflayer_zfs:destroy(Dataset).


create_jail(Config) ->
    Jail = ?config(jail, Config),
    {ok, _Pid} = mindflayer_jail:start_jail([Jail#jail { command = "/bin/ls", command_args=["/"] }]),
    timer:sleep(1000), % start_jail is async so we'll have to wait for the gen_server to finish.
    mindflayer_jail:umount_devfs(Jail#jail.path),
    ok.


create_jail_and_wait_on_finish(Config) ->
    Jail = ?config(jail, Config),
    {ok, {exit_status, _N}} = mindflayer_jail:start_and_finish_jail([Jail#jail { command = "/bin/ls", command_args=["/"] }]),
    mindflayer_jail:umount_devfs(Jail#jail.path),
    ok.


destroy_jail(Config) ->
    Jail = ?config(jail, Config),
    {ok, _Pid} = mindflayer_jail:start_jail([Jail#jail { command = "/bin/sh", command_args=["etc/rc"] }]),
    timer:sleep(1000),
    mindflayer_jail:destroy(Jail),
    timer:sleep(15000), % It takes some time to close down a fullblown and recently created jail!
    mindflayer_jail:umount_devfs(Jail#jail.path),
    ok.


create_layer(_Config) ->
    mindflayer_layers:start_link(),
    Cmd = "/bin/sh",
    CmdArgs = ["-c", "echo 'lol' > /root/test.txt"],
    ParentlayerId = base,
    {ok, #layer{ location = LayerLocation }} = mindflayer_layers:create_layer(Cmd, CmdArgs, ParentlayerId),
    [Path | _Rest] = string:split(LayerLocation, "@"),
    {ok, <<"lol\n">>} = file:read_file("/" ++ Path ++ "/root/test.txt"),
    mindflayer_zfs:destroy(LayerLocation),
    ok.

start_image_builder(_Config) ->
    mindflayer_layers:start_link(),
    Instructions = [
                    {from, base},
                    {run, "/bin/sh", ["-c", "echo 'lol1' > /root/test_1.txt"]},
                    {run, "/bin/sh", ["-c", "echo 'lol2' > /root/test_2.txt"]}
                    ],
    {ok, [#layer { location = LayerLocation2 }, #layer { location = LayerLocation1 }]} = mindflayer_image_builder:create_image(Instructions),

    [Path1 | _Rest] = string:split(LayerLocation1, "@"),
    {ok, <<"lol1\n">>} = file:read_file("/" ++ Path1 ++ "/root/test_1.txt"),
    {error, enoent} = file:read_file("/" ++ Path1 ++ "/root/test_2.txt"),

    [Path2 | _Rest] = string:split(LayerLocation2, "@"),
    {ok, <<"lol2\n">>} = file:read_file("/" ++ Path2 ++ "/root/test_2.txt").
