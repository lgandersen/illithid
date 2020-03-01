-module(illithid_cli_eunit).

-include_lib("include/illithid.hrl").
-include_lib("printing.hrl").
-include_lib("eunit/include/eunit.hrl").

initialize() ->
    application:set_env([{lager, [{colored, true}]}], [{persistent, true}]),
    lager:start(),
    lager:set_loglevel(lager_console_backend, ?TEST_LOG_LEVEL),
    illithid_engine_zfs:clear_zroot(),
    %ok = application:start(illithid_engine),
    {ok, SupPid} = illithid_engine_sup:start_link(),
    unlink(SupPid),
    illithid_engine_metadata:clear_all(),
    {ok, Pid} = illithid_cli_engine_client:start_link(),
    unlink(Pid),
    register(cli_process, self()),
    {SupPid, Pid}.

stop({SupPid, Pid}) ->
    gen_server:stop(Pid),
    %ok = application:stop(illithid_engine),
    exit(SupPid, shutdown),
    unregister(cli_process),
    ok.

instructions_test_() ->
     {foreach, fun initialize/0, fun stop/1, [
                                  fun test_clear_zroot/1,
                                  fun test_build_image/1,
                                  fun test_build_image_with_tag/1,
                                  fun test_list_images/1,
                                  fun test_run_image/1,
                                  fun test_run_image_with_custom_command/1
                                  ]
     }.


test_clear_zroot(_) ->
    0 = illithid_engine_zfs:create(?ZROOT("test_cli_clear_all")),
    illithid_cli:main_(["clear", "zroot"]),
    [ZRootCleared] = receive_messages("test-clear-zroot"),
    RCode = illithid_engine_zfs:create(?ZROOT("test_cli_clear_all")),
    [?_assertEqual(
        ?ZROOT_CLEARED, ZRootCleared),
     ?_assertEqual(
        0, RCode)
     ].

-define(_assertTextEqual(Expected, Got),
        ?_assertEqual(Expected, unicode:characters_to_list(Got))).

test_list_images(_) ->
    Image1 = #image {
               id      = "lolololololooooooooooooooool",
               name    = "test",
               tag     = "latest",
               created = {1578, 330264, 608742}
              },
    Image2 = #image {
               id      = "leleleleleleeeeee",
               name    = "test",
               tag     = "oldest",
               created = {1578, 330200, 0}
              },
    illithid_engine_metadata:add_image(Image1),
    illithid_engine_metadata:add_image(Image2),
    illithid_cli:main_(["images"]),
    [Header, ImageStr_1, ImageStr_2] = receive_messages("test-list-images"),

    [
     ?_assertTextEqual(
        ?LIST_IMAGES_HEADER,
        Header),
     ?_assertTextEqual(
        "test          latest      lolololololo  2020-01-06 17:04:24  n/a MB\n",
        ImageStr_1),
     ?_assertTextEqual(
        "test          oldest      lelelelelele  2020-01-06 17:03:20  n/a MB\n",
        ImageStr_2)
    ].


test_build_image(_) ->
    [Id] = build_image("build-image", []),
    FileContent = file:read_file("/" ++ ?ZROOT ++ "/" ++ Id  ++ "/root/test.txt"),
    Images = illithid_engine_metadata:list_images(),
    [?_assertEqual(
       {ok, <<"lol\n">>},
       FileContent),
     ?_assertMatch(
       [#image { id = Id, name = none, tag = none }],
       Images)
    ].


test_build_image_with_tag(_) ->
    [Id] = build_image("build-image", ["-t", "testcliengine:latest"]),
    FileContent = file:read_file("/" ++ ?ZROOT ++ "/" ++ Id  ++ "/root/test.txt"),
    Images = illithid_engine_metadata:list_images(),
    [?_assertEqual(
       {ok, <<"lol\n">>},
       FileContent),
     ?_assertMatch(
       [#image { id = Id, name = "testcliengine", tag = "latest" }],
       Images)
    ].


test_run_image(_) ->
    [Id] = build_image("run-image", []),
    illithid_cli:main_(["run", Id]),
    [Msg0, Msg1, Msg2, Msg3] = receive_messages("run-image"),
    [?_assertTextEqual(
        "moduli\n", Msg0),
     ?_assertTextEqual(
        "ssh_config\n", Msg1),
     ?_assertTextEqual(
        "sshd_config\n", Msg2),
     ?_assertTextEqual(
        "Container exited with status 0\n", Msg3)
    ].


test_run_image_with_custom_command(_) ->
    [Id] = build_image("run-image-with-custom", []),
    illithid_cli:main_(["run", Id, "cat", "/root/test.txt"]),
    [Msg0, Msg1] = receive_messages("run-image-with-custom"),
    [?_assertTextEqual(
        "lol\n", Msg0),
     ?_assertTextEqual(
        "Container exited with status 0\n", Msg1)
    ].

%%%===================================================================
%%% Internal utilities
%%%===================================================================

build_image(TestName, BuildArgs) ->
    {ok, Root} = file:get_cwd(),
    Path = Root ++ "/apps/illithid_cli/test/data",
    illithid_cli:main_(["build"] ++ BuildArgs ++ [Path]),
    [_Msg1, Msg2] = receive_messages(TestName),
    [_, Id] = string:split(Msg2, "Image id: "),
    lists:droplast(Id).


receive_messages(Name) ->
    receive_messages_([], 0, Name).

receive_messages_(Messages, Counter, Name) ->
    receive
        done ->
            ?LOG("done!~n", []),
            lists:reverse(Messages);

        {cli_msg, Message} ->
            ?LOG("~p climsg ~p:~s", [Name, Counter, unicode:characters_to_list(Message)]),
            receive_messages_([Message | Messages], Counter + 1, Name);

        UnknownMsg ->
            ?LOG("~p Unknown message! ~p:~p~n", [Name, Counter, UnknownMsg]),
            receive_messages_(Messages, Counter + 1, Name)
    end.
