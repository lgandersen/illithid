-module(illithid_engine_tests).

-include_lib("include/illithid.hrl").
-include_lib("eunit/include/eunit.hrl").

container_start() ->
    application:set_env([{lager, [{colored, true}]}], [{persistent, true}]),
    lager:start(),
    lager:set_loglevel(lager_console_backend, ?TEST_LOG_LEVEL),
    Opts = [
            {image, ?BASE_IMAGE#image { created = erlang:timestamp()}},
            {jail_param, ["mount.devfs",
                          "exec.stop=\"/bin/sh /etc/rc.shutdown\""
                         ]}
           ],
    ok = illithid_engine_zfs:clear_zroot(),
    {ok, _} = illithid_engine_layer:start_link(),
    {ok, _} = illithid_engine_network:start_link(),
    Opts.


container_stop(_) ->
    gen_server:stop(illithid_engine_layer),
    gen_server:stop(illithid_engine_network),
    ok.


container_test_() ->
     {foreach, fun container_start/0, fun container_stop/1, [
                                        fun create_container_async/1,
                                        fun create_container_as_nonroot/1,
                                        fun create_container_await/1,
                                        fun container_shutdown_await/1
                                        ]
     }.


create_container_async(Opts) ->
    {ok, Pid} = illithid_engine_container:create([{cmd, ["/bin/ls", "/"]} | Opts]),
    ?_assertEqual(ok, illithid_engine_container:start(Pid)).


create_container_as_nonroot(Opts) ->
    {ok, Pid} = illithid_engine_container:create([{cmd, ["/usr/bin/id"]}, {user, "ntpd"} | Opts]),
    illithid_engine_container:attach(Pid),
    ok = illithid_engine_container:start(Pid),
    receive
        Msg ->
            ?_assertEqual({container_msg, Pid, {data, {eol, "uid=123(ntpd) gid=123(ntpd) groups=123(ntpd)"}}}, Msg)
    end.


create_container_await(Opts) ->
    {ok, Pid} = illithid_engine_container:create([{cmd, ["/bin/ls", "/"]} | Opts]),
    ?_assertEqual({ok, {exit_status, 0}}, illithid_engine_container:start_await(Pid)).


container_shutdown_await(Opts) ->
    {ok, Pid} = illithid_engine_container:create([{cmd, ["/bin/sh", "/etc/rc"]} | Opts]),
    unlink(Pid),
    [?_assertEqual({ok, {exit_status, 0}}, illithid_engine_container:start_await(Pid)),
    {timeout, 20, ?_assertEqual({ok, {exit_status, 0}}, illithid_engine_container:stop_await(Pid))}].


image_building_start() ->
    lager:start(),
    ok = illithid_engine_zfs:clear_zroot(),
    {ok, SupervisorPid} = illithid_engine_sup:start_link(),
    [SupervisorPid].


image_building_stop([SupervisorPid]) ->
    unlink(SupervisorPid),
    exit(SupervisorPid, shutdown),
    ok.


image_building_test_() ->
     {foreach, fun image_building_start/0, fun image_building_stop/1, [
                                        fun create_image_with_copy/1,
                                        fun create_image_with_run/1,
                                        fun create_image_three_layers/1
                                        ]
     }.


create_image_with_copy([_]) ->
    Context = create_test_context("test_image_builder_copy"),
    Instructions = [{from, base},
                    {copy, ["test.txt", "/root/"]}
                    ],
    {ok, #image {
            layers = [ #layer { path = Path }, ?BASE_LAYER ] } } = illithid_engine_image:create_image(Instructions, Context),
    ?_assertEqual({ok, <<"lol\n">>}, file:read_file(Path ++ "/root/test.txt")).


create_image_with_run([_]) ->
    Instructions = [{from, base},
                    {run, ["/bin/sh", "-c", "echo 'lol1' > /root/test_1.txt"]}
                    ],
    {ok, #image {layers = [#layer { path = Path },
                           ?BASE_LAYER ] } } = illithid_engine_image:create_image(Instructions),
    ?_assertEqual({ok, <<"lol1\n">>}, file:read_file(Path ++ "/root/test_1.txt")).


create_image_three_layers([_]) ->
    Context = create_test_context("test_image_builder_three_layers"),
    Instructions = [{from, base},
                    {copy, ["test.txt", "/root/"]},                             % Layer1
                    {run, ["/bin/sh", "-c", "echo 'lol1' > /root/test_1.txt"]}, % Layer2
                    {run, ["/bin/sh", "-c", "echo 'lol2' > /root/test_2.txt"]}  % Layer3
                    ],
    {ok, #image { layers = [#layer { path = Path3 },
                            #layer { path = Path2 },
                            #layer { path = Path1 },
                            ?BASE_LAYER
                           ]
                }
    } = illithid_engine_image:create_image(Instructions, Context),

    [?_assertEqual({ok, <<"lol\n">>}, file:read_file("/" ++ Path1 ++ "/root/test.txt")),
    ?_assertEqual({error, enoent}, file:read_file("/" ++ Path1 ++ "/root/test_1.txt")),
    ?_assertEqual({error, enoent}, file:read_file("/" ++ Path1 ++ "/root/test_2.txt")),

    ?_assertEqual({ok, <<"lol\n">>}, file:read_file("/" ++ Path2 ++ "/root/test.txt")),
    ?_assertEqual({ok, <<"lol1\n">>}, file:read_file("/" ++ Path2 ++ "/root/test_1.txt")),
    ?_assertEqual({error, enoent}, file:read_file("/" ++ Path2 ++ "/root/test_2.txt")),

    ?_assertEqual({ok, <<"lol\n">>}, file:read_file("/" ++ Path3 ++ "/root/test.txt")),
    ?_assertEqual({ok, <<"lol1\n">>}, file:read_file("/" ++ Path3 ++ "/root/test_1.txt")),
    ?_assertEqual({ok, <<"lol2\n">>}, file:read_file("/" ++ Path3 ++ "/root/test_2.txt"))
    ].


create_test_context(Name) ->
    ContextDataset = ?ZROOT ++ "/" ++ Name,
    ContextRoot = "/" ++ ContextDataset,
    illithid_engine_zfs:create(ContextDataset),
    [] = os:cmd("echo 'lol' > " ++ ContextRoot ++ "/test.txt"),
    ContextRoot.
