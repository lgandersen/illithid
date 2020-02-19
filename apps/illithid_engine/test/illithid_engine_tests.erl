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
                          "ip4.addr=10.13.37.3",
                          "exec.stop=\"/bin/sh /etc/rc.shutdown\""
                         ]}
           ],
    ok = illithid_engine_zfs:clear_zroot(),
    {ok, LayerPid} = illithid_engine_layer:start_link(),
    [Opts, LayerPid].


container_stop([_, LayerPid]) ->
    gen_server:stop(LayerPid),
    ok.


container_test_() ->
     {foreach, fun container_start/0, fun container_stop/1, [
                                        fun create_container_async/1,
                                        fun create_container_as_nonroot/1,
                                        fun create_container_sync/1,
                                        fun container_shutdown_sync/1
                                        ]
     }.


create_container_async([Opts, _]) ->
    {ok, Pid} = illithid_engine_container:start_link([{cmd, ["/bin/ls", "/"]} | Opts]),
    ?_assertEqual(ok, illithid_engine_container:run(Pid)).


create_container_as_nonroot([Opts, _]) ->
    {ok, Pid} = illithid_engine_container:start_link([{cmd, ["/usr/bin/id"]}, {user, "ntpd"} | Opts]),
    illithid_engine_container:attach(Pid),
    ok = illithid_engine_container:run(Pid),
    receive
        Msg ->
            ?_assertEqual({container_msg, {Pid, {data, {eol, "uid=123(ntpd) gid=123(ntpd) groups=123(ntpd)"}}}}, Msg)
    end.


create_container_sync([Opts, _]) ->
    {ok, Pid} = illithid_engine_container:start_link([{cmd, ["/bin/ls", "/"]} | Opts]),
    ?_assertEqual({ok, {exit_status, 0}}, illithid_engine_container:run_sync(Pid)).


container_shutdown_sync([Opts, _]) ->
    {ok, Pid} = illithid_engine_container:start_link([{cmd, ["/bin/sh", "/etc/rc"]} | Opts]),
    [?_assertEqual({ok, {exit_status, 0}}, illithid_engine_container:run_sync(Pid)),
    {timeout, 20, ?_assertEqual({ok, {exit_status, 0}}, illithid_engine_container:stop_sync(Pid))}].


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
