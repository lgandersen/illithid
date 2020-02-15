-module(illithid_cli).

-export([main/1]).
-ifdef(TEST).
-export([main_/1]).
-endif.


-include_lib("include/illithid.hrl").

main(Cmd) ->
    register(cli_process, self()),
    {ok, Pid} = illithid_cli_engine_client:start_link(),
    unlink(Pid),
    main_(Cmd),
    relay_messages().

main_(["images"]) ->
    illithid_cli_engine_client:command(list_images),
    ok;

main_(["clear", "all"]) ->
    illithid_cli_engine_client:command(clear_zroot),
    ok;

main_(["build", Path]) ->
    illithid_cli_engine_client:command({build, Path ++ "/Dockerfile", {none, none}}),
    ok;

main_(["build", "-t", NameTagRaw, Path]) ->
    %%TODO NameTag should be probably be sanity-checked using a regex
    {_Name, _Tag} = NameTag = parse_nametag(NameTagRaw),
    illithid_cli_engine_client:command({build, Path ++ "/Dockerfile", NameTag}),
    ok;

main_(["run", Image]) ->
    illithid_cli_engine_client:command({run, Image, none}),
    ok;

main_(["run", Image | Command]) ->
    illithid_cli_engine_client:command({run, Image, Command}),
    ok;


main_(Args) ->
    io:format("Unkown command: ~p~n", [Args]).


parse_nametag(NameTag) ->
    case string:split(NameTag, ":") of
        [Name,Tag] ->
            {Name, Tag};

        [_, _ | _] ->
            io:format("Unable to parse '-t <name>:<tag>' input.");

        Name ->
            {Name, "latest"}

    end.


relay_messages() ->
    receive
        done ->
            ok;

        Msg ->
            io:format(Msg),
            relay_messages()
    end.
