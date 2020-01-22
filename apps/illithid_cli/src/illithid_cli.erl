-module(illithid_cli).

-export([main/1]).

-include_lib("include/illithid.hrl").

main(Cmd) ->
    illithid_cli_engine_client:start_link(),
    main_(Cmd).

main_(["images"]) ->
    illithid_cli_engine_client:command(list_images),
    ok;

main_(["clear", "all"]) ->
    illithid_cli_engine_client:command(clear_zroot),
    ok;

main_(["build", Path]) ->
    illithid_cli_engine_client:command({build, Path ++ "/Dockerfile"}),
    ok;

main_(["run", Image]) ->
    %% Image can be a ImageId or a tag
    illithid_cli_engine_client:command({run, Image}),
    ok;


main_(Args) ->
    io:format("Unkown command: ~p~n", [Args]).

