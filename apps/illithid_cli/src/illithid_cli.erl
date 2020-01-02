-module(illithid_cli).

-export([main/1]).

-include_lib("include/illithid.hrl").

main(["clean"]) ->
    illithid_engine_zfs:destroy_force(?ZROOT),
    illithid_engine_zfs:create(?ZROOT),
    ok;

main(["build", Path]) ->
    illithid_engine_layers:start_link(),
    Instructions = illithid_engine_dockerfile:parse(Path ++ "/Dockerfile"),
    {ok, _Layers} = illithid_engine_image:create_image(Instructions, Path),
    ok;

main(Args) ->
    io:format("Unkown command: ~p~n", [Args]).
