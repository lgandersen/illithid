-module(mindflayer).

-export([main/1]).

-include_lib("mindflayer.hrl").

main(["clean"]) ->
    mindflayer_zfs:destroy_force(?ZROOT),
    mindflayer_zfs:create(?ZROOT),
    ok;

main(["build", Path]) ->
    mindflayer_layers:start_link(),
    Instructions = mindflayer_dockerfile:parse(Path ++ "/Dockerfile"),
    {ok, _Layers} = mindflayer_image_builder:create_image(Instructions, Path),
    ok;

main(Args) ->
    io:format("unkown command: ~p~n", [Args]).
