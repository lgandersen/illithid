-module(mindflayer).

-export([main/1]).

-include_lib("mindflayer.hrl").

main(["clean"]) ->
    %io:format("Args: ~p~n", ["lol"]),
    mindflayer_zfs:destroy_force(?ZROOT),
    mindflayer_zfs:create(?ZROOT),
    ok.
