-module(mindflayer).

-export([main/1]).

-include_lib("mindflayer.hrl").

main(["clean"]) ->
    mindflayer_zfs:destroy_force(?ZROOT),
    mindflayer_zfs:create(?ZROOT),
    ok.
