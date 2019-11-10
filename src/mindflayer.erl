-module(mindflayer).

-export([main/1]).

-define(TEST_MF_JAIL_DATASET, "zroot/mindflayer_dev/unittest_jail_jail").

main(["clean"]) ->
    %io:format("Args: ~p~n", ["lol"]),
    mindflayer_zfs:destroy_force(?TEST_MF_JAIL_DATASET),
    ok.
