-module(mindflayer).

-export([main/1]).

-include_lib("test.hrl").

main(["clean"]) ->
    %io:format("Args: ~p~n", ["lol"]),
    mindflayer_zfs:destroy_force(?TEST_MF_JAIL_DATASET),
    mindflayer_zfs:destroy_force(?TEST_MF_JAIL_BASEJAIL_SNAPSHOT),

    mindflayer_zfs:destroy_force(?TEST_MF_ZFS_TESTJAIL),
    mindflayer_zfs:destroy_force(?TEST_MF_ZFS_BASEJAIL_SNAPSHOT),
    ok.
