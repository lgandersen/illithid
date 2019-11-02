-module(mindflayer_zfs).

-export([
         zfs/1,
         create/1,
         destroy/1,
         snapshot/1,
         clone/2
        ]).

%% zfs
create(Dataset) ->
    % zfs create [-pu] [-o property=value]... filesystem
    zfs("create " ++ Dataset).


destroy(Dataset) ->
    % zfs destroy [-dnpRrv] snapshot[%snapname][,...]
    % zfs destroy [-fnpRrv] filesystem|volume
    zfs("destroy " ++ Dataset).


snapshot(Name) ->
    % zfs snapshot|snap [-r] [-o property=value]
    zfs("snapshot " ++ Name).


clone(Snapshot, CloneName) ->
    % zfs clone [-p] [-o property=value]... snapshot filesystem|volume
    zfs("clone " ++ Snapshot ++ " " ++ CloneName).


zfs(Cmd) ->
    mindflayer_utils:exec("/sbin/zfs " ++ Cmd).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(BASEJAIL_SNAPSHOT, "zroot/mindflayer_base@base_unittest").
-define(TESTJAIL, "zroot/mindflayer_dev/unittestjail").
create_clone_test() ->
    "" = snapshot(?BASEJAIL_SNAPSHOT),
    "" = clone(?BASEJAIL_SNAPSHOT, ?TESTJAIL),
    "" = destroy(?TESTJAIL),
    "" = destroy(?BASEJAIL_SNAPSHOT).
-endif.
