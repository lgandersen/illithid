-module(mindflayer_zfs).

-export([
         zfs/1,
         create/1,
         destroy/1,
         destroy_force/1,
         snapshot/1,
         clone/2
        ]).


create(Dataset) ->
    % zfs create [-pu] [-o property=value]... filesystem
    zfs("create " ++ Dataset).


destroy_force(Dataset) ->
    zfs("destroy -f " ++ Dataset).

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
-include_lib("test.hrl").
create_clone_test() ->
    "" = snapshot(?TEST_MF_ZFS_BASEJAIL_SNAPSHOT),
    "" = clone(?TEST_MF_ZFS_BASEJAIL_SNAPSHOT, ?TEST_MF_ZFS_TESTJAIL),
    "" = destroy(?TEST_MF_ZFS_TESTJAIL),
    "" = destroy(?TEST_MF_ZFS_BASEJAIL_SNAPSHOT).
-endif.
