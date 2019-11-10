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
    zfs(["create", Dataset]).


destroy_force(Dataset) ->
    zfs(["destroy", "-f", Dataset]).

destroy(Dataset) ->
    % zfs destroy [-dnpRrv] snapshot[%snapname][,...]
    % zfs destroy [-fnpRrv] filesystem|volume
    zfs(["destroy", Dataset]).


snapshot(Name) ->
    % zfs snapshot|snap [-r] [-o property=value]
    zfs(["snapshot", Name]).


clone(Snapshot, CloneName) ->
    % zfs clone [-p] [-o property=value]... snapshot filesystem|volume
    zfs(["clone", Snapshot, CloneName]).


zfs(Args) ->
    Cmd = string:join(["zfs" | Args], " "),
    io:format(user, "DEBUG CMD:~s~n", [Cmd]),
    Port = open_port({spawn_executable, "/sbin/zfs"}, [exit_status, {line, 1024}, {args, Args}]),
    receive
        {Port, {exit_status, N}} ->
            N
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("test.hrl").
create_clone_test() ->
    0 = snapshot(?TEST_MF_ZFS_BASEJAIL_SNAPSHOT),
    0 = clone(?TEST_MF_ZFS_BASEJAIL_SNAPSHOT, ?TEST_MF_ZFS_TESTJAIL),
    0 = destroy(?TEST_MF_ZFS_TESTJAIL),
    0 = destroy(?TEST_MF_ZFS_BASEJAIL_SNAPSHOT).
-endif.
