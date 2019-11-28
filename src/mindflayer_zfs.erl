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
    zfs(["destroy", "-rf", Dataset]).


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
-include_lib("mindflayer.hrl").

create_clone_test() ->
    ZRootTest =?ZROOT ++ "/test",
    0 = create(ZRootTest),
    0 = clone(?BASEJAIL_IMAGE ++ "@image", ZRootTest ++ "/zfs_test"),
    0 = snapshot(ZRootTest ++ "/zfs_test@lol"),
    0 = destroy(ZRootTest ++ "/zfs_test@lol"),
    0 = destroy(ZRootTest ++ "/zfs_test"),
    0 = destroy(ZRootTest).
-endif.
