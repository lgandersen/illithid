-module(mindflayer_zfs).

-export([
         zfs/1,
         create/1,
         destroy/1,
         destroy_force/1,
         rename/2,
         snapshot/1,
         clone/2,
         fingerprint/2
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


fingerprint(SnapshotBegin, SnapshotEnd) ->
    %% This snapshotting-mechanism uses the entire dataset which is very inefficient
    Cmd = "/bin/sh",
    CmdArgs = ["-c", "/sbin/zfs send -i " ++ SnapshotBegin ++ " " ++ SnapshotEnd ++ " | sha256"],
    Port = open_port({spawn_executable, Cmd}, [exit_status, {line, 1024}, {args, CmdArgs}]),
    Line = receive
        {Port, {data, {eol, L}}} ->
            L
    end,
    receive
        {Port, {exit_status, _N}} ->
            ok
    end,
    {ok, Line}.

rename(Dataset, NewDataset) ->
    zfs(["rename", Dataset, NewDataset]).


zfs(Args) ->
    Cmd = string:join(["zfs" | Args], " "),
    io:format(user, "DEBUG CMD:~p~n", [Cmd]),
    Port = open_port({spawn_executable, "/sbin/zfs"}, [exit_status, {line, 1024}, {args, Args}]),
    receive
        {Port, {exit_status, N}} ->
            N
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mindflayer.hrl").

create_clone_test() ->
    ZRootTest =?ZROOT ++ "/create_clone_test",
    0 = create(ZRootTest),
    0 = clone(?BASEJAIL_IMAGE_LOCATION, ZRootTest ++ "/zfs_test"),
    0 = snapshot(ZRootTest ++ "/zfs_test@lol"),
    0 = destroy(ZRootTest ++ "/zfs_test@lol"),
    0 = destroy(ZRootTest ++ "/zfs_test"),
    0 = destroy(ZRootTest).

fingerprint_test() ->
    ZRootTest =?ZROOT ++ "/create_fingerprint_test",
    0 = create(ZRootTest),
    0 = snapshot(ZRootTest ++ "@lol1"),
    0 = snapshot(ZRootTest ++ "@lol2"),
    {ok, Line} = fingerprint(ZRootTest ++ "@lol1", ZRootTest ++ "@lol2"),
    io:format(user, "What: ~p~n", [Line]),
    0 = destroy(ZRootTest ++ "@lol1"),
    0 = destroy(ZRootTest ++ "@lol2"),
    0 = destroy(ZRootTest).

rename_test() ->
    ZRootTest =?ZROOT ++ "/rename_test",
    ZRootTestNew =?ZROOT ++ "/rename_test_newname",
    0 = create(ZRootTest),
    0 = rename(ZRootTest, ZRootTestNew),
    0 = destroy(ZRootTestNew).
-endif.
