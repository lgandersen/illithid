%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%% This gen_server manages jails: Creates, destroys and monitor jails.
%%% @end
%%%-------------------------------------------------------------------
-module(mindflayer_jail).

-behaviour(gen_server).

-include_lib("mindflayer.hrl").

%% API
-export([start_link/1,
        create/1,
        destroy/1,
        umount_devfs/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          jail_table = none,
          next_jid = 1,
          basejail_snapshot = none
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(BasejailSnapshot) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [BasejailSnapshot], []).

create(Jail) ->
    gen_server:cast(?SERVER, {create, Jail}).

destroy(Jail) ->
    gen_server:cast(?SERVER, {destroy, Jail}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([BasejailSnapshot]) ->
    JailTable = ets:new(jail_table, [protected, {keypos, 1}]),
    {ok, #state{ jail_table = JailTable, basejail_snapshot = BasejailSnapshot }}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({create, #jail { zfs_dataset = Dataset} = Jail}, #state{ jail_table = JailTable, basejail_snapshot = BasejailSnapshot } = State) ->
    0 = mindflayer_zfs:clone(BasejailSnapshot, Dataset),
    Port = create_(Jail),
    true = ets:insert(JailTable, Jail#jail { port = Port }),
    {noreply, State};


handle_cast({destroy, #jail { name = Name } = Jail}, #state{ jail_table = Table } = State) ->
    destroy_(Jail),
    true = ets:delete(Table, Name),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({Port, {exit_status, N}}, #state { jail_table = Table } = State) ->
    true = ets:match_delete(Table, #jail{port=Port}),
    io:format(user, "Port ~p exited with status ~p~n", [Port, N]),
    {noreply, State};

handle_info(Info, State) ->
    io:format(user, "Unknow message received to jail manager: ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
        {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
create_(#jail{name=Name, path=Path, command=Cmd, command_args=CmdArgs, parameters=Parameters}) ->
% $ jail -c path=/data/jail/testjail mount.devfs host.hostname=testhostname ip4.addr=192.0.2.100 command=/bin/sh
    %mindflayer_utils:exec(io_lib:format("jail -c path=~p name=~p mount.devfs ip4.addr=~p command=~p", [Path, Name, IP, Cmd]) ++ Args).
    Executable = "/usr/sbin/jail",
    Args = ["-c",
            "path=" ++ Path,
            "name=" ++ Name
           ] ++ Parameters ++ [
            "command=" ++ Cmd
           ] ++ CmdArgs,
    Port = open_port({spawn_executable, Executable},
                     [exit_status,
                      {line, 1024},
                      {args, Args}
                      ]),
    DebugCmd = string:join([Executable | Args], " "),
    io:format(user, "DEBUG CMD:~s~n", [DebugCmd]),
    Port.


destroy_(#jail{name=Name, path=Path}) ->
    mindflayer_utils:exec(io_lib:format("jail -r ~s", [Name])),
    umount_devfs(Path).


umount_devfs(JailPath) ->
    %% Try using devfs instead
    case mindflayer_utils:exec("/sbin/umount " ++ JailPath ++ "/dev") of
        "" ->
            umount_devfs(JailPath);
        OutPut ->
            OutPut
    end.
