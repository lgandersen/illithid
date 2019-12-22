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
-export([start_jail/1,
        start_and_finish_jail/1,
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
          caller = none,
          port =  none,
          jail_config = none
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_jail([Jail]) ->
    gen_server:start_link(?MODULE, [Jail], []).

start_and_finish_jail([#jail {path = Path } = Jail]) ->
    gen_server:start_link(?MODULE, [Jail, self()], []),
    receive
        {ok, {exit_status, N}} ->
            umount_devfs(Path),
            {ok, {exit_status, N}}
    end.

destroy(Jail) ->
    gen_server:cast(?SERVER, {destroy, Jail}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Jail, CallerPid]) ->
    Port = create(Jail),
    {ok, #state{ jail_config = Jail, port=Port, caller = CallerPid }};

init([Jail]) ->
    %JailTable = ets:new(jail_table, [protected, {keypos, 1}]),
    Port = create(Jail),
    {ok, #state{ jail_config = Jail, port=Port }}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({destroy, Jail}, State) ->
    destroy_(Jail),
    %true = ets:delete(Table, Name),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({Port, {exit_status, N}}, #state { caller = none } = State) ->
    %true = ets:match_delete(Table, #jail{ port = Port }),
    io:format(user, "Port ~p exited with status ~p~n", [Port, N]),
    {noreply, State};

handle_info({Port, {exit_status, N}}, #state { caller = Caller } = State) ->
    Caller ! {ok, {exit_status, N}},
    handle_info({Port, {exit_status, N}}, State#state { caller = none });

handle_info({Port, {data, {eol, Line}}}, State) ->
    io:format(user, "~p: ~p~n", [Port, Line]),
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
create(Jail) ->
    create_(Jail).


create_(#jail{path=Path, command=Cmd, command_args=CmdArgs, parameters=Parameters}) ->
% $ jail -c path=/data/jail/testjail mount.devfs host.hostname=testhostname ip4.addr=192.0.2.100 command=/bin/sh
    %mindflayer_utils:exec(io_lib:format("jail -c path=~p name=~p mount.devfs ip4.addr=~p command=~p", [Path, Name, IP, Cmd]) ++ Args).
    Name = jail_name_from_pid(),
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

destroy_(#jail{path=Path}) ->
    mindflayer_utils:exec(io_lib:format("jail -r ~s", [jail_name_from_pid()])),
    umount_devfs(Path).


jail_name_from_pid() ->
    binary:bin_to_list(
      binary:replace(
        binary:replace(
          binary:replace(
            binary:list_to_bin(pid_to_list(self())),
            <<"<">>, <<"mf_jail_">>),
          <<">">>, <<"">>),
        <<".">>, <<"_">>, [global])).


umount_devfs(JailPath) ->
    case mindflayer_utils:exec("/sbin/umount " ++ JailPath ++ "/dev") of
        "" ->
            umount_devfs(JailPath);
        OutPut ->
            OutPut
    end.
