%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%% This gen_server manages jails: Creates, destroys and monitor jails.
%%% @end
%%%-------------------------------------------------------------------
-module(illithid_engine_jail).

-behaviour(gen_server).

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

-include_lib("include/illithid.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          caller = none,
          jail_config = none,
          closing_port = none,
          starting_port = none
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_jail([Jail]) ->
    gen_server:start_link(?MODULE, [Jail], []).

start_and_finish_jail([Jail]) ->
    gen_server:start_link(?MODULE, [Jail, self()], []),
    receive
        {ok, {exit_status, N}} ->
            umount_devfs(Jail#jail.path),
            {ok, {exit_status, N}}
    end.

destroy(Jail) ->
    gen_server:cast(?SERVER, {destroy, Jail}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Jail, CallerPid]) ->
    Port = create(Jail),
    {ok, #state{ jail_config = Jail, starting_port = Port, caller = CallerPid }};

init([Jail]) ->
    Port = create(Jail),
    {ok, #state{ jail_config = Jail, starting_port = Port }}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({destroy, Jail}, State) ->
    Port= destroy_(Jail),
    {noreply, State#state {closing_port = Port }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, N}}, #state { closing_port = Port, jail_config = Jail } = State) ->
    lager:info("~p: Jail shut down with exit-code: ~p", [Port, N]),
    umount_devfs(Jail#jail.path),
    %%TODO shouldn't we just exit (normally) here?
    {noreply, State#state { closing_port = none }};

handle_info({Port, {exit_status, N}}, #state { caller = Caller, starting_port = Port } = State) ->
    lager:info("Jail starting port ~p exited with status ~p", [Port, N]),
    NewState = case Caller of
        none ->
            State;

        _Pid ->
            Caller ! {ok, {exit_status, N}},
            State#state { caller = none }
    end,
    {noreply, NewState};

handle_info({Port, {data, {eol, Line}}}, State) ->
    lager:info("~p: Last line: ~p", [Port, Line]),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("Unknow message received to jail manager: ~p", [Info]),
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
    lager:info("Creating jail:~s", [DebugCmd]),
    Port.


destroy_(_Jail) ->
    Executable = "/usr/sbin/jail",
    Name = jail_name_from_pid(),
    Args = ["-c", Name],
    Port = open_port({spawn_executable, Executable},
                     [exit_status,
                      {line, 1024},
                      {args, Args}
                      ]),
    lager:info("~p: Shutting down jail: ~p", [Port, Args]),
    Port.


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
    Args = [JailPath ++ "/dev"],
    Port = open_port({spawn_executable, "/sbin/umount"}, [exit_status, {line, 1024}, {args, Args}]),
    lager:info("~p: Executing umount-command: ~p", [Port, Args]),
    receive
        {Port, {exit_status, N}} ->
            lager:info("Umount-command finished with exit code: ~p", [N]),
            N
    end.
