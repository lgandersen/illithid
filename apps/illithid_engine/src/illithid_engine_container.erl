%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%% This gen_server manages jails: Creates, runs, destroys and monitors jails.
%%% @end
%%%-------------------------------------------------------------------
-module(illithid_engine_container).

-behaviour(gen_server).

%% API
-export([create/1,
        run/1,
        run_sync/1,
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
          jail = none,
          closing_port = none,
          starting_port = none
         }).

%%%===================================================================
%%% API
%%%===================================================================
create(Jail) ->
    gen_server:start_link(?MODULE, [Jail], []).


run(Pid) ->
    gen_server:cast(Pid, run).


run_sync(Pid) ->
    gen_server:cast(Pid, {run_sync, self()}),
    receive
        {ok, {exit_status, N}} ->
            {ok, {exit_status, N}}
    end.


destroy(Pid) ->
    gen_server:cast(Pid, destroy).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Jail]) ->
    {ok, #state { jail = Jail }}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({run_sync, Pid}, #state { jail = Jail } = State) ->
    Port = run_(Jail),
    {noreply, State#state { starting_port = Port, caller = Pid }};

handle_cast(run, #state { jail = Jail } = State) ->
    Port = run_(Jail),
    {noreply, State#state { starting_port = Port }};

handle_cast(destroy, #state { jail = Jail } = State) ->
    Port = destroy_(Jail),
    {noreply, State#state { closing_port = Port }};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({Port, {exit_status, N}}, #state { closing_port = Port, jail = Jail } = State) ->
    lager:info("~p: Jail shut down with exit-code: ~p", [Port, N]),
    umount_devfs(Jail#jail.path),
    %%TODO shouldn't we just exit (normally) here?
    {noreply, State#state { closing_port = none }};

handle_info({Port, {exit_status, N}}, #state { starting_port = Port, caller = Caller, jail = Jail } = State) ->
    lager:info("Jail starting port ~p exited with status ~p", [Port, N]),
    umount_devfs(Jail#jail.path),
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
run_(#jail { path = Path, command = Cmd, command_args = CmdArgs, parameters = Parameters }) ->
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
