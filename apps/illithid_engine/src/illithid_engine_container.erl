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
-export([start_link/1,
         fetch/1,
         attach/1,
         run/1,
         run_sync/1,
         stop_sync/1,
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
          relay_to = none,
          image = none,
          container = none,
          closing_port = none,
          starting_port = none
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).


fetch(Pid) ->
    gen_server:call(Pid, fetch).


attach(Pid) ->
    gen_server:call(Pid, {attach, self()}).


run(Pid) ->
    gen_server:cast(Pid, run).


run_sync(Pid) ->
    ok = attach(Pid),
    gen_server:cast(Pid, run),
    receive_exit_status(Pid).


stop_sync(Pid) ->
    ok = attach(Pid),
    gen_server:cast(Pid, {stop_sync, self()}),
    receive
        {ok, {exit_status, N}} ->
            {ok, {exit_status, N}}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Opts]) ->
    #image {user    = DefaultUser,
            command = DefaultCmd } = Image = proplists:get_value(image, Opts, ?BASE_IMAGE),
    {ok, Layer} = illithid_engine_layer:new(Image),
    Cmd = proplists:get_value(cmd, Opts, DefaultCmd),
    User = proplists:get_value(user, Opts, DefaultUser),
    RelayTo = proplists:get_value(relay_to, Opts, none),
    JailParam = proplists:get_value(jail_param, Opts, []),

    Container = #container {
                   id         = illithid_engine_util:uuid(),
                   pid        = self(),
                   command    = Cmd,
                   layer      = Layer,
                   parameters = ["exec.jail_user=" ++ User | JailParam]
                  },
    illithid_engine_metadata:add_container(Container),
    {ok, #state { container = Container, relay_to = RelayTo, image = Image }}.


handle_call({attach, Pid}, _From, State) ->
    {reply, ok, State#state { relay_to = Pid }};

handle_call(fetch, _From, #state { container = Container } = State) ->
    {reply, Container, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(run, #state { container = Container} = State) ->
    Port = run_(Container),
    {noreply, State#state { starting_port = Port}};

handle_cast({stop_sync, Pid}, State) ->
    Port = destroy_(),
    {noreply, State#state { closing_port = Port, caller = Pid }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, N}}, #state { closing_port = Port, caller = Caller,  container = Container } = State) ->
    lager:info("container-~s: jail closed with exit-code: ~p", [Container#container.id, N]),
    #container { layer = #layer {path = Path}} = Container,
    umount_devfs(Path),
    NewState = case Caller of
        none ->
            State;

        _Pid ->
            Caller ! {ok, {exit_status, N}},
            State#state { caller = none }
    end,
    %%TODO shouldn't we just exit (normally) here?
    {noreply, NewState#state { closing_port = none }};

handle_info({Port, Msg}, State = #state { starting_port = Port, relay_to = RelayTo, container = Container }) ->
    lager:debug("container#~s message received: ~p", [Container#container.id, Msg]),
    case RelayTo of
        none ->
            ok;

        Pid when is_pid(Pid) ->
            RelayTo ! {container_msg, {self(), Msg}}
    end,
    {noreply, State};

handle_info(Info, #state { container = #container { id = Id }} = State) ->
    lager:warning("container#~s message not understood: ~p", [Id, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
        {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
receive_exit_status(Pid) ->
    receive
        {container_msg, {Pid, {exit_status, N}}} ->
            {ok, {exit_status, N}};

        {container_msg, {Pid, _Msg}} ->
            receive_exit_status(Pid)
    end.


run_(#container {
        id         = Id,
        layer      = #layer { path = Path },
        command    = [Cmd | CmdArgs],
        parameters = Parameters }) ->
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
    lager:info("container#~s: ~s", [Id, DebugCmd]),
    Port.


destroy_() ->
    Executable = "/usr/sbin/jail",
    Name = jail_name_from_pid(),
    Args = ["-r", Name],
    Port = open_port({spawn_executable, Executable},
                     [exit_status,
                      {line, 1024},
                      {args, Args}
                      ]),
    lager:info("~p: Shutting down jail: ~p", [Port, [Executable | Args]]),
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


umount_devfs(Path) ->
    Args = [Path ++ "/dev"],
    Port = open_port({spawn_executable, "/sbin/umount"}, [exit_status, {line, 1024}, {args, Args}]),
    lager:info("~p: Executing umount-command: ~p", [Port, Args]),
    receive
        {Port, {exit_status, N}} ->
            lager:info("Umount-command finished with exit code: ~p", [N]),
            N
    end.
