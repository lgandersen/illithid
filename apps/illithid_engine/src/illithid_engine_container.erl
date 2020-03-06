%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%% This gen_server manages jails: Creates, starts, destroys and monitors jails.
%%% @end
%%%-------------------------------------------------------------------
-module(illithid_engine_container).

-behaviour(gen_server).

%% API
-export([create/1,
         metadata/1,
         attach/1,
         start/1,
         start_await/1,
         stop_await/1,
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
          caller        = none,
          relay_to      = none,
          image         = none,
          container     = none,
          closing_port  = none,
          starting_port = none
         }).

%%%===================================================================
%%% API
%%%===================================================================
create(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).


attach(Pid) ->
    gen_server:call(Pid, {attach, self()}).


start(Pid) ->
    gen_server:cast(Pid, start).


start_await(Pid) ->
    ok = attach(Pid),
    gen_server:cast(Pid, start),
    await_exit_status(Pid).


stop_await(Pid) ->
    ok = attach(Pid),
    gen_server:cast(Pid, {stop_await, self()}),
    receive
        {container_msg, Pid, {exit_status, N}} ->
            {ok, {exit_status, N}}
    end.


metadata(Pid) ->
    gen_server:call(Pid, metadata).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Opts]) ->
    #image {id      = ImageId,
            user    = DefaultUser,
            command = DefaultCmd } = Image = proplists:get_value(image, Opts, ?BASE_IMAGE),
    {ok, Layer} = illithid_engine_layer:new(Image),
    Cmd = proplists:get_value(cmd, Opts, DefaultCmd),
    User = proplists:get_value(user, Opts, DefaultUser),
    RelayTo = proplists:get_value(relay_to, Opts, none),
    JailParam = proplists:get_value(jail_param, Opts, []),

    Container = #container {
                   id         = illithid_engine_util:uuid(),
                   name       = illithid_engine_name_generator:new(),
                   ip         = illithid_engine_network:new_ip(),
                   pid        = self(),
                   command    = Cmd,
                   layer      = Layer,
                   image_id   = ImageId,
                   parameters = ["exec.jail_user=" ++ User | JailParam],
                   created    = erlang:timestamp()
                  },
    illithid_engine_metadata:add_container(Container),
    {ok, #state { container = Container, relay_to = RelayTo, image = Image }}.


handle_call({attach, Pid}, _From, State) ->
    {reply, ok, State#state { relay_to = Pid }};

handle_call(metadata, _From, #state { container = Container } = State) ->
    {reply, Container, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(start, #state { container = Container} = State) ->
    Port = start_(Container),
    UpdatedContainer = Container#container { running = true },
    illithid_engine_metadata:add_container(UpdatedContainer),
    {noreply, State#state {
                starting_port = Port,
                container     = UpdatedContainer }
    };

handle_cast({stop_await, Pid}, #state { container = Container} = State) ->
    Port = stop_jail(Container),
    {noreply, State#state { closing_port = Port, caller = Pid }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, N} = Msg}, #state { closing_port = Port, caller = Caller,  container = Container } = State) ->
    #container { id = Id, layer = #layer { path = Path }} = Container,
    lager:info("container-~s jail was closed with exit-code: ~p", [Id, N]),
    umount_devfs(Path),
    relay_message(Msg, Caller),
    UpdatedContainer = Container#container { running = false, pid = none },
    illithid_engine_metadata:add_container(UpdatedContainer),
    {stop, {shutdown, jail_stopped}, State#state {
                                       caller       = none,
                                       closing_port = none,
                                       container    = UpdatedContainer }
    };

handle_info({Port, {exit_status, N} = Msg}, State = #state { starting_port = Port, relay_to = RelayTo, container = Container }) ->
    #container { id = Id, layer = #layer { path = Path }} = Container,
    lager:info("container-~s jail finished with exit-code: ~p", [Id, N]),
    umount_devfs(Path),
    relay_message(Msg, RelayTo),
    {noreply, State};

handle_info({Port, Msg}, State = #state { starting_port = Port, relay_to = RelayTo, container = Container }) ->
    lager:debug("container#~s message received: ~p", [Container#container.id, Msg]),
    relay_message(Msg, RelayTo),
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
relay_message(Msg, RelayTo) ->
    case {RelayTo, Msg} of
        {none, _} ->
            ok;

        {Pid, {exit_status, N}} when is_pid(Pid) ->
            RelayTo ! {container_msg, self(), {exit_status, N}};

        {Pid, _} when is_pid(Pid) ->
            RelayTo ! {container_msg, self(), Msg}
    end.


await_exit_status(Pid) ->
    receive
        {container_msg, Pid, {exit_status, N}} ->
            {ok, {exit_status, N}};

        {container_msg, Pid, _Msg} ->
            await_exit_status(Pid);

        UnknownMsg ->
            lager:warning("Unknown message received while waiting for exit status: ~p", [UnknownMsg]),
            await_exit_status(Pid)
    end.


start_(#container {
        id         = Id,
        layer      = #layer { path = Path },
        command    = [Cmd | CmdArgs],
        ip         = Ip,
        parameters = Parameters }) ->

    Executable = "/usr/sbin/jail",
    Args = ["-c",
            "path=" ++ Path,
            "name=" ++ Id,
            "ip4.addr=" ++ Ip
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


stop_jail(#container { id = Id }) ->
    Executable = "/usr/sbin/jail",
    Args = ["-r", Id],
    Port = open_port({spawn_executable, Executable},
                     [exit_status,
                      {line, 1024},
                      {args, Args}
                      ]),
    lager:info("~p: Shutting down jail: ~p", [Port, [Executable | Args]]),
    Port.


umount_devfs(Path) ->
    Args = [Path ++ "/dev"],
    Port = open_port({spawn_executable, "/sbin/umount"}, [exit_status, {line, 1024}, {args, Args}]),
    lager:info("~p: Executing umount-command: ~p", [Port, Args]),
    receive
        {Port, {exit_status, N}} ->
            lager:info("Umount-command finished with exit code: ~p", [N]),
            N
    end.
