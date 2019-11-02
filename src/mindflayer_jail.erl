%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%%
%%% @end
%%% Created : 2019-10-14 09:16:54.803198
%%%-------------------------------------------------------------------
-module(mindflayer_jail).

-behaviour(gen_server).

%% API
-export([start_link/0,
        create/1,
        destroy/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(BASEJAIL_SNAPSHOT, "zroot/mindflayer_base@base").

-record(state, {
          jail_table = none,
          next_jid = 1
         }).

-record(jail, {
          jid      = none,
          name     = none,
          path     = none,
          command  = none,
          arg      = "",
          param    = []
          }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    "" = mindflayer_zfs:snapshot(?BASEJAIL_SNAPSHOT),
    JailTable = ets:new(jail_table, [protected, {keypos, 0}]),
    {ok, #state{jail_table=JailTable}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create(#jail{jid=JId, name=Name, path=Path, command=Cmd, arg=Arg, param=Param}) ->
    {ok, ParamString} = commandify_param(Param, ""),
    % $ jail -c path=/data/jail/testjail mount.devfs host.hostname=testhostname ip4.addr=192.0.2.100 command=/bin/sh
    % Creating a permanent jail: create("testjail", "/" ++ ?TESTJAIL, "10.13.37.3", "/bin/sh", " /etc/rc"),

    %mindflayer_utils:exec(io_lib:format("jail -c path=~p name=~p mount.devfs ip4.addr=~p command=~p", [Path, Name, IP, Cmd]) ++ Args).
    ExecString = "jail -c jid=~p path=~p name=~p " ++ ParamString ++ " command=~p",
    mindflayer_utils:exec(io_lib:format(ExecString, [JId, Path, Name, Cmd]) ++ Arg).


commandify_param([Param | Rest], ParamStr) ->
    commandify_param(Rest, ParamStr ++ " " ++ Param);

commandify_param([], ParamStr) ->
    {ok, ParamStr}.


destroy(#jail{name=Name, path=Path}) ->
    umount_devfs(Path),
    mindflayer_utils:exec(io_lib:format("jail -r ~s", [Name])).

umount_devfs(JailPath) ->
    %% Try using devfs instead
    case mindflayer_utils:exec("/sbin/umount " ++ JailPath ++ "/dev") of
        "" ->
            umount_devfs(JailPath);
        OutPut ->
            OutPut
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_BASEJAIL_SNAPSHOT, "zroot/mindflayer_base@base_unittest_jail").
-define(TESTJAIL, "unittest_jail_jail").
-define(TESTJAIL_PATH, "zroot/mindflayer_dev/" ++ ?TESTJAIL).

jail_creation_test() ->
    Jail = #jail{
              jid = 1,
              name= ?TESTJAIL,
              path= "/" ++ ?TESTJAIL_PATH,
              command="/bin/ls",
              param=["mount.devfs", "ip4.addr=10.13.37.3"]
             },
    "" = mindflayer_zfs:snapshot(?TEST_BASEJAIL_SNAPSHOT),
    "" = mindflayer_zfs:clone(?TEST_BASEJAIL_SNAPSHOT, ?TESTJAIL_PATH),
    Output = create(Jail),
    Output = create(Jail#jail{ arg = " /" }),
    destroy(Jail),
    "" = mindflayer_zfs:destroy(?TESTJAIL_PATH),
    "" = mindflayer_zfs:destroy(?TEST_BASEJAIL_SNAPSHOT).
-endif.
