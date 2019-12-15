%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%%
%%% @end
%%% Created : 2019-11-16 14:11:22.836399
%%%-------------------------------------------------------------------
-module(mindflayer_images).

-behaviour(gen_server).

-include_lib("mindflayer.hrl").

%% API
-export([start_link/0,
        create_image/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { }).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


create_image(Cmd, CmdArgs, Parent, Destination) ->
    gen_server:call(?SERVER, {create_image, {Cmd, CmdArgs, Parent, Destination}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(image_table, [protected, named_table, {keypos, 1}]),
    {ok, #state { }}.


handle_call({create_image, {Cmd, CmdArgs, Parent, Destination}}, _From, State) ->
    %mindflayer_zfs:clone(Parent ++ "@image", Destination), % auto mount-path
    Jail = #jail{
              name         = "what_should_we_call_this",
              image        = Parent, %TODO FIXME image and zfs_dataset have overlapping meaning. Resolve this.
              path         = "/" ++ Destination, %% Relying on mountpoint and dataset structure are equal
              zfs_dataset  = Destination,
              parameters   = ["mount.devfs", "ip4.addr=10.13.37.3"],
              command      = Cmd,
              command_args = CmdArgs
             },

    {ok, {exit_status, 0}} = mindflayer_jail:start_and_finish_jail([Jail]),
    mindflayer_zfs:snapshot(Destination ++ "@image"),
    ets:insert(image_table, [#image { parent = Parent, destination = Destination }]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
