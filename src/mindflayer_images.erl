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
        create_image/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { counter }).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


create_image(Cmd, CmdArgs, Parent) ->
    gen_server:call(?SERVER, {create_image, {Cmd, CmdArgs, Parent}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(image_table, [protected, named_table, {keypos, 1}]),
    {ok, #state { counter = 0 }}.


handle_call({create_image, {Cmd, CmdArgs, Parent}}, _From, #state { counter = N } = State) ->
    Dataset = ?ZROOT ++ "/" ++ "image_build_" ++ erlang:integer_to_list(N),
    0 = mindflayer_zfs:clone(?BASEJAIL_IMAGE ++ "@image", Dataset),
    Jail = #jail{
              path         = "/" ++ Dataset, %% Relying on mountpoint and dataset structure are equal
              parameters   = ["mount.devfs", "ip4.addr=10.13.37.3"],
              command      = Cmd,
              command_args = CmdArgs
             },

    SnapBegin = Dataset ++ "@image_init",
    SnapEnd = Dataset ++ "@image",

    mindflayer_zfs:snapshot(SnapBegin),
    {ok, {exit_status, 0}} = mindflayer_jail:start_and_finish_jail([Jail]),
    mindflayer_zfs:snapshot(SnapEnd),

    {ok, DigestID} = mindflayer_zfs:fingerprint(SnapBegin, SnapEnd),
    DatasetNew = ?ZROOT ++ "/" ++ DigestID,

    mindflayer_zfs:destroy(SnapBegin),
    mindflayer_zfs:rename(Dataset, DatasetNew),

    Image = #image {
               id             = DigestID,
               parent_dataset = Parent,
               dataset        = DatasetNew
              },
    ets:insert(image_table, [Image]),
    {reply, {ok, Image}, State#state { counter = N + 1 }};

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
