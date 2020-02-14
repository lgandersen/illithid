%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%%
%%% @end
%%% Created : 2019-11-16 14:11:22.836399
%%%-------------------------------------------------------------------
-module(illithid_engine_layer).

-behaviour(gen_server).

-include_lib("include/illithid.hrl").

%% API
-export([start_link/0,
         new/1,
         finalize_layer/1
         ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(SnapEnd(Dataset), Dataset ++ "@layer").


-record(state, { counter }).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


new(#image { layers = [#layer {id = LayerId } | _Rest ]}) ->
    gen_server:call(?SERVER, {new, LayerId}).


finalize_layer(LayerId) ->
    gen_server:call(?SERVER, {finalize_layer, LayerId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(layer_table, [protected, named_table, {keypos, 2}]),
    ets:insert(layer_table, [?BASE_LAYER]),
    {ok, #state { counter = 0 }}.


handle_call({new, LayerId}, _From, State) ->
    ?LOG("Creating new layer based on ~s~n", [LayerId]),
    {ok, Layer} = Reply = initialize_layer(LayerId),
    ets:insert(layer_table, [Layer]),
    {reply, Reply, State};


handle_call({finalize_layer, LayerId}, _From, State) ->
    [Layer] = ets:lookup(layer_table, LayerId),
    #layer {dataset = Dataset } = Layer,
    illithid_engine_zfs:snapshot(?SnapEnd(Dataset)),
    UpdLayer = Layer#layer { location = ?SnapEnd(Dataset) },
    ets:insert(layer_table, [UpdLayer]),
    {reply, {ok, UpdLayer}, State};


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
initialize_layer(ParentLayerId) ->
    Id = illithid_engine_util:uuid(),
    Dataset = ?ZROOT ++ "/" ++ Id,
    ParentLocation = fetch_location(ParentLayerId),
    0 = illithid_engine_zfs:clone(ParentLocation, Dataset),
    {ok, #layer { id = Id, parent_id = ParentLayerId, dataset = Dataset, path = "/" ++ Dataset}}.


fetch_location(LayerId) ->
    [#layer { location = Location }] = ets:lookup(layer_table, LayerId),
    Location.
