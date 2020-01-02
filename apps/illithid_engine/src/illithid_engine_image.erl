%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-24 12:39:08.920497
%%%-------------------------------------------------------------------
-module(illithid_engine_image).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("include/illithid.hrl").

%% API
-export([create_image/1, create_image/2]).

-record(build_state, {
          instructions = none,
          context      = none,
          parent_layer = none,
          caller       = none,
          image_record = none
         }).


-record(state, {
         image_table = image_table
         }).

create_image(Instructions) ->
    create_image(Instructions, "./").

create_image(Instructions, ContextPath) ->
    ImageRecord = #image { name = none, tag = none, layers = [], command = none },
    BuildState = #build_state { instructions = Instructions, context = ContextPath, image_record = ImageRecord },
    gen_server:call(?SERVER, {build_image, BuildState}).


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(image_table, [protected, named_table, {keypos, 2}]),
    ets:insert(image_table, [?BASE_IMAGE]),
    {ok, #state{}}.


handle_call({build_image, BuildState}, _From, State) ->
    {ok, Image} = Reply = proces_instructions(BuildState),
    ets:insert(image_table, [Image]),
    {reply, Reply, State};


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

proces_instructions(#build_state { instructions = [ {from, ImageId} | Rest ] } = State) ->
    NewState = State#build_state {
                  instructions = Rest,
                  parent_layer = ImageId
                 },
    proces_instructions(NewState);

proces_instructions(#build_state { instructions = [ {cmd, Args} | Rest ], image_record = Image } = State) ->
    NewState = State#build_state {
                  instructions = Rest,
                  image_record = Image#image { command = Args }
                 },
    proces_instructions(NewState);


proces_instructions(#build_state {
                       instructions = [ Instruction | Rest ],
                       parent_layer = ParentImageId,
                       image_record = #image { layers = Layers },
                       context      = Context
                       } = State) ->

    {ok, #layer { id = ImageId } = Layer} = illithid_engine_layer:create_layer(Context, Instruction, ParentImageId),
    NewState = State#build_state {
                 instructions = Rest,
                 parent_layer = ImageId,
                 image_record = #image {
                                   layers = [ Layer | Layers ]
                                  }},
    proces_instructions(NewState);

proces_instructions(#build_state { instructions = [], image_record = #image { layers = [#layer { id = ImageId } | _Rest] } = Image }) ->
    {ok, Image#image {id = ImageId }}.
