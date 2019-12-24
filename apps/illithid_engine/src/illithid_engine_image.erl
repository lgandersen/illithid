%%%-------------------------------------------------------------------
%%% @author Lasse Grinderslev Andersen
%%% @copyright (C) 2019, Lasse Grinderslev Andersen
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-15 10:00:19.045431
%%%-------------------------------------------------------------------
-module(illithid_engine_image).

-include_lib("include/illithid.hrl").

%% API
-export([create_image/1, create_image/2]).

-record(state, {
          instructions = none,
          context      = none,
          parent_layer = none,
          layers       = none,
          caller       = none
         }).


create_image(Instructions) ->
    create_image(Instructions, "./").

create_image(Instructions, ContextPath) ->
    State = #state { instructions = Instructions, layers = [], context = ContextPath},
    proces_instructions(State).


proces_instructions(#state { instructions = [ {from, ImageId} | Rest ] } = State) ->
    NewState = State#state {
                  instructions = Rest,
                  parent_layer = ImageId
                 },
    proces_instructions(NewState);

proces_instructions(#state {
                       instructions = [ {run, Cmd, CmdArgs} | Rest ],
                       parent_layer = ParentImageId,
                       layers       = Layers
                       } = State) ->

    {ok, #layer { id = ImageId } = Layer} = illithid_engine_layer:create_layer(run, ParentImageId, [Cmd, CmdArgs]),
    NewState = State#state {
                 instructions = Rest,
                 parent_layer = ImageId,
                 layers       = [ Layer | Layers ]},
    proces_instructions(NewState);

proces_instructions(#state {
                       instructions = [ {copy, DestAndSrc} | Rest ],
                       parent_layer = ParentImageId,
                       layers       = Layers,
                       context      = Context
                       } = State) ->

    {ok, #layer { id = ImageId } = Layer} = illithid_engine_layer:create_layer(copy, ParentImageId, [Context, DestAndSrc]),
    NewState = State#state {
                 instructions = Rest,
                 parent_layer = ImageId,
                 layers       = [ Layer | Layers ]},
    proces_instructions(NewState);

proces_instructions(#state { instructions = [], layers = Layers }) ->
    {ok, Layers}.
