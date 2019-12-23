%%%-------------------------------------------------------------------
%%% @author Lasse Grinderslev Andersen
%%% @copyright (C) 2019, Lasse Grinderslev Andersen
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-15 10:00:19.045431
%%%-------------------------------------------------------------------
-module(mindflayer_image_builder).

-include_lib("mindflayer.hrl").

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

create_image(Instructions, Context) ->
    State = #state { instructions = Instructions, layers = [], context = Context},
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

    {ok, #layer { id = ImageId } = Layer} = mindflayer_layers:create_layer(run, ParentImageId, [Cmd, CmdArgs]),
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

    {ok, #layer { id = ImageId } = Layer} = mindflayer_layers:create_layer(copy, ParentImageId, [Context, DestAndSrc]),
    NewState = State#state {
                 instructions = Rest,
                 parent_layer = ImageId,
                 layers       = [ Layer | Layers ]},
    proces_instructions(NewState);

proces_instructions(#state { instructions = [], layers = Layers }) ->
    {ok, lists:reverse(Layers)}.
