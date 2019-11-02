%%%-------------------------------------------------------------------
%% @doc mindflayer public API
%% @end
%%%-------------------------------------------------------------------

-module(mindflayer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mindflayer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
