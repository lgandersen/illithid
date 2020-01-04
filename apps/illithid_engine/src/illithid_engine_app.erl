%%%-------------------------------------------------------------------
%% @doc illithid public API
%% @end
%%%-------------------------------------------------------------------

-module(illithid_engine_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    illithid_engine_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
