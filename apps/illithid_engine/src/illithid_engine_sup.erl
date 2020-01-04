%%%-------------------------------------------------------------------
%% @doc illithid_engine top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(illithid_engine_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id      => illithid_engine_api,
                    start   => {illithid_engine_api, start_link, []}
                   },
                  #{id      => illithid_engine_layer,
                    start   => {illithid_engine_layer, start_link, []}
                   },
                  #{id      => illithid_engine_image,
                    start   => {illithid_engine_image, start_link, []}
                   },
                  #{id      => illithid_engine_jail_pool,
                    start   => {illithid_engine_jail_pool, start_link, []},
                    type    => supervisor
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
