-module(illithid_engine_container_pool).
-behaviour(supervisor).

%% API.
-export([start_link/0,
         new/0,
         stop/1
        ]).

%% Supervisor callbacks.
-export([init/1]).


%% From supervisor.
-type start_link_err() :: {already_started, pid()} | shutdown | term().
-type start_link_ret() :: {ok, pid()} | ignore | {error, start_link_err()}.

-define(SERVER, ?MODULE).


-spec start_link() -> start_link_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(Pid) ->
    supervisor:terminate_child({local, ?MODULE}, Pid).


-spec new() -> {ok, Pid} | {error, Reason}
    when
        Pid    :: pid(),
        Reason :: term().
new() ->
    supervisor:start_child(?SERVER, []).


%% @private
-spec init([]) -> {ok, {{simple_one_for_one, non_neg_integer(), non_neg_integer()}, []}}.
init([]) ->
    SupFlags = #{
      strategy => simple_one_for_one,
      intensity => 10,
      period => 10
     },
    ChildSpec = #{
      id => illithid_engine_container,
      start => {illithid_engine_container, start_link, []},
      restart => temporary,
      shutdown => 5000,
      type => worker,
      modules => [illithid_engine_container]
     },
    {ok, {SupFlags, [ChildSpec]}}.
