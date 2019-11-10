-module(mindflayer_jail_pool).
-behaviour(supervisor).

%% API.
-export([start_link/0,
         start_jail/1
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


-spec start_jail(tuple()) -> {ok, Pid} | {error, Reason}
    when
        Pid    :: pid(),
        Reason :: term().
start_jail(Jail) ->
    supervisor:start_child(?SERVER, [Jail]).


%% @private
-spec init([]) -> {ok, {{simple_one_for_one, non_neg_integer(), non_neg_integer()}, []}}.
init([]) ->
    SupFlags = #{
      strategy => simple_one_for_one,
      intensity => 10,
      period => 10
     },
    ChildSpec = #{
      id => mindflayer_jail,
      start => {mindflayer_jail, start_link, []},
      restart => temporary,
      shutdown => 5000,
      type => worker,
      modules => [mindflayer_jail]
     },
    {ok, {SupFlags, [ChildSpec]}}.
