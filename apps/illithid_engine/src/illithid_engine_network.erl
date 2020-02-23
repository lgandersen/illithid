%%%-------------------------------------------------------------------
%%% @author Lasse Grinderslev Andersen
%%% @copyright (C) 2020, Lasse Grinderslev Andersen
%%% @doc
%%%
%%% @end
%%% Created : 2020-02-20 10:20:54.780679
%%%-------------------------------------------------------------------
-module(illithid_engine_network).

-behaviour(gen_server).

-include_lib("include/illithid.hrl").

%% API
-export([start_link/0,
         new_ip/0,
         remove_ip/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
         ip_range = none,
         used_ips = none
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


new_ip() ->
    gen_server:call(?SERVER, new_ip).


remove_ip(Ip) ->
    gen_server:cast(?SERVER, {remove, Ip}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    State = #state {
              ip_range = ?IP_RANGE,
              used_ips = sets:new()
              },
    exec_cmd("ifconfig ~s destroy", [?INET_IF]),
    exec_cmd("ifconfig lo create name ~s", [?INET_IF]),
    process_flag(trap_exit, true),
    {ok, State}.


handle_call(new_ip, _From, State) ->
    {Ip, NewState} = generate_new_ip(State),
    {reply, Ip, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({remove, Ip}, State) ->
    NewState = remove_ip(Ip, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    log("terminating illithid_engine_network"),
    exec_cmd("ifconfig ~s destroy", [?INET_IF]),
    ok.


code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_new_ip(#state { ip_range = {StartIp, EndIp}, used_ips = UsedIps } = State) ->
    StartIpN = ip2int(StartIp),
    EndIpN = ip2int(EndIp),
    {Ip, NewUsedIps} = generate_new_ip_(StartIpN, EndIpN, UsedIps),
    {Ip, State#state { used_ips = NewUsedIps}}.


generate_new_ip_(IpN, EndIpN, UsedIps) when IpN > EndIpN ->
    {out_of_ips, UsedIps};

generate_new_ip_(IpN, EndIpN, UsedIps) ->
    Ip = int2ip(IpN),
    case sets:is_element(Ip, UsedIps) of
        true ->
            NextIpN = IpN + 1,
            generate_new_ip_(NextIpN, EndIpN, UsedIps);

        false ->
            NewUsedIps = sets:add_element(Ip, UsedIps),
            IpString = inet:ntoa(Ip),
            [] = exec_cmd("ifconfig ~s alias ~s/32", [?INET_IF, IpString]),
            {IpString, NewUsedIps}
    end.

-define(POW(N), erlang:round(math:pow(256, N))).

ip2int({A, B, C, D}) ->
    D + C * ?POW(1) + B * ?POW(2) + A * ?POW(3).


int2ip(N) ->
    int2ip_(N, 3, []).


int2ip_(N, 0, Prev) ->
    erlang:list_to_tuple(lists:reverse([N | Prev]));

int2ip_(N, Order, Prev) ->
    X = erlang:floor(N / ?POW(Order)),
    N_next = N -  X * ?POW(Order),
    int2ip_(N_next, Order - 1, [X | Prev]).


remove_ip(IpString, #state { used_ips = UsedIps } = State) ->
    {ok, Ip} = inet:parse_address(IpString),
    NewUsedIp = sets:del_element(Ip, UsedIps),
    [] = exec_cmd("ifconfig ~s -alias ~s", [?INET_IF, IpString]),
    State#state { used_ips = NewUsedIp }.


exec_cmd(CmdTemplate, Args) ->
    Cmd = io_lib:format(CmdTemplate, Args),
    log(Cmd),
    os:cmd(Cmd).


log(Cmd) ->
    lager:info(erlang:atom_to_list(?MODULE) ++ ": " ++ Cmd).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
simple_generate_new_ip_test() ->
    lager:start(),
    {ok, _Pid} = illithid_engine_network:start_link(),
    State = #state{ ip_range = {
                      {10, 13, 37, 1}, {10, 13, 37, 3}
                    },
                    used_ips = sets:new()
                  },
    {Ip1, NewState1} = generate_new_ip(State),
    ?assertEqual("10.13.37.1", Ip1),
    {Ip2, NewState2} = generate_new_ip(NewState1),
    ?assertEqual("10.13.37.2", Ip2),
    {Ip3, NewState3} = generate_new_ip(NewState2),
    ?assertEqual("10.13.37.3", Ip3),
    {Ip4, _NewState4} = generate_new_ip(NewState3),
    ?assertEqual(out_of_ips, Ip4),
    gen_server:stop(illithid_engine_network).

advanced_generate_new_ip_test() ->
    {ok, _Pid} = illithid_engine_network:start_link(),
    State = #state{ ip_range = {
                      {10, 13, 37, 254}, {10, 13, 38, 0}
                    },
                    used_ips = sets:new()
                  },
    {Ip1, NewState1} = generate_new_ip(State),
    ?assertEqual("10.13.37.254", Ip1),
    {Ip2, NewState2} = generate_new_ip(NewState1),
    ?assertEqual("10.13.37.255", Ip2),
    {Ip3, NewState3} = generate_new_ip(NewState2),
    ?assertEqual("10.13.38.0", Ip3),
    {Ip4, _NewState4} = generate_new_ip(NewState3),
    ?assertEqual(out_of_ips, Ip4),
    gen_server:stop(illithid_engine_network).
-endif.
