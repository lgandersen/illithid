%%%-------------------------------------------------------------------
%%% @author Lasse Grinderslev Andersen
%%% @copyright (C) 2020, Lasse Grinderslev Andersen
%%% @doc
%%%
%%% @end
%%% Created : 2020-01-22 16:15:22.710405
%%%-------------------------------------------------------------------
-module(illithid_cli_engine_client).

-behaviour(gen_server).

%% API
-export([start_link/0,
         command/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-include_lib("include/illithid.hrl").

-record(state, { socket = none, cmd = none, cli_pid = none, buffer = <<>> }).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).


command(Cmd) ->
    gen_server:cast(?SERVER, {send, Cmd}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([CliPid]) ->
    case gen_tcp:connect({local, ?API_SOCKET}, 0, [binary, {packet, raw}, {active, true}]) of
        {ok, Socket} ->
            {ok, #state{ socket = Socket, cli_pid = CliPid }};

        {error, Reason} ->
            io:format("Error! Could not connect to backend on ~p~n", [?API_SOCKET]),
            {stop, Reason}
    end.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({send, Cmd}, #state { socket = Socket } = State) ->
    CmdBin = erlang:term_to_binary(Cmd),
    case gen_tcp:send(Socket, CmdBin) of
        ok ->
            {noreply, State#state { cmd = Cmd }};

        {error, Reason} ->
            io:format("Sending command to backend failed: ~p~n", [Reason]),
            {stop, Reason, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({tcp_closed, _Socket}, #state { cli_pid = CliPid } = State) ->
    CliPid ! done,
    {stop, {shutdown, tcp_closed}, State};

handle_info({tcp_error, _Socket, Reason}, #state { cli_pid = CliPid } = State) ->
    io:format("An error occured while communicating with the backend: ~s", [Reason]),
    CliPid ! done,
    {stop, {shutdown, {tcp_error, Reason}}, State};


handle_info({tcp, _Socket, ReplyBin}, #state { cmd = Cmd, buffer = Buffer } = State) ->
    %% TODO: The present buffering is not entirely safe (although in a request/reply setting it is:
    %% binary_to_term silently drops bytes that is not part of the encoded term
    NewBuffer = <<Buffer/binary, ReplyBin/binary>>,
    case erlang:binary_to_term(NewBuffer) of
        badarg ->
            {noreply, State#state{ buffer = NewBuffer }};

        Reply ->
            handle_reply(Cmd, Reply),
            {noreply, State#state { buffer = <<>> }}
    end;


handle_info(Info, State) ->
    lager:warning("Unkown message received: ~p ~n", [Info]),
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%FIXME: Consider moving this to the cli. But in a way that makes it easily testable.
%% perhaps io_lib:format should be used instead and the out sent to the cli. This would
%% make it possible to easily test the messages back'n'forth before testing the cli-tool itself
%% using os:cmd/1
handle_reply(clear_zroot, _) ->
    io:format("ZRoot cleared!~n");

handle_reply(list_images, Images) ->
    io:format("REPOSITORY    TAG               IMAGE ID         CREATED               SIZE\n"),
    print_images(Images);

handle_reply({build, Path, _NameTag}, {ok, Image}) ->
    io:format("Succesfully built image from ~p~n", [Path]),
    io:format("Image id: ~s~n", [Image#image.id]);

handle_reply({run, ImageIdentifier}, Reply) ->
    io:format(user, "LOOOL~p: ~p~n", [ImageIdentifier, Reply]);

handle_reply(_Request, Reply) ->
    io:format(user, "LEEEL: ~p~n", [Reply]),
    Reply.


print_images([#image { id = Id, tag = Tag, created = Created } | Rest]) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(Created),

    %% "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00" => "2019-09-19T15:07:03.0+00:00"
    Datetime = io_lib:format(
                 "~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                 [Year, Month, Day, Hour, Min, Sec]),

    io:format(
      "n/a           ~s       ~s     ~s   n/a MB~n",
      [cell(Tag, 11), cell(Id, 12), cell(Datetime, 11)]),
    print_images(Rest);

print_images([]) ->
    ok.


cell(Content, Size) ->
    case length(Content) =< Size of
        true ->
            Content ++ string:copies(" ", Size - length(Content));

        false ->
            string:sub_string(Content, 1, Size)
    end.



%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").
%
%initialize() ->
%    illithid_engine_zfs:clear_zroot(),
%    ok = application:start(illithid_engine),
%    illithid_engine_metadata:clear_all(),
%    ok.
%
%stop(_) ->
%    ok = application:stop(illithid_engine),
%    ok.
%
%instructions_test_() ->
%     {foreach, fun initialize/0, fun stop/1, [
%                                  fun test_build_image/1
%                                  ]
%     }.
%
%
%%% FIXME WIP
%test_build_image(_) ->
%    {ok, Pid} = illithid_cli_engine_client:start_link(),
%    unlink(Pid),
%    Path = "./apps/illithid_cli/test/illithid_cli_SUITE_data/Dockerfile",
%    io:format(user, "Command not understood: DSDFSDFDSF~n", []),
%    illithid_cli_engine_client:command({build, Path, {"testcliengine", "latest"}}),
%    receive
%        done ->
%            io:format(user, "HMMM~n", []),
%            ok;
%
%        UnkownMsg ->
%            io:format(user, "Command not understood: ~p~n", [UnkownMsg])
%    end,
%    ?_assertEqual(ok, ok).
%
%-endif.
