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

-include_lib("include/illithid.hrl").
-include_lib("include/printing.hrl").

-define(SERVER, ?MODULE).


-record(state, { socket = none, cmd = none, cli_pid = none, buffer = <<>> }).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).


command(Cmd) ->
    gen_server:cast(?SERVER, {command, Cmd}).

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


handle_cast({command, Cmd}, #state { socket = Socket } = State) ->
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


handle_info({tcp, Socket, ReplyBin}, #state { cmd = Cmd, buffer = Buffer } = State) ->
    NewBuffer = decode_buffer(<<Buffer/binary, ReplyBin/binary>>, Cmd, Socket),
    {noreply, State#state{ buffer = NewBuffer }};


handle_info(Info, State) ->
    io:format(user, "illithid_cli_engine_client: Unkown message received: ~p ~n", [Info]),
    {noreply, State}.



terminate(Reason, #state{ socket = Socket }) ->
    io:format(user, "illithid_cli_engine_client: Shutting down ~p ~n", [Reason]),
    gen_tcp:close(Socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
        {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_buffer(<<>>, _Cmd, _Socket) ->
    <<>>;

decode_buffer(Buffer, Cmd, Socket) ->
    case erlang:binary_to_term(Buffer) of
        badarg ->
            Buffer;

        Reply ->
            BufferUsed = size(erlang:term_to_binary(Reply)),
            handle_reply(Cmd, Reply, Socket),
            NewBuffer = binary:part(Buffer, BufferUsed, size(Buffer) - BufferUsed),
            decode_buffer(NewBuffer, Cmd, Socket)
    end.


handle_reply(clear_zroot, ok, _Socket) ->
    to_cli(?ZROOT_CLEARED),
    done();

handle_reply(list_images, Images, _Socket) ->
    to_cli(?LIST_IMAGES_HEADER),
    print_images(Images);

handle_reply({build, Path, _NameTag}, {ok, Image}, _Socket) ->
    to_cli("Succesfully built image from ~p~n", [Path]),
    to_cli("Image id: ~s~n", [Image#image.id]),
    done();

handle_reply({run, _ImageId, _Command}, {data, {eol, Line}} ,_) ->
    to_cli(Line ++ "\n");

handle_reply({run, _ImageId, _Command}, {exit_status, N} ,_) ->
    to_cli("Container exited with status ~p~n", [N]),
    done();

handle_reply({run, _ImageId, _Command}, Reply ,_) ->
    io:format(user, "Unkown command: ~p~n", [Reply]);

handle_reply(_Request, Reply,_) ->
    io:format(user, "Reply not understood: ~p~n", [Reply]),
    Reply.


print_images([#image { id = Id, name = Name, tag = Tag, created = Created } | Rest]) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(Created),

    %% "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00" => "2019-09-19T15:07:03.0+00:00"
    Datetime = io_lib:format(
                 "~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                 [Year, Month, Day, Hour, Min, Sec]),

    to_cli(
      "~s  ~s  ~s  ~s  n/a MB~n",
      [cell(Name, 12), cell(Tag, 10), cell(Id, 12), cell(Datetime, 11)]),
    print_images(Rest);

print_images([]) ->
    done(),
    ok.


cell(Content, Size) ->
    case length(Content) =< Size of
        true ->
            Content ++ string:copies(" ", Size - length(Content));

        false ->
            string:sub_string(Content, 1, Size)
    end.


to_cli(Msg) ->
    to_cli(Msg, []).

to_cli(Msg, Args) ->
    cli_process ! {cli_msg, io_lib:format(Msg, Args)}.


done() ->
    cli_process ! done.
