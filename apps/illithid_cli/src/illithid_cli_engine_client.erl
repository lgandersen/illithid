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

-define(to_cli(Msg), cli_process ! io_lib:format(Msg, [])).
-define(to_cli(Msg, Args), cli_process ! io_lib:format(Msg, Args)).
-define(LIST_IMAGES_HEADER, "REPOSITORY    TAG               IMAGE ID         CREATED               SIZE\n").
-define(ZROOT_CLEARED, "Illithid zroot cleared.\n").

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
    ?to_cli(?ZROOT_CLEARED),
    cli_process ! done;

handle_reply(list_images, Images, _Socket) ->
    ?to_cli(?LIST_IMAGES_HEADER),
    print_images(Images);

handle_reply({build, Path, _NameTag}, {ok, Image}, _Socket) ->
    ?to_cli("Succesfully built image from ~p~n", [Path]),
    ?to_cli("Image id: ~s~n", [Image#image.id]),
    cli_process ! done;

handle_reply({run, _ImageId}, {data, {eol, Line}} ,_) ->
    ?to_cli(Line ++ "\n");

handle_reply({run, _ImageId}, {exit_status, N} ,_) ->
    ?to_cli("Container exited with status ~p~n", [N]),
    cli_process ! done;

handle_reply({run, _ImageId}, Reply ,_) ->
    io:format(user, "Unkown command: ~p~n", [Reply]);

handle_reply(_Request, Reply,_) ->
    io:format(user, "Reply not understood: ~p~n", [Reply]),
    Reply.


print_images([#image { id = Id, tag = Tag, created = Created } | Rest]) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(Created),

    %% "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00" => "2019-09-19T15:07:03.0+00:00"
    Datetime = io_lib:format(
                 "~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                 [Year, Month, Day, Hour, Min, Sec]),

    ?to_cli(
      "n/a           ~s       ~s     ~s   n/a MB~n",
      [cell(Tag, 11), cell(Id, 12), cell(Datetime, 11)]),
    print_images(Rest);

print_images([]) ->
    cli_process ! done,
    ok.


cell(Content, Size) ->
    case length(Content) =< Size of
        true ->
            Content ++ string:copies(" ", Size - length(Content));

        false ->
            string:sub_string(Content, 1, Size)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

initialize() ->
    illithid_engine_zfs:clear_zroot(),
    %ok = application:start(illithid_engine),
    {ok, SupPid} = illithid_engine_sup:start_link(),
    unlink(SupPid),
    illithid_engine_metadata:clear_all(),
    {ok, Pid} = illithid_cli_engine_client:start_link(),
    unlink(Pid),
    register(cli_process, self()),
    {SupPid, Pid}.

stop({SupPid, Pid}) ->
    gen_server:stop(Pid),
    %ok = application:stop(illithid_engine),
    exit(SupPid, shutdown),
    unregister(cli_process),
    ok.

instructions_test_() ->
     {foreach, fun initialize/0, fun stop/1, [
                                  fun test_clear_zroot/1,
                                  fun test_build_image/1,
                                  fun test_list_images/1,
                                  fun test_run_image/1
                                  ]
     }.


test_clear_zroot(_) ->
    0 = illithid_engine_zfs:create(?ZROOT("test_cli_clear_all")),
    illithid_cli:main_(["clear", "all"]),
    [{tcp_closed, _Port}, ZRootCleared] = receive_messages("test-clear-zroot"),
    RCode = illithid_engine_zfs:create(?ZROOT("test_cli_clear_all")),
    [?_assertEqual(
        ?ZROOT_CLEARED,
        ZRootCleared
       ),
     ?_assertEqual(
        0,
        RCode
       )
     ].

-define(_assertTextEqual(Expected, Got),
        ?_assertEqual(Expected, unicode:characters_to_list(Got))).

test_list_images(_) ->
    Image1 = #image {
               id      = "lolololololooooooooooooooool",
               tag     = "test:latest",
               created = {1578, 330264, 608742}
              },
    Image2 = #image {
               id      = "leleleleleleeeeee",
               tag     = "test:oldest",
               created = {1578, 330200, 0}
              },
    illithid_engine_metadata:add_image(Image1),
    illithid_engine_metadata:add_image(Image2),
    illithid_cli:main_(["images"]),
    [Header, ImageStr_1, ImageStr_2] = receive_messages("test-list-images"),

    [
     ?_assertTextEqual(
        ?LIST_IMAGES_HEADER,
        Header),
     ?_assertTextEqual(
        "n/a           test:latest       lolololololo     2020-01-06 17:04:24   n/a MB\n",
        ImageStr_1),
     ?_assertTextEqual(
        "n/a           test:oldest       lelelelelele     2020-01-06 17:03:20   n/a MB\n",
        ImageStr_2)
    ].


test_build_image(_) ->
    [Id] = build_image("build-image"),
    FileContent = file:read_file("/" ++ ?ZROOT ++ "/" ++ Id  ++ "/root/test.txt"),
    %NameTagRaw = "testcliengine:latest",
    %illithid_cli:main_(["build", "-t", NameTagRaw, Path]),
    ?_assertEqual(
       {ok, <<"lol\n">>},
       FileContent).


test_run_image(_) ->
    [Id] = build_image("run-image"),
    illithid_cli:main_(["run", Id]),
    [Msg0, Msg1, Msg2, Msg3] = receive_messages("run-image"),
    [?_assertTextEqual(
        "moduli\n", Msg0),
     ?_assertTextEqual(
        "ssh_config\n", Msg1),
     ?_assertTextEqual(
        "sshd_config\n", Msg2),
     ?_assertTextEqual(
        "Container exited with status 0\n", Msg3)
    ].


build_image(Name) ->
    {ok, Root} = file:get_cwd(),
    Path = "/apps/illithid_cli/test/illithid_cli_SUITE_data",
    illithid_cli:main_(["build", Root ++ Path]),
    [_Msg1, Msg2] = receive_messages(Name),
    [_, Id] = string:split(Msg2, "Image id: "),
    lists:droplast(Id).


receive_messages(Name) ->
    receive_messages_([], 0, Name).

receive_messages_(Messages, Counter, Name) ->
    receive
        done ->
            ?LOG("done!~n", []),
            lists:reverse(Messages);

        Message ->
            ?LOG("~p climsg ~p:~p", [Name, Counter, Message]),
            receive_messages_([Message | Messages], Counter + 1, Name)
    end.
-endif.
