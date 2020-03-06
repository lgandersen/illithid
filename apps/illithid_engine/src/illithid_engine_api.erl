%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2020, lga
%%% @doc
%%%
%%% @end
%%% Created : 2020-01-02 13:39:30.213936
%%%-------------------------------------------------------------------
-module(illithid_engine_api).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
        listen/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("include/illithid.hrl").
-define(SERVER, ?MODULE).

-record(state, { listening_socket = none, socket = none, listener_pid = none }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    file:delete(?API_SOCKET),
    {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, raw}, {active, false}, {ip, {local, ?API_SOCKET}}]),
    Pid = spawn_link(illithid_engine_api, listen, [self(), LSocket]),
    {ok, #state{ listening_socket = LSocket, listener_pid = Pid }}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


%% From illithid_engine_container
handle_info({client_connection, Socket}, State) ->
    {noreply, State#state { socket = Socket}};

handle_info({container_msg, Pid, Msg}, #state { socket = Socket } = State) when is_pid(Pid) ->
    send_to_cli(Msg, Socket),
    {noreply, State};

handle_info({tcp, Socket, Cmd}, State) ->
    handle_command(erlang:binary_to_term(Cmd), Socket),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    gen_tcp:close(Socket),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("Unkown message received: ~p ~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state { listening_socket = LSocket }) ->
    ok = gen_tcp:close(LSocket),
    file:delete(?API_SOCKET),
    ok.


code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
listen(APIProces, LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            gen_tcp:controlling_process(Socket, APIProces),
            inet:setopts(Socket, [{active, true}]),
            APIProces ! {client_connection, Socket},
            listen(APIProces, LSocket);

        {error, Reason} ->
            lager:warning("API-servers crashed because ~p", [Reason]),
            ok = gen_tcp:close(LSocket),
            exit(normal)
    end.


handle_command(["container", "ls"], Socket) ->
    Containers = illithid_engine_metadata:list_containers(),
    send_to_cli(Containers, Socket);

handle_command(["container", "run", ImageIdentifier | Command], _Socket) ->
    Image = illithid_engine_metadata:get_image(ImageIdentifier),
    ImgOption = case Command of
        [] ->
            {image, Image};

        [_Cmd|_Args] ->
            {image, Image#image { command = Command }}
    end,
    {ok, Pid} = illithid_engine_container_pool:create([ImgOption]),
    illithid_engine_container:attach(Pid),
    illithid_engine_container:start(Pid);

handle_command(["clear", "zroot"], Socket) ->
    ok = illithid_engine_zfs:clear_zroot(),
    send_to_cli(ok, Socket);

handle_command(["image", "ls"], Socket) ->
    Images = illithid_engine_metadata:list_images(),
    send_to_cli(Images, Socket);

handle_command(["image", "build", "-t", NameTagRaw, Path], Socket) ->
    DockerFilePath = Path ++ "/Dockerfile",
    {Name, Tag} = parse_nametag(NameTagRaw),
    Instructions = illithid_engine_dockerfile:parse(DockerFilePath),
    {ok, Image} = illithid_engine_image:create_image(Instructions, DockerFilePath),
    illithid_engine_metadata:add_image(Image#image { name = Name, tag = Tag }),
    send_to_cli({ok, Image}, Socket);

handle_command(UnkownCommand, _Socket) ->
    lager:warning("Command ~p not understood.", [UnkownCommand]).


parse_nametag("") ->
    {none, none};

parse_nametag(NameTag) ->
    case string:split(NameTag, ":") of
        [Name,Tag] ->
            {Name, Tag};

        [_, _ | _] ->
            %%TODO Move this to cli-output
            io:format("Unable to parse '-t <name>:<tag>' input.");

        Name ->
            {Name, "latest"}

    end.


send_to_cli(Term, Socket) ->
    TermBin = erlang:term_to_binary(Term),
    ok = gen_tcp:send(Socket, TermBin),
    ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%% Most tests are in the illithid_cli app.
startup_test() ->
    {ok, Pid} = illithid_engine_api:start_link(),
    {ok, _Socket} = gen_tcp:connect({local, ?API_SOCKET}, 0, [binary, {packet, raw}, {active, true}]),
    Pid ! {'EXIT', self(), normal},
    {error, enoent} = file:open(?API_SOCKET, [raw]),
    ok.
-endif.
