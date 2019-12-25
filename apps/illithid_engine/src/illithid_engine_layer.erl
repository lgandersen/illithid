%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%%
%%% @end
%%% Created : 2019-11-16 14:11:22.836399
%%%-------------------------------------------------------------------
-module(illithid_engine_layer).

-behaviour(gen_server).

-include_lib("include/illithid.hrl").

%% API
-export([start_link/0,
        create_layer/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { counter }).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


create_layer(Context, Instruction, ParentLayerId) ->
    gen_server:call(?SERVER, {create_layer, {Context, Instruction, ParentLayerId}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(layer_table, [protected, named_table, {keypos, 2}]),
    ets:insert(layer_table, [?BASE_LAYER]),
    {ok, #state { counter = 0 }}.


handle_call({create_layer, {Context, Instruction, ParentLayerId}}, _From, #state { counter = N } = State) ->
    {ok, Dataset} = initialize_layer(ParentLayerId, N),

    case Instruction of
        {run, Cmd, CmdArgs} ->
            Jail = #jail{
                          path         = "/" ++ Dataset, %% Relying on mountpoint and dataset structure are equal
                          parameters   = ["mount.devfs", "ip4.addr=10.13.37.3"],
                          command      = Cmd,
                          command_args = CmdArgs
                         },
            {ok, {exit_status, 0}} = illithid_engine_jail:start_and_finish_jail([Jail]);


        {copy, SrcAndDest} ->
            copy_files(Context, "/" ++ Dataset, SrcAndDest) %TODO Dataset should be a mountpoint instead
    end,

    {ok, DigestId, DatasetNew} = finalize_layer(Dataset),

    Layer = #layer {
               id        = DigestId,
               parent_id = ParentLayerId,
               location  = DatasetNew ++ "@layer"
              },
    ets:insert(layer_table, [Layer]),
    {reply, {ok, Layer}, State#state { counter = N + 1 }};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-define(SnapBegin(Dataset), Dataset ++ "@layer_init").
-define(SnapEnd(Dataset), Dataset ++ "@layer").

initialize_layer(ParentLayerId, N) ->
    Dataset = ?ZROOT ++ "/" ++ "layer_build_" ++ erlang:integer_to_list(N),
    ParentLocation = fetch_location(ParentLayerId),
    0 = illithid_engine_zfs:clone(ParentLocation, Dataset),
    illithid_engine_zfs:snapshot(?SnapBegin(Dataset)),
    {ok, Dataset}.


finalize_layer(Dataset) ->
    illithid_engine_zfs:snapshot(?SnapEnd(Dataset)),
    {ok, DigestId} = illithid_engine_zfs:fingerprint(?SnapBegin(Dataset), ?SnapEnd(Dataset)),
    DatasetNew = ?ZROOT ++ "/" ++ DigestId,
    illithid_engine_zfs:rename(Dataset, DatasetNew),
    {ok, DigestId, DatasetNew}.


fetch_location(LayerId) ->
    [#layer { location = Location }] = ets:lookup(layer_table, LayerId),
    Location.


copy_files(ContextRoot, JailRoot, SrcAndDest) ->
    true = lists:all(fun verify_depth/1, SrcAndDest),
    Dest = JailRoot ++ lists:last(SrcAndDest),
    SrcList = lists:map(fun(Src) -> ContextRoot ++ "/" ++ Src end, lists:droplast(SrcAndDest)),

    Port = open_port({spawn_executable, "/bin/cp"}, [exit_status, {line, 1024}, {args, lists:reverse([Dest | SrcList])}]),
    receive
        {Port, {exit_status, N}} ->
            io:format(user, "LEL ~p~n", [N]);

        {Port, Other} ->
            io:format(user, "LOL ~p~n", [Other])
    end.


verify_depth(Path) ->
    case verify_depth_(string:tokens(Path, "/"), 0) of
        ok ->
            true;

        below_root ->
            false
    end.


verify_depth_([".." | _Rest], 0) ->
    below_root;

verify_depth_([".." | Rest], Count) ->
    verify_depth_(Rest, Count - 1);

verify_depth_([_Dir | Rest], Count) ->
    verify_depth_(Rest, Count + 1);

verify_depth_([], _Count) ->
    ok.
