%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2019, lga
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-24 12:39:08.920497
%%%-------------------------------------------------------------------
-module(illithid_engine_image).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("include/illithid.hrl").

%% API
-export([create_image/1, create_image/2,
         get_image/1,
         list_images/0,
         add_image/1]).

-record(build_state, {
          instructions = none,
          context      = none,
          caller       = none,
          image_record = none
         }).


-record(state, {
         image_table = image_table
         }).

add_image(Image) ->
    gen_server:cast(?SERVER, {add, Image}).


list_images() ->
    gen_server:call(?SERVER, list).


get_image(ImageId) ->
    gen_server:call(?SERVER, {get_image, ImageId}).


create_image(Instructions) ->
    create_image(Instructions, "./").

create_image(Instructions, ContextPath) ->
    ImageRecord = #image { tag = none, layers = [], command = none },
    BuildState = #build_state { instructions = Instructions, context = ContextPath, image_record = ImageRecord },
    gen_server:call(?SERVER, {create, BuildState}).


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(image_table, [protected, named_table, {keypos, 2}]),
    ets:insert(image_table, ?BASE_IMAGE),
    {ok, #state{}}.

handle_call({get_image, ImageId}, _From, State) ->
    Image = get_image_(ImageId),
    {reply, Image, State};


handle_call(list, _From, State) ->
    ImagesAll = ets:match_object(image_table, '$1'),
    ImagesUnordered = lists:filter(fun(Image) ->
                                  case Image#image.id of
                                      base -> false;
                                      _ -> true
                                  end
                          end, ImagesAll),
    Images = lists:reverse(lists:sort(
               fun(#image { created = A }, #image { created = B }) ->
                       A =< B
               end, ImagesUnordered)),
    {reply, Images, State};

handle_call({create, BuildState}, _From, State) ->
    {ok, Image} = Reply = proces_instructions(BuildState),
    ets:insert(image_table, Image),
    {reply, Reply, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({add, Image}, State) ->
    ets:insert(image_table, Image),
    {noreply, State};

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
proces_instructions(#build_state { instructions = [ {from, ImageId} | Rest ] } = State) ->
    NewState = State#build_state {
                  instructions = Rest,
                  image_record = get_image_(ImageId)
                 },
    proces_instructions(NewState);

proces_instructions(#build_state {
                       instructions = [ {run, Cmd} | Rest ],
                       image_record = #image { layers = Layers } = Image
                       } = State) ->

    {ok, Pid } = illithid_engine_container_pool:new(),
    #container { layer = #layer{ id = LayerId }} = illithid_engine_container:create(Pid, Image#image { command = Cmd}, []),
    {ok, {exit_status, _N}} = illithid_engine_container:run_sync(Pid),
    {ok, LayerUpd} = illithid_engine_layer:finalize_layer(LayerId),

    NewState = State#build_state {
                 instructions = Rest,
                 image_record = #image { layers = [ LayerUpd | Layers ] }
                                  },
    proces_instructions(NewState);

proces_instructions(#build_state {
                       instructions = [ {copy, SrcAndDest} | Rest ],
                       image_record = #image { layers = Layers } = Image,
                       context      = Context
                       } = State) ->
    {ok ,#layer { dataset = Dataset, id = LayerId }} = illithid_engine_layer:new(Image),
    copy_files(Context, "/" ++ Dataset, SrcAndDest), %% TODO Dataset should be a mountpoint instead
    {ok, LayerUpd} = illithid_engine_layer:finalize_layer(LayerId),
    NewState = State#build_state {
                 instructions = Rest,
                 image_record = #image {
                                   layers = [ LayerUpd | Layers ]
                                  }},
    proces_instructions(NewState);

proces_instructions(#build_state { instructions = [ {cmd, Args} | Rest ], image_record = Image } = State) ->
    NewState = State#build_state {
                  instructions = Rest,
                  image_record = Image#image { command = Args }
                 },
    proces_instructions(NewState);

proces_instructions(#build_state { instructions = [], image_record = #image { layers = [#layer { id = ImageId } | _Rest] } = Image }) ->
    {ok, Image#image {id = ImageId, created = erlang:timestamp() }}.


get_image_(ImageId) ->
    lager:info("Fetching image for id ~p", [ImageId]),
    [Image] = ets:lookup(image_table, ImageId),
    Image.


copy_files(ContextRoot, JailRoot, SrcAndDest) ->
    true = lists:all(fun verify_depth/1, SrcAndDest),
    Dest = JailRoot ++ lists:last(SrcAndDest),
    SrcList = lists:map(fun(Src) -> ContextRoot ++ "/" ++ Src end, lists:droplast(SrcAndDest)),

    Port = open_port({spawn_executable, "/bin/cp"}, [exit_status, {line, 1024}, {args, lists:reverse([Dest | SrcList])}]),
    receive
        {Port, {exit_status, N}} ->
            lager:info("copy operation ended with status: ~p", [N]);

        {Port, Other} ->
            lager:info("Received unkown message from copying port: ~p", [Other])
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
