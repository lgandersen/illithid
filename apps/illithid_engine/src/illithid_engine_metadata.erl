%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2020, lga
%%% @doc
%%%
%%% @end
%%% Created : 2020-01-24 16:46:16.962994
%%%-------------------------------------------------------------------
-module(illithid_engine_metadata).

-behaviour(gen_server).

-include_lib("include/illithid.hrl").

%% API
-export([start_link/0,
         add_image/1,
         get_image/1,
         list_images/0,
         add_container/1,
         clear_all/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


add_container(Container) ->
    mnesia:transaction(fun() -> mnesia:write(Container) end).


add_image(Image) ->
    mnesia:transaction(fun() -> mnesia:write(Image) end).


get_image(ImageId) ->
    {atomic, [Image]} = mnesia:transaction(fun() -> mnesia:read(image, ImageId) end),
    Image.


list_images() ->
    ImagesAll = mnesia:dirty_match_object({image, '_', '_', '_', '_', '_', '_'}),
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
    Images.


clear_all() ->
    {atomic, ok} = mnesia:clear_table(image),
    add_image(?BASE_IMAGE),
    {atomic, ok} = mnesia:clear_table(container).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    application:set_env(mnesia, dir, "/" ++ ?ZROOT),
    mnesia:create_schema([]),
    application:start(mnesia),
    mnesia:create_table(container,
        [{attributes, record_info(fields, container)},
        {index, [#container.name]},
        {disc_copies, []}]),
    mnesia:create_table(image,
        [{attributes, record_info(fields, image)},
        {index, [#image.name]},
        {disc_copies, []}]),
    add_image(?BASE_IMAGE),
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    application:stop(mnesia),
    ok.


code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

starting_mnesia_test() ->
    illithid_engine_zfs:clear_zroot(),
    ?assertMatch({ok, _Pid}, illithid_engine_metadata:start_link()).


add_container_test() ->
    Container = #container { id = "test_id", name = "container for testing"},
    ?assertEqual({atomic, ok}, add_container(Container)).


add_and_get_image_test() ->
    Image = #image {
               id      = "lolololololooooooooooooooool",
               tag     = "test:oldest",
               created = erlang:timestamp()
              },
    ?assertEqual({atomic, ok}, add_image(Image)),
    ?assertEqual(Image, get_image(Image#image.id)).


get_ordered_images_test() ->
    Image = #image {
               id      = "leleleleleleeeeee",
               tag     = "test:latest",
               created = erlang:timestamp()
              },
    ?assertEqual({atomic, ok}, add_image(Image)),
    Images = list_images(),
    ?assertMatch([Image, #image{ tag = "test:oldest"}], Images).


close_mnesia_test() ->
    clear_all(),
    ?assertMatch({error, {already_started, _Pid}}, illithid_engine_metadata:start_link()),
    {error, {already_started, Pid}} = illithid_engine_metadata:start_link(),
    ?assertEqual(ok, gen_server:stop(Pid)).

-endif.
