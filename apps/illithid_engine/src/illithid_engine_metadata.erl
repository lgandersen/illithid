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
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0,
         add_image/1,
         get_image/1,
         list_images/0,
         add_container/1,
         list_containers/0,
         list_running_containers/0,
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


list_running_containers() ->
    MatchSpec = ets:fun2ms(
                  fun(#container{ running = true } = Container) ->
                          Container
                  end),
    {atomic, Containers} = mnesia:transaction(fun() -> mnesia:select(container, MatchSpec, read) end),
    sort_containers(Containers).


list_containers() ->
    MatchSpec = ets:fun2ms(
                  fun(#container{} = Container) ->
                          Container
                  end),
    {atomic, Containers} = mnesia:transaction(fun() -> mnesia:select(container, MatchSpec, read) end),
    sort_containers(Containers).


add_image(Image) ->
    mnesia:transaction(fun add_image_/1, [Image]).


get_image(ImageId) ->
    {atomic, Result} = mnesia:transaction(fun get_image_/1, [ImageId]),
    Result.


list_images() ->
    MatchSpec = ets:fun2ms(
                  fun(#image{ id = Id } = Image) when Id =/= base ->
                          Image
                  end),
    {atomic, Images} = mnesia:transaction(fun() -> mnesia:select(image, MatchSpec, read) end),
    sort_images(Images).


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
sort_containers(Containers) ->
    lists:sort(
      fun(#container { created = A }, #container { created = B }) ->
              A >= B
      end, Containers).


sort_images(Images) ->
    lists:sort(
               fun(#image { created = A }, #image { created = B }) ->
                       A >= B
               end, Images).


add_image_(#image { name = Name, tag = Tag } = Image) ->
    Match = ets:fun2ms(
              fun(#image { name = ImgName, tag = ImgTag } = Img)
                    when ImgName =:= Name, ImgTag =:= Tag ->
                      Img
              end),

    case mnesia:select(image, Match) of
        [ExistingImage] ->
            mnesia:write(ExistingImage#image { name = none, tag = none });
        [] ->
            ok
    end,
    mnesia:write(Image).


get_image_("base") ->
    ?BASE_IMAGE;

get_image_(IdOrTag) ->
    case mnesia:read(image, IdOrTag) of
        [Image] ->
            Image;

        [] ->
            %% The identifier was not an Id. Try looking for tag instead
            {Name, Tag} = illithid_engine_util:decode_tagname(IdOrTag),
            Match = ets:fun2ms(
                      fun(#image { name = ImgName, tag = ImgTag } = Img)
                            when ImgName =:= Name, ImgTag =:= Tag ->
                              Img
                      end),

            case mnesia:select(image, Match) of
                [Image] ->
                    Image;

                [] ->
                    not_found
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

starting_mnesia_test() ->
    illithid_engine_zfs:clear_zroot(),
    ?assertMatch({ok, _Pid}, illithid_engine_metadata:start_link()).


add_container_test() ->
    Container = #container { id = "1337", name = "testing-1", created = erlang:timestamp()},
    ?assertEqual({atomic, ok}, add_container(Container)).


list_containers_test() ->
    add_container(#container { id = "1338", name = "testing-2", created = erlang:timestamp()}),
    add_container(#container { id = "1339", name = "testing-3", created = erlang:timestamp()}),
    Containers = list_containers(),
    ?assertMatch([#container { id = "1339"},
                  #container { id = "1338"},
                  #container { id = "1337"}], Containers).


list_running_containers_test() ->
    add_container(#container { id = "1340", name = "testing-2", running = true, created = erlang:timestamp()}),
    Containers = list_running_containers(),
    ?assertMatch([#container { id = "1340"}], Containers).


add_and_get_image_test() ->
    Image1 = #image {
               id      = "lolololololooooooooooooooool",
               name    = "test",
               tag     = "oldest",
               created = erlang:timestamp()
              },
    ?assertEqual({atomic, ok}, add_image(Image1)),
    Image2 = #image {
               id      = "leleleleleleeeeee",
               name    = "test",
               tag     = "latest",
               created = erlang:timestamp()
              },
    ?assertEqual({atomic, ok}, add_image(Image2)),
    ?assertEqual(Image1, get_image(Image1#image.id)),
    ?assertEqual(Image1, get_image("test:oldest")),
    ?assertEqual(Image2, get_image("test")).


list_images_test() ->
    Images = list_images(),
    ?assertMatch([_Image, #image{ tag = "oldest"}], Images).


reset_tag_if_new_with_same_is_inserted_test() ->
    Image = #image {
               id      = "originalimage",
               name    = "testing",
               tag     = "latest",
               created = erlang:timestamp()
              },
    ?assertEqual({atomic, ok}, add_image(Image)),
    ImageOverwrite = Image#image { id = "newimage", created = erlang:timestamp()},
    ?assertEqual({atomic, ok}, add_image(ImageOverwrite)),
    Images = list_images(),
    ?assertMatch([#image { id = "newimage" }, #image{ id = "originalimage", name = none, tag = none} | _], Images).


close_mnesia_test() ->
    clear_all(),
    ?assertMatch({error, {already_started, _Pid}}, illithid_engine_metadata:start_link()),
    {error, {already_started, Pid}} = illithid_engine_metadata:start_link(),
    ?assertEqual(ok, gen_server:stop(Pid)).

-endif.
