%%%-------------------------------------------------------------------
%%% @author lga
%%% @copyright (C) 2020, lga
%%% @doc
%%%
%%% @end
%%% Created : 2020-01-04 13:54:36.702587
%%%-------------------------------------------------------------------
-module(illithid_cli_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         t_clear_all/1,
         t_build_simple_image/1
        ]).

-include_lib("include/illithid.hrl").
-include_lib("common_test/include/ct.hrl").


all() ->
    [
     %% TODO: Group names here e.g. {group, crud}
     t_clear_all,
     t_build_simple_image
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
        %% TODO: group definitions here e.g.
        %% {crud, [], [
        %%          t_create_resource,
        %%          t_read_resource,
        %%          t_update_resource,
        %%          t_delete_resource
        %%         ]}

    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    illithid_engine_zfs:clear_zroot(),
    lager:start(),
    Config.

end_per_suite(_Config) ->
    %illithid_engine_zfs:clear_zroot(),
    ok.


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->

    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    ok = application:start(illithid_engine),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = application:stop(illithid_engine),
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
t_clear_all(_Config) ->
    0 = illithid_engine_zfs:create(?ZROOT("test_cli_clear_all")),
    Msg = os:cmd("../../../default/bin/illithid clear all"),
    lager:info("Output from running illithid-cli: ~p", [Msg]),
    0 = illithid_engine_zfs:create(?ZROOT("test_cli_clear_all")),
    ok.


t_build_simple_image(Config) ->
    Path = ?config(data_dir, Config),
    Msg = os:cmd("../../../default/bin/illithid build " ++ Path),
    lager:info("Output from running illithid-cli: ~p", [Msg]),
    [_, Id] = string:split(Msg, "Image id: "),
    timer:sleep(1000),
    {ok, <<"lol\n">>} = file:read_file("/" ++ ?ZROOT ++ "/" ++ lists:droplast(Id) ++ "/root/test.txt"),
    ok.
