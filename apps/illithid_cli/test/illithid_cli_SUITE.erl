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
         t_build_simple_image/1,
         t_list_image/1
        ]).

-include_lib("include/illithid.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [
     %% TODO: Group names here e.g. {group, crud}
     t_clear_all,
     t_build_simple_image,
     t_list_image
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
    lager:start(),
    Config.

end_per_suite(_Config) ->
    illithid_engine_zfs:clear_zroot(),
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
    illithid_engine_zfs:clear_zroot(),
    ok = application:start(illithid_engine),
    illithid_engine_metadata:clear_all(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = application:stop(illithid_engine),
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
t_clear_all(_Config) ->
    0 = illithid_engine_zfs:create(?ZROOT("test_cli_clear_all")),
    run_cli_command("illithid clear all"),
    0 = illithid_engine_zfs:create(?ZROOT("test_cli_clear_all")),
    ok.


t_build_simple_image(Config) ->
    Path = ?config(data_dir, Config),
    Msg = run_cli_command("illithid build " ++ Path),
    ?assertMatch([_, _Id], string:split(Msg, "Image id: ")),
    [_, Id] = string:split(Msg, "Image id: "),
    timer:sleep(1000),
    {ok, <<"lol\n">>} = file:read_file("/" ++ ?ZROOT ++ "/" ++ lists:droplast(Id) ++ "/root/test.txt"),
    ok.


t_list_image(_Config) ->
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
    ImageList = run_cli_command("illithid images"),
    [_ | Output ] = string:tokens(ImageList, "\n"),
    ExpectedOutput = [
        "n/a           test:latest       lolololololo     2020-01-06 17:04:24   n/a MB",
        "n/a           test:oldest       lelelelelele     2020-01-06 17:03:20   n/a MB"
                     ],
    ?assertEqual(ExpectedOutput, Output),
    ok.


t_run_image(Config) ->
    %%FIXME: need to support tagging to be able to make a proper cli-test.
    Path = ?config(data_dir, Config),
    run_cli_command("illithid build " ++ Path),
    Msg = run_cli_command("illithid build " ++ Path),
    ok.


print_many_lines(Text) ->
    Lines = string:split(Text, "\n"),
    lists:map(fun(Line) -> lager:info(Line) end, Lines),
    ok.


run_cli_command(Cmd) ->
    Output = os:cmd("../../../default/bin/" ++ Cmd),
    lager:info("Output from running illithid-cli:"),
    ok = print_many_lines(Output),
    Output.
