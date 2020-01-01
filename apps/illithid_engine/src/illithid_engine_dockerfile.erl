-module(illithid_engine_dockerfile).

-export([
         tokenize/1,
         parse/1
        ]).

parse(FileName) when is_list(FileName) ->
    {ok, FileRaw} = file:read_file(FileName),
    parse(FileRaw);


parse(FileRaw) when is_binary(FileRaw) ->
    {ok, Tokens} = tokenize(FileRaw),
    ok = starts_with_from_instruction(Tokens),
    lists:map(fun parse_/1, Tokens).


parse_({run, <<"[", CmdAsExec/binary>>}) ->
    %% Should support both forms of RUN according to docker docs

    true = binary:last(<<"]">>) =:= binary:last(CmdAsExec),

    CmdsBin = binary:replace(
             binary:replace(CmdAsExec, <<"]">>, <<"">>),
             <<"\"">>, <<"">>, [global]
            ),
    [Cmd | CmdArgs] = string:tokens(
             binary:bin_to_list(CmdsBin),
             ","),
    {run, Cmd, lists:map(fun string:strip/1, CmdArgs)};

parse_({run, CmdAsShell}) ->
    Cmd = "/bin/sh",
    CmdArgs = ["-c", binary:bin_to_list(CmdAsShell)],
    {run, Cmd, CmdArgs};

parse_({copy, <<"[", ArgsAsList/binary>>}) ->
    %% COPY ["<src>",... "<dest>"]
    %% TODO fix [--chown=<user>:<group>] and [--from=<name|index> optional parameters
    true = binary:last(<<"]">>) =:= binary:last(ArgsAsList),
    CmdsBin = binary:replace(ArgsAsList, <<"]">>, <<"">>),
    Cmds = binary:bin_to_list(CmdsBin),
    CmdList = string:tokens(Cmds, ","),
    CmdListClean = lists:map(
                     fun(Cmd) ->
                             string:strip(string:strip(Cmd), both, $")
                     end, CmdList),
    {copy, CmdListClean};

parse_({copy, ArgsBin}) ->
    %% COPY <src>... <dest>
    %% TODO fix [--chown=<user>:<group>] and [--from=<name|index> optional parameters
    Args = binary:bin_to_list(ArgsBin),
    ArgList = string:tokens(Args, " "),
    {copy, ArgList};


parse_({from, Args}) ->
    lager:info("Parser: ~p", [{from, Args}]),
    case binary:split(Args, <<" AS ">>, [global]) of
        [<<"scratch">>] ->
            {from, base};
        [Image] ->
            {from, binary:bin_to_list(Image)};
        [Image, Name] ->
            {from, binary:bin_to_list(Image), binary:bin_to_list(Name)}
    end;

parse_({expose, Port}) ->
    {expose, binary_to_integer(Port)};

parse_(Token) ->
    Token.


tokenize(FileRaw) ->
    %% Remove escaped newlines
    File = binary:replace(FileRaw, <<"\\\n">>, <<"">>, [global]),

    AllLines = binary:split(File, <<"\n">>, [global]),
    Lines = lists:filtermap(fun remove_blanks_and_comments/1, AllLines),
    Tokens = lists:map(fun tokenize_line/1, Lines),
    {ok, Tokens}.


remove_blanks_and_comments(<<"">>) ->
    false;

remove_blanks_and_comments(<<"#", _Comment/binary>>) ->
    false;

remove_blanks_and_comments(_Line) ->
    true.


starts_with_from_instruction([{arg, _} | Rest]) ->
    starts_with_from_instruction(Rest);

starts_with_from_instruction([{from, _} | _Rest]) ->
    ok.


tokenize_line(Line) ->
    [InstructionRaw, Args] = binary:split(Line, <<" ">>),
    Instruction = case string:uppercase(binary:bin_to_list(InstructionRaw)) of
        "FROM"   -> from;
        "ARG"    -> arg;
        "EXPOSE" -> expose;
        "VOLUME" -> volume;
        "CMD"    -> cmd;
        "USER"   -> user;
        "RUN"    -> run;
        "COPY"   -> copy;
        UnkownInstruction ->
            lager:warning("WARNING: Instruction '~p' not understood~n", [UnkownInstruction]),
            {unparsed, UnkownInstruction}
    end,
    lager:info("tokenize:~p", [{Instruction, Args}]),
    {Instruction, Args}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

initialize() ->
    ok.

stop(_) ->
    ok.

instructions_test_() ->
     {foreach, fun initialize/0, fun stop/1, [
                                  fun test_general_tokenization/1,
                                  fun test_from_instruction/1,
                                  fun test_run_instruction_/1,
                                  fun test_copy_instruction/1,
                                  fun test_expose_instruction/1]
     }.

test_general_tokenization(_) ->
    [?_assertMatch(_ParsedTokens, parse("./apps/illithid_engine/test/eunit_data/Dockerfile"))].


test_from_instruction(_) ->
    FileRaw1 = <<"# Testing\nFROM lol\n# One more comment">>,
    FileRaw2 = <<"# Testing\nFROM lol AS maxlol\n# One more comment">>,
    ?_assertEqual([{from, "lol"}], parse(FileRaw1)),
    ?_assertEqual([{from, "lol", "maxlol"}], parse(FileRaw2)).


test_run_instruction_(_) ->
    FileRaw1 = <<"# Testing\nFROM lol\nRUN cat lol.txt">>,
    FileRaw2 = <<"# Testing\nFROM lol\nRUN [\"/bin/sh\", \"-c\", \"cat lol.txt\"]">>,
    [?_assertEqual([{from, "lol"}, {run, "/bin/sh", ["-c", "cat lol.txt"]}], parse(FileRaw1)),
     ?_assertEqual([{from, "lol"}, {run, "/bin/sh", ["-c", "cat lol.txt"]}], parse(FileRaw2))].


test_expose_instruction(_) ->
    FileRaw = <<"# Testing\nFROM lol\nEXPOSE 1337">>,
    ?_assertEqual([{from, "lol"}, {expose, 1337}], parse(FileRaw)).


test_copy_instruction(_) ->
    FileRaw = <<"# Testing\nFROM lol\nCOPY [\"lol1\", \"lol2\", \"lol3\"]">>,
    FileRaw2 = <<"# Testing\nFROM lol\nCOPY lol1 lol2 lol3">>,
    [?_assertEqual([{from, "lol"}, {copy, ["lol1", "lol2", "lol3"]}], parse(FileRaw)),
    ?_assertEqual([{from, "lol"}, {copy, ["lol1", "lol2", "lol3"]}], parse(FileRaw2))].
-endif.
