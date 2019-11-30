-module(mindflayer_dockerfile).

-export([parse/1]).


parse(FileName) ->
    {ok, File} = file:read_file(FileName),
    Lines = binary:split(File, <<"\n">>, [global]),
    parse_(Lines, []),
    ok.


parse_([<<"">> | Rest], Tokens) ->
    parse_(Rest, Tokens);

parse_([<<"#", Comment/binary>> | Rest], Tokens) ->
    log_and_proceed({comment, Comment}, Tokens, Rest);

parse_([<<"FROM", Args/binary>> | Rest], Tokens) ->
    log_and_proceed({from, Args}, Tokens, Rest);

parse_([<<"EXPOSE", Args/binary>> | Rest], Tokens) ->
    log_and_proceed({expose, Args}, Tokens, Rest);

parse_([<<"VOLUME", Args/binary>> | Rest], Tokens) ->
    log_and_proceed({volume, Args}, Tokens, Rest);

parse_([<<"CMD", Args/binary>> | Rest], Tokens) ->
    log_and_proceed({cmd, Args}, Tokens, Rest);

parse_([<<"USER", Args/binary>> | Rest], Tokens) ->
    log_and_proceed({user, Args}, Tokens, Rest);

parse_([<<"RUN", Command/binary>> = Line, NextLine | Rest], Tokens) ->
    case binary:part(Command,  size(Command) - 1, 1) of
        <<"\\">> ->
            NewLine = binary:part(Line, 0, size(Line) - 1),
            parse_([<<NewLine/binary, NextLine/binary>> | Rest], Tokens);

        _ ->
            log_and_proceed({run, Command}, Tokens, Rest)
    end;

parse_([Line | Rest], Tokens) ->
    log_and_proceed({unparsed, Line}, Tokens, Rest);

parse_([], Tokens) ->
    {ok, Tokens}.


log_and_proceed(Token, Tokens, Rest) ->
    io:format(user, "~p~n", [Token]),
    parse_(Rest, [Token | Tokens]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
parse_test() ->
    ok = parse("./test/eunit_data/Dockerfile").
-endif.
