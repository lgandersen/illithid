-module(mindflayer_utils).

-export([exec/1]).

exec(Cmd) ->
    io:format(user, "DEBUG CMD:~s~n", [Cmd]),
    Output = os:cmd(Cmd),
    io:format(user, "DEBUG OUTPUT:~s~n", [Output]),
    Output.
