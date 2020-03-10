-module(illithid_cli).

-export([main/1]).
-ifdef(TEST).
-export([main_/1]).
-endif.

-define(CREATE_SHORTCUT(SHORTCUT, ORIGIN), main_(SHORTCUT) -> main_(ORIGIN)).
-define(CREATE_COMMAND(Input), main_(Input) -> illithid_cli_engine_client:command(Input), ok).


-include_lib("include/illithid.hrl").

main(Cmd) ->
    register(cli_process, self()),
    case illithid_cli_engine_client:start_link() of
        {ok, Pid} ->
            unlink(Pid),
            main_(Cmd),
            relay_messages();

        {error, eacces} ->
            io:format("Error! Insufficient permissions for connecting to the illithid daemon socket at ~s~n", [?API_SOCKET]),
            io:format("Do you have the right privileges?~n");

        {error, enoent} ->
            io:format("The socket at ~s does not seem to exist~n", [?API_SOCKET]),
            io:format("Is the illithid daemon running?~n");

        UnknownReturn ->
            io:format("Unknown error occured: ~p~n", [UnknownReturn])
    end.


?CREATE_COMMAND(["image", "build", "-t", NameTagRaw, Path]);
?CREATE_COMMAND(["image", "ls"]);
?CREATE_COMMAND(["container", "run", Image | Command]);
?CREATE_COMMAND(["container", "ls"]);
?CREATE_COMMAND(["container", "ls", "--all"]);
?CREATE_COMMAND(["clear", "zroot"]);


?CREATE_SHORTCUT(
   ["run", Image | Command],
   ["container", "run", Image | Command]);
?CREATE_SHORTCUT(
   ["images"],
   ["image", "ls"]);
?CREATE_SHORTCUT(
   ["build", "-t", NameTagRaw, Path],
   ["image", "build", "-t", NameTagRaw, Path]);
?CREATE_SHORTCUT(
   ["build", Path],
   ["build", "-t", "", Path]);


main_(["--help"]) ->
    io:format("I am sorry but there is no help available at the moment :(~n"),
    self() ! done;

main_(Args) ->
    io:format("illithid: '~s' is not a illithid command.~n", [string:join(Args, " ")]),
    io:format("See 'illithid --help'~n"),
    self() ! done.


relay_messages() ->
    receive
        done ->
            ok;

        {cli_msg, Msg} ->
            io:format(Msg),
            relay_messages();

        Msg ->
            io:format("Warning! Unknown message received from backend: ~p~n", [Msg])

    end.
