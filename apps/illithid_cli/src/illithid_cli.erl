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
    {ok, Pid} = illithid_cli_engine_client:start_link(),
    unlink(Pid),
    main_(Cmd),
    relay_messages().


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

main_(Args) ->
    io:format("Unkown command: ~p~n", [Args]).


relay_messages() ->
    receive
        done ->
            ok;

        Msg ->
            io:format(Msg),
            relay_messages()
    end.
