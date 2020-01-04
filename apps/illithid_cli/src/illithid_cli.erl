-module(illithid_cli).

-export([main/1]).

-include_lib("include/illithid.hrl").

main(["clear", "all"]) ->
    ok = send_to_backend(clear_zroot),
    io:format("ZRoot cleared!~n"),
    ok;

main(["build", Path]) ->
    {ok, Image} = send_to_backend({build, Path ++ "/Dockerfile"}),
    io:format("Succesfully built image from ~p~n", [Path]),
    io:format("Image id: ~s~n", [Image#image.id]),
    ok;

main(Args) ->
    io:format("Unkown command: ~p~n", [Args]).


send_to_backend(Msg) ->
    MsgBin = erlang:term_to_binary(Msg),
    case gen_tcp:connect({local, ?API_SOCKET}, 0, [binary, {packet, raw}, {active, true}]) of
        {ok, Socket} ->
            receive_reply(
              gen_tcp:send(Socket, MsgBin)
             );

        {error, Reason} ->
            io:format("Connecting to backend failed with reason: ~p~n", [Reason]),
            {error, Reason}
    end.


receive_reply(ok) ->
    receive
        {tcp, _Socket, Reply} ->
            erlang:binary_to_term(Reply);

        Error ->
            io:format("Error receiving reply from illithid-engine: ~p ~n" , [Error]),
            {error, Error}
    end;

receive_reply({error, Reason}) ->
    io:format("Sending command to backend failed: ~p~n", [Reason]),
    error.
