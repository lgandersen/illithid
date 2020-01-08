-module(illithid_cli).

-export([main/1]).

-include_lib("include/illithid.hrl").


main(["images"]) ->
    Images = send_to_backend(list_images),
    io:format("REPOSITORY    TAG               IMAGE ID         CREATED               SIZE\n"),
    print_images(Images),
    ok;

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


print_images([#image { id = Id, tag = Tag, created = Created } | Rest]) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(Created),
    %% "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00" => "2019-09-19T15:07:03.0+00:00"
    Datetime = io_lib:format("~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                             [Year, Month, Day, Hour, Min, Sec]),
    io:format("n/a           ~s       ~s     ~s   n/a MB~n",
              [cell(Tag, 11), cell(Id, 12), cell(Datetime, 11)]),

    print_images(Rest);

print_images([]) ->
    ok.


cell(Content, Size) ->
    case length(Content) =< Size of
        true ->
            Content ++ string:copies(" ", Size - length(Content));

        false ->
            string:sub_string(Content, 1, Size)
    end.


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
