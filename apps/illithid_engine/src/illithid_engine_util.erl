-module(illithid_engine_util).

-export([uuid/0]).

uuid() ->
    Input = erlang:pid_to_list(self()) ++ erlang:binary_to_list(erlang:term_to_binary(erlang:timestamp())),
    binary_to_list(hex_digest(Input)).

hex_digest(Binary) ->
    << << if N >= 10 -> N -10 + $a;
             true    -> N     + $0 end >>
       || <<N:4>> <= crypto:hash(sha256, Binary) >>.
