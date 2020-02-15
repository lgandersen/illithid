-module(illithid_engine_util).

-export([uuid/0,
         decode_tagname/1]).

decode_tagname(TagName) ->
    case string:split(TagName, ":") of
        [Name] ->
            {Name, "latest"};
        [Name, Tag] ->
            {Name, Tag}
    end.

uuid() ->
    InputAll = erlang:pid_to_list(self()) ++ erlang:binary_to_list(erlang:term_to_binary(erlang:timestamp())),
    Input = binary:part(hex_digest(InputAll), {0, 12}),
    binary_to_list(Input).

hex_digest(Binary) ->
    << << if N >= 10 -> N -10 + $a;
             true    -> N     + $0 end >>
       || <<N:4>> <= crypto:hash(sha256, Binary) >>.
