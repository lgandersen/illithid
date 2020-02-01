-module(illithid_cli).

-export([main/1]).

-include_lib("include/illithid.hrl").

main(Cmd) ->
    illithid_cli_engine_client:start_link(),
    main_(Cmd),
    receive
        done -> ok
    end.

main_(["images"]) ->
    illithid_cli_engine_client:command(list_images),
    ok;

main_(["clear", "all"]) ->
    illithid_cli_engine_client:command(clear_zroot),
    ok;

main_(["build", Path]) ->
    illithid_cli_engine_client:command({build, Path ++ "/Dockerfile", {none, none}}),
    ok;

main_(["build", "-t", NameTagRaw, Path]) ->
    %%TODO NameTag should be probably be sanity-checked and converted to {Name, Tag}
    {_Name, _Tag} = NameTag = parse_nametag(NameTagRaw),
    illithid_cli_engine_client:command({build, Path ++ "/Dockerfile", NameTag}),
    ok;

main_(["run", Image]) ->
    %% FIXME: This it not implemented. Moved the 'parse_image' functionality into illithid_engine_metadata
    illithid_cli_engine_client:command({run, parse_image(Image)}),
    ok;


main_(Args) ->
    io:format("Unkown command: ~p~n", [Args]).


parse_image("base") ->
    base;

parse_image(Image) ->
    %% Image can be a ImageId or a tag
    case string:split(Image, ":") of
        [Name,Tag] ->
            {tag, Name, Tag};

        NameId ->
            {name_or_id, NameId}
    end.


parse_nametag(NameTag) ->
    case string:split(NameTag, ":") of
        [Name,Tag] ->
            {Name, Tag};

        [_, _ | _] ->
            io:format("Unable to parse '-t <name>:<tag>' input.");

        Name ->
            {Name, "latest"}

    end.
