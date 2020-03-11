%%% Templates and strings used for CLI output
-define(LIST_IMAGES_HEADER, "REPOSITORY    TAG         IMAGE ID       CREATED     SIZE\n").
-define(LIST_CONTAINERS_HEADER, "CONTAINER ID   IMAGE                       COMMAND                   CREATED               STATUS    PORTS   NAMES\n").
-define(ZROOT_CLEARED, "Illithid zroot cleared.\n").

-define(IMAGE_BUILD_ERROR_MSG_1(Path),
        io_lib:format("illithid-engine unable to open Dockerfile ~s~n", [Path])).
-define(IMAGE_BUILD_ERROR_MSG_2(Reason),
        io_lib:format("reason: ~p~n", [Reason])).
