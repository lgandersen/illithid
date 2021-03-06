-define(ZROOT, "zroot/mindflayer_dev").
-define(ZROOT(Name), ?ZROOT ++ "/" ++ Name).
-define(BASE_LAYER_LOCATION, "zroot/mindflayer_basejail@image"). % Default image-snap to use when cloning: '@image'
-define(BASE_LAYER_PATH, "/zroot/mindflayer_basejail").

-define(API_SOCKET, "/var/run/illithid.sock").
-define(IP_RANGE, {{10,13,37,1}, {10,13,37,255}}).
-define(INET_IF, "illithid0").

-record(layer, {
         %%% When 'id' i set to 'base' it refers to dataset ?BASE_LAYER (is referenced in Dockerfiles with "FROM scratch")
         id        = none,
         parent_id = none,
         dataset   = none,
         location  = none, % zfs snapshot of the particular layer
         path      = none
         }).


-record(container, {
          id           = none,
          name         = none,
          running      = false,
          pid          = none, % Erlang proces id of the gen_server
          command      = none,
          layer        = none,
          ip           = none,
          image_id     = none,
          parameters   = [],
          created      = none
          }).


-record(image, {
          id      = none,
          name    = none,
          tag     = none,
          layers  = none,
          command = none,
          user    = "root",
          created = none
         }).


-define(BASE_LAYER, #layer {id = "base", location = ?BASE_LAYER_LOCATION, path = ?BASE_LAYER_PATH }).
-define(BASE_IMAGE, #image {id = "base", tag = "base", layers = [?BASE_LAYER]}).


-define(TEST_LOG_LEVEL, info).
-define(LOG(Msg, Args), lager:info(Msg, Args)).
-define(LOG(Msg), ?LOG(Msg, [])).
