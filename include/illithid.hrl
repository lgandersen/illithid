-define(ZROOT, "zroot/mindflayer_dev").
-define(BASEJAIL_IMAGE_LOCATION, "zroot/mindflayer_basejail@image"). %default image-snap to use when cloning: '@image'

-record(jail, {
          jid          = none,
          pid          = none,
          path         = none,
          command      = none,
          command_args = none,
          parameters   = []
          }).


-record(layer, {
         id        = none, % when id i set to 'base' it refers to dataset ?BASEJAIL_IMAGE (is referenced in Dockerfiles with "FROM scratch")
         parent_id = none,
         location  = none % zfs snapshot of the particular layer
         }).

-record(image, {
          name   = none,
          tag    = none,
          layers = none,
          command= none
         }).
