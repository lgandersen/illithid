-define(ZROOT, "zroot/mindflayer_dev").
-define(BASEJAIL_IMAGE, "zroot/mindflayer_basejail"). %default image-snap to use when cloning: '@image'

-record(jail, {
          jid          = none,
          pid          = none,
          path         = none,
          command      = none,
          command_args = none,
          parameters   = []
          }).


-record(image, {
         id             = none,
         dataset        = base, %dataset when base is used it refers to ?BASEJAIL_IMAGE (is referenced in Dockerfiles with "FROM scratch")
         parent_dataset = none  %dataset
         }).
