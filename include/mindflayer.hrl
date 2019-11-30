-define(ZROOT, "zroot/mindflayer_dev").
-define(BASEJAIL_IMAGE, "zroot/mindflayer_basejail"). %default image-snap to use when cloning: '@image'

-record(jail, {
          name         = none,
          jid          = none,
          pid          = none,
          path         = none,
          image        = none,
          zfs_dataset  = none,
          command      = none,
          command_args = none,
          parameters   = []
          }).


-record(image, {
         tag         = none,
         destination = none, %dataset
         parent      = none  %dataset
         }).
