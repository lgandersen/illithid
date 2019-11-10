-define(ZROOT, "zroot/mindflayer_dev").
-define(BASEJAIL_IMAGE, "zroot/mindflayer_dev/basejail@image").

-record(jail, {
          name        = none,
          jid         = none,
          path        = none,
          image       = none,
          zfs_dataset = none,
          command     = none,
          command_args = none,
          parameters   = []
          }).


-record(image, {
         name        = none,
         destination = none, %dataset@freeze
         parent      = none  %dataset@freeze
         }).
