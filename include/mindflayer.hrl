-record(jail, {
          name        = none,
          jid         = none,
          path        = none,
          zfs_dataset = none,
          command     = none,
          command_args = none,
          port        = none,
          parameters   = []
          }).


-record(image, {
         name        = none,
         destination = none, %dataset@freeze
         parent      = none  %dataset@freeze
         }).
