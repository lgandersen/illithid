#/bin/sh
sudo rebar3 escriptize
sudo ./_build/default/bin/illithid clean
zfs list | grep mindflayer
zfs list -t snapshot | grep mindflayer
sudo rebar3 ct
