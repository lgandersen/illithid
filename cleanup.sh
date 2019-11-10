#/bin/sh
sudo rebar3 escriptize
sudo ./_build/default/bin/mindflayer clean
zfs list | grep mindflayer
zfs list -t snapshot | grep mindflayer
