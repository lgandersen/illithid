-module(mindflayer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("mindflayer.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([create_jail/1, create_jail_and_wait_on_finish/1, destroy_jail/1, create_image/1]).

-define(TEST_MF_JAIL_TESTJAIL, "unittest_jail_jail").
-define(TEST_MF_JAIL_DATASET, "zroot/mindflayer_dev/unittest_jail_jail").

all() -> [
          create_jail,
          create_jail_and_wait_on_finish,
          %destroy_jail,
          create_image
         ].


create_jail(Config) ->
    Jail = ?config(jail, Config),
    {ok, _Pid} = mindflayer_jail:start_jail([Jail#jail { command = "/bin/ls", command_args=["/"] }]),
    timer:sleep(1000), % Atm the call above is async which means that we'll have to wait for the gen_server to actually execute.
    mindflayer_jail:umount_devfs(Jail#jail.path),
    ok.


create_jail_and_wait_on_finish(Config) ->
    Jail = ?config(jail, Config),
    {ok, {exit_status, _N}} = mindflayer_jail:start_and_finish_jail([Jail#jail { command = "/bin/ls", command_args=["/"] }]),
    mindflayer_jail:umount_devfs(Jail#jail.path),
    ok.


destroy_jail(Config) ->
    Jail = ?config(jail, Config),
    {ok, _Pid} = mindflayer_jail:start_jail([Jail#jail { command = "/bin/sh", command_args=["etc/rc"] }]),
    timer:sleep(1000), % Atm the call above is async which means that we'll have to wait for the gen_server to actually execute.
    mindflayer_jail:destroy(Jail),
    timer:sleep(25000), % It takes some time to close down a fullblown and recently created jail!
    mindflayer_jail:umount_devfs(Jail#jail.path),
    ok.


create_image(_Config) ->
    mindflayer_images:start_link(),
    Cmd = "/bin/sh",
    CmdArgs = ["-c", "echo 'lol' > /root/test.txt"],
    Parent = ?BASEJAIL_IMAGE,
    Destination = ?TEST_MF_JAIL_DATASET,
    mindflayer_images:create_image(Cmd, CmdArgs, Parent, Destination),
    {ok, <<"lol\n">>} = file:read_file("/" ++ Destination ++ "/root/test.txt"),
    mindflayer_zfs:destroy(?TEST_MF_JAIL_DATASET ++ "@image"),
    ok.


init_per_testcase(_TestCase, Config) ->
    Jail = #jail{
              name= ?TEST_MF_JAIL_TESTJAIL,
              path= "/" ++ ?TEST_MF_JAIL_DATASET, %% Atm mindflayer_jail is relying on the fact the clone created for a jail is automatically mount into this path. Should be more generic.
              zfs_dataset = ?TEST_MF_JAIL_DATASET,
              image = ?BASEJAIL_IMAGE,
              parameters=["mount.devfs", "ip4.addr=10.13.37.3", "exec.stop=\"/bin/sh /etc/rc.shutdown\""] % "mount.devfs", <-- using mount.devfs requires additional unmounting or that zfs destroy -f is used
             },
    [{jail, Jail} | Config].


end_per_testcase(_TestCase, _Config) ->
    0 = mindflayer_zfs:destroy(?TEST_MF_JAIL_DATASET),
    ok.
