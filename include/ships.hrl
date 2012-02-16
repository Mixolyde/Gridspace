%%%-------------------------------------------------------------------
%%% File    : ships.hrl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Ship-related structures (e.g. - ship frames, launched ship
%%%   fitted ship)
%%%
%%% Created :  03 Feb 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-record(ship_frame, {sname, base_shield, base_armor,
  gun_mounts, launcher_mounts, equip_mounts, upgrade_mounts}).

-record(fitted_ship, {ref, ship_frame = #ship_frame{}, guns, launchers, equips,
    upgrades} ).

-record(ship_in_space, {ref, ship_pid, fitted_ship, current_heading, 
    current_velocity, set_heading, set_velocity} ).


