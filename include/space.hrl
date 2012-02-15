%%%-------------------------------------------------------------------
%%% File    : space.hrl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Space-related structures (e.g. - systems, stations,
%%%  planets, moons, belts, etc)
%%%
%%% Created :  03 Feb 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-record(planet, {pname, system = #system{}, xloc, yloc} ).

-record(station, {sname, system = #system{}, xloc, yloc} ).

-record(system, {sname, stations, planets, moons, belts} ).

