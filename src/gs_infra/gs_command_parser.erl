%%%-------------------------------------------------------------------
%%% File    : gs_command_parser.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Util methods for parsing player data into commands
%%%   and running them
%%%
%%% Created :  14 Feb 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(gs_command_parser).
-author("Brian E. Williams").
-compile([debug_info, export_all]).

-include("../../include/player.hrl").

parse_command(docked, InData, #player{pname = PlayerName}) ->
  error_logger:info_msg("Parsing command: ~s send by player: ~p while docked.~n", [InData, PlayerName]),
  {command, cname, [args]};

parse_command(inspace, InData, #player{pname = PlayerName}) ->
  error_logger:info_msg("Parsing command: ~s send by player: ~p while in space.~n", [InData, PlayerName]),
  {command, cname, [args]}.
