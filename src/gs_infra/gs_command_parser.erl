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

parse_command(_InData) ->
  {command, cname, [args]}.

