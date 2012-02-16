%%%-------------------------------------------------------------------
%%% File    : gs_command_executor.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Util methods for executing player commands in various
%%%   states
%%%
%%% Created :  14 Feb 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(gs_command_executor).
-author("Brian E. Williams").
-compile([debug_info, export_all]).

execute_command({command, Cname, Args}, Player) ->
  error_logger:info_msg("Executing command: ~p with args: ~p from player: ~p~n", [Cname, Args, Player]),
  ok.
