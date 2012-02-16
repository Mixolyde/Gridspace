%%%-------------------------------------------------------------------
%%% File    : gs_prompt.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Prompt display utility methods
%%%
%%% Created :  14 Feb 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(gs_prompt).
-author("Brian E. Williams").
-compile([debug_info, export_all]).

-include("../../include/player.hrl").

get_prompt(docked, #player{pname = PlayerName}) -> 
  io_lib:format("~s Currently Docked>~n", [PlayerName]);
get_prompt(inspace, #player{pname = PlayerName}) -> 
  io_lib:format("~s Currently In Space>~n", [PlayerName]).


