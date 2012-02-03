%%%-------------------------------------------------------------------
%%% File    : gs_player.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Player data methods
%%%
%%% Created :  17 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(gs_player).

%% API
-export([add_player/2]).

add_player(Name, Password) ->
    gs_player_auth:add_player(Name, Password).

