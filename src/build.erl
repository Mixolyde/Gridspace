%%%-------------------------------------------------------------------
%%% File    : build.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Build scripts and utils
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------
-module(build).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-define(DIRMODULES, [gs_player_auth, gs_tcp]).
-define(TOPMODULES, [gridspace_super, gridspace_app, build]).

all() ->
    {ok, Dir} = file:get_cwd(),
    c:cd("../src"),
    make:all([load]),
    file:set_cwd(Dir).

change() -> oktest2.