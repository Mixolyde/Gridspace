%%%-------------------------------------------------------------------
%%% File    : gs_super.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Supervisor for all top-level Gridspace behaviours
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(gs_super).
-author("mixolyde@gmail.com").
-behaviour(supervisor). % see erl -man supervisor

-export([start/0, start_in_shell_for_testing/0, start_link/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).

start() ->
    spawn(fun() ->
        supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
    end).

start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
    {ok, {
        {one_for_one, ?MAX_RESTART, ?MAX_TIME}, % super options
        [
            % player authentication server
            {gs_player_auth,
            {gs_player_auth, start_link, []},
            permanent,
            10000,
            worker,
            [gs_player_auth]},
            % tcp listener
            {gs_echo_server,
            {gs_echo_server, start_link, []},
            permanent,
            10000,
            worker,
            [gs_echo_server]}

       ]
    }}.