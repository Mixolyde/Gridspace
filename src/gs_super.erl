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

-export([start/0, start_link/1, init/1, stop/0]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).

start() ->
    spawn(fun() ->
      {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
      error_logger:info_msg("Starting gridspace supervisor in unlinked mode with Pid: ~p~n", [Pid])
    end).

start_link(Args) ->
  {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, Args),
  error_logger:info_msg("Starting gridspace supervisor in linked mode with Pid: ~p~n", [Pid]),
  {ok, Pid}.

init([]) ->
    {ok, {
        {one_for_one, ?MAX_RESTART, ?MAX_TIME}, % super options
        [
            % player authentication server
            {gs_player_db,
            {gs_player_db, start_link, []},
            permanent,
            10000,
            worker,
            [gs_player_db]},
            % telnet listener
            {gs_telnet_server,
            {gs_telnet_server, start_link, []},
            permanent,
            10000,
            worker,
            [gs_telnet_server, gs_telnet_client]}

       ]
    }}.

stop() ->
  error_logger:info_msg("Shutting down gridspace from stop call in main supervisor.~n", []),
  gs_player_db:stop(),
  gs_telnet_server:stop(),
  error_logger:info_msg("Completed shutdown of gridspace supervised servers.~n", []).
