%%%-------------------------------------------------------------------
%%% File    : gs_app.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Gridspace application which starts the Gridspace
%%%     Supervisor.
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(gs_app).
-author("mixolyde@gmail.com").
-behaviour(application).
-export([start/2, stop/1]).
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%% {ok, Pid, State} |
%% {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    gs_super:start_link(StartArgs).
%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_State) ->
  error_logger:info_msg("Called stop at gs_app level~n", []),
  gs_super:stop(),
  ok.
