%%%-------------------------------------------------------------------
%%% File    : gs_telnet_server.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Telnet Server using gs_gen_listener_tcp behavior
%%%   accepts the incoming request, creates a telnet_client and hands
%%%   off
%%%
%%% Created :  06 Feb 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% Improves on the simple echo server, using the gen_listener_tcp behavior

-module(gs_telnet_server).
-behaviour(gs_gen_listener_tcp).

-define(TCP_PORT, 5001).
-define(TCP_OPTS, [binary, inet,
                   {active,    false},
                   {backlog,   10},
                   {nodelay,   true},
                   {packet,    raw},
                   {reuseaddr, true}]).
%% includes
-include("../../include/messages.hrl").

%% API
-export([start_link/0]).

%% gen_listener_tcp callbacks
-export([init/1,
         handle_accept/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @doc Start the server.
start_link() ->
    gs_gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, {?TCP_PORT, ?TCP_OPTS}, nil}.

handle_accept(Sock, State) ->
  % Create the login finite state machine
  {ok, FSMPid} = gs_login_fsm:start_link(),
  % Create the specific echo_client function which handles the
  %   communication
  ClientPid = spawn(fun() -> gs_telnet_client:telnet_client(Sock, FSMPid) end),
  % Change the controlling process of the socket, to avoid crashing
  %   the tcp server
  gen_tcp:controlling_process(Sock, ClientPid),
  % Send the game's welcome message to the client to get them started
  ClientPid ! {send, ?WELCOME_MSG},
  {noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

