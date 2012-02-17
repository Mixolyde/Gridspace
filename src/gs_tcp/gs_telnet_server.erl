%%%-------------------------------------------------------------------
%%% File    : gs_telnet_server.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Telnet Server using gs_gen_listener_tcp behavior
%%%   accepts the incoming request, creates a telnet_client and hands
%%%   off. Acts as a server for telnet requests, but also supervises
%%%   the FSM children
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

-record(state, {connections = []}).

%% includes
-include("../../include/messages.hrl").

%% API
-export([start_link/0, stop/0, info/0]).

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

stop() -> gs_gen_listener_tcp:call(?MODULE, stop).

info() -> gs_gen_listener_tcp:call(?MODULE, info).

init([]) ->
  % trap exits from FSM child classes
  process_flag(trap_exit, true),
  {ok, {?TCP_PORT, ?TCP_OPTS}, #state{connections = []}}.

handle_accept(Sock, State) ->
  % Create the login finite state machine and link to it
  {ok, FSMPid} = gs_login_fsm:start_link(),
  % Create the specific echo_client function which handles the
  %   communication
  ClientPid = spawn(fun() -> gs_telnet_client:telnet_client(Sock, FSMPid) end),
  % Change the controlling process of the socket, so the client loop receives
  %   the socket input
  gen_tcp:controlling_process(Sock, ClientPid),
  % Send the game's welcome message to the client to get them started
  ClientPid ! {send, ?WELCOME_MSG},
  % add a connection record to the list of connections being monitored by this server
  {noreply, #state{connections = [{Sock, ClientPid, FSMPid} | State#state.connections]}}.



handle_call(info, _From, State = #state{connections = Conns}) ->
    % error_logger:info_msg("~p received info request from: ~p with state ~p~n", [?MODULE, _From, State]),
    Reply = {telnet_server_info, 
      [{count, length(Conns)}]},
    {reply, Reply, State};
handle_call(stop, From, State) ->
    error_logger:info_msg("~p received stop from: ~p~n", [?MODULE, From]),
    {stop, normal, stopped, State};
handle_call(Request, _From, State) ->
  error_logger:info_msg("Telnet Server received unexpected call: ~p~n", [Request]),
  {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
  error_logger:info_msg("Telnet Server received unexpected cast: ~p~n", [_Request]),
  {noreply, State}.

handle_info(_Info, State) ->
  error_logger:info_msg("Telnet Server received unexpected info message: ~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  error_logger:info_msg("Shutting down Telnet Server~n", []),
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

