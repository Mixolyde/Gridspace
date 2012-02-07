%%%-------------------------------------------------------------------
%%% File    : gs_echo_server.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Echo Server using gen_listener_tcp behavior
%%%   echos the incoming data back out on the connection
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% Implements a simple Echo server using the gen_listener_tcp behaviour.

-module(gs_echo_server).
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

%% @doc The echo client process.
echo_client(Socket, FSMPid) ->
    error_logger:info_msg("echo_client() looped~n"),
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        %% handle outgoing messages to the client
        {send, Data} ->
            error_logger:info_msg("Sending Data to client: ~p", [Data]),
            gen_tcp:send(Socket, Data),
            echo_client(Socket, FSMPid);
        %% handle tcp data from the client
        {tcp, Socket, <<"quit", _R/binary>>} ->
            error_logger:info_msg("Quit Requested."),
            gen_tcp:send(Socket, "Bye now.\r\n"),
            gen_fsm:send_all_state_event(FSMPid, {quit}),
            gen_tcp:close(Socket);
        {tcp, Socket, UntrimmedData} ->
            Data = lists:reverse(trim(lists:reverse(binary_to_list(UntrimmedData)))),
            error_logger:info_msg("Got Data: ~p~n", [Data]),
            % gen_tcp:send(Socket, "I Received " ++ Data),
            gen_fsm:send_event(FSMPid, {self(), data, Data}),
            echo_client(Socket, FSMPid);
        {tcp_closed, Socket} ->
            gen_fsm:send_all_state_event(FSMPid, {quit, socket_closed}),
            error_logger:info_msg("Client Disconnected.");
        Unmatched ->
            gen_fsm:send_all_state_event(FSMPid, {quit, unmatched_input}),
            gen_tcp:close(Socket),
            error_logger:info_msg("Closing connection, unmatched in echo_client: ~p~n", [Unmatched])
    end.

init([]) ->
    {ok, {?TCP_PORT, ?TCP_OPTS}, nil}.

handle_accept(Sock, State) ->
  % Create the login finite state machine
  {ok, FSMPid} = gs_player_fsm:start_link(),
  % Create the specific echo_client function which handles the
  %   communication
  ClientPid = spawn(fun() -> echo_client(Sock, FSMPid) end),
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

trim([$\n | Rest] ) -> trim(Rest);
trim([$\r | Rest] ) -> trim(Rest);
trim(Trimmed) -> Trimmed.
