%%%-------------------------------------------------------------------
%%% File    : gs_telnet_client.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Telnet client which is started when a new telnet
%%%   connection is made, connected to a FSM for controlling user
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% Implements a simple Echo server using the gen_listener_tcp behaviour.

-module(gs_telnet_client).

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
-export([telnet_client/2]).

%% @doc The echo client process.
telnet_client(Socket, FSMPid) ->
    error_logger:info_msg("telnet_client() looped~n"),
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        %% handle outgoing messages to the client
        {send, Data} ->
            % error_logger:info_msg("Sending Data to client: ~s", [Data]),
            gen_tcp:send(Socket, Data),
            telnet_client(Socket, FSMPid);
        %% handle tcp data from the client
        {tcp, Socket, <<"quit", _R/binary>>} ->
            error_logger:info_msg("Quit Requested.~n"),
            gen_tcp:send(Socket, "Thanks for playing Gridspace!\r\n"),
            gen_fsm:send_all_state_event(FSMPid, {quit, user_quit}),
            gen_tcp:close(Socket);
        {tcp, Socket, UntrimmedData} ->
          % TODO check for malformed requests and injection attacks
            Data = lists:reverse(trim(lists:reverse(binary_to_list(UntrimmedData)))),
            % error_logger:info_msg("Received data from connection: ~p~n", [Data]),
            % gen_tcp:send(Socket, "I Received " ++ Data),
            gen_fsm:send_event(FSMPid, {self(), data, Data}),
            telnet_client(Socket, FSMPid);
        {tcp_closed, Socket} ->
            gen_fsm:send_all_state_event(FSMPid, {quit, socket_closed}),
            error_logger:info_msg("Client Disconnected.~n");
        Unmatched ->
            gen_fsm:send_all_state_event(FSMPid, {quit, unmatched_input}),
            gen_tcp:close(Socket),
            error_logger:error_msg("Closing connection, unmatched in echo_client: ~p~n", [Unmatched])
    end.

trim([$\n | Rest] ) -> trim(Rest);
trim([$\r | Rest] ) -> trim(Rest);
trim(Trimmed) -> Trimmed.
