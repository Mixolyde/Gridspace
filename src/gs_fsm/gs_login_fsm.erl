%%%-------------------------------------------------------------------
%%% File    : gs_login_fsm.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Initial login state machine, should hand off to in_game
%%%  state machine on successful login
%%%
%%% Created :  17 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% States : welcome, enter_pass, create_new, create_new_password, authenticating, docked, quitting


-module(gs_login_fsm).

-behaviour(gen_fsm).

-include("../../include/messages.hrl").
-include("../../include/player.hrl").

%% API
-export([start_link/0]).

%% states
-export([welcome/2, enter_pass/2, create_new_yorn/2, create_new_pass/2, confirm_new_pass/2, docked/2, inspace/2]).

%% gen_fsm callbacks
-export([init/1, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------


start_link() ->
  % do not register with a global name, as there will be an FSM for each
  % logged in player
  gen_fsm:start_link(?MODULE, [], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([]) ->
  error_logger:info_msg("Created login fsm~n", []),

  {ok, welcome, []}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%--------------------------------------------------------------------
welcome({SocketPid, data, InData}, State) when length(InData) > 3 ->
    Data = string:to_lower(InData),
    error_logger:info_msg("Received data event ~p in welcome state for FSM State: ~p~n", [Data, State]),
    case gs_player_db:player_exists(Data) of
        {exists, Data} ->
            SocketPid ! {send, "Password: "},
            {next_state, enter_pass, [{player, Data}]};
        {no_player, Data} ->
          % TODO check for reserved words
            SocketPid ! {send, "That player name does not exist!\r\nCreate a new player (y or n): "},
            {next_state, create_new_yorn, [{player, Data}]}
    end;
welcome({SocketPid, data, InData}, _State) ->
  error_logger:info_msg("Received a login name less than 4 chars: ~p~n", [InData]),
  SocketPid ! {send, "Character names must be greater than 3 chars.\r\n"},
  SocketPid ! {send, ?WELCOME_MSG},
  {next_state, welcome, []};
welcome(_Event, State) ->
    error_logger:info_msg("Received event ~p in welcome state for FSM State: ~p~n", [_Event, State]),
  {next_state, welcome, State}.

enter_pass({SocketPid, data, Data}, State = [{player, PlayerName}]) ->
    error_logger:info_msg("Received data event ~p in enter_pass state for FSM State: ~p~n", [Data, State]),
    case gs_player_db:authenticate(PlayerName, Data) of
        {authenticated, PlayerName, Player} ->
            SocketPid ! {send, "Password accepted!\r\nYou have entered the Gridspace Universe!\r\n"},
            {next_state, docked, [{player, Player}]};
        {bad_password, PlayerName} ->
            SocketPid ! {send, "Invalid password.\r\n"},
            SocketPid ! {send, ?WELCOME_MSG},
            {next_state, welcome, []}
    end;
enter_pass(_Event, State) ->
    error_logger:info_msg("Received event ~p in enter_pass state for FSM State: ~p~n", [_Event, State]),
    {next_state, enter_pass, State}.

create_new_yorn({SocketPid, data, InData}, State = [{player, PlayerName}]) ->
    Data = string:to_lower(InData),
    error_logger:info_msg("Received data event ~p in create_new_yorn state for FSM State: ~p~n", [Data, State]),
    case Data of
        "y" ->
            SocketPid ! {send, io_lib:format("Ok, new player: ~s (You can set your Display Name later.)~nEnter New Password: ", [PlayerName])},
            {next_state, create_new_pass, [{player, PlayerName}]};
        _Else ->
            SocketPid ! {send, "Ok.\r\n"},
            SocketPid ! {send, ?WELCOME_MSG},
            {next_state, welcome, []}
    end;
create_new_yorn(_Event, State) ->
    error_logger:info_msg("Received event ~p in create_new_yorn state for FSM State: ~p~n", [_Event, State]),
    {next_state, create_new_yorn, State}.

create_new_pass({SocketPid, data, Data}, State = [{player, PlayerName}]) ->
    error_logger:info_msg("Received data event ~p in create_new_pass state for FSM State: ~p~n", [Data, State]),
    SocketPid ! {send, io_lib:format("Confirm Password: ", [])},
    {next_state, confirm_new_pass, [{player, PlayerName}, {password, Data}]};
create_new_pass(_Event, State) ->
    error_logger:info_msg("Received event ~p in create_new_pass state for FSM State: ~p~n", [_Event, State]),
    {next_state, create_new_pass, State}.

confirm_new_pass({SocketPid, data, Data}, State = [{player, PlayerName}, {password, Password}]) ->
    error_logger:info_msg("Received data event ~p in confirm_new_pass state for FSM State: ~p~n", [Data, State]),
    case Data of
        Password ->
            SocketPid ! {send, io_lib:format("Password confirmed. Welcome to Gridspace, ~s!~n", [PlayerName])},
            % TODO retrieve player record from DB and use that as the state
            gs_player_db:add_player(PlayerName, Password),
            {player, Player} = gs_player_db:retrieve_player(PlayerName),
            {next_state, docked, [{player, Player}]};
        _Else ->
            SocketPid ! {send, "Passwords don't match, player not created.\r\n"},
            SocketPid ! {send, ?WELCOME_MSG},
            {next_state, welcome, []}
    end;
confirm_new_pass(_Event, State) ->
    error_logger:info_msg("Received event ~p in confirm_new_pass state for FSM State: ~p~n", [_Event, State]),
    {next_state, confirm_new_pass, State}.

docked({SocketPid, data, Data}, State = [{player, Player}]) ->
    error_logger:info_msg("Received event ~p in docked state for FSM State: ~p~n", [Data, State]),
    Command = gs_command_parser:parse_command(docked, Data, Player),
    _Executed = gs_command_executor:execute_command(Command, Player),
    Prompt = gs_prompt:get_prompt(docked, Player),
    1=0,
    SocketPid ! {send, io_lib:format("Command echo: ~p~n", [Command])},
    SocketPid ! {send, Prompt},
    {next_state, docked, State};
docked(_Event, State) ->
    error_logger:error_msg("Received non-standard event ~p in docked state for FSM State: ~p~n", [_Event, State]),
    {next_state, docked, State}.

inspace({SocketPid, data, Data}, State = [{player, Player}]) ->
    error_logger:info_msg("Received event ~p in inspace state for FSM State: ~p~n", [Data, State]),
    SocketPid ! {send, io_lib:format("~s Currently In Space> Command echo: ~p~n", [Player, Data])},
    {next_state, inspace, State};
inspace(_Event, State) ->
    error_logger:error_msg("Received non-standard event ~p in inspace state for FSM State: ~p~n", [_Event, State]),
    {next_state, inspace, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                                                NextState} |
%%                                          {next_state, NextStateName,
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event({quit, Reason}, StateName, State) ->
    io:format("Player FSM received quit message: ~p while in State: ~p~n", [Reason, StateName]),
  {stop, normal, State};
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, StateName, _State) ->
  error_logger:info_msg("Login FSM terminating while in state: ~p for reason: ~p~n", [StateName, Reason]),
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
