%%%-------------------------------------------------------------------
%%% File    : gs_player_auth.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Player Login authentication server for Gridspace
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------
-module(gs_player_auth).
-author("mixolyde@gmail.com").

-behaviour(gen_server).

%% Authentication Record
-record(player, {name, password}).

%% API
-export([start_link/0, stop/0, authenticate/2, player_exists/1, add_player/2, remove_player/2, change_password/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    io:format("Starting local link with name ~p~n", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    io:format("Trying to stop ~p~n", [?MODULE]),
    gen_server:call(?MODULE, stop).

authenticate(Name, Password)                    -> gen_server:call(?MODULE, {authenticate, Name, Password}).
player_exists(Name)                             -> gen_server:call(?MODULE, {player_exists, Name}).
add_player(Name, Password)                      -> gen_server:call(?MODULE, {add_player, Name, Password}).
remove_player(Name, Password)                   -> gen_server:call(?MODULE, {remove_player, Name, Password}).
change_password(Name, OldPassword, NewPassword) -> gen_server:call(?MODULE, {change_password, Name, OldPassword, NewPassword}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    % set the key position to 2 to account for record syntax
    Tab = ets:new(?MODULE,[{keypos,2}]),
    TestPlayers = [
      {"admin", "testadmin"},
      {"mixol", "testmix"}],
    lists:map(fun ({Name, Pass}) ->
          Login = #player{name = Name, password = Pass},
          io:format("Inserting default player into table: ~p~n", [Login]),
          ets:insert(Tab, Login) end, 
      TestPlayers),
    
    {ok, Tab}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({authenticate, Name, Password}, _From, Tab) ->
    Player = #player{name = Name, password = Password},
    Reply = case ets:lookup(Tab, Name) of
        [] -> {no_player, Name};
        [Player] ->
            {authenticated, Name};
        [#player{name = Name, password = _DifferentPassword}] ->
            {bad_password, Name}
    end,
    {reply, Reply, Tab};
handle_call({player_exists, Name}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Name) of
        [] -> {no_player, Name};
        [#player{name = Name, password = _DifferentPassword}] ->
            {exists, Name}
    end,
    {reply, Reply, Tab};
handle_call({add_player, Name, Password}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Name) of
        [] -> ets:insert(Tab, #player{name = Name, password = Password}),
            {welcome, Name};
        [_] ->
            {already_a_player, Name}
    end,
    {reply, Reply, Tab};
handle_call({remove_player, Name, Password}, _From, Tab) ->
    Player = #player{name = Name, password = Password},
    Reply = case ets:lookup(Tab, Name) of
        [] -> {not_a_player, Name};
    [Player] ->
        io:format("Passwords match, removing player: ~p~n", [Name]),
        ets:delete(Tab, Name),
        {player_removed, Name};
    [_] ->
        io:format("Passwords DO NOT match, not removing player: ~p~n", [Name]),
        {bad_password, Name}
    end,
{reply, Reply, Tab};
handle_call({change_password, Name, _OldPassword, _NewPassword}, _From, Tab) ->
    % Player = #player{name = Name, password = Password},
    Reply = case ets:lookup(Tab, Name) of
        [] -> {not_a_player, Name};
        [_] ->
            {Name, you_already_are_a_customer}
    end,
    {reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
    io:format("Received stop from: ~p~n", [_From]),
    {stop, normal, stopped, Tab};
handle_call(_Request, _From, State) ->
    io:format("Received unrecognized request: ~p from: ~p~n", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Called terminate on authentication server with reason: ~p~n", [_Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    io:format("Called a code change on authentication server~n", []),
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
