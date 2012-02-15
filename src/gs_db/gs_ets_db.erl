%%%-------------------------------------------------------------------
%%% File    : gs_ets_db.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Sever which maintains the ets tables for the app
%%%
%%% Created :  10 Aug 2010 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------
-module(gs_ets_db).
-author("mixolyde@gmail.com").

-behaviour(gen_server).

%% Authentication Record
-record(templateobject, {objectkey, objectcolumn}).

%% API
-export([start_link/0, stop/0, object_exists/1, add_object/2, remove_object/1]).

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
    io:format("Starting ets server: ~p~n", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    io:format("Stopping ets server: ~p~n", [?MODULE]),
    gen_server:call(?MODULE, stop).

object_exists(Key)                        -> gen_server:call(?MODULE, {object_exists, Key}).
add_object(Key, Col)                      -> gen_server:call(?MODULE, {add_object, Key, Col}).
remove_object(Key)                        -> gen_server:call(?MODULE, {remove_player, Key}).

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
    % set the key position to 2 to account for record syntax {player, name, pass, ...}
    Tab = ets:new(?MODULE,[{keypos,2}]),
    TestObjects = [
      {"admin", "testadmin"},
      {"mixol", "testmix"},
      {"bravo", "testbravo"}],
    lists:map(fun ({Key, Col}) ->
          TableEntry = #templateobject{objectkey = Key, objectcolumn = Col},
          io:format("Inserting default Table Entry into table: ~p~n", [TableEntry]),
          ets:insert(Tab, TableEntry) end, 
      TestObjects),
    
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
handle_call({object_exists, Key}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Key) of
        [] -> {no_object, Key};
        [#templateobject{objectkey = Key}] ->
            {exists, Key}
    end,
    {reply, Reply, Tab};
handle_call({add_object, Key, Col}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Key) of
        [] -> 
          Obj = #templateobject{objectkey = Key, objectcolumn = Col},
          ets:insert(Tab, Obj),
            {created, Obj};
        [_] ->
            {already_exists, Key}
    end,
    {reply, Reply, Tab};
handle_call({remove_player, Key}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Key) of
      [] -> {no_object, Key};
      [_] ->
        ets:delete(Tab, Key),
        {object_removed, Key}
    end,
    {reply, Reply, Tab};
handle_call(stop, From, Tab) ->
    io:format("~p received stop from: ~p~n", [?MODULE, From]),
    {stop, normal, stopped, Tab};
handle_call(Request, From, State) ->
    io:format("~p received unrecognized request: ~p from: ~p~n", [?MODULE, Request, From]),
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
terminate(Reason, _State) ->
    io:format("Called terminate on ~p with reason: ~p~n", [?MODULE, Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    io:format("Called a code change on ~p~n", [?MODULE]),
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
