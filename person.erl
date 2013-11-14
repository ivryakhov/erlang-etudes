%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 11-4: Chat Room
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(person).

-behaviour(gen_server).

%% API
-export([start_link/1, get_chat_node/0, login/1, logout/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CHATROOM, chatroom).

-record(state, {chat_node, profile}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ChatRoom) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ChatRoom, []).

%%--------------------------------------------------------------------
%% @doc
%% Gets the name of the chatroom node
%%
%% @spec get_chat_node() -> string()
%% @end
%%--------------------------------------------------------------------
get_chat_node() ->
    gen_server:call(?SERVER, get_chat_node).

%%--------------------------------------------------------------------
%% @doc
%% Sends a call to the chatroom node with login request
%%
%% @spec login(string()) -> string()
%% @end
%%--------------------------------------------------------------------
login(UserName) ->
    gen_server:call({?CHATROOM, get_chat_node()}, {login, UserName, node()}).

%%--------------------------------------------------------------------
%% @doc
%% Sends a call to the chatroom node with logout request
%%
%% @spec login(string()) -> string()
%% @end
%%--------------------------------------------------------------------
logout() ->
    gen_server:call({?CHATROOM, get_chat_node()}, logout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(ChatRoom) ->
    State = #state{chat_node=ChatRoom, profile=[]},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_chat_node, _From, State) ->
    Reply = State#state.chat_node,
    {reply, Reply, State};
handle_call(get_profile, _From, State) ->
    Reply = State#state.profile,
    {reply, Reply, State};
handle_call({set_profile, Key, Value}, _From, State) ->
    Profile = case lists:keymember(Key, 1, State#state.profile) of
        true ->
            lists:keyreplace(Key, 1, State#state.profile, {Key, Value});
        false ->
            State#state.profile ++ [{Key, Value}]
    end,
    NewState = #state{chat_node = State#state.chat_node, profile = Profile},
    {reply, Profile, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
