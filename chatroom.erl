%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 11-4: Chat Room
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(chatroom).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(PERSON, person).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, []}.

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
handle_call({login, UserName, ServerName}, From, State) ->
    {Pid, _Ref} = From,
    case lists:keyfind(Pid, 2, State) of
        false ->
            case lists:keyfind({UserName, ServerName}, 1, State) of
                false ->
                    NewState = State ++ [{{UserName, ServerName}, Pid}],
                    Reply = {ok, "Logged in."};
                _ ->
                    NewState = State,
                    Reply = {nok, "User " ++ UserName ++ " from "
                             ++ ServerName ++ " server is already logged in."}
            end;
        _ ->
            NewState = State,
            Reply = {nok, "You are already logged in."}
    end,
    {reply, Reply, NewState};

handle_call(logout, From, State) ->
    {Pid, _Ref} = From,
    case lists:keyfind(Pid, 2, State) of
        false ->
            Reply = {nok, "You are not logged in."},
            NewState = State;
        _ ->
            NewState = lists:keydelete(Pid, 2, State),
            Reply = {ok, "Logged out."}
    end,
    {reply, Reply, NewState};

handle_call(users, _From, State) ->
    Reply = State,
    {reply, Reply, State};

handle_call({say, Text}, From, State) ->
    {Pid, _Ref} = From,
    Reply = case lists:keymember(Pid, 2, State) of
        true ->
            {{UserName, UserServer}, _Pid} = lists:keyfind(Pid, 2, State),
            Message = UserName ++ " (" ++ atom_to_list(UserServer) ++ ") says: "
                      ++ "\"" ++ Text ++ "\"~n",
            [gen_server:cast({?PERSON, SenderServer}, Message) ||
                {{SenderName, SenderServer}, _} <- State, SenderName /= UserName],
            ok;
        false ->
            {nok, "You are not logged in."}
    end,
    {reply, Reply, State};

handle_call({who, Person, ServerName}, _From, State) ->
    Reply = case lists:keymember({Person, ServerName}, 1, State) of
        true ->
            gen_server:call({?PERSON, ServerName}, get_profile);
        false ->
            {nok, "No such user"}
    end,
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    {ok, {error, "Unhandled Request", Request}, State}.


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
