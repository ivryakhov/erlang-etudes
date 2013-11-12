%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 11-1: Get the Weather,
%%%       Étude 11-2: Wrapper Functions
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(weather).

-behaviour(gen_server).
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([start_link/0]).
-export([report/1, recent/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

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

report(StationCode) ->
    gen_server:call(?MODULE, StationCode).

recent() -> gen_server:cast(?MODULE, "").

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
    inets:start(),
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
handle_call(Request, _From, State) ->
    Url = "http://w1.weather.gov/xml/current_obs/" ++ Request ++ ".xml",
    {Result, Info} = httpc:request(Url),
    case Result of
        error -> Reply = {Result, Info};
        ok ->
            {{_HttpVer, Code, _CodeMess}, _Header, XmlContent} = Info,
            case Code of
                404 ->
                    Reply = {error, 404};
                200 ->
                    Reply = analyze_info(XmlContent)
            end
    end,
    {reply, Reply, [Request | lists:sublist(State, 10)]}.

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
    io:format("Most recent requests: ~p~n", [State]),
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
    inets:stop(),
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

%% Take raw XML data and return a set of {key, value} tuples

analyze_info(WebData) ->
%% list of fields that you want to extract
    ToFind = [location, observation_time_rfc822, weather, temperature_string],

%% get just the parsed data from the XML parse result
    Parsed = element(1, xmerl_scan:string(WebData)),

%% This is the list of all children under <current_observation>
    Children = Parsed#xmlElement.content,

%% Find only XML elements and extract their names and their text content.
%% You need the guard so that you don't process the newlines in the
%% data (they are XML text descendants of the root element).
    ElementList = [{El#xmlElement.name, extract_text(El#xmlElement.content)}
      || El <- Children, element(1, El) == xmlElement],

    %% ElementList is now a keymap; get the data you want from it.
    lists:map(fun(Item) -> lists:keyfind(Item, 1, ElementList) end, ToFind).


%% Given the parsed content of an XML element, return its first node value
%% (if it's a text node); otherwise return the empty string.

extract_text(Content) ->
    Item = hd(Content),
    case element(1, Item) of
        xmlText -> Item#xmlText.value;
        _ -> ""
    end.
