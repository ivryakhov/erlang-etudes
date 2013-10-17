%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 6-4: Lists of Lists
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(teeth).
-export([alert/1]).
-revision('Revision: 0.1').
-created('Date: 2013/10/17').
-modified('Date: 2013/10/17').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Takes a list of 32 lists of six numbers and produces 
%%      a list of the tooth numbers that require attention.
%% @spec alert(list(list(integer()))) -> list(integer())
%% @end
%%-------------------------------------------------------------------
alert(ListOfLists) ->
    alert(ListOfLists, 1, []).

%%-------------------------------------------------------------------
%% @doc helper function for alert/1 that process the list by list
%%      and counts teeth with problems
%% @spec alert(list(list(integer())), integer(), list(integer())) -> list(integer())
%% @end
%%-------------------------------------------------------------------
alert([], _, Result) ->
    Result;
alert([H|T], Count, Result) ->
    case check_tooth(H) of
        true -> ResIter = Result ++ [Count];
        false -> ResIter = Result
    end,
    alert(T, Count + 1, ResIter).

%%-------------------------------------------------------------------
%% @doc checks tooh for parameters and decides of attention is required
%% @spec check_tooth(list(integer())) -> bool()
%% @end
%%-------------------------------------------------------------------
check_tooth([]) -> false;
check_tooth([H|T]) ->
    case H >= 4 of
        true -> true;
        false -> check_tooth(T)
    end.
