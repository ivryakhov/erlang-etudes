%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 7-2: List Comprehensions and Pattern Matching
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(people).
-export([list_comprehension1/1, list_comprehension2/1]).
-revision('Revision: 0.1').
-created('Date: 2013/10/19').
-modified('Date: 2013/10/19').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Takes a list of tuples in format {Name, Gender, Age} where
%%      Name is a string(), Gender is a char() with $F or $M value and
%%      Age is an integer().
%%      Returns a list consisting of the names of all males who are
%%      over 40.
%% @spec list_comprehension1(list(tuple(string(), char(), integer()))) -> list(string())
%% @end
%%-------------------------------------------------------------------
list_comprehension1(PeopleList) ->
    list_comprehension1(PeopleList, []).

%%-------------------------------------------------------------------
%% @doc helper function for list_comprehension1/1 that process the PeopleList
%%      and fills the ListIter using guards.
%% @spec list_comprehension1(list(tuple(string(), char(), integer())),
%%                           list(string())) -> list(string())
%% @end
%%-------------------------------------------------------------------
list_comprehension1([], ListIter) ->
    ListIter;

list_comprehension1([{Name, Gender, Age}|OtherPeople], ListIter) when (Gender == $M) and (Age > 40) ->
    list_comprehension1(OtherPeople, ListIter ++ [Name]);

list_comprehension1([_|OtherPeople], ListIter) ->
    list_comprehension1(OtherPeople, ListIter).


%%-------------------------------------------------------------------
%% @doc Takes a list of tuples in format {Name, Gender, Age} where
%%      Name is a string(), Gender is a char() with $F or $M value and
%%      Age is an integer().
%%      Returns a list consisting of the names of people whoa who are
%%      male or over 40.
%% @spec list_comprehension2(list(tuple(string(), char(), integer()))) -> list(string())
%% @end
%%-------------------------------------------------------------------
list_comprehension2(PeopleList) ->
    list_comprehension2(PeopleList, []).

%%-------------------------------------------------------------------
%% @doc helper function for list_comprehension1/1 that process the PeopleList
%%      and fills the ListIter using guards.
%% @spec list_comprehension2(list(tuple(string(), char(), integer())),
%%                           list(string())) -> list(string())
%% @end
%%-------------------------------------------------------------------
list_comprehension2([], ListIter) ->
    ListIter;

list_comprehension2([{Name, Gender, Age}|OtherPeople], ListIter) when Gender == $M orelse Age > 40 ->
    list_comprehension2(OtherPeople, ListIter ++ [Name]);

list_comprehension2([_|OtherPeople], ListIter) ->
    list_comprehension2(OtherPeople, ListIter).
