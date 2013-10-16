%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Chapter 6. Lists
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(stats).
-export([minimum/1, maximum/1, range/1]).
-revision('Revision: 0.1').
-created('Date: 2013/10/15').
-modified('Date: 2013/10/16').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Takes a list of numbers and return the minumim number
%% @spec minimum(list(number()) -> number()
%% @end
%%-------------------------------------------------------------------
minimum([H|T]) ->
    extrem(T, H, min).

%%-------------------------------------------------------------------
%% @doc Takes a list of numbers and return the maximum number
%% @spec maximum(list(number()) -> number()
%% @end
%%-------------------------------------------------------------------
maximum([H|T]) ->
    extrem(T, H, max).

%%-------------------------------------------------------------------
%% @doc Takes a list of numbers and return the list with minimum
%%      and maximum numbers
%% @spec range(list(number()) -> list(number())
%% @end
%%-------------------------------------------------------------------
range(ListOfNumbers) ->
    [minimum(ListOfNumbers), maximum(ListOfNumbers)].

%%-------------------------------------------------------------------
%% @doc Helper function that takes a list of numbers and return the
%%      extremum value depends on third parameter ('min' or 'max')
%% @spec extrem(list(number()), number(), atom()) -> number()
%% @end
%%-------------------------------------------------------------------
extrem([], ResIter, _) ->
    ResIter;

extrem([H|T], ResIter, Direction) ->
    case Direction of
        min ->
            BoolRes = H < ResIter;
        max ->
            BoolRes = H > ResIter
    end,

    case BoolRes of
        true -> Res = H;
        false -> Res = ResIter
    end,

    extrem(T, Res, Direction).
