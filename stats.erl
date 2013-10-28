%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Chapter 6. Lists, 
%%%         Etude 7-3: Using lists:foldl/3, Etude 9-1: try and catch
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(stats).
-export([minimum/1, maximum/1, range/1, mean/1, stdv/1]).
-revision('Revision: 0.2').
-created('Date: 2013/10/15').
-modified('Date: 2013/10/28').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% API functions
%%-------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc Takes a list of numbers and return the minumim number
%% @spec minimum(list(number()) -> number()
%% @end
%%-------------------------------------------------------------------
minimum([]) ->
    {error,badarg};
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
%% @doc Calculates the mean for lits of numbers
%% @spec mean(list(number()) -> float()
%% @end
%%-------------------------------------------------------------------
mean(NumbersList) ->
    try lists:foldl(fun(X, Sum) -> X + Sum end, 0, NumbersList) / length(NumbersList)
    catch
        error:Error -> {error, Error}
    end.

%%-------------------------------------------------------------------
%% @doc Calculates the standart deviation for lits of numbers
%% @spec stdv(list(number()) -> float()
%% @end
%%-------------------------------------------------------------------
stdv(NumbersList) ->
    try
        N = length(NumbersList),
        {Sum, SqrSum} = lists:foldl(fun(X, {SumIter, SqrSumIter}) -> {X + SumIter, X * X + SqrSumIter} end,
                                    {0, 0}, NumbersList),
        math:sqrt((SqrSum * N - Sum * Sum) / (N * (N - 1)))
    catch
        error:Error -> {error, Error}
    end.
    

%%-------------------------------------------------------------------
%% Private functions
%%-------------------------------------------------------------------

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
