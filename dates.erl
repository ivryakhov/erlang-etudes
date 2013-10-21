%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 5-2: Using the re Module,
%%%         Etude 7-4: Using lists:split/2
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(dates).
-export([date_parts/1, julian/1]).
-revision('Revision: 0.2').
-created('Date: 2013/10/14').
-modified('Date: 2013/10/16').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Takes a string in ISO date format ("yyyy-mm-dd") and returns 
%%      the Julian date: the day of the year
%% @spec julian(string()) -> integer()
%% @end
%%-------------------------------------------------------------------
julian(Date) ->
    [Year, Month, Day ] = date_parts(Date),
    DaysPerMonth = [31, days_in_feb(Year), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
    julian(Month, Day, DaysPerMonth).

%%-------------------------------------------------------------------
%% @doc Helper function for julian/1 that takes 4 arguments:
%%      month, day, list of days per month and total days count.
%%      Return the number of day in a year.
%% @spec julian(integer(), integer, list(integer()), integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
julian(Month, Day, DaysPerMonth) when Month < 13 ->
    { MonthsUntilCurrent , _OtherMonths } = lists:split(Month - 1, DaysPerMonth),
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, MonthsUntilCurrent) + Day;

julian(_, _, _) ->
    io:format("Wrong input data~n").

%%-------------------------------------------------------------------
%% @doc Takes a year number and returns the number days in Febriary
%% @spec days_in_feb(integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
days_in_feb(Year) ->
    case (Year rem 4 == 0 andalso Year rem 100 /= 0)
          orelse (Year rem 400 == 0) of
        true -> 29;
        false -> 28
    end.

%%-------------------------------------------------------------------
%% @doc Takes a string in ISO date format ("yyyy-mm-dd") and returns 
%%      a list of integers in the form [yyyy, mm, dd]
%% @spec date_parts(string()) -> list(integer())
%% @end
%%-------------------------------------------------------------------
date_parts(Date) ->
    date_parts(re:split(Date,"-",[{return,list}]), []).

%%-------------------------------------------------------------------
%% @doc  Helper function to convert list of strings to list of integers
%% @spec date_parts(list(string()), list()) -> list(integer())
%% @end
%%-------------------------------------------------------------------
date_parts([], Res) ->
    Res;
date_parts([H| T], Res) -> 
    {Int, _Rest} = string:to_integer(H),
    date_parts(T, lists:append(Res, [Int])).
