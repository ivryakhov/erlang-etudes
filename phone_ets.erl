%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 10-1: Using ETS
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(phone_ets).
-include("phone_records.hlr").
-export([setup/1, summary/1, summary/0]).
-author('ivryakhov').
-revision('Revision: 0.1').
-created('Date: 2013/11/05').
-modified('Date: 2013/11/05').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Opens the .csv file with records data, creates the phone_call
%%      ets named table and fill it with file's data.
%% @spec setup(string()) -> atom()/tuple(error, string())
%% @end
%%-------------------------------------------------------------------
setup(FileName) ->
    case file:open(FileName, [read]) of
        {ok, InputFile} ->
            case ets:info(phone_calls) of
                undefined -> false;
                _ -> ets:delete(phone_calls)
            end,

            ets:new(phone_calls, [named_table, bag,  {keypos, #calls.phone_number}]),
            process_file(io:get_line(InputFile, ""), InputFile),
            file:close(InputFile);
        {error, Why} ->
            {error, Why}
    end.

%%-------------------------------------------------------------------
%% @doc Returns a summary of minutes number for give phone number
%% @spec summary(string()) -> list(tuple(string(), integer())
%% @end
%%-------------------------------------------------------------------
summary(PhoneNumber) ->
    Summ = process_calls(ets:lookup(phone_calls, PhoneNumber), 0),
    [{PhoneNumber, Summ}].

%%-------------------------------------------------------------------
%% @doc Returns a summary of minutes number for all numbers
%% @spec summary() -> list(tuple(string(), integer())
%% @end
%%-------------------------------------------------------------------
summary() ->
    summary(ets:tab2list(phone_calls), []).

%%-------------------------------------------------------------------
%% @doc Helper function for setup/0 that process the all phone numbers
%%      in phone_calls ets table
%% @spec summary(list(record()), list(tuple(string(), integer())) -> list(tuple(string(), integer())
%% @end
%%-------------------------------------------------------------------
summary([], ResultList) ->
    ResultList;
summary([H|T], ResultList) ->
    Key = H#calls.phone_number,
    case lists:keyfind(Key, 1, ResultList) of
            false ->
                CurRes = ResultList ++ summary(Key);
            _ ->
                CurRes = ResultList
    end,
    summary(T, CurRes).

%%-------------------------------------------------------------------
%% @doc Process input file line by line and inserts record values to
%%      phone_calls ets table
%% @spec process_file(string()/atom(), file:io_device()) -> atom()
%% @end
%%-------------------------------------------------------------------
process_file(eof, _) ->
    ok;
process_file(Line, InputFile) ->
    [PhoneNumber, StartingDate, StartingTime, EndDate, EndTime]
         = re:split(Line, "[,\n]", [trim, {return, list}]),
    ets:insert(phone_calls, #calls{phone_number = PhoneNumber,
                                   starting_date = string_to_tuple(StartingDate),
                                   starting_time = string_to_tuple(StartingTime),
                                   end_date = string_to_tuple(EndDate),
                                   end_time = string_to_tuple(EndTime)}), 
    process_file(io:get_line(InputFile, ""), InputFile).

%%-------------------------------------------------------------------
%% @doc Converst input string with format "yyyy-mm-dd" or "hh:mm:ss"
%%      to tuple with 3 integers
%% @spec string_to_tuple(string()) -> tuple(integer(), integer(), integer())
%% @end
%%-------------------------------------------------------------------
string_to_tuple(String) ->
    StrNum = re:split(String, "[-:]", [{return, list}]),
    [Num1, Num2, Num3] = lists:map(fun(Str) -> {Num, _Rest} = string:to_integer(Str), Num end, StrNum),
    {Num1, Num2, Num3}.

%%-------------------------------------------------------------------
%% @doc Calculates the number of minutes in list for one phone number
%% @spec process_calls(list(record()), integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
process_calls([], Accum) ->
    Accum;
process_calls([H|T], Accum) ->
    StartInSec = calendar:datetime_to_gregorian_seconds({H#calls.starting_date, H#calls.starting_time}),
    EndInSec = calendar:datetime_to_gregorian_seconds({H#calls.end_date, H#calls.end_time}),
    process_calls(T, Accum + ((EndInSec - StartInSec + 59) div 60)).
