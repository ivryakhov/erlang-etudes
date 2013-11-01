-module(phone_ets).
-include("phone_records.hlr").
-export([setup/1, summary/1]).

setup(FileName) ->
    case file:open(FileName, [read]) of
        {ok, InputFile} ->
            ets:new(phone_calls, [named_table, bag, {keypos, #calls.phone_number}]),
            process_file(io:get_line(InputFile, ""), InputFile),
            file:close(InputFile);
        {error, Why} ->
            {error, Why}
    end.

summary(PhoneNumber) ->
    CallsList = ets:lookup(phone_calls, PhoneNumber),
    process_calls(CallsList, 0).

process_file(eof, _) ->
    ok;
process_file(Line, InputFile) ->
    [PhoneNumber, StartingDate, StartingTime, EndDate, EndTime]
         = re:split(Line, "[,\n]", [trim, {return, list}]),
    ets:insert(phone_calls, #calls{phone_number = PhoneNumber, starting_date = string_to_tuple(StartingDate),
            starting_time = string_to_tuple(StartingTime),
            end_date = string_to_tuple(EndDate), end_time = string_to_tuple(EndTime)}), 
    process_file(io:get_line(InputFile, ""), InputFile).

string_to_tuple(String) ->
    StrNum = re:split(String, "[-:]", [{return, list}]),
    [Num1, Num2, Num3] = lists:map(fun(Str) -> {Num, _Rest} = string:to_integer(Str), Num end, StrNum),
    {Num1, Num2, Num3}.

process_calls([H|T], Summ) ->
    io:format("Calls: ~p~n", [H]).
