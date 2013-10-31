-module(phone_ets).
-include("phone_records.hlr").
-export([setup/1]).

setup(FileName) ->
    case file:open(FileName, [read]) of
        {ok, InputFile} ->
            TableId = ets:new(phone_calls, [bag]),
            process_file(io:get_line(InputFile, ""), InputFile, TableId),
            file:close(InputFile);
        {error, Why} ->
            {error, Why}
    end.

process_file(eof, _, _) ->
    ok;
process_file(Line, InputFile, TableId) ->
    [PhoneNumber, StartingDate, StartingTime, EndDate, EndTime]
         = re:split(Line, "[,\n]", [trim, {return, list}]),
    ets:insert(TableId, #calls{phone_number = PhoneNumber, starting_date = string_to_tuple(StartingDate),
            starting_time = string_to_tuple(StartingTime),
            end_date = string_to_tuple(EndDate), end_time = string_to_tuple(EndTime)}), 
    process_file(io:get_line(InputFile, ""), InputFile, TableId).

string_to_tuple(String) ->
    StrNum = re:split(String, "[-:]", [{return, list}]),
    [Num1, Num2, Num3] = lists:map(fun(Str) -> {Num, _Rest} = string:to_integer(Str), Num end, StrNum),
    {Num1, Num2, Num3}.
