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
    ets:insert(TableId, #calls{phone_number = PhoneNumber, starting_date = StartingDate,
            starting_time = StartingTime, end_date = EndDate, end_time = EndTime}), 
    process_file(io:get_line(InputFile, ""), InputFile, TableId).
