-module(phone_mnesia).
-include("phone_records.hlr").
-export([setup/2]).

setup(CallsData, CustomersData) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    fill_table(phone_calls, CallsData, record_info(fields, calls)),
    fill_table(customers_info, CustomersData, record_info(fields, customers)),
    mnesia:stop().

fill_table(TableName, FileToRead, RecordInfo) ->
    mnesia:create_table(TableName, [{attributes, RecordInfo}]),
    case file:open(FileToRead, [read]) of
        {ok, InputFile} ->
            DataTable = process_file(TableName, InputFile, []),
            io:format("DataTable: ~p~n", [DataTable]),
            file:close(InputFile),
            mnesia:clear_table(TableName),
            F = fun() -> mnesia:write(DataTable) end,
            mnesia:transaction(F);
        {error, Why} ->
            {error, Why}
    end.

process_file(TableName, FileName, ResultList) ->
    Line = io:get_line(FileName, ""),
    case Line of
        eof ->
            ResultList;
        _   ->
            SplitedList = [TableName] ++ re:split(Line, "[,\n]", [trim, {return, list}]),
            ResultTuple = list_to_tuple(SplitedList),
            process_file(TableName, FileName, ResultList ++ [ResultTuple])
    end.
