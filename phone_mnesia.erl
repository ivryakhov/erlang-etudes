-module(phone_mnesia).
-include("phone_records.hlr").
-include_lib("stdlib/include/qlc.hrl").
-export([setup/2, summary/3]).

setup(CallsData, CustomersData) ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    fill_table(phone_calls, CallsData, record_info(fields, phone_calls), bag),
    fill_table(customers, CustomersData, record_info(fields, customers), set).

summary(LastName, FirstName, MiddleName) ->
    Query1 = qlc:q([Customer ||
                Customer <- mnesia:table(customers),
                Customer#customers.last_name == LastName,
                Customer#customers.first_name == FirstName,
                Customer#customers.middle_name == MiddleName]
            ),
    [Customer|_] = make_transaction(Query1),

    Query2 = qlc:q([Call || 
                Call <- mnesia:table(phone_calls),
                Call#phone_calls.phone_number =:= Customer#customers.phone_number]
            ),
    Calls = make_transaction(Query2),

    TotalMinutes = lists:foldl(fun subtotal/2, 0, Calls),
    {RatePaid, _} = string:to_float(Customer#customers.rate_paid),

    [{Customer#customers.phone_number, 
      TotalMinutes,
      TotalMinutes * RatePaid}].

subtotal(Item, Accumulator) ->
    StartSeconds = calendar:datetime_to_gregorian_seconds(
            {string_to_tuple(Item#phone_calls.starting_date),
             string_to_tuple(Item#phone_calls.starting_time)}),
    EndSeconds = calendar:datetime_to_gregorian_seconds(
                {string_to_tuple(Item#phone_calls.end_date),string_to_tuple(Item#phone_calls.end_time)}),
    Accumulator + ((EndSeconds - StartSeconds + 59) div 60).
    
make_transaction(Query) ->
    Fun = fun() -> qlc:e(Query) end,
    case mnesia:transaction(Fun) of
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]);
        {atomic, Res} ->
            Res
    end.

fill_table(TableName, FileToRead, RecordInfo, TableType) ->
    mnesia:delete_table(TableName),
    mnesia:create_table(TableName, [{attributes, RecordInfo}, {type, TableType}]),

    case file:open(FileToRead, [read]) of
        {ok, InputFile} ->
            DataTable = process_file(TableName, InputFile, []),
            file:close(InputFile),
            mnesia:clear_table(TableName),
            F = fun() ->
                        lists:foreach(fun mnesia:write/1, DataTable)
            end,
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
