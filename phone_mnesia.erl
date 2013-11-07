%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 10-2: Using Mnesia
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(phone_mnesia).
-include("phone_records.hlr").
-include_lib("stdlib/include/qlc.hrl").
-export([setup/2, summary/3]).

%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc Reads the .csv files with records data, creates the mnesia 
%%      tables and fills them with file's data.
%% @spec setup(string(), string()) -> atom()
%% @end
%%-------------------------------------------------------------------
setup(CallsData, CustomersData) ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    fill_table(phone_calls, CallsData, record_info(fields, phone_calls), bag),
    fill_table(customers, CustomersData, record_info(fields, customers), set).


%%-------------------------------------------------------------------
%% @doc Produes a tuple that contains the person's phone number,
%%      total number of minutes and total cost for those minutes
%% @spec summary(string(), string(), string()) -> tuple(string(), integer(), float())
%% @end
%%-------------------------------------------------------------------
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



%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc Calculates the number of minutes in list for one phone number
%% @spec subtotal(list(record()), integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
subtotal(Item, Accumulator) ->
    StartSeconds = calendar:datetime_to_gregorian_seconds(
            {string_to_tuple(Item#phone_calls.starting_date),
             string_to_tuple(Item#phone_calls.starting_time)}),
    EndSeconds = calendar:datetime_to_gregorian_seconds(
                {string_to_tuple(Item#phone_calls.end_date),string_to_tuple(Item#phone_calls.end_time)}),
    Accumulator + ((EndSeconds - StartSeconds + 59) div 60).

    
%%-------------------------------------------------------------------
%% @doc Makes a transaction of Query and handles aborting cases
%% @spec make_transaction(query_handle()) -> record()
%% @end
%%-------------------------------------------------------------------
make_transaction(Query) ->
    Fun = fun() -> qlc:e(Query) end,
    case mnesia:transaction(Fun) of
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]);
        {atomic, Res} ->
            Res
    end.


%%-------------------------------------------------------------------
%% @doc Fills a mnesia table with data from files
%% @spec fill_table(atom(), string(), atrribute(), atom()) -> atom()
%% @end
%%-------------------------------------------------------------------
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


%%-------------------------------------------------------------------
%% @doc Reads a file line by line and assemble a list of records fileds
%% @spec subtotal(atom(), io:device(), list(tuple())) -> atom()
%% @end
%%-------------------------------------------------------------------
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
