-module(phone_ets).
-include("phone_records.hlr").
-export([setup/1]).

setup(FileName) ->
    case file:open(FileName, [read]) of
        {ok, InputFile} ->
            TableId = ets:new(calls, [bag]),
            process_file(InputFile, TableId);
        {error, Why} ->
            {error, Why}
    end.

process_file(InputFile, TableId) ->
