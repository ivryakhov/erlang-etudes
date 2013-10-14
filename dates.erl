%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 5-2: Using the re Module
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(dates).
-export([date_parts/1]).
-revision('Revision: 0.1').
-created('Date: 2013/10/14').
-modified('Date: 2013/10/14').
-created_by('ivryakhov').

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
