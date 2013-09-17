%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercise. Simple module and function.
%%%      http://chimera.labs.oreilly.com/books/1234000000726/ch02.html
%%% @end
%%%-------------------------------------------------------------------
-module(geom).
-export([area/2]).
-author('ivryakhov').
-revision('Revision: 1.0').
-created('Date: 2013/09/13').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Returns the area of rectangle with Length and Width values.
%% @spec area(Length::integer(), Width::integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
area(Length, Width) ->
    Length * Width.
