%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Simple module and function.
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(geom).
-export([area/1]).
-author('ivryakhov').
-revision('Revision: 1.2').
-created('Date: 2013/09/13').
-modified('Date: 2013/09/19').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Transforms given tuple to three parameters and
%%      calls the private function area/3
%% @spec area({Shape::atom(), A::number(), B::number()}) -> number()
%% @end
%%-------------------------------------------------------------------
area({Shape, A, B}) -> area(Shape, A, B).


%%-------------------------------------------------------------------
%% @doc Returns the area of the shape declared in a first parameter
%%      using dimentions from two other parameters.
%% @spec area(Shape::atom(), A::number(), B::number) -> number
%% @end
%%-------------------------------------------------------------------
area(Shape, A, B) when A >= 0, B >= 0 ->
    case Shape of
        rectangle   -> A * B;
        triangle    -> A * B / 2.0;
        ellipse     -> A * B * math:pi();
        _   -> 0
    end;
area(_Shape, _A, _B) -> 0.
