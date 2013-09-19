%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercise. Simple module and function.
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(geom).
-export([area/3]).
-author('ivryakhov').
-revision('Revision: 1.1').
-created('Date: 2013/09/13').
-modified('Date: 2013/09/19').
-created_by('ivryakhov').

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
        ellipse     -> A * B * math:pi()
    end.
