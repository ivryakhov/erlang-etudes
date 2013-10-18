%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Étude 7-1: Simple Higher Order Functions
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(calculus).
-export([derivative/2]).
-revision('Revision: 0.1').
-created('Date: 2013/10/18').
-modified('Date: 2013/10/18').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Declares the delta constant
%% @spec delta() -> float()
%% @end
%%-------------------------------------------------------------------
delta() -> 1.0e-10.

%%-------------------------------------------------------------------
%% @doc calculate the rate of change of a function by calculating:
%%      (F(X + Delta) - F(X)) / Delta
%% @spec derivative(function(), number()) -> float()
%% @end
%%-------------------------------------------------------------------
derivative(F, Point) ->
    (F(Point + delta()) - F(Point)) / delta().
