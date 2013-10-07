%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Chapter 4. Logic and Recursion
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(powers).
-export([raise/2]).
-author('ivryakhov').
-revision('Revision: 0.1').
-created('Date: 2013/10/07').

%%-------------------------------------------------------------------
%% @doc takes parameters X and N and returns the value of X^N
%% @spec raise(X::number(), N::integer()}) -> number()
%% @end
%%-------------------------------------------------------------------
raise(_X, 0) -> 1;
raise(X, 1) -> X;
raise(X, N) when N > 0 -> X * raise(X, N - 1);
raise(X, N) when N < 0 -> 1.0 / raise(X, -N).
