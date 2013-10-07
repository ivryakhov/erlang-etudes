%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Chapter 4. Logic and Recursion
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(powers).
-export([raise/2, nth_root/2]).
-author('ivryakhov').
-revision('Revision: 0.2').
-created('Date: 2013/10/07').

%%-------------------------------------------------------------------
%% @doc takes parameters X and N and returns the value of X^N
%% @spec raise(X::number(), N::integer()) -> number()
%% @end
%%-------------------------------------------------------------------
raise(_X, 0) -> 1;
raise(X, N) when N > 0 -> raise(X, N, 1);
raise(X, N) when N < 0 -> 1.0 / raise(X, -N).

%%-------------------------------------------------------------------
%% @doc helper function for raise/2 function for tail recursive evaluating
%%      of X^N with iterated accumulator
%% @spec raise(X::number(), N::integer(), Accumulator:integer()) -> number()
%% @end
%%-------------------------------------------------------------------
raise(_X, 0, Accumulator) -> Accumulator;
raise(X, N, Accumulator) -> raise(X, N-1, X * Accumulator).


%%-------------------------------------------------------------------
%% @doc calculates the Nth root of an X number
%%      of X^N with iterated accumulator
%% @spec raise(X::number(), N::integer()) -> number()
%% @end
%%-------------------------------------------------------------------
nth_root(_X, N) when N < 0 -> 0;
nth_root(X, N) -> nth_root(X, N, X / 2.0).

%%-------------------------------------------------------------------
%% @doc helper function for nth_root/2 for tail recursive evaluating
%%      of Nth root of an X number with guess approximation
%% @spec raise(X::number(), N::integer()) -> number()
%% @end
%%-------------------------------------------------------------------
nth_root(X, N, A) ->
    io:format("Current guess is ~p~n", [A]),
    F = raise(A, N) - X,
    Fprime = N * raise(A, N - 1),
    Next = A - F / Fprime,
    Change = abs(Next - A),
    if 
        Change < 1.0e-8 -> Next;
        true -> nth_root(X, N, Next)
    end.
