%%%-------------------------------------------------------------------
%%%%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%%%%% @copyright 2013
%%%%%% @doc 'Etudes for Erlang' exercises. Simple module and function.
%%%%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%%%%% @end
%%%%%%-------------------------------------------------------------------
-module(dijkstra).
-export([gcd/2]).
-author('ivryakhov').
-revision('Revision: 0.1').
-created('Date: 2013/10/04').

%%-------------------------------------------------------------------
%% @doc finds the greatest common divisor (GCD) of two integers
%% @spec area(M::integer(), B::integer()}) -> integer()
%% @end
%%-------------------------------------------------------------------
gcd(M, N) when M == N -> M;
gcd(M, N) when M > N -> gcd(M - N, N);
gcd(M, N) -> gcd(M, N - M).
