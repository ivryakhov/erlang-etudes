%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Chapter 6. Lists, 
%%%         Etude 7-3: Using lists:foldl/3
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(cards).
-export([make_deck/0]).
-revision('Revision: 0.1').
-created('Date: 2013/10/22').
-modified('Date: 2013/10/22').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Generates a deck of cards as a list 52 tuples
%% @spec make_deck() -> list(tuple(string(), string()))
%% @end
%%-------------------------------------------------------------------
make_deck() ->
    Suits = ["Clubs", "Diamonds", "Hearts", "Spades"],
    Ranks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"],
    [{Rank, Suit} || Rank <- Ranks, Suit <- Suits].
