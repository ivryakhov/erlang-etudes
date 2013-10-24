%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 7-5: Multiple Generators 
%%%         in List Comprehensions, Etude 7-6: Explaining an Algorithm
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(cards).
-export([make_deck/0,shuffle/1, start_the_game/0, dealer/0, player/2]).
-revision('Revision: 0.1').
-created('Date: 2013/10/22').
-modified('Date: 2013/10/22').
-created_by('ivryakhov').

start_the_game() ->
    Dealer = spawn(cards, dealer, []),
    Players = ["Andrea", "Bertram"],
    Dealer ! {start_the_game, Players}.

dealer() ->
    receive
        {start_the_game, Players} ->
            io:format("Dealer: starting the game...~n"),
            Deck = shuffle(make_deck()),
            io:format("Dealer: the deck is: ~p~n", [Deck]),
            NumCards = round(length(Deck) / length(Players)),
            PlayersId = spawning_players(Players, Deck, NumCards, [])
    end.

player(Name, Cards) ->
    receive
        {start_the_game} ->
            io:format("~s: I am ready for the game. My cards: ~p~n", [Name, Cards])
    end.

spawning_players([], _, _, PlayersId) ->
    PlayersId;
spawning_players([CurPlayer|Others], Deck, NumCards, PlayersId) ->
    {PlayerCards,OtherDeck} = lists:split(NumCards, Deck),
    CurPlayerId = spawn(cards, player, [CurPlayer, PlayerCards]),
    CurPlayerId ! {start_the_game},
    spawning_players(Others, OtherDeck, NumCards, PlayersId ++ [CurPlayerId]).


%%-------------------------------------------------------------------
%% @doc Generates a deck of cards as a list 52 tuples
%% @spec make_deck() -> list(tuple(string(), string()))
%% @end
%%-------------------------------------------------------------------
make_deck() ->
    Suits = ["Clubs", "Diamonds", "Hearts", "Spades"],
    %Ranks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"],
    Ranks = ["A", "2", "3", "4"],
    [{Rank, Suit} || Rank <- Ranks, Suit <- Suits].

%%-------------------------------------------------------------------
%% @doc Shuffles a list (deck of cards in this case)
%% @spec shuffle(list()) -> list()
%% @end
%%-------------------------------------------------------------------
shuffle(List) -> shuffle(List, []).

%%-------------------------------------------------------------------
%% @doc helper function for shuffle/1. Takes a random position in an
%%      input list, splits list on two parts, gets the head element
%%      from second part when and put it to accumalation list. Calls
%%      themself after that with concatenation of first part and tail
%%      from second part
%% @spec shuffle(list(), list()) -> list()
%% @end
%%-------------------------------------------------------------------
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).
