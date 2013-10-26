%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 7-5: Multiple Generators 
%%%         in List Comprehensions, Etude 7-6: Explaining an Algorithm
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(cards).
-export([make_deck/0,shuffle/1, start_the_game/0, dealer/3, player/2]).
-revision('Revision: 0.1').
-created('Date: 2013/10/22').
-modified('Date: 2013/10/22').
-created_by('ivryakhov').

start_the_game() ->
    PlayersNames = ["Andrea", "Bertram"],
    Dealer = spawn(cards, dealer, [[], [], []]),
    Dealer ! {start_the_game, PlayersNames}.

dealer(Pile, BottomCards, Players) ->
    Dealer = self(),
    receive
        {start_the_game, PlayersNames} ->
            io:format("Dealer: starting the game...~n"),
            Deck = shuffle(make_deck()),
            io:format("Dealer: the deck is: ~p~n", [Deck]),
            NumCards = round(length(Deck) / length(PlayersNames)),
            NewPlayers = spawning_players(PlayersNames, Deck, NumCards, []),
            Dealer ! {pre_battle, NewPlayers},
            dealer([], [], NewPlayers);
        {pre_battle, Players} ->
            io:format("Dealer: pile is: ~p~n", [Pile]),
            io:format("Dealer: ~p give me your cards:~n", [Players]),
            case Pile of
                [] -> CardsNumber = 1;
                _  -> CardsNumber = 3
            end,
            lists:map(fun({_Name, PlayerId}) -> PlayerId ! {give_me_cards, CardsNumber, Dealer} end,
                      Players),
            dealer(Pile, [], Players);
        {awaiting_battle} ->
            case length(BottomCards) < length(Players) of
                true ->
                    io:foprmat("Dealer: pile is: ~p~n", [Pile]),
                    dealer(Pile, BottomCards, Players);
                false ->
                    io:format("Dealer: all cards received.~nPile:~p~nBottomCards: ~p~n",
                              [Pile, BottomCards]),
                    Dealer ! {check_cards},
                    dealer(Pile, BottomCards, Players)
            end;
        {check_cards} ->
            Winners = find_winner(BottomCards, 0, []),
            io:format("Dealer: battle winners: ~p~n", [Winners]);
            
        {here_are_cards, Name, TopCards} ->
            io:format("Dealer: ~s have sent ~p~n", [Name, TopCards]),
            BottomCard = lists:last(TopCards),
            Dealer ! {awaiting_battle},
            dealer(Pile ++ [TopCards], BottomCards ++ [{Name, BottomCard}], Players)
    end.

player(Name, Cards) ->
    receive
        {start_the_game} ->
            io:format("~s: I am ready for the game. My cards: ~p~n", [Name, Cards]),
            player(Name, Cards);
        {give_me_cards, CardsNumber, Dealer} ->
            {TopCards, Rest} = lists:split(CardsNumber, Cards),
            io:format("~s: Here is my card: ~p~n", [Name, TopCards]),
            Dealer ! {here_are_cards, Name, TopCards}
    end.

spawning_players([], _, _, PlayersId) ->
    PlayersId;
spawning_players([CurPlayer|Others], Deck, NumCards, Players) ->
    {PlayerCards,RestOfDeck} = lists:split(NumCards, Deck),
    CurPlayerId = spawn(cards, player, [CurPlayer, PlayerCards]),
    CurPlayerId ! {start_the_game},
    spawning_players(Others, RestOfDeck, NumCards, Players ++ [{CurPlayer, CurPlayerId}]).

find_winner([], _BestCard, Winners) -> Winners;
find_winner([CurPlayerCard | RestOfPlayers], BestCard, Winners) ->
    {Name, {Rank, _Suite}} = CurPlayerCard,
    RankInt = convert_rank_to_int(is_integer(Rank), Rank),
    if
        RankInt > BestCard ->
            UpdateBestCard = RankInt,
            UpdateWinners = [Name];
        RankInt == BestCard ->
            UpdateWinners = Winners ++ [Name],
            UpdateBestCard = BestCard;
        true ->
           UpdateBestCard = BestCard,
           UpdateWinners = Winners
    end,
    find_winner(RestOfPlayers, UpdateBestCard, UpdateWinners).
    
convert_rank_to_int(true,  Rank) -> Rank;
convert_rank_to_int(false, Rank) ->
    case hd(Rank) of
        $J -> 11;
        $Q -> 12;
        $K -> 13;
        $A -> 14
    end.
    


%%-------------------------------------------------------------------
%% @doc Generates a deck of cards as a list 52 tuples
%% @spec make_deck() -> list(tuple(string()|integer(), string()))
%% @end
%%-------------------------------------------------------------------
make_deck() ->
    Suits = ["Clubs", "Diamonds", "Hearts", "Spades"],
    %Ranks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"],
    Ranks = ["A", 2, 3, 4],
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
