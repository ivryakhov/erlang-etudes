%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 7-5: Multiple Generators 
%%%         in List Comprehensions, Etude 7-6: Explaining an Algorithm,
%%%         Etude 8-1: Using Processes to Simulate a Card Game
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(cards).
-export([start_the_game/0, dealer/3, player/2]).
-revision('Revision: 0.4').
-created('Date: 2013/10/22').
-modified('Date: 2013/10/28').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Starts the game with variuos players number that less than number
%%      of cards in deck of course. Players names and it's number are
%%      defined in PlayersNames list.
%% @spec start_the_game() -> atom()
%% @end
%%-------------------------------------------------------------------
start_the_game() ->
    PlayersNames = ["Andrea", "Bertram", "Bill", "Alice"],
    Dealer = spawn(cards, dealer, [[], [], []]),
    Dealer ! {start_the_game, PlayersNames}.

%%-------------------------------------------------------------------
%% @doc The agent that process the game. It controls the state of the game,
%%      comminucation with players, cards assigment and game completion.
%%      It receives control messages:
%%          {start_the_game, PlayersNames} -- to start the game with list of
%%                  players names. Denerates the deck, split it for some parts
%%                  that is number of players and spawn the player processes.
%%          {pre_battle} -- check the pile and decides if it is battle or war,
%%                  sends the message to give the number of cards to each player
%%                  depends of round type (battle of war)
%%          {awaiting_battle} -- waiting cards from each player. If some player do
%%                  not have any more cards, sent message to him to left the game.
%%                  Starting cards checking when all cards received from all players
%%                  with non-empty cards list.
%%          {check_cards} -- to check cards and define the winner by comparise of 
%%                  bottom cards. If winner is one, send message to him to get pile.
%%                  It there are some winners, it is a war, going to pre_battle stage.
%%                  If there is only one player, he is the winner of the game.
%% @spec dealer(list(tuple()), list(tuple(tuple())), list(tuple())) -> atom()
%% @end
%%-------------------------------------------------------------------
dealer(Pile, BottomCards, Players) ->
    Dealer = self(),
    receive
        {start_the_game, PlayersNames} ->
            random:seed(now()),
            io:format("Dealer: starting the game...~n"),
            Deck = shuffle(make_deck()),
            io:format("Dealer: the deck is: ~p~n", [Deck]),
            NumCards = round(length(Deck) / length(PlayersNames)),
            NewPlayers = spawning_players(PlayersNames, Deck, NumCards, []),
            Dealer ! {pre_battle},
            dealer([], [], NewPlayers);

        {pre_battle} ->
            io:format("Dealer: pile is: ~p~n", [Pile]),
            io:format("Dealer: ~p give me your cards:~n", [Players]),
            case Pile of
                [] -> CardsNumber = 1;
                _  -> CardsNumber = 3
            end,
            lists:map(fun({_Name, PlayerId}) -> PlayerId ! {give_me_cards, CardsNumber, Dealer} end,
                      Players),
            dealer(Pile, [], Players);

        {awaiting_battle, {Name, PlayerPid}, TopCards} ->
            case TopCards of
                [] ->
                    io:format("Dealer: ~s don't have more cards and left the game.~n", [Name]),
                    PlayerPid ! {left_the_game},
                    NewPlayers = lists:delete({Name, PlayerPid}, Players),
                    NewBottomCards = BottomCards,
                    NewPile = Pile;
                _ ->
                    io:format("Dealer: ~s have sent ~p~n", [Name, TopCards]),
                    NewPile = lists:append(Pile,TopCards),
                    BottomCard = lists:last(TopCards),
                    NewBottomCards = BottomCards ++ [{{Name, PlayerPid}, BottomCard}],
                    NewPlayers = Players
            end,
            case length(NewBottomCards) < length(NewPlayers) of
                true ->
                    dealer(NewPile, NewBottomCards, NewPlayers);
                false ->
                    io:format("Dealer: all cards received.~nPile:~p~nBottomCards: ~p~n",
                              [NewPile, NewBottomCards]),
                    Dealer ! {check_cards},
                    dealer(NewPile, NewBottomCards, NewPlayers)
            end;

        {check_cards} ->
            case length(Players) of
                0 ->
                    io:format("Dealer: there is no winners!~n");
                1 ->
                    {Name, _PlayerId} = hd(Players),
                    io:format("Dealer: the winner is ~s. My congratulations!~n", [Name]);
                _ ->
                    Winners = find_winner(BottomCards, 0, []),
                    io:format("Dealer: battle winners: ~p~n", [Winners]),
                    case length(Winners) of
                        1 ->
                            {Name, PlayerPid} = hd(Winners),
                            io:format("Dealer: the winner is ~s, Take your cards.~n", [Name]),
                            PlayerPid ! {take_your_cards, Pile},
                            Dealer ! {pre_battle},
                            dealer([], [], Players);
                        _ ->
                            io:format("Dealer: It is a war. Give me 3 cards elese~n"),
                            Dealer ! {pre_battle},
                            dealer(Pile, [], Players)
                    end
            end

    end.

%%-------------------------------------------------------------------
%% @doc player process that contolrs a behaviour of players.
%%      Received messages:
%%          {start_the_game} -- to write a message with one's name
%%              and cards in hands.
%%          {give_me_cards} -- to get the number (CardsNumber) of cards
%%              from hands and get it ti dealer.
%%          {take_your_cards} -- to get all cards from pile in case of
%%              this player is winner.
%%          {left_the_game} -- to left the game and finish one's process
%% @spec player(string(), list(tuple())) -> atom()
%% @end
%%-------------------------------------------------------------------
player(Name, Cards) ->
    receive
        {start_the_game} ->
            io:format("~s: I am ready for the game. My cards: ~p~n", [Name, Cards]),
            player(Name, Cards);
        {give_me_cards, CardsNumber, Dealer} ->
            case Cards of
                [] ->
                    TopCards = [],
                    Rest = [],
                    io:format("~s: I have no more cards.~n", [Name]);
                _ ->
                    case length(Cards) >= CardsNumber of
                        true ->
                            {TopCards, Rest} = lists:split(CardsNumber, Cards);
                        false ->
                            TopCards = Cards,
                            Rest = []
                    end,
                    io:format("~s: Here is my card: ~p~n", [Name, TopCards])
            end,
            Dealer ! {awaiting_battle, {Name, self()}, TopCards},
            player(Name, Rest);
        {take_your_cards, NewCards} ->
            UpdatedCards = lists:append([Cards, NewCards]),
            io:format("~s: Here is my cards now: ~p~n", [Name, UpdatedCards]),
            player(Name, UpdatedCards);
        {left_the_game} ->
            io:format("~s: Im leaving the game. Bye.~n", [Name])
    end.

%%-------------------------------------------------------------------
%% @doc spawns the number of players processes. Each player should take
%%      the number of curds to one's hand. Returns the list of players
%%      with one's names and process id.
%% @spec spawning_players(list(string()), list(tuple()), integer(), list(tuple())) -> atom()
%% @end
%%-------------------------------------------------------------------
spawning_players([], _, _, PlayersId) ->
    PlayersId;
spawning_players([CurPlayer|Others], Deck, NumCards, Players) ->
    {PlayerCards,RestOfDeck} = lists:split(NumCards, Deck),
    CurPlayerId = spawn(cards, player, [CurPlayer, PlayerCards]),
    CurPlayerId ! {start_the_game},
    spawning_players(Others, RestOfDeck, NumCards, Players ++ [{CurPlayer, CurPlayerId}]).

%%-------------------------------------------------------------------
%% @doc comparise the cards from players by rank. The best rank defines the winner.
%%      If some players have a cards with equal rank, so there are some winners.
%% @spec find_winner(list(tuple()), tuple(), list(tuple())) -> list(tuple())
%% @end
%%-------------------------------------------------------------------
find_winner([], _BestCard, Winners) -> Winners;
find_winner([CurPlayerCard | RestOfPlayers], BestCard, Winners) ->
    {Name, {Rank, _Suite}} = CurPlayerCard,
    io:format("IVAN Rank is: ~p~n", [Rank]),
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
    
%%-------------------------------------------------------------------
%% @doc convers a rank of card to integer to help to comparise it with others
%% @spec convert_rank_to_int(boolean(), integer()/string()) -> integer()
%% @end
%%-------------------------------------------------------------------
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
    Ranks = ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
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
