%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 9-2: Logging Errors
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(bank).
-export([account/1]).
-revision('Revision: 0.1').
-created('Date: 2013/10/29').
-modified('Date: 2013/10/29').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Takes a numberic Balance and repeatly asks for a transaction
%%      (deposit, withdraw, balance inquiry or quit) and performs a
%%      corresponding operation.
%% @spec account(number()) -> atom()
%% @end
%%-------------------------------------------------------------------
account(Balance) ->
    Answer = io:get_line("D)eposit, W)ithdraw, B)alance, Q)uit: "),
    [FirstLetter | _Tail] = Answer,
    case string:to_lower(FirstLetter) of
        $d -> deposit(Balance);
        $w -> withdraw(Balance);
        $b -> balance(Balance);
        $q -> true;
        _  -> io:format("Unknown command ~s~n", [Answer]), 
              account(Balance)
    end.

%%-------------------------------------------------------------------
%% @doc Asks for amount to deposit and performs a transaction
%% @spec deposit(number()) -> none
%% @end
%%-------------------------------------------------------------------
deposit(Balance) ->
    Amount = get_number("Amount to deposit: "),
    if 
        Amount < 0 ->
            error_logger:error_msg("Negative deposit amount ~p~n", [Amount]),
            io:format("Deposits may not be less than zero."),
            NewBalance = Balance;
        Amount >= 10000 ->
            error_logger:warning_msg("Excessive deposit ~p~n", [Amount]),
            io:format("Your deposit of $~p may be subject to hold.", [Amount]),
            NewBalance = Balance + Amount,
            io:format("Your new balance is ~p~n", [NewBalance]);
        Amount >= 0 ->
            error_logger:info_msg("Successful deposit ~p~n", [Amount]),
            NewBalance = Balance + Amount,
            io:format("Your new balance is ~p~n", [NewBalance])
    end,
    account(NewBalance).

%%-------------------------------------------------------------------
%% @doc Asks for amount to withdraw and performs a transaction
%% @spec withdraw(number()) -> none
%% @end
%%-------------------------------------------------------------------
withdraw(Balance) ->
    Amount = get_number("Amount to withdraw: "),
    if
        Amount > Balance ->
            error_logger:error_msg("Overdraw ~p from balance ~p~n", [Amount, Balance]),
            io:format("You cannot withdraw more than your current balance of ~p.~n", [Balance]),
            NewBalance = Balance;
        Amount < 0 ->
            error_logger:error_msg("Negative withdrawal amount ~p~n", [Amount]),
            io:format("Withdrawals may not be less than zero."),
            NewBalance = Balance;
        Amount >= 0 ->
            error_logger:info_msg("Successful withdrawal ~p~n", [Amount]),
            NewBalance = Balance - Amount,
            io:format("Your new balance is ~p~n", [NewBalance])
    end,
    account(NewBalance).

%%-------------------------------------------------------------------
%% @doc Prints a current balance
%% @spec balance(number()) -> none
%% @end
%%-------------------------------------------------------------------
balance(Balance) ->
    error_logger:info_msg("Balance inquiry ~p~n", [Balance]),
    account(Balance).


%%-------------------------------------------------------------------
%% @doc Asks for required dimension and convert it to number
%% @spec get_number(string()) -> number()
%% @end
%%-------------------------------------------------------------------
get_number(Prompt) ->
    Number  = io:get_line(Prompt),
    {Float, _FloatRest} = string:to_float(Number),
    Res = case Float of
        error ->
            {Int, _IntRest} = string:to_integer(Number),
            case Int of
                error ->
                    illegal_value;
                _ ->
                    Int
            end;
        _ ->
            Float
    end,
    Res.
