%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 6-5: Random Numbers; Generating Lists of Lists
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(non_fp).
-export([generate_teeth/2]).
-revision('Revision: 0.1').
-created('Date: 2013/10/17').
-modified('Date: 2013/10/17').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Generates a list of lists of six numers meaning pocket depths
%% @spec generate_teeth(string(), float()) -> list(list(integer()))
%% @end
%%-------------------------------------------------------------------
generate_teeth(ToothPresence, Probability) ->
    random:seed(now()),
    generate_teeth(ToothPresence, Probability, []).

%%-------------------------------------------------------------------
%% @doc Helper function that process generating tooth accumulation
%% @spec generate_teeth(string(), float(), list(list(integer())) -> list(list(integer()))
%% @end
%%-------------------------------------------------------------------
generate_teeth([], _, IterList) ->
    IterList;
generate_teeth([$T|OtherTooths], Probability, IterList) ->
    generate_teeth(OtherTooths, Probability,
                   IterList ++ [generate_tooth(Probability)]);
generate_teeth([$F|OtherTooths],  Probability, IterList) ->
    generate_teeth(OtherTooths, Probability,
                   IterList ++ [0]).
    

%%-------------------------------------------------------------------
%% @doc Generates he list of numbers for a single tooth
%% @spec generate_tooth(float()) -> list(integer())
%% @end
%%-------------------------------------------------------------------
generate_tooth(Probability) ->
    case random:uniform() < Probability of
        true -> BaseDepth = 2;
        false -> BaseDepth = 3
    end,
    generate_tooth(BaseDepth, 6, []).

%%-------------------------------------------------------------------
%% @doc Helper function that process single tooth numbers accumulation
%% @spec generate_tooth(integer(), float(), list(integer()) -> list(integer())
%% @end
%%-------------------------------------------------------------------
generate_tooth(_, 0, IterList) ->
    IterList;
generate_tooth(BaseDepth, NumberLeft, IterList) ->
    generate_tooth(BaseDepth, NumberLeft - 1, 
                   IterList ++ [BaseDepth + (random:uniform(3) - 2)]).
