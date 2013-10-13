%%%-------------------------------------------------------------------
%%% @author Ryakhov Ivan <ivryakhov@gmail.com>
%%% @copyright 2013
%%% @doc 'Etudes for Erlang' exercises. Etude 5-1: Validating Input
%%%       [http://chimera.labs.oreilly.com/books/1234000000726]
%%% @end
%%%-------------------------------------------------------------------
-module(ask_area).
-export([area/0]).
-revision('Revision: 0.2').
-created('Date: 2013/10/09').
-modified('Date: 2013/10/13').
-created_by('ivryakhov').

%%-------------------------------------------------------------------
%% @doc Asks for the first letter of the shape and appropriative
%%      dimensions. Retursh the area of shape
%% @spec area() -> number()
%% @end
%%-------------------------------------------------------------------
area() ->
    Answer = io:get_line("R)ectangle, T)riangle, E)llipse > "),
    case char_to_shape(Answer) of
        {ok, Shape} -> compile_area_request(Shape);
        {nok, unknown_shape} ->
            io:format("Unknow shape: ~s", [Answer])
    end.

%%-------------------------------------------------------------------
%% @doc Cheks the enetered string fo available dimensions
%% @spec shar_to_shape(string()) -> {atom(), atom()}
%% @end
%%-------------------------------------------------------------------
char_to_shape(Char) ->
    [Head | _Tail] = Char,
    case string:to_lower(Head) of
        $r -> {ok, rectangle};
        $t -> {ok, triangle};
        $e -> {ok, ellipse};
        _ -> {nok, unknown_shape}
    end.

%%-------------------------------------------------------------------
%% @doc Asks for required dimension and convert it to number
%% @spec get_number(string()) -> number()
%% @end
%%-------------------------------------------------------------------
get_number(Prompt) ->
    Number  = io:get_line("Enter " ++ Prompt ++ ": "),
    {Float, _FloatRest} = string:to_float(Number),
    Res =case Float of
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


%%-------------------------------------------------------------------
%% @doc Compile the request for external geom:area/1 function
%% @spec shar_to_shape(atom()) -> number()
%% @end
%%-------------------------------------------------------------------
compile_area_request(Shape) ->
    Numbers =case Shape of
        rectangle -> get_dimensions("width", "height");
        triangle  -> get_dimensions("base", "height");
        ellipse   -> get_dimensions("major axis", "minor axis");
        _ -> io:format("Unknow shape: ~s~n", [Shape]),
            {error, error}
    end,
    
    check_dimensions(Shape, element(1, Numbers), element(2, Numbers)).
    

%%-------------------------------------------------------------------
%% @doc Calls the get_dimensions/2 function for required parameters
%% @spec get_dimensions(sting(), string()) -> {number(), number()}
%% @end
%%-------------------------------------------------------------------
get_dimensions(Parameter1, Parameter2) ->
    First = get_number(Parameter1),
    Second = get_number(Parameter2),
    {First, Second}.


%%-------------------------------------------------------------------
%% @doc Cheks the numbers and evaluates the external geom:area/1 funcion
%% @spec chek_dimensions(string()), number(), number())
%% @end
%%-------------------------------------------------------------------
check_dimensions(_, illegal_value, _) ->
    io:format("Error in first number.~n");
check_dimensions(_, _, illegal_value) ->
    io:format("Error in second number.~n");
check_dimensions(_, First, Second) when First < 0; Second < 0 ->
    io:format("Both numbers must be greater than or equal to zero~n");
check_dimensions(Shape, First, Second) -> 
    geom:area({Shape, First, Second}).
