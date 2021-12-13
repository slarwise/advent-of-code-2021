-module(day2).

-include_lib("eunit/include/eunit.hrl").

-export([run/1]).

-behaviour(aoc).

-type direction() :: forward | down | up.
-type increment() :: pos_integer().
-type command() :: {direction(), increment()}.
-type position1() :: #{
    horizontal := non_neg_integer(),
    depth := non_neg_integer()
}.
-type position2() :: #{
    horizontal := non_neg_integer(),
    depth := non_neg_integer(),
    aim := non_neg_integer()
}.

-spec run(argparse:part()) -> ok.
run(Part) ->
    Commands = read_input(),
    FinalPosition =
        case Part of
            1 -> calculate_final_position1(Commands);
            2 -> calculate_final_position2(Commands)
        end,
    io:format("Final position: ~p~n", [FinalPosition]),
    #{horizontal := Horizontal, depth := Depth} = FinalPosition,
    io:format("Product: ~p~n", [Horizontal * Depth]).

-spec calculate_final_position1([command()]) -> position1().
calculate_final_position1(Commands) ->
    StartPosition = #{horizontal => 0, depth => 0},
    lists:foldl(fun move1/2, StartPosition, Commands).

-spec move1(command(), position1()) -> position1().
move1({forward, Increment}, Position = #{horizontal := Horizontal}) ->
    Position#{horizontal => Horizontal + Increment};
move1({down, Increment}, Position = #{depth := Depth}) ->
    Position#{depth => Depth + Increment};
move1({up, Increment}, Position = #{depth := Depth}) ->
    Position#{depth => Depth - Increment}.

calculate_final_position1_test() ->
    ?assertEqual(
        #{horizontal => 15, depth => 10},
        calculate_final_position1(example_commands())
    ).

-spec calculate_final_position2([command()]) -> position2().
calculate_final_position2(Commands) ->
    StartPosition = #{horizontal => 0, depth => 0, aim => 0},
    lists:foldl(fun move2/2, StartPosition, Commands).

-spec move2(command(), position2()) -> position2().
move2(
    {forward, Increment},
    Position = #{horizontal := Horizontal, depth := Depth, aim := Aim}
) ->
    Position#{
        horizontal => Horizontal + Increment,
        depth => Depth + (Aim * Increment)
    };
move2({down, Increment}, Position = #{aim := Aim}) ->
    Position#{aim => Aim + Increment};
move2({up, Increment}, Position = #{aim := Aim}) ->
    Position#{aim => Aim - Increment}.

calculate_final_position2_test() ->
    ?assertEqual(
        #{horizontal => 15, depth => 60, aim => 10},
        calculate_final_position2(example_commands())
    ).

-spec example_commands() -> [command()].
example_commands() ->
    [
        {forward, 5},
        {down, 5},
        {forward, 8},
        {up, 3},
        {down, 8},
        {forward, 2}
    ].

-spec read_input() -> Input :: [command()].
read_input() ->
    {ok, Binary} = file:read_file("input/day2"),
    StringList = string:lexemes(binary_to_list(Binary), "\n"),
    lists:map(fun string_to_command/1, StringList).

-spec string_to_command(string()) -> command().
string_to_command(String) ->
    [Command, Increment | []] = string:lexemes(String, " "),
    {list_to_atom(Command), list_to_integer(Increment)}.

string_to_command_test() ->
    ?assertEqual({forward, 3}, string_to_command("forward 3")).
