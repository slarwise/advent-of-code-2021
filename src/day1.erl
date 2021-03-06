-module(day1).

-behaviour(aoc).

-include_lib("eunit/include/eunit.hrl").

-type report() :: [integer(), ...].

-export([run/1]).

-spec run(argparse:part()) -> ok.
run(Part) ->
    Report = read_input(),
    WindowSize =
        case Part of
            1 -> 1;
            2 -> 3
        end,
    Increases = count_increases(Report, WindowSize),
    io:format("The number of increases is ~p~n", [Increases]).

-spec count_increases(report(), WindowSize) -> Increases when
    WindowSize :: pos_integer(),
    Increases :: non_neg_integer().
count_increases(Report, WindowSize) ->
    FirstWindow = lists:sublist(Report, WindowSize),
    {Increases, _LastWindow} =
        lists:foldl(
            fun(CurrentMeasurement, {Increases, PreviousWindow}) ->
                PreviousSum = lists:sum(PreviousWindow),
                CurrentWindow = tl(PreviousWindow) ++ [CurrentMeasurement],
                CurrentSum = lists:sum(CurrentWindow),
                case CurrentSum > PreviousSum of
                    true -> {Increases + 1, CurrentWindow};
                    false -> {Increases, CurrentWindow}
                end
            end,
            {_InitialIncreases = 0, FirstWindow},
            lists:nthtail(WindowSize, Report)
        ),
    Increases.

count_increases_part1_test() ->
    ?assertEqual(7, count_increases(example_report(), _WindowSize = 1)).

count_increases_part2_test() ->
    ?assertEqual(5, count_increases(example_report(), _WindowSize = 3)).

-spec example_report() -> report().
example_report() ->
    [
        199,
        200,
        208,
        210,
        200,
        207,
        240,
        269,
        260,
        263
    ].

-spec read_input() -> Input :: report().
read_input() ->
    {ok, Binary} = file:read_file("input/day1"),
    StringList = string:lexemes(binary_to_list(Binary), "\n"),
    lists:map(fun list_to_integer/1, StringList).
