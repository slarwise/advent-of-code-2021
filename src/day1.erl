-module(day1).

-include_lib("eunit/include/eunit.hrl").

-export([run/1]).

-spec run(Part :: 1..2) -> ok.
run(Part) ->
    Report = read_input(),
    WindowSize =
        case Part of
            1 -> 1;
            2 -> 3
        end,
    Increases = count_increases(Report, WindowSize),
    io:format("The number of increases is ~p~n", [Increases]).

count_increases(Report, WindowSize) ->
    % Let the last element be the oldest measurement
    FirstWindow = lists:reverse(
        lists:sublist(Report, WindowSize)
    ),
    {Increases, _LastWindow} =
        lists:foldl(
            fun(Current, {Increases, Window}) ->
                Sum = lists:sum(Window),
                NewWindow = [Current | lists:droplast(Window)],
                NewSum = lists:sum(NewWindow),
                case NewSum > Sum of
                    true -> {Increases + 1, NewWindow};
                    false -> {Increases, NewWindow}
                end
            end,
            {0, FirstWindow},
            lists:nthtail(WindowSize, Report)
        ),
    Increases.

count_increases_part1_test() ->
    Report = [
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
    ],
    ?assertEqual(7, count_increases(Report, _WindowSize = 1)).

count_increases_part2_test() ->
    Report = [
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
    ],
    ?assertEqual(5, count_increases(Report, _WindowSize = 3)).

-spec read_input() -> Input :: [integer()].
read_input() ->
    {ok, Binary} = file:read_file("input/day1"),
    StringList = string:lexemes(binary_to_list(Binary), "\n"),
    lists:map(fun list_to_integer/1, StringList).
