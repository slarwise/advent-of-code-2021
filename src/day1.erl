-include_lib("eunit/include/eunit.hrl").

-module(day1).

-export([run/1]).

-spec run(Part :: 1) -> ok.
run(1) ->
    Report = read_input(),
    Increases = count_increases(Report),
    io:format("The number of increases is ~p~n", [Increases]).

count_increases(Report) ->
    {Increases, _LastValue} =
        lists:foldl(
            fun(Current, {Increases, Previous}) ->
                case Current > Previous of
                    true -> {Increases + 1, Current};
                    false -> {Increases, Current}
                end
            end,
            {0, infinity},
            Report
        ),
    Increases.

count_increases_test() ->
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
    ?assertEqual(7, count_increases(Report)).

-spec read_input() -> Input :: [integer()].
read_input() ->
    Filename = "input/day1",
    {ok, Binary} = file:read_file(Filename),
    StringList = string:lexemes(binary_to_list(Binary), "\n"),
    lists:map(fun list_to_integer/1, StringList).
