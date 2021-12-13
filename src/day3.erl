-module(day3).

-behaviour(aoc).

-include_lib("eunit/include/eunit.hrl").

-type bit() :: 0..1.
-type report() :: [[bit(), ...], ...].

-export([run/1]).

-spec run(argparse:part()) -> ok.
run(Part) ->
    Report = read_report(),
    case Part of
        1 -> part1(Report);
        2 -> part2(Report)
    end.

-spec part1(report()) -> ok.
part1(Report) ->
    GammaRate = most_common_bits(Report),
    EpsilonRate = [Bit bxor 1 || Bit <- GammaRate],
    GammaRateDecimal = bits_to_integer(GammaRate),
    EpsilonRateDecimal = bits_to_integer(EpsilonRate),
    io:format("Gamma rate bits: ~p~n", [GammaRate]),
    io:format("Gamma rate decimal: ~p~n", [GammaRateDecimal]),
    io:format("Epsilon rate bits: ~p~n", [EpsilonRate]),
    io:format("Epsilon rate decimal: ~p~n", [bits_to_integer(EpsilonRate)]),
    io:format("Product: ~p~n", [GammaRateDecimal * EpsilonRateDecimal]).

-spec part2(report()) -> ok.
part2(Report) ->
    OxygenRating = rating_filter(oxygen, Report),
    OxygenRatingDecimal = bits_to_integer(OxygenRating),
    C02Rating = rating_filter(c02, Report),
    C02RatingDecimal = bits_to_integer(C02Rating),
    io:format("Oxygen generator rating: ~p~n", [OxygenRating]),
    io:format("Oxygen generator rating decimal: ~p~n", [OxygenRatingDecimal]),
    io:format("C02 generator rating: ~p~n", [C02Rating]),
    io:format("C02 generator rating decimal: ~p~n", [C02RatingDecimal]),
    io:format("Product: ~p~n", [OxygenRatingDecimal * C02RatingDecimal]).

-spec most_common_bits(report()) -> [bit()].
most_common_bits(Report) ->
    NbrPositions = length(hd(Report)),
    lists:foldl(
        fun(Position, GammaRateAcc) ->
            BitsInPosition = [lists:nth(Position, X) || X <- Report],
            [most_common_bit(BitsInPosition) | GammaRateAcc]
        end,
        [],
        lists:seq(NbrPositions, 1, -1)
    ).

most_common_bits_test() ->
    ?assertEqual([1, 0, 1, 1, 0], most_common_bits(example_report())).

-spec rating_filter(oxygen | c02, report()) -> [bit()].
rating_filter(Type, Report) ->
    NbrPositions = length(hd(Report)),
    Rating =
        catch lists:foldl(
            fun
                (_Position, [ValidNumber | []]) ->
                    throw(ValidNumber);
                (Position, ValidNumbers) ->
                    BitsInPosition = [lists:nth(Position, X) || X <- ValidNumbers],
                    MostCommonBit = most_common_bit(BitsInPosition),
                    MatchingBit =
                        case Type of
                            oxygen -> MostCommonBit;
                            c02 -> 1 bxor MostCommonBit
                        end,
                    lists:filtermap(
                        fun
                            ({Bit, Number}) when Bit == MatchingBit ->
                                {true, Number};
                            ({_, _}) ->
                                false
                        end,
                        lists:zip(BitsInPosition, ValidNumbers)
                    )
            end,
            Report,
            lists:seq(1, NbrPositions)
        ),
    lists:flatten(Rating).

oxygen_generator_rating_filter_test() ->
    ?assertEqual(
        [1, 0, 1, 1, 1],
        rating_filter(oxygen, example_report())
    ).

co2_scrubber_rator_rating_filter_test() ->
    ?assertEqual(
        [0, 1, 0, 1, 0],
        rating_filter(c02, example_report())
    ).

-spec most_common_bit([bit()]) -> bit().
most_common_bit(Bits) -> round(average(Bits)).

-spec average([bit()]) -> float().
average(Bits) -> lists:sum(Bits) / length(Bits).

average_test() -> ?assertEqual(0.5, average([0, 1])).

-spec bits_to_integer([bit()]) -> integer().
bits_to_integer(Bits) ->
    StartPow = math:pow(2, length(Bits) - 1),
    {Integer, _LastPow} = lists:foldl(
        fun(B, {Sum, Pow}) ->
            {Sum + B * Pow, Pow / 2}
        end,
        {0, StartPow},
        Bits
    ),
    round(Integer).

bits_to_integer_test() -> ?assertEqual(3, bits_to_integer([0, 0, 1, 1])).

-spec example_report() -> report().
example_report() ->
    [
        [0, 0, 1, 0, 0],
        [1, 1, 1, 1, 0],
        [1, 0, 1, 1, 0],
        [1, 0, 1, 1, 1],
        [1, 0, 1, 0, 1],
        [0, 1, 1, 1, 1],
        [0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0],
        [1, 0, 0, 0, 0],
        [1, 1, 0, 0, 1],
        [0, 0, 0, 1, 0],
        [0, 1, 0, 1, 0]
    ].

-spec read_report() -> report().
read_report() ->
    {ok, Binary} = file:read_file("input/day3"),
    Rows = string:lexemes(binary_to_list(Binary), "\n"),
    lists:map(fun string_to_report/1, Rows).

-spec string_to_report(string()) -> [bit()].
string_to_report(String) -> [S - $0 || S <- String].
