-module(argparse).

-include_lib("eunit/include/eunit.hrl").

-export([parse/1]).

-type cli_arg() :: string().
-type cli_args() :: [cli_arg()].
-type day() :: 1..24.
-type part() :: 1..2.
-type config() :: #{day := day(), part := part()}.
-type reason() :: atom().
-type result(Result) :: {continue, Result} | {stop, reason()}.

-export_type([cli_args/0, config/0]).

-spec parse(cli_args()) -> result(config()).
parse(Args) ->
    ValidatedArgs = lists:foldl(
                      fun(Fun, AccResult) ->
                              Fun(AccResult)
                      end,
                      {continue, Args},
                      [
                       fun is_help/1,
                       fun length_is_two/1,
                       fun all_args_are_integers/1
                      ]
                     ),

    DayAndPart = extract_day_and_part(ValidatedArgs),

    Result = lists:foldl(
               fun(Fun, AccResult) ->
                       Fun(AccResult)
               end,
               DayAndPart,
               [
                fun check_day_range/1,
                fun check_part_range/1,
                fun check_implemented/1
               ]
              ),
    Result.

parse_help_test() ->
    ?assertEqual({stop, help}, parse(["-h"])).
parse_error_test() ->
    ?assertEqual({stop, incorrect_length}, parse(["1"])).
parse_run_test() ->
    Args = ["1", "1"],
    Config = #{day => 1, part => 1},
    ?assertEqual({continue, Config}, parse(Args)).

-spec is_help(result(cli_args())) ->
    result(cli_args()).
is_help({stop, Reason}) ->
    {stop, Reason};
is_help({continue, Args}) ->
    case lists:any(
           fun(Result) -> Result end,
           [
            lists:member("-h", Args),
            lists:member("--help", Args)
           ]
          ) of
        true -> {stop, help};
        false -> {continue, Args}
    end.

is_help_true_continue_h_test() ->
    ?assertEqual({stop, help}, is_help({continue, ["-h"]})).
is_help_true_continue_h_second_test() ->
    ?assertEqual({stop, help}, is_help({continue, ["1", "-h"]})).
is_help_true_continue_help_test() ->
    ?assertEqual({stop, help}, is_help({continue, ["--help"]})).
is_help_false_continue_help_test() ->
    ?assertEqual({continue, ["1"]}, is_help({continue, ["1"]})).
is_help_error_help_test() ->
    ?assertEqual({stop, error}, is_help({stop, error})).

-spec length_is_two(result(cli_args())) -> result(cli_args()).
length_is_two({stop, Reason}) -> {stop, Reason};
length_is_two({continue, Args}) when length(Args) == 2 -> {continue, Args};
length_is_two({continue, _Args})-> {stop, incorrect_length}.

length_is_two_zero_args_test() ->
    ?assertEqual({stop, incorrect_length}, length_is_two({continue, []})).
length_is_two_one_args_test() ->
    ?assertEqual({stop, incorrect_length}, length_is_two({continue, ["1"]})).
length_is_two_two_args_test() ->
    ?assertEqual({continue, Args = ["1", "2"]}, length_is_two({continue, Args})).
length_is_two_three_args_test() ->
    ?assertEqual({stop, incorrect_length}, length_is_two({continue, ["1", "2", "3"]})).

-spec all_args_are_integers(result(cli_args())) -> result(cli_args()).
all_args_are_integers({stop, Reason}) ->
    {stop, Reason};
all_args_are_integers({continue, Args}) ->
    case lists:all(fun string_is_integer/1, Args) of
        true -> {continue, Args};
        false -> {stop, args_must_be_integers}
    end.
all_args_are_integers_true_test() ->
    ?assertEqual({continue, Args = ["1", "2"]},
                 all_args_are_integers({continue, Args})).

-spec string_is_integer(string()) -> boolean().
string_is_integer(String) ->
    case string:to_integer(String) of
        {_Int, []} -> true;
        {error, no_integer} -> false;
        {_Int, _Rest} -> false
    end.

string_is_integer_true_test() ->
    ?assertEqual(true, string_is_integer("1")).
string_is_integer_false_decimal_test() ->
    ?assertEqual(false, string_is_integer("1.1")).
string_is_integer_false_nan_test() ->
    ?assertEqual(false, string_is_integer("not_a_number")).

-spec extract_day_and_part(result(cli_args())) -> result(config()).
extract_day_and_part({stop, Reason}) ->
    {stop, Reason};
extract_day_and_part({continue, Args}) ->
    [Day, Part | []] = lists:map(
                        fun(Arg) ->
                                {Int, _Rest} = string:to_integer(Arg),
                                Int
                        end,
                        Args
                       ),
    {continue, #{day => Day, part => Part}}.

extract_day_and_part_test() ->
    Args = ["1", "2"],
    Config = #{day => 1, part => 2},
    ?assertEqual({continue, Config}, extract_day_and_part({continue, Args})).

-spec check_day_range(result(config())) -> result(config()).
check_day_range({stop, Reason}) ->
    {stop, Reason};
check_day_range({continue, Config = #{day := Day}}) when Day > 0, Day < 25 ->
    {continue, Config};
check_day_range({continue, _Config}) ->
    {stop, day_out_of_range}.

check_day_range_too_low_test() ->
    Config = #{day => 0, part => 1},
    ?assertEqual({stop, day_out_of_range},
                 check_day_range({continue, Config})).
check_day_range_too_high_test() ->
    Config = #{day => 25, part => 1},
    ?assertEqual({stop, day_out_of_range},
                 check_day_range({continue, Config})).
check_day_range_in_range_test() ->
    Config = #{day => 1, part => 1},
    ?assertEqual({continue, Config},
                 check_day_range({continue, Config})).

-spec check_part_range(result(config())) -> result(config()).
check_part_range({stop, Reason}) ->
    {stop, Reason};
check_part_range({continue, Config = #{part := Part}}) when Part > 0, Part < 3 ->
    {continue, Config};
check_part_range({continue, _Config}) ->
    {stop, part_out_of_range}.

check_part_range_too_low_test() ->
    Config = #{day => 1, part => 0},
    ?assertEqual({stop, part_out_of_range},
                 check_part_range({continue, Config})).
check_part_range_too_high_test() ->
    Config = #{day => 1, part => 3},
    ?assertEqual({stop, part_out_of_range},
                 check_part_range({continue, Config})).
check_part_range_in_range_test() ->
    Config = #{day => 1, part => 1},
    ?assertEqual({continue, Config}, check_part_range({continue, Config})).

-spec check_implemented(result(config())) -> result(config()).
check_implemented({stop, Reason}) ->
    {stop, Reason};
check_implemented({continue, Config}) ->
    Implemented = sets:from_list([#{day => 1, part => 1}]),
    case sets:is_element(Config, Implemented) of
        true -> {continue, Config};
        false -> {stop, not_implemented}
    end.

check_implemented_test() ->
    Config = #{day => 1, part => 1},
    ?assertEqual({continue, Config}, check_implemented({continue, Config})).
