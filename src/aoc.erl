-module(aoc).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main(Args :: cli_args()) -> none().
main(Args) ->
    case validate_args(Args) of
        ok ->
            {Day, Part} = parse_args(Args),
            case validate_day_and_part_range(Day, Part) of
                ok ->
                    run(Day, Part);
                {error, Error} ->
                    io:format("~p~n", [Error]),
                    ErrorMsg = create_error_message(Error),
                    io:format("~s~n", [ErrorMsg]),
                    io:format("~s~n", [usage_message()]),
                    erlang:halt(1)
            end;
        {error, Error} ->
            ErrorMsg = create_error_message(Error),
            io:format("~s~n", [ErrorMsg]),
            io:format("~s~n", [usage_message()]),
            erlang:halt(1)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-type result() :: ok | {error, Message :: atom()}.
-type cli_arg() :: string().
-type cli_args() :: [cli_arg()].
-type check_function() :: fun((cli_args()) -> result()).
-type check_range_function() :: fun((Day :: integer(), Part :: integer()) -> result()).

-spec validate_args(cli_args()) -> result().
validate_args(Args) ->
    Checks = [
              fun check_args_length/1,
              fun check_args_are_integers/1
             ],
    validate_args(Args, Checks).

-spec validate_args(cli_args(), check_function()) -> result().
validate_args(_Args, []) ->
    ok;
validate_args(Args, [Check | Rest]) ->
    case Check(Args) of
        ok -> validate_args(Args, Rest);
        {error, Error} -> {error, Error}
    end.

-spec check_args_length(cli_args()) -> result().
check_args_length(Args) when length(Args) == 2 -> ok;
check_args_length(Args) when length(Args) < 2 -> {error, too_few_arguments};
check_args_length(Args) when length(Args) > 2 -> {error, too_many_arguments}.

-spec check_args_are_integers(cli_args()) -> result().
check_args_are_integers(Args) ->
    case lists:all(fun string_is_integer/1, Args) of
        true -> ok;
        false -> {error, args_must_be_integers}
    end.

-spec string_is_integer(string()) -> boolean().
string_is_integer(String) ->
    case string:to_integer(String) of
        {_Int, _Rest = []} -> true;
        {error, no_integer} -> false;
        {_Int, _Rest} -> false
    end.

-spec validate_day_and_part_range(Day, Part) -> result() when
      Day :: integer(),
      Part :: integer().
validate_day_and_part_range(Day, Part) ->
    Checks = [
              fun check_correct_range/2,
              fun check_day_is_implemented/2
             ],
    validate_day_and_part_range(Day, Part, Checks).

-spec validate_day_and_part_range(Day, Part, Checks) -> result() when
      Day :: integer(),
      Part :: integer(),
      Checks :: [check_range_function()].
validate_day_and_part_range(_Day, _Part, []) ->
    ok;
validate_day_and_part_range(Day, Part, [Check | Rest]) ->
    case Check(Day, Part) of
        ok -> validate_day_and_part_range(Day, Part, Rest);
        {error, Error} -> {error, Error}
    end.

-spec check_correct_range(Day :: integer(), Part :: integer()) -> result().
check_correct_range(Day, Part) when
      (Day > 0 andalso Day < 25) andalso
      (Part > 0 andalso Part < 3) ->
    ok;
check_correct_range(_Day, _Part) -> {error, incorrect_range}.

-spec check_day_is_implemented(Day :: integer(), Part :: integer()) -> result().
check_day_is_implemented(Day, _Part) when Day == 1 -> ok;
check_day_is_implemented(_Day, _Part) -> {error, day_is_not_implemented}.


-spec parse_args(cli_args()) -> {Day, Part} when
      Day :: pos_integer(),
      Part :: pos_integer().
parse_args([DayStr, PartStr]) ->
    {Day, []} = string:to_integer(DayStr),
    {Part, []} = string:to_integer(PartStr),
    {Day, Part}.

-spec usage_message() -> string().
usage_message() ->
    string:join([
                 "usage: aoc [-h] day part",
                 "",
                 "Positional arguments:",
                 " day          The day to run, an integer between 1 and 24",
                 " part         The part of the day to run, an integer between 1 and 2",
                 "",
                 "options:",
                 "-h, --help    show this help message and exit"
                ],
                "\n"
               ).

-spec create_error_message(Error :: atom()) -> string().
create_error_message(too_few_arguments) ->
    "aoc: error: exactly two arguments must be given";
create_error_message(too_many_arguments) ->
    "aoc: error: exactly two arguments must be given";
create_error_message(args_must_be_integers) ->
    "aoc: error: day and part must both be integers";
create_error_message(incorrect_range) ->
    "aoc: error: day must be in {1, ..., 24} and part in {1, 2}";
create_error_message(day_is_not_implemented) ->
    "aoc: error: the given day is not implemented".

-spec run(Day :: pos_integer(), Part :: pos_integer()) -> any().
run(1, Part) ->
    day1:run(Part).
