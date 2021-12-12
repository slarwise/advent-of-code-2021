-module(aoc).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main(Args :: argparse:cli_args()) -> no_return().
main(Args) ->
    case argparse:parse(Args) of
        {continue, Config} ->
            run(Config),
            erlang:halt(0);
        {stop, help} ->
            io:format("~s~n", [usage_message()]),
            erlang:halt(0);
        {stop, Error} ->
            ErrorMsg = create_error_message(Error),
            io:format("~s~n", [ErrorMsg]),
            erlang:halt(1)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-spec usage_message() -> string().
usage_message() ->
    string:join([
                 "usage: aoc [-h] <day> <part>",
                 "",
                 "Positional arguments:",
                 " day          the day to run, an integer between 1 and 24",
                 " part         the part of the day to run, either 1 or 2",
                 "",
                 "options:",
                 "-h, --help    show this help message and exit"
                ],
                "\n"
               ).

-spec create_error_message(Error :: atom()) -> string().
create_error_message(incorrect_length) ->
    "aoc: error: exactly two arguments must be given";
create_error_message(args_must_be_integers) ->
    "aoc: error: day and part must both be integers";
create_error_message(day_out_of_range) ->
    "aoc: error: day must be in {1, ..., 24}";
create_error_message(part_out_of_range) ->
    "aoc: error: part must be either 1 or 2";
create_error_message(not_implemented) ->
    "aoc: error: the given day and part is not implemented".

-spec run(argparse:config()) -> any().
run(#{day := 1, part := Part}) -> day1:run(Part).
