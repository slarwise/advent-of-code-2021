-module(day1).

-export([run/1]).

-spec run(Part :: 1..2) -> ok.
run(Part) ->
    io:format("Running day 1, part ~p~n", [Part]).
