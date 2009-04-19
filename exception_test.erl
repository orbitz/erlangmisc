-module(exception_test).

-export([test/0]).

testor() ->
    throw({plop, "bonk"}).

test() ->
    Test = {noexist, noexist},
    try Test() of
        {plop, _} ->
            io:format("fine~n")
    catch
        _:_ ->
            io:format("caught exception~n")
    end.
