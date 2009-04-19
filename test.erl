-module(test).

-export([test/1]).

test(N) when N > 0, N < 10; N > 15, N < 20 ->
    N.
