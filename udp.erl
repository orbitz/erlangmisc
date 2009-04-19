-module(udp).

-export([test/0]).

test() ->
    {ok, Socket} = gen_udp:open(1025),
    Ret = gen_udp:recv(Socket, 1000, 1000),
    gen_udp:close(Socket),
    Ret.
