-module(echo).

-export([init/0, terminate/1, server_busy/1, react_to/3]).

init() ->
    {ok, []}.

terminate(_State) ->
    ok.

server_busy(Socket) ->
    gen_tcp:send(Socket, <<"The Server is too busy right now.\n">>),
    ok.

react_to(_Server, Socket, BinData) ->
    gen_tcp:send(Socket, [<<"You said: ">>, BinData]),
    gen_tcp:close(Socket),
    ok.
