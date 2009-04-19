-module(link_test).

-export([test/0, foo/0]).

foo() ->
    timer:sleep(5000),
    a = b.

test() ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, foo, []),
    get_messages().

get_messages() ->
    receive
        Anything ->
            io:format("~w~n", [Anything]),
            get_messages()
    after
        10000 ->
            ok
    end.

        
