-module(assig3).
-export([start/0, loop/0, test/0]).

start() ->
  spawn(assig3, loop, []).

loop() ->
  receive
    {output, Data} ->
      io:format("~s~n", [Data]),
      loop();
    {stop} ->
      ok
  end.

test() ->
  Pid = start(),
  Pid ! {output, "string1"},
  Pid ! {output, "string2"},
  Pid ! {stop},
  ok.
