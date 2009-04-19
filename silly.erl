-module(silly).
-export([start/1, encode/1, decode/1, test/0, loop/2, newencoding/1]).

% Encoding should be a dictionary which maps each letter to the letter it should be
start(Encoding) ->
  Decoding = create_decode(Encoding),
  register(silly, spawn(silly, loop, [Encoding, Decoding])).

create_decode(Encoding) ->
  F = fun(Key, Val, Accum) ->
      dict:store(Val, Key, Accum)
    end,
  dict:fold(F, dict:new(), Encoding).

loop(Encoding, Decoding) ->
  receive
    {encode, From, Data} ->
      From ! {encode, code_data(Data, Encoding)},
      loop(Encoding, Decoding);
    {decode, From, Data} ->
      From ! {decode, code_data(Data, Decoding)},
      loop(Encoding, Decoding);
    {change_encoding, From, Newencoding} ->
      From ! success,
      loop(Newencoding, create_decode(Newencoding))
  end.

code_data(L, Coding) ->
  lists:map(code_map(Coding), L).

code_map(Coding) ->
  fun(I) ->
    {ok, Value} = dict:find(I, Coding),
    Value
  end.

newencoding(Encoding) ->
  silly ! {change_encoding, self(), Encoding},
  receive
    success ->
      success
  end.

encode(Data) ->
  silly ! {encode, self(), Data},
  receive
    {encode, Value} ->
      {ok, Value}
  end.

decode(Data) ->
  silly ! {decode, self(), Data},
  receive
    {decode, Value} ->
      {ok, Value}
  end.

test() ->
  Coding = dict:from_list([{$a, $b}, {$b, $c}, {$c, $d}, {$d, $e}, {$e, $f}, {$f, $g}, {$g, $h}, {$h, $i}, {$i, $j}, {$j, $k}, {$k, $m}, {$l, $m}, {$m, $n}, {$n, $o}, {$o, $p}]),
  start(Coding),
  {ok, Enc} = encode("malcolm"),
  {ok, Dec} = decode(Enc),
  {Enc, Dec}.
