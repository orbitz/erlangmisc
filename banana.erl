-module(banana).
-export([int_to_b128/1, b128_to_int/1, object_to_banana/1, banana_to_object/1, start/0, loop/0, test/1, encode/2, stop/1]).

%Profiles
-define(PROFILES, [none]).

% Our various types
-define(LIST, 16#80).
-define(POS_INT, 16#81).
-define(NEG_INT, 16#83).
-define(POS_LARGE_INT, 16#85).
-define(NEG_LARGE_INT, 16#86).
-define(FLOAT, 16#84).
-define(STRING, 16#82).

start() ->
  spawn_link(banana, loop, []).

stop(Pid) ->
  Pid ! {stop, self()}.

profiles() ->
  ?PROFILES.

profiles_to_banana() ->
  object_to_banana(profiles()).

int_to_b128(0) ->
  [0];
int_to_b128(Num) when integer(Num) ->
  int_to_b128(Num, []).

int_to_b128(0, Enc) ->
  Enc;
int_to_b128(Num, Enc)  ->
 int_to_b128(Num bsr 7, lists:append(Enc, [Num band 16#7f])). 

b128_to_int(List) when list(List) ->
  b128_to_int(List, 0, 1).

b128_to_int([H|Tail], Num, Place) ->
  b128_to_int(Tail, Num + (H *  Place), Place * 128);
b128_to_int([], Num, _) ->
  Num.
  
  

object_to_banana(Object) ->
  list_to_binary(object_to_banana(Object, [])).

object_to_banana(Object, Banana) when list(Object) ->
  Bnew = lists:append(Banana, int_to_b128(length(Object))),
  list_to_banana(Object, lists:append(Bnew, [?LIST]));
object_to_banana(Object, Banana) when tuple(Object) ->
  object_to_banana(tuple_to_list(Object), Banana);
object_to_banana(Object, Banana) when integer(Object), Object >= 0, Object =< 2147483647  ->
  lists:append(Banana, lists:append(int_to_b128(Object), [?POS_INT]));
object_to_banana(Object, Banana) when integer(Object), Object < 0, Object >= -2147483648->
  lists:append(Banana, lists:append(int_to_b128(-Object), [?NEG_INT]));
object_to_banana(Object, Banana) when float(Object) ->
  lists:append(Banana, lists:append([?FLOAT], binary_to_list(<<Object/float>>)));
object_to_banana(Object, Banana) when integer(Object), Object >= 0 ->
  lists:append(Banana, lists:append(int_to_b128(Object), [?POS_LARGE_INT]));
object_to_banana(Object, Banana) when integer(Object), Object < 0 ->
  lists:append(Banana, lists:append(int_to_b128(-Object), [?NEG_LARGE_INT]));
object_to_banana(Object, Banana) when atom(Object) ->
  Atom = atom_to_list(Object),
  lists:append(Banana, lists:append(int_to_b128(length(Atom)), lists:append([?STRING], Atom))).

list_to_banana([H|Tail], Banana) ->
  list_to_banana(Tail, lists:append(Banana, object_to_banana(H, [])));
list_to_banana([], Banana) ->
  Banana.


recv() ->
  receive
    {decode, Pid, Data} ->
      {decode, Pid, binary_to_list(Data)};
    {encode, Pid, Data} ->
      {encode, Pid, Data};
    {stop, Pid} ->
      {stop, Pid}
  end.

% I am unsure of how i feel about this. When decode data comes in it just return sit.
% When encode data comes in it handles the entire responce here. is this ugly?
dispatch_message() ->
  case recv() of
    {decode, Pid, Data} ->
      {decode, Pid, Data};
    {encode, Pid, Data} ->
      responce(Pid, object_to_banana(Data), encode);
    {stop, Pid} ->
      throw({stop, Pid})
  end.

% This is the main loop, we are only in this when we are waiting for a new sexpr
loop() ->
  loop(start, []).
loop(_, []) ->
  case catch dispatch_message() of
    {decode, Pid, Data} ->
      loop(Pid, Data);
    {stop, Pid} ->
      Pid ! {stop}
  end;
loop(Pid, Data) ->
  case catch banana_to_object(Data) of
    {ok, Res, Tail} ->
      responce(Pid, Res, decode),
      loop(Pid, Tail);
    {stop, Pid} ->
      Pid ! {stop}
  end.
      
responce(Pid, Res, Action) ->
  Pid ! {Action, Res}.

decode(Pid, Data) ->
  Pid ! {decode, self(), Data}.


encode(Pid, Data) ->
  Pid ! {encode, self(), Data}.

banana_to_object(Object) ->
  banana_to_object(Object, []).

banana_to_object([?LIST|Tail], Sofar) ->
  banana_to_list(Tail, b128_to_int(Sofar));
banana_to_object([?POS_INT|Tail], Sofar) ->
  {ok, b128_to_int(Sofar), Tail};
banana_to_object([?NEG_INT|Tail], Sofar) ->
  {ok, -b128_to_int(Sofar), Tail};
banana_to_object([?FLOAT|Tail], _) ->
  banana_to_float(Tail);
banana_to_object([?POS_LARGE_INT|Tail], Sofar) ->
  {ok, b128_to_int(Sofar), Tail};
banana_to_object([?NEG_LARGE_INT|Tail], Sofar) ->
  {ok, -b128_to_int(Sofar), Tail};
banana_to_object([?STRING|Tail], Sofar) ->
  banana_to_string(Tail, b128_to_int(Sofar));
banana_to_object([H|_], _) when H >= 16#80 ->
  throw({unknown_type, H});
banana_to_object([H|Tail], Sofar) ->
  banana_to_object(Tail, lists:append(Sofar, [H]));
banana_to_object([], Sofar) ->
  case dispatch_message() of
    {decode, _, Data} ->
      banana_to_object(Data, Sofar)
  end.

banana_to_float(Object) ->
  banana_to_float(Object, 8, []).

banana_to_float(Tail, 0, Res) ->
  Binfloat = list_to_binary(Res),
  <<Float/float>> = Binfloat,
  {ok, Float, Tail};
banana_to_float([H|Tail], Length, Res) ->
  banana_to_float(Tail, Length - 1, lists:append(Res, [H]));
banana_to_float([], Length, Res) ->
  case dispatch_message() of 
    {decode, _, Data} ->
      banana_to_float(Data, Length, Res)
  end.
  
banana_to_string(Object, Length) ->
  banana_to_string(Object, Length, []).

banana_to_string(Tail, 0, Res) ->
  {ok, list_to_atom(Res), Tail};
banana_to_string([H|Tail], Length, Res) ->
  banana_to_string(Tail, Length - 1, lists:append(Res, [H]));
banana_to_string([], Length, Res) ->
  case dispatch_message() of
    {decode, _, Data} ->
      banana_to_string(Data, Length, Res)
  end.
  
banana_to_list(Object, Length) ->
  banana_to_list(Object, Length, []).

banana_to_list(Tail, 0, Res) ->
  {ok, Res, Tail};
banana_to_list([], Length, Res) ->
  case dispatch_message() of
    {decode, _, Data} ->
      banana_to_list(Data, Length, Res)
  end;
banana_to_list(List, Length, Res) ->
  {ok, Object, Rest} = banana_to_object(List),
  banana_to_list(Rest, Length - 1, lists:append(Res, [Object])).
 
test(Object) ->
  Banana = start(),
  Data = object_to_banana(Object),
  decode(Banana, Data),
  receive
    {decode, Result} ->
      stop(Banana),
      {ok, Result}
  end.

