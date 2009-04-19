-module(merge).
-export([sort/1, split/3, merge/3]).


sort([]) -> [];
sort([X]) -> [X];
sort(X) ->
    {Left, Right} = split(X,[],[]),
    merge(sort(Left), sort(Right), []).

split( [], L1, L2 ) -> { L1, L2 };
split( [X] , L1, L2 ) -> { [X|L1], L2 };
split( [X,Y|Z], L1, L2 ) ->
    split( Z, [X|L1], [Y|L2] ).


merge([], [], Res) ->
    lists:reverse(Res);
merge([L | L1], [], Res) ->
    merge(L1, [], [L | Res]);
merge([], [R | R1], Res) ->
    merge([], R1, [R | Res]);
merge([L | L1], [R | R1], Res) when L < R ->
    merge(L1, [R | R1], [L | Res]);
merge([L | L1], [R | R1], Res) ->
    merge([L | L1], R1, [R | Res]).
    
