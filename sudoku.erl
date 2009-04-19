-module(sudoku).

-export([solve/1, print/1]).

solve(Puzzle) when is_list(Puzzle) ->
    solve_puzzle(dict_from_list(Puzzle)).

print(Puzzle) ->
    lists:foreach(fun(X) ->
                          lists:foreach(fun(Y) ->
                                                io:format("~w ", [dict:fetch({X, Y}, Puzzle)])
                                        end, lists:seq(0, 8)),
                          io:format("~n", [])
                  end, lists:seq(0, 8)).
                          
dict_from_list(List) ->
    element(2, lists:foldl(fun(Elm, {X, Dict}) ->
                                   {_, DDict} = lists:foldl(fun(Elem, {Y, NDict}) ->
                                                                    {Y + 1, dict:store({X, Y}, Elem, NDict)}
                                                            end, {0, Dict}, Elm),
                                   {X + 1, DDict}
                           end, {0, dict:new()}, List)).

solve_puzzle(Puzzle) ->
    case generate_open_spots(Puzzle) of
        [{{X, Y}, Set} | _] ->
            try_value({X, Y}, Set, Puzzle);
        [] ->
            {solved, Puzzle}
    end.

try_value(_, [], Puzzle) ->
    %print(Puzzle),
    %io:format("~n", []),
    failed;
try_value({X, Y}, [H | R], Puzzle) ->
    case solve_puzzle(dict:store({X, Y}, H, Puzzle)) of
        {solved, RPuzzle} ->
            {solved, RPuzzle};
        failed ->
            try_value({X, Y}, R, Puzzle)
    end.
                      
generate_open_spots(Puzzle) ->
    OpenSquareList = dict:fold(fun(Key, b, Acc) ->
                                       [Key | Acc];
                                  (_Key, _Value, Acc) ->
                                       Acc
                               end, [], Puzzle),
    lists:sort(fun({{_X1, _Y1}, E1}, {{_X2, _Y2}, E2}) when length(E1) < length(E2) ->
                       true;
                  (_E1, _E2) ->
                       false
               end, generate_open_values(OpenSquareList, Puzzle)).

generate_open_values(List, Puzzle) ->
    generate_open_values(List, [], Puzzle).

generate_open_values([], Acc, _Puzzle) ->
    Acc;
generate_open_values([{X, Y} | R], Acc, Puzzle) ->
    generate_open_values(R, [{{X, Y}, remove_region_vals({X, Y},
                                                         remove_x_vals(Y,
                                                                       remove_y_vals(X, lists:seq(1, 9),
                                                                                     Puzzle),
                                                                       Puzzle),
                                                         Puzzle)} | Acc],
                         Puzzle).

remove_x_vals(Y, List, Puzzle) ->
    lists:foldl(fun(Idx, Acc) ->
                        case dict:fetch({Idx, Y}, Puzzle) of
                            b ->
                                Acc;
                            E ->
                                lists:delete(E, Acc)
                        end
                        end,
                        List, lists:seq(0, 8)).

remove_y_vals(X, List, Puzzle) ->
    lists:foldl(fun(Idx, Acc) ->
                        case dict:fetch({X, Idx}, Puzzle) of
                            b ->
                                Acc;
                            E ->
                                lists:delete(E, Acc)
                        end
                        end,
                        List, lists:seq(0, 8)).


remove_region_vals({X, Y}, List, Puzzle) ->
    {RX, RY} = find_region(X, Y),
    lists:foldl(fun(IX, AccX) ->
                        lists:foldl(fun(IY, AccY) ->
                                            case dict:fetch({IX, IY}, Puzzle) of
                                                b ->
                                                    AccY;
                                                E ->
                                                    lists:delete(E, AccY)
                                            end
                                    end, AccX, lists:seq(RY, RY + 2))
                        end, List, lists:seq(RX, RX + 2)).

find_region(X, Y) ->
    {find_region(X), find_region(Y)}.

find_region(V) when V >= 0, V < 3 ->
    0;
find_region(V) when V >= 3, V < 6 ->
    3;
find_region(V) when V >= 6, V < 9 ->
    6.
    
