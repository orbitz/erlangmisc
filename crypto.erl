-module(crypto).

-export([abstract_word/1, index_files/1, solve/2, solve/3, solve/4, apply_map/2]).

abstract_word(Word) ->
    lists:reverse(element(2, lists:foldl(fun(Elm, {N, NewWord, Acc}) ->
                                                 case dict:find(Elm, Acc) of
                                                     {ok, Value} ->
                                                         {N, [Value | NewWord], Acc};
                                                     error ->
                                                         {N + 1, [N | NewWord], dict:store(Elm, N, Acc)}
                                                 end
                                         end, {0, [], dict:new()}, Word))).

index_files(NList) ->
    lists:foldl(fun(Elm, Acc) ->
                        index_file(Elm, Acc)
                end, dict:new(), NList).

index_file(Name, Dict) ->
    for_each_line_in_file(Name, [read],
                          fun(Line, Acc) ->
                                  SLine = string:strip(http_util:to_lower(Line), right, $\n),
                                  dict:append(abstract_word(SLine), SLine, Acc)
                          end, Dict).

generate_letter_map(Word1, Word2) when length(Word1) == length(Word2) ->
    dict:from_list(lists:zip(Word1, Word2)).
    
apply_map(Sentence, Map) ->
    lists:map(fun(Elm) ->
                      case dict:find(Elm, Map) of
                          {ok, V} ->
                              V;
                          error ->
                              Elm
                      end
              end, Sentence).


solve(Sentence, Index) ->
    solve(Sentence, dict:new(), Index).

solve(Sentence, Map, Index, onlydict) ->
    Sorted = lists:sort(fun(E1, E2) ->
                                lists:max(abstract_word(E1)) > lists:max(abstract_word(E2))
                        end, lists:usort(string:tokens(Sentence, " "))),
    solve_wordlist(Sorted, Map, Index, []).

solve(Sentence, Index, onlydict) ->
    solve(Sentence, dict:new(), Index, onlydict);
solve(Sentence, Map, Index) ->
    Sorted = lists:filter(fun(Elm) -> dict:is_key(abstract_word(Elm), Index) end,
                          lists:sort(fun(E1, E2) ->
                                             lists:max(abstract_word(E1)) > lists:max(abstract_word(E2))
                                     end, lists:usort(string:tokens(Sentence, " ")))),
    solve_wordlist(Sorted, Map, Index, []).

solve_wordlist([], Map, _, Results) ->
    [Map | Results];
solve_wordlist([H | Rest], Map, Index, Results) ->
    AbH = abstract_word(H),
    try_words_choice(H, dict:fetch(AbH, Index), Rest, Map, Index, Results).


try_words_choice(_, [], _, _, _, Results) ->
    Results;
try_words_choice(OrigWord, [W | R], Wordlist, Map, Index, Results) ->
    Res = case lists:all(fun({K, V}) ->
                                 case dict:find(K, Map) of
                                     {ok, V} ->
                                         true;
                                     {ok, _} ->
                                         false;
                                     error ->
                                         true
                                 end
                         end, dict:to_list(generate_letter_map(OrigWord, W))) of
              true ->
                  solve_wordlist(Wordlist, dict:merge(fun(_K, V1, _V2) ->
                                                              V1
                                                      end,
                                                      generate_letter_map(OrigWord, W), Map),
                                 Index, Results);
              false ->
                  Results
          end,
    try_words_choice(OrigWord, R, Wordlist, Map, Index, Res).
            
    
    
%% Superfluous to iterate over a file
for_each_line_in_file(Name, Mode, Proc, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
                    for_each_line(Device, Proc, NewAccum)
    end.
