-module(p1).
-export([start/0, loop/0, eval/2]).

start() ->
    spawn_link(p1, loop, []).

loop() ->
    loop(dict:new()).

loop(Functions) ->
    receive
	{evaluate, Pid, Expression} ->
            {Result, Vars} = evaluate(Expression, Functions),
	    Pid ! {ok, Result},
            loop(Vars);
        {stop, Pid} ->
            Pid ! ok
    end.

eval(Pid, Expression) ->
    Pid ! {evaluate, self(), Expression},
    receive
        {ok, Result} ->
            Result
    end.
            
evaluate(Expression, Functions) ->
    {ok, Expr, _} = erl_scan:string(Expression),
    {ok, Tokenized} = p1_parser:parse(Expr),
    eval_tree(Tokenized, Functions).

eval_tree({assign, {var, _, Lhs}, Rhs}, Functions) ->
    {Rhs, dict:store(Lhs, Rhs, Functions)};
eval_tree({sequence, First, Second}, Functions) ->
    {_, Newfun} = eval_tree(First, Functions),
    eval_tree(Second, Newfun);
eval_tree(Operation, Functions) ->
    {eval_tokens(Operation, Functions), Functions}.

eval_tokens({integer, _, Value}, _) ->
    Value;
eval_tokens({var, _, Value}, Functions) ->
    eval_tokens(dict:fetch(Value, Functions), Functions);
eval_tokens({Operator, Loperand, Roperand}, Functions) ->
    eval_binary(Operator, eval_tokens(Loperand, Functions),  eval_tokens(Roperand, Functions));
eval_tokens({Operator, Operand}, Functions) ->
    eval_unary(Operator, eval_tokens(Operand, Functions)).

eval_binary(land, Lparam, Rparam) when Lparam /= 0, Rparam /= 0 ->
    1;
eval_binary(land, _, _) ->
    0;
eval_binary(lor, Lparam, Rparam) when Lparam /= 0; Rparam /= 0 ->
    1;
eval_binary(lor, _, _) ->
    0;
eval_binary(add, Lparam, Rparam) ->
    Lparam + Rparam;
eval_binary(subtract, Lparam, Rparam) ->
    Lparam - Rparam;
eval_binary(multiply, Lparam, Rparam) ->
    Lparam * Rparam;
eval_binary(divide, Lparam, Rparam) ->
    Lparam / Rparam.

eval_unary(positive, Param) ->
    Param;
eval_unary(negative, Param) ->
    -Param.
    
