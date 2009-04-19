-module(p1_parser).
-define(THIS_MODULE, p1_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    case catch yeccpars1(Tokens, false, 0, [], []) of
	error ->
	    Errorline =
		if Tokens == [] -> 0; true -> element(2, hd(Tokens)) end,
	    {error,
	     {Errorline, ?THIS_MODULE, "syntax error at or after this line."}};
	Other ->
	    Other
    end.

parse_and_scan({Mod, Fun, Args}) ->
    case apply(Mod, Fun, Args) of
	{eof, _} ->
	    {ok, eof};
	{error, Descriptor, _} ->
	    {error, Descriptor};
	{ok, Tokens, _} ->
	    yeccpars1(Tokens, {Mod, Fun, Args}, 0, [], [])
    end.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser toplevel.
% Doesn't have to be exported!
return_error(Line, Message) ->
    throw({error, {Line, ?THIS_MODULE, Message}}).


% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {M, F, A}, State, States, Vstack) ->
    case catch apply(M, F, A) of
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?THIS_MODULE, Reason}};
        {ok, Tokens, _Endline} ->
	    case catch yeccpars1(Tokens, {M, F, A}, State, States, Vstack) of
		error ->
		    Errorline = element(2, hd(Tokens)),
		    {error, {Errorline, ?THIS_MODULE,
			     "syntax error at or after this line."}};
		Other ->
		    Other
	    end
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?THIS_MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);

yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


yeccpars2(0, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '==', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(statement, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(5, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(statement, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(7, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(7, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(9, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {param,__1,nil},
 yeccpars2(yeccgoto(params, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(13, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {param,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(params, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(15, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, '==', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {func,__1,__3,__6},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(function, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(19, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(20, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(22, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(23, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {lor,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {land,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(28, '==', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {eq,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(29, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {divide,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(30, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {subtract,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(31, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {add,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(32, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {multiply,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(33, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, '==', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {func,__1,nil,__5},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(function, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(35, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {sequence,__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(statement, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(37, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {negative,__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(38, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {positive,__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(39, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '==', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 exit({parser, __Other, missing_state_in_action_table}).

yeccgoto(expr, 0) ->
 4;
yeccgoto(expr, 1) ->
 39;
yeccgoto(expr, 2) ->
 38;
yeccgoto(expr, 3) ->
 37;
yeccgoto(expr, 16) ->
 17;
yeccgoto(expr, 19) ->
 32;
yeccgoto(expr, 20) ->
 31;
yeccgoto(expr, 21) ->
 30;
yeccgoto(expr, 22) ->
 29;
yeccgoto(expr, 23) ->
 28;
yeccgoto(expr, 24) ->
 27;
yeccgoto(expr, 25) ->
 26;
yeccgoto(expr, 33) ->
 34;
yeccgoto(expr, 35) ->
 4;
yeccgoto(function, 0) ->
 5;
yeccgoto(function, 35) ->
 5;
yeccgoto(params, 9) ->
 11;
yeccgoto(params, 13) ->
 14;
yeccgoto(statement, 0) ->
 7;
yeccgoto(statement, 35) ->
 36;
yeccgoto(__Symbol, __State) ->
 exit({__Symbol, __State, missing_in_goto_table}).


