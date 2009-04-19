%%% The contents of this file are Open Source.  They may be freely
%%% copied, used or enhanced in any way without restriction provided
%%% acknowledgement of the original author is included in all
%%% distributions of any derivative works.  This file is intended
%%% as an example tutorial only and is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
%%% The software is not guaranteed to work, nor is the author
%%% liable for any damage or lost opportunity caused by the use of
%%% this software.
%%% 
%%%-------------------------------------------------------------------
%%% File    : echoweb.erl
%%% Author  : Jay Nelson <jay@duomark.com>
%%% Description : A simple module to echo browser requests back to
%%%               the browser client.
%%%
%%%   Date    Auth    Desc
%%% 24-03-03   JN   Initial creation
%%%-------------------------------------------------------------------
-module(echoweb).

-export([init/0, terminate/1, server_busy/1, react_to/3]).
-include("web_utils.hrl").

init() ->
    {ok, []}.

terminate(_State) ->
    ok.

% When the server is busy, send a real HTML page.
server_busy(Socket) ->
    gen_tcp:send(Socket, ?SERVER_BUSY),
    ok.

% GET request
react_to(_Server, Socket, Data = <<"GET ", Rest/binary>>) ->

    % Parse the data in the header
    {URL, Hdrs} = web_utils:parse_get_request(Rest),
    Host = web_utils:get_matching_vals(<<"Host">>, Hdrs),

    % Echo it back to the browser
     gen_tcp:send(Socket,
		  concat_binary([<<"HTTP/1.0 200 Ok\r\n">>,
				 <<"Connection: close\r\n">>,
				 <<"Content-Type: text/plain\r\n\r\n">>,
				 Data,
				<<"\r\n\r\n">>,
				<<"-------- Parsed Results -------\r\n">>,
				<<"URL: ">>, URL, <<"\r\n">>,
				Host])),
    gen_tcp:close(Socket),
    ok;

% POST request
react_to(_Server, Socket, Data = <<"POST ", Rest/binary>>) ->

    % Parse the data in the header
    {URL, Hdrs, FormAttrs} = web_utils:parse_post_request(Rest),
    Accept = web_utils:get_matching_attrs(<<"Accept">>, Hdrs),
    Attrs = [concat_binary([Param, <<" -> ">>, Value, <<"\r\n">> ])
	     || [Param,Value] <- FormAttrs],

    % Echo it back to the browser
    gen_tcp:send(Socket,
		 concat_binary([<<"HTTP/1.0 200 Ok\r\n">>,
				<<"Connection: close\r\n">>,
				<<"Content-Type: text/plain\r\n\r\n">>,
				Data,
				<<"\r\n\r\n">>,
				<<"-------- Parsed Results -------\r\n">>,
				<<"URL: ">>, URL, <<"\r\n">>,
				Accept, <<"\r\n">>,
				Attrs])),
    gen_tcp:close(Socket),
    ok;

% All others are considered Bad Requests (although some should be valid).
react_to(_Server, Socket, Other) ->
    gen_tcp:send(Socket, ?BAD_REQUEST),
    gen_tcp:close(Socket),
    {error, bad_request, Other}.
