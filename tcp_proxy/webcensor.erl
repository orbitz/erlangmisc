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
%%% File    : webcensor.erl
%%% Author  : Jay Nelson <jay@duomark.com>
%%% Description : A simple proxy server that blocks websites that are
%%%               contained in a banned list.
%%%
%%%   Date    Auth    Desc
%%% 03-04-13   JN   Initial creation
%%%-------------------------------------------------------------------
-module(webcensor).

-export([init/0, add_url/3, terminate/1, server_busy/1, react_to/3]).
-include("web_utils.hrl").
-define(BLOCK_LIST_FILE, "webcensor.block").
-define(BLOCK_LIST_TABLE, 'WebCensor').

init() ->
    Table = ets:new(?BLOCK_LIST_TABLE, [ordered_set, named_table]),
    URLs = file:read_file(?BLOCK_LIST_FILE),
    case URLs of
	{ok, Binary} ->
	    Sites = bin_utils:split_lines(Binary),
	    io:format("~w blocked websites loaded~n", [length(Sites)]),
	    load(Table, Sites);
	Other ->
	    ok
    end,
    {ok, Table}.


% Load the table of URLs to block
load(Table, [URL | More]) ->
    add_url(init, Table, URL),
    load(Table, More);
load(Table, []) ->
    ok.

% Used by load and as an external server call
add_url(From, Table, URL) when list(URL) ->
    add_url(From, Table, list_to_binary(URL));
add_url(_From, Table, URL) when binary(URL) ->
    case URL of
	<<"http://", _Rest/binary>> ->
	    ets:insert(Table, {URL});
	_Other ->
	    ets:insert(Table, {<<"http://", URL/binary>>})
    end,
    {ok, Table}.

% Write the table to disk in case new URLs were added    
terminate(Table) ->
    AllURLs = [<<URL/binary, "\n">> || {URL} <- ets:tab2list(Table)],
    file:write_file(?BLOCK_LIST_FILE, concat_binary(AllURLs)),
    ok.

% When the server is busy, send a real HTML page.
server_busy(Client) ->
    gen_tcp:send(Client, ?SERVER_BUSY),
    ok.

% Retrieve a GET page if it is not blocked
react_to(Server, Client, Data = <<"GET ", Rest/binary>>) ->
    {URL, Hdrs} = web_utils:parse_get_request(Rest),

    %% Uncomment this line to see what pages are requested
    %% io:format("~s~n", [binary_to_list(URL)]),

    case ets:member(?BLOCK_LIST_TABLE, URL) of
	true ->
	    gen_tcp:send(Client, ?ACCESS_BLOCKED);
	false ->
	    [Host] = web_utils:get_matching_vals(<<"Host">>, Hdrs),
	    get_html_page(Client, URL, Host, Hdrs, Data)
    end,
    gen_tcp:close(Client),
    ok;

% Retrieve a POST page if it is not blocked
react_to(Server, Client, Data = <<"POST ", Rest/binary>>) ->
    {URL, Hdrs, FormAttrs} = web_utils:parse_post_request(Rest),
    case ets:member(?BLOCK_LIST_TABLE, URL) of
	true ->
	    gen_tcp:send(Client, ?ACCESS_BLOCKED);
	false ->
	    [Host] = web_utils:get_matching_vals(<<"Host">>, Hdrs),
	    post_html_page(Client, URL, Host, Hdrs, FormAttrs, Data)
    end,
    gen_tcp:close(Client),
    ok;
   
% All others are considered Bad Requests (although some should be valid).
react_to(Server, Client, Other) ->
    gen_tcp:send(Client, ?BAD_REQUEST),
    gen_tcp:close(Client),
    {error, {bad_request, Other}}.



%%--------------------------------------------------------------------
%% HTML parsing functions to reformat the request
%%--------------------------------------------------------------------

connect(Host) ->
    case bin_utils:split_first($:, Host) of
	[Hostname, Port] ->
	    connect(Hostname, Port);
	[Hostname] ->
	    connect(Hostname, 80);
	Other ->
	    {error, unknown}
    end.
connect(Host, Port) when binary(Host) ->
    connect(binary_to_list(Host), Port);
connect(Host, Port) when list(Host) ->
    %% Uncomment this line to see what servers are connected to
    %% io:format("~s~s~n", ["Connecting to ", Host]),
    gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, 0}]).

send_bad_gateway(Client, Error) ->
    gen_tcp:send(Client, ?BAD_GATEWAY),
    gen_tcp:send(Client, lists:flatten(io_lib:print(Error, 1, 30, 100))),
    gen_tcp:send(Client, <<"\r\n">>).
    
% URL and Hdrs are provided in case there are special cases to consider
get_html_page(Client, _URL, Host, _Hdrs, Data) ->
    case connect(Host) of

	% Get the page
	{ok, Socket} ->
	    gen_tcp:send(Socket, Data),
	    receive_page(Client, Socket),
	    gen_tcp:close(Socket);
	
	% Report the error
	{error, Reason} ->
	    send_bad_gateway(Client, Reason);
	Other ->
	    send_bad_gateway(Client, Other)
    end.


% URL and Hdrs are provided in case there are special cases to consider
post_html_page(Client, _URL, Host, _Hdrs, _FormAttrs, Data) ->
    case connect(Host) of

	% Get the page
	{ok, Socket} ->
	    gen_tcp:send(Socket, Data),
	    receive_page(Client, Socket),
	    gen_tcp:close(Socket);
	
	% Report the error
	{error, Reason} ->
	    send_bad_gateway(Client, Reason);
	Other ->
	    send_bad_gateway(Client, Other)
    end.


receive_page(Client, Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
	{ok, Bin} ->
	    gen_tcp:send(Client, Bin),
	    receive_page(Client, Socket);
	{error, closed} ->
	    ok;
	{error, timeout} ->
	    gen_tcp:send(Client, ?GATEWAY_TIMEOUT);
	Other ->
	    send_bad_gateway(Client, Other)
    end.


