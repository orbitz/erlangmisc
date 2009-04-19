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
%%% File    : web_utils.erl
%%% Author  : Jay Nelson <jay@duomark.com>
%%% Description : A simple module to echo browser requests back to
%%%               the browser client.
%%%
%%%   Date    Auth    Desc
%%% 03-04-13   JN   Initial creation
%%%-------------------------------------------------------------------
-module(web_utils).

-export([parse_get_request/1, parse_post_request/1, get_matching_attrs/2,
	 get_matching_vals/2]).

%%--------------------------------------------------------------------
%% HTML parsing functions to reformat the request
%%--------------------------------------------------------------------

parse_get_request(Data) ->
    [Request | Attrs] = bin_utils:split_http_lines(Data),
    case Request of
	<<"http://", More/binary>> ->
	    Endpos = bin_utils:find_next(More, <<$ >>),
	    case Endpos of
		none ->
		    URL = <<>>;
		Endpos ->
		    URL = bin_utils:sub_binary(Request, 1, Endpos+7)
	    end;
	<<"/", More/binary>> ->
	    Endpos = bin_utils:find_next(More, <<$ >>),
	    case Endpos of
		none ->
		    URL = <<>>;
		Endpos ->
		    URL = bin_utils:sub_binary(Request, 1, Endpos+7)
	    end;
	_Other ->
	    URL = {unknown_type, binary_to_list(Request)}
    end,
    Pairs = [bin_utils:split_first(<<$:>>, Attrib) || Attrib <- Attrs],
    {URL, Pairs}.

parse_post_request(Data) ->
    BodyPos = bin_utils:find_next(Data, <<"\r\n\r\n">>),
    Hdrs = bin_utils:sub_binary(Data, 1, BodyPos),
    Body = bin_utils:sub_binary(Data, BodyPos+4),
    [Request | Attrs] = bin_utils:split_http_lines(Hdrs),
    case Request of
	<<"http://", More/binary>> ->
	    Endpos = bin_utils:find_next(More, <<$ >>),
	    case Endpos of
		none ->
		    URL = <<>>;
		Endpos ->
		    URL = bin_utils:sub_binary(Request, 1, Endpos+7)
	    end;
	_Other ->
	    URL = {unknown_type, binary_to_list(Request)}
    end,
    Pairs = [bin_utils:split_first(<<$:>>, Attrib) || Attrib <- Attrs],
    BodyPairs = bin_utils:split_tokens(<<$&>>, Body),
    BodyProps = [bin_utils:split_tokens(<<$=>>, Attrib) || Attrib <- BodyPairs],
    {URL, Pairs, BodyProps}.

get_matching_attrs(Pre, Attribs) when binary(Pre) ->
    [concat_binary([A,<<$:>>,V,<<"\r\n">>]) || [A,V] <- Attribs,
					       bin_utils:starts_with(Pre,A)].

get_matching_vals(Pre, Attribs) when binary(Pre) ->
    [bin_utils:sub_binary(V,2) || [A,V] <- Attribs,
				  bin_utils:starts_with(Pre,A)].




