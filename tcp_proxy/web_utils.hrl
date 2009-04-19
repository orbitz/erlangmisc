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
%%% File    : web_utils.hrl
%%% Author  : Jay Nelson <jay@duomark.com>
%%% Description : Constants used in generating HTML.
%%%  See http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
%%%
%%%   Date    Auth    Desc
%%% 03-04-13   JN   Initial creation
%%%-------------------------------------------------------------------
-define(SERVER_BUSY,
	<<"HTTP/1.0 503 Service Unavailable\r\n",
	 "Connection: close\r\n",
	 "Content-Type: text/html\r\n\r\n",
	 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n",
	 "<!DOCTYPE html\r\n",
	 "PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\r\n",
	 "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\r\n",
	 "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\r\n",
	 "<html><head>\r\n",
	 "<title>503 Service Unavailable</title>\r\n",
	 "</head><body>\r\n",
	 "<h1>Error 503: Service Unavailable</h1>\r\n",
	 "<h3>The server is busy</h3>\r\n",
	 "</body></html>\r\n">>).

-define(BAD_REQUEST,
	<<"HTTP/1.0 400 Bad Request\r\n",
	 "Connection: close\r\n",
	 "Content-Type: text/html\r\n\r\n",
	 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n",
	 "<!DOCTYPE html\r\n",
	 "PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\r\n",
	 "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\r\n",
	 "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\r\n",
	 "<html><head>\r\n",
	 "<title>400 Bad Request</title>\r\n",
	 "</head><body>\r\n",
	 "<h1>Error 400: Bad Request</h1>\r\n",
	 "</body></html>\r\n">>).

-define(ACCESS_BLOCKED,
	<<"HTTP/1.0 403 Forbidden\r\n",
	 "Connection: close\r\n",
	 "Content-Type: text/html\r\n\r\n",
	 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n",
	 "<!DOCTYPE html\r\n",
	 "PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\r\n",
	 "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\r\n",
	 "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\r\n",
	 "<html><head>\r\n",
	 "<title>403 Forbidden</title>\r\n",
	 "</head><body>\r\n",
	 "<h1>Error 403: Forbidden</h1>\r\n",
	 "<h3>Access is blocked to this document</h3>\r\n",
	 "</body></html>\r\n">>).

-define(GATEWAY_TIMEOUT,
	<<"HTTP/1.0 504 Gateway Timeout\r\n",
	 "Connection: close\r\n",
	 "Content-Type: text/html\r\n\r\n",
	 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n",
	 "<!DOCTYPE html\r\n",
	 "PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\r\n",
	 "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\r\n",
	 "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\r\n",
	 "<html><head>\r\n",
	 "<title>504 Gateway Timeout</title>\r\n",
	 "</head><body>\r\n",
	 "<h1>Error 504: The server did not respond to the proxy request</h1>\r\n",
	 "</body></html>\r\n">>).

-define(BAD_GATEWAY,
	<<"HTTP/1.0 502 Bad Gateway\r\n",
	 "Connection: close\r\n",
	 "Content-Type: text/plain\r\n\r\n",
	 "502 Bad Gateway\r\n">>).

