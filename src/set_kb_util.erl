%%
%% SETools - SysVision Erlang Tools
%% 
%% Copyright (C) 2017 SysVision - Consultadoria e Desenvolvimento em Sistemas de Informática, Lda.  
%% 
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%%
-module(set_kb_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_header/2, get_headers/2, get_query_params/1]).

get_header(Request, Header) ->
	Headers = kb_action_helper:get_headers(Request),
	case maps:find(Header, Headers) of
		{ok, Content} -> {ok, Content};
		_ -> missing
	end.

% Returns an ordered list of the values of the headers. The header value can also be 'missing' or 'invalid'
get_headers(Request, Headers) ->
	RequestHeaders = kb_action_helper:get_headers(Request),
	Results = get_headers(RequestHeaders, Headers, []),
	{ok, Results}.

get_query_params(Request) ->
	{Args, Request1} = kb_action_helper:get_args(Request),
	{ok, lists:reverse(Args), Request1}.

%% ====================================================================
%% API functions
%% ====================================================================
get_headers(_RequestHeaders, [], AccResult) -> lists:reverse(AccResult);
get_headers(RequestHeaders, [Header|Rest], AccResult) ->
	HeaderValue = case maps:find(Header, RequestHeaders) of
		{ok, Value} -> Value;
		_ -> missing
	end,
	get_headers(RequestHeaders, Rest, [HeaderValue|AccResult]).
