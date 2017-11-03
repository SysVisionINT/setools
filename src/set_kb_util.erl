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
-export([get_header/2]).

get_header(Request, Header) ->
	{Headers, Request1} = kb_action_helper:get_headers(Request),
	case proplists:get_value(Header, Headers) of
		undefined -> {missing, Request1};
		Value when is_binary(Value) -> {ok, Value, Request1};
		_ -> {invalid, Request1}
	end.
