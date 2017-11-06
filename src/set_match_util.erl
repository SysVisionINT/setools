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
-module(set_match_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([is_valid_simple_pattern/1, compile_simple_pattern/1]).

is_valid_simple_pattern(Pattern) ->
	case compile_simple_pattern(Pattern) of
		{ok, _} -> true;
		_ -> false
	end.

compile_simple_pattern(Pattern) ->
	% 1. Change * to .* and . to \.
	Regexp = << (replace_char(Char)) || <<Char>> <= Pattern >>,
	% 2. Compile the regexp pattern
	case re:compile(Regexp) of
		{ok, CompiledPattern} -> {ok, CompiledPattern};
		Error ->
			error_logger:error_msg("~p:compile_simple_pattern(~p): Error compiling pattern ~p: ~p", [?MODULE, Pattern, Regexp, Error]),
			error
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
replace_char($*) -> <<".*">>;
replace_char($.) -> <<"\\.">>;
replace_char(Char) -> <<Char>>.
