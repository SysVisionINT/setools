%%
%% SJTools - SysVision Java Tools
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
-module(set_email_util).

-define(PATTERN_POINT_FOLLOWED_BY_POINT, ".+\\.\\..+").
-define(PATTERN_POINT_FOLLOWED_BY_AT, ".+\\.@.+").
-define(PATTERN_AT_FOLLOWED_BY_POINT, ".+@\\..+").
-define(PATTERN_REGULAR_PATTERN, "^[a-zA-Z0-9]+[a-zA-Z0-9+_.-]*@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}$").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_patterns/0, is_valid/1, is_valid/2]).

get_patterns() ->
	case compile_patterns([?PATTERN_POINT_FOLLOWED_BY_POINT, ?PATTERN_POINT_FOLLOWED_BY_AT, ?PATTERN_AT_FOLLOWED_BY_POINT,
	                       ?PATTERN_REGULAR_PATTERN]) of
		[PointFollwedByPoint, PointFollowedByAt, AtFollowedByPoint, Regular] ->
			{ok, {PointFollwedByPoint, PointFollowedByAt, AtFollowedByPoint, Regular}};
		_ -> error
	end.

is_valid(Email) when is_binary(Email) ->
	try
		{ok, EmailPatterns} = get_patterns(),
		is_valid(Email, EmailPatterns)
	catch
		_:Error ->
			error_logger:error_msg("~p:is_valid_email(~p): Error validating email: ~p", [?MODULE, Email, Error]),
			false
	end;
is_valid(_Invalid) -> false.

is_valid(Email, {PointFollwedByPoint, PointFollowedByAt, AtFollowedByPoint, Regular}) when is_binary(Email) ->
	try
		nomatch = re:run(Email, PointFollwedByPoint),
		nomatch = re:run(Email, PointFollowedByAt),
		nomatch = re:run(Email, AtFollowedByPoint),
		{match, [Email]} = re:run(Email, Regular, [{capture, all, binary}]),
		true
	catch
		_:_ -> false
	end;
is_valid(_Invalid, _) -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================
compile_patterns(Patterns) -> compile_patterns(Patterns, []).

compile_patterns([], CompiledPatterns) -> lists:reverse(CompiledPatterns);
compile_patterns([Pattern|Rest], CompiledPatterns) ->
	case re:compile(Pattern) of
		{ok, CompiledPattern} -> compile_patterns(Rest, [CompiledPattern|CompiledPatterns]);
		Error ->
			error_logger:error_msg("~p:compile_patterns([~p | ~p], ...): Error compiling pattern: ~p", [?MODULE, Pattern, Rest, Error]),
			error
	end.