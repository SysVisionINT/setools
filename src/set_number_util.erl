%%
%% SETools - SysVision Erlang Tools
%% 
%% Copyright (C) 2018 SysVision - Consultadoria e Desenvolvimento em Sistemas de Informática, Lda.  
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
-module(set_number_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([round_float/2, integer_to_bin/2, bin_to_integer/2, get_integer_from_digits_list/2]).

% http://www.codecodex.com/wiki/Round_a_number_to_a_specific_decimal_place#Erlang
round_float(Value, DecimalPlaces) when is_float(Value) andalso is_integer(DecimalPlaces) ->
	P = math:pow(10, DecimalPlaces),
	round(Value * P) / P.

% Alphabet is a string like "0123456789ABCDEFG", with at least 2 chars
integer_to_bin(Integer, Alphabet) when is_integer(Integer) andalso Integer >= 0 andalso is_list(Alphabet) andalso length(Alphabet) > 1 ->
	integer_to_bin(Integer, Alphabet, <<>>);
integer_to_bin(_, _) ->
	error.

% Alphabet is a string like "0123456789ABCDEFG", with at least 2 chars
bin_to_integer(Binary, Alphabet) when is_binary(Binary) andalso is_list(Alphabet) andalso length(Alphabet) > 1 ->
	bin_to_integer(Binary, Alphabet, []);
bin_to_integer(_, _) ->
	error.

get_integer_from_digits_list(List, Base) when is_list(List) ->
	Fun = fun(Digit, {Multiplier, Acc}) ->
		NewAcc = Digit * Multiplier + Acc,
		{Multiplier div Base, NewAcc}
	end,
	StartMultiplier = trunc(math:pow(Base, length(List) - 1)),
	{0, FinalInteger} = lists:foldl(Fun, {StartMultiplier, 0}, List),
	FinalInteger.

%% ====================================================================
%% Internal functions
%% ====================================================================
integer_to_bin(Integer, Alphabet, Acc) when Integer < length(Alphabet) ->
	Char = lists:nth(Integer + 1, Alphabet), % We add one because erlang indexes start at 1
	{ok, <<Char, Acc/binary>>};
integer_to_bin(Integer, Alphabet, Acc) ->
	Length = length(Alphabet),
	NewInteger = Integer div Length,
	Index = Integer rem Length,
	Char = lists:nth(Index + 1, Alphabet), % We add one because erlang indexes start at 1
	integer_to_bin(NewInteger, Alphabet, <<Char, Acc/binary>>).

bin_to_integer(<<>>, Alphabet, Acc) ->
	{ok, get_integer_from_digits_list(lists:reverse(Acc), length(Alphabet))};
bin_to_integer(<<Char:8, Rest/binary>>, Alphabet, Acc) ->
	Index = set_list_util:index_of(Char, Alphabet),
	bin_to_integer(Rest, Alphabet, [Index - 1|Acc]). % We subtract one because erlang indexes start at 1
