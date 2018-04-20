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
-module(set_list_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([split/2, index_of/2]).

split(List, Size) when is_list(List) andalso is_integer(Size) andalso Size > 0 ->
	inner_split(List, Size);
split(_, _) ->
	error.

index_of(Value, List) when is_list(List) -> index_of(Value, List, 1);
index_of(_, _) ->
	error.

%% ====================================================================
%% Internal functions
%% ====================================================================
inner_split([], _) ->
	{ok, []};
inner_split(List, Size) when Size >= length(List) ->
	{ok, [List]};
inner_split(List, Size) ->
	inner_split(List, Size, 0, [], []).

inner_split([], _, _, Acc, GlobalAcc) ->
	{ok, lists:reverse([lists:reverse(Acc)|GlobalAcc])};
inner_split(List, Size, Size, Acc, GlobalAcc) ->
	inner_split(List, Size, 0, [], [lists:reverse(Acc)|GlobalAcc]);
inner_split([Element|Rest], Size, Count, Acc, GlobalAcc) ->
	inner_split(Rest, Size, Count + 1, [Element|Acc], GlobalAcc).

index_of(_, [], _) -> not_found;
index_of(Value, [Value|_], Index) -> Index;
index_of(Value, [_|Rest], Index) -> index_of(Value, Rest, Index + 1).
