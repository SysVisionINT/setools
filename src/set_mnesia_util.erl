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
-module(set_mnesia_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([delete/2, select_delete/2, write/2, select/2, read/2, copy_cache_tables/2]).

delete(Table, Key) ->
	case mnesia:transaction(fun() -> mnesia:delete(Table, Key, write) end) of
		{atomic, ok} -> ok;
		{aborted, Error} -> {nok, Error}
	end.

% MatchSpec has to return a list of Keys
select_delete(Table, MatchSpec) ->
	case mnesia:transaction(fun() -> [mnesia:delete(Table, Key, write) || Key <- mnesia:select(Table, MatchSpec)], removed end) of
		{atomic, removed} -> ok;
		{aborted,  Error} -> {nok, Error}
	end.

write(Table, Record) ->
	case mnesia:transaction(fun() -> mnesia:write(Table, Record, write) end) of
		{atomic, ok} -> ok;
		{aborted, Error} -> {nok, Error}
	end.

select(Table, MatchSpec) ->
	case mnesia:transaction(fun() -> mnesia:select(Table, MatchSpec) end) of
		{atomic, Results} -> Results;
		{aborted, Error} -> {nok, Error}
	end.

read(Table, Key) ->
	case mnesia:transaction(fun() -> mnesia:read(Table, Key) end) of
		{atomic, Results} -> Results;
		{aborted, Error} -> {nok, Error}
	end.

copy_cache_tables(FromNode, Table) ->
	% Copy the table to our node
	case mnesia:change_config(extra_db_nodes, [FromNode]) of
		{ok, _} ->
			case mnesia:add_table_copy(Table, node(), ram_copies) of
				{atomic, ok} -> ok;
				{aborted, Error} -> {nok, Error}
			end;
		{error, Reason} -> {nok, Reason}
	end.
