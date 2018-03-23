%%
%% SETools - SysVision Erlang Tools
%% 
%% Copyright (C) 2017-18 SysVision - Consultadoria e Desenvolvimento em Sistemas de Informática, Lda.  
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
-module(set_epgsql_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute_statement/4, execute_statement/5, with_transaction/2, execute/4, execute/6, execute/7]).

% Connection - epgsql connection
% Statements - [{StatementName - list(), PreparedStatement - epgsql statement}]
% StatementName - list()
% Parameters - [any()]
execute_statement(Connection, Statements, StatementName, Parameters) ->
	execute_statement(Connection, Statements, StatementName, Parameters, 0).

execute_statement(Connection, Statements, StatementName, Parameters, MaxRows) ->
	% MaxRows 0 means no limit
	case lists:keyfind(StatementName, 1, Statements) of
		{StatementName, Statement} ->
			case run_bind(Connection, Statement, Parameters) of
				ok ->
					case run_execute(Connection, Statement, MaxRows) of
						{Success, Result} when Success =:= ok orelse Success =:= partial -> {ok, Result};
						{error, Why} ->
							error_logger:error_msg("~p:execute_statement(..., ..., ~p, ~p, ...): Error executing the prepared statement: ~p~n",
							                       [?MODULE, StatementName, Parameters, Why]),
							{error, execute_statement}
					end;
				{error, Why} ->
					error_logger:error_msg("~p:execute_statement(..., ..., ~p, ~p, ...): Error binding parameters: ~p~n",
					                       [?MODULE, StatementName, Parameters, Why]),
					{error, binding_parameters}
			end;
		_Error ->
			error_logger:error_msg("~p:execute_statement(..., ~p, ~p, ..., ...): Prepared statement not found", [?MODULE, Statements, StatementName]),
			{error, statement_not_found}
	end.

% Transaction function that uses run_squery
% Connection - epgsql connection
with_transaction(Connection, Function) ->
	try
		{ok, [], []} = run_squery(Connection, "BEGIN WORK"),
		Result = Function(),
		{ok, [], []} = run_squery(Connection, "COMMIT WORK"),
		Result
	catch
		Error:Reason ->
			Trace = erlang:get_stacktrace(),
			error_logger:error_msg("~p:with_transaction(..., ...): Transaction rollback: ERROR ~p:~p~nTRACE: ~p~n", [?MODULE, Error, Reason, Trace]),
			try
				RollbackResult = run_squery(Connection, "ROLLBACK WORK"),
				error_logger:error_msg("~p:with_transaction(..., ...): Rollback result: ~p~n", [?MODULE, RollbackResult])
			catch
				Error2:Reason2 ->
					error_logger:error_msg("~p:with_transaction(..., ...): Error on rollback: ~p:~p~n", [?MODULE, Error2, Reason2])
			end,
			error
	end.

% Sql - string()
% Parameters - [any()]
% Connection - epgsql connection
% Statements - [{StatementName - list(), PreparedStatement - epgsql statement}]
execute(Sql, Parameters, Connection, Statements) ->
	execute(Sql, Parameters, 0, 0, Connection, Statements).

% Sql - string()
% Parameters - [any()]
% MaxRows - integer() (0 means no limit)
% Skip - integer()
% Connection - epgsql connection
% Statements - [{StatementName - list(), PreparedStatement - epgsql statement}]
execute(Sql, Parameters, MaxRows, Skip, Connection, Statements) ->
	case append_offset(Sql, Parameters, Skip) of
		{ok, Sql2, Parameters2} ->
			StatementName = hash(Sql2),
			case get_statement(Connection, StatementName, Sql2, Statements) of
				{Statement, NewStatements} ->
					case run_bind(Connection, Statement, Parameters2) of
						ok ->
							case run_execute(Connection, Statement, MaxRows) of
								{Success, Result} when Success =:= ok orelse Success =:= partial ->
									{ok, Result, NewStatements};
								{error, Why} ->
									error_logger:error_msg("~p:execute(~p, ~p, ..., ..., ..., ...): Error executing the prepared statement (~p): ~p~n", [?MODULE, Sql, Parameters, StatementName, Why]),
									{error, execute_statement, NewStatements}
							end;
						{error, Why} ->
							error_logger:error_msg("~p:execute(~p, ~p, ..., ..., ..., ...): Error binding parameters to prepared statement (~p): ~p~n", [?MODULE, Sql, Parameters, StatementName, Why]),
							{error, binding_parameters, NewStatements}
					end;
				Error ->
					error_logger:error_msg("~p:execute(~p, ~p, ..., ..., ..., ...): Error getting prepared statement (~p): ~p~n", [?MODULE, Sql, Parameters, StatementName, Error]),
					{error, get_statement, Statements}
			end;
		Error ->
			error_logger:error_msg("~p:execute(~p, ~p, ..., ..., ..., ...): Error appending offset to query (~p): ~p~n", [?MODULE, Sql, Parameters, hash(Sql), Error]),
			{error, append_offset, Statements}
	end.

% Sql - string()
% Parameters - [any()]
% MaxRows - integer() (0 means no limit)
% Skip - integer()
% RecordName - atom()
% Connection - epgsql connection
% Statements - [{StatementName - list(), PreparedStatement - epgsql statement}]
execute(Sql, Parameters, MaxRows, Skip, RecordName, Connection, Statements) when is_atom(RecordName) ->
	case execute(Sql, Parameters, MaxRows, Skip, Connection, Statements) of
		{ok, Result, NewStatements} ->
			Records = [erlang:insert_element(1, Row, RecordName) || Row <- Result],
			{ok, Records, NewStatements};
		Error -> Error
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% Errors originating from the PostgreSQL backend are returned as {error, #error{}}, see epgsql.hrl for the record definition.
%% epgsql functions may also return {error, What} where What is one of the following:
%%     {unsupported_auth_method, Method} - required auth method is unsupported
%%     timeout - request timed out
%%     closed - connection was closed
%%     sync_required - error occured and epgsql:sync must be called
run_bind(Connection, Statement, Parameters) ->
	case epgsql:bind(Connection, Statement, Parameters) of
		{error, sync_required} ->
			case epgsql:sync(Connection) of
				ok -> run_bind(Connection, Statement, Parameters);
				Other -> Other
			end;
		Other -> Other
	end.

run_execute(Connection, Statement, MaxRows) ->
	% MaxRows value 0 means no limit. On insert/update/delete the return is just one line, as we expect
	case epgsql:execute(Connection, Statement, MaxRows) of
		{error, sync_required} ->
			case epgsql:sync(Connection) of
				ok -> run_execute(Connection, Statement, MaxRows);
				Other -> Other
			end;
		{Success, Result} ->
			case Statement of
				{statement, _, Columns, _} when length(Columns) > 0 ->
					{Success, fix_types(Result, Columns)};
				_ -> {Success, Result}
			end
	end.

run_squery(Connection, SQL) ->
	case epgsql:squery(Connection, SQL) of
		{error, sync_required} ->
			case epgsql:sync(Connection) of
				ok -> run_squery(Connection, SQL);
				Other -> Other
			end;
		Other -> Other
	end.

run_parse(Connection, Name, Sql) ->
	case epgsql:parse(Connection, Name, Sql, []) of
		{error, sync_required} ->
			case epgsql:sync(Connection) of
				ok -> run_parse(Connection, Name, Sql);
				Other -> Other
			end;
		{error,{_,_,_,duplicate_prepared_statement,_,_}} ->
			epgsql:describe(Connection, statement, Name);
		Other -> Other
	end.

get_statement(Connection, StatementName, Sql, Statements) ->
	case proplists:get_value(StatementName, Statements) of
		undefined ->
			{ok, Statement} = run_parse(Connection, StatementName, Sql),
			{Statement, [{StatementName, Statement}|Statements]};
		Statement ->
			{Statement, Statements}
	end.

hash(Str) ->
	hex:bin_to_hexstr(crypto:hash(md5, Str)).

append_offset(Sql, Parameters, 0) -> {ok, Sql, Parameters};
append_offset(Sql, Parameters, Offset) when is_integer(Offset), Offset > 0 ->
	NewSql = Sql ++ " OFFSET $" ++ erlang:integer_to_list(length(Parameters) + 1),
	NewParameters = Parameters ++ [Offset],
	{ok, NewSql, NewParameters};
append_offset(_Sql, _Parameters, _Offset) -> error.

fix_types([], _Columns) -> [];
fix_types([Row|Rows], Columns) ->
	RowList = erlang:tuple_to_list(Row),
	FixedRowList = [fix_type(Type, Elem) || {Elem, {column, _, Type, _, _, _}} <- lists:zip(RowList, Columns)],
	FixedRow = erlang:list_to_tuple(FixedRowList),
	[FixedRow|fix_types(Rows, Columns)].

fix_type(numeric, null) -> null;
fix_type(numeric, Value) when is_binary(Value) -> erlang:binary_to_float(Value);
fix_type(_Type, Value) -> Value.
