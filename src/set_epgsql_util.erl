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
-module(set_epgsql_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute_statement/4, execute_statement/5, with_transaction/2]).

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
							error_logger:error_msg("~p:execute_statement(~p, ..., ...): Error executing the prepared statement: ~p", [?MODULE, StatementName, Why]),
							{error, execute_statement}
					end;
				{error, Why} ->
					error_logger:error_msg("~p:execute_statement(~p, ..., ...): Error binding parameters: ~p", [?MODULE, StatementName, Why]),
					{error, binding_parameters}
			end;
		_Error ->
			error_logger:error_msg("~p:execute_statement(~p, ..., ~p): Prepared statement not found", [?MODULE, StatementName, Statements]),
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
			error_logger:error_msg("~p:with_transaction(..., ...): Transaction rollback: ~p:~p~n", [?MODULE, Error, Reason]),
			run_squery(Connection, "ROLLBACK WORK"),
			error
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
		Other -> Other
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
