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
-module(handler_gapi_matrix).

-include("set_gapi_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_event/2]).

init(Config) ->
	{Config, #gapi_matrix{}}.


handle_event({key, <<"error_message">>}, {Config, Matrix}) when is_record(Matrix, gapi_matrix) ->
	% Error Message initialization
	{Config, {error_message, Matrix}};

handle_event({key, <<"status">>}, {Config, Matrix}) when is_record(Matrix, gapi_matrix) ->
	% Status initialization
	{Config, {status, Matrix}};

handle_event({key, <<"status">>}, {Config, {element, RowElement, Elements, Rows, Matrix}}) ->
	% Status initialization
	{Config, {status, RowElement, Elements, Rows, Matrix}};

handle_event({key, <<"origin_addresses">>}, {Config, Matrix}) ->
	% Origins initialization
	{Config, {add_address, origin_addresses, [], Matrix}};

handle_event({key, <<"destination_addresses">>}, {Config, Matrix}) ->
	% Destinations initialization
	{Config, {add_address, destination_addresses, [], Matrix}};

handle_event({key, <<"rows">>}, {Config, Matrix}) ->
	% Rows initialization
	{Config, {rows, [], Matrix}};

handle_event({key, <<"elements">>}, {Config, {rows, Rows, Matrix}}) ->
	% Elements initialization
	{Config, {elements, [], Rows, Matrix}};

handle_event({key, <<"duration">>}, {Config, {element, RowElement, Elements, Rows, Matrix}}) ->
	% Duration initialization
	{Config, {duration, #gapi_info{}, RowElement, Elements, Rows, Matrix}};

handle_event({key, <<"duration_in_traffic">>}, {Config, {element, RowElement, Elements, Rows, Matrix}}) ->
	% Duration in traffic initialization
	{Config, {duration_in_traffic, #gapi_info{}, RowElement, Elements, Rows, Matrix}};

handle_event({key, <<"distance">>}, {Config, {element, RowElement, Elements, Rows, Matrix}}) ->
	% Distance initialization
	{Config, {distance, #gapi_info{}, RowElement, Elements, Rows, Matrix}};

handle_event({key, <<"fare">>}, {Config, {element, RowElement, Elements, Rows, Matrix}}) ->
	% Fare initialization
	{Config, {fare, #gapi_fare{}, RowElement, Elements, Rows, Matrix}};

handle_event({key, <<"text">>}, {Config, {InfoField, ElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	% Text field
	{Config, {text, InfoField, ElementInfo, RowElement, Elements, Rows, Matrix}};

handle_event({key, <<"value">>}, {Config, {InfoField, ElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	% Value field
	{Config, {value, InfoField, ElementInfo, RowElement, Elements, Rows, Matrix}};

handle_event({key, <<"currency">>}, {Config, {fare, ElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	% Currency field
	{Config, {currency, fare, ElementInfo, RowElement, Elements, Rows, Matrix}};


handle_event({string, Value}, {Config, {error_message, Matrix}}) when is_record(Matrix, gapi_matrix) ->
	% Set the error_message value
	{Config, Matrix#gapi_matrix{error_message=Value}};

handle_event({string, Value}, {Config, {status, Matrix}}) ->
	% Set the status value
	{Config, Matrix#gapi_matrix{status=Value}};

handle_event({string, Address}, {Config, {add_address, LogicalField, Addresses, Matrix}}) ->
	% Add an address
	{Config, {add_address, LogicalField, [Address|Addresses], Matrix}};

handle_event({string, Value}, {Config, {status, RowElement, Elements, Rows, Matrix}}) ->
	% Set the status value
	{Config, {element, RowElement#gapi_matrix_row_element{status=Value}, Elements, Rows, Matrix}};

handle_event({string, Value}, {Config, {text, InfoField, ElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	NewElementInfo = case ElementInfo of
		Info when is_record(Info, gapi_info) -> ElementInfo#gapi_info{text=Value};
		Fare when is_record(Fare, gapi_fare) -> ElementInfo#gapi_fare{text=Value}
	end,
	{Config, {InfoField, NewElementInfo, RowElement, Elements, Rows, Matrix}};

handle_event({string, Value}, {Config, {currency, fare, ElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	{Config, {fare, ElementInfo#gapi_fare{currency=Value}, RowElement, Elements, Rows, Matrix}};

handle_event({Type, Value}, {Config, {value, InfoField, ElementInfo, RowElement, Elements, Rows, Matrix}}) when Type == integer orelse Type == float ->
	NewElementInfo = case ElementInfo of
		Info when is_record(Info, gapi_info) -> ElementInfo#gapi_info{value=Value};
		Fare when is_record(Fare, gapi_fare) -> ElementInfo#gapi_fare{value=Value}
	end,
	{Config, {InfoField, NewElementInfo, RowElement, Elements, Rows, Matrix}};


handle_event(start_object, {Config, {elements, Elements, Rows, Matrix}}) ->
	% New element
	{Config, {element, #gapi_matrix_row_element{}, Elements, Rows, Matrix}};


handle_event(end_array, {Config, {add_address, origin_addresses, Addresses, Matrix}}) ->
	% Set the origin array
	{Config, Matrix#gapi_matrix{origin_addresses=lists:reverse(Addresses)}};

handle_event(end_array, {Config, {add_address, destination_addresses, Addresses, Matrix}}) ->
	% Set the destination array
	{Config, Matrix#gapi_matrix{destination_addresses=lists:reverse(Addresses)}};

handle_event(end_array, {Config, {rows, Rows, Matrix}}) ->
	% Set the rows array
	{Config, Matrix#gapi_matrix{rows=lists:reverse(Rows)}};

handle_event(end_array, {Config, {elements, Elements, Rows, Matrix}}) ->
	% Add elements the rows array
	{Config, {rows, [lists:reverse(Elements)|Rows], Matrix}};


handle_event(end_object, {Config, {element, RowElement, Elements, Rows, Matrix}}) ->
	% Add element to list
	{Config, {elements, [RowElement|Elements], Rows, Matrix}};

handle_event(end_object, {Config, {duration, RowElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	% Add element to object
	{Config, {element, RowElement#gapi_matrix_row_element{duration=RowElementInfo}, Elements, Rows, Matrix}};

handle_event(end_object, {Config, {duration_in_traffic, RowElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	% Add element to object
	{Config, {element, RowElement#gapi_matrix_row_element{duration_in_traffic=RowElementInfo}, Elements, Rows, Matrix}};

handle_event(end_object, {Config, {distance, RowElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	% Add element to object
	{Config, {element, RowElement#gapi_matrix_row_element{distance=RowElementInfo}, Elements, Rows, Matrix}};

handle_event(end_object, {Config, {fare, RowElementInfo, RowElement, Elements, Rows, Matrix}}) ->
	% Add element to object
	{Config, {element, RowElement#gapi_matrix_row_element{fare=RowElementInfo}, Elements, Rows, Matrix}};


handle_event(end_json, {_Config, Matrix}) ->
	% Return the record
	Matrix;

% Safe
handle_event(_Event, State) ->
	State.
