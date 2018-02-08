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
-module(handler_gapi_snap_to_road).

-include("set_gapi_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_event/2]).

% ReturnType: simple / full
init(ReturnType) ->
	ReturnType.


handle_event({key, <<"snappedPoints">>}, ReturnType) ->
	% Start with an empty list
	{ReturnType, {snapped_points, []}};

handle_event({key, <<"location">>}, {ReturnType, {snapped_point, SnappedPoint, SnappedPoints}}) ->
	% Create a new location - type gapi_position()
	{ReturnType, {location, {undefined, undefined}, SnappedPoint, SnappedPoints}};

handle_event({key, <<"latitude">>}, {ReturnType, {location, Location, SnappedPoint, SnappedPoints}}) ->
	% Prepare to set the latitude
	{ReturnType, {latitude, Location, SnappedPoint, SnappedPoints}};

handle_event({key, <<"longitude">>}, {ReturnType, {location, Location, SnappedPoint, SnappedPoints}}) ->
	% Prepare to set the longitude
	{ReturnType, {longitude, Location, SnappedPoint, SnappedPoints}};

handle_event({key, <<"originalIndex">>}, {full, {snapped_point, SnappedPoint, SnappedPoints}}) ->
	% Prepare to set the original_index
	{full, {original_index, SnappedPoint, SnappedPoints}};

handle_event({key, <<"placeId">>}, {full, {snapped_point, SnappedPoint, SnappedPoints}}) ->
	% Prepare to set the place_id
	{full, {place_id, SnappedPoint, SnappedPoints}};


handle_event({float, Latitude}, {ReturnType, {latitude, {_, Longitude}, SnappedPoint, SnappedPoints}}) ->
	% Set the latitude
	{ReturnType, {location, {Latitude, Longitude}, SnappedPoint, SnappedPoints}};

handle_event({float, Longitude}, {ReturnType, {longitude, {Latitude, _}, SnappedPoint, SnappedPoints}}) ->
	% Set the longitude
	{ReturnType, {location, {Latitude, Longitude}, SnappedPoint, SnappedPoints}};

handle_event({integer, Value}, {full, {original_index, SnappedPoint, SnappedPoints}}) ->
	% Set the original_index
	{full, {snapped_point, SnappedPoint#gapi_snapped_point{original_index=Value}, SnappedPoints}};

handle_event({string, Value}, {full, {place_id, SnappedPoint, SnappedPoints}}) ->
	% Set the place_id
	{full, {snapped_point, SnappedPoint#gapi_snapped_point{place_id=Value}, SnappedPoints}};


handle_event(start_object, {ReturnType, {snapped_points, SnappedPoints}}) ->
	% Create a new point
	{ReturnType, {snapped_point, #gapi_snapped_point{}, SnappedPoints}};


handle_event(end_object, {ReturnType, {snapped_point, SnappedPoint, SnappedPoints}}) ->
	% Add the point to the list
	{ReturnType, {snapped_points, [SnappedPoint|SnappedPoints]}};

handle_event(end_object, {simple, {location, Location, _SnappedPoint, SnappedPoints}}) ->
	% Assign the location
	{simple, {snapped_point, Location, SnappedPoints}};

handle_event(end_object, {ReturnType, {location, Location, SnappedPoint, SnappedPoints}}) ->
	% Assign the location
	{ReturnType, {snapped_point, SnappedPoint#gapi_snapped_point{location=Location}, SnappedPoints}};


handle_event(end_array, {_ReturnType, {snapped_points, SnappedPoints}}) ->
	% Final operation
	lists:reverse(SnappedPoints);

% Safe
handle_event(_Event, State) ->
	State.
