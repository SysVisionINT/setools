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
-module(handler_gapi_geocode).

-include("set_gapi_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_event/2]).

init(Config) ->
	{Config, #gapi_geocode{}}.

handle_event({key, <<"status">>}, {Config, Geocode}) when is_record(Geocode, gapi_geocode) ->
	% Status initialization
	{Config, {status, Geocode}};

handle_event({key, <<"error_message">>}, {Config, Geocode}) when is_record(Geocode, gapi_geocode) ->
	% Error Message initialization
	{Config, {error_message, Geocode}};

handle_event({key, <<"results">>}, {Config, Geocode}) when is_record(Geocode, gapi_geocode) ->
	% Results initialization
	{Config, {results, [], Geocode}};

handle_event({key, <<"types">>}, {Config, {result, Result, Results, Geocode}}) ->
	% Types initialization
	{Config, {types, [], {Result, Results, Geocode}}};

handle_event({key, <<"types">>}, {Config, {address_component, AddressComponent, AddressComponents, Result, Results, Geocode}}) ->
	% Types initialization
	{Config, {types, [], {AddressComponent, AddressComponents, Result, Results, Geocode}}};

handle_event({key, <<"formatted_address">>}, {Config, {result, Result, Results, Geocode}}) ->
	% Formatted Address initialization
	{Config, {formatted_address, Result, Results, Geocode}};

handle_event({key, <<"address_components">>}, {Config, {result, Result, Results, Geocode}}) ->
	% Address Components initialization
	{Config, {address_components, [], Result, Results, Geocode}};

handle_event({key, <<"long_name">>}, {Config, {address_component, AddressComponent, AddressComponents, Result, Results, Geocode}}) ->
	% Long Name initialization
	{Config, {long_name, AddressComponent, AddressComponents, Result, Results, Geocode}};

handle_event({key, <<"short_name">>}, {Config, {address_component, AddressComponent, AddressComponents, Result, Results, Geocode}}) ->
	% Short Name initialization
	{Config, {short_name, AddressComponent, AddressComponents, Result, Results, Geocode}};

handle_event({key, <<"postcode_localities">>}, {Config, {result, Result, Results, Geocode}}) ->
	% Postalcode Localities initialization
	{Config, {postcode_localities, [], Result, Results, Geocode}};

handle_event({key, <<"geometry">>}, {Config, {result, Result, Results, Geocode}}) ->
	% Geometry initialization
	{Config, {geometry, #gapi_geocode_geometry{}, Result, Results, Geocode}};

handle_event({key, <<"location">>}, {Config, {geometry, Geometry, Result, Results, Geocode}}) ->
	% Location initialization
	{Config, {lat_lng, {undefined, undefined}, {location, Geometry, Result, Results, Geocode}}};

% Generic latitude/longitude
handle_event({key, <<"lat">>}, {Config, {lat_lng, Location, OtherState}}) ->
	% Latitude initialization
	{Config, {latitude, Location, OtherState}};

% Generic latitude/longitude
handle_event({key, <<"lng">>}, {Config, {lat_lng, Location, OtherState}}) ->
	% Latitude initialization
	{Config, {longitude, Location, OtherState}};

handle_event({key, <<"location_type">>}, {Config, {geometry, Geometry, Result, Results, Geocode}}) ->
	% Location Type initialization
	{Config, {location_type, Geometry, Result, Results, Geocode}};

handle_event({key, <<"viewport">>}, {Config, {geometry, Geometry, Result, Results, Geocode}}) ->
	% Viewport initialization - type gapi_bounds
	{Config, {bounds, {undefined, undefined}, {viewport, Geometry, Result, Results, Geocode}}};

handle_event({key, <<"bounds">>}, {Config, {geometry, Geometry, Result, Results, Geocode}}) ->
	% Bounds initialization - type gapi_bounds
	{Config, {bounds, {undefined, undefined}, {bounds, Geometry, Result, Results, Geocode}}};

% Generic bounds
handle_event({key, <<"southwest">>}, {Config, {bounds, Bounds, OtherState}}) ->
	% Southwest initialization
	{Config, {lat_lng, {undefined, undefined}, {southwest, Bounds, OtherState}}};

% Generic bounds
handle_event({key, <<"northeast">>}, {Config, {bounds, Bounds, OtherState}}) ->
	% Southwest initialization
	{Config, {lat_lng, {undefined, undefined}, {northeast, Bounds, OtherState}}};

handle_event({key, <<"partial_match">>}, {Config, {result, Result, Results, Geocode}}) ->
	% Partial Match initialization
	{Config, {partial_match, Result, Results, Geocode}};

handle_event({key, <<"place_id">>}, {Config, {result, Result, Results, Geocode}}) ->
	% Place Id initialization
	{Config, {place_id, Result, Results, Geocode}};


handle_event({string, Value}, {Config, {status, Geocode}}) when is_record(Geocode, gapi_geocode) ->
	% Set the status value
	{Config, Geocode#gapi_geocode{status=Value}};

handle_event({string, Value}, {Config, {error_message, Geocode}}) when is_record(Geocode, gapi_geocode) ->
	% Set the error_message value
	{Config, Geocode#gapi_geocode{error_message=Value}};

handle_event({string, Value}, {Config, {types, Types, OtherState}}) ->
	% Add a type
	{Config, {types, [Value|Types], OtherState}};

handle_event({string, Value}, {Config, {formatted_address, Result, Results, Geocode}}) ->
	% Set the formatted_address value
	{Config, {result, Result#gapi_geocode_result{formatted_address=Value}, Results, Geocode}};

handle_event({string, Value}, {Config, {long_name, AddressComponent, AddressComponents, Result, Results, Geocode}}) ->
	% Set the long_name value
	{Config, {address_component, AddressComponent#gapi_geocode_address_component{long_name=Value}, AddressComponents, Result, Results, Geocode}};

handle_event({string, Value}, {Config, {short_name, AddressComponent, AddressComponents, Result, Results, Geocode}}) ->
	% Set the short_name value
	{Config, {address_component, AddressComponent#gapi_geocode_address_component{short_name=Value}, AddressComponents, Result, Results, Geocode}};

handle_event({string, Value}, {Config, {postcode_localities, PostcodeLocalities, Result, Results, Geocode}}) ->
	% Add a postalcode locality
	{Config, {postcode_localities, [Value|PostcodeLocalities], Result, Results, Geocode}};

handle_event({string, Value}, {Config, {location_type, Geometry, Result, Results, Geocode}}) ->
	% Set the location_type value
	{Config, {geometry, Geometry#gapi_geocode_geometry{location_type=Value}, Result, Results, Geocode}};

handle_event({string, Value}, {Config, {place_id, Result, Results, Geocode}}) ->
	% Set the place_id value
	{Config, {result, Result#gapi_geocode_result{place_id=Value}, Results, Geocode}};

handle_event({float, Value}, {Config, {latitude,{_, Longitude}, OtherState}}) ->
	% Set the latitude
	{Config, {lat_lng, {Value, Longitude}, OtherState}};

handle_event({float, Value}, {Config, {longitude, {Latitude, _}, OtherState}}) ->
	% Set the longitude
	{Config, {lat_lng, {Latitude, Value}, OtherState}};

handle_event({literal, Value}, {Config, {partial_match, Result, Results, Geocode}}) ->
	% Set the partial_match value
	{Config, {result, Result#gapi_geocode_result{partial_match=Value}, Results, Geocode}};


handle_event(start_object, {Config, {results, Results, Geocode}}) ->
	% New Result
	{Config, {result, #gapi_geocode_result{}, Results, Geocode}};

handle_event(start_object, {Config, {address_components, AddressComponents, Result, Results, Geocode}}) ->
	% New Address Component
	{Config, {address_component, #gapi_geocode_address_component{}, AddressComponents, Result, Results, Geocode}};


handle_event(end_array, {Config, {types, Types, {Result, Results, Geocode}}}) ->
	% Set the types array
	{Config, {result, Result#gapi_geocode_result{types=Types}, Results, Geocode}};

handle_event(end_array, {Config, {types, Types, {AddressComponent, AddressComponents, Result, Results, Geocode}}}) ->
	% Set the types array
	{Config, {address_component, AddressComponent#gapi_geocode_address_component{types=Types}, AddressComponents, Result, Results, Geocode}};

handle_event(end_array, {Config, {address_components, AddressComponents, Result, Results, Geocode}}) ->
	% Set the results array
	{Config, {result, Result#gapi_geocode_result{address_components=AddressComponents}, Results, Geocode}};

handle_event(end_array, {Config, {postcode_localities, PostcodeLocalities, Result, Results, Geocode}}) ->
	% Set the postcode_localities array
	{Config, {result, Result#gapi_geocode_result{postcode_localities=PostcodeLocalities}, Results, Geocode}};

handle_event(end_array, {Config, {results, Results, Geocode}}) ->
	% Set the results array
	{Config, Geocode#gapi_geocode{results=Results}};


handle_event(end_object, {Config, {address_component, AddressComponent, AddressComponents, Result, Results, Geocode}}) ->
	% Add element to list
	{Config, {address_components, [AddressComponent|AddressComponents], Result, Results, Geocode}};

handle_event(end_object, {Config, {geometry, Geometry, Result, Results, Geocode}}) ->
	% Set the geometry
	{Config, {result, Result#gapi_geocode_result{geometry=Geometry}, Results, Geocode}};

handle_event(end_object, {Config, {lat_lng, Location, {location, Geometry, Result, Results, Geocode}}}) ->
	% Set the location
	{Config, {geometry, Geometry#gapi_geocode_geometry{location=Location}, Result, Results, Geocode}};

handle_event(end_object, {Config, {bounds, Bounds, {viewport, Geometry, Result, Results, Geocode}}}) ->
	% Set Viewport value
	{Config, {geometry, Geometry#gapi_geocode_geometry{viewport=Bounds}, Result, Results, Geocode}};

handle_event(end_object, {Config, {bounds, Bounds, {bounds, Geometry, Result, Results, Geocode}}}) ->
	% Set Viewport value
	{Config, {geometry, Geometry#gapi_geocode_geometry{bounds=Bounds}, Result, Results, Geocode}};

handle_event(end_object, {Config, {lat_lng, Location, {southwest, {NE, _SW}, OtherState}}}) ->
	% Set the location - type gapi_bounds
	{Config, {bounds, {NE, Location}, OtherState}};

handle_event(end_object, {Config, {lat_lng, Location, {northeast, {_NE, SW}, OtherState}}}) ->
	% Set the location - type gapi_bounds
	{Config, {bounds, {Location, SW}, OtherState}};

handle_event(end_object, {Config, {result, Result, Results, Geocode}}) ->
	% Add element to list
	{Config, {results, [Result|Results], Geocode}};


handle_event(end_json, {_Config, Geocode}) ->
	% Return the record
	Geocode;

% Safe
handle_event(_Event, State) ->
	State.
