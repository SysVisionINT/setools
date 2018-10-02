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
-module(set_gapi_util).

-include("set_gapi_constants.hrl").

-define(URL_ENCODED_COLON, "%3A").
-define(URL_ENCODED_PIPE,  "%7C").

%% ====================================================================
%% Types
%% ====================================================================
-type gapi_position() :: {Latitude :: float(), Longitude :: float()}.
-type gapi_location() :: string() | binary() | gapi_position().
-type gapi_bounds() :: {Northeast :: gapi_position(), Southwest :: gapi_position()}.

-type gapi_travel_mode() :: driving | walking | bicycling | transit.
-type gapi_avoid() :: tolls | highways | ferries | indoor.
-type gapi_units() :: metric | imperial.
-type gapi_traffic_model() :: best_guess | pessimistic | optimistic.
-type gapi_transit_mode() :: bus | subway | train | tram | rail.
-type gapi_transit_routing_preference() :: less_walking | fewer_transfers.

-type gapi_component() :: {string() | binary(), string() | binary()}.

-type gapi_matrix_config_element() :: {travel_mode, gapi_travel_mode()} |
                                      {avoid, gapi_avoid()} |
                                      {units, gapi_units()} |
                                      {arrival_time, integer()} | % Seconds from 1/1/1970 UTC
                                      {departure_time, integer()} | % Seconds from 1/1/1970 UTC
                                      {traffic_model, gapi_traffic_model()} |
                                      {transit_mode, gapi_transit_mode()} |
                                      {transit_routing_preference, gapi_transit_routing_preference()}.
-type gapi_directions_config_element() :: {mode, gapi_travel_mode()} |
                                          {alternatives, boolean()} |
                                          {avoid, gapi_avoid()} |
                                          {language, string() | binary()} |
                                          {units, gapi_units()} |
                                          {region, string() | binary()} |
                                          {arrival_time, integer()} | % Seconds from 1/1/1970 UTC
                                          {departure_time, integer()} | % Seconds from 1/1/1970 UTC
                                          {traffic_model, gapi_traffic_model()} |
                                          {transit_mode, gapi_transit_mode()} |
                                          {transit_routing_preference, gapi_transit_routing_preference()}.
-type gapi_geocode_config_element() :: {bounds, gapi_bounds()} |
                                       {language, string() | binary()} |
                                       {region, string() | binary()}.
-type gapi_reverse_geocode_config_element() :: {language, string() | binary()} |
                                               {result_type, [string() | binary()]} |
                                               {location_type, [string() | binary()]}.

-export_type([gapi_position/0, gapi_location/0, gapi_bounds/0, gapi_travel_mode/0, gapi_avoid/0, gapi_units/0, gapi_traffic_model/0,
              gapi_transit_mode/0, gapi_transit_routing_preference/0, gapi_component/0, gapi_matrix_config_element/0,
              gapi_directions_config_element/0, gapi_geocode_config_element/0, gapi_reverse_geocode_config_element/0]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([snap_to_roads/2, snap_to_roads/3,
         get_route/2, get_route/3,
         matrix/4, matrix/5,
         directions/6, directions/7,
		 geocode/4, geocode/5,
		 reverse_geocode/3, reverse_geocode/4]).
%TODO acrescentar funcoes sem parametros opcionais

-spec snap_to_roads(ApiKey :: string(), Locations :: [gapi_position()]) -> {ok, NewPositions :: [#gapi_snapped_point{}]} | {nok, Error :: any()}.
snap_to_roads(ApiKey, Locations) ->
	snap_to_roads(?GAPI_ENDPOINT_SNAP_TO_POINTS, ApiKey, Locations).

-spec snap_to_roads(Endpoint :: string(), ApiKey :: string(), Locations :: [gapi_position()]) -> {ok, NewPositions :: [#gapi_snapped_point{}]} | {nok, Error :: any()}.
snap_to_roads(Endpoint, ApiKey, Locations) ->
	snap_to_roads(Endpoint, ApiKey, Locations, full).

-spec get_route(ApiKey :: string(), Positions :: [gapi_position()]) -> {ok, Route :: #gapi_route{}} | {nok, Error :: any()}.
get_route(ApiKey, Positions) ->
	get_route(?GAPI_ENDPOINT_SNAP_TO_POINTS, ApiKey, Positions).

-spec get_route(Endpoint :: string(), ApiKey :: string(), Positions :: [gapi_position()]) -> {ok, Route :: #gapi_route{}} | {nok, Error :: any()}.
get_route(Endpoint, ApiKey, Positions) ->
	case snap_to_roads(Endpoint, ApiKey, Positions, simple) of
		{ok, SnappedPoints} ->
			Distance = calculate_distance(SnappedPoints),
			DistanceInMeters = round(Distance * 1000),
			Polyline = noesis_polyline:encode(SnappedPoints),
			#gapi_route{distance=DistanceInMeters, polyline=Polyline};
		Other -> {nok, Other}
	end.

-spec matrix(ApiKey :: string(), Origins :: [gapi_location()], Destinations :: [gapi_location()], Config :: [gapi_matrix_config_element()]) -> {ok, Matrix :: #gapi_matrix{}} | {nok, Error :: any()}.
matrix(ApiKey, Origins, Destinations, Config) ->
	matrix(?GAPI_ENDPOINT_MATRIX, ApiKey, Origins, Destinations, Config).

-spec matrix(Endpoint :: string(), ApiKey :: string(), Origins :: [gapi_location()], Destinations :: [gapi_location()], Config :: [gapi_matrix_config_element()]) -> {ok, Matrix :: #gapi_matrix{}} | {nok, Error :: any()}.
matrix(Endpoint, ApiKey, Origins, Destinations, Config) ->
	HandlerFun = jsx:decoder(handler_gapi_matrix, [], jsx_config:extract_config([])),
	EncodedOrigins = encode_locations(Origins),
	EncodedDestinations = encode_locations(Destinations),
	% Optional parts
	EncodedConfig = encode_config(Config),
	UrlParts = [Endpoint, "json?key=", ApiKey, "&origins=", EncodedOrigins, "&destinations=", EncodedDestinations, EncodedConfig],
	RequestUrl = lists:flatten(UrlParts),
	%% timeout in ms
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} ->
			{ok, HandlerFun(list_to_binary(ResponseBody))};
		Other -> {nok, Other}
	end.

-spec directions(ApiKey :: string(), Origin :: gapi_location(), Destination :: gapi_location(), Waypoints :: [gapi_location()],
                 Optimize :: boolean(), Config :: [gapi_directions_config_element()]) -> {ok, Directions :: #gapi_directions{}} | {nok, Error :: any()}.
directions(ApiKey, Origin, Destination, Waypoints, Optimize, Config) ->
	directions(?GAPI_ENDPOINT_DIRECTIONS, ApiKey, Origin, Destination, Waypoints, Optimize, Config).

-spec directions(Endpoint :: string(), ApiKey :: string(), Origin :: gapi_location(), Destination :: gapi_location(), Waypoints :: [gapi_location()],
                 Optimize :: boolean(), Config :: [gapi_directions_config_element()]) -> {ok, Directions :: #gapi_directions{}} | {nok, Error :: any()}.
directions(Endpoint, ApiKey, Origin, Destination, Waypoints, Optimize, Config) ->
	HandlerFun = jsx:decoder(handler_gapi_directions, [], jsx_config:extract_config([])),
	EncodedOrigin = encode_location(Origin),
	EncodedDestination = encode_location(Destination),
	% Optional parts
	EncodedWaypoints = encode_waypoints(Waypoints, Optimize),
	EncodedConfig = encode_config(Config),
	UrlParts = [Endpoint, "json?key=", ApiKey, "&origin=", EncodedOrigin, "&destination=", EncodedDestination, EncodedWaypoints, EncodedConfig],
	RequestUrl = lists:flatten(UrlParts),
	%% timeout in ms
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} ->
			{ok, HandlerFun(list_to_binary(ResponseBody))};
		Other -> {nok, Other}
	end.

-spec geocode(ApiKey :: string(), Address :: string() | binary(), Components :: [gapi_component()], Config :: [gapi_geocode_config_element()]) ->
              {ok, Geocode :: #gapi_geocode{}} | {nok, Error :: any()}.
geocode(ApiKey, Address, Components, Config) ->
	geocode(?GAPI_ENDPOINT_GEOCODE, ApiKey, Address, Components, Config).

 -spec geocode(Endpoint :: string(), ApiKey :: string(), Address :: string() | binary(), Components :: [gapi_component()],
               Config :: [gapi_geocode_config_element()]) -> {ok, Geocode :: #gapi_geocode{}} | {nok, Error :: any()}.
geocode(Endpoint, ApiKey, Address, Components, Config) ->
	EncodedAddress = case Address of
		undefined -> [];
		_ -> ["&address=", encode_address(Address)]
	end,
	EncodedComponents = encode_components(Components),
	EncodedConfig = encode_config(Config),
	UrlParts = [Endpoint, "json?key=", ApiKey, EncodedAddress, EncodedComponents, EncodedConfig],
	geocode(UrlParts).

-spec reverse_geocode(ApiKey :: string(), Position :: gapi_position(), Config :: [gapi_reverse_geocode_config_element()]) ->
                      {ok, Geocode :: #gapi_geocode{}} | {nok, Error :: any()}.
reverse_geocode(ApiKey, Position, Config) ->
	reverse_geocode(?GAPI_ENDPOINT_GEOCODE, ApiKey, Position, Config).

-spec reverse_geocode(Endpoint :: string(), ApiKey :: string(), Position :: gapi_position(), Config :: [gapi_reverse_geocode_config_element()]) ->
                      {ok, Geocode :: #gapi_geocode{}} | {nok, Error :: any()}.
reverse_geocode(Endpoint, ApiKey, Position, Config) ->
	EncodedPosition = encode_location(Position),
	EncodedConfig = encode_config(Config),
	UrlParts = [Endpoint, "json?key=", ApiKey, "&latlng=", EncodedPosition, EncodedConfig],
	geocode(UrlParts).

%% ====================================================================
%% Internal functions
%% ====================================================================
snap_to_roads(Endpoint, ApiKey, Locations, ReturnType) ->
	case set_list_util:split(Locations, ?GAPI_SNAP_TO_ROAD_MAX_POSITIONS) of
		{ok, Lists} ->
			HandlerFun = jsx:decoder(handler_gapi_snap_to_road, ReturnType, jsx_config:extract_config([])),
			snap_to_roads(Endpoint, ApiKey, Lists, [], HandlerFun);
		_Other -> {nok, error}
	end.

snap_to_roads(_Endpoint, _ApiKey, [], Processed, _HandlerFun) -> {ok, lists:flatten(lists:reverse(Processed))};
snap_to_roads(Endpoint, ApiKey, [Points|Rest], Processed, HandlerFun) ->
	EncodedPoints = encode_points(Points),
	UrlParts = [Endpoint, "?interpolate=true&key=", ApiKey, "&path=", EncodedPoints],
	RequestUrl = lists:flatten(UrlParts),
	%% timeout in ms
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} ->
			case HandlerFun(list_to_binary(ResponseBody)) of
				NewPositions when is_list(NewPositions) -> snap_to_roads(Endpoint, ApiKey, Rest, [NewPositions|Processed], HandlerFun);
				Other -> Other
			end;
		Other -> {nok, Other}
	end.

geocode(UrlParts) ->
	HandlerFun = jsx:decoder(handler_gapi_geocode, [], jsx_config:extract_config([])),
	RequestUrl = lists:flatten(UrlParts),
	%% timeout in ms
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} ->
			{ok, HandlerFun(list_to_binary(ResponseBody))};
		Other -> {nok, Other}
	end.


encode_address(Address) when is_binary(Address) ->
	http_uri:encode(binary_to_list(Address));
encode_address(Address) when is_list(Address) ->
	http_uri:encode(Address).

encode_point({Latitude, Longitude}) ->
	% Format: <latitude>,<longitude> 
	L1 = float_to_list(Longitude, [{decimals, 8}]),
	L2 = [$, | L1],
	http_uri:encode(lists:append(float_to_list(Latitude, [{decimals, 8}]), L2)).

encode_location(Location) when is_binary(Location) orelse is_list(Location) ->
	encode_address(Location);
encode_location(Other) ->
	encode_point(Other).

encode_points(Points) when is_list(Points) ->
	EncodedPoints = [encode_point(Point) || Point <- Points],
	string:join(EncodedPoints, ?URL_ENCODED_PIPE). %TODO ver alternativa para isto

encode_locations(Locations) when is_list(Locations) ->
	EncodedLocations = [encode_location(Location) || Location <- Locations],
	string:join(EncodedLocations, ?URL_ENCODED_PIPE). %TODO ver alternativa para isto

encode_waypoints(undefined, _Optimize) -> [];
encode_waypoints([], _Optimize) -> [];
encode_waypoints(Waypoints, Optimize) ->
	["&waypoints=optimize", ?URL_ENCODED_COLON, atom_to_list(Optimize), ?URL_ENCODED_PIPE, encode_locations(Waypoints)].

encode_components(undefined) -> [];
encode_components(Components) when is_list(Components) ->
	encode_components(Components, []).

encode_components([], []) -> [];
encode_components([], [_|Components]) -> % The first element is |
	["&components=", Components];
encode_components([{Key, Value}|Rest], Acc) ->
	Component = [encode_value(Key), ?URL_ENCODED_COLON, encode_value(Value)],
	encode_components(Rest, [?URL_ENCODED_PIPE, Component|Acc]).

% Calculate the total distance of a list of coordinates
calculate_distance([]) -> 0;
calculate_distance([First|Rest]) -> calculate_distance(Rest, First, 0).

calculate_distance([], _, Distance) -> Distance;
calculate_distance([EndLocation|Rest], StartLocation, Distance) ->
	NewDistance = noesis_geometry:distance(StartLocation, EndLocation),
	calculate_distance(Rest, EndLocation, Distance + NewDistance).

encode_config(undefined) -> [];
encode_config(Optional) when is_list(Optional) ->
	encode_config(Optional, []).

encode_config([], Acc) -> Acc;
encode_config([{Key, Value}|Rest], Acc) ->
	Optional = [$&, atom_to_list(Key), $=, encode_config_value(Key, Value)],
	encode_config(Rest, [Optional|Acc]).


encode_config_value(Key, Values) when (Key =:= result_type orelse Key =:= location_type) andalso is_list(Values) ->
	EncodedValues = [encode_value(Value) || Value <- Values],
	string:join(EncodedValues, ?URL_ENCODED_PIPE);
encode_config_value(_Key, {NE, SW}) ->
	EncodedNE = [$, | encode_point(NE)],
	EncodedSW = encode_point(SW),
	http_uri:encode(lists:append(EncodedSW, EncodedNE));
encode_config_value(_Key, Value) -> encode_value(Value).

encode_value(Value) when is_integer(Value) -> integer_to_list(Value);
encode_value(Value) when is_atom(Value) -> atom_to_list(Value);
encode_value(Value) when is_binary(Value) -> http_uri:encode(binary_to_list(Value));
encode_value(Value) when is_list(Value) -> http_uri:encode(Value).
