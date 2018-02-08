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

%% ====================================================================
%% Constants
%% ====================================================================
-define(GAPI_ENDPOINT_SNAP_TO_POINTS, "https://roads.googleapis.com/v1/snapToRoads").
-define(GAPI_ENDPOINT_MATRIX,         "https://maps.googleapis.com/maps/api/distancematrix/").
-define(GAPI_ENDPOINT_DIRECTIONS,     "https://maps.googleapis.com/maps/api/directions/").
-define(GAPI_ENDPOINT_GEOCODE,        "https://maps.googleapis.com/maps/api/geocode/").

-define(GAPI_SNAP_TO_ROAD_MAX_POSITIONS, 100).

%% ====================================================================
%% Records
%% ====================================================================
-record(gapi_snapped_point, {location, original_index, place_id}).
-record(gapi_route, {distance, polyline}).

-record(gapi_fare, {currency, text, value}).
-record(gapi_info, {text, value}).

-record(gapi_matrix_row_element, {status, duration, duration_in_traffic, distance, fare}).
-record(gapi_matrix, {status, error_message, origin_addresses, destination_addresses, rows}).

-record(gapi_directions_transit_detail_line_vehicle, {name, type, icon, local_icon}).
-record(gapi_directions_transit_detail_line_agency, {name, url, phone}).
-record(gapi_directions_transit_detail_line, {name, short_name, color, agencies, url, icon, text_color, vehicle}).
-record(gapi_directions_transit_detail_time, {text, value, timezone}).
-record(gapi_directions_transit_detail_stop, {name, location}).
-record(gapi_directions_transit_detail, {arrival_stop, departure_stop, arrival_time, departure_time, headsign, headway, num_stops, line}).
-record(gapi_directions_leg_step, {html_instructions, distance, duration, start_location, end_location, maneuver, polyline, travel_mode, steps, transit_details}).
-record(gapi_directions_leg, {steps, distance, duration, duration_in_traffic, arrival_time, departure_time, start_location, end_location, start_address, end_address}).
-record(gapi_directions_route, {summary, legs, waypoint_order, overview_polyline, bounds, copyrights, warnings, fare}).
-record(gapi_directions_geocoded_waypoint, {geocoder_status, partial_match, place_id, types}).
-record(gapi_directions, {status, error_message, geocoded_waypoints, routes, available_travel_modes}).

-record(gapi_geocode_geometry, {location, location_type, viewport, bounds}).
-record(gapi_geocode_address_component, {types, long_name, short_name}).
-record(gapi_geocode_result, {types, formatted_address, address_components, postcode_localities, geometry, partial_match, place_id}).
-record(gapi_geocode, {status, error_message, results}).
