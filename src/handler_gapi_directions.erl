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
-module(handler_gapi_directions).

-include("set_gapi_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_event/2]).



%TODO terminar e organizar

init(Config) ->
	{Config, #gapi_directions{}}.



handle_event({key, <<"status">>}, {Config, Directions}) when is_record(Directions, gapi_directions) ->
	% Status initialization
	{Config, {status, Directions}};

handle_event({string, Value}, {Config, {status, Directions}}) when is_record(Directions, gapi_directions) ->
	% Set the status value
	{Config, Directions#gapi_directions{status=Value}};




handle_event({key, <<"error_message">>}, {Config, Directions}) when is_record(Directions, gapi_directions) ->
	% Error Message initialization
	{Config, {error_message, Directions}};

handle_event({string, Value}, {Config, {error_message, Directions}}) when is_record(Directions, gapi_directions) ->
	% Set the error_message value
	{Config, Directions#gapi_directions{error_message=Value}};




handle_event({key, <<"available_travel_modes">>}, {Config, Directions}) when is_record(Directions, gapi_directions) ->
	% Available Travel Modes initialization
	{Config, {available_travel_modes, [], Directions}};

handle_event({string, Value}, {Config, {available_travel_modes, AvailableTravelModes, Directions}}) ->
	% Add a travel mode
	{Config, {available_travel_modes, [Value|AvailableTravelModes], Directions}};

handle_event(end_array, {Config, {available_travel_modes, AvailableTravelModes, Directions}}) ->
	% Set the available_travel_modes array
	{Config, Directions#gapi_directions{available_travel_modes=AvailableTravelModes}};





handle_event({key, <<"geocoded_waypoints">>}, {Config, Directions}) when is_record(Directions, gapi_directions) ->
	% Available Travel Modes initialization
	{Config, {geocoded_waypoints, [], Directions}};

handle_event(start_object, {Config, {geocoded_waypoints, GeocodedWaypoints, Directions}}) ->
	% New Geocoded Waypoint
	{Config, {geocoded_waypoint, #gapi_directions_geocoded_waypoint{}, GeocodedWaypoints, Directions}};

handle_event({key, <<"geocoder_status">>}, {Config, {geocoded_waypoint, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Geocoder Status initialization
	{Config, {geocoder_status, GeocodedWaypoint, GeocodedWaypoints, Directions}};

handle_event({string, Value}, {Config, {geocoder_status, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Set the geocoder_status value
	{Config, {geocoded_waypoint, GeocodedWaypoint#gapi_directions_geocoded_waypoint{geocoder_status=Value}, GeocodedWaypoints, Directions}};

handle_event({key, <<"place_id">>}, {Config, {geocoded_waypoint, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Place Id initialization
	{Config, {place_id, GeocodedWaypoint, GeocodedWaypoints, Directions}};

handle_event({string, Value}, {Config, {place_id, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Set the place_id value
	{Config, {geocoded_waypoint, GeocodedWaypoint#gapi_directions_geocoded_waypoint{place_id=Value}, GeocodedWaypoints, Directions}};

handle_event({key, <<"partial_match">>}, {Config, {geocoded_waypoint, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Partial Match initialization
	{Config, {partial_match, GeocodedWaypoint, GeocodedWaypoints, Directions}};

handle_event({literal, Value}, {Config, {partial_match, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Set the partial_match value
	{Config, {geocoded_waypoint, GeocodedWaypoint#gapi_directions_geocoded_waypoint{partial_match=Value}, GeocodedWaypoints, Directions}};

handle_event({key, <<"types">>}, {Config, {geocoded_waypoint, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Types initialization
	{Config, {types, [], GeocodedWaypoint, GeocodedWaypoints, Directions}};

handle_event({string, Value}, {Config, {types, Types, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Add a type
	{Config, {types, [Value|Types], GeocodedWaypoint, GeocodedWaypoints, Directions}};

handle_event(end_array, {Config, {types, Types, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Set the types array
	{Config, {geocoded_waypoint, GeocodedWaypoint#gapi_directions_geocoded_waypoint{types=Types}, GeocodedWaypoints, Directions}};

handle_event(end_object, {Config, {geocoded_waypoint, GeocodedWaypoint, GeocodedWaypoints, Directions}}) ->
	% Add element to list
	{Config, {geocoded_waypoints, [GeocodedWaypoint|GeocodedWaypoints], Directions}};

handle_event(end_array, {Config, {geocoded_waypoints, GeocodedWaypoints, Directions}}) ->
	% Set the geocoded_waypoints array
	{Config, Directions#gapi_directions{geocoded_waypoints=GeocodedWaypoints}};

% If handle_event is called with an unknown key, we set skip mode on
handle_event({key, Key}, State={Config, {geocoded_waypoint, _GeocodedWaypoint, _GeocodedWaypoints, _Directions}}) ->
	% Unknown key inside geocoded_waypoint
	skip(Key, State);






%%%%%%%%%%% ROUTES %%%%%%%%%%%%


handle_event({key, <<"routes">>}, {Config, Directions}) when is_record(Directions, gapi_directions) ->
	% Routes initialization
	{Config, {routes, [], Directions}};




handle_event(start_object, {Config, {routes, Routes, Directions}}) ->
	% New Route
	{Config, {route, #gapi_directions_route{}, Routes, Directions}};

handle_event({key, <<"summary">>}, {Config, {route, Route, Routes, Directions}}) ->
	% Summary initialization
	{Config, {summary, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {summary, Route, Routes, Directions}}) ->
	% Set the summary value
	{Config, {route, Route#gapi_directions_route{summary=Value}, Routes, Directions}};




handle_event({key, <<"legs">>}, {Config, {route, Route, Routes, Directions}}) ->
	% Legs initialization
	{Config, {legs, [], Route, Routes, Directions}};

handle_event(start_object, {Config, {legs, Legs, Route, Routes, Directions}}) ->
	% New Leg
	{Config, {leg, #gapi_directions_leg{}, Legs, Route, Routes, Directions}};



%TODO generalizar steps porque tem substeps
handle_event({key, <<"steps">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% Steps initialization
	{Config, {steps, [], Leg, Legs, Route, Routes, Directions}};


handle_event(start_object, {Config, {steps, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% New Step
	{Config, {step, #gapi_directions_leg_step{}, Steps, Leg, Legs, Route, Routes, Directions}};




handle_event({key, <<"html_instructions">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% HTML Instructions initialization
	{Config, {html_instructions, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {html_instructions, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the html_instructions value
	{Config,  {step, Step#gapi_directions_leg_step{html_instructions=Value}, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"distance">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Distance initialization
	{Config, {distance, #gapi_info{}, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"duration">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Duration initialization
	{Config, {duration, #gapi_info{}, Step, Steps, Leg, Legs, Route, Routes, Directions}};


handle_event({key, <<"text">>}, {Config, {InfoField, Info, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Text initialization
	{Config, {text, InfoField, Info, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"value">>}, {Config, {InfoField, Info, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Value initialization
	{Config, {value, InfoField, Info, Step, Steps, Leg, Legs, Route, Routes, Directions}};


handle_event({string, Value}, {Config, {text, InfoField, Info, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the text
	{Config, {InfoField, Info#gapi_info{text=Value}, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({integer, Value}, {Config, {value, InfoField, Info, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the value
	{Config, {InfoField, Info#gapi_info{value=Value}, Step, Steps, Leg, Legs, Route, Routes, Directions}};


handle_event(end_object, {Config, {distance, Info, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the distance
 	{Config, {step, Step#gapi_directions_leg_step{distance=Info}, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event(end_object, {Config, {duration, Info, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the duration
 	{Config, {step, Step#gapi_directions_leg_step{duration=Info}, Steps, Leg, Legs, Route, Routes, Directions}};



%TODO REAPROVEITAR para o bounds
handle_event({key, <<"start_location">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Start Location initialization
	{Config, {start_location, {undefined, undefined}, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"end_location">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% End Location initialization
	{Config, {end_location, {undefined, undefined}, Step, Steps, Leg, Legs, Route, Routes, Directions}};


%TODO REAPROVEITAR para o bounds
handle_event({key, <<"lat">>}, {Config, {LocationField, Location, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Latitude initialization
	{Config, {latitude, LocationField, Location, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"lng">>}, {Config, {LocationField, Location, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Latitude initialization
	{Config, {longitude, LocationField, Location, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({float, Value}, {Config, {latitude, LocationField, {_, Longitude}, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the latitude
	{Config, {LocationField, {Value, Longitude}, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({float, Value}, {Config, {longitude, LocationField, {Latitude, _}, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the longitude
	{Config, {LocationField, {Latitude, Value}, Step, Steps, Leg, Legs, Route, Routes, Directions}};


handle_event(end_object, {Config, {start_location, Location, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the location
 	{Config, {step, Step#gapi_directions_leg_step{start_location=Location}, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event(end_object, {Config, {end_location, Location, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the location
 	{Config, {step, Step#gapi_directions_leg_step{end_location=Location}, Steps, Leg, Legs, Route, Routes, Directions}};



handle_event({key, <<"maneuver">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% HTML Instructions initialization
	{Config, {maneuver, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {maneuver, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the html_instructions value
	{Config,  {step, Step#gapi_directions_leg_step{maneuver=Value}, Steps, Leg, Legs, Route, Routes, Directions}};



%TODO REAPROVEITAR PARA O OVERVIEW_POLYLINE
handle_event({key, <<"polyline">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Polyline initialization
	{Config, {polyline, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {polyline, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the polyline
	{Config, {polyline, Step#gapi_directions_leg_step{polyline=Value}, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event(end_object, {Config, {polyline, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Finish the polyline object
	{Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"travel_mode">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% HTML Instructions initialization
	{Config, {travel_mode, Step, Steps, Leg, Legs, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {travel_mode, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the html_instructions value
	{Config,  {step, Step#gapi_directions_leg_step{travel_mode=Value}, Steps, Leg, Legs, Route, Routes, Directions}};


%% %TODO generalizar steps - estes sao os substeps
%% handle_event({key, <<"steps">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
%% 	{Config, {todo_generalizar_steps, Step, Steps, Leg, Legs, Route, Routes, Directions}};
%% 
%% handle_event(end_object, {Config, {todo_generalizar_steps, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
%% 	{Config, {Step, Steps, Leg, Legs, Route, Routes, Directions}};
%% 
%% %TODO terminar transit_details
%% handle_event({key, <<"transit_details">>}, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
%% 	{Config, {todo_transit_details, Step, Steps, Leg, Legs, Route, Routes, Directions}};
%% 
%% handle_event(end_object, {Config, {todo_transit_details, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
%% 	{Config, {Step, Steps, Leg, Legs, Route, Routes, Directions}};

% If handle_event is called with an unknown key, we set skip mode on
handle_event({key, Key}, State={Config, {step, _Step, _Steps, _Leg, _Legs, _Route, _Routes, _Directions}}) ->
	% Unknown key inside step
	skip(Key, State);




handle_event(end_object, {Config, {step, Step, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Add element to list
	{Config, {steps, [Step|Steps], Leg, Legs, Route, Routes, Directions}};

handle_event(end_array, {Config, {steps, Steps, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the steps array
	{Config, {leg, Leg#gapi_directions_leg{steps=lists:reverse(Steps)}, Legs, Route, Routes, Directions}};






%TODO REAPROVEITAR o anterior, visto que basicamente só muda o State
handle_event({key, <<"distance">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% Distance initialization
	{Config, {distance, #gapi_info{}, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"duration">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% Duration initialization
	{Config, {duration, #gapi_info{}, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"duration_in_traffic">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% Duration In Traffic initialization
	{Config, {duration_in_traffic, #gapi_info{}, Leg, Legs, Route, Routes, Directions}};


handle_event({key, <<"text">>}, {Config, {InfoField, Info, Leg, Legs, Route, Routes, Directions}}) ->
	% Text initialization
	{Config, {text, InfoField, Info, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"value">>}, {Config, {InfoField, Info, Leg, Legs, Route, Routes, Directions}}) ->
	% Value initialization
	{Config, {value, InfoField, Info, Leg, Legs, Route, Routes, Directions}};


handle_event({string, Value}, {Config, {text, InfoField, Info, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the text
	{Config, {InfoField, Info#gapi_info{text=Value}, Leg, Legs, Route, Routes, Directions}};

handle_event({integer, Value}, {Config, {value, InfoField, Info, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the value
	{Config, {InfoField, Info#gapi_info{value=Value}, Leg, Legs, Route, Routes, Directions}};


handle_event(end_object, {Config, {distance, Info, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the distance
 	{Config, {leg, Leg#gapi_directions_leg{distance=Info}, Legs, Route, Routes, Directions}};

handle_event(end_object, {Config, {duration, Info, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the distance
 	{Config, {leg, Leg#gapi_directions_leg{duration=Info}, Legs, Route, Routes, Directions}};

handle_event(end_object, {Config, {duration_in_traffic, Info, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the distance
 	{Config, {leg, Leg#gapi_directions_leg{duration_in_traffic=Info}, Legs, Route, Routes, Directions}};

%% 
%% handle_event({key, <<"arrival_time">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
%% 	% Steps initialization
%% 	{Config, {todo_arrival_time, Leg, Legs, Route, Routes, Directions}};
%% handle_event(end_object, {Config, {todo_arrival_time, Leg, Legs, Route, Routes, Directions}}) ->
%% 	% Add element to list
%% 	{Config, {leg, Leg, Legs, Route, Routes, Directions}};
%% 
%% handle_event({key, <<"departure_time">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
%% 	% Steps initialization
%% 	{Config, {todo_departure_time, Leg, Legs, Route, Routes, Directions}};
%% handle_event(end_object, {Config, {todo_departure_time, Leg, Legs, Route, Routes, Directions}}) ->
%% 	% Add element to list
%% 	{Config, {leg, Leg, Legs, Route, Routes, Directions}};
%% 





%TODO REAPROVEITAR para o bounds
handle_event({key, <<"start_location">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% Start Location initialization
	{Config, {start_location, {undefined, undefined}, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"end_location">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% End Location initialization
	{Config, {end_location, {undefined, undefined}, Leg, Legs, Route, Routes, Directions}};


%TODO REAPROVEITAR para o bounds
handle_event({key, <<"lat">>}, {Config, {LocationField, Location, Leg, Legs, Route, Routes, Directions}}) ->
	% Latitude initialization
	{Config, {latitude, LocationField, Location, Leg, Legs, Route, Routes, Directions}};

handle_event({key, <<"lng">>}, {Config, {LocationField, Location, Leg, Legs, Route, Routes, Directions}}) ->
	% Latitude initialization
	{Config, {longitude, LocationField, Location, Leg, Legs, Route, Routes, Directions}};

handle_event({float, Value}, {Config, {latitude, LocationField, {_, Longitude}, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the latitude
	{Config, {LocationField, {Value, Longitude}, Leg, Legs, Route, Routes, Directions}};

handle_event({float, Value}, {Config, {longitude, LocationField, {Latitude, _}, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the longitude
	{Config, {LocationField, {Latitude, Value}, Leg, Legs, Route, Routes, Directions}};


handle_event(end_object, {Config, {start_location, Location, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the location
	{Config, {leg, Leg#gapi_directions_leg{start_location=Location}, Legs, Route, Routes, Directions}};
handle_event(end_object, {Config, {end_location, Location, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the location
 	{Config, {leg, Leg#gapi_directions_leg{end_location=Location}, Legs, Route, Routes, Directions}};

%TODO FIM REAPROVEITAR BOUNDS



handle_event({key, <<"start_address">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% Start Address initialization
	{Config, {start_address, Leg, Legs, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {start_address, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the start_address value
	{Config, {leg, Leg#gapi_directions_leg{start_address=Value}, Legs, Route, Routes, Directions}};


handle_event({key, <<"end_address">>}, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% End Address initialization
	{Config, {end_address, Leg, Legs, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {end_address, Leg, Legs, Route, Routes, Directions}}) ->
	% Set the end_address value
	{Config, {leg, Leg#gapi_directions_leg{end_address=Value}, Legs, Route, Routes, Directions}};













handle_event(end_object, {Config, {leg, Leg, Legs, Route, Routes, Directions}}) ->
	% Add element to list
	{Config, {legs, [Leg|Legs], Route, Routes, Directions}};

handle_event(end_array, {Config, {legs, Legs, Route, Routes, Directions}}) ->
	% Set the routes array
	{Config, {route, Route#gapi_directions_route{legs=lists:reverse(Legs)}, Routes, Directions}};


% If handle_event is called with an unknown key, we set skip mode on
handle_event({key, Key}, State={Config, {leg, _Leg, _Legs, _Route, _Routes, _Directions}}) ->
	% Unknown key inside leg
	skip(Key, State);



handle_event({key, <<"waypoint_order">>}, {Config, {route, Route, Routes, Directions}}) ->
	% Waypoint Order initialization
	{Config, {waypoint_order, [], Route, Routes, Directions}};

handle_event({integer, Value}, {Config, {waypoint_order, WaypointOrder, Route, Routes, Directions}}) ->
	% Add a waypoint order
	{Config, {waypoint_order, [Value|WaypointOrder], Route, Routes, Directions}};

handle_event(end_array, {Config, {waypoint_order, WaypointOrder, Route, Routes, Directions}}) ->
	% Set the waypoint_order array
	{Config, {route, Route#gapi_directions_route{waypoint_order=WaypointOrder}, Routes, Directions}};











%TODO REAPROVEITAR PARA O OVERVIEW_POLYLINE
handle_event({key, <<"overview_polyline">>}, {Config, {route, Route, Routes, Directions}}) ->
	% Polyline initialization
	{Config, {overview_polyline, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {overview_polyline, Route, Routes, Directions}}) ->
	% Set the polyline
	{Config, {overview_polyline, Route#gapi_directions_route{overview_polyline=Value}, Routes, Directions}};

handle_event(end_object, {Config, {overview_polyline, Route, Routes, Directions}}) ->
	% Finish the polyline object
	{Config, {route, Route, Routes, Directions}};











handle_event({key, <<"bounds">>}, {Config, {route, Route, Routes, Directions}}) ->
	% Bounds initialization - type gapi_bounds
	{Config, {bounds, {undefined, undefined}, Route, Routes, Directions}};



handle_event(end_object, {Config, {bounds, Bounds, Route, Routes, Directions}}) ->
	% Set bounds value
	{Config, {route, Route#gapi_directions_route{bounds=Bounds}, Routes, Directions}};


%TODO REAPROVEITAR para o bounds
handle_event({key, <<"southwest">>}, {Config, {bounds, Bounds, Route, Routes, Directions}}) ->
	% Southwest initialization
	{Config, {southwest, {undefined, undefined}, Bounds, Route, Routes, Directions}};

handle_event({key, <<"northeast">>}, {Config, {bounds, Bounds, Route, Routes, Directions}}) ->
	% Northeast initialization
	{Config, {northeast, {undefined, undefined}, Bounds, Route, Routes, Directions}};


%TODO REAPROVEITAR para o bounds
handle_event({key, <<"lat">>}, {Config, {LocationField, Location, Bounds, Route, Routes, Directions}}) ->
	% Latitude initialization
	{Config, {latitude, LocationField, Location, Bounds, Route, Routes, Directions}};

handle_event({key, <<"lng">>}, {Config, {LocationField, Location, Bounds, Route, Routes, Directions}}) ->
	% Latitude initialization
	{Config, {longitude, LocationField, Location, Bounds, Route, Routes, Directions}};

handle_event({float, Value}, {Config, {latitude, LocationField, {_, Longitude}, Bounds, Route, Routes, Directions}}) ->
	% Set the latitude
	{Config, {LocationField, {Value, Longitude}, Bounds, Route, Routes, Directions}};

handle_event({float, Value}, {Config, {longitude, LocationField, {Latitude, _}, Bounds, Route, Routes, Directions}}) ->
	% Set the longitude
	{Config, {LocationField, {Latitude, Value}, Bounds, Route, Routes, Directions}};


handle_event(end_object, {Config, {southwest, Location, {NE, _SW}, Route, Routes, Directions}}) ->
	% Set the location - type gapi_bounds
	{Config, {bounds, {NE, Location}, Route, Routes, Directions}};

handle_event(end_object, {Config, {northeast, Location, {_NE, SW}, Route, Routes, Directions}}) ->
	% Set the location - type gapi_bounds
	{Config, {bounds, {Location, SW}, Route, Routes, Directions}};




%TODO FIM TERMINAR


handle_event({key, <<"copyrights">>}, {Config, {route, Route, Routes, Directions}}) ->
	% Copyrights initialization
	{Config, {copyrights, Route, Routes, Directions}};

handle_event({string, Value}, {Config, {copyrights, Route, Routes, Directions}}) ->
	% Set the copyrights value
	{Config, {route, Route#gapi_directions_route{copyrights=Value}, Routes, Directions}};



handle_event({key, <<"warnings">>}, {Config, {route, Route, Routes, Directions}}) ->
	% Warnings initialization
	{Config, {warnings, [], Route, Routes, Directions}};

handle_event({string, Value}, {Config, {warnings, Warnings, Route, Routes, Directions}}) ->
	% Add a warning
	{Config, {warnings, [Value|Warnings], Route, Routes, Directions}};

handle_event(end_array, {Config, {warnings, Warnings, Route, Routes, Directions}}) ->
	% Set the available_travel_modes array
	{Config, {route, Route#gapi_directions_route{warnings=Warnings}, Routes, Directions}};




%% 
%% handle_event({key, <<"fare">>}, {Config, {route, Route, Routes, Directions}}) ->
%% 	% Copyrights initialization
%% 	{Config, {todo_fare, Route, Routes, Directions}};
%% handle_event(end_object, {Config, {todo_fare, Route, Routes, Directions}}) ->
%% 	% Add element to list
%% 	{Config, {route, Route, Routes, Directions}};









% If handle_event is called with an unknown key, we set skip mode on
handle_event({key, Key}, State={Config, {route, _Route, _Routes, _Directions}}) ->
	% Unknown key inside route
	skip(Key, State);




handle_event(end_object, {Config, {route, Route, Routes, Directions}}) ->
	% Add element to list
	{Config, {routes, [Route|Routes], Directions}};

handle_event(end_array, {Config, {routes, Routes, Directions}}) ->
	% Set the routes array
	{Config, Directions#gapi_directions{routes=lists:reverse(Routes)}};


%%%%%%%%%% ROUTES %%%%%%%%%%%%







% If handle_event is called with an unknown key, we set skip mode on
handle_event({key, Key}, State={_Config, Directions}) when is_record(Directions, gapi_directions) ->
	skip(Key, State);






% Skip mode is based on skip level and end operations decrease until it reaches 0. On 0 we return the OldState
handle_event(start_object, {skip, Skip, OldState}) -> {skip, Skip + 1, OldState};
handle_event(start_array, {skip, Skip, OldState}) -> {skip, Skip + 1, OldState};

handle_event(end_array, {skip, 1, OldState}) -> OldState;
handle_event(end_array, {skip, Skip, OldState}) -> {skip, Skip - 1, OldState};
handle_event(end_object, {skip, 1, OldState}) -> OldState;
handle_event(end_object, {skip, Skip, OldState}) -> {skip, Skip - 1, OldState};

handle_event({_, _}, {skip, 0, OldState}) -> OldState;

handle_event(_, {skip, Skip, OldState}) -> {skip, Skip, OldState};


handle_event(end_json, {_Config, Directions}) ->
	% Return the record
	Directions;

% Safe
handle_event(_Event, State) ->
	State.

%% ====================================================================
%% API functions
%% ====================================================================
skip(_Key, State) ->
	{skip, 0, State}.
