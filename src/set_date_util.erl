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
-module(set_date_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_current_localtime/0, localtime_to_iso8601/1, localtime_to_utc/1, parse_iso8601_date/1, date_to_iso8601/1, difference/2]).
-export([iso8601_to_localtime/1]).

get_current_localtime() ->
	Timestamp = {_, _, Micro} = erlang:timestamp(),
	{Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Timestamp),
	{Date, {Hours, Minutes, (Seconds + (Micro / 1000000))}}.

localtime_to_iso8601(Localtime) ->
	UTC = convert(Localtime, fun localtime_to_utc/1),
	iso8601:format(UTC).

iso8601_to_localtime(Iso8601) ->
	try iso8601:parse_exact(Iso8601) of
		UTC -> {ok, convert(UTC, fun calendar:universal_time_to_local_time/1)}
	catch
		error:badarg -> nok
	end.

localtime_to_utc(Localtime) ->
	case calendar:local_time_to_universal_time_dst(Localtime) of
		[] -> Localtime;
		[UTCDST, _UTC] -> UTCDST;
		[UTC] -> UTC
	end.

parse_iso8601_date(<<BinYear:4/binary, BinMonth:2/binary, BinDay:2/binary>>) ->
	try
		IntYear = erlang:binary_to_integer(BinYear),
		IntMonth = erlang:binary_to_integer(BinMonth),
		IntDay = erlang:binary_to_integer(BinDay),
		{ok, {IntYear, IntMonth, IntDay}}
	catch
		error:badarg -> nok
	end;
parse_iso8601_date(<<BinYear:4/binary, Sep:1/binary, BinMonth:2/binary, Sep:1/binary, BinDay:2/binary>>) when Sep =:= <<"-">> ->
	parse_iso8601_date(<<BinYear/binary, BinMonth/binary, BinDay/binary>>);
parse_iso8601_date(_) -> nok.

date_to_iso8601({Year, Month, Day}) ->
	BinYear = integer_to_binary(Year),
	BinMonth = integer_to_binary(Month),
	BinDay = integer_to_binary(Day),
	PaddedBinMonth =
		if
			Month >= 10 -> BinMonth;
			Month < 10 -> << <<"0">>/binary, BinMonth/binary >>
		end,
	PaddedBinDay =
		if
			Day >= 10 -> BinDay;
			Day < 10 -> << <<"0">>/binary, BinDay/binary >>
		end,
	<< BinYear/binary, <<"-">>/binary, PaddedBinMonth/binary, <<"-">>/binary, PaddedBinDay/binary >>.

difference({{Y1, M1, D1}, {H1, Min1, S1}}, {{Y2, M2, D2}, {H2, Min2, S2}}) ->
	Date1 = {{Y1, M1, D1}, {H1, Min1, round(S1)}},
	Date2 = {{Y2, M2, D2}, {H2, Min2, round(S2)}},
	calendar:datetime_to_gregorian_seconds(Date1) - calendar:datetime_to_gregorian_seconds(Date2).

%% ====================================================================
%% Internal functions
%% ====================================================================
convert({PDate, {PHours, PMinutes, PSeconds}}, ConvertFun) ->
	TruncatedSeconds = trunc(PSeconds),
	MiliSeconds = PSeconds - TruncatedSeconds,
	{RDate, {RHours, RMinutes, RSeconds}} = ConvertFun({PDate, {PHours, PMinutes, TruncatedSeconds}}),
	{RDate, {RHours, RMinutes, RSeconds + MiliSeconds}}.