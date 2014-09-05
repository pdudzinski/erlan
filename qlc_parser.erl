-module(qlc).
-export([parse_query/1]).

parse_query(Q) ->
	F = fun() ->
		qlc:e(Q)
	end,
	{atomic, Value} = mnesia:transaction(F), Value.

convert_to_json(Lines) ->
	Data = [{obj, 
		[
			{airport, Line#airport.code},
			{city, Line#airport.city},
			{country, Line#airport.country},
			{name, Line#airport.name}
		]} || Line <- Lines],
	JsonData = {obj, [{data, Data}]}
	rfc4267:encode(JsonData).
