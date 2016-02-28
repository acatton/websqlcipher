-module(websqlcipher_db_handler).

-export([handle_get_json/2, handle_post_json/2]).
-export([init/2, allowed_methods/2, content_types_provided/2,
         content_types_accepted/2, malformed_request/2]).

-record(state, {database_name, database_worker=undefined}).
-record(sql_query, {sql, parameters}).

-include_lib("stdlib/include/assert.hrl").

init(Req, _Opts) ->
	DBName = cowboy_req:binding(dbname, Req),
	?assertNotEqual(DBName, <<"">>),
	{cowboy_rest, Req, #state{database_name=DBName}}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, '*'}, handle_get_json}
	 ], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, handle_post_json}
	 ], Req, State}.

malformed_request(Req, State) ->
	case websqlcipher_index_server:get_database(State#state.database_name) of
		{ok, Worker} ->
			State2 = State#state{database_worker=Worker},
			{false, Req, State2};
		{error, invalid_database_name} ->
			{true, Req, State}
	end.

validate_json_query(Query) ->
	case is_binary(Query) of
		true ->
			Query;
		false ->
			throw({error, {bad_json, "The query is supposed to be a string"}})
	end.

validate_json_parameters({ParameterKeyValueList}) ->
	dict:from_list(ParameterKeyValueList);
validate_json_parameters(ParameterList) when is_list(ParameterList) ->
	ParameterList;
validate_json_parameters(_) ->
	throw({error, {bad_json, "Parameters must be a JSON Object or a JSON Array"}}).

% Validate the JSON {"query": "SELECT * FROM table WHERE value = ?"}
validate_json({List}) ->
	Dict = dict:from_list(List),
	Keys = sets:from_list(dict:fetch_keys(Dict)),
	ExpectedKeys = sets:from_list([<<"query">>, <<"parameters">>]),

	MissingKeys = sets:subtract(ExpectedKeys, Keys),
	sets:size(MissingKeys) == 0 orelse throw(
		{error, {bad_json, "Missing keys. (query and parameters required)"}}
	),

	ExtraKeys = sets:subtract(Keys, ExpectedKeys),
	sets:size(ExtraKeys) == 0 orelse throw(
		{error, {bad_json, "Extra keys. (only query and parameters are required)"}}
	),

	Query = validate_json_query(dict:fetch(<<"query">>, Dict)),
	Parameters = validate_json_parameters(dict:fetch(<<"parameters">>, Dict)),

	#sql_query{sql=Query, parameters=Parameters};
validate_json(_) ->
	throw({error, {bad_json, "The root element is expected to be JSON Object"}}).

handle_post_json(Req, State) ->
	{ok, Body, Req2} = cowboy_req:body(Req),
	try jiffy:decode(Body) of
		Json ->
			try validate_json(Json) of
				Query ->
					Req3 = run_query(Req2, State, Query),
					{true, Req3, State}
			catch
				throw:{error, {bad_json, Reason}} ->
					Req3 = rest_error(Req2, Reason),
					{false, Req3, State}
			end
	catch
		throw:{error,{_,invalid_json}} ->
			{false, Req2, State}
	end.

rest_error(Req2, _Error) ->
	Req2. % TODO: Set error as JSON

run_query(Req, State, Query) ->
	Worker = State#state.database_worker,
	?assertNotEqual(Worker, undefined),
	SQL = Query#sql_query.sql,
	Parameters = Query#sql_query.parameters,
	% TODO: Handle error
	{ok, Result} = websqlcipher_database_worker:execute(Worker, SQL, Parameters),
	JSON = {[{<<"result">>, convert_dbresult_to_json(Result)}]},
	Body = jiffy:encode(JSON),
	cowboy_req:set_resp_body(Body, Req).

handle_get_json(Req, State) ->
	Worker = State#state.database_worker,
	?assertNotEqual(Worker, undefined),
	% TODO: Handle error
	{ok, Result} = websqlcipher_database_worker:list_tables(Worker),
	JSON = {[{<<"tables">>, {Result}}]},
	Body = jiffy:encode(JSON),
	{Body, Req, State}.

convert_dbresult_to_json(Result) when is_list(Result) ->
	lists:map(fun convert_dbresult_to_json/1, Result);
convert_dbresult_to_json(Result) when is_tuple(Result) ->
	convert_dbresult_to_json(
	  [element(I, Result) || I <- lists:seq(1, tuple_size(Result))]
	);
convert_dbresult_to_json(undefined) ->
	null;
convert_dbresult_to_json(Result) ->
	Result.
