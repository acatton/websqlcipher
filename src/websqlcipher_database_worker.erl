-module(websqlcipher_database_worker).
-behaviour(gen_server).

-export([start/1]).
-export([execute/3, list_tables/1]).
-export([init/1, handle_call/3]).

-record(state, {name}).

start(Name) ->
	gen_server:start(?MODULE, [Name], []).

init([Name]) ->
	State = #state{name=Name},
	case re:run(Name, <<"^[a-zA-Z0-9]{1,100}$">>, [{capture, none}]) of
		match ->
			{ok, State};
		nomatch ->
			{stop, invalid_database_name}
	end.

execute(Worker, Query, Parameters) ->
	gen_server:call(Worker, {'query', Query, Parameters}).

list_tables(Worker) ->
	gen_server:call(Worker, list_tables).

handle_call({'query', Query, Parameters}, _From, State) ->
	{reply, {ok, []}, State};
handle_call(list_tables, _From, State) ->
	{reply, {ok, []}, State}.
