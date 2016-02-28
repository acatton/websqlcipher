-module(websqlcipher_database_worker).
-behaviour(gen_server).

-export([start/1]).
-export([execute/3, list_tables/1]).
-export([init/1, handle_call/3]).

-record(state, {name, connection=undef}).

start(Name) ->
	gen_server:start(?MODULE, [Name], []).

init([Name]) ->
	State = #state{name=Name},
	case re:run(Name, <<"^[a-zA-Z0-9_]{1,100}$">>, [{capture, none}]) of
		match ->
			{ok, State};
		nomatch ->
			{stop, invalid_database_name}
	end.

execute(Worker, Query, Parameters) ->
	gen_server:call(Worker, {'query', Query, Parameters}).

list_tables(Worker) ->
	gen_server:call(Worker, list_tables).

% TODO: This copy/paste a lot of code, this needs to be factored
handle_call({'query', Query, Parameters}, _From, State) ->
	State2 = idempotent_connect(State),
	Connection = State2#state.connection,
	Result = get_query_results(Connection, Query, Parameters),
	{reply, {ok, Result}, State2};
handle_call(list_tables, _From, State) ->
	State2 = idempotent_connect(State),
	Connection = State2#state.connection,
	Result = get_table_list(Connection),
	{reply, {ok, Result}, State2}.

idempotent_connect(#state{connection=undef, name=Name} = State) ->
	{ok, Connection} = esqlite3:open(":memory:"),
	State#state{connection=Connection};
idempotent_connect(State) ->
	State.

get_table_list(Connection) ->
	Query = "SELECT name, sql "
	        "FROM   sqlite_master "
	        "WHERE  type = 'table';",
	get_query_results(Connection, Query, []).

get_query_results(Connection, Query, Parameters) ->
	esqlite3:q(Query, Parameters, Connection).
