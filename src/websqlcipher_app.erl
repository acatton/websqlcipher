-module(websqlcipher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		% All hosts
		{'_', [
			{"/", websqlcipher_list_handler, []},
			{"/:dbname", websqlcipher_db_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(
		websqlcipher_http_listener,
		100,
		[{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]
	),
	websqlcipher_sup:start_link().

stop(_State) ->
	ok.
