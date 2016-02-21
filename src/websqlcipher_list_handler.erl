-module(websqlcipher_list_handler).

-export([init/2]).

init(Req, Opts) ->
	{ok, List} = websqlcipher_index_server:get_database_list(),
	Resp = cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json">>}
	], jiffy:encode(List, [pretty]), Req),
	{ok, Resp, Opts}.
