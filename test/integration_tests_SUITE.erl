-module(integration_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, init_per_testcase/2]).
-export([test_get_tables_empty/1]).

all() ->
	[test_get_tables_empty].

init_per_suite(Config) ->
	{ok, _} = application:ensure_all_started(websqlcipher),
	ok = application:ensure_started(inets),
	{ok, _} = inets:start(httpc, [{profile, websqlcipher_test_client}]),
	Config.

init_per_testcase(TestCase, Config) ->
	Url = [<<"http://127.0.0.1:8080/">>, atom_to_binary(TestCase, utf8)],
	[{url, binary_to_list(iolist_to_binary(Url))} | Config].

test_get_tables_empty(Config) ->
	Url = ?config(url, Config),
	Headers = [{"Accept", "*/*"}],
	{ok, Content} = httpc:request(get, {Url, Headers}, [], []),
	{StatusLine, _Headers, Body} = Content,
	{_Version, 200, _Msg} = StatusLine,
	Json = jiffy:decode(Body),
	{MainDict} = Json,
	{<<"tables">>, []} = proplists:lookup(<<"tables">>, MainDict),
	ok.
