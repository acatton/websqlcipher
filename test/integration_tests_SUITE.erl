-module(integration_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, init_per_testcase/2]).
-export([test_get_tables_empty/1, test_create_table/1]).

all() ->
	[test_get_tables_empty, test_create_table].

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
	{<<"tables">>, {[]}} = proplists:lookup(<<"tables">>, MainDict),
	ok.

test_create_table(Config) ->
	Url = ?config(url, Config),
	Headers = [{"Accept", "*/*"}],
	Sql = <<"CREATE TABLE foo( id integer )">>,
	PostContent = jiffy:encode({[
		{<<"query">>, Sql},
		{<<"parameters">>, []}
	]}),

	{ok, Content} = httpc:request(post, {Url, Headers, "application/json", PostContent}, [], []),
	{StatusLine, _Headers, _Body} = Content,
	{_Version, 200, _Msg} = StatusLine,

	{ok, Content2} = httpc:request(get, {Url, Headers}, [], []),
	{StatusLine2, _Headers2, Body} = Content2,
	{_Version2, 200, _Msg2} = StatusLine2,
	Json = jiffy:decode(Body),
	{MainDict} = Json,
	{<<"tables">>, {Tables}} = proplists:lookup(<<"tables">>, MainDict),
	{<<"foo">>, Sql} = proplists:lookup(<<"foo">>, Tables),

	ok.
