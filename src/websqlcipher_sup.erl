-module(websqlcipher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Flags = #{strategy => one_for_one, intensity => 1, period => 5},
	IndexServer = #{id => websqlcipher_index_server,
	                start => {websqlcipher_index_server, start_link, []},
	                restart => permanent,
	                type => worker,
	                module => [websqlcipher_index_server]},
	{ok, {Flags, [IndexServer]} }.
