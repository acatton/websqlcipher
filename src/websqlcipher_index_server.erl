-module(websqlcipher_index_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([get_database/1, get_database_list/0]).
-export([init/1, handle_call/3, handle_info/2, code_change/3, handle_cast/2, terminate/2]).

-record(state, {database_workers}).

start_link() ->
	gen_server:start_link(
		{local, ?MODULE},
		?MODULE,
		[],
		[]
	).

init(_Args) ->
	DatabaseWorkers = dict:new(),
	State = #state{database_workers=DatabaseWorkers},
	{ok, State}.

get_database(Name) ->
	gen_server:call(?MODULE, {get_database, Name}).

get_database_list() ->
	gen_server:call(?MODULE, get_database_list).

handle_call(get_database_list, _From, State) ->
	{reply, {ok, []}, State};
handle_call({get_database, Name}, _From, State) ->
	DatabaseWorkers = State#state.database_workers,
	case get_or_create_database_worker(Name, DatabaseWorkers) of
		{ok, {Worker, DatabaseWorkers2}} ->
			State2 = State#state{database_workers=DatabaseWorkers2},
			{reply, {ok, Worker}, State2};
		Error = {error, _} ->
			{reply, Error, State}
	end.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
	State2 = remove_worker(Pid, State),
	{noreply, State2}.

remove_worker(WorkerToRemove, #state{database_workers=DatabaseWorkers} = State) ->
	DatabaseWorkers2 = dict:filter(
		fun (_, Worker) -> Worker =/= WorkerToRemove end,
		DatabaseWorkers
	),
	State#state{database_workers=DatabaseWorkers2}.

code_change(_OldVsn, State, _Extra) ->
	{noreply, State}.

handle_cast(_Msg, _State) ->
	{error, undef}. % This should never happen.

terminate(_Reason, State) ->
	DatabaseWorkersDict = State#state.database_workers,
	DatabaseWorkersKeyValueList = dict:to_list(DatabaseWorkersDict),
	DatabaseWorkerList = lists:map(
		fun ({_Key, Value}) -> Value end,
		DatabaseWorkersKeyValueList
	),
	lists:foreach(
		fun (Worker) -> stop_worker(Worker) end,
		DatabaseWorkerList
	),
	ok.

stop_worker(WorkerPid) ->
	exit(WorkerPid, normal).

create_database_worker(Name, WorkerDict) ->
	case websqlcipher_database_worker:start(Name) of
		{ok, Worker} ->
			monitor(process, Worker),
			WorkerDict2 = dict:store(Name, Worker, WorkerDict),
			{ok, WorkerDict2};
		Error = {error, _} ->
			Error
	end.

get_or_create_database_worker(Name, DatabaseWorkers) ->
	case dict:find(Name, DatabaseWorkers) of
		{ok, Worker} ->
			{ok, {Worker, DatabaseWorkers}};
		error ->
			case create_database_worker(Name, DatabaseWorkers) of
				{ok, DatabaseWorkers2} ->
					{ok, Worker} = dict:find(Name, DatabaseWorkers2),
					{ok, {Worker, DatabaseWorkers2}};
				Error = {error, _} ->
					Error
			end
	end.
