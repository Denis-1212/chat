-module(client_store).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([list/0, add/3, remove/1, list_except/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{}}.

handle_call({list_except, ExcludePid}, _From, State) ->
    Filtered = maps:filter(fun(_Id, {_Username, Pid}) -> Pid =/= ExcludePid end, State),
    {reply, maps:to_list(Filtered), State};
handle_call(list, _From, State) ->
    {reply, maps:to_list(State), State}.

handle_cast({add, Id, Username, Pid}, State) ->
    NewState = State#{Id => {Username, Pid}},
    io:format("Client added: ~p -> ~p~n", [Id, Username]),
    {noreply, NewState};
handle_cast({remove, Id}, State) ->
    NewState = maps:remove(Id, State),
    io:format("Client removed: ~p~n", [Id]),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

%% Публичный API
list() ->
    gen_server:call(?MODULE, list).

add(Id, Username, Pid) ->
    gen_server:cast(?MODULE, {add, Id, Username, Pid}).

remove(Id) ->
    gen_server:cast(?MODULE, {remove, Id}).

list_except(ExcludePid) ->
    gen_server:call(?MODULE, {list_except, ExcludePid}).
