-module(client_store).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([list/0, add/3, remove/1, get_pid/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{}}.

handle_call(list, _From, State) ->
    {reply, maps:to_list(State), State};
handle_call({remove, Id}, _From, State) ->
    NewState = maps:remove(Id, State),
    io:format("Client removed: ~p~n", [Id]),
    {reply, maps:to_list(NewState), NewState};
handle_call({get_pid, Username}, _From, State) ->
    Result =
        lists:filtermap(fun ({_Id, {U, Pid}}) when U =:= Username ->
                                {true, Pid};
                            (_) ->
                                false
                        end,
                        maps:to_list(State)),
    case Result of
        [Pid] ->
            {reply, {ok, Pid}, State};
        [] ->
            {reply, {error, not_found}, State}
    end.

handle_cast({add, Id, Username, Pid}, State) ->
    NewState = State#{Id => {Username, Pid}},
    io:format("Client added: ~p -> ~p~n", [Id, Username]),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

%% Публичный API
list() ->
    gen_server:call(?MODULE, list).

add(Id, Username, Pid) ->
    gen_server:cast(?MODULE, {add, Id, Username, Pid}).

remove(Id) ->
    gen_server:call(?MODULE, {remove, Id}).

get_pid(Username) ->
    gen_server:call(?MODULE, {get_pid, Username}).
