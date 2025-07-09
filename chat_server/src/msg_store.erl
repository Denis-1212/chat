-module(msg_store).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([list/0, add/4]).

%% gen_server callbacks
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  {ok, []}.

handle_call(list, _From, State) ->
  {reply, lists:reverse(State), State}.

handle_cast({add, Id, Username, Msg, DateTime}, State) ->
  NewState = [{Id, Username, Msg, DateTime} | State],
  io:format("Message added [~ts]: ~ts (~ts)~n", [DateTime, Msg, Username]),
  {noreply, NewState}.

handle_info(_Info, State) ->
  {noreply, State}.

%% Публичный API
list() ->
  gen_server:call(?MODULE, list).

add(Id, Username, Msg, DateTime) ->
  gen_server:cast(?MODULE, {add, Id, Username, Msg, DateTime}).

%% @doc Возвращает список всех сообщений.
-spec list() ->
            [{Id :: binary(), Username :: binary(), Msg :: binary(), DateTime :: binary()}].
%% @doc Добавляет сообщение.
-spec add(binary(), binary(), binary(), binary()) -> ok.
