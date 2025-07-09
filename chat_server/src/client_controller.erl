-module(client_controller).

-include("json_keys.hrl").

-export([init_client/2, handle/4]).

handle(Action, Payload, Pid, State) ->
  Username = maps:get(?USERNAME, Payload, undefined),
  case Action of
    ?LOGIN_ACTION ->
      init_client(Username, Pid);
    _Other ->
      io:format("Action not supported! ~n"),
      {ok, State}
  end.

init_client(Username, Pid) ->
  ClientId = generate_id(),
  ok = client_store:add(ClientId, Username, Pid),
  io:format("Client ~p, Id ~p connected~n", [Username, ClientId]),

  %% Ответ для подключившегося клиента
  Reply =
    jsx:encode(#{?TYPE => ?CLIENT_INFO,
                 ?CLIENT => #{?ID => ClientId, ?USERNAME => Username}}),

  %% Собираем список всех клиентов
  AllClients = client_store:list(),

  %% Формируем персонализированные списки для каждого
  Broadcasts =
    [make_client_list_for(ToId, ToPid, AllClients) || {ToId, {_Name, ToPid}} <- AllClients],

  {ClientId, Username, Reply, Broadcasts}.

%% Вспомогательная функция: сгенерировать персонализированный список клиентов
make_client_list_for(ToId, ToPid, AllClients) ->
  FilteredClients =
    [#{?ID => Id, ?USERNAME => Name} || {Id, {Name, _}} <- AllClients, Id =/= ToId],
  Msg = jsx:encode(#{?TYPE => ?CLIENT_LIST, ?CLIENTS => FilteredClients}),
  {ToPid, Msg}.

%% Утилита генерации UUID
generate_id() ->
  <<ID:128>> = crypto:strong_rand_bytes(16),
  iolist_to_binary(io_lib:format("~32.16.0b", [ID])).
