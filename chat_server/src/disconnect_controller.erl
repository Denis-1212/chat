-module(disconnect_controller).

-include("json_keys.hrl").

-export([disconnect_client/1]).

disconnect_client(ClientId) ->
  Clients = client_store:remove(ClientId),
  Broadcasts =
    [make_client_list(ToId, ToPid, Clients) || {ToId, {_Name, ToPid}} <- Clients],
  [Pid ! {send_ws, Msg} || {Pid, Msg} <- Broadcasts],
  ok.

make_client_list(ToId, ToPid, Clients) ->
  FilteredClients =
    [#{?ID => Id, ?USERNAME => Name} || {Id, {Name, _}} <- Clients, Id =/= ToId],
  Msg = jsx:encode(#{?TYPE => ?CLIENT_LIST, ?CLIENTS => FilteredClients}),
  {ToPid, Msg}.
