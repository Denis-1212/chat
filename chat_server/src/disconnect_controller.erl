-module(disconnect_controller).

-include("json_keys.hrl").

-export([disconnect_client/2]).

disconnect_client(ClientId, Pid) ->
  client_store:remove(ClientId),
  OtherClients = client_store:list_except(Pid),
  Broadcasts =
    [make_client_list(ToId, ToPid, OtherClients) || {ToId, {_Name, ToPid}} <- OtherClients],
  Broadcasts.

make_client_list(ToId, ToPid, OtherClients) ->
  FilteredClients =
    [#{?ID => Id, ?USERNAME => Name} || {Id, {Name, _}} <- OtherClients, Id =/= ToId],
  Msg = jsx:encode(#{?TYPE => ?CLIENT_LIST, ?CLIENTS => FilteredClients}),
  {ToPid, Msg}.
