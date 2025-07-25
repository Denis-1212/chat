-module(client_controller).

-include("json_keys.hrl").

-export([handle/3]).

handle(Action, Payload, Pid) ->
  case Action of
    ?LOGIN_ACTION ->
      init_client(maps:get(?USERNAME, Payload, undefined), Pid);
    _Other ->
      io:format("~p : Action not supported! ~n", [?MODULE]),
      ok
  end.

init_client(Username, Pid) ->
  ClientId = generate_id(),
  ok = client_store:add(ClientId, Username, Pid),
  io:format("Client ~p, Id ~p connected~n", [Username, ClientId]),
  Reply =
    jsx:encode(#{?TYPE => ?CLIENT_INFO,
                 ?CLIENT => #{?ID => ClientId, ?USERNAME => Username}}),
  Pid ! {send_ws, Reply},

  broadcast_client_list(),
  broadcast_msg_list(Pid),
  {ClientId, Username}.

broadcast_msg_list(Pid) ->
  MessageList = msg_store:list(),
  MsgListJson =
    lists:map(fun({Mid, MUser, MMsg, MDate}) ->
                 #{?ID => Mid,
                   ?USERNAME => MUser,
                   ?MESSAGE => MMsg,
                   ?DATETIME => MDate}
              end,
              MessageList),
  MsgToSend = jsx:encode(#{?TYPE => ?MESSAGES, ?MESSAGES => MsgListJson}),
  Pid ! {send_ws, MsgToSend}.

broadcast_client_list() ->
  AllClients = client_store:list(),
  Broadcasts =
    [make_client_list_for(ToId, ToPid, AllClients) || {ToId, {_Name, ToPid}} <- AllClients],

  [ToPid ! {send_ws, Msg} || {ToPid, Msg} <- Broadcasts].

make_client_list_for(ToId, ToPid, AllClients) ->
  FilteredClients =
    [#{?ID => Id, ?USERNAME => Name} || {Id, {Name, _}} <- AllClients, Id =/= ToId],
  Msg = jsx:encode(#{?TYPE => ?CLIENT_LIST, ?CLIENTS => FilteredClients}),
  {ToPid, Msg}.

generate_id() ->
  <<ID:128>> = crypto:strong_rand_bytes(16),
  iolist_to_binary(io_lib:format("~32.16.0b", [ID])).
