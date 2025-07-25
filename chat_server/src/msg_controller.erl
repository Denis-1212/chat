-module(msg_controller).

-include("json_keys.hrl").

-export([handle/2]).

handle(Action, Payload) ->
  case Action of
    ?SEND_MESSAGE_ACTION ->
      add_message(Payload);
    _Other ->
      io:format("Action not supported! ~n"),
      ok
  end.

add_message(Payload) ->
  Id = maps:get(?USER_ID, Payload, undefined),
  Username = maps:get(?USERNAME, Payload, undefined),
  Msg = maps:get(?MESSAGE_CONTENT, Payload, undefined),
  DateTime = maps:get(?DATETIME, Payload, undefined),
  msg_store:add(Id, Username, Msg, DateTime),
  AllClients = client_store:list(),
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
  [ToPid ! {send_ws, MsgToSend} || {_ToId, {_Name, ToPid}} <- AllClients].
