-module(msg_controller).

-export([add_message/4]).

add_message(Id, Username, Msg, DateTime) ->
  msg_store:add(Id, Username, Msg, DateTime),

  AllClients = client_store:list(),
  MessageList = msg_store:list(),
  {MessageList, AllClients}.
