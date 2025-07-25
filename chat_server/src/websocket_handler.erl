-module(websocket_handler).

-include("json_keys.hrl").

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {client_id :: binary(), username :: binary()}).
-record(incoming_message,
        {controller :: binary(), action :: binary(), payload :: binary()}).

-define(PING_TIMEOUT, 5000).

init(Req, _Opts) ->
    whereis(client_store),
    whereis(msg_store),
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    erlang:send_after(?PING_TIMEOUT, self(), ping),
    {ok, State}.

websocket_handle({text, Message}, State) ->
    try
        #incoming_message{controller = Controller,
                          action = Action,
                          payload = Payload} =
            parse_incoming_message(Message),

        case Controller of
            ?USER_CONTROLLER ->
                io:format("Incoming message for : ~p~n", [?USER_CONTROLLER]),
                {ClientId, Username} = client_controller:handle(Action, Payload, self()),
                NewState = State#state{client_id = ClientId, username = Username},
                {ok, NewState};
            ?MESSAGE_CONTROLLER ->
                io:format("Incoming message for : ~p~n", [?MESSAGE_CONTROLLER]),
                msg_controller:handle(Action, Payload),
                {ok, State};
            ?SIGNALING_CONTROLLER ->
                io:format("Incoming message for : ~p~n", [?SIGNALING_CONTROLLER]),
                signaling_controller:handle(Action, Payload, State#state.username),
                {ok, State};
            _Other ->
                io:format("Сontroller not supported ~n"),
                {ok, State}
        end
    catch
        _:_ ->
            io:format("Websocket_handle Error! ~n"),
            {ok, State}
    end;
websocket_handle(Frame, State) ->
    io:format("~p: ~p~n", [Frame, State#state.client_id]),%pong
    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send_ws, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(ping, State) ->
    erlang:send_after(?PING_TIMEOUT, self(), ping),
    io:format("ping: ~p~n", [State#state.client_id]),
    {reply, ping, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, #state{client_id = undefined}) ->
    ok;
terminate(_Reason, _Req, #state{client_id = ClientId, username = Username}) ->
    io:format("Client ~ts disconnected~n", [Username]),
    disconnect_controller:disconnect_client(ClientId),
    ok.

parse_incoming_message(Message) ->
    Decoded = jsx:decode(Message, [return_maps]),
    Controller = maps:get(?CONTROLLER, Decoded, undefined),
    Action = maps:get(?ACTION, Decoded, undefined),
    Payload = maps:get(?PAYLOAD, Decoded, #{}),
    #incoming_message{controller = Controller,
                      action = Action,
                      payload = Payload}.
