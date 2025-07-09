-module(websocket_handler).

-include("json_keys.hrl").

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {client_id :: binary(), username :: binary(), clients_pid :: pid()}).

-define(PING_TIMEOUT, 5000).

init(Req, _Opts) ->
    ClientsPid = whereis(client_store), % Получаем Pid хранилища
    whereis(msg_store),
    {cowboy_websocket, Req, #state{clients_pid = ClientsPid}}.

websocket_init(State) ->
    erlang:send_after(?PING_TIMEOUT, self(), ping),
    {ok, State}.

websocket_handle({text, Message}, State) ->
    try
        Decoded = jsx:decode(Message, [return_maps]),
        Controller = maps:get(?CONTROLLER, Decoded, undefined),
        Action = maps:get(?ACTION, Decoded, undefined),
        Payload = maps:get(?PAYLOAD, Decoded, #{}),

        io:format("Incoming message for controller: ~p~n", [Controller]),
        case Controller of
            ?USER_CONTROLLER ->
                io:format("Incoming message for : ~p~n", [?USER_CONTROLLER]),
                {ClientId, Username, Reply, Broadcasts} =
                    client_controller:handle(Action, Payload, self(), State),
                [Pid ! {send_ws, Msg} || {Pid, Msg} <- Broadcasts],
                NewState = State#state{client_id = ClientId, username = Username},

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
                self() ! {send_ws, MsgToSend},
                {reply, {text, Reply}, NewState};
            ?MESSAGE_CONTROLLER ->
                io:format("Incoming message for : ~p~n", [?MESSAGE_CONTROLLER]),
                {ok, State};
            ?SIGNALING_CONTROLLER ->
                io:format("Incoming message for : ~p~n", [?SIGNALING_CONTROLLER]),
                {ok, State};
            _Other ->
                {ok, State}
        end
    catch
        _:_ ->
            io:format("Error! ~n"),
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
terminate(_Reason,
          _Req,
          #state{client_id = ClientId,
                 username = Username,
                 clients_pid = ClientsPid}) ->
    io:format("Client ~ts disconnected~n", [Username]),
    Broadcasts = disconnect_controller:disconnect_client(ClientId, ClientsPid),
    [Pid ! {send_ws, Msg} || {Pid, Msg} <- Broadcasts],
    ok.
