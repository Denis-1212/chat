-module(signaling_controller).

-include("json_keys.hrl").

-export([handle/3]).

handle(Action, Payload, FromUser) ->
  io:format("Action: ~p ~n FromUser: ~p ~n ", [Action, FromUser]),
  case maps:get(?SIGNALING_TO_USER, Payload, undefined) of
    undefined ->
      error;
    ToUser ->
      case Action of
        ?SIGNALING_OFFER_ACTION ->
          offer(ToUser, Payload, FromUser);
        ?SIGNALING_ANSWER_ACTION ->
          answer(ToUser, Payload, FromUser);
        ?SIGNALING_ICE_CANDIDATE_ACTION ->
          ice_candidate(ToUser, Payload, FromUser);
        _Other ->
          io:format("~p : Action not supported! ~n", [?MODULE]),
          ok
      end
  end;
handle(_, _, _) ->
  error.

offer(ToUser, Payload, FromUser) ->
  Offer_payload =
    #{?TYPE => ?SIGNALING_OFFER_ACTION,
      ?SIGNALING_FROM => FromUser,
      ?SIGNALING_SDP => maps:get(?SIGNALING_SDP, Payload, <<"">>)},
  % io:format("Offer_payload : ~p~n", [Offer_payload]),
  send_signaling(ToUser, Offer_payload),
  ok.

answer(ToUser, Payload, FromUser) ->
  Answer_payload =
    #{?TYPE => ?SIGNALING_ANSWER_ACTION,
      ?SIGNALING_FROM => FromUser,
      ?SIGNALING_SDP => maps:get(?SIGNALING_SDP, Payload, <<"">>)},
  send_signaling(ToUser, Answer_payload),
  ok.

ice_candidate(ToUser, Payload, FromUser) ->
  Ice_candidate_payload =
    #{?TYPE => ?SIGNALING_ICE_CANDIDATE_ACTION,
      ?SIGNALING_FROM => FromUser,
      ?SIGNALING_CANDIDATE => maps:get(?SIGNALING_CANDIDATE, Payload, #{})},
  send_signaling(ToUser, Ice_candidate_payload),
  ok.

send_signaling(ToUser, Payload) ->
  % io:format("send_signaling ToUser ~p~n ", [ToUser]),
  case client_store:get_pid(ToUser) of
    {ok, Pid} ->
      io:format("ToUser ~p send_signaling~n", [ToUser]),
      Pid ! {send_ws, jsx:encode(Payload)};
    error ->
      io:format("User ~p not found for signaling~n", [ToUser])
  end,
  ok.
