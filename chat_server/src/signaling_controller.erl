-module(signaling_controller).

-export([handle/3]).

%% handle(SignalingType, Msg, FromUser) -> {ok, ToUser, Payload} | error.
handle(<<"offer">>, Msg, FromUser) ->
  case maps:get(<<"to">>, Msg, undefined) of
    undefined ->
      error;
    ToUser ->
      Payload =
        #{<<"type">> => <<"webrtc_offer">>,
          <<"from">> => FromUser,
          <<"sdp">> => maps:get(<<"sdp">>, Msg, <<"">>)},
      {ok, ToUser, Payload}
  end;
handle(<<"answer">>, Msg, FromUser) ->
  case maps:get(<<"to">>, Msg, undefined) of
    undefined ->
      error;
    ToUser ->
      Payload =
        #{<<"type">> => <<"webrtc_answer">>,
          <<"from">> => FromUser,
          <<"sdp">> => maps:get(<<"sdp">>, Msg, <<"">>)},
      {ok, ToUser, Payload}
  end;
handle(<<"ice_candidate">>, Msg, FromUser) ->
  case maps:get(<<"to">>, Msg, undefined) of
    undefined ->
      error;
    ToUser ->
      Payload =
        #{<<"type">> => <<"ice_candidate">>,
          <<"from">> => FromUser,
          <<"candidate">> => maps:get(<<"candidate">>, Msg, #{})},
      {ok, ToUser, Payload}
  end;
handle(_, _, _) ->
  error.
