-module(chat_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(SERVER_WS_PORT, 8080).

start(_StartType, _StartArgs) ->
    io:format("Starting chat server on port ~p~n", [?SERVER_WS_PORT]),

    %% Запускаем сервер
    application:ensure_all_started(cowboy),
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/assets/[...]",
                                  cowboy_static,
                                  {priv_dir, chat_server, "static/assets"}},
                                 {"/",
                                  cowboy_static,
                                  {priv_file, chat_server, "static/index.html"}},
                                 {"/websocket", websocket_handler, []}]}]),
    {ok, _} =
        cowboy:start_clear(http, [{port, ?SERVER_WS_PORT}], #{env => #{dispatch => Dispatch}}),
    chat_server_sup:start_link().

stop(_State) ->
    ok.
