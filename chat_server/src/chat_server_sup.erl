%%%-------------------------------------------------------------------
%% @doc chat_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children =
        [%% Клиенты
         #{id => client_store,
           start => {client_store, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [clientClientsPid_store]},
         %% Сообщения
         #{id => msg_store,
           start => {msg_store, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [msg_store]}],

    {ok, {{one_for_one, 5, 10}, Children}}.
