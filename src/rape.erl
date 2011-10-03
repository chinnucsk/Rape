%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc rape.

-module(rape).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the rape server.
start() ->
    rape_deps:ensure(),
    ensure_started(crypto),
    application:start(rape).


%% @spec stop() -> ok
%% @doc Stop the rape server.
stop() ->
    application:stop(rape).
