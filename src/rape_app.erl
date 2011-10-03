%% @author Mochi Media <dev@mochimedia.com>
%% @copyright rape Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the rape application.

-module(rape_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for rape.
start(_Type, _StartArgs) ->
    rape_deps:ensure(),
    rape_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for rape.
stop(_State) ->
    ok.
