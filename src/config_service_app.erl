%%% -*- erlang -*-
-module(config_service_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    config_service_sup:start_link(config_service:port()).

stop(_State) ->
    ok.
