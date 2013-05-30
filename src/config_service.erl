%%% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(config_service).

-export([start/0, port/0]).

start() ->
    apptools:ensure_started(?MODULE, permanent).

port() ->
    port(os:getenv("PORT")).

port(false) ->
    8000;
port(Val) ->
    list_to_integer(Val).
