%%% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% 
%%% @end
%%% Created : 30 May 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(config_rest).

-export([start/0]).


start() ->
    ok = apptools:ensure_started(?MODULE, permanent),
    Dispatch = cowboy_router:compile(
                 config_rest_urls:dispatch()
                ),
    Port = port(),

    {ok, _} = cowboy:start_http(?MODULE, 100,
                                [{port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    error_logger:info_msg("Listening on ~w~n", [Port]),
    ok.
                                

port() ->
    port(os:getenv("PORT")).

port(false) ->
    8000;
port(Val) ->
    list_to_integer(Val).
