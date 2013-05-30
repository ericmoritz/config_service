%%%% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(config_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(HTTPPort) ->
    Ret = supervisor:start_link({local, ?MODULE}, ?MODULE, [HTTPPort]),
    error_logger:info_msg("Cowboy listening on ~w~n", [HTTPPort]),
    Ret.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([HTTPPort]) ->
    CowboySpec = cowboy_spec(HTTPPort),
    {ok, { {one_for_one, 5, 10}, [CowboySpec]} }.

cowboy_spec(HTTPPort) ->
    Dispatch = cowboy_router:compile(
                 quickdraw:dispatch([
                                     config_rest_urls
                                    ])
                ),
    ranch:child_spec(
      config_service, 100,
      ranch_tcp, [{port, HTTPPort}],
      cowboy_protocol, [{env, [{dispatch, Dispatch}]}]
     ).
    
      
    
