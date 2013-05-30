% -*- erlang -*-
%%% @author Moritz <emoritz@GCI-EMORITZ-M.local>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% Public API to the config_backend
%%% @end
%%% Created : 29 May 2013 by Moritz <emoritz@GCI-EMORITZ-M.local>

-module(config_backend).

%% public API
-export([start/0, get_config_graph/1, store_config/2, delete_config/1]).

%% Conifguration functions
-type config_error() :: {error, not_configured | value_error}.
-export([backend/0, pool/0, poolname/0]).

-type key() :: binary().
-type error() :: {error, any()}.

start() ->
    ok = apptools:ensure_started(?MODULE, permanent).

%% ===================================================================
%% Public API
%% ===================================================================

-spec backend() -> {ok, {Mod :: atom(), Args :: list()}} | config_error().
backend() ->
    get_env(backend).

-spec pool() -> {ok, [proplists:property()]} | config_error().
pool() ->
    get_env(pool).

-spec poolname() -> {ok, atom()}.
poolname() ->
    {ok, ?MODULE}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieve the config DAG as a list
%% @end
%%--------------------------------------------------------------------
-spec get_config_graph(Keys :: any()) -> {ok, list()} | error().
get_config_graph(Keys) ->
    poolboy:transaction(
      ?MODULE,
      fun(Pid) ->
              gen_server:call(Pid, {get_config_graph, Keys})
      end
     ).

%%--------------------------------------------------------------------
%% @doc
%% Store a config
%% @end
%%--------------------------------------------------------------------
-spec store_config(Key :: key(), Properties :: list()) -> ok | error().
store_config(Key, Properties) ->
    poolboy:transaction(
      ?MODULE,
      fun(Pid) ->
              gen_server:call(Pid, {store_config, Key, Properties})
      end
     ).

%%--------------------------------------------------------------------
%% @doc
%% Delete a config
%% @end
%%--------------------------------------------------------------------
-spec delete_config(Key :: key()) -> ok | error().
delete_config(Key) ->
    poolboy:transaction(
      ?MODULE,
      fun(Pid) ->
              gen_server:call(Pid, {delete_config, Key})
      end
     ).


%% ===================================================================
%% Internal
%% ===================================================================
-spec get_env(Name :: any()) -> {ok, any()} | config_error().
get_env(Name) ->
    case application:get_env(?MODULE, Name) of
        undefined ->
            {error, not_configured};
        {ok, V} ->
            {ok, V}
    end.

