%%% @author Moritz <emoritz@GCI-EMORITZ-M.local>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% Public API to the config_backend
%%% @end
%%% Created : 29 May 2013 by Moritz <emoritz@GCI-EMORITZ-M.local>

-module(config_backend).

-export([get_config_graph/1, store_config/2, delete_config/1]).

-type key() :: binary().
-type error() :: {error, any()}.

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

