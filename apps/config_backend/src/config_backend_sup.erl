% -*- erlang -*-
-module(config_backend_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {WorkerMod, WorkerArgs}} = config_backend:backend(),
    {ok, SizeArgs} = config_backend:pool(),
    {ok, PoolName} = config_backend:poolname(),

    PoolArgs = [
                {name, {local, PoolName}},
                {worker_module, WorkerMod}
               ] ++ SizeArgs,

    PoolSpec = poolboy:child_spec(
                 PoolName,
                 PoolArgs,
                 WorkerArgs
               ),
    {ok, { {one_for_one, 5, 10}, [PoolSpec]} }.

