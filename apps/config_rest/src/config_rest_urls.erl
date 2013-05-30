%%% -*- erlang -*-
%%% @author Moritz <emoritz@GCI-EMORITZ-M.local>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% URLs for the rest service
%%% @end
%%% Created : 30 May 2013 by Moritz <emoritz@GCI-EMORITZ-M.local>

-module(config_rest_urls).

-export([dispatch/0, config_url/1]).

%%--------------------------------------------------------------------
%% @doc
%% Returns the cowboy dispatch for the config_rest service
%% @end
%%--------------------------------------------------------------------
dispatch() ->
    [
     {'_', 
      [
       {"/config-service/v1/configs/:keys", config_rest_config_handler, []}
      ]
     }
    ].

%%--------------------------------------------------------------------
%% @doc
%% Return the URL for a single config
%% @end
%%--------------------------------------------------------------------
-spec config_url(ConfigKeys :: binary()) -> binary().
config_url(ConfigKeys) ->
    <<"/config-service/v1/configs/", ConfigKeys/binary>>.
