%%% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% RESTful handler for single configuration nodes
%%% @end
%%% Created : 30 May 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(config_rest_config_handler).
-include_lib("eunit/include/eunit.hrl").

-record(state, {keys, props}).

% Cowboy handler callbacks
-export([init/3]).

% Cowboy REST callbacks
-export([rest_init/2, allowed_methods/2]).

% Cowboy REST GET callbacks
-export([resource_exists/2, content_types_provided/2, to_json/2]).

% Cowboy REST PUT callbacks

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {KeyCSV, Req2} = cowboy_req:binding(keys, Req),
    Keys = split_keys(KeyCSV),
    {ok, Req2, #state{keys=Keys}}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

resource_exists(Req, State=#state{keys=Keys}) ->
    {ok, Props} = materialize_config_graph(config_backend:get_config_graph(Keys)),
    {true, Req, State#state{props=Props}}.

content_types_provided(Req, State) ->
    {
      [
       %% TODO: {{<<"application">>, <<"x-www-form-urlencoded">>, []}, to_urlencode},
       {{<<"application">>, <<"json">>, []}, to_json}
      ],
      Req, State
    }.

to_json(Req, State=#state{props=Props}) ->
    JSONPacket = jiffy:encode({Props}),
    {JSONPacket, Req, State}.


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
-spec split_keys(KeyCSV :: binary()) -> [binary()].
split_keys(KeyCSV) ->
    split_keys(KeyCSV, [], <<>>).

split_keys(<<>>, Accum, CurrentKey) ->
    lists:reverse([CurrentKey|Accum]);
split_keys(<<",", Rest/binary>>, Accum, CurrentKey) ->
    split_keys(Rest, [CurrentKey|Accum], <<>>);
split_keys(<<C:8, Rest/binary>>, Accum, CurrentKey) ->
    split_keys(Rest, Accum, <<CurrentKey/binary, C:8>>).

split_keys_test() ->
    ?assertEqual(
       [<<"human">>],
       split_keys(<<"human">>)
      ),

    ?assertEqual(
       [<<"adult">>,<<"human">>],
       split_keys(<<"adult,human">>)
      ),

    ?assertEqual(
       [<<"eric">>,<<"adult">>,<<"human">>],
       split_keys(<<"eric,adult,human">>)
      ).

materialize_config_graph(E={error, _}) ->
    E;
materialize_config_graph({ok, PropGraph}) ->
    FlattenedProps = flatten(PropGraph),
    {ok, FlattenedProps}.


flatten(PropGraph) ->
    Data = config_data:new(PropGraph),
    config_data:fold(
      fun(P, Accum) ->
              [P|Accum]
      end,
      [],
      Data
    ).

flatten_test() ->
    ?assertEqual(
       [{bar, baz}, {foo, bar}],
       flatten(
         [
          [{foo, bar}],
          [{foo, baz}, {bar, baz}]
         ])
      ).
