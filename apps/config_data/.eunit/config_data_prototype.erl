% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% prototypical inheritence of proplists
%%% @end
%%% Created : 29 May 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(config_data_prototype).
-include_lib("eunit/include/eunit.hrl").

-export([
         new/1,
         get_value/2,
         get_value/3,
         store/3,
         fold/3
]).

%%--------------------------------------------------------------------
%% Public
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc 
%% Create a new 
%% [Properties, Parent, Grandparent]
%% @end
%%--------------------------------------------------------------------
new(FamilyTree) ->
    FamilyTree.

%%--------------------------------------------------------------------
%% @doc returns the value from the prototype graph
%% @end
%%--------------------------------------------------------------------
-spec get_value(Key :: any(), ListOfLists :: list()) -> any() | undefined.
get_value(Key, FamilyTree) ->
    get_value(Key, FamilyTree, undefined).

get_value_2_test() ->
    PP = new([
              [{foo, bar}],
              [{foo, baz}, {bar, bing}]
             ]
            ),
    ?assertEqual(get_value(foo, PP), bar),
    ?assertEqual(get_value(bar, PP), bing),
    ?assertEqual(get_value(bazinga, PP), undefined).

%%--------------------------------------------------------------------
%% @doc returns the value from the prototype graph
%% returning Default if the key is not found
%% @end
%%--------------------------------------------------------------------
-spec get_value(Key :: any(), ListOfLists :: list(), Default :: any()) -> any().
get_value(_, [], Default) ->
    Default;
get_value(Key, [H|Prototype], Default) ->
    case proplists:get_value(Key, H, {?MODULE, key_not_found}) of
        {?MODULE, key_not_found} ->
            get_value(Key, Prototype, Default);
        Value ->
            Value
    end.

get_value_3_test() ->
    PP = new([
              [{foo, bar}],
              [{foo, baz}, {bar, bing}]
             ]
            ),
    ?assertEqual(get_value(foo, PP, xxx), bar),
    ?assertEqual(get_value(bar, PP, xxx), bing),
    ?assertEqual(get_value(bazinga, PP, xxx), xxx).

%%--------------------------------------------------------------------
%% @doc Update the root proplist with the property
%% @end
%%--------------------------------------------------------------------

-spec store(Key :: any(), Value :: any(), ListOfLists :: list()) -> list().
store(Key, Value, [Root|Prototype]) ->
    NewRoot = [{Key, Value}|Root],
    [NewRoot|Prototype].

store_test() ->
    PP = new([
              [{foo, bar}],
              [{foo, bing}]
             ]            
            ),
    PP2 = store(foo, baz, PP),

    R = get_value(foo, PP2),
    
    ?assertEqual(R, baz).

%%--------------------------------------------------------------------
%% @doc Fold over deduped key/value pairs
%%--------------------------------------------------------------------
-spec fold(Callback :: function(), Initial :: any(), list()) -> any().
fold(Callback, Initial, ListOfLists) ->
    {_Callback, _Seen, Result} = lists:foldl(
                       fun dedupe_proplists/2,
                       {Callback, sets:new(), Initial},
                       ListOfLists
                      ),
    Result.

fold_test() ->
    R = fold(
      fun(P, Accum) ->
              [P|Accum]
      end,
      [],
      new([
           [{foo, bar}],
           [{foo, baz}, {bar, bing}]
          ])
    ),
    ?assertEqual(
       R,
       [{bar, bing}, {foo, bar}]
    ).
    

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
dedupe_proplists(Proplist, {Callback, Seen, Accum}) ->
    lists:foldl(
      fun dedupe_properties/2,
      {Callback, Seen, Accum},
      Proplist
     ).

dedupe_properties(P={Key, _}, {Callback, Seen, Accum}) ->
    case sets:is_element(Key, Seen) of
        true ->
            % Ignore already seen key
            {Callback, Seen, Accum};
        false ->
            Accum2 = Callback(P, Accum),
            {
              Callback,
              sets:add_element(Key, Seen),
              Accum2
            }
    end.

