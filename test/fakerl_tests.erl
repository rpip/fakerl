%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 31 Jan 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_tests).
-include_lib("eunit/include/eunit.hrl").
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
codebox_test_() ->
    {spawn, 
     {setup,
      fun setup/0,
      fun teardown/1,
      [
       {"Check application locale",
        fun locale/0},
       {"Retrieve the locale data key",
        fun locale_data_key/0},
       {"Random data retrieval/generation",
        fun random/0},
       {"Index based variables",
        fun safe_var_idx_regex/0},
       {"Test number generation from templates",
        fun numerify/0},
       {"Test character generation",
        fun letterify/0},
       {"Parse an empty template string",
        fun parse_empty_string/0},
       {"Test configs",
        fun config/0}
     ]
     }
    }.


%%%-------------------------------------------------------------------
%%% Setup / Cleanup
%%%-------------------------------------------------------------------
setup() ->
    fakerl:locale(?DEFAULT_LOCALE).

teardown(_) ->
    ok.

%%%-------------------------------------------------------------------
%%% tests
%%%-------------------------------------------------------------------
locale() ->
    ?assertEqual(?DEFAULT_LOCALE, fakerl:locale()).

locale_data_key() ->
    ?assertEqual(en_data, fakerl:locale_data_key(?DEFAULT_LOCALE)).

random() ->
    ?assert($a =< fakerl:random_letter()),
    N = fakerl:random_number(),
    ?assert(lists:member(N, lists:seq(0,9))).

safe_var_idx_regex() ->
    ?assertEqual("{(0)}", fakerl:safe_var_idx_regex("0")),
    ?assertEqual("{(0)}", fakerl:safe_var_idx_regex(0)).
    
parse_empty_string() ->
    ?assertEqual({error, empty_string}, fakerl:parse("", address)).

numerify() ->
    Number = list_to_integer(fakerl:numerify("#")),
    ?assert(Number =< 10).

letterify() ->
    Letter = fakerl:letterify("?"),
    AToZ = lists:seq($a,$z),
    ?assert(string:rstr(AToZ, Letter) > 0).

config() ->
    fakerl:config(foo, bar),
    ?assertEqual({ok, bar}, fakerl:config(foo)).
