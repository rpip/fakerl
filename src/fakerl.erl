%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").
-compile([export_all]).

name() ->
   undefined.

address() ->
    undefined.

text() ->
    undefined.

random_number() ->
    fakerl_base_provider:random_number().

random_letter() ->
    fakerl_base_provider:random_letter().

random_letter(caps) ->
    fakerl_base_provider:random_letter(caps).

random_element(L) ->
    fakerl_base_provider:random_element(L).

random(From, To) ->
    fakerl_base_provider:random(From, To).

random(From, To, LowerBound, UpperBound) ->
    fakerl_base_provider:random(From, To, LowerBound, UpperBound).
