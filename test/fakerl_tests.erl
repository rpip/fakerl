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
-define(FIRST_NAME, "Mawuli").
-define(LAST_NAME, "Adzaku").
-define(TEMPLATE, "My first name is {{first_name}} and my last name is {{last_name}}").
-define(INAVLID_TEMPLATE, "My first name is {first_name} and my last name is {{last_name}.").
-export([first_name/0, last_name/0]).

%%%-------------------------------------------------------------------
%%% TESTS
%%%-------------------------------------------------------------------
parser_empty_string_test() ->
    Response = fakerl:parse("", ?MODULE),
    ?assertEqual({error, empty_string}, Response).

parser_nomatch_test() ->
    Response = fakerl:parse(?INAVLID_TEMPLATE, ?MODULE),
    ?assertEqual({error, nomatch}, Response).

parser_match_test() ->
    RawText = "My first name is " ++ first_name() ++ " and my last name is " ++ last_name(),
    RenderedTemplate = fakerl:parse(?TEMPLATE, ?MODULE),
    ?assertEqual(RawText, RenderedTemplate).

name_test() ->
    Fullname = ?FIRST_NAME ++ " " ++ ?LAST_NAME,
    ?assertNotEqual(Fullname, fakerl:name()).

%%%-------------------------------------------------------------------
%%% HELPERS
%%%-------------------------------------------------------------------
first_name() ->
    ?FIRST_NAME.

last_name() ->
    ?LAST_NAME.
