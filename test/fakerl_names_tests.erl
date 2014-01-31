%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 31 Jan 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_names_tests).
-include_lib("eunit/include/eunit.hrl").
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-define(FIRST_NAME, "Mawuli").
-define(LAST_NAME, "Adzaku").

%%%-------------------------------------------------------------------
%%% tests
%%%-------------------------------------------------------------------
first_name_test() ->
    ?assertNotEqual(?FIRST_NAME, fakerl_names:first_name()).

last_name_test() ->
    ?assertNotEqual(?LAST_NAME, fakerl_names:last_name()).

name_test() ->
    Fullname = ?FIRST_NAME ++ " " ++ ?LAST_NAME,
    ?assertNotEqual(Fullname, fakerl_names:name()).
