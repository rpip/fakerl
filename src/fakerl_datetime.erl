%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Date and time fake data generator
%%% @end
%%% todo: date/time arithmetic
%%% Created :  3 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_datetime).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").
-export([unixtime/0,
         date_time/0,
         date_time/1,
         date/0,
         date/1,
         time/0,
         time/1,
         am_pm/0,
         timezones/0,
         timezone/0,
         year/0,
         month_name/0,
         month/0,
         day_of_month/0,
         day_of_week/0,
         century/0,
         iso8601/0]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Get a timestamp between January 1, 1970 and now
%% :example 1061306726
unixtime() ->
    qdate:to_unixtime(now()).

date_time() ->
    Pattern = fakerl:fetch("datetime.datetime"),
    date_time(Pattern).

date_time(Pattern) ->
    qdate:to_string(Pattern, now()).

%% @doc example '2003-10-21T16:05:52+0000'
iso8601() ->
    date_time("c").

%% @doc Get a date string between January 1, 1970 and now
%% example: '2008-11-27'
date() ->
    ?MODULE:date(fakerl:fetch("date.date")).

date(Pattern) ->
    qdate:to_string(Pattern, now()).

%% @doc Get a time string (24h format by default)
%% example: '15:02:34'
time() ->
    ?MODULE:time(fakel:fetch("datetime.time")).

time(Pattern) ->
    qdate:to_string(Pattern, now()).

am_pm() ->
    ?MODULE:date("a").

day_of_month() ->
    ?MODULE:date("d").

day_of_week() ->
    ?MODULE:date("w").

month() ->
    ?MODULE:date('m').

month_name() ->    
    ?MODULE:date("F").

year() ->
    ?MODULE:date("Y").

%% @doc Retunrn a randomly selected century: 'XVII'
century() ->
    fakerl:fetch("datetime.centuries").

timezones() ->
    fakerl:fetch("address.timezones", all).

timezone() ->
    fakerl:fetch("address.timezones").
