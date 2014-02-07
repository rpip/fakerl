%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Date and time fake data generator
%%% @end
%%% @todo date/time arithmetic
%%% Created :  3 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_datetime).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").
-export([unixtime/0, date_time/0, date_time/1,
         date/0, date/1, time/0, time/1, am_pm/0,
         timezones/0, timezone/0, year/0, month_name/0, month/0,
         day_of_month/0, day_of_week/0, century/0,
        iso8601/0, country/0, countries/0]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Get a timestamp between January 1, 1970 and now
%% :example 1061306726
unixtime() ->
    qdate:to_unixtime(now()).

date_time() ->
    Pattern = fakerl:random_element(datetime_formats()),
    date_time(Pattern).

date_time(Pattern) ->
    qdate:to_string(Pattern, now()).

%% @doc example '2003-10-21T16:05:52+0000'
iso8601() ->
    date_time("c").

%% @doc Get a date string between January 1, 1970 and now
%% example: '2008-11-27'
date() ->
    ?MODULE:date(fakerl:random_element(date_formats())).

date(Pattern) ->
    qdate:to_string(Pattern, now()).

%% @doc Get a time string (24h format by default)
%% example: '15:02:34'
time() ->
    ?MODULE:time(fakel:random_element(time_formats())).

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

%% @doc Retunrn a randomly selected century'XVII'
century() ->
    fakerl:random_element(centuries()).

timezones() ->
    [Timezones ||
        [{<<"timezones">>, Timezones},
         {<<"code">>, _},
         {<<"continent">>, _},
         {<<"name">>, _},
         {<<"capital">>, _}
        ] <- countries()
    ].

timezone() ->
    Timezones = fakerl:random_element(timezones),
    fakerl:random_element(Timezones).

-spec countries() -> binary() | {error, Reason :: string()}.
countries() ->
    case file:read_file(?COUNTRIES_JSON_FILE) of
        {ok, Bin} ->
            jsx:decode(Bin);
        {error, enoent} ->
            error("Missing countries json file: " ++ ?COUNTRIES_JSON_FILE)
    end.


country() ->
    fakerl:random_element(countries()).

%%%-------------------------------------------------------------------
%%% formats and helpers
%%%-------------------------------------------------------------------
centuries() ->
    ['I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 
     'VIII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV', 
     'XV', 'XVI','XVII', 'XVIII', 'XIX', 'XX', 'XXI'].

date_formats() ->
    ["Y-m-d"].

time_formats() ->
    ["h:m:s", "g:ia"].

datetime_formats() ->
    ["Y-m-d H:i:s T", "Y-m-d g:ia", "n/j/Y g:ia", "D, d M Y H:i:s O"].
