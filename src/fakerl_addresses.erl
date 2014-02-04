%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Generates fake addresses
%%% @end
%%% Created : 31 Jan 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_addresses).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-define(FIRST_NAMES, ["New", "Nairobi" , "Melbourne", "Instanbul", "Karachi", "Santiago"]).
-define(LAST_NAMES, ["Berlin", "Lagos", "Shangai", "York", "Riyadh"]).
-define(CITY_SUFFIXES, ["Ville", "Township", "Metropolis"]).
-define(STREET_SUFFIXES, ["Street", "Avenue", "Boulevard", "Central", "Road", "Lane"]).
-compile([export_all]).


building_number_formats() ->
    ["##"].

postcode_formats() ->
    ["#####"].

city_formats() ->
    ["{{city}} {{city_suffix}}", "{{city}}", "{{first_name}}", "{{last_name}}"].

street_name_formats() ->
    ["{{last_name}} {{street_suffix}}"].

street_address_formats() ->
    ["{{building_number}} {{street_name}}"].

address_formats() ->
    ["{{street_address}} {{postcode}} {{city}}"].

countries() ->
    [binary_to_list(Name) ||
        [{<<"timezones">>, _},
         {<<"code">>, _},
         {<<"continent">>, _},
         {<<"name">>, Name},
         {<<"capital">>, _}
        ] <- fakerl_datetime:countries()
    ].

cities() ->
    [binary_to_list(Capital) ||
        [{<<"timezones">>, _},
         {<<"code">>, _},
         {<<"continent">>, _},
         {<<"name">>, _},
         {<<"capital">>, Capital}
        ] <- fakerl_datetime:countries()
    ].

city_suffix() ->
    fakerl:random_element(?CITY_SUFFIXES).

street_suffix() ->
    fakerl:random_element(?STREET_SUFFIXES).

first_name() ->
    fakerl:random_element(?FIRST_NAMES).

last_name() ->
    fakerl:random_element(?LAST_NAMES).

building_number() ->
    Format = fakerl:random_element(building_number_formats()),
    fakerl:numerify(Format).

city() ->
    Format = fakerl:random_element(city_formats()),
    fakerl:parse(Format, ?MODULE).

street_name() ->
    Format = fakerl:random_element(street_name_formats()),
    fakerl:parse(Format, ?MODULE).

street_address() ->
    Format = fakerl:random_element(street_address_formats()),
    fakerl:parse(Format, ?MODULE).

postcode() ->
    Format = fakerl:random_element(postcode_formats()),
    fakerl:numerify(Format).

%% @doc
%% @example: '791 Crist Parks, Sashabury, IL 86039-9874'
address() ->
    Format = fakerl:random_element(address_formats()),
    fakerl:parse(Format, ?MODULE).

country() ->
    fakerl:random_element(countries()).

geo_coordinate() ->
    N = fakerl:random(-180000000, 180000000),
    N1 = N / 1000000,
    Random = random:uniform(2),
    L = [N1 + 0.5, N1 - 0.5],
    lists:nth(Random, L).
    

%% @doc Returns a laitude co-ordinate
%%  Latitude has a range of -90 to 90, so divide by two.
latitude() ->
    geo_coordinate() / 2.

%% @doc Returns a longitude co-ordinate
longitude() ->
    geo_coordinate().
