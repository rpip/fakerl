%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mhp>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Company data generator 
%%% @end
%%% Created :  5 Feb 2014 by Mawuli Adzaku <mawuli@mhp>
%%%-------------------------------------------------------------------
-module(fakerl_company).
-export([first_name/0, last_name/0, company_name/0]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
first_name() ->
    fakerl:random_element(first_names()).

last_name() ->
    fakerl:random_element(last_names()).

company_name() ->
    fakerl:random_element(company_name_formats()).


%%%-------------------------------------------------------------------
%%% formats and helpers
%%%-------------------------------------------------------------------
last_names() ->
    ["Mobile", "Oil", "Farms", "Friends"].

first_names() ->
    ["Merrill", "Goldman", "First", "Virgin"].

company_name_formats() ->
    ["{{first_name}} and {{last_name}}", 
     "The {{first_name}} {{last_name}}", 
    "{{first_name}} {{last_name}} {{company_suffix}}", 
     "{{last_name}} and Associates"].

company_suffix() ->
    fakerl:random_element(company_suffixes()).

company_suffixes() ->
    ["Ltd", "Group", "LLC", "Holdings", "Partners", "Services"].
