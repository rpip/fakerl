%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mhp>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Company data generator 
%%% @end
%%% Created :  5 Feb 2014 by Mawuli Adzaku <mawuli@mhp>
%%%-------------------------------------------------------------------
-module(fakerl_company).
-define(LAST_NAMES, ["Mobile", "Oil", "Farms", "Friends"]).
-define(FIRST_NAMES, ["Merrill", "Goldman", "First", "Virgin"]).
-compile([export_all]).

firt_name() ->
    fakerl:random_element(?FIRST_NAMES).

last_name() ->
    fakerl:random_element(?LAST_NAMES).

company_name_formats() ->
    ["{{first_name}} and {{last_name}}", 
     "The {{first_name}} {{last_name}}", 
    "{{first_name}} {{last_name}} {{company_suffix}}", 
     "{{last_name}} and Associates"].

company_suffixes() ->
    ["Ltd", "Group", "LLC", "Holdings", "Partners", "Services"].

company_name() ->
    fakerl:random_element(company_name_formats()).
