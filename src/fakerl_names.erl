%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Fake name generator
%%% @end
%%% Created : 31 Jan 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_names).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").
-export([name/0, first_name/0, last_name/0]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Returns a random name.
%% This name is a combination of a first name and a last name.
-spec name() -> Name when
      Name :: list().
name() ->
    fakerl:parse("{{name.name}}", ?MODULE).

%% @doc Returns a random 'first name'
-spec first_name() -> Name when
      Name :: list().
first_name() ->
    fakerl:fetch("name.first_name").

%% @doc Returns a random 'surname', also known as 'last name'.
-spec last_name() -> Name when
      Name :: list().
last_name() ->
    fakerl:fetch("name.last_name").
