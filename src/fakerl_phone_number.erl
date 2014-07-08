%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Phone number generator
%%% @end
%%% Created :  8 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_phone_number).

-export([phone_number/0]).

%% @doc Returns a radomly generated phone number
%% By default, the generated phone number is a US phone number format
-spec phone_number() -> string().
phone_number() ->
    fakerl:fetch("phone_number.number").
