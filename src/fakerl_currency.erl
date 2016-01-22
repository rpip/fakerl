%%%-------------------------------------------------------------------
%%% @author Adam Kovari <kovariadam@gmail.com>
%%% @copyright (C) 2014, Adam Kovari
%%% @doc
%%% Fake name generator
%%% @end
%%% Created : 21 Jan 2016 by Adam Kovari <kovariadam@gmail.com>
%%%-------------------------------------------------------------------
-module(fakerl_currency).
-author("Adam Kovari <kovariadam@gmail.com>").
-include("fakerl.hrl").
-export([code/0]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Returns a random 'currency code'
-spec code() -> Code when
      Code :: list().
code() ->
    fakerl:fetch("currency.codes").
