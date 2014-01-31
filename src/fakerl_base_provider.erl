%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Base module for all the fake data providers.
%%% @end
%%% Created : 30 Jan 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_base_provider).
-include("fakerl.hrl").
-compile([export_all]).

%% @doc Returns a random number betweeen 0 and 10
-spec random_number() -> integer().
random_number() ->
    random(0,10).

%% @doc Returns a random alphabetic character
-spec random_letter() -> char().
random_letter() ->
    Int = random($a, $z),
    [Int].

%% @doc Returns a random uppercase alphabetic character
-spec random_letter(caps) -> char().
random_letter(caps) ->
    Int = random($A, $Z),
    [Int].

%% @doc Returns a randomly selected element from the list provided.
-spec random_element(list()) -> integer().
random_element([X|[]]) ->
    X;
random_element(Xs) ->
    N = random(1,length(Xs)),
    lists:nth(N, Xs).

%% @doc Returns a randomly selected number between From, and To.
%% Uses the default macros: LOWER_BOUND, and UPPER_BOUND for the test data generated range
-spec random(From, To) -> RandomElement when
      From :: integer(),
      To :: integer(),
      RandomElement :: any().     
random(From, To) ->
    random(From, To, ?LOWER_BOUND, ?UPPER_BOUND).

%% @doc Returns a randomly selected number between From, and To.
%% The LowerBound and UpperBound represent the range of the generated test data.
-spec random(From, To, LowerBound, UpperBound) -> RandomElement when
      From :: integer(),
      To :: integer(),
      LowerBound :: integer(),
      UpperBound :: integer(),
      RandomElement :: any().     
random(From, To, LowerBound, UpperBound) ->
    L = [crypto:rand_uniform(From, To) || _ <- lists:seq(LowerBound, UpperBound)],
    lists:nth(1, L).

