%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").
-compile([export_all]).

name() ->
    undefined.

address() ->
    undefined.

text() ->
    undefined.

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

%% @doc Compiles the format template into a string.
-spec parse(Template, Module) -> string() when
      Template :: string(),
      Module :: atom().
parse([], _Template) ->
    {error, empty_string};
parse(Template, Module) ->
    case re:run(Template, ?VAR_REGEX, [global, {capture, all, list}]) of 
        nomatch ->
            {error, nomatch};
        {match, Matches} ->
            render(Matches, Template, Module)
    end.

%% @doc Replaces all regex matches with equivalent function calls
%% in the given module
-spec render(RegexMatches, Template, Module) -> RenderedTemplate when
      RegexMatches :: regex_match_list(),
      Template :: string(),
      Module :: atom(),
      RenderedTemplate :: list().
render([], Template, _Module) ->
    Bin = iolist_to_binary(Template),
    binary_to_list(Bin);
render([[Regex, Function]|Tail], Template, Module) ->
    Function1 = list_to_atom(Function),
    Value = Module:Function1(),
    Template1 = re:replace(Template, Regex, Value),
    render(Tail, Template1, Module).
