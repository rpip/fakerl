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

%% Interface to common functions
-export([first_name/0,
         last_name/0, 
         user_name/0,
         name/0, 
         address/0, 
         text/0, 
         company/0,
         country/0]).

%% API
-export([random_number/0,
         random_number/1,
         random_letter/1,
         random_letter/0,
         random_element/1,
         random/1,
         random/2]).

-export([fetch/1,
         fetch/2,
         parse/1, 
         parse/2,
         format/2,
         numerify/1,
         letterify/1,
         bothify/1,
         shuffle/1,
         locale/0,
         locale/1]).


%%%-------------------------------------------------------------------
%%% Interface to commonly used fake data generators
%%%-------------------------------------------------------------------
first_name() ->
    fakerl_names:first_name().

last_name() ->
    fakerl_names:last_name().

user_name() ->
    fakerl_internet:user_name().

name() ->
    fakerl_names:name().

address() ->
    fakerl_address:address().

text() ->
    fakerl_lorem:text().

company() ->
    fakerl_company:company_name().

country() ->
    fakerl_datetime:country().

%%%-------------------------------------------------------------------
%%% core/shared logic
%%%-------------------------------------------------------------------

%% @doc Returns a random number betweeen 0 and 10
-spec random_number() -> integer().
random_number() ->
    random(0,10).

random_number(Length) ->
    random_number(Length, []).
random_number(0, Acc) ->
    lists:flatten(Acc);
random_number(Length, Acc) ->
    N = random_number(),
    random_number(Length - 1, [Acc|[N]]).


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
random_element([]) ->
    error(empty_list);
random_element([X|[]]) ->
    X;
random_element(Xs) ->
    N = random(1,length(Xs)),
    lists:nth(N, Xs).

random(L) when is_list(L) ->
    random_element(L).

%% @doc Returns a randomly selected number between From, and To.
-spec random(From, To) -> RandomElement when
      From :: integer(),
      To :: integer(),
      RandomElement :: any().     
random(From, To) ->
   crypto:rand_uniform(From, To).

%% @doc Returns key value in locale config
fetch(Key) ->
    Locale = locale()
    Return = fetch(Key, Locale),
    case Return of
        {error, {notfound, _}} ->
            if 
                Locale /= ?DEFAULT_LOCALE ->
                    %% Look for key in default locale.
                    %% Do this when key is missing in the chosen locale.
                   case fetch(Key, ?DEFAULT_LOCALE) of
                       {error, {notfound, _}} ->
                           error("Missing key (" Key ++ ") in default locale config file: " ++ locale_file(Locale));
                       {ok, Value} ->
                           Value
                   end;
                true ->
                    error("Missing key (" Key ++ ") in locale config file: " ++ locale_file(Locale))
            end;
         {ok, Value} ->
            Value
     end.

%% @doc Fetches key from local yaml config file
-spec fetch(Key, Locale) -> {ok, Value} | {error, {notfound, ErrMsg}} when
      Key :: string(),
      Locale :: string(),
      Value :: string(),
      ErrMsg :: string().
fetch(Key, Locale) ->
    [Config] = yamerl:decode_file(locale_file(Locale)),
    LocaleKey = Locale ++ "." ++ Key,
    case kvc:path(LocaleKey, Config) of
        [] ->
            {error, {notfound, "Missing key in locale config file: " ++ Key}};
        Values ->
            {ok, random(Values)}
    end.

locale_file(Locale) when is_atom(Locale) ->
    atom_to_list(Locale);
locale_file(Locale) ->
    ?LOCALES_DIR ++ Locale ++ ".yml".

format([], _Ctx) ->
    {error, empty_string};
format(Template, Ctx) ->
    case re:run(Template, ?VAR_INDEX_REGEX, [global, {capture, all, list}]) of 
        nomatch ->
            {error, nomatch};
        {match, Matches} ->
            RenderedTemplate = render_format_string(Matches, Template, Ctx),
            if 
                length(Matches) < length(Ctx) ->
                    error("Template index out of range");
                true ->
                    bothify(RenderedTemplate)
            end,            
    end.
render_format_string([], Template, _Ctx) ->
    Bin = iolist_to_binary(Template),
    binary_to_list(Bin);
render_format_string([[_, Index]|Tail], Template, Ctx) ->
    VarIdx = list_to_integer(Index),
    Value =  lists:nth(VarIdx, Ctx),
    Regex = safe_var_idx_regex(Index),
    Template1 = re:replace(Template, Regex, Value),
    render(Tail, Template1, Ctx).

%% @doc Retrns a safe regex for integer based templates indexes for use in replacements
%% example: "0" | 0 -> "{(0)}"
safe_var_idx_regex(Index) when is_integer(Index) ->
    safe_var_idx_regex(integer_to_list(Index));
safe_var_idx_regex(Index) ->
    lists:flatten(io_lib:format("{(~s)}", [Index])).

%% @doc Compiles the template within the main fakerl module context
parse(Template) ->
    parse(Template, ?MODULE).
%% @doc Compiles the format template into a string.
-spec parse(Template, Module) -> {Status, string()} when
      Template :: string(),
      Module :: atom(),
      Status :: error | match | nomatch.
parse([], _Module) ->
    {error, empty_string};
parse(Template, Module) ->
    case re:run(Template, ?VAR_REGEX, [global, {capture, all, list}]) of 
        nomatch ->
            {error, nomatch};
        {match, Matches} ->
            RenderedTemplate = render(Matches, Template, Module),
            bothify(RenderedTemplate)
    end.

%% @doc Replaces tokens ('{{ tokenName }}') with the result from the token method call
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
    Value = try Module:Function1() of 
               Result -> Result
            catch
                error:undef -> 
                    ?MODULE:Function1()
            end,
    Template1 = re:replace(Template, Regex, Value),
    render(Tail, Template1, Module).

%% @doc Converts a string of N "#" into N integers
-spec numerify(String :: string()) -> [integer()].
numerify(String) ->
    numerify(String, []).
numerify([], Acc) ->
    lists:flatten(Acc);
numerify([X|Xs], Acc) ->
    if 
        X =:= $# ->
            N = fakerl:random_number(),
            NStr = erlang:integer_to_list(N),
            numerify(Xs, [Acc|[NStr]]);
        true ->
            numerify(Xs, [Acc|[X]])
    end.

%% @doc Replaces all question mark ('?') occurrences with a random letter
letterify(String) ->
    letterify(String, []).
letterify([], Acc) ->
    lists:flatten(Acc);
letterify([X|Xs], Acc) ->
    if 
        X =:= $? ->
            Letter = fakerl:random_letter(),
            letterify(Xs, [Acc|[Letter]]);
        true ->
            letterify(Xs, [Acc|[X]])
    end.

%% @doc Replaces all '#' and '?' with numbers and letters respectively
-spec bothify(Text :: string()) -> string().
bothify(Text) ->
    T = numerify(Text),
    letterify(T).

%% @doc Return new list by shuffling the members of list
-spec shuffle(list()) -> list().
shuffle(L) when is_list(L) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])].

%% @doc Sets the locale
locale(Locale) when is_atom(Locale) ->
    locale(atom_to_list(Locale));
locale(Locale) ->
    application:set_env(fakerl, locale, Locale).

%% @doc Returns the locale
locale() ->
    case application:get_env(fakerl, locale) of
        undefined ->
            ?DEFAULT_LOCALE;
        Locale ->
            Locale
    end.
