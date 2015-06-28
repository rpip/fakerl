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

-export([start/0,
         fetch/1,
         fetch/2,
         parse/1,
         parse/2,
         tokenize/1,
         format/2,
         numerify/1,
         letterify/1,
         bothify/1,
         shuffle/1,
         safe_var_idx_regex/1,
         locale_data_key/1,
         fetch_from_nodes/1,
         locale/0,
         locale/1,
         locale_file/1,
         config/1,
         config/2,
         get_locale_data/1]).


%%%-------------------------------------------------------------------
%%% Start fakerl
%%%-------------------------------------------------------------------
start() ->
    application:start(?MODULE).

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
    fakerl_company:name().

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
    Locale = locale(),
    Return = fetch(Key, Locale),
    case Return of
        {error, {notfound, _}} ->
            if
                Locale /= ?DEFAULT_LOCALE ->
                    %% Look for key in default locale.
                    %% Do this when key is missing in the chosen locale.
                   case fetch(Key, ?DEFAULT_LOCALE) of
                       {error, {notfound, _}} ->
                           error("Missing key (" ++ Key ++ ") in default locale config file: " ++ locale_file(Locale));
                       {ok, Value} ->
                           Value
                   end;
                true ->
                    error("Missing key (" ++ Key ++ ") in locale config file: " ++ locale_file(Locale))
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
    Data = get_locale_data(Locale),
    LocaleKey = atom_to_list(Locale) ++ "." ++ Key,
    case kvc:path(LocaleKey, Data) of
        [] ->
            {error, {notfound, "Missing key in locale config file: " ++ Key}};
        Values ->
            {ok, random(Values)}
    end.

%% @doc Returns the local data. Caches data if it's not already cached.
-spec get_locale_data(Locale :: atom()) -> Data :: list().
get_locale_data(Locale) ->
    case config(locale_data_key(Locale)) of
        {ok, Data} ->
            Data;
        _ ->
            [Data] = yamerl:decode_file(locale_file(Locale)),
            %% cache data
            config(locale_data_key(Locale), Data),
            Data
    end.
%% @doc Returns a key for setting/getting locale data
-spec locale_data_key(Locale :: atom()) -> list().
locale_data_key(Locale) ->
    list_to_atom(atom_to_list(Locale) ++ "_data").

%% @doc Returns path to locale data file
-spec locale_file(Locale :: atom()) -> Filepath :: list().
locale_file(Locale) when is_atom(Locale) ->
    locale_file(atom_to_list(Locale));
locale_file(Locale) ->
    filename:join([priv_dir(), "locales", Locale ++ ".yaml"]).

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
            end
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

%% @doc Breakup template into tokens and regex matches
-spec tokenize(Template::string()) -> nomatch | {match, Matches :: list()}.
tokenize(Template) ->
    re:run(Template, ?VAR_REGEX, [global, {capture, all, list}]).

%% @doc Compiles the template within the main fakerl module context
parse(TemplateNode) ->
    case tokenize(TemplateNode) of
        nomatch ->
            Template = fetch(TemplateNode),
            Tokens = string:tokens(TemplateNode, "."),
            Node = lists:nth(1, Tokens),
            parse(Template, Node);
        {match, RegexMatches} ->
            Match = lists:nth(1, RegexMatches),
            Key = lists:nth(2, Match),
            Node = lists:nth(1, string:tokens(Key, ".")),
            render(RegexMatches, TemplateNode, Node)
    end.

%% @doc Compiles the format template into a string.
-spec parse(Template, Node) -> {Status, string()} when
      Template :: string(),
      Node :: atom(),
      Status :: error | match | nomatch.
parse([], _Node) ->
    {error, empty_string};
parse(Template, Node) ->
    Tokens = tokenize(Template),
    parse_template(Tokens, Template, Node).
parse_template(nomatch, Template, _Node) ->
    bothify(Template);
parse_template({match, Matches}, Template, Node) ->
    Template1 = render(Matches, Template, Node),
    parse(Template1, Node).

%% @doc Replaces tokens ('{{ tokenName }}') with the result from the key values in the nodes
%% in the given module
-spec render(RegexMatches, Template, Node) -> RenderedTemplate when
      RegexMatches :: regex_match_list(),
      Template :: string(),
      Node :: atom() | string(),
      RenderedTemplate :: list().
render([], Template, Node) ->
    Template1 = iolist_to_binary(Template),
    Template2 = binary_to_list(Template1),
    parse(Template2, Node);
render(RegexMatches, Template, Node) when is_atom(Node) ->
    render(RegexMatches, Template, atom_to_list(Node));
render([[Regex, Key]|Tail], Template, Node) ->
    Value = try fetch(Key) of
                KeyValue ->
                    KeyValue
            catch error:_ ->
                    Key1 = Node ++ "." ++ Key,
                    try fetch(Key1) of
                        KV ->
                            KV
                    catch error:ErrMsg ->
                            case fetch_from_nodes(Key) of
                                {error, notfound} ->
                                    throw(ErrMsg);
                                Val ->
                                    Val
                            end
                    end
             end,
    Template1 = re:replace(Template, Regex, Value),
    render(Tail, Template1, Node).

%% Searches for key in all the parent nodes
-spec fetch_from_nodes(Key::string()) -> string() | {error, notfound}.
fetch_from_nodes(Key) ->
    Data = get_locale_data(locale()),
    LocaleKey = atom_to_list(locale()),
    Nodes = lists:map(fun({Name, _Fields}) -> Name end, kvc:path(LocaleKey, Data)),
    fetch_from_nodes(Key, Nodes).

fetch_from_nodes(_, []) ->
    {error, notfound};
fetch_from_nodes(Key, [Node|Rest]) ->
    NodeKey = Node ++ "." ++ Key,
    try fetch(NodeKey) of
        Value ->
            Value
    catch error:_ErrMsg ->
            fetch_from_nodes(Key, Rest)
    end.

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
    config(locale, Locale);
locale(_) ->
    error("Locale key/name must be an atom").
%% @doc Returns the locale
locale() ->
    case config(locale) of
        undefined ->
            ?DEFAULT_LOCALE;
        {ok, Locale} ->
            Locale
    end.

%% @doc Set config
-spec config(Key :: atom(), Value :: any()) -> any().
config(Key, Value) ->
    application:set_env(fakerl, Key, Value).

%% @doc Get config
-spec config(Key :: atom()) -> any().
config(Key) ->
    application:get_env(fakerl, Key).

%% @doc
-spec priv_dir() -> file:filename().
priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(Ebin), "priv");
        Dir ->
            Dir
    end.
