%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 29 Jan 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% types
%%%-------------------------------------------------------------------
-type regex_variable_match() :: [string()].
-type regex_match_list() :: [regex_variable_match()].

%%%-------------------------------------------------------------------
%%% records
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% constants, miscellaneuos/helper fucntions
%%%-------------------------------------------------------------------
-define(VAR_REGEX, "{{([A-Za-z0-9_]+)}}").
-define(DEFAULT_MAX_TEXT_CHARS, 200).
-define(DEFAULT_NWORDS, 3).
-define(DEFAULT_SENTENCES_BLOCK_LENGTH, 3).
-define(DEFAULT_SENTENCE_NWORDS, 6).
-define(DEFAULT_SENTENCE_MAX_NWORDS, 20).
-define(DEFAULT_PARAGRAPH_LENGTH, 3).
-define(DEFAULT_PARAGRAPH_SENTENCE_LENGTH, 3).
-define(LOWER_BOUND, 0).
-define(COUNTRIES_JSON_FILE, code:priv_dir(fakerl) ++ "/countries.json"). 
-define(UPPER_BOUND, 10).
-define(FIRST_NAMES, ["John", "Jane" , "Bill", "Douglas", "Yaw", "Nancy"]).
-define(LAST_NAMES, ["Doe", "Cosby", "Rain", "Drew", "Asare"]).

%% Turns a record into a proplist
-define(R2P(Record,RecordType), lists:zip(record_info(fields, RecordType), tl(tuple_to_list(Record)))).

