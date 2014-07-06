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
-record(credit_card, {name, prefixes, length, security_code='CVC', security_code_length}).

%%%-------------------------------------------------------------------
%%% constants, miscellaneuos/helper fucntions
%%%-------------------------------------------------------------------
%% regex for {{variable_name}}
-define(VAR_REGEX, "{{([A-Za-z0-9_\.]+)}}").

%% regex for format strings: {0}, {1}
-define(VAR_INDEX_REGEX, "{([0-9]+)}").

-define(DEFAULT_MAX_TEXT_CHARS, 200).
-define(DEFAULT_NWORDS, 3).
-define(DEFAULT_SENTENCES_BLOCK_LENGTH, 3).
-define(DEFAULT_SENTENCE_NWORDS, 6).
-define(DEFAULT_SENTENCE_MAX_NWORDS, 20).
-define(DEFAULT_PARAGRAPH_LENGTH, 3).
-define(DEFAULT_PARAGRAPH_SENTENCE_LENGTH, 3).
-define(LOWER_BOUND, 0).
-define(COUNTRIES_JSON_FILE, filename:join([code:priv_dir(fakerl), "countries.json"])). 
-define(UPPER_BOUND, 10).
-define(ValidCreditCardCharactersPattern, '^[0-9 ]*$').
-define(CATCH_PHRASE_LENGTH, 10).

%% locales
-define(DEFAULT_LOCALE, en).
-define(LOCALES_DIR, filename:join([code:priv_dir(fakerl), "locales"])).

%% file category
-define(FILE_CATEGORIES, [application,
                          audio,
                          image,
                          message,
                          model,
                          multipart,
                          text,
                          video]).

%% credit card prefixes
-define(VisaPrefixList,
        [
         [4, 5, 3, 9],
         [4, 5, 5, 6],
         [4, 9, 1, 6],
         [4, 5, 3, 2],
         [4, 9, 2, 9],
         [4, 0, 2, 4, 0, 0, 7, 1],
         [4, 4, 8, 6],
         [4, 7, 1, 6],
         [4]
        ]
       ).

-define(MastercardPrefixList,
        [
         [5, 1],
         [5, 2],
         [5, 3],
         [5, 4],
         [5, 5]
        ]
       ).

-define(AmexPrefixList,
        [
         [3, 4],
         [3, 7]
        ]
       ).

-define(DiscoverPrefixList,
        [
         [6, 0, 1, 1]
        ]
       ).

-define(DinersPrefixList, 
        [
         [3, 0, 0],
         [3, 0, 1],
         [3, 0, 2],
         [3, 0, 3],
         [3, 6],
         [3, 8]
        ]
       ).

-define(EnroutePrefixList,
        [
         [2, 0, 1, 4],
         [2, 1, 4, 9]
        ]
       ).

-define(Jcb16PrefixList,
        [
         [3, 0, 8, 8],
         [3, 0, 9, 6],
         [3, 1, 1, 2],
         [3, 1, 5, 8],
         [3, 3, 3, 7],
         [3, 5, 2, 8]
        ]
       ).

-define(Jcb15PrefixList, 
        [
         [2, 1, 0, 0],
         [1, 8, 0, 0]
        ]
        ).

-define(VoyagerPrefixList, 
        [
         [8,6,9,9]
        ]
       ).

%% Turns a record into a proplist
-define(R2P(Record,RecordType), lists:zip(record_info(fields, RecordType), tl(tuple_to_list(Record)))).
