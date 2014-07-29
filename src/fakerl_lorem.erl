%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Text data generator: Lorem Ipsum
%%% @end
%%% Created :  5 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_lorem).
-include("fakerl.hrl").

-export([word_list/0,
         word/0,
         words/1,
         sentence/0,
         sentence/1,
         sentences/0, 
         sentences/1,
         paragraph/0,
         paragraph/1,
         paragraphs/0,
         paragraphs/1,
         text/0,
         text/1]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
%% @doc Generate random word
%% :example "Lorem"
word() ->
    fakerl:random_element(word_list()).

%% @doc Generate an array of random words
%% :example array('Lorem', 'ipsum', 'dolor')
words() ->
    words(?DEFAULT_NWORDS).

words(NWords) ->
    [word() || _ <- lists:seq(0, NWords)].

%% @doc Generate a random sentence
%% :example 'Lorem ipsum dolor sit amet.'
sentence() ->
    sentence(?DEFAULT_SENTENCE_NWORDS).

sentence(NWords) when is_integer(NWords) ->
    L = words(NWords),
    string:join(L, " ");
sentence(true) ->
    L = words(fakerl:random(?DEFAULT_SENTENCE_NWORDS, ?DEFAULT_SENTENCE_MAX_NWORDS)),
    string:join(L, " ") ++ ".".

%% @doc Generate an array of sentences
%% :example array('Lorem ipsum dolor sit amet.', 'Consectetur adipisicing eli.')
sentences() ->
    sentences(?DEFAULT_SENTENCES_BLOCK_LENGTH).

sentences(BlockLength) ->
    [sentence() || _ <- lists:seq(0, BlockLength)].

%% @doc Generate a single paragraph
paragraph() ->
    paragraph(?DEFAULT_PARAGRAPH_SENTENCE_LENGTH).

paragraph(NSentences) ->
    L = sentences(NSentences),
    string:join(L, " ").

%% @doc Generate an array of paragraphs
%% :example [Paragraph1, Paragraph2, Paragraph3]
paragraphs() ->
    paragraphs(?DEFAULT_PARAGRAPH_LENGTH).

paragraphs(NumberOfParagraphs) ->
    [paragraph() || _ <- lists:seq(0, NumberOfParagraphs)].

%% @doc Generate a text string.
%% Depending on the number of characters, it returns a string made of words, sentences, or paragraphs.
%% Example: 'Sapiente sunt omnis. Ut pariatur ad autem ducimus et. Voluptas rem voluptas sint modi dolorem amet.'
text() ->
    text(?DEFAULT_MAX_TEXT_CHARS).

text(MaxTextChars) when is_integer(MaxTextChars) ->
    L = paragraphs(),
    Str = string:join(L, " "),
    StrLength = length(Str),
    if 
        StrLength > MaxTextChars ->
            string:sub_string(Str, MaxTextChars);
        true ->
            Str
    end.

%%%-------------------------------------------------------------------
%%% formats and helpers
%%%-------------------------------------------------------------------
word_list() ->
    ["alias", "consequatur", "aut", "perferendis", "sit", "voluptatem",
    "accusantium", "doloremque", "aperiam", "eaque", "ipsa", "quae", "ab",
    "illo", "inventore", "veritatis", "et", "quasi", "architecto",
    "beatae", "vitae", "dicta", "sunt", "explicabo", "aspernatur", "aut",
    "odit", "aut", "fugit", "sed", "quia", "consequuntur", "magni",
    "dolores", "eos", "qui", "ratione", "voluptatem", "sequi", "nesciunt",
    "neque", "dolorem", "ipsum", "quia", "dolor", "sit", "amet",
    "consectetur", "adipisci", "velit", "sed", "quia", "non", "numquam",
    "eius", "modi", "tempora", "incidunt", "ut", "labore", "et", "dolore",
    "magnam", "aliquam", "quaerat", "voluptatem", "ut", "enim", "ad",
    "minima", "veniam", "quis", "nostrum", "exercitationem", "ullam",
    "corporis", "nemo", "enim", "ipsam", "voluptatem", "quia", "voluptas",
    "sit", "suscipit", "laboriosam", "nisi", "ut", "aliquid", "ex", "ea",
    "commodi", "consequatur", "quis", "autem", "vel", "eum", "iure",
    "reprehenderit", "qui", "in", "ea", "voluptate", "velit", "esse",
    "quam", "nihil", "molestiae", "et", "iusto", "odio", "dignissimos",
    "ducimus", "qui", "blanditiis", "praesentium", "laudantium", "totam",
    "rem", "voluptatum", "deleniti", "atque", "corrupti", "quos",
    "dolores", "et", "quas", "molestias", "excepturi", "sint",
    "occaecati", "cupiditate", "non", "provident", "sed", "ut",
    "perspiciatis", "unde", "omnis", "iste", "natus", "error",
    "similique", "sunt", "in", "culpa", "qui", "officia", "deserunt",
    "mollitia", "animi", "id", "est", "laborum", "et", "dolorum", "fuga",
    "et", "harum", "quidem", "rerum", "facilis", "est", "et", "expedita",
    "distinctio", "nam", "libero", "tempore", "cum", "soluta", "nobis",
    "est", "eligendi", "optio", "cumque", "nihil", "impedit", "quo",
    "porro", "quisquam", "est", "qui", "minus", "id", "quod", "maxime",
    "placeat", "facere", "possimus", "omnis", "voluptas", "assumenda",
    "est", "omnis", "dolor", "repellendus", "temporibus", "autem",
    "quibusdam", "et", "aut", "consequatur", "vel", "illum", "qui",
    "dolorem", "eum", "fugiat", "quo", "voluptas", "nulla", "pariatur",
    "at", "vero", "eos", "et", "accusamus", "officiis", "debitis", "aut",
    "rerum", "necessitatibus", "saepe", "eveniet", "ut", "et",
    "voluptates", "repudiandae", "sint", "et", "molestiae", "non",
    "recusandae", "itaque", "earum", "rerum", "hic", "tenetur", "a",
    "sapiente", "delectus", "ut", "aut", "reiciendis", "voluptatibus",
    "maiores", "doloribus", "asperiores", "repellat"].

