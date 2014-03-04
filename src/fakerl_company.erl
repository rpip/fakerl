%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Company data generator 
%%% @end
%%% Created :  5 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_company).
-include("fakerl.hrl").
-export([name/0,
         suffix/0,
         buzzwords/0,
         bs/0
        ]).


name() ->
    fakerl:parse("company.name").

suffix() ->
    fakerl:fetch("company.suffix").

buzzwords() ->
    catch_phrase("buzzwords", ?CATCH_PHRASE_LENGTH).

bs() ->
    catch_phrase("bs", ?CATCH_PHRASE_LENGTH).

catch_phrase(Section, Length) ->
    Words = fakerl:fetch("company." ++ Section, all),
    Words1 = fakerl:shuffle(Words),
    Words2 = lists:sublist(Words1, Length),
    string:join(Words2, " ").
