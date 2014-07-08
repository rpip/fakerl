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
         bs/0]).

%% @doc Retruns a full company name
-spec name() -> string().
name() ->
    fakerl:parse("company.name").

%% @doc Returns a company name suffix
%% Examples: Ltd, Inc, Group
-spec suffix() -> string().
suffix() ->
    fakerl:fetch("company.suffix").

%% @doc Returns a string of buzzwords.
-spec buzzwords() -> string().
buzzwords() ->
    catch_phrase("buzzwords", ?CATCH_PHRASE_LENGTH).

%% @doc Returns a string totally nonsensical words.
-spec bs() -> string().
bs() ->
    catch_phrase("bs", ?CATCH_PHRASE_LENGTH).


%% @doc Returns a catch of length Length from the given section
%% under the company node of the locale file.
-spec catch_phrase(Section, Length) -> string when
      Section :: string(),
      Length  :: integer().
catch_phrase(Section, Length) ->
    MaxNum = Length * fakerl:random_number(),
    Words = [ 
              fakerl:fetch("company." ++ Section) 
              || _X <- lists:seq(1, MaxNum)
            ],
    Words1 = fakerl:shuffle(Words),
    Words2 = lists:sublist(Words1, Length),
    case string:join(lists:merge(Words2), " ") of
        [] ->
            catch_phrase(Section, Length);
        Phrase ->
            Phrase
    end.
