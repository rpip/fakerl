%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mhp>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2014 by Mawuli Adzaku <mawuli@mhp>
%%%-------------------------------------------------------------------
-module(fakerl_files_tests).
-include_lib("eunit/include/eunit.hrl").
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
fakerl_files_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (Data) ->
             [
              application_mime_types(Data),
              audio_mime_types(Data),
              image_mime_types(Data),
              message_mime_types(Data),
              model_mime_types(Data),
              multipart_mime_types(Data),
              text_mime_types(Data),
              video_mime_types(Data),
              mime_type(Data)
             ]
     end
    }.


%%%-------------------------------------------------------------------
%%% Setup / Cleanup
%%%-------------------------------------------------------------------
setup() ->
    application:start(fakerl),
    Data = fakerl:get_locale_data(en),
    kvc:path("en.files", Data).

teardown(_) ->
    ok.

%%%-------------------------------------------------------------------
%%% tests
%%%-------------------------------------------------------------------

application_mime_types(Data) ->
    MimeType = fakerl_files:application_mime_types(),
    Bool = lists:member(MimeType, kvc:path("application_mime_types", Data)),
    ?_assert(Bool).

video_mime_types(Data) ->
    MimeType = fakerl_files:video_mime_types(),
    Bool = lists:member(MimeType, kvc:path("video_mime_types", Data)),
    ?_assert(Bool).

audio_mime_types(Data) ->
    MimeType = fakerl_files:audio_mime_types(),
    Bool = lists:member(MimeType, kvc:path("audio_mime_types", Data)),
    ?_assert(Bool).

image_mime_types(Data) ->
    MimeType = fakerl_files:image_mime_types(),
    Bool = lists:member(MimeType, kvc:path("image_mime_types", Data)),
    ?_assert(Bool).

message_mime_types(Data) ->
    MimeType = fakerl_files:message_mime_types(),
    Bool = lists:member(MimeType, kvc:path("message_mime_types", Data)),
    ?_assert(Bool).

model_mime_types(Data) ->
    MimeType = fakerl_files:model_mime_types(),
    Bool = lists:member(MimeType, kvc:path("model_mime_types", Data)),
    ?_assert(Bool).

multipart_mime_types(Data) ->
    MimeType = fakerl_files:multipart_mime_types(),
    Bool = lists:member(MimeType, kvc:path("multipart_mime_types", Data)),
    ?_assert(Bool).

text_mime_types(Data) ->
    MimeType = fakerl_files:text_mime_types(),
    Bool = lists:member(MimeType, kvc:path("text_mime_types", Data)),
    ?_assert(Bool).

mime_type(Data) ->
    MimeType = fakerl_files:mime_type(),
    AllMimeTyes = [ MimeTypes || {_FileCategory, MimeTypes } <- Data ],
    Bool = lists:member(MimeType, lists:merge(AllMimeTyes)),
    ?_assert(Bool).

    
