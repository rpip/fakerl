%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Files generator; metadata etc
%%% @end
%%% Created :  8 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_files).
-include("include/fakerl.hrl").

- export([application_mime_types/0,
          audio_mime_types/0,
          image_mime_types/0,
          message_mime_types/0,
          model_mime_types/0,
          multipart_mime_types/0,
          text_mime_types/0,
          video_mime_types/0,
          mime_type/0
         ]).

application_mime_types() ->
    fakerl:fetch("files.application_mime_types").

audio_mime_types() ->
    fakerl:fetch("files.audio_mime_types").

image_mime_types() ->
    fakerl:fetch("files.image_mime_types").

message_mime_types() ->
    fakerl:fetch("files.message_mime_types").

model_mime_types() ->
    fakerl:fetch("files.model_mime_types").
    
multipart_mime_types() ->
    fakerl:fetch("files.multipart_mime_types").

text_mime_types() ->
    fakerl:fetch("files.text_mime_types").

video_mime_types() ->
    fakerl:fetch("files.video_mime_types").

mime_types() ->   
    [
     {application, application_mime_types()},
     {audio, audio_mime_types()},
     {image, image_mime_types()},
     {message, message_mime_types()},
     {model, model_mime_types()},
     {multipart,  multipart_mime_types()},
     {text, text_mime_types()},
     {video, video_mime_types()}
    ].

%% @doc Returns a random file mime type
mime_type() ->
    Category = random_mime_category(),
    mime_type(Category).

%% @doc Returns file mime type from the given category
%% category: application|audio|image|message|model|multipart|text|video
mime_type(Category) when is_atom(Category) ->    
    case lists:member(Category, ?FILE_CATEGORIES) of
        true ->
            proplist:get_value(Category, mime_types());
        false ->
            {error, {invalid_category, Category}}
    end.
    
random_mime_category() ->
    fakerl:random(?FILE_CATEGORIES).
