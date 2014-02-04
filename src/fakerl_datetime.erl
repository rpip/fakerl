%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Date and time fake data generator
%%% @end
%%% Created :  3 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_datetime).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").
-compile([export_all]).

-spec countries() -> binary() | {error, Reason :: string()}.
countries() ->
    case file:read_file(?COUNTRIES_JSON_FILE) of
        {ok, Bin} ->
          jsx:decode(Bin);
        {error, enoent} ->
            error("Missing countries json file: " ++ ?COUNTRIES_JSON_FILE)
    end.
                         
