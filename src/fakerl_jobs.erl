%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Jobs generator
%%% @end
%%% Created :  8 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_jobs).
-export([job/0]).


%% @doc Returns a job title
-spec job() -> string().
job() ->
    fakerl:fetch("jobs.jobs").
