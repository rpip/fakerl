%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Internet related data generator: usernames, email addresses etc
%%% @end
%%% todo: ipv4, ipv6, slugs
%%% Created :  5 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------

-module(fakerl_internet).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("fakerl.hrl").

-export([email/0,
         safe_email/0,
         free_email/0,
         company_email/0,
         free_email_domain/0,
         domain_name/0,
         user_name/0,
         url/0,
         uri_path/0,
         uri_extension/0,
         uri_path/1,
         uri/0,
         tld/0,
         uri_page/0,
         slug/0]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
email() ->
    Pattern = fakerl:fetch("internet.email"),
    fakerl:parse(Pattern).

safe_email() ->
   user_name() ++ "@example." ++ fakerl:fetch("internet.safe_email_tlds").

free_email() ->
    user_name() ++ "@" ++ free_email_domain().

company_email() ->
    user_name() ++ "@" ++ domain_name().

free_email_domain() ->
    fakerl:fetch("internet.free_email_domains").

user_name() ->
    Pattern = fakerl:fetch("internet.user_name"),
    Shuffled = fakerl:parse(Pattern),
    string:to_lower(Shuffled).

domain_name() ->
    domain_word() ++ "." ++ tld().

domain_word() ->
    fakerl:parse("name.name").

tld() ->
    fakerl:fetch("internet.tlds").

url() ->
    Pattern = fakerl:fetch("internet.url"),
    fakerl:parse(Pattern).

uri_page() ->
    fakerl:fetch("internet.uri_pages").

uri_path() ->
    Depth = fakerl:random(1,3),
    uri_path(Depth).

uri_path(Depth) ->
    L = [fakerl:fetch("internet.uri_paths") || _ <- lists:seq(0, Depth)],
    string:join(L, "/").

uri_extension() ->
    fakerl:fetch("internet.uri_extensions").

uri() ->
    Pattern = fakerl:fetch("internet.uri"),
    fakerl:parse(Pattern).

slug() ->
    undefined.
