%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mhp>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Internet related data generator: usernames, email addresses etc
%%% @end
%%% @todo: ipv4, ipv6, slugs
%%% Created :  5 Feb 2014 by Mawuli Adzaku <mawuli@mhp>
%%%-------------------------------------------------------------------
-module(fakerl_internet).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-compile([export_all]).

safe_email_tlds() -> 
    ["org", "com", "net"].

free_email_domains() -> 
    ["gmail.com", "yahoo.com", "hotmail.com"].

tlds() -> 
    ["com", "com", "com", "com", "com", "com", "biz", "info", "net", "org"].

uri_pages() ->
  ["index", "home", "search", "main", "post", 
   "homepage", "category", "register", "login", 
   "faq", "about", "terms","privacy", "author"].

uri_paths() ->
  ["app", "main", "wp-content", "search", 
   "category", "tag", "categories", "tags", 
   "blog", "posts", "list", "explore"].

uri_extensions() ->
    [".html", ".html", ".html", ".htm", ".htm", ".php", ".php", ".jsp", ".asp"].

user_name_formats() -> 
  ["{{last_name}}.{{first_name}}",
  "{{first_name}}.{{last_name}}",
  "{{first_name}}##",
  "?{{last_name}}"].

email_formats() ->
  ["{{user_name}}@{{domain_name}}",
  "{{user_name}}@{{free_email_domain}}"].

url_formats() ->
  ["http://www.{{domain_name}}/", "http://{{domain_name}}/"].

uri_formats() ->
 ["{{url}}",
  "{{url}}{{uri_page}}/",
  "{{url}}{{uri_page}}{{uri_extension}}",
  "{{url}}{{uri_path}}/{{uri_page}}/",
  "{{url}}{{uri_path}}/{{uri_page}}{{uri_extension}}"].

email() ->
    Pattern = fakerl:random_element(email_formats()),
    fakerl:parse(Pattern).

safe_email() ->
   user_name() ++ "@example." ++ fakerl:random_element(safe_email_tlds()).

free_email() ->
    user_name() ++ "@" ++ free_email_domain().

company_email() ->
    user_name() ++ "@" ++ domain_name().

free_email_domain() ->
    fakerl:random_element(free_email_domains()).

user_name() ->
    Pattern = fakerl:random_element(user_name_formats()),
    Shuffled = fakerl:bothify(fakerl:parse(Pattern)),
    string:to_lower(Shuffled).

domain_name() ->
    domain_word() ++ "." ++ tld().

domain_word() ->
    fakerl:parse("{{company}}").

tld() ->
    fakerl:random_element(tlds()).

url() ->
    Pattern = fakerl:random_element(url_formats()),
    fakerl:parse(Pattern).


uri_page() ->
    fakerl:random_element(uri_pages()).

uri_path() ->
    Depth = fakerl:random(1,3),
    uri_path(Depth).

uri_path(Depth) ->
    L = [fakerl:random_element(uri_paths()) || _ <- lists:seq(0, Depth)],
    string:join(L, "/").

uri_extension() ->
    fakerl:random_element(uri_extensions()).

uri() ->
    Pattern = fakerl:random_element(uri_formats()),
    fakerl:parse(Pattern).

slug() ->
    undefined.

