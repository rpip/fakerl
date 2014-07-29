%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% User agent generator
%%% @end
%%% Created :  8 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------

-module(fakerl_user_agent).

-export([mac_processor/0,
         linux_processor/0,
         user_agent/0,
         safari/0,
         internet_explorer/0,
         firefox/0,
         opera/0,
         chrome/0]).

mac_processor() ->
    fakerl:fetch("useragent.mac_processors").

linux_processor() ->
    fakerl:fetch("useragent.linux_processors").

user_agent() ->
    fakerl:fetch("user_agents.user_agents").

chrome() ->
        Saf = fakerl:random(531, 536) + fakerl:random(0, 2),
        Platforms = [{
                      "({0}) AppleWebKit/{1} (KHTML, like Gecko) Chrome/{2}.0.{3}.0 Safari/{4}",
                      [linux_platform_token(), Saf, fakerl:random(13, 15), fakerl:random(800, 899), Saf]
                     },
                     {
                       "({0}) AppleWebKit/{1} (KHTML, like Gecko) Chrome/{2}.0.{3}.0 Safari/{4}",
                       [windows_platform_token(), Saf, fakerl:random(13, 15), fakerl:random(800, 899), Saf]
                     },
                     {
                       "({0}) AppleWebKit/{1} (KHTML, like Gecko) Chrome/{2}.0.{3}.0 Safari/{4}",
                       [mac_platform_token(), Saf, fakerl:random(13, 15), fakerl:random(800, 899), Saf]
                     }],
        {Template, Ctx} = fakerl:random(Platforms),
        Platform = fakerl:format(Template, Ctx),
        "Mozilla/5.0" ++ Platform.

firefox() ->
    Ver = [
           {
            "Gecko/{0} Firefox/{1}.0",
            [fakerl_datetime:date_time("d-m-Y"), fakerl:random(4, 15)]
           },
           {"Gecko/{0} Firefox/3.6.{1}",
            [fakerl_datetime:date_time("d-m-Y"), fakerl:random(1, 20)]
           },
           {"Gecko/{0} Firefox/3.8", 
            [fakerl_datetime:date_time("d-m-Y")]
           }
          ],
    Platforms = [
                 {
                  "({0}; {1}; rv:1.9.{2}.20) {3}",
                  [windows_platform_token(), lang(), fakerl:random(0, 2)]
                 },
                 {
                   "({0}; rv:1.9.{1}.20) {2}",
                   [linux_platform_token(), fakerl:random(5, 7)]
                 },
                 {
                   "({0}; rv:1.9.{1}.20) {2}", 
                   [mac_platform_token(), fakerl:random(2, 6)]
                 }
                ],
    {VerTpl, VerCtx} = fakerl:random(Ver),
    Version = fakerl:format(VerTpl, VerCtx), 
    {Template, Ctx} = fakerl:random(Platforms),
    Platform = fakerl:format(Template, Ctx ++ [Version]),
    "Mozilla/5.0 " ++ Platform.

lang() ->
    fakerl:fetch("user_agents.langs").

safari() ->
    Saf = fakerl:format("{0}.{1}.{2}", 
                        [fakerl:random(531, 535), fakerl:random(1, 50), fakerl:random(1, 7)]),
    N = fakerl:random(0, 1),
    if 
        N == 0 ->
            Ver = fakerl:format("{0}.{1}", [fakerl:random(4, 5), fakerl:random(0, 1)]);
        true ->
            Ver = fakerl:format("{0}.0.{1}", [fakerl:random(4, 5), fakerl:format(1, 5)])
    end,

    Platforms = [
                 {"(Windows; U; {0}) AppleWebKit/{1} (KHTML, like Gecko) Version/{2} Safari/{3}",
                  [windows_platform_token(), Saf, Ver, Saf]
                  },
                 {
                   "({0} rv:{1}.0; {2}) AppleWebKit/{3} (KHTML, like Gecko) Version/{4} Safari/{5}",
                   [mac_platform_token(), fakerl:random(2, 6), lang(), Saf, Ver, Saf]
                 },
                 {
                   "(iPod; U; CPU iPhone OS {0}_{1} like Mac OS X; {2}) AppleWebKit/{3} (KHTML, like Gecko) Version/{4}.0.5 Mobile/8B{5} Safari/6{6}",
                   [fakerl:random(3, 4), fakerl:random(0, 3), lang(), 
                    Saf, fakerl:random(3, 4),fakerl:random(111, 119), Saf]
                 }
                ],
    {Template, Ctx} = fakerl:random(Platforms),
    Platform = fakerl:format(Template, Ctx),
    "Mozilla/5.0 " ++ Platform.

opera() ->
    Platforms = [{
                  "({0}; {1}) Presto/2.9.{2} Version/{3}.00", 
                   [linux_platform_token(), lang(), fakerl:random(160, 190), fakerl:random(10, 12)]
                 },
                 {
                   "({0}; {1}) Presto/2.9.{2} Version/{3}.00",
                   [windows_platform_token(), lang(), fakerl:random(160, 190), fakerl:random(10, 12)]
                 }
                ],
    {PlatformTpl, PlatformCtx} = fakerl:random(Platforms),
    Platform = fakerl:format(PlatformTpl, PlatformCtx),
    fakerl:random("Opera/{0}.{1}.{2}",
                  [fakerl:random(8, 9), fakerl:random(10, 99), Platform]).

internet_explorer() ->
    fakerl:format("Mozilla/5.0 (compatible; MSIE {0}.0; {1}; Trident/{2}.{3})",
                  [
                   fakerl:random(5, 9),
                   windows_platform_token(),
                   fakerl:random(3, 5),
                   fakerl:random(0, 1)
                  ]
                 ).

windows_platform_token() ->
    fakerl:fetch("user_agents.windows_platform_tokens").

linux_platform_token() ->
    fakerl:format("X11; Linux {0}", linux_processor()).

mac_platform_token() ->
    fakerl:format("Macintosh; {0} Mac OS X 10_{1}_{2}",
                  mac_processor(), fakerl:random(5, 8), fakerl:random(0, 9)).
