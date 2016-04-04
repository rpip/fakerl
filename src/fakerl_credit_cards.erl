%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2014, Mawuli Adzaku
%%% @doc
%%% Credit cards generator; card numbers, security codes, card owner etc
%%% @end
%%% Created :  8 Feb 2014 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(fakerl_credit_cards).
-include("fakerl.hrl").
-export([credit_card_types/0,
         random_card_type/0,
         credit_card_type/0,
         credit_card_type/1,
         credit_card_provider/0,
         credit_card_number/0,
         credit_card_number/1,
         credit_card_security_code/0,
         credit_card_security_code/1,
         credit_card_full/0,
         credit_card_full/1
        ]).

credit_card_types() ->
    [
     {mastercard, #credit_card{name="Mastercard", prefixes=?MastercardPrefixList, length=16, security_code='CVV', security_code_length=4}},
     {visa16, #credit_card{name="VISA 16 digit", prefixes=?VisaPrefixList}},
     {visa, #credit_card{name="VISA 16 digit", prefixes=?VisaPrefixList}},
     {visa13, #credit_card{name="VISA 13 digit", prefixes=?VisaPrefixList, security_code_length=13}},
     {amex, #credit_card{name="American Express", prefixes=?AmexPrefixList, security_code_length=15}},
     {discover, #credit_card{name="Discover", prefixes=?DiscoverPrefixList}},
     {diners, #credit_card{name="Diners Club / Carte Blanche", prefixes=?DinersPrefixList, length=14}},
     {enroute, #credit_card{name="enRoute", prefixes=?EnroutePrefixList, length=15}},
     {jcb15, #credit_card{name="JCB 15 digit", prefixes=?Jcb15PrefixList, length=15}},
     {jcb16, #credit_card{name="JCB 16 digit", prefixes=?Jcb16PrefixList, length=16}},
     {jcb, #credit_card{name="JCB 16 digit", prefixes=?Jcb16PrefixList, length=16}},
     {voyager, #credit_card{name="Voyager", prefixes=?VoyagerPrefixList, length=15}}
    ].
      

random_card_type() ->
    fakerl:random_element(proplists:get_keys(credit_card_types())).

credit_card_provider() ->
    CardType = random_card_type(),
    credit_card_provider(CardType).

credit_card_type() ->
    credit_card_type(random_card_type()).

credit_card_type(CardType) ->
    proplists:get_value(CardType, credit_card_types()).

credit_card_provider(CardType) when is_atom(CardType) ->
    Card = credit_card_type(CardType),
    Card#credit_card.name.

credit_card_number() ->
    credit_card_number(undefined).

credit_card_number(undefined) ->
    Type = random_card_type(),
    credit_card_number(Type);
credit_card_number(CardType) ->
    Card = credit_card_type(CardType),
    Prefix = fakerl:random_element(Card#credit_card.prefixes),
    Number = generate_number(Prefix, Card#credit_card.length),
    Number.

%% @doc Generate credit card number
%%  'prefix' is the start of the CC number, any number of digits.
%%  'length' is the length of the CC number to generate. Typically 13 or 16
generate_number(Prefix, Length) ->
    CardNumber = [Prefix],
    Number = fakerl:random_number(),
    do_generate_number(Length - 1, [CardNumber | [Number]]).

do_generate_number(0, Acc) ->
    lists:flatten(Acc);
do_generate_number(Length, Acc) ->
    N = fakerl:random_number(),
    do_generate_number(Length, [Acc|[N]]).

credit_card_expire() ->
    credit_card_expire(now, "+10y", "%m%y").

credit_card_expire(Start, End, DateFormat) ->
    ExpireDate = fakerl_datetime:date_time_between(Start, End),
    qdate:to_string(ExpireDate, DateFormat).

credit_card_full() ->
    Type = random_card_type(),
    credit_card_full(Type).

credit_card_full(CardType) ->
    Card = credit_card_type(CardType),
    Template = "{{provider}} {{owner}} {{number}}  {{expire_date}}
                  {{security}}: {{security_nb}}",
     Provider = Card#credit_card.name,
     Owner = fakerl:parse("{{name.first_name}} {{name.last_name}}"),
     Number = credit_card_number(Card),
     ExpireDate = credit_card_expire(),
     Security = Card#credit_card.security_code,
     SecurityNB  = credit_card_security_code(card),
     FullCard = fakerl:parse(Template, [{provider, Provider},
                                        {owner, Owner},
                                        {number, Number},
                                        {expire_date, ExpireDate},
                                        {security, Security},
                                        {security_code, SecurityNB}
                                       ]),
    FullCard.

credit_card_security_code() ->
    Type = random_card_type(),
    credit_card_security_code(Type).

credit_card_security_code(CardType) ->
    Card = credit_card_type(CardType),
    fakerl:random_number(Card#credit_card.security_code_length).
