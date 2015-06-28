     _______    ___       __  ___  _______ .______       __
    |   ____|  /   \     |  |/  / |   ____||   _  \     |  |
    |  |__    /  ^  \    |  '  /  |  |__   |  |_)  |    |  |
    |   __|  /  /_\  \   |    <   |   __|  |      /     |  |
    |  |    /  _____  \  |  .  \  |  |____ |  |\  \----.|  `----.
    |__|   /__/     \__\ |__|\__\ |_______|| _| `._____||_______|


*Fakerl* is an Erlang application that generates fake data for you.
Whether you need to bootstrap your database, create good-looking XML documents,
fill-in your persistence to stress test it, or anonymize data taken from a production service, Faker is for you.

Fakerl is inspired by Python's [Faker][python-faker] and Ruby's [Faker][ruby-faker].

[![Build Status](https://travis-ci.org/mawuli/fakerl.svg?branch=master)](https://travis-ci.org/mawuli/fakerl)

*work in progress*

## How it works

At the core of fakerl is a simple template parser that maps template variables and contexts to key-value nodes in a YAML config file.
This YAML file acts as a locale-specific translation of the generated texts/data.
By default, the locale is set to *"en"*, and it is available in **priv/locales/en.yaml**

To add a locale specific data, just add a new locale file at **priv/locales/your_new_locale_name.yaml**.

Fakerl also ships with Erlang modules for easily generating subject-specific fake data.


#### Example:

```erlang
Hi {{name.first_name}}!

My name is {{name.name}}.
I am a ## year old student from {{address.country}}.
My email address is {{address.email_address}}, and I look forward to our meeting tomorrow at {{address.address}}.

Best regards,
{{name.name}}
```

#### Output:

```text
Hi Douglas Rain!

My name is Yaw Doe.
I am a 23 year old student from Singapore.
My email address is bill.rain@example.net.gh, and I look forward to our meeting tomorrow at 22 Cafe Street, Soma.

Best regards,
Sarah Cosby
```

At present, new data is generated when a template variable is parsed.
There are plans to support optional one-off persistence so that the same variable
rendered multiple times in a template string will yield the same output.

#### More Examples
Here's an example of using Fakerl in the Erlang shell:

```erlang
Erlang R16B (erts-5.10.1) [source] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> fakerl:fetch("jobs.jobs").
Art gallery manager
2> fakerl:parse("name.name").
"Dawson Pollich"
3> fakerl:fetch("internet.tlds")
"biz"
4> Tpl="My name is {{name.name}} and I am ## yearls old. I live in {{address.city}}. I am also on Twitter(@???????)".
5> fakerl:parse(Tpl).
"My name is Elliot Thompson and I am 75 yearls old. I live in Maye berg. I am also on Twitter(@whooxja)"
```

## Generators / Providers

The following fake data providers are available:

* Names
* Person
* Datetime
* Addresses
* Profile
* Company
* Lorem
* Internet
* Credit cards
* User Agents
* Jobs
* Phone numbers
* Files

## Build

Fakerl requires make and rebar to build.

To build, go to the Fakerl directory and simply type:

```bash
make && make tests
```

To run fakerl in an Erlang shell, simple type:

```erlang
make shell
```

## Documentation

```erlang
make docs
```

## Tests

```erlang
make tests
```

## TODO

see the bundled TODO file

## License

Fakerl is released under the MIT Licence. See the bundled LICENSE file for details.


Contributing
-------------
Issues, forks, and pull requests are welcome!


Credits
--------

- [joke2k][joke2k] / [Faker][python-faker]
- [stympy][stympy] / [Faker][ruby-faker]

[python-faker]: https://github.com/joke2k/Faker "Python faker"
[ruby-faker]: https://github.com/stympy/faker "Ruby faker"
[joke2k]: https://github.com/joke2k "joke2k"
[stympy]: https://github.com/stympy "Benjamin Curtis"
