APPNAME = fakerl
DIALYZER = dialyzer
ERL = $(shell which erl)
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin -s $(APPNAME)
REBAR = ./rebar $(REBAR_ARGS)
REBAR_URL := https://github.com/rebar/rebar/wiki/rebar

.PHONY: all compile docs clean tests build-plt dialyze shell distclean pdf \
update-deps rebuild


# ================================================================
# Verify that porgrames needed by this Makefile are available
# ================================================================
ifeq ($(ERL),)
$(error "Erlang not running on this system")
endif

# ==============================================================
# Make build rules
# ==============================================================
all: app

app: rebar deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(RENDER) compile

compile:
	$(REBAR) skip_deps=true compile

clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/ebin
	- rm -rf $(CURDIR)/*.dump
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rvf $(CURDIR)/deps
	- rm -rf $(CURDIR)/doc/*

tests: REBAR_ARGS = -C rebar.test.config
tests: clean app eunit ct

eunit: compile clean
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(APPNAME)_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .$(APPNAME)_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true

pdf:
	pandoc README.md -o README.pdf

shell: deps compile
	- @$(REBAR) skip-deps=true eunit
	@$(ERL) $(ERLFLAGS)

rebar:
	$(ERL) -noshell -s inets -s ssl \
	-eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
	-s init stop
	chmod +x ./rebar
