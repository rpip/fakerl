APPNAME = fakerl
DIALYZER = dialyzer
REBAR = $(shell which rebar)
ERL = $(shell which erl)
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

.PHONY: all compile docs clean tests build-plt dialyze shell distclean pdf \
update-deps rebuild

# ================================================================
# Verify that porgrames needed by this Makefile are available
# ================================================================
ifeq ($(ERL),)
$(error "Erlang not running on this system")
endif

ifeq ($(REBAR),)
$(error "Rebar not running on this system")
endif


all: app

# ==============================================================
# Make build rules
# ==============================================================
app: deps
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

tests: clean app eunit ct

eunit: compile clean
	@$(REBAR) -C rebar.test.config eunit skip_deps=true

ct:
	@$(REBAR) -C rebar.test.config ct skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(APPNAME)_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .$(APPNAME)_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true

pdf:
	pandoc README -o README.pdf

shell: deps compile
	- @$(REBAR) skip-deps=true eunit
	@$(ERL) $(ERLFLAGS)
