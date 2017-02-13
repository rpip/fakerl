#
# credit https://github.com/chef/mini_s3
#

#
# Use rebar3 from either:
# - ./rebar3
# - rebar3 on the PATH (found via which)
# - Downloaded from $REBAR3_URL
#
.PHONY: all clean tests disclean tests rel travis update xref \
	shell install dialyzer rebar3

ifeq ($(ERL),)
	$(error "Erlang not available on this system")
endif

# If building on travis, use the rebar in the current directory
ifeq ($(TRAVIS),true)
	REBAR = $(CURDIR)/rebar
endif

ifeq ($(wildcard rebar3),rebar3)
  REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
	REBAR3 = rebar3
endif

REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

all: $(REBAR3)
	@$(REBAR3) do clean, compile, eunit

rel: all
	@$(REBAR3) release

tests:
	@$(REBAR3) eunit ct

shell:
	@$(REBAR3) as shell shell

dialyzer:
	@$(REBAR3) dialyzer

xref:
	@$(REBAR3) xref

update:
	@$(REBAR3) update

install: $(REBAR3) distclean update

distclean: clean
	@rm -rf _build

clean:
	@$(REBAR3) clean skip_deps=true

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod +x ./rebar3

travis: all
	@echo "Travis'd!"
