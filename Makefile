OPTS         ?=
ERL          ?= $(shell which erl || echo "'erl' not found, please install or use ERL=... env variable when running Make")
PROJECT      ?= $(shell basename `pwd` | sed -e 's|\-.*||')
REBAR        ?= rebar3
REBAR_CONFIG := rebar.config

.PHONY: all
all: compile

.PHONY: compile
compile:
	$(REBAR) compile

# Compiles and opens a pretty shell with all module search paths
.PHONY: shell
shell:
	rebar3 shell

# Runs a XREF check on sources
.PHONY: xref
xref: all
	$(REBAR) xref

.PHONY: dialyzer
dialyzer:
	$(REBAR) dialyzer

.PHONY: clean
clean:
	rm -f erl_crash.dump; rebar3 clean

# TODO REVIEW THIS, outdated paths and rebar2 is used here
.PHONY: distclean
distclean: clean
	@if test -f $(REBAR); then $(REBAR) -C $(REBAR_CONFIG) delete-deps; rm -f $(REBAR); else break; fi
	@rm -f $(PWD)/src/content/copy_samples/*.md.erlang
	@rm -rf deps $(PWD)/ebin $(PWD)/priv
	@rm -rf .rebar $(PWD)/deps/*/.rebar $(REBAR)
	@cd $(PWD)/src/content/cmark_parse/cmark; make $@

.PHONY: help
help:
	@echo "$(MAKE) all           : (re)build the project"
	@echo "$(MAKE) clean         : clean-up object files"
	@echo "$(MAKE) shell [OPTS=] : start an Erlang shell"
	@echo "$(MAKE) xref          : xref check"
