# -*- MakeFile -*-

# ----------------------------------------------------
# SETTINGS
# ----------------------------------------------------
OPTS         ?=
ERL          ?=$(shell which erl || echo no)
EMAKE        ?=$(shell basename $(MAKE))
CMAKE        ?=$(shell which cmake || echo no)
PROJECT      ?=$(shell basename `pwd` | sed -e 's|\-.*||')
REBAR        ?=./rebar
REBAR_CONFIG :=rebar.config

# ----------------------------------------------------
# PREREQUISITES
# ----------------------------------------------------
ifeq ($(ERL),no)
$(error "Erlang is not available on this system")
endif

ifeq ($(CMAKE),no)
$(error "CMAKE is not available on this system")
endif

# ----------------------------------------------------
# PHONY
# ----------------------------------------------------
.PHONY: all 					\
				clean 				\
				compile 			\
				distclean 		\
				flymake 			\
				get-deps			\
			  help          \
				shell 				\
				update-deps		\
				xref

# ----------------------------------------------------
# MAIN
# ----------------------------------------------------
all: $(REBAR) get-deps flymake
	@$(REBAR) -C $(REBAR_CONFIG) compile skip_deps=true

# ----------------------------------------------------
# SHELL
# ----------------------------------------------------
shell:
	@./start.sh ${OPTS}

# ----------------------------------------------------
# REBAR
# ----------------------------------------------------
./rebar:
	@echo " fetching 'rebar'..."
	@erl -noshell -s inets start -s ssl start \
	-eval 'httpc:request(get, {"https://github.com/rebar/rebar/wiki/rebar", []}, [], [{stream, "$(PWD)/rebar"}]), init:stop().'
	@chmod +x $(REBAR)

get-deps:
	@$(REBAR) -C $(REBAR_CONFIG) check-deps >/dev/null || $(REBAR) -C $(REBAR_CONFIG) get-deps && $(REBAR) -C $(REBAR_CONFIG) compile

update-deps:
	@$(REBAR) -C $(REBAR_CONFIG) check-deps >/dev/null || $(REBAR) -C $(REBAR_CONFIG) update-deps && $(REBAR) compile

# ----------------------------------------------------
# XREF check
# ----------------------------------------------------
xref: all
	@$(REBAR) -C $(REBAR_CONFIG) $@ skip_deps=true

# ----------------------------------------------------
# CLEANUP
# ----------------------------------------------------
flymake:
	@rm -f $(PWD)/src/*flymake* $(PWD)/src/*/*flymake* $(PWD)/deps/*/src/*flymake*

clean:
	@if test -f $(REBAR); then $(REBAR) -C $(REBAR_CONFIG) $@ skip_deps=true; else break; fi
	@rm -f erl_crash.dump
	@rm -f *~ */*~ */*/*~ */*/*/*~ */*/*/*/*~

distclean: clean
	@if test -f $(REBAR); then $(REBAR) -C $(REBAR_CONFIG) delete-deps; rm -f $(REBAR); else break; fi
	@rm -f $(PWD)/src/content/copy_samples/*.md.erlang
	@rm -rf deps $(PWD)/ebin $(PWD)/priv
	@rm -rf .rebar $(PWD)/deps/*/.rebar $(REBAR)
	@cd $(PWD)/src/content/cmark_parse/cmark; make $@

# ----------------------------------------------------
# HELP
# ----------------------------------------------------
help:
	@echo "$(EMAKE) all           : (re)build the project"
	@echo "$(EMAKE) clean         : clean-up object files"
	@echo "$(EMAKE) distclean     : clean-up everything"
	@echo "$(EMAKE) shell [OPTS=] : start an Erlang shell"
	@echo "$(EMAKE) update-deps   : grab and build all dependencies"
	@echo "$(EMAKE) xref          : xref check"
