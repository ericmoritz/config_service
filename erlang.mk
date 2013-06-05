PLT_APPS ?=
SRC ?= src
EBIN ?= ebin deps/*/ebin
DIALYZER_OPTS ?= \
	-Werror_handling\
	-Wunmatched_returns

.PHONY: all compile deps demo-shell shell test rel relclean pltclean dialyze

all: compile

compile: get-deps
	rebar compile

deps:
	rebar get-deps

demo-shell: compile
	erl -pa $(EBIN) -s $(PROJECT)

shell: compile
	erl -pa $(EBIN)

test:
	rebar eunit skip_deps=true

rel: compile
	rebar generate

relclean:
	rm -rf rel/$(PROJECT)

# Dialyzer.

.$(PROJECT).plt:
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
	    --apps erts kernel stdlib $(PLT_APPS)

dialyze: .$(PROJECT).plt
	@dialyzer --src $(SRC) \
	    --plt .$(PROJECT).plt $(DIALYZER_OPTS)

pltclean:
	rm .$(PROJECT).plt
