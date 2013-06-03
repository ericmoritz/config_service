PLT_APPS ?=
SRC ?= src
EBIN ?= ebin deps/*/ebin
DIALYZER_OPTS ?= \
	-Werror_handling\
	-Wunmatched_returns

all: compile

compile: get-deps
	rebar compile

get-deps:
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

build-plt:
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
	    --apps erts kernel stdlib $(PLT_APPS)

dialyze: get-deps
	@dialyzer --src $(SRC) \
	    --plt .$(PROJECT).plt $(DIALYZER_OPTS)
