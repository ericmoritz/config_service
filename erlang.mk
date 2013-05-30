PLT_APPS ?=
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions \
-Wunmatched_returns # -Wunderspecs
SRC ?= src
EBIN ?= ebin deps/*/ebin

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

# Dialyzer.

build-plt:
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
	    --apps erts kernel stdlib $(PLT_APPS)

dialyze:
	@dialyzer --src $(SRC) \
	    --plt .$(PROJECT).plt --no_native $(DIALYZER_OPTS)
