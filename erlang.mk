all: compile

compile:
	rebar compile

get-deps:
	rebar get-deps

shell:
	erl -pa deps/*/ebin ebin

test:
	rebar eunit

# Dialyzer.

PLT_APPS ?=
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions \
	-Wunmatched_returns # -Wunderspecs

build-plt:
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
	    --apps erts kernel stdlib $(PLT_APPS)

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native $(DIALYZER_OPTS)
