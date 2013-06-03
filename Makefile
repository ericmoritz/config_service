PROJECT = config_service
SRC = deps/*/src apps/*/src
EBIN = apps/*/ebin deps/*/ebin
PLT_APPS = crypto ssl public_key

include erlang.mk

clean:
	rm -rf deps apps/*/ebin
