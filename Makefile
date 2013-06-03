PROJECT = config_service
SRC = src deps/*/src apps/*/src
EBIN = apps/*/ebin ebin deps/*/ebin
PLT_APPS = crypto ssl public_key

include erlang.mk

clean:
	rm -rf deps ebin apps/*/ebin
