PROJECT = config_service
SRC = src apps/*/src deps/apptools/src deps/jiffy/src deps/cowboy/src
EBIN = apps/*/ebin ebin deps/*/ebin

include erlang.mk

clean:
	rm -rf deps ebin
