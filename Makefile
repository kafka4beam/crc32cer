PROJECT = crc32cer
PROJECT_DESCRIPTION = CRC32C as nif for Erlang
PROJECT_VERSION = 0.1.1

SP = 2

CPPFLAGS = -Wno-sign-compare -Wno-unused-function
LDFLAGS += -shared -lstdc++

ERLC_OPTS = -Werror +warn_unused_vars +warn_shadow_vars +warn_unused_import +warn_obsolete_guard +debug_info

EUNIT_OPTS = verbose

ifeq ($(shell uname -s),Darwin)
	CFLAGS += -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -O3 -arch x86_64 -finline-functions -Wall
	LDFLAGS += -arch x86_64 -flat_namespace -undefined suppress
endif

include erlang.mk

hex-publish: distclean
	rebar3 hex publish
