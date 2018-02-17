PROJECT = crc32cer
PROJECT_DESCRIPTION = CRC32C as nif for Erlang
PROJECT_VERSION = 0.1.0

SP = 2

CPPFLAGS = -Wno-sign-compare -Wno-unused-function
LDFLAGS += -shared -lstdc++

ERLC_OPTS = -Werror +warn_unused_vars +warn_shadow_vars +warn_unused_import +warn_obsolete_guard +debug_info

EUNIT_OPTS = verbose

include erlang.mk

hex-publish: distclean
	rebar3 hex publish
