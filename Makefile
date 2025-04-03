.PHONY: all clean compile tests hex-publish

build_dir := c_build

nif:
	@echo "Compiling NIF in $(build_dir)"
	@mkdir -p $(build_dir)
	@cmake . -B $(build_dir)
	@cmake --build $(build_dir)

all: compile

compile:
	@rebar3 compile

tests:
	@rebar3 eunit -v

clean:
	@rm -f priv/*.so
	@rm -rf $(build_dir)
	@rebar3 clean

hex-publish: clean
	@rebar3 hex publish --repo=hexpm
