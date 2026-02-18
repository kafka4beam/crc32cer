# 1.1.3

- Use `crc32c` as a submodule.

# 1.1.2

- Fix link issue on aarch64: "undefined symbol: __cxa_guard_release"

# 1.1.1

- Add FreeBSD to build system list in rebar.config.

# 1.1.0

- Add `nif_iolist` and `nif_iolist_d` APIs for iolist with large binary chunks.

# 1.0.5

- Add dummy `port_spec` to rebar.config

# 1.0.4

- Fix link error on Enterprise Linux (centos/rockylinux/redhat).

# 1.0.3

- Improve build. Skip unnecessary submodule clones.

# 1.0.2

- Changed to build with cmake from Google's repo for 10x performance improvements see original [PR](https://github.com/zmstone/crc32cer/pull/7).

