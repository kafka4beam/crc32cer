#include <crc32c/crc32c.h>
#include <stdint.h>

#include "erl_nif.h"

static ERL_NIF_TERM crc32c_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  ErlNifBinary bin;
  uint32_t crc;

  if (!enif_get_uint(env, argv[0], &crc)) {
    return enif_make_badarg(env);
  }

  if (!(enif_inspect_binary(env, argv[1], &bin) ||
        enif_inspect_iolist_as_binary(env, argv[1], &bin))) {
    return enif_make_badarg(env);
  }

  uint32_t result = crc32c_extend(crc, bin.data, bin.size);

  return enif_make_uint(env, result);

}

static int on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
  return 0;
}

static ErlNifFunc nif_funs[] =
{
	    {"nif", 2, crc32c_nif, 0},
      {"nif_d", 2, crc32c_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(crc32cer, nif_funs, on_load, NULL, NULL, NULL);

