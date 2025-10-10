#include <crc32c/crc32c.h>
#include <stdint.h>

#include "erl_nif.h"

// Stack-based iolist traversal following OTP's generic_iolist_copy algorithm
// This avoids deep recursion by using an explicit stack like the VM does
// If depth exceeds MAX_STACK_SIZE, fall back to enif_inspect_iolist_as_binary
#define MAX_STACK_SIZE 64


// Extract and validate a number as a byte for CRC calculation
// Returns 0 on success, 1 on badarg
// CRC value is returned through the crc_ptr parameter
static int process_number_as_byte(ErlNifEnv* env, ERL_NIF_TERM term, uint32_t current_crc, uint32_t* crc_ptr) {
  int signed_code;
  if (enif_get_int(env, term, &signed_code)) {
    if (signed_code < 0 || signed_code > 255) {
      return 1; // badarg - number out of range
    }
    uint8_t byte = (uint8_t)signed_code;
    *crc_ptr = crc32c_extend(current_crc, &byte, 1);
    return 0;
  }
  return 1; // badarg - invalid number
}

// Stack-based iolist processing with error handling
// Returns 0 on success, 1 on badarg
// CRC value is returned through the crc_ptr parameter
static int crc32c_iolist_stack_based(ErlNifEnv* env, ERL_NIF_TERM term, uint32_t initial_crc, uint32_t* crc_ptr) {
  // Early exit for simple binary case
  if (enif_is_binary(env, term)) {
    ErlNifBinary bin;
    if (enif_inspect_binary(env, term, &bin)) {
      *crc_ptr = crc32c_extend(initial_crc, bin.data, bin.size);
      return 0;
    }
    return 1;
  }

  ERL_NIF_TERM stack[MAX_STACK_SIZE];
  int stack_top = 0;
  ERL_NIF_TERM current = term;
  uint32_t crc = initial_crc;

  while (1) {
    // Process current term
    if (enif_is_binary(env, current)) {
      ErlNifBinary bin;
      if (enif_inspect_binary(env, current, &bin)) {
        crc = crc32c_extend(crc, bin.data, bin.size);
      } else {
        // Failed to allocate ?
        return 1;
      }
    } else if (enif_is_list(env, current)) {
      ERL_NIF_TERM head, tail;
      if (enif_get_list_cell(env, current, &head, &tail)) {
        // Push tail onto stack for later processing (only if not empty)
        if (stack_top < MAX_STACK_SIZE) {
          // Only push non-empty lists onto the stack
          if (!enif_is_empty_list(env, tail)) {
            stack[stack_top++] = tail;
          }
          // Process head immediately
          current = head;
        } else {
          // Stack overflow - fall back to VM approach for head
          if (enif_is_number(env, head)) {
            // Handle integers (character codes in strings)
            if (process_number_as_byte(env, head, crc, &crc) != 0) {
              return 1; // badarg
            }
          } else {
            ErlNifBinary bin;
            if (enif_inspect_iolist_as_binary(env, head, &bin)) {
              crc = crc32c_extend(crc, bin.data, bin.size);
            } else {
              // Invalid data in head
              return 1; // badarg
            }
          }
          current = tail;
        }
        continue;
      }
      // Empty list - continue with stack
    } else if (enif_is_number(env, current)) {
      // Handle integers (character codes in strings)
      if (process_number_as_byte(env, current, crc, &crc) != 0) {
        return 1; // badarg
      }
    } else {
      // Invalid data encountered during traversal
      return 1; // badarg
    }
    // Pop next item from stack
    if (stack_top > 0) {
      current = stack[--stack_top];
    } else {
      break;
    }
  }

  *crc_ptr = crc;
  return 0; // success
}

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

// Stack-based optimized approach NIF with initial CRC (optimized for large binaries)
static ERL_NIF_TERM crc32c_nif_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  uint32_t crc;
  uint32_t result_crc;
  ERL_NIF_TERM term;

  if (argc != 2) {
    return enif_make_badarg(env);
  }

  if (!enif_get_uint(env, argv[0], &crc)) {
    return enif_make_badarg(env);
  }

  term = argv[1];

  // Call the stack-based function with error handling
  if (crc32c_iolist_stack_based(env, term, crc, &result_crc) != 0) {
    return enif_make_badarg(env);
  }

  return enif_make_uint(env, result_crc);
}


static int on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
  return 0;
}

static ErlNifFunc nif_funs[] =
{
  {"nif", 2, crc32c_nif, 0},
  {"nif_d", 2, crc32c_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"nif_iolist", 2, crc32c_nif_iolist, 0},
  {"nif_iolist_d", 2, crc32c_nif_iolist, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(crc32cer, nif_funs, on_load, NULL, NULL, NULL);

