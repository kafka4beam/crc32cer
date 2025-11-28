%%-------------------------------------------------------------------
%% crc32cer - CRC32C (Castagnoli) with optional NIF
%%
%% If the NIF loads successfully, its implementation of nif_d/2 will
%% override this module's Erlang version.
%%
%% If the NIF fails to load (e.g. OTP 27 + musl ABI mismatch), we fall
%% back to a pure Erlang CRC32C implementation so callers (brod /
%% kafka_protocol) still work and do not crash with
%% crc32cer_nif_not_loaded.
%%-------------------------------------------------------------------

-module(crc32cer).

-on_load(init/0).

-export([crc32/1, crc32/2, nif_d/2]).

-define(LIBNAME, "libcrc32cer_nif").

%% CRC32C (Castagnoli) parameters
-define(CRC32C_POLY, 16#82F63B78).
-define(CRC32C_INIT, 16#FFFFFFFF).
-define(CRC32C_XOROUT, 16#FFFFFFFF).

%%-------------------------------------------------------------------
%% NIF loading
%%-------------------------------------------------------------------

-spec init() -> ok.
init() ->
    %% Try to load the NIF; if it fails, log and fall back to Erlang.
    %% We *never* crash here, so callers always have a working implementation.
    case erlang:load_nif(?LIBNAME, 0) of
        ok ->
            ok;
        {error, Reason} ->
            error_logger:warning_msg(
              "crc32cer: failed to load NIF (~p), using Erlang CRC32C fallback~n",
              [Reason]),
            ok
    end.

%%-------------------------------------------------------------------
%% NIF entry point
%%
%% When the NIF is successfully loaded, its implementation of nif_d/2
%% will replace this one. If not, we use the Erlang CRC32C fallback.
%%-------------------------------------------------------------------

-spec nif_d(non_neg_integer(), iodata()) -> non_neg_integer().
nif_d(Acc, Data) ->
    crc32(Acc, Data).

%%-------------------------------------------------------------------
%% Public API
%%-------------------------------------------------------------------

-spec crc32(iodata()) -> non_neg_integer().
crc32(Data) ->
    crc32(?CRC32C_INIT, Data).

-spec crc32(non_neg_integer(), iodata()) -> non_neg_integer().
crc32(Acc, Data) when is_binary(Data) ->
    crc32c_binary(Data, Acc);
crc32(Acc, Data) when is_list(Data) ->
    crc32(Acc, list_to_binary(Data)).

%%-------------------------------------------------------------------
%% Pure Erlang CRC32C implementation (reflected, Castagnoli)
%%-------------------------------------------------------------------

%% Process the whole binary and apply final XOR
-spec crc32c_binary(binary(), non_neg_integer()) -> non_neg_integer().
crc32c_binary(Bin, Acc0) when is_binary(Bin) ->
    Acc1 = crc32c_binary_loop(Bin, Acc0 band 16#FFFFFFFF),
    (Acc1 bxor ?CRC32C_XOROUT) band 16#FFFFFFFF.

%% Walk bytes
-spec crc32c_binary_loop(binary(), non_neg_integer()) -> non_neg_integer().
crc32c_binary_loop(<<>>, Acc) ->
    Acc;
crc32c_binary_loop(<<Byte, Rest/binary>>, Acc) ->
    Acc1 = (Acc bxor Byte) band 16#FFFFFFFF,
    crc32c_binary_loop(Rest, crc32c_step8(Acc1)).

%% Apply polynomial for 8 bits (one byte)
-spec crc32c_step8(non_neg_integer()) -> non_neg_integer().
crc32c_step8(Crc0) ->
    crc32c_step8(Crc0, 8).

-spec crc32c_step8(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
crc32c_step8(Crc, 0) ->
    Crc band 16#FFFFFFFF;
crc32c_step8(Crc, N) ->
    Crc1 =
        case Crc band 1 of
            1 -> ((Crc bsr 1) bxor ?CRC32C_POLY);
            0 -> (Crc bsr 1)
        end,
    crc32c_step8(Crc1 band 16#FFFFFFFF, N - 1).
