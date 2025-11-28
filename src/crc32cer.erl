%%-------------------------------------------------------------------
%% crc32cer - CRC32C (Castagnoli) implementation with NIF or fallback
%%
%% If NIF loads successfully, its functions override nif_d/2.
%% brod and kafka_protocol call nif/1, so this file provides
%% a fallback implementation that never crashes.
%%-------------------------------------------------------------------

-module(crc32cer).

-on_load(init/0).

-export([crc32/1, crc32/2, nif_d/2, nif/1]).

%% NIF filename
-define(LIBNAME, "libcrc32cer_nif").

%% CRC32C (Castagnoli) poly and flags
-define(CRC32C_POLY,   16#82F63B78).
-define(CRC32C_INIT,   16#FFFFFFFF).
-define(CRC32C_XOROUT, 16#FFFFFFFF).

%%-------------------------------------------------------------------
%% on_load – attempt to load NIF, fallback if fails
%%-------------------------------------------------------------------

init() ->
    case erlang:load_nif(?LIBNAME, 0) of
        ok ->
            ok;
        {error, Reason} ->
            error_logger:warning_msg(
              "crc32cer: failed to load NIF (~p), using pure Erlang fallback~n",
              [Reason]),
            ok
    end.

%%-------------------------------------------------------------------
%% Public API
%%-------------------------------------------------------------------

%% brod & kafka_protocol expect this function
-spec nif(iodata()) -> non_neg_integer().
nif(Data) ->
    crc32(Data).

%% If NIF loads successfully, it overrides this
-spec nif_d(non_neg_integer(), iodata()) -> non_neg_integer().
nif_d(Acc, Data) ->
    crc32(Acc, Data).

%% Standard API
-spec crc32(iodata()) -> non_neg_integer().
crc32(Data) ->
    crc32(?CRC32C_INIT, Data).

-spec crc32(non_neg_integer(), iodata()) -> non_neg_integer().
crc32(Acc, Data) when is_binary(Data) ->
    crc32c_binary(Data, Acc);
crc32(Acc, Data) when is_list(Data) ->
    crc32(Acc, list_to_binary(Data)).

%%-------------------------------------------------------------------
%% Pure Erlang CRC32C implementation
%%-------------------------------------------------------------------

-spec crc32c_binary(binary(), non_neg_integer()) -> non_neg_integer().
crc32c_binary(Bin, Acc0) ->
    Acc1 = crc32c_binary_loop(Bin, Acc0 band 16#FFFFFFFF),
    (Acc1 bxor ?CRC32C_XOROUT) band 16#FFFFFFFF.

crc32c_binary_loop(<<>>, Acc) ->
    Acc;
crc32c_binary_loop(<<Byte, Rest/binary>>, Acc) ->
    Acc1 = (Acc bxor Byte) band 16#FFFFFFFF,
    crc32c_binary_loop(Rest, crc32c_step8(Acc1)).

-spec crc32c_step8(non_neg_integer()) -> non_neg_integer().
crc32c_step8(Crc0) ->
    crc32c_step8(Crc0, 8).

crc32c_step8(Crc, 0) ->
    Crc band 16#FFFFFFFF;
crc32c_step8(Crc, N) ->
    Crc1 =
        case Crc band 1 of
            1 -> ((Crc bsr 1) bxor ?CRC32C_POLY);
            0 -> (Crc bsr 1)
        end,
    crc32c_step8(Crc1 band 16#FFFFFFFF, N - 1).
