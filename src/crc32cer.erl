%% @doc CRC32C (Castagnoli) checksum calculation library for Erlang.
%%
%% This module provides high-performance CRC32C checksum calculation functions
%% using native C implementations (NIFs). CRC32C is optimized for modern CPUs
%% and is commonly used in storage systems, networking protocols, and data
%% integrity verification.
%%
%% The module offers two main approaches:
%% <ul>
%% <li><b>Standard approach</b> (`nif`/`nif_d`): Uses VM-optimized iolist processing
%%     for general-purpose checksum calculation. Best for most use cases.</li>
%% <li><b>Iolist optimized</b> (`nif_iolist_d`): Uses stack-based
%%     processing optimized for large binary chunks. Best for processing batch of large
%%     binaries without creating temporary binaries.</li>
%% </ul>
%%
%% == Performance Characteristics ==
%%
%% <ul>
%% <li><b>Standard approach</b>: Best for general iolist processing, small to medium data</li>
%% <li><b>Iolist optimized</b>: Up to 28x faster for batches of large binary chunks (>100KB)</li>
%% <li><b>Pre-converted binary</b>: Fastest overall when data is already a binary</li>
%% </ul>
%%
%% == Examples ==
%%
%% ```
%% %% Basic usage
%% Crc1 = crc32cer:nif(<<"hello world">>),
%% Crc2 = crc32cer:nif(0, <<"hello world">>),
%%
%% %% Iolist processing
%% IoList = [<<"hello">>, " ", <<"world">>],
%% Crc3 = crc32cer:nif(IoList),
%%
%% %% Batch of large binary optimization
%% LargeChunks = [binary:copy(<<"chunk">>, 100000) || _ <- lists:seq(1, 5)],
%% Crc4 = crc32cer:nif_iolist_d(0, LargeChunks),
%% ```
%%
%% @see <a href="https://en.wikipedia.org/wiki/Cyclic_redundancy_check#CRC-32C">CRC-32C on Wikipedia</a>
%% @see <a href="https://tools.ietf.org/html/rfc3720#section-12.1">RFC 3720 - CRC32C</a>
-module(crc32cer).

-export([
    nif/1,
    nif/2,
    nif_d/1,
    nif_d/2,
    nif_iolist/1,
    nif_iolist/2,
    nif_iolist_d/1,
    nif_iolist_d/2
]).

-on_load(init/0).

-spec init() -> ok.
init() ->
    _ = erlang:load_nif(so_path(), 0),
    ok.

%% @doc Calculate CRC32C checksum of iodata with initial CRC of 0.
%%
%% This is the most commonly used function for CRC32C calculation. It processes
%% any iodata (binary, iolist, or mixed) and returns the CRC32C checksum.
%%
%% == Examples ==
%% ```
%% %% Binary data
%% Crc1 = crc32cer:nif(<<"hello world">>),
%%
%% %% Iolist data
%% IoList = [<<"hello">>, " ", <<"world">>],
%% Crc2 = crc32cer:nif(IoList),
%%
%% %% Mixed data
%% Mixed = [<<"header">>, [<<"nested">>, <<"data">>], <<"footer">>],
%% Crc3 = crc32cer:nif(Mixed),
%% ```
%%
%% IoData The iodata to calculate CRC32C for
%% Returns the CRC32C checksum as a non-negative integer
-spec nif(iodata()) -> non_neg_integer().
nif(IoData) ->
    nif(0, IoData).

%% @doc Calculate CRC32C checksum of iodata with custom initial CRC.
%%
%% This function allows you to specify an initial CRC value, which is useful for
%% incremental checksum calculation or when continuing from a previous calculation.
%%
%% == Examples ==
%% ```
%% %% With custom initial CRC
%% Crc1 = crc32cer:nif(16#12345678, <<"hello world">>),
%%
%% %% Incremental calculation
%% Crc2 = crc32cer:nif(0, <<"hello">>),
%% Crc3 = crc32cer:nif(Crc2, <<" world">>),
%%
%% %% Same as: crc32cer:nif(<<"hello world">>)
%% ```
%%
%% The initial CRC value (0 for fresh calculation)
%% IoData The iodata to calculate CRC32C for
%% Returns the CRC32C checksum as a non-negative integer
-spec nif(integer(), iodata()) -> non_neg_integer().
nif(Acc, IoData) ->
    nif_d(Acc, IoData).

%% @doc Calculate CRC32C checksum of iodata with initial CRC of 0 (dirty scheduler job).
%%
%% This function is identical to `nif/1` but runs as a dirty scheduler job, which
%% allows it to use more CPU time without blocking the scheduler. Use this
%% for CPU-intensive checksum calculations.
%%
%% == Examples ==
%% ```
%% %% Large data processing
%% LargeData = binary:copy(<<"data">>, 1000000),
%% Crc = crc32cer:nif_d(LargeData),
%%
%% %% Iolist processing
%% IoList = [binary:copy(<<"chunk">>, 1000) || _ <- lists:seq(1, 100)],
%% Crc2 = crc32cer:nif_d(IoList),
%% ```
%%
%% IoData The iodata to calculate CRC32C for
%% Returns the CRC32C checksum as a non-negative integer
-spec nif_d(iodata()) -> non_neg_integer().
nif_d(IoData) ->
    nif_d(0, IoData).

%% @doc Calculate CRC32C checksum of iodata with custom initial CRC (dirty scheduler job).
%%
%% This function is identical to `nif/2` but runs as a dirty scheduler job, which
%% allows it to use more CPU time without blocking the scheduler. Use this
%% for CPU-intensive checksum calculations with custom initial CRC values.
%%
%% == Examples ==
%% ```
%% %% With custom initial CRC
%% Crc1 = crc32cer:nif_d(16#12345678, <<"hello world">>),
%%
%% %% Incremental calculation of large data
%% Crc2 = crc32cer:nif_d(0, <<"part1">>),
%% Crc3 = crc32cer:nif_d(Crc2, <<"part2">>),
%% Crc4 = crc32cer:nif_d(Crc3, <<"part3">>),
%% ```
%%
%% Acc The initial CRC value (0 for fresh calculation)
%% IoData The iodata to calculate CRC32C for
%% Returns the CRC32C checksum as a non-negative integer
-spec nif_d(integer(), iodata()) -> non_neg_integer().
nif_d(_Acc, _IoData) ->
    erlang:nif_error({crc32cer_nif_not_loaded, so_path()}).

%% @doc Calculate CRC32C checksum of iodata with initial CRC of 0 (iolist optimized).
%%
%% This function is identical to `nif/1` but uses an iolist-optimized approach
%% that can be significantly faster for batches of large binary chunks.
%%
%% == Examples ==
%% ```
%% %% Batch of large binary chunks
%% LargeChunks = [binary:copy(<<"chunk">>, 100000) || _ <- lists:seq(1, 10)],
%% Crc = crc32cer:nif_iolist(LargeChunks),
%%
%% %% Mixed iolist with large binaries
%% IoList = [<<"header">>, binary:copy(<<"data">>, 50000), <<"footer">>],
%% Crc2 = crc32cer:nif_iolist(IoList),
%% ```
%%
%% @param IoData The iodata to calculate CRC32C for
%% @returns The CRC32C checksum as a non-negative integer
-spec nif_iolist(iodata()) -> non_neg_integer().
nif_iolist(IoData) ->
    nif_iolist(0, IoData).

%% @doc Calculate CRC32C checksum optimized for batches of large binary chunks.
%%
%% This function uses a stack-based approach optimized for processing batches of
%% large binary chunks without creating temporary binaries. It can be up to 28x
%% faster than the standard approach for batches of large data (>100KB per chunk).
%%
%% == Performance Characteristics ==
%%
%% <ul>
%% <li><b>Best for</b>: Batches of large binary chunks, memory-constrained environments</li>
%% <li><b>Performance</b>: Up to 28x faster than standard approach for batches of large data</li>
%% <li><b>Memory</b>: No temporary binary creation, fixed 64-entry stack</li>
%% <li><b>Fallback</b>: Automatically falls back to VM approach for deep nesting (>64 levels)</li>
%% </ul>
%%
%% == Examples ==
%% ```
%% %% Batch of large binary chunks
%% LargeChunks = [binary:copy(<<"chunk">>, 100000) || _ <- lists:seq(1, 10)],
%% Crc1 = crc32cer:nif_iolist(0, LargeChunks),
%%
%% %% Mixed iolist with large binaries
%% IoList = [<<"header">>, binary:copy(<<"data">>, 50000), <<"footer">>],
%% Crc2 = crc32cer:nif_iolist(0, IoList),
%%
%% %% With custom initial CRC
%% Crc3 = crc32cer:nif_iolist(16#12345678, LargeChunks),
%% ```
%%
%% @param Acc The initial CRC value (0 for fresh calculation)
%% @param IoData The iodata to calculate CRC32C for
%% @returns The CRC32C checksum as a non-negative integer
-spec nif_iolist(integer(), iodata()) -> non_neg_integer().
nif_iolist(_Acc, _IoData) ->
    erlang:nif_error({crc32cer_nif_not_loaded, so_path()}).

%% @doc Calculate CRC32C checksum of iodata with initial CRC of 0 (iolist optimized, dirty scheduler).
%%
%% This function is identical to `nif_iolist/1` but runs as a dirty scheduler job.
%%
%% == Examples ==
%% ```
%% %% Batch of large binary chunks
%% LargeChunks = [binary:copy(<<"chunk">>, 100000) || _ <- lists:seq(1, 10)],
%% Crc = crc32cer:nif_iolist_d(LargeChunks),
%%
%% %% Mixed iolist with large binaries
%% IoList = [<<"header">>, binary:copy(<<"data">>, 50000), <<"footer">>],
%% Crc2 = crc32cer:nif_iolist_d(IoList),
%% ```
%%
%% @param IoData The iodata to calculate CRC32C for
%% @returns The CRC32C checksum as a non-negative integer
-spec nif_iolist_d(iodata()) -> non_neg_integer().
nif_iolist_d(IoData) ->
    nif_iolist_d(0, IoData).

%% @doc Calculate CRC32C checksum optimized for batches of large binary chunks (dirty scheduler job).
%%
%% This function uses a stack-based approach optimized for processing batches of
%% large binary chunks without creating temporary binaries. It can be up to 28x
%% faster than the standard approach for batches of large data (>100KB per chunk).
%%
%% == Performance Characteristics ==
%%
%% <ul>
%% <li><b>Best for</b>: Batches of large binary chunks, memory-constrained environments</li>
%% <li><b>Performance</b>: Up to 28x faster than standard approach for batches of large data</li>
%% <li><b>Memory</b>: No temporary binary creation, fixed 64-entry stack</li>
%% <li><b>Fallback</b>: Automatically falls back to VM approach for deep nesting (>64 levels)</li>
%% </ul>
%%
%% == Examples ==
%% ```
%% %% Batch of large binary chunks
%% LargeChunks = [binary:copy(<<"chunk">>, 100000) || _ <- lists:seq(1, 10)],
%% Crc1 = crc32cer:nif_iolist_d(0, LargeChunks),
%%
%% %% Mixed iolist with large binaries
%% IoList = [<<"header">>, binary:copy(<<"data">>, 50000), <<"footer">>],
%% Crc2 = crc32cer:nif_iolist_d(0, IoList),
%%
%% %% With custom initial CRC
%% Crc3 = crc32cer:nif_iolist_d(16#12345678, LargeChunks),
%% ```
%%
%% @param Acc The initial CRC value (0 for fresh calculation)
%% @param IoData The iodata to calculate CRC32C for
%% @returns The CRC32C checksum as a non-negative integer
-spec nif_iolist_d(integer(), iodata()) -> non_neg_integer().
nif_iolist_d(_Acc, _IoData) ->
    erlang:nif_error({crc32cer_nif_not_loaded, so_path()}).

-spec so_path() -> string().
so_path() ->
    filename:join([get_nif_bin_dir(), "libcrc32cer_nif"]).

get_nif_bin_dir() ->
    {ok, Cwd} = file:get_cwd(),
    get_nif_bin_dir(
        [
            code:priv_dir(crc32cer),
            filename:join([Cwd, "..", "priv"]),
            filename:join(Cwd, "priv"),
            os:getenv("NIF_BIN_DIR")
        ]
    ).

get_nif_bin_dir([]) ->
    erlang:error(crc32cer_nif_not_found);
get_nif_bin_dir([false | Rest]) ->
    get_nif_bin_dir(Rest);
get_nif_bin_dir([Dir | Rest]) ->
    case filelib:wildcard(filename:join([Dir, "libcrc32cer_nif*"])) of
        [] -> get_nif_bin_dir(Rest);
        [_ | _] -> Dir
    end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
