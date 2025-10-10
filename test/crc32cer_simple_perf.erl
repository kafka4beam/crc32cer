%% @doc Simple performance benchmark tests for CRC32C iolist processing approaches.
%%
%% This module provides focused performance tests comparing:
%% 1. Standard approach: Using nif/2 with accumulation for each iolist element
%% 2. Optimized approach: Using nif_iolist/1 for direct iolist processing

-module(crc32cer_simple_perf).

-include_lib("eunit/include/eunit.hrl").

-define(KB, 1024).

%% =============================================================================
%% Main Performance Test
%% =============================================================================

%% @doc Run performance comparison tests
iolist_performance_comparison_test_() ->
    {"Iolist Performance Comparison", fun iolist_performance_comparison/0}.

iolist_performance_comparison() ->
    ?debugFmt("=== CRC32C Iolist Performance Comparison ===", []),
    ?debugFmt("System: ~s", [erlang:system_info(system_architecture)]),
    ?debugFmt("Erlang/OTP: ~s", [erlang:system_info(otp_release)]),
    ?debugFmt("", []),

    %% Test cases with flat binary lists
    TestCases = [
        {"Simple iolist", [<<"hello">>, <<" ">>, <<"world">>]},
        {"Small chunks", [binary:copy(<<"c">>, 100) || _ <- lists:seq(1, 10)]},
        {"Medium chunks", [binary:copy(<<"data">>, 1000) || _ <- lists:seq(1, 5)]},
        {"Large chunks", [binary:copy(<<"chunk">>, 5000) || _ <- lists:seq(1, 3)]},
        {"Many small chunks", [binary:copy(<<"x">>, 50) || _ <- lists:seq(1, 20)]},
        {"Character codes", [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]},
        {"Mixed binary and chars", [<<"hello">>, 32, <<"world">>, 33]},
        {"Single large binary", [binary:copy(<<"data">>, 10000)]}
    ],

    lists:foreach(
        fun({Name, Data}) ->
            run_comparison(Name, Data, 100)
        end,
        TestCases
    ),

    ?debugFmt("=== Performance comparison completed ===", []).

%% =============================================================================
%% Core Comparison Function
%% =============================================================================

%% @doc Run a performance comparison between the two approaches
run_comparison(Name, Data, Iterations) ->
    ?debugFmt("Testing: ~s", [Name]),

    %% Verify correctness first
    CrcAccumulation = crc32cer_iolist_accumulation(Data),
    CrcOptimized = crc32cer:nif_iolist(Data),
    ?assertEqual(
        CrcAccumulation,
        CrcOptimized,
        io_lib:format(
            "~s: CRC mismatch (~p != ~p)",
            [Name, CrcAccumulation, CrcOptimized]
        )
    ),

    %% Warm up
    warm_up(Data, 5),

    %% Benchmark accumulation approach
    {AccTime, _} = timer:tc(fun() ->
        lists:foreach(
            fun(_) -> crc32cer_iolist_accumulation(Data) end,
            lists:seq(1, Iterations)
        )
    end),

    %% Benchmark optimized approach
    {OptTime, _} = timer:tc(fun() ->
        lists:foreach(
            fun(_) -> crc32cer:nif_iolist(Data) end,
            lists:seq(1, Iterations)
        )
    end),

    %% Calculate statistics
    AccAvg = AccTime / Iterations,
    OptAvg = OptTime / Iterations,
    Speedup = AccTime / OptTime,

    %% Report results
    ?debugFmt("  Accumulation: ~.2f μs/op (~p μs total)", [AccAvg, AccTime]),
    ?debugFmt("  Optimized:    ~.2f μs/op (~p μs total)", [OptAvg, OptTime]),
    ?debugFmt("  Speedup:      ~.2fx", [Speedup]),
    ?debugFmt("", []).

%% @doc Iolist processing using nif/2 with simple iteration (no recursion)
crc32cer_iolist_accumulation(Int) when is_integer(Int) ->
    crc32cer:nif(0, [Int]);
crc32cer_iolist_accumulation(Bin) when is_binary(Bin) ->
    crc32cer:nif(0, Bin);
crc32cer_iolist_accumulation(L) when is_list(L) ->
    lists:foldl(
        fun(I, Acc) ->
            case I of
                Int when is_integer(Int) ->
                    crc32cer:nif(Acc, [Int]);
                Bin when is_binary(Bin) ->
                    crc32cer:nif(Acc, Bin);
                _ ->
                    crc32cer:nif(Acc, I)
            end
        end,
        0,
        L
    ).

%% @doc Warm up function to ensure consistent timing
warm_up(Data, Count) ->
    lists:foreach(
        fun(_) ->
            crc32cer_iolist_accumulation(Data),
            crc32cer:nif_iolist(Data)
        end,
        lists:seq(1, Count)
    ).

%% =============================================================================
%% Helper Functions
%% =============================================================================

%% No helper functions needed for flat binary lists

%% =============================================================================
%% Correctness Tests
%% =============================================================================

%% @doc Verify that both approaches produce identical results
correctness_test_() ->
    {"Correctness Verification", fun correctness_test/0}.

correctness_test() ->
    ?debugFmt("=== Correctness Verification ===", []),

    %% Test various flat data structures
    TestCases = [
        {<<"simple_binary">>, "Simple binary"},
        {[<<"part1">>, <<"part2">>, <<"part3">>], "Simple iolist"},
        {[<<"header">>, lists:duplicate(100, $a), <<"footer">>], "Mixed iolist"},
        {[binary:copy(<<"c">>, 10) || _ <- lists:seq(1, 5)], "Multiple chunks"},
        {[72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100], "Character codes"},
        {[<<"hello">>, 32, <<"world">>], "Mixed binary and character codes"},
        {[binary:copy(<<"data">>, 1000) || _ <- lists:seq(1, 3)], "Medium chunks"},
        {[binary:copy(<<"x">>, 50) || _ <- lists:seq(1, 10)], "Many small chunks"}
    ],

    lists:foreach(
        fun({TestData, Name}) ->
            CrcAccumulation = crc32cer_iolist_accumulation(TestData),
            CrcOptimized = crc32cer:nif_iolist(TestData),
            ?assertEqual(
                CrcAccumulation,
                CrcOptimized,
                io_lib:format(
                    "~s: results don't match (~p != ~p)",
                    [Name, CrcAccumulation, CrcOptimized]
                )
            )
        end,
        TestCases
    ),

    ?debugFmt("All correctness tests passed", []).

%% =============================================================================
%% Large Data Performance Test
%% =============================================================================

%% @doc Test with larger data to show the performance difference more clearly
large_data_performance_test_() ->
    {"Large Data Performance Test", fun large_data_performance/0}.

large_data_performance() ->
    ?debugFmt("=== Large Data Performance Test ===", []),

    %% Test with larger flat binary lists
    TestCases = [
        {"Medium chunks", [binary:copy(<<"data">>, 1000) || _ <- lists:seq(1, 10)]},
        {"Large chunks", [binary:copy(<<"chunk">>, 5000) || _ <- lists:seq(1, 5)]},
        {"Many small chunks", [binary:copy(<<"x">>, 100) || _ <- lists:seq(1, 50)]},
        {"Very large chunks", [binary:copy(<<"big">>, 10000) || _ <- lists:seq(1, 3)]},
        {"Massive small chunks", [binary:copy(<<"a">>, 10) || _ <- lists:seq(1, 100)]}
    ],

    lists:foreach(
        fun({Name, Data}) ->
            run_comparison(Name, Data, 50)
        end,
        TestCases
    ),

    ?debugFmt("=== Large data performance test completed ===", []).
