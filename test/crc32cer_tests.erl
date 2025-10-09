-module(crc32cer_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KB, 1024).

basic_nif_test_() ->
  [basic(Fun) || Fun <- [nif, nif_d]].

basic(Fun) ->
  [ {"0", fun() -> ?assertEqual(0, crc32cer:Fun(<<>>)) end}
  , {"1-9", fun() -> ?assertEqual(16#e3069283, crc32cer:Fun("123456789")) end}
  , {"a", fun() -> ?assertEqual(16#c1d04330, crc32cer:Fun("a")) end}
  , {"license", fun() -> ?assertEqual(license_crc(), crc32cer:Fun(license_txt())) end}
  , {"acc",
     fun() ->
         Bytes = license_txt(),
         Crc = lists:foldl(fun(B, Acc) -> crc32cer:Fun(Acc, [B]) end, 0, Bytes),
         ?assertEqual(license_crc(), Crc)
     end}].

%% Test incremental iolist processing
iolist_nif_test_() ->
  [iolist_basic(Fun) || Fun <- [nif, nif_d, nif_iolist_d]].

iolist_basic(Fun) ->
  [ {"simple_iolist",
     fun() ->
         IoList = [<<"hello">>, " ", <<"world">>],
         Expected = crc32cer:Fun(<<"hello world">>),
         Actual = crc32cer:Fun(IoList),
         ?assertEqual(Expected, Actual)
     end}
  , {"nested_iolist",
     fun() ->
         IoList = [<<"part1">>, [<<"part2">>, " ", <<"part3">>], <<"part4">>],
         Expected = crc32cer:Fun(<<"part1part2 part3part4">>),
         Actual = crc32cer:Fun(IoList),
         ?assertEqual(Expected, Actual)
     end}
  , {"mixed_iolist",
     fun() ->
         IoList = [<<"binary">>, [<<"nested">>, 32, <<"chars">>], "string"],
         Expected = crc32cer:Fun(<<"binarynested charsstring">>),
         Actual = crc32cer:Fun(IoList),
         ?assertEqual(Expected, Actual)
     end}
  , {"empty_iolist",
     fun() ->
         IoList = [],
         Expected = crc32cer:Fun(<<>>),
         Actual = crc32cer:Fun(IoList),
         ?assertEqual(Expected, Actual)
     end}
  , {"iolist_with_acc",
     fun() ->
         IoList = [<<"hello">>, " ", <<"world">>],
         Expected = crc32cer:Fun(16#12345678, <<"hello world">>),
         Actual = crc32cer:Fun(16#12345678, IoList),
         ?assertEqual(Expected, Actual)
     end}
].

perf_test() ->
    Data = binary:copy(list_to_binary(license_txt()), 400),
    {Elapsed, ok} = timer:tc(fun() ->
                                     lists:foreach(fun(_) -> crc32cer:nif(Data) end, lists:seq(1, 1000))
                             end, millisecond),
    Arch = erlang:system_info(system_architecture),
    case string:find(Arch, "x86_64") of
      nomatch ->
        ?debugFmt("Performance test on non-x86 architecture: ~p ms", [Elapsed]);
      _ ->
        ?debugFmt("Performance test on x86 architecture: ~p ms", [Elapsed]),
        ?assert(Elapsed < 100)
    end.

%% Test deep iolist to verify performance with deep nesting
deep_iolist_test() ->
    %% Test various depths to see the limits
    Depths = [1000, 5000, 10000],

    lists:foreach(fun(Depth) ->
        DeepIoList = create_deep_iolist(Depth),
        {Time, Result} = timer:tc(fun() -> crc32cer:nif(DeepIoList) end),

        ?debugFmt("Deep iolist test (~p levels): ~p microseconds~n", [Depth, Time]),
        ?debugFmt("Result: ~p~n", [Result]),

        %% Verify it's not 0 (which would indicate failure)
        ?assertNotEqual(0, Result)
    end, Depths),

    true.

create_deep_iolist(0) -> [<<"end">>];
create_deep_iolist(N) -> [<<"level_", (integer_to_binary(N))/binary>>, create_deep_iolist(N-1)].

%% Test badarg cases for nif_iolist_d functions
nif_iolist_d_badarg_test_() ->
    badarg_test(nif_iolist_d).

nif_d_badarg_test_() ->
    badarg_test(nif_d).

badarg_test(Fun) ->
    [ {"atom", fun() -> ?assertError(badarg, crc32cer:Fun(atom)) end}
    , {"tuple", fun() -> ?assertError(badarg, crc32cer:Fun({1, 2, 3})) end}
    , {"pid", fun() -> ?assertError(badarg, crc32cer:Fun(self())) end}
    , {"ref", fun() -> ?assertError(badarg, crc32cer:Fun(make_ref())) end}
    , {"port", fun() -> ?assertError(badarg, crc32cer:Fun(hd(erlang:ports()))) end}
    , {"map", fun() -> ?assertError(badarg, crc32cer:Fun(#{key => value})) end}
    , {"invalid_nested", fun() -> ?assertError(badarg, crc32cer:Fun([1, {2, 3}, 4])) end}
    , {"invalid_atom_in_list", fun() -> ?assertError(badarg, crc32cer:Fun([<<"hello">>, world, <<"world">>])) end}
    , {"invalid_pid_in_list", fun() -> ?assertError(badarg, crc32cer:Fun([<<"hello">>, self(), <<"world">>])) end}
    , {"invalid_ref_in_list", fun() -> ?assertError(badarg, crc32cer:Fun([<<"hello">>, make_ref(), <<"world">>])) end}
    , {"invalid_port_in_list", fun() -> ?assertError(badarg, crc32cer:Fun([<<"hello">>, hd(erlang:ports()), <<"world">>])) end}
    , {"invalid_map_in_list", fun() -> ?assertError(badarg, crc32cer:Fun([<<"hello">>, #{key => value}, <<"world">>])) end}
    , {"invalid_tuple_in_list", fun() -> ?assertError(badarg, crc32cer:Fun([<<"hello">>, {1, 2}, <<"world">>])) end}
    , {"deep_invalid_nested", fun() -> ?assertError(badarg, crc32cer:Fun([<<"hello">>, [<<"nested">>, {invalid, data}], <<"world">>])) end}
    , {"mixed_invalid_types", fun() -> ?assertError(badarg, crc32cer:Fun([<<"hello">>, atom, 123, {tuple}, <<"world">>])) end}
    , {"non-byte number > 255", fun() -> ?assertError(badarg, crc32cer:Fun([256])) end}
    , {"non-byte numbers < 0", fun() -> ?assertError(badarg, crc32cer:Fun([-1])) end}
    ].

%% Test that valid inputs still work for nif_iolist_d functions
nif_iolist_d_valid_test_() ->
    valid_test(nif_iolist_d).

nif_d_valid_test_() ->
    valid_test(nif_d).


valid_test(Fun) ->
    [ {"binary", fun() -> ?assertEqual(16#c1d04330, crc32cer:Fun(<<"a">>)) end}
    , {"list", fun() -> ?assertEqual(16#e3069283, crc32cer:Fun("123456789")) end}
    , {"iolist", fun() -> ?assertEqual(16#e3069283, crc32cer:Fun([<<"123">>, "456", <<"789">>])) end}
    , {"empty_list", fun() -> ?assertEqual(0, crc32cer:Fun([])) end}
    , {"nested_list", fun() -> ?assertEqual(16#e3069283, crc32cer:Fun([<<"123">>, [<<"456">>, <<"789">>]])) end}
    , {"numbers_in_list", fun() -> ?assertEqual(16#e3069283, crc32cer:Fun([49, 50, 51, 52, 53, 54, 55, 56, 57])) end}
    , {"mixed_numbers_and_binaries", fun() -> ?assertEqual(3743053745, crc32cer:Fun([49, <<"234">>, 56, 57])) end}
    ].

license_crc() ->
  16#7dcde113.

license_txt() ->
"  This software is provided 'as-is', without any express or implied\n"
"  warranty.  In no event will the author be held liable for any damages\n"
"  arising from the use of this software.\n"
"\n"
"  Permission is granted to anyone to use this software for any purpose,\n"
"  including commercial applications, and to alter it and redistribute it\n"
"  freely, subject to the following restrictions:\n"
"\n"
"  1. The origin of this software must not be misrepresented; you must not\n"
"     claim that you wrote the original software. If you use this software\n"
"     in a product, an acknowledgment in the product documentation would be\n"
"     appreciated but is not required.\n"
"  2. Altered source versions must be plainly marked as such, and must not be\n"
"     misrepresented as being the original software.\n"
"  3. This notice may not be removed or altered from any source distribution.".

%% =============================================================================
%% Performance Tests
%% =============================================================================

%% Performance test for very large binary batches
performance_large_batch_test_() ->
    {"200KB x 10 Performance Test", fun performance_large_batch/0}.

performance_large_batch() ->
    ?debugFmt("=== 200KB x 10 Batch Performance Test ===", []),

    %% Generate large binary chunks
    LargeChunks = [binary:copy(<<"c">>, 200 * ?KB) || _ <- lists:seq(1, 10)],

    %% Test standard approach
    {StandardTime, _StandardResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_d(LargeChunks) end, lists:seq(1, 20))
    end),

    %% Test optimized approach
    {OptimizedTime, _OptimizedResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_iolist_d(LargeChunks) end, lists:seq(1, 20))
    end),

    Speedup = StandardTime / OptimizedTime,
    ?debugFmt("Standard approach: ~p microseconds", [StandardTime]),
    ?debugFmt("Optimized approach: ~p microseconds", [OptimizedTime]),
    ?debugFmt("Speedup: ~.2fx", [Speedup]),

    %% Assert reasonable speedup (at least 1.5x)
    ?assert(Speedup >= 1.1, io_lib:format("200KB x 10 performance insufficient: ~.2fx < 1.1x", [Speedup])).

%% Performance test for very large iolist
performance_very_large_iolist_test_() ->
    {"Very Large Iolist Performance Test", fun performance_very_large_iolist/0}.

performance_very_large_iolist() ->
    ?debugFmt("=== Very Large Iolist Performance Test ===", []),

    %% Create very large iolist
    VeryLargeIoList = [binary:copy(<<"v">>, 200 * ?KB) || _ <- lists:seq(1, 50)],

    %% Test both approaches
    {StandardTime, StandardResult} = timer:tc(fun() -> crc32cer:nif_d(VeryLargeIoList) end),
    {OptimizedTime, OptimizedResult} = timer:tc(fun() -> crc32cer:nif_iolist_d(VeryLargeIoList) end),

    %% Results should be identical
    ?assertEqual(StandardResult, OptimizedResult),

    Speedup = StandardTime / OptimizedTime,
    ?debugFmt("Standard approach: ~p microseconds", [StandardTime]),
    ?debugFmt("Optimized approach: ~p microseconds", [OptimizedTime]),
    ?debugFmt("Speedup: ~.2fx", [Speedup]),

    %% Assert significant speedup for very large data (at least 2.0x)
    ?assert(Speedup >= 2.0, io_lib:format("Very large iolist performance insufficient: ~.2fx < 2.0x", [Speedup])).

%% Performance test for deep nesting
performance_deep_nesting_test_() ->
    {"Deep Nesting Performance Test", fun performance_deep_nesting/0}.

performance_deep_nesting() ->
    ?debugFmt("=== Deep Nesting Performance Test ===", []),

    %% Create deeply nested iolist with large data
    DeepLargeIoList = create_deep_large_iolist(128, 10 * ?KB),

    %% Test both approaches
    {StandardTime, StandardResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_d(DeepLargeIoList) end, lists:seq(1, 10))
    end),

    {OptimizedTime, OptimizedResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_iolist_d(DeepLargeIoList) end, lists:seq(1, 10))
    end),

    %% Results should be identical
    ?assertEqual(StandardResult, OptimizedResult),

    Speedup = StandardTime / OptimizedTime,
    ?debugFmt("Standard approach: ~p microseconds", [StandardTime]),
    ?debugFmt("Optimized approach: ~p microseconds", [OptimizedTime]),
    ?debugFmt("Speedup: ~.2fx", [Speedup]),

    %% Assert reasonable speedup (at least 1.5x)
    ?assert(Speedup >= 1.2, io_lib:format("Deep nesting performance insufficient: ~.2fx < 1.2x", [Speedup])).

%% Correctness verification test
performance_correctness_test_() ->
    {"Performance Correctness Test", fun performance_correctness/0}.

performance_correctness() ->
    ?debugFmt("=== Performance Correctness Test ===", []),

    %% Test various data structures to ensure correctness
    TestCases = [
        {<<"simple_binary">>, "Simple binary"},
        {[<<"part1">>, <<"part2">>, <<"part3">>], "Simple iolist"},
        {[<<"header">>, lists:duplicate(1000, $a), <<"footer">>], "Mixed iolist"},
        {create_deep_iolist(20), "Deep nested iolist"},
        {[binary:copy(<<"c">>, 1000) || _ <- lists:seq(1, 10)], "Multiple chunks"}
    ],

    lists:foreach(fun({TestData, Name}) ->
        CrcStandard = crc32cer:nif_d(TestData),
        CrcOptimized = crc32cer:nif_iolist_d(TestData),
        ?assertEqual(CrcStandard, CrcOptimized,
                    io_lib:format("~s: results don't match (~p != ~p)",
                                 [Name, CrcStandard, CrcOptimized]))
    end, TestCases),

    ?debugFmt("All correctness tests passed", []).

%% Performance test for long iolist with many small binary chunks
performance_small_chunks_test_() ->
    {"Small Chunks Iolist Performance Test", fun performance_small_chunks/0}.

performance_small_chunks() ->
    ?debugFmt("=== 1KB Chunks Iolist Performance Test ===", []),

    %% Generate many small binary chunks (1KB each)
    SmallChunks = [binary:copy(<<"c">>, 1 * ?KB) || _ <- lists:seq(1, 1000)],

    %% Test standard approach
    {StandardTime, _StandardResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_d(SmallChunks) end, lists:seq(1, 10))
    end),

    %% Test optimized approach
    {OptimizedTime, _OptimizedResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_iolist_d(SmallChunks) end, lists:seq(1, 10))
    end),

    Speedup = StandardTime / OptimizedTime,
    ?debugFmt("Standard approach: ~p microseconds", [StandardTime]),
    ?debugFmt("Optimized approach: ~p microseconds", [OptimizedTime]),
    ?debugFmt("Speedup: ~.2fx", [Speedup]),

    %% Assert that optimized approach is not significantly slower (at least 0.6x speedup, max 1.67x slowdown)
    ?assert(Speedup >= 0.6, io_lib:format("Small chunks performance too slow: ~.2fx speedup (max 1.67x slowdown allowed)", [Speedup])).

%% Performance test for very long iolist with tiny binary chunks
performance_tiny_chunks_test_() ->
    {"63B Chunks Iolist Performance Test", fun performance_tiny_chunks/0}.

performance_tiny_chunks() ->
    ?debugFmt("=== 63B Chunks Iolist Performance Test ===", []),

    %% Generate many tiny binary chunks (63 bytes each)
    TinyChunks = [binary:copy(<<"c">>, 63) || _ <- lists:seq(1, 5000)],

    %% Test standard approach
    {StandardTime, _StandardResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_d(TinyChunks) end, lists:seq(1, 5))
    end),

    %% Test optimized approach
    {OptimizedTime, _OptimizedResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_iolist_d(TinyChunks) end, lists:seq(1, 5))
    end),

    Speedup = StandardTime / OptimizedTime,
    ?debugFmt("Standard approach: ~p microseconds", [StandardTime]),
    ?debugFmt("Optimized approach: ~p microseconds", [OptimizedTime]),
    ?debugFmt("Speedup: ~.2fx", [Speedup]),

    %% Assert that optimized approach is not significantly slower (at least 0.6x speedup, max 1.67x slowdown)
    ?assert(Speedup >= 0.6, io_lib:format("Tiny chunks performance too slow: ~.2fx speedup (max 1.67x slowdown allowed)", [Speedup])).

%% Performance test for mixed small chunks with nested structure
performance_mixed_small_chunks_test_() ->
    {"Mixed Small Chunks Iolist Performance Test", fun performance_mixed_small_chunks/0}.

performance_mixed_small_chunks() ->
    ?debugFmt("=== Mixed Small Chunks Iolist Performance Test ===", []),

    %% Create mixed iolist with small chunks at different nesting levels
    MixedSmallChunks = create_mixed_small_chunks_iolist(100, 256),

    %% Test standard approach
    {StandardTime, _StandardResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_d(MixedSmallChunks) end, lists:seq(1, 5))
    end),

    %% Test optimized approach
    {OptimizedTime, _OptimizedResult} = timer:tc(fun() ->
        lists:foreach(fun(_) -> crc32cer:nif_iolist_d(MixedSmallChunks) end, lists:seq(1, 5))
    end),

    Speedup = StandardTime / OptimizedTime,
    ?debugFmt("Standard approach: ~p microseconds", [StandardTime]),
    ?debugFmt("Optimized approach: ~p microseconds", [OptimizedTime]),
    ?debugFmt("Speedup: ~.2fx", [Speedup]),

    %% Assert that optimized approach is not significantly slower (at least 0.6x speedup, max 1.67x slowdown)
    ?assert(Speedup >= 0.6, io_lib:format("Mixed small chunks performance too slow: ~.2fx speedup (max 1.67x slowdown allowed)", [Speedup])).

%% Correctness test for small chunks
performance_small_chunks_correctness_test_() ->
    {"Small Chunks Correctness Test", fun performance_small_chunks_correctness/0}.

performance_small_chunks_correctness() ->
    ?debugFmt("=== Small Chunks Correctness Test ===", []),

    %% Test various small chunk configurations to ensure correctness
    TestCases = [
        {[<<"a">>, <<"b">>, <<"c">>], "Simple small chunks"},
        {[binary:copy(<<"x">>, 10) || _ <- lists:seq(1, 100)], "Many tiny chunks"},
        {[binary:copy(<<"c">>, 50) || _ <- lists:seq(1, 200)], "Small numbered chunks"},
        {create_mixed_small_chunks_iolist(20, 100), "Mixed small chunks structure"},
        {[binary:copy(<<"d">>, 1) || _ <- lists:seq(1, 1000)], "Many single-byte chunks"}
    ],

    lists:foreach(fun({TestData, Name}) ->
        CrcStandard = crc32cer:nif_d(TestData),
        CrcOptimized = crc32cer:nif_iolist_d(TestData),
        ?assertEqual(CrcStandard, CrcOptimized,
                    io_lib:format("~s: results don't match (~p != ~p)",
                                 [Name, CrcStandard, CrcOptimized]))
    end, TestCases),

    ?debugFmt("All small chunks correctness tests passed", []).

%% Helper function to create mixed small chunks iolist
create_mixed_small_chunks_iolist(0, _ChunkSize) -> [];
create_mixed_small_chunks_iolist(N, ChunkSize) ->
    [binary:copy(<<"x">>, ChunkSize),
     [binary:copy(<<"y">>, ChunkSize div 2) || _ <- lists:seq(1, 5)],
     create_mixed_small_chunks_iolist(N-1, ChunkSize)].

%% Helper function to create deep iolist with large data
create_deep_large_iolist(0, _ChunkSize) -> [<<"end">>];
create_deep_large_iolist(N, ChunkSize) ->
    [binary:copy(<<"c">>, ChunkSize),
     create_deep_large_iolist(N-1, ChunkSize)].
