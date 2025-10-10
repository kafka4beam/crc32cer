# crc32cer - High-Performance CRC32C NIF for Erlang/OTP

A Erlang NIF (Native Implemented Function) library for CRC32C (Castagnoli) checksum calculation, built on top of Google's [crc32c](https://github.com/google/crc32c) library with hardware acceleration support.

## 🔧 Usage

### Basic Usage

```erlang
%% Simple binary checksum
Crc = crc32cer:nif(<<"hello world">>).

%% With custom initial CRC
Crc = crc32cer:nif(16#12345678, <<"hello world">>).

%% Iolist processing
IoList = [<<"hello">>, " ", <<"world">>],
Crc = crc32cer:nif(IoList).
```

## 🛠️ API Reference

### Standard APIs

#### `nif/1`
```erlang
-spec nif(iodata()) -> non_neg_integer().
```
Calculate CRC32C checksum with initial CRC of 0.

#### `nif/2`
```erlang
-spec nif(integer(), iodata()) -> non_neg_integer().
```
Calculate CRC32C checksum with custom initial CRC.

### Dirty Scheduler APIs

#### `nif_d/1`
```erlang
-spec nif_d(iodata()) -> non_neg_integer().
```
CPU-intensive version of `nif/1` that runs on dirty schedulers.

#### `nif_d/2`
```erlang
-spec nif_d(integer(), iodata()) -> non_neg_integer().
```
CPU-intensive version of `nif/2` that runs on dirty schedulers.

### Optimized APIs

#### `nif_iolist_d/1`
```erlang
-spec nif_iolist_d(iodata()) -> non_neg_integer().
```
Optimized for batches of large binary chunks with initial CRC of 0.

#### `nif_iolist_d/2`
```erlang
-spec nif_iolist_d(integer(), iodata()) -> non_neg_integer().
```
Optimized for batches of large binary chunks with custom initial CRC.

## 📄 License

Licensed under the Apache License 2.0. See [LICENSE](LICENSE) for details.

## 📊 Benchmarks for `nif_iolist` vs `nif`

Performance comparison on x86_64 with SSE4.2 (based on 10-run average test results, 100 iterations per scenario):

### Large Binary Batch Performance
| Test Scenario | Standard (nif) | Optimized (nif_iolist) | Speedup | Winner |
|---------------|----------------|------------------------|---------|---------|
| 10 chunks × 200KB (2MB total) | 25.43 ± 1.06ms | 9.91 ± 0.02ms | **2.57x** | nif_iolist |
| 50 chunks × 200KB (10MB total) | 141.56 ± 2.29ms | 51.10 ± 2.95ms | **2.78x** | nif_iolist |

### Deep Nesting Performance
| Test Scenario | Standard (nif) | Optimized (nif_iolist) | Speedup | Winner |
|---------------|----------------|------------------------|---------|---------|
| 128 levels × 10KB (1.28MB total) | 16.55 ± 0.26ms | 7.62 ± 0.03ms | **2.17x** | nif_iolist |

### Small Chunks Performance
| Test Scenario | Standard (nif) | Optimized (nif_iolist) | Speedup | Winner |
|---------------|----------------|------------------------|---------|---------|
| 1000 chunks × 1KB (1MB total) | 14.96 ± 0.08ms | 9.82 ± 0.02ms | **1.52x** | nif_iolist |
| 5000 chunks × 63B (315KB total) | 14.55 ± 6.66ms | 25.08 ± 3.16ms | **0.59x** | **nif** |
| Mixed small chunks (256B each) | 2.06 ± 0.37ms | 3.46 ± 0.48ms | **0.60x** | **nif** |

*Results based on 10-run average testing with various data patterns and sizes. Performance may vary based on hardware and data characteristics. Both approaches run on regular schedulers for fair comparison. The optimized approach shows significant improvements for large chunks and deep nesting, but may be slower for very small chunks due to additional overhead.*

**Important Note**: The key point of this benchmark test is not to prove that `nif_iolist` is more performant than `nif` in computing CRC32C itself, but to demonstrate that:
1. Performance does not get significantly worse for main use cases
2. It does not create intermediate binaries which may add system load not visible in benchmark tests (reducing memory pressure and GC overhead)
3. Both approaches run on regular schedulers for fair comparison (no dirty scheduler overhead)
