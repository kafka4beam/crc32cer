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

## 📊 Benchmarks for `nif_iolist_d`

Performance comparison on x86_64 with SSE4.2 (based on actual test results):

### Large Binary Batch Performance
| Test Scenario | Standard API | Optimized API | Speedup |
|---------------|-------------|---------------|---------|
| 10 chunks × 200KB (2MB total) | 5.52ms | 2.12ms | **2.60x** |
| 50 chunks × 200KB (10MB total) | 9.43ms | 0.87ms | **10.89x** |

### Deep Nesting Performance
| Test Scenario | Standard API | Optimized API | Speedup |
|---------------|-------------|---------------|---------|
| 128 levels × 10KB (1.28MB total) | 1.996ms | 1.378ms | **1.45x** |

### Small Chunks Performance
| Test Scenario | Standard API | Optimized API | Speedup |
|---------------|-------------|---------------|---------|
| 1000 chunks × 1KB (1MB total) | 1.83ms | 1.01ms | **1.81x** |
| 5000 chunks × 63B (315KB total) | 1.64ms | 1.83ms | **0.90x** |
| Mixed small chunks (256B each) | 0.23ms | 0.18ms | **1.22x** |

*Results based on comprehensive testing with various data patterns and sizes. Performance may vary based on hardware and data characteristics.*
