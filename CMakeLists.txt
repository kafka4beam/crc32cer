cmake_minimum_required(VERSION 3.16)

set(CMAKE_BUILD_TYPE "RelWithDebInfo")

project(crc32cer_nif)
set(SOURCES
  c_src/crc32c_nif.c
)

SET(CMAKE_EXPORT_COMPILE_COMMANDS TRUE)
SET(Erlang_EI_INCLUDE_DIRS   ${Erlang_OTP_LIB_DIR}/${Erlang_EI_DIR}/include)
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/priv/)
# OTP limitation
SET(CMAKE_SHARED_LIBRARY_SUFFIX ".so")

# For cerl picking up the OTP_ROOT
if (DEFINED ENV{Erlang_OTP_ROOT_DIR})
  SET(Erlang_OTP_ROOT_DIR $ENV{Erlang_OTP_ROOT_DIR})
else()
EXECUTE_PROCESS(
        COMMAND         erl -noshell -eval "io:format(\"~s\", [code:root_dir()])" -s erlang halt
        OUTPUT_VARIABLE Erlang_OTP_ROOT_DIR
        )
endif()

add_library(crc32cer_nif SHARED ${SOURCES})
target_include_directories(crc32cer_nif PRIVATE ${Erlang_OTP_ROOT_DIR}/usr/include/)

set(CMAKE_CXX_EXTENSIONS ON)
set_property(GLOBAL  PROPERTY CXX_STANDARD 11)

include(ExternalProject)
ExternalProject_Add(google-crc32c
         GIT_REPOSITORY "https://github.com/google/crc32c.git"
         GIT_TAG "1.1.2"
         GIT_SHALLOW 1
         GIT_SUBMODULES_SHALLOW 1
         PREFIX "${CMAKE_BINARY_DIR}/external"
         SOURCE_DIR "${CMAKE_BINARY_DIR}/external/crc32c"
         CMAKE_ARGS -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCRC32C_INSTALL=ON -DCRC32C_BUILD_TESTS=OFF -DCRC32C_BUILD_BENCHMARKS=OFF -DCRC32C_USE_GLOG=0
         BUILD_COMMAND ${CMAKE_COMMAND} --build .
         PATCH_COMMAND patch -N -p1 < ${PROJECT_SOURCE_DIR}/crc32c_cmake.patch || true
         INSTALL_COMMAND ""
         )

# Set appropriate include and link directories
include_directories(${CMAKE_BINARY_DIR}/external/crc32c/include)
add_library(crc32c SHARED IMPORTED)

# Specify the dependency on the external library
add_dependencies(crc32cer_nif google-crc32c)
set_property(TARGET crc32c PROPERTY IMPORTED_LOCATION ${CMAKE_BINARY_DIR}/external/src/google-crc32c-build/libcrc32c.a)

if (CMAKE_SYSTEM_NAME MATCHES Darwin)
  target_link_libraries(crc32cer_nif PRIVATE crc32c c++ "-Wl,-undefined,dynamic_lookup")
else()
 target_link_libraries(crc32cer_nif crc32c)
endif()

install(TARGETS crc32cer_nif LIBRARY DESTINATION ${PROJECT_SOURCE_DIR}/priv/)

set_target_properties(crc32cer_nif
    PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/priv/
)

set_target_properties(crc32c
    PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/priv/
)
