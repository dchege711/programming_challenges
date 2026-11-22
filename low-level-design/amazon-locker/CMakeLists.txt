cmake_minimum_required(VERSION 4.4.5)

# Identify the project and enable C++ compiler detection.
project(amazon_locker LANGUAGES CXX)

# Require C++23 rather than allowing fallback to an older language standard.
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Export compiler flags and include paths so clangd can match the actual build.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Use standard C++ instead of compiler-specific language extensions.
set(CMAKE_CXX_EXTENSIONS OFF)

# Build reusable implementation code without requiring an application main().
add_library(amazon_locker STATIC
  src/access_token.cc
  src/access_token.h
  src/amazon_locker_with_size_fallback.cc
  src/amazon_locker_with_size_fallback.h
  src/amazon_locker.cc
  src/amazon_locker.h
  src/compartment.cc
  src/compartment.h
)

# Expose headers and the C++20 requirement to targets that link this library.
target_include_directories(amazon_locker PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/src")
target_compile_features(amazon_locker PUBLIC cxx_std_20)

# Allow CMake to download and configure the test dependency during configuration.
include(FetchContent)

# Pin GoogleTest v1.17.0 to an exact revision for consistent dependency sources.
# https://github.com/google/googletest/releases
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/52eb8108c5bdec04579160ae17225d66034bd723.zip
)
# Download GoogleTest and make its CMake targets available for linking.
FetchContent_MakeAvailable(googletest)

# Enable CTest registration so tests can run through the ctest command.
enable_testing()

# Build the test runner separately from the implementation library.
add_executable(
    amazon_locker_test
    test/amazon_locker_test.cc
    test/amazon_locker_with_size_fallback_test.cc
)
# Link the implementation and GoogleTest's assertions and provided main().
target_link_libraries(amazon_locker_test PRIVATE amazon_locker GTest::gtest_main)

# Register individual GoogleTest cases with CTest after building the runner.
include(GoogleTest)
gtest_discover_tests(amazon_locker_test)
