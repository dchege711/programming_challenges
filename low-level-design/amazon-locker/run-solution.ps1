$ErrorActionPreference = 'Stop' # Only covers PowerShell errors.

clang-format -i src/*.h src/*.cc test/*.cc

cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

cmake --build build
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

clang-tidy -p build src/*.cc test/*.cc
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

ctest --test-dir build --output-on-failure
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
