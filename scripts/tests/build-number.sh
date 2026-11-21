#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd -P)"
TEST_ROOT="$(mktemp -d)"
trap 'rm -rf -- "$TEST_ROOT"' EXIT
mkdir -p "$TEST_ROOT/repo/scripts" "$TEST_ROOT/bin"
cp "$REPO_ROOT/build" "$REPO_ROOT/release" "$TEST_ROOT/repo/"
cp "$REPO_ROOT/scripts/common" "$TEST_ROOT/repo/scripts/"
cat > "$TEST_ROOT/bin/dotnet" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$BUILD_NUMBER_TEST_LOG"
if [[ "$1" == msbuild ]]; then printf '3.0.0\n'; fi
STUB
chmod +x "$TEST_ROOT/bin/dotnet"
export PATH="$TEST_ROOT/bin:$PATH"
export BUILD_NUMBER_TEST_LOG="$TEST_ROOT/dotnet.log"

for script in build release; do
    for invalid in invalid -2 1.5 +1 '1 2' 0x10 ''; do
        : > "$BUILD_NUMBER_TEST_LOG"
        if bash "$TEST_ROOT/repo/$script" --version-suffix ci --build-number "$invalid" > "$TEST_ROOT/output.log" 2>&1; then
            printf 'Expected %s to reject build number: %s\n' "$script" "$invalid" >&2
            exit 1
        fi
        [[ ! -s "$BUILD_NUMBER_TEST_LOG" ]]
    done
    for pair in '-1:omit' '0:00000' '42:00042' '00042:00042' '000000:00000' '123456:123456' '999999999999999999999:999999999999999999999'; do
        number="${pair%%:*}"
        expected="${pair#*:}"
        : > "$BUILD_NUMBER_TEST_LOG"
        bash "$TEST_ROOT/repo/$script" --version-suffix ci --build-number "$number" > "$TEST_ROOT/output.log" 2>&1
        if [[ "$expected" == omit ]]; then
            ! grep -q -- '-p:BuildNumber=' "$BUILD_NUMBER_TEST_LOG"
            grep -q -- '-p:PackageVersion=3.0.0-ci' "$BUILD_NUMBER_TEST_LOG"
        else
            grep -q -- "-p:BuildNumber=$expected" "$BUILD_NUMBER_TEST_LOG"
            grep -q -- "-p:PackageVersion=3.0.0-ci-$expected" "$BUILD_NUMBER_TEST_LOG"
        fi
    done
done
printf 'Build-number validation tests passed.\n'
