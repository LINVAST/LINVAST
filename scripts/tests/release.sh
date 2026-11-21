#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd -P)"
TEST_ROOT="$(mktemp -d)"
trap 'rm -rf -- "$TEST_ROOT"' EXIT

# Exercise a disposable checkout with a fake dotnet command. Never run release
# against the real repository or invoke a real build from this safety test.
mkdir -p "$TEST_ROOT/repo/scripts" "$TEST_ROOT/bin"
cp "$REPO_ROOT/release" "$TEST_ROOT/repo/release"
cp "$REPO_ROOT/scripts/common" "$TEST_ROOT/repo/scripts/common"
printf 'keep repository\n' > "$TEST_ROOT/repo/sentinel"
printf 'keep parent\n' > "$TEST_ROOT/sentinel"

cat > "$TEST_ROOT/bin/dotnet" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$RELEASE_TEST_LOG"
if [[ "$1" == pack ]]; then
    while (($#)); do
        if [[ "$1" == -o ]]; then
            printf 'package\n' > "$2/LINVAST.test.nupkg"
            exit 0
        fi
        shift
    done
    exit 1
fi
STUB
chmod +x "$TEST_ROOT/bin/dotnet"
export PATH="$TEST_ROOT/bin:$PATH"
export RELEASE_TEST_LOG="$TEST_ROOT/dotnet.log"

expect_rejection() {
    : > "$RELEASE_TEST_LOG"
    if bash "$TEST_ROOT/repo/release" --artifacts "$1" > "$TEST_ROOT/output.log" 2>&1; then
        printf 'Expected rejection of artifact directory: %s\n' "$1" >&2
        exit 1
    fi
    [[ ! -s "$RELEASE_TEST_LOG" ]]
    [[ -f "$TEST_ROOT/repo/sentinel" && -f "$TEST_ROOT/sentinel" ]]
}

# A fake rm ensures even a regression that tries to delete / is harmless. It
# records the attempt as a failure instead of allowing any recursive deletion.
cat > "$TEST_ROOT/bin/rm" <<'STUB'
#!/usr/bin/env bash
printf 'Unexpected removal attempt: %s\n' "$*" >&2
printf 'rm %s\n' "$*" >> "$RELEASE_TEST_LOG"
exit 1
STUB
chmod +x "$TEST_ROOT/bin/rm"
# The test's own cleanup bypasses the fake command.
trap '/usr/bin/rm -rf -- "$TEST_ROOT"' EXIT

expect_rejection "$TEST_ROOT/repo"
expect_rejection .
expect_rejection artifacts/..
expect_rejection "$TEST_ROOT"
expect_rejection ..
expect_rejection /
ln -s "$TEST_ROOT/repo" "$TEST_ROOT/repo-alias"
expect_rejection "$TEST_ROOT/repo-alias"
ln -s "$TEST_ROOT" "$TEST_ROOT/parent-alias"
expect_rejection "$TEST_ROOT/parent-alias"

mkdir -p "$TEST_ROOT/repo/artifacts"
printf 'unrelated\n' > "$TEST_ROOT/repo/artifacts/sentinel"
printf 'older package\n' > "$TEST_ROOT/repo/artifacts/old.nupkg"
bash "$TEST_ROOT/repo/release" > "$TEST_ROOT/output.log" 2>&1
[[ -f "$TEST_ROOT/repo/artifacts/LINVAST.test.nupkg" ]]
[[ "$(cat "$TEST_ROOT/repo/artifacts/sentinel")" == unrelated ]]
[[ "$(cat "$TEST_ROOT/repo/artifacts/old.nupkg")" == 'older package' ]]

# Repeated releases, paths containing spaces, and ordinary symlink outputs work.
bash "$TEST_ROOT/repo/release" > "$TEST_ROOT/output.log" 2>&1
bash "$TEST_ROOT/repo/release" --artifacts "$TEST_ROOT/output with spaces" > "$TEST_ROOT/output.log" 2>&1
[[ -f "$TEST_ROOT/output with spaces/LINVAST.test.nupkg" ]]
ln -s "$TEST_ROOT/output with spaces" "$TEST_ROOT/output-alias"
bash "$TEST_ROOT/repo/release" --artifacts "$TEST_ROOT/output-alias" > "$TEST_ROOT/output.log" 2>&1
[[ -f "$TEST_ROOT/output with spaces/LINVAST.test.nupkg" ]]

printf 'Release directory safety tests passed.\n'
