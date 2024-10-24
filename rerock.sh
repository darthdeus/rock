#!/bin/bash
set -e

SCRIPT_DIR="$(dirname "$(realpath "$0")")"

# Determine the correct version of stdbuf based on the operating system
if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  if ! command -v gstdbuf &> /dev/null; then
    echo "Error: gstdbuf is not installed. Install it using 'brew install coreutils'."
    exit 1
  fi
  STDBUF="gstdbuf"
else
  # Linux (or any other OS)
  if ! command -v stdbuf &> /dev/null; then
    echo "Error: stdbuf is not installed."
    exit 1
  fi
  STDBUF="stdbuf"
fi

# Log both stdout and stderr
# $STDBUF -o0 tee -a /tmp/rock-lsp-stdin.log | $STDBUF -o0 "$SCRIPT_DIR/target/debug/rock-lsp" "$@" \
#   > >($STDBUF -o0 tee -a /tmp/rock-lsp-stdout.log) \
#   2> >($STDBUF -o0 sed 's/\\n/\n/g' | $STDBUF -o0 tee -a /tmp/rock-lsp-stderr.log >&2)

# tee -a /tmp/rock-lsp-stdin.log | "$SCRIPT_DIR/target/debug/rock-lsp" "$@" \
#   > >(tee -a /tmp/rock-lsp-stdout.log) \
#   2> >(sed 's/\\n/\n/g' | tee -a /tmp/rock-lsp-stderr.log >&2)


# $STDBUF -o0 tee -a /tmp/rock-lsp-stdin.log | $STDBUF -o0 ./target/debug/stick client ./target/debug/rock-lsp
# $STDBUF -o0 tee -a /tmp/rock-lsp-stdin.log | $STDBUF -o0 ./target/debug/rock-lsp $@
# ./target/debug/rock-lsp

# tee -a /tmp/rock-lsp-stdin.log | ./target/debug/stick client ./target/debug/rock-lsp
./target/debug/stick client ./target/debug/rock-lsp

