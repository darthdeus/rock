#!/bin/bash

~/projects/rock/target/debug/rock-lsp "$@" \
  2> >(stdbuf -o0 sed 's/\\n/\n/g' | tee -a /tmp/rock-lsp-stderr.log >&2) \
  | tee -a /tmp/rock-lsp-stdout.log
