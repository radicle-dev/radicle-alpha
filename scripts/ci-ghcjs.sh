#!/bin/bash

set -euo pipefail

export PATH=$(stack path --bin-path)
export STACK_YAML=stack-ghcjs.yaml

stack build

