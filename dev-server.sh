#!/usr/bin/env bash
port=${1:-8000}
ghcid -W --test "devMainAutoReload staticServer mainW $port"
# ghcid -W --test "main"
