#!/usr/bin/env sh

cspell-cli \
    --config .cspell/cspell.json \
    **/*.hs \
    **/*.md \
    **/*.json \
    **/*.toml \
    **/*.yml \
    **/*.yaml
