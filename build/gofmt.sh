#!/bin/bash

IFS=$'\n'
files=( $(git status --porcelain --untracked-files=all | sed 's/^...//' | grep '\.go$' || true) )
unset IFS

for f in "${files[@]}"; do
    if [ -e "$f" ]; then
        gofmt -s -w "$f"
    fi
done
