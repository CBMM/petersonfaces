#!/bin/sh

if ! command -v nix-shell >/dev/null ; then
  echo Setting up environment
  . ~/.nix-profile/etc/profile.d/nix.sh
fi

deps/reflex-platform/
