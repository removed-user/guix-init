#!/bin/bash
# rg guix /proc/*/cmdline 2>/dev/null | grep 34 | cut -d / -f3 | kill
guix-daemon --build-users-group=guixbuild --listen=/var/guix/daemon-socket/socket --system=x86_64-linux --discover=no --no-substitutes -c 0 &
