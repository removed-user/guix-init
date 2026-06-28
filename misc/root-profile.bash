#!/bin/bash
    echo "Linking the root user's profile"
    mkdir -p ~root/.config/guix
    ln -sf /var/guix/profiles/per-user/root/current-guix \
       ~root/.config/guix/current

    GUIX_PROFILE=~root/.config/guix/current
    # The profile just prepends to search paths, which is not needed for
    # effective linting.
    # shellcheck disable=SC1091
    source "${GUIX_PROFILE}/etc/profile"
    echo "activated root profile at ${GUIX_PROFILE}"
