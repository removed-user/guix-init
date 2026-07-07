#!/bin/bash
function make_dirs(){

local PATHS=(
'/var/guix/profiles/per-user/root'
'/var/log/guix'
'/gnu/store'
'/var/guix/daemon-socket/'
'/etc/guix/'
'/etc/guix/channels.d'
)


for path in "${PATHS}"
do 
if [[ ! -d "${path}" ]] then
 mkdir -p "${path}"
fi
done
}
make_dirs

