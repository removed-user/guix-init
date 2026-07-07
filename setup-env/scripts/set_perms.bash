#!/bin/bash
function set_owner() {
	function var-log-guix(){
chown -R guix-daemon:guix-daemon /gnu /var/guix
chown guix-daemon:guix-daemon /var/log/guix
chmod 755 /var/log/guix
}
var-log-guix

	function daemon-socket(){
chown guix-daemon:guix-daemon /var/guix/daemon-socket/
chmod 755 /var/guix/daemon-socket/
}
daemon-socket

chown -R root:root /var/guix/profiles/per-user/root
}
set_owner
