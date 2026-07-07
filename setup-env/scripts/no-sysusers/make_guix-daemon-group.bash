
PAS=$'[ \033[32;1mPASS\033[0m ] '
ERR=$'[ \033[31;1mFAIL\033[0m ] '
WAR=$'[ \033[33;1mWARN\033[0m ] '
INF="[ INFO ] "

DEBUG=0

_err()
{ # All errors go to stderr.
    printf "[%s]: ${ERR}%s\n" "$(date +%s.%3N)" "$1"
}

_msg()
{ # Default message to stdout.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

_msg_pass()
{
    _msg "$PAS$1"
}

_msg_warn()
{
    _msg "$WAR$1"
}

_msg_info()
{
    _msg "$INF$1"
}

_debug()
{
    if [ "${DEBUG}" = '1' ]; then
        printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
    fi
}

die()
{
    _err "$*"
    exit 1
}

# Return true if user answered yes, false otherwise.  The prompt is
# yes-biased, that is, when the user simply enter newline, it is equivalent to
# answering "yes".
# $1: The prompt question.
prompt_yes_no() {
    local -l yn
    read -rp "$1 [Y/n]" yn
    [[ ! $yn || $yn = y || $yn = yes ]] || return 1
}

chk_require()
{ # Check that every required command is available.
    declare -a warn
    local c

    _debug "--- [ ${FUNCNAME[0]} ] ---"

    for c in "$@"; do
        command -v "$c" &>/dev/null || warn+=("$c")
    done

    [ "${#warn}" -ne 0 ] && die "Missing commands: ${warn[*]}."

    _msg_pass "verification of required commands completed"
}

	if getent group guix-daemon > /dev/null; then
	    _msg_info "group guix-daemon exists"
	else
	    groupadd --system guix-daemon
	    _msg_pass "group guix-daemon created"
	fi
