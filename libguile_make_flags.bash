#!/bin/bash
#  `configure' configurer
#  
#  Usage: ./configure [OPTION]... [VAR=VALUE]...
#  
#  To assign environment variables (e.g., CC, CFLAGS...), specify them as
#  VAR=VALUE.  See below for descriptions of some of the useful variables.
#  
#  Defaults for the options are specified in brackets.
#  
#  Configuration:
#    -h, --help              display this help and exit
#        --help=short        display options specific to this package
#        --help=recursive    display the short help of all the included packages
#    -V, --version           display version information and exit
#    -q, --quiet, --silent   do not print `checking ...' messages
#        --cache-file=FILE   cache test results in FILE [disabled]
#    -C, --config-cache      alias for `--cache-file=config.cache'
#    -n, --no-create         do not create output files
#        --srcdir=DIR        find the sources in DIR [configure dir or `..']

# Fine tuning of the installation directories:
function printArgs() {
declare -a  DIRECTORIES=()
DIRECTORIES+=' --bindir=/bin'
DIRECTORIES+=' --sbindir=sbin'
DIRECTORIES+=' --libexecdir=/usr/libexec'
DIRECTORIES+=' --sysconfdir=/etc'
DIRECTORIES+=' --localstatedir=/var'
DIRECTORIES+=' --libdir=/lib'
DIRECTORIES+=' --includedir=/usr/include'
DIRECTORIES+=' --oldincludedir=/usr/include'
DIRECTORIES+=' --datarootdir=/usr/share'
DIRECTORIES+=' --datadir=/usr/share'
DIRECTORIES+=' --infodir=/usr/share/info'
DIRECTORIES+=' --localedir=/usr/share/locales'
DIRECTORIES+=' --mandir=/usr/share/man'
DIRECTORIES+=' --docdir=/usr/share/doc/guile'
# System types:
# --build=BUILD                                # configure for building on BUILD [guessed]
# --host=HOST                                  # cross-compile to build programs to run on HOST [BUILD]
# --target=TARGET                              # configure for building compilers for TARGET [HOST]



# Optional Features:
declare -a FEATURES=()
FEATURES+=' --with-pic'
FEATURES+=' --enable-lto'
FEATURES+=' --with-threads'
FEATURES+=' --enable-static=yes'
# do not use Native Language Support
FEATURES+=' --disable-nls'
# omit POSIX tmpnam
FEATURES+=' --disable-tmpnam'
# use mini-gmp instead of full GMP library
FEATURES+=' --enable-mini-gmp'
# enable just-in-time code generation [default=auto]
FEATURES+=' --enable-jit=auto'
FEATURES+=' --disable-deprecated'
FEATURES+=' --disable-silent-rules'
FEATURES+=' --enable-error-on-warning'
FEATURES+=' --enable-ld-version-script'
# specify policy for cross-compilation guesses
FEATURES+=' --enable-cross-guesses=conservative'
# optimize for fast installation [default=yes]
FEATURES+=' --enable-fast-install=yes'


# Unlikely flags
  # --disable-FEATURE                            # do not include FEATURE (same as --enable-FEATURE=no)
  # --enable-FEATURE[=ARG]                       # include FEATURE [ARG=yes]
  # --enable-dependency-tracking                 # do not reject slow dependency extractors
  # --disable-dependency-tracking                # speeds up one-time build
  # --enable-shared[=PKGS]                       # build shared libraries [default=yes]
  # --enable-year2038                            # support timestamps after 2038


# Optional Packages:
  # --with-PACKAGE[=ARG]                         # use PACKAGE [ARG=yes]
  # --without-PACKAGE                            # do not use PACKAGE (same as --with-PACKAGE=no)
  # --with-pkgconfigdir                          # pkg-config installation directory   ['${libdir}/pkgconfig']
  # --with-modules=FILES                         # Add support for dynamic modules
  # --with-bdw-gc=PKG                            # name of BDW-GC pkg-config file (boehm / libgc)

# Unlikely or bad flags
  # --with-gnu-ld                              # assume the C compiler uses GNU ld [default=no]
  # --disable-libtool-lock                     # avoid locking (might break parallel builds)
  # --enable-guile-debug                       # include internal "C-level" debugging functions
  # --disable-networking                       # omit networking interfaces
  # --disable-posix                            # omit non-essential POSIX interfaces
  # --disable-regex                            # omit regular expression interfaces
  # --disable-option-checking                  # ignore unrecognized --enable/--with options
  # --enable-silent-rules                      # less verbose build output (undo: "make V=1")
  # --with-gnu-ld                              # assume the C compiler uses GNU ld [default=no]
  # --without-included-regex                   # don't compile regex; default recent GNU C Lib (breaks musl)
  # --with-sysroot[=DIR]                       # Search for dependent libraries within sysroot DIR 


# Don't search for $lib in includedir and libdir
declare -a WITHOUT=()
WITHOUT+=' --without-libgmp-prefix'              # libgmp (Gnu Multiple Precision Arithmetic, )
WITHOUT+=' --without-libintl-prefix'             # libintl (part of gnu gettext, swaps words from ISO-x to ISO-y)
# '--without-libiconv-prefix'                    # libiconv (Converts byte-encodings to UTF-8)
# '--without-libreadline-prefix'                 # libreadline (usually shell builtin, def-keep)
# '--without-libunistring-prefix'                # libunistring (linguistic math to manipilate unicode strings)

#  Search for $lib in DIR/include and DIR/lib
#       '--with-libgmp-prefix[=DIR]'             # libgmp
#      '--with-libintl-prefix[=DIR]'             # libintl
#     '--with-libiconv-prefix[=DIR]'             # libiconv 
#  '--with-libreadline-prefix[=DIR]'             # libreadline
# '--with-libunistring-prefix[=DIR]'             # libunistring

# Some influential environment variables:
    #  CC
    #  CPP
    #  CFLAGS
    #  CPPFLAGS
    #  LIBFFI_CFLAGS               C compiler flags for LIBFFI, overriding pkg-config
    #  BDW_GC_CFLAGS               C compiler flags for BDW_GC, overriding pkg-config
    #  LDFLAGS
    #  LIBFFI_LIBS                 linker flags for LIBFFI, overriding pkg-config
    #  BDW_GC_LIBS                 linker flags for BDW_GC, overriding pkg-config
    #  LIBS                        libraries to pass to the linker, e.g. -l<library>
    #  LT_SYS_LIBRARY_PATH         User-defined run-time library search path.

    #  PKG_CONFIG                  Path to pkg-config utility
    #  PKG_CONFIG_PATH             Add to pkg-config's search path
    #  PKG_CONFIG_LIBDIR           Override pkg-config's built-in search path

    #  EMACS                       The Emacs editor command
    #  EMACSLOADPATH               The Emacs library search path

    #  CC_FOR_BUILD                build system C compiler
    #  GUILE_FOR_BUILD             Guile for the build system

# printf '%s\n'    ${WITHOUT[@]}
# printf '%s\n'    ${FEATURES[@]}
# printf '%s\n' ${DIRECTORIES[@]}
# echo "${WITHOUT[@]}" "${FEATURES[@]}" "${DIRECTORIES[@]}"
# All_Args=$(printf '%s\n' $(printf '%s\n' "${WITHOUT[@]}" "${FEATURES[@]}" "${DIRECTORIES[@]}"))
Clean_Args=$(echo $(printf '%s\n' $(printf '%s\n' "${WITHOUT[@]}" "${FEATURES[@]}" "${DIRECTORIES[@]}")))
 echo "${Clean_Args}"
# declare -gx libguile_config_flags="$(echo "${WITHOUT[@]}" "${FEATURES[@]}" "${DIRECTORIES[@]}")"
}
printArgs
