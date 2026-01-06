#!/bin/sh
#
# Configure CMake build for hackney QUIC NIF
# Supports: Linux, macOS (amd64/arm64), FreeBSD, NetBSD, OpenBSD, Solaris
#
# Environment variables:
#   HACKNEY_QUIC_OPTS - Additional CMake options
#

set -e

# Check if QUIC dependencies are available (lsquic + BoringSSL in c_src/)
if [ ! -d "c_src/lsquic" ]; then
    echo "ERROR: QUIC dependencies not found (c_src/lsquic missing)"
    echo "The lsquic library should be vendored in c_src/"
    exit 1
fi

if [ ! -d "c_src/boringssl" ]; then
    echo "ERROR: QUIC dependencies not found (c_src/boringssl missing)"
    echo "The boringssl library should be vendored in c_src/"
    exit 1
fi

mkdir -p _build/cmake
cd _build/cmake

# Find cmake
if type cmake3 > /dev/null 2>&1 ; then
    CMAKE=cmake3
else
    CMAKE=cmake
fi

# Detect architecture
ARCH=$(uname -m)
case "$ARCH" in
    x86_64|amd64)
        CMAKE_ARCH_FLAGS=""
        ;;
    aarch64|arm64)
        CMAKE_ARCH_FLAGS=""
        ;;
    *)
        echo "Warning: Unknown architecture $ARCH, building without arch-specific flags"
        CMAKE_ARCH_FLAGS=""
        ;;
esac

# Detect OS for platform-specific flags
OS=$(uname -s)
case "$OS" in
    Darwin)
        # macOS - detect if we need universal binary or native
        if [ -n "$MACOSX_DEPLOYMENT_TARGET" ]; then
            CMAKE_PLATFORM_FLAGS="-DCMAKE_OSX_DEPLOYMENT_TARGET=$MACOSX_DEPLOYMENT_TARGET"
        else
            CMAKE_PLATFORM_FLAGS=""
        fi
        ;;
    FreeBSD)
        # FreeBSD - use system OpenSSL or ports
        if [ -d "/usr/local/include/openssl" ]; then
            CMAKE_PLATFORM_FLAGS="-DOPENSSL_ROOT_DIR=/usr/local"
        else
            CMAKE_PLATFORM_FLAGS=""
        fi
        ;;
    NetBSD|OpenBSD)
        # NetBSD/OpenBSD - use pkgsrc or ports OpenSSL
        if [ -d "/usr/pkg/include/openssl" ]; then
            CMAKE_PLATFORM_FLAGS="-DOPENSSL_ROOT_DIR=/usr/pkg"
        elif [ -d "/usr/local/include/openssl" ]; then
            CMAKE_PLATFORM_FLAGS="-DOPENSSL_ROOT_DIR=/usr/local"
        else
            CMAKE_PLATFORM_FLAGS=""
        fi
        ;;
    Linux)
        CMAKE_PLATFORM_FLAGS=""
        ;;
    SunOS)
        # Solaris
        CMAKE_PLATFORM_FLAGS=""
        ;;
    *)
        CMAKE_PLATFORM_FLAGS=""
        ;;
esac

echo "Configuring hackney QUIC NIF for $OS ($ARCH)..."

${CMAKE} \
    ${CMAKE_ARCH_FLAGS} \
    ${CMAKE_PLATFORM_FLAGS} \
    "$@" \
    ../../c_src || exit 1

echo "CMake configuration done."
