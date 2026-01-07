#!/bin/sh
#
# Build hackney QUIC NIF
# Supports: Linux, macOS (amd64/arm64), FreeBSD, NetBSD, OpenBSD, Solaris
#
# Environment variables:
#   HACKNEY_QUIC_BUILDOPTS - Additional build options (e.g., -j4)
#

set -e

# Check if QUIC dependencies are available (lsquic + BoringSSL in c_src/)
if [ ! -d "c_src/lsquic" ] || [ ! -d "c_src/boringssl" ]; then
    echo "ERROR: QUIC dependencies not found (c_src/lsquic or c_src/boringssl missing)"
    echo "The lsquic and boringssl libraries should be vendored in c_src/"
    exit 1
fi

# Check if cmake was configured
if [ ! -d "_build/cmake" ]; then
    echo "CMake not configured. Run do_cmake.sh first."
    exit 1
fi

cd _build/cmake

# Find cmake
if type cmake3 > /dev/null 2>&1 ; then
    CMAKE=cmake3
else
    CMAKE=cmake
fi

# Detect number of CPU cores for parallel build
# Works on: Linux, macOS, FreeBSD, NetBSD, OpenBSD, Solaris
get_cores() {
    # Try sysctl first (macOS, FreeBSD, NetBSD, OpenBSD)
    CORES=$(sysctl -n hw.ncpu 2>/dev/null) && [ -n "$CORES" ] && echo "$CORES" && return

    # Try nproc (Linux, some BSDs)
    CORES=$(nproc 2>/dev/null) && [ -n "$CORES" ] && echo "$CORES" && return

    # Try /proc/cpuinfo (Linux)
    if [ -f /proc/cpuinfo ]; then
        CORES=$(grep -c '^processor' /proc/cpuinfo 2>/dev/null)
        [ -n "$CORES" ] && echo "$CORES" && return
    fi

    # Try getconf (POSIX, Solaris)
    CORES=$(getconf _NPROCESSORS_ONLN 2>/dev/null) && [ -n "$CORES" ] && echo "$CORES" && return

    # Try psrinfo (Solaris)
    CORES=$(psrinfo 2>/dev/null | wc -l) && [ -n "$CORES" ] && [ "$CORES" -gt 0 ] && echo "$CORES" && return

    # Fallback
    echo "4"
}

# Build
case "$@" in
    *-j*)
        # User specified parallelism
        echo "Building hackney QUIC NIF..."
        ${CMAKE} --build . -- "$@" || exit 1
        ;;
    *)
        CORES=$(get_cores)
        echo "Building hackney QUIC NIF with $CORES parallel jobs..."
        ${CMAKE} --build . -- -j${CORES} $@ || exit 1
        ;;
esac

echo "Build done."

# Verify output
if [ -f "../../priv/hackney_quic.so" ] || [ -f "../../priv/hackney_quic.dylib" ]; then
    echo "NIF built successfully: priv/hackney_quic.*"
else
    echo "Warning: NIF output not found in priv/"
fi
