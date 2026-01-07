/**
 * gcc_compat.h - GCC compatibility macros for BoringSSL
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2025 Benoit Chesneau
 *
 * BoringSSL's generated assembly files use __has_feature() which is
 * a Clang-specific macro. GCC doesn't define it, causing compilation
 * failures. This header provides a fallback definition.
 */

#ifndef GCC_COMPAT_H
#define GCC_COMPAT_H

/* GCC doesn't have __has_feature, define it to always return 0 */
#ifndef __has_feature
#define __has_feature(x) 0
#endif

#endif /* GCC_COMPAT_H */
