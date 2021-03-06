/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */

/*
 * Copyright 2009 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */

#ifndef _ASM_CPU_H
#define	_ASM_CPU_H

#include <sys/types.h>

#ifdef	__cplusplus
extern "C" {
#endif

#if !defined(__lint) && defined(__GNUC__)

extern __inline__ void
prefetch_read_many(void *addr)
{
#if defined(__sparcv9)
	__asm__ __volatile__(
	    "prefetch	[%0],#n_reads\n\t"
	    : "=r" (addr)
	    : "0" (addr));
#else
#error	"port me"
#endif
}

extern __inline__ void
prefetch_read_once(void *addr)
{
#if defined(__sparcv9)
	__asm__ __volatile__(
	    "prefetch	[%0],#one_read\n\t"
	    : "=r" (addr)
	    : "0" (addr));
#else
#error	"port me"
#endif
}

extern __inline__ void
prefetch_write_many(void *addr)
{
#if defined(__sparcv9)
	__asm__ __volatile__(
	    "prefetch	[%0],#n_writes\n\t"
	    : "=r" (addr)
	    : "0" (addr));
#else
#error	"port me"
#endif
}

extern __inline__ void
prefetch_write_once(void *addr)
{
#if defined(__sparcv9)
	__asm__ __volatile__(
	    "prefetch	[%0],#one_write\n\t"
	    : "=r" (addr)
	    : "0" (addr));
#else
#error	"port me"
#endif
}

#endif	/* !__lint && __GNUC__ */

#ifdef	__cplusplus
}
#endif

#endif	/* _ASM_CPU_H */
