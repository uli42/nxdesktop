/*
   rdesktop: A Remote Desktop Protocol client.
   Master include file
   Copyright (C) Matthew Chapman 1999-2004
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/**************************************************************************/
/*                                                                        */
/* Copyright (c) 2001,2003 NoMachine, http://www.nomachine.com.           */
/*                                                                        */
/* NXDESKTOP, NX protocol compression and NX extensions to this software    */
/* are copyright of NoMachine. Redistribution and use of the present      */
/* software is allowed according to terms specified in the file LICENSE   */
/* which comes in the source distribution.                                */
/*                                                                        */
/* Check http://www.nomachine.com/licensing.html for applicability.       */
/*                                                                        */
/* NX and NoMachine are trademarks of Medialogic S.p.A.                   */
/*                                                                        */
/* All rights reserved.                                                   */
/*                                                                        */
/**************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>

#define VERSION "1.3.1"

#undef WITH_DEBUG

#ifdef WITH_DEBUG
#define DEBUG(args)	printf args;
#else
#define DEBUG(args)
#endif

#ifdef WITH_DEBUG_KBD
#define DEBUG_KBD(args) printf args;
#else
#define DEBUG_KBD(args)
#endif

#ifdef WITH_DEBUG_RDP5
#define DEBUG_RDP5(args) printf args;
#else
#define DEBUG_RDP5(args)
#endif

#ifdef WITH_DEBUG_CLIPBOARD
#define DEBUG_CLIPBOARD(args) printf args;
#else
#define DEBUG_CLIPBOARD(args)
#endif

#define STRNCPY(dst,src,n)	{ strncpy(dst,src,n-1); dst[n-1] = 0; }

#ifndef MIN
#define MIN(x,y)		(((x) < (y)) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y)		(((x) > (y)) ? (x) : (y))
#endif

#include "parse.h"
#include "constants.h"
#include "types.h"

#ifndef MAKE_PROTO
#include "proto.h"
#endif


/*
 * NX enhancements.
 */

#undef  NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
#undef  NXDESKTOP_XWIN_USES_SYNC_IN_LOOP
#undef	NXDESKTOP_DEBUG_XPUTIMAGE

#undef NXDESKTOP_NXKARMA_DEBUG

#define NXDESKTOP_XWIN_USES_PACKED_IMAGES
#define NXDESKTOP_XWIN_USES_COMPRESSED_PACKED_IMAGES
#define NXDESKTOP_IMGCACHE_USES_COMPRESSED_IMAGES
#define NXDESKTOP_XWIN_USES_PIXMAP_CACHE
#define NXDESKTOP_USES_NXKARMA_IN_LOOP
#define NXDESKTOP_LOGO
#define NXDESKTOP_ONSTART
#define NXDESKTOP_SPLASH
#define NXWIN_USES_PACKED_RDP_TEXT

/*
 * This is size of our cache reported to remote server.
 * See rdp.c, cache.c and xwin.c. Original buffer cache
 * is allocating 0x38400 * 4 (= 921600) bytes, that is
 * memory amount needed to accomodate 0x38400 pixels
 * translated to 32bits visuals.
 */

#define DESKTOP_CACHE_SIZE 0x38400

#ifdef WITH_DEBUG_KBD

#define DEBUG_KBD(args) printf args;

#else

#define DEBUG_KBD(args)

#endif

#include "NX.h"
#include <sys/wait.h>
