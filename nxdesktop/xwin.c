/* -*- c-basic-offset: 8 -*-
   rdesktop: A Remote Desktop Protocol client.
   User interface services - X Window System
   Copyright (C) Matthew Chapman 1999-2005

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
/* Copyright (c) 2001,2005 NoMachine, http://www.nomachine.com.           */
/*                                                                        */
/* NXDESKTOP, NX protocol compression and NX extensions to this software  */
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <NXlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <errno.h>
#include <strings.h>
#include "rdesktop.h"
#include "xproto.h"
#include <signal.h>
#include "proto.h"
#include <X11/Xlibint.h>

#include "version.h"
#include "icon.h"
#include <X11/xpm.h>
#include "X11/Xatom.h"
#include <X11/cursorfont.h>

#include <X11/keysym.h>

#ifdef NXDESKTOP_FWINDOW_MODE
/* For seamless */

#include <X11/extensions/shape.h>

/* For seamless V2 */
static VCHANNEL *fwindow_channel;
#endif

#undef NXDESKTOP_XWIN_DEBUG
#undef NXDESKTOP_USES_SYNC_IN_LOOP

#define NXDESKTOP_FWINDOW_DEBUG

/* These controls the debug of the shape functions */
#undef NXDESKTOP_MASK_DUMP
#undef NXDESKTOP_MASK_DUMP_DISK
#undef NXDESKTOP_MASK_DUMP_DISK_SIGNAL
#undef NXDESKTOP_SHAPE_FORCESYNC

#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES



unsigned int *last_colormap;
unsigned int last_colormap_entries;

#endif

#ifdef NXDESKTOP_XWIN_USES_PIXMAP_CACHE

typedef struct _PIXCACHE
{
	Pixmap pixmap;
	unsigned int offset;
} PIXCACHE;


#define PIXCACHE_ENTRIES  20

static PIXCACHE pixmap_cache[PIXCACHE_ENTRIES];

#endif

#ifdef NXDESKTOP_FWINDOW_MODE
typedef struct
{
    Window wnd;
    char *msg;
    int op;
    int id;
    char *title;
    int x;
    int y;
    int w;
    int h;
    int max_min_type;
} NXCLIPPER_WINDOWS;
#define MAX_NXCLIPPER_WINDOWS 50
static unsigned int nxclipper_windows_entries = 0;
static BOOL first_rdp_window = True;
static NXCLIPPER_WINDOWS nxclipper_windows[MAX_NXCLIPPER_WINDOWS];
#endif

BOOL rdp_window_moving = False;
#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
Window x_rdp_wnd = 0;
static Pixmap pix_move_buffer;
#endif

#ifdef NXDESKTOP_ONSTART

/* Atom nxdesktop_WM_START; */

#endif

#define NXDESKTOP_MIN_WIDTH 120
#define NXDESKTOP_MIN_HEIGHT 90

extern int g_width;
extern int g_height;
extern int g_xpos;
extern int g_ypos;
extern BOOL g_sendmotion;
extern BOOL g_fullscreen;
extern BOOL g_grab_keyboard;
extern BOOL g_hide_decorations;
extern BOOL g_use_rdp5;

extern char g_title[];
extern int g_server_bpp;
extern int g_win_button_size;

extern int xo;
extern int yo;
extern BOOL ipaq;
extern BOOL magickey;
extern char windowName[255];

Display *g_display;
Time g_last_gesturetime;
static int g_x_socket;
static Screen *g_screen;
Window g_wnd;
extern BOOL viewport_mode;
Window g_viewport_wnd = 0;
uint32 g_embed_wnd;
static Window wnd2;
BOOL g_enable_compose = False;
BOOL g_Unobscured;		/* used for screenblt */
static GC g_gc = NULL;
static GC g_create_bitmap_gc = NULL;
static GC g_create_glyph_gc = NULL;
static Visual *g_visual;
static int g_depth;
static int g_bpp;
static XIM g_IM;
static XIC g_IC;
static XModifierKeymap *g_mod_map;
static Cursor g_current_cursor;
static HCURSOR g_null_cursor = NULL;
static Cursor viewportCursor;
static int viewportLastX;
static int viewportLastY;
int g_viewport_width;
int g_viewport_height;
int g_saved_viewport_width = 0;
int g_saved_viewport_height= 0;
int g_saved_viewport_x= 0;
int g_saved_viewport_y= 0;
int g_saved_wnd_x = 0;
int g_saved_wnd_y = 0;
int g_wnd_x;
int g_wnd_y;

void nxdesktopMoveViewport(int hShift, int vShift);

/* stuff for the new FILL_RECT */

#define REC_BUF_SIZE 100

#ifdef NXDESKTOP_USES_RECT_BUF
static XRectangle buf_rects[REC_BUF_SIZE];
static int last_color;
static int num_buf_rects = -1;
#endif

Atom nxdesktopAtoms[NXDESKTOP_NUM_ATOMS];
static BOOL nxdesktopWithWM = True;
char *nxdesktopAtomNames[NXDESKTOP_NUM_ATOMS+1] =
{
    "NX_IDENTITY",          /* 0  */
    "WM_PROTOCOLS",         /* 1  */
    "WM_DELETE_WINDOW",     /* 2  */
    "WM_NX_READY",          /* 3  */
    "MCOPGLOBALS",          /* 4  */
    "NX_CUT_BUFFER_SERVER", /* 5  */
    "TARGETS",              /* 6  */
    "TEXT",                 /* 7  */
    "NX_AGENT_SIGNATURE",   /* 8  */
    "NXDARWIN",             /* 9  */
			      NULL
};

static int PACK_RDP_COMPRESSED;
static int PACK_RDP_PLAIN;
static BOOL g_focused;
static BOOL g_mouse_in_wnd;
static BOOL g_arch_match = False; /* set to True if RGB XServer and little endian */

/*
 * Avoid to use special NX operations
 * when not using NX transport. Ugly
 * but works.
 */

BOOL nxdesktopUseNXTrans = False;
BOOL nxdesktopUseNXRdpImages = False;
BOOL nxdesktopUseNXCompressedRdpImages = False;

#ifdef NXDESKTOP_USES_NXKARMA_IN_LOOP
BOOL nxdesktopSleep = False;
#endif
/* nxproxy control parameters */

#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
unsigned int create_bitmap_total = 0;
unsigned int paint_bitmap_total = 0;
unsigned int create_glyph_total = 0;
unsigned int create_bitmap_times = 0;
unsigned int paint_bitmap_times = 0;
unsigned int create_glyph_times = 0;
unsigned int paint_bitmap_backstore_times = 0;
unsigned int paint_bitmap_backstore_total = 0;
#endif

int nxdesktopStopKarmaSz = -1;
unsigned int nxdesktopLinkType;
unsigned int nxdesktopPackMethod, nxdesktopPackQuality;
unsigned int nxdesktopProtocolMajor, nxdesktopProtocolMinor, nxdesktopProtocolPatch;
int nxdesktopSplitSize;
int nxdesktopDataLevel, nxdesktopStreamLevel, nxdesktopDeltaLevel;
unsigned int nxdesktopLoadCache, nxdesktopSaveCache, nxdesktopStartupCache;

/* nxproxy Xlib and image parameters */
int nxdesktopCleanGet    = 0;
int nxdesktopCleanAlloc  = 0;
int nxdesktopCleanFlush  = 0;
int nxdesktopCleanSend   = 0;
int nxdesktopCleanImages = 0;

int nxdesktopImageSplit  = 0;
int nxdesktopImageMask   = 0;
int nxdesktopImageFrame  = 0;

unsigned int nxdesktopImageSplitMethod  = 0;
unsigned int nxdesktopImageMaskMethod   = 0;


/*#ifdef NXWIN_USES_PACKED_RDP_TEXT*/
BOOL nxdesktopCanPackRDPText = False;
/*#endif*/

/* Image cache flag */
extern BOOL rdp_img_cache;

/* Is the nx rdp bitmap cache active? */
extern BOOL rdp_img_cache_nxcompressed;

/* endianness */
static BOOL g_host_be;
static BOOL g_xserver_be;
static int g_red_shift_r, g_blue_shift_r, g_green_shift_r;
static int g_red_shift_l, g_blue_shift_l, g_green_shift_l;

extern BOOL float_window_mode;
int g_x_offset = 0;
int g_y_offset = 0;

#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
/* shape support */
static XImage *shape_image;
static Pixmap shape_bitmap;
static GC shape_gc;
static BOOL shape_need_update = False;
static BOOL first_shape = True;
static BOOL shape_enabled = False;
/* For w2000 */
#define BACKGROUND 0x3a6ea5
/* for 2003 */
#define BACKGROUND 0x666f74 
#endif

/* software backing store */
extern BOOL g_ownbackstore;
static Pixmap g_backstore = 0;

/* Moving in single app mode */
static BOOL g_moving_wnd;
static int g_move_x_offset = 0;
static int g_move_y_offset = 0;

/*  NX Mods */
/* icon pixmaps */
#define NX_DEFAULT_ICON "nxdesktop.xpm"
static Pixmap nxIconPixmap;
static Pixmap nxIconShape;
Bool useXpmIcon = False;

Bool getNXIcon(Display *g_display, Pixmap *nxIcon, Pixmap *nxMask);

extern key_translation
xkeymap_translate_key(uint32 keysym, unsigned int keycode, unsigned int state);

extern void xkeymap_init(void);

extern BOOL handle_special_keys(uint32 keysym, unsigned int state, uint32 ev_time, BOOL pressed);

int     nx_white;
int     nx_red;
int     nx_black;
int     nx_depth;

#ifdef NXDESKTOP_LOGO
BOOL    showNXlogo = True;
void nomachineLogo(Window win, GC gc, int scale);
#endif

static char PressedKeys[256];

/*
static char MacToPC_keys[256] =
*/

/* nxkbd stuff */
Bool  checkIpaq();
Bool  xkbdRunning = False;
pid_t pidkbd;
extern Bool ipaqWorking;

uint32  last_Xtime = 0; /* last X server time received in an xevent */

extern char nxDisplay[255];

#ifdef WITH_RDPSND
extern int g_dsp_fd;
extern BOOL g_dsp_busy;
extern BOOL g_rdpsnd;
#endif

/* MWM decorations */
#define MWM_HINTS_DECORATIONS   (1L << 1)
#define PROP_MOTIF_WM_HINTS_ELEMENTS    5
typedef struct
{
	uint32 flags;
	uint32 functions;
	uint32 decorations;
	sint32 inputMode;
	uint32 status;
}
PropMotifWmHints;

typedef struct
{
	uint32 red;
	uint32 green;
	uint32 blue;
}
PixelColour;

/* NX */
/* Holds the key modifiers state */
unsigned int buf_key_vector;

/* NX */

#define FILL_RECTANGLE(x,y,cx,cy)\
{ \
	XFillRectangle(g_display, g_wnd, g_gc, x, y, cx, cy); \
	if (g_ownbackstore) \
		XFillRectangle(g_display, g_backstore, g_gc, x, y, cx, cy); \
}

#define FILL_RECTANGLE_BACKSTORE(x,y,cx,cy)\
{ \
	XFillRectangle(g_display, g_ownbackstore ? g_backstore : g_wnd, g_gc, x, y, cx, cy); \
}

#define FILL_POLYGON(p,np)\
{ \
	XFillPolygon(g_display, g_wnd, g_gc, p, np, Complex, CoordModePrevious); \
	if (g_ownbackstore) \
		XFillPolygon(g_display, g_backstore, g_gc, p, np, Complex, CoordModePrevious); \
}

#define DRAW_ELLIPSE(x,y,cx,cy,m)\
{ \
	switch (m) \
	{ \
	    case 0:/* Outline */ \
		XDrawArc(g_display, g_wnd, g_gc, x, y, cx, cy, 0, 360*64); \
		if (g_ownbackstore) \
			XDrawArc(g_display, g_backstore, g_gc, x, y, cx, cy, 0, 360*64); \
	    break; \
	    case 1: /* Filled */ \
		XFillArc(g_display, g_ownbackstore ? g_backstore : g_wnd, g_gc, x, y, \
			 cx, cy, 0, 360*64); \
		if (g_ownbackstore) \
			XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y); \
	    break; \
	} \
}

/* colour maps */
extern BOOL g_owncolmap;
static Colormap g_xcolmap;
static uint32 *g_colmap = NULL;
static unsigned int r, b, g, or, ob, og, off;

#define TRANSLATE(col)		( g_server_bpp != 8 ? translate_colour(col) : g_owncolmap ? col : g_colmap[col] )
#define SET_FOREGROUND(col)	XSetForeground(g_display, g_gc, TRANSLATE(col));
#define SET_BACKGROUND(col)	XSetBackground(g_display, g_gc, TRANSLATE(col));

#define MAGIC_PIXEL
#define NXDESKTOP_ONEXIT
#undef  NXDESKTOP_OWNBACK

static int rop2_map[] = {
	GXclear,		/* 0 */
	GXnor,			/* DPon */
	GXandInverted,		/* DPna */
	GXcopyInverted,		/* Pn */
	GXandReverse,		/* PDna */
	GXinvert,		/* Dn */
	GXxor,			/* DPx */
	GXnand,			/* DPan */
	GXand,			/* DPa */
	GXequiv,		/* DPxn */
	GXnoop,			/* D */
	GXorInverted,		/* DPno */
	GXcopy,			/* P */
	GXorReverse,		/* PDno */
	GXor,			/* DPo */
	GXset			/* 1 */
};

#define SET_FUNCTION(rop2)	{ if (rop2 != ROP2_COPY) XSetFunction(g_display, g_gc, rop2_map[rop2]); }
#define RESET_FUNCTION(rop2)	{ if (rop2 != ROP2_COPY) XSetFunction(g_display, g_gc, GXcopy); }


static void
mwm_hide_decorations(void)
{
	PropMotifWmHints motif_hints;
	Atom hintsatom;

	/* setup the property */
	motif_hints.flags = MWM_HINTS_DECORATIONS;
	motif_hints.decorations = 0;

	/* get the atom for the property */
	hintsatom = XInternAtom(g_display, "_MOTIF_WM_HINTS", False);
	if (!hintsatom)
	{
		warning("Failed to get atom _MOTIF_WM_HINTS: probably your window manager does not support MWM hints\n");
		return;
	}

	XChangeProperty(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, hintsatom, hintsatom, 32, PropModeReplace,
			(unsigned char *) &motif_hints, PROP_MOTIF_WM_HINTS_ELEMENTS);
}

#define SPLITCOLOUR15(colour, rv) \
{ \
	rv.red = ((colour >> 7) & 0xf8) | ((colour >> 12) & 0x7); \
	rv.green = ((colour >> 2) & 0xf8) | ((colour >> 8) & 0x7); \
	rv.blue = ((colour << 3) & 0xf8) | ((colour >> 2) & 0x7); \
}

#define SPLITCOLOUR16(colour, rv) \
{ \
	rv.red = ((colour >> 8) & 0xf8) | ((colour >> 13) & 0x7); \
	rv.green = ((colour >> 3) & 0xfc) | ((colour >> 9) & 0x3); \
	rv.blue = ((colour << 3) & 0xf8) | ((colour >> 2) & 0x7); \
} \

#define SPLITCOLOUR24(colour, rv) \
{ \
	rv.blue = (colour & 0xff0000) >> 16; \
	rv.green = (colour & 0x00ff00) >> 8; \
	rv.red = (colour & 0x0000ff); \
}

#define MAKECOLOUR(pc) \
	((pc.red >> g_red_shift_r) << g_red_shift_l) \
		| ((pc.green >> g_green_shift_r) << g_green_shift_l) \
		| ((pc.blue >> g_blue_shift_r) << g_blue_shift_l) \

#define BSWAP16(x) { x = (((x & 0xff) << 8) | (x >> 8)); }
#define BSWAP24(x) { x = (((x & 0xff) << 16) | (x >> 16) | (x & 0xff00)); }
#define BSWAP32(x) { x = (((x & 0xff00ff) << 8) | ((x >> 8) & 0xff00ff)); \
			x = (x << 16) | (x >> 16); }

#define BOUT16(o, x) { *(o++) = x >> 8; *(o++) = x; }
#define BOUT24(o, x) { *(o++) = x >> 16; *(o++) = x >> 8; *(o++) = x; }
#define BOUT32(o, x) { *(o++) = x >> 24; *(o++) = x >> 16; *(o++) = x >> 8; *(o++) = x; }
#define LOUT16(o, x) { *(o++) = x; *(o++) = x >> 8; }
#define LOUT24(o, x) { *(o++) = x; *(o++) = x >> 8; *(o++) = x >> 16; }
#define LOUT32(o, x) { *(o++) = x; *(o++) = x >> 8; *(o++) = x >> 16; *(o++) = x >> 24; }

static uint32
translate_colour(uint32 colour)
{
	PixelColour pc;
	switch (g_server_bpp)
	{
		case 15:
			SPLITCOLOUR15(colour, pc);
			break;
		case 16:
			SPLITCOLOUR16(colour, pc);
			break;
		case 24:
			SPLITCOLOUR24(colour, pc);
			break;
	}
	return MAKECOLOUR(pc);
}

/* indent is confused by UNROLL8 */
/* *INDENT-OFF* */

/* repeat and unroll, similar to bitmap.c */
/* potentialy any of the following translate */
/* functions can use repeat but just doing */
/* the most common ones */

#define UNROLL8(stm) { stm stm stm stm stm stm stm stm }
/* 2 byte output repeat */
#define REPEAT2(stm) \
{ \
	while (out <= end - 8 * 2) \
		UNROLL8(stm) \
	while (out < end) \
		{ stm } \
}
/* 3 byte output repeat */
#define REPEAT3(stm) \
{ \
	while (out <= end - 8 * 3) \
		UNROLL8(stm) \
	while (out < end) \
		{ stm } \
}
/* 4 byte output repeat */
#define REPEAT4(stm) \
{ \
	while (out <= end - 8 * 4) \
		UNROLL8(stm) \
	while (out < end) \
		{ stm } \
}
/* *INDENT-ON* */

static void
translate8to8(const uint8 * data, uint8 * out, uint8 * end)
{
	while (out < end)
		*(out++) = (uint8) g_colmap[*(data++)];
}

static void
translate8to16(const uint8 * data, uint8 * out, uint8 * end)
{
	uint16 value;

	if (g_arch_match)
	{
		/* *INDENT-OFF* */
		REPEAT2
		(
			*((uint16 *) out) = g_colmap[*(data++)];
			out += 2;
		)
		/* *INDENT-ON* */
	}
	else if (g_xserver_be)
	{
		while (out < end)
		{
			value = (uint16) g_colmap[*(data++)];
			BOUT16(out, value);
		}
	}
	else
	{
		while (out < end)
		{
			value = (uint16) g_colmap[*(data++)];
			LOUT16(out, value);
		}
	}
}

/* little endian - conversion happens when colourmap is built */
static void
translate8to24(const uint8 * data, uint8 * out, uint8 * end)
{
	uint32 value;

	if (g_xserver_be)
	{
		while (out < end)
		{
			value = g_colmap[*(data++)];
			BOUT24(out, value);
		}
	}
	else
	{
		while (out < end)
		{
			value = g_colmap[*(data++)];
			LOUT24(out, value);
		}
	}
}

static void
translate8to32(const uint8 * data, uint8 * out, uint8 * end)
{
	uint32 value;

	if (g_arch_match)
	{
		/* *INDENT-OFF* */
		REPEAT4
		(
			*((uint32 *) out) = g_colmap[*(data++)];
			out += 4;
		)
		/* *INDENT-ON* */
	}
	else if (g_xserver_be)
	{
		while (out < end)
		{
			value = g_colmap[*(data++)];
			BOUT32(out, value);
		}
	}
	else
	{
		while (out < end)
		{
			value = g_colmap[*(data++)];
			LOUT32(out, value);
		}
	}
}

static void
translate15to16(const uint16 * data, uint8 * out, uint8 * end)
{
	uint16 pixel;
	uint16 value;
	PixelColour pc;

	if (g_xserver_be)
	{
		while (out < end)
		{
			pixel = *(data++);
			if (g_host_be)
			{
				BSWAP16(pixel);
			}
			SPLITCOLOUR15(pixel, pc);
			value = MAKECOLOUR(pc);
			BOUT16(out, value);
		}
	}
	else
	{
		while (out < end)
		{
			pixel = *(data++);
			if (g_host_be)
			{
				BSWAP16(pixel);
			}
			SPLITCOLOUR15(pixel, pc);
			value = MAKECOLOUR(pc);
			LOUT16(out, value);
		}
	}
}

static void
translate15to24(const uint16 * data, uint8 * out, uint8 * end)
{
	uint32 value;
	uint16 pixel;
	PixelColour pc;

	if (g_arch_match)
	{
		/* *INDENT-OFF* */
		REPEAT3
		(
			pixel = *(data++);
			SPLITCOLOUR15(pixel, pc);
			*(out++) = pc.blue;
			*(out++) = pc.green;
			*(out++) = pc.red;
		)
		/* *INDENT-ON* */
	}
	else if (g_xserver_be)
	{
		while (out < end)
		{
			pixel = *(data++);
			if (g_host_be)
			{
				BSWAP16(pixel);
			}
			SPLITCOLOUR15(pixel, pc);
			value = MAKECOLOUR(pc);
			BOUT24(out, value);
		}
	}
	else
	{
		while (out < end)
		{
			pixel = *(data++);
			if (g_host_be)
			{
				BSWAP16(pixel);
			}
			SPLITCOLOUR15(pixel, pc);
			value = MAKECOLOUR(pc);
			LOUT24(out, value);
		}
	}
}

static void
translate15to32(const uint16 * data, uint8 * out, uint8 * end)
{
	uint16 pixel;
	uint32 value;
	PixelColour pc;

	if (g_arch_match)
	{
		/* *INDENT-OFF* */
		REPEAT4
		(
			pixel = *(data++);
			SPLITCOLOUR15(pixel, pc);
			*(out++) = pc.blue;
			*(out++) = pc.green;
			*(out++) = pc.red;
			*(out++) = 0;
		)
		/* *INDENT-ON* */
	}
	else if (g_xserver_be)
	{
		while (out < end)
		{
			pixel = *(data++);
			if (g_host_be)
			{
				BSWAP16(pixel);
			}
			SPLITCOLOUR15(pixel, pc);
			value = MAKECOLOUR(pc);
			BOUT32(out, value);
		}
	}
	else
	{
		while (out < end)
		{
			pixel = *(data++);
			if (g_host_be)
			{
				BSWAP16(pixel);
			}
			SPLITCOLOUR15(pixel, pc);
			value = MAKECOLOUR(pc);
			LOUT32(out, value);
		}
	}
}

static void
translate16to16(const uint16 * data, uint8 * out, uint8 * end)
{
	uint16 pixel;
	uint16 value;
	PixelColour pc;

	if (g_xserver_be)
	{
		if (g_host_be)
		{
			while (out < end)
			{
				pixel = *(data++);
				BSWAP16(pixel);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				BOUT16(out, value);
			}
		}
		else
		{
			while (out < end)
			{
				pixel = *(data++);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				BOUT16(out, value);
			}
		}
	}
	else
	{
		if (g_host_be)
		{
			while (out < end)
			{
				pixel = *(data++);
				BSWAP16(pixel);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				LOUT16(out, value);
			}
		}
		else
		{
			while (out < end)
			{
				pixel = *(data++);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				LOUT16(out, value);
			}
		}
	}
}

static void
translate16to24(const uint16 * data, uint8 * out, uint8 * end)
{
	uint32 value;
	uint16 pixel;
	PixelColour pc;

	if (g_arch_match)
	{
		/* *INDENT-OFF* */
		REPEAT3
		(
			pixel = *(data++);
			SPLITCOLOUR16(pixel, pc);
			*(out++) = pc.blue;
			*(out++) = pc.green;
			*(out++) = pc.red;
		)
		/* *INDENT-ON* */
	}
	else if (g_xserver_be)
	{
		if (g_host_be)
		{
			while (out < end)
			{
				pixel = *(data++);
				BSWAP16(pixel);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				BOUT24(out, value);
			}
		}
		else
		{
			while (out < end)
			{
				pixel = *(data++);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				BOUT24(out, value);
			}
		}
	}
	else
	{
		if (g_host_be)
		{
			while (out < end)
			{
				pixel = *(data++);
				BSWAP16(pixel);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				LOUT24(out, value);
			}
		}
		else
		{
			while (out < end)
			{
				pixel = *(data++);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				LOUT24(out, value);
			}
		}
	}
}

static void
translate16to32(const uint16 * data, uint8 * out, uint8 * end)
{
	uint16 pixel;
	uint32 value;
	PixelColour pc;

	if (g_arch_match)
	{
		/* *INDENT-OFF* */
		REPEAT4
		(
			pixel = *(data++);
			SPLITCOLOUR16(pixel, pc);
			*(out++) = pc.blue;
			*(out++) = pc.green;
			*(out++) = pc.red;
			*(out++) = 0;
		)
		/* *INDENT-ON* */
	}
	else if (g_xserver_be)
	{
		if (g_host_be)
		{
			while (out < end)
			{
				pixel = *(data++);
				BSWAP16(pixel);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				BOUT32(out, value);
			}
		}
		else
		{
			while (out < end)
			{
				pixel = *(data++);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				BOUT32(out, value);
			}
		}
	}
	else
	{
		if (g_host_be)
		{
			while (out < end)
			{
				pixel = *(data++);
				BSWAP16(pixel);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				LOUT32(out, value);
			}
		}
		else
		{
			while (out < end)
			{
				pixel = *(data++);
				SPLITCOLOUR16(pixel, pc);
				value = MAKECOLOUR(pc);
				LOUT32(out, value);
			}
		}
	}
}

static void
translate24to16(const uint8 * data, uint8 * out, uint8 * end)
{
	uint32 pixel = 0;
	uint16 value;
	PixelColour pc;

	while (out < end)
	{
		pixel = *(data++) << 16;
		pixel |= *(data++) << 8;
		pixel |= *(data++);
		SPLITCOLOUR24(pixel, pc);
		value = MAKECOLOUR(pc);
		if (g_xserver_be)
		{
			BOUT16(out, value);
		}
		else
		{
			LOUT16(out, value);
		}
	}
}

static void
translate24to24(const uint8 * data, uint8 * out, uint8 * end)
{
	uint32 pixel;
	uint32 value;
	PixelColour pc;

	if (g_xserver_be)
	{
		while (out < end)
		{
			pixel = *(data++) << 16;
			pixel |= *(data++) << 8;
			pixel |= *(data++);
			SPLITCOLOUR24(pixel, pc);
			value = MAKECOLOUR(pc);
			BOUT24(out, value);
		}
	}
	else
	{
		while (out < end)
		{
			pixel = *(data++) << 16;
			pixel |= *(data++) << 8;
			pixel |= *(data++);
			SPLITCOLOUR24(pixel, pc);
			value = MAKECOLOUR(pc);
			LOUT24(out, value);
		}
	}
}

static void
translate24to32(const uint8 * data, uint8 * out, uint8 * end)
{
	uint32 pixel;
	uint32 value;
	PixelColour pc;

	if (g_arch_match)
	{
		/* *INDENT-OFF* */
#ifdef NEED_ALIGN
		REPEAT4
		(
			*(out++) = *(data++);
			*(out++) = *(data++);
			*(out++) = *(data++);
			*(out++) = 0;
		)
#else
		REPEAT4
		(
			*((uint32 *) out) = *((uint32 *) data);
			out += 4;
			data += 3;
		)
#endif
		/* *INDENT-ON* */
	}
	else if (g_xserver_be)
	{
		while (out < end)
		{
			pixel = *(data++) << 16;
			pixel |= *(data++) << 8;
			pixel |= *(data++);
			SPLITCOLOUR24(pixel, pc);
			value = MAKECOLOUR(pc);
			BOUT32(out, value);
		}
	}
	else
	{
		while (out < end)
		{
			pixel = *(data++) << 16;
			pixel |= *(data++) << 8;
			pixel |= *(data++);
			SPLITCOLOUR24(pixel, pc);
			value = MAKECOLOUR(pc);
			LOUT32(out, value);
		}
	}
}

static uint8 *
translate_image(int width, int height, uint8 * data)
{
	int size;
	uint8 *out;
	uint8 *end;

	/* if server and xserver bpp match, */
	/* and arch(endian) matches, no need to translate */
	/* just return data */
	if (g_arch_match)
	{
		if (g_depth == 15 && g_server_bpp == 15)
			return data;
		if (g_depth == 16 && g_server_bpp == 16)
			return data;
		if (g_depth == 24 && g_bpp == 24 && g_server_bpp == 24)
			return data;
	}

	size = width * height * (g_bpp / 8);
	out = (uint8 *) xmalloc(size);
	end = out + size;

	switch (g_server_bpp)
	{
		case 24:
			switch (g_bpp)
			{
				case 32:
					translate24to32(data, out, end);
					break;
				case 24:
					translate24to24(data, out, end);
					break;
				case 16:
					translate24to16(data, out, end);
					break;
			}
			break;
		case 16:
			switch (g_bpp)
			{
				case 32:
					translate16to32((uint16 *) data, out, end);
					break;
				case 24:
					translate16to24((uint16 *) data, out, end);
					break;
				case 16:
					translate16to16((uint16 *) data, out, end);
					break;
			}
			break;
		case 15:
			switch (g_bpp)
			{
				case 32:
					translate15to32((uint16 *) data, out, end);
					break;
				case 24:
					translate15to24((uint16 *) data, out, end);
					break;
				case 16:
					translate15to16((uint16 *) data, out, end);
					break;
			}
			break;
		case 8:
			switch (g_bpp)
			{
				case 8:
					translate8to8(data, out, end);
					break;
				case 16:
					translate8to16(data, out, end);
					break;
				case 24:
					translate8to24(data, out, end);
					break;
				case 32:
					translate8to32(data, out, end);
					break;
			}
			break;
	}
	return out;
}


unsigned int nxLogoColor(unsigned int colorin){
      /*
       take color values in rgb24 0xff0000 0x00ff00 0x0000ff
       and find an equivalent for your visual.
       */
      int cr=0, cg=0, cb=0;

      cr = (colorin >> (or))&r;
      cg = (colorin >> (og-8))&g;
      cb = (colorin >> (ob-16))&b;
      return cr| cg| cb;
  }

void nxQC(Display *d, Colormap c){
      XColor X;
      X.pixel = 0xff;
      X.pad = 0;
      X.flags = 0;
      XQueryColor(d,c,&X);
      #ifdef NXDESKTOP_XWIN_DEBUG
      nxdesktopDebug("nxQC","XQUERY: %x %x %x\n", X.red, X.blue, X.green);
      #endif
}

void
sigusr_func (int s)
{
    switch (s)
    {
    case SIGUSR1:
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("sigusr_func","Received SIGUSR1, unmapping window.\n");
	#endif
	if(ipaq)
	{
	    XIconifyWindow (g_display, wnd2, DefaultScreen(g_display));
    	    XMapWindow(g_display, wnd2);
	}
	XUnmapWindow (g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd);
    break;
    case SIGUSR2:
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	#ifdef NXDESKTOP_FWINDOW_DEBUG
	nxdesktopDebug("sigusr_func","Received SIGUSR2 - Mask debug mode.\n");
	    #ifdef NXDESKTOP_MASK_DUMP_DISK_SIGNAL
	    if (float_window_mode)
	    {
		XWriteBitmapFile(g_display,"shape_bitmap.xbm",shape_bitmap,g_width,g_height,-1,-1);
		XWriteBitmapFile(g_display,"background.xbm",g_backstore,g_width,g_height,-1,-1);
	    }
	    else nxdesktopDebug("sigusr_func","float_window_mode = False.\n");
	    #else
	    nxdesktopDebug("sigusr_func","Lack dump from here to video yet.\n");
	    #endif
	#endif
	#endif
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("sigusr_func","Received SIGUSR2, unmapping window.\n");
	#endif
	if(ipaq)
	{
    	    XMapWindow (g_display, g_wnd);
    	    XUnmapWindow(g_display, wnd2);
	}
	else 
	{
    	    XMapRaised (g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd);
    	    XIconifyWindow (g_display, wnd2, DefaultScreen(g_display));
	}
    break;
    default:
    break;
    }
    XFlush (g_display);
}

BOOL
get_key_state(unsigned int state, uint32 keysym)
{
	int modifierpos, key, keysymMask = 0;
	int offset;

	KeyCode keycode = XKeysymToKeycode(g_display, keysym);

	if (keycode == NoSymbol)
		return False;

	for (modifierpos = 0; modifierpos < 8; modifierpos++)
	{
		offset = g_mod_map->max_keypermod * modifierpos;

		for (key = 0; key < g_mod_map->max_keypermod; key++)
		{
			if (g_mod_map->modifiermap[offset + key] == keycode)
				keysymMask |= 1 << modifierpos;
		}
	}

	return (state & keysymMask) ? True : False;
}

BOOL ui_open_display()

{
    g_display = XOpenDisplay((char *)nxDisplay);
    if (g_display == NULL)
    {
        error("Failed to open display\n");
        return False;
    }
    return True;
}

void ui_close_display()
{
  XCloseDisplay(g_display);
}

void ui_get_display_size(int *width, int *height)
{
   *width  = WidthOfScreen(DefaultScreenOfDisplay(g_display));
   *height = HeightOfScreen(DefaultScreenOfDisplay(g_display));
   *width  = *width  & ~3; /* make width a multiple of 32 bits */
}

int nxdesktopErrorHandler(Display *dpy, XErrorEvent *err)
{
    char msg[81];
    XGetErrorText(dpy, err -> error_code, msg, 80);
    info("X request #%d failed with message '%s'.\n", err -> request_code, msg);
    info("X sequence was %ld with resource %ld.\n", err -> serial & 0xffff, err -> resourceid);
    return 1;
}

static void
calculate_shifts(uint32 mask, int *shift_r, int *shift_l)
{
	*shift_l = ffs(mask) - 1;
	mask >>= *shift_l;
	*shift_r = 8 - ffs(mask & ~(mask >> 1));
}

BOOL
ui_init(void)
{
	XVisualInfo vi;
	XPixmapFormatValues *pfm;
	uint16 test;
	int minkey, maxkey;
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	int event_base, error_base;
	#endif
	int i, screen_num, nvisuals;
	XVisualInfo *vmatches = NULL;
	XVisualInfo template;
	Bool TrueColorVisual = False;
	
	/*g_display = XOpenDisplay(nxDisplay);
	if (g_display == NULL)
	{
		error("Failed to open g_display: %s\n", XDisplayName(NULL));
		return False;
	}*/
   
	g_x_socket = ConnectionNumber(g_display); /* NX */
	{
          extern void tcp_resize_buf(int, int, int);
          extern int rdp_bufsize;
          tcp_resize_buf(g_x_socket, 0, rdp_bufsize);
        }
	
	screen_num = DefaultScreen(g_display);
	g_screen = ScreenOfDisplay(g_display, screen_num);
	g_depth = DefaultDepthOfScreen(g_screen);
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	/* Check for the XShape extension */
	if (!XShapeQueryExtension(g_display, &event_base, &error_base))
	{
	    warning("XShape extension not found. Floating window mode will not work properly\n");
	}
	#ifdef NXDESKTOP_FWINDOW_DEBUG
	else
	{
	    nxdesktopDebug("ui_init","XShape extension OK\n");
	}
	#endif
	#endif

	/* Search for best TrueColor depth */
	template.class = TrueColor;
	vmatches = XGetVisualInfo(g_display, VisualClassMask, &template, &nvisuals);

	nvisuals--;
	while (nvisuals >= 0)
	{
		if ((vmatches + nvisuals)->depth > g_depth)
		{
			g_depth = (vmatches + nvisuals)->depth;
		}
		nvisuals--;
		TrueColorVisual = True;
	}

	/* NX */
	XFree(vmatches);
	/* NX */
	
	test = 1;
	g_host_be = !(BOOL) (*(uint8 *) (&test));
	g_xserver_be = (ImageByteOrder(g_display) == MSBFirst);

	if ((g_server_bpp == 8) && ((!TrueColorVisual) || (g_depth <= 8)))
	{
		/* we use a colourmap, so the default visual should do */
		g_visual = DefaultVisualOfScreen(g_screen);
		g_depth = DefaultDepthOfScreen(g_screen);

		/* Do not allocate colours on a TrueColor visual */
		if (g_visual->class == TrueColor)
		{
			g_owncolmap = False;
		}
	}
	else
	{
		/* need a truecolour visual */
		if (!XMatchVisualInfo(g_display, screen_num, g_depth, TrueColor, &vi))
		{
			error("The display does not support true colour - high colour support unavailable.\n");
			return False;
		}

		g_visual = vi.visual;
		g_owncolmap = False;
		calculate_shifts(vi.red_mask, &g_red_shift_r, &g_red_shift_l);
		calculate_shifts(vi.blue_mask, &g_blue_shift_r, &g_blue_shift_l);
		calculate_shifts(vi.green_mask, &g_green_shift_r, &g_green_shift_l);

		/* if RGB video and everything is little endian */
		if ((vi.red_mask > vi.green_mask && vi.green_mask > vi.blue_mask) &&
		    !g_xserver_be && !g_host_be)
		{
			if (g_depth <= 16 || (g_red_shift_l == 16 && g_green_shift_l == 8 &&
					      g_blue_shift_l == 0))
			{
				g_arch_match = True;
			}
		}

		if (g_arch_match)
		{
			DEBUG(("Architectures match, enabling little endian optimisations.\n"));
		}
	}
	
	if (nxDisplay[0] != 0)
	{
	    nxdesktopUseNXTrans = (strncasecmp(nxDisplay, "nx", 2) == 0);
	}
	else
	{	
	    nxdesktopUseNXTrans = (strncasecmp(XDisplayName(NULL), "nx", 2) == 0);
	}

        XSetErrorHandler(nxdesktopErrorHandler);

	#ifndef NXDESKTOP_USES_RECT_BUF
	warning("XFillRect optimization disabled\n");
	#endif

	pfm = XListPixmapFormats(g_display, &i);
	if (pfm != NULL)
	{
		/* Use maximum bpp for this depth - this is generally
		   desirable, e.g. 24 bits->32 bits. */
		while (i--)
		{
			if ((pfm[i].depth == g_depth) && (pfm[i].bits_per_pixel > g_bpp))
			{
				g_bpp = pfm[i].bits_per_pixel;
			}
		}
		XFree(pfm);
	}
	/* NX */ 
	else
	{
	    error("Failed to alloc list of pixmap formats.\n");
	    XCloseDisplay(g_display);
	    return False;
	}
	/* NX */

	if (g_bpp < 8)
	{
		error("Less than 8 bpp not currently supported.\n");
		XCloseDisplay(g_display);
		return False;
	}

	if (!g_owncolmap)
	{
		g_xcolmap =
			XCreateColormap(g_display, RootWindowOfScreen(g_screen), g_visual,
					AllocNone);
		if (g_depth <= 8)
			warning("Screen depth is 8 bits or lower: you may want to use -C for a private colourmap\n");
	}

	if ((!g_ownbackstore) && (DoesBackingStore(g_screen) != Always))
	{
		warning("External BackingStore not available, using internal\n");
		g_ownbackstore = True;
	}

	/*
	 * Determine desktop size
	 */
	if (g_fullscreen) 
	{
		g_width = WidthOfScreen(g_screen);
		g_height = HeightOfScreen(g_screen);
	}
	else if (g_width < 0)
	{
		/* Percent of screen */
		g_height = HeightOfScreen(g_screen) * (-g_width) / 100;
		g_width = WidthOfScreen(g_screen) * (-g_width) / 100;
	}
	else if (g_width == 0) 
	{
		/* Fetch geometry from _NET_WORKAREA */
		uint32 x, y, cx, cy;

		if (get_current_workarea(&x, &y, &cx, &cy) == 0)
		{
			g_width = cx;
			g_height = cy;
		}
		else
		{
			warning("Failed to get workarea: probably your window manager does not support extended hints\n");
			g_width = 800;
			g_height = 600;
		}
	}
	else if (float_window_mode)
	{
	    g_width = WidthOfScreen(g_screen);
	    g_height = HeightOfScreen(g_screen);
	}

	/* make sure width is a multiple of 4 */
	g_width = (g_width + 3) & ~3;

	XDisplayKeycodes(g_display, &minkey, &maxkey);
	g_mod_map = XGetModifierMapping(g_display);

	xkeymap_init();

	if (g_enable_compose)
		g_IM = XOpenIM(g_display, NULL, NULL, NULL);

	xclip_init();

	DEBUG_RDP5(("server bpp %d client bpp %d depth %d\n", g_server_bpp, g_bpp, g_depth));

	return True;
}

BOOL
ui_init_nx(void)

{
    #ifdef NXDESKTOP_XWIN_DEBUG
    int i;
    
    
    nxdesktopDebug("ui_init","Entered NX transport init process.\n");
    #endif  
    
    if (nxdesktopUseNXTrans)
    {
	rdp_img_cache = True;
	rdp_img_cache_nxcompressed = True;
        NXGetControlParameters(g_display, &nxdesktopLinkType, &nxdesktopProtocolMajor,
				&nxdesktopProtocolMinor, &nxdesktopProtocolPatch,
				&nxdesktopStopKarmaSz, &nxdesktopSplitSize,
				&nxdesktopPackMethod, &nxdesktopPackQuality,
                                &nxdesktopDataLevel, &nxdesktopStreamLevel,
                                &nxdesktopDeltaLevel, &nxdesktopLoadCache,
                                &nxdesktopSaveCache, &nxdesktopStartupCache);


	NXGetCleanupParameters(g_display, &nxdesktopCleanGet, &nxdesktopCleanAlloc,
				&nxdesktopCleanFlush, &nxdesktopCleanSend,
				&nxdesktopCleanImages);

	NXGetImageParameters(g_display,	&nxdesktopImageSplit, &nxdesktopImageMask,
				&nxdesktopImageFrame, &nxdesktopImageSplitMethod,
				&nxdesktopImageMaskMethod);
		
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_init","RDP image cache: %d.\n",rdp_img_cache);
	nxdesktopDebug("ui_init","RDP image compressed cache: %d.\n",rdp_img_cache_nxcompressed);
	#endif
	/*
	 * Activate shared memory PutImages
	 * in X server proxy. Shared memory
         * on path from agent to X client
         * proxy is not implemented yet.
	 */

	{
	    unsigned int enableClient = 0;
	    unsigned int enableServer = 1;
	    unsigned int clientSegment, serverSegment;

	    NXGetShmemParameters(g_display, &enableClient, &enableServer,
				    &clientSegment, &serverSegment);
	    if (enableServer == False)
	    {
		info("Not using shared memory support in X server.\n");
	    }
	    else
	    {
		info("Using shared memory support in X server.\n");
	    }
	}

        nxdesktopStopKarmaSz = nxdesktopStopKarmaSz;

	/*
	 * Get unpack methods implemented by the proxy
	 * chain and check if ours are supported.
	 */
	 
	if (nxdesktopPackMethod != NO_PACK)
	{
	    unsigned char methods[NXNumberOfPackMethods];

	    unsigned int entries = NXNumberOfPackMethods;

	    if (NXGetUnpackParameters (g_display, &entries, methods) == 0 ||
		entries != NXNumberOfPackMethods)
    	    {
		error ("ui_init", "NXGetUnpackParameters() failed on g_display '%s'.\n",
	        XDisplayName ((char *) nxDisplay));
		return False;
	    }

	    #ifdef NXWIN_USES_PACKED_RDP_TEXT
	    nxdesktopCanPackRDPText = methods[PACK_RDP_TEXT];
	    #endif
	    
	    #ifdef NXDESKTOP_XWIN_DEBUG
	    for (i = 0; i <= NXNumberOfPackMethods; i++)
    	    {
		if (methods[i])
		    nxdesktopDebug ("ui_init", "Registered pack method %d.\n", i);
    	    }
	    #endif

	    /* COMPRESS methods */
	    if ((methods[PACK_RDP_COMPRESSED_16M_COLORS] == True) && (g_server_bpp > 16))
    	    {
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_COMPRESSED_16M_COLORS.\n");
		#endif
		PACK_RDP_COMPRESSED = PACK_RDP_COMPRESSED_16M_COLORS;
		nxdesktopUseNXCompressedRdpImages = True;
    	    }
	    else
    	    if ((methods[PACK_RDP_COMPRESSED_64K_COLORS] == True) && (g_server_bpp > 8))
    	    {
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_COMPRESSED_64K_COLORS.\n");
		#endif
		PACK_RDP_COMPRESSED = PACK_RDP_COMPRESSED_64K_COLORS;
		nxdesktopUseNXCompressedRdpImages = True;
    	    }
	    else 
	    if (methods[PACK_RDP_COMPRESSED_256_COLORS] == True)
    	    {
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_COMPRESSED_256_COLORS.\n");
		#endif
		PACK_RDP_COMPRESSED = PACK_RDP_COMPRESSED_256_COLORS;
		nxdesktopUseNXCompressedRdpImages = True;
    	    }
	    
	    /* PLAIN methods */
	    if ((methods[PACK_RDP_PLAIN_16M_COLORS] == True) && (g_server_bpp > 16))
    	    {
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_PLAIN_16M_COLORS.\n");
		#endif
		PACK_RDP_PLAIN = PACK_RDP_PLAIN_16M_COLORS;
		nxdesktopUseNXRdpImages = True;
    	    }
	    else
    	    if ((methods[PACK_RDP_PLAIN_64K_COLORS] == True) && (g_server_bpp > 8))
    	    {
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_PLAIN_64K_COLORS.\n");
		#endif
		PACK_RDP_PLAIN = PACK_RDP_PLAIN_64K_COLORS;
		nxdesktopUseNXRdpImages = True;
    	    }
	    else 
	    if (methods[PACK_RDP_PLAIN_256_COLORS] == True)
    	    {
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug ("ui_init", "sing pack method PACK_RDP_PLAIN_256_COLORS.\n");
		#endif
		PACK_RDP_PLAIN = PACK_RDP_PLAIN_256_COLORS;
		nxdesktopUseNXRdpImages = True;
    	    }
	    else
    	    {
		warning ("No available RDP pack method on g_display '%s'.\n", XDisplayName ((char *) nxDisplay));
		rdp_img_cache_nxcompressed = False;
    	    }
	}
	else
	{
	    warning ("Cache disabled.\n");
	    rdp_img_cache = False;
	    rdp_img_cache_nxcompressed = False;
	}

	/*
        * Inform remote proxy about pixel geometry
        * to be used to unpack images.
        */

	if (nxdesktopUseNXCompressedRdpImages || nxdesktopUseNXRdpImages)
	{
	    if (NXSetUnpackGeometry(g_display, 0, g_screen, g_visual) == 0)
	    {
		error("NXSetUnpackGeometry() failed on g_display '%s'.\n", XDisplayName((char *)nxDisplay));
		return False;
	    }
	}
    }
	
    return True;
}
	
void
ui_deinit(void)
{
	#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
	nxdesktopDebug("XPutImage","create_bitmap total = %d\n",create_bitmap_total);
	nxdesktopDebug("XPutImage","create_bitmap times = %d\n",create_bitmap_times);
	nxdesktopDebug("XPutImage","create_glyph total = %d\n",create_glyph_total);
	nxdesktopDebug("XPutImage","create_glyph times = %d\n",create_glyph_times);
	nxdesktopDebug("XPutImage","paint_bitmap total = %d\n",paint_bitmap_total);
	nxdesktopDebug("XPutImage","paint_bitmap times = %d\n",paint_bitmap_times);
	nxdesktopDebug("XPutImage","paint_bitmap_backstore total = %d\n",paint_bitmap_backstore_total);
	nxdesktopDebug("XPutImage","paint_bitmap_backstore times = %d\n",paint_bitmap_backstore_times);
	#endif	
	
	if (g_IM != NULL)
		XCloseIM(g_IM);

	if (g_null_cursor != NULL)
		ui_destroy_cursor(g_null_cursor);

	XFreeModifiermap(g_mod_map);

	if (g_ownbackstore)
		XFreePixmap(g_display, g_backstore);

	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{
	    XDestroyImage(shape_image);
 	    XFreePixmap(g_display,shape_bitmap);
 	    XFreeGC(g_display,shape_gc);
	}
	#endif
	
	XFreeGC(g_display, g_gc);
	XCloseDisplay(g_display);
	g_display = NULL;
}

#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING

/* This function returns a pixmap, suitable to create a window mask, */

HBITMAP create_mask(HBITMAP bitmap, int sx, int sy, int w, int h)

{
    XImage *aux_rdp_image, *aux_shape_image;
    Pixmap aux_pixmap;
    int x, y;
    
    aux_rdp_image = XGetImage(g_display, (Pixmap)bitmap, sx, sy, w, h, AllPlanes, ZPixmap);
    aux_shape_image = XCreateImage(g_display, g_visual, 1, XYBitmap, 0, NULL, w, h, 8, 0);
    aux_shape_image->data = malloc(aux_shape_image->bytes_per_line * aux_shape_image->height);
    aux_pixmap = XCreatePixmap(g_display, g_wnd, w, h, (unsigned int) 1);
    
    #ifdef NXDESKTOP_MASK_DUMP
    nxdesktopDebug("create mask","aux_rdp_image\n");
    hexdump(aux_rdp_image->data,(w/8)*h);
    #endif
    
    for (y = 0; y < h; y++)
    {
        for (x = 0; x < w; x++)
        {
	    if (XGetPixel(aux_rdp_image,x,y) == BACKGROUND)
		XPutPixel(aux_shape_image,x,y,1);
	    else
		XPutPixel(aux_shape_image,x,y,0);
	}
    }

    #ifdef NXDESKTOP_MASK_DUMP
    nxdesktopDebug("create mask","aux_shape_image\n");
    hexdump(aux_shape_image->data,(w/8)*h);
    #endif
    
    XPutImage(g_display, aux_pixmap, shape_gc, aux_shape_image, 0, 0, 0, 0, w, h);
    XFree(aux_rdp_image);
    XFree(aux_shape_image);
    return (HBITMAP) aux_pixmap;
}


void test_shape(void)
{
    /* Creates a test shape mask from the backstore and optionaly dumps it to disk */
    shape_bitmap = (Pixmap)create_mask((HBITMAP)g_backstore, 0, 0, g_width,g_height);
    
    #ifdef NXDESKTOP_MASK_DUMP_DISK
    XWriteBitmapFile(g_display,"shape_bitmap.xbm",shape_bitmap,g_width,g_height,-1,-1);
    XWriteBitmapFile(g_display,"background.xbm",g_backstore,g_width,g_height,-1,-1);
    #endif
    
    XShapeCombineMask(g_display, g_wnd, ShapeBounding, 0, 0, shape_bitmap, ShapeSet);
    XSync(g_display,False);
}

void put_mask(HBITMAP bitmap, int srcx, int srcy, int x, int y, int w, int h)

{
    #ifdef NXDESKTOP_MASK_DUMP_DISK
    nxdesktopDebug("put_mask","bitmap coords %d %d %d %d %d %d\n",srcx, srcy, x, y, w, h);
    XWriteBitmapFile(g_display,"bitmap.xbm",(Pixmap)bitmap,w,h,-1,-1);
    #endif
    XCopyArea(g_display, (Pixmap)bitmap, shape_bitmap, shape_gc, srcx, srcy, w, h, x, y);
    XShapeCombineMask(g_display, g_wnd, ShapeBounding, 0, 0, shape_bitmap, ShapeSet);
    #ifdef NXDESKTOP_MASK_DUMP_DISK
    XWriteBitmapFile(g_display,"shape_bitmap.xbm",shape_bitmap,g_width,g_height,-1,-1);
    #endif
}

void set_first_shape()
{
    /* Forces the application to restore the size from it's maximized state when 
       it's initiated by simulating a click on the maximize/restore icon */
    rdp_send_input(time(NULL), RDP_INPUT_MOUSE, 32768|4096, g_width-28, 8);
    rdp_send_input(time(NULL), RDP_INPUT_MOUSE, 4096, g_width-28, 8);
    
    /* Captures the current window and creates a first shape_bitmap */
    shape_bitmap = (Pixmap)create_mask((HBITMAP)g_backstore, 0, 0, g_width,g_height);
    
    #ifdef NXDESKTOP_MASK_DUMP_DISK
    XWriteBitmapFile(g_display,"shape_bitmap.xbm",shape_bitmap,g_width,g_height,-1,-1);
    XWriteBitmapFile(g_display,"background.xbm",g_backstore,g_width,g_height,-1,-1);
    XWriteBitmapFile(g_display,"window.xbm",g_wnd,g_width,g_height,-1,-1);
    #endif

    XShapeCombineMask(g_display, g_wnd, ShapeBounding, 0, 0, shape_bitmap, ShapeSet);
    
    #ifdef NXDESKTOP_SHAPE_FORCESYNC
    XSync(g_display,False);
    #endif
    
    first_shape=False;
}

void make_mask_hole(int x, int y, int w, int h, int colour, BOOL hole)

{
    if (colour > 0)
    {
	if (TRANSLATE(colour) == BACKGROUND)
	    XSetForeground(g_display, shape_gc, 0);
	else
	    XSetForeground(g_display, shape_gc, 1);
    } else
    {
	if (hole)
	    XSetForeground(g_display, shape_gc, 0);
	else 
	    XSetForeground(g_display, shape_gc, 1);
    }
    XFillRectangle(g_display, shape_bitmap, shape_gc, x, y, w, h);
    
}

void nxdesktopShape(Pixmap src_pix, int src_x, int src_y, int dest_x, int dest_y, int width, int height, BOOL Dump)

{
    HBITMAP pix;
    
    pix = create_mask((HBITMAP)src_pix, src_x, src_y, width, height);
    
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    nxdesktopDebug("nxdesktopShape","shape coords %d %d %d %d %d %d\n",src_x, src_y, dest_x, dest_y, width, height);
    #endif

    XCopyArea(g_display, (Pixmap)pix, shape_bitmap, shape_gc, 0, 0, width, height, dest_x, dest_y);
    XShapeCombineMask(g_display, g_wnd, ShapeBounding, 0, 0, shape_bitmap, ShapeSet);
    
    #ifdef NXDESKTOP_MASK_DUMP_DISK
    if (Dump && float_window_mode)
    {
	XWriteBitmapFile(g_display,"pix.xbm",(Pixmap)pix,width,height,-1,-1);
    }
    #endif
    
    shape_need_update = False;
    
    #ifdef NXDESKTOP_SHAPE_FORCESYNC
    XSync(g_display, False);
    #endif
}

/* Verify point (x, y), and set shape_need_update accordingly */

void
shape_update_check(int x, int y, int rdp_color)
{
	BOOL in_region;
	uint32 col;

	col = TRANSLATE(rdp_color);

	in_region = !XGetPixel(shape_image, x, y);

	if (in_region && (col == BACKGROUND))
	{
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("shape_update_check","Painting with background in region\n");
	    #endif
	    shape_need_update = True;
	}
	else if (!in_region && (col != BACKGROUND))
	{
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("shape_update_check","Painting with normal col outside region\n");
	    #endif
	    shape_need_update = True;
	}
}

#endif

void nxdesktopSetAtoms()
{
	
    Atom identity;
    int type[4];
            
    type[0] = NXDESKTOP_SESSION;
    type[1] = NXDESKTOP_MAJOR_VERSION;
    type[2] = NXDESKTOP_MINOR_VERSION;
    type[3] = NXDESKTOP_RELEASE;

    XInternAtoms(g_display, nxdesktopAtomNames, NXDESKTOP_NUM_ATOMS, False, nxdesktopAtoms);
    
    if (nxdesktopAtoms[1] > nxdesktopAtoms[0])
    {	
	nxdesktopWithWM = False;
    }
    
    if (nxdesktopAtoms[8] > nxdesktopAtoms[0])
    {
	nxdesktopAtoms[8] = None;
    }

    #if NXDESKTOP_ATOMS_DEBUG
    for (i = 0; i < NXDESKTOP_NUM_ATOMS; i++)
    {
	nxdesktopDebug("nxagentQueryAtoms","Created intern atom [%s] with id [%ld].\n",
		nxdesktopAtomNames[i], nxdesktopAtoms[i]);
    }
    #endif
    
    identity = nxdesktopAtoms[0];
    XChangeProperty(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, identity, XA_ATOM, sizeof(int) * 8, PropModeReplace, (unsigned char *) &type,  4);
    
    XSetSelectionOwner(g_display, nxdesktopAtoms[5], g_viewport_wnd ? g_viewport_wnd : g_wnd, CurrentTime);
    
    XSetWMProtocols(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, &nxdesktopAtoms[2], 1);
}


BOOL
ui_create_window(void)
{
	uint8 null_pointer_mask[1] = { 0x80 };
	uint8 null_pointer_data[24] = { 0x00 };

	XSetWindowAttributes attribs;
	XClassHint *classhints;
	XSizeHints *sizehints;
	int wndwidth, wndheight;
	long input_mask, ic_input_mask;
 	XEvent xevent;
	/* NX */
	int i = 1;
	int x1, y1;
	XWMHints wmhints;
	static struct sigaction sigusr_act;
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	GC temp_gc;
	Pixmap temp_pix;
	#endif
	
	/* NX */

	if (!g_embed_wnd)
	{
	    wndwidth = g_width;
	    wndheight = g_height;
        }
	else
        {
	    wndwidth = g_fullscreen ? WidthOfScreen(g_screen) : g_width;
	    wndheight = g_fullscreen ? HeightOfScreen(g_screen) : g_height;
        }

	attribs.background_pixel = BlackPixelOfScreen(g_screen);
	attribs.border_pixel = WhitePixelOfScreen(g_screen);
	attribs.backing_store = g_ownbackstore ? NotUseful : Always;
	attribs.override_redirect = g_fullscreen;
	attribs.colormap = g_xcolmap;
	
	/* NX */
	#ifndef NXDESKTOP_OWNBACK
	if (attribs.backing_store == NotUseful)
	#endif
		g_ownbackstore = True;
	/*
	 * Always disable NoExpose and GraphicsExpose events.
	 */

        if (nxdesktopUseNXTrans)
           NXSetExposeEvents(g_display, True, False, False);

     {
	XVisualInfo *nxVisuals;
	XVisualInfo vi;
	int nxNumVisuals;
	long mask;
	XVisualInfo pV;

	nx_depth = DefaultDepth(g_display, DefaultScreen(g_display));
	mask = VisualScreenMask;
	vi.screen = DefaultScreen(g_display);
	nxVisuals = XGetVisualInfo(g_display, mask, &vi, &nxNumVisuals);
	pV = nxVisuals[0];
	r = pV.red_mask;
	g = pV.green_mask;
	b = pV.blue_mask;
	
	XFree(nxVisuals);
	
	if(!pV.red_mask|| !pV.green_mask|| !pV.blue_mask)
	{
		nx_black = 0x000000;
		nx_red   = 0xff0000;
		nx_white = 0xffffff;
	}
	else
	{
		for(or=0,off=0x800000;(r&(off>>or)) == 0; or++);
		for(og=0,off=0x800000;(g&(off>>og)) == 0; og++);
		for(ob=0,off=0x800000;(b&(off>>ob)) == 0; ob++);
		nx_red   = nxLogoColor(0xff0000);
		nx_black = nxLogoColor(0x000000);
		/* nx_white = nxLogoColor(0xffffff); */
		nx_white = 0xffffff;
	}
	useXpmIcon = getNXIcon(g_display, &nxIconPixmap, &nxIconShape);
    }
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	/* Warning - this is temporary as the flaoting mode should run fullscreen */
	
	sigusr_act.sa_handler = sigusr_func;
	sigfillset(&(sigusr_act.sa_mask));
	sigaction(SIGUSR1, &sigusr_act, NULL);
	sigaction(SIGUSR2, &sigusr_act, NULL);
	
	#endif
	
	if (g_fullscreen || ipaq)
	{
		attribs.override_redirect = True;
	/* Prepare signal handler for SIGUSR1 and SIGUSR2 */
		sigusr_act.sa_handler = sigusr_func;

	/* Install signal handler for SIGUSR1 and SIGUSR2 */
		sigfillset(&(sigusr_act.sa_mask));
		sigaction(SIGUSR1, &sigusr_act, NULL);
		sigaction(SIGUSR2, &sigusr_act, NULL);
		NXTransSignal(SIGUSR1, NX_SIGNAL_FORWARD);
		NXTransSignal(SIGUSR2, NX_SIGNAL_FORWARD);
		
                if (ipaq)
                {
                  g_width = WidthOfScreen(g_screen);
                  g_height = HeightOfScreen(g_screen);
                }

		/*
                if(ipaq)
                {
                    height = HeightOfScreen(screen)-1;
                }
		*/
                /*
                 * here move the window on -1 -1 to
                 * avoid seeing the border
                 */
                xo = 0;
                yo = 0;
	}
	else
	{
		attribs.override_redirect = False;
		sigusr_act.sa_handler = SIG_IGN;
		g_width = (g_width + 3) & ~3; /* make width a multiple of 32 bits */
	}
	
	g_width = g_width  & ~3; /* make width a multiple of 32 bits */

	/* NX */
	/*g_wnd = XCreateWindow(g_display, RootWindowOfScreen(g_screen), xo,yo, wndwidth, wndheight,
			      0, CopyFromParent, InputOutput, CopyFromParent,
			      CWBackPixel | CWBackingStore | CWOverrideRedirect |
			      CWColormap | CWBorderPixel, &attribs);*/

	g_wnd = XCreateWindow(g_display, RootWindowOfScreen(g_screen), xo, yo, wndwidth, wndheight,
			      0, CopyFromParent, InputOutput, CopyFromParent,
			      CWBackingStore | (g_fullscreen ? CWOverrideRedirect:
			      SubstructureRedirectMask) | StructureNotifyMask, &attribs);
	
	if (g_gc == NULL)
		g_gc = XCreateGC(g_display, g_wnd, 0, NULL);

	if (g_create_bitmap_gc == NULL)
		g_create_bitmap_gc = XCreateGC(g_display, g_wnd, 0, NULL);

	if ((g_ownbackstore) && (g_backstore == 0))
	{
		g_backstore = XCreatePixmap(g_display, g_wnd, g_width, g_height, g_depth);

		/* clear to prevent rubbish being exposed at startup */
		XSetForeground(g_display, g_gc, BlackPixelOfScreen(g_screen));
		XFillRectangle(g_display, g_backstore, g_gc, 0, 0, g_width, g_height);
	}
	
	/* NX */
	
	if (g_fullscreen)
	{
		attribs.override_redirect = False;

		/*wnd2 = XCreateWindow (g_display, DefaultRootWindow (g_display), 0, 0, 1, 1,
				0, CopyFromParent, InputOutput, CopyFromParent,
				CWBackingStore | CWBackPixel , &attribs);*/
                                
		wnd2 = XCreateWindow (g_display, RootWindowOfScreen(g_screen), 0, 0, 1, 1,
				0, CopyFromParent, InputOutput, CopyFromParent,
				CWBackingStore | CWBackPixel , &attribs);

	}
	
	/* NX */
	
        if (!g_embed_wnd && !ipaq)
	{
	    if (g_fullscreen)
	    {
        	x1 = (WidthOfScreen(g_screen) - g_width)/2;
        	y1 = (HeightOfScreen(g_screen) - g_height)/2;
        	g_viewport_width = (WidthOfScreen(g_screen));
        	g_viewport_height= (HeightOfScreen(g_screen));
	    }
            else
	    {
        	xo = g_saved_viewport_x;
        	yo = g_saved_viewport_y;

        	if (g_saved_wnd_x == 0 && g_saved_wnd_y == 0)
        	{
        	    x1 = y1 = 0;
        	}
        	else
        	{
            	    x1 = g_saved_wnd_x;
        	    y1 = g_saved_wnd_y;
        	}

        	if (g_saved_viewport_width && g_saved_viewport_height)
        	{
            	    g_viewport_width = g_saved_viewport_width;
            	    g_viewport_height = g_saved_viewport_height;
        	}
        	else
        	{
    		    g_viewport_width = wndwidth < WidthOfScreen(g_screen) ? wndwidth : WidthOfScreen(g_screen)*3/4;
    		    g_viewport_height= wndheight< HeightOfScreen(g_screen)? wndheight: HeightOfScreen(g_screen)*3/4;
    		}
    	    }

	    attribs.override_redirect = g_fullscreen;

            if (viewport_mode)
	    {
		g_viewport_wnd = XCreateWindow (g_display, RootWindowOfScreen(g_screen), xo,yo,
                                		    g_viewport_width, g_viewport_height, 0, g_depth,
                                			InputOutput, g_visual, CWBackPixel | CWBackingStore |
                                        		    CWOverrideRedirect | CWColormap |
                                            			CWBorderPixel, &attribs);

    		XSelectInput(g_display, g_viewport_wnd, ButtonPressMask | KeyPressMask |
                		StructureNotifyMask | (g_fullscreen ? EnterWindowMask : 0) |
                    		    (g_grab_keyboard ? (EnterWindowMask | LeaveWindowMask) : 0));
		XReparentWindow (g_display, g_wnd, g_viewport_wnd, x1,y1);
	    }
	}

	/* Check XStoreName consistency */
		
	XStoreName(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, g_title);

	if (g_hide_decorations)
		mwm_hide_decorations();

	classhints = XAllocClassHint();
	if (classhints != NULL)
	{
		classhints->res_name = classhints->res_class = "nxdesktop";
		XSetClassHint(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, classhints);
		XFree(classhints);
	}

	sizehints = XAllocSizeHints();
	if (sizehints)
	{
	    sizehints->flags = USPosition | PMinSize | PMaxSize;
	    sizehints->min_width = NXDESKTOP_MIN_WIDTH;
            sizehints->max_width = g_width;
	    sizehints->min_height = NXDESKTOP_MIN_HEIGHT;
            sizehints->max_height = g_height;
	    XSetStandardProperties(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd,
				(windowName?windowName:"nxdesktop"),
				(windowName?windowName:"nxdesktop"),
				nxIconPixmap, 0, 0, sizehints);

	    wmhints.icon_pixmap = nxIconPixmap;
	    if (useXpmIcon)
	    {
		wmhints.icon_mask = nxIconShape;
		wmhints.flags = IconPixmapHint | IconMaskHint;
	    } else 
	    {
		wmhints.flags = StateHint | IconPixmapHint;
	    }
	    XSetWMHints (g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, &wmhints);
	    XSetWMNormalHints(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, sizehints);
	}

	if (g_embed_wnd)
	{
	    XReparentWindow(g_display, g_wnd, (Window) g_embed_wnd, 0, 0);
	}

	/* NX */
	input_mask = KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask |
		VisibilityChangeMask | FocusChangeMask;
		
	if (g_fullscreen)
	 	input_mask |= (EnterWindowMask | LeaveWindowMask);	
	
	/* NX */
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{
	    /* Sets an inital mask */
	    temp_pix = XCreatePixmap(g_display, g_wnd, g_width, g_height, (unsigned int) 1);
 	    temp_gc = XCreateGC(g_display, temp_pix, 0, NULL);
	    XSetForeground(g_display, temp_gc, BlackPixelOfScreen(g_screen));
	    XFillRectangle(g_display, temp_pix, temp_gc, 0, 0, g_width, g_height);
	    /*XSetForeground(g_display, shape_gc, WhitePixelOfScreen(g_screen));
	      XFillRectangle(g_display, shape_bitmap, shape_gc, 300, 300, g_width-200, g_height-200); */
	    XShapeCombineMask(g_display, g_wnd, ShapeBounding, 0, 0, temp_pix, ShapeSet);
	    #ifdef NXDESKTOP_SHAPE_FORCESYNC
	    XSync(g_display,False);
	    #endif
	    XFreePixmap(g_display, temp_pix);
	    XFreeGC(g_display, temp_gc);
	}
	#endif
	/* NX */
	
	if (g_fullscreen)
        {

          XStoreName(g_display, wnd2, (windowName?windowName:"nxdesktop"));

          classhints = XAllocClassHint();
          if (classhints != NULL)
          {
            classhints->res_name = classhints->res_class = "nxdesktop";
            XSetClassHint(g_display, wnd2, classhints);
            XFree(classhints);
          }

		sizehints->flags = USPosition | PMinSize | PMaxSize;
		sizehints->min_width = sizehints->max_height = 1;
		sizehints->min_height = sizehints->max_height = 1;
    		XSetStandardProperties(g_display,
			   wnd2,
			   (windowName?windowName:"nxdesktop"),
			   (windowName?windowName:"nxdesktop"),
			   nxIconPixmap,
			   0, 0, sizehints);

	/* Start helper window in iconified state */
		wmhints.flags = IconPixmapHint | IconMaskHint;
		wmhints.initial_state = IconicState;
		wmhints.icon_pixmap = nxIconPixmap;
		if (useXpmIcon)
		{
		   wmhints.icon_mask = nxIconShape;
		   wmhints.flags = IconPixmapHint | IconMaskHint;
		} else {
		   wmhints.flags = StateHint | IconPixmapHint;
		}
		XSetWMHints (g_display, wnd2, &wmhints);
                if(!ipaq)
		    XMapWindow( g_display, wnd2 );
	}

        if (sizehints)
	  XFree(sizehints);

	/* NX */
	if (g_sendmotion)
		input_mask |= PointerMotionMask;
	if (g_ownbackstore)
		input_mask |= ExposureMask;
	if (g_fullscreen || g_grab_keyboard)
		input_mask |= EnterWindowMask;
	if (g_grab_keyboard)
		input_mask |= LeaveWindowMask;

	if (g_IM != NULL)
	{
		g_IC = XCreateIC(g_IM, XNInputStyle, (XIMPreeditNothing | XIMStatusNothing),
				 XNClientWindow, g_wnd, XNFocusWindow, g_wnd, NULL);

		if ((g_IC != NULL)
		    && (XGetICValues(g_IC, XNFilterEvents, &ic_input_mask, NULL) == NULL))
			input_mask |= ic_input_mask;
	}

	XSelectInput(g_display, g_wnd, input_mask);
	if (g_fullscreen)
		XSelectInput (g_display, wnd2, (input_mask & ~(KeyPressMask | KeyReleaseMask)) | StructureNotifyMask);
	
	if (g_viewport_wnd)
	{
    	    XMapWindow (g_display, g_viewport_wnd);
    	}
        XMapWindow(g_display, g_wnd); 
	XGrabKeyboard(g_display, g_wnd, True, GrabModeAsync, GrabModeAsync, CurrentTime);

	/* wait for VisibilityNotify 
	do
	{
		XMaskEvent(g_display, VisibilityChangeMask, &xevent);
	}
	while (xevent.type != VisibilityNotify);*/
	g_Unobscured = xevent.xvisibility.state == VisibilityUnobscured;

	g_focused = False;
	g_mouse_in_wnd = False;

	/* handle the WM_DELETE_WINDOW protocol */
	/*g_protocol_atom = XInternAtom(g_display, "WM_PROTOCOLS", True);
	g_kill_atom = XInternAtom(g_display, "WM_DELETE_WINDOW", True);*/

	nxdesktopSetAtoms();
	/* create invisible 1x1 cursor to be used as null cursor */
	if (g_null_cursor == NULL)
		g_null_cursor = ui_create_cursor(0, 0, 1, 1, null_pointer_mask, null_pointer_data);
		
	/* NX */
	#if defined(NXDESKTOP_LOGO) && !defined(NKDESKTOP_SPLASH)
	if (showNXlogo)
        {
	    /*
	    * if you want some animation, change the initial value to 5 and the sleep to 1
	    */
          
	    Cursor cursor;
	    cursor = XCreateFontCursor(g_display, 0);
	    XDefineCursor(g_display, g_wnd, (Cursor)cursor);
	  
	    while (i)
	    {
        	nomachineLogo(g_wnd, g_gc, i);
        	i--;
        	sleep(3);
	    }
	    
	    XSync(g_display, True);
        }
	#endif
        if (ipaq)
	{
            XWindowChanges ch;
            unsigned int ch_mask;
            ch.stack_mode = Below;
            ch_mask = CWStackMode;
            XConfigureWindow(g_display, g_wnd, ch_mask, &ch);
        }

        if (g_fullscreen)
        {
           XIconifyWindow (g_display, wnd2, DefaultScreen(g_display));
        }
        #ifdef NXDESKTOP_XWIN_USES_PIXMAP_CACHE
	
	for (i = 0; i < PIXCACHE_ENTRIES; i++)
	{
		pixmap_cache[i].pixmap = (Pixmap) NULL;
		pixmap_cache[i].offset = 0;
	}

	#endif
	
	#ifdef NXDESKTOP_FWINDOW_MODE
	for (i = 0; i < MAX_NXCLIPPER_WINDOWS; i++)
	{
		nxclipper_windows[i].id = 0;
		nxclipper_windows[i].wnd = (Window) NULL;
	}
	#endif

	/*
        {
            g_mod_map = XGetModifierMapping(g_display);
            xkeymap_init();
        }
	 NX */
	 
	return True;
}

void
ui_resize_window()
{
	XSizeHints *sizehints;
	Pixmap bs;

	sizehints = XAllocSizeHints();
	if (sizehints)
	{
		sizehints->flags = PMinSize | PMaxSize;
		sizehints->min_width = sizehints->max_width = g_width;
		sizehints->min_height = sizehints->max_height = g_height;
		XSetWMNormalHints(g_display, g_wnd, sizehints);
		XFree(sizehints);
	}

	if (!(g_fullscreen || g_embed_wnd))
	{
		XResizeWindow(g_display, g_wnd, g_width, g_height);
	}

	/* create new backstore pixmap */
	if (g_backstore != 0)
	{
		bs = XCreatePixmap(g_display, g_wnd, g_width, g_height, g_depth);
		XSetForeground(g_display, g_gc, BlackPixelOfScreen(g_screen));
		XFillRectangle(g_display, bs, g_gc, 0, 0, g_width, g_height);
		XCopyArea(g_display, g_backstore, bs, g_gc, 0, 0, g_width, g_height, 0, 0);
		XFreePixmap(g_display, g_backstore);
		g_backstore = bs;
	}
}


void
ui_destroy_window(void)
{
	if (g_IC != NULL)
		XDestroyIC(g_IC);

        if (g_viewport_wnd)
        {
	    XDestroyWindow (g_display, g_viewport_wnd);
        }
        else
        {
	    XDestroyWindow(g_display, g_wnd);
        }

	/* NX */
	if (g_fullscreen)
	  XDestroyWindow (g_display, wnd2);
	 /* NX */
}

void
xwin_toggle_fullscreen(void)
{
	Pixmap contents = 0;
        Bool savedShowNXlogo = showNXlogo;

	if (!g_ownbackstore)
	{
		/* need to save contents of window */
		contents = XCreatePixmap(g_display, g_wnd, g_width, g_height, g_depth);
		XCopyArea(g_display, g_wnd, contents, g_gc, 0, 0, g_width, g_height, 0, 0);
	}

	showNXlogo = False;
        
        if (!g_fullscreen)
        {
          g_saved_wnd_x = g_wnd_x;
          g_saved_wnd_y = g_wnd_y;
        }

	ui_destroy_window();
	g_fullscreen = !g_fullscreen;
	ui_create_window();

	showNXlogo = savedShowNXlogo;

	XDefineCursor(g_display, g_wnd, g_current_cursor);

	if (!g_ownbackstore)
	{
		XCopyArea(g_display, contents, g_wnd, g_gc, 0, 0, g_width, g_height, 0, 0);
		XFreePixmap(g_display, contents);
	}
}

/* NX
 static uint16
 xwin_translate_mouse(unsigned long button)
 {
 	switch (button)
 	{
 		case Button1:	 left
 			return MOUSE_FLAG_BUTTON1;
 		case Button2:	 middle 
 			return MOUSE_FLAG_BUTTON3;
 		case Button3:	 right
 			return MOUSE_FLAG_BUTTON2;
 	}
 
 	return 0;
 }
 NX */

/* Process all events in Xlib queue
   Returns 0 after user quit, 1 otherwise */
static int
xwin_process_events(void)
{
	XEvent xevent;
	KeySym keysym;
	uint16 button, flags;
	key_translation tr;
	char str[256];
	Status status;

	/* NX used only to send RDP inputs for which we can't obtain the real X server time */
	uint32 ev_time = (last_Xtime == 0) ? (uint32)time(NULL): last_Xtime;
	
	while (XPending(g_display) > 0)
	{
		XNextEvent(g_display, &xevent);
		if ((g_IC != NULL) && (XFilterEvent(&xevent, None) == True))
		{
		    #ifdef NXDESKTOP_XWIN_DEBUG
		    nxdesktopDebug("xwin_process_events","Filtering event.\n");
		    #endif
		    DEBUG_KBD(("Filtering event\n"));
		    continue;
		}

		flags = 0;
		
		/* NX */
		if (xevent.xclient.message_type == nxdesktopAtoms[1])
          	{
			Atom wmAtom, deleteAtom;
			
			wmAtom = (Atom) xevent.xclient.data.l[0];
			deleteAtom = (Atom) nxdesktopAtoms[2];
			
			if (wmAtom == deleteAtom)
			{
				/* simulate ESC pressed */
		     		rdp_send_input(ev_time, RDP_INPUT_SCANCODE, 0, 0x01, 0);
		     		/* simulate ESC released */
		     		rdp_send_input(ev_time, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0x01, 0);
                     		/* simulates CTRL-ALT-DEL */
		     		rdp_send_input(ev_time, RDP_INPUT_SCANCODE, 0, 0x1D, 0);
		     		rdp_send_input(ev_time, RDP_INPUT_SCANCODE, 0, 0x38, 0);
		     		rdp_send_input(ev_time, RDP_INPUT_SCANCODE, 0, 0xD3, 0);
                     		/* simulates CTRL-ALT-DEL released */
		     		rdp_send_input(ev_time, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0x1D, 0);
		     		rdp_send_input(ev_time, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0x38, 0);
		     		rdp_send_input(ev_time, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0xD3, 0);
		     		if (g_fullscreen)
		         	    sigusr_func(SIGUSR2);
			}
	  	}
		/*
		#ifdef NXDESKTOP_USES_NXKARMA_IN_LOOP
		if (xevent.xclient.window == 0 &&
              		xevent.xclient.message_type == 0 &&
              		xevent.xclient.format == 32 &&
              		(int) xevent.xclient.data.l[0] == NXSyncNotify)
          	{
             		nxdesktopSleep = False;
			#ifdef NXDESKTOP_NXKARMA_DEBUG
             		fprintf(stderr," NXKarma wake up arrived!\n");
			#endif
          	}
		#endif
		*/
		/* NX */
			
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("xwin_process_events","Xevent type %d.\n",xevent.type);
		#endif
		
		
		
		switch (xevent.type)
		{
			/* NX */
			
			case MapNotify:
			    if (g_fullscreen) 
				sigusr_func(SIGUSR2);
			break;
			
			/* NX */
			
			case VisibilityNotify:
				g_Unobscured = xevent.xvisibility.state == VisibilityUnobscured;
				break;
				
			/*
			case ClientMessage:
				 the window manager told us to quit 
				if ((xevent.xclient.message_type == g_protocol_atom)
				    && ((Atom) xevent.xclient.data.l[0] == g_kill_atom))
					 Quit 
					return 0;
				break;
			*/
			
			case KeyPress:
				g_last_gesturetime = xevent.xkey.time;
				if (g_IC != NULL)
					/* Multi_key compatible version */
				{
				    XmbLookupString(g_IC, &xevent.xkey, str, sizeof(str), &keysym,
							&status);
					if (!((status == XLookupKeySym) || (status == XLookupBoth)))
					{
						error("XmbLookupString failed with status 0x%x\n",
						      status);
						break;
					}
				}
				else
				{
					/* Plain old XLookupString */
					DEBUG_KBD(("\nNo input context, using XLookupString\n"));
					XLookupString((XKeyEvent *) & xevent,
						      str, sizeof(str), &keysym, NULL);
				}
				
				
				#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
				#ifdef NXDESKTOP_FWINDOW_DEBUG
				if ((keysym == XK_F12) && (float_window_mode))
				{
				    XRectangle rect;
				
				    rect.x = 0;
				    rect.y = 0;
				    rect.width = g_width;
				    rect.height = g_height;
				    XShapeCombineRectangles(g_display, g_wnd, ShapeBounding, 0, 0, &rect, 1, ShapeSet, 0);
				}
				
				if ((keysym == XK_F11) && (float_window_mode))
				{
				    fprintf(stderr,"Creating snapshot\n");
				    XWriteBitmapFile(g_display,"shape_bitmap.xbm",shape_bitmap,g_width,g_height,-1,-1);
				    XWriteBitmapFile(g_display,"background.xbm",g_backstore,g_width,g_height,-1,-1);
				    XWriteBitmapFile(g_display,"window.xbm",g_wnd,g_width,g_height,-1,-1);
				    fprintf(stderr,"Snapshot done\n");
				}
				#endif
				#endif

				DEBUG_KBD(("KeyPress for (keysym 0x%lx, %s)\n", keysym,
					   get_ksname(keysym)));

				ev_time = time(NULL);
				if (handle_special_keys(keysym, xevent.xkey.state, ev_time, True))
					break;

				tr = xkeymap_translate_key(keysym,
							   xevent.xkey.keycode, xevent.xkey.state);
							   
				/* NX
				scancode = tr.scancode; */

				if ( (xevent.xkey.keycode == 130) && ipaq )
				{
					kill (pidkbd, 1);
					#ifdef NXDESKTOP_XWIN_DEBUG
					nxdesktopDebug("xwin_process_events","signal send -HUP\n");
					#endif
					break;
				}
                                last_Xtime = xevent.xkey.time;
				/* NX */

				if (tr.scancode == 0)
					break;
				/* NX */
				/* When in g_fullscreen, unmap window on Ctrl-Alt-Escape */

				/* 
                                 * if ((tr.scancode == 0x01) &&
                                 */

                                if ((keysym == XK_m || keysym == XK_M) &&
					(xevent.xkey.state & (ControlMask | Mod1Mask )) ==
					(ControlMask|Mod1Mask)  )
				{
					DEBUG (("Ctrl-Alt-ESC pressed\n"));
					if (magickey && g_fullscreen)
					{
					   if (nxdesktopWithWM)
					   {
					      sigusr_func(SIGUSR1);
					   }
					   else
					   {
					       rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, 0, 0xD3, 0);
					       rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0xD3, 0);
					   }
					   break;
					}
				}
				/* NX */	
				save_remote_modifiers(tr.scancode);
				ensure_remote_modifiers(ev_time, tr);
				rdp_send_scancode(ev_time, RDP_KEYPRESS, tr.scancode);
				restore_remote_modifiers(ev_time, tr.scancode);
				
				/* NX */
				PressedKeys[tr.scancode] = 1;
				/* NX */
				
				break;

			case KeyRelease:
				g_last_gesturetime = xevent.xkey.time;
				XLookupString((XKeyEvent *) & xevent, str,
					      sizeof(str), &keysym, NULL);

				DEBUG_KBD(("\nKeyRelease for (keysym 0x%lx, %s)\n", keysym,
					   get_ksname(keysym)));

				ev_time = time(NULL);
				if (handle_special_keys(keysym, xevent.xkey.state, ev_time, False))
					break;

				tr = xkeymap_translate_key(keysym,
							   xevent.xkey.keycode, xevent.xkey.state);
							   
				/* NX
				scancode = tr.scancode; */

                                last_Xtime = xevent.xkey.time;
				/* NX */
				
				if (tr.scancode == 0)
					break;

				rdp_send_scancode(ev_time, RDP_KEYRELEASE, tr.scancode);
				break;
				
				/* NX */
				PressedKeys[tr.scancode] = 0;
				/* NX */
				
			case ButtonPress:
				flags = MOUSE_FLAG_DOWN;
				/* fall through */
				
				/* NX - Still go?
				button = xwin_translate_mouse(event.xbutton.button); */
                                button = xkeymap_translate_button(xevent.xbutton.button);
				
				last_Xtime = xevent.xbutton.time;
				
				if (button == 0)
					break;
				
				#ifdef MAGIC_PIXEL
			         /* Iconify rdesktop when left-clicking in upper right corner */
				if ((xevent.xbutton.button == 1) &&
					(xevent.xbutton.x_root >= (WidthOfScreen(g_screen) - 2)) &&
						(xevent.xbutton.y_root <= 2))
				{
					if (g_fullscreen)
					{
					   if (nxdesktopWithWM)
					   {
					      sigusr_func(SIGUSR1);
					   }
					   else
					   {
						rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, 0, 0x01, 0);
						/* simulate ESC released */
						rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0x01, 0);
						/* simulates CTRL-ALT-DEL */
						rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, 0, 0x1D, 0);
						rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, 0, 0x38, 0);
						rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, 0, 0xD3, 0);
						/* simulates CTRL-ALT-DEL released */
						rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0x1D, 0);
						rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0x38, 0);
						rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE, KBD_FLAG_DOWN | KBD_FLAG_UP, 0xD3, 0);
					   }
					   break;
					}
				}
				#endif
				if (xevent.xbutton.window == g_viewport_wnd)
				{
					if ((xevent.xbutton.x < (g_width - 2) ||
						(xevent.xbutton.x >= (g_width - 2) && 
							xevent.xbutton.y > 2)))
					{
					    break;
					}
					if (xevent.xbutton.button != 1)
					{
					    break;
					}
				}
                                if ((xevent.xbutton.state & (ControlMask | Mod1Mask)) ==
                                        (ControlMask | Mod1Mask) && g_viewport_wnd)
                                {
                                  /*
                                   * Start viewport navigation mode.
                                   */

                                  viewportCursor = XCreateFontCursor(g_display, XC_fleur);

                                  XGrabPointer(g_display, g_wnd, True,
                                                   PointerMotionMask | ButtonPressMask |
                                                       ButtonReleaseMask, GrabModeAsync,
                                                           GrabModeAsync, None, viewportCursor,
                                                               CurrentTime);
                                  viewportLastX = xevent.xbutton.x_root;
                                  viewportLastY = xevent.xbutton.y_root;

                                  break;
                                }

				/* NX */
				
			case ButtonRelease:
				g_last_gesturetime = xevent.xbutton.time;
				/* NX */
				last_Xtime = g_last_gesturetime;
				
                                if (viewportCursor)
                                {
                                  /*
                                   * Leave viewport navigation mode.
                                   */

                                  XUngrabPointer(g_display, CurrentTime);
                                  XFreeCursor(g_display, viewportCursor);
                                  viewportCursor = None;

                                  break;
                                }

				button = xkeymap_translate_button(xevent.xbutton.button);
				if (button == 0)
					break;
					
				/* If win_button_size is nonzero, enable single app mode */
				if (xevent.xbutton.y < g_win_button_size)
				{
					/* Stop moving window when button is released, regardless of cursor position */
					if (g_moving_wnd && (xevent.type == ButtonRelease))
						g_moving_wnd = False;
						
					/*  Check from right to left: */

					if (xevent.xbutton.x >= g_width - g_win_button_size)
					{
						/* The close button, continue */
						;
					}
					else if (xevent.xbutton.x >=
						 g_width - g_win_button_size * 2)
					{
						/* The maximize/restore button. Do not send to
						   server.  It might be a good idea to change the
						   cursor or give some other visible indication
						   that rdesktop inhibited this click */
						break;
					}
					else if (xevent.xbutton.x >=
						 g_width - g_win_button_size * 3)
					{
						/* The minimize button. Iconify window. */
						XIconifyWindow(g_display, g_wnd,
							       DefaultScreen(g_display));
						break;
					}
					else if (xevent.xbutton.x <= g_win_button_size)
					{
						/* The system menu. Ignore. */
						break;
					}
					else
					{
						/* The title bar. */
						if ((xevent.type == ButtonPress) && !g_fullscreen
						    && g_hide_decorations)
						{
							g_moving_wnd = True;
							g_move_x_offset = xevent.xbutton.x;
							g_move_y_offset = xevent.xbutton.y;
						}
						break;

					}
				}
				
				if ((float_window_mode) && (xevent.xbutton.y>3) && (xevent.xbutton.y<20))
				{
				    if (g_moving_wnd)
					g_moving_wnd = False;
				    else
				    {
				    	g_moving_wnd = True;
					g_move_x_offset = xevent.xbutton.x;
					g_move_y_offset = xevent.xbutton.y;
				    }
				}
				
				rdp_send_input(time(NULL), RDP_INPUT_MOUSE,
					       flags | button, xevent.xbutton.x + g_x_offset, xevent.xbutton.y + g_y_offset);
				break;

			case MotionNotify:
			
				if (g_moving_wnd)
				{
					XMoveWindow(g_display, g_wnd,
						    xevent.xmotion.x_root - g_move_x_offset,
						    xevent.xmotion.y_root - g_move_y_offset);
					break;
				}
				
                                if (viewportCursor)
                                {
                                  /*
                                   * Pointer is in viewport navigation mode.
                                   */
				   
                                  nxdesktopMoveViewport(viewportLastX - xevent.xmotion.x_root,
                                                            viewportLastY - xevent.xmotion.y_root);

                                  viewportLastX = xevent.xmotion.x_root;
                                  viewportLastY = xevent.xmotion.y_root;

                                  break;
                                }

				if (g_fullscreen && !g_focused)
					XSetInputFocus(g_display, g_wnd, RevertToPointerRoot,
						       CurrentTime);      
				last_Xtime = xevent.xmotion.time;
				
				rdp_send_input(time(NULL), RDP_INPUT_MOUSE,
					       MOUSE_FLAG_MOVE, xevent.xmotion.x + g_x_offset, xevent.xmotion.y + g_y_offset);
					       
				break;

			case FocusIn:
				if (xevent.xfocus.mode == NotifyGrab)
					break;
				g_focused = True;
				reset_modifier_keys();
				if (g_grab_keyboard && g_mouse_in_wnd && g_fullscreen)
					XGrabKeyboard(g_display, g_wnd, True,
						      GrabModeAsync, GrabModeAsync, CurrentTime);
				break;

			case FocusOut:
				if (xevent.xfocus.mode == NotifyUngrab)
					break;
				g_focused = False;
				if (xevent.xfocus.mode == NotifyWhileGrabbed && !g_fullscreen)
					XUngrabKeyboard(g_display, CurrentTime);
				break;

			case EnterNotify:
				/* we only register for this event when in fullscreen mode */
				/* or grab_keyboard */
				g_mouse_in_wnd = True;
				if (g_fullscreen)
				{
					XSetInputFocus(g_display, g_wnd, RevertToPointerRoot,
						       CurrentTime);
					break;
				}
				
				/* NX */
				last_Xtime = xevent.xcrossing.time;
				/* NX */
				
				if (g_focused && g_fullscreen)
					{
					XGrabKeyboard(g_display, g_wnd, True,
						      GrabModeAsync, GrabModeAsync, CurrentTime);
					}
				break;

			case LeaveNotify:
				/* we only register for this event when grab_keyboard */
				g_mouse_in_wnd = False;
				
				/* NX */
				{
				  int i;
                                  last_Xtime = xevent.xcrossing.time;
				  for (i = 0; i < 256; i++)
				  if (PressedKeys[i])
				     rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE,
						KBD_FLAG_DOWN | KBD_FLAG_UP, i, 0);
				}
                                if(ipaq)
                                   break;
				/* NX */				

				XUngrabKeyboard(g_display, CurrentTime);
				break;

			case Expose:
				XCopyArea(g_display, g_backstore, g_wnd, g_gc,
					  xevent.xexpose.x, xevent.xexpose.y,
					  xevent.xexpose.width,
					  xevent.xexpose.height,
					  xevent.xexpose.x, xevent.xexpose.y);
				break;

			case MappingNotify:
				/* Refresh keyboard mapping if it has changed. This is important for
				   Xvnc, since it allocates keycodes dynamically */
				if (xevent.xmapping.request == MappingKeyboard
				    || xevent.xmapping.request == MappingModifier)
					XRefreshKeyboardMapping(&xevent.xmapping);

				if (xevent.xmapping.request == MappingModifier)
				{
					XFreeModifiermap(g_mod_map);
					g_mod_map = XGetModifierMapping(g_display);
				}
				break;

				/* clipboard stuff */
			case SelectionNotify:
				if (g_use_rdp5)
				    xclip_handle_SelectionNotify(&xevent.xselection);
				break;
			case SelectionRequest:
				if (g_use_rdp5)
				    xclip_handle_SelectionRequest(&xevent.xselectionrequest);
				break;
			case SelectionClear:
				if (g_use_rdp5)
				    xclip_handle_SelectionClear();
				break;
			case PropertyNotify:
				if (g_use_rdp5)
				    xclip_handle_PropertyNotify(&xevent.xproperty);
				break;
                        case ConfigureNotify:
                                if ((xevent.xconfigure.window == g_viewport_wnd) && !g_fullscreen)
                                {
                    		    g_saved_viewport_x = xevent.xconfigure.x;
                		    g_saved_viewport_y = xevent.xconfigure.y;
                                    g_viewport_width = g_saved_viewport_width = (xevent.xconfigure.width);
                                    g_viewport_height = g_saved_viewport_height = (xevent.xconfigure.height);
                                    nxdesktopMoveViewport(0, 0);
                    		}
                                break;
		}
	}
	/* Keep going */
	return 1;
}

/* NX */

void run_events()

{
    if (!xwin_process_events())
    {
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("run_events","No more events to process.\n");
	#endif
    }
    return;
}

int nxdesktopSelect(int maxfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout)
{
    #ifdef NX_SELECT_DEBUG
    nxdesktopDebug("nxdesktopSelect","Called with [%d][%p][%p][%p][%p].\n", 
	maxfds, (void *) readfds, (void *) writefds, (void *) exceptfds, (void *) timeout);
    #endif
    if (NXTransRunning())
    {
        fd_set t_readfds, t_writefds;
        struct timeval t_timeout;
        int n, r, e;
	#ifdef NX_SELECT_DEBUG
	if (exceptfds != NULL)
	{
	    nxdesktopDebug("nxdesktopSelect", "Can't handle exception fds in select. Exiting.\n");
	    exit(1);
	}
	#endif
	if (readfds == NULL)
	{
	    FD_ZERO(&t_readfds);
            readfds = &t_readfds;
        }
        if (writefds == NULL)
        {
            FD_ZERO(&t_writefds);
	    writefds = &t_writefds;
	}
        if (timeout == NULL)
        {
	    t_timeout.tv_sec  = 10;
	    t_timeout.tv_usec = 0;
	    timeout = &t_timeout;
	}

        n = maxfds;

	if (NXTransPrepare(&n, readfds, writefds, timeout) != 0)
	{
	    NXTransSelect(&r, &e, &n, readfds, writefds, timeout);
            NXTransExecute(&r, &e, &n, readfds, writefds, timeout);
	    errno = e;
            return r;
	}
	else
	{
	    return 0;
	}
    }
    else
    {
	return select(maxfds, readfds, writefds, exceptfds, timeout);
    }
}

/* Returns 0 after user quit, 1 otherwise */
int
ui_select(int rdp_socket)
{
	int n;
	fd_set rfds, wfds;
	struct timeval tv;
	BOOL s_timeout = False;
	
	/* NX
	 * Be sure we get all events from X server.
	 */
	 
	#ifdef NXDESKTOP_XWIN_USES_SYNC_IN_LOOP
	XSync(g_display, False);
	#else
	#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
        XFlush(g_display);
	#endif
	#endif
	
	while (True)
	{
	    n = (rdp_socket > g_x_socket) ? rdp_socket : g_x_socket;
	    /* Process any events already waiting */
	    if (!xwin_process_events())
	    {
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_select","User quit.\n");
		#endif
		return 0;
	    }
	    
	    FD_ZERO(&rfds);
	    FD_ZERO(&wfds);
	    FD_SET(rdp_socket, &rfds);
	    FD_SET(g_x_socket, &rfds);
	

	    #ifdef WITH_RDPSND
	    /* FIXME: there should be an API for registering fds */
	    if (g_dsp_busy)
	    {
		FD_SET(g_dsp_fd, &wfds);
		n = (g_dsp_fd > n) ? g_dsp_fd : n;
	    }
	    #endif
	    /* default timeout */
	    tv.tv_sec = 60;
	    tv.tv_usec = 0;
	    
	    /* add redirection handles */
	    rdpdr_add_fds(&n, &rfds, &wfds, &tv, &s_timeout);
	    n++;

	    Retry:
	    switch (nxdesktopSelect(n, &rfds, &wfds, NULL, &tv))
	    {
		case -1:
		    if (errno != EINTR)
		    {
			error("select: %s\n", strerror(errno));
		    }
		    else
		    {
			warning("ui_select", "EINTR fired!\n");
			goto Retry;
		    }
		case 0:
		    /* Abort serial read calls */
		    if (s_timeout)
			rdpdr_check_fds(&rfds, &wfds, (BOOL) True);
		    continue;
	    }
		
	    rdpdr_check_fds(&rfds, &wfds, (BOOL) False);
		
	    if (FD_ISSET(rdp_socket, &rfds))
		return 1;

	    #ifdef WITH_RDPSND
	    if (g_dsp_busy && FD_ISSET(g_dsp_fd, &wfds))
	        wave_out_play();
	    #endif
	}
}

void
ui_move_pointer(int x, int y)
{
	XWarpPointer(g_display, g_wnd, g_wnd, 0, 0, 0, 0, x, y);
}

HBITMAP
ui_create_bitmap(int width, int height, uint8 * data, int size, BOOL compressed)
{
	XImage *image;
	Pixmap bitmap;
	uint8 *tdata;
	int bitmap_pad;

	if (g_server_bpp == 8)
	{
		bitmap_pad = 8;
	}
	else
	{
		bitmap_pad = g_bpp;

		if (g_bpp == 24)
			bitmap_pad = 32;
	}
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_create_bitmap","dump bitmap compressed: %d , size: %d\n",compressed,size);;
	hexdump(data,size);
	#endif
	
	bitmap = XCreatePixmap(g_display, g_wnd, width, height, g_depth);
				 
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_create_bitmap","XPutImage on pixmap %d,%d,%d,%d.\n", 0, 0, width, height);
	#endif
	
	#ifdef NXDESKTOP_DEBUG_XPUTIMAGE	    
	create_bitmap_times++;
	create_bitmap_total+=image->height*image->bytes_per_line;
	#endif
	
	#ifdef NXDESKTOP_IMGCACHE_USES_COMPRESSED_IMAGES
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	if (compressed)
	    nxdesktopDebug("ui_create_bitmap","Received compressed bitmap.\n");
	else 
	    nxdesktopDebug("ui_create_bitmap","Received uncompressed bitmap.\n");
	#endif
	
	if (rdp_img_cache_nxcompressed)
	{
	    
	    tdata = data;
	    if (compressed)
	    {
		image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_COMPRESSED,
					    g_depth, ZPixmap, data, size,
					    width, height, bitmap_pad, 0);
					    
		NXPutPackedImage(g_display, 0, bitmap, g_gc, image, PACK_RDP_COMPRESSED,
				g_depth, 0, 0, 0, 0, width, height);
	    }
	    else
	    {
		image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_PLAIN,
					g_depth, ZPixmap, data, size,
					width, height, bitmap_pad, 0);

		NXPutPackedImage(g_display, 0, bitmap, g_gc, image, PACK_RDP_PLAIN,
				g_depth, 0, 0, 0, 0, width, height);
	    }
	}
	else
	{
	    
	    tdata = (g_owncolmap ? data : translate_image(width, height, data));
	    image = XCreateImage(g_display, g_visual, g_depth, ZPixmap, 0,
				(char *) tdata, width, height, bitmap_pad, 0);
	
	    XPutImage(g_display, bitmap, g_gc, image, 0, 0, 0, 0, width, height); 
	    XFree(image);
	    if (tdata != data)
		xfree(tdata);
	}
	#else
	tdata = (g_owncolmap ? data : translate_image(width, height, data));
	image = XCreateImage(g_display, g_visual, g_depth, ZPixmap, 0,
			    (char *) tdata, width, height, bitmap_pad, 0);
	
	XPutImage(g_display, bitmap, g_gc, image, 0, 0, 0, 0, width, height); 
	#endif
	
	#ifndef NXDESKTOP_IMGCACHE_USES_COMPRESSED_IMAGES
	XFree(image);
	if (tdata != data)
		xfree(tdata);
	#endif
	return (HBITMAP) bitmap;
}

void
ui_paint_bitmap(int x, int y, int cx, int cy, int width, int height, uint8 * data)
{
	XImage *image;
	uint8 *tdata;
	int bitmap_pad;

	if (g_server_bpp == 8)
	{
		bitmap_pad = 8;
	}
	else
	{
		bitmap_pad = g_bpp;

		if (g_bpp == 24)
			bitmap_pad = 32;
	}
	
	x = x-g_x_offset;
	y = y-g_y_offset;
	
	#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES
	if (nxdesktopUseNXRdpImages)
	{
		int data_length;
		tdata = data;
		data_length = width * height;

		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_paint_bitmap","NXCreatePackedImage with g_owncolmap %d and g_depth %d.\n",
				g_owncolmap, g_depth);
		#endif

		image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_COMPRESSED,
					    g_depth, ZPixmap, tdata, data_length,
					    width, height, BitmapPad(g_display), 0);
					    
		if (g_ownbackstore)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_paint_bitmap","NXPutPackedImage on backingstore pixmap %d,%d,%d,%d (%d,%d).\n",
					x, y, cx, cy, width, height);
			#endif

			NXPutPackedImage(g_display, 0, g_backstore, g_gc, image, PACK_RDP_COMPRESSED,
					 g_depth, 0, 0, x, y, cx, cy);

			XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);

			#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
			XFlush(g_display);
			#endif
		}
		else
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_paint_bitmap","NXPutPackedImage on window %d,%d,%d,%d (%d,%d).\n",
					x, y, cx, cy, width, height);
			#endif

			NXPutPackedImage(g_display, 0, g_wnd, g_gc, image, PACK_RDP_COMPRESSED,
					 g_depth, 0, 0, x, y, cx, cy);
		}

		/*
		 * NXDestroyPackedImage(), like XDestroyImage()
		 * would deallocate both image and data, that's
		 * not what we want.
		 */

		XFree(image);

	}
	else
	{

		tdata = (g_owncolmap ? data : translate_image(width, height, data));

		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_paint_bitmap","XCreateImage with owncolmap %d and depth %d.\n",
				g_owncolmap, g_depth);
		#endif

		image = XCreateImage(g_display, g_visual, g_depth, ZPixmap,
					0, tdata, width, height, 8, 0);

		if (g_ownbackstore)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_paint_bitmap","XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
					x, y, cx, cy);
			#endif
			
			#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
			paint_bitmap_backstore_times++;
			paint_bitmap_backstore_total+=image->height*image->bytes_per_line;
			#endif
			
			XPutImage(g_display, g_backstore, g_gc, image, 0, 0, x, y, cx, cy);

			XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
			
		}
		else
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_paint_bitmap","XPutImage on window %d,%d,%d,%d.\n",
					x, y, cx, cy);
			#endif
			
			#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
			paint_bitmap_times++;
			paint_bitmap_total+=image->height*image->bytes_per_line;
			#endif
			
			XPutImage(g_display, g_wnd, g_gc, image, 0, 0, x, y, cx, cy);
			
		}

		XFree(image);

		if (!g_owncolmap)
			xfree(tdata);
		
	}

	#else /* NXDESKTOP_XWIN_USES_PACKED_IMAGES */
	
	tdata = (g_owncolmap ? data : translate_image(width, height, data));
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_paint_bitmap","XCreateImage with owncolmap %d and depth %d.\n",
			g_owncolmap, g_depth);
	#endif
	
	image = XCreateImage(g_display, g_visual, g_depth, ZPixmap, 0,
			     (char *) tdata, width, height, bitmap_pad, 0);

	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_paint_bitmap","XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		
		#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
		paint_bitmap_backstore_nonx_times++;
		paint_bitmap_backstore_nonx_total+=image->height*image->bytes_per_line;
		#endif
		
		XPutImage(g_display, g_backstore, g_gc, image, 0, 0, x, y, cx, cy);
		
		XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
		
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_paint_bitmap","XPutImage on window %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		
		#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
		paint_bitmap_nonx_times++;
		paint_bitmap_nonx_total+=image->height*image->bytes_per_line;
		#endif
		
		XPutImage(g_display, g_wnd, g_gc, image, 0, 0, x, y, cx, cy);
	}
	
	XFree(image);
	if (tdata != data)
		xfree(tdata);
	
	#endif
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("ui_paintbitmap ","paint bitmap\n");
	    #endif
	    nxdesktopShape(g_backstore,x,y,x,y,cx,cy,True);
	}
	#endif
	
}
#ifdef NXDESKTOP_XWIN_USES_COMPRESSED_PACKED_IMAGES

void
ui_paint_compressed_bitmap(int x, int y, int cx, int cy, int width, int height,
				uint8 *compressed_data, int compressed_size)
{
	XImage *image;


	x = x-g_x_offset;
	y = y-g_y_offset;
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_paint_compressed_bitmap","NXCreatePackedImage with depth %d and size %d.\n",
			g_depth, compressed_size);
	#endif
	
	image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_COMPRESSED,
				    g_depth, ZPixmap, compressed_data, compressed_size,
				    width, height, BitmapPad(g_display), 0);
	
	if (image == NULL)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_paint_compressed_bitmap","NXCreatePackedImage failure.\n");
		#endif

		return;
	}


	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_paint_compressed_bitmap","NXPutPackedImage on backingstore pixmap %d,%d,%d,%d (%d,%d).\n",
				x, y, cx, cy, width, height);
		#endif

		NXPutPackedImage(g_display, 0, g_backstore, g_gc, image, PACK_RDP_COMPRESSED,
				 g_depth, 0, 0, x, y, cx, cy);

		XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);

		#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
		XFlush(g_display);
		#endif
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_paint_compressed_bitmap","NXPutPackedImage on window %d,%d,%d,%d (%d,%d).\n",
				x, y, cx, cy, width, height);
		#endif

		NXPutPackedImage(g_display, 0, g_wnd, g_gc, image, PACK_RDP_COMPRESSED,
				 g_depth, 0, 0, x, y, cx, cy);
	}

	/*
	 * NXDestroyPackedImage(), like XDestroyImage()
	 * would deallocate both image and data, that's
	 * not what we want.
	 */

	XFree(image);
}

#endif

/* NX */
void
ui_destroy_pixmap(Pixmap bmp)
{
	XFreePixmap(g_display, bmp);
}
/* NX */

void
ui_destroy_bitmap(HBITMAP bmp)
{
	XFreePixmap(g_display, (Pixmap) bmp);
}

HGLYPH
ui_create_glyph(int width, int height, uint8 * data)
{
	XImage *image;
	Pixmap bitmap;
	int scanline;

	scanline = (width + 7) / 8;

	bitmap = XCreatePixmap(g_display, g_wnd, width, height, 1);
	if (g_create_glyph_gc == 0)
		g_create_glyph_gc = XCreateGC(g_display, bitmap, 0, NULL);

	image = XCreateImage(g_display, g_visual, 1, ZPixmap, 0, (char *) data,
			     width, height, 8, scanline);
	image->byte_order = MSBFirst;
	image->bitmap_bit_order = MSBFirst;
	XInitImage(image);
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_create_glyph","XPutImage on pixmap %d,%d,%d,%d.\n",
			0, 0, width, height);
	#endif
	
	#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
	create_glyph_times++;
	create_glyph_total+=image->height*image->bytes_per_line;
	#endif
	
	XPutImage(g_display, bitmap, g_create_glyph_gc, image, 0, 0, 0, 0, width, height);

	XFree(image);
	return (HGLYPH) bitmap;
}

void
ui_destroy_glyph(HGLYPH glyph)
{
	XFreePixmap(g_display, (Pixmap) glyph);
}

HCURSOR
ui_create_cursor(unsigned int x, unsigned int y, int width, int height,
		 uint8 * andmask, uint8 * xormask)
{
	HGLYPH maskglyph, cursorglyph;
	XColor bg, fg;
	Cursor xcursor;
	uint8 *cursor, *pcursor;
	uint8 *mask, *pmask;
	uint8 nextbit;
	int scanline, offset;
	int i, j;

	scanline = (width + 7) / 8;
	offset = scanline * height;

	cursor = (uint8 *) xmalloc(offset);
	memset(cursor, 0, offset);

	mask = (uint8 *) xmalloc(offset);
	memset(mask, 0, offset);

	/* approximate AND and XOR masks with a monochrome X pointer */
	for (i = 0; i < height; i++)
	{
		offset -= scanline;
		pcursor = &cursor[offset];
		pmask = &mask[offset];

		for (j = 0; j < scanline; j++)
		{
			for (nextbit = 0x80; nextbit != 0; nextbit >>= 1)
			{
				if (xormask[0] || xormask[1] || xormask[2])
				{
					*pcursor |= (~(*andmask) & nextbit);
					*pmask |= nextbit;
				}
				else
				{
					*pcursor |= ((*andmask) & nextbit);
					*pmask |= (~(*andmask) & nextbit);
				}

				xormask += 3;
			}

			andmask++;
			pcursor++;
			pmask++;
		}
	}

	fg.red = fg.blue = fg.green = 0xffff;
	bg.red = bg.blue = bg.green = 0x0000;
	fg.flags = bg.flags = DoRed | DoBlue | DoGreen;

	cursorglyph = ui_create_glyph(width, height, cursor);
	maskglyph = ui_create_glyph(width, height, mask);

	xcursor =
		XCreatePixmapCursor(g_display, (Pixmap) cursorglyph,
				    (Pixmap) maskglyph, &fg, &bg, x, y);

	ui_destroy_glyph(maskglyph);
	ui_destroy_glyph(cursorglyph);
	xfree(mask);
	xfree(cursor);
	return (HCURSOR) xcursor;
}

void
ui_set_cursor(HCURSOR cursor)
{
	g_current_cursor = (Cursor) cursor;
	XDefineCursor(g_display, g_wnd, g_current_cursor);
}

void
ui_destroy_cursor(HCURSOR cursor)
{
	XFreeCursor(g_display, (Cursor) cursor);
}

void
ui_set_null_cursor(void)
{
	ui_set_cursor(g_null_cursor);
}

#define MAKE_XCOLOR(xc,c) \
		(xc)->red   = ((c)->red   << 8) | (c)->red; \
		(xc)->green = ((c)->green << 8) | (c)->green; \
		(xc)->blue  = ((c)->blue  << 8) | (c)->blue; \
		(xc)->flags = DoRed | DoGreen | DoBlue;

/* #define NXDESKTOP_XWIN_DEBUG */

HCOLOURMAP
ui_create_colourmap(COLOURMAP * colours)
{
	COLOURENTRY *entry;
	int i, ncolours = colours->ncolours;
	
	#ifdef NKDESKTOP_ONSTART
        if (showNXlogo)
        {
           /*setOwnerNX_WM(g_wnd);
           XSync(g_display, True);*/
        }
	#endif
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_create_colourmap","Creating new colormap.\n");
	#endif
	
	if (!g_owncolmap)
	{
		/*
		 * This used to generate >768 X_AllocColor requests and
		 * replies. By its own it caused desktop to take ~60 to
		 * start, and other ~60 second at each change of palette.
		 * Fixed by adding a new request/reply: NXAllocColors.
		 * ---
		 * This way all the collors are allocated at once but 
		 * there is a problem with the new color depths support:
		 * before, with 256 colors, if the XAllocColor failed, the
		 * respective color was chosen as white and life went on.
		 * Now, a closest match is found and them used.
		 * 
		 */
		
		uint32 *map = (uint32 *) xmalloc(sizeof(*g_colmap) * ncolours);
		XColor xentry[ncolours];
		Bool alloc_done[ncolours]; 
		XColor xc_cache[256];
		uint32 colour;
		int colLookup = 256;
		
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_create_colourmap","g_owncolmap used for %d numcolors.\n",ncolours);
		#endif
		for (i = 0; i < ncolours; i++)
		{
			entry = &colours->colours[i];
			MAKE_XCOLOR(&xentry[i], entry);
		}
		if (NXAllocColors(g_display, g_xcolmap, ncolours, xentry, alloc_done) == 0)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_create_colourmap: ERROR! Failed to allocate all requested colors.\n");
			#endif
			/* Allocation failed, find closest match. */
			
			for (i = 0; i < ncolours; i++)
			{
				if (!alloc_done[i])
				{
					int j = 256;
					int nMinDist = 3 * 256 * 256;
					long nDist = nMinDist;
					/* only get the colors once */
					while (colLookup--)
					{
						xc_cache[colLookup].pixel = colLookup;
						xc_cache[colLookup].red = xc_cache[colLookup].green =
						xc_cache[colLookup].blue = 0;
						xc_cache[colLookup].flags = 0;
						XQueryColor(g_display,
					    		DefaultColormap(g_display,
					    		DefaultScreen(g_display)),
					    		&xc_cache[colLookup]);
					}
					colLookup = 0;

					/* approximate the pixel */
					while (j--)
					{
						if (xc_cache[j].flags)
						{
							nDist = ((long) (xc_cache[j].red >> 8) -
						 	 	(long) (xentry[i].red >> 8)) *
								((long) (xc_cache[j].red >> 8) -
						 		(long) (xentry[i].red >> 8)) +
								((long) (xc_cache[j].green >> 8) -
						 	 	(long) (xentry[i].green >> 8)) *
								((long) (xc_cache[j].green >> 8) -
						 	 	(long) (xentry[i].green >> 8)) +
								((long) (xc_cache[j].blue >> 8) -
						 		(long) (xentry[i].blue >> 8)) *
								((long) (xc_cache[j].blue >> 8) -
						 		(long) (xentry[i].blue >> 8));
						}
						if (nDist < nMinDist)
						{
							nMinDist = nDist;
							xentry[i].pixel = j;
						}
					}
				}
			}
		}
		for (i = 0; i < ncolours; i++)
		{
			colour = xentry[i].pixel;
			/* Part of the matching routine above */
			/* update our cache */
			if (xentry[i].pixel < 256)
			{
				xc_cache[xentry[i].pixel].red = xentry[i].red;
				xc_cache[xentry[i].pixel].green = xentry[i].green;
				xc_cache[xentry[i].pixel].blue = xentry[i].blue;

			}
			map[i] = colour;
		}
		
		#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES
		last_colormap_entries = ncolours;
		last_colormap = map;
		#endif
		
		return map;
	}
	else
	{
		XColor *xcolours, *xentry;
		Colormap map;

		xcolours = (XColor *) xmalloc(sizeof(XColor) * ncolours);
		for (i = 0; i < ncolours; i++)
		{
			entry = &colours->colours[i];
			xentry = &xcolours[i];
			xentry->pixel = i;
			MAKE_XCOLOR(xentry, entry);
		}

		map = XCreateColormap(g_display, g_wnd, g_visual, AllocAll);
		XStoreColors(g_display, map, xcolours, ncolours);

		xfree(xcolours);
		
		#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES
		last_colormap_entries = 0;
		last_colormap = NULL;
		#endif
		
		return (HCOLOURMAP) map;
	}
}

void
ui_destroy_colourmap(HCOLOURMAP map)
{
	if (!g_owncolmap)
		xfree(map);
	else
		XFreeColormap(g_display, (Colormap) map);
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_destroy_colourmap","Destroyed colormap at %p.\n",
			map);
	#endif
}

void
ui_set_colourmap(HCOLOURMAP map)
{	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_set_colourmap","Setting new colormap at address %p.\n",
			map);
	#endif

	if (!g_owncolmap)
	{
		if (g_colmap)
			xfree(g_colmap);

		g_colmap = (uint32 *) map;
	}
	else
		XSetWindowColormap(g_display, g_wnd, (Colormap) map);
		
	#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES
	
	/* NX */
	if (nxdesktopUseNXTrans)
	{
	    if (g_owncolmap == 0 && last_colormap_entries != 0 && last_colormap != NULL)
	    {
		int i;

        	if (g_host_be != g_xserver_be && (nx_depth == 16 || nx_depth == 15 || nx_depth == 24 || nx_depth == 32))
		{
		    unsigned int *swap_colormap = xmalloc(sizeof(unsigned int) * last_colormap_entries);

		    for (i = 0; i < last_colormap_entries; i++)
		    {
            		swap_colormap[i] = last_colormap[i];
			BSWAP32(swap_colormap[i]);
            	    }
		    NXSetUnpackColormap(g_display, 0, last_colormap_entries, swap_colormap);
            	    xfree(swap_colormap);
        	}
    		else
        	{
		    NXSetUnpackColormap(g_display, 0, last_colormap_entries, last_colormap);
        	}
		#ifdef NXDESKTOP_XWIN_DUMP
		nxdesktopDebug("ui_set_colourmap","Dumping colormap entries:\n");
		for (i = 0; i < last_colormap_entries; i++)
		{
		    nxdesktopDebug("ui_set_colourmap"," [%d] [%p].\n",i, (void *) last_colormap[i]);
		}
		#endif
	    }
	}
	#endif
}

void
ui_set_clip(int x, int y, int cx, int cy)
{
	XRectangle rect;

	rect.x = x;
	rect.y = y;
	rect.width = cx;
	rect.height = cy;
	XSetClipRectangles(g_display, g_gc, 0, 0, &rect, 1, YXBanded);
}

void
ui_reset_clip(void)
{
	XRectangle rect;

	rect.x = 0;
	rect.y = 0;
	rect.width = g_width;
	rect.height = g_height;
	XSetClipRectangles(g_display, g_gc, 0, 0, &rect, 1, YXBanded);
}

void
ui_bell(void)
{
	XBell(g_display, 0);
}

void
ui_destblt(uint8 opcode,
	   /* dest */ int x, int y, int cx, int cy)
{
	SET_FUNCTION(opcode);
	FILL_RECTANGLE(x, y, cx, cy);
	RESET_FUNCTION(opcode);
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("ui_destblt","shape check coords x=%d y=%d cx=%d cy=%d\n",x, y, cx, cy);
    	    #endif
	    make_mask_hole(x, y, cx, cy, -1, True);
	}
	#endif
}

static uint8 hatch_patterns[] = {
	0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00, 0x00,	/* 0 - bsHorizontal */
	0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,	/* 1 - bsVertical */
	0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01,	/* 2 - bsFDiagonal */
	0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,	/* 3 - bsBDiagonal */
	0x08, 0x08, 0x08, 0xff, 0x08, 0x08, 0x08, 0x08,	/* 4 - bsCross */
	0x81, 0x42, 0x24, 0x18, 0x18, 0x24, 0x42, 0x81	/* 5 - bsDiagCross */
};

void
ui_patblt(uint8 opcode,
	  /* dest */ int x, int y, int cx, int cy,
	  /* brush */ BRUSH * brush, int bgcolour, int fgcolour)
{
	Pixmap fill;
	uint8 i, ipattern[8];

	SET_FUNCTION(opcode);

	switch (brush->style)
	{
		case 0:	/* Solid */
			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_patblt","brush-style 0 - Solid.\n");
			#endif
			SET_FOREGROUND(fgcolour);
			FILL_RECTANGLE_BACKSTORE(x, y, cx, cy);
			
			#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
			if (float_window_mode)
			    make_mask_hole(x, y, cx, cy, fgcolour, True);
			break;
			#endif

		case 2:	/* Hatch */
			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_patblt","brush-style 2 - Hatch.\n");
			#endif
			fill = (Pixmap) ui_create_glyph(8, 8,
							hatch_patterns + brush->pattern[0] * 8);
			SET_FOREGROUND(fgcolour);
			SET_BACKGROUND(bgcolour);
			XSetFillStyle(g_display, g_gc, FillOpaqueStippled);
			XSetStipple(g_display, g_gc, fill);
			XSetTSOrigin(g_display, g_gc, brush->xorigin, brush->yorigin);
			FILL_RECTANGLE_BACKSTORE(x, y, cx, cy);
			XSetFillStyle(g_display, g_gc, FillSolid);
			XSetTSOrigin(g_display, g_gc, 0, 0);
			ui_destroy_glyph((HGLYPH) fill);
			
			#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
			if (float_window_mode)
			    make_mask_hole(x, y, cx, cy, fgcolour, True);
			#endif
			    
			break;

		case 3:	/* Pattern */
			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_patblt","brush-style 3 - Pattern.\n");
			#endif
			for (i = 0; i != 8; i++)
				ipattern[7 - i] = brush->pattern[i];
			fill = (Pixmap) ui_create_glyph(8, 8, ipattern);
			SET_FOREGROUND(bgcolour);
			SET_BACKGROUND(fgcolour);
			XSetFillStyle(g_display, g_gc, FillOpaqueStippled);
			XSetStipple(g_display, g_gc, fill);
			XSetTSOrigin(g_display, g_gc, brush->xorigin, brush->yorigin);
			FILL_RECTANGLE_BACKSTORE(x, y, cx, cy);
			XSetFillStyle(g_display, g_gc, FillSolid);
			XSetTSOrigin(g_display, g_gc, 0, 0);
			ui_destroy_glyph((HGLYPH) fill);
			
			#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
			if (float_window_mode)
			    make_mask_hole(x, y, cx, cy, fgcolour, True);
			#endif
			
			break;

		default:
			unimpl("ui_patblt","brush %d\n", brush->style);
	}

	RESET_FUNCTION(opcode);

	if (g_ownbackstore)
		XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
}

void
ui_screenblt(uint8 opcode,
	     /* dest */ int x, int y, int cx, int cy,
	     /* src */ int srcx, int srcy)
{
    #ifdef NXDESKTOP_XWIN_DEBUG
    nxdesktopDebug("ui_screenblt","Opcode=%d, Backstore=%d, x=%d, y=%d, cx=%d, cy=%d, srcx=%d, scry=%d\n",opcode, g_ownbackstore, x, y, cx, cy, srcx, srcy);
    #endif
	SET_FUNCTION(opcode);
	if (g_ownbackstore)
	{
		if (g_Unobscured)
		{
			XCopyArea(g_display, g_wnd, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
			XCopyArea(g_display, g_backstore, g_backstore, g_gc, srcx, srcy, cx, cy, x, y);
			
			#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
			if (float_window_mode)
			{
			    #ifdef NXDESKTOP_FWINDOW_DEBUG
			    nxdesktopDebug("ui_screenblt","screenblt unobsc\n");
    			    #endif
			    make_mask_hole(srcx,srcy,cx,cy,-1,True);
			    nxdesktopShape(g_backstore,srcx,srcy,x,y,cx,cy,False);
			}
			#endif
		}
		else
		{
			XCopyArea(g_display, g_backstore, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
			XCopyArea(g_display, g_backstore, g_backstore, g_gc, srcx, srcy, cx, cy, x, y);
			
			#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
			if (float_window_mode)
			{
			    #ifdef NXDESKTOP_FWINDOW_DEBUG
			    nxdesktopDebug("ui_screenblt","screenblt\n");
    			    #endif
			    make_mask_hole(srcx,srcy,cx,cy,-1,True);
			    nxdesktopShape(g_backstore,srcx,srcy,x,y,cx,cy,False);
			    
			}
			#endif
		}
	}
	else
	{
		XCopyArea(g_display, g_wnd, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
		
		#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("ui_screenblt","This shouldn't be happening!\n");
    		#endif
		if (float_window_mode)
		    nxdesktopShape(g_backstore,srcx,srcy,x,y,cx,cy,False);
		#endif

	}
	RESET_FUNCTION(opcode);
}

void
ui_memblt(uint8 opcode,
	  /* dest */ int x, int y, int cx, int cy,
	  /* src */ HBITMAP src, int srcx, int srcy)
{
    #ifdef NXDESKTOP_XWIN_DEBUG
    nxdesktopDebug("ui_memblt","Opcode=%d, Backstore=%d, x=%d, y=%d, cx=%d, cy=%d, srcx=%d, scry=%d\n",opcode, g_ownbackstore, x, y, cx, cy, srcx, srcy);
    #endif
    
    SET_FUNCTION(opcode);
    XCopyArea(g_display, (Pixmap) src, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
    if (g_ownbackstore)
	XCopyArea(g_display, (Pixmap) src, g_backstore, g_gc, srcx, srcy, cx, cy, x, y);
    
    #ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
    if (float_window_mode)
    {
	#ifdef NXDESKTOP_FWINDOW_DEBUG
	nxdesktopDebug("ui_memblt","memblt\n");
    	#endif
	nxdesktopShape((Pixmap)src,srcx,srcy,x,y,cx,cy,True);	    
    }
    #endif
    
    RESET_FUNCTION(opcode);
}

void
ui_triblt(uint8 opcode,
	  /* dest */ int x, int y, int cx, int cy,
	  /* src */ HBITMAP src, int srcx, int srcy,
	  /* brush */ BRUSH * brush, int bgcolour, int fgcolour)
{
	/* This is potentially difficult to do in general. Until someone
	   comes up with a more efficient way of doing it I am using cases. */
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_triblt","Opcode = %d.\n",opcode);
	#endif
	switch (opcode)
	{
		case 0x69:	/* PDSxxn */
			
			ui_memblt(ROP2_XOR, x, y, cx, cy, src, srcx, srcy);
			ui_patblt(ROP2_NXOR, x, y, cx, cy, brush, bgcolour, fgcolour);
			break;

		case 0xb8:	/* PSDPxax */
			ui_patblt(ROP2_XOR, x, y, cx, cy, brush, bgcolour, fgcolour);
			ui_memblt(ROP2_AND, x, y, cx, cy, src, srcx, srcy);
			ui_patblt(ROP2_XOR, x, y, cx, cy, brush, bgcolour, fgcolour);
			break;

		case 0xc0:	/* PSa */
			ui_memblt(ROP2_COPY, x, y, cx, cy, src, srcx, srcy);
			ui_patblt(ROP2_AND, x, y, cx, cy, brush, bgcolour, fgcolour);
			break;

		default:
			unimpl("triblt", "0x%x\n", opcode);
			ui_memblt(ROP2_COPY, x, y, cx, cy, src, srcx, srcy);
	}
}

void
ui_line(uint8 opcode,
	/* dest */ int startx, int starty, int endx, int endy,
	/* pen */ PEN * pen)
{
    #ifdef NXDESKTOP_XWIN_DEBUG
    nxdesktopDebug("ui_line","Opcode=%d, startx=%d, starty=%d, endx=%d, endy=%d.\n",opcode,startx,starty,endx,endy);
    #endif
	SET_FUNCTION(opcode);
	SET_FOREGROUND(pen->colour);
	XDrawLine(g_display, g_wnd, g_gc, startx, starty, endx, endy);
	if (g_ownbackstore)
		XDrawLine(g_display, g_backstore, g_gc, startx, starty, endx, endy);
	RESET_FUNCTION(opcode);
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("ui_line","shape_check coords startx=%d starty=%d endx=%d endy=%d\n",startx, starty, endx, endy);
    	    #endif
	    make_mask_hole(startx, starty, endx, endy, pen->colour, False);
	}
	#endif
}

/* NX */
void
ui_poly_line(uint8 opcode, short *points, int count,
	/* pen */ PEN *pen)
{
    #ifdef NXDESKTOP_XWIN_DEBUG
    nxdesktopDebug("ui_poly_line","Opcode=%d, count=%d.\n",opcode,count);
    #endif
    SET_FUNCTION(opcode);
    SET_FOREGROUND(pen->colour);

    XDrawLines(g_display, g_wnd, g_gc, (XPoint*)points, count, CoordModeOrigin);
    if (g_ownbackstore)
	XDrawLines(g_display, g_backstore, g_gc, (XPoint*)points, count, CoordModeOrigin);
    RESET_FUNCTION(opcode);
}

#ifdef NXDESKTOP_USES_RECT_BUF
/* Fills a whole bunch of solid rectangles at once */
void buf_rect(int x, int y, int cx, int cy)
{
    if (num_buf_rects < REC_BUF_SIZE)
    {
	#ifdef NXDESTOP_RECT_DEBUG
	nxdesktopDebug("buf_rect","Buffered %d %d %d %d num_buf_rects = %d\n",x,y,cx,cy,num_buf_rects);
	#endif
	buf_rects[num_buf_rects].x = x;
	buf_rects[num_buf_rects].y = y;
	buf_rects[num_buf_rects].width = cx;
	buf_rects[num_buf_rects].height = cy;
	num_buf_rects++;
    }
    else
    {
	#ifdef NXDESTOP_RECT_DEBUG
	nxdesktopDebug("buf_rect","Flushing %d rects.\n",num_buf_rects);
	#endif
	flush_rects();
    }
}

void flush_rects()
{
    if (num_buf_rects > 0)
    {
	#ifdef NXDESTOP_RECT_DEBUG
	nxdesktopDebug("flush_rects","num_buf_rects=%d\n",num_buf_rects);
	#endif
	SET_FOREGROUND(last_color);
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{    
	    if (TRANSLATE(colour) == BACKGROUND)
		make_mask_hole(x, y, cx, cy, -1, True);
	    else make_mask_hole(x, y, cx, cy, -1, False);
	}
	#endif
	
	XFillRectangles(g_display, g_wnd, g_gc, (XRectangle *)buf_rects, num_buf_rects);
	if (g_ownbackstore)
	    XFillRectangles(g_display, g_backstore, g_gc, (XRectangle *)buf_rects, num_buf_rects);
	num_buf_rects = 0;
    }
}
#endif

/* NX */

void
ui_rect(int x, int y, int cx, int cy, int colour)
{
    /* NX */
    /* Makes sure that the first one is in sync with the cache */
    #ifdef NXDESKTOP_USES_RECT_BUF
    if (num_buf_rects == -1)
    {
	num_buf_rects = 0;
	#ifdef NXDESKTOP_RECT_DEBUG
	nxdesktopDebug("ui_rect","First rect arrived\n");
	#endif
	last_color = colour;
	buf_rect(x,y,cx,cy);
	flush_rects();
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("ui_rect (cache on)","shape_check coords x=%d y=%d cx=%d cy=%d\n",x, y, cx, cy);
    	    #endif
	}
	#endif
    }
    
    /* If it's a rect but the color changes, force flush */
    if (colour != last_color)
    {
	#ifdef NXDESTOP_RECT_DEBUG
	nxdesktopDebug("ui_rect","Color change from %d to %d\n",last_color, colour);
	#endif
	flush_rects();
	last_color = colour;
    } 
    
    /* Put the rect into the buffer */
    buf_rect(x,y,cx,cy);
    #else
    
    SET_FOREGROUND(colour);
    
    #ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
    if (float_window_mode)
    {
	#ifdef NXDESKTOP_FWINDOW_DEBUG
	nxdesktopDebug("ui_rect (cache off)","shape_check coords x=%d y=%d cx=%d cy=%d\n",x, y, cx, cy);
    	#endif
	
	make_mask_hole(x, y, cx, cy, colour, True);
    }
    FILL_RECTANGLE(x, y, cx, cy);
    #endif
    
    #endif
}

void
ui_polygon(uint8 opcode,
		/* mode */ uint8 fillmode,
		/* dest */ POINT * point, int npoints,
		/* brush */ BRUSH * brush, int bgcolour, int fgcolour)
{
	uint8 style, i, ipattern[8];
	Pixmap fill;

	SET_FUNCTION(opcode);

	switch (fillmode)
	{
		case ALTERNATE:
			XSetFillRule(g_display, g_gc, EvenOddRule);
			break;
		case WINDING:
			XSetFillRule(g_display, g_gc, WindingRule);
			break;
		default:
			unimpl("ui_polygon","fill mode %d\n", fillmode);
	}

	if (brush)
		style = brush->style;
	else
		style = 0;

	switch (style)
	{
		case 0:	/* Solid */
			SET_FOREGROUND(fgcolour);
			FILL_POLYGON((XPoint *) point, npoints);
			break;

		case 2:	/* Hatch */
			fill = (Pixmap) ui_create_glyph(8, 8,
						        hatch_patterns + brush->pattern[0] * 8);
			SET_FOREGROUND(fgcolour);
			SET_BACKGROUND(bgcolour);
			XSetFillStyle(g_display, g_gc, FillOpaqueStippled);
			XSetStipple(g_display, g_gc, fill);
			XSetTSOrigin(g_display, g_gc, brush->xorigin, brush->yorigin);
			FILL_POLYGON((XPoint *) point, npoints);
			XSetFillStyle(g_display, g_gc, FillSolid);
			XSetTSOrigin(g_display, g_gc, 0, 0);
			ui_destroy_glyph((HGLYPH) fill);
			break;

		case 3:	/* Pattern */
			for (i = 0; i != 8; i++)
				ipattern[7 - i] = brush->pattern[i];
			fill = (Pixmap) ui_create_glyph(8, 8, ipattern);
			SET_FOREGROUND(bgcolour);
			SET_BACKGROUND(fgcolour);
			XSetFillStyle(g_display, g_gc, FillOpaqueStippled);
			XSetStipple(g_display, g_gc, fill);
			XSetTSOrigin(g_display, g_gc, brush->xorigin, brush->yorigin);
			FILL_POLYGON((XPoint *) point, npoints);
			XSetFillStyle(g_display, g_gc, FillSolid);
			XSetTSOrigin(g_display, g_gc, 0, 0);
			ui_destroy_glyph((HGLYPH) fill);
			break;

		default:
			unimpl("ui_polygon","brush %d\n", brush->style);
	}

	RESET_FUNCTION(opcode);
}

void
ui_polyline(uint8 opcode,
	    /* dest */ POINT * points, int npoints,
	    /* pen */ PEN * pen)
{
	/* TODO: set join style */
	SET_FUNCTION(opcode);
	SET_FOREGROUND(pen->colour);
	XDrawLines(g_display, g_wnd, g_gc, (XPoint *) points, npoints, CoordModePrevious);
	if (g_ownbackstore)
		XDrawLines(g_display, g_backstore, g_gc, (XPoint *) points, npoints,
			   CoordModePrevious);
	RESET_FUNCTION(opcode);
}

void
ui_ellipse(uint8 opcode,
	   /* mode */ uint8 fillmode,
	   /* dest */ int x, int y, int cx, int cy,
	   /* brush */ BRUSH * brush, int bgcolour, int fgcolour)
{
	uint8 style, i, ipattern[8];
	Pixmap fill;

	SET_FUNCTION(opcode);

	if (brush)
		style = brush->style;
	else
		style = 0;

	switch (style)
	{
		case 0:	/* Solid */
			SET_FOREGROUND(fgcolour);
			DRAW_ELLIPSE(x, y, cx, cy, fillmode);
			break;

		case 2:	/* Hatch */
			fill = (Pixmap) ui_create_glyph(8, 8,
							hatch_patterns + brush->pattern[0] * 8);
			SET_FOREGROUND(fgcolour);
			SET_BACKGROUND(bgcolour);
			XSetFillStyle(g_display, g_gc, FillOpaqueStippled);
			XSetStipple(g_display, g_gc, fill);
			XSetTSOrigin(g_display, g_gc, brush->xorigin, brush->yorigin);
			DRAW_ELLIPSE(x, y, cx, cy, fillmode);
			XSetFillStyle(g_display, g_gc, FillSolid);
			XSetTSOrigin(g_display, g_gc, 0, 0);
			ui_destroy_glyph((HGLYPH) fill);
			break;

		case 3:	/* Pattern */
			for (i = 0; i != 8; i++)
				ipattern[7 - i] = brush->pattern[i];
			fill = (Pixmap) ui_create_glyph(8, 8, ipattern);
			SET_FOREGROUND(bgcolour);
			SET_BACKGROUND(fgcolour);
			XSetFillStyle(g_display, g_gc, FillOpaqueStippled);
			XSetStipple(g_display, g_gc, fill);
			XSetTSOrigin(g_display, g_gc, brush->xorigin, brush->yorigin);
			DRAW_ELLIPSE(x, y, cx, cy, fillmode);
			XSetFillStyle(g_display, g_gc, FillSolid);
			XSetTSOrigin(g_display, g_gc, 0, 0);
			ui_destroy_glyph((HGLYPH) fill);
			break;

		default:
			unimpl("ui_ellipse","brush %d\n", brush->style);
	}

	RESET_FUNCTION(opcode);
}

/* warning, this function only draws on wnd or backstore, not both */
void
ui_draw_glyph(int mixmode,
	      /* dest */ int x, int y, int cx, int cy,
	      /* src */ HGLYPH glyph, int srcx, int srcy,
	      int bgcolour, int fgcolour)
{
	SET_FOREGROUND(fgcolour);
	SET_BACKGROUND(bgcolour);

	XSetFillStyle(g_display, g_gc,
		      (mixmode == MIX_TRANSPARENT) ? FillStippled : FillOpaqueStippled);
	XSetStipple(g_display, g_gc, (Pixmap) glyph);
	XSetTSOrigin(g_display, g_gc, x, y);

	FILL_RECTANGLE_BACKSTORE(x, y, cx, cy);
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	    make_mask_hole(x, y, cx, cy, fgcolour, True);
	#endif
	
	XSetFillStyle(g_display, g_gc, FillSolid);
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("ui_draw_glyph","shape_check coords x=%d y=%d cx=%d cy=%d\n",x, y, cx, cy);
    	    #endif
	}
	#endif
}

#define DO_GLYPH(ttext,idx) \
{\
  glyph = cache_get_font (font, ttext[idx]);\
  if (!(flags & TEXT2_IMPLICIT_X))\
  {\
    xyoffset = ttext[++idx];\
    if ((xyoffset & 0x80))\
    {\
      if (flags & TEXT2_VERTICAL)\
        y += ttext[idx+1] | (ttext[idx+2] << 8);\
      else\
        x += ttext[idx+1] | (ttext[idx+2] << 8);\
      idx += 2;\
    }\
    else\
    {\
      if (flags & TEXT2_VERTICAL)\
        y += xyoffset;\
      else\
        x += xyoffset;\
    }\
  }\
  if (glyph != NULL)\
  {\
    x1 = x + glyph->offset;\
    y1 = y + glyph->baseline;\
    XSetStipple(g_display, g_gc, (Pixmap) glyph->pixmap);\
    XSetTSOrigin(g_display, g_gc, x1, y1);\
    FILL_RECTANGLE_BACKSTORE(x1, y1, glyph->width, glyph->height);\
    if (flags & TEXT2_IMPLICIT_X)\
      x += glyph->width;\
  }\
}

void
ui_draw_text(uint8 font, uint8 flags, int mixmode, int x, int y,
	     int clipx, int clipy, int clipcx, int clipcy,
	     int boxx, int boxy, int boxcx, int boxcy, int bgcolour,
	     int fgcolour, uint8 * text, uint8 length)
{
	FONTGLYPH *glyph;
	int i, j, xyoffset, x1, y1;
	DATABLOB *entry;
	
	/* NX */
	xNXRDPGlyph rdp_text[1024];
	NXPackedImage *image;
	int elements = 0;
	/* NX */
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	if (float_window_mode)
	{
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("ui_draw_text","shape_check coords x=%d y=%d\n",x, y);
    	    #endif
	}
	#endif
	
	SET_FOREGROUND(bgcolour);

	/* Sometimes, the boxcx value is something really large, like
	   32691. This makes XCopyArea fail with Xvnc. The code below
	   is a quick fix. */
	if (boxx + boxcx > g_width)
		boxcx = g_width - boxx;

	if (boxcx > 1)
	{
		FILL_RECTANGLE_BACKSTORE(boxx, boxy, boxcx, boxcy);
		
		#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
		if (float_window_mode)
		    make_mask_hole(boxx, boxy, boxcx, boxcy, bgcolour, False);
		#endif
		
	}
	else if (mixmode == MIX_OPAQUE)
	{
		FILL_RECTANGLE_BACKSTORE(clipx, clipy, clipcx, clipcy);
		
		#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
		if (float_window_mode)    
		    make_mask_hole(clipx, clipy, clipcx, clipcy, bgcolour, False);
		#endif
		
	}

	SET_FOREGROUND(fgcolour);
	SET_BACKGROUND(bgcolour);
	XSetFillStyle(g_display, g_gc, FillStippled);
	
	/* Paint text, character by character */
	for (i = 0; i < length;)
	{
		switch (text[i])
		{
			case 0xff:
				if (i + 2 < length)
					cache_put_text(text[i + 1], text, text[i + 2]);
				else
				{
				    error("this shouldn't be happening\n");
				    exit(1);
				}
				/* this will move pointer from start to first character after FF command */
				length -= i + 3;
				text = &(text[i + 3]);
				i = 0;
				break;

			case 0xfe:
				entry = cache_get_text(text[i + 1]);
				if (entry != NULL)
				{
				    if ((((uint8 *) (entry->data))[1] == 0) && (!(flags & TEXT2_IMPLICIT_X)))
				    {
					if (flags & TEXT2_VERTICAL)
					    y += text[i + 2];
					else
					    x += text[i + 2];
				    }
				    for (j = 0; j < entry->size; j++)
				    {
					if (nxdesktopUseNXTrans && nxdesktopCanPackRDPText)
					{
					    glyph = cache_get_font(font, ((uint8 *) (entry->data))[j]);
					    if (!(flags & TEXT2_IMPLICIT_X))
					    {
						xyoffset = ((uint8 *) (entry->data))[++j];
						if ((xyoffset & 0x80))
    						{
      							if (flags & TEXT2_VERTICAL)
        							y += ((uint8 *) (entry->data))[j+1] | (((uint8 *) (entry->data))[j+2] << 8);
      							else
        							x += ((uint8 *) (entry->data))[j+1] | (((uint8 *) (entry->data))[j+2] << 8);
      							j += 2;
    						}
						else
						{
						if (flags & TEXT2_VERTICAL)
							y += xyoffset;
						else
							x += xyoffset;
						}
					    }
					    if (glyph != NULL)
					    {
						#ifdef NXDESKTOP_XWIN_DEBUG
						fprintf(stderr, "ui_draw_text: Building element [%d] with pixmap [0x%lx].\n",j, (Pixmap)glyph->pixmap);
						#endif
    						rdp_text[elements].x      = x + (short)glyph->offset;
						rdp_text[elements].y      = y + (short)glyph->baseline;
						rdp_text[elements].width  = glyph->width;
						rdp_text[elements].height = glyph->height;
						rdp_text[elements].pixmap = (Pixmap)glyph->pixmap;

						elements++;

						if (flags & TEXT2_IMPLICIT_X)									
						    x += glyph->width;
					    }
					}
					else
					{
					    DO_GLYPH(((uint8 *) (entry->data)), j);
					}
				    }
				}
				if (i + 2 < length)
					i += 3;
				else
					i += 2;
				length -= i;
				/* this will move pointer from start to first character after FE command */
				text = &(text[i]);
				i = 0;
				break;

			default:
				if (nxdesktopUseNXTrans && nxdesktopCanPackRDPText)
				{
				    glyph = cache_get_font(font, text[i]);
				    if (!(flags & TEXT2_IMPLICIT_X))
				    {	
					xyoffset = text[++i];
					if ((xyoffset & 0x80))
    					{
      					    if (flags & TEXT2_VERTICAL)
        					y += text[i+1] | (text[i+2] << 8);
      					    else
        					x += text[i+1] | (text[i+2] << 8);
      					    i += 2;
    					}
					else
					{
					if (flags & TEXT2_VERTICAL)
						y += xyoffset;
					else
						x += xyoffset;
					}
				    }
				    if (glyph != NULL)
				    {
					#ifdef NXDESKTOP_XWIN_DEBUG
					fprintf(stderr, "ui_draw_text: Building element [%d] with pixmap [0x%lx].\n",i, (Pixmap)glyph->pixmap);
					#endif
					rdp_text[elements].x      = x + (short)glyph->offset;
					rdp_text[elements].y      = y + (short)glyph->baseline;
					rdp_text[elements].width  = glyph->width;
					rdp_text[elements].height = glyph->height;
					rdp_text[elements].pixmap = (Pixmap)glyph->pixmap;
    					elements++;
					if (flags & TEXT2_IMPLICIT_X)
					    x += glyph->width;
				    }
				}
				else
				{
				    DO_GLYPH(text, i);
				}
				i++;
				break;
		}
	}
	
	if (nxdesktopUseNXTrans && nxdesktopCanPackRDPText)
	{
		image = NXEncodeRDPText(g_display, TRANSLATE(bgcolour), TRANSLATE(fgcolour),
						(mixmode == MIX_TRANSPARENT ? FillStippled :
							FillOpaqueStippled), &rdp_text[0], elements);
		if (image)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_draw_text: Using packed image with drawable [0x%lx] and gc [0x%lx].\n",g_wnd, (long unsigned int)g_gc);
			#endif

			NXPutPackedImage(g_display, 0, (g_ownbackstore) ? g_backstore : g_wnd, g_gc, image, PACK_RDP_TEXT, 1, 0, 0, 0, 0, elements, 1);
			NXDestroyPackedImage(image);
		}	

		/*
		 * Synchronize GC and force dirty state.
		 */

		SET_BACKGROUND(fgcolour);
		SET_FOREGROUND(bgcolour);
		SET_FOREGROUND(fgcolour);
		SET_BACKGROUND(bgcolour);
		XSetFillStyle(g_display, g_gc, FillStippled);
		XSetFillStyle(g_display, g_gc, FillSolid);
		
	}
	
	XSetFillStyle(g_display, g_gc, FillSolid);

	if (g_ownbackstore)
	{
		if (boxcx > 1)
			XCopyArea(g_display, g_backstore, g_wnd, g_gc, boxx,
				  boxy, boxcx, boxcy, boxx, boxy);
		else
			XCopyArea(g_display, g_backstore, g_wnd, g_gc, clipx,
				  clipy, clipcx, clipcy, clipx, clipy);
	}
}

void
ui_desktop_save(uint32 offset, int x, int y, int cx, int cy)
{
	
	/* NX */
	#ifdef NXDESKTOP_XWIN_USES_PIXMAP_CACHE

	int i;

	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_desktop_save","Called with offset [%d]. Using pixmap cache.\n",
			offset);
	#endif

	/*
	 * Force cleanup of outdated caches.
	 */

	if (offset == 0)
	{
		for (i = 0; i < PIXCACHE_ENTRIES; i++)
		{
			if (pixmap_cache[i].pixmap != (Pixmap) NULL)
			{
				XFreePixmap(g_display, pixmap_cache[i].pixmap);

				pixmap_cache[i].pixmap = (Pixmap) NULL;
				pixmap_cache[i].offset = 0;
			}
		}
	}

	for (i = 0; i < PIXCACHE_ENTRIES; i++)
	{
		if (pixmap_cache[i].pixmap == (Pixmap) NULL ||
			pixmap_cache[i].offset == offset)
		{
			Pixmap cache;

			if (pixmap_cache[i].pixmap != (Pixmap) NULL &&
				pixmap_cache[i].offset == offset)
			{
				XFreePixmap(g_display, pixmap_cache[i].pixmap);
			}

			cache = XCreatePixmap(g_display, g_wnd, g_width, g_height, g_depth);

			pixmap_cache[i].pixmap = cache;
			pixmap_cache[i].offset = offset;

			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_desktop_save","Saved area in pixmap cache [%lx] index [%d].\n",
					cache, i);
			#endif

			if (g_ownbackstore)
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				nxdesktopDebug("ui_desktop_save","XCopyArea from backingstore to pixmap cache %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, g_backstore, cache, g_gc, x, y, cx, cy, 0, 0);
			}
			else
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				nxdesktopDebug("ui_desktop_save","XCopyArea from window to pixmap cache %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, g_wnd, cache, g_gc, x, y, cx, cy, 0, 0);
			}

			break;
		}
	}

	if (i == PIXCACHE_ENTRIES)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_desktop_save","PANIC! Couldn't find any free pixmap cache slot.\n");
		#endif
	}

	#else /* NXDESKTOP_XWIN_USES_PIXMAP_CACHE */
	
	Pixmap pix;
	XImage *image;
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_desktop_save","Called with offset [%d]. Not using pixmap cache.\n",
			offset);
	#endif
	/* NX */

	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_desktop_save","XGetImage from backingstore pixmap %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		image = XGetImage(g_display, g_backstore, x, y, cx, cy, AllPlanes, ZPixmap);
	}
	else
	{
		pix = XCreatePixmap(g_display, g_wnd, cx, cy, g_depth);
		XCopyArea(g_display, g_wnd, pix, g_gc, x, y, cx, cy, 0, 0);
		image = XGetImage(g_display, pix, 0, 0, cx, cy, AllPlanes, ZPixmap);
		XFreePixmap(g_display, pix);
	}

	offset *= g_bpp / 8;
	cache_put_desktop(offset, cx, cy, image->bytes_per_line, g_bpp / 8, (uint8 *) image->data);

	XDestroyImage(image);
	
	#endif
}

void
ui_desktop_restore(uint32 offset, int x, int y, int cx, int cy)
{
	/* NX */
	#ifdef NXDESKTOP_XWIN_USES_PIXMAP_CACHE

	int i;

	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_desktop_restore","Called with offset [%d]. Using pixmap cache.\n",
			offset);
	#endif

	for (i = 0; i < PIXCACHE_ENTRIES; i++)
	{
		if (pixmap_cache[i].offset == offset &&
			pixmap_cache[i].pixmap != (Pixmap) NULL)
		{
			Pixmap cache = pixmap_cache[i].pixmap;

			if (g_ownbackstore)
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				nxdesktopDebug("ui_desktop_restore","XCopyArea from pixmap cache to backingstore %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, cache, g_backstore, g_gc, 0, 0, cx, cy, x, y);
				XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
			}
			else
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				nxdesktopDebug("ui_desktop_restore","XCopyArea from pixmap cache to window %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, cache, g_wnd, g_gc, 0, 0, cx, cy, x, y);
			}

			#ifdef NXDESKTOP_XWIN_DEBUG
			nxdesktopDebug("ui_desktop_restore","Restored area from pixmap cache [%lx] index [%d].\n",
					cache, i);
			#endif


			XFreePixmap(g_display, cache);

			pixmap_cache[i].pixmap = (Pixmap) NULL;
			pixmap_cache[i].offset = 0;

			break;
		}
	}

	if (i == PIXCACHE_ENTRIES)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_desktop_restore","PANIC! Couldn't find pixmap cache slot.\n");
		#endif
	}


	#else /* NXDESKTOP_XWIN_USES_PIXMAP_CACHE */
	/* NX */
	
	XImage *image;
	uint8 *data;
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	nxdesktopDebug("ui_desktop_restore","Called with offset [%d]. Not using pixmap cache.\n",
			offset);
	#endif

	offset *= g_bpp / 8;
	data = cache_get_desktop(offset, cx, cy, g_bpp / 8);
	if (data == NULL)
		return;

	image = XCreateImage(g_display, g_visual, g_depth, ZPixmap, 0,
			     (char *) data, cx, cy, BitmapPad(g_display), cx * g_bpp / 8);

	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_desktop_restore","XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		
		#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
		desktop_restore_backstore_nonx_times++;
		desktop_restore_backstore_nonx_total+=image->height*image->bytes_per_line;
		#endif
				
		XPutImage(g_display, g_backstore, g_gc, image, 0, 0, x, y, cx, cy);
		XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		nxdesktopDebug("ui_desktop_restore","XPutImage on window %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		
		#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
		desktop_restore_backstore_times++;
		desktop_restore_backstore_total+=image->height*image->bytes_per_line;
		#endif
		
		XPutImage(g_display, g_wnd, g_gc, image, 0, 0, x, y, cx, cy);
	}

	XFree(image);
	
	#endif
	
	#ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	/*if (float_window_mode)
	{   
	    fprintf(stderr,"desktop_restore\n"); 
	    nxdesktopShape(0, 0, x, y, cx, cy, False);
	}*/
	#endif
}

/* NX */

#ifdef NXDESKTOP_LOGO
void nomachineLogo(Window win, GC gc, int scale)
{
    XPoint    rect[4];
    XPoint    m[12];
    int w, h, c, w2, h2;

#ifdef NXDESKTOP_LOGO_DEBUG
    nxdesktopDebug("nomachineLogo","begin\n");
    nxdesktopDebug("nomachineLogo","gen params are: %d %x %x %x\n",
            nx_depth, nx_red,
            nx_white, nx_black);
#endif

    w = g_width/scale;
    h = g_height/scale;
    w2 = w/2;
    h2 = h/2;
    if (g_height > g_width)
    {	    
      c = w/30;
    }
    else
    {
      c = w/48;
    }
    
    rect[0].x = 0;	       rect[0].y = 0;
    rect[1].x = 0;	       rect[1].y = h;
    rect[2].x = w;	       rect[2].y = h;
    rect[3].x = w;	       rect[3].y = 0;
    
    XSetFunction(g_display, gc, GXcopy);
    XSetFillStyle(g_display, gc, FillSolid);
    XSetForeground(g_display, gc, nx_black);
    XSetBackground(g_display, gc, nx_red);

    XFillPolygon(g_display, win, gc, rect, 4, Convex, CoordModeOrigin);

#ifdef NXDESKTOP_LOGO_DEBUG
    nxdesktopDebug("nomachineLogo","filled first poly\n");
#endif

    XSetForeground(g_display, gc, nx_red);
    XSetBackground(g_display, gc, nx_white);

    rect[0].x = w2-10*c;	       rect[0].y = h2-8*c;
    rect[1].x = w2-10*c;	       rect[1].y = h2+8*c;
    rect[2].x = w2+10*c;	       rect[2].y = h2+8*c;
    rect[3].x = w2+10*c;	       rect[3].y = h2-8*c;

    XFillPolygon(g_display, win, gc, rect, 4, Convex, CoordModeOrigin);

#ifdef NXDESKTOP_LOGO_DEBUG
    nxdesktopDebug("nomachineLogo","filled red rect\n");
#endif

    rect[0].x = w2-9*c;	       rect[0].y = h2-7*c;
    rect[1].x = w2-9*c;	       rect[1].y = h2+7*c;
    rect[2].x = w2+9*c;	       rect[2].y = h2+7*c;
    rect[3].x = w2+9*c;	       rect[3].y = h2-7*c;

    XSetForeground(g_display, gc, nx_white);
    XSetBackground(g_display, gc, nx_red);

    XFillPolygon(g_display, win, gc, rect, 4, Convex, CoordModeOrigin);

    /* begin M */
    m[0].x = w2-3*c;  m[0].y = h2-5*c;
    m[1].x = w2+7*c;  m[1].y = h2-5*c;
    m[2].x = w2+7*c;  m[2].y = h2+5*c;
    m[3].x = w2+5*c;  m[3].y = h2+5*c;
    m[4].x = w2+5*c;  m[4].y = h2-3*c;
    m[5].x = w2+3*c;  m[5].y = h2-3*c;
    m[6].x = w2+3*c;  m[6].y = h2+5*c;
    m[7].x = w2+1*c;  m[7].y = h2+5*c;
    m[8].x = w2+1*c;  m[8].y = h2-3*c;
    m[9].x = w2-1*c;  m[9].y = h2-3*c;
    m[10].x = w2-1*c; m[10].y = h2+5*c;
    m[11].x = w2-3*c; m[11].y = h2+5*c;

    XSetForeground(g_display, gc, nx_red);
    XSetBackground(g_display, gc, nx_white);

    XFillPolygon(g_display, win, gc, m, 12, Nonconvex, CoordModeOrigin);
    #ifdef NXDESKTOP_LOGO_DEBUG
    nxdesktopDebug("nomachineLogo","filled M\n");
    #endif
    /* end M */

    /* begin ! */
    rect[0].x = w2-7*c;	       rect[0].y = h2-5*c;
    rect[1].x = w2-5*c;	       rect[1].y = h2-5*c;
    rect[2].x = w2-5*c;	       rect[2].y = h2+2*c;
    rect[3].x = w2-7*c;	       rect[3].y = h2+2*c;

    XFillPolygon(g_display, win, gc, rect, 4, Convex, CoordModeOrigin);

    rect[0].x = w2-7*c;	       rect[0].y = h2+3*c;
    rect[1].x = w2-5*c;	       rect[1].y = h2+3*c;
    rect[2].x = w2-5*c;	       rect[2].y = h2+5*c;
    rect[3].x = w2-7*c;	       rect[3].y = h2+5*c;

    XFillPolygon(g_display, win, gc, rect, 4, Convex, CoordModeOrigin);
    #ifdef NXDESKTOP_LOGO_DEBUG
    nxdesktopDebug("nomachineLogo","filled !\n");
    #endif


/*    XFlush(g_display);*/
    XSync(g_display, True);

#ifdef NXDESKTOP_LOGO_DEBUG
    nxdesktopDebug("nomachineLogo","end\n");
#endif
}
#endif

#ifdef NXDESKTOP_ONSTART
void setOwnerNX_WM(Window win)
{
  XSetSelectionOwner(g_display, nxdesktopAtoms[3], win, CurrentTime);
  showNXlogo = False;
}
#endif

Bool getNXIcon(Display *g_display, Pixmap *nxIcon, Pixmap *nxMask)
{
  char *env_path = getenv("PATH");
  int lenght_env_path = 0;
  char icon_filename [256];
  char default_path [256];
  char *icon_path = malloc( strlen(env_path) + sizeof(icon_filename) );
  FILE *icon_fp;
  int status;
  Bool existXpmIcon = False;

  Pixmap IconPixmap;
  Pixmap IconShape;

  if (env_path == NULL)
    lenght_env_path = 0;
  else
    lenght_env_path = strlen(env_path) + 1;
  strncpy(icon_filename, "", 255);
  strncpy(default_path, "", 255);

  strcat(icon_filename, NX_DEFAULT_ICON);
  strcat(default_path,"/usr/NX/share/images/");
  strcat(default_path,icon_filename);

  if ((icon_fp = fopen(default_path, "r")) == NULL)
  {
    char *s;
    char *temp_path = malloc(lenght_env_path + strlen(icon_filename) );
    char *temp_path1 = malloc(lenght_env_path + strlen(icon_filename) );

    strncpy(temp_path, env_path, strlen(env_path));
    strncpy(temp_path1, "", lenght_env_path + strlen(icon_filename) );

    while ( strlen(temp_path) > 0)
    {
       s = strpbrk (temp_path, ":");
       if (s == NULL) break;

       strncpy ( temp_path1, temp_path , strlen(temp_path) - strlen(s) );
       strncat ( temp_path1, "/", 1);
       strncat ( temp_path1, icon_filename, strlen(icon_filename));
       if ((icon_fp = fopen(temp_path1, "r")) != NULL)
       {
          fclose (icon_fp);
          existXpmIcon = True;
          strcpy(icon_path,temp_path1);
          break;
       }
       strncpy(temp_path1, "", lenght_env_path + strlen(icon_filename) );
       strncpy(temp_path1, s + 1, strlen(s)-1);
       strncpy(temp_path, "", lenght_env_path + strlen(icon_filename) );
       strcpy(temp_path, temp_path1 );
       strncpy(temp_path1, "", lenght_env_path + strlen(icon_filename) );
     }
     free(temp_path);
     free(temp_path1);
  }
  else
  {
     fclose (icon_fp);
     existXpmIcon = True;
     strcpy(icon_path, default_path);
  }

  if (existXpmIcon)
  {
     status = XpmReadFileToPixmap(g_display,
		  	       DefaultRootWindow(g_display),
			       icon_path,
			       &IconPixmap,
			       &IconShape,
			       NULL);
     if (status != XpmSuccess) {
        error("XpmError: %s\n", XpmGetErrorString(status));
        /* exit(1); */
	existXpmIcon = False;
     }
  }

  if (!existXpmIcon)
  {
     IconPixmap = XCreatePixmapFromBitmapData(g_display,
					DefaultRootWindow(g_display),
					(char *)icon_bits,
					icon_width,
					icon_height,
                                        nx_red,
                                        nx_white,
					DefaultDepth(g_display, DefaultScreen(g_display)));
     IconShape = 0;
  }

  free(icon_path);
  *nxIcon = IconPixmap;
  *nxMask = IconShape;
  return existXpmIcon;
}

void RunOrKillNXkbd()
{
  info("nxkbd is running\n");
  if (xkbdRunning)
  {
#ifdef NXAGENT_XKBD_DEBUG
    nxdesktopDebug("RunOrKillNXkbd","nxkbd now is NOT running\n");
#endif
    xkbdRunning = False;
    kill( pidkbd, 9 );
  }
  else
  {
    char temp_string[256];
    char kbddisplay[256];
    char *kbdargs[7];
    int kbd_i, kbddisplay_len;
  
    kbddisplay_len = strlen(DisplayString(g_display));
    strncpy(temp_string,"",256);
    strncpy(kbddisplay,"",256);
    strcpy(kbddisplay,"localhost");
    strncpy(temp_string, DisplayString(g_display), kbddisplay_len);
    for (kbd_i=0; kbd_i<7; kbd_i++) kbddisplay[kbd_i+9] = temp_string[kbddisplay_len - 7 + kbd_i];
  
    kbdargs[0] = "nxkbd";
    kbdargs[1] = "-geometry";
    kbdargs[2] = "240x70+0+250";
    kbdargs[3] = "-g_display";
    kbdargs[4] = kbddisplay;
    kbdargs[5] = "-3";
    kbdargs[6] = NULL;

    switch ( pidkbd = fork() )
    {
      case 0:
        execvp(kbdargs[0], kbdargs);
#ifdef NXAGENT_XKBD_DEBUG
	nxdesktopDebug("RunOrKillNXkbd","execvp of nxkbd failed\n");
#endif
        exit(1);
      case -1:
#ifdef NXAGENT_XKBD_DEBUG
        nxdesktopDebug("RunOrKillNXkbd","can't fork to run nxkbd\n");
#endif
        break;
      default:
        ;
    }
#ifdef NXAGENT_XKBD_DEBUG
    nxdesktopDebug("RunOrKillNXkbd","nxkbd now is running on $DISPLAY %s\n", kbddisplay);
#endif
    xkbdRunning = True;
  }
}  
/* Seamless mode V2 */
/* 
    
How virtual channels work

The use a virtual channel, the basic steps are:
- Init the server side channel
    A name to the channel is registered and a call back function is provided to allow
    the terminal server notifies the client about the events concerning the session
- Open the client side channel
    This step hooks another callback function which is used to have notificastions about the virtual
    channel.
Once the channel is stablished it's a matter of sending and retreiving data.
Closing the channels is not necessary as the server will do that once the session is closed unless a
persisten channel is open. In this case, the data will continue to flow even if the session is closed.

*/

/* TODO: 

The registration process seens fine. Discover how to start the server side of the channel
Basically calling channel_init with the correct parameters which I'll discover as soon as I
debug the windows DLL

*/

/* Sends data to the fwindow channel */

/*static void
fwindow_send(STREAM s)
{
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    nxdesktopDebug("fwindow_send","sent: \n");
    hexdump(s->channel_hdr + 8, s->end - s->channel_hdr - 8);
    #endif
    channel_send(s, fwindow_channel);
}*/

/* Callback for the fwindow channel.
   Disabled to avoid compiler warnings.
   At this moment, we don't have a mechanism to receive events related to 
   the channel itself but just the channel data processing. This is not 
   a problem as we don't have to monitor the channel state to use it so this 
   function will remain disabled for now. 

static void
fwindow_state_process(STREAM s)
{
    int event dummy1, dummy2, dummy3;
    
    in_uint16_le(s, event);
    in_uint16_le(s, dummy1);
    in_uint16_le(s, dummy2);
    in_uint16_le(s, dummy3);
    
    
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    nxdesktopDebug("fwindow_state_process","CLIPPER received: \n");
    nxdesktopDebug("fwindow_state_process","stream headers: size %0x, iso %0x, sec %0x, mcs %0x, rdp %0x channel %0x\n",s->size, s->iso_hdr, s->sec_hdr, s->mcs_hdr, s->rdp_hdr, s->channel_hdr);
    hexdump(s->data, s->end - s->p);
    #endif
    event = s->data[1];
    
    switch (event)
    {
	case CHANNEL_EVENT_INITIALIZED:
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_state_process","CHANNEL_EVENT_INTIALIZED received:\n");
	    #endif
	    break;
	case CHANNEL_EVENT_CONNECTED:
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_state_process","CHANNEL_EVENT_CONNECTED received:\n");
	    #endif
	    break;
	case CHANNEL_EVENT_V1_CONNECTED:
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_state_process","CHANNEL_EVENT_V1_CONNECTED received:\n");
	    #endif
	    break;
	case CHANNEL_EVENT_DISCONNECTED:
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_state_process","CHANNEL_EVENT_DISCONNECTED received:\n");
	    #endif
	    break;
	case CHANNEL_EVENT_TERMINATED:
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_state_process","CHANNEL_EVENT_TERMINATED received:\n");
	    #endif
	    break;
	default:
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_state_process","Other event: (%d) received:\n",event);
	    #endif
	    break;
    }
	
} */

/* This is where the data from the clipper channel will be processed.

void
process_rdp_windows(RDP_WINDOWS c)
{
    int i;
    
    for (i = 0; i < num_rdp_windows; i++)
    {
	if (rdp_windows[i].id == c.id)
	{
	    update_shape(c.id);
	    return;
	}
    }
    if (num_rdp_windows < MAX_RDP_WINDOWS)
    {
	rdp_windows[num_rdp_windows] = c;
	num_rdp_windows++;
    }
    
}*/

#ifdef NXDESKTOP_FWINDOW_MODE
void
add_rdp_windows(NXCLIPPER_WINDOWS c)
{
    int i;
    
    for (i = 0; i < nxclipper_windows_entries; i++)
    {
	if (nxclipper_windows[i].id == 0)
	{
	    nxclipper_windows[i] = c;
	    return;
	}
    }
    nxclipper_windows_entries++;
    if (nxclipper_windows_entries > MAX_NXCLIPPER_WINDOWS)
    {
	error("add_rdp_windows: Maximum number of RDP windows reached.\n");
    }
    else
    {
	nxclipper_windows[nxclipper_windows_entries] = c;
	fprintf(stderr, "Window %d added. ID = %d.\n", nxclipper_windows_entries,c.id);
    }
}

void
update_rdp_windows(NXCLIPPER_WINDOWS c)
{
    int i;
    
    for (i = 0; i < nxclipper_windows_entries; i++)
    {
	if (nxclipper_windows[i].id == c.id)
	{
	    nxclipper_windows[i] = c;
	    return;
	}
    }
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    warning("update_rdp_windows: RDP Window ID not found.\n");
    #endif
}

NXCLIPPER_WINDOWS
get_rdp_windows(unsigned int id)
{
    int i;
    
    for (i = 0; i < nxclipper_windows_entries; i++)
    {
	if (nxclipper_windows[i].id == id)
	{
	    return nxclipper_windows[i];
	}
    }
    error("get_rdp_windows: RDP window id not found.\n");
    return (NXCLIPPER_WINDOWS) ;
}

void fwindow_send(STREAM s)
{
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    nxdesktopDebug("fwindow_send","sent: \n");
    hexdump(s->channel_hdr + 8, s->end - s->channel_hdr - 8);
    #endif
    channel_send(s, fwindow_channel);
}



static void
fwindow_process(STREAM s)
{
    
    char *data, *aux, *value;
    NXCLIPPER_WINDOWS tmp_win;
    
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    nxdesktopDebug("fwindow_process","CLIPPER received: \n");
    nxdesktopDebug("fwindow_process","stream headers: size %0x, iso %0x, sec %0x, mcs %0x, rdp %0x channel %0x\n",s->size, s->iso_hdr, s->sec_hdr, s->mcs_hdr, s->rdp_hdr, s->channel_hdr);
    hexdump(s->p, s->end - s->p);
    #endif

    data = s->p;
    
    data[s->end-s->p-1] = '\0';
    
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    nxdesktopDebug("fwindow_process","Raw message data: %s\n",data);
    #endif
    
    aux = strtok(data,";");
    while (aux)
    {
	value=strchr(aux,'=');
	if (value)
	{
	    value++;
	    if (strstr(aux,"MSG="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","MSG received. Value: %s\n",value);
		#endif
		tmp_win.msg = value;
	    }
	    else
	    if (strstr(aux,"OP="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","OP received. Value: %s\n",value);
		#endif
		tmp_win.op = atoi(value);
	    }
	    else
	    if (strstr(aux,"ID="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","ID received. Value: %s\n",value);
		#endif
		tmp_win.id = atoi(value);
	    }
	    else    
	    if (strstr(aux,"TITLE="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","TITLE received. Value: %s\n",value);
		#endif
		tmp_win.title = value;
	    }
	    else    
	    if (strstr(aux,"X="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","X received. Value: %s\n",value);
		#endif
		tmp_win.x = atoi(value);
	    }
	    else    
	    if (strstr(aux,"Y="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","Y received. Value: %s\n",value);
		#endif
		tmp_win.y = atoi(value);
	    }
	    else
	    if (strstr(aux,"TYPE="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","Y received. Value: %s\n",value);
		#endif
		tmp_win.max_min_type = atoi(value);
	    }
	    else
	    if (strstr(aux,"W="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","W received. Value: %s\n",value);
		#endif
		tmp_win.w = atoi(value);
	    }
	    else    
	    if (strstr(aux,"H="))
	    {
		#ifdef NXDESKTOP_FWINDOW_DEBUG
		nxdesktopDebug("fwindow_process","H received. Value: %s\n",value);
		#endif
		tmp_win.h = atoi(value);
	    }
	    else 
	    {
		unimpl("fwindow_process: ","Unknown values from clipper channel '%s' and '%s' .\n",aux,value);
	    }
	    
	}
	aux = strtok('\0',";");
    
    }
    
    switch (tmp_win.op)
    {
	case 0 :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 0 (CREATE) received.\n");
	    #endif
	    
	    #ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	    XShapeCombineRectangles(g_display, g_wnd, ShapeBounding, 0, 0, &rect, 1, ShapeUnion, 0);
	    #endif
	    if (first_rdp_window)
	    {
		g_x_offset = 0;//tmp_win.x; ? Not sure if I should do that way.
		g_y_offset = 0;//tmp_win.y; ? Let's keep it zeroed for now.
		g_width = tmp_win.w;
		g_height = tmp_win.h;
		
		XMoveResizeWindow(g_display, g_wnd, tmp_win.x, tmp_win.y, tmp_win.w, tmp_win.h);
		
		first_rdp_window = False;
		add_rdp_windows(tmp_win);
		
		
	    } 
	    else
	    {	
		tmp_win.wnd = XCreateSimpleWindow(g_display, g_wnd, tmp_win.x, tmp_win.y, tmp_win.w, tmp_win.h, 0, 0, 0);
		XMapWindow(g_display, tmp_win.wnd);
		add_rdp_windows(tmp_win);
	    }
		
	    break;
	case 1 :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 1 (DESTROY) received.\n");
	    #endif
	    #ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	    XShapeCombineRectangles(g_display, g_wnd, ShapeBounding, 0, 0, &rect, 1, ShapeSubtract, 0);
	    #endif
	    break;
	case 2 :
	    break; // Temporary?
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 2 (MOVING) received.\n");
	    #endif
	    #ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	    if (!rdp_window_moving)
	    {
	        pix_move_buffer = XCreatePixmap(g_display, g_wnd, tmp_win.w, tmp_win.h, g_depth);
	        XCopyArea(g_display, g_wnd, pix_move_buffer, g_gc, tmp_win.x, tmp_win.y, tmp_win.w, tmp_win.h, 0, 0);
	    }	
	    
	    XCopyArea(g_display, pix_move_buffer, g_wnd, g_gc, 0, 0, tmp_win.w, tmp_win.h, tmp_win.x, tmp_win.y);
	    XShapeCombineRectangles(g_display, g_wnd, ShapeBounding, 0, 0, &rect, 1, ShapeSet, Unsorted);
	    #endif
	    fprintf(stderr,"moving %d %d %d %d\n", tmp_win.x, tmp_win.y, tmp_win.w, tmp_win.h);
	    XMoveWindow(g_display, g_wnd, tmp_win.x, tmp_win.y);
	    
	    break;
	case 3 :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 3 (RESIZING) received.\n");
	    #endif
	    #ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	    XShapeCombineRectangles(g_display, g_wnd, ShapeBounding, 0, 0, &rect, 1, ShapeSet, 0);
	    #endif
	    XResizeWindow(g_display, g_wnd, tmp_win.w, tmp_win.h);
	    break;
	case 4 :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 4 (MIN_MAX) received. %d %d %d %d\n", tmp_win.x, tmp_win.y, tmp_win.w, tmp_win.h);
	    #endif
	    switch (tmp_win.max_min_type)
	    {
		case 2 :
		case 6 :
		    XIconifyWindow(g_display, g_wnd, DefaultScreen(g_display));
		    break;
		case 3:
		case 11:
		    XMoveResizeWindow(g_display, g_wnd, tmp_win.x, tmp_win.y, tmp_win.w, tmp_win.h);
		    XMapRaised (g_display, g_wnd);
		    break;
		default:
		    nxdesktopDebug("fwindow_process","TYPE %d.\n", tmp_win.max_min_type);
		    break;
	    }
	    g_x_offset = tmp_win.x;
	    g_y_offset = tmp_win.y;
	    break;
	case 5 :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 5 (MOVE_SIZE) received.\n");
	    #endif
	    #ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	    if (rdp_window_moving)
	    {
		rdp_window_moving = False;
		XCopyArea(g_display, pix_move_buffer, g_wnd, g_gc, 0, 0, tmp_win.w, tmp_win.h, tmp_win.x, tmp_win.y);
		XFreePixmap(g_display, pix_move_buffer);
	    }
	    XShapeCombineRectangles(g_display, g_wnd, ShapeBounding, 0, 0, &rect, 1, ShapeSet, 0);
	    #endif
	    rdp_window_moving = False;
	    break;
	case 6 :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 6 (SIZING) received.\n");
	    #endif
	    #ifdef NXDESKTOP_ENABLE_MASK_PROCESSING
	    XShapeCombineRectangles(g_display, g_wnd, ShapeBounding, 0, 0, &rect, 1, ShapeSet, 0);
	    #endif
	    g_width = tmp_win.w;
	    g_height = tmp_win.h;
	    XResizeWindow(g_display, g_wnd, tmp_win.w, tmp_win.h);
	    break;
	case 7 :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 7 (CHANGING Z POS) received.\n");
	    #endif
	    break;
	case 8 :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","OP 8 (CHANGED Z POS) received.\n");
	    #endif
	    break;
	default :
	    #ifdef NXDESKTOP_FWINDOW_DEBUG
	    nxdesktopDebug("fwindow_process","Unknown OP received.\n");
	    #endif
	    break;
	
    }
    update_rdp_windows(tmp_win);
}


/* Opens (registers) the client side channel and provides the callback to it */

BOOL
fwindow_register(void)
{
    
    fwindow_channel = channel_register("clipper", CHANNEL_OPTION_INITIALIZED, fwindow_process);
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    if (fwindow_channel != NULL)
	nxdesktopDebug("fwindow_register","Channel CLIPPER registered.\n");
    #endif
    return (fwindow_channel != NULL);
    
}
 
void
fwindow_init(void)
{

    STREAM s;
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    nxdesktopDebug("fwindow_init","Channel CLIPPER initialized.\n");
    #endif
    s = channel_init(fwindow_channel, 12); /* I'm not sure if this dummy header works but so far, so good */
    #ifdef NXDESKTOP_FWINDOW_DEBUG
    
    if (s != NULL)
    {
	nxdesktopDebug("fwindow_init","STREAM is valid.\n");
	nxdesktopDebug("fwindow_init","stream headers: size %0x, iso %0x, sec %0x, mcs %0x, rdp %0x channel %0x\n",s->size, s->iso_hdr, s->sec_hdr, s->mcs_hdr, s->rdp_hdr, s->channel_hdr);
    }
    #endif
}
#endif

void nxdesktopMoveViewport(int hShift, int vShift)
{
    int newX;
    int newY;
    Bool do_move = False;
    int viewportXSpan = g_viewport_width - g_width;
    int viewportYSpan = g_viewport_height - g_height;

    if (!g_viewport_wnd)
    {
	return;
    }

  /*
   * We must keep x coordinate between viewportXSpan and zero, if viewportXSpan
   * is less then zero. If viewportXSpan is greater or equal to zero, it means
   * the agent root window has a size smaller than the agent default window.
   * In this case we keep the old coordinate.
   */

    if (viewportXSpan < 0)
    {
	newX = g_wnd_x - hShift;

	if (newX > 0)
	{
	    newX = 0;
	}
	else if (newX < viewportXSpan)
	{
	    newX = viewportXSpan;
	}
    }
    else if (viewportXSpan == 0)
    {
	newX = 0;
    }
    else
    {
	newX = g_wnd_x;
    }

    if (viewportYSpan < 0)
    {
	newY = g_wnd_y - vShift;
	if (newY > 0)
	{
	    newY = 0;
	}
	else if (newY < viewportYSpan)
	{
	    newY = viewportYSpan;
	}
    }
    else if (viewportYSpan == 0)
    {
	newY = 0;
    }
    else
    {
	newY = g_wnd_y;
    }
    if (newX != g_wnd_x)
    {
	g_wnd_x = newX;
	do_move = True;
    }
    if (newY != g_wnd_y)
    {
	g_wnd_y = newY;
	do_move = True;
    }

    if (do_move)
    {
	#ifdef TEST
	nxdesktopDebug("nxdesktopMoveViewport: New viewport geometry: (%d, %d)-"
                "(%d, %d)\n", -g_wnd_x, -g_wnd_y, -g_wnd_x + g_width,
                     -g_wnd_y + g_height);
	#endif
	XMoveWindow(g_display, g_wnd, g_wnd_x, g_wnd_y);
    }
}

void
ui_begin_update(void)
{
}

void
ui_end_update(void)
{
}
/* end */
