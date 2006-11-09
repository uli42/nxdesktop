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
/* Copyright (c) 2001,2006 NoMachine, http://www.nomachine.com.           */
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

#include "NXalert.h"

#undef NXDESKTOP_XWIN_EVENTS_DEBUG
#undef NXDESKTOP_XWIN_METHODS_DEBUG
#undef NXDESKTOP_XWIN_SELECT_DEBUG
#undef NXDESKTOP_XWIN_GRAPHICS_DEBUG
#undef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
#undef NXDESKTOP_XWIN_TEXT_DEBUG

#undef NXDESKTOP_USES_SYNC_IN_LOOP

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

BOOL rdp_window_moving = False;

/* We can't include Xproto.h because of conflicting defines for BOOL */
#define X_ConfigureWindow              12

#ifdef NXDESKTOP_ONSTART

/* Atom nxdesktop_WM_START; */

#endif

#define NXDESKTOP_MIN_WIDTH 120
#define NXDESKTOP_MIN_HEIGHT 90

extern int g_width;
extern int g_height;
extern int g_xpos;
extern int g_ypos;
extern int g_pos;
extern BOOL g_sendmotion;
extern BOOL g_fullscreen;
extern BOOL g_grab_keyboard;
extern BOOL g_hide_decorations;
extern BOOL g_use_rdp5;

extern char g_title[];
/* Color depth of the RDP session.
   As of RDP 5.1, it may be 8, 15, 16 or 24. */
extern int g_server_depth;

extern int g_win_button_size;

extern int xo;
extern int yo;
extern BOOL magickey;
extern char windowName[255];

Display *g_display;
Time g_last_gesturetime;
static int g_x_socket;
static Screen *g_screen;
Window g_wnd;
extern BOOL viewport_mode;
BOOL viewport_keys_enabled = False;
BOOL viewport_mode_locked = False;
Window g_viewport_wnd = 0;
uint32 g_embed_wnd;
static Window wnd2;
BOOL g_Unobscured;		/* used for screenblt */
static Visual *g_visual;
static int g_depth;
static int g_bpp;
static XIM g_IM;
static XIC g_IC;
static XModifierKeymap *g_mod_map;
static Cursor g_current_cursor;
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
int viewport_x1;
int viewport_y1;
extern int perm_xo;
extern int perm_yo;

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

/*
 * Avoid to use special NX operations
 * when not using NX transport. Ugly
 * but works.
 */

BOOL nxdesktopUseNXTrans = False;
BOOL nxdesktopUseNXRdpImages = False;
BOOL nxdesktopUseNXCompressedRdpImages = False;
BOOL nxdesktopUseRDPFilter = True;
BOOL nxdesktopCongestion = False;

BOOL nxdesktopColormapCompat = False;
BOOL nxdesktopColormapPacked = False;

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

unsigned int nxdesktopLinkType;
unsigned int nxdesktopPackMethod, nxdesktopPackQuality;
unsigned int nxdesktopProtocolLocalMajor, nxdesktopProtocolLocalMinor, nxdesktopProtocolLocalPatch;
unsigned int nxdesktopProtocolRemoteMajor, nxdesktopProtocolRemoteMinor, nxdesktopProtocolRemotePatch;
int nxdesktopFrameTimeout, nxdesktopPingTimeout;
int nxdesktopSplitMode, nxdesktopSplitSize;
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
extern BOOL nxclient_is_windows;

#ifdef NXWIN_USES_PACKED_RDP_TEXT
BOOL nxdesktopCanPackRDPText = False;
#endif

/* Image cache flag */
extern BOOL rdp_img_cache;

/* Is the nx rdp bitmap cache active? */
extern BOOL rdp_img_cache_nxcompressed;

/* endianness */
static BOOL g_host_be;
static BOOL g_xserver_be;
static int g_red_shift_r, g_blue_shift_r, g_green_shift_r;
static int g_red_shift_l, g_blue_shift_l, g_green_shift_l;

/* SeamlessRDP support */
typedef struct _seamless_group
{
	Window wnd;
	unsigned long id;
	unsigned int refcnt;
} seamless_group;
typedef struct _seamless_window
{
	Window wnd;
	unsigned long id;
	unsigned long behind;
	seamless_group *group;
	int xoffset, yoffset;
	int width, height;
	int state;		/* normal/minimized/maximized. */
	unsigned int desktop;
	struct timeval *position_timer;

	BOOL outstanding_position;
	unsigned int outpos_serial;
	int outpos_xoffset, outpos_yoffset;
	int outpos_width, outpos_height;

	struct _seamless_window *next;
} seamless_window;
static seamless_window *g_seamless_windows = NULL;
static unsigned long g_seamless_focused = 0;
static BOOL g_seamless_started = False;	/* Server end is up and running */
static BOOL g_seamless_active = False;	/* We are currently in seamless mode */
static BOOL g_seamless_hidden = False;	/* Desktop is hidden on server */
extern BOOL g_seamless_rdp;

extern BOOL float_window_mode;
int g_x_offset = 0;
int g_y_offset = 0;

extern uint32 g_embed_wnd;
BOOL g_enable_compose = False;
BOOL g_Unobscured;		/* used for screenblt */
static GC g_gc = NULL;
static GC g_create_bitmap_gc = NULL;
static GC g_create_glyph_gc = NULL;
static XRectangle g_clip_rectangle;
static Visual *g_visual;
/* Color depth of the X11 visual of our window (e.g. 24 for True Color R8G8B visual).
   This may be 32 for R8G8B8 visuals, and then the rest of the bits are undefined
   as far as we're concerned. */
static int g_depth;
/* Bits-per-Pixel of the pixmaps we'll be using to draw on our window.
   This may be larger than g_depth, in which case some of the bits would
   be kept solely for alignment (e.g. 32bpp pixmaps on a 24bpp visual). */
static int g_bpp;
static XIM g_IM;
static XIC g_IC;
static XModifierKeymap *g_mod_map;
static Cursor g_current_cursor;
static HCURSOR g_null_cursor = NULL;
static Atom g_protocol_atom, g_kill_atom;
extern Atom g_net_wm_state_atom;
extern Atom g_net_wm_desktop_atom;
static BOOL g_focused;
static BOOL g_mouse_in_wnd;
/* Indicates that:
   1) visual has 15, 16 or 24 depth and the same color channel masks
      as its RDP equivalent (implies X server is LE),
   2) host is LE
   This will trigger an optimization whose real value is questionable.
*/
static BOOL g_compatible_arch;
/* Indicates whether RDP's bitmaps and our XImages have the same
   binary format. If so, we can avoid an expensive translation.
   Note that this can be true when g_compatible_arch is false,
   e.g.:
   
     RDP(LE) <-> host(BE) <-> X-Server(LE)
     
   ('host' is the machine running rdesktop; the host simply memcpy's
    so its endianess doesn't matter)
 */
static BOOL g_no_translate_image = False;

/* endianness */
static BOOL g_host_be;
static BOOL g_xserver_be;
static int g_red_shift_r, g_blue_shift_r, g_green_shift_r;
static int g_red_shift_l, g_blue_shift_l, g_green_shift_l;

/* software backing store */
extern BOOL g_ownbackstore;
static Pixmap g_backstore = 0;

/* Moving in single app mode */
static BOOL g_moving_wnd;
static int g_move_x_offset = 0;
static int g_move_y_offset = 0;
static BOOL g_using_full_workarea = False;

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
	unsigned long flags;
	unsigned long functions;
	unsigned long decorations;
	long inputMode;
	unsigned long status;
}
PropMotifWmHints;

typedef struct
{
	uint32 red;
	uint32 green;
	uint32 blue;
}
PixelColour;

#define ON_ALL_SEAMLESS_WINDOWS(func, args) \
        do { \
                seamless_window *sw; \
                XRectangle rect; \
		if (!g_seamless_windows) break; \
                for (sw = g_seamless_windows; sw; sw = sw->next) { \
                    rect.x = g_clip_rectangle.x - sw->xoffset; \
                    rect.y = g_clip_rectangle.y - sw->yoffset; \
                    rect.width = g_clip_rectangle.width; \
                    rect.height = g_clip_rectangle.height; \
                    XSetClipRectangles(g_display, g_gc, 0, 0, &rect, 1, YXBanded); \
                    func args; \
                } \
                XSetClipRectangles(g_display, g_gc, 0, 0, &g_clip_rectangle, 1, YXBanded); \
        } while (0)

static void
seamless_XFillPolygon(Drawable d, XPoint * points, int npoints, int xoffset, int yoffset)
{
	points[0].x -= xoffset;
	points[0].y -= yoffset;
	XFillPolygon(g_display, d, g_gc, points, npoints, Complex, CoordModePrevious);
	points[0].x += xoffset;
	points[0].y += yoffset;
}

static void
seamless_XDrawLines(Drawable d, XPoint * points, int npoints, int xoffset, int yoffset)
{
	points[0].x -= xoffset;
	points[0].y -= yoffset;
	XDrawLines(g_display, d, g_gc, points, npoints, CoordModePrevious);
	points[0].x += xoffset;
	points[0].y += yoffset;
}

/* NX */
/* Holds the key modifiers state */
unsigned int buf_key_vector;

/* NX */

#define FILL_RECTANGLE(x,y,cx,cy)\
{ \
	XFillRectangle(g_display, g_wnd, g_gc, x, y, cx, cy); \
 	ON_ALL_SEAMLESS_WINDOWS(XFillRectangle, (g_display, sw->wnd, g_gc, x-sw->xoffset, y-sw->yoffset, cx, cy)); \
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
	ON_ALL_SEAMLESS_WINDOWS(seamless_XFillPolygon, (sw->wnd, p, np, sw->xoffset, sw->yoffset)); \
}

#define DRAW_ELLIPSE(x,y,cx,cy,m)\
{ \
	switch (m) \
	{ \
		case 0:	/* Outline */ \
			XDrawArc(g_display, g_wnd, g_gc, x, y, cx, cy, 0, 360*64); \
			ON_ALL_SEAMLESS_WINDOWS(XDrawArc, (g_display, sw->wnd, g_gc, x-sw->xoffset, y-sw->yoffset, cx, cy, 0, 360*64)); \
			if (g_ownbackstore) \
				XDrawArc(g_display, g_backstore, g_gc, x, y, cx, cy, 0, 360*64); \
			break; \
		case 1: /* Filled */ \
			XFillArc(g_display, g_wnd, g_gc, x, y, cx, cy, 0, 360*64); \
			ON_ALL_SEAMLESS_WINDOWS(XFillArc, (g_display, sw->wnd, g_gc, x-sw->xoffset, y-sw->yoffset, cx, cy, 0, 360*64)); \
			if (g_ownbackstore) \
				XFillArc(g_display, g_backstore, g_gc, x, y, cx, cy, 0, 360*64); \
			break; \
	} \
}

/* colour maps */
extern BOOL g_owncolmap;
static Colormap g_xcolmap;
static uint32 *g_colmap = NULL;
static unsigned int r, b, g, or, ob, og, off;

#define TRANSLATE(col)		( g_server_depth != 8 ? translate_colour(col) : g_owncolmap ? col : g_colmap[col] )
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

static seamless_window *
sw_get_window_by_id(unsigned long id)
{
	seamless_window *sw;
	for (sw = g_seamless_windows; sw; sw = sw->next)
	{
		if (sw->id == id)
			return sw;
	}
	return NULL;
}

static seamless_window *
sw_get_window_by_wnd(Window wnd)
{
	seamless_window *sw;
	for (sw = g_seamless_windows; sw; sw = sw->next)
	{
		if (sw->wnd == wnd)
			return sw;
	}
	return NULL;
}


static void
sw_remove_window(seamless_window * win)
{
	seamless_window *sw, **prevnext = &g_seamless_windows;
	for (sw = g_seamless_windows; sw; sw = sw->next)
	{
		if (sw == win)
		{
			*prevnext = sw->next;
			sw->group->refcnt--;
			if (sw->group->refcnt == 0)
			{
				XDestroyWindow(g_display, sw->group->wnd);
				xfree(sw->group);
			}
			xfree(sw->position_timer);
			xfree(sw);
			return;
		}
		prevnext = &sw->next;
	}
	return;
}


/* Move all windows except wnd to new desktop */
static void
sw_all_to_desktop(Window wnd, unsigned int desktop)
{
	seamless_window *sw;
	for (sw = g_seamless_windows; sw; sw = sw->next)
	{
		if (sw->wnd == wnd)
			continue;
		if (sw->desktop != desktop)
		{
			ewmh_move_to_desktop(sw->wnd, desktop);
			sw->desktop = desktop;
		}
	}
}


/* Send our position */
static void
sw_update_position(seamless_window * sw)
{
	XWindowAttributes wa;
	int x, y;
	Window child_return;
	unsigned int serial;

	XGetWindowAttributes(g_display, sw->wnd, &wa);
	XTranslateCoordinates(g_display, sw->wnd, wa.root,
			      -wa.border_width, -wa.border_width, &x, &y, &child_return);

	serial = seamless_send_position(sw->id, x, y, wa.width, wa.height, 0);

	sw->outstanding_position = True;
	sw->outpos_serial = serial;

	sw->outpos_xoffset = x;
	sw->outpos_yoffset = y;
	sw->outpos_width = wa.width;
	sw->outpos_height = wa.height;
}


/* Check if it's time to send our position */
static void
sw_check_timers()
{
	seamless_window *sw;
	struct timeval now;

	gettimeofday(&now, NULL);
	for (sw = g_seamless_windows; sw; sw = sw->next)
	{
		if (timerisset(sw->position_timer) && timercmp(sw->position_timer, &now, <))
		{
			timerclear(sw->position_timer);
			sw_update_position(sw);
		}
	}
}


static void
sw_restack_window(seamless_window * sw, unsigned long behind)
{
	seamless_window *sw_above;

	/* Remove window from stack */
	for (sw_above = g_seamless_windows; sw_above; sw_above = sw_above->next)
	{
		if (sw_above->behind == sw->id)
			break;
	}

	if (sw_above)
		sw_above->behind = sw->behind;

	/* And then add it at the new position */
	for (sw_above = g_seamless_windows; sw_above; sw_above = sw_above->next)
	{
		if (sw_above->behind == behind)
			break;
	}

	if (sw_above)
		sw_above->behind = sw->id;

	sw->behind = behind;
}


static void
sw_handle_restack(seamless_window * sw)
{
	Status status;
	Window root, parent, *children;
	unsigned int nchildren, i;
	seamless_window *sw_below;

	status = XQueryTree(g_display, RootWindowOfScreen(g_screen),
			    &root, &parent, &children, &nchildren);
	if (!status || !nchildren)
		return;

	sw_below = NULL;

	i = 0;
	while (children[i] != sw->wnd)
	{
		i++;
		if (i >= nchildren)
			goto end;
	}

	for (i++; i < nchildren; i++)
	{
		sw_below = sw_get_window_by_wnd(children[i]);
		if (sw_below)
			break;
	}

	if (!sw_below && !sw->behind)
		goto end;
	if (sw_below && (sw_below->id == sw->behind))
		goto end;

	if (sw_below)
	{
		seamless_send_zchange(sw->id, sw_below->id, 0);
		sw_restack_window(sw, sw_below->id);
	}
	else
	{
		seamless_send_zchange(sw->id, 0, 0);
		sw_restack_window(sw, 0);
	}

      end:
	XFree(children);
}

static seamless_group *
sw_find_group(unsigned long id, BOOL dont_create)
{
	seamless_window *sw;
	seamless_group *sg;
	XSetWindowAttributes attribs;

	for (sw = g_seamless_windows; sw; sw = sw->next)
	{
		if (sw->group->id == id)
			return sw->group;
	}

	if (dont_create)
		return NULL;

	sg = xmalloc(sizeof(seamless_group));

	sg->wnd =
		XCreateWindow(g_display, RootWindowOfScreen(g_screen), -1, -1, 1, 1, 0,
			      CopyFromParent, CopyFromParent, CopyFromParent, 0, &attribs);

	sg->id = id;
	sg->refcnt = 0;

	return sg;
}

static void
mwm_hide_decorations(Window wnd)
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

	XChangeProperty(g_display, wnd, hintsatom, hintsatom, 32, PropModeReplace,
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

/* The following macros output the same octet sequences
   on both BE and LE hosts: */

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
	switch (g_server_depth)
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
		default:
			/* Avoid warning */
			pc.red = 0;
			pc.green = 0;
			pc.blue = 0;
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

	if (g_compatible_arch)
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

	if (g_compatible_arch)
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

	if (g_compatible_arch)
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

	if (g_compatible_arch)
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

	if (g_compatible_arch)
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

	if (g_compatible_arch)
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

	if (g_compatible_arch)
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

	if (g_compatible_arch)
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
	 	/* Only read 3 bytes. Reading 4 bytes means reading beyond buffer. */
		*((uint32 *) out) = *((uint16 *) data) + (*((uint8 *) data + 2) << 16);
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

	/*
	   If RDP depth and X Visual depths match,
	   and arch(endian) matches, no need to translate:
	   just return data.
	   Note: select_visual should've already ensured g_no_translate
	   is only set for compatible depths, but the RDP depth might've
	   changed during connection negotiations.
	 */
	if (g_no_translate_image)
	{
		if ((g_depth == 15 && g_server_depth == 15) ||
		    (g_depth == 16 && g_server_depth == 16) ||
		    (g_depth == 24 && g_server_depth == 24))
			return data;
	}

	size = width * height * (g_bpp / 8);
	out = (uint8 *) xmalloc(size);
	end = out + size;

	switch (g_server_depth)
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
	#ifdef NXDESKTOP_XWIN_EVENTS_DEBUG
	nxdesktopDebug("sigusr_func","Received SIGUSR1, unmapping window.\n");
	#endif
	XUnmapWindow (g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd);
    break;
    case SIGUSR2:
	#ifdef NXDESKTOP_XWIN_EVENTS_DEBUG
	nxdesktopDebug("sigusr_func","Received SIGUSR2, unmapping window.\n");
	#endif
    	XMapRaised (g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd);
    	XIconifyWindow (g_display, wnd2, DefaultScreen(g_display));
    break;
    default:
    break;
    }
    #ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
    XFlush (g_display);
    #endif
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

static void
calculate_shifts(uint32 mask, int *shift_r, int *shift_l)
{
	*shift_l = ffs(mask) - 1;
	mask >>= *shift_l;
	*shift_r = 8 - ffs(mask & ~(mask >> 1));
}

/* Given a mask of a colour channel (e.g. XVisualInfo.red_mask),
   calculates the bits-per-pixel of this channel (a.k.a. colour weight).
 */
static unsigned
calculate_mask_weight(uint32 mask)
{
	unsigned weight = 0;
	do
	{
		weight += (mask & 1);
	}
	while (mask >>= 1);
	return weight;
}

static BOOL
select_visual(int screen_num)
{
	XPixmapFormatValues *pfm;
	int pixmap_formats_count, visuals_count;
	XVisualInfo *vmatches = NULL;
	XVisualInfo template;
	int i;
	unsigned red_weight, blue_weight, green_weight;

	red_weight = blue_weight = green_weight = 0;

	if (g_server_depth == -1)
	{
		g_server_depth = DisplayPlanes(g_display, DefaultScreen(g_display));
	}

	pfm = XListPixmapFormats(g_display, &pixmap_formats_count);
	if (pfm == NULL)
	{
		error("Unable to get list of pixmap formats from display.\n");
		XCloseDisplay(g_display);
		return False;
	}

	/* Search for best TrueColor visual */
	template.class = TrueColor;
	template.screen = screen_num;
	vmatches =
		XGetVisualInfo(g_display, VisualClassMask | VisualScreenMask, &template,
			       &visuals_count);
	g_visual = NULL;
	g_no_translate_image = False;
	g_compatible_arch = False;
	if (vmatches != NULL)
	{
		for (i = 0; i < visuals_count; ++i)
		{
			XVisualInfo *visual_info = &vmatches[i];
			BOOL can_translate_to_bpp = False;
			int j;

			/* Try to find a no-translation visual that'll
			   allow us to use RDP bitmaps directly as ZPixmaps. */
			if (!g_xserver_be && (((visual_info->depth == 15) &&
					       /* R5G5B5 */
					       (visual_info->red_mask == 0x7c00) &&
					       (visual_info->green_mask == 0x3e0) &&
					       (visual_info->blue_mask == 0x1f)) ||
					      ((visual_info->depth == 16) &&
					       /* R5G6B5 */
					       (visual_info->red_mask == 0xf800) &&
					       (visual_info->green_mask == 0x7e0) &&
					       (visual_info->blue_mask == 0x1f)) ||
					      ((visual_info->depth == 24) &&
					       /* R8G8B8 */
					       (visual_info->red_mask == 0xff0000) &&
					       (visual_info->green_mask == 0xff00) &&
					       (visual_info->blue_mask == 0xff))))
			{
				g_visual = visual_info->visual;
				g_depth = visual_info->depth;
				g_compatible_arch = !g_host_be;
				g_no_translate_image = (visual_info->depth == g_server_depth);
				if (g_no_translate_image)
					/* We found the best visual */
					break;
			}
			else
			{
				g_compatible_arch = False;
			}

			if (visual_info->depth > 24)
			{
				/* Avoid 32-bit visuals and likes like the plague.
				   They're either untested or proven to work bad
				   (e.g. nvidia's Composite 32-bit visual).
				   Most implementation offer a 24-bit visual anyway. */
				continue;
			}

			/* Only care for visuals, for whose BPPs (not depths!)
			   we have a translateXtoY function. */
			for (j = 0; j < pixmap_formats_count; ++j)
			{
				if (pfm[j].depth == visual_info->depth)
				{
					if ((pfm[j].bits_per_pixel == 16) ||
					    (pfm[j].bits_per_pixel == 24) ||
					    (pfm[j].bits_per_pixel == 32))
					{
						can_translate_to_bpp = True;
					}
					break;
				}
			}

			/* Prefer formats which have the most colour depth.
			   We're being truly aristocratic here, minding each
			   weight on its own. */
			if (can_translate_to_bpp)
			{
				unsigned vis_red_weight =
					calculate_mask_weight(visual_info->red_mask);
				unsigned vis_green_weight =
					calculate_mask_weight(visual_info->green_mask);
				unsigned vis_blue_weight =
					calculate_mask_weight(visual_info->blue_mask);
				if ((vis_red_weight >= red_weight)
				    && (vis_green_weight >= green_weight)
				    && (vis_blue_weight >= blue_weight))
				{
					red_weight = vis_red_weight;
					green_weight = vis_green_weight;
					blue_weight = vis_blue_weight;
					g_visual = visual_info->visual;
					g_depth = visual_info->depth;
				}
			}
		}
		XFree(vmatches);
	}

	if (g_visual != NULL)
	{
		g_owncolmap = False;
		calculate_shifts(g_visual->red_mask, &g_red_shift_r, &g_red_shift_l);
		calculate_shifts(g_visual->green_mask, &g_green_shift_r, &g_green_shift_l);
		calculate_shifts(g_visual->blue_mask, &g_blue_shift_r, &g_blue_shift_l);
	}
	else
	{
		template.class = PseudoColor;
		template.depth = 8;
		template.colormap_size = 256;
		vmatches =
			XGetVisualInfo(g_display,
				       VisualClassMask | VisualDepthMask | VisualColormapSizeMask,
				       &template, &visuals_count);
		if (vmatches == NULL)
		{
			error("No usable TrueColor or PseudoColor visuals on this display.\n");
			XCloseDisplay(g_display);
			XFree(pfm);
			return False;
		}

		/* we use a colourmap, so the default visual should do */
		g_owncolmap = True;
		g_visual = vmatches[0].visual;
		g_depth = vmatches[0].depth;
	}

	g_bpp = 0;
	for (i = 0; i < pixmap_formats_count; ++i)
	{
		XPixmapFormatValues *pf = &pfm[i];
		if (pf->depth == g_depth)
		{
			g_bpp = pf->bits_per_pixel;

			if (g_no_translate_image)
			{
				switch (g_server_depth)
				{
					case 15:
					case 16:
						if (g_bpp != 16)
							g_no_translate_image = False;
						break;
					case 24:
						/* Yes, this will force image translation
						   on most modern servers which use 32 bits
						   for R8G8B8. */
						if (g_bpp != 24)
							g_no_translate_image = False;
						break;
					default:
						g_no_translate_image = False;
						break;
				}
			}

			/* Pixmap formats list is a depth-to-bpp mapping --
			   there's just a single entry for every depth,
			   so we can safely break here */
			break;
		}
	}
	XFree(pfm);
	pfm = NULL;
	return True;
}

static XErrorHandler g_old_error_handler;

static int
error_handler(Display * dpy, XErrorEvent * eev)
{
	if ((eev->error_code == BadMatch) && (eev->request_code == X_ConfigureWindow))
	{
		fprintf(stderr, "Got \"BadMatch\" when trying to restack windows.\n");
		fprintf(stderr,
			"This is most likely caused by a broken window manager (commonly KWin).\n");
		return 0;
	}

	return g_old_error_handler(dpy, eev);
}

BOOL ui_open_display()

{
    
    extern char errorMsg[511];
    extern char errorCaption[511];
    
    g_display = XOpenDisplay((char *)nxDisplay);
    if (g_display == NULL)
    {
        error("Failed to open display\n");
	snprintf(errorMsg,511,"The display %s could not be opened.\nPlease report this problem to support\npersonnel.",(char *)nxDisplay);
	snprintf(errorCaption,511,"Error");
	NXTransDialog(errorCaption, errorMsg, (char *)g_wnd, "ok", 0, (char *)nxDisplay );
	wait(NULL);
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

static void nxdesktopDisplayCongestionHandler(Display *display, int reason)
{

    if (reason == NXBeginCongestion)
    {
	#ifdef NXDESKTOP_CONGESTION_DEBUG
        nxdesktopDebug("nxdesktopDisplayCongestionHandler","Congestion state is [start].\n");
	#endif
	nxdesktopCongestion = True;
    }
    else
    {
	#ifdef NXDESKTOP_CONGESTION_DEBUG
        nxdesktopDebug("nxdesktopDisplayCongestionHandler","Congestion state is [end].\n");
	#endif
	nxdesktopCongestion = False;
    }
}

static void nxdesktopDisplayFlushHandler(Display *display, int reason)
{
    fprintf(stderr, "nxdesktopDisplayCongestionHandler: FLUSH! Flush requested in the proxy link.\n");
}

BOOL
ui_init(void)
{
	int screen_num;
	
	g_display = XOpenDisplay(nxDisplay);
	if (g_display == NULL)
	{
		error("Failed to open display: %s\n", XDisplayName(NULL));
		return False;
	}
   
	g_x_socket = ConnectionNumber(g_display); /* NX */
	{
          	extern void tcp_resize_buf(int, int, int);
          	extern int rdp_bufsize;
          	tcp_resize_buf(g_x_socket, 0, rdp_bufsize);
        }
	
	{
		uint16 endianess_test = 1;
		g_host_be = !(BOOL) (*(uint8 *) (&endianess_test));
	}

	g_old_error_handler = XSetErrorHandler(error_handler);
	g_xserver_be = (ImageByteOrder(g_display) == MSBFirst);
	screen_num = DefaultScreen(g_display);
	g_screen = ScreenOfDisplay(g_display, screen_num);
	g_depth = DefaultDepthOfScreen(g_screen);
	
	if (!select_visual(screen_num))
		return False;

	if (g_no_translate_image)
	{
		DEBUG(("Performance optimization possible: avoiding image translation (colour depth conversion).\n"));
	}

	if (g_server_depth > g_bpp)
	{
		warning("Remote desktop colour depth %d higher than display colour depth %d.\n",
			g_server_depth, g_bpp);
	}

	DEBUG(("RDP depth: %d, display depth: %d, display bpp: %d, X server BE: %d, host BE: %d\n",
	       g_server_depth, g_depth, g_bpp, g_xserver_be, g_host_be));

	XSetErrorHandler(nxdesktopErrorHandler);

	if (!g_owncolmap)
	{
		g_xcolmap = XCreateColormap(g_display, RootWindowOfScreen(g_screen), g_visual,	AllocNone);
		if (g_depth <= 8)
		    warning("Display colour depth is %d bits: you may want to use -C for a private colourmap\n", g_depth);
	}

	if ((!g_ownbackstore) && (DoesBackingStore(g_screen) != Always))
	{
		warning("External BackingStore not available. Using internal\n");
		g_ownbackstore = True;
	}

	/*
	 * Determine desktop size
	 */
	if (g_fullscreen)
	{
		g_width = WidthOfScreen(g_screen);
		g_height = HeightOfScreen(g_screen);
		g_using_full_workarea = True;
	}
	else if (g_width < 0)
	{
		/* Percent of screen */
		if (-g_width >= 100)
			g_using_full_workarea = True;
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
			g_using_full_workarea = True;
		}
		else
		{
			warning("Failed to get workarea: probably your window manager does not support extended hints\n");
	    		g_width = WidthOfScreen(g_screen);
	   		g_height = HeightOfScreen(g_screen);
		}
	}

	/* make sure width is a multiple of 4 */
	g_width = (g_width + 3) & ~3;

	g_mod_map = XGetModifierMapping(g_display);

	xkeymap_init();

	if (g_enable_compose)
		g_IM = XOpenIM(g_display, NULL, NULL, NULL);

	xclip_init();
	ewmh_init();
	if (g_seamless_rdp)
		seamless_init();

	DEBUG_RDP5(("server bpp %d client bpp %d depth %d\n", g_server_depth, g_bpp, g_depth));

	return True;
}

BOOL
ui_init_nx(void)

{
    #ifdef NXDESKTOP_XWIN_METHODS_DEBUG
    int i;
    
    nxdesktopDebug("ui_init","Entered NX transport init process.\n");
    #endif
    
    BOOL use_nx_display;
    
    nxdesktopUseNXTrans = NXTransRunning(NX_FD_ANY);
    
    if (nxDisplay[0] != 0)
    {
	use_nx_display = (strncasecmp(nxDisplay, "nx", 2) == 0);
    }
    else
    {	
	use_nx_display = (strncasecmp(XDisplayName(NULL), "nx", 2) == 0);
    }
    
    if (use_nx_display)
    {
	unsigned int enableClient = 0;
	unsigned int enableServer = 1;

	unsigned int clientSegment, serverSegment;
	unsigned int clientSize, serverSize;

	#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
	nxdesktopDebug("ui_init","NX transport avaliable.\n");
	#endif

        NXGetControlParameters(g_display, &nxdesktopLinkType,
				&nxdesktopProtocolLocalMajor, &nxdesktopProtocolLocalMinor,
				&nxdesktopProtocolLocalPatch, &nxdesktopProtocolRemoteMajor,
				&nxdesktopProtocolRemoteMinor, &nxdesktopProtocolRemotePatch,
				&nxdesktopFrameTimeout, &nxdesktopPingTimeout,
				&nxdesktopSplitMode, &nxdesktopSplitSize,
				&nxdesktopPackMethod, &nxdesktopPackQuality,
                                &nxdesktopDataLevel, &nxdesktopStreamLevel,
                                &nxdesktopDeltaLevel, &nxdesktopLoadCache,
                                &nxdesktopSaveCache, &nxdesktopStartupCache);

	#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
	nxdesktopDebug("ui_init","RDP image cache: %d.\n",rdp_img_cache);
	nxdesktopDebug("ui_init","RDP image compressed cache: %d.\n",rdp_img_cache_nxcompressed);
	#endif
	/*
	 * Activate shared memory PutImages
	 * in the X server proxy. 
	 */
        NXGetShmemParameters(g_display, &enableClient, &enableServer,
                             &clientSegment, &serverSegment,
                             &clientSize, &serverSize);

        if (enableServer == False)
	{
	  info("Not using shared memory support in X server.\n");
	}
	else
	{
	  info("Using shared memory support in X server.\n");
	}

        /*
         * Check if we need to use backward
         * compatible requests.
         */

        if (nxdesktopProtocolRemoteMajor < 2)
        {
            warning("Using backward compatible colormap requests.\n");

            nxdesktopColormapCompat = 1;
        }
        else
        {
            warning("Using new colormap requests.\n");

            nxdesktopColormapCompat = 0;
        }


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
	    
	    #ifdef NXDESKTOP_XWIN_METHODS_DEBUG
	    for (i = 0; i <= NXNumberOfPackMethods; i++)
    	    {
		if (methods[i])
		    nxdesktopDebug ("ui_init", "Registered pack method %d.\n", i);
    	    }
	    #endif

	    /* COMPRESS methods */
	    if ((methods[PACK_RDP_COMPRESSED_16M_COLORS] == True) && (g_server_depth > 16))
    	    {
		#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_COMPRESSED_16M_COLORS.\n");
		#endif
		PACK_RDP_COMPRESSED = PACK_RDP_COMPRESSED_16M_COLORS;
		nxdesktopUseNXCompressedRdpImages = True;
    	    }
	    else
    	    if ((methods[PACK_RDP_COMPRESSED_64K_COLORS] == True) && (g_server_depth > 15))
    	    {
		#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_COMPRESSED_64K_COLORS.\n");
		#endif
		PACK_RDP_COMPRESSED = PACK_RDP_COMPRESSED_64K_COLORS;
		nxdesktopUseNXCompressedRdpImages = True;
    	    }
	    else
	    if ((methods[PACK_RDP_COMPRESSED_32K_COLORS] == True) && (g_server_depth > 8))
    	    {
		#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_COMPRESSED_32K_COLORS.\n");
		#endif
		PACK_RDP_COMPRESSED = PACK_RDP_COMPRESSED_32K_COLORS;
		nxdesktopUseNXCompressedRdpImages = True;
    	    }
	    else 
	    if (methods[PACK_RDP_COMPRESSED_256_COLORS] == True)
    	    {
		#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_COMPRESSED_256_COLORS.\n");
		#endif
		PACK_RDP_COMPRESSED = PACK_RDP_COMPRESSED_256_COLORS;
		nxdesktopUseNXCompressedRdpImages = True;
    	    }
	    
	    /* PLAIN methods */
	    if ((methods[PACK_RDP_PLAIN_16M_COLORS] == True) && (g_server_depth > 16))
    	    {
		#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_PLAIN_16M_COLORS.\n");
		#endif
		PACK_RDP_PLAIN = PACK_RDP_PLAIN_16M_COLORS;
		nxdesktopUseNXRdpImages = True;
    	    }
	    else
    	    if ((methods[PACK_RDP_PLAIN_64K_COLORS] == True) && (g_server_depth > 15))
    	    {
		#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_PLAIN_64K_COLORS.\n");
		#endif
		PACK_RDP_PLAIN = PACK_RDP_PLAIN_64K_COLORS;
		nxdesktopUseNXRdpImages = True;
    	    }
	    else
    	    if ((methods[PACK_RDP_PLAIN_32K_COLORS] == True) && (g_server_depth > 8))
    	    {
		#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_PLAIN_32K_COLORS.\n");
		#endif
		PACK_RDP_PLAIN = PACK_RDP_PLAIN_32K_COLORS;
		nxdesktopUseNXRdpImages = True;
    	    }
	    else 
	    if (methods[PACK_RDP_PLAIN_256_COLORS] == True)
    	    {
		#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
		nxdesktopDebug ("ui_init", "Using pack method PACK_RDP_PLAIN_256_COLORS.\n");
		#endif
		PACK_RDP_PLAIN = PACK_RDP_PLAIN_256_COLORS;
		nxdesktopUseNXRdpImages = True;
    	    }
	    else
    	    {
		warning ("No available RDP pack method on g_display '%s'.\n", XDisplayName ((char *) nxDisplay));
		rdp_img_cache_nxcompressed = False;
    	    }

            /*
             * Check if the remote supports decompression
             * of colormap data sent in compressed form.
             */

            if (methods[PACK_COLORMAP] == True)
            {
		warning("Using colormap data in compressed form.\n");

                nxdesktopColormapPacked = 1;
            }
            else
            {
		warning("Not using colormap data in compressed form.\n");

                nxdesktopColormapPacked = 0;
            }
	}
	else
	{
	    warning ("Compressed cache disabled.\n");
	    rdp_img_cache_nxcompressed = False;
	}

	/*
        * Inform remote proxy about pixel geometry
        * to be used to unpack images.
        */

	if (nxdesktopUseNXCompressedRdpImages || nxdesktopUseNXRdpImages)
	{
	    if (NXSetUnpackGeometry(g_display, 0, g_visual) == 0)
	    {
		error("NXSetUnpackGeometry() failed on g_display '%s'.\n", XDisplayName((char *)nxDisplay));
		return False;
	    }
	}
	NXInitDisplay(g_display);
	NXSetDisplayFlushHandler(nxdesktopDisplayFlushHandler, g_display);
	NXSetDisplayCongestionHandler(nxdesktopDisplayCongestionHandler, g_display);
	
    }
    else
    {
	#ifdef NXDESKTOP_XWIN_METHODS_DEBUG
	nxdesktopDebug ("ui_init", "NX transport unavaliable.\n");
	#endif
	rdp_img_cache_nxcompressed = False;
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

	while (g_seamless_windows)
	{
		XDestroyWindow(g_display, g_seamless_windows->wnd);
		sw_remove_window(g_seamless_windows);
	}

	xclip_deinit();
	
	if (g_IM != NULL)
		XCloseIM(g_IM);

	if (g_null_cursor != NULL)
		ui_destroy_cursor(g_null_cursor);

	XFreeModifiermap(g_mod_map);

	if (g_ownbackstore)
		XFreePixmap(g_display, g_backstore);

	XFreeGC(g_display, g_gc);
	XCloseDisplay(g_display);
	g_display = NULL;
}

static void
get_window_attribs(XSetWindowAttributes * attribs)
{
	attribs->background_pixel = BlackPixelOfScreen(g_screen);
	attribs->background_pixel = WhitePixelOfScreen(g_screen);
	attribs->border_pixel = WhitePixelOfScreen(g_screen);
	attribs->backing_store = g_ownbackstore ? NotUseful : Always;
	attribs->override_redirect = g_fullscreen;
	attribs->colormap = g_xcolmap;
}

static void
get_input_mask(long *input_mask)
{
	*input_mask = KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask |
		VisibilityChangeMask | FocusChangeMask | StructureNotifyMask;

	if (g_sendmotion)
		*input_mask |= PointerMotionMask;
	if (g_ownbackstore)
		*input_mask |= ExposureMask;
	if (g_fullscreen || g_grab_keyboard)
		*input_mask |= EnterWindowMask;
	if (g_grab_keyboard)
		*input_mask |= LeaveWindowMask;
}

void nxdesktopSetColormap(char *data, unsigned int entries)
{
        /*
         * Check if we are connected to a newer proxy
         * version and so can send the colormap data
         * in compressed form.
         */

        #ifdef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
        nxdesktopDebug("nxdesktopSetColormap", "Entries are [%u].\n", entries);
        #endif

        if (nxdesktopColormapCompat == 0)
        {
            unsigned int data_size = entries << 2;

            unsigned int packed_size;
            char *packed_data;

            if (nxdesktopColormapPacked == 1)
            {
                #ifdef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
                nxdesktopDebug("nxdesktopSetColormap", "Encoding colormap data.\n");
                #endif

                packed_data = NXEncodeColormap(data, data_size, &packed_size);

                #ifdef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
                nxdesktopDebug("nxdesktopSetColormap", "Input size was [%u] output size is [%u].\n",
                                   data_size, packed_size);
                #endif
            }
            else
            {
                packed_data = NULL;
            }

            if (packed_data != NULL)
            {
                #ifdef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
                nxdesktopDebug("nxdesktopSetColormap", "Sending packed data.\n");
                #endif

                NXSetUnpackColormap(g_display, 0, PACK_COLORMAP, entries, packed_data, packed_size);

                Xfree(packed_data);
            }
            else
            {
                #ifdef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
                nxdesktopDebug("nxdesktopSetColormap", "Sending plain data.\n");
                #endif

                NXSetUnpackColormap(g_display, 0, NO_PACK, entries, data, data_size);
            }
        }
        else
        {
            #ifdef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
            nxdesktopDebug("nxdesktopSetColormap", "Sending backward compatible colormap data.\n");
            #endif

            NXSetUnpackColormapCompat(g_display, 0, entries, data);
        }
}

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
	nxdesktopDebug("nxdesktopSetAtoms","Created intern atom [%s] with id [%ld].\n",
		nxdesktopAtomNames[i], nxdesktopAtoms[i]);
    }
    #endif
    
    identity = nxdesktopAtoms[0];
    XChangeProperty(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, identity, XA_ATOM, sizeof(int) * 8, PropModeReplace, (unsigned char *) &type,  4);
    
    XSetSelectionOwner(g_display, nxdesktopAtoms[5], g_viewport_wnd ? g_viewport_wnd : g_wnd, CurrentTime);
    
    XSetWMProtocols(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, &nxdesktopAtoms[2], 1);
}


BOOL
ui_create_window(BOOL ToggleFullscreen)
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
           NXSetExposeParameters(g_display, True, False, False);

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
	if (g_fullscreen)
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
		if (ToggleFullscreen)
		{
		    xo = perm_xo;
		    yo = perm_yo;
		    
		}
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
	{
		g_gc = XCreateGC(g_display, g_wnd, 0, NULL);
		ui_reset_clip();
	}

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
	
	if (((((g_width >= WidthOfScreen(g_screen)*1.05)) || 
		(g_height >= (HeightOfScreen(g_screen)*1.1))) && (nxdesktopUseNXTrans)) || (ToggleFullscreen))
	{
	    viewport_mode_locked = True;
	    viewport_keys_enabled = True;
	}
	
	/* NX */
	
        if ((!g_embed_wnd) && (viewport_mode))
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
		if (!ToggleFullscreen)
		{
		    xo = g_saved_viewport_x;
        	    yo = g_saved_viewport_y;
		}
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
		    
    		    g_viewport_width = wndwidth < (WidthOfScreen(g_screen)*1.05) ? wndwidth : WidthOfScreen(g_screen)*3/4;
    		    g_viewport_height= wndheight< (HeightOfScreen(g_screen)*1.1) ? wndheight: HeightOfScreen(g_screen)*3/4;
		    g_viewport_width = g_viewport_width  & ~3;
		    if (nxclient_is_windows)
		    {
			g_viewport_width = WidthOfScreen(g_screen);
			g_viewport_height = HeightOfScreen(g_screen);
		    }
		}
    	    }
	    
	    attribs.override_redirect = g_fullscreen;
	    
	    #ifdef NXDESKTOP_DEBUG_VIEWPORT
	    fprintf(stderr, "Geometry data: dw = %d dh = %d ws = %d hs = %d gw = %d gh = %d xo = %d yo = %d\n", DisplayWidth(g_display, DefaultScreen(g_display)), DisplayHeight(g_display, DefaultScreen(g_display)), 
	    	    WidthOfScreen(g_screen), HeightOfScreen(g_screen), g_width, g_height, xo, yo);
	    #endif
            
	    if (viewport_mode)
	    {
		if ((xo == 0) && (yo == 0) && (!nxclient_is_windows))
		{
		    xo = (WidthOfScreen(g_screen) - g_viewport_width)/2;
        	    yo = (HeightOfScreen(g_screen) - g_viewport_height)/2;
		}
		g_viewport_wnd = XCreateWindow (g_display, RootWindowOfScreen(g_screen), xo, yo, g_viewport_width, g_viewport_height, 
						0, g_depth, InputOutput, g_visual, (nxclient_is_windows ? 0 : CWBackPixel) | CWBackingStore | CWOverrideRedirect | CWColormap | CWBorderPixel, &attribs);
						
    		XSelectInput(g_display, g_viewport_wnd, ButtonPressMask | KeyPressMask |
                		StructureNotifyMask | (g_fullscreen ? EnterWindowMask : 0) |
                    		    (g_grab_keyboard ? (EnterWindowMask | LeaveWindowMask) : 0));
				    
		/*XReparentWindow (g_display, g_wnd, g_viewport_wnd, (ToggleFullscreen && g_fullscreen) ? perm_xo : viewport_x1, (ToggleFullscreen && g_fullscreen) ? perm_yo : viewport_y1);*/
		XReparentWindow (g_display, g_wnd, g_viewport_wnd, (ToggleFullscreen && g_fullscreen) ? perm_xo : 0, (ToggleFullscreen && g_fullscreen) ? perm_yo : 0);
	    }
	}

	/* Check XStoreName consistency */
		
	XStoreName(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd, g_title);

	if (g_hide_decorations)
		mwm_hide_decorations(g_wnd);

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
	    
	    /*XSetStandardProperties(g_display, g_viewport_wnd ? g_viewport_wnd : g_wnd,
				(windowName?windowName:"nxdesktop"),
				(windowName?windowName:"nxdesktop"),
				nxIconPixmap, 0, 0, sizehints);*/
				
	    XSetStandardProperties(g_display, g_wnd,
				(windowName?windowName:"nxdesktop"),
				(windowName?windowName:"nxdesktop"),
				nxIconPixmap, 0, 0, sizehints);

	    if (viewport_mode)
	    {
		sizehints->min_width = NXDESKTOP_MIN_WIDTH;
		sizehints->max_width = g_width;
		sizehints->min_height = NXDESKTOP_MIN_HEIGHT;
		sizehints->max_height = g_height;
		XSetStandardProperties(g_display, g_viewport_wnd,
				(windowName?windowName:"nxdesktop"),
				(windowName?windowName:"nxdesktop"),
				nxIconPixmap, 0, 0, sizehints);
	    }
	    
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
	
	if (viewport_mode)
	{
    	    XMapWindow (g_display, g_viewport_wnd);
    	}
	
	XMapWindow(g_display, g_wnd); 
	
	/* wait for VisibilityNotify */ 

	do
	{
		XMaskEvent(g_display, VisibilityChangeMask, &xevent);
	}
	while (xevent.type != VisibilityNotify);
	g_Unobscured = xevent.xvisibility.state == VisibilityUnobscured;

	g_focused = False;
	g_mouse_in_wnd = False;

	/* handle the WM_DELETE_WINDOW protocol */
	g_protocol_atom = XInternAtom(g_display, "WM_PROTOCOLS", True);
	g_kill_atom = XInternAtom(g_display, "WM_DELETE_WINDOW", True);
	XSetWMProtocols(g_display, g_wnd, &g_kill_atom, 1);

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
          
           /*
	    Cursor cursor;
	    cursor = XCreateFontCursor(g_display, 0);
	    XDefineCursor(g_display, g_wnd, (Cursor)cursor);
            */
	  
	    while (i)
	    {
        	nomachineLogo(g_wnd, g_gc, i);
        	i--;
        	sleep(3);
	    }
	    
	    XSync(g_display, True);
        }
	#endif

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
	
	/*
        {
            g_mod_map = XGetModifierMapping(g_display);
            xkeymap_init();
        }
	 NX */
	 
	/*  I don't think this is needed for now 
	    viewport_mode = !ToggleFullscreen;
	*/
	 
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
	#ifdef NXDESKTOP_LOGO
        Bool savedShowNXlogo = showNXlogo;
	#endif
	XSetWindowAttributes attribs;
	
	attribs.background_pixel = BlackPixelOfScreen(g_screen);
	attribs.border_pixel = WhitePixelOfScreen(g_screen);
	attribs.backing_store = g_ownbackstore ? NotUseful : Always;
	attribs.override_redirect = False;
	attribs.colormap = g_xcolmap;

        if (g_seamless_active)
                /* Turn off SeamlessRDP mode */
                ui_seamless_toggle();

	if (!g_ownbackstore)
	{
		/* need to save contents of window */
		contents = XCreatePixmap(g_display, g_wnd, g_width, g_height, g_depth);
		XCopyArea(g_display, g_wnd, contents, g_gc, 0, 0, g_width, g_height, 0, 0);
	}
    
	#ifdef NXDESKTOP_LOGO
	showNXlogo = False;
        #endif
        if (!g_fullscreen)
        {
          g_saved_wnd_x = g_wnd_x;
          g_saved_wnd_y = g_wnd_y;
        }
	
	ui_destroy_window();
	g_fullscreen = !g_fullscreen;
	if (!viewport_mode_locked)
	    viewport_keys_enabled = !g_fullscreen;
	ui_create_window(True);
	#ifdef NXDESKTOP_LOGO
	showNXlogo = savedShowNXlogo;
	#endif
	/* XDefineCursor(g_display, g_wnd, g_current_cursor); */

	if (!g_ownbackstore)
	{
	    XCopyArea(g_display, contents, g_wnd, g_gc, 0, 0, g_width, g_height, 0, 0);
	    XFreePixmap(g_display, contents);
	}
	
}

/* Process events in Xlib queue
   Returns 0 after user quit, 1 otherwise */
static int
xwin_process_events(void)
{
	XEvent xevent;
	KeySym keysym;
	uint16 button, flags;
	key_translation tr;
	uint32 ev_time;
	char str[256];
	Status status;
	int events = 0;
	seamless_window *sw;

	/* NX used only to send RDP inputs for which we can't obtain the real X server time */
	ev_time = (last_Xtime == 0) ? (uint32)time(NULL): last_Xtime;

	while ((XPending(g_display) > 0) && events++ < 20)
	{
		XNextEvent(g_display, &xevent);
		if ((g_IC != NULL) && (XFilterEvent(&xevent, None) == True))
		{
		    #ifdef NXDESKTOP_XWIN_EVENTS_DEBUG
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
			
		#ifdef NXDESKTOP_XWIN_EVENTS_DEBUG
		nxdesktopDebug("xwin_process_events","Xevent type %d.\n",xevent.type);
		#endif
		
		switch (xevent.type)
		{
			case VisibilityNotify:
				if (xevent.xvisibility.window == g_wnd)
					g_Unobscured = xevent.xvisibility.state == VisibilityUnobscured;
				break;
					
			case ClientMessage:
				/* the window manager told us to quit */
				if ((xevent.xclient.message_type == g_protocol_atom)
				    && ((Atom) xevent.xclient.data.l[0] == g_kill_atom))
					/* Quit */
					return 0;
				break;
	
			case KeyPress:
				g_last_gesturetime = xevent.xkey.time;
				if (g_IC != NULL)
					/* Multi_key compatible version */
				{
				    XmbLookupString(g_IC, &xevent.xkey, str, sizeof(str), 
							&keysym, &status);
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
				
				DEBUG_KBD(("KeyPress for keysym (0x%lx, %s)\n", keysym,
					   get_ksname(keysym)));

				ev_time = time(NULL);
				if (handle_special_keys(keysym, xevent.xkey.state, ev_time, True))
					break;

				tr = xkeymap_translate_key(keysym,
							   xevent.xkey.keycode, xevent.xkey.state);

				/*xkeymap_send_keys(keysym, xevent.xkey.keycode, xevent.xkey.state,
						  ev_time, True, 0);*/
				   
				/* NX
				scancode = tr.scancode; */

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

				DEBUG_KBD(("\nKeyRelease for keysym (0x%lx, %s)\n", keysym,
					   get_ksname(keysym)));

				ev_time = time(NULL);
				if (handle_special_keys(keysym, xevent.xkey.state, ev_time, False))
					break;

				tr = xkeymap_translate_key(keysym,
							   xevent.xkey.keycode, xevent.xkey.state);

				/* xkeymap_send_keys(keysym, xevent.xkey.keycode, xevent.xkey.state,
						  ev_time, False, 0); */
			   
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
                                if (((xevent.xbutton.state & (ControlMask | Mod1Mask)) ==
                                        (ControlMask | Mod1Mask) && g_viewport_wnd) && (viewport_keys_enabled))
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

				if (xevent.xmotion.window == g_wnd)
				{
					rdp_send_input(time(NULL), RDP_INPUT_MOUSE, MOUSE_FLAG_MOVE,
						       xevent.xmotion.x, xevent.xmotion.y);
				}
				else
				{
					/* SeamlessRDP */
					rdp_send_input(time(NULL), RDP_INPUT_MOUSE, MOUSE_FLAG_MOVE,
						       xevent.xmotion.x_root,
						       xevent.xmotion.y_root);
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

				sw = sw_get_window_by_wnd(xevent.xfocus.window);
				if (!sw)
					break;

				if (sw->id != g_seamless_focused)
				{
					seamless_send_focus(sw->id, 0);
					g_seamless_focused = sw->id;
				}
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
				/* NX */				

				XUngrabKeyboard(g_display, CurrentTime);
				break;

			case Expose:
				if (xevent.xexpose.window == g_wnd)
				{
					XCopyArea(g_display, g_backstore, xevent.xexpose.window,
						  g_gc,
						  xevent.xexpose.x, xevent.xexpose.y,
						  xevent.xexpose.width, xevent.xexpose.height,
						  xevent.xexpose.x, xevent.xexpose.y);
				}
				else
				{
					sw = sw_get_window_by_wnd(xevent.xexpose.window);
					if (!sw)
						break;
					XCopyArea(g_display, g_backstore,
						  xevent.xexpose.window, g_gc,
						  xevent.xexpose.x + sw->xoffset,
						  xevent.xexpose.y + sw->yoffset,
						  xevent.xexpose.width,
						  xevent.xexpose.height, xevent.xexpose.x,
						  xevent.xexpose.y);
				}

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
                                {
				    xclip_handle_SelectionNotify(&xevent.xselection);
                                }
				break;
			case SelectionRequest:
				if (g_use_rdp5)
				{
                                    xclip_handle_SelectionRequest(&xevent.xselectionrequest);
                                }
				break;
			case SelectionClear:
				if (g_use_rdp5)
				{
                                    xclip_handle_SelectionClear();
                                }
				break;
			case PropertyNotify:
				if (g_use_rdp5)
				{
                                    xclip_handle_PropertyNotify(&xevent.xproperty);
                                } 

				if (xevent.xproperty.window == g_wnd)
					break;
				if (xevent.xproperty.window == DefaultRootWindow(g_display))
					break;

				/* seamless */
				sw = sw_get_window_by_wnd(xevent.xproperty.window);
				if (!sw)
					break;

				if ((xevent.xproperty.atom == g_net_wm_state_atom)
				    && (xevent.xproperty.state == PropertyNewValue))
				{
					sw->state = ewmh_get_window_state(sw->wnd);
					seamless_send_state(sw->id, sw->state, 0);
				}

				if ((xevent.xproperty.atom == g_net_wm_desktop_atom)
				    && (xevent.xproperty.state == PropertyNewValue))
				{
					sw->desktop = ewmh_get_window_desktop(sw->wnd);
					sw_all_to_desktop(sw->wnd, sw->desktop);
				}

				break;
			case MapNotify:
				/* NX */
        	                    if (g_fullscreen)
                	                sigusr_func(SIGUSR2);
	                        /* NX */
				if (!g_seamless_active)
					rdp_send_client_window_status(1);
				break;
                        /*  
			case UnmapNotify:
				if (!g_seamless_active)
					rdp_send_client_window_status(0);
			 */
                        case ConfigureNotify:

                                if ((xevent.xconfigure.window == g_viewport_wnd) && !g_fullscreen)
                                {
                    		    g_saved_viewport_x = xevent.xconfigure.x;
                		    g_saved_viewport_y = xevent.xconfigure.y;
                                    g_viewport_width = g_saved_viewport_width = (xevent.xconfigure.width);
                                    g_viewport_height = g_saved_viewport_height = (xevent.xconfigure.height);
                                    nxdesktopMoveViewport(0, 0);
				    if ((xevent.xconfigure.width < g_width) || (xevent.xconfigure.height < g_height) || (!viewport_mode_locked))
				    {
					viewport_keys_enabled = True;
				    }
				    else
				    {	
					viewport_keys_enabled = False;
				    }
                    		}

				if (!g_seamless_active)
					break;

				sw = sw_get_window_by_wnd(xevent.xconfigure.window);
				if (!sw)
					break;

				gettimeofday(sw->position_timer, NULL);
				if (sw->position_timer->tv_usec + SEAMLESSRDP_POSITION_TIMER >=
				    1000000)
				{
					sw->position_timer->tv_usec +=
						SEAMLESSRDP_POSITION_TIMER - 1000000;
					sw->position_timer->tv_sec += 1;
				}
				else
				{
					sw->position_timer->tv_usec += SEAMLESSRDP_POSITION_TIMER;
				}

				sw_handle_restack(sw);

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
	#ifdef NXDESKTOP_XWIN_EVENTS_DEBUG
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
    if (NXTransRunning(NX_FD_ANY))
    {
        fd_set t_readfds, t_writefds;
        struct timeval t_timeout;
        int n, r, e;
	#ifdef NX_SELECT_DEBUG
	if (exceptfds != NULL)
	{
	    nxdesktopDebug("nxdesktopSelect", "Can't handle exception fds in select. Exiting.\n");
	    nxdesktopExit(1);
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
		#ifdef NXDESKTOP_XWIN_SELECT_DEBUG
		nxdesktopDebug("ui_select","User quit.\n");
		#endif
		return 0;
	    }

           /*
            * Tell the NX transport to write any produced 
            * data to the remote end.
            */

            NXFlushDisplay(g_display, NXFlushLink);

	    if (g_seamless_active)
		sw_check_timers();

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
	    seamless_select_timeout(&tv);
	    n++;

	    Retry:
	    switch (nxdesktopSelect(n, &rfds, &wfds, NULL, &tv))
	    {
		case -1:
		    if (errno != EINTR)
		    {
			error("ui_select: %s\n", strerror(errno));
		    }
		    else
		    {
			#ifdef NXDESKTOP_XWIN_SELECT_DEBUG
			warning("ui_select: EINTR fired!\n");
			#endif
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

	if (g_server_depth == 8)
	{
		bitmap_pad = 8;
	}
	else
	{
		bitmap_pad = g_bpp;

		if (g_bpp == 24)
			bitmap_pad = 32;
	}
	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
	nxdesktopDebug("ui_create_bitmap","dump bitmap compressed: %d , size: %d\n",compressed,size);
	hexdump(data,size);
	#endif
	
	bitmap = XCreatePixmap(g_display, g_wnd, width, height, g_depth);
				 
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
	nxdesktopDebug("ui_create_bitmap","XPutImage on pixmap %d,%d,%d,%d.\n", 0, 0, width, height);
	#endif
	
	#ifdef NXDESKTOP_IMGCACHE_USES_COMPRESSED_IMAGES
	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
	    nxdesktopDebug("ui_create_bitmap","Received bitmap.\n");
	#endif
	
	if (rdp_img_cache_nxcompressed)
	{
	    tdata = data;
	    if (compressed)
	    {
		image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_COMPRESSED,
					    g_depth, ZPixmap, (char *)data, size,
					    width, height, bitmap_pad, 0);
					    
		NXPutPackedImage(g_display, 0, bitmap, g_gc, image, PACK_RDP_COMPRESSED,
				g_depth, 0, 0, 0, 0, width, height);
	    }
	    else
	    {
		image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_PLAIN,
					g_depth, ZPixmap, (char *)data, size,
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
	    #ifdef NXDESKTOP_DEBUG_XPUTIMAGE	    
	    create_bitmap_times++;
	    create_bitmap_total+=image->height*image->bytes_per_line;
	    #endif
	    XFree(image);
	    if (tdata != data)
		xfree(tdata);
	}
	#else
	tdata = (g_owncolmap ? data : translate_image(width, height, data));
	bitmap = XCreatePixmap(g_display, g_wnd, width, height, g_depth);
	image = XCreateImage(g_display, g_visual, g_depth, ZPixmap, 0,
			    (char *) tdata, width, height, bitmap_pad, 0);
	
	XPutImage(g_display, bitmap, g_create_bitmap_gc, image, 0, 0, 0, 0, width, height); 
	#ifdef NXDESKTOP_DEBUG_XPUTIMAGE	    
	create_bitmap_times++;
	create_bitmap_total+=image->height*image->bytes_per_line;
	#endif
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

	if (g_server_depth == 8)
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
		data_length = width * height * g_server_depth;

		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_paint_bitmap","NXCreatePackedImage with g_owncolmap %d and g_depth %d.\n",
				g_owncolmap, g_depth);
		#endif

		image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_PLAIN,
					    g_depth, ZPixmap, (char *)data, data_length,
					    width, height, bitmap_pad, 0);
					    
		if (g_ownbackstore)
		{
		    if ((width > 0) && (height > 0) && (nxdesktopUseRDPFilter))
		    {	
			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
			nxdesktopDebug("ui_paint_bitmap","NXPutPackedImage on backingstore pixmap %d,%d,%d,%d (%d,%d).\n",
					x, y, cx, cy, width, height);
			#endif

			NXPutPackedImage(g_display, 0, g_backstore, g_gc, image, PACK_RDP_PLAIN,
					 g_depth, 0, 0, x, y, cx, cy);

			XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);

			#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
			XFlush(g_display);
			#endif
		    }
		}
		else
		{
		    if ((width > 0) && (height > 0) && (nxdesktopUseRDPFilter))
		    {
			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
			nxdesktopDebug("ui_paint_bitmap","NXPutPackedImage on window %d,%d,%d,%d (%d,%d).\n",
					x, y, cx, cy, width, height);
			#endif

			NXPutPackedImage(g_display, 0, g_wnd, g_gc, image, PACK_RDP_PLAIN,
					 g_depth, 0, 0, x, y, cx, cy);
		    }
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

		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_paint_bitmap","XCreateImage with owncolmap %d and depth %d.\n",
				g_owncolmap, g_depth);
		#endif

		image = XCreateImage(g_display, g_visual, g_depth, ZPixmap,
					0, (char *)tdata, width, height, 8, 0);

		if (g_ownbackstore)
		{
			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
	nxdesktopDebug("ui_paint_bitmap","XCreateImage with owncolmap %d and depth %d.\n",
			g_owncolmap, g_depth);
	#endif
	
	image = XCreateImage(g_display, g_visual, g_depth, ZPixmap, 0,
			     (char *) tdata, width, height, bitmap_pad, 0);

	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_paint_bitmap","XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		
		#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
		paint_bitmap_backstore_nonx_times++;
		paint_bitmap_backstore_nonx_total+=image->height*image->bytes_per_line;
		#endif
		
		XPutImage(g_display, g_backstore, g_gc, image, 0, 0, x, y, cx, cy);
		
		XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
		ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
					(g_display, g_backstore, sw->wnd, g_gc, x, y, cx, cy,
					 x - sw->xoffset, y - sw->yoffset));
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_paint_bitmap","XPutImage on window %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		
		#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
		paint_bitmap_nonx_times++;
		paint_bitmap_nonx_total+=image->height*image->bytes_per_line;
		#endif
		
		XPutImage(g_display, g_wnd, g_gc, image, 0, 0, x, y, cx, cy);
		ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
					(g_display, g_wnd, sw->wnd, g_gc, x, y, cx, cy,
					 x - sw->xoffset, y - sw->yoffset));
	}
	
	XFree(image);
	if (tdata != data)
		xfree(tdata);
	
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
	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
	nxdesktopDebug("ui_paint_compressed_bitmap","NXCreatePackedImage with depth %d and size %d.\n",
			g_depth, compressed_size);
	#endif
	
	image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_COMPRESSED,
				    g_depth, ZPixmap, (char *)compressed_data, compressed_size,
				    width, height, BitmapPad(g_display), 0);
	
	if (image == NULL)
	{
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_paint_compressed_bitmap","NXCreatePackedImage failure.\n");
		#endif

		return;
	}


	if (g_ownbackstore)
	{
	    if ((width > 0) && (height > 0) && (nxdesktopUseRDPFilter))
	    {
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
	}
	else
	{
	    if ((width > 0) && (height > 0) && (nxdesktopUseRDPFilter))
	    {
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_paint_compressed_bitmap","NXPutPackedImage on window %d,%d,%d,%d (%d,%d).\n",
				x, y, cx, cy, width, height);
		#endif

		NXPutPackedImage(g_display, 0, g_wnd, g_gc, image, PACK_RDP_COMPRESSED,
				 g_depth, 0, 0, x, y, cx, cy);
	    }
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
	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
	ON_ALL_SEAMLESS_WINDOWS(XDefineCursor, (g_display, sw->wnd, g_current_cursor));
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

HCOLOURMAP
ui_create_colourmap(COLOURMAP * colours)
{
	COLOURENTRY *entry;
	int i, ncolours = colours->ncolours;
	
	/*#ifdef NKDESKTOP_ONSTART
        if (showNXlogo)
        {
           setOwnerNX_WM(g_wnd);
           XSync(g_display, True);
        }
	#endif*/
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
		
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_create_colourmap","g_owncolmap used for %d numcolors.\n",ncolours);
		#endif
		for (i = 0; i < ncolours; i++)
		{
			entry = &colours->colours[i];
			MAKE_XCOLOR(&xentry[i], entry);
		}
		if (NXAllocColors(g_display, g_xcolmap, ncolours, xentry, alloc_done) == 0)
		{
			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
		else
		{
		    #ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		    nxdesktopDebug("ui_create_colourmap","Colors allocated\n");
		    #endif
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
	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
	nxdesktopDebug("ui_destroy_colourmap","Destroyed colormap at %p.\n",
			map);
	#endif
}

void
ui_set_colourmap(HCOLOURMAP map)
{	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
	{
		XSetWindowColormap(g_display, g_wnd, (Colormap) map);
		ON_ALL_SEAMLESS_WINDOWS(XSetWindowColormap, (g_display, sw->wnd, (Colormap) map));
	}
		
	#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES
	
	/* NX */
	if (nxdesktopUseNXTrans)
	{
	    if (g_owncolmap == 0 && last_colormap_entries != 0 && last_colormap != NULL)
	    {
		int i;

        	if (g_host_be != g_xserver_be && (nx_depth == 16 ||
			nx_depth == 15 || nx_depth == 24 || nx_depth == 32))
		{
		    unsigned int *swap_colormap = xmalloc(sizeof(unsigned int) * last_colormap_entries);

		    for (i = 0; i < last_colormap_entries; i++)
		    {
            		swap_colormap[i] = last_colormap[i];
			BSWAP32(swap_colormap[i]);
            	    }
		    nxdesktopSetColormap((char *) swap_colormap, last_colormap_entries);
            	    xfree(swap_colormap);
        	}
    		else
        	{
		    nxdesktopSetColormap((char *) last_colormap, last_colormap_entries);
        	}

		#ifdef NXDESKTOP_XWIN_GRAPHICS_DUMP_DEBUG
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
	g_clip_rectangle.x = x;
	g_clip_rectangle.y = y;
	g_clip_rectangle.width = cx;
	g_clip_rectangle.height = cy;
	XSetClipRectangles(g_display, g_gc, 0, 0, &g_clip_rectangle, 1, YXBanded);
}

void
ui_reset_clip(void)
{
	g_clip_rectangle.x = 0;
	g_clip_rectangle.y = 0;
	g_clip_rectangle.width = g_width;
	g_clip_rectangle.height = g_height;
	XSetClipRectangles(g_display, g_gc, 0, 0, &g_clip_rectangle, 1, YXBanded);
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
			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
			nxdesktopDebug("ui_patblt","brush-style 0 - Solid.\n");
			#endif
			SET_FOREGROUND(fgcolour);
			FILL_RECTANGLE_BACKSTORE(x, y, cx, cy);
			break;
		
		case 2:	/* Hatch */
			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
			break;

		case 3:	/* Pattern */
			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
			break;

		default:
			unimpl("ui_patblt","brush %d\n", brush->style);
	}

	RESET_FUNCTION(opcode);

	if (g_ownbackstore)
		XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
	ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
				(g_display, g_ownbackstore ? g_backstore : g_wnd, sw->wnd, g_gc,
				 x, y, cx, cy, x - sw->xoffset, y - sw->yoffset));
 
}

void
ui_screenblt(uint8 opcode,
	     /* dest */ int x, int y, int cx, int cy,
	     /* src */ int srcx, int srcy)
{
    #ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
    nxdesktopDebug("ui_screenblt","Opcode=%d, Backstore=%d, x=%d, y=%d, cx=%d, cy=%d, srcx=%d, scry=%d\n",
		    opcode, g_ownbackstore, x, y, cx, cy, srcx, srcy);
    #endif
	SET_FUNCTION(opcode);
	if (g_ownbackstore)
	{
		XCopyArea(g_display, g_Unobscured ? g_wnd : g_backstore,
			  g_wnd, g_gc, srcx, srcy, cx, cy, x, y);

		XCopyArea(g_display, g_backstore, g_backstore, g_gc, srcx, srcy, cx, cy, x, y);
	}
	else
	{
		XCopyArea(g_display, g_wnd, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
		ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
					(g_display, g_ownbackstore ? g_backstore : g_wnd,
					 sw->wnd, g_gc, x, y, cx, cy, x - sw->xoffset, y - sw->yoffset));

	}
	RESET_FUNCTION(opcode);
}

void
ui_memblt(uint8 opcode,
	  /* dest */ int x, int y, int cx, int cy,
	  /* src */ HBITMAP src, int srcx, int srcy)
{
    #ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
    nxdesktopDebug("ui_memblt","Opcode=%d, Backstore=%d, x=%d, y=%d, cx=%d, cy=%d, srcx=%d, scry=%d\n",opcode, g_ownbackstore, x, y, cx, cy, srcx, srcy);
    #endif
    
    SET_FUNCTION(opcode);
    XCopyArea(g_display, (Pixmap) src, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
    ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
				(g_display, (Pixmap) src, sw->wnd, g_gc,
				 srcx, srcy, cx, cy, x - sw->xoffset, y - sw->yoffset));
    if (g_ownbackstore)
	XCopyArea(g_display, (Pixmap) src, g_backstore, g_gc, srcx, srcy, cx, cy, x, y);
    
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
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
    #ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
    nxdesktopDebug("ui_line","Opcode=%d, startx=%d, starty=%d, endx=%d, endy=%d.\n",opcode,startx,starty,endx,endy);
    #endif
	SET_FUNCTION(opcode);
	SET_FOREGROUND(pen->colour);
	XDrawLine(g_display, g_wnd, g_gc, startx, starty, endx, endy);
	ON_ALL_SEAMLESS_WINDOWS(XDrawLine, (g_display, sw->wnd, g_gc,
					    startx - sw->xoffset, starty - sw->yoffset,
					    endx - sw->xoffset, endy - sw->yoffset));
	if (g_ownbackstore)
		XDrawLine(g_display, g_backstore, g_gc, startx, starty, endx, endy);
	RESET_FUNCTION(opcode);
}

/* NX */
void
ui_poly_line(uint8 opcode, short *points, int count,
	/* pen */ PEN *pen)
{
    #ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
    FILL_RECTANGLE(x, y, cx, cy);
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

	ON_ALL_SEAMLESS_WINDOWS(seamless_XDrawLines,
				(sw->wnd, (XPoint *) points, npoints, sw->xoffset, sw->yoffset));

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
	
	XSetFillStyle(g_display, g_gc, FillSolid);
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
ui_draw_text(uint8 font, uint8 flags, uint8 opcode, int mixmode, int x, int y,
	     int clipx, int clipy, int clipcx, int clipcy,
	     int boxx, int boxy, int boxcx, int boxcy, BRUSH * brush,
	     int bgcolour, int fgcolour, uint8 * text, uint8 length)
{
	/* TODO: use brush appropriately */

	FONTGLYPH *glyph;
	int i, j, xyoffset, x1, y1;
	DATABLOB *entry;
	
	/* NX */
	xNXRDPGlyph rdp_text[1024];
	NXPackedImage *image;
	int elements = 0;
	/* NX */
	
	SET_FOREGROUND(bgcolour);

	/* Sometimes, the boxcx value is something really large, like
	   32691. This makes XCopyArea fail with Xvnc. The code below
	   is a quick fix. */
	if (boxx + boxcx > g_width)
		boxcx = g_width - boxx;

	if (boxcx > 1)
	{
		FILL_RECTANGLE_BACKSTORE(boxx, boxy, boxcx, boxcy);
	}
	else if (mixmode == MIX_OPAQUE)
	{
		FILL_RECTANGLE_BACKSTORE(clipx, clipy, clipcx, clipcy);
		
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
				/* At least two bytes needs to follow */
				if (i + 3 > length)
				{
					warning("Skipping short 0xff command:");
					for (j = 0; j < length; j++)
						fprintf(stderr, "%02x ", text[j]);
					fprintf(stderr, "\n");
					i = length = 0;
					break;
				}
				cache_put_text(text[i + 1], text, text[i + 2]);
				i += 3;
				length -= i;
				/* this will move pointer from start to first character after FF command */
				text = &(text[i]);
				i = 0;
				break;
			
			case 0xfe:
				/* At least one byte needs to follow */
				if (i + 2 > length)
				{
					warning("Skipping short 0xfe command:");
					for (j = 0; j < length; j++)
						fprintf(stderr, "%02x ", text[j]);
					fprintf(stderr, "\n");
					i = length = 0;
					break;
				}

				entry = cache_get_text(text[i + 1]);
				if (entry->data != NULL)
				{
				    if ((((uint8 *) (entry->data))[1] == 0) 
					&& (!(flags & TEXT2_IMPLICIT_X)) && (i + 2 < length))
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
						#ifdef NXDESKTOP_XWIN_TEXT_DEBUG
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
					#ifdef NXDESKTOP_XWIN_TEXT_DEBUG
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
		    #ifdef NXDESKTOP_XWIN_TEXT_DEBUG
		    fprintf(stderr, "ui_draw_text: Using packed image with drawable [0x%lx] and gc [0x%lx].\n",g_wnd, (long unsigned int)g_gc);
		    #endif
		    if ((image->height > 0) && (image->width > 0) && (nxdesktopUseRDPFilter))
		    {
			NXPutPackedImage(g_display, 0, (g_ownbackstore) ? g_backstore : g_wnd, g_gc, image, PACK_RDP_TEXT, 1, 0, 0, 0, 0, elements, 1);
			NXDestroyPackedImage(image);
		    }
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
		{
			XCopyArea(g_display, g_backstore, g_wnd, g_gc, boxx,
				  boxy, boxcx, boxcy, boxx, boxy);
			ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
						(g_display, g_backstore, sw->wnd, g_gc,
						 boxx, boxy,
						 boxcx, boxcy,
						 boxx - sw->xoffset, boxy - sw->yoffset));
		}
		else
		{
			XCopyArea(g_display, g_backstore, g_wnd, g_gc, clipx,
				  clipy, clipcx, clipcy, clipx, clipy);
			ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
						(g_display, g_backstore, sw->wnd, g_gc,
						 clipx, clipy,
						 clipcx, clipcy, clipx - sw->xoffset,
						 clipy - sw->yoffset));
		}
	}
}

void
ui_desktop_save(uint32 offset, int x, int y, int cx, int cy)
{
	
	/* NX */
	#ifdef NXDESKTOP_XWIN_USES_PIXMAP_CACHE

	int i;

	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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

			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
			nxdesktopDebug("ui_desktop_save","Saved area in pixmap cache [%lx] index [%d].\n",
					cache, i);
			#endif

			if (g_ownbackstore)
			{
				#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
				nxdesktopDebug("ui_desktop_save","XCopyArea from backingstore to pixmap cache %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, g_backstore, cache, g_gc, x, y, cx, cy, 0, 0);
			}
			else
			{
				#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_desktop_save","PANIC! Couldn't find any free pixmap cache slot.\n");
		#endif
	}

	#else /* NXDESKTOP_XWIN_USES_PIXMAP_CACHE */
	
	Pixmap pix;
	XImage *image;
	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
	nxdesktopDebug("ui_desktop_save","Called with offset [%d]. Not using pixmap cache.\n",
			offset);
	#endif
	/* NX */

	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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

	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
				#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
				nxdesktopDebug("ui_desktop_restore","XCopyArea from pixmap cache to backingstore %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, cache, g_backstore, g_gc, 0, 0, cx, cy, x, y);
				XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
			}
			else
			{
				#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
				nxdesktopDebug("ui_desktop_restore","XCopyArea from pixmap cache to window %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, cache, g_wnd, g_gc, 0, 0, cx, cy, x, y);
			}

			#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_desktop_restore","PANIC! Couldn't find pixmap cache slot.\n");
		#endif
	}


	#else /* NXDESKTOP_XWIN_USES_PIXMAP_CACHE */
	/* NX */
	
	XImage *image;
	uint8 *data;
	
	#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
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
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_desktop_restore","XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		
		#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
		desktop_restore_backstore_nonx_times++;
		desktop_restore_backstore_nonx_total+=image->height*image->bytes_per_line;
		#endif
				
		XPutImage(g_display, g_backstore, g_gc, image, 0, 0, x, y, cx, cy);
		XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
		ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
					(g_display, g_backstore, sw->wnd, g_gc,
					 x, y, cx, cy, x - sw->xoffset, y - sw->yoffset));
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_GRAPHICS_DEBUG
		nxdesktopDebug("ui_desktop_restore","XPutImage on window %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif
		
		#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
		desktop_restore_backstore_times++;
		desktop_restore_backstore_total+=image->height*image->bytes_per_line;
		#endif
		
		XPutImage(g_display, g_wnd, g_gc, image, 0, 0, x, y, cx, cy);
		ON_ALL_SEAMLESS_WINDOWS(XCopyArea,
					(g_display, g_wnd, sw->wnd, g_gc, x, y, cx, cy,
					 x - sw->xoffset, y - sw->yoffset));
	}

	XFree(image);
	
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


    XFlush(g_display);
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
  #ifdef NXDESKTOP_LOGO
  showNXlogo = False;
  #endif
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


int
nxdesktopDialog(int type, int code)
{
    int real_code = -1;
    char *error_msg = 0;
    char error_caption[512];
    
    #ifdef NXDESKTOP_DIALOG_DEBUG
    nxdesktopDebug("nxdesktopDialog", "Message type = %d, message code = %d\n", type, code);
    #endif
    
    switch (type)
    {
	case TCP_MESSAGE:
	    switch (code)
	    {
		case EADDRINUSE:
		case EBUSY:
		case ENOTCONN:
		case ETIMEDOUT:
		case ECONNREFUSED:
		    real_code = REMOTE_SERVER_RDP_REFUSED_ALERT;
		    error_msg = REMOTE_SERVER_RDP_REFUSED_STRING;
		    break;
		    
		case ENETDOWN:
		case ENETUNREACH:
		case ENETRESET:
		case ECONNRESET:
		case EHOSTDOWN:
		    real_code = REMOTE_SERVER_RDP_SHUTDOWN_ALERT;
		    error_msg = REMOTE_SERVER_RDP_SHUTDOWN_STRING;
		    break;
		
		case REMOTE_SERVER_RDP_NOT_FOUND_ALERT:
		    real_code = REMOTE_SERVER_RDP_NOT_FOUND_ALERT;
		    error_msg = REMOTE_SERVER_RDP_NOT_FOUND_STRING;
		    break;
		
		default:
		    real_code = REMOTE_SERVER_RDP_CONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_CONNECT_STRING;
		    break;
	    }
	    break;
	case RDP_MESSAGE:
	    switch (code)
	    {
		case exDiscReasonNoInfo:
		    real_code = REMOTE_SERVER_RDP_TERMINATED_ALERT;
		    error_msg = REMOTE_SERVER_RDP_TERMINATED_STRING;
		    break;

		case exDiscReasonAPIInitiatedDisconnect:
		    real_code = REMOTE_SERVER_RDP_MANDATED_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_MANDATED_DISCONNECT_STRING;
		    break;

		case exDiscReasonAPIInitiatedLogoff:
		    real_code = REMOTE_SERVER_RDP_LOGOFF_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_LOGOFF_DISCONNECT_STRING;
		    break;

		case exDiscReasonServerIdleTimeout:
		    real_code = REMOTE_SERVER_RDP_IDLE_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_IDLE_DISCONNECT_STRING;	
		    break;

		case exDiscReasonServerLogonTimeout:
		    real_code = REMOTE_SERVER_RDP_LOGON_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_LOGON_DISCONNECT_STRING;
		    break;

		case exDiscReasonReplacedByOtherConnection:
		    real_code = REMOTE_SERVER_RDP_REPLACED_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_REPLACED_DISCONNECT_STRING;
		    break;

		case exDiscReasonOutOfMemory:
		    real_code = REMOTE_SERVER_RDP_MEMORY_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_MEMORY_DISCONNECT_STRING;
		    break;

		case exDiscReasonServerDeniedConnection:
		    real_code = REMOTE_SERVER_RDP_DENIED_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_DENIED_DISCONNECT_STRING;
		    break;

		case exDiscReasonServerDeniedConnectionFips:
		    real_code = REMOTE_SERVER_RDP_SECURITY_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_SECURITY_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseInternal:
		    real_code = REMOTE_SERVER_RDP_FORBIDDEN_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_FORBIDDEN_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseNoLicenseServer:
		    real_code = REMOTE_SERVER_RDP_NETWORK_LICENSE_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_NETWORK_LICENSE_DISCONNECT_STRING;
		    break;    
		
		case exDiscReasonLicenseNoLicense:
		    real_code = REMOTE_SERVER_RDP_NO_LICENSE_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_NO_LICENSE_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseErrClientMsg:
		    real_code = REMOTE_SERVER_RDP_LICENSE_MESSAGE_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_LICENSE_MESSAGE_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseHwidDoesntMatchLicense:
		    real_code = REMOTE_SERVER_RDP_LICENSE_ERROR_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_LICENSE_ERROR_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseErrClientLicense:
		    real_code = REMOTE_SERVER_RDP_INVALID_LICENSE_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_INVALID_LICENSE_DISCONNECT_STRING;
		    break;
		    
		case exDiscReasonLicenseCantFinishProtocol:
		    real_code = REMOTE_SERVER_RDP_LICENSE_ABORT_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_LICENSE_ABORT_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseClientEndedProtocol:
		    real_code = REMOTE_SERVER_RDP_LICENSE_PROTO_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_LICENSE_PROTO_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseErrClientEncryption:
		    real_code = REMOTE_SERVER_RDP_LICENSE_FORMAT_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_LICENSE_FORMAT_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseCantUpgradeLicense:
		    real_code = REMOTE_SERVER_RDP_LICENSE_UPGRADE_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_LICENSE_UPGRADE_DISCONNECT_STRING;
		    break;

		case exDiscReasonLicenseNoRemoteConnections:
		    real_code = REMOTE_SERVER_RDP_FORBIDDEN_DISCONNECT_ALERT;
		    error_msg = REMOTE_SERVER_RDP_FORBIDDEN_DISCONNECT_STRING;
		    break;
		
		case REMOTE_SERVER_RDP_GEOMETRY_LIMIT_ALERT:
		    real_code = code;
		    error_msg = REMOTE_SERVER_RDP_GEOMETRY_LIMIT_STRING;
		    break;
		    
		case REMOTE_SERVER_RDP_COLOR_LIMIT_ALERT:
		    real_code = code;
		    error_msg = REMOTE_SERVER_RDP_COLOR_LIMIT_STRING;
		    break;
		    
		case REMOTE_SERVER_RDP_AUTHENTICATION_ALERT:
		    real_code = code;
		    error_msg = REMOTE_SERVER_RDP_AUTHENTICATION_STRING;
		    break;

		default:
		    if (code > 0x1000 && code < 0x7fff)
		    {
			real_code = REMOTE_SERVER_RDP_PROTOCOL_DISCONNECT_ALERT;
			error_msg = REMOTE_SERVER_RDP_PROTOCOL_DISCONNECT_STRING;
		    } else
		    {
			real_code = REMOTE_SERVER_RDP_LICENSE_SYSTEM_DISCONNECT_ALERT;
			error_msg = REMOTE_SERVER_RDP_LICENSE_SYSTEM_DISCONNECT_STRING;
		    }
		    break;
	    }
	    break;
    }
    #ifdef NXDESKTOP_DIALOG_DEBUG
    nxdesktopDebug("nxdesktopDialog", "real code = %d", real_code);
    #endif
    if (nxDisplay[0] != 0)
    {
	if (NXTransRunning(NX_FD_ANY))
	{
	    return (NXTransAlert(real_code, NX_ALERT_REMOTE));
	} else 
	{
	    if (error_caption[0])
		snprintf(error_caption,511,"Error");
	    NXTransDialog(error_caption, error_msg, (char *)g_wnd, "ok", 0, (char *)nxDisplay );
	    wait(NULL);
	    return 0;
	}
    }
    return 0;
}		

/* these do nothing here but are used in uiports */
void
ui_begin_update(void)
{
}

void
ui_end_update(void)
{
}

void
ui_seamless_begin(BOOL hidden)
{
	if (!g_seamless_rdp)
		return;

	if (g_seamless_started)
		return;

	g_seamless_started = True;
	g_seamless_hidden = hidden;

	if (!hidden)
		ui_seamless_toggle();
}


void
ui_seamless_hide_desktop()
{
	if (!g_seamless_rdp)
		return;

	if (!g_seamless_started)
		return;

	if (g_seamless_active)
		ui_seamless_toggle();

	g_seamless_hidden = True;
}


void
ui_seamless_unhide_desktop()
{
	if (!g_seamless_rdp)
		return;

	if (!g_seamless_started)
		return;

	g_seamless_hidden = False;

	ui_seamless_toggle();
}


void
ui_seamless_toggle()
{
	if (!g_seamless_rdp)
		return;

	if (!g_seamless_started)
		return;

	if (g_seamless_hidden)
		return;

	if (g_seamless_active)
	{
		/* Deactivate */
		while (g_seamless_windows)
		{
			XDestroyWindow(g_display, g_seamless_windows->wnd);
			sw_remove_window(g_seamless_windows);
		}
		XMapWindow(g_display, g_wnd);
	}
	else
	{
		/* Activate */
		XUnmapWindow(g_display, g_wnd);
		seamless_send_sync();
	}

	g_seamless_active = !g_seamless_active;
}


void
ui_seamless_create_window(unsigned long id, unsigned long group, unsigned long parent,
			  unsigned long flags)
{
	Window wnd;
	XSetWindowAttributes attribs;
	XClassHint *classhints;
	XSizeHints *sizehints;
	XWMHints *wmhints;
	long input_mask;
	seamless_window *sw, *sw_parent;

	if (!g_seamless_active)
		return;

	/* Ignore CREATEs for existing windows */
	sw = sw_get_window_by_id(id);
	if (sw)
		return;

	get_window_attribs(&attribs);
	wnd = XCreateWindow(g_display, RootWindowOfScreen(g_screen), -1, -1, 1, 1, 0, g_depth,
			    InputOutput, g_visual,
			    CWBackPixel | CWBackingStore | CWColormap | CWBorderPixel, &attribs);

	XStoreName(g_display, wnd, "SeamlessRDP");
	ewmh_set_wm_name(wnd, "SeamlessRDP");

	mwm_hide_decorations(wnd);

	classhints = XAllocClassHint();
	if (classhints != NULL)
	{
		classhints->res_name = "rdesktop";
		classhints->res_class = "SeamlessRDP";
		XSetClassHint(g_display, wnd, classhints);
		XFree(classhints);
	}

	/* WM_NORMAL_HINTS */
	sizehints = XAllocSizeHints();
	if (sizehints != NULL)
	{
		sizehints->flags = USPosition;
		XSetWMNormalHints(g_display, wnd, sizehints);
		XFree(sizehints);
	}

	/* Parent-less transient windows */
	if (parent == 0xFFFFFFFF)
	{
		XSetTransientForHint(g_display, wnd, RootWindowOfScreen(g_screen));
		/* Some buggy wm:s (kwin) do not handle the above, so fake it
		   using some other hints. */
		ewmh_set_window_popup(wnd);
	}
	/* Normal transient windows */
	else if (parent != 0x00000000)
	{
		sw_parent = sw_get_window_by_id(parent);
		if (sw_parent)
			XSetTransientForHint(g_display, wnd, sw_parent->wnd);
		else
			warning("ui_seamless_create_window: No parent window 0x%lx\n", parent);
	}

	if (flags & SEAMLESSRDP_CREATE_MODAL)
	{
		/* We do this to support buggy wm:s (*cough* metacity *cough*)
		   somewhat at least */
		if (parent == 0x00000000)
			XSetTransientForHint(g_display, wnd, RootWindowOfScreen(g_screen));
		ewmh_set_window_modal(wnd);
	}

	/* FIXME: Support for Input Context:s */

	get_input_mask(&input_mask);
	input_mask |= PropertyChangeMask;

	XSelectInput(g_display, wnd, input_mask);

	/* handle the WM_DELETE_WINDOW protocol. FIXME: When killing a
	   seamless window, we could try to close the window on the
	   serverside, instead of terminating rdesktop */ 
	XSetWMProtocols(g_display, wnd, &g_kill_atom, 1);

	sw = xmalloc(sizeof(seamless_window));
	sw->wnd = wnd;
	sw->id = id;
	sw->behind = 0;
	sw->group = sw_find_group(group, False);
	sw->group->refcnt++;
	sw->xoffset = 0;
	sw->yoffset = 0;
	sw->width = 0;
	sw->height = 0;
	sw->state = SEAMLESSRDP_NOTYETMAPPED;
	sw->desktop = 0;
	sw->position_timer = xmalloc(sizeof(struct timeval));
	timerclear(sw->position_timer);

	sw->outstanding_position = False;
	sw->outpos_serial = 0;
	sw->outpos_xoffset = sw->outpos_yoffset = 0;
	sw->outpos_width = sw->outpos_height = 0;

	sw->next = g_seamless_windows;
	g_seamless_windows = sw;

	/* WM_HINTS */
	wmhints = XAllocWMHints();
	if (wmhints)
	{
		wmhints->flags = WindowGroupHint;
		wmhints->window_group = sw->group->wnd;
		XSetWMHints(g_display, sw->wnd, wmhints);
		XFree(wmhints);
	}
}


void
ui_seamless_destroy_window(unsigned long id, unsigned long flags)
{
	seamless_window *sw;

	if (!g_seamless_active)
		return;

	sw = sw_get_window_by_id(id);
	if (!sw)
	{
		warning("ui_seamless_destroy_window: No information for window 0x%lx\n", id);
		return;
	}

	XDestroyWindow(g_display, sw->wnd);
	sw_remove_window(sw);
}

void
ui_seamless_destroy_group(unsigned long id, unsigned long flags)
{
	seamless_window *sw, *sw_next;

	if (!g_seamless_active)
		return;

	for (sw = g_seamless_windows; sw; sw = sw_next)
	{
		sw_next = sw->next;

		if (sw->group->id == id)
		{
			XDestroyWindow(g_display, sw->wnd);
			sw_remove_window(sw);
		}
	}
}

void
ui_seamless_move_window(unsigned long id, int x, int y, int width, int height, unsigned long flags)
{
	seamless_window *sw;

	if (!g_seamless_active)
		return;

	sw = sw_get_window_by_id(id);
	if (!sw)
	{
		warning("ui_seamless_move_window: No information for window 0x%lx\n", id);
		return;
	}

	/* We ignore server updates until it has handled our request. */
	if (sw->outstanding_position)
		return;

	if (!width || !height)
		/* X11 windows must be at least 1x1 */
		return;

	sw->xoffset = x;
	sw->yoffset = y;
	sw->width = width;
	sw->height = height;

	/* If we move the window in a maximized state, then KDE won't
	   accept restoration */
	switch (sw->state)
	{
		case SEAMLESSRDP_MINIMIZED:
		case SEAMLESSRDP_MAXIMIZED:
			return;
	}

	/* FIXME: Perhaps use ewmh_net_moveresize_window instead */
	XMoveResizeWindow(g_display, sw->wnd, sw->xoffset, sw->yoffset, sw->width, sw->height);
}


void
ui_seamless_restack_window(unsigned long id, unsigned long behind, unsigned long flags)
{
	seamless_window *sw;

	if (!g_seamless_active)
		return;

	sw = sw_get_window_by_id(id);
	if (!sw)
	{
		warning("ui_seamless_restack_window: No information for window 0x%lx\n", id);
		return;
	}

	if (behind)
	{
		seamless_window *sw_behind;
		Window wnds[2];

		sw_behind = sw_get_window_by_id(behind);
		if (!sw_behind)
		{
			warning("ui_seamless_restack_window: No information for window 0x%lx\n",
				behind);
			return;
		}

		wnds[1] = sw_behind->wnd;
		wnds[0] = sw->wnd;

		XRestackWindows(g_display, wnds, 2);
	}
	else
	{
		XRaiseWindow(g_display, sw->wnd);
	}

	sw_restack_window(sw, behind);
}


void
ui_seamless_settitle(unsigned long id, const char *title, unsigned long flags)
{
	seamless_window *sw;

	if (!g_seamless_active)
		return;

	sw = sw_get_window_by_id(id);
	if (!sw)
	{
		warning("ui_seamless_settitle: No information for window 0x%lx\n", id);
		return;
	}

	/* FIXME: Might want to convert the name for non-EWMH WMs */
	XStoreName(g_display, sw->wnd, title);
	ewmh_set_wm_name(sw->wnd, title);
}


void
ui_seamless_setstate(unsigned long id, unsigned int state, unsigned long flags)
{
	seamless_window *sw;

	if (!g_seamless_active)
		return;

	sw = sw_get_window_by_id(id);
	if (!sw)
	{
		warning("ui_seamless_setstate: No information for window 0x%lx\n", id);
		return;
	}

	switch (state)
	{
		case SEAMLESSRDP_NORMAL:
		case SEAMLESSRDP_MAXIMIZED:
			ewmh_change_state(sw->wnd, state);
			XMapWindow(g_display, sw->wnd);
			break;
		case SEAMLESSRDP_MINIMIZED:
			/* EWMH says: "if an Application asks to toggle _NET_WM_STATE_HIDDEN
			   the Window Manager should probably just ignore the request, since
			   _NET_WM_STATE_HIDDEN is a function of some other aspect of the window
			   such as minimization, rather than an independent state." Besides,
			   XIconifyWindow is easier. */
			if (sw->state == SEAMLESSRDP_NOTYETMAPPED)
			{
				XWMHints *hints;
				hints = XGetWMHints(g_display, sw->wnd);
				if (hints)
				{
					hints->flags |= StateHint;
					hints->initial_state = IconicState;
					XSetWMHints(g_display, sw->wnd, hints);
					XFree(hints);
				}
				XMapWindow(g_display, sw->wnd);
			}
			else
				XIconifyWindow(g_display, sw->wnd, DefaultScreen(g_display));
			break;
		default:
			warning("SeamlessRDP: Invalid state %d\n", state);
			break;
	}

	sw->state = state;
}


void
ui_seamless_syncbegin(unsigned long flags)
{
	if (!g_seamless_active)
		return;

	/* Destroy all seamless windows */
	while (g_seamless_windows)
	{
		XDestroyWindow(g_display, g_seamless_windows->wnd);
		sw_remove_window(g_seamless_windows);
	}
}


void
ui_seamless_ack(unsigned int serial)
{
	seamless_window *sw;
	for (sw = g_seamless_windows; sw; sw = sw->next)
	{
		if (sw->outstanding_position && (sw->outpos_serial == serial))
		{
			sw->xoffset = sw->outpos_xoffset;
			sw->yoffset = sw->outpos_yoffset;
			sw->width = sw->outpos_width;
			sw->height = sw->outpos_height;
			sw->outstanding_position = False;

			/* Do a complete redraw of the window as part of the
			   completion of the move. This is to remove any
			   artifacts caused by our lack of synchronization. */
			XCopyArea(g_display, g_backstore,
				  sw->wnd, g_gc,
				  sw->xoffset, sw->yoffset, sw->width, sw->height, 0, 0);

			break;
		}
	}
}
