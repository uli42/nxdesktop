/* -*- c8-basic-offset: 8 -*-
   rdesktop: A Remote Desktop Protocol client.
   User interface services - X Window System
   Copyright (C) Matthew Chapman 1999-2002

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

#include "version.h"
#include "icon.h"
#include <X11/xpm.h>
#include "X11/Xatom.h"

#undef NXDESKTOP_XWIN_DEBUG

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

#ifdef NXDESKTOP_ONSTART

//Atom nxdesktop_WM_START;

#endif

extern int g_width;
extern int g_height;
extern BOOL g_sendmotion;
extern BOOL g_fullscreen;
extern BOOL g_grab_keyboard;
extern BOOL g_hide_decorations;
extern char g_title[];
extern int g_server_bpp;
extern int g_win_button_size;

extern int xo;
extern int yo;
extern BOOL ipaq;
extern BOOL magickey;
static int x_socket;
extern char windowName[255];

Display *g_display;
Time g_last_gesturetime;
static int g_x_socket;
static Screen *g_screen;
Window g_wnd;
uint32 g_embed_wnd;
static Window wnd2;
BOOL g_enable_compose = False;
static GC g_gc = NULL;
static Visual *g_visual;
static int g_depth;
static int g_bpp;
static XIM g_IM;
static XIC g_IC;
static XModifierKeymap *g_mod_map;
static Cursor g_current_cursor;
static HCURSOR g_null_cursor = NULL;
/* static Atom g_protocol_atom, g_kill_atom; */
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

static BOOL g_focused;
static BOOL g_mouse_in_wnd;
static BOOL g_arch_match = False; /* set to True if RGB XServer and little endian */

static XModifierKeymap *g_mod_map;

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


#ifdef NXWIN_USES_PACKED_RDP_TEXT
BOOL nxdesktopCanPackRDPText = False;
#endif

/* Image cache flag */

extern BOOL rdp_img_cache;

/* endianness */
static BOOL g_host_be;
static BOOL g_xserver_be;
static int g_red_shift_r, g_blue_shift_r, g_green_shift_r;
static int g_red_shift_l, g_blue_shift_l, g_green_shift_l;

/* software backing store */
BOOL g_ownbackstore = False;	/* We can't rely on external BackingStore */
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

/* colour maps */
BOOL g_owncolmap = False;
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

	XChangeProperty(g_display, g_wnd, hintsatom, hintsatom, 32, PropModeReplace,
			(unsigned char *) &motif_hints, PROP_MOTIF_WM_HINTS_ELEMENTS);
}

static PixelColour
split_colour15(uint32 colour)
{
	PixelColour rv;
	rv.red = (colour & 0x7c00) >> 7;
	rv.green = (colour & 0x03e0) >> 2;
	rv.blue = (colour & 0x001f) << 3;
	return rv;
}

static PixelColour
split_colour16(uint32 colour)
{
	PixelColour rv;
	rv.red = (colour & 0xf800) >> 8;
	rv.green = (colour & 0x07e0) >> 3;
	rv.blue = (colour & 0x001f) << 3;
	return rv;
}

static PixelColour
split_colour24(uint32 colour)
{
	PixelColour rv;
	rv.blue = (colour & 0xff0000) >> 16;
	rv.green = (colour & 0x00ff00) >> 8;
	rv.red = (colour & 0x0000ff);
	return rv;
}

static uint32
make_colour(PixelColour pc)
{
	return (((pc.red >> g_red_shift_r) << g_red_shift_l)
		| ((pc.green >> g_green_shift_r) << g_green_shift_l)
		| ((pc.blue >> g_blue_shift_r) << g_blue_shift_l));
}

#define BSWAP16(x) { x = (((x & 0xff) << 8) | (x >> 8)); }
#define BSWAP24(x) { x = (((x & 0xff) << 16) | (x >> 16) | (x & 0xff00)); }
#define BSWAP32(x) { x = (((x & 0xff00ff) << 8) | ((x >> 8) & 0xff00ff)); \
			x = (x << 16) | (x >> 16); }

static uint32
translate_colour(uint32 colour)
{
	PixelColour pc;
	switch (g_server_bpp)
	{
		case 15:
			pc = split_colour15(colour);
			break;
		case 16:
			pc = split_colour16(colour);
			break;
		case 24:
			pc = split_colour24(colour);
			break;
	}
	return make_colour(pc);
}

static void
translate8to8(uint8 * data, uint8 * out, uint8 * end)
{
	while (out < end)
		*(out++) = (uint8) g_colmap[*(data++)];
}

static void
translate8to16(uint8 * data, uint8 * out, uint8 * end)
{
	uint16 value;

	if (g_xserver_be)
	{
		while (out < end)
		{
			value = (uint16) g_colmap[*(data++)];
			*(out++) = value >> 8;
			*(out++) = value;
		}
	}
	else
	{
		while (out < end)
		{
			value = (uint16) g_colmap[*(data++)];
			*(out++) = value;
			*(out++) = value >> 8;
		}
	}
}

/* little endian - conversion happens when colourmap is built */
static void
translate8to24(uint8 * data, uint8 * out, uint8 * end)
{
	uint32 value;

	if (g_xserver_be)
	{
		while (out < end)
		{
			value = g_colmap[*(data++)];
			*(out++) = value >> 16;
			*(out++) = value >> 8;
			*(out++) = value;
		}
	}
	else
	{
		while (out < end)
		{
			value = g_colmap[*(data++)];
			*(out++) = value;
			*(out++) = value >> 8;
			*(out++) = value >> 16;
		}
	}
}

static void
translate8to32(uint8 * data, uint8 * out, uint8 * end)
{
	uint32 value;

	if (g_xserver_be)
	{
		while (out < end)
		{
			value = g_colmap[*(data++)];
			*(out++) = value >> 24;
			*(out++) = value >> 16;
			*(out++) = value >> 8;
			*(out++) = value;
		}
	}
	else
	{
		while (out < end)
		{
			value = g_colmap[*(data++)];
			*(out++) = value;
			*(out++) = value >> 8;
			*(out++) = value >> 16;
			*(out++) = value >> 24;
		}
	}
}

static void
translate15to16(uint16 * data, uint8 * out, uint8 * end)
{
	uint16 pixel;
	uint16 value;

	while (out < end)
	{
		pixel = *(data++);

		if (g_host_be)
		{
			BSWAP16(pixel);
		}

		value = make_colour(split_colour15(pixel));

		if (g_xserver_be)
		{
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
		}
	}
}

static void
translate15to24(uint16 * data, uint8 * out, uint8 * end)
{
	uint32 value;
	uint16 pixel;

	while (out < end)
	{
		pixel = *(data++);

		if (g_host_be)
		{
			BSWAP16(pixel);
		}

		value = make_colour(split_colour15(pixel));
		if (g_xserver_be)
		{
			*(out++) = value >> 16;
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
			*(out++) = value >> 16;
		}
	}
}

static void
translate15to32(uint16 * data, uint8 * out, uint8 * end)
{
	uint16 pixel;
	uint32 value;

	while (out < end)
	{
		pixel = *(data++);

		if (g_host_be)
		{
			BSWAP16(pixel);
		}

		value = make_colour(split_colour15(pixel));

		if (g_xserver_be)
		{
			*(out++) = value >> 24;
			*(out++) = value >> 16;
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
			*(out++) = value >> 16;
			*(out++) = value >> 24;
		}
	}
}

static void
translate16to16(uint16 * data, uint8 * out, uint8 * end)
{
	uint16 pixel;
	uint16 value;

	while (out < end)
	{
		pixel = *(data++);

		if (g_host_be)
		{
			BSWAP16(pixel);
		}

		value = make_colour(split_colour16(pixel));

		if (g_xserver_be)
		{
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
		}
	}
}

static void
translate16to24(uint16 * data, uint8 * out, uint8 * end)
{
	uint32 value;
	uint16 pixel;

	while (out < end)
	{
		pixel = *(data++);

		if (g_host_be)
		{
			BSWAP16(pixel);
		}

		value = make_colour(split_colour16(pixel));

		if (g_xserver_be)
		{
			*(out++) = value >> 16;
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
			*(out++) = value >> 16;
		}
	}
}

static void
translate16to32(uint16 * data, uint8 * out, uint8 * end)
{
	uint16 pixel;
	uint32 value;

	while (out < end)
	{
		pixel = *(data++);

		if (g_host_be)
		{
			BSWAP16(pixel);
		}

		value = make_colour(split_colour16(pixel));

		if (g_xserver_be)
		{
			*(out++) = value >> 24;
			*(out++) = value >> 16;
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
			*(out++) = value >> 16;
			*(out++) = value >> 24;
		}
	}
}

static void
translate24to16(uint8 * data, uint8 * out, uint8 * end)
{
	uint32 pixel = 0;
	uint16 value;
	while (out < end)
	{
		pixel = *(data++) << 16;
		pixel |= *(data++) << 8;
		pixel |= *(data++);

		value = (uint16) make_colour(split_colour24(pixel));

		if (g_xserver_be)
		{
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
		}
	}
}

static void
translate24to24(uint8 * data, uint8 * out, uint8 * end)
{
	uint32 pixel;
	uint32 value;

	while (out < end)
	{
		pixel = *(data++) << 16;
		pixel |= *(data++) << 8;
		pixel |= *(data++);

		value = make_colour(split_colour24(pixel));

		if (g_xserver_be)
		{
			*(out++) = value >> 16;
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
			*(out++) = value >> 16;
		}
	}
}

static void
translate24to32(uint8 * data, uint8 * out, uint8 * end)
{
	uint32 pixel;
	uint32 value;

	while (out < end)
	{
		pixel = *(data++) << 16;
		pixel |= *(data++) << 8;
		pixel |= *(data++);

		value = make_colour(split_colour24(pixel));

		if (g_xserver_be)
		{
			*(out++) = value >> 24;
			*(out++) = value >> 16;
			*(out++) = value >> 8;
			*(out++) = value;
		}
		else
		{
			*(out++) = value;
			*(out++) = value >> 8;
			*(out++) = value >> 16;
			*(out++) = value >> 24;
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
      fprintf(stderr,"XQUERY: %x %x %x\n", X.red, X.blue, X.green);
}

void
sigusr_func (int s)
{
  switch (s)

  {
  case SIGUSR1:
    DEBUG (("Received SIGUSR1, unmapping window.\n"));
    if(ipaq)
    {
       XIconifyWindow (g_display, wnd2, DefaultScreen(g_display));
       XMapWindow(g_display, wnd2);
    }
    XUnmapWindow (g_display, g_wnd);
    break;
  case SIGUSR2:
    DEBUG (("Received SIGUSR2, mapping window.\n"));
    if(ipaq)
    {
       XMapWindow (g_display, g_wnd);
       XUnmapWindow(g_display, wnd2);
    }
    else {
       XMapRaised (g_display, g_wnd);
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

  fprintf(stderr, "Info: X request #%d failed with message '%s'.\n",
              err -> request_code, msg);

  fprintf(stderr, "Info: X sequence was %ld with resource %ld.\n",
              err -> serial & 0xffff, err -> resourceid);

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
   
	x_socket = ConnectionNumber(g_display); /* NX */
	{
          extern void tcp_resize_buf(int, int, int);
          extern int rdp_bufsize;
          tcp_resize_buf(x_socket, 0, rdp_bufsize);
        }
	
	screen_num = DefaultScreen(g_display);
	g_x_socket = ConnectionNumber(g_display);
	g_screen = ScreenOfDisplay(g_display, screen_num);
	g_depth = DefaultDepthOfScreen(g_screen);

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
		
		/* if RGB video and averything is little endian */
		if (vi.red_mask > vi.green_mask && vi.green_mask > vi.blue_mask)
			if (!g_xserver_be && !g_host_be)
				g_arch_match = True;
	}

	if (nxDisplay != NULL)
	{
		nxdesktopUseNXTrans = (strncasecmp(nxDisplay, "nx", 2) == 0);
	}
	else
	{
		nxdesktopUseNXTrans = (strncasecmp(XDisplayName(NULL), "nx", 2) == 0);
	}

        XSetErrorHandler(nxdesktopErrorHandler);

	if (nxdesktopUseNXTrans)
	{

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
		
		if (nxdesktopLinkType == LINK_TYPE_MODEM ||
		    nxdesktopLinkType == LINK_TYPE_ISDN ||
		    nxdesktopLinkType == LINK_TYPE_ADSL)
		{
	    	    rdp_img_cache = True;
		}
		else
		{
		    rdp_img_cache = False;
		} 
				 
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
				fprintf(stderr, "Info: Not using shared memory support in X server.\n");
			}
			else
			{
				fprintf(stderr, "Info: Using shared memory support in X server.\n");
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
			
			if (NXGetUnpackParameters(g_display, &entries, methods) == 0 ||
				entries != NXNumberOfPackMethods)
			{
				fprintf(stderr, "ui_init: ERROR! NXGetUnpackParameters() failed on g_display '%s'.\n",
						XDisplayName((char *)nxDisplay));

				exit(1);
			}
			
#ifdef NXWIN_USES_PACKED_RDP_TEXT
                        nxdesktopCanPackRDPText = methods[PACK_RDP_TEXT];
#endif
			if (methods[PACK_RDP_COMPRESSED_256_COLORS] == True)
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_init: Using pack method PACK_RDP_COMPRESSED_256_COLORS.\n");
				#endif

				nxdesktopUseNXCompressedRdpImages = True;
        	        }    
			
			if (methods[PACK_RDP_PLAIN_256_COLORS] == True)
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_init: Using pack method PACK_RDP_PLAIN_256_COLORS.\n");
				#endif
			
				nxdesktopUseNXRdpImages = True;
        	        }
			
			if (methods[PACK_RDP_PLAIN_256_COLORS] == False &&
			    methods[PACK_RDP_COMPRESSED_256_COLORS] == False)
			{  
			    fprintf(stderr, "ui_init: WARNING! No available RDP pack method on g_display '%s'.\n",
						XDisplayName((char *)nxDisplay));
			}
			/*if (nxdesktopPackMethod == PACK_RDP_COMPRESSED_256_COLORS &&
				methods[PACK_RDP_COMPRESSED_256_COLORS] == True)
        	        {
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_init: Using pack method PACK_RDP_COMPRESSED_256_COLORS.\n");
				#endif

				nxdesktopUseNXCompressedRdpImages = True;
        	        }
			else if (nxdesktopPackMethod == PACK_RDP_PLAIN_256_COLORS &&
				    methods[PACK_RDP_PLAIN_256_COLORS] == True)
	                {
				fprintf(stderr, "ui_init: Using pack method PACK_RDP_PLAIN_256_COLORS.\n");

				nxdesktopUseNXRdpImages = True;
        	        }
			else
			{
				fprintf(stderr, "ui_init: WARNING! No available RDP pack method on g_display '%s'.\n",
						XDisplayName(nxDisplay));
			}*/
		}

		/*
		 * Inform remote proxy about pixel geometry
		 * to be used to unpack images.
		 */

		if (nxdesktopUseNXCompressedRdpImages ||
			nxdesktopUseNXRdpImages)
		{
			if (NXSetUnpackGeometry(g_display, 0, g_screen, g_visual) == 0)
			{
				fprintf(stderr, "ui_init: ERROR! NXSetUnpackGeometry() failed on g_display '%s'.\n",
						XDisplayName((char *)nxDisplay));

				exit(1);
			}
		}
	}
	
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

	/* make sure width is a multiple of 4 */
	g_width = (g_width + 3) & ~3;

	g_mod_map = XGetModifierMapping(g_display);

	xkeymap_init();

	if (g_enable_compose)
		g_IM = XOpenIM(g_display, NULL, NULL, NULL);

	xclip_init();

	DEBUG_RDP5(("server bpp %d client bpp %d depth %d\n", g_server_bpp, g_bpp, g_depth));

	return True;
}

void
ui_deinit(void)
{
	#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
	fprintf(stderr,"create_bitmap total = %d\n",create_bitmap_total);
	fprintf(stderr,"create_bitmap times = %d\n",create_bitmap_times);
	fprintf(stderr,"create_glyph total = %d\n",create_glyph_total);
	fprintf(stderr,"create_glyph times = %d\n",create_glyph_times);
	fprintf(stderr,"paint_bitmap total = %d\n",paint_bitmap_total);
	fprintf(stderr,"paint_bitmap times = %d\n",paint_bitmap_times);
	fprintf(stderr,"paint_bitmap_backstore total = %d\n",paint_bitmap_backstore_total);
	fprintf(stderr,"paint_bitmap_backstore times = %d\n",paint_bitmap_backstore_times);
	#endif	
	
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
	fprintf(stderr,"nxagentQueryAtoms: Created intern atom [%s] with id [%ld].\n",
		nxdesktopAtomNames[i], nxdesktopAtoms[i]);
    }
    #endif
    
    identity = nxdesktopAtoms[0];
    XChangeProperty(g_display, g_wnd, identity, XA_ATOM, sizeof(int) * 8, PropModeReplace, (unsigned char *) &type,  4);
    
    XSetSelectionOwner(g_display, nxdesktopAtoms[5], g_wnd, CurrentTime);
    
    XSetWMProtocols(g_display, g_wnd, &nxdesktopAtoms[2], 1);
}


BOOL
ui_create_window(void)
{
	uint8 null_pointer_mask[1] = { 0x80 };
	uint8 null_pointer_data[4] = { 0x00, 0x00, 0x00, 0x00 };
	XSetWindowAttributes attribs;
	XClassHint *classhints;
	XSizeHints *sizehints;
	int wndwidth, wndheight;
	long input_mask, ic_input_mask;
 	/* XEvent xevent; */
	/* NX */
	int i;
	XWMHints wmhints;
	static struct sigaction sigusr_act;
	/* NX */

	wndwidth = g_fullscreen ? WidthOfScreen(g_screen) : g_width;
	wndheight = g_fullscreen ? HeightOfScreen(g_screen) : g_height;

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
/*		ownbackstore = False;
       NXSetExposeEvents(g_display, False, False, False);
*/
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

	if (g_fullscreen || ipaq)
	{
		attribs.override_redirect = True;
	/* Prepare signal handler for SIGUSR1 and SIGUSR2 */
		sigusr_act.sa_handler = sigusr_func;

	/* Install signal handler for SIGUSR1 and SIGUSR2 */
		sigfillset(&(sigusr_act.sa_mask));
		sigaction(SIGUSR1, &sigusr_act, NULL);
		sigaction(SIGUSR2, &sigusr_act, NULL);

		g_width = WidthOfScreen(g_screen);
                g_height = HeightOfScreen(g_screen);
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
	
	/*g_wnd = XCreateWindow(g_display, RootWindowOfScreen(g_screen), 100,100, 1000, 1000,
			      0, g_depth, InputOutput, g_visual,
			      CWBackPixel | CWBackingStore | CWOverrideRedirect |
			      CWColormap | CWBorderPixel, &attribs);*/
	g_wnd = XCreateWindow(g_display, RootWindowOfScreen(g_screen), xo, yo, wndwidth, wndheight,
			      0, CopyFromParent, InputOutput, CopyFromParent,
			      CWBackPixel | CWBackingStore | (g_fullscreen ? CWOverrideRedirect:
			      SubstructureRedirectMask) | StructureNotifyMask, &attribs);
	
	
	
	if (g_gc == NULL)
		g_gc = XCreateGC(g_display, g_wnd, 0, NULL);

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
/*
		wnd2 = XCreateWindow (g_display, DefaultRootWindow (g_display), 0, 0, 1, 1,
				0, CopyFromParent, InputOutput, CopyFromParent,
				CWBackingStore | CWBackPixel , &attribs);
                                */
		wnd2 = XCreateWindow (g_display, RootWindowOfScreen(g_screen), 0, 0, 1, 1,
				0, CopyFromParent, InputOutput, CopyFromParent,
				CWBackingStore | CWBackPixel , &attribs);

	}
	//
	/* NX */
	/* Check XStoreName consistency */
		
	XStoreName(g_display, g_wnd, g_title);

	if (g_hide_decorations)
		mwm_hide_decorations();

	classhints = XAllocClassHint();
	if (classhints != NULL)
	{
		classhints->res_name = classhints->res_class = "nxdesktop";
		XSetClassHint(g_display, g_wnd, classhints);
		XFree(classhints);
	}

	sizehints = XAllocSizeHints();
	if (sizehints)
	{
		sizehints->flags = USPosition | PMinSize | PMaxSize;
		sizehints->min_width = sizehints->max_width = g_width;
		sizehints->min_height = sizehints->max_height = g_height;
		XSetStandardProperties(g_display,
			   g_wnd,
			   (windowName?windowName:"nxdesktop"),
			   (windowName?windowName:"nxdesktop"),
			   nxIconPixmap,
			   0, 0, sizehints);

		wmhints.icon_pixmap = nxIconPixmap;
		if (useXpmIcon)
		{
		   wmhints.icon_mask = nxIconShape;
		   wmhints.flags = IconPixmapHint | IconMaskHint;
		} else {
		   wmhints.flags = StateHint | IconPixmapHint;
		}
		XSetWMHints (g_display, g_wnd, &wmhints);
		XSetWMNormalHints(g_display, g_wnd, sizehints);
	}

        if ( g_embed_wnd )
        {
                XReparentWindow(g_display, g_wnd, (Window)g_embed_wnd, 0, 0);
        }

	/* NX */
	input_mask = KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask |
		VisibilityChangeMask | FocusChangeMask;
	
	
	 if (g_fullscreen)
	 	input_mask |= (EnterWindowMask | LeaveWindowMask);	
	/* NX */
	XMapWindow(g_display, g_wnd);		
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
	
	XGrabKeyboard(g_display, g_wnd, True, GrabModeAsync, GrabModeAsync, CurrentTime);
	
	

	/* wait for VisibilityNotify 
	do
	{
		XMaskEvent(g_display, VisibilityChangeMask, &xevent);
	}
	while (xevent.type != VisibilityNotify); */
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
          int i = 1;
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

	
        {
            int minkey, maxkey;
            XDisplayKeycodes(g_display, &minkey, &maxkey);
            g_mod_map = XGetModifierMapping(g_display);
            xkeymap_init();
        }
	/* NX */
	return True;
}

void
ui_destroy_window(void)
{
	if (g_IC != NULL)
		XDestroyIC(g_IC);

	XDestroyWindow(g_display, g_wnd);
	/* NX */
	if (g_fullscreen)
	  XDestroyWindow (g_display, wnd2);
	 /* NX */
}

void
xwin_toggle_fullscreen(void)
{
	Pixmap contents = 0;

	if (!g_ownbackstore)
	{
		/* need to save contents of window */
		contents = XCreatePixmap(g_display, g_wnd, g_width, g_height, g_depth);
		XCopyArea(g_display, g_wnd, contents, g_gc, 0, 0, g_width, g_height, 0, 0);
	}

	ui_destroy_window();
	g_fullscreen = !g_fullscreen;
	ui_create_window();

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
			
		switch (xevent.type)
		{
			/* NX */
			case MapNotify:
					if (g_fullscreen) 
						sigusr_func(SIGUSR2);
					break;
			/* NX */
			
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
					XmbLookupString(g_IC,
							&xevent.xkey, str, sizeof(str), &keysym,
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
					/* fprintf(stderr,"signal send -HUP\n"); */
					break;
				}
                                last_Xtime = xevent.xkey.time;
				/* NX */

				if (tr.scancode == 0)
					break;
				/* NX */
				/* When in g_fullscreen, unmap window on Ctrl-Alt-Escape */
				if ((tr.scancode == 0x01) &&
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
					       rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE,
					                               KBD_FLAG_DOWN | KBD_FLAG_UP, 0xD3, 0);
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
			         /* Iconify rdesktop when left-clicking in lower right corner */
				if ((xevent.xbutton.button == 1) &&
					(xevent.xbutton.x >= (g_width - 2)) && (xevent.xbutton.y <= 2))
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
				/* NX */
				
			case ButtonRelease:
				g_last_gesturetime = xevent.xbutton.time;
				/* NX */
				last_Xtime = g_last_gesturetime;
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

				rdp_send_input(time(NULL), RDP_INPUT_MOUSE,
					       flags | button, xevent.xbutton.x, xevent.xbutton.y);
				break;

			case MotionNotify:
				if (g_moving_wnd)
				{
					XMoveWindow(g_display, g_wnd,
						    xevent.xmotion.x_root - g_move_x_offset,
						    xevent.xmotion.y_root - g_move_y_offset);
					break;
				}

				if (g_fullscreen && !g_focused)
					XSetInputFocus(g_display, g_wnd, RevertToPointerRoot,
						       CurrentTime);
				/* NX */
				last_Xtime = xevent.xmotion.time;
				/* NX */		       
				
				rdp_send_input(time(NULL), RDP_INPUT_MOUSE,
					       MOUSE_FLAG_MOVE, xevent.xmotion.x, xevent.xmotion.y);
				break;

			case FocusIn:
				if (xevent.xfocus.mode == NotifyGrab)
					break;
				g_focused = True;
				reset_modifier_keys();
				if (g_grab_keyboard && g_mouse_in_wnd && g_fullscreen)
				    {
					XGrabKeyboard(g_display, g_wnd, True,
						      GrabModeAsync, GrabModeAsync, CurrentTime);
				    }
				break;

			case FocusOut:
				if (xevent.xfocus.mode == NotifyUngrab)
					break;
				g_focused = False;
				if (xevent.xfocus.mode == NotifyWhileGrabbed)
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
				xclip_handle_SelectionNotify(&xevent.xselection);
				break;
			case SelectionRequest:
				xclip_handle_SelectionRequest(&xevent.xselectionrequest);
				break;
			case SelectionClear:
				xclip_handle_SelectionClear();
				break;
			case PropertyNotify:
				xclip_handle_PropertyNotify(&xevent.xproperty);
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
	return;
}

/* Returns 0 after user quit, 1 otherwise */
int
ui_select(int rdp_socket)
{
	int n = (rdp_socket > g_x_socket) ? rdp_socket : g_x_socket;
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
			/* User quit */
			return 0;

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

		switch (select(n, &rfds, &wfds, NULL, &tv))
		{
			case -1:
				/*error("select: %s\n", strerror(errno)); */

			case 0:
				/* TODO: if tv.tv_sec just times out
				 * we will segfault.
				 * FIXME:
				 */
				//s_timeout = True;
				//rdpdr_check_fds(&rfds, &wfds, (BOOL) True);
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
	#ifndef NXDESKTOP_IMGCACHE_USES_COMPRESSED_IMAGES
	uint8 *tdata;
	#endif
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
	
	bitmap = XCreatePixmap(g_display, g_wnd, width, height, g_depth);
				 
	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_create_bitmap: XPutImage on pixmap %d,%d,%d,%d.\n", 0, 0, width, height);
	#endif
	
	#ifdef NXDESKTOP_DEBUG_XPUTIMAGE	    
	create_bitmap_times++;
	create_bitmap_total+=image->height*image->bytes_per_line;
	#endif
	
	#ifdef NXDESKTOP_IMGCACHE_USES_COMPRESSED_IMAGES
	
	# if 0
	if (compressed)
	    fprintf(stderr,"C");
	else 
	    fprintf(stderr,"N\n");
	#endif
	
	if (compressed)
	{
	    image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_COMPRESSED_256_COLORS,
					g_depth, ZPixmap, data, size,
					width, height, bitmap_pad, 0);

	    NXPutPackedImage(g_display, 0, bitmap, g_gc, image,
			    PACK_RDP_COMPRESSED_256_COLORS,
			    g_depth, 0, 0, 0, 0, width, height);
	}
	else
	{
	    image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_PLAIN_256_COLORS,
					g_depth, ZPixmap, data, size,
					width, height, bitmap_pad, 0);

	    NXPutPackedImage(g_display, 0, bitmap, g_gc, image,
			    PACK_RDP_PLAIN_256_COLORS,
			    g_depth, 0, 0, 0, 0, width, height);
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
		
	#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES

	if (nxdesktopUseNXRdpImages)
	{
		int data_length;
		tdata = data;
		data_length = width * height;

		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_bitmap: NXCreatePackedImage with g_owncolmap %d and g_depth %d.\n",
				g_owncolmap, g_depth);
		#endif

		image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_PLAIN_256_COLORS,
						g_depth, ZPixmap, tdata, data_length,
							width, height, BitmapPad(g_display), 0);

		if (g_ownbackstore)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_paint_bitmap: NXPutPackedImage on backingstore pixmap %d,%d,%d,%d (%d,%d).\n",
					x, y, cx, cy, width, height);
			#endif

			NXPutPackedImage(g_display, 0, g_backstore, g_gc, image,
						PACK_RDP_PLAIN_256_COLORS,
							g_depth, 0, 0, x, y, cx, cy);

			XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);

			#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
			XFlush(g_display);
			#endif
		}
		else
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_paint_bitmap: NXPutPackedImage on window %d,%d,%d,%d (%d,%d).\n",
					x, y, cx, cy, width, height);
			#endif

			NXPutPackedImage(g_display, 0, g_wnd, g_gc, image,
						PACK_RDP_PLAIN_256_COLORS,
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
		fprintf(stderr, "ui_paint_bitmap: XCreateImage with owncolmap %d and depth %d.\n",
				g_owncolmap, g_depth);
		#endif

		image = XCreateImage(g_display, g_visual, g_depth, ZPixmap,
					0, tdata, width, height, 8, 0);

		if (g_ownbackstore)
		{
			
			
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_paint_bitmap: XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
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
			fprintf(stderr, "ui_paint_bitmap: XPutImage on window %d,%d,%d,%d.\n",
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
	fprintf(stderr, "ui_paint_bitmap: XCreateImage with owncolmap %d and depth %d.\n",
			owncolmap, depth);
	#endif
	
	image = XCreateImage(g_display, g_visual, g_depth, ZPixmap, 0,
			     (char *) tdata, width, height, bitmap_pad, 0);

	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_bitmap: XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
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
		fprintf(stderr, "ui_paint_bitmap: XPutImage on window %d,%d,%d,%d.\n",
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
}
#ifdef NXDESKTOP_XWIN_USES_COMPRESSED_PACKED_IMAGES

void
ui_paint_compressed_bitmap(int x, int y, int cx, int cy, int width, int height,
				uint8 *compressed_data, int compressed_size)
{
	XImage *image;

	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_paint_compressed_bitmap: NXCreatePackedImage with depth %d and size %d.\n",
			g_depth, compressed_size);
	#endif

	image = NXCreatePackedImage(g_display, g_visual, PACK_RDP_COMPRESSED_256_COLORS,
					g_depth, ZPixmap, compressed_data, compressed_size,
						width, height, BitmapPad(g_display), 0);

	if (image == NULL)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_compressed_bitmap: NXCreatePackedImage failure.\n");
		#endif

		return;
	}


	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_compressed_bitmap: NXPutPackedImage on backingstore pixmap %d,%d,%d,%d (%d,%d).\n",
				x, y, cx, cy, width, height);
		#endif

		NXPutPackedImage(g_display, 0, g_backstore, g_gc, image,
					PACK_RDP_COMPRESSED_256_COLORS,
						g_depth, 0, 0, x, y, cx, cy);

		XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);

		#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
		XFlush(g_display);
		#endif
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_compressed_bitmap: NXPutPackedImage on window %d,%d,%d,%d (%d,%d).\n",
				x, y, cx, cy, width, height);
		#endif

		NXPutPackedImage(g_display, 0, g_wnd, g_gc, image,
					PACK_RDP_COMPRESSED_256_COLORS,
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
	GC gc;

	scanline = (width + 7) / 8;

	bitmap = XCreatePixmap(g_display, g_wnd, width, height, 1);
	gc = XCreateGC(g_display, bitmap, 0, NULL);

	image = XCreateImage(g_display, g_visual, 1, ZPixmap, 0, (char *) data,
			     width, height, 8, scanline);
	image->byte_order = MSBFirst;
	image->bitmap_bit_order = MSBFirst;
	XInitImage(image);
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_create_glyph: XPutImage on pixmap %d,%d,%d,%d.\n",
			0, 0, width, height);
	#endif
	
	#ifdef NXDESKTOP_DEBUG_XPUTIMAGE
	create_glyph_times++;
	create_glyph_total+=image->height*image->bytes_per_line;
	#endif
	
	XPutImage(g_display, bitmap, gc, image, 0, 0, 0, 0, width, height);

	XFree(image);
	XFreeGC(g_display, gc);
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
           setOwnerNX_WM(g_wnd);
           XSync(g_display, True);
        }
	#endif
	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_create_colourmap: Creating new colormap.\n");
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
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_create_colourmap: !g_owncolmap used for %d numcolors.\n",ncolours);
		#endif
		uint32 *map = (uint32 *) xmalloc(sizeof(*g_colmap) * ncolours);
		XColor xentry[ncolours];
		Bool alloc_done[ncolours]; 
		XColor xc_cache[256];
		uint32 colour;
		int colLookup = 256;
		
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
	fprintf(stderr, "ui_destroy_colourmap: Destroyed colormap at %p.\n",
			map);
	#endif
}

void
ui_set_colourmap(HCOLOURMAP map)
{	
	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_set_colourmap: Setting new colormap at address %p.\n",
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
		fprintf(stderr, "ui_set_colourmap: Dumping colormap entries:\n");
		for (i = 0; i < last_colormap_entries; i++)
		{
		    fprintf(stderr, "ui_set_colourmap: [%d] [%p].\n",i, (void *) last_colormap[i]);
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
			SET_FOREGROUND(fgcolour);
			FILL_RECTANGLE(x, y, cx, cy);
			break;

		case 2:	/* Hatch */
			fill = (Pixmap) ui_create_glyph(8, 8,
							hatch_patterns + brush->pattern[0] * 8);
			SET_FOREGROUND(fgcolour);
			SET_BACKGROUND(bgcolour);
			XSetFillStyle(g_display, g_gc, FillOpaqueStippled);
			XSetStipple(g_display, g_gc, fill);
			XSetTSOrigin(g_display, g_gc, brush->xorigin, brush->yorigin);
			FILL_RECTANGLE(x, y, cx, cy);
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

			FILL_RECTANGLE(x, y, cx, cy);

			XSetFillStyle(g_display, g_gc, FillSolid);
			XSetTSOrigin(g_display, g_gc, 0, 0);
			ui_destroy_glyph((HGLYPH) fill);
			break;

		default:
			unimpl("brush %d\n", brush->style);
	}

	RESET_FUNCTION(opcode);
}

void
ui_screenblt(uint8 opcode,
	     /* dest */ int x, int y, int cx, int cy,
	     /* src */ int srcx, int srcy)
{
	SET_FUNCTION(opcode);
	if (g_ownbackstore)
	{
		XCopyArea(g_display, g_backstore, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
		XCopyArea(g_display, g_backstore, g_backstore, g_gc, srcx, srcy, cx, cy, x, y);
	}
	else
	{
		XCopyArea(g_display, g_wnd, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
	}
	RESET_FUNCTION(opcode);
}

void
ui_memblt(uint8 opcode,
	  /* dest */ int x, int y, int cx, int cy,
	  /* src */ HBITMAP src, int srcx, int srcy)
{
	SET_FUNCTION(opcode);
	XCopyArea(g_display, (Pixmap) src, g_wnd, g_gc, srcx, srcy, cx, cy, x, y);
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
			unimpl("triblt 0x%x\n", opcode);
			ui_memblt(ROP2_COPY, x, y, cx, cy, src, srcx, srcy);
	}
}

void
ui_line(uint8 opcode,
	/* dest */ int startx, int starty, int endx, int endy,
	/* pen */ PEN * pen)
{
	SET_FUNCTION(opcode);
	SET_FOREGROUND(pen->colour);
	XDrawLine(g_display, g_wnd, g_gc, startx, starty, endx, endy);
	if (g_ownbackstore)
		XDrawLine(g_display, g_backstore, g_gc, startx, starty, endx, endy);
	RESET_FUNCTION(opcode);
}

/* NX */
void
ui_poly_line(uint8 opcode, short *points, int count,
	/* pen */ PEN *pen)
{
	SET_FUNCTION(opcode);
	SET_FOREGROUND(pen->colour);

	XDrawLines(g_display, g_wnd, g_gc, (XPoint*)points, count, CoordModeOrigin);
	if (g_ownbackstore)
        	XDrawLines(g_display, g_backstore, g_gc, (XPoint*)points, count, CoordModeOrigin);
	RESET_FUNCTION(opcode);
}
/* NX */

void
ui_rect(
	       /* dest */ int x, int y, int cx, int cy,
	       /* brush */ int colour)
{
	SET_FOREGROUND(colour);
	FILL_RECTANGLE(x, y, cx, cy);
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
    XGCValues values;\
    x1 = x + glyph->offset;\
    y1 = y + glyph->baseline;\
    memset(&values, 0, sizeof(XGCValues));\
    values.stipple = (Pixmap)glyph->pixmap;\
    values.ts_x_origin = x + (short) glyph->offset;\
    values.ts_y_origin = y + (short) glyph->baseline;\
    XChangeGC(g_display, g_gc, GCStipple | GCTileStipXOrigin | GCTileStipYOrigin, &values);\
    FILL_RECTANGLE_BACKSTORE(x1, y1, glyph->width, glyph->height);\
    if (flags & TEXT2_IMPLICIT_X)\
      x += glyph->width;\
  }\
}

/*
	Just in case i need to return the XChengeGC above
	XSetStipple(g_display, g_gc, (Pixmap) glyph->pixmap);\
	XSetTSOrigin(g_display, g_gc, x1, y1);\
*/

void
ui_draw_text(uint8 font, uint8 flags, int mixmode, int x, int y,
	     int clipx, int clipy, int clipcx, int clipcy,
	     int boxx, int boxy, int boxcx, int boxcy, int bgcolour,
	     int fgcolour, uint8 * text, uint8 length)
{
	FONTGLYPH *glyph;
	int i, xyoffset, x1, y1;
/* 	DATABLOB *entry;*/

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

#ifdef NXWIN_USES_PACKED_RDP_TEXT
	
	if (nxdesktopUseNXTrans && nxdesktopCanPackRDPText)
	{
	
		xNXRDPGlyph rdp_text[length];
		NXPackedImage *image;
		int elements = 0;
	
		/*
		 * Fill PACK_RDP_TEXT array, character by character.
		 */

		for (i = 0; i < length; i++)
		{
			glyph = cache_get_font(font, text[i]);

			if (!(flags & TEXT2_IMPLICIT_X))
			{
				xyoffset = text[++i];
				if (xyoffset & 0x80)
					xyoffset = ((xyoffset & 0x7f) << 8) | text[++i];

				if (flags & TEXT2_VERTICAL)
					y += xyoffset;
				else
					x += xyoffset;
			}

			if (glyph != NULL)
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_draw_text: Building element [%d] with pixmap [0x%lx].\n",
						i, (Pixmap)glyph->pixmap);
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

		image = NXEncodeRDPText(g_display, TRANSLATE(bgcolour), TRANSLATE(fgcolour),
						(mixmode == MIX_TRANSPARENT ? FillStippled :
							FillOpaqueStippled), &rdp_text[0], elements);

		if (image)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_draw_text: Using packed image with drawable [0x%lx] and gc [0x%lx].\n",
					g_wnd, g_gc);
			#endif

			NXPutPackedImage(g_display, 0, (g_ownbackstore) ? g_backstore : g_wnd, 
				         g_gc, image, PACK_RDP_TEXT,
						1, 0, 0, 0, 0, elements, 1);

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
	else
	{
#endif
	SET_FOREGROUND(fgcolour);
	SET_BACKGROUND(bgcolour);
	/* XSetFillStyle(g_display, g_gc, FillStippled); */
	XSetFillStyle(g_display, g_gc, (mixmode == MIX_TRANSPARENT)
		      ? FillStippled : FillOpaqueStippled);
		      
	/* Paint text, character by character */
	for (i = 0; i < length; i++)
	{
		DO_GLYPH(text, i);
		
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
#ifdef NXWIN_USES_PACKED_RDP_TEXT
	}	
#endif
	
}

void
ui_desktop_save(uint32 offset, int x, int y, int cx, int cy)
{
	
	/* NX */
	#ifdef NXDESKTOP_XWIN_USES_PIXMAP_CACHE

	int i;

	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_desktop_save: Called with offset [%d]. Using pixmap cache.\n",
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
			fprintf(stderr, "ui_desktop_save: Saved area in pixmap cache [%lx] index [%d].\n",
					cache, i);
			#endif

			if (g_ownbackstore)
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_desktop_save: XCopyArea from backingstore to pixmap cache %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, g_backstore, cache, g_gc, x, y, cx, cy, 0, 0);
			}
			else
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_desktop_save: XCopyArea from window to pixmap cache %d,%d,%d,%d.\n",
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
		fprintf(stderr, "ui_desktop_save: PANIC! Couldn't find any free pixmap cache slot.\n");
		#endif
	}

	#else /* NXDESKTOP_XWIN_USES_PIXMAP_CACHE */
	
	Pixmap pix;
	XImage *image;
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_desktop_save: Called with offset [%d]. Not using pixmap cache.\n",
			offset);
	#endif
	/* NX */

	if (g_ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_desktop_save: XGetImage from backingstore pixmap %d,%d,%d,%d.\n",
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
	fprintf(stderr, "ui_desktop_restore: Called with offset [%d]. Using pixmap cache.\n",
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
				fprintf(stderr, "ui_desktop_restore: XCopyArea from pixmap cache to backingstore %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, cache, g_backstore, g_gc, 0, 0, cx, cy, x, y);
				XCopyArea(g_display, g_backstore, g_wnd, g_gc, x, y, cx, cy, x, y);
			}
			else
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_desktop_restore: XCopyArea from pixmap cache to window %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(g_display, cache, g_wnd, g_gc, 0, 0, cx, cy, x, y);
			}

			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_desktop_restore: Restored area from pixmap cache [%lx] index [%d].\n",
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
		fprintf(stderr, "ui_desktop_restore: PANIC! Couldn't find pixmap cache slot.\n");
		#endif
	}


	#else /* NXDESKTOP_XWIN_USES_PIXMAP_CACHE */
	/* NX */
	
	XImage *image;
	uint8 *data;
	
	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_desktop_restore: Called with offset [%d]. Not using pixmap cache.\n",
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
		fprintf(stderr, "ui_desktop_restore: XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
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
		fprintf(stderr, "ui_desktop_restore: XPutImage on window %d,%d,%d,%d.\n",
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
}

/* NX */

#ifdef NXDESKTOP_LOGO
void nomachineLogo(Window win, GC gc, int scale)
{
    XPoint    rect[4];
    XPoint    m[12];
    int w, h, c, w2, h2;

#ifdef NXDESKTOP_LOGO_DEBUG
    fprintf(stderr,"nomachineLogo: begin\n");
    fprintf(stderr,"nomachineLogo: gen params are: %d %x %x %x\n",
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
    fprintf(stderr,"filled first poly\n");
#endif

    XSetForeground(g_display, gc, nx_red);
    XSetBackground(g_display, gc, nx_white);

    rect[0].x = w2-10*c;	       rect[0].y = h2-8*c;
    rect[1].x = w2-10*c;	       rect[1].y = h2+8*c;
    rect[2].x = w2+10*c;	       rect[2].y = h2+8*c;
    rect[3].x = w2+10*c;	       rect[3].y = h2-8*c;

    XFillPolygon(g_display, win, gc, rect, 4, Convex, CoordModeOrigin);

#ifdef NXDESKTOP_LOGO_DEBUG
    fprintf(stderr,"filled red rect\n");
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

/*    XFlush(g_display);*/
    XSync(g_display, True);

#ifdef NXDESKTOP_LOGO_DEBUG
    fprintf(stderr,"nomachineLogo: end\n");
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
  strcat(default_path,"/usr/NX/share/");
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
        fprintf(stderr, "XpmError: %s\n", XpmGetErrorString(status));
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
  fprintf(stderr, "nxkbd is running\n");
  if (xkbdRunning)
  {
#ifdef NXAGENT_XKBD_DEBUG
    fprintf(stderr, "Events: nxkbd now is NOT running\n");
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
        fprintf(stderr, "Events: execvp of nxkbd failed\n");
#endif
        exit(1);
      case -1:
#ifdef NXAGENT_XKBD_DEBUG
        fprintf(stderr, "Events: can't fork to run nxkbd\n");
#endif
        break;
      default:
        ;
    }
#ifdef NXAGENT_XKBD_DEBUG
    fprintf(stderr, "Events: nxkbd now is running on $DISPLAY %s\n", kbddisplay);
#endif
    xkbdRunning = True;
  }
}
