/*
   rdesktop: A Remote Desktop Protocol client.
   User interface services - X-Windows
   Copyright (C) Matthew Chapman 1999-2001

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
#include <X11/Xlibint.h>
#include <X11/Xutil.h>
#include <NXlib.h>
#include <time.h>
#include <errno.h>
#include "rdesktop.h"
#include <unistd.h>
#include <signal.h>

#include "version.h"
#include "icon.h"
#include <X11/xpm.h>
#include "X11/Xatom.h"

#undef NXDESKTOP_XWIN_DEBUG

#undef NXDESKTOP_USES_SYNC_IN_LOOP

#undef NXDESKTOP_NXKARMA_DEBUG

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

Atom nxdesktop_WM_START;

#endif


extern int xo;
extern int yo;
extern int width;
extern int height;
extern BOOL sendmotion;
extern BOOL fullscreen;
extern BOOL ipaq;
extern BOOL magickey;
extern char *windowName;
extern char *nxDisplay;

Display *display;
static int x_socket;
static Window wnd;
static Window wnd2; /* Helper window, used only in fullscreen mode */
static GC gc;
static Visual *visual;
static int depth;
static int bpp;

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

/* WM predefined atom
*/
Atom nxdesktop_WM = None;

/* NX clipboard and ID Atom */

Atom nxdesktop_NX = None;
/* endianness */
static BOOL host_be;
static BOOL xserver_be;

/* software backing store */
static BOOL ownbackstore;
static Pixmap backstore;

/* icon pixmaps */
#define NX_DEFAULT_ICON "nxdesktop.xpm"
static Pixmap nxIconPixmap;
static Pixmap nxIconShape;
Bool useXpmIcon = False;

Bool getNXIcon(Display *display, Pixmap *nxIcon, Pixmap *nxMask);

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



uint32  last_Xtime = 0; /* last X server time received in an event */

#define FILL_RECTANGLE(x,y,cx,cy)\
{ \
	if (cx < 2  && cy < 2){\
          XDrawPoint(display, wnd, gc, x, y); \
	  if (ownbackstore) \
            XDrawPoint(display, backstore, gc, x, y); \
        }else\
        {\
	  XFillRectangle(display, wnd, gc, x, y, cx, cy); \
	  if (ownbackstore) \
		XFillRectangle(display, backstore, gc, x, y, cx, cy); \
        }\
}

/* colour maps */
static BOOL owncolmap;
static Colormap xcolmap;
static uint32 white;
static uint32 *colmap;


static unsigned int r, b, g, or, ob, og, off;


#define TRANSLATE(col)		( owncolmap ? (unsigned int)col : translate_colour(colmap[col]) )
#define SET_FOREGROUND(col)	XSetForeground(display, gc, TRANSLATE(col));
#define SET_BACKGROUND(col)	XSetBackground(display, gc, TRANSLATE(col));

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

#define SET_FUNCTION(rop2)	{ if (rop2 != ROP2_COPY) XSetFunction(display, gc, rop2_map[rop2]); }
#define RESET_FUNCTION(rop2)	{ if (rop2 != ROP2_COPY) XSetFunction(display, gc, GXcopy); }


static void
translate8(uint8 *data, uint8 *out, uint8 *end)
{
	while (out < end)
		*(out++) = (uint8)colmap[*(data++)];
}

static void
translate16(uint8 *data, uint16 *out, uint16 *end)
{
	while (out < end)
		*(out++) = (uint16)colmap[*(data++)];
}

/* little endian - conversion happens when colourmap is built */
static void
translate24(uint8 *data, uint8 *out, uint8 *end)
{
	uint32 value;

	while (out < end)
	{
		value = colmap[*(data++)];
		*(out++) = value;
		*(out++) = value >> 8;
		*(out++) = value >> 16;
	}
}

static void
translate32(uint8 *data, uint32 *out, uint32 *end)
{
	while (out < end)
		*(out++) = colmap[*(data++)];
}

static uint8 *
translate_image(int width, int height, uint8 *data)
{
	int size = width * height * bpp/8;
	uint8 *out = xmalloc(size);
	uint8 *end = out + size;

	switch (bpp)
	{
		case 8:
			translate8(data, out, end);
			break;

		case 16:
			translate16(data, (uint16 *)out, (uint16 *)end);
			break;

		case 24:
			translate24(data, out, end);
			break;

		case 32:
			translate32(data, (uint32 *)out, (uint32 *)end);
			break;
	}

	return out;
}

#define BSWAP16(x) { x = (((x & 0xff) << 8) | (x >> 8)); }
#define BSWAP24(x) { x = (((x & 0xff) << 16) | (x >> 16) | ((x >> 8) & 0xff00)); }
#define BSWAP32(x) { x = (((x & 0xff00ff) << 8) | ((x >> 8) & 0xff00ff)); \
			x = (x << 16) | (x >> 16); }

static uint32
translate_colour(uint32 colour)
{
	switch (bpp)
	{
		case 16:
			if (host_be != xserver_be)
				BSWAP16(colour);
			break;

		case 24:
			if (xserver_be)
				BSWAP24(colour);
			break;

		case 32:
			if (host_be != xserver_be)
				BSWAP32(colour);
			break;
	}

	return colour;
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
       XIconifyWindow (display, wnd2, DefaultScreen(display));
       XMapWindow(display, wnd2);
    }
    XUnmapWindow (display, wnd);
    break;
  case SIGUSR2:
    DEBUG (("Received SIGUSR2, mapping window.\n"));
    if(ipaq)
    {
       XMapWindow (display, wnd);
       XUnmapWindow(display, wnd2);
    }
    else {
       XMapRaised (display, wnd);
       XIconifyWindow (display, wnd2, DefaultScreen(display));
    }
    break;
  default:
    break;
  }
  XFlush (display);
}

BOOL
get_key_state(unsigned int state, uint32 keysym)
{
	int modifierpos, key, keysymMask = 0;
	int offset;

	KeyCode keycode = XKeysymToKeycode(display, keysym);

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
   display = XOpenDisplay(nxDisplay);
   if (display == NULL)
   {
     error("Failed to open display\n");
     return False;
   }
   return True;
}

void ui_close_display()
{
  XCloseDisplay(display);
}


void ui_get_display_size(int *width, int *height)
{
   *width  = WidthOfScreen(DefaultScreenOfDisplay(display));
   *height = HeightOfScreen(DefaultScreenOfDisplay(display));
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

BOOL
ui_create_window(char *title)
{
	XSetWindowAttributes attribs;
	XClassHint *classhints;
	XSizeHints *sizehints;
	XWMHints wmhints;
	unsigned long input_mask;
	XPixmapFormatValues *pfm;
	Screen *screen;
	uint16 test;
  	int i;
	static struct sigaction sigusr_act;

/*
	display = XOpenDisplay(nxDisplay);
	if (display == NULL)
	{
		error("Failed to open display\n");
		return False;
	}
*/
	x_socket = ConnectionNumber(display);

        {
          extern void tcp_resize_buf(int, int, int);
          extern int rdp_bufsize;
          tcp_resize_buf(x_socket, 0, rdp_bufsize);
        }

	screen = DefaultScreenOfDisplay(display);
	visual = DefaultVisualOfScreen(screen);
	depth = DefaultDepthOfScreen(screen);

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

                NXGetControlParameters(display, &nxdesktopLinkType, &nxdesktopProtocolMajor,
						&nxdesktopProtocolMinor, &nxdesktopProtocolPatch,
						&nxdesktopStopKarmaSz, &nxdesktopSplitSize,
						&nxdesktopPackMethod, &nxdesktopPackQuality,
                                                &nxdesktopDataLevel, &nxdesktopStreamLevel,
                                                &nxdesktopDeltaLevel, &nxdesktopLoadCache,
                                                &nxdesktopSaveCache, &nxdesktopStartupCache);


		NXGetCleanupParameters(display, &nxdesktopCleanGet, &nxdesktopCleanAlloc,
						&nxdesktopCleanFlush, &nxdesktopCleanSend,
						&nxdesktopCleanImages);

		NXGetImageParameters(display,	&nxdesktopImageSplit, &nxdesktopImageMask,
						&nxdesktopImageFrame, &nxdesktopImageSplitMethod,
						&nxdesktopImageMaskMethod);

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

			NXGetShmemParameters(display, &enableClient, &enableServer,
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

			if (NXGetUnpackParameters(display, &entries, methods) == 0 ||
				entries != NXNumberOfPackMethods)
			{
				fprintf(stderr, "ui_create_window: ERROR! NXGetUnpackParameters() failed on display '%s'.\n",
						XDisplayName(nxDisplay));

				exit(1);
			}

#ifdef NXWIN_USES_PACKED_RDP_TEXT
                        nxdesktopCanPackRDPText = methods[PACK_RDP_TEXT];
#endif

			if (nxdesktopPackMethod == PACK_RDP_COMPRESSED_256_COLORS &&
				methods[PACK_RDP_COMPRESSED_256_COLORS] == True)
        	        {
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_create_window: Using pack method PACK_RDP_COMPRESSED_256_COLORS.\n");
				#endif

				nxdesktopUseNXCompressedRdpImages = True;
        	        }
			else if (nxdesktopPackMethod == PACK_RDP_PLAIN_256_COLORS &&
					methods[PACK_RDP_PLAIN_256_COLORS] == True)
	                {
				fprintf(stderr, "ui_create_window: Using pack method PACK_RDP_PLAIN_256_COLORS.\n");

				nxdesktopUseNXRdpImages = True;
        	        }
			else
			{
				fprintf(stderr, "ui_create_window: WARNING! No available RDP pack method on display '%s'.\n",
						XDisplayName(nxDisplay));
			}
		}

		/*
		 * Inform remote proxy about pixel geometry
		 * to be used to unpack images.
		 */

		if (nxdesktopUseNXCompressedRdpImages ||
			nxdesktopUseNXRdpImages)
		{
			if (NXSetUnpackGeometry(display, 0, screen, visual) == 0)
			{
				fprintf(stderr, "ui_create_window: ERROR! NXSetUnpackGeometry() failed on display '%s'.\n",
						XDisplayName(nxDisplay));

				exit(1);
			}
		}
	}

	pfm = XListPixmapFormats(display, &i);
	if (pfm != NULL)
	{
		/* Use maximum bpp for this depth - this is generally
		   desirable, e.g. 24 bits->32 bits. */
		while (i--)
		{
			if ((pfm[i].depth == depth)
			    && (pfm[i].bits_per_pixel > bpp))
			{
				bpp = pfm[i].bits_per_pixel;
			}
		}
		XFree(pfm);
	}

	if (bpp < 8)
	{
		error("Less than 8 bpp not currently supported.\n");
		XCloseDisplay(display);
		return False;
	}

	if (depth <= 8)
		owncolmap = True;
	else
        {
		xcolmap = DefaultColormapOfScreen(screen);
        }

	test = 1;
	host_be = !(BOOL)(*(uint8 *)(&test));
	xserver_be = (ImageByteOrder(display) == MSBFirst);

	white = WhitePixelOfScreen(screen);
	attribs.background_pixel = BlackPixelOfScreen(screen);
	attribs.backing_store = DoesBackingStore(screen);

/*
FIXME
*/

#ifndef NXDESKTOP_OWNBACK
	if (attribs.backing_store == NotUseful)
#endif
		ownbackstore = True;
/*		ownbackstore = False;
       NXSetExposeEvents(display, False, False, False);
*/
	/*
	 * Always disable NoExpose and GraphicsExpose events.
	 */

        if (nxdesktopUseNXTrans)
           NXSetExposeEvents(display, True, False, False);

     {
	XVisualInfo *nxVisuals;
	XVisualInfo vi;
	int nxNumVisuals;
	long mask;
	XVisualInfo pV;

	nx_depth = DefaultDepth(display, DefaultScreen(display));
	mask = VisualScreenMask;
	vi.screen = DefaultScreen(display);
	nxVisuals = XGetVisualInfo(display, mask, &vi, &nxNumVisuals);
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

	useXpmIcon = getNXIcon(display, &nxIconPixmap, &nxIconShape);
    }

	if (fullscreen || ipaq)
	{
		attribs.override_redirect = True;
	/* Prepare signal handler for SIGUSR1 and SIGUSR2 */
		sigusr_act.sa_handler = sigusr_func;

	/* Install signal handler for SIGUSR1 and SIGUSR2 */
		sigfillset(&(sigusr_act.sa_mask));
		sigaction(SIGUSR1, &sigusr_act, NULL);
		sigaction(SIGUSR2, &sigusr_act, NULL);

		width = WidthOfScreen(screen);
                height = HeightOfScreen(screen);
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
		width = (width + 3) & ~3; /* make width a multiple of 32 bits */
	}
	width = width  & ~3; /* make width a multiple of 32 bits */

        wnd = XCreateWindow(display, RootWindowOfScreen(screen),
			    xo, yo, width, height, 0, CopyFromParent,
			    InputOutput, CopyFromParent,
			    CWBackingStore | CWBackPixel | (fullscreen?CWOverrideRedirect:SubstructureRedirectMask) | StructureNotifyMask,
			    &attribs);

        /*XXX new atom for nxwin */
        {
            Atom IdentityAtom;
            int Type[4];
            Type[0] = NXDESKTOP_SESSION;
            Type[1] = NXDESKTOP_MAJOR_VERSION;
            Type[2] = NXDESKTOP_MINOR_VERSION;
            Type[3] = NXDESKTOP_RELEASE;

            IdentityAtom = XInternAtom(display, "NX_IDENTITY", False);

            XChangeProperty(display,
                            wnd,
                            IdentityAtom,
                            XA_ATOM,
                            sizeof(int)*8,
                            PropModeReplace,
                            (unsigned char*)&Type,
                            4
                           );
        }


	if (fullscreen)
	{
		attribs.override_redirect = False;
/*
		wnd2 = XCreateWindow (display, DefaultRootWindow (display), 0, 0, 1, 1,
				0, CopyFromParent, InputOutput, CopyFromParent,
				CWBackingStore | CWBackPixel , &attribs);
                                */
		wnd2 = XCreateWindow (display, RootWindowOfScreen(screen), 0, 0, 1, 1,
				0, CopyFromParent, InputOutput, CopyFromParent,
				CWBackingStore | CWBackPixel , &attribs);

	}

	XStoreName(display, wnd, (windowName?windowName:"nxdesktop"));

	classhints = XAllocClassHint();
	if (classhints != NULL)
	{
		classhints->res_name = classhints->res_class = "nxdesktop";
		XSetClassHint(display, wnd, classhints);
		XFree(classhints);
	}

	sizehints = XAllocSizeHints();
	if (sizehints)
	{
		sizehints->flags = PPosition | PMinSize | PMaxSize;
		sizehints->min_width = sizehints->max_width = width;
		sizehints->min_height = sizehints->max_height = height;

    		XSetStandardProperties(display,
			   wnd,
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
		XSetWMHints (display, wnd, &wmhints);
	}

	input_mask = KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask;

	 if (fullscreen)
	 	input_mask |= (EnterWindowMask | LeaveWindowMask);

	if (sendmotion)
		input_mask |= PointerMotionMask;

	if (ownbackstore)
		input_mask |= ExposureMask;

	if (fullscreen)
        {

          XStoreName(display, wnd2, (windowName?windowName:"nxdesktop"));

          classhints = XAllocClassHint();
          if (classhints != NULL)
          {
            classhints->res_name = classhints->res_class = "nxdesktop";
            XSetClassHint(display, wnd2, classhints);
            XFree(classhints);
          }

		sizehints->flags = PMinSize | PMaxSize;
		sizehints->min_width = sizehints->max_height = 1;
		sizehints->min_height = sizehints->max_height = 1;
    		XSetStandardProperties(display,
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
		XSetWMHints (display, wnd2, &wmhints);
                if(!ipaq)
		    XMapWindow( display, wnd2 );
	}
	XFree(sizehints);

        XSelectInput (display, wnd, input_mask);

	if (fullscreen)
		XSelectInput (display, wnd2, (input_mask & ~(KeyPressMask | KeyReleaseMask)) | StructureNotifyMask);

#ifdef NXDESKTOP_ONEXIT
  if ((nxdesktop_WM = XInternAtom (display, "WM_PROTOCOLS", True)) != None)
  {
    Atom deleteWMatom = XInternAtom (display, "WM_DELETE_WINDOW", False);
#ifdef NXDESKTOP_ONEXIT_DEBUG
    fprintf(stderr,"WM atom is [%d]\n",deleteWMatom);
#endif
    XSetWMProtocols(display, wnd, &deleteWMatom, 1);
    if (fullscreen)
    	XSetWMProtocols(display, wnd2, &deleteWMatom, 1);
  }
#endif

	gc = XCreateGC(display, wnd, 0, NULL);

	if (ownbackstore)
		backstore = XCreatePixmap(display, wnd, width, height, depth);


        XMapWindow(display, wnd);

        XGrabKeyboard(display, wnd, True, GrabModeAsync, GrabModeAsync, CurrentTime);

#if defined(NXDESKTOP_LOGO) && !defined(NKDESKTOP_SPLASH)
        if (showNXlogo)
        {
          /*
          * if you want some animation, change the initial value to 5 and the sleep to 1
          */
          int i = 1;
          Cursor cursor;
          cursor = XCreateFontCursor(display, 0);
          XDefineCursor(display, wnd, (Cursor)cursor);
          while (i)
          {
            nomachineLogo(wnd, gc, i);
            i--;
            sleep(3);
          }

          XSync(display, True);
        }
#endif
        if(ipaq){
            XWindowChanges ch;
            unsigned int ch_mask;
            ch.stack_mode = Below;
            ch_mask = CWStackMode;
            XConfigureWindow(display, wnd, ch_mask, &ch);
        }

        if (fullscreen)
        {
           XIconifyWindow (display, wnd2, DefaultScreen(display));
/*           XMapWindow(display, wnd2);*/
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
            XDisplayKeycodes(display, &minkey, &maxkey);
            g_mod_map = XGetModifierMapping(display);
            xkeymap_init();
        }
	return True;
}

void
ui_destroy_window()
{
	if (ownbackstore)
		XFreePixmap(display, backstore);

	XFreeGC(display, gc);
	XDestroyWindow(display, wnd);
	if (fullscreen)
	  XDestroyWindow (display, wnd2);
	XCloseDisplay(display);
	display = NULL;
}

static uint8
xwin_translate_key(unsigned long key)
{
	DEBUG(("KEY(code=0x%lx)\n", key));

        if ((key > 8) && (key <= 0x60))
		return (key - 8);

	switch (key)
	{
		case 0x61:	/* home */
			return 0x47 | 0x80;
		case 0x62:	/* up arrow */
			return 0x48 | 0x80;
		case 0x63:	/* page up */
			return 0x49 | 0x80;
		case 0x64:	/* left arrow */
			return 0x4b | 0x80;
		case 0x66:	/* right arrow */
			return 0x4d | 0x80;
		case 0x67:	/* end */
			return 0x4f | 0x80;
		case 0x68:	/* down arrow */
			return 0x50 | 0x80;
		case 0x69:	/* page down */
			return 0x51 | 0x80;
		case 0x6a:	/* insert */
			return 0x52 | 0x80;
		case 0x6b:	/* delete */
			return 0x53 | 0x80;
		case 0x6c:	/* keypad enter */
			return 0x1c | 0x80;
		case 0x6d:	/* right ctrl */
			return 0x1d | 0x80;
		case 0x6f:	/* ctrl - print screen */
			return 0x37 | 0x80;
		case 0x70:	/* keypad '/' */
			return 0x35 | 0x80;
		case 0x71:	/* right alt */
			return 0x38 | 0x80;
		case 0x72:	/* ctrl break */
			return 0x46 | 0x80;
		case 0x73:	/* left window key */
			return 0xff;	/* real scancode is 5b */
		case 0x74:	/* right window key */
			return 0xff;	/* real scancode is 5c */
		case 0x75:	/* menu key */
			return 0x5d | 0x80;
	}

	return 0;
}

static uint16
xwin_translate_mouse(unsigned long button)
{
	switch (button)
	{
		case Button1:	/* left */
			return MOUSE_FLAG_BUTTON1;
		case Button2:	/* middle */
			return MOUSE_FLAG_BUTTON3;
		case Button3:	/* right */
			return MOUSE_FLAG_BUTTON2;
	}

	return 0;
}

static void
xwin_process_events()
{
	XEvent event;
	uint8 scancode;
	uint16 button;
        key_translation tr;
        KeySym keysym;
        char str[256];

        /* used only to send RDP inputs for which we can't obtain the real X server time */
	uint32 ev_time = (last_Xtime == 0) ? (uint32)time(NULL): last_Xtime;

	if (display == NULL)
		return;

	if (XCheckTypedEvent(display, ClientMessage, &event))
	{
          if (event.xclient.message_type == nxdesktop_WM && nxdesktop_WM != None)
          {
		Atom wmAtom, deleteWMatom;
		wmAtom = (Atom) event.xclient.data.l[0];
		deleteWMatom = XInternAtom (display, "WM_DELETE_WINDOW", True);
		if (wmAtom == deleteWMatom)
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

		     if (fullscreen)
		         sigusr_func(SIGUSR2);
		}
	  }
#ifdef NXDESKTOP_USES_NXKARMA_IN_LOOP
          if (event.xclient.window == 0 &&
              event.xclient.message_type == 0 &&
              event.xclient.format == 32 &&
              (int) event.xclient.data.l[0] == NXSyncNotify/*NXKarmaNotify*/)
          {
             nxdesktopSleep = False;
#ifdef NXDESKTOP_NXKARMA_DEBUG
             fprintf(stderr," NXKarma wake up arrived!\n");
#endif
          }
#endif
	}

	if (fullscreen)
		while (XCheckWindowEvent (display, wnd2, ~0, &event))
		{
			switch (event.type)
			{
				case MapNotify:
					sigusr_func(SIGUSR2);
					break;
			}
		}

	while (XCheckWindowEvent(display, wnd, ~0, &event))
	{
		ev_time = time(NULL);

		switch (event.type)
		{
			case KeyPress:
                        {
				XLookupString((XKeyEvent *) & event,
						      str, sizeof(str), &keysym, NULL);

				DEBUG_KBD(("\nKeyPress for (keysym 0x%lx, %s)\n", keysym,
					   get_ksname(keysym)));

				if (handle_special_keys(keysym, event.xkey.state, ev_time, False))
					break;

				tr = xkeymap_translate_key(keysym,
							   event.xkey.keycode, event.xkey.state);

				scancode = tr.scancode;

				if ( (event.xkey.keycode == 130) && ipaq )
				{
					kill (pidkbd, 1);
					/* fprintf(stderr,"signal send -HUP\n"); */
					break;
				}
                                last_Xtime = event.xkey.time;

				if (scancode == 0)
					break;


			/* When in fullscreen, unmap window on Ctrl-Alt-Escape */
				if ((scancode == 0x01) &&
					(event.xkey.state & (ControlMask | Mod1Mask )) ==
					(ControlMask|Mod1Mask)  )
				{
					DEBUG (("Ctrl-Alt-ESC pressed\n"));
					if (magickey && fullscreen)
					{
					   if (nxdesktop_WM != None)
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
				save_remote_modifiers(tr.scancode);
				ensure_remote_modifiers(ev_time, tr);
				rdp_send_scancode(ev_time, RDP_KEYPRESS, tr.scancode);
				restore_remote_modifiers(ev_time, tr.scancode);

/*				rdp_send_input(event.xkey.time, RDP_INPUT_SCANCODE, 0,
					       scancode, 0);
*/
                                PressedKeys[scancode] = 1;
				break;
                        }
			case KeyRelease:
                        
				XLookupString((XKeyEvent *) & event, str,
					      sizeof(str), &keysym, NULL);

				DEBUG_KBD(("\nKeyRelease for (keysym 0x%lx, %s)\n", keysym,
					   get_ksname(keysym)));

				ev_time = time(NULL);
				if (handle_special_keys(keysym, event.xkey.state, ev_time, False))
					break;

				tr = xkeymap_translate_key(keysym,
							   event.xkey.keycode, event.xkey.state);

				if (tr.scancode == 0)
					break;

				scancode = tr.scancode;

                                last_Xtime = event.xkey.time;
				if (scancode == 0)
					break;

				rdp_send_scancode(ev_time, RDP_KEYRELEASE, tr.scancode);
/*
                                rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE,
					       KBD_FLAG_DOWN | KBD_FLAG_UP,
					       scancode, 0);
*/
                                PressedKeys[scancode] = 0;

				break;

			case ButtonPress:
				button = xwin_translate_mouse(event.xbutton.button);
                                last_Xtime = event.xbutton.time;
				if (button == 0)
					break;


			#ifdef MAGIC_PIXEL
			         /* Iconify rdesktop when left-clicking in lower right corner */
				if ((event.xbutton.button == 1) &&
					(event.xbutton.x >= (width - 2)) && (event.xbutton.y <= 2))
				{
					if (fullscreen)
					{
					   if (nxdesktop_WM != None)
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
				rdp_send_input(last_Xtime, RDP_INPUT_MOUSE,
					       button | MOUSE_FLAG_DOWN,
					       event.xbutton.x,
					       event.xbutton.y);
				break;

			case ButtonRelease:
				button = xwin_translate_mouse(event.xbutton.button);
                                last_Xtime = event.xbutton.time;
				if (button == 0)
					break;

                                rdp_send_input(last_Xtime, RDP_INPUT_MOUSE,
					       button,
					       event.xbutton.x,
					       event.xbutton.y);
				break;

			case MotionNotify:
                                last_Xtime = event.xmotion.time;
				rdp_send_input(last_Xtime, RDP_INPUT_MOUSE,
					       MOUSE_FLAG_MOVE,
					       event.xmotion.x,
					       event.xmotion.y);
				break;

			case EnterNotify:
                                last_Xtime = event.xcrossing.time;
				XGrabKeyboard(display, wnd, True, GrabModeAsync,
					      GrabModeAsync, CurrentTime);
				break;

			case LeaveNotify:
				{
				  int i;
                                  last_Xtime = event.xcrossing.time;
				  for (i = 0; i < 256; i++)
				  if (PressedKeys[i])
				     rdp_send_input(last_Xtime, RDP_INPUT_SCANCODE,
					    	KBD_FLAG_DOWN | KBD_FLAG_UP, i, 0);
				}
                                if(ipaq)
                                   break;
				XUngrabKeyboard(display, CurrentTime);


				break;

			case Expose:
				if (ownbackstore)
					XCopyArea(display, backstore, wnd, gc,
						  event.xexpose.x, event.xexpose.y,
					  	  event.xexpose.width, event.xexpose.height,
					  	  event.xexpose.x, event.xexpose.y);
				break;

		}
	}
}

void
ui_select(int rdp_socket, BOOL needKarma)
{
	int n = (rdp_socket > x_socket) ? rdp_socket+1 : x_socket+1;
	fd_set rfds;

	/*
	 * Be sure we get all events from X server.
	 */

	#ifdef NXDESKTOP_XWIN_USES_SYNC_IN_LOOP
	XSync(display, False);
	#else
	#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
        XFlush(display);
	#endif
	#endif

#ifdef NXDESKTOP_USES_NXKARMA_IN_LOOP
        if (nxdesktopUseNXTrans && needKarma)
        {
/*          NXKarma(display, 0);*/
/*          NXSync(display, NXSyncFlush, 0);*/

            NXSync(display, NXSyncWaitForLink, 0);

           nxdesktopSleep = True;
#ifdef NXDESKTOP_NXKARMA_DEBUG
           fprintf(stderr," NXKarma sent, waiting for wakeup...\n");
#endif
           while (nxdesktopSleep)
           {
              XEvent x_event;
              if (XPeekEvent(display, &x_event))
                xwin_process_events();
           }
        }
#endif
        if (needKarma)
          return;

	FD_ZERO(&rfds);

        while (True)
	{
		/*
		 * Check if we left events in queue.
		 */

		xwin_process_events();

		FD_ZERO(&rfds);
		FD_SET(rdp_socket, &rfds);
		FD_SET(x_socket, &rfds);

		switch (select(n, &rfds, NULL, NULL, NULL))
		{
			case -1:
				error("select: %s\n", strerror(errno));

			case 0:
				continue;
		}

		if (FD_ISSET(x_socket, &rfds))
			xwin_process_events();


		if (FD_ISSET(rdp_socket, &rfds))
			return;
	}
}

void
ui_move_pointer(int x, int y)
{
	XWarpPointer(display, wnd, wnd, 0, 0, 0, 0, x, y);
}

HBITMAP
ui_create_bitmap(int width, int height, uint8 *data)
{
	XImage *image;
	Pixmap bitmap;
	uint8 *tdata;

	tdata = (owncolmap ? data : translate_image(width, height, data));
	bitmap = XCreatePixmap(display, wnd, width, height, depth);
	image = XCreateImage(display, visual, depth, ZPixmap,
			     0, tdata, width, height, 8, 0);

	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_create_bitmap: XPutImage on pixmap %d,%d,%d,%d.\n",
			0, 0, width, height);
	#endif

	XPutImage(display, bitmap, gc, image, 0, 0, 0, 0, width, height);

	XFree(image);
	if (!owncolmap)
		xfree(tdata);
	return (HBITMAP) bitmap;
}

void
ui_paint_bitmap(int x, int y, int cx, int cy,
		int width, int height, uint8 *data)
{
	XImage *image;
	uint8 *tdata;

	#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES

	if (nxdesktopUseNXRdpImages)
	{
		int data_length;

		tdata = data;
		data_length = width * height;

		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_bitmap: NXCreatePackedImage with owncolmap %d and depth %d.\n",
				owncolmap, depth);
		#endif

		image = NXCreatePackedImage(display, visual, PACK_RDP_PLAIN_256_COLORS,
						depth, ZPixmap, tdata, data_length,
							width, height, BitmapPad(display), 0);

		if (ownbackstore)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_paint_bitmap: NXPutPackedImage on backingstore pixmap %d,%d,%d,%d (%d,%d).\n",
					x, y, cx, cy, width, height);
			#endif

			NXPutPackedImage(display, 0, backstore, gc, image,
						PACK_RDP_PLAIN_256_COLORS,
							depth, 0, 0, x, y, cx, cy);

			XCopyArea(display, backstore, wnd, gc, x, y, cx, cy, x, y);

			#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
			XFlush(display);
			#endif
		}
		else
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_paint_bitmap: NXPutPackedImage on window %d,%d,%d,%d (%d,%d).\n",
					x, y, cx, cy, width, height);
			#endif

			NXPutPackedImage(display, 0, wnd, gc, image,
						PACK_RDP_PLAIN_256_COLORS,
							depth, 0, 0, x, y, cx, cy);
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

		tdata = (owncolmap ? data : translate_image(width, height, data));

		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_bitmap: XCreateImage with owncolmap %d and depth %d.\n",
				owncolmap, depth);
		#endif

		image = XCreateImage(display, visual, depth, ZPixmap,
					0, tdata, width, height, 8, 0);

		if (ownbackstore)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_paint_bitmap: XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
					x, y, cx, cy);
			#endif

			XPutImage(display, backstore, gc, image, 0, 0, x, y, cx, cy);

			XCopyArea(display, backstore, wnd, gc, x, y, cx, cy, x, y);
		}
		else
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_paint_bitmap: XPutImage on window %d,%d,%d,%d.\n",
					x, y, cx, cy);
			#endif

			XPutImage(display, wnd, gc, image, 0, 0, x, y, cx, cy);
		}

		XFree(image);

		if (!owncolmap)
			xfree(tdata);

	}

	#else /* NXDESKTOP_XWIN_USES_PACKED_IMAGES */

	tdata = (owncolmap ? data : translate_image(width, height, data));

	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_paint_bitmap: XCreateImage with owncolmap %d and depth %d.\n",
			owncolmap, depth);
	#endif

	image = XCreateImage(display, visual, depth, ZPixmap,
				0, tdata, width, height, 8, 0);

	if (ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_bitmap: XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif

		XPutImage(display, backstore, gc, image, 0, 0, x, y, cx, cy);

		XCopyArea(display, backstore, wnd, gc, x, y, cx, cy, x, y);
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_bitmap: XPutImage on window %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif

		XPutImage(display, wnd, gc, image, 0, 0, x, y, cx, cy);
	}

	XFree(image);

	if (!owncolmap)
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
			depth, compressed_size);
	#endif

	image = NXCreatePackedImage(display, visual, PACK_RDP_COMPRESSED_256_COLORS,
					depth, ZPixmap, compressed_data, compressed_size,
						width, height, BitmapPad(display), 0);

	if (image == NULL)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_compressed_bitmap: NXCreatePackedImage failure.\n");
		#endif

		return;
	}


	if (ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_compressed_bitmap: NXPutPackedImage on backingstore pixmap %d,%d,%d,%d (%d,%d).\n",
				x, y, cx, cy, width, height);
		#endif

		NXPutPackedImage(display, 0, backstore, gc, image,
					PACK_RDP_COMPRESSED_256_COLORS,
						depth, 0, 0, x, y, cx, cy);

		XCopyArea(display, backstore, wnd, gc, x, y, cx, cy, x, y);

		#ifdef NXDESKTOP_XWIN_USES_FLUSH_IN_LOOP
		XFlush(display);
		#endif
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_paint_compressed_bitmap: NXPutPackedImage on window %d,%d,%d,%d (%d,%d).\n",
				x, y, cx, cy, width, height);
		#endif

		NXPutPackedImage(display, 0, wnd, gc, image,
					PACK_RDP_COMPRESSED_256_COLORS,
						depth, 0, 0, x, y, cx, cy);
	}

	/*
	 * NXDestroyPackedImage(), like XDestroyImage()
	 * would deallocate both image and data, that's
	 * not what we want.
	 */

	XFree(image);
}

#endif

void
ui_destroy_bitmap(HBITMAP bmp)
{
	XFreePixmap(display, (Pixmap)bmp);
}

HGLYPH
ui_create_glyph(int width, int height, uint8 *data)
{
	XImage *image;
	Pixmap bitmap;
	int scanline;
	GC gc;

	scanline = (width + 7) / 8;

	bitmap = XCreatePixmap(display, wnd, width, height, 1);
	gc = XCreateGC(display, bitmap, 0, NULL);

	image = XCreateImage(display, visual, 1, ZPixmap, 0,
			     data, width, height, 8, scanline);
	image->byte_order = MSBFirst;
	image->bitmap_bit_order = MSBFirst;
	XInitImage(image);

	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_create_glyph: XPutImage on pixmap %d,%d,%d,%d.\n",
			0, 0, width, height);
	#endif

	XPutImage(display, bitmap, gc, image, 0, 0, 0, 0, width, height);

	XFree(image);
	XFreeGC(display, gc);
	return (HGLYPH)bitmap;
}

void
ui_destroy_glyph(HGLYPH glyph)
{
	XFreePixmap(display, (Pixmap)glyph);
}

HCURSOR
ui_create_cursor(unsigned int x, unsigned int y, int width,
		 int height, uint8 *andmask, uint8 *xormask)
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

	cursor = xmalloc(offset);
	memset(cursor, 0, offset);

	mask = xmalloc(offset);
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

	xcursor = XCreatePixmapCursor(display, (Pixmap)cursorglyph,
				(Pixmap)maskglyph, &fg, &bg, x, y);

	ui_destroy_glyph(maskglyph);
	ui_destroy_glyph(cursorglyph);
	xfree(mask);
	xfree(cursor);
	return (HCURSOR)xcursor;
}

void
ui_set_cursor(HCURSOR cursor)
{
        XDefineCursor(display, wnd, (Cursor)cursor);
}

void
ui_destroy_cursor(HCURSOR cursor)
{
	XFreeCursor(display, (Cursor)cursor);
}

#define MAKE_XCOLOR(xc,c) \
		(xc)->red   = ((c)->red   << 8) | (c)->red; \
		(xc)->green = ((c)->green << 8) | (c)->green; \
		(xc)->blue  = ((c)->blue  << 8) | (c)->blue; \
		(xc)->flags = DoRed | DoGreen | DoBlue;

HCOLOURMAP
ui_create_colourmap(COLOURMAP *colours)
{
	COLOURENTRY *entry;
	int i, ncolours = colours->ncolours;

#ifdef NKDESKTOP_ONSTART
        if (showNXlogo)
        {
           setOwnerNX_WM(wnd);
           XSync(display, True);
        }
#endif
	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_create_colourmap: Creating new colormap.\n");
	#endif

	if (owncolmap)
	{
		/*
		 * Own colormap is only used when depth is <= 8,
		 * it then only store all colors with a single
		 * request. Never mind.
		 */

       		XColor *xcolours, *xentry;
		Colormap map;

		xcolours = xmalloc(sizeof(XColor) * ncolours);
		for (i = 0; i < ncolours; i++)
		{
			entry = &colours->colours[i];
			xentry = &xcolours[i];
			xentry->pixel = i;
			MAKE_XCOLOR(xentry, entry);
		}

		map = XCreateColormap(display, wnd, visual, AllocAll);
		XStoreColors(display, map, xcolours, ncolours);

		xfree(xcolours);

		#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES

		last_colormap_entries = 0;
		last_colormap = NULL;

		#endif

		return (HCOLOURMAP)map;
	}
	else
	{
		/*
		 * This used to generate >768 X_AllocColor requests and
		 * replies. By its own it caused desktop to take ~60 to
		 * start, and other ~60 second at each change of palette.
		 * Fixed by adding a new request/reply: NXAllocColors.
		 */

		uint32 *map = xmalloc(sizeof(*colmap) * ncolours);
		XColor xentry[ncolours];
		uint32 colour;

		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_create_colourmap: Created colormap at %p with %d entries and size %d.\n",
				map, ncolours, sizeof(*colmap) * ncolours);
		#endif

		for (i = 0; i < ncolours; i++)
		{
			entry = &colours->colours[i];
			MAKE_XCOLOR(&(xentry[i]), entry);
		}

		if (NXAllocColors(display, xcolmap, ncolours, xentry) == 0)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_create_colourmap: ERROR! Failed to allocate all requested colors.\n");
			#endif
		}


		for (i = 0; i < ncolours; i++)
		{
			colour = xentry[i].pixel;

			/*
			 * Byte swap here to make translate_image faster.
			 */

                        map[i] = translate_colour(colour);

}

		#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES

		last_colormap_entries = ncolours;
		last_colormap = map;

		#endif

		return map;
	}
}

void
ui_destroy_colourmap(HCOLOURMAP map)
{
	if (owncolmap)
		XFreeColormap(display, (Colormap)map);
	else
		xfree(map);

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

	if (owncolmap)
		XSetWindowColormap(display, wnd, (Colormap)map);
	else
		colmap = map;


	#ifdef NXDESKTOP_XWIN_USES_PACKED_IMAGES

	if (nxdesktopUseNXTrans)
	{
		if (owncolmap == 0 &&
			last_colormap_entries != 0 &&
				last_colormap != NULL)
		{
			int i;

                        if (host_be != xserver_be && (nx_depth == 16 || nx_depth == 15))
                        {
                          unsigned int *swap_colormap = xmalloc(sizeof(unsigned int) * last_colormap_entries);

			  for (i = 0; i < last_colormap_entries; i++)
			  {
                            swap_colormap[i] = last_colormap[i] << 16;
                          }
			  NXSetUnpackColormap(display, 0, last_colormap_entries, swap_colormap);
                          xfree(swap_colormap);
                        }
                        else
                        {
			  NXSetUnpackColormap(display, 0, last_colormap_entries, last_colormap);
                        }


			#ifdef NXDESKTOP_XWIN_DUMP

			fprintf(stderr, "ui_set_colourmap: Dumping colormap entries:\n");

			for (i = 0; i < last_colormap_entries; i++)
			{
				fprintf(stderr, "ui_set_colourmap: [%d] [%p].\n",
						i, (void *) last_colormap[i]);
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
	XSetClipRectangles(display, gc, 0, 0, &rect, 1, YXBanded);
}

void
ui_reset_clip()
{
	XRectangle rect;

	rect.x = 0;
	rect.y = 0;
	rect.width = width;
	rect.height = height;
        XSetClipRectangles(display, gc, 0, 0, &rect, 1, YXBanded);
}

void
ui_bell()
{
	XBell(display, 0);
}

void
ui_destblt(uint8 opcode,
	   /* dest */ int x, int y, int cx, int cy)
{
	SET_FUNCTION(opcode);
	FILL_RECTANGLE(x, y, cx, cy);
	RESET_FUNCTION(opcode);
}

void
ui_patblt(uint8 opcode,
	  /* dest */ int x, int y, int cx, int cy,
	  /* brush */ BRUSH *brush, int bgcolour, int fgcolour)
{
	Pixmap fill;

	SET_FUNCTION(opcode);

	switch (brush->style)
	{
		case 0:	/* Solid */
			SET_FOREGROUND(fgcolour);
			FILL_RECTANGLE(x, y, cx, cy);
			break;

		case 3:	/* Pattern */
			fill = (Pixmap)ui_create_glyph(8, 8, brush->pattern);

			SET_FOREGROUND(bgcolour);
			SET_BACKGROUND(fgcolour);
			XSetFillStyle(display, gc, FillOpaqueStippled);
			XSetStipple(display, gc, fill);
			XSetTSOrigin(display, gc, brush->xorigin, brush->yorigin);

			FILL_RECTANGLE(x, y, cx, cy);

			XSetFillStyle(display, gc, FillSolid);
			ui_destroy_glyph((HGLYPH)fill);
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
	XCopyArea(display, wnd, wnd, gc, srcx, srcy, cx, cy, x, y);
	if (ownbackstore)
		XCopyArea(display, backstore, backstore, gc, srcx, srcy,
			  cx, cy, x, y);
	RESET_FUNCTION(opcode);
}

void
ui_memblt(uint8 opcode,
	  /* dest */ int x, int y, int cx, int cy,
	  /* src */ HBITMAP src, int srcx, int srcy)
{
	SET_FUNCTION(opcode);
	XCopyArea(display, (Pixmap)src, wnd, gc, srcx, srcy, cx, cy, x, y);
	if (ownbackstore)
		XCopyArea(display, (Pixmap)src, backstore, gc, srcx, srcy,
			  cx, cy, x, y);
	RESET_FUNCTION(opcode);
}

void
ui_triblt(uint8 opcode,
	  /* dest */ int x, int y, int cx, int cy,
	  /* src */ HBITMAP src, int srcx, int srcy,
	  /* brush */ BRUSH *brush, int bgcolour, int fgcolour)
{
	/* This is potentially difficult to do in general. Until someone
	   comes up with a more efficient way of doing it I am using cases. */

	switch (opcode)
	{
		case 0x69:	/* PDSxxn */
			ui_memblt(ROP2_XOR, x, y, cx, cy, src, srcx, srcy);
			ui_patblt(ROP2_NXOR, x, y, cx, cy,
				  brush, bgcolour, fgcolour);
			break;

		case 0xb8:	/* PSDPxax */
			ui_patblt(ROP2_XOR, x, y, cx, cy,
				  brush, bgcolour, fgcolour);
			ui_memblt(ROP2_AND, x, y, cx, cy, src, srcx, srcy);
			ui_patblt(ROP2_XOR, x, y, cx, cy,
				  brush, bgcolour, fgcolour);
			break;

		case 0xc0:	/* PSa */
			ui_memblt(ROP2_COPY, x, y, cx, cy, src, srcx, srcy);
			ui_patblt(ROP2_AND, x, y, cx, cy, brush, bgcolour,
				  fgcolour);
			break;

		default:
			unimpl("triblt 0x%x\n", opcode);
			ui_memblt(ROP2_COPY, x, y, cx, cy, src, srcx, srcy);
	}
}

void
ui_line(uint8 opcode,
	/* dest */ int startx, int starty, int endx, int endy,
	/* pen */ PEN *pen)
{
	SET_FUNCTION(opcode);
	SET_FOREGROUND(pen->colour);

	XDrawLine(display, wnd, gc, startx, starty, endx, endy);
	if (ownbackstore)
		XDrawLine(display, backstore, gc, startx, starty, endx, endy);
	RESET_FUNCTION(opcode);
}


void
ui_poly_line(uint8 opcode, short *points, int count,
	/* pen */ PEN *pen)
{
	SET_FUNCTION(opcode);
	SET_FOREGROUND(pen->colour);

	XDrawLines(display, wnd, gc, (XPoint*)points, count, CoordModeOrigin);
	if (ownbackstore)
        	XDrawLines(display, backstore, gc, (XPoint*)points, count, CoordModeOrigin);
	RESET_FUNCTION(opcode);
}



void
ui_rect(
	       /* dest */ int x, int y, int cx, int cy,
	       /* brush */ int colour)
{
	SET_FOREGROUND(colour);
	FILL_RECTANGLE(x, y, cx, cy);
}

void
ui_draw_glyph(int mixmode,
	      /* dest */ int x, int y, int cx, int cy,
	      /* src */ HGLYPH glyph, int srcx, int srcy, int bgcolour,
	      int fgcolour)
{
	SET_FOREGROUND(fgcolour);
	SET_BACKGROUND(bgcolour);

	XSetFillStyle(display, gc, (mixmode == MIX_TRANSPARENT)
		      ? FillStippled : FillOpaqueStippled);
	XSetStipple(display, gc, (Pixmap)glyph);
	XSetTSOrigin(display, gc, x, y);

	FILL_RECTANGLE(x, y, cx, cy);

	XSetFillStyle(display, gc, FillSolid);
}

void
ui_draw_text(uint8 font, uint8 flags, int mixmode, int x, int y,
	     int clipx, int clipy, int clipcx, int clipcy,
	     int boxx, int boxy, int boxcx, int boxcy,
	     int bgcolour, int fgcolour, uint8 *text, uint8 length)
{
	FONTGLYPH *glyph;
	int i, offset;

	if (boxcx > 1)
	{
	        SET_FOREGROUND(bgcolour);
		FILL_RECTANGLE(boxx, boxy, boxcx, boxcy);
	}
	else if (mixmode == MIX_OPAQUE)
	{
	        SET_FOREGROUND(bgcolour);
		FILL_RECTANGLE(clipx, clipy, clipcx, clipcy);
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
				offset = text[++i];
				if (offset & 0x80)
					offset = ((offset & 0x7f) << 8) | text[++i];

				if (flags & TEXT2_VERTICAL)
					y += offset;
				else
					x += offset;
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

		image = NXEncodeRDPText(display, TRANSLATE(bgcolour), TRANSLATE(fgcolour),
						(mixmode == MIX_TRANSPARENT ? FillStippled :
							FillOpaqueStippled), &rdp_text[0], elements);

		if (image)
		{
			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_draw_text: Using packed image with drawable [0x%lx] and gc [0x%lx].\n",
					wnd, ((GC) gc) -> gid);
			#endif

			NXPutPackedImage(display, 0, (ownbackstore) ? backstore : wnd, 
				         gc, image, PACK_RDP_TEXT,
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
		XSetFillStyle(display, gc, FillStippled);
		XSetFillStyle(display, gc, FillSolid);

		if (ownbackstore)
		{
			if (boxcx > 1)
			{
				XCopyArea(display, backstore, wnd, gc, boxx, boxy,
						boxcx, boxcy, boxx, boxy);
			}
			else /* if (mixmode == MIX_OPAQUE) */
			{
				XCopyArea(display, backstore, wnd, gc, clipx, clipy,
						clipcx, clipcy, clipx, clipy);
			}
		}
	}
	else
	{

#endif /* NXWIN_USES_PACKED_RDP_TEXT */

        SET_FOREGROUND(fgcolour);
	SET_BACKGROUND(bgcolour);
	XSetFillStyle(display, gc, (mixmode == MIX_TRANSPARENT)
		      ? FillStippled : FillOpaqueStippled);

	/* Paint text, character by character */
	for (i = 0; i < length; i++)
	{
		glyph = cache_get_font(font, text[i]);

		if (!(flags & TEXT2_IMPLICIT_X))
		{
			offset = text[++i];
			if (offset & 0x80)
				offset = ((offset & 0x7f) << 8) | text[++i];

			if (flags & TEXT2_VERTICAL)
				y += offset;
			else
				x += offset;
		}

		if (glyph != NULL)
		{
                   XGCValues values;
                   memset(&values, 0, sizeof(XGCValues));
                   values.stipple = (Pixmap)glyph->pixmap;
                   values.ts_x_origin = x + (short) glyph->offset;
                   values.ts_y_origin = y + (short) glyph->baseline;
                   XChangeGC(display, gc, GCStipple | GCTileStipXOrigin | GCTileStipYOrigin, &values);

/*
                	XSetStipple(display, gc, (Pixmap)glyph->pixmap);
                        XSetTSOrigin(display, gc, x + (short) glyph->offset, y + (short) glyph->baseline);
*/
                	FILL_RECTANGLE(x + (short)glyph->offset, y + (short)glyph->baseline,
                                       glyph->width, glyph->height);


/*
			ui_draw_glyph(mixmode, x + (short) glyph->offset,
				      y + (short) glyph->baseline,
				      glyph->width, glyph->height,
				      glyph->pixmap, 0, 0,
				      bgcolour, fgcolour);
*/
			if (flags & TEXT2_IMPLICIT_X)
				x += glyph->width;
		}
	}
	XSetFillStyle(display, gc, FillSolid);
#ifdef NXWIN_USES_PACKED_RDP_TEXT
     }
#endif
}

void
ui_desktop_save(uint32 offset, int x, int y, int cx, int cy)
{
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
				XFreePixmap(display, pixmap_cache[i].pixmap);

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
				XFreePixmap(display, pixmap_cache[i].pixmap);
			}

			cache = XCreatePixmap(display, wnd, width, height, depth);

			pixmap_cache[i].pixmap = cache;
			pixmap_cache[i].offset = offset;

			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_desktop_save: Saved area in pixmap cache [%lx] index [%d].\n",
					cache, i);
			#endif

			if (ownbackstore)
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_desktop_save: XCopyArea from backingstore to pixmap cache %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(display, backstore, cache, gc, x, y, cx, cy, 0, 0);
			}
			else
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_desktop_save: XCopyArea from window to pixmap cache %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(display, wnd, cache, gc, x, y, cx, cy, 0, 0);
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

	if (ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_desktop_save: XGetImage from backingstore pixmap %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif

		image = XGetImage(display, backstore, x, y, cx, cy, AllPlanes,
				  ZPixmap);
	}
	else
	{
		pix = XCreatePixmap(display, wnd, cx, cy, depth);
		XCopyArea(display, wnd, pix, gc, x, y, cx, cy, 0, 0);
		image = XGetImage(display, pix, 0, 0, cx, cy, AllPlanes,
				  ZPixmap);
		XFreePixmap(display, pix);
	}

	offset *= bpp/8;
	cache_put_desktop(offset, cx, cy, image->bytes_per_line,
			  bpp/8, (uint8 *)image->data);

	XDestroyImage(image);

	#endif
}

void
ui_desktop_restore(uint32 offset, int x, int y, int cx, int cy)
{
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

			if (ownbackstore)
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_desktop_restore: XCopyArea from pixmap cache to backingstore %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(display, cache, backstore, gc, 0, 0, cx, cy, x, y);
				XCopyArea(display, backstore, wnd, gc, x, y, cx, cy, x, y);
			}
			else
			{
				#ifdef NXDESKTOP_XWIN_DEBUG
				fprintf(stderr, "ui_desktop_restore: XCopyArea from pixmap cache to window %d,%d,%d,%d.\n",
						x, y, cx, cy);
				#endif

				XCopyArea(display, cache, wnd, gc, 0, 0, cx, cy, x, y);
			}

			#ifdef NXDESKTOP_XWIN_DEBUG
			fprintf(stderr, "ui_desktop_restore: Restored area from pixmap cache [%lx] index [%d].\n",
					cache, i);
			#endif


			XFreePixmap(display, cache);

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

	XImage *image;
	uint8 *data;

	#ifdef NXDESKTOP_XWIN_DEBUG
	fprintf(stderr, "ui_desktop_restore: Called with offset [%d]. Not using pixmap cache.\n",
			offset);
	#endif

	offset *= bpp/8;
	data = cache_get_desktop(offset, cx, cy, bpp/8);
	if (data == NULL)
		return;

	image = XCreateImage(display, visual, depth, ZPixmap,
			     0, data, cx, cy, BitmapPad(display),
			     cx * bpp/8);

	if (ownbackstore)
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_desktop_restore: XPutImage on backingstore pixmap %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif

		XPutImage(display, backstore, gc, image, 0, 0, x, y, cx, cy);
		XCopyArea(display, backstore, wnd, gc, x, y, cx, cy, x, y);
	}
	else
	{
		#ifdef NXDESKTOP_XWIN_DEBUG
		fprintf(stderr, "ui_desktop_restore: XPutImage on window %d,%d,%d,%d.\n",
				x, y, cx, cy);
		#endif

		XPutImage(display, wnd, gc, image, 0, 0, x, y, cx, cy);
	}

	XFree(image);

	#endif
}


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

    w = width/scale;
    h = height/scale;
    w2 = w/2;
    h2 = h/2;
    if (height > width)
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

    XSetFunction(display, gc, GXcopy);
    XSetFillStyle(display, gc, FillSolid);
    XSetForeground(display, gc, nx_black);
    XSetBackground(display, gc, nx_red);

    XFillPolygon(display, win, gc, rect, 4, Convex, CoordModeOrigin);

#ifdef NXDESKTOP_LOGO_DEBUG
    fprintf(stderr,"filled first poly\n");
#endif

    XSetForeground(display, gc, nx_red);
    XSetBackground(display, gc, nx_white);

    rect[0].x = w2-10*c;	       rect[0].y = h2-8*c;
    rect[1].x = w2-10*c;	       rect[1].y = h2+8*c;
    rect[2].x = w2+10*c;	       rect[2].y = h2+8*c;
    rect[3].x = w2+10*c;	       rect[3].y = h2-8*c;

    XFillPolygon(display, win, gc, rect, 4, Convex, CoordModeOrigin);

#ifdef NXDESKTOP_LOGO_DEBUG
    fprintf(stderr,"filled red rect\n");
#endif

    rect[0].x = w2-9*c;	       rect[0].y = h2-7*c;
    rect[1].x = w2-9*c;	       rect[1].y = h2+7*c;
    rect[2].x = w2+9*c;	       rect[2].y = h2+7*c;
    rect[3].x = w2+9*c;	       rect[3].y = h2-7*c;

    XSetForeground(display, gc, nx_white);
    XSetBackground(display, gc, nx_red);

    XFillPolygon(display, win, gc, rect, 4, Convex, CoordModeOrigin);

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

    XSetForeground(display, gc, nx_red);
    XSetBackground(display, gc, nx_white);

    XFillPolygon(display, win, gc, m, 12, Nonconvex, CoordModeOrigin);

    /* end M */

    /* begin ! */
    rect[0].x = w2-7*c;	       rect[0].y = h2-5*c;
    rect[1].x = w2-5*c;	       rect[1].y = h2-5*c;
    rect[2].x = w2-5*c;	       rect[2].y = h2+2*c;
    rect[3].x = w2-7*c;	       rect[3].y = h2+2*c;

    XFillPolygon(display, win, gc, rect, 4, Convex, CoordModeOrigin);

    rect[0].x = w2-7*c;	       rect[0].y = h2+3*c;
    rect[1].x = w2-5*c;	       rect[1].y = h2+3*c;
    rect[2].x = w2-5*c;	       rect[2].y = h2+5*c;
    rect[3].x = w2-7*c;	       rect[3].y = h2+5*c;

    XFillPolygon(display, win, gc, rect, 4, Convex, CoordModeOrigin);

/*    XFlush(display);*/
    XSync(display, True);

#ifdef NXDESKTOP_LOGO_DEBUG
    fprintf(stderr,"nomachineLogo: end\n");
#endif
}
#endif

#ifdef NXDESKTOP_ONSTART
void setOwnerNX_WM(Window win)
{
  XSetSelectionOwner(display, nxdesktop_WM_START, win, CurrentTime);
  showNXlogo = False;
}
#endif

void nxdesktopSetAtoms()
{
   /*
    * Set NX clipboard Atom. Used to find
    * out if there is any agent running.
    */

    nxdesktop_NX = XInternAtom (display, "NX_CUT_BUFFER_SERVER", False);

    XSetSelectionOwner(display, nxdesktop_NX, wnd, CurrentTime);

    /*
     * Create Window manager ready atom, not so usefull here, but in the future ...
     *
     */
#ifdef NXDESKTOP_ONSTART
    nxdesktop_WM_START = XInternAtom (display, "WM_NX_READY", False);
#endif
}


Bool getNXIcon(Display *display, Pixmap *nxIcon, Pixmap *nxMask)
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
     status = XpmReadFileToPixmap(display,
		  	       DefaultRootWindow(display),
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
     IconPixmap = XCreatePixmapFromBitmapData(display,
					DefaultRootWindow(display),
					(char *)icon_bits,
					icon_width,
					icon_height,
                                        nx_red,
                                        nx_white,
					DefaultDepth(display, DefaultScreen(display)));
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
  
    kbddisplay_len = strlen(DisplayString(display));
    strncpy(temp_string,"",256);
    strncpy(kbddisplay,"",256);
    strcpy(kbddisplay,"localhost");
    strncpy(temp_string, DisplayString(display), kbddisplay_len);
    for (kbd_i=0; kbd_i<7; kbd_i++) kbddisplay[kbd_i+9] = temp_string[kbddisplay_len - 7 + kbd_i];
  
    kbdargs[0] = "nxkbd";
    kbdargs[1] = "-geometry";
    kbdargs[2] = "240x70+0+250";
    kbdargs[3] = "-display";
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

