/* -*- c-basic-offset: 8 -*-
   rdesktop: A Remote Desktop Protocol client.
   Entrypoint and utility functions
   Copyright (C) Matthew Chapman 1999-2003

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
/* Copyright (c) 2001,2004 NoMachine, http://www.nomachine.com.           */
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

#include <stdarg.h>		/* va_list va_start va_end */
#include <unistd.h>		/* read close getuid getgid getpid getppid gethostname */
#include <fcntl.h>		/* open */
#include <pwd.h>		/* getpwuid */
#include <termios.h>		/* tcgetattr tcsetattr */
#include <sys/stat.h>		/* stat */
#include <sys/time.h>		/* gettimeofday */
#include <sys/times.h>		/* times */
#include <ctype.h>		/* toupper */
#include <errno.h>
#include "rdesktop.h"
#include "version.h"

#ifdef EGD_SOCKET
#include <sys/socket.h>		/* socket connect */
#include <sys/un.h>		/* sockaddr_un */
#endif

#ifdef WITH_OPENSSL
#include <openssl/md5.h>
#else
#include "crypto/md5.h"
#endif

char g_title[64] = "";
char g_username[64];
char hostname[16];
char keymapname[16];
int keylayout = 0x409;		/* Defaults to US keyboard layout */

int g_width = 800;		/* width is special: If 0, the
				   geometry will be fetched from
				   _NET_WORKAREA. If negative,
				   absolute value specifies the
				   percent of the whole screen. */
int g_height = 600;
int tcp_port_rdp = TCP_PORT_RDP;
int g_server_bpp = 8;
int g_win_button_size = 0;	/* If zero, disable single app mode */
BOOL g_bitmap_compression = True;
BOOL g_sendmotion = True;
BOOL g_orders = True;
BOOL g_encryption = True;
BOOL packet_encryption = True;
BOOL g_desktop_save = True;
BOOL g_fullscreen = False;
BOOL g_grab_keyboard = True;
BOOL g_hide_decorations = False;
/* NX */
BOOL g_use_rdp5 = True;
BOOL rdp_img_cache = True;
BOOL username_option = False;
BOOL prompt_password = True;
/* NX */
BOOL g_console_session = False;
BOOL g_numlock_sync = False;
extern BOOL g_owncolmap;
extern BOOL g_ownbackstore;
extern uint32 g_embed_wnd;
extern void RunOrKillNXkbd();
uint32 g_rdp5_performanceflags = RDP5_NO_WALLPAPER | RDP5_NO_FULLWINDOWDRAG | RDP5_NO_MENUANIMATIONS;

#ifdef WITH_RDPSND
BOOL g_rdpsnd = False;
#endif

extern RDPDR_DEVICE g_rdpdr_device[];
extern uint32 g_num_devices;

#ifdef RDP2VNC
extern int rfb_port;
extern int defer_time;
void
rdp2vnc_connect(char *server, uint32 flags, char *domain, char *password,
		char *shell, char *directory);
#endif

/* Here starts the NX mods */
int agentArgument(int i, char *argv[]);
void NXTranslateKeymap();
void ShowHeaderInfo();
static char *nxdesktopReadPasswdFromFile(char *fname);
#undef NXDESKTOP_DISABLE_DESKTOP_SAVE
#define NXDESKTOP_RDP_BUFSIZE   4096
char nxDisplay[255];
BOOL ipaq = False;
BOOL magickey = True;
extern int XParseGeometry(char *, int *, int *, unsigned int *, unsigned int *);
int xo;
int yo;
char windowName[255];
char passwordFile[255];
int  rdp_bufsize = NXDESKTOP_RDP_BUFSIZE;
/* end NX mods */

/* Display usage information */
static void
usage(char *program)
{
	fprintf(stderr, "rdesktop: A Remote Desktop Protocol client.\n");
	fprintf(stderr, "Version " VERSION ". Copyright (C) 1999-2003 Matt Chapman.\n");
	fprintf(stderr, "See http://www.rdesktop.org/ for more information.\n\n");

	fprintf(stderr, "Usage: %s [options] server[:port]\n", program);
#ifdef RDP2VNC
	fprintf(stderr, "   -V: vnc port\n");
	fprintf(stderr, "   -Q: defer time (ms)\n");
#endif
	fprintf(stderr, "   -u: user name\n");
	fprintf(stderr, "   -d: domain\n");
	fprintf(stderr, "   -s: shell\n");
	fprintf(stderr, "   -c: working directory\n");
	fprintf(stderr, "   -p: password (- to prompt)\n");
	fprintf(stderr, "   -n: client hostname\n");
	fprintf(stderr, "   -k: keyboard layout on server (en-us, de, sv, etc.)\n");
	fprintf(stderr, "   -g: desktop geometry (WxH)\n");
	fprintf(stderr, "   -f: full-screen mode\n");
	fprintf(stderr, "   -b: force bitmap updates\n");
	fprintf(stderr, "   -v: use BackingStore of X-server (if available)\n");
	fprintf(stderr, "   -e: disable encryption (French TS)\n");
	fprintf(stderr, "   -E: disable encryption from client to server\n");
	fprintf(stderr, "   -m: do not send motion events\n");
	fprintf(stderr, "   -C: use private colour map\n");
	fprintf(stderr, "   -D: hide window manager decorations\n");
	fprintf(stderr, "   -K: keep window manager key bindings\n");
	fprintf(stderr, "   -S: caption button size (single application mode)\n");
	fprintf(stderr, "   -T: window title\n");
	fprintf(stderr, "   -N: enable numlock syncronization\n");
	fprintf(stderr, "   -X: embed into another window with a given id.\n");
	fprintf(stderr, "   -a: connection colour depth\n");
	fprintf(stderr, "   -x: RDP5 experience (m[odem 28.8], b[roadband], l[an] or hex number)\n");
	fprintf(stderr, "   -r: enable specified device redirection (this flag can be repeated)\n");
	fprintf(stderr,
		"         '-r comport:COM1=/dev/ttyS0': enable serial redirection of /dev/ttyS0 to COM1\n");
	fprintf(stderr, "             or      COM1=/dev/ttyS0,COM2=/dev/ttyS1\n");
	fprintf(stderr,
		"         '-r disk:A=/mnt/floppy': enable redirection of /mnt/floppy to A:\n");
	fprintf(stderr, "             or   A=/mnt/floppy,D=/mnt/cdrom'\n");
	fprintf(stderr,
		"         '-r lptport:LPT1=/dev/lp0': enable parallel redirection of /dev/lp0 to LPT1\n");
	fprintf(stderr, "             or      LPT1=/dev/lp0,LPT2=/dev/lp1\n");
	fprintf(stderr, "         '-r printer:mydeskjet': enable printer redirection\n");
	fprintf(stderr,
		"             or      mydeskjet=\"HP LaserJet IIIP\" to enter server driver as well\n");
	fprintf(stderr, "         '-r sound:[local|off|remote]': enable sound redirection\n");
	fprintf(stderr, "                     remote would leave sound on server\n");
	fprintf(stderr, "   -0: attach to console\n");
	fprintf(stderr, "   -4: use RDP version 4\n");
	fprintf(stderr, "   -5: use RDP version 5 (default)\n");
	fprintf(stderr, "   -M: disable the \"Ctrl-Alt-Esc\" magic key-sequence\n");
	fprintf(stderr, "   -B: set receive buffer of RDP socket to value (default %d)\n\n", rdp_bufsize);
}

static BOOL
read_password(char *password, int size)
{
	struct termios tios;
	BOOL ret = False;
	int istty = 0;
	char *p;

	if (tcgetattr(STDIN_FILENO, &tios) == 0)
	{
		fprintf(stderr, "Password: ");
		tios.c_lflag &= ~ECHO;
		tcsetattr(STDIN_FILENO, TCSANOW, &tios);
		istty = 1;
	}

	if (fgets(password, size, stdin) != NULL)
	{
		ret = True;

		/* strip final newline */
		p = strchr(password, '\n');
		if (p != NULL)
			*p = 0;
	}

	if (istty)
	{
		tios.c_lflag |= ECHO;
		tcsetattr(STDIN_FILENO, TCSANOW, &tios);
		fprintf(stderr, "\n");
	}

	return ret;
}

static void
parse_server_and_port(char *server)
{
	char *p;
#ifdef IPv6
	int addr_colons;
#endif

#ifdef IPv6
	p = server;
	addr_colons = 0;
	while (*p)
		if (*p++ == ':')
			addr_colons++;
	if (addr_colons >= 2)
	{
		/* numeric IPv6 style address format - [1:2:3::4]:port */
		p = strchr(server, ']');
		if (*server == '[' && p != NULL)
		{
			if (*(p + 1) == ':' && *(p + 2) != '\0')
				tcp_port_rdp = strtol(p + 2, NULL, 10);
			/* remove the port number and brackets from the address */
			*p = '\0';
			strncpy(server, server + 1, strlen(server));
		}
	}
	else
	{
		/* dns name or IPv4 style address format - server.example.com:port or 1.2.3.4:port */
		p = strchr(server, ':');
		if (p != NULL)
		{
			tcp_port_rdp = strtol(p + 1, NULL, 10);
			*p = 0;
		}
	}
#else /* no IPv6 support */
	p = strchr(server, ':');
	if (p != NULL)
	{
		tcp_port_rdp = strtol(p + 1, NULL, 10);
		*p = 0;
	}
#endif /* IPv6 */

}

/* Client program */
int
main(int argc, char *argv[])

{
	char server[64];
	char fullhostname[64];
	char domain[16];
	char password[64];
	char shell[128];
	char directory[32];
	BOOL rdp_retval = False;
	struct passwd *pw;
	uint32 flags;
	char *p;
	int c, i;
	int nx_argc = 0; 
	char *nx_argv[argc];	
	
	/* show initial info */
	ShowHeaderInfo();
	
	flags = RDP_LOGON_NORMAL;
	domain[0] = password[0] = shell[0] = directory[0] = 0;
	strcpy(keymapname, "en-us");
	g_embed_wnd = 0;
	g_num_devices = 0;

#ifdef RDP2VNC
#define VNCOPT "V:Q:"
#else
#define VNCOPT
#endif
	/* Parse the extra parameters from the NXAgent */
	#if 0
	for (i = 1;i<argc; i++)
	{
	    fprintf(stderr,"par1 '%i' '%s'\n",i,argv[i]);
	}
	#endif
	nx_argv[0] = argv[0];
	i = 1;
	while (i<argc)
	{
	    c = agentArgument(i, argv);
	    if (c == 0)
	    {
		nx_argc++;
		nx_argv[nx_argc] = argv[i];
		i++;
	    }
	    else
	    {
		i+=c;
	    }
	}
	nx_argc++;	
	
	#ifdef __sun
	nx_argv[nx_argc] = "\0";
	#else
	nx_argv[nx_argc] = NULL;
	#endif
	
	#if 0
	for (i = 0; i<=nx_argc; i++)
	    fprintf(stderr,"par2 '%i' '%s'\n",i,nx_argv[i]);
	#endif
	
	while ((c = getopt(nx_argc, nx_argv, VNCOPT "u:d:s:c:p:n:k:g:fbBeEmCDKS:T:NX:a:x:r:045h?")) != -1)
	{
		switch (c)
		{
		    
#ifdef RDP2VNC
			case 'V':
				rfb_port = strtol(optarg, NULL, 10);
				if (rfb_port < 100)
					rfb_port += 5900;
				break;

			case 'Q':
				defer_time = strtol(optarg, NULL, 10);
				if (defer_time < 0)
					defer_time = 0;
				break;
#endif

			case 'u':
				STRNCPY(g_username, optarg, sizeof(g_username));
				username_option = True;
				break;

			case 'd':
				STRNCPY(domain, optarg, sizeof(domain));
				break;

			case 's':
				STRNCPY(shell, optarg, sizeof(shell));
				break;

			case 'c':
				STRNCPY(directory, optarg, sizeof(directory));
				break;

			case 'p':
				if ((optarg[0] == '-') && (optarg[1] == 0))
				{
					prompt_password = True;
					break;
				}

				STRNCPY(password, optarg, sizeof(password));
				flags |= RDP_LOGON_AUTO;

				/* try to overwrite argument so it won't appear in ps */
				p = optarg;
				while (*p)
					*(p++) = 'X';
				break;

			case 'n':
				STRNCPY(hostname, optarg, sizeof(hostname));
				break;

			case 'k':
				STRNCPY(keymapname, optarg, sizeof(keymapname));
				break;

			case 'g':
				g_fullscreen = False;
				if (!strcmp(optarg, "workarea"))
				{
					g_width = g_height = 0;
					break;
				}

				g_width = strtol(optarg, &p, 10);
				if (g_width <= 0)
				{
					error("invalid geometry\n");
					return 1;
				}

				if (*p == 'x')
					g_height = strtol(p + 1, NULL, 10);

				if (g_height <= 0)
				{
					error("invalid geometry\n");
					return 1;
				}

				if (*p == '%')
					g_width = -g_width;

				break;

			case 'f':
				g_fullscreen = True;
				xo = -1;
				yo = -1;
				break;
			
			case 'i':
				g_fullscreen = True;
                                ipaq = True;
				xo = -1;
				yo = -1;
				break;

			case 'b':
				g_orders = False;
				break;

			case 'v': /* supposed to be B - changed to keep compat. with NX desktop */
				g_ownbackstore = False;
				break;

			case 'e':
				g_encryption = False;
				break;
				
			case 'E':
				packet_encryption = False;
				break;
				
			case 'm':
				g_sendmotion = False;
				break;

			case 'C':
				g_owncolmap = True;
				break;

			case 'D':
				g_hide_decorations = True;
				break;

			case 'K':
				g_grab_keyboard = False;
				break;

			case 'S':
				if (!strcmp(optarg, "standard"))
				{
					g_win_button_size = 18;
					break;
				}

				g_win_button_size = strtol(optarg, &p, 10);

				if (*p)
				{
					error("invalid button size\n");
					return 1;
				}

				break;

			case 'T':
				STRNCPY(g_title, optarg, sizeof(g_title));
				break;

			case 'N':
				g_numlock_sync = True;
				break;

			case 'X':
				g_embed_wnd = strtol(optarg, NULL, 10);
				break;
				
			case 'a':
				g_server_bpp = strtol(optarg, NULL, 10);
				if (g_server_bpp != 8 && g_server_bpp != 16 && g_server_bpp != 15
				    && g_server_bpp != 24)
				{
					error("invalid server bpp\n");
					return 1;
				}
				break;

			case 'x':
				
				if (strncmp("modem", optarg, 1) == 0)
				{
					g_rdp5_performanceflags = RDP5_NO_WALLPAPER | RDP5_NO_FULLWINDOWDRAG | RDP5_NO_MENUANIMATIONS | RDP5_NO_THEMING;
				}
				else if (strncmp("broadband", optarg, 1) == 0)
				{
					g_rdp5_performanceflags = RDP5_NO_WALLPAPER;
				}
				else if (strncmp("lan", optarg, 1) == 0)
				{
					g_rdp5_performanceflags = RDP5_DISABLE_NOTHING;
				}
				else
				{
					g_rdp5_performanceflags = strtol(optarg, NULL, 16);
				}
				break;
				
			case 'r':

				if (strncmp("sound", optarg, 5) == 0)
				{
					optarg += 5;

					if (*optarg == ':')
					{
						*optarg++;
						while ((p = next_arg(optarg, ',')))
						{
							if (strncmp("remote", optarg, 6) == 0)
								flags |= RDP_LOGON_LEAVE_AUDIO;

							if (strncmp("local", optarg, 5) == 0)
#ifdef WITH_RDPSND
								g_rdpsnd = True;
#else
								warning("Not compiled with sound support");
#endif

							if (strncmp("off", optarg, 3) == 0)
								g_rdpsnd = False;

							optarg = p;
						}
					}
					else
					{
#ifdef WITH_RDPSND
						g_rdpsnd = True;
#else
						warning("Not compiled with sound support");
#endif
					}
				}
				else if (strncmp("disk", optarg, 4) == 0)
				{
					/* -r disk:h:=/mnt/floppy */
					disk_enum_devices(&g_num_devices, optarg + 4);
				}
				else if (strncmp("comport", optarg, 7) == 0)
				{
					serial_enum_devices(&g_num_devices, optarg + 7);
				}
				else if (strncmp("lptport", optarg, 7) == 0)
				{
					parallel_enum_devices(&g_num_devices, optarg + 7);
				}
				else if (strncmp("printer", optarg, 7) == 0)
				{
					printer_enum_devices(&g_num_devices, optarg + 7);
				}
				else
				{
					warning("Unknown -r argument\n\n\tPossible arguments are: comport, disk, lptport, printer, sound\n");
				}
				break;

			case '0':
				g_console_session = True;
				break;

			case '4':
				g_use_rdp5 = False;
				break;

			case '5':
				g_use_rdp5 = True;
				break;
			
			/* NX options */
			case 'M':
				magickey = False;
				break;
			case 'B':
				rdp_bufsize = strtol(optarg, NULL, 10);
				if (rdp_bufsize <= 0)
				{
					error("Invalid value '%s' for option -B. Try '-R nnn' with nnn > 0.\n");
					return 1;
				}
				else if (rdp_bufsize < 1024)
				{
					rdp_bufsize = 1024;
				}
				break;
			/* End NX options */
			case 'h':
			case '?':
			default:
				usage(argv[0]);
				return 1;
		}
	}
	
	if (nx_argc - optind < 1)
	{
		usage(argv[0]);
		return 1;
	}

	STRNCPY(server, nx_argv[optind], sizeof(server));
	parse_server_and_port(server);	

	NXTranslateKeymap();
	
	if (!username_option)
	{
		pw = getpwuid(getuid());
		if ((pw == NULL) || (pw->pw_name == NULL))
		{
			error("could not determine username, use -u\n");
			return 1;
		}

		STRNCPY(g_username, pw->pw_name, sizeof(g_username));
	}

	if (hostname[0] == 0)
	{
		if (gethostname(fullhostname, sizeof(fullhostname)) == -1)
		{
			error("could not determine local hostname, use -n\n");
			return 1;
		}

		p = strchr(fullhostname, '.');
		if (p != NULL)
			*p = 0;

		STRNCPY(hostname, fullhostname, sizeof(hostname));
	}
	/* read the passwordfile or fallback to the prompt or standard comandline option */
	if (passwordFile[0] == 0)
        {
		if (prompt_password && read_password(password, sizeof(password)))
			flags |= RDP_LOGON_AUTO;
	}
	else
	{
	    
           char *filePass = nxdesktopReadPasswdFromFile((char *)passwordFile);
	   
           if (filePass != NULL)
           {
              strcpy(password, filePass);
              flags |= RDP_LOGON_AUTO;
           }
           else
           {
              fprintf(stderr,"invalid pass from file\n");
           }
        }
	
	if (g_title[0] == 0)
	{
		strcpy(g_title, "nxdesktop - ");
		strncat(g_title, server, sizeof(g_title) - sizeof("nxdesktop - "));
	}

	/* NX */
	fprintf(stderr, "Info: Connecting to RDP server '%s'\n",server);
	/* NX */
		
#ifdef RDP2VNC
	rdp2vnc_connect(server, flags, domain, password, shell, directory);
	return 0;
#else
	if (!test_rdp_connect(server))
	    return 1;
	
	if (!ui_open_display())
	    {
		fprintf(stderr, "Error: nxdesktop cannot open X display.\n");
		return 1;
	    }
	if (!ui_init())
		return 1;
	#ifdef WITH_RDPSND
	if (g_rdpsnd)
		rdpsnd_init();
	#endif
	
	/* Redirection will be kept disabled until needed */
	/* rdpdr_init(); */
	
	if (!rdp_connect(server, flags, domain, password, shell, directory))
	{
		fprintf(stderr, "Error: Connection to RDP server '%s' failed.\n",server);
		return 1;
	}
	else
	{
	    if ((g_width == 0) || (g_height == 0))
	    {
		g_width = 800;
		g_height = 600;
	    }
	    if (g_fullscreen)
		ui_get_display_size(&g_width, &g_height);
	}
	/* By setting encryption to False here, we have an encrypted login 
	   packet but unencrypted transfer of other packets */
	if (!packet_encryption)
		g_encryption = False;
	
	/* NX */
	fprintf(stderr, "Info: Connected to RDP server '%s'\n",server);
	
	fprintf(stderr, "Info: Color depth %d.\n",g_server_bpp);
	/* NX */

	DEBUG(("Connection successful.\n"));
	memset(password, 0, sizeof(password));
	
	if (ui_create_window())
	{	
	    /* NX */
	    if (ipaq) RunOrKillNXkbd();
	    /* NX */
	    rdp_retval = rdp_main_loop();
	    if (ipaq) RunOrKillNXkbd();
	    ui_destroy_window();
	}

	DEBUG(("Disconnecting...\n"));
	rdp_disconnect();
	fprintf(stderr, "Info: Disconnecting from RDP server '%s'\n",server);
	ui_deinit();

	if (True == rdp_retval)
		return 0;
	else
		return 2;
	

#endif

}

#ifdef EGD_SOCKET
/* Read 32 random bytes from PRNGD or EGD socket (based on OpenSSL RAND_egd) */
static BOOL
generate_random_egd(uint8 * buf)
{
	struct sockaddr_un addr;
	BOOL ret = False;
	int fd;

	fd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (fd == -1)
		return False;

	addr.sun_family = AF_UNIX;
	memcpy(addr.sun_path, EGD_SOCKET, sizeof(EGD_SOCKET));
	if (connect(fd, (struct sockaddr *) &addr, sizeof(addr)) == -1)
		goto err;

	/* PRNGD and EGD use a simple communications protocol */
	buf[0] = 1;		/* Non-blocking (similar to /dev/urandom) */
	buf[1] = 32;		/* Number of requested random bytes */
	if (write(fd, buf, 2) != 2)
		goto err;

	if ((read(fd, buf, 1) != 1) || (buf[0] == 0))	/* Available? */
		goto err;

	if (read(fd, buf, 32) != 32)
		goto err;

	ret = True;

      err:
	close(fd);
	return ret;
}
#endif

/* Generate a 32-byte random for the secure transport code. */
void
generate_random(uint8 * random)
{
	struct stat st;
	struct tms tmsbuf;
	MD5_CTX md5;
	uint32 *r;
	int fd, n;

	/* If we have a kernel random device, try that first */
	if (((fd = open("/dev/urandom", O_RDONLY)) != -1)
	    || ((fd = open("/dev/random", O_RDONLY)) != -1))
	{
		n = read(fd, random, 32);
		close(fd);
		if (n == 32)
			return;
	}

#ifdef EGD_SOCKET
	/* As a second preference use an EGD */
	if (generate_random_egd(random))
		return;
#endif

	/* Otherwise use whatever entropy we can gather - ideas welcome. */
	r = (uint32 *) random;
	r[0] = (getpid()) | (getppid() << 16);
	r[1] = (getuid()) | (getgid() << 16);
	r[2] = times(&tmsbuf);	/* system uptime (clocks) */
	gettimeofday((struct timeval *) &r[3], NULL);	/* sec and usec */
	stat("/tmp", &st);
	r[5] = st.st_atime;
	r[6] = st.st_mtime;
	r[7] = st.st_ctime;

	/* Hash both halves with MD5 to obscure possible patterns */
	MD5_Init(&md5);
	MD5_Update(&md5, random, 16);
	MD5_Final(random, &md5);
	MD5_Update(&md5, random + 16, 16);
	MD5_Final(random + 16, &md5);
}

/* malloc; exit if out of memory */
void *
xmalloc(int size)
{
	void *mem = malloc(size);
	if (mem == NULL)
	{
		error("xmalloc %d\n", size);
		exit(1);
	}
	return mem;
}

/* realloc; exit if out of memory */
void *
xrealloc(void *oldmem, int size)
{
	void *mem = realloc(oldmem, size);
	if (mem == NULL)
	{
		error("xrealloc %d\n", size);
		exit(1);
	}
	return mem;
}

/* free */
void
xfree(void *mem)
{
	free(mem);
}

/* report an error */
void
error(char *format, ...)
{
	va_list ap;

	fprintf(stderr, "ERROR: ");

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
}

/* report a warning */
void
warning(char *format, ...)
{
	va_list ap;

	fprintf(stderr, "WARNING: ");

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
}

/* report an unimplemented protocol feature */
void
unimpl(char *format, ...)
{
	va_list ap;

	fprintf(stderr, "NOT IMPLEMENTED: ");

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
}

/* produce a hex dump */
void
hexdump(unsigned char *p, unsigned int len)
{
	unsigned char *line = p;
	int i, thisline, offset = 0;

	while (offset < len)
	{
		printf("%04x ", offset);
		thisline = len - offset;
		if (thisline > 16)
			thisline = 16;

		for (i = 0; i < thisline; i++)
			printf("%02x ", line[i]);

		for (; i < 16; i++)
			printf("   ");

		for (i = 0; i < thisline; i++)
			printf("%c", (line[i] >= 0x20 && line[i] < 0x7f) ? line[i] : '.');

		printf("\n");
		offset += thisline;
		line += thisline;
	}
}

/*
  input: src is the string we look in for needle.
  	 Needle may be escaped by a backslash, in
	 that case we ignore that particular needle.
  return value: returns next src pointer, for
  	succesive executions, like in a while loop
	if retval is 0, then there are no more args.
  pitfalls:
  	src is modified. 0x00 chars are inserted to
	terminate strings.
	return val, points on the next val chr after ins
	0x00

	example usage:
	while( (pos = next_arg( optarg, ',')) ){
		printf("%s\n",optarg);
		optarg=pos;
	}

*/
char *
next_arg(char *src, char needle)
{
	char *nextval;
	char *p;
	char *mvp = 0;

	/* EOS */
	if (*src == (char) 0x00)
		return 0;

	p = src;
	/*  skip escaped needles */
	while ((nextval = strchr(p, needle)))
	{
		mvp = nextval - 1;
		/* found backslashed needle */
		if (*mvp == '\\' && (mvp > src))
		{
			/* move string one to the left */
			while (*(mvp + 1) != (char) 0x00)
			{
				*mvp = *(mvp + 1);
				*mvp++;
			}
			*mvp = (char) 0x00;
			p = nextval;
		}
		else
		{
			p = nextval + 1;
			break;
		}

	}

	/* more args available */
	if (nextval)
	{
		*nextval = (char) 0x00;
		return ++nextval;
	}

	/* no more args after this, jump to EOS */
	nextval = src + strlen(src);
	return nextval;
}


void
toupper_str(char *p)
{
	while (*p)
	{
		if ((*p >= 'a') && (*p <= 'z'))
			*p = toupper((int) *p);
		p++;
	}
}


/* not all clibs got ltoa */
#define LTOA_BUFSIZE (sizeof(long) * 8 + 1)

char *
l_to_a(long N, int base)
{
	static char ret[LTOA_BUFSIZE];

	char *head = ret, buf[LTOA_BUFSIZE], *tail = buf + sizeof(buf);

	register int divrem;

	if (base < 36 || 2 > base)
		base = 10;

	if (N < 0)
	{
		*head++ = '-';
		N = -N;
	}

	tail = buf + sizeof(buf);
	*--tail = 0;

	do
	{
		divrem = N % base;
		*--tail = (divrem <= 9) ? divrem + '0' : divrem + 'a' - 10;
		N /= base;
	}
	while (N);

	strcpy(head, tail);
	return ret;
}


int
load_licence(unsigned char **data)
{
	char *home, *path;
	struct stat st;
	int fd, length;

	home = getenv("HOME");
	if (home == NULL)
		return -1;

	path = (char *) xmalloc(strlen(home) + strlen(hostname) + sizeof("/.rdesktop/licence."));
	sprintf(path, "%s/.rdesktop/licence.%s", home, hostname);

	fd = open(path, O_RDONLY);
	if (fd == -1)
		return -1;

	if (fstat(fd, &st))
		return -1;

	*data = (uint8 *) xmalloc(st.st_size);
	length = read(fd, *data, st.st_size);
	close(fd);
	xfree(path);
	return length;
}

void
save_licence(unsigned char *data, int length)
{
	char *home, *path, *tmppath;
	int fd;

	home = getenv("HOME");
	if (home == NULL)
		return;

	path = (char *) xmalloc(strlen(home) + strlen(hostname) + sizeof("/.rdesktop/licence."));

	sprintf(path, "%s/.rdesktop", home);
	if ((mkdir(path, 0700) == -1) && errno != EEXIST)
	{
		perror(path);
		return;
	}

	/* write licence to licence.hostname.new, then atomically rename to licence.hostname */

	sprintf(path, "%s/.rdesktop/licence.%s", home, hostname);
	tmppath = (char *) xmalloc(strlen(path) + sizeof(".new"));
	strcpy(tmppath, path);
	strcat(tmppath, ".new");

	fd = open(tmppath, O_WRONLY | O_CREAT | O_TRUNC, 0600);
	if (fd == -1)
	{
		perror(tmppath);
		return;
	}

	if (write(fd, data, length) != length)
	{
		perror(tmppath);
		unlink(tmppath);
	}
	else if (rename(tmppath, path) == -1)
	{
		perror(path);
		unlink(tmppath);
	}

	close(fd);
	xfree(tmppath);
	xfree(path);
}

/* the NX mods implementation starts here */

/* agentArgument parses the extra agent paramenters */

int 
agentArgument (i, argv)

int i;
char *argv[];

{
    if (!strcmp(argv[i], "-sync"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-imgcache"))
    {
	rdp_img_cache = True;
	return 1;
    }

    if (!strcmp(argv[i], "-full"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-class"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-cc"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-depth"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-sss"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-display"))
    {
	strncpy(nxDisplay,(char *)argv[i+1],strlen((char *)argv[i+1]));
	return 2;
    }

    if (!strcmp(argv[i], "-geometry"))
    {
	if (!strcmp(argv[i+1],"fullscreen"))
	{
	    g_fullscreen = True;
    	}
        else 
	if (!strcmp(argv[i+1],"-ipaq"))
    	{
    	    g_fullscreen = True;
    	    ipaq = True;
	}
        else 
	{
	    int j = 0;
	    XParseGeometry(argv[i+1], &xo, &yo, &g_width, &g_height);
	    while(argv[i+1][j])
	    {
		if(argv[i+1][j] == '+')
		{
		    argv[i+1][j] = 'x';
                }
                j++;
            }
        }
	return 2;
    }
    
    if (!strcmp(argv[i], "-name"))
    {
	strncpy(windowName,(char *)argv[i+1],strlen((char *)argv[i+1]));
	return 2;
    }
    
    if (!strcmp(argv[i], "-bw"))
    {
	return 1;
    }
    
    if (!strcmp(argv[i], "-scrns"))
    {
	return 1;
    }
    
    if (!strcmp(argv[i], "-install"))
    {
	return 1;
    }
    
    if (!strcmp(argv[i], "-parent"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-imgstop"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-getifocus"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-karma"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-bstimeout"))
    {
	return 1;
    }
    
    if (!strcmp(argv[i], "-imgframe"))
    {
	#ifdef __sun
	putenv("NX_IMAGEFRAME");
	#else
	setenv("NX_IMAGEFRAME", "", 1);
	#endif
	return 1;
    }

    if (!strcmp(argv[i], "-imgsplit"))
    {
	#ifdef __sun
        char envBuffer[512];
        sprintf(envBuffer,"NX_IMAGESPLIT=%s",argv[i]);
        putenv(envBuffer);
	#else
        setenv("NX_IMAGESPLIT", argv[i], 1);
	#endif
	return 1;
    }
    
    if (!strcmp(argv[i], "-imgcolors"))
    {
	#ifdef __sun
	char envBuffer[512];
	sprintf(envBuffer,"NX_IMAGEMASK=%s",argv[i]);
	putenv(envBuffer);
	#else
	setenv("NX_IMAGEMASK", argv[i], 1);
	#endif
	return 1;
    }
    
    if (!strcmp(argv[i], "-autoexpose"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-backingstore"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-maxbsarea"))
    {
       return 1;
    }
    
    if (!strcmp(argv[i], "-minbsarea"))
    {
	return 1;
    }
    
    if (!strcmp(argv[i], "-safe"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-noreset"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-fp"))
    {
       return 1;
    }

    if (!strcmp(argv[i], "-auth"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-bs"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "+bs"))
    {
	return 1;
    }

    if (!strcmp(argv[i], "-keyboard"))
    {
	keylayout = strtol(argv[i+1], NULL, 16);
        if (keylayout == 0)
        {
           error("Invalid keyboard layout\n");
        }
	return 2;
    }

    if (!strcmp(argv[i], "-passwd"))
    {
	prompt_password = False;
	strncpy(passwordFile,(char *)argv[i+1],strlen((char *)argv[i+1]));
	return 2;
    }

    return 0;
}

char *nxdesktopReadPasswdFromFile(char *fname)
{
    FILE *fp;
    int  i;
    char ch;
    char *passwd = malloc(17);

    if (passwd == NULL) return NULL;

    memset(passwd, 0, 17);

    if ((fp = fopen(fname,"r")) == NULL)
    {
     free(passwd);
     return NULL;
    }

    for (i = 0; i < 16; i++)
    {
       ch = getc(fp);
       if (ch == EOF)
          break;
       passwd[i] = ch;
    }
    fclose(fp);
    if (i == 0)
    {
      free(passwd);
      return NULL;
    }
    return (char *)passwd;
}

/* Translates the keymap number received to the internal rdesktop keymap name */

void NXTranslateKeymap()
{
    switch(keylayout)
    {
	case 0x0401: strcpy(keymapname, "ar"); break;
	case 0x0406: strcpy(keymapname, "da"); break;
	case 0x0407: strcpy(keymapname, "de"); break;
	case 0x0807: strcpy(keymapname, "de-ch"); break;
	case 0x040a: strcpy(keymapname, "es"); break;
	case 0x0809: strcpy(keymapname, "en-gb"); break;
	case 0x0409: strcpy(keymapname, "en-us"); break;
	case 0x040b: strcpy(keymapname, "fi"); break;
	case 0x0438: strcpy(keymapname, "fo"); break;
	case 0x040c: strcpy(keymapname, "fr"); break;
	case 0x080c: strcpy(keymapname, "fr-be"); break;
	case 0x0c0c: strcpy(keymapname, "fr-ca"); break;
	case 0x100c: strcpy(keymapname, "fr-ch"); break;
	case 0x041a: strcpy(keymapname, "hr"); break;
	case 0x040e: strcpy(keymapname, "hu"); break;
	case 0x0410: 
	case 0x0810: strcpy(keymapname, "it"); break;
	case 0x0411: strcpy(keymapname, "ja"); break;
	case 0x0427: strcpy(keymapname, "lt"); break;
	case 0x0426: strcpy(keymapname, "lv"); break;
	case 0x042f: strcpy(keymapname, "mk"); break;
	case 0x0414: strcpy(keymapname, "no"); break;
	case 0x0415: strcpy(keymapname, "pl"); break;
	case 0x0816: strcpy(keymapname, "pt"); break;
	case 0x0416: strcpy(keymapname, "pt-br"); break;
	case 0x0419: strcpy(keymapname, "ru"); break;
	case 0x0424: strcpy(keymapname, "sl"); break;
	case 0x041d: strcpy(keymapname, "sv"); break;
	case 0x041e: strcpy(keymapname, "th"); break;
	case 0x041f: strcpy(keymapname, "tr"); break;
	default: fprintf(stderr, "Warning: unknown keyboard layout [%X].\n", keylayout);
    }
    DEBUG_KBD(("keymapname is [%s].\n", keymapname));
}

/* show_startup_info - Just show some header information */

void ShowHeaderInfo(void)

{
	fprintf(stderr, "\nNXDESKTOP - Version %i.%i.%i\n\n",NXDESKTOP_MAJOR_VERSION,NXDESKTOP_MINOR_VERSION,NXDESKTOP_RELEASE);
	fprintf(stderr, "Remote Desktop Protocol client for NX.\n");
	fprintf(stderr, "Copyright (C) 2001, 2004 NoMachine.\n");
	fprintf(stderr, "See http://www.nomachine.com/ for more information.\n\n");
	fprintf(stderr, "Based on rdesktop version "VERSION"\n"), 
	fprintf(stderr, "Copyright (C) 1999-2003 Matt Chapman.\n");
	fprintf(stderr, "See http://www.rdesktop.org/ for more information.\n\n");
	fprintf(stderr, "Info: Agent running with pid '%d'.\n", getpid());
}
