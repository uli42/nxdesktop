/*
   rdesktop: A Remote Desktop Protocol client.
   Entrypoint and utility functions
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

#include <stdlib.h>		/* malloc realloc free */
#include <stdarg.h>		/* va_list va_start va_end */
#include <unistd.h>		/* read close getuid getgid getpid getppid gethostname */
#include <fcntl.h>		/* open */
#include <pwd.h>		/* getpwuid */
#include <sys/stat.h>		/* stat */
#include <sys/time.h>		/* gettimeofday */
#include <sys/times.h>		/* times */
#include "rdesktop.h"

extern int XParseGeometry(char *, int *, int *, unsigned int *, unsigned int *);

#undef NXDESKTOP_DISABLE_DESKTOP_SAVE

#define NXDESKTOP_VERSION       "1.3.1"
#define NXDESKTOP_RDP_BUFSIZE   4096

char username[16];
char hostname[16];
int width;
int height;
int xo;
int yo;
int keylayout = 0x409;
BOOL bitmap_compression = True;
BOOL sendmotion = True;
BOOL orders = True;
BOOL licence = True;
BOOL encryption = True;

#ifdef NXDESKTOP_DISABLE_DESKTOP_SAVE
BOOL desktop_save = False;
#else
BOOL desktop_save = True;
#endif

BOOL fullscreen = False;
BOOL ipaq = False;
BOOL magickey = True;
char *windowName = NULL;
char *nxDisplay  = NULL;
int  rdp_bufsize = NXDESKTOP_RDP_BUFSIZE;
char *passwordFile = NULL;

char keymapname[16];


static int agentArgument(int argc , char *argv[], int i);
static char *nxdesktopReadPasswdFromFile(char *fname);

extern void RunOrKillNXkbd();

void NXTranslateKeymap();


/* Display usage information */
static void
usage(char *program)
{
	printf("Usage: %s [options] server\n\n", program);
	printf("   -u: user name\n");
	printf("   -d: domain\n");
	printf("   -s: shell\n");
	printf("   -c: working directory\n");
	printf("   -p: password (autologon)\n");
	printf("   -n: client hostname\n");
	printf("   -k: keyboard layout (hex)\n");
	printf("   -g: desktop geometry (WxH)\n");
	printf("   -f: full-screen mode\n");
	printf("   -b: force bitmap updates\n");
	printf("   -e: disable encryption (French TS)\n");
	printf("   -m: do not send motion events\n");
	printf("   -l: do not request licence\n");
	printf("   -M: disable the \"Ctrl-Alt-Esc\" magic key-sequence\n");
	printf("   -B: set receive buffer of RDP socket to value (default %d)\n\n", rdp_bufsize);
}


int pre_parse_agent(int argc, char *argv[])
{
  int i, count;
  count = 0;
  for (i = 1; i < argc; i++)
  	count += agentArgument (argc, argv, i);
  return count;
}




/* Client program */
int
main(int argc, char *argv[])
{
	char fullhostname[64];
	char domain[16];
	char password[16];
	char shell[32];
	char directory[32];
	char title[32];
	struct passwd *pw;
	char *server, *p;
	uint32 flags;
	int c,pre_count;

	fprintf(stderr, "\nNXDESKTOP - Version " NXDESKTOP_VERSION "\n\n");
	fprintf(stderr, "Remote Desktop Protocol client for NX.\n");
	fprintf(stderr, "Copyright (C) 2001,2003 NoMachine.\n");
	fprintf(stderr, "See http://www.nomachine.com/ for more information.\n\n");
	fprintf(stderr, "Based on rdesktop version " VERSION ".\n");
	fprintf(stderr, "Copyright (C) 1999-2001 Matt Chapman.\n");
	fprintf(stderr, "See http://www.rdesktop.org/ for more information.\n\n");

        fprintf(stderr, "Info: Agent running with pid '%d'.\n", getpid());

	flags = RDP_LOGON_NORMAL;
	domain[0] = password[0] = shell[0] = directory[0] = 0;
	strcpy(keymapname, "en-us");

	pre_count = pre_parse_agent(argc, argv);
	while ((c = getopt(argc, argv, "(:u:d:s:c:p:n:k:g:B:(fibemlh?M")) != -1)
	{
		switch (c)
		{
		        case '(':
			        break;
			case 'u':
				STRNCPY(username, optarg, sizeof(username));
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
				STRNCPY(password, optarg, sizeof(password));
				flags |= RDP_LOGON_AUTO;
				break;

			case 'n':
				STRNCPY(hostname, optarg, sizeof(hostname));
				break;

			case 'k':
				keylayout = strtol(optarg, NULL, 16);
				if (keylayout == 0)
				{
					error("Invalid keyboard layout\n");
					return 1;
				}
				break;

			case 'g':
				width = strtol(optarg, &p, 10);
				if (*p == 'x')
					height = strtol(p+1, NULL, 10);

				if ((width == 0) || (height == 0))
				{
					error("Invalid geometry\n");
					return 1;
				}
				break;

			case 'f':
				fullscreen = True;
				xo = -1;
				yo = -1;
				break;
			
			case 'i':
				fullscreen = True;
                                ipaq = True;
				xo = -1;
				yo = -1;
				break;

			case 'b':
				orders = False;
				break;

			case 'e':
				encryption = False;
				break;

			case 'm':
				sendmotion = False;
				break;

			case 'l':
				licence = False;
				break;
		        case 'M':
				magickey = False;
				break;
			case 'B':
				rdp_bufsize = strtol(optarg, NULL, 10);
				if (rdp_bufsize <= 0)
				{
					error("Invalid value '%s' for option -B. Try '-B nnn' with nnn > 0.\n");
					return 1;
				}
				else if (rdp_bufsize < 1024)
				{
					rdp_bufsize = 1024;
				}
				break;
			case 'h':
			case '?':
			default:
				usage(argv[0]);
				return 1;
		}
	}

#ifdef __sun
        if (pre_count > 1 )
          pre_count--;
#endif

        optind += pre_count;

	if (argc - optind < 1)
	{
		usage(argv[0]);
		return 1;
	}

	server = argv[argc - 1];

        NXTranslateKeymap();

        if (username[0] == 0)
	{
		pw = getpwuid(getuid());
		if ((pw == NULL) || (pw->pw_name == NULL))
		{
			error("Could not determine username, use -u\n");
			return 1;
		}

		STRNCPY(username, pw->pw_name, sizeof(username));
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
        if (passwordFile == NULL)
        {
	        if (!strcmp(password, "-"))
	        {
        		p = getpass("Password: ");
		        if (p == NULL)
		        {
        			error("failed to read password\n");
			        return 0;
		        }
		        STRNCPY(password, p, sizeof(password));
                }
	}
        else
        {
           char *filePass = nxdesktopReadPasswdFromFile(passwordFile);
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

	strcpy(title, "nxdesktop - ");
	strncat(title, server, sizeof(title) - sizeof("nxdesktop - "));

/*
        if (!ui_open_display())
        {
           fprintf(stderr, "Error: nxdesktop cannot open X display.\n");
           return 1;
        }

	if ((width == 0) || (height == 0))
	{
		width = 800;
		height = 600;
	}
        if (fullscreen)
        {
          ui_get_display_size(&width, &height);
        }
	if (ui_create_window(windowName!=NULL?windowName:title))
	{
           extern void nxdesktopSetAtoms(void);
        	if (!rdp_connect(server, flags, domain, password, shell,
	                     directory))
                {
		   fprintf(stderr, "\nError: RDP server connection failed.\n");
                   return 1;
                }

		fprintf(stderr, "Info: RDP connection successful.\n");
		if (ipaq) RunOrKillNXkbd();
                nxdesktopSetAtoms();
		rdp_main_loop();
		fprintf(stderr, "Info: Disconnecting RDP ...\n");
		if (ipaq) RunOrKillNXkbd();
		ui_destroy_window();
	}
*/
	if (rdp_connect(server, flags, domain, password, shell, directory))
	{
           extern void nxdesktopSetAtoms(void);
	   extern void RunOrKillNXkbd();
           rdp_disconnect();

           if (!ui_open_display())
           {
             fprintf(stderr, "Error: nxdesktop cannot open X display.\n");
             return 1;
           }
	   if ((width == 0) || (height == 0))
	   {
		width = 800;
		height = 600;
	   }
           if (fullscreen)
           {
             ui_get_display_size(&width, &height);
           }


	   if (!ui_create_window(windowName!=NULL?windowName:title))
    	     return 1;
	   if (ipaq) RunOrKillNXkbd();

    	   fprintf(stderr, "Info: RDP connection successful.\n");

	   rdp_connect_login(server, flags, domain, password, shell, directory);

           nxdesktopSetAtoms();

	   rdp_main_loop();
	   fprintf(stderr, "Info: Disconnecting RDP ...\n");
	   if (ipaq) RunOrKillNXkbd();
	   ui_destroy_window();
	}
        else{
	   fprintf(stderr, "\nError: RDP server connection failed.\n");
           exit(1);
	}


        rdp_disconnect();
	return 0;
}

/* Generate a 32-byte random for the secure transport code. */
void
generate_random(uint8 *random)
{
	struct stat st;
	struct tms tmsbuf;
	uint32 *r = (uint32 *) random;
	int fd;

	/* If we have a kernel random device, use it. */
	if (((fd = open("/dev/urandom", O_RDONLY)) != -1)
	    || ((fd = open("/dev/random", O_RDONLY)) != -1))
	{
		read(fd, random, 32);
		close(fd);
		return;
	}

	/* Otherwise use whatever entropy we can gather - ideas welcome. */
	r[0] = (getpid()) | (getppid() << 16);
	r[1] = (getuid()) | (getgid() << 16);
	r[2] = times(&tmsbuf);	/* system uptime (clocks) */
	gettimeofday((struct timeval *) &r[3], NULL);	/* sec and usec */
	stat("/tmp", &st);
	r[5] = st.st_atime;
	r[6] = st.st_mtime;
	r[7] = st.st_ctime;
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
	unsigned int thisline, offset = 0;
	unsigned int i;

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
			printf("%c",
			       (line[i] >= 0x20
				&& line[i] < 0x7f) ? line[i] : '.');

		printf("\n");
		offset += thisline;
		line += thisline;
	}
}


int agentArgument (argc, argv, i)
    int	argc;
    char *argv[] ;
    int	i;
{
  if (!strcmp(argv[i], "-sync"))
  {
    argv[i][1]='(';
    return 0;
  }
  if (!strcmp(argv[i], "-full"))
  {
    argv[i][1]='(';
    return 1;
  }
  if (!strcmp(argv[i], "-class"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
	return 1;
    }
    return 0;
  }
  if (!strcmp(argv[i], "-cc"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
       return 1;
    }
    return 0;
  }
  if (!strcmp(argv[i], "-depth"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
	return 1;
    }
    return 0;
  }
  if (!strcmp(argv[i], "-sss"))
  {
    argv[i][1]='(';
    return 0;
  }
  if (!strcmp(argv[i], "-display"))
  {
      argv[i][1]='(';
      if (++i < argc)
      {
        nxDisplay = argv[i];
#ifdef __sun
      return 2;
#else
      return 1;
#endif
      }
      return 0;
  }
  if (!strcmp(argv[i], "-geometry"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
      if (!strcmp(argv[i],"fullscreen"))
      {
          fullscreen = True;
      }
      else if (!strcmp(argv[i],"ipaq"))
      {
          fullscreen = True;
          ipaq = True;
      }
      else {
          int j = 0;
          XParseGeometry(argv[i], &xo, &yo, &width, &height);
          while(argv[i][j]){
              if(argv[i][j] == '+'){
                  argv[i][j] = 'x';
              }
              j++;
          }
      }
#ifdef __sun
      return 2;
#else
      return 1;
#endif
    }
    return 0;
  }
  if (!strcmp(argv[i], "-name"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
      windowName = argv[i];
#ifdef __sun
      return 2;
#else
      return 1;
#endif
    }
    return 0;
  }

  if (!strcmp(argv[i], "-bw"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
	return 1;
    }
    return 0;
  }

  if (!strcmp(argv[i], "-scrns"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
	return 1;
    }
    return 0;
  }
  if (!strcmp(argv[i], "-install"))
  {
    argv[i][1]='(';
    return 0;
  }
  if (!strcmp(argv[i], "-parent"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
	return 1;
    }
    return 0;
  }

  if (!strcmp(argv[i], "-imgstop"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
	return 1;
    }
    return 0;
  }

  if (!strcmp(argv[i], "-getifocus"))
  {
    argv[i][1]='(';
    return 0;
  }

  if (!strcmp(argv[i], "-karma"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
	return 1;
    }
    return 0;
  }

  if (!strcmp(argv[i], "-bstimeout"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
	return 1;
    }
    return 0;
  }

  if (!strcmp(argv[i], "-imgframe"))
  {
#ifdef __sun
    putenv("NX_IMAGEFRAME");
#else
    setenv("NX_IMAGEFRAME", "", 1);
#endif
    return 0;
  }

  if (!strcmp(argv[i], "-imgsplit"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
#ifdef __sun
        char envBuffer[512];
        sprintf(envBuffer,"NX_IMAGESPLIT=%s",argv[i]);
        putenv(envBuffer);
#else
        setenv("NX_IMAGESPLIT", argv[i], 1);
#endif
#ifdef __sun
      return 2;
#else
      return 1;
#endif
    }
    return 0;
  }

  if (!strcmp(argv[i], "-imgcolors"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
#ifdef __sun
       char envBuffer[512];
       sprintf(envBuffer,"NX_IMAGEMASK=%s",argv[i]);
       putenv(envBuffer);
#else
       setenv("NX_IMAGEMASK", argv[i], 1);
#endif
#ifdef __sun
      return 2;
#else
      return 1;
#endif
    }
    return 0;
  }

  if (!strcmp(argv[i], "-autoexpose"))
  {
    argv[i][1]='(';
    return 0;
  }


  if (!strcmp(argv[i], "-backingstore"))
  {
    argv[i][1]='(';
    return 0;
  }

  if (!strcmp(argv[i], "-maxbsarea"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
       return 1;
    }
    return 0;
  }
  if (!strcmp(argv[i], "-minbsarea"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
       return 1;
    }
    return 0;
  }
  if (!strcmp(argv[i], "-safe"))
  {
    argv[i][1]='(';
    return 0;
  }

  if (!strcmp(argv[i], "-noreset"))
  {
    argv[i][1]='(';
    return 0;
  }

  if (!strcmp(argv[i], "-fp"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
       return 1;
    }
    return 0;
  }

  if (!strcmp(argv[i], "-auth"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
       return 1;
    }
    return 0;
  }

  if (!strcmp(argv[i], "-bs"))
  {
    argv[i][1]='(';
    return 0;
  }

  if (!strcmp(argv[i], "+bs"))
  {
    argv[i][0]='-';
    argv[i][1]='(';
    return 0;
  }

  if (!strcmp(argv[i], "-keyboard"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
        keylayout = strtol(argv[i], NULL, 16);
        if (keylayout == 0)
        {
           error("Invalid keyboard layout\n");
        }
#ifdef __sun
      return 2;
#else
      return 1;
#endif
    }
  }
  if (!strcmp(argv[i], "-passwd"))
  {
    argv[i][1]='(';
    if (++i < argc)
    {
       passwordFile = &(argv[i])[0];
#ifdef __sun
      return 2;
#else
      return 1;
#endif
    }
    return 0;
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
    default:
      fprintf(stderr, "Warning: unknown keyboard layout [%X].\n", keylayout);
      strcpy(keymapname, "en-us");
 }
 DEBUG_KBD(("keymapname is [%s].\n", keymapname));
}






