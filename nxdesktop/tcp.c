/*
   rdesktop: A Remote Desktop Protocol client.
   Protocol services - TCP layer
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

#include <unistd.h>		/* select read write close */
#include <sys/socket.h>		/* socket connect setsockopt */
#include <sys/time.h>		/* timeval */
#include <netdb.h>		/* gethostbyname */
#include <netinet/in.h>		/* sockaddr_in */
#include <netinet/tcp.h>	/* TCP_NODELAY */
#include <arpa/inet.h>		/* inet_addr */
#include <errno.h>		/* errno */
#include "rdesktop.h"

#undef  NXDESKTOP_TCP_DEBUG
#undef  NXDESKTOP_TCP_DUMP

extern int rdp_bufsize;

#ifdef NXDESKTOP_TCP_DUMP

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

static int rdpDump = -1;

#endif

static int sock;
static struct stream in;
static struct stream out;

extern int  nxdesktopStopKarmaSz;
static long bytesFromLastKarma = 0;

#ifdef NXDESKTOP_TCP_DEBUG
static unsigned long tcpRead;
static unsigned long tcpWritten;
#endif

/* Initialise TCP transport data packet */
STREAM
tcp_init(int maxlen)
{
	#ifdef NXDESKTOP_TCP_DUMP
	if (rdpDump == -1)
        {
		rdpDump = open("/tmp/rdp.dump", O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU);
	}
	#endif

	if ((unsigned int)maxlen > out.size)
	{
		out.data = xrealloc(out.data, maxlen);
		out.size = maxlen;
	}

	out.p = out.data;
	out.end = out.data + out.size;
	return &out;
}

/* Send TCP transport data packet */
void
tcp_send(STREAM s)
{
	int length = s->end - s->data;
	int sent, total = 0;

	#ifdef NXDESKTOP_TCP_DEBUG
	int kiloWritten = tcpWritten / 1024;
	#endif

	while (total < length)
	{
		sent = send(sock, s->data + total, length - total, 0);
		if (sent <= 0)
		{
			error("send: %s\n", strerror(errno));
			return;
		}

		#ifdef NXDESKTOP_TCP_DEBUG
		tcpWritten += sent;
		#endif

		total += sent;
	}

	#ifdef NXDESKTOP_TCP_DEBUG
	if ((tcpWritten / 1024) > kiloWritten)
	{
		fprintf(stderr, "tcp_send: %ld total bytes written.\n", tcpWritten);
	}
	#endif
}

/* Receive a message on the TCP layer */
STREAM
tcp_recv(int length)
{
	int rcvd = 0;
	/* NX */
	char errorMsg[512];
	char errorCaption[512];
	extern char *nxDisplay;
	/* NX */
#ifdef NXDESKTOP_TCP_DEBUG
	int kiloRead = tcpRead / 1024;
#endif

        if ((unsigned int)length > in.size)
	{
		in.data = xrealloc(in.data, length);
		in.size = length;
	}

	in.end = in.p = in.data;

#ifdef NXDESKTOP_USES_NXKARMA_IN_LOOP
        bytesFromLastKarma += length;
#endif

	while (length > 0)
	{

                ui_select(sock, False);

                rcvd = recv(sock, in.end, length, 0);

		if (rcvd == -1)
		{
		    //error("recv: %s\n", strerror(errno));
		    snprintf(errorMsg,511,"Connection to RDP server failed.\nError is %d, '%s'.",errno,strerror(errno));
		    snprintf(errorCaption,511,"Error");
		    NXDialog(errorCaption, errorMsg, "ok", 0, nxDisplay );
		    wait(NULL);
		    //return False
		    return NULL;
		}

		#ifdef NXDESKTOP_TCP_DUMP
		if (rdpDump != -1 && rcvd > 0)
		{
			write(rdpDump, in.end, rcvd);
		}
		#endif

/*                fprintf(stderr,"tcp_recv receive [%d] bytes\n", rcvd);
*/
		#ifdef NXDESKTOP_TCP_DEBUG
		tcpRead += rcvd;
		#endif

		in.end += rcvd;
		length -= rcvd;
	}

	#ifdef NXDESKTOP_TCP_DEBUG
	if ((tcpRead / 1024) > kiloRead)
	{
		fprintf(stderr, "tcp_recv: %ld total bytes read.\n", tcpRead);
	}
	#endif

#ifdef NXDESKTOP_USES_NXKARMA_IN_LOOP
        if (bytesFromLastKarma >= nxdesktopStopKarmaSz)
        {
#ifdef NXDESKTOP_NXKARMA_DEBUG
                fprintf(stderr,"Karma will be sent, received bytes [%d]\n", bytesFromLastKarma);
#endif
                ui_select(sock, True);
                bytesFromLastKarma = 0;
        }
#endif


	return &in;
}

/* Establish a connection on the TCP layer */
BOOL
tcp_connect(char *server)
{
	struct hostent *nslookup;
	struct sockaddr_in servaddr;
	int true = 1;
	extern char *nxDisplay;
	extern char *windowName;
	char errorMsg[512];
	char errorCaption[512];

	if ((nslookup = gethostbyname(server)) != NULL)
	{
		memcpy(&servaddr.sin_addr, nslookup->h_addr,
		       sizeof(servaddr.sin_addr));
	}
	else if (!(servaddr.sin_addr.s_addr = inet_addr(server)))
	{
		error("%s: unable to resolve host\n", server);
		return False;
	}

	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
		error("socket: %s\n", strerror(errno));
		return False;
	}

	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons(TCP_PORT_RDP);

	if (connect
	    (sock, (struct sockaddr *) &servaddr,
	     sizeof(struct sockaddr)) < 0)
	{
		error("connect: %s\n", strerror(errno));
		close(sock);
		snprintf(errorMsg,511,"Connection to RDP server '%s' failed.\nError is %d, '%s'.",server,errno,strerror(errno));
		snprintf(errorCaption,511,"%s",windowName);
		NXDialog(errorCaption, errorMsg, "ok", 0, nxDisplay );
		wait(NULL);
		return False;
	}

	setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *) &true,
		   sizeof(true));

	/*
	 * Set TCP read buffer (to a smaller value,
         * we suppose) to increase interactivity.
	 */

	if (rdp_bufsize > 0 &&
		 setsockopt(sock, SOL_SOCKET, SO_RCVBUF,
				&rdp_bufsize, sizeof(rdp_bufsize)) < 0)
	{
		fprintf(stderr, "tcp_connect: Cannot resize receive buffer to %d. Error is '%s'.\n",
					rdp_bufsize, strerror(errno));
		close(sock);

		return False;
	}

	in.size = 4096;
	in.data = xmalloc(in.size);

	out.size = 4096;
	out.data = xmalloc(out.size);

	return True;
}

/* Disconnect on the TCP layer */
void
tcp_disconnect()
{
	close(sock);

	#ifdef NXDESKTOP_TCP_DUMP
	if (rdpDump != -1)
	{
		close(rdpDump);
	}
	#endif
}


void
tcp_resize_buf(int fd, int sense, int size)
{
   if (size > 0)
   {
     setsockopt(fd, SOL_SOCKET, sense?SO_RCVBUF:SO_SNDBUF, &size, sizeof(int));
   }
}
