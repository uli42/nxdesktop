/*
   rdesktop: A Remote Desktop Protocol client.
   Protocol services - TCP layer
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

#include <unistd.h>		/* select read write close */
#include <sys/socket.h>		/* socket connect setsockopt */
#include <sys/time.h>		/* timeval */
#include <netdb.h>		/* gethostbyname */
#include <netinet/in.h>		/* sockaddr_in */
#include <netinet/tcp.h>	/* TCP_NODELAY */
#include <arpa/inet.h>		/* inet_addr */
#include <errno.h>		/* errno */
#include <signal.h>		/* sigaction - NX */
#include <NXlib.h>
#include "rdesktop.h"

#ifndef INADDR_NONE
#define INADDR_NONE ((unsigned long) -1)
#endif

// NX
#undef  NXDESKTOP_TCP_DEBUG
#undef  NXDESKTOP_TCP_DUMP

extern int rdp_bufsize;

#ifdef NXDESKTOP_TCP_DUMP

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

static int rdpDump = -1;

#endif
/* NX */
static BOOL sock_done = False;
static int sock;
static struct stream in;
static struct stream out;
int g_tcp_port_rdp = TCP_PORT_RDP;
extern BOOL nxdesktopUseNXTrans;
/* NX */
char errorMsg[512];
char errorCaption[512];
extern char nxDisplay[255];
extern Display *g_display;
extern char windowName[255];

extern int  nxdesktopStopKarmaSz;
static long bytesFromLastKarma = 0;
static struct sigaction sigact;

#ifdef NXDESKTOP_TCP_DEBUG
static unsigned long tcpRead;
static unsigned long tcpWritten;
#endif
// NX

/* Initialise TCP transport data packet */
STREAM
tcp_init(uint32 maxlen)
{
	#ifdef NXDESKTOP_TCP_DUMP
	if (rdpDump == -1)
        {
		rdpDump = open("/tmp/rdp.dump", O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU);
	}
	#endif
	
	if (maxlen > out.size)
	{
		out.data = (uint8 *) xrealloc(out.data, maxlen);
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
		alarm(30);
		sent = send(sock, s->data + total, length - total, 0);
		alarm(0);
		
		if (sent <= 0)
		{
			error("tcp_send: %s\n", strerror(errno));
			s = NULL;
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
		nxdesktopDebug("tcp_send","%ld total bytes written.\n", tcpWritten);
	}
	#endif
}

/* NX */
static Bool SyncPredicate(Display *display, XEvent *event, XPointer ptr)
{
  return (event -> xclient.data.l[0] == NXSyncNotify &&
              event -> type == ClientMessage && event -> xclient.window == 0 &&
                      event -> xclient.message_type == 0 && event -> xclient.format == 32);
}
/* NX */

/* Receive a message on the TCP layer */
STREAM
tcp_recv(STREAM s, uint32 length)
{
	unsigned int new_length, end_offset, p_offset;
	int rcvd = 0;
	XEvent event;
	
#ifdef NXDESKTOP_TCP_DEBUG
	int kiloRead = tcpRead / 1024;
#endif
	
	if (s == NULL)
	{
		/* read into "new" stream */
		if (length > in.size)
		{
			in.data = (uint8 *) xrealloc(in.data, length);
			in.size = length;
		}
		in.end = in.p = in.data;
		s = &in;
	}
	else
	{
		/* append to existing stream */
		new_length = (s->end - s->data) + length;
		if (new_length > s->size)
		{
			p_offset = s->p - s->data;
			end_offset = s->end - s->data;
			s->data = (uint8 *) xrealloc(s->data, new_length);
			s->size = new_length;
			s->p = s->data + p_offset;
			s->end = s->data + end_offset;
		}
	}

	while (length > 0)
	{
	    if (!ui_select(sock))
	    {
		/* User quit */
		return NULL;
		s = NULL;
	    }

		alarm(30);
		rcvd = recv(sock, s->end, length, 0);
		alarm(0);
		
		if (rcvd < 0)
		{
			error("tcp_recv: Receiving error %s\n", strerror(errno));
			close(sock);
			if (nxDisplay[0] != 0)
			{
			    snprintf(errorMsg,511,"The RDP server closed the connection.\nPlease report this problem to support\npersonnel.\nError is %d.",errno);
		    	    snprintf(errorCaption,511,"Error");
		    	    NXDialog(errorCaption, errorMsg, "ok", 0, (char *)nxDisplay );
		    	    wait(NULL);
			}
			s = NULL;
			return NULL;
		}
		else if (rcvd == 0)
		{
			error("tcp_recv: Connection closed\n");
			s = NULL;
			return NULL;
		}
		
		/* NX */
		#ifdef NXDESKTOP_TCP_DUMP
		if (rdpDump != -1 && rcvd > 0)
		{
			write(rdpDump, in.end, rcvd);
		}
		#endif
		
		#ifdef NXDESKTOP_USES_NXKARMA_IN_LOOP
		if (nxdesktopUseNXTrans)
		{
		    bytesFromLastKarma += rcvd;
		    if (bytesFromLastKarma >= nxdesktopStopKarmaSz)
        		{
			    #ifdef NXDESKTOP_NXKARMA_DEBUG
                	    nxdesktopDebug("tcp_recv","Karma will be sent, received bytes [%ld]\n", bytesFromLastKarma);
			    #endif
			
			    NXSync(g_display, NXSyncWaitForLink, 0);
			    while (!XCheckIfEvent(g_display, &event, SyncPredicate, NULL))
    			    {
			    }
			    #ifdef NXDESKTOP_NXKARMA_DEBUG
                	    nxdesktopDebug("tcp_recv","Karma received\n");
			    #endif
                	    bytesFromLastKarma = 0;
			}
		}
		#endif
				
		#ifdef NXDESKTOP_TCP_DEBUG
		tcpRead += rcvd;
		#endif
		/* NX */
		s->end += rcvd;
		length -= rcvd;
	}
	
	/* NX */
	#ifdef NXDESKTOP_TCP_DEBUG
	if ((tcpRead / 1024) > kiloRead)
	{
	    nxdesktopDebug("tcp_recv","%ld total bytes read.\n", tcpRead);
	}
	#endif

	/* NX */
	
	return s;
}

/* Establish a connection on the TCP layer */
BOOL
tcp_connect(char *server)
{
	int true_value = 1;
	

#ifdef IPv6

	int n;
	struct addrinfo hints, *res, *ressave;
	char tcp_port_rdp_s[10];

	if (sock_done)
	    return True;
	snprintf(tcp_port_rdp_s, 10, "%d", tcp_port_rdp);

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;

	if ((n = getaddrinfo(server, tcp_port_rdp_s, &hints, &res)))
	{
		error("getaddrinfo failed: %s\n", gai_strerror(n));
		return False;
	}

	ressave = res;
	sock = -1;
	while (res)
	{
		sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
		if (!(sock < 0))
		{
			if (connect(sock, res->ai_addr, res->ai_addrlen) == 0)
				break;
			close(sock);
			sock = -1;
		}
		res = res->ai_next;
	}
	freeaddrinfo(ressave);

	if (sock == -1)
	{
		error("Unable to connect to '%s'\n", server);
		return False;
	}

#else /* no IPv6 support */

	struct hostent *nslookup;
	struct sockaddr_in servaddr;

	if (sock_done)
	    return True;
	if ((nslookup = gethostbyname(server)) != NULL)
	{
		memcpy(&servaddr.sin_addr, nslookup->h_addr, sizeof(servaddr.sin_addr));
	}
	else if ((servaddr.sin_addr.s_addr = inet_addr(server)) == INADDR_NONE)
	{
		error("Unable to resolve host '%s'\n", server);
		if (nxDisplay[0] != 0)
		{
		    snprintf(errorMsg,511,"Connection to RDP server '%s' failed.\nUnable to resolve host.",server);
		    snprintf(errorCaption,511,"%s",(char *)windowName);
		    NXDialog(errorCaption, errorMsg, "ok", 0, (char *)nxDisplay );
		    wait(NULL);
		}
		return False;
	}

	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
		error("Connection to RDP server '%s' failed. Reason is %d: %s.\n",server,errno,strerror(errno));
		if (nxDisplay[0] != 0)
		{
		    snprintf(errorMsg,511,"Connection to RDP server '%s' failed.\nError is %d, '%s'.",server,errno,strerror(errno));
		    snprintf(errorCaption,511,"%s",(char *)windowName);
		    NXDialog(errorCaption, errorMsg, "ok", 0, (char *)nxDisplay );
		    wait(NULL);
		}
		return False;
	}

	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons(g_tcp_port_rdp);
	
	/* NX */
	sigaction(SIGALRM, NULL, &sigact);
	sigact.sa_handler = AlarmHandler;
	sigact.sa_flags &= ~SA_RESTART;
	sigaction(SIGALRM, &sigact, NULL); 

	//alarm(30);
	/* NX */

	if (connect(sock, (struct sockaddr *) &servaddr, sizeof(struct sockaddr)) < 0)
	{
		alarm(0);
		if (errno == 4)
		{
			errno = 110;
		}
		error("Connection to RDP server '%s' failed. Reason is %d: %s.\n",server,errno,strerror(errno));
		close(sock);
		sigaction(SIGALRM, NULL, &sigact);
		if (nxDisplay[0] != 0)
		{
		    snprintf(errorMsg,511,"Connection to RDP server '%s' failed.\nError is %d, '%s'.",server,errno,strerror(errno));
		    snprintf(errorCaption,511,"%s",(char *)windowName);
		    NXDialog(errorCaption, errorMsg, "ok", 0, (char *)nxDisplay );
		    wait(NULL);
		}
		return False;
	}

#endif /* IPv6 */

	setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *) &true_value, sizeof(true_value));
	
	/* NX */
	/*
	 * Set TCP read buffer (to a smaller value,
         * we suppose) to increase interactivity.
	 */

	if (rdp_bufsize > 0 &&
		 setsockopt(sock, SOL_SOCKET, SO_RCVBUF,
				&rdp_bufsize, sizeof(rdp_bufsize)) < 0)
	{
		error("Cannot resize receive buffer to %d. Error is '%s'.\n",
					rdp_bufsize, strerror(errno));
		close(sock);

		return False;
	}
	
	/* NX */
	
	in.size = 4096;
	in.data = (uint8 *) xmalloc(in.size);

	out.size = 4096;
	out.data = (uint8 *) xmalloc(out.size);
	
	sock_done = True;
	return True;
}

/* Disconnect on the TCP layer */
void
tcp_disconnect(void)
{
	close(sock);
	#ifdef NXDESKTOP_TCP_DUMP
	if (rdpDump != -1)
	{
		close(rdpDump);
	}
	#endif
}

char *
tcp_get_address()
{
	static char ipaddr[32];
	struct sockaddr_in sockaddr;
	socklen_t len = sizeof(sockaddr);
	if (getsockname(sock, (struct sockaddr *) &sockaddr, &len) == 0)
	{
		unsigned char *ip = (unsigned char *) &sockaddr.sin_addr;
		sprintf(ipaddr, "%d.%d.%d.%d", ip[0], ip[1], ip[2], ip[3]);
	}
	else
		strcpy(ipaddr, "127.0.0.1");
	
	return ipaddr;
}

/* NX */
void
tcp_resize_buf(int fd, int sense, int size)
{
   if (size > 0)
   {
     setsockopt(fd, SOL_SOCKET, sense?SO_RCVBUF:SO_SNDBUF, &size, sizeof(int));
   }
}

void
AlarmHandler(int signal)
{
    error("Connection timed out. Session closing\n");
    alarm(0);
    exit(1);
}
/* NX */
