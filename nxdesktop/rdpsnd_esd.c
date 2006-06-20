/* 
   rdesktop: A Remote Desktop Protocol client.
   Sound Channel Process Functions - ESD
   Copyright (C) Matthew Chapman 2003
   Copyright (C) GuoJunBo guojunbo@ict.ac.cn 2003

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

#include "rdesktop.h"
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/soundcard.h>
#include <esd.h>
#include <genrand.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <NX.h>

#define NXDESKTOP_SOUND_ESD

#define MAX_QUEUE	10

static int g_snd_rate = -1;
static short g_samplewidth;
static int esd_flags = -1;
int fds[2];
int g_dsp_fd = -1;
int int_sock;
BOOL g_dsp_busy = False;
esd_info_t *esd_info;
esd_player_info_t *esd_player_info;
extern int rdp_socket;
extern int g_x_socket;

static struct audio_packet
{
	struct stream s;
	uint16 tick;
	uint8 index;
} packet_queue[MAX_QUEUE];
static unsigned int queue_hi, queue_lo;

void
do_select(int socket)
{
	int n;
	fd_set rfds, wfds;
	struct timeval tv;
	BOOL s_timeout = False;
	return;
	    FD_ZERO(&rfds);
	    FD_ZERO(&wfds);
	    FD_SET(socket, &rfds);
	    FD_SET(socket, &wfds);
	    n = socket;
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
			error("do_select: %s\n", strerror(errno));
		    }
		    else
		    {
			#ifdef NXDESKTOP_XWIN_SELECT_DEBUG
			warning("do_select: EINTR fired!\n");
			#endif
			goto Retry;
		    }
		case 0:
		    if (s_timeout)
			rdpdr_check_fds(&rfds, &wfds, (BOOL) True);
	    }
		
	    rdpdr_check_fds(&rfds, &wfds, (BOOL) False);
		
}

int esd_set_socket_buffers( int sock, int src_format, 
			    int src_rate, int base_rate )
{
    int buf_size = ESD_BUF_SIZE;

    if ( src_rate > 0 ) buf_size = ( buf_size * base_rate ) / src_rate;
    if ( ( src_format & ESD_MASK_BITS ) == ESD_BITS16 )
	buf_size *= 2;
    if ( ! ( ( src_format & ESD_MASK_CHAN ) == ESD_MONO ) )
	buf_size *= 2;

    setsockopt( sock, SOL_SOCKET, SO_SNDBUF, &buf_size, sizeof( buf_size ) );
    setsockopt( sock, SOL_SOCKET, SO_RCVBUF, &buf_size, sizeof( buf_size ) );
    return buf_size;
}

void
flush_sound()
{
    /* Ack all remaining packets */
    while (queue_lo != queue_hi)
    {
	rdpsnd_send_completion(packet_queue[queue_lo].tick, packet_queue[queue_lo].index);
	free(packet_queue[queue_lo].s.data);
	queue_lo = (queue_lo + 1) % MAX_QUEUE;
    }
}

int
esd_connect()
{

    int val;
    
	nxdesktopDebug("esd_connect","fds setting\n");
	if (socketpair(PF_LOCAL, SOCK_STREAM, 0, fds) < 0)
	{
	    error("esd_connect: Unable to create pair of sockets.\n");
	    return False;
	}	 
	else
	{
	    if((val = fcntl(fds[0], F_GETFL, 0)) == -1)
		return False;
	    if (!(val & O_NONBLOCK)) 
	    {
		val |= O_NONBLOCK;
		fcntl(fds[0], F_SETFL, val);
	    }
	    /* experimental */
	    if((val = fcntl(fds[1], F_GETFL, 0)) == -1)
		return False;
	    if (!(val & O_NONBLOCK)) 
	    {
		val |= O_NONBLOCK;
		fcntl(fds[1], F_SETFL, val);
	    }
	}
	if (NXTransChannel(NX_FD_ANY, fds[1], NX_CHANNEL_MEDIA) <= 0)
	{
	    error("esd_connect: NXTransChannel failed.\n");
	    goto error_out;
	}
    NXTransContinue(NULL);
    return fds[0];

 error_out:
    if (fds[0] >= 0)
	close(fds[0]);
    if (fds[1] >= 0)
	close(fds[1]);
    return False;
}

int nx_esd_send_auth( int sock )
{
    int auth_fd = -1;
    int endian = ESD_ENDIAN_KEY;
    int reply;
    char *auth_filename = NULL;
    unsigned char auth_key[ESD_KEY_LEN];
    const char *home = NULL;
    int namelen, retval;
  
    /* assemble the authorization filename */
    home = getenv( "HOME" );
    if ( !home ) 
    {
	fprintf( stderr, "HOME environment variable not set?\n" );
	return -1;
    }

    namelen = strlen(home) + sizeof("/.esd_auth");
    if ((auth_filename = malloc(namelen + 1)) == 0) 
    {
	fprintf( stderr, "Memory exhausted\n" );
	return -1;
    }
    strcpy( auth_filename, home );
    strcat( auth_filename, "/.esd_auth" );

    retval = 0;
    /* open the authorization file */
    if ( -1 == (auth_fd = open( auth_filename, O_RDONLY ) ) ) 
    {
	/* it doesn't exist? create one */
	auth_fd = open( auth_filename, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR );

	if ( -1 == auth_fd ) 
	{
	    /* coun't even create it?  bail */
	    perror( auth_filename );
	    goto exit_fn;
	}
	esound_genrand(auth_key, ESD_KEY_LEN);
	write( auth_fd, auth_key, ESD_KEY_LEN);
    } 
    else
      /* read the key from the authorization file */
    if ( ESD_KEY_LEN != read( auth_fd, auth_key, ESD_KEY_LEN ) )
    {
	goto exit_fd;
    } 
    
    /* send the key to the server */
    if ( ESD_KEY_LEN != write( sock, auth_key, ESD_KEY_LEN ) )
    {
	/* send key failed */
	error("Auth failed. Reason is %d: %s.\n",errno,strerror(errno));
	goto exit_fd;
    }

    /* send the key to the server */
    if ( sizeof(endian) != write( sock, &endian, sizeof(endian) ) )
    {
	/* send key failed */
	goto exit_fd;
    }

    /* read auth reply. esd will reply 1 as an int for yes and 0 for no */
    /* then close the connection */
    if ( sizeof(reply) != read( fds[1], &reply, sizeof(reply) ) ) 
    {
	/* read ok failed */
	retval = 0;
	goto exit_fd;
    }
    /* we got a reply and it's no - so esd will close the socket now */
    /* on us anyway... time to return invalid auth... */
    if (reply == 0) 
    {
	/* auth failed */
	retval = 0;
	goto exit_fd;
    }
  
    /* we've run the gauntlet, everything's ok, proceed as usual */
    /* fsync( sock ); */
    retval = 1;

 exit_fd:
    close( auth_fd );
 exit_fn:
    free( auth_filename );
    return retval;
}


BOOL
wave_out_open(void)
{
    #ifdef NXDESKTOP_SOUND_ESD
    nxdesktopDebug("wave_out_open:","called.\n");
    #endif
    /* Open ESD with the desired parameters. */
    if (g_dsp_fd <= 0)
    {
	int_sock=esd_connect();
	do_select(int_sock);
	if ((nx_esd_send_auth(int_sock))<=0) 
	{
	    error("wave_out_open: unable to connect to ESD.\n");
	    return False;
	} else
	{
	    nxdesktopDebug("wave_out_open","Socket for sound channel %d openned\n",int_sock);
	}
	g_dsp_fd = 0;
    }
    return True;
}

void
wave_out_close(void)
{
    #ifdef NXDESKTOP_SOUND_ESD
    nxdesktopDebug("wave_out_close:","called.\n");
    #endif
    flush_sound();
    if (int_sock)
	close(int_sock);
}

BOOL
wave_out_format_supported(WAVEFORMATEX * pwfx)
{
    
    #ifdef NXDESKTOP_SOUND_ESD
    nxdesktopDebug("wave_out_format_supported:","Audio format required PCM %d Channels %d BPS %d.\n",
		   pwfx->wFormatTag == WAVE_FORMAT_PCM, pwfx->nChannels, pwfx->wBitsPerSample);
    #endif

    if (pwfx->wFormatTag != WAVE_FORMAT_PCM)
    	return False;
    if ((pwfx->nChannels != 1) && (pwfx->nChannels != 2))
	return False;
    if ((pwfx->wBitsPerSample != 8) && (pwfx->wBitsPerSample != 16))
	return False;
    return True;
}

BOOL
wave_out_set_format(WAVEFORMATEX * pwfx)
{

    int proto = ESD_PROTO_STREAM_PLAY;
    char name_buf [ESD_NAME_MAX];

    #ifdef NXDESKTOP_SOUND_ESD
    nxdesktopDebug("wave_out_set_format:","Audio format to set PCM %d Channels %d BPS %d.\n",
	pwfx->wFormatTag == WAVE_FORMAT_PCM, pwfx->nChannels, pwfx->wBitsPerSample);
    #endif
    
    if (pwfx->wBitsPerSample == 8)
	esd_flags = ESD_BITS8;
    else if (pwfx->wBitsPerSample == 16)
	esd_flags = ESD_BITS16;

    g_samplewidth = pwfx->wBitsPerSample / 8;

    if (pwfx->nChannels == 2)
    {
	esd_flags |= ESD_STEREO;
	g_samplewidth *= 2;
    }
    else
    {
	esd_flags |= ESD_MONO;
    }
    g_snd_rate = pwfx->nSamplesPerSec;
    esd_flags |= ESD_PLAY | ESD_STREAM;
	
    strncpy( name_buf, "nxdesktop", ESD_NAME_MAX);
    
    do_select(int_sock);
    
    if ( write( int_sock, &proto, sizeof(proto) ) != sizeof(proto) ) 
    {
	nxdesktopDebug("wave_out_write:","error writting proto.\n");
	return False;
    }
    
    if ( write( int_sock, &esd_flags, sizeof(esd_flags) ) != sizeof(esd_flags) ) 
    {
	nxdesktopDebug("wave_out_write:","error writting flags.\n");
	return False;
    }
    
    if( write( int_sock, &g_snd_rate, sizeof(g_snd_rate) ) != sizeof(g_snd_rate) ) 
    {
	nxdesktopDebug("wave_out_write:","error writting bitrate.\n");
	return False;
    }
	
    if( write( int_sock, name_buf, ESD_NAME_MAX ) != ESD_NAME_MAX ) 
    {
	nxdesktopDebug("wave_out_write:","error writting stream ID name.\n");
	return False;
    }

    /* Reduce buffers on sockets to the minimum needed */
    esd_set_socket_buffers(int_sock, esd_flags, g_snd_rate, 44100 );

    return True;
}

void
wave_out_volume(uint16 left, uint16 right)
{
    /* Do nothing for now */
    return;
    uint32 volume;
    int fd_mix = -1;

    volume = left / (65536 / 100);
    volume |= right / (65536 / 100) << 8;

    if ((fd_mix = open("/dev/mixer", O_RDWR | O_NONBLOCK)) == -1)
    {
	perror("open /dev/mixer");
	return;
    }

    if (ioctl(fd_mix, MIXER_WRITE(SOUND_MIXER_PCM), &volume) == -1)
    {
	perror("MIXER_WRITE(SOUND_MIXER_PCM)");
	return;
    }

    close(fd_mix);
    
    
    /* FIX ME - I can't find a reliable way to find the esd_player id yet
    int volume_left, volume_right;

    volume_left = left / (256/100);
    volume_right = right / (256/100);
    
    #ifdef NXDESKTOP_SOUND_ESD
    nxdesktopDebug("wave_out_volume:","called.\n");
    #endif
    
    
    if (!esd_set_stream_pan(esd_server, esd_player, volume_left, volume_right))
        warning("Set volume failed\n");
    */
}

void
wave_out_write(STREAM s, uint16 tick, uint8 index)
{
	struct audio_packet *packet = &packet_queue[queue_hi];
	unsigned int next_hi = (queue_hi + 1) % MAX_QUEUE;
	
	
	#ifdef NXDESKTOP_SOUND_ESD
	nxdesktopDebug("wave_out_write:","called.\n");
	#endif
	if (next_hi == queue_lo)
	{
		error("No space to queue audio packet\n");
		return;
	} 

	queue_hi = next_hi;

	packet->s = *s;
	packet->tick = tick;
	packet->index = index;
	packet->s.p += 4;

	/* we steal the data buffer from s, give it a new one */
	s->data = (uint8 *) malloc(s->size);

	if (!g_dsp_busy)
	{
	    do_select(int_sock);
	    wave_out_play();
	}
}

void
wave_out_play(void)
{

    /* This is broken yet */
	struct audio_packet *packet;
	ssize_t len;
	STREAM out;
	static long startedat_us;
	static long startedat_s;
	static BOOL started = False;
	struct timeval tv;
	audio_buf_info info;
	
	#ifdef NXDESKTOP_SOUND_ESD
	nxdesktopDebug("wave_out_play:","called.\n");
	#endif

	while (1)
	{
		if (queue_lo == queue_hi)
		{
			g_dsp_busy = 0;
			return;
		}

		packet = &packet_queue[queue_lo];
		out = &packet->s;
		return;

		if (!started)
		{
			gettimeofday(&tv, NULL);
			startedat_us = tv.tv_usec;
			startedat_s = tv.tv_sec;
			started = True;
		}

		len = out->end - out->p;

		/* Test the session before run this block? */
			memset(&info, 0, sizeof(info));

			if (info.fragments * info.fragsize < len
			    && info.fragments * info.fragsize > 0)
			{
				len = info.fragments * info.fragsize;
			}
		
		/* until here */
		len = write(int_sock, out->p, len);
		fprintf(stderr,"len = %d\n",len);
		if (len == -1)
		{
			if (errno != EWOULDBLOCK)
				perror("write audio");
			g_dsp_busy = 1;
			nxdesktopDebug("wave_out_play:","write to esd socket error.\n");
			return;
		} 
		out->p += len;
		if (out->p == out->end)
		{
			long long duration;
			long elapsed;

			gettimeofday(&tv, NULL);
			duration = (out->size * (1000000 / (g_samplewidth * g_snd_rate)));
			elapsed = (tv.tv_sec - startedat_s) * 1000000 + (tv.tv_usec - startedat_us);

			if (elapsed >= (duration * 85) / 100)
			{
				rdpsnd_send_completion(packet->tick, packet->index);
				free(out->data);
				queue_lo = (queue_lo + 1) % MAX_QUEUE;
				started = False;
			}
			else
			{
				g_dsp_busy = 1;
				return;
			}
		}
	}
}
