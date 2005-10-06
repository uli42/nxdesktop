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

#include "rdesktop.h"
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/soundcard.h>
#include <esd.h>

#undef NXDESKTOP_SOUND_ESD

#define MAX_QUEUE	10

static int g_snd_rate = -1;
static short g_samplewidth;
static int esd_flags = -1;
int g_dsp_fd;
BOOL g_dsp_busy = False;
int esd_server;
int esd_player;
esd_info_t *esd_info;
esd_player_info_t *esd_player_info;

static struct audio_packet
{
	struct stream s;
	uint16 tick;
	uint8 index;
} packet_queue[MAX_QUEUE];
static unsigned int queue_hi, queue_lo;

BOOL
wave_out_open(void)
{
    #ifdef NXDESKTOP_SOUND_ESD
    nxdesktopDebug("wave_out_open:","called.\n");
    #endif
    /* Open ESD with the desired parameters. */
    if (!esd_server)
    {
	if ((esd_server = esd_open_sound(NULL))<=0) 
	{
	    error("wave_out_open: unable to connect to ESD.\n");
	    return False;
	}
    }
    return True;
}

void
wave_out_close(void)
{
    #ifdef NXDESKTOP_SOUND_ESD
    nxdesktopDebug("wave_out_close:","called.\n");
    #endif
    if (esd_server)
	close(esd_server);
    if (g_dsp_fd)
	close(g_dsp_fd);
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
	
	if (g_dsp_fd)
	    close(g_dsp_fd);
	g_dsp_fd = esd_play_stream(esd_flags | ESD_PLAY | ESD_STREAM, g_snd_rate, NULL,"nxdesktop");

	return True;
}

void
wave_out_volume(uint16 left, uint16 right)
{

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
    
    
    if (!esd_set_stream_pan(esd_server, esd_player, 10,10));//volume_left, volume_right))
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
		wave_out_play();
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

		len = write(g_dsp_fd, out->p, len);
		if (len == -1)
		{
			if (errno != EWOULDBLOCK)
				perror("write audio");
			g_dsp_busy = 1;
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
