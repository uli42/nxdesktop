#
# rdesktop: A Remote Desktop Protocol client
# Makefile
# Copyright (C) Matthew Chapman 1999-2001
#

# Configuration defaults

CC       = cc
CFLAGS   = -O2 -Wall
INCLUDES = -I/usr/X11R6/include -I../nx-X11/exports/include -I../nxcomp  -I../nxcompext 
LDLIBS   = -L../nx-X11/exports/lib -lX11 -lXpm -L../nxcompext -lXcompext -L../nxcomp -lXcomp

PREFIX   = /usr/local
EPREFIX  = $(PREFIX)
BINDIR   = $(EPREFIX)/bin
MANDIR   = $(PREFIX)/man

RDPOBJ   = rdesktop.o tcp.o iso.o mcs.o secure.o licence.o rdp.o orders.o bitmap.o cache.o xwin.o xkeymap.o
CRYPTOBJ = crypto/rc4_enc.o crypto/rc4_skey.o crypto/md5_dgst.o crypto/sha1dgst.o crypto/bn_exp.o crypto/bn_mul.o crypto/bn_div.o crypto/bn_sqr.o crypto/bn_add.o crypto/bn_shift.o crypto/bn_asm.o crypto/bn_ctx.o crypto/bn_lib.o

include Makeconf  # local configuration

nxdesktop: $(RDPOBJ) $(CRYPTOBJ)
	$(CC) $(CFLAGS) -o nxdesktop $(RDPOBJ) $(CRYPTOBJ) $(LDDIRS) $(LDLIBS)

Makeconf:
	./configure

install: installbin

installbin: nxdesktop
	mkdir -p $(BINDIR)
	cp nxdesktop $(BINDIR)
	strip $(BINDIR)/nxdesktop
	chmod 755 $(BINDIR)/nxdesktop

installman: rdesktop.1
	mkdir -p $(MANDIR)/man1
	cp rdesktop.1 $(MANDIR)/man1
	chmod 755 $(MANDIR)/man1/rdesktop.1

proto:
	cproto -DMAKE_PROTO -o proto.h *.c

clean:
	rm -f *.o crypto/*.o *~ core core.* nxdesktop

distclean: clean

.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ -c $<

