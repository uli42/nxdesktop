#
# rdesktop: A Remote Desktop Protocol client
# Makefile
# Copyright (C) Matthew Chapman 1999-2004
#

#**************************************************************************
#*                                                                        *
#* Copyright (c) 2001,2005 NoMachine, http://www.nomachine.com.           *
#*                                                                        *
#* NXDESKTOP, NX protocol compression and NX extensions to this software  *
#* are copyright of NoMachine. Redistribution and use of the present      *
#* software is allowed according to terms specified in the file LICENSE   *
#* which comes in the source distribution.                                *
#*                                                                        *
#* Check http://www.nomachine.com/licensing.html for applicability.       *
#*                                                                        *
#* NX and NoMachine are trademarks of Medialogic S.p.A.                   *
#*                                                                        *
#* All rights reserved.                                                   *
#*                                                                        *
#**************************************************************************


# Configuration defaults

prefix      = /usr/NX
exec_prefix = $(prefix)
bindir      = $(exec_prefix)/bin
mandir      = $(prefix)/man
datadir     = $(prefix)/share

INCLUDES = -I../nxcomp -I../nxcompext
LDLIBS   = -lXpm -L../nxcompext -lXcompext -L../nxcomp -lXcomp -lX11 -lXext
VERSION     = 1.5.0-2
KEYMAP_PATH = $(datadir)/keymaps-windows/

RDPOBJ   = tcp.o iso.o mcs.o secure.o licence.o rdp.o orders.o bitmap.o cache.o rdp5.o channels.o rdpdr.o serial.o printer.o disk.o parallel.o printercache.o mppc.o pstcache.o
X11OBJ   = rdesktop.o xwin.o xkeymap.o ewmhints.o xclip.o cliprdr.o
VNCOBJ   = vnc/rdp2vnc.o vnc/vnc.o vnc/xkeymap.o vnc/x11stubs.o

include Makeconf  # configure-generated

all: $(TARGETS)

nxdesktop: $(X11OBJ) $(SOUNDOBJ) $(RDPOBJ)
	$(CC) $(CFLAGS) -o nxdesktop $(X11OBJ) $(SOUNDOBJ) $(RDPOBJ) $(CRYPTOBJ) $(LDFLAGS) $(LDLIBS) 

rdp2vnc: $(VNCOBJ) $(SOUNDOBJ) $(RDPOBJ)
	$(CCLD) $(CFLAGS) -o rdp2vnc $(VNCOBJ) $(SOUNDOBJ) $(RDPOBJ) $(CRYPTOBJ) $(LDFLAGS) $(LDVNC)

vnc/rdp2vnc.o: rdesktop.c
	$(CC) $(CFLAGS) $(VNCINC) -DRDP2VNC -o vnc/rdp2vnc.o -c rdesktop.c

vnc/vnc.o: vnc/vnc.c
	$(CC) $(CFLAGS) $(VNCINC) -DRDP2VNC -o vnc/vnc.o -c vnc/vnc.c

vnc/xkeymap.o: xkeymap.c
	$(CC) $(CFLAGS) $(VNCINC) -DRDP2VNC -o vnc/xkeymap.o -c xkeymap.c

vnc/x11stubs.o: vnc/x11stubs.c
	$(CC) $(CFLAGS) $(VNCINC) -o vnc/x11stubs.o -c vnc/x11stubs.c

Makeconf:
	./configure

install: installbin installkeymaps installman

installbin: nxdesktop
	mkdir -p $(DESTDIR)/$(bindir)
	$(INSTALL) nxdesktop $(DESTDIR)/$(bindir)
	strip $(DESTDIR)/$(bindir)/nxdesktop
	chmod 755 $(DESTDIR)/$(bindir)/nxdesktop

installman: doc/nxdesktop.1
	mkdir -p $(DESTDIR)/$(mandir)/man1
	cp doc/nxdesktop.1 $(DESTDIR)/$(mandir)/man1
	chmod 644 $(DESTDIR)/$(mandir)/man1/nxdesktop.1

installkeymaps:
	mkdir -p $(DESTDIR)/$(KEYMAP_PATH)
# Prevent copying the CVS directory
	cp keymaps/?? keymaps/??-?? $(DESTDIR)/$(KEYMAP_PATH)
	cp keymaps/common $(DESTDIR)/$(KEYMAP_PATH)
	cp keymaps/modifiers $(DESTDIR)/$(KEYMAP_PATH)
	chmod 644 $(DESTDIR)/$(KEYMAP_PATH)/*

proto:
	cproto -DMAKE_PROTO -DWITH_OPENSSL -o proto.h *.c

clean:
	rm -f *.o crypto/*.o *~ vnc/*.o vnc/*~ nxdesktop rdp2vnc

distclean: clean

dist:
	mkdir -p /tmp/nxdesktop-make-dist-dir
	ln -sf `pwd` /tmp/nxdesktop-make-dist-dir/nxdesktop-$(VERSION)
	(cd /tmp/nxdesktop-make-dist-dir; \
	tar zcvf /tmp/nxdesktop-$(VERSION).tar.gz \
	nxdesktop-$(VERSION)/COPYING \
	nxdesktop-$(VERSION)/README \
	nxdesktop-$(VERSION)/configure \
	nxdesktop-$(VERSION)/Makefile \
	nxdesktop-$(VERSION)/rdesktop.spec \
	nxdesktop-$(VERSION)/*.c \
	nxdesktop-$(VERSION)/*.h \
	nxdesktop-$(VERSION)/crypto/README \
	nxdesktop-$(VERSION)/crypto/*.c \
	nxdesktop-$(VERSION)/crypto/*.h \
	nxdesktop-$(VERSION)/keymaps/?? \
	nxdesktop-$(VERSION)/keymaps/??-?? \
	nxdesktop-$(VERSION)/keymaps/common \
	nxdesktop-$(VERSION)/keymaps/modifiers \
	nxdesktop-$(VERSION)/keymaps/convert-map \
	nxdesktop-$(VERSION)/doc/HACKING \
	nxdesktop-$(VERSION)/doc/AUTHORS \
	nxdesktop-$(VERSION)/doc/TODO \
	nxdesktop-$(VERSION)/doc/ChangeLog \
	nxdesktop-$(VERSION)/doc/keymapping.txt \
	nxdesktop-$(VERSION)/doc/keymap-names.txt \
	nxdesktop-$(VERSION)/doc/ipv6.txt \
	nxdesktop-$(VERSION)/doc/nxdesktop.1 )
	rm -rf /tmp/nxdesktop-make-dist-dir

.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ -c $<

