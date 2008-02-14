SFC    = sfc -v
CC     = gcc -O3 -Wall
prefix = $HOME/Local/bin

SUBDIRS = scheme bootstrap
VERSION = qa0-1.0.0

.PHONY: all dist clean realclean install tar

all:
	$(MAKE) -C bootstrap $@

clean realclean:
	for d in $(SUBDIRS); do \
	  $(MAKE) -C $$d $@ || exit 1; \
	done

dist:
	$(MAKE) -C scheme all

install: all
	[ -d $(prefix) ] || mkdir -p $(prefix)
	cp bootstrap/qa0 $(prefix)/

tar: realclean
	x = `pwd`; cd .. tar -cvf - `basename $$x` | bzip2 -9 > $$x.tar.bz2
