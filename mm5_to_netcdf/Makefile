
include ARCH.opt

MM5IOLIB=$(TOPDIR)/mm5iolib
PROJLIB=$(TOPDIR)/projlib
SRCDIR=$(TOPDIR)/src

all:: credits
	@cd $(PROJLIB); make;
	@cd $(MM5IOLIB); make;
	@cd $(SRCDIR); make;

clean:
	@cd $(PROJLIB); make clean;
	@cd $(MM5IOLIB); make clean;
	@cd $(SRCDIR); make clean;

install: all
	cp -f bin/archiver $(prefix)/bin

credits:
	@cat README
