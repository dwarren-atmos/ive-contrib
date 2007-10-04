
include ../ARCH.opt

PROG =	$(BINDIR)/archiver
PROG1 =	$(BINDIR)/compresser

SRCS	= archiver.c
SRCS1	= compresser.c

OBJS	= archiver.o
OBJS1	= compresser.o

LIBS = $(LIBDIR)/libmm5io.a $(LIBDIR)/libmm5proj.a

INCS = $(INCDIR)/mm5_io.h $(INCDIR)/map_proj.h \
       $(INCDIR)/interp.h $(LATSINCDIR)/lats.h $(NETCDFINCDIR)/netcdf.h

all: $(PROG) $(PROG1)

$(PROG): $(OBJS) $(LIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LINK)

$(PROG1): $(OBJS1) $(LIBS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS1) $(LINK)

clean:
	rm -f $(PROG) $(PROG1) $(OBJS) $(OBJS1)
