
include ../ARCH.opt

SRCS =	interp.c map_proj.c

OBJS =	interp.o map_proj.o

INCS =	$(INCDIR)/map_proj.h $(INCDIR)/interp.h

LIBS =	libmm5proj.a

all: $(LIBDIR)/$(LIBS)

$(LIBDIR)/$(LIBS): $(OBJS) $(INCS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

clean:
	rm -f $(TEST) $(OBJS) $(LIBDIR)/$(LIBS)

map_proj.o: $(INCDIR)/map_proj.h
interp.o: $(INCDIR)/map_proj.h $(INCDIR)/interp.h
