
include ../ARCH.opt

SRCS =	MM5_field.c MM5_header.c MM5_init.c utils.c

OBJS =	MM5_field.o MM5_header.o MM5_init.o utils.o

INCS =	$(INCDIR)/mm5_io.h $(INCDIR)/utils.h $(INCDIR)/map_proj.h 

LIBS =	libmm5io.a

all: $(LIBDIR)/$(LIBS)

$(LIBDIR)/$(LIBS): $(OBJS) $(INCS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

clean:
	rm -f $(OBJS) $(LIBDIR)/$(LIBS)

MM5_field.o: $(INCDIR)/mm5_io.h $(INCDIR)/utils.h $(INCDIR)/map_proj.h
MM5_header.o: $(INCDIR)/mm5_io.h
MM5_init.o: $(INCDIR)/mm5_io.h $(INCDIR)/utils.h
utils.o: $(INCDIR)/utils.h
