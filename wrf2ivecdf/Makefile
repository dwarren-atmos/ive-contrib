include ./config.user

BIN = .
LIB = .
MODLIB = .

wrf_nogood = wrf_doright.o
MODS = module_netcdf.o

targets = wrf_doright

all : ${targets} 

.f.o:
	$(F90) $(FFLAGS) -I/usr/include -I. -module $(MODLIB) -c $<

wrf_doright : ${wrf_nogood}
	$(F90) $(FFLAGS) ${MODS} $< -o $(BIN)/$@.exe $(NETCDFCOMP) $(EXTRALIBS)

${wrf_nogood} : ${MODS}

clean :
	$(RM_CMD)
