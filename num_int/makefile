## Parameters
AUTOBUILDS=
BUILDS=tester
MODULES=num_int_1d seed_init
LIBS=slatec
LIBPATH=$(HOME)/lib/


## Preferences
FC=gfortran
FFLAGS=-g -ffree-form
LDFLAGS=
RM=rm -f
GARBAGE=*.o *.mod
EXEC=*.out *.exe


## DEF
LIBARGS=-L$(LIBPATH) $(LIBS:%=-l%)

define LINK
$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@.out $(LIBARGS)
endef

define COMPILE
$(FC) $(FFLAGS) -c $<
endef


## General
all: builds

builds: $(BUILDS) $(AUTOBUILDS)

modules: $(MODULES)


## Dependencies
tester: %: num_int_1d.o %.o
	$(LINK)


## STNRD
$(AUTOBUILDS): %: %.o
	$(LINK)

%.o: %.f
	$(COMPILE)


## UTILS
.INTERMEDIATE: $(BUILDS:%=%.o) $(AUTOBUILDS:%=%.o) $(MODULES:%=%.o)

.PHONY: all clean vclean builds modules

clean:
	$(RM) $(GARBAGE)

vclean: clean
	$(RM) $(EXEC)
