##Parameters
AUTOBUILDS=
BUILDS=tester
MODULES=num_int_1d seed_init
LIBS=


##Preferences
FC=gfortran
FFLAGS=-g -ffree-form
LDFLAGS=
RM=rm -f
GARBAGE=*.o *.mod
EXEC=*.out *.exe

define LINK
$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@.out
endef

define COMPILE
$(FC) $(FFLAGS) -c $<
endef


## General
all: $(BUILDS) $(AUTOBUILDS)


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

.PHONY: all clean vclean

clean:
	$(RM) $(GARBAGE)

vclean: clean
	$(RM) $(EXEC)
