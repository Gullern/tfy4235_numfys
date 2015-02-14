## SOURCE
AUTOBUILDS=langevin
BUILDS=
MODULES=
STDMODS=resources
LIBS=slatec


## PATHS
LIBPATH=$(HOME)/lib/
STDMODPATH=.


## COMMANDS
FC=gfortran
FFLAGS=-g -ffree-form
LDFLAGS=
RM=rm -f


## FILES
GARBAGE=*.o *.mod
EXEC=*.out *.exe
OUTPUT=.out
SOURCE=.f
OBJECT=.o


## DEFS
LIBARGS=-L$(LIBPATH) $(LIBS:%=-l%)

define LINK
$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@$(OUTPUT) $(LIBARGS) $(STDMODPATH)/$(STDMODS)$(OBJECT)
endef

define COMPILE
$(FC) $(FFLAGS) -c $<
endef


## GENERAL
all: builds

builds: $(BUILDS) $(AUTOBUILDS)

modules: $(MODULES)

stdmods: $(STDMODS)


## DEPENDENCIES



## STNRD
$(AUTOBUILDS): %: %$(OBJECT)
	$(LINK)

$(MODULES): %: %$(SOURCE) $(STDMODS)
	$(COMPILE)

$(STDMODS): %: %$(SOURCE) $(STDMODS)
	$(COMPILE)

%$(OBJECT): %$(SOURCE) $(STDMODS)
	$(COMPILE)


## UTILS
.INTERMEDIATE: $(BUILDS:%=%$(OBJECT)) $(AUTOBUILDS:%=%$(OBJECT))

.PHONY: all clean vclean builds modules stdmods

clean:
	$(RM) $(GARBAGE)

vclean: clean
	$(RM) $(EXEC)
