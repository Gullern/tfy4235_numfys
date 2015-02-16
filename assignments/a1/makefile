## SOURCE
AUTOBUILDS=langevin
BUILDS=
MODULES=
STDMODS=resources
LIBS=slatec
UPFILES=*.png

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


## UPLOAD
UPHOST=jangu@babel.nt.ntnu.no:~/web/
UPC=scp
UPFLAGS=


## DEFS
AUTOBUILDS:=$(AUTOBUILDS:%=%$(OUTPUT))
BUILDS:=$(BUILDS:%=%$(OUTPUT))
MODULES:=$(MODULES:%=%$(OBJECT))
STDMODS:=$(STDMODS:%=%$(OBJECT))

LIBARGS=-L$(LIBPATH) $(LIBS:%=-l%)
STDMODARGS=$(STDMODS:%=$(STDMODPATH)/%)

define LINK
$(FC) $(FFLAGS) $^ $(LDFLAGS) -o $@ $(LIBARGS) $(STDMODARGS)
endef

define COMPILE
$(FC) $(FFLAGS) -c $<
endef


## GENERAL
all: builds

remake:
	$(MAKE) --always-make all

builds: $(BUILDS) $(AUTOBUILDS)

modules: $(MODULES)

stdmods: $(STDMODS)

upload:
	$(UPC) $(UPFLAGS) $(UPFILES) $(UPHOST)

## DEPENDENCIES



## PATTERNS
$(AUTOBUILDS): %$(OUTPUT): %$(OBJECT)
	$(LINK)

$(MODULES): %$(OBJECT): %$(SOURCE) $(STDMODS)
	$(COMPILE)

$(STDMODS): %$(OBJECT): %$(SOURCE)
	$(COMPILE)

%$(OBJECT): %$(SOURCE) $(STDMODS)
	$(COMPILE)


## UTILS
.INTERMEDIATE: $(AUTOBUILDS:%$(OUTPUT)=%$(OBJECT)) $(BUILDS:%$(OUTPUT)=%$(OBJECT))

.PHONY: all remake clean vclean builds modules stdmods upload

clean:
	$(RM) $(GARBAGE)

vclean: clean
	$(RM) $(EXEC)

