## SOURCE
AUTOBUILDS=langevin
BUILDS=
MODULES=
STDMODS=resources
LIBS=slatec

## SECONDARY
UPFILES=*.png
PATHFILE=.structure

## PATHS
CURRENT_DIR:=$(abspath $(MAKEFILE_LIST))
LIBPATH=$(HOME)/lib/
STDMODPATH=.
FIGPATH=$(CURRENT_DIR)/fig/
OUTPUTPATH=$(CURRENT_DIR)/output/


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
INIT_SH=.path_init.sh


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
all: init builds

remake:
	$(MAKE) --always-make all

builds: $(BUILDS) $(AUTOBUILDS)

modules: $(MODULES)

stdmods: $(STDMODS)

upload:
	$(UPC) $(UPFLAGS) $(UPFILES) $(UPHOST)

init: $(PATHFILE)


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

.PHONY: all remake clean vclean init builds modules stdmods upload

clean:
	$(RM) $(GARBAGE)

vclean: clean
	$(RM) $(EXEC)


## PATH FILE
$(PATHFILE):
	@echo "PATH_LIB=$(LIBPATH)" > $(PATHFILE)
	@echo "PATH_STDMOD=$(STDMODPATH)" >> $(PATHFILE)
	@echo "PATH_FIG=$(FIGPATH)" >> $(PATHFILE)
	@echo "PATH_OUTPUT=$(OUTPUTPATH)" >> $(PATHFILE)
	@echo >> $(PATHFILE)

