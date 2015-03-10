## SOURCE
AUTOBUILDS  = langevin
BUILDS      =
MODULES     =
STDMODS     = resources
LIBS        =
LIBPATH     = $(HOME)/lib/
STDMODPATH  = .

## SECONDARY
UPFILES     = fig/*
PATHFILE    = .structure

## CUSTOM PATHS
CURRENT_DIR :=  $(dir $(abspath $(MAKEFILE_LIST)))
CUSTOMPATHS =   PATH_FIG=\"$(CURRENT_DIR)fig/\"            \
                PATH_OUTPUT=\"$(CURRENT_DIR)output/\"
PATHARGS    =   $(CUSTOMPATHS:%=-DPRE_%)


## COMMANDS
FC      = gfortran
FFLAGS  = -g -ffree-form -cpp
LDFLAGS =
RM      = rm -f


## FILES
GARBAGE = *.o *.mod *.pyc
EXEC    = *.out *.exe
OUTPUT  = .out
SOURCE  = .f
OBJECT  = .o


## UPLOAD
UPHOST  = jangu@babel.nt.ntnu.no:~/web/
UPC     = scp
UPFLAGS =


## DEFS
AUTOBUILDS  := $(AUTOBUILDS:%=%$(OUTPUT))
BUILDS      := $(BUILDS:%=%$(OUTPUT))
MODULES     := $(MODULES:%=%$(OBJECT))
STDMODS     := $(STDMODS:%=%$(OBJECT))

LIBARGS     = -L$(LIBPATH) $(LIBS:%=-l%)
STDMODARGS  = $(STDMODS:%=$(STDMODPATH)/%)

define LINK
$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(LIBARGS) $(STDMODARGS)
endef

define COMPILE
$(FC) $(FFLAGS) -c $< $(PATHARGS)
endef


## GENERAL
all: init builds

builds: $(BUILDS) $(AUTOBUILDS)

modules: $(MODULES)

stdmods: $(STDMODS)


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

init: $(PATHFILE)

remake: vclean
	$(MAKE) --always-make all

upload:
	$(UPC) $(UPFLAGS) $(UPFILES) $(UPHOST)

clean:
	$(RM) $(GARBAGE)

vclean: clean
	$(RM) $(EXEC)
	$(RM) $(PATHFILE)


## PATH FILE
$(PATHFILE):
	@touch $(PATHFILE)
	@$(foreach PATH, $(CUSTOMPATHS), echo "$(PATH)" >> $(PATHFILE);) 
	@echo >> $(PATHFILE)
	@echo "Building path file: $(PATHFILE)"

