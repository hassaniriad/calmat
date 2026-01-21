##############################################################################
#                        Makefile for the pk2 library
#-----------------------------------------------------------------------------

define colorRed
      @tput setaf 1
      @echo $1
      @tput sgr0
endef
define colorBlue
      @tput setaf 4
      @echo $1
      @tput sgr0
endef

##############################################################################
# Help:
#-----------------------------------------------------------------------------

#h |-------------------------------------------------------------------------|
#h | Usage:                                                                  |
#h |    make comp=<your compiler>  [opt="<your options>"]                    |
#h |                               [kind="<your choices>"]                   |
#h |                               [clean] [cleanall]                        |
#h |                                                                         |
#h | where                                                                   |
#h |   o <your compiler>: is the name of your compiler (e.g. ifort)          |
#h |                      This version was tested only with:                 |
#h |                      . ifort (mac): version 2021.7.0                    |
#h |                      . ifx (linux): version 2024.1.2                    |
#h |                      . gfortran (mac): 14.1.0 (Homebrew GCC 14.1.0)     |
#h |                      . gfortran (linux): (GCC) 11.2.1 (Red Hat 11.2.1-9)|
#h |                      . nagfor (mac): Release 7.2 Build 7213             |
#h |   o <your options> : is a string corresponding to a set of compilation  |
#h |                      options (default is opt="-O3").                    |
#h |   o <your choices> : is a string corresponding to you choices of kind   |
#h |                      parameters.     |
#h |                      It may contain -DI32 or -DI64 (int32 or int64 for  |
#h |                      integers) and -DSP or -DDP (real32 or real64 for   |
#h |                      reals and complexes).                              |
#h |                      The  default is  kind="-DI32 -DDP".                |
#h |                                                                         |
#h | Examples:                                                               |
#h |   . make                                                                |
#h |      prints this help.                                                  |
#h |   . make comp=ifort clean                                               |
#h |      deletes the object files *.o (from the directory obj/ifort)        |
#h |   . make comp=ifort cleanall                                            |
#h |      deletes the object files *.o (from the directory obj/ifort),       |
#h |              the module files *.mod (from the directory mod/ifort),     |
#h |              the library file libpk2*.a (from the directory lib/ifort). |
#h |   . make comp=gfortran                                                  |
#h |      compiles the library with -O3 optimization flag and default kinds  |
#h |   . make comp=gfortran kind="-DSP"                                      |
#h |      compiles with real32 reals/complexes (and 32 bits integers)        |
#h |   . make comp=ifort opt="-O3 -fp-model fast=2 -parallel"                |
#h |       compiles the library with the specified options.                  |
#h |   . make comp=nagfor opt=check                                          |
#h |       compiles the library with checking options (predefined            |
#h |       according to one of the three compilers cited above).             |
#h |-------------------------------------------------------------------------|


ifeq ($(comp),)
   help:     #// Show this help.
	@sed -ne '/@sed/!s/#h //p' $(MAKEFILE_LIST)
endif


##############################################################################
#  Settings:
#-----------------------------------------------------------------------------

arch      = /usr/bin/ar
archflag  = cr
ranlib    = /usr/bin/ranlib

#
#  Directories where the library, the objects and the module files will be placed:
#
pk2dir = ..
dirlib = $(pk2dir)/lib/$(comp)
dirmod = $(pk2dir)/mod/$(comp)
dirobj = $(pk2dir)/obj/$(comp)
dirbin = $(pk2dir)/bin/$(comp)

#
#  Create these directories if not present:
#
$(shell mkdir -p $(dirlib))
$(shell mkdir -p $(dirmod))
$(shell mkdir -p $(dirobj))
$(shell mkdir -p $(dirbin))

$(shell mkdir -p $(dirobj)/user)

abspk2dir := $(abspath $(pk2dir))
absdirlib := $(abspath $(dirlib))
absdirmod := $(abspath $(dirmod))
absdirobj := $(abspath $(dirobj))
absdirbin := $(abspath $(dirbin))

liblapack = -llapack -lblas

##############################################################################
#  Compilation options:
#-----------------------------------------------------------------------------

INTEL := ifort ifx
GNU := gfortran gfortran-14 gfortran-11
NAG := nagfor
FLANG := flang flang-new

ifeq ($(kind),)
#
#  kind option not used: the default defined in module kindParameters will be used
#   
   OPTS =  
else
   OPTS =$(kind)
endif     	
              
ifneq ($(filter $(comp),$(GNU)),)
   ifeq ($(opt),)
      OPTS += -O3
   else ifeq ($(opt),check)
      # removed flag: -Wall (causes an ICE with gcc 14.2.0)
      OPTS += -Og -fcheck=all -fbacktrace -fimplicit-none   
   else       
      OPTS += $(opt)
   endif
   flags = $(OPTS) -cpp -J $(dirmod)
   # added flag: -ffree-line-length-none (problem with cpp directives)   
   flags += -fmax-errors=5 -ffree-line-length-none
   flagdynlib = -fPIC -shared
   
else ifneq ($(filter $(comp),$(INTEL)),)
   ifeq ($(opt),)
      OPTS += -O3
   else ifeq ($(opt),check)
      OPTS += -check all -traceback -gen-interfaces -warn interfaces
      # due to a compiler bug I disable temporarily bounds checking:
      OPTS += -check nobounds                 
   else       
      OPTS += $(opt)
   endif   
   flags = $(OPTS) -fpp -module $(dirmod) -I $(dirmodpk2) 
   flagdynlib = -fPIC -shared  -Wl,-ld_classic   
   
else ifneq ($(filter $(comp),$(NAG)),)
   ifeq ($(opt),)
      OPTS += -O3
   else ifeq ($(opt),check)
      OPTS += -O0 -C=all
   else       
      OPTS += $(opt)
   endif   
   flags = $(OPTS) -fpp -kind=byte -ieee=full -mdir $(dirmod) -I $(dirmod)
   flagdynlib = -PIC -Wl,-shared

else ifneq ($(filter $(comp),$(FLANG)),)
   ifeq ($(opt),)
      OPTS += -O3
   else ifeq ($(opt),check)
      OPTS += -Og -fimplicit-none   
   else       
      OPTS += $(opt)
   endif   
   flags = $(OPTS) -cpp -module-dir $(dirmod) -I $(dirmod)
   flagdynlib = -PIC -Wl,-shared
   
else
   $(error unknown compiler ($(comp)), please adapt the pk2/app/calmat/src/makefile)

endif

flags += -D__CDATE=\""$(shell date)"\" -D__DIR=\""$(abspk2dir)\"" -D__COMP=\""$(comp)\""

################################################################################
# Compilation:
#-------------------------------------------------------------------------------

src = LapackInterface.f90 kindParameters.f90 ansiColor.f90 str.f90 \
      pk2Constants.f90 err.f90 util_Strings.f90 util_Other.f90     \
      util.f90 signalHandler.f90 dispmodule.f90 disp_i8mod.f90     \
      bk2.f90 pk2.f90 pk2f.f90 pk2obj.f90 pk2Interpreter.f90       \
      pk2mod.f90

srcu = userfunc.f90 

obj  = $(patsubst %.f90,$(dirobj)/%.o,$(src))
obju = $(patsubst %.f90,$(dirobj)/user/%.o,$(srcu))

src_utildeps = util_sort.inc util_reshape.inc util_allock1.inc util_allock2.inc \
               util_updatek1.inc util_poldec.inc

utildeps = $(patsubst %,include/%,$(src_utildeps))

src_bk2deps = bk2_add.inc             bk2_ediv.inc          bk2_emult.inc   \
              bk2_mult_by_matmul.inc  bk2_mult_by_blas.inc  bk2_epow.inc    \
              bk2_copysubmat.inc      bk2_insertmat.inc     bk2_setmat.inc  \
              bk2_setij.inc           bk2_delrows.inc       bk2_resizev.inc \
              bk2_gt.inc              bk2_ge.inc            bk2_eq.inc      \
              bk2_ne.inc              bk2_mult_prepare.inc

bk2deps = $(patsubst %,include/%,$(src_bk2deps))

src_pk2deps = pk2_extracsubmat.inc pk2_fsettotype.inc pk2_setsubmat.inc pk2_insertmat.inc

pk2deps = $(patsubst %,include/%,$(src_pk2deps))

libpk2 = libpk2.a
libpk2dyn = libpk2_d.so

libpk2: $(obj) $(obju)
# static version:
	$(arch) $(archflag) $(dirlib)/$(libpk2) $(obj) $(obju)
	$(ranlib) $(dirlib)/$(libpk2)
# shared version:
#	$(comp) $(flags) $(flagdynlib) $(obj) $(obju) $(liblapack) -o $(dirlib)/$(libpk2dyn)
# utility:
	$(comp) $(flags) util/addFun.f90 -I $(dirmod) -L$(dirlib) -lpk2 $(liblapack) -o $(dirbin)/addFun
	
	@echo ""
	$(call colorBlue, "*****************************************")
	$(call colorBlue, "***   Successful compilation of pk2   ***")
	$(call colorBlue, "*****************************************")
	@echo ""
	$(call colorBlue, ". The libraries are in the directory: $(absdirlib)")
	$(call colorBlue, ". The module files are in: $(absdirmod)")
	$(call colorBlue, ". The object files are in: $(absdirobj)")   
   
$(dirobj)/kindParameters.o: kindParameters.f90 
	@echo " "
	$(call colorRed, "> compiling kindParameters.f90") 
	@echo " "
	$(comp) $(flags) -c kindParameters.f90 -o $(dirobj)/kindParameters.o

$(dirobj)/ansiColor.o: ansiColor.f90 
	@echo " "
	$(call colorRed, "> compiling ansicolor.f90")
	@echo " "
	$(comp) $(flags) -c ansiColor.f90 -o $(dirobj)/ansiColor.o

$(dirobj)/str.o: str.f90 \
                 $(dirobj)/ansiColor.o
	@echo " "
	$(call colorRed, "> compiling str.f90")
	@echo " "
	$(comp) $(flags) -c str.f90 -o $(dirobj)/str.o

$(dirobj)/pk2Constants.o: pk2Constants.f90 \
                        $(dirobj)/kindParameters.o
	@echo " "
	$(call colorRed, "> compiling pk2Constants.f90")
	@echo " "
	$(comp) $(flags) -c pk2Constants.f90 -o $(dirobj)/pk2Constants.o

$(dirobj)/err.o: err.f90 \
                        $(dirobj)/kindParameters.o \
                        $(dirobj)/ansiColor.o
	@echo " "
	$(call colorRed, "> compiling err.f90")
	@echo " "
	$(comp) $(flags) -c err.f90 -o $(dirobj)/err.o

$(dirobj)/LapackInterface.o: LapackInterface.f90 
	@echo " "
	$(call colorRed, "> compiling LapackInterface.f90")
	@echo " "
	$(comp) $(flags) -c LapackInterface.f90 -o $(dirobj)/LapackInterface.o

$(dirobj)/util_Strings.o: util_Strings.f90           \
                          $(dirobj)/kindParameters.o \
                          $(dirobj)/str.o            \
                          $(dirobj)/pk2Constants.o   \
                          $(dirobj)/err.o
	@echo " "
	$(call colorRed, "> compiling util_Strings.f90")
	@echo " "
	$(comp) $(flags) -c util_Strings.f90 -o $(dirobj)/util_Strings.o

$(dirobj)/util_Other.o: util_Other.f90  $(utildeps)  \
                        $(dirobj)/kindParameters.o   \
                        $(dirobj)/str.o              \
                        $(dirobj)/pk2Constants.o     \
                        $(dirobj)/err.o              \
                        $(dirobj)/util_Strings.o
	@echo " "
	$(call colorRed, "> compiling util_Other.f90")
	@echo " "
	$(comp) $(flags) -c util_Other.f90 -o $(dirobj)/util_Other.o

$(dirobj)/util.o: util.f90                   \
                  $(dirobj)/kindParameters.o \
                  $(dirobj)/str.o            \
                  $(dirobj)/pk2Constants.o   \
                  $(dirobj)/err.o            \
                  $(dirobj)/util_Strings.o   \
                  $(dirobj)/util_Other.o
	@echo " "
	$(call colorRed, "> compiling util.f90")
	@echo " "
	$(comp) $(flags) -c util.f90 -o $(dirobj)/util.o

$(dirobj)/signalHandler.o: signalHandler.f90          \
                           $(dirobj)/kindParameters.o \
                           $(dirobj)/pk2Constants.o   \
                           $(dirobj)/util_Strings.o   
	@echo " "
	$(call colorRed, "> compiling signalHandler.f90")
	@echo " "
	$(comp) $(flags) -c signalHandler.f90 -o $(dirobj)/signalHandler.o

$(dirobj)/dispmodule.o: third_party/dispmodule.f90
	@echo " "
	$(call colorRed, "> compiling dispmodule.f90")
	@echo " "
	$(comp) $(flags) -c third_party/dispmodule.f90 -o $(dirobj)/dispmodule.o

$(dirobj)/disp_i8mod.o: third_party/disp_i8mod.f90 \
                        $(dirobj)/dispmodule.o
	@echo " "
	$(call colorRed, "> compiling disp_i8mod.f90")
	@echo " "
	$(comp) $(flags) -c third_party/disp_i8mod.f90 -o $(dirobj)/disp_i8mod.o

$(dirobj)/bk2.o: bk2.f90  $(bk2deps)         \
                 $(dirobj)/kindParameters.o  \
                 $(dirobj)/str.o             \
                 $(dirobj)/pk2Constants.o    \
                 $(dirobj)/err.o             \
                 $(dirobj)/LapackInterface.o \
                 $(dirobj)/util_Strings.o    \
                 $(dirobj)/util_Other.o      \
                 $(dirobj)/util.o
	@echo " "
	$(call colorRed, "> compiling bk2.f90")
	@echo " "
	$(comp) $(flags) -c bk2.f90 -o $(dirobj)/bk2.o

$(dirobj)/pk2.o: pk2.f90  $(pk2deps)         \
                 $(dirobj)/kindParameters.o  \
                 $(dirobj)/str.o             \
                 $(dirobj)/pk2Constants.o    \
                 $(dirobj)/err.o             \
                 $(dirobj)/LapackInterface.o \
                 $(dirobj)/util_Strings.o    \
                 $(dirobj)/util_Other.o      \
                 $(dirobj)/util.o            \
                 $(dirobj)/bk2.o
	@echo " "
	$(call colorRed, "> compiling pk2.f90")
	@echo " "
	$(comp) $(flags) -c pk2.f90 -o $(dirobj)/pk2.o

$(dirobj)/pk2f.o: pk2f.f90                    \
                  $(dirobj)/kindParameters.o  \
                  $(dirobj)/str.o             \
                  $(dirobj)/pk2Constants.o    \
                  $(dirobj)/err.o             \
                  $(dirobj)/LapackInterface.o \
                  $(dirobj)/util_Strings.o    \
                  $(dirobj)/util_Other.o      \
                  $(dirobj)/util.o            \
                  $(dirobj)/bk2.o             \
                  $(dirobj)/pk2.o 
	@echo " "
	$(call colorRed, "> compiling pk2f.f90")
	@echo " "
	$(comp) $(flags) -c pk2f.f90 -o $(dirobj)/pk2f.o

$(dirobj)/pk2obj.o: pk2obj.f90               \
                 $(dirobj)/kindParameters.o  \
                 $(dirobj)/str.o             \
                 $(dirobj)/pk2Constants.o    \
                 $(dirobj)/err.o             \
                 $(dirobj)/LapackInterface.o \
                 $(dirobj)/util_Strings.o    \
                 $(dirobj)/util_Other.o      \
                 $(dirobj)/util.o            \
                 $(dirobj)/bk2.o             \
                 $(dirobj)/pk2.o
	@echo " "
	$(call colorRed, "> compiling pk2obj.f90")
	@echo " "
	$(comp) $(flags) -c pk2obj.f90 -o $(dirobj)/pk2obj.o

$(dirobj)/user/userfunc.o: userfunc.f90     \
                           $(dirobj)/pk2.o  
	@echo " "
	$(call colorRed, "> compiling userfunc.f90")
	@echo " "
	$(comp) $(flags) -c userfunc.f90 -o $(dirobj)/user/userfunc.o

$(dirobj)/pk2Interpreter.o: pk2Interpreter.f90          \
                            $(dirobj)/kindParameters.o  \
                            $(dirobj)/str.o             \
                            $(dirobj)/pk2Constants.o    \
                            $(dirobj)/err.o             \
                            $(dirobj)/LapackInterface.o \
                            $(dirobj)/util_Strings.o    \
                            $(dirobj)/util_Other.o      \
                            $(dirobj)/util.o            \
                            $(dirobj)/bk2.o             \
                            $(dirobj)/pk2.o             \
                            $(dirobj)/pk2f.o            \
                            $(dirobj)/user/userfunc.o
	@echo " "
	$(call colorRed, "> compiling pk2interpreter.f90")
	@echo " "
	$(comp) $(flags) -c pk2Interpreter.f90 -o $(dirobj)/pk2Interpreter.o

$(dirobj)/pk2mod.o: pk2mod.f90                  \
                    $(dirobj)/kindParameters.o  \
                    $(dirobj)/str.o             \
                    $(dirobj)/ansiColor.o       \
                    $(dirobj)/pk2Constants.o    \
                    $(dirobj)/err.o             \
                    $(dirobj)/LapackInterface.o \
                    $(dirobj)/util_Strings.o    \
                    $(dirobj)/util_Other.o      \
                    $(dirobj)/util.o            \
                    $(dirobj)/signalHandler.o   \
                    $(dirobj)/bk2.o             \
                    $(dirobj)/pk2.o             \
                    $(dirobj)/pk2f.o            \
                    $(dirobj)/pk2obj.o          \
                    $(dirobj)/pk2Interpreter.o
	@echo " "
	$(call colorRed, "> compiling pk2mod.f90")
	@echo " "
	$(comp) $(flags) -c pk2mod.f90 -o $(dirobj)/pk2mod.o                    

##############################################################################
# Cleaning:
#-----------------------------------------------------------------------------

clean:
	@echo " "
	$(call colorRed, "> Cleanning pk2:")
	@echo " "
	/bin/rm -f $(dirobj)/*.o 

cleanall:
	@echo " "
	$(call colorRed, "> Cleanning pk2:")
	@echo " "
	/bin/rm -f $(dirobj)/*.o $(dirmod)/*.mod $(dirlib)/*.a $(dirlib)/*.so
