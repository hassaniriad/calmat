################################################################################
#                        Makefile for calmat 
#-------------------------------------------------------------------------------

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

################################################################################
# Help:
#-------------------------------------------------------------------------------

#h |-------------------------------------------------------------------------|
#h | Usage: make comp = <your compiler>  [opt = "<your options>"] [clean]    |
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
#h |                                                                         |
#h | Examples:                                                               |
#h |   . make                                                                |
#h |        prints this help.                                                |
#h |   . make comp=ifort clean                                               |
#h |        deletes the object files (from the directory obj/ifort),         |
#h |                the module files (from the directory mod/ifort),         |
#h |   . make comp=gfortran                                                  |
#h |        compiles with -O3 optimization flag.                             |
#h |   . make comp=ifort opt="-O3 -fp-model fast=2 -parallel"                |
#h |         compiles with the specified options.                            |
#h |   . make comp=nagfor opt=check                                          |
#h |         compiles with checking options (predefined according to one of  |
#h |         the three compilers cited above).                               |
#h |-------------------------------------------------------------------------|


ifeq ($(comp),)
   help:     #// Show this help.
	@sed -ne '/@sed/!s/#h //p' $(MAKEFILE_LIST)
endif


################################################################################
#  Settings:
#-------------------------------------------------------------------------------

arch      = /usr/bin/ar
archflag  = cr
ranlib    = /usr/bin/ranlib

#
# Name of the executables and library:
#
execs = calmat_s.exe
execd = calmat_d.exe
libr = libcalmat.a

#
#  Directory of the pk2 library (libpk2.a) and directory of the corresponding
#  module files:
#
pk2dir = ../pk2
dirlibpk2 = $(pk2dir)/lib/$(comp)
dirmodpk2 = $(pk2dir)/mod/$(comp)
dirobjpk2 = $(pk2dir)/obj/$(comp)
dirbinpk2 = $(pk2dir)/bin/$(comp)

absdirlibpk2 := $(abspath $(dirlibpk2))
absdirbinpk2 := $(abspath $(dirbinpk2))
absdirmodpk2 := $(abspath $(dirmodpk2))

libs = -L$(dirlibpk2) -lpk2   -llapack -lblas #caution: the order may matter 
libd = -L$(dirlibpk2) -lpk2_d -llapack -lblas #caution: the order may matter 

#
#  Directories where the executable, the object files and the module files
#  will be placed:
#
direxe = ../bin/$(comp)
dirmod = ../mod/$(comp)
dirobj = ../obj/$(comp)
dirlib = ../lib/$(comp)

absdirlib := $(abspath $(dirlib))
absdirmod := $(abspath $(dirmod))
absdirobj := $(abspath $(dirobj))
absdirexe := $(abspath $(direxe))


#
#  Create these directories if not present:
#
$(shell mkdir -p $(direxe))
$(shell mkdir -p $(dirmod))
$(shell mkdir -p $(dirobj))
$(shell mkdir -p $(dirlib))


################################################################################
#  Compilation options:
#-------------------------------------------------------------------------------

INTEL := ifort ifx
GNU := gfortran gfortran-14 gfortran-11
NAG := nagfor
              
ifneq ($(filter $(comp),$(GNU)),)
   ifeq ($(opt),)
      OPTS = -O3
   else ifeq ($(opt),check)
      OPTS = -Og -fcheck=all -fbacktrace -fimplicit-none   
   else       
      OPTS = $(opt)
   endif
   cpflags = $(OPTS)    
   flagcomp = $(OPTS)
   flags = $(OPTS) -cpp -J $(dirmod) -I $(dirmodpk2)
   # added flag: -ffree-line-length-none (problem with cpp directives)   
   flags += -fmax-errors=5 -ffree-line-length-none

else ifneq ($(filter $(comp),$(INTEL)),)
   ifeq ($(opt),)
      OPTS = -O3 
   else ifeq ($(opt),check)
      OPTS = -O0 -check all -traceback -gen-interfaces -warn interfaces
      # due to a compiler bug I disable temporarily bounds checking:
      OPTS += -check nobounds                 
   else       
      OPTS = $(opt)
   endif   
   cpflags = $(OPTS)       
   flagcomp = $(OPTS)
   flags = $(OPTS) -fpp -module $(dirmod) -I $(dirmodpk2) 
   # added flag: -heap-arrays. Alternative: let the stack grow unlimited: ulimit -s unlimited
   flags += -heap-arrays 200 
   cpflags += -heap-arrays 200   
   
else ifneq ($(filter $(comp),$(NAG)),)
   ifeq ($(opt),)
      OPTS = -O3
   else ifeq ($(opt),check)
      OPTS = -O0 -C=all
   else       
      OPTS =$(opt)
   endif   
   cpflags = $(OPTS) -kind=byte -ieee=full
   flagcomp = $(OPTS) 
   flags = $(OPTS) -fpp -kind=byte -ieee=full -mdir $(dirmod) -I $(dirmod) -I $(dirmodpk2)
   flagdynlib = -PIC -Wl,-shared

else
   $(error unknown compiler ($(comp)), please adapt the pk2/app/calmat/src/makefile)

endif

flags += -D__CDATE=\""$(shell date)"\" -D__COPTS=\""$(cpflags)\""

################################################################################
# Compilation:
#-------------------------------------------------------------------------------

src0 = CalmatConstants.f90 CalmatInstruction.f90 CalmatVar.f90 CalmatObject.f90 CalmatGlobal.f90 CalmatUtil.f90 CalmatCmd.f90 f90getopt.f90 Calmat.f90 CalmatVersion.f90 CalmatSetNGet.f90

src = $(src0) main.f90

srcdeps_setnget = calmatsetnget_cp_x_to_var.inc calmatsetnget_mv_x_to_var.inc \
                  calmatsetnget_cp_var_to_x.inc calmatsetnget_mv_var_to_x.inc \
                  calmatsetnget_ptr_var_to_x.inc 
deps_setnget = $(patsubst %,include/%,$(srcdeps_setnget))

obj0 = $(patsubst %.f90,$(dirobj)/%.o,$(src0))
obj  = $(patsubst %.f90,$(dirobj)/%.o,$(src))

libcalmat = $(dirlib)/$(libr)

calmat: $(obj)
#  linking with the static pk2 library (=> calmat_s.exe):
	$(comp) $(obj) $(flags) $(libs) -o $(direxe)/$(execs)
#  linking with the shared pk2 library (=> calmat_d.exe):
	$(comp) $(obj) $(flags) $(libd) -o $(direxe)/$(execd)
#  static calmat library:
	$(arch) $(archflag) $(libcalmat) $(obj0)
	$(ranlib) $(libcalmat) 
#  shared calmat library:
	$(comp) $(flags) $(flagdynlib) $(obj) $(libd) -o $(dirlib)/libcalmat_d.so
	@echo " "
	$(call colorBlue, "******************************************")
	$(call colorBlue, "***  Successful compilation of calmat  ***")
	$(call colorBlue, "******************************************")
	@echo " "
	$(call colorBlue, ". The executable '$(exec)' is in the directory: $(absdirexe)")
	$(call colorBlue, ". The library '$(libr)' is in the directory: $(absdirlib)")
	$(call colorBlue, ". The module files are in: $(absdirmod)")
	$(call colorBlue, ". The object files are in: $(absdirobj)")
	@echo " "
#  shell script used to launch calmat_d.exe:
	$(shell echo "# This file was automatically generated by calmat makefile" > $(direxe)/calmat_d)
	$(shell echo "# It is rewritten every time the makefile is run. So do not" >> $(direxe)/calmat_d)
	$(shell echo "# make changes directly in this file, use src/calmat_d.in instead." >> $(direxe)/calmat_d)
	$(shell echo " " >> $(direxe)/calmat_d)
	$(shell echo "#!/bin/bash" >> $(direxe)/calmat_d)
	$(shell echo "pk2bin='$(absdirbinpk2)'" >> $(direxe)/calmat_d)
	$(shell echo "pk2lib='$(absdirlibpk2)'" >> $(direxe)/calmat_d)
	$(shell echo "calmatbin='$(absdirexe)'" >> $(direxe)/calmat_d)
	$(shell cat calmat_d.in >> $(direxe)/calmat_d)
	$(shell chmod +x $(direxe)/calmat_d)
#  shell script used to launch calmat_s.exe:
	$(shell echo "# This file was automatically generated by calmat makefile" > $(direxe)/calmat)
	$(shell echo "# It is rewritten every time the makefile is run. So do not" >> $(direxe)/calmat)
	$(shell echo "# make changes directly in this file, use src/calmat_s.in instead." >> $(direxe)/calmat)
	$(shell echo " " >> $(direxe)/calmat)
	$(shell echo "#!/bin/bash" >> $(direxe)/calmat)
	$(shell echo "pk2bin='$(absdirbinpk2)'" >> $(direxe)/calmat)
	$(shell echo "pk2lib='$(absdirlibpk2)'" >> $(direxe)/calmat)
	$(shell echo "calmatbin='$(absdirexe)'" >> $(direxe)/calmat)	
	$(shell echo "pk2mod='$(absdirmodpk2)'" >> $(direxe)/calmat)
	$(shell echo "calmatobj='$(absdirobj)'" >> $(direxe)/calmat)
	$(shell echo "comp=$(comp)" >> $(direxe)/calmat)
	$(shell echo "flags='$(flagcomp)'" >> $(direxe)/calmat)
	$(shell cat calmat_s.in >> $(direxe)/calmat)
	$(shell chmod +x $(direxe)/calmat)

dummy:  
	@# just to force CalmatVersion to be always compiled (it initialiazes compil date)

$(dirobj)/CalmatVersion.o: CalmatVersion.f90 dummy
	@echo " "
	@echo $(pk2dir)
	$(call colorRed, "> compiling CalmatVersion.f90")
	@echo " "
	$(comp) $(flags) -c CalmatVersion.f90 -o $(dirobj)/CalmatVersion.o

$(dirobj)/CalmatConstants.o: CalmatConstants.f90
	@echo " "
	$(call colorRed, "> compiling CalmatConstants.f90")
	@echo " "
	$(comp) $(flags) -c CalmatConstants.f90 -o $(dirobj)/CalmatConstants.o

$(dirobj)/CalmatInstruction.o: CalmatInstruction.f90      \
                               $(dirobj)/CalmatConstants.o
	@echo " "
	$(call colorRed, "> compiling CalmatInstruction.f90")
	@echo " "
	$(comp) $(flags) -c CalmatInstruction.f90 -o $(dirobj)/CalmatInstruction.o

$(dirobj)/CalmatVar.o: CalmatVar.f90              \
                       $(dirobj)/CalmatConstants.o 
	@echo " "
	$(call colorRed, "> compiling CalmatVar.f90")
	@echo " "
	$(comp) $(flags) -c CalmatVar.f90 -o $(dirobj)/CalmatVar.o

$(dirobj)/CalmatObject.o: CalmatObject.f90           \
                          $(dirobj)/CalmatConstants.o
	@echo " "
	$(call colorRed, "> compiling CalmatObject.f90")
	@echo " "
	$(comp) $(flags) -c CalmatObject.f90 -o $(dirobj)/CalmatObject.o

$(dirobj)/CalmatGlobal.o: CalmatGlobal.f90              \
                          $(dirobj)/CalmatConstants.o   \
                          $(dirobj)/CalmatInstruction.o \
                          $(dirobj)/CalmatVar.o         \
                          $(dirobj)/CalmatObject.o
	@echo " "
	$(call colorRed, "> compiling CalmatGlobal.f90")
	@echo " "
	$(comp) $(flags) -c CalmatGlobal.f90 -o $(dirobj)/CalmatGlobal.o

$(dirobj)/CalmatUtil.o: CalmatUtil.f90           \
                        $(dirobj)/CalmatGlobal.o
	@echo " "
	$(call colorRed, "> compiling CalmatUtil.f90")
	@echo " "
	$(comp) $(flags) -c CalmatUtil.f90 -o $(dirobj)/CalmatUtil.o

$(dirobj)/CalmatSetNGet.o: CalmatSetNGet.f90        \
                           $(deps_setnget)          \
                           $(dirobj)/CalmatGlobal.o
	@echo " "
	$(call colorRed, "> compiling CalmatSetNGet.f90")
	@echo " "
	$(comp) $(flags) -c CalmatSetNGet.f90 -o $(dirobj)/CalmatSetNGet.o

$(dirobj)/CalmatCmd.o: CalmatCmd.f90             \
                       $(dirobj)/CalmatGlobal.o  \
                       $(dirobj)/CalmatUtil.o
	@echo " "
	$(call colorRed, "> compiling CalmatCmd.f90")
	@echo " "
	$(comp) $(flags) -c CalmatCmd.f90 -o $(dirobj)/CalmatCmd.o

$(dirobj)/f90getopt.o: f90getopt.f90
	@echo " "
	$(call colorRed, "> compiling f90getopt.f90")
	@echo " "
	$(comp) $(flags) -c f90getopt.f90 -o $(dirobj)/f90getopt.o

$(dirobj)/Calmat.o: Calmat.f90                  \
                    $(dirobj)/CalmatGlobal.o    \
                    $(dirobj)/CalmatVersion.o   \
                    $(dirobj)/CalmatUtil.o      \
                    $(dirobj)/CalmatSetNGet.o   \
                    $(dirobj)/CalmatCmd.o       \
                    $(dirobj)/f90getopt.o 
	@echo " "
	$(call colorRed, "> compiling Calmat.f90")
	@echo " "
	$(comp) $(flags) -c Calmat.f90 -o $(dirobj)/Calmat.o

$(dirobj)/main.o: main.f90           \
                  $(dirobj)/Calmat.o            
	@echo " "
	$(call colorRed, "> compiling main.f90")
	@echo " "
	$(comp) $(flags) -c main.f90 -o $(dirobj)/main.o

################################################################################
# Cleaning:
#-------------------------------------------------------------------------------

clean:
	@echo " "
	$(call colorRed,"==> Cleaning calmat:")
	@echo " "
	/bin/rm -f $(dirobj)/*.o 
	/bin/rm -f $(dirmod)/*.mod *.o *.mod
	/bin/rm -f *.mod
cleanall:
	@echo " "
	$(call colorRed,"==> Cleaning calmat:")
	@echo " "
	/bin/rm -f $(dirobj)/*.o 
	/bin/rm -f $(dirmod)/*.mod *.o 
	/bin/rm -f $(dirlib)/*.a
	/bin/rm -f *.mod
