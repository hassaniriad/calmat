################################################################################
#                        Makefile for calmat 
#                (09/2026: modified/adapted by Google AI)
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

# Default rule displayed when no compiler is specified
ifeq ($(comp),)
help:
	@echo "Usage: make comp=<compiler> [opt=\"options\"] [clean|cleanall]"
	@echo "Supported compilers: gfortran, ifx, flang, nagfor"
endif

################################################################################
#  Supported compilers
#-------------------------------------------------------------------------------

INTEL := ifort ifx
GNU   := gfortran gfortran-14 gfortran-11
NAG   := nagfor
FLANG := flang flang-new

################################################################################
#  Settings & OS Detection
#-------------------------------------------------------------------------------

arch      = /usr/bin/ar
archflag  = cr
ranlib    = /usr/bin/ranlib

execs = calmat_s.exe
execd = calmat_d.exe
libr  = libcalmat.a

# OS Auto-detection
UNAME_S := $(shell uname -s)

# Default Multi-platform Math libraries configurations (Linux / Standard fallback)
liblapack   = -llapack -lblas
SYS_LDFLAGS = 
SO_EXT = so
RPATH_FLAG = -Wl,-rpath,$(absdirlibpk2)

ifeq ($(UNAME_S),Darwin)
   # Safe-check for Apple Silicon (Intel ifx compiler is missing on ARM macOS)
   ifeq ($(comp),ifx)
      $(error Intel compiler 'ifx' is not available on macOS ARM architectures)
   endif

   # Native macOS Math Engine: Use Apple Accelerate Framework (Zero homebrew dependency)
   SDK_PATH    := $(shell xcrun --show-sdk-path)
   liblapack   := -Wl,-F$(SDK_PATH)/System/Library/Frameworks -Wl,-framework,Accelerate
   SYS_LDFLAGS := -L$(SDK_PATH)/usr/lib

   SO_EXT = dylib
   RPATH_FLAG = -Wl,-rpath,$(absdirlibpk2)
endif


# Library path references
pk2dir    = ../pk2
dirlibpk2 = $(pk2dir)/lib/$(comp)
dirmodpk2 = $(pk2dir)/mod/$(comp)
dirobjpk2 = $(pk2dir)/obj/$(comp)
dirbinpk2 = $(pk2dir)/bin/$(comp)

absdirlibpk2 := $(abspath $(dirlibpk2))
absdirbinpk2 := $(abspath $(dirbinpk2))
absdirmodpk2 := $(abspath $(dirmodpk2))

# Math & External libraries linking strings setup
libs = -L$(dirlibpk2) -lpk2   $(SYS_LDFLAGS) $(liblapack)
libd = -L$(dirlibpk2) -lpk2_d $(RPATH_FLAG) $(SYS_LDFLAGS) $(liblapack)


# Target directory outputs setup
direxe = ../bin/$(comp)
dirmod = ../mod/$(comp)
dirobj = ../obj/$(comp)
dirlib = ../lib/$(comp)

absdirlib := $(abspath $(dirlib))
absdirmod := $(abspath $(dirmod))
absdirobj := $(abspath $(dirobj))
absdirexe := $(abspath $(direxe))

# Automation tool to generate missing folders safely
$(shell mkdir -p $(direxe) $(dirmod) $(dirobj) $(dirlib))

################################################################################
#  Compilers Optimization & Debug flags selection
#-------------------------------------------------------------------------------

ifneq ($(filter $(comp),$(GNU)),)
   ifeq ($(opt),)
      OPTS = -O3
   else ifeq ($(opt),check)
      OPTS = -Og -fcheck=all -fbacktrace -fimplicit-none   
   else       
      OPTS = $(opt)
   endif
   cpflags  = $(OPTS)    
   flagcomp = $(OPTS)
   flags    = $(OPTS) -cpp -J $(dirmod) -I $(dirmodpk2) -fmax-errors=5 -ffree-line-length-none

else ifneq ($(filter $(comp),$(INTEL)),)
   ifeq ($(opt),)
      OPTS = -O3 
   else ifeq ($(opt),check)
      OPTS = -O0 -check all -traceback -gen-interfaces -warn interfaces -check nobounds                 
   else       
      OPTS = $(opt)
   endif   
   cpflags  = $(OPTS) -heap-arrays 200      
   flagcomp = $(OPTS)
   flags    = $(OPTS) -fpp -module $(dirmod) -I $(dirmodpk2) -heap-arrays 200 
   
else ifneq ($(filter $(comp),$(NAG)),)
   ifeq ($(opt),)
      OPTS = -O3
   else ifeq ($(opt),check)
      OPTS = -O0 -C=all
   else       
      OPTS = $(opt)
   endif   
   cpflags    = $(OPTS) -kind=byte -ieee=full
   flagcomp   = $(OPTS) 
   flags      = $(OPTS) -fpp -kind=byte -ieee=full -mdir $(dirmod) -I $(dirmod) -I $(dirmodpk2)
   flagdynlib = -PIC -Wl,-shared

else ifneq ($(filter $(comp),$(FLANG)),)
   ifeq ($(opt),)
      OPTS += -O3
   else ifeq ($(opt),check)
      OPTS += -O0 -fimplicit-none   
   else       
      OPTS += $(opt)
   endif   
   cpflags  = $(OPTS)    
   flagcomp = $(OPTS)
   flags    = $(OPTS) -cpp -module-dir $(dirmod) -I $(dirmodpk2)
endif

# Clean escaping strings for preprocessors macros variables (Supports folder spaces and dates)
flags += -D__CDATE='"/$(shell date)/"' -D__COPTS='"/$(cpflags)/"'

################################################################################
# Source Code Manifest
#-------------------------------------------------------------------------------

src0 = CalmatConstants.f90 CalmatInstruction.f90 CalmatVar.f90 CalmatObject.f90 \
       CalmatGlobal.f90 CalmatUtil.f90 CalmatCmd.f90 f90getopt.f90 \
       Calmat.f90 CalmatVersion.f90 CalmatSetNGet.f90

src = $(src0) main.f90

deps_setnget = include/calmatsetnget_cp_x_to_var.inc include/calmatsetnget_mv_x_to_var.inc \
               include/calmatsetnget_cp_var_to_x.inc include/calmatsetnget_mv_var_to_x.inc \
               include/calmatsetnget_ptr_var_to_x.inc 

obj0 = $(patsubst %.f90,$(dirobj)/%.o,$(src0))
obj  = $(patsubst %.f90,$(dirobj)/%.o,$(src))

libcalmat = $(dirlib)/$(libr)
libpk2    = $(dirlibpk2)/libpk2.a

################################################################################
# Compilation & Assembly Targets
#-------------------------------------------------------------------------------

# Core program generation target
calmat: $(obj) $(libpk2)
#       linking with the static pk2 library (=> calmat_s.exe):
	$(comp) $(obj) $(OPTS) $(libs) -o $(direxe)/$(execs)
#       linking with the shared pk2 library (=> calmat_d.exe) (Now active!):
	$(comp) $(obj) $(OPTS) $(libd) -o $(direxe)/$(execd)
#       static calmat library:
	$(arch) $(archflag) $(libcalmat) $(obj0)
	$(ranlib) $(libcalmat) 
	@echo " "
	$(call colorBlue, "******************************************")
	$(call colorBlue, "***  Successful compilation of calmat  ***")
	$(call colorBlue, "******************************************")
	@echo " "
	$(call colorBlue, ". The script launchers 'calmat' and 'calmat_d' are located in the directory: $(absdirexe)")
	$(call colorBlue, ". The executables '$(execs)' and '$(execd)' are located in the directory: $(absdirexe)")
	$(call colorBlue, ". The library '$(libr)' is located in the directory: $(absdirlib)")
	$(call colorBlue, ". The module files are located in: $(absdirmod)")
	$(call colorBlue, ". The object files are located in: $(absdirobj)")
#	Automated wrapper scripts generation
	@exec > $(direxe)/calmat_d; \
	echo "#!/bin/bash"; \
	echo "pk2bin='$(absdirbinpk2)'"; \
	echo "pk2lib='$(absdirlibpk2)'"; \
	echo "calmatbin='$(absdirexe)'"; \
	cat calmat_d.in
	@chmod +x $(direxe)/calmat_d
	@exec > $(direxe)/calmat; \
	echo "#!/bin/bash"; \
	echo "pk2bin='$(absdirbinpk2)'"; \
	echo "pk2lib='$(absdirlibpk2)'"; \
	echo "calmatbin='$(absdirexe)'"; \
	echo "pk2mod='$(absdirmodpk2)'"; \
	echo "calmatobj='$(absdirobj)'"; \
	echo "comp=$(comp)"; \
	echo "flags='$(flagcomp)'"; \
	cat calmat_s.in
	@chmod +x $(direxe)/calmat

# Dummy pattern to enforce recompilation of version details on each build run
dummy:  

# Implicit generic compilation sequence for project source files
$(dirobj)/%.o: %.f90
	@echo " "
	$(call colorRed, "> compiling $<")
	$(comp) $(flags) -c $< -o $@

################################################################################
# Fortran Project Structural Dependency Tree
#-------------------------------------------------------------------------------
$(dirobj)/CalmatVersion.o: CalmatVersion.f90 dummy $(libpk2)
$(dirobj)/CalmatConstants.o: CalmatConstants.f90 $(libpk2)
$(dirobj)/CalmatInstruction.o: CalmatInstruction.f90 $(dirobj)/CalmatConstants.o $(libpk2)
$(dirobj)/CalmatVar.o: CalmatVar.f90 $(dirobj)/CalmatConstants.o $(libpk2)
$(dirobj)/CalmatObject.o: CalmatObject.f90 $(dirobj)/CalmatConstants.o $(libpk2)
$(dirobj)/CalmatGlobal.o: CalmatGlobal.f90 $(dirobj)/CalmatConstants.o $(dirobj)/CalmatInstruction.o $(dirobj)/CalmatVar.o $(dirobj)/CalmatObject.o $(libpk2)
$(dirobj)/CalmatUtil.o: CalmatUtil.f90 $(dirobj)/CalmatGlobal.o $(libpk2)
$(dirobj)/CalmatSetNGet.o: CalmatSetNGet.f90 $(deps_setnget) $(dirobj)/CalmatGlobal.o $(libpk2)
$(dirobj)/CalmatCmd.o: CalmatCmd.f90 $(dirobj)/CalmatGlobal.o $(dirobj)/CalmatUtil.o $(libpk2)
$(dirobj)/f90getopt.o: f90getopt.f90
$(dirobj)/Calmat.o: Calmat.f90 $(dirobj)/CalmatGlobal.o $(dirobj)/CalmatVersion.o $(dirobj)/CalmatUtil.o $(dirobj)/CalmatSetNGet.o $(dirobj)/CalmatCmd.o $(dirobj)/f90getopt.o $(libpk2)
$(dirobj)/main.o: main.f90 $(dirobj)/Calmat.o $(libpk2)

################################################################################
# Workspace Housekeeping
#-------------------------------------------------------------------------------
clean:
	/bin/rm -f $(dirobj)/*.o $(dirmod)/*.mod *.o *.mod

cleanall:
	/bin/rm -f $(dirobj)/*.o $(dirmod)/*.mod $(dirlib)/*.a *.o *.mod $(direxe)/calmat $(direxe)/calmat_d $(direxe)/$(execs)

