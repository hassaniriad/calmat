##############################################################################
#                        Makefile for the pk2 library
#                 (09/2026: modified/adapted by Google AI)
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

ifeq ($(comp),)
help:
	@echo "Usage: make comp=<compiler> [opt=\"options\"] [kind=\"choices\"] [clean|cleanall]"
	@echo "Compilateurs supportés : gfortran, ifx, flang, nagfor"
endif

##############################################################################
#  Supported compilers
#-----------------------------------------------------------------------------

INTEL := ifort ifx
GNU   := gfortran gfortran-14 gfortran-11
NAG   := nagfor
FLANG := flang flang-new


##############################################################################
#  Settings & OS Detection
#-----------------------------------------------------------------------------

arch      = /usr/bin/ar
archflag  = cr
ranlib    = /usr/bin/ranlib

# Détection de l'OS
UNAME_S := $(shell uname -s)

# Configuration par défaut (Linux / Standard)
liblapack   = -llapack -lblas
SYS_LDFLAGS = 
# Default extensions (Linux)
SO_EXT        = so
DYNLINK_FLAGS = -shared

ifeq ($(UNAME_S),Darwin)
   ifeq ($(comp),ifx)
      $(error Le compilateur Intel 'ifx' n'est pas disponible sur macOS ARM (Apple Silicon))
   endif

   SDK_PATH    := $(shell xcrun --show-sdk-path)
   SYS_LDFLAGS := -L$(SDK_PATH)/usr/lib

   # macOS Specific Extensions
   SO_EXT        = dylib
      
   # Universally passing Apple Framework Accelerate flags
   liblapack   := -Wl,-F$(SDK_PATH)/System/Library/Frameworks -Wl,-framework,Accelerate
      
   # Adapt shared library generation flags based on the chosen compiler
   ifneq ($(filter $(comp),$(FLANG)),)
     # Syntax required by LLVM Flang driver
     DYNLINK_FLAGS = -Wl,-dynamic -Wl,-dylib -Wl,-install_name,@rpath/libpk2_d.dylib
   else ifneq ($(filter $(comp),$(NAG)),)
     # Syntax required by NAG nagfor driver (-Wl= passes comma-separated flags directly)
     DYNLINK_FLAGS = -Wl=-dynamiclib,-install_name,@rpath/libpk2_d.dylib
     DYNLINK_FLAGS = -Wl,-dynamiclib -Wl,-install_name -Wl,@rpath/libpk2_d.dylib
   else
     # Standard syntax for gfortran and others
     DYNLINK_FLAGS = -dynamiclib -Wl,-install_name,@rpath/libpk2_d.dylib
   endif

endif

# Arborescence des dossiers
pk2dir = ..
dirlib = $(pk2dir)/lib/$(comp)
dirmod = $(pk2dir)/mod/$(comp)
dirobj = $(pk2dir)/obj/$(comp)
dirbin = $(pk2dir)/bin/$(comp)

# Création automatique des dossiers de destination
$(shell mkdir -p $(dirlib) $(dirmod) $(dirobj) $(dirbin) $(dirobj)/user)

abspk2dir := $(abspath $(pk2dir))
absdirlib := $(abspath $(dirlib))
absdirmod := $(abspath $(dirmod))
absdirobj := $(abspath $(dirobj))
absdirbin := $(abspath $(dirbin))

##############################################################################
#  Compilers Flags Configuration
#-----------------------------------------------------------------------------

OPTS = $(kind)

ifneq ($(filter $(comp),$(GNU)),)
   ifeq ($(opt),)
      OPTS += -O3
   else ifeq ($(opt),check)
      OPTS += -Og -fcheck=all -fbacktrace -fimplicit-none   
   else       
      OPTS += $(opt)
   endif
   flags = $(OPTS) -cpp -J $(dirmod) -fmax-errors=5 -ffree-line-length-none
   flagdynlib = -fPIC -shared
   
else ifneq ($(filter $(comp),$(INTEL)),)
   ifeq ($(opt),)
      OPTS += -O3
   else ifeq ($(opt),check)
      OPTS += -check all -traceback -gen-interfaces -warn interfaces -check nobounds                 
   else       
      OPTS += $(opt)
   endif   
   flags = $(OPTS) -fpp -module $(dirmod)
   flagdynlib = -fPIC -shared -Wl,-ld_classic   
   
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
      OPTS += -O0 -fimplicit-none   
   else       
      OPTS += $(opt)
   endif   
   flags = $(OPTS) -cpp -module-dir $(dirmod) -I $(dirmod)
   flagdynlib = -PIC -Wl,-shared
endif

# Nettoyage et sécurisation absolue des macros textuelles pour gfortran / cpp
# Les triples protections empêchent le compilateur d'éclater les chaînes en morceaux s'il y a des espaces
flags += -D__CDATE='"/$(shell date)/"' -D__DIR='"/$(abspk2dir)/"' -D__COMP='"/$(comp)/"'

# CORRECTIF LINUX : 
ifneq ($(UNAME_S),Darwin)
   flags += -fPIC
endif

################################################################################
# Source & Objects Management
#-------------------------------------------------------------------------------

src = LapackInterface.f90 kindParameters.f90 ansiColor.f90 str.f90 \
      pk2Constants.f90 err.f90 util_Strings.f90 util_Other.f90     \
      util.f90 signalHandler.f90 dispmodule.f90 disp_i8mod.f90     \
      bk2.f90 pk2.f90 pk2f.f90 pk2obj.f90 pk2Interpreter.f90       \
      pk2mod.f90

srcu = userfunc.f90 

obj  = $(patsubst %.f90,$(dirobj)/%.o,$(src))
obju = $(patsubst %.f90,$(dirobj)/user/%.o,$(srcu))

libpk2    = libpk2.a
libpk2dyn = libpk2_d.$(SO_EXT)

################################################################################
# Compilation Rules
#-------------------------------------------------------------------------------

# Règle principale (Construction de la bibliothèque statique)
libpk2: $(obj) $(obju)
# Static version:
	$(arch) $(archflag) $(dirlib)/$(libpk2) $(obj) $(obju)
	$(ranlib) $(dirlib)/$(libpk2)
# Shared/Dynamic version (Now active!):
	$(comp) $(flags) $(DYNLINK_FLAGS) $(obj) $(obju) $(SYS_LDFLAGS) $(liblapack) -o $(dirlib)/$(libpk2dyn)
# Utility binary:
	$(comp) $(flags) -I $(dirmod) util/addFun.f90 -L$(dirlib) -lpk2 $(SYS_LDFLAGS) $(liblapack) -o $(dirbin)/addFun
	@echo ""
	$(call colorBlue, "*****************************************")
	$(call colorBlue, "***   Successful compilation of pk2   ***")
	$(call colorBlue, "*****************************************")
	@echo ""
	$(call colorBlue, ". The libraries '$(libpk2)' and '$(libpk2dyn)' are in the directory: $(absdirlib)")
	$(call colorBlue, ". The module files are in: $(absdirmod)")
	$(call colorBlue, ". The object files are in: $(absdirobj)")   

# Règle générique pour compiler tous les fichiers .f90 de la racine
$(dirobj)/%.o: %.f90
	@echo " "
	$(call colorRed, "> compiling $<")
	$(comp) $(flags) -c $< -o $@

# Règle générique pour le dossier third_party/
$(dirobj)/%.o: third_party/%.f90
	@echo " "
	$(call colorRed, "> compiling $<")
	$(comp) $(flags) -c $< -o $@

# Règle générique pour le dossier user/
$(dirobj)/user/%.o: %.f90
	@echo " "
	$(call colorRed, "> compiling $<")
	$(comp) $(flags) -c $< -o $@

################################################################################
# Fortran Modules Dependency Tree (Pour forcer l'ordre de compilation)
#-------------------------------------------------------------------------------
$(dirobj)/str.o: $(dirobj)/ansiColor.o
$(dirobj)/pk2Constants.o: $(dirobj)/kindParameters.o
$(dirobj)/err.o: $(dirobj)/kindParameters.o $(dirobj)/ansiColor.o
$(dirobj)/util_Strings.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/pk2Constants.o $(dirobj)/err.o
$(dirobj)/util_Other.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/pk2Constants.o $(dirobj)/err.o $(dirobj)/util_Strings.o
$(dirobj)/util.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/pk2Constants.o $(dirobj)/err.o $(dirobj)/util_Strings.o $(dirobj)/util_Other.o
$(dirobj)/signalHandler.o: $(dirobj)/kindParameters.o $(dirobj)/pk2Constants.o $(dirobj)/util_Strings.o
$(dirobj)/disp_i8mod.o: $(dirobj)/dispmodule.o
$(dirobj)/bk2.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/pk2Constants.o $(dirobj)/err.o $(dirobj)/LapackInterface.o $(dirobj)/util_Strings.o $(dirobj)/util_Other.o $(dirobj)/util.o
$(dirobj)/pk2.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/pk2Constants.o $(dirobj)/err.o $(dirobj)/LapackInterface.o $(dirobj)/util_Strings.o $(dirobj)/util_Other.o $(dirobj)/util.o $(dirobj)/bk2.o
$(dirobj)/pk2f.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/pk2Constants.o $(dirobj)/err.o $(dirobj)/LapackInterface.o $(dirobj)/util_Strings.o $(dirobj)/util_Other.o $(dirobj)/util.o $(dirobj)/bk2.o $(dirobj)/pk2.o
$(dirobj)/pk2obj.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/pk2Constants.o $(dirobj)/err.o $(dirobj)/LapackInterface.o $(dirobj)/util_Strings.o $(dirobj)/util_Other.o $(dirobj)/util.o $(dirobj)/bk2.o $(dirobj)/pk2.o
$(dirobj)/user/userfunc.o: $(dirobj)/pk2.o
$(dirobj)/pk2Interpreter.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/pk2Constants.o $(dirobj)/err.o $(dirobj)/LapackInterface.o $(dirobj)/util_Strings.o $(dirobj)/util_Other.o $(dirobj)/util.o $(dirobj)/bk2.o $(dirobj)/pk2.o $(dirobj)/pk2f.o $(dirobj)/user/userfunc.o
$(dirobj)/pk2mod.o: $(dirobj)/kindParameters.o $(dirobj)/str.o $(dirobj)/ansiColor.o $(dirobj)/pk2Constants.o $(dirobj)/err.o $(dirobj)/LapackInterface.o $(dirobj)/util_Strings.o $(dirobj)/util_Other.o $(dirobj)/util.o $(dirobj)/signalHandler.o $(dirobj)/bk2.o $(dirobj)/pk2.o $(dirobj)/pk2f.o $(dirobj)/pk2obj.o $(dirobj)/pk2Interpreter.o

##############################################################################
# Cleaning
#-----------------------------------------------------------------------------
clean:
	/bin/rm -f $(dirobj)/*.o $(dirobj)/user/*.o

cleanall:
	/bin/rm -f $(dirobj)/*.o $(dirobj)/user/*.o $(dirmod)/*.mod $(dirlib)/*.a $(dirlib)/*.so $(dirbin)/addFun

