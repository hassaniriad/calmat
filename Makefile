################################################################################
#              Root Master Makefile for pk2 & calmat workspace
#                 (09/2026: modified/adapted by Google AI)
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

# Propagate settings parameters down to children Makefiles
export comp
export opt
export kind

# Path coordinates to sub-project directories
PK2_DIR    = pk2/src
CALMAT_DIR = src

.PHONY: all pk2 calmat clean cleanall help

# 1. If 'comp' is empty, intercept immediately and display the full user guide
ifeq ($(comp),)
all: help
else
# 2. If 'comp' is provided, execute the normal compilation sequence
all: pk2 calmat
endif

pk2:
	@echo "" 
	$(call colorRed, "==================================================================")
	$(call colorRed, "===> Compiling PK2 Library Dependency")
	$(call colorRed, "==================================================================")
	$(MAKE) -C $(PK2_DIR)

calmat: pk2
	@echo "" 
	$(call colorRed, "==================================================================")
	$(call colorRed,  "===> Compiling CALMAT Program Execution Engine")
	$(call colorRed, "==================================================================")
	$(MAKE) -C $(CALMAT_DIR)

################################################################################
# Maintenance routines cascading targets
#-------------------------------------------------------------------------------
clean:
	$(call colorRed,"==> Cleaning pk2 and calmat:")
	$(MAKE) -C $(PK2_DIR) clean
	$(call colorRed,"==> Cleaning calmat:")
	$(MAKE) -C $(CALMAT_DIR) clean

cleanall:
	$(MAKE) -C $(PK2_DIR) cleanall
	$(MAKE) -C $(CALMAT_DIR) cleanall

help:
	@echo "|------------------------------------------------------------------------|"
	@echo "| Usage:                                                                 |"
	@echo "|    make comp=<your_compiler> [opt=\"<options>\"] [kind=\"<choices>\"]      |"
	@echo "|                              [clean] [cleanall]                        |"
	@echo "|                                                                        |"
	@echo "| Where:                                                                 |"
	@echo "|   o <your_compiler>: Target compiler name. Tested options:             |"
	@echo "|                      . gfortran (macOS ARM & Linux)                    |"
	@echo "|                      . ifx      (Linux only)                           |"
	@echo "|                      . flang    (macOS ARM & Linux)                    |"
	@echo "|                      . nagfor   (macOS ARM & Linux)                    |"
	@echo "|   o <options>      : Compilation flag string (default: opt=\"-O3\").     |"
	@echo "|   o <choices>      : Setup string for Fortran kind parameters.         |"
	@echo "|                      Can include -DI32 / -DI64 (int32 or int64)        |"
	@echo "|                      and -DSP / -DDP (real32 or real64).               |"
	@echo "|                      Default configuration is kind=\"-DI32 -DDP\".       |"
	@echo "|                                                                        |"
	@echo "| Examples:                                                              |"
	@echo "|   . make                                                               |"
	@echo "|     Prints this detailed help menu.                                    |"
	@echo "|   . make comp=gfortran                                                 |"
	@echo "|     Compiles everything with -O3 flag and default 32-bit/double kinds. |"
	@echo "|   . make comp=gfortran kind=\"-DSP\"                                     |"
	@echo "|     Compiles using single precision real32 (and 32-bit integers).      |"
	@echo "|   . make comp=ifx opt=\"-O3 -heap-arrays 200\"                           |"
	@echo "|     Compiles on Linux with Intel Next-Gen and custom performance opts. |"
	@echo "|   . make comp=nagfor opt=check                                         |"
	@echo "|     Compiles the workspace with strict safety & bounds checking flags. |"
	@echo "|   . make cleanall                                                      |"
	@echo "|     Deletes all objects (*.o), modules (*.mod) and binary exec files.  |"
	@echo "|------------------------------------------------------------------------|"

