
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

pk2=pk2
pk2 := $(abspath $(pk2))


help:
#h |--------------------------------------------------------------------------|
#h | Usage:  make comp=yourCompiler  [opt="yourOptions"]                      |
#h |                                 [kind="yourChoices"]  [clean?]           |
#h |                                                                          |
#h |                                                                          |
#h | where                                                                    |
#h |   o yourCompiler: is the name of your compiler (e.g. ifort).             |
#h |                   This version was tested only with:                     |
#h |                     . ifort (mac): version 2021.7.0                      |
#h |                     . ifx (linux): version 2024.1.2                      |
#h |                     . gfortran (mac): 14.1.0 (Homebrew GCC 14.1.0)       |
#h |                     . gfortran (linux): (GCC) 11.2.1 (Red Hat 11.2.1-9)  |
#h |                     . nagfor (mac): Release 7.2 Build 7213               |
#h |   o yourOptions : is a string corresponding to a set of compilation      |
#h |                   options (default is opt="-O3").                        |
#h |   o yourChoices : is a string corresponding to you choices for kind      |
#h |                   parameters. It may contain -DI32 or -DI64 (for int32   |
#h |                   or int64 for integers) and -DSP or -DDP (for real32 or |
#h |                   real64 for reals and complexes).                       |
#h |                   The  default is  kind="-DI32 -DDP".                    |
#h |   o clean?      : can be cleanpk2, cleancalmat or cleanall               |
#h |                   and will remove .mod, .o and .a files of the pk2 lib   |
#h |                   or of calmat or both, respectively.                    |
#h |                                                                          |
#h | Examples:                                                                |
#h |   . make                                                                 |
#h |        prints this help.                                                 |
#h |   . make comp=ifort cleanpk2                                             |
#h |        deletes the object files (from obj/ifort),                        |
#h |                the module files (from mod/ifort).                        |
#h |                the archive file (from lib/ifort).                        |
#h |   . make comp=ifort cleancalmat                                          |
#h |        deletes the object files (from app/calmat/obj/ifort),             |
#h |                the module files (from app/calmat/mod/ifort),             |
#h |                the archive file (from app/calmat/lib/ifort).             |
#h |   . make comp=gfortran                                                   |
#h |        compiles with the default (-O3) flag.                             |
#h |   . make comp=gfortran kind="-DSP"                                       |
#h |        compiles with real32 reals/complexes (and 32 bits integers)       |
#h |   . make comp=gfortran kind="-DSP -DI64"                                 |
#h |        compiles also with int64                                          |
#h |   . make comp=ifort opt="-O3 -fp-model fast=2 -parallel"                 |
#h |         compiles with the specified options.                             |
#h |   . make comp=nagfor opt=check                                           |
#h |         compiles with some checking options (predefined according to     |
#h |         one of the compilers cited above).                               |
#h |--------------------------------------------------------------------------|
ifeq ($(comp),)
   help:     #// Show this help.
	@sed -ne '/@sed/!s/#h //p' $(MAKEFILE_LIST)
else
	@echo " "
	$(call colorRed, "*************************************")
	$(call colorRed, "****  COMPILING THE PK2 LIBRARY  ****")
	$(call colorRed, "*************************************")
	@echo " "	
	@( cd $(pk2)/src; $(MAKE)  )

	@echo " "
	$(call colorRed, "************************************")
	$(call colorRed, "****      COMPILING CALMAT      ****")
	$(call colorRed, "************************************")
	@echo " "	
	@( cd src; $(MAKE) )


cleanpk2:
	@echo " "
	$(call colorRed,"==> Cleaning pk2:")
	@echo " "
	rm -f $(pk2)/obj/$(comp)/*.o 
	rm -f $(pk2)/mod/$(comp)/*.mod 
	rm -f $(pk2)/lib/$(comp)/*.a 
	rm -f $(pk2)/lib/$(comp)/*.so 

cleancalmat:
	@echo " "
	$(call colorRed,"==> Cleaning calmat:")
	@echo " "
	rm -f obj/$(comp)/*.o 
	rm -f mod/$(comp)/*.mod 
	rm -f lib/$(comp)/*.a 

cleanall:
	@echo " "
	$(call colorRed,"==> Cleaning pk2 and calmat:")
	@echo " "
	rm -f $(pk2)/obj/$(comp)/*.o 
	rm -f $(pk2)/mod/$(comp)/*.mod 
	rm -f $(pk2)/lib/$(comp)/*.a 
	rm -f $(pk2)/lib/$(comp)/*.so 
	rm -f obj/$(comp)/*.o 
	rm -f mod/$(comp)/*.mod 
	rm -f lib/$(comp)/*.a 
endif
