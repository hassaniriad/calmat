################################################################################
#                        Makefile for test programs
#-------------------------------------------------------------------------------


################################################################################
# Help:
#-------------------------------------------------------------------------------

#h |-------------------------------------------------------------------------|
#h | Usage: make comp = <your compiler>  [opt = "<your options>"] [clean]    |
#h |                                                                         |
#h | where                                                                   |
#h |   . <your compiler>: is the name of your compiler (note: this version   |
#h |                      was tested with ifort, gfortran and nagfor).       |
#h |   . <your options> : is a string corresponding to a set of compilation  |
#h |                      options (default is opt="-O3").                    |
#h |                                                                         |
#h | Examples:                                                               |
#h |   . make                                                                |
#h |        prints this help.                                                |
#h |   . make comp=ifort clean                                               |
#h |        deletes the executables                                          |
#h |   . make comp=gfortran                                                  |
#h |        compiles with -O3 optimization flag.                             |
#h |   . make comp=ifort opt="-O3 -fp-model fast=2 -parallel"                |
#h |         compiles with the specified options.                            |
#h |   . make comp=ifort opt=check                                           |
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

all: check_Util_Strings check_Util_Other check_bk2 check_pk2 check_pk2f check_pk2obj check_pk2interpreter check_pk2movealloc check_str

#
#  Directory of the pk2 library (libpk2.a) and directory of the corresponding
#  module files:
#
dirlibpk2 = ../../lib/$(comp)
dirmodpk2 = ../../mod/$(comp)

#
#  Blas and Lapack lib:
#
liblapack = -llapack -lblas

#
#  Create the subdirectory (bin) for executables
#
$(shell mkdir -p bin)

################################################################################
#  Compilation options:
#-------------------------------------------------------------------------------

ifeq ($(opt),)
#
#  No options given, set them to "-O3":
#   
   opt_optim = -O3 
   opt_check =     
else
     
   ifeq ($(opt),check)
#
#     Default options for checking (compiler dependent):
#
      ifeq ($(comp),gfortran)
         opt_optim = -Og
         opt_check = -fcheck=all -fbacktrace -Wall -fimplicit-none
      endif
           
      ifeq ($(comp),ifort)
         opt_optim = -O0
         opt_check = -check all -traceback -gen-interfaces -warn interfaces
      endif
           
      ifeq ($(comp),ifx)
         opt_optim = -O0
         opt_check = -check all -traceback -gen-interfaces -warn interfaces
      endif

      ifeq ($(comp),nagfor)
         opt_optim = -O0
         opt_check = -C=all
      endif

      ifeq ($(comp),flang)
         opt_optim = -O0
      endif

   else
#
#     User's options:
#
      opt_optim =$(opt)
      opt_check =
   endif
endif

cflags = $(opt_optim) $(opt_check) -I $(dirmodpk2)

lflags  = -L$(dirlibpk2) -lpk2 $(liblapack)

ifeq ($(comp),nagfor)
   cflags += -kind=byte -ieee=full 
endif


################################################################################
# Compilation:
#-------------------------------------------------------------------------------

check_Util_Strings: Check_Util_Strings.f90
	$(comp) Check_Util_Strings.f90 $(cflags) $(lflags) -o bin/check_Util_Strings

check_Util_Other: Check_Util_Other.f90
	$(comp) Check_Util_Other.f90 $(cflags) $(lflags) -o bin/check_Util_Other

check_bk2: Check_bk2.f90
	$(comp) Check_bk2.f90 $(cflags) $(lflags) -o bin/check_bk2

check_pk2: Check_pk2.f90
	$(comp) Check_pk2.f90 $(cflags) $(lflags) -o bin/check_pk2

check_pk2f: Check_pk2f.f90
	$(comp) Check_pk2f.f90 $(cflags) $(lflags) -o bin/check_pk2f

check_pk2obj: Check_pk2obj.f90
	$(comp) Check_pk2obj.f90 $(cflags) $(lflags) -o bin/check_pk2obj

check_pk2interpreter: Check_pk2interpreter.f90
	$(comp) Check_pk2interpreter.f90 $(cflags) $(lflags) -o bin/check_pk2interpreter

check_pk2movealloc: Check_pk2moveAlloc.f90
	$(comp) Check_pk2moveAlloc.f90 $(cflags) $(lflags) -o bin/check_pk2movealloc

check_str: Check_str.f90
	$(comp) Check_str.f90 $(cflags) $(lflags) -o bin/check_str
################################################################################
# Cleaning:
#-------------------------------------------------------------------------------

clean:
	/bin/rm -f bin/*
