################################################################################
#                        Makefile for test programs
#-------------------------------------------------------------------------------


################################################################################
# Help:
#-------------------------------------------------------------------------------

#h |-------------------------------------------------------------------------|
#h | Usage:  make comp = your_compiler [opt = "your_options"] [clean] [prog] |
#h |                                                                         |
#h | where                                                                   |
#h |   . your_compiler: is the name of your compiler (note: this version     |
#h |                    was tested with ifort, gfortran and nagfor).         |
#h |   . your_options : is a string corresponding to a set of compilation    |
#h |                    options (default is opt="-O3").                      |
#h |   . prog         : may be all (default) or one of the sample0 programs   |
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

#
#  Directory of the calmat library (calmat.a) and directory of the corresponding
#  module files:
#
dirlibcalmat = ../../lib/$(comp)
dirmodcalmat = ../../mod/$(comp)

#
#  Directory of the pk2 library (libpk2.a) and directory of the corresponding
#  module files:
#
dirlibpk2 = ../../pk2/lib/$(comp)
dirmodpk2 = ../../pk2/mod/$(comp)

#
#  Blas and Lapack lib:
#
liblapack = -llapack -lblas


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

      ifeq ($(comp),flang-new)
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

cflags = $(opt_optim) $(opt_check) -I $(dirmodcalmat) -I $(dirmodpk2)

lflags = -L$(dirlibcalmat) -lcalmat   -L$(dirlibpk2) -lpk2   $(liblapack)

lDflags = -L$(dirlibcalmat) -lcalmat_d -L$(dirlibpk2) -lpk2_d $(liblapack)

ifeq ($(comp),nagfor)
   cflags += -kind=byte -ieee=full  
endif


################################################################################
# Compilation:
#-------------------------------------------------------------------------------

all: sample01_waysOfCallingCalmat sample02_getAndSetByCopy sample03_getAndSetByMoveAlloc sample04_getPointer sample05_parseThenEvaluate sample06_evaluateAsum sample07_performance sample08_userExpression1 sample09_userExpression2 sample11_userFunctions

sample01_waysOfCallingCalmat: sample01_waysOfCallingCalmat.f90
	$(comp) sample01_waysOfCallingCalmat.f90 $(cflags) $(lflags) -o bin/sample01_waysOfCallingCalmat

sample02_getAndSetByCopy: sample02_getAndSetByCopy.f90
	$(comp) sample02_getAndSetByCopy.f90 $(cflags) $(lflags) -o bin/sample02_getAndSetByCopy

sample03_getAndSetByMoveAlloc: sample03_getAndSetByMoveAlloc.f90
	$(comp) sample03_getAndSetByMoveAlloc.f90 $(cflags) $(lflags) -o bin/sample03_getAndSetByMoveAlloc

sample04_getPointer: sample04_getPointer.f90
	$(comp) sample04_getPointer.f90 $(cflags) $(lflags) -o bin/sample04_getPointer

sample05_parseThenEvaluate: sample05_parseThenEvaluate.f90
	$(comp) sample05_parseThenEvaluate.f90 $(cflags) $(lflags) -o bin/sample05_parseThenEvaluate

sample06_evaluateAsum: sample06_evaluateAsum.f90
	$(comp) sample06_evaluateAsum.f90 $(cflags) $(lflags) -o bin/sample06_evaluateAsum

sample07_performance: sample07_performance.f90
	$(comp) sample07_performance.f90 $(cflags) $(lflags) -o bin/sample07_performance

sample08_userExpression1: sample08_userExpression1.f90
	$(comp) sample08_userExpression1.f90 $(cflags) $(lflags) -o bin/sample08_userExpression1

sample09_userExpression2: sample09_userExpression2.f90
	$(comp) sample09_userExpression2.f90 $(cflags) $(lflags) -o bin/sample09_userExpression2

#linked with the shared libraries:
sample11_userFunctions: sample11_userFunctions.f90
	$(comp) sample11_userFunctions.f90 $(cflags) $(lDflags) -o bin/sample11_userFunctions
################################################################################
# Cleaning:
#-------------------------------------------------------------------------------

clean:
	/bin/rm -f bin/*
