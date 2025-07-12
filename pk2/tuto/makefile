################################################################################
#                        Makefile for test programs
#-------------------------------------------------------------------------------


################################################################################
# Help:
#-------------------------------------------------------------------------------

#h |-------------------------------------------------------------------------|
#h | Usage: make comp = your_compiler  [opt = "your_options"] [clean] [prog] |
#h |                                                                         |
#h | where                                                                   |
#h |   . your_compiler: is the name of your compiler (note: this version     |
#h |                    was tested with ifort, gfortran and nagfor).         |
#h |   . your_options : is a string corresponding to a set of compilation    |
#h |                    options (default is opt="-O3").                      |
#h |   . prog         : may be all (default) or one of the programs          |
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
#  Directory of the pk2 library (libpk2.a) and directory of the corresponding
#  module files:
#
dirlibpk2 = ../lib/$(comp)
dirmodpk2 = ../mod/$(comp)

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

ifeq ($(comp),gfortran)
   cflags += -cpp
endif

ifeq ($(comp),ifort)
   cflags += -fpp
endif

ifeq ($(comp),ifx)
   cflags += -fpp 
endif

ifeq ($(comp),nagfor)
   cflags += -fpp -kind=byte -ieee=full
endif


################################################################################
# Compilation:
#-------------------------------------------------------------------------------

all: assign assign2 booleans print error_handling poisson2d read_a_text_file \
     ex_copy ex_delrows ex_extracmat ex_insert ex_mergemats \
     ex_movealloc1 ex_movealloc2 ex_pointer ex_reshape ex_setsubmat \
     using_Interpreter_1 using_Interpreter_2 using_Interpreter_3 \
     using_Interpreter_4 using_Interpreter_5 using_Interpreter_6 \
     adding_userFunctions

assign: assign.f90
	$(comp) assign.f90 $(cflags) $(lflags) -o bin/assign

assign2: assign2.f90
	$(comp) assign2.f90 $(cflags) $(lflags) -o bin/assign2

booleans: booleans.f90
	$(comp) booleans.f90 $(cflags) $(lflags) -o bin/booleans

print: print.f90
	$(comp) print.f90 $(cflags) $(lflags) -o bin/print

error_handling: error_handling.f90
	$(comp) error_handling.f90 $(cflags) $(lflags) -o bin/error_handling

poisson2d: poisson2d.f90
	$(comp) poisson2d.f90 $(cflags) $(lflags) -o bin/poisson2d

ex_copy: ex_copy.f90
	$(comp) ex_copy.f90 $(cflags) $(lflags) -o bin/ex_copy

ex_delrows: ex_delrows.f90
	$(comp) ex_delrows.f90 $(cflags) $(lflags) -o bin/ex_delrows

ex_extracmat: ex_extracmat.f90
	$(comp) ex_extracmat.f90 $(cflags) $(lflags) -o bin/ex_extracmat

ex_insert: ex_insert.f90
	$(comp) ex_insert.f90 $(cflags) $(lflags) -o bin/ex_insert

ex_mergemats: ex_mergemats.f90
	$(comp) ex_mergemats.f90 $(cflags) $(lflags) -o bin/ex_mergemats

ex_movealloc1: ex_movealloc1.f90
	$(comp) ex_movealloc1.f90 $(cflags) $(lflags) -o bin/ex_movealloc1

ex_movealloc2: ex_movealloc2.f90
	$(comp) ex_movealloc2.f90 $(cflags) $(lflags) -o bin/ex_movealloc2

ex_pointer: ex_pointer.f90
	$(comp) ex_pointer.f90 $(cflags) $(lflags) -o bin/ex_pointer

ex_reshape: ex_reshape.f90
	$(comp) ex_reshape.f90 $(cflags) $(lflags) -o bin/ex_reshape

ex_setsubmat: ex_setsubmat.f90
	$(comp) ex_setsubmat.f90 $(cflags) $(lflags) -o bin/ex_setsubmat

read_a_text_file: read_a_text_file.f90
	$(comp) read_a_text_file.f90 $(cflags) $(lflags) -o bin/read_a_text_file

using_Interpreter_1: using_Interpreter_1.f90
	$(comp) using_Interpreter_1.f90 $(cflags) $(lflags) -o bin/using_Interpreter_1

using_Interpreter_2: using_Interpreter_2.f90
	$(comp) using_Interpreter_2.f90 $(cflags) $(lflags) -o bin/using_Interpreter_2

using_Interpreter_3: using_Interpreter_3.f90
	$(comp) using_Interpreter_3.f90 $(cflags) $(lflags) -o bin/using_Interpreter_3

using_Interpreter_4: using_Interpreter_4.f90
	$(comp) using_Interpreter_4.f90 $(cflags) $(lflags) -o bin/using_Interpreter_4

using_Interpreter_5: using_Interpreter_5.f90
	$(comp) using_Interpreter_5.f90 $(cflags) $(lflags) -o bin/using_Interpreter_5

using_Interpreter_6: using_Interpreter_6.f90
	$(comp) using_Interpreter_6.f90 $(cflags) $(lflags) -o bin/using_Interpreter_6

adding_userFunctions: adding_userFunctions.f90
#       if userfunc_m.mod  and libpk2 are in mylibs/ subdirectory:
	$(comp) adding_userFunctions.f90 -I mylibs $(cflags) -Lmylibs -lmypk2 -llapack -lblas -o bin/adding_userFunctions
################################################################################
# Cleaning:
#-------------------------------------------------------------------------------

clean:
	/bin/rm -f bin/* 
