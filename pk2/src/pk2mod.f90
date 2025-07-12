!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: pk2mod
!
! Description: 
! wrapper for the whole pk2 package
!---------------------------------------------------------------------------------------------

MODULE pk2mod_m
!
!- global variables:
!
   use kindParameters_m
   use pk2Constants_m
!   
!- utilities:
!   
   use str_m
   use err_m
   use util_Strings_m
   use util_Other_m
   use signalHandler_m
   use ansiColor_m
!
!- main pk2 modules:
!  
   use bk2_m  
   use pk2_m 
   use pk2f_m

   use pk2obj_m
   !use pk2ptr
!
!- an interpreter of litteral expression involving pk2 variables:
!   
   use pk2Interpreter_m
!
!- Third party:   
!
   use dispmodule ; use disp_i8mod ! Printing arrays (by Kristj n JÛnasson)
   
END MODULE pk2mod_m
