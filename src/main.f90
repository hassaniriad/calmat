!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite Cote d'Azur
!
! Date: 11/18
! Modified: 03/19, 05/20
!---------------------------------------------------------------------------------------------

program main

   use Calmat_m
   
   implicit none

   type(err_t) :: stat
!
!- proceed:
!
   call Calmat ( stat = stat )

end program main
