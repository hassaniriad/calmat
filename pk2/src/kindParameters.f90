!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: kindParameters
!
! Description: 
!---------------------------------------------------------------------------------------------

MODULE kindParameters_m
!
!- Kind type parameters used to define some generic procedures: 
!
   use, intrinsic :: iso_fortran_env, only: i32 => int32 , i64 => int64 , &
                                            rSP => real32, rDP => real64, rQP => real128

   implicit none
   
   public
!
!- Selected kind type parameters for default integers and reals:
!
#ifdef SP
   integer, parameter :: RKIND = rSP
#elif DP
   integer, parameter :: RKIND = rDP
#elif QP
   integer, parameter :: RKIND = rQP
#else
   integer, parameter :: RKIND = rDP !< the default for RKIND
#endif

#ifdef I32
   integer, parameter :: IKIND = i32
#elif I64
   integer, parameter :: IKIND = i64
#else
   integer, parameter :: IKIND = i32 !< the default for IKIND
#endif


!   integer, parameter :: Ikind = i32, Rkind = rDP
   
   
!
!  Old:     
!
!- Kind type parameters used to define some generic procedures: 
!      
!  * For integers: 
!
!   integer, parameter :: i32 = SELECTED_INT_KIND(9)  ! 10 digits; 32 bits; 
!                                                     ! range: {-2^31,+2^31-1}
!
!   integer, parameter :: i64 = SELECTED_INT_KIND(18) ! 19 digit, 64 bits; 
!                                                     ! range: {-2^63,+2^63-1}
!   
!  * For reals and complexes:
!
!   integer, parameter :: rSP = SELECTED_REAL_KIND(6,37)    ! 6 digits; 32 bits;
!                                                           ! range: [1e-37, 1e+37-1]
!  
!   integer, parameter :: rDP = SELECTED_REAL_KIND(15,307)  ! 15 digits; 64 bits; 
!                                                           ! range: [1e-307 , 1e+307-1]
!  the following is not available for nagfor:                                                          
!   integer, parameter :: rQP = selected_real_kind(33,4931) ! 33 digits; 128 bits;
!                                                           ! range: [1e-4931, 1e+4931-1]
!  use this instead (double double):
!   integer, parameter :: rQP = selected_real_kind(31,291)
                                         
END MODULE kindParameters_m
