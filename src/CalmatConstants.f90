!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Module CalmatConstants
!
! This module contains some global constants (their names start with a 'G_')
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Date: 11/18
! Modified: 03/19
!---------------------------------------------------------------------------------------------

MODULE CalmatConstants_m
      
   use, intrinsic :: ieee_arithmetic
   
   use kindParameters_m, only: Ikind, Rkind
   use pk2Constants_m, only: IERROR, UERROR, WARNING
!
!- Version ID (release(year.month).version):
!
   character(len=*), parameter :: G_VERSIONID = '2020.7.0' 
!
!- Name of the user default file:
!   
   character(len=*), parameter :: G_FDEF = '.calmatdefaults_v1' ! name of the user default file
!
!- Codes for Internal errors (G_IERROR), User's errors (G_UERROR) and warnings (G_WARNING):
!
   integer(Ikind), parameter :: G_IERROR = IERROR, G_UERROR = UERROR, G_WARNING = WARNING 
!
!- Codes for variable status (default: 0 = free)
!
   integer(Ikind), parameter :: G_FREE          = 0, &
                                G_USED          = 1, &
                                G_PROTECTED     = 2, &
                                G_USERPROTECTED = 3, &
                                G_TEMPORARY     = 4, &
                                G_HIDDEN        = 5, &
                                G_DELETE        =-1 
!
!- Codes for block construct (for and if):
!
   integer(Ikind), parameter :: G_FORBLOCK = 1, G_IFBLOCK = 2, G_EXECBLOCK = 3
!
!- Codes for the task manager:
!
   integer(Ikind), parameter :: G_STOP = 0, G_CONTINUE = 1, G_ERR = 2 
!
!- Other constants:
!   
   logical       , parameter :: G_ON        = .true., G_OFF   = .false., &
                                G_SIGNALING = .true., G_QUIET = .false.

END MODULE CalmatConstants_m

   
