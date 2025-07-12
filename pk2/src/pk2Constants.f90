!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: constants
!
! Description: 
!---------------------------------------------------------------------------------------------

MODULE pk2Constants_m

   use kindParameters_m
!
!- Standard input, output and error units :
!   
   use, intrinsic :: iso_fortran_env, only:  input_unit, output_unit, error_unit
   
   integer(Ikind), parameter :: STDIN  = int(input_unit ,Ikind), &
                                STDOUT = int(output_unit,Ikind), &
                                STDERR = int(error_unit ,Ikind)
!
!- Codes for internal error (IERROR), user's error (UERROR) and read error (RERROR) 
!  Codes for warning (WARNING) and end-of-file (EOF)
!
   integer(Ikind), parameter :: IERROR  = 1_Ikind, UERROR = 2_Ikind, RERROR = 3_Ikind, &
                                WARNING =-1_Ikind, EOF = -2_Ikind                              
!
!- Some usefull constants:
!
   integer(Ikind), parameter :: EMPTY = 0, ITYP = 1, RTYP = 2, CTYP = 3, LTYP = 4, STYP = 5
   
   integer(Ikind), parameter :: IZERO = 0_Ikind                 , IONE = 1_Ikind
   real   (Rkind), parameter :: RZERO = 0.0_Rkind               , RONE = 1.0_Rkind
   complex(Rkind), parameter :: CZERO = cmplx(RZERO,RZERO,Rkind), CONE = cmplx(RONE,RZERO,Rkind)
   logical       , parameter :: LZERO = .false.                 , LONE = .true.
   
   integer(Ikind), parameter :: IMAX = huge(1_Ikind)

   real   (Rkind), parameter :: EPS = epsilon(RONE), RMIN  = tiny(RONE), RMAX = huge(RONE), &
                                PI  = acos(-RONE)  , NEPER = exp(RONE) , HALF = 0.5e0_Rkind

   complex(Rkind), parameter :: CIMAG = cmplx(RZERO,RONE,Rkind)
   
   integer(Ikind), parameter :: MDSTR = 256, LGSTR = 3000
   
   logical       , parameter :: ON = .true., OFF = .false.
!
!- Ascii code for a new line:
!   
   character(len=*), parameter :: NL = char(10), NLT = char(10)//'    '

   character(len=*), parameter :: HELPCOLOR = achar(27) // '[1;104m'
   
END MODULE pk2Constants_m
