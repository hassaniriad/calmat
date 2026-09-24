!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Module CalmatVersion
!
! Initialises all that depends on the version
!
! Author: R. Hassani, Universite Cote d'Azur
!
! Date: 09/22
!---------------------------------------------------------------------------------------------

MODULE CalmatVersion_m
         
   implicit none
   
   private
   public :: CalmatVersion_versionID

CONTAINS

!=============================================================================================
   SUBROUTINE CalmatVersion_versionID 
!=============================================================================================
   use CalmatGlobal_m, only: G_VERSIONID, G_FDEF, &
                             G_numversion, G_filedef, G_compilDate, G_compilOPts
!---------------------------------------------------------------------------------------------

   integer :: n
   
   G_numversion = G_VERSIONID
   G_filedef    = G_FDEF
   
#ifdef __CDATE
   G_compilDate = trim(adjustl(__CDATE))
   n = len(G_compilDate)
   if ( G_compilDate(1:1) == '/' .and. G_compilDate(n:n) == '/' ) &
      G_compilDate = G_compilDate(2:n-1)
#else
   G_compilDate = ""
#endif   

#ifdef __COPTS   
   G_compilOPts = trim(adjustl(__COPTS))
   n = len(G_compilOPts)
   if ( G_compilOPts(1:1) == '/' .and. G_compilOPts(n:n) == '/' ) &
      G_compilOPts = G_compilOPts(2:n-1)
#else
   G_compilOPts = ""
#endif     

   END SUBROUTINE CalmatVersion_versionID
      
END MODULE CalmatVersion_m   
