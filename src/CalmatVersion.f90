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

   G_numversion = G_VERSIONID
   G_filedef    = G_FDEF
   
#ifdef __CDATE
   G_compilDate = __CDATE
#else
   G_compilDate = ""
#endif   

#ifdef __COPTS   
   G_compilOPts = __COPTS
#else
   G_compilOPts = ""
#endif     

   END SUBROUTINE CalmatVersion_versionID
      
END MODULE CalmatVersion_m   
