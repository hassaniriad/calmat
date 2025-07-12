program ex_movealloc2
!---------------------------------------------------------------------------------------------
!  Usage (2/2) of pk2_movealloc:
!  - from pk2_t to intrinsics
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   integer(Ikind), allocatable :: imat(:,:)
   logical       , allocatable :: lmat(:,:)
   type   (pk2_t)              :: A, B
!---------------------------------------------------------------------------------------------

   ! Let A and B contain an integer random n x n matrix:
   A = randi ( imin=1_Ikind, imax=20_Ikind, n=3_Ikind, m=3_Ikind )
   B = A

   ! 1) Move allocation from a pk2_t to an intrinsic:
   call pk2_moveAlloc ( from = A, to = imat )
            
   ! 2) Move allocation with a type mismatch (error):
   print '(/,a,/)','Intentional error'
   call pk2_moveAlloc ( from = B, to = lmat )
   
end program ex_movealloc2
