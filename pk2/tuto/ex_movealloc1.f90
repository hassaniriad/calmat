program ex_movealloc1
!---------------------------------------------------------------------------------------------
!  Usage (1/2) of pk2_movealloc:
!  - from pk2_t to pk2_t
!  - from intrinsics to pk2_t
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   real(Rkind), allocatable :: rmat(:,:)
   type(pk2_t)              :: A, B
!---------------------------------------------------------------------------------------------

   A%name = 'myA' ; B%name = 'myB'

   ! Let rmat be the matrix:
   rmat = reshape([1,2,3,4,5,6],[3,2])
   
   ! and let A contains an integer random n x n matrix:
   call s_randi ( imin=1_Ikind, imax=20_Ikind, n=3_Ikind, m=3_Ikind, res=A )
   call A%printMe ( msg = 'The initial A variable:')

   ! 1) Move allocation between two pk2_t:
   call pk2_moveAlloc ( from = A, to = B, movename = .false. )
      
   call A%printMe ( msg = 'The A variable after moveAlloc(A,B):')
   call B%printMe ( msg = 'and the B variable: ')
      
   ! 2) Move allocation from an intrisic to a pk2_t:
   A%name = 'myNewA'

   call pk2_moveAlloc ( from = rmat, to = A )
   
   call A%printMe ( msg = 'The A variable after moveAlloc(rmat,A):')
   print '(/,a,g0)','Allocated(rmat): ',allocated(rmat)

end program ex_movealloc1
