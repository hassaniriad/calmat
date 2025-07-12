program sample3_getAndSetByMoveAlloc
!
! Examples showing how to transfer by move allocation:
! - an intrinsic array into a calmat variable (setter)
! - a calmat variable into an intrinsic array (getter)
!
!---------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
   integer(Ikind), allocatable :: mat(:,:)
   integer                     :: i, j
!---------------------------------------------------------------------------------------------

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 1) move allocation from an intrinsic array to a calmat variable
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print '(/,a)','1) Move allocation from "mat" to the calmat variable "foo"'
   print '(a)'  ,'=========================================================='
   
   ! Create a calmat variable named "foo":
   call calmat ( exprIn = 'foo = [];', welcome = .false., dispRes = .false. )

   call calmat_display ( 'foo', msg ='"foo" is the calmat variable:' )
   
   mat = reshape([1,2,3,4,5,6],[2,3])

   print '(/,a)','and "mat" is the matrix:' 
   do i = 1, size(mat,1)
      print '(*(i0,1x))',(mat(i,j),j=1,size(mat,2))
   end do
   
   print '(/,a)', '==> Move allocation from "mat" to "foo":'
   
   call calmat_moveAlloc ( from = mat, to = 'foo' )
   
   call calmat_display ( 'foo', msg ='"foo" is now the calmat variable:' )

   print '(/,a,g0)','and mat is now unallocated: allocated(mat) = ',allocated(mat)
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 2) move allocation from a calmat variable to an intrinsic array
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print '(/,a)','2) Move allocation from the calmat variable "i" to "mat"'
   print '(a)'  ,'========================================================'

   call calmat ( exprIn = 'i = randi([0,9],[4,5]);' )

   call calmat_display ( 'i', msg ='"i" is the calmat variable:' )

   print '(/,a)', '==> Move allocation from "i" to "mat":'
   
   call calmat_moveAlloc ( from = 'i', to = mat )
   
   print '(/,a)','"mat" is now the matrix:'
   do i = 1, size(mat,1)
      print '(*(i0,1x))',(mat(i,j),j=1,size(mat,2))
   end do

   call calmat_display ( 'i', msg ='and "i" is now the calmat variable:' )
   
end program sample3_getAndSetByMoveAlloc
