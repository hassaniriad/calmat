program sample2_getAndSetByCopy
!
! Examples showing how to use "calmat_copy" for copying:
! - an intrinsic array into a calmat variable (setter)
! - a calmat variable into an intrinsic array (getter)
!
!---------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
   integer(Ikind), allocatable :: mat(:,:), vec(:), scal
   integer                     :: i, j
!---------------------------------------------------------------------------------------------

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 0a) copy an intrinsic rank-0 array (scalar) to a calmat variable
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print '(/,a)','0a) Copy "scal" into the calmat variable "foo"'
   print '(a)'  ,'=============================================='

   ! Create a calmat variable named "foo":
   call calmat ( exprIn = 'foo = [];', welcome = .false., dispRes = .false. )
   
   scal = 1

   print '(/,a)','"scal" is the scalar:' 
   print '(g0)',scal
   
   call calmat_display ( 'foo', form = 'values', msg = 'and "foo" is the calmat variable: ' )
 
   print '(/,a)', '==> Copy "scal" into "foo":'
  
   call calmat_copy ( from = scal, to = 'foo' )
   
   call calmat_display ( 'foo', form = 'values', msg = '"foo" is now the calmat variable: ' )
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 0b) copy a calmat variable into an intrinsic rank-0 array
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print '(/,a)','0b) Copy the calmat variable "i" into "scal"'
   print '(a)'  ,'============================================'

   call calmat ( exprIn = 'i = randi([0,9],[4,5])' )

   call calmat_display ( 'i', form = 'values', msg = '"i" is the calmat variable: ' )

   print '(/,a)', '==> Copy "i" into "scal":'
   
   call calmat_copy ( from = 'i', to = scal )
   
   print '(/,a)','"scal" is now the scalar (the 1st element of "i"):' 
   print '(g0)',scal
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 1a) copy an intrinsic rank-1 array to a calmat variable
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print '(/,a)','1a) Copy "vec" into the calmat variable "foo"'
   print '(a)'  ,'============================================='

   ! Create a calmat variable named "foo":
   call calmat ( exprIn = 'foo = [];', welcome = .false., dispRes = .false. )
   
   vec = [1,2,3,4,5,6]

   print '(/,a)','"vec" is the vector:' 
   print '(*(g0,1x))',(vec(i),i=1,size(vec))
   
   call calmat_display ( 'foo', form = 'values', msg = 'and "foo" is the calmat variable: ' )
 
   print '(/,a)', '==> Copy "vec" into "foo":'
  
   call calmat_copy ( from = vec, to = 'foo' )
   
   call calmat_display ( 'foo', form = 'values', msg = '"foo" is now the calmat variable: ' )
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 1b) copy a calmat variable into an intrinsic rank-1 array
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print '(/,a)','1b) Copy the calmat variable "i" into "vec"'
   print '(a)'  ,'==========================================='

   call calmat ( exprIn = 'i = randi([0,9],[4,5])' )

   call calmat_display ( 'i', form = 'values', msg = '"i" is the calmat variable: ' )

   print '(/,a)', '==> Copy "i" into "vec":'
   
   call calmat_copy ( from = 'i', to = vec )
   
   print '(/,a)','"vec" is now the vector ("i" in column-major order):' 
   print '(*(g0,1x))',(vec(i),i=1,size(vec))
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 2a) copy an intrinsic rank-2 array to a calmat variable
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print '(/,a)','2a) Copy "mat" into the calmat variable "foo"'
   print '(a)'  ,'============================================='

   ! Create a calmat variable named "foo":
   call calmat ( exprIn = 'foo = [];', welcome = .false., dispRes = .false. )
   
   mat = reshape([1,2,3,4,5,6],[2,3])

   print '(/,a)','"mat" is the matrix:' 
   do i = 1, size(mat,1)
      print '(*(g0,1x))',(mat(i,j),j=1,size(mat,2))
   end do
   
   call calmat_display ( 'foo', form = 'values', msg = 'and "foo" is the calmat variable: ' )
 
   print '(/,a)', '==> Copy "mat" into "foo":'
  
   call calmat_copy ( from = mat, to = 'foo' )
   
   call calmat_display ( 'foo', form = 'values', msg = '"foo" is now the calmat variable: ' )
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 2b) copy a calmat variable into an intrinsic rank-2 array
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   print '(/,a)','2b) Copy the calmat variable "i" into "mat"'
   print '(a)'  ,'==========================================='

   call calmat ( exprIn = 'i = randi([0,9],[4,5])' )

   call calmat_display ( 'i', form = 'values', msg = '"i" is the calmat variable: ' )

   print '(/,a)', '==> Copy "i" into "mat":'
   
   call calmat_copy ( from = 'i', to = mat )
   
   print '(/,a)','"mat" is now the matrix:' 
   do i = 1, size(mat,1)
      print '(*(g0,1x))',(mat(i,j),j=1,size(mat,2))
   end do
   
end program sample2_getAndSetByCopy
