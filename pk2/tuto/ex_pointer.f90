program ex_pointer
   use pk2mod_m
   implicit none
   type   (pk2_t) :: A
   integer        :: i, j

   example1: block
      integer(Ikind), pointer :: iscal, ivec(:), imat(:,:)

      ! Let A contains an integer random n x n matrix:
      A = randi ( imin=0_Ikind, imax=200_Ikind, n=3_Ikind, m=4_Ikind ) 
      call A % printMe ( msg = 'Let A = ', form = 'values' )

      ! 1) Pointer to A(1,1):
      call A % pointer ( iscal )
      print '(/,a,i0)','pointer to A(1,1): ',iscal  

      ! 2) Pointer to A(2,3):
      call A % pointer ( iscal, 2_Ikind, 3_Ikind )
      print '(/,a,i0)','pointer to A(2,3): ',iscal  
         
      ! 3) Pointer to A(:):
      call A % pointer ( ivec )
      print '(/,a,*(i0,1x))','pointer to A(:): ',ivec
   
      ! 4) Pointer to A(:,:):
      call A % pointer ( imat )
      print '(/,a)','pointer to A(:,:): '
      do i = 1, size(imat,1)
         print '(*(i3,1x))',(imat(i,j),j=1,size(imat,2))
      end do
      !...
   
      ! Don't forger to nullify pointers:
      nullify(iscal,ivec,imat)
      !...
   end block example1
   
   example2: block
      type(str_t), pointer :: sscal, svec(:), smat(:,:)

      A = reshape(['Fortran','C++    ','Pascal ','Rust   ','BASIC  ','ALGOL  '],[2,3])
      A = trim(A)
      call A % printMe ( msg = 'Let A = ', form = 'values' )

      ! 1) Pointer to A(1,1):
      call A % pointer ( sscal )
      print '(/,a,a)','pointer to A(1,1): ',sscal%str  

      ! 2) Pointer to A(2,3):
      call A % pointer ( sscal, 2_Ikind, 2_Ikind )
      print '(/,a,a)','pointer to A(2,2): ',sscal%str 
         
      ! 3) Pointer to A(:):
      call A % pointer ( svec )
      print '(/,a,*(a,1x))','pointer to A(:): ',(svec(i)%str,i=1,size(svec))
   
      ! 4) Pointer to A(:,:):
      call A % pointer ( smat )
      print '(/,a)','pointer to A(:,:): '
      do i = 1, size(smat,1)
         print '(*(a7,1x))',(smat(i,j)%str,j=1,size(smat,2))
      end do
      
      ! Modify iscal (the element A(2,3)):
      sscal = 'COBOL'
      call A % printMe ( msg = 'A(2,2) has changed: A = ', form = 'values' )
      print '(/,a,*(a,1x))','pointer to A(:): ',(svec(i)%str,i=1,size(svec))

      !...

      ! Don't forger to nullify pointers:
      nullify(sscal,svec,smat)
      !...  
   end block example2
   
end program ex_pointer

