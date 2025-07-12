program ex_copy
!---------------------------------------------------------------------------------------------
!  Example showing how to get a copy of the container of a pk2 variable into an intrinsic
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
      
   integer(Ikind), parameter   :: i1 = 0, i2 = 10, n = 4
   integer(Ikind), allocatable :: iscal
   real   (Rkind), allocatable :: rmat(:,:), rvec(:)
   complex(Rkind), allocatable :: cvec(:)

   type   (pk2_t)              :: A, eigA
!---------------------------------------------------------------------------------------------

   ! Let A contains an integer random n x n matrix:
   A = randi ( imin=i1, imax=i2, n=n, m=n ) 
   A = (A**2) - (3*A) + 1 + eye(n,n) 
         
   call A % get ( rmat ) ! the cont. of A as a real rank 2 array

   call A % get ( rvec ) ! the cont. of A as a real rank 1 array

   call A % get ( 2_Ikind, 3_Ikind, iscal )  ! copy the element A(2,3) into iscal

   call s_eig ( A, eigA )  ! compute the eigenvalues of A
       
   if ( eigA%typ == RTYP ) then
      print '(/,a)','eigenvalues of A are reals'
      call eigA % get ( rvec )
      ! ... 
   elseif ( eigA%typ == CTYP ) then
      print '(/,a)','eigenvalues of A are complexes:'
      call eigA % get ( cvec )
      ! ...
   end if
   !...
end program ex_copy
