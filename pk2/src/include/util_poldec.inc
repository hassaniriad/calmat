!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
! 
! Author: R. Hassani, Universite Cote d'Azur
!
! include file "util_poldec.inc" for the module util_Other. Common lines for subroutines:
!      . util_poldecI
!      . util_poldecR
!---------------------------------------------------------------------------------------------

   interface transp
      procedure transR, transC
   end interface
   
   n = size(F,1) ; m = size(F,2)
   if ( n /= m ) then
      call stat%set (UERROR, HERE, 'The matrix must be square' )          
      return
   end if     
   
   if ( present(R) ) then
      if ( size(R,1) /= n .or. size(R,2) /= n ) then
         call stat%set ( UERROR, HERE, 'The matrix R must be of shape'// &
                         util_intToChar(n)//'x'//util_intToChar(n) )          
         return
      end if
   end if

   if ( present(U) ) then
      if ( size(U,1) /= n .or. size(U,2) /=n ) then
         call stat%set ( UERROR, HERE, 'The matrix U must be of shape'// &
                         util_intToChar(n)//'x'//util_intToChar(n) )          
         return
      end if
   end if

   allocate(SS(n), UU(n,n), VVt(n,n), stat = err)

   if ( err /= 0 ) then
      call stat%set ( UERROR, HERE, 'Allocation failure for SS, UU or VV' )          
      return
   end if 
!
!- Compute the SVD of F: F = UU * SS * VVt
!      
   FF = F
   call util_Svd ( A=FF, S=SS, stat=stat, saveA=.false., U=UU, VT=VVt )    
   error_TraceNreturn(stat, HERE)
   
   if ( present(R) ) then
!
!-    R = UU * VVt:
!   
      R = matmul(UU,VVt)
      
      if ( present(U) ) then
!
!-       U = Rt * F:
!
         U = matmul(transp(R),F)

         return
      end if
   end if

   if ( present(U) ) then  
!
!-    Compute S * VVt:
!
      do j = 1, n
         do i = 1, n
            U(i,j) = SS(i) * VVt(i,j)
         end do
      end do
!
!-    U = VV * S * VVt:
!      
      U = matmul(transp(VVt),U)
   end if
   
   contains

      function transR ( a ) result( b )
      real(Rkind), intent(in) :: a(:,:)
      real(Rkind)             :: b(size(a,1),size(a,2))
      b = transpose(a)
      end function transR 

      function transC ( a ) result( b )
      complex(Rkind), intent(in) :: a(:,:)
      complex(Rkind)             :: b(size(a,1),size(a,2))
      b = conjg(transpose(a))
      end function transC