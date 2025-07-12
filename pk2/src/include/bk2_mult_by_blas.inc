!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_mult_by_blas.inc" for the module bk2. Common lines for subroutines:
!      . bk2_multIR
!      . bk2_multIC
!      . bk2_multRR
!      . bk2_multRC
!      . bk2_multCC
!---------------------------------------------------------------------------------------------

      if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      
         if ( .not. commute) then
            ! compute A*B:
            call LapackInterface_gemm ("N","N", n1,m2,m1, one, A, n1, B, n2, zero, res%v, n1)
         else
            ! compute B*A:
            call LapackInterface_gemm ("N","N", n2,m1,m2, one, B, n2, A, n1, zero, res%v, n2)
         end if
             
      else if (n1m1 == 1) then
         ! compute A(1,1) * B:
         res%v(:,:) = A(1,1) * B
       
      else
      ! compute B(1,1) * A:
         res%v(:,:) = B(1,1) * A
      end if
      
!---------------------------------------------------------------------------------------------         

