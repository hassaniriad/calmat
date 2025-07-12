!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! include file "bk2_mult_by_matmul.inc" for the module bk2. Common lines for subroutines:
!      . bk2_multII0
!      . bk2_multIR0
!      . bk2_multIC0
!      . bk2_multRR0
!      . bk2_multRC0
!      . bk2_multCC0
!---------------------------------------------------------------------------------------------
 
   if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      if (.not. commute) then
         ! Compute A * B
         res%v(:,:) = matmul(A,B) 
      else
         ! Compute B * A
         res%v(:,:) = matmul(B,A) 
      end if                 
   else if (n1m1 == 1) then
      ! Compute A(1,1) * B
      res%v(:,:) = A(1,1) * B
   else if (n2m2 == 1) then
      ! Compute B(1,1) * A
      res%v(:,:) = B(1,1) * A
   end if                            
!---------------------------------------------------------------------------------------------         

