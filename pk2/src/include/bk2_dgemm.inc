!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_mult.inc" for the module bk2. Common lines for subroutines:
!      . bk2_multII
!      . bk2_multIR
!      . bk2_multIC
!      . bk2_multRR
!      . bk2_multRC
!      . bk2_multCC
!---------------------------------------------------------------------------------------------
         if (n1m1 /= 1 .and. n2m2 /= 1) then
            allocate(A(n1,m1), B(n2,m2), stat = err) 
            if (err == 0) then
               one = 1.0d0 ; zero = 0.0d0
               A(:,:) = real(A1(:,:),kind=Rprec) ; B(:,:) = real(A2(:,:),kind=Rprec)
               if (.not. commute) then
                  ! compute C = A*B:
                  allocate(C(n1,m2), stat=err)
                  if (err == 0) then
                     call LapackInterface_gemm ("N", "N", n1, m2, m1, one, A, n1, B, n2, zero, C, n1)
                     res%nrow = n1 ; res%ncol = m2 ; allocate(res%v(n1,m2), stat = err) 
                  end if
               else
                  ! compute C = B*A:
                  allocate(C(n2,m1), stat=err)
                  if (err == 0) then
                     call LapackInterface_gemm ("N", "N", n2, m1, m2, one, B, n2, A, n1, zero, C, n2)
                     res%nrow = n2 ; res%ncol = m1 ; allocate(res%v(n2,m1), stat = err)
                  end if
               end if
            end if                  
             
         else if (n1m1 == 1) then
            ! compute A1(1,1) * A2
            res%nrow = n2 ; res%ncol = m2 ; allocate(res%v(n2,m2), stat=err)  
            if (err == 0) res%v(:,:) = A1(1,1) * A2(:,:)
            
         else if (n2m2 == 1) then
            ! compute A2(1,1) * A1
            res%nrow = n1 ; res%ncol = m1 ; allocate(res%v(n1,m1), stat=err)  
            if (err == 0) res%v(:,:) = A2(1,1) * A1(:,:)
         
         end if     
!---------------------------------------------------------------------------------------------         

