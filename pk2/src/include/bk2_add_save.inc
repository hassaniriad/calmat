!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
! 
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_add.inc" for the module bk2. Common lines for subroutines:
!      . bk2_addII
!      . bk2_addIR
!      . bk2_addIC
!      . bk2_addRR
!      . bk2_addRC
!      . bk2_addCC
!---------------------------------------------------------------------------------------------
         n = max(n1,n2) ; m = max(m1,m2) ; allocate(res%v(n,m), stat = err)
         if (err /= 0) then
            call opflag%set ( stat = IERROR, abort = ErrAbort, &
                               msg = HERE //' Allocation failure for array res%v' )
            return
         end if  
         res%nrow = n ; res%ncol = m
         if (n1m1 /= 1 .and. n2m2 /= 1) then
            if (s) then
               res%v = A1 + A2
            else   
               res%v = A1 - A2
            end if   
         else if (n1m1 == 1) then
            if (s) then
               res%v = A1(1,1) + A2
            else
               res%v = A1(1,1) - A2
            end if   
         else if (n2m2 == 1) then
            if (s) then
               res%v = A1 + A2(1,1)
            else
               res%v = A1 - A2(1,1)
            end if   
         end if                  
!---------------------------------------------------------------------------------------------         
