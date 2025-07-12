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

   n1 = size(A1,1) ; m1 = size(A1,2) ; n1m1 = n1*m1
   n2 = size(A2,1) ; m2 = size(A2,2) ; n2m2 = n2*m2

   n = max(n1,n2) ; m = max(m1,m2) ; allocate(tmp(n,m), stat = err)
   
   if (err /= 0) then
      call opflag%set ( IERROR, HERE, 'Allocation failure for array tmp' )
      return
    end if  
    
    if (n1m1 /= 1 .and. n2m2 /= 1) then
       if (s) then
          tmp = A1 + A2
       else   
          tmp = A1 - A2
       end if   
    else if (n1m1 == 1) then
       if (s) then
          tmp = A1(1,1) + A2
       else
          tmp = A1(1,1) - A2
       end if   
    else if (n2m2 == 1) then
       if (s) then
          tmp = A1 + A2(1,1)
       else
          tmp = A1 - A2(1,1)
       end if   
    end if                  
!---------------------------------------------------------------------------------------------         
