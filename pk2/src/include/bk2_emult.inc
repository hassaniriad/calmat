!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_emult.inc" for the module bk2. Common lines for subroutines:
!      . bk2_eMultII
!      . bk2_eMultIR
!      . bk2_eMultIC
!      . bk2_eMultRR
!      . bk2_eMultRC
!      . bk2_eMultCC
!---------------------------------------------------------------------------------------------
   
   n1m1 = n1*m1 ; n2m2 = n2*m2
         
   if (n1m1 /= 1 .and. n2m2 /= 1) then
      res%v(:,:) = A1 * A2
   else if (n1m1 == 1) then
      res%v(:,:) = A1(1,1) * A2
   else if (n2m2 == 1) then
      res%v(:,:) = A2(1,1) * A1
   end if 
!---------------------------------------------------------------------------------------------         
