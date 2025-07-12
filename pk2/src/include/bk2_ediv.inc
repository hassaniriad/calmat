!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_ediv.inc" for the module bk2. Common lines for subroutines:
!      . bk2_edivIR
!      . bk2_edivIC
!      . bk2_edivRR
!      . bk2_edivRC
!      . bk2_edivCC
!---------------------------------------------------------------------------------------------

   n1m1 = n1*m1 ; n2m2 = n2*m2
         
   select case ( commute )
      case ( .false. )
         if (n1m1 /= 1 .and. n2m2 /= 1) then
            res%v(:,:) = A1 / A2
         else if (n1m1 == 1) then 
            res%v(:,:) = A1(1,1) / A2
         else if (n2m2 == 1) then
            res%v(:,:) = A1 / A2(1,1)
         end if
      case ( .true. )
         if (n1m1 /= 1 .and. n2m2 /= 1) then
            res%v(:,:) = A2 / A1
         else if (n1m1 == 1) then 
            res%v(:,:) = A2 / A1(1,1)
         else if (n2m2 == 1) then
            res%v(:,:) = A2(1,1) / A1
         end if
   end select    
!---------------------------------------------------------------------------------------------         
