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

   n1m1 = n1*m1 ; n2m2 = n2*m2

   select case ( s )
      case ( .true. )
         if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
            res%v = A1 + A2
         else if ( n1m1 == 1 ) then
            res%v = A1(1,1) + A2
         else
            res%v = A1 + A2(1,1)
         end if   
      case ( .false. )
         if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
            res%v = A1 - A2
         else if ( n1m1 == 1 ) then
            res%v = A1(1,1) - A2
         else
            res%v = A1 - A2(1,1)
         end if
   end select                       
!---------------------------------------------------------------------------------------------         
