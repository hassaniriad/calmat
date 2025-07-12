!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_mult_prepare.inc" for the module bk2. Common lines for subroutines:
!      . bk2_multIC
!      . bk2_multRC
!      . bk2_multCC
!---------------------------------------------------------------------------------------------

   n1 = size(A,1) ; m1 = size(A,2) ; n1m1 = n1*m1
   n2 = size(B,1) ; m2 = size(B,2) ; n2m2 = n2*m2
   
   err = 0
            
   if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      if ( .not. commute ) then      
         if ( m1 /= n2 ) then
            err = 1
         else
            nres = n1 ; mres = m2
         end if
      else
         if ( m2 /= n1 ) then
            err = 1
         else
            nres = n2 ; mres = m1
         end if
      end if
      if ( err == 1 ) then      
         call opflag%Set ( UERROR, HERE, 'Incompatible shapes for << * >>' )
         return
      end if
   else if ( n1m1 == 1 ) then
      nres = n2 ; mres = m2
   else
      nres = n1 ; mres = m1
   end if
   
   call bk2_reallocIfNeeded ( res, typres, nres, mres ) ; error_TraceNreturn(opflag, HERE)

      
!---------------------------------------------------------------------------------------------         

