!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite Cote d'Azur
!
! include file "pk2_getMatGeneric.inc" for the module pk2. Common lines for subroutines:
!      . pk2_Get?stat
! with ? = I, R, C, L, S
!---------------------------------------------------------------------------------------------

   if ( opflag > IZERO ) return !!opflag = err_t ()

   select rank ( lhs )
      rank(0)
         call rhs%GetMatij ( IONE, IONE, lhs, opflag )
      rank(1)
         call rhs%GetMatPacked ( lhs, opflag )
      rank(2)
         call rhs%GetMat ( lhs, opflag )
      rank default
         call opflag%set ( msg = "Unexpected rank", where = HERE, stat = UERROR ) 
         return
   end select  
