! 
! This file was auto created by addfun from the user file myfun0.txt
! with the command:
! 
!      ../../bin/nagfor/addFun -i myfun0.txt
! 
MODULE userfunc_m
   use pk2_m

   implicit none

   !private
   !public :: userProcList, userProcDriver, s_userProcDriver


   interface s_userProcDriver
      module procedure usersubr_driver1, usersubr_driver2
   end interface

   interface userProcDriver
      module procedure userfunc_driver1, userfunc_driver2
   end interface

   integer(Ikind), parameter :: maxOptionalOutputs = 0


CONTAINS

!=============================================================================================
   SUBROUTINE userProcList ( nfunc, funcList, nbrOptOutputs )
!=============================================================================================
   integer(Ikind),                        intent(out) :: nfunc
   type   (str_t), optional, allocatable, intent(out) :: funcList(:)
   integer(Ikind), optional, allocatable, intent(out) :: nbrOptOutputs(:)
!---------------------------------------------------------------------------------------------

   nfunc = 0
   if ( present(funcList) ) then
      allocate(funcList(nfunc))
   end if

   if ( present(nbrOptOutputs) ) then
      allocate(nbrOptOutputs(nfunc))
   end if

   END SUBROUTINE userProcList


!=============================================================================================
   SUBROUTINE usersubr_driver1 ( fname, inputs, firstOut, optOuts, nOpt )
!=============================================================================================
   character(len=*),           intent(in    ) :: fname
   type     (pk2_t), target,   intent(in    ) :: inputs(:)
   type     (pk2_t)          , intent(in out) :: firstOut
   type     (pk2_t), optional, intent(in out) :: optOuts(:)
   integer  (Ikind), optional, intent(   out) :: nOpt
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "usersubr_driver1"
   type     (pk2_t)            :: r2(maxOptionalOutputs)
   integer  (Ikind)            :: i, n
!---------------------------------------------------------------------------------------------

   select case ( fname )
      case default
         call opflag%Set(UERROR,HERE,"Unknown user function << " // fname // " >>")
   end select

   END SUBROUTINE usersubr_driver1


!=============================================================================================
   SUBROUTINE usersubr_driver2 ( ifunc, inputs, firstOut, optOuts, nOpt )
!=============================================================================================
   integer(Ikind),           intent(in    ) :: ifunc
   type   (pk2_t), target,   intent(in    ) :: inputs(:)
   type   (pk2_t),           intent(in out) :: firstOut
   type   (pk2_t), optional, intent(in out) :: optOuts(:)
   integer(Ikind), optional, intent(   out) :: nOpt
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "usersubr_driver2"
   type     (pk2_t)            :: r2(maxOptionalOutputs)
   integer  (Ikind)            :: i, n
!---------------------------------------------------------------------------------------------

   select case ( ifunc )
      case default
         call opflag%Set(UERROR,HERE,"Unknown user function ID ("//util_intToChar(ifunc)//")")
   end select

   END SUBROUTINE usersubr_driver2


!=============================================================================================
   FUNCTION userfunc_driver1 ( fname, inputs, optOuts, nOpt ) result( firstOut )
!=============================================================================================
   character(len=*),           intent(in    ) :: fname
   type     (pk2_t), target,   intent(in    ) :: inputs(:)
   type     (pk2_t), optional, intent(in out) :: optOuts(:)
   integer  (Ikind), optional, intent(   out) :: nOpt
   type     (pk2_t)                           :: firstOut
!---------------------------------------------------------------------------------------------

   call usersubr_driver1 ( fname, inputs, firstOut, optOuts, nOpt )

   END FUNCTION userfunc_driver1


!=============================================================================================
   FUNCTION userfunc_driver2 ( ifunc, inputs, optOuts, nOpt ) result( firstOut )
!=============================================================================================
   integer  (Ikind),           intent(in    ) :: ifunc
   type     (pk2_t), target,   intent(in    ) :: inputs(:)
   type     (pk2_t), optional, intent(in out) :: optOuts(:)
   integer  (Ikind), optional, intent(   out) :: nOpt
   type     (pk2_t)                           :: firstOut
!---------------------------------------------------------------------------------------------

   call usersubr_driver2 ( ifunc, inputs, firstOut, optOuts, nOpt )

   END FUNCTION userfunc_driver2


END MODULE userfunc_m
