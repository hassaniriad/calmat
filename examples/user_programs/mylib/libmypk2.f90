! 
! This file was auto created by addfun from the user file data/myfun.txt
! with the command:
! 
!      ../../pk2/bin/nagfor/addFun -i data/myfun.txt -o mylib/libmypk2 -l dynamic -mdir mylib
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

   integer(Ikind), parameter :: maxOptionalOutputs = 1


CONTAINS

!=============================================================================================
   SUBROUTINE userProcList ( nfunc, funcList, nbrOptOutputs )
!=============================================================================================
   integer(Ikind),                        intent(out) :: nfunc
   type   (str_t), optional, allocatable, intent(out) :: funcList(:)
   integer(Ikind), optional, allocatable, intent(out) :: nbrOptOutputs(:)
!---------------------------------------------------------------------------------------------

   nfunc = 3
   if ( present(funcList) ) then
      allocate(funcList(nfunc))
      funcList(1)%str = "f"
      funcList(2)%str = "g"
      funcList(3)%str = "h"
   end if

   if ( present(nbrOptOutputs) ) then
      allocate(nbrOptOutputs(nfunc))
      nbrOptOutputs(1) = 0
      nbrOptOutputs(2) = 0
      nbrOptOutputs(3) = 0
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
      case ( "f" )
         if ( size(inputs) < 2 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function f needs 2 input arguments" // NLT // &
                 "Signature of this function: r = f(x,y) ")
            return
         end if
         call s_f( x=inputs(1), y=inputs(2), r=firstOut )

         if ( present(nOpt) ) nOpt = 0

      case ( "g" )
         if ( size(inputs) < 1 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function g needs 1 input arguments" // NLT // &
                 "Signature of this function: y = g(x) ")
            return
         end if
         call s_g( x=inputs(1), y=firstOut )

         if ( present(nOpt) ) nOpt = 0

      case ( "h" )
         if ( size(inputs) < 1 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function h needs 1 input arguments" // NLT // &
                 "Signature of this function: y = h(x) ")
            return
         end if
         call s_h( x=inputs(1), y=firstOut )

         if ( present(nOpt) ) nOpt = 0

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
      case ( 1 )
         if ( size(inputs) < 2 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function f needs 2 input arguments" // NLT // &
                 "Signature of this function: r = f(x,y) ")
            return
         end if
         call s_f( x=inputs(1), y=inputs(2), r=firstOut )

         if ( present(nOpt) ) nOpt = 0

      case ( 2 )
         if ( size(inputs) < 1 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function g needs 1 input arguments" // NLT // &
                 "Signature of this function: y = g(x) ")
            return
         end if
         call s_g( x=inputs(1), y=firstOut )

         if ( present(nOpt) ) nOpt = 0

      case ( 3 )
         if ( size(inputs) < 1 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function h needs 1 input arguments" // NLT // &
                 "Signature of this function: y = h(x) ")
            return
         end if
         call s_h( x=inputs(1), y=firstOut )

         if ( present(nOpt) ) nOpt = 0

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


!=============================================================================================
   SUBROUTINE user_s_f ( x, y, r )
!=============================================================================================
   real(Rkind) :: x, y, r
   r = x**2 + y**2

   END SUBROUTINE user_s_f


!=============================================================================================
   SUBROUTINE s_f ( x, y, r )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x, y
   type(pk2_t),         intent(in out) :: r
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "f"
   real(Rkind), pointer :: p_x => NULL()
   type(pk2_t) :: xtmp
   real(Rkind), pointer :: p_y => NULL()
   type(pk2_t) :: ytmp
   real(Rkind) :: r_r
!---------------------------------------------------------------------------------------------

   select case ( x%typ )
      case ( RTYP )
         call x%pointer ( p_x, stat = opflag )
      case ( ITYP )
         call pk2_assign ( xtmp ,x )
         call bk2_convertToRk2 ( xtmp%m ) ; xtmp%typ = RTYP
         call xtmp%pointer ( p_x, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "1-st argument of 'f' (x) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_x => NULL() ; return ; end if

   select case ( y%typ )
      case ( RTYP )
         call y%pointer ( p_y, stat = opflag )
      case ( ITYP )
         call pk2_assign ( ytmp ,y )
         call bk2_convertToRk2 ( ytmp%m ) ; ytmp%typ = RTYP
         call ytmp%pointer ( p_y, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "2-nd argument of 'f' (y) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_y => NULL() ; return ; end if

   call user_s_f ( p_x, p_y, r_r )

   call pk2_assign ( rhs = r_r, lhs = r )

   nullify ( p_x, p_y )

   END SUBROUTINE s_f


!=============================================================================================
   FUNCTION f ( x, y ) result( r )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x, y
   type(pk2_t) :: r
!---------------------------------------------------------------------------------------------

   call s_f ( x, y, r )

   END FUNCTION f


!=============================================================================================
   SUBROUTINE user_s_g ( x, y )
!=============================================================================================
   real(Rkind) :: x, y
   y = sin(x) - 1

   END SUBROUTINE user_s_g


!=============================================================================================
   SUBROUTINE s_g ( x, y )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x
   type(pk2_t),         intent(in out) :: y
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "g"
   real(Rkind), pointer :: p_x => NULL()
   type(pk2_t) :: xtmp
   real(Rkind) :: r_y
!---------------------------------------------------------------------------------------------

   select case ( x%typ )
      case ( RTYP )
         call x%pointer ( p_x, stat = opflag )
      case ( ITYP )
         call pk2_assign ( xtmp ,x )
         call bk2_convertToRk2 ( xtmp%m ) ; xtmp%typ = RTYP
         call xtmp%pointer ( p_x, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "1-st argument of 'g' (x) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_x => NULL() ; return ; end if

   call user_s_g ( p_x, r_y )

   call pk2_assign ( rhs = r_y, lhs = y )

   nullify ( p_x )

   END SUBROUTINE s_g


!=============================================================================================
   FUNCTION g ( x ) result( y )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x
   type(pk2_t) :: y
!---------------------------------------------------------------------------------------------

   call s_g ( x, y )

   END FUNCTION g


!=============================================================================================
   SUBROUTINE user_s_h ( x, y )
!=============================================================================================
   real(Rkind) :: x, y
   y = x**2 + 2*x + 1

   END SUBROUTINE user_s_h


!=============================================================================================
   SUBROUTINE s_h ( x, y )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x
   type(pk2_t),         intent(in out) :: y
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "h"
   real(Rkind), pointer :: p_x => NULL()
   type(pk2_t) :: xtmp
   real(Rkind) :: r_y
!---------------------------------------------------------------------------------------------

   select case ( x%typ )
      case ( RTYP )
         call x%pointer ( p_x, stat = opflag )
      case ( ITYP )
         call pk2_assign ( xtmp ,x )
         call bk2_convertToRk2 ( xtmp%m ) ; xtmp%typ = RTYP
         call xtmp%pointer ( p_x, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "1-st argument of 'h' (x) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_x => NULL() ; return ; end if

   call user_s_h ( p_x, r_y )

   call pk2_assign ( rhs = r_y, lhs = y )

   nullify ( p_x )

   END SUBROUTINE s_h


!=============================================================================================
   FUNCTION h ( x ) result( y )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x
   type(pk2_t) :: y
!---------------------------------------------------------------------------------------------

   call s_h ( x, y )

   END FUNCTION h


END MODULE userfunc_m
