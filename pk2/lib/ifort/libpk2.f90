! 
! This file was auto created by addfun from the user file myfun.txt
! with the command:
! 
!      ../bin/ifort/addFun -i myfun.txt -o ../lib/ifort/libpk2 -mdir ../mod/ifort
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
 
   integer(Ikind), parameter :: maxOptionalOutputs = 4
 
 
CONTAINS
 
!=============================================================================================
   SUBROUTINE userProcList ( nfunc, funcList, nbrOptOutputs )
!=============================================================================================
   integer(Ikind),                        intent(out) :: nfunc
   type   (str_t), optional, allocatable, intent(out) :: funcList(:)
   integer(Ikind), optional, allocatable, intent(out) :: nbrOptOutputs(:)
!---------------------------------------------------------------------------------------------
 
   nfunc = 5
   if ( present(funcList) ) then
      allocate(funcList(nfunc))
      funcList(1)%str = "hello"
      funcList(2)%str = "foo0"
      funcList(3)%str = "foo1"
      funcList(4)%str = "foo2"
      funcList(5)%str = "solve3x3"
   end if
 
   if ( present(nbrOptOutputs) ) then
      allocate(nbrOptOutputs(nfunc))
      nbrOptOutputs(1) = -1
      nbrOptOutputs(2) = 0
      nbrOptOutputs(3) = 0
      nbrOptOutputs(4) = 1
      nbrOptOutputs(5) = 3
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
      case ( "hello" )
         if ( size(inputs) < 0 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function hello needs 0 input arguments" // NLT // &
                 "Signature of this function: hello ")
            return
         end if
         call s_hello
 
         if ( present(nOpt) ) nOpt = 0
 
      case ( "foo0" )
         if ( size(inputs) < 1 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function foo0 needs 1 input arguments" // NLT // &
                 "Signature of this function: y = foo0(x) ")
            return
         end if
         call s_foo0( x=inputs(1), y=firstOut )
 
         if ( present(nOpt) ) nOpt = 0
 
      case ( "foo1" )
         if ( size(inputs) < 1 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function foo1 needs 1 input arguments" // NLT // &
                 "Signature of this function: y = foo1(x) ")
            return
         end if
         call s_foo1( x=inputs(1), y=firstOut )
 
         if ( present(nOpt) ) nOpt = 0
 
      case ( "foo2" )
         if ( size(inputs) < 3 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function foo2 needs 3 input arguments" // NLT // &
                 "Signature of this function: [ x, M ] = foo2 ( vec, b, c ) ")
            return
         end if
         call s_foo2( vec=inputs(1), b=inputs(2), c=inputs(3), x=firstOut, M=r2(1) )
 
         if ( present(optOuts) ) then
 
            if ( size(optOuts) < 1 ) then
               call opflag%set ( UERROR, HERE, msg = & 
                    "Size of the optional outputs (optOuts array) must be at least 1" )
               return
            end if
 
            call pk2_movealloc(from=r2(1), to=optOuts(1))
            if ( present(nOpt) ) nOpt = 1
 
         else
            if ( present(nOpt) ) nOpt = 0
         end if
 
      case ( "solve3x3" )
         if ( size(inputs) < 2 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function solve3x3 needs 2 input arguments" // NLT // &
                 "Signature of this function: [ regular, x, detA, invA ] = solve3x3 ( A, b ) ")
            return
         end if
         call s_solve3x3( A=inputs(1), b=inputs(2), regular=firstOut, x=r2(1), detA=r2(2), invA=r2(3) )
 
         if ( present(optOuts) ) then
 
            if ( size(optOuts) < 1 ) then
               call opflag%set ( UERROR, HERE, msg = & 
                    "Size of the optional outputs (optOuts array) must be at least 1" )
               return
            end if
 
            n = min(3,size(optOuts))
            do i = 1, n
                call pk2_movealloc(from=r2(i), to=optOuts(i))
            end do
            if ( present(nOpt) ) nOpt = n
 
         else
            if ( present(nOpt) ) nOpt = 0
         end if
 
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
         if ( size(inputs) < 0 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function hello needs 0 input arguments" // NLT // &
                 "Signature of this function: hello ")
            return
         end if
         call s_hello
 
         if ( present(nOpt) ) nOpt = 0
 
      case ( 2 )
         if ( size(inputs) < 1 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function foo0 needs 1 input arguments" // NLT // &
                 "Signature of this function: y = foo0(x) ")
            return
         end if
         call s_foo0( x=inputs(1), y=firstOut )
 
         if ( present(nOpt) ) nOpt = 0
 
      case ( 3 )
         if ( size(inputs) < 1 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function foo1 needs 1 input arguments" // NLT // &
                 "Signature of this function: y = foo1(x) ")
            return
         end if
         call s_foo1( x=inputs(1), y=firstOut )
 
         if ( present(nOpt) ) nOpt = 0
 
      case ( 4 )
         if ( size(inputs) < 3 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function foo2 needs 3 input arguments" // NLT // &
                 "Signature of this function: [ x, M ] = foo2 ( vec, b, c ) ")
            return
         end if
         call s_foo2( vec=inputs(1), b=inputs(2), c=inputs(3), x=firstOut, M=r2(1) )
 
         if ( present(optOuts) ) then
 
            if ( size(optOuts) < 1 ) then
               call opflag%set ( UERROR, HERE, msg = & 
                    "Size of the optional outputs (optOuts array) must be at least 1" )
               return
            end if
 
            call pk2_movealloc(from=r2(1), to=optOuts(1))
            if ( present(nOpt) ) nOpt = 1
 
         else
            if ( present(nOpt) ) nOpt = 0
         end if
 
      case ( 5 )
         if ( size(inputs) < 2 ) then
            call opflag%set ( UERROR, HERE, &
                 "The function solve3x3 needs 2 input arguments" // NLT // &
                 "Signature of this function: [ regular, x, detA, invA ] = solve3x3 ( A, b ) ")
            return
         end if
         call s_solve3x3( A=inputs(1), b=inputs(2), regular=firstOut, x=r2(1), detA=r2(2), invA=r2(3) )
 
         if ( present(optOuts) ) then
 
            if ( size(optOuts) < 1 ) then
               call opflag%set ( UERROR, HERE, msg = & 
                    "Size of the optional outputs (optOuts array) must be at least 1" )
               return
            end if
 
            n = min(3,size(optOuts))
            do i = 1, n
                call pk2_movealloc(from=r2(i), to=optOuts(i))
            end do
            if ( present(nOpt) ) nOpt = n
 
         else
            if ( present(nOpt) ) nOpt = 0
         end if
 
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
   SUBROUTINE user_s_hello
!=============================================================================================
   print*,'hello world!'

   END SUBROUTINE user_s_hello


!=============================================================================================
   SUBROUTINE s_hello
!=============================================================================================

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "hello"
!---------------------------------------------------------------------------------------------

   call user_s_hello (  )

   END SUBROUTINE s_hello


!=============================================================================================
   SUBROUTINE user_s_foo0 ( x, y )
!=============================================================================================
   real(Rkind) :: x, y
   y = x**2 + 2*x + 1

   END SUBROUTINE user_s_foo0


!=============================================================================================
   SUBROUTINE s_foo0 ( x, y )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x
   type(pk2_t),         intent(in out) :: y
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "foo0"
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
         call opflag%set ( UERROR,HERE, "1-st argument of 'foo0' (x) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_x => NULL() ; return ; end if

   call user_s_foo0 ( p_x, r_y )

   call pk2_assign ( rhs = r_y, lhs = y )

   nullify ( p_x )

   END SUBROUTINE s_foo0


!=============================================================================================
   FUNCTION foo0 ( x ) result( y )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x
   type(pk2_t) :: y
!---------------------------------------------------------------------------------------------

   call s_foo0 ( x, y )

   END FUNCTION foo0


!=============================================================================================
   SUBROUTINE user_s_foo1 ( x, y )
!=============================================================================================
   real(Rkind) :: x(:)
   real(Rkind), allocatable :: y(:)
   if ( allocated(y) ) then
      if ( size(y) /= size(x) ) deallocate(y)
   end if
   if ( .not. allocated(y) ) allocate(y, mold=x)
   y = (x-1)*(x-3)

   END SUBROUTINE user_s_foo1


!=============================================================================================
   SUBROUTINE s_foo1 ( x, y )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x
   type(pk2_t),         intent(in out) :: y
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "foo1"
   real(Rkind), pointer :: p_x(:) => NULL()
   type(pk2_t) :: xtmp
   real(Rkind), allocatable, save :: r_y(:)
!---------------------------------------------------------------------------------------------

   select case ( x%typ )
      case ( RTYP )
         call x%pointer ( p_x, stat = opflag )
      case ( ITYP )
         call pk2_assign ( xtmp ,x )
         call bk2_convertToRk2 ( xtmp%m ) ; xtmp%typ = RTYP
         call xtmp%pointer ( p_x, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "1-st argument of 'foo1' (x) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_x => NULL() ; return ; end if

   call user_s_foo1 ( p_x, r_y )

   call pk2_assign ( rhs = r_y, lhs = y )

   nullify ( p_x )

   END SUBROUTINE s_foo1


!=============================================================================================
   FUNCTION foo1 ( x ) result( y )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: x
   type(pk2_t) :: y
!---------------------------------------------------------------------------------------------

   call s_foo1 ( x, y )

   END FUNCTION foo1


!=============================================================================================
   SUBROUTINE user_s_foo2 ( vec, b, c, x, M )
!=============================================================================================
   real(Rkind) :: vec(3), b, c
   real(Rkind) :: x, M(2,2)
   real(Rkind) :: s, p
   s = vec(1)+b+c ; p = vec(2)*b*c
   x = s + 3*p
   M(1,:) = [ sin(x) / ( s + 1.23 ), cos(x) ]
   M(2,:) = [ exp(M(1,1)) , M(1,1)+M(1,2) ]

   END SUBROUTINE user_s_foo2


!=============================================================================================
   SUBROUTINE s_foo2 ( vec, b, c, x, M )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: vec, b, c
   type(pk2_t),         intent(in out) :: x, M
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "foo2"
   real(Rkind), pointer :: p_vec(:) => NULL()
   type(pk2_t) :: vectmp
   real(Rkind), pointer :: p_b => NULL()
   type(pk2_t) :: btmp
   real(Rkind), pointer :: p_c => NULL()
   type(pk2_t) :: ctmp
   real(Rkind) :: r_x
   real(Rkind) :: r_M(2,2)
!---------------------------------------------------------------------------------------------

   if ( vec%nrow * vec%ncol /= 3 ) then
      call opflag%set ( UERROR,HERE,"1-st argument of 'foo2' (vec) must be a 3x1 or a 1x3 array")
      return
   end if

   select case ( vec%typ )
      case ( RTYP )
         call vec%pointer ( p_vec, stat = opflag )
      case ( ITYP )
         call pk2_assign ( vectmp ,vec )
         call bk2_convertToRk2 ( vectmp%m ) ; vectmp%typ = RTYP
         call vectmp%pointer ( p_vec, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "1-st argument of 'foo2' (vec) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_vec => NULL() ; return ; end if

   select case ( b%typ )
      case ( RTYP )
         call b%pointer ( p_b, stat = opflag )
      case ( ITYP )
         call pk2_assign ( btmp ,b )
         call bk2_convertToRk2 ( btmp%m ) ; btmp%typ = RTYP
         call btmp%pointer ( p_b, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "2-nd argument of 'foo2' (b) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_b => NULL() ; return ; end if

   select case ( c%typ )
      case ( RTYP )
         call c%pointer ( p_c, stat = opflag )
      case ( ITYP )
         call pk2_assign ( ctmp ,c )
         call bk2_convertToRk2 ( ctmp%m ) ; ctmp%typ = RTYP
         call ctmp%pointer ( p_c, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "3-rd argument of 'foo2' (c) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_c => NULL() ; return ; end if

   call user_s_foo2 ( p_vec, p_b, p_c, r_x, r_M )

   call pk2_assign ( rhs = r_x, lhs = x )
   call pk2_assign ( rhs = r_M, lhs = M )

   nullify ( p_vec, p_b, p_c )

   END SUBROUTINE s_foo2


!=============================================================================================
   FUNCTION foo2 ( vec, b, c, M ) result( x )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: vec, b, c
   type(pk2_t),         intent(in out) :: M
   type(pk2_t) :: x
!---------------------------------------------------------------------------------------------

   call s_foo2 ( vec, b, c, x, M )

   END FUNCTION foo2


!=============================================================================================
   SUBROUTINE user_s_solve3x3 ( A, b, regular, x, detA, invA )
!=============================================================================================
   real(Rkind) :: A(3,3), b(3), x(3), invA(3,3), detA
   logical :: regular
   real(Rkind), parameter :: TOLDET = 1e-10
   regular = .true.
   invA(:,1) = cross ( A(2,:), A(3,:) )
   detA = dot ( invA(:,1), A(1,:) )
   if ( abs(detA) <= TOLDET * maxval(abs(A)) ) then
      regular = .false.
      x = 0.0 ; invA = 0.0
      opflag = err_t( stat=UERROR,where='solve3x3b',msg='could not solve '// &
               'the system, the matrix is singular or near singular' // NLT// &
               'Note: the results (x, invA) are set to 0' )
      return
   end if
   invA(:,1) = invA(:,1) / detA
   invA(:,2) = cross ( A(3,:), A(1,:) ) / detA
   invA(:,3) = cross ( A(1,:), A(2,:) ) / detA
   x = b(1)*invA(:,1) + b(2)*invA(:,2) + b(3)*invA(:,3)
   contains
     function cross ( x, y ) result(z)
       real(Rkind), intent(in) :: x(3), y(3)
       real(Rkind) :: z(3)
       z(1) = x(2)*y(3) - x(3)*y(2)
       z(2) =-x(1)*y(3) + x(3)*y(1)
       z(3) = x(1)*y(2) - x(2)*y(1)
     end function cross
     function dot ( x, y ) result(s)
       real(Rkind), intent(in) :: x(3), y(3)
       real(Rkind) :: s
       s = x(1)*y(1) + x(2)*y(2) + x(3)*y(3)
     end function dot

   END SUBROUTINE user_s_solve3x3


!=============================================================================================
   SUBROUTINE s_solve3x3 ( A, b, regular, x, detA, invA )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: A, b
   type(pk2_t),         intent(in out) :: regular, x, detA, invA
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = "solve3x3"
   real(Rkind), pointer :: p_A(:,:) => NULL()
   type(pk2_t) :: Atmp
   real(Rkind), pointer :: p_b(:) => NULL()
   type(pk2_t) :: btmp
   logical :: r_regular
   real(Rkind) :: r_x(3)
   real(Rkind) :: r_detA
   real(Rkind) :: r_invA(3,3)
!---------------------------------------------------------------------------------------------

   if ( A%nrow /= 3 .or. A%ncol /=3 ) then
      call opflag%set ( UERROR,HERE,"1-st argument of 'solve3x3' (A) must be a 3x3 array")
      return
   end if

   select case ( A%typ )
      case ( RTYP )
         call A%pointer ( p_A, stat = opflag )
      case ( ITYP )
         call pk2_assign ( Atmp ,A )
         call bk2_convertToRk2 ( Atmp%m ) ; Atmp%typ = RTYP
         call Atmp%pointer ( p_A, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "1-st argument of 'solve3x3' (A) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_A => NULL() ; return ; end if

   if ( b%nrow * b%ncol /= 3 ) then
      call opflag%set ( UERROR,HERE,"2-nd argument of 'solve3x3' (b) must be a 3x1 or a 1x3 array")
      return
   end if

   select case ( b%typ )
      case ( RTYP )
         call b%pointer ( p_b, stat = opflag )
      case ( ITYP )
         call pk2_assign ( btmp ,b )
         call bk2_convertToRk2 ( btmp%m ) ; btmp%typ = RTYP
         call btmp%pointer ( p_b, stat=opflag )
      case default
         call opflag%set ( UERROR,HERE, "2-nd argument of 'solve3x3' (b) must be of real or integer type" )
   end select

   if ( opflag /= 0 ) then ; p_b => NULL() ; return ; end if

   call user_s_solve3x3 ( p_A, p_b, r_regular, r_x, r_detA, r_invA )

   call pk2_assign ( rhs = r_regular, lhs = regular )
   call pk2_assign ( rhs = r_x, lhs = x )
   call pk2_assign ( rhs = r_detA, lhs = detA )
   call pk2_assign ( rhs = r_invA, lhs = invA )

   nullify ( p_A, p_b )

   END SUBROUTINE s_solve3x3


!=============================================================================================
   FUNCTION solve3x3 ( A, b, x, detA, invA ) result( regular )
!=============================================================================================
   type(pk2_t), target, intent(in    ) :: A, b
   type(pk2_t),         intent(in out) :: x, detA, invA
   type(pk2_t) :: regular
!---------------------------------------------------------------------------------------------

   call s_solve3x3 ( A, b, regular, x, detA, invA )

   END FUNCTION solve3x3


END MODULE userfunc_m
