! Assume you want to add some procedures to the pk2 librairie
! but you don't want to modify the pk2 sources.
!
!
! WRITING YOUR PROCEDURES
! =======================
!
! The first thing to do is to write (in very "basic" Fortran) your procedures into a text 
! file between the keywords "fortranproc" and "endfortranproc" and specifying "à la matlab"
! the input and output arguments as in the following examples:
!
! cat myfun.txt:
!
! fortranproc [ regular, x, detA, invA ] = solve3x3 ( A, b )
!    real    :: A(3,3), b(3), x(3), invA(3,3), detA
!    logical :: regular
! !----------------------------------------------------------------------------------- 
! ! Solves the 3x3 linear system A*x = b using 3 cross products and 1 dot product
! ! Returns regular = .false. if the matrix is singular
! ! Computes also (if A is not singular) the inverse matrix
! ! If A is singular prints a warning and returns with x = 0
! !----------------------------------------------------------------------------------- 
! 
! !- local variables -----------------------------------------------------------------
!    real, parameter :: TINY = 1e-20 ! test
! !----------------------------------------------------------------------------------- 
!    regular = .true.
!    invA(:,1) = cross ( A(2,:), A(3,:) )
!    detA = dot ( invA(:,1), A(1,:) )
!    if ( abs(detA) < TINY ) then
!       regular = .false.
!       ! you can use the error/warning handler (opflag is a module variable of pk2. See
!       ! the documentation of the err_m module)
!       opflag = err_t(stat=WARNING,where='solve3x3b',msg=&
!                     'could not solve the system, the matrix is singular or near singular')
!       return
!    end if
!    invA(:,1) = invA(:,1) / detA
!    invA(:,2) = cross ( A(3,:), A(1,:) ) / detA
!    invA(:,3) = cross ( A(1,:), A(2,:) ) / detA
!    x = b(1)*invA(:,1) + b(2)*invA(:,2) + b(3)*invA(:,3)
!    
!    contains 
! !
! !-   internal functions of solve3x3:
! !
!      function cross ( x, y ) result(z)
!        real, intent(in) :: x(3), y(3)
!        real             :: z(3)
!        z(1) = x(2)*y(3) - x(3)*y(2)
!        z(2) =-x(1)*y(3) + x(3)*y(1)
!        z(3) = x(1)*y(2) - x(2)*y(1)
!      end function cross
! 
!      function dot ( x, y ) result(s)
!        real, intent(in) :: x(3), y(3)
!        real,            :: s
!        s = x(1)*y(1) + x(2)*y(2) + x(3)*y(3)
!      end function dot
! endfortranproc solve3x3
! !
! fortranproc hello
!   print*,'hello world'
! endfortranproc hello
!
! fortranproc [ x, y, z ] = foo ( x, y, z, u ) ! (another one)
! !...
! endfortranproc foo
!
!
! CREATING THE LIBRARY
! ====================
!
! Then you launch the code "addfun" in the bin/$comp directory, e.g.
!
!    addfun -i myfun.txt -o libmypk2
!
! It will create the augmented library libmypk2.a (or .so if you use the option -l dynamic)
! that can be used in place of libpk2.a (or libpk2_d.so)
! You can choose the destination directory of libmypk2.a by giving the path of this later
! (e.g.  addfun -i datafiles/myfun.txt -o mylibs/libmypk2
!
! You can also safely choose to directly overwrite the original libpk2.a (or libpk2_d.so) 
! library by giving its path, e.g. if the pk2 installation directory is /Users/me/pk2:
!
!    addfun  -i myfun.txt  -o /Users/me/pk2/lib/$comp/libpk2
!
! Note that addfun creates also the module file userfunc_m.mod that can be needed during the
! compilation your program (see below). 
! By default this file is located in your current directory. You can specify its destination
! with the -mdir option. If you wish for example to overwrite the original one, enter:
!
!    addfun  -i myfun.txt  -o /Users/me/pk2/lib/$comp/libpk2  -mdir /Users/me/pk2/mod/$comp
!
! COMPILING YOUR PROGRAM
! ======================
!
! Now you can compile and link your program against this library, e.g.
!
! $comp myprog.f90 -I ../../mod/$comp/ -L/path/to/libmypk2 -lmypk2 -llapack -lblas -o myprog
!
! Note: as mentioned above the module file userfunc_m.mod created by addfun can be needed by 
!       your program. This is the case when you wish to call your procedures without using the
!       supplied driver (see below).
!       In that case, you can move userfunc_m.mod into mod/$comp/ (see above) or add another
!       -I when compiling your program.
!
!
! HOW TO CALL YOUR PROCEDURES
! ===========================
!
! addfun will record your procedures as subroutines and also as functions if the option
! -onlysub is not used (of course for procedures that have at least one output argument). 
! If you use the subroutine versions, add the prefix "s_" when calling them. 
!
! Considere the previous example (definition of solve3x3)
!
! 1) Calling your procedure as a function
! ---------------------------------------
!
! Since a Fortran function returns a single variable, the first output argument ("regular"
! in the example) will be the result. The others ("x", "detA", "invA") are inout variables and
! follow the input variables. The interface of the function solve3x3 is then:
!
!           function solve3x3 ( A, b, x, detA, invA ) result ( regular )
!              type(pk2_t), intent(in    ) :: A, b
!              type(pk2_t), intent(in out) :: x, detA, invA
!              type(pk2_t)                 :: regular
!
! Here is a user program that use this function:
!     ...
!     type(pk2_t) :: A, b, x, reg, detA, invA
!     A = ...
!     b = ...
!
!     reg = solve3x3 ( A, b, x, detA, invA ) 
!
! 2) Calling your procedure as a subroutine
! -----------------------------------------
! 
! In the subroutine version the output arguments follow simply the input ones.
! The interface of the corresponding subroutine s_solve3x3 is then:
!
!          subroutine s_solve3x3 ( A, b, regular, x, detA, invA )
!
! Thus the calling sequence is as follow:
!     ...
!     type(pk2_t) :: A, b, x, reg, invA
!     A = ...
!     b = ...
!
!     call s_solve3x3 ( A, b, reg, x, detA, invA )
!
!
! 3) Calling your procedure via the drivers
! -----------------------------------------
!
! There are two drivers called userProcDriver and s_userProcDriver.
!
! The main advantage a driver (compared to a direct call to your procedures) is:
! - the interface does not change and is already known by pk2, so you do not need the module
!   file userfunc_m.mod to compile your program
! - you don't have to request all the outputs
!
! The interfaces of these two drivers are:
!
!    function userProcDriver ( fname, inputs, optOuts ) result ( firstOut )
!    character(len=*),           intent(in    ) :: fname
!    type     (pk2_t),           intent(in    ) :: inputs(:)
!    type     (pk2_t), optional, intent(in out) :: optOuts(:)
!    type     (pk2_t)                           :: firstOut
!
! and
!
!    subroutine s_userProcDriver ( fname, inputs, firstOut, optOuts )
!
! Example of use:
!
!     type(pk2_t) :: A, b, regular, opts(3)
!     A = ...
!     b = ...
!
!     regular = userProcDriver ( fname = 'solve3x3', inputs = [A, b], optOuts  = opts )
!
!  (opts(1:3) will contain x, detA and invA, respectively)
!
!  If only x is requested:
!
!     regular = userProcDriver ( fname = 'solve3x3', inputs = [A, b], optOuts = opts(1) )
!
!  and if only the principal output (regular) is requested:
!
!     regular = userProcDriver ( fname = 'solve3x3', inputs = [A, b] )
!
program userfunctions
!
!- Tests
!
   use pk2mod_m
   
   implicit none
   
   call test1
   call test2

contains

!=============================================================================================   
   subroutine test1 
!=============================================================================================   

!---------------------------------------------------------------------------------------------
!  Version 1: calling the user function/subroutine directly.
!
!  Note: compilation of this program needs the new module file userfunc_m.mod created during 
!  the execution of addfun (since the interfaces of user procedures are not known by pk2mod)
!---------------------------------------------------------------------------------------------

   type(pk2_t)          :: A, b, x, invA, detA, regular
   logical, allocatable :: continue

   print*
   print*,'Version 1'
   print*,'========='
   print*

!
!- 1) A user procedure that has no input and output arguments:
!
   call s_hello()
   
   A = reshape([1.,1.,2.,1.,2.,5.,2.,5.,14.],[3,3])
   call A%printMe ( msg = '. Let A = ', form = 'values')

   b = [9.,20.,54.]
   call b%printMe ( msg = '. and b = ', form = 'values')
!
!- 2) A user procedure with 2 inputs and 4 outputs (1 + 3)
!
   ! 2a)  with the subroutine version (comment/decomment):

   ! call s_solve3x3 ( A, b,   regul, x, detA, invA )
 
   ! 2b) with the function version (comment/decomment):

   regular = solve3x3 ( A, b,   x, detA, invA )
 
   ! Print the results:

   call regular%printMe ( msg = '. Is A regular?: ', form = 'values')

   call regular%get ( 1_Ikind, 1_Ikind, continue )

   if ( continue ) then
      call x%printMe ( msg = '. The solution of A*x = b is: ', form = 'values')
      call invA%printMe ( msg = '. The inverse of A is: ', form = 'values')
   else
      print*,'. The system is singular'
   end if
   call detA%printMe ( msg = '. The determinant of A is: ', form = 'values')
   
   end subroutine test1


!=============================================================================================   
   subroutine test2
!=============================================================================================   

!---------------------------------------------------------------------------------------------
!  Version 2: calling the drivers userProcDriver (function version) or s_userProcDriver 
!  (subroutine version). 
!
!  Note: The new userfunc_m.mod (created during the execution of addfun) is not needed since  
!  the interface of userProcDriver (resp. s_userProcDriver) has not changed.
!---------------------------------------------------------------------------------------------

   type(pk2_t) :: A, b, regular, opts(3), dummy
   logical, allocatable :: continue

   print*
   print*,'Version 2'
   print*,'========='
   print*
!
!- 1) A user procedure that has no input and output arguments:
!
   ! 1a) With the subroutine version of the driver (comment/decomment):

   !call s_userProcDriver ( fname    = 'hello', &
   !                        inputs   = [dummy], &
   !                        firstOut = dummy    )

   ! 1b) With the function version of the driver (comment/decomment):

   dummy = userProcDriver ( fname    = 'hello', &
                           inputs   = [dummy] )
!  
!- 2) A user procedure with 2 inputs and 4 outputs (1 + 3)
!  
   A = reshape([1.,1.,2.,1.,2.,5.,2.,5.,14.],[3,3])
   call A%printMe ( msg = '. Let A = ', form = 'values')

   b = [9.,20.,54.]
   call b%printMe ( msg = '. and b = ', form = 'values')

   ! 2a) With the subroutine version of the driver (comment/decomment):

   !call s_userProcDriver ( fname    = 'solve3x3', &
   !                        inputs   = [A, b],     &
   !                        firstOut = regular,    &
   !                        optOuts  = opts        )
   
   ! 2b) With the function version of the driver (comment/decomment):

   regular = userProcDriver ( fname    = 'solve3x3', &
                              inputs   = [A, b],     &
                              optOuts  = opts        )

   ! Print the results:

   call regular%printMe ( msg = '. Is A regular?: ', form = 'values')
   
   call regular%get ( 1_Ikind, 1_Ikind, continue )

   if ( continue ) then
      call opts(1)%printMe ( msg = '. The solution of A*x = b is: ', form = 'values')
      call opts(3)%printMe ( msg = '. The inverse of A is: ', form = 'values')
   else
      print*,'. The system is singular'
   end if
   call opts(2)%printMe ( msg = '. The determinant of A is: ', form = 'values')
   
   end subroutine test2   


end program userfunctions
