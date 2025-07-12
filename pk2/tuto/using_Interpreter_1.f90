program using_interpreter_1
!---------------------------------------------------------------------------------------------
!- Example (Basic use) showing how to use the pk2 interpreter (call to pk2Interpreter_driver)
!
!  (Also consider using the higher-level library "calmat" for greater convenience. 
!   See examples in pk2/app/calmat).
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod using_interpreter_1.f90 -L$dirlib -lpk2 -llapack -lblas
!  ifort    -fpp -I $dirmod using_interpreter_1.f90 -L$dirlib -lpk2 -llapack -lblas
!  nagfor   -fpp -I $dirmod using_interpreter_1.f90 -L$dirlib -lpk2 -llapack -lblas -kind=byte
!
!  where $dirmod is ../mod/$comp  and $dirlib is ./../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   character(len=:), allocatable :: expr     ! The expression to evaluate
   type     (pk2_t)              :: vars(10) ! A list of 10 pk2 arrays
   type     (pk2_t), allocatable :: res(:)   ! The result of an evaluation (must be an alloc.)
   type     (err_t)              :: flag     ! Flag error
   integer  (Ikind)              :: n = 3
!---------------------------------------------------------------------------------------------
   
   ! If we disable automatic stop due to error (by default, halting =.true.) we have to control
   ! the error flag after each call to the interpreter to see if something went wrong:

   call err_SetHaltingMode ( halting = .false., displayWarning = .false. )
   
   ! We set our current variables (4 for example):
   vars(1) = n         ; vars(1)%name = 'n'   ! an integer scalar
   vars(2) = rand(n,n) ; vars(2)%name = 'A'   ! a random 3x3 array
   vars(3) = rand(n,n) ; vars(3)%name = 'B'   ! another one
   vars(4) = 1.234     ; vars(4)%name = 't'   ! a real scalar

   call vars(2)% PrintMe (form = 'values', msg = 'Let A = ')
   call vars(3)% PrintMe (form = 'values', msg = 'and B = ')
   call vars(4)% PrintMe (form = 'values', msg = 'and t = ')
   
   ! A first expression to evaluate: expr = "A * B + t^2"
   expr = "A * B + t^2"
   
   ! Call the interperter (the result will be stored in res(1)):
   call pk2Interpreter_driver ( expr, vars, res, flag )

   ! We check whether an error (or warning) has occured:
   if ( flag%code /= 0 ) then
      call flag%display( abort=.true. ) ! display the message (if any) and stop if code > 0
   end if
 
   ! Display the result (by default on stdout, unless optional arg. "unit = " is used):
   call res(1)% PrintMe ( form = 'values', &
                           msg = 'The result of the evaluation of "'//trim(expr)//'" is: ' )

   ! We can save this result in the list of variables:
   vars(5) = res(1) ; vars(5)%name = 'Z'
   
   ! A new evaluation (again, the result will be stored in res(1)):
   call pk2Interpreter_driver ( "Z(:,2) + sin(A(:,1))", vars, res, flag )
   
   ! We check whether an error (or warning) has occured:
   if (flag%code /= 0) then
      call flag%display( abort=.true. ) ! display the message (if any) and stop if code > 0
   end if

   ! Display the result (by default on stdout, unless optional arg. "unit = " is used):
   call res(1)% PrintMe ( form = 'values', dispstyle = 'ABOVE', &
                           msg = 'The result of the evaluation of "Z(:,2) + sin(A(:,1))"'// &
                                   NL//'(where Z is the previous result) is: ' )

   write(*,'(/,a)')'Terminated'

end program using_interpreter_1

