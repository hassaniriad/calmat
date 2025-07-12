program using_interpreter_3
!---------------------------------------------------------------------------------------------
!- Example showing the two possible ways to use the pk2 interpreter:
!
!  . by calling pk2Interpreter_driver (useful for an expression to be evaluated once)
!
!  . by calling pk2Interpreter_Tokenizor first and then pk2Interpreter_Evaluator (for an 
!    expression to be evaluated several times)
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod using_interpreter_3.f90 -L$dirlib -lpk2 -llapack -lblas
!  ifort    -fpp -I $dirmod using_interpreter_3.f90 -L$dirlib -lpk2 -llapack -lblas
!  nagfor   -fpp -I $dirmod using_interpreter_3.f90 -L$dirlib -lpk2 -llapack -lblas -kind=byte
!
!  where $dirmod is ../mod/$comp  and $dirlib is ./../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   type     (pk2_t)              :: vars(10) ! A list of 10 pk2 arrays
   type     (pk2_t), allocatable :: res(:)   ! The result of an evaluation
   type     (err_t)              :: flag     ! Flag error
   integer  (Ikind)              :: n = 3, i
   type     (hdl_t)              :: h
   character(len=9)              :: cnum
!---------------------------------------------------------------------------------------------
   
   ! We set our current variables (4 for example):
   vars(1) = n         ; vars(1)%name = 'n'  ! an integer scalar
   vars(2) = rand(n,n) ; vars(2)%name = 'A'  ! a random 3x3 array
   vars(3) = rand(n,n) ; vars(3)%name = 'B'  ! another one
   vars(4) = 1.234     ; vars(4)%name = 'mu' ! a real scalar

   call vars(2)%Printme ( form = 'values', msg = 'Let A = ' )
   call vars(3)%Printme ( form = 'values', msg = 'and B = ' )
   call vars(4)%Printme ( form = 'values', msg = 'and mu = ' )
!
!- 1) An expression to be evaluated once (using the default driver):
!
   print '(/,a)','1) An expression to be evaluated once (using the default driver):'
   
   call pk2Interpreter_driver ( expr = "eig(A-mu*eye(n,n))", vars = vars, &
                                valExpr = res, flagerr = flag )

   ! Print the result:
   call res(1)%PrintMe ( form = 'values', msg = 'The eigenvalues of A-mu*I are: ' )
!
!- 1-bis) The same expression, first parsed and then evaluated:
!
   print '(/,a)','1-bis) The same expression, first parsed and then evaluated:'
   
   ! We call the Tokenizor: 
   call pk2Interpreter_Tokenizor ( expr = "eig(A-mu*eye(n,n))", vars = vars, &
                                   handle = h, flagerr=flag )

   print '(/,a)','Analysis: done (no error found)'

   ! We call now the Evaluator: 
   call pk2Interpreter_Evaluator ( handle = h, vars = vars, valExpr = res, flagerr = flag )
   
   ! Print the result:
   call res(1)%PrintMe ( form = 'values', msg = 'The eigenvalues of A-mu*I are: ' )
!
!- 2) An expression to be evaluated several times (but parsed only once):
!
   print '(/,a)','2) An expression to be evaluated several times (but parsed only once):'

   ! We call the Tokenizor:    
   call pk2Interpreter_Tokenizor ( expr = "A*B/norm(B)", vars = vars, &
                                   handle = h, flagerr = flag )

   print '(/,a)','Analysis: done (no error found)'

   do i = 1, 40
         
      ! call now the Evaluator: 
      call pk2Interpreter_Evaluator ( handle = h, vars = vars, valExpr = res, flagerr = flag )

      ! print the result:
      write(cnum,'(i0)')i 
      call res(1)%PrintMe ( form = 'values', msg = '(A^'//trim(cnum)//')*B/||B|| = ' )
   
      ! change  the value of B (overwritten by the result):
      vars(3) = res(1)
   
   end do
   
   write(*,'(/,a)')'Terminated'
   
end program using_interpreter_3

