program using_interpreter_6
!---------------------------------------------------------------------------------------------
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod using_interpreter_6.f90 -L$dirlib -lpk2 -lblas -llapack
!  ifort    -fpp -I $dirmod using_interpreter_6.f90 -L$dirlib -lpk2 -lblas -llapack
!  nagfor   -fpp -I $dirmod using_interpreter_6.f90 -L$dirlib -lpk2 -lblas -llapack -kind=byte
!
!  where $dirmod is ../mod/$comp  and $dirlib is ./../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   type     (pk2_t)              :: vars(10) ! A list of 10 pk2 arrays
   type     (pk2_t), allocatable :: res(:)   ! The result of an evaluation
   type     (err_t)              :: flag     ! Flag error
   type     (hdl_t)              :: h
!---------------------------------------------------------------------------------------------

   call err_SetHaltingMode ( halting = .false. )  ! disable automatic stop due to error
   
   ! We set our current variables (5 for example):
   vars(1) = 1 ; vars(1)%name = 'x'  
   vars(2) = 2 ; vars(2)%name = 'y'  
   vars(3) = 1 ; vars(3)%name = 'a'  
   vars(4) = 2 ; vars(4)%name = 'b' 
   vars(5) = 3 ; vars(5)%name = 'c' 
!
!- 1) Analyse the expression  "a*x+b*y+c":
!   
   call pk2Interpreter_Tokenizor ( expr = "a*x+b*y+c", vars = vars, handle = h, flagerr = flag )
!
!- 2) Evaluate it (res=8):
!
   print '(/,a)','First evaluation (res=8):'
   print '(a  )','========================='
   
   call pk2Interpreter_Evaluator ( handle = h, vars = vars, valExpr = res, flagerr= flag )

   if ( flag > 0 ) then
      call flag%Display()
   else
      call res(1)%PrintMe ( form = 'values', msg = 'a*x+b*y+c = ' )
   end if
!
!- 3) Now assume the list of variables has changed: the variable vars(3) ("a") was
!     overwritten (by "aa") and the variable "a" is no longer present in vars. 

   vars(3) = 'hello' ; vars(3)%name = 'aa'
!
!  then the call to the evaluator will cause an error (and firstly a warning to indicate a 
!  change in the list since the last analysis):

   print '(/,a)','Second evaluation (=> warning+error):'
   print '( a )','====================================='

   call pk2Interpreter_Evaluator ( handle = h, vars = vars, valExpr = res, flagerr = flag )
   
   if ( flag > 0 ) then
      call flag%Display ( title = 'intentional error:' )
   else
      call res(1)%PrintMe ( form = 'values', msg = 'a*x+b*y+c = ' )
   end if   
!
!- 4) but if we redefine a new variable "a", not necessarily in the same position in vars, 
!     then it will work:
!   
   vars(9) = 1 ; vars(9)%name = 'a'
 
   print '(/,a)','Third evaluation (=> warning):'
   print '( a )','=============================='
  
   call pk2Interpreter_Evaluator ( handle = h, vars = vars, valExpr = res, flagerr = flag )
   
   if ( flag > 0 ) then
      call flag%Display()
   else
      call res(1)%PrintMe ( form = 'values', msg = 'a*x+b*y+c = ' )
   end if   
!
!- 5) Now we change only the value of the variables:
 
   vars(1) = 2 ! 'x'  
   vars(2) = 0 ! 'y'  
   vars(4) = 2 ! 'b' 
   vars(5) =-30.0 ! 'c' 
   vars(9) = 3 ! 'a' 
   
   print '(/,a)','Fourth evaluation (res=-24.0):'
   print '( a )','============================='

  
   call pk2Interpreter_Evaluator ( handle = h, vars = vars, valExpr = res, flagerr = flag )
   
   if ( flag > 0 ) then
      call flag%Display()
   else
      call res(1)%PrintMe ( form = 'values', msg = 'a*x+b*y+c = ' )
   end if   

   
   write(*,'(/,a)')'Terminated'
  
end program using_interpreter_6

