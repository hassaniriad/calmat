PROGRAM using_interpreter_5
!---------------------------------------------------------------------------------------------
!  For compiling this program:
!
!  gfortran -O3 -cpp -I $dirmod using_interpreter_5.f90 -L$dirlib -lpk2 -lblas -llapack
!  ifort    -O3 -fpp -I $dirmod using_interpreter_5.f90 -L$dirlib -lpk2 -lblas -llapack
!  nagfor   -O3 -fpp -I $dirmod using_interpreter_5.f90 -L$dirlib -lpk2 -lblas -llapack -kind=byte
!
!  where $dirmod is ../mod/$comp  and $dirlib is ./../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------
   implicit none
   character(len=10),  dimension(6) :: variables
   real             ,  dimension(6) :: variablesvalues
   real                             :: result
   character(len=: ), allocatable   :: aLongExpression
!---------------------------------------------------------------------------------------------

   variables(1) = 'x'  ; variablesvalues(1) = 0.175
   variables(2) = 'x1' ; variablesvalues(2) = 0.508
   variables(3) = 'y'  ; variablesvalues(3) = 0.110
   variables(4) = 'z'  ; variablesvalues(4) = 0.900
   variables(5) = 'a'  ; variablesvalues(5) = 0.500
   variables(6) = 'b'  ; variablesvalues(6) = 0.750

   ! a given expression involving all or a subset of these variables (here x, y, and z):
   aLongExpression = &
    ' x + y + z + x*y + x*z + y*z + x/y + x/z + y/z + x*cos(x) + y*sin(y) + z*tan(z)*2 / &
    & ( x + y + z + x*y + x*z + y*z + x/y + x/z + y/z + x*cos(x) + y*sin(y) + z*tan(z) )*3 + &
    & sqrt( x*y*z + x + y + z ) * log10( sqrt( x*2 +y*2 +z*2 ) + x + y + z )'

   call eval ( variables, variablesvalues, aLongExpression, result )
   
   print '(/,a,g0)','result = ',result
   
   
   print '(//,a,/)','MAKING AN INTENTIONAL ERROR (undefined variable "y"):'
   
   variables(3) = 'yy'
   
   call eval ( variables, variablesvalues, aLongExpression, result )

CONTAINS

!=============================================================================================
   SUBROUTINE eval ( variables, variablesvalues, func, result )
!=============================================================================================
   use pk2mod_m
   character(len=*), intent(in ) :: variables(:), func
   real            , intent(in ) :: variablesvalues(:) 
   real            , intent(out) :: result
!---------------------------------------------------------------------------------------------        

!- local variables ---------------------------------------------------------------------------          
   type(pk2_t)              :: vars(size(variables))
   type(pk2_t), allocatable :: res(:)
   type(err_t)              :: flag
   real(Rkind), allocatable :: r(:,:)
   integer                  :: i
!---------------------------------------------------------------------------------------------        

   do i = 1, size(variables)
      vars(i) = variablesvalues(i)
      vars(i)%name = variables(i)
   end do
   
   call pk2Interpreter_driver ( expr = func, vars = vars, valExpr = res, flagerr = flag )
      
   call pk2_moveAlloc ( from = res(1), to = r )
    
   result = r(1,1)
   
   END SUBROUTINE eval


END PROGRAM using_interpreter_5