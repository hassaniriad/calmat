program print
!---------------------------------------------------------------------------------------------
!
!- A small demo showing how to use the pk2 library
!
!  Example showing how to 
!  . print a pk2 variable in different format
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod print.f90 -L$dirlib -lpk2 -lblas -llapack
!  ifort    -fpp -I $dirmod print.f90 -L$dirlib -lpk2 -lblas -llapack
!  nagfor   -fpp -I $dirmod print.f90 -L$dirlib -lpk2 -lblas -llapack -kind=byte
!
!  where $dirmod is ../mod/$comp  and $dirlib is ./../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------
   use pk2mod_m, c => str_color
   type(pk2_t) :: a(5) ! five pk2 variables
!---------------------------------------------------------------------------------------------

!
!  Set the 5 variables:
!
   ! a 2x3 integer array (without name):
   a(1) = reshape([1,2,3,4,5,6],[2,3])
   ! a boolean scalar (without name):
   a(2) = 2 < 1
   ! a boolean scalar (without name): 
   a(3) = .true.
   ! a real scalar (with a name):
   a(4)%name = 'sin(A)' ;  a(4) = sin(a(1)) 
   ! a 2x3 colored string array (with a name):
   a(5)%name = 'compilers'
   a(5) = reshape ( [                                                                   &
         c('ifort','g'),                                                                &   
         c('n','rhb')+c('a','bhb')+c('g','ghb')+c('f','yhb')+c('o','chb')+c('r','mhb'), &
         c('gfortran','whB'),                                                           &
         c('xlf','ybU'),                                                                &
         c('c++','chb'),                                                                &
         c('absoft','m')                                                                &
                     ], [2,3] )

!=============================================================================================
!-                                       Printing
!=============================================================================================
 
   write(*,'(/,a)')'******** Printing *********'

!- Print a(4) using the default format:

   write(*,'(/,a)')"1) Print a(4) using the default: call a(4) % PrintMe ()"
   call a(4) % PrintMe ()

!- Print a(4) using the default format preceded by a message:

   write(*,'(/,a)')"2) Print a(4) with a message: call a(4) % PrintMe (msg = 'this variable ...:')"
   call a(4) % PrintMe (msg = 'this variable contains the following data:')

!- Do not Print the values of a(4): 

   write(*,'(/,a)')"3) Don't Print the values of a(4): call a(4) % PrintMe (form = 'novalues')"   
   call a(4) % PrintMe (form = 'novalues')
   
!- Print only the values of a(4) preceded by its name and a '=': 

   write(*,'(/,a)')"4) Print the values of a(4): call a(4) % PrintMe (form = 'values')"
   call a(4) % PrintMe (form = 'values')

!- Print only the values of a(4):

   write(*,'(/,a)')"5) Print only the values of a(4): call a(4) % PrintMe (form = 'values', msg ='')"
   call a(4) % PrintMe (form = 'values', msg = '')

!- Print a(5) using the default format:

   write(*,'(/,a)')"6) Print a(5) using the default: call a(5) % PrintMe ()"
   write(*,'(a)'  )"   (in color, unless the stdout is redirected)"
   call a(5) % PrintMe ()
   write(*,'(/,a)')'Terminated'

end program print
