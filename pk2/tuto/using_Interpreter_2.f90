program using_interpreter_2
!---------------------------------------------------------------------------------------------
!- Example showing how to use the pk2 interpreter (call to pk2Interpreter_driver)
!  and explaining why the result must be an allocatable
!
!  (Also consider using the higher-level library "calmat" for greater convenience. 
!   See examples in pk2/app/calmat).
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod using_interpreter_2.f90 -L$dirlib -lpk2 -llapack -lblas
!  ifort    -fpp -I $dirmod using_interpreter_2.f90 -L$dirlib -lpk2 -llapack -lblas
!  nagfor   -fpp -I $dirmod using_interpreter_2.f90 -L$dirlib -lpk2 -llapack -lblas -kind=byte
!
!  where $dirmod is ../../mod/$comp  and $dirlib is ./../../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   type     (pk2_t)              :: vars(10) ! A list of 10 pk2 arrays
   type     (pk2_t), allocatable :: res(:)   ! The result of an evaluation
   type     (err_t)              :: flag     ! Flag error
   integer  (Ikind)              :: n = 3
!---------------------------------------------------------------------------------------------
   
   ! We disable automatic stop due to error (by default, ErrAbort=.true.)
   ! That means we have to control the error flag after each call to the interpreter
   ! to see if something went wrong.

   call err_SetHaltingMode ( halting = .false. )  ! disable automatic stop due to error
   
   ! We set our current variables (4 for example):
   vars(1) = n         ; vars(1)%name = 'n'  ! an integer scalar
   vars(2) = rand(n,n) ; vars(2)%name = 'A'  ! a random 3x3 array
   vars(3) = rand(n,n) ; vars(3)%name = 'B'  ! another one
   vars(4) = 1.234     ; vars(4)%name = 'mu' ! a real scalar
!
!- 1) Computing the eigenvalues of a square matrix:
!
   print '(/,a)','1) Computing the eigenvalues of a square matrix:'
   
   ! We first want to compute only the eigenvalues of the real square matrix A - mu*I. 
   ! The expected result is a single pk2 array (of real or complex values).
   ! Then we keep the number of answers to its default value of 1:
   
   call pk2Interpreter_driver ( "eig(A-mu*eye(n,n))", vars, res, flag )
   
   ! We check whether an error (or warning) has occured:
   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0

   call res(1)%PrintMe ( form = 'values', msg = 'The eigenvalues of A-mu*I are: ' )
!
!- 2) Computing the eigenvalues AND the eigenvectors of a square matrix:
!
   print '(/,a)','2) Computing the eigenvalues and the eigenvectors of a square matrix:'
   
   ! Now, we want to compute both the eigenvalues and the eigenvectors. 
   ! We expect two pk2 arrays (of real or complex values): the  1st one ("res(1)") will 
   ! contains the eigenvalues (an n x 1 matrix) and the 2nd one ("res(2)") the eigenvectors
   ! (an n x n matrix).
   ! Then we set the number of answers (optional argument "nanswer") to 2:

   call pk2Interpreter_driver ( "eig(A-mu*eye(n,n))", vars, res, flag, nanswer = 2_Ikind )

   ! We check whether an error (or warning) has occured:
   call flag%display( abort = .true. ) ! display the message (if any) and stop if code > 0

   call res(1)%PrintMe ( form = 'values', msg = 'The eigenvalues of A-mu*I are: ' )
   call res(2)%PrintMe ( form = 'values', msg = 'The eigenvectors of A-mu*I are: ' )
 
   ! We can save the result in the set of variables "vars":
   vars(5) = res(1) ; vars(5)%name = 'd'
   vars(6) = res(2) ; vars(6)%name = 'P'
   
   ! And we can use them in other expressions, e.g.
   call pk2Interpreter_driver ( "(A-mu*eye(n,n))*P - P*diag(d)", vars, res, flag )

   ! We check whether an error (or warning) has occured:   
   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0

   call res(1)%PrintMe ( form = 'values', msg = '(A-mu*I)*P - P*D (residuals): ' )   

   ! Another example:
   call pk2Interpreter_driver ( "norm((A-mu*eye(n,n))*P - P*diag(d))", vars, res, flag )
   
   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0

   call res(1)%PrintMe ( form='values', msg = '|| (A-mu*I)*P - P*D || = ' )   
   
!
!- 3) Sorting array elements:
!
   print '(/,a)','3) Sorting array elements:'   
   
   ! 3.1) A first example (with a vector):
   vars(6) = [10,-1,3,2,9,8,3,15,4,6,5,-2] ; vars(6)%name = 'v'
   
   call vars(6)%PrintMe ( form = 'values', msg = 'Let v = ' )
   
   ! return only the sorted array (and by default in increasing order):
   call pk2Interpreter_driver ( "sort(v)", vars, res, flag )
   
   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0
   
   call res(1)%PrintMe ( form = 'values', msg = 'then sort(v) = ' )
   
   ! return also the original indices:
   call pk2Interpreter_driver ( "sort(v)", vars, res, flag, nanswer = 2_Ikind )

   call flag%display ( abort=.true. ) ! display the message (if any) and stop if code > 0

   call res(2)%PrintMe ( form = 'values', msg = 'and the original indices are: ' )

   ! 3.2) A second example (with a matrix):

   vars(6) = reshape([10,-1,3,2,9,8,3,15,4,6,5,-2],[4,3]) ; vars(6)%name = 'v'

   call vars(6)%PrintMe ( form = 'values', msg = 'Let v = ' )

   ! sort each column (default) in decreasing order. Return only the sorted array:
   call pk2Interpreter_driver ( "sort(v,'d')", vars, res, flag )

   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0

   call res(1)%PrintMe ( form = 'values', msg = "then sort(v,'d') = sort(v,'d',1) = " )

   ! sort each row in decreasing order. Return only the sorted array:
   call pk2Interpreter_driver ( "sort(v,'d',2)", vars, res, flag )
   
   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0

   call res(1)%PrintMe ( form = 'values', msg = "and sort(v,'d',2) = " )   

   ! sort the elements in decreasing order. Return only the sorted array:
   call pk2Interpreter_driver ( "sort(v(:),'d')", vars, res, flag )

   call flag%display ( abort=.true. ) ! display the message (if any) and stop if code > 0

   call res(1)%PrintMe ( form = 'values', msg = "and sort(v(:),'d') = " )   
   
   ! 3.3) A third example with a vector of strings:
   
   vars(6) = ['xlf     ','gfortran','Xlf     ','nagfor  ','ifort   ','g95     ']
   vars(6)%name = 'my_fortran_compilers'
   call vars(6)%PrintMe ( form = 'values' )  

   ! sort the elements in increasing order (in case sensitive mode):
   call pk2Interpreter_driver ( "sort(my_fortran_compilers)", vars, res, flag )
   
   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0
   
   call res(1)%PrintMe ( form = 'values', msg = "then sort(my_fortran_compilers) : " )   
   
   ! sort the elements in increasing order (in case insensitive mode):
   call pk2Interpreter_driver ( "sort(my_fortran_compilers,'CaseInsensitive')", &
                                vars, res, flag )

   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0
   
   call res(1)%PrintMe ( form = 'values', &
                          msg = "then sort(my_fortran_compilers,'CaseInsensitive') : " )   
   

   ! 3.3) A fourth example with a matrix of strings:

   vars(6) = reshape(['xlf     ','gfortran','Xlf     ','nagfor  ','ifort   ','g95     '], &
                     [2,3])
   vars(6)%name = 'my_fortran_compilers'
   call vars(6)%PrintMe ( form = 'values' )  

   ! sort each row in increasing order (in case sensitive mode):
   call pk2Interpreter_driver ( "sort(my_fortran_compilers,2)", vars, res, flag )
   
   call flag%display ( abort = .true. ) ! display the message (if any) and stop if code > 0
   
   call res(1)%PrintMe ( form = 'values', msg = "then sort(my_fortran_compilers,2) : " )   
   
   write(*,'(/,a)')'Terminated'
   
end program using_interpreter_2

