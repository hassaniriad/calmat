program booleans

!---------------------------------------------------------------------------------------------
!
!- A small demo showing how to use the pk2 library
!
!  Example showing how to 
!  . play with boolean operators
!  . and with relational operators
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod booleans.f90 -L$dirlib -lpk2 -lblas -llapack
!  ifort    -fpp -I $dirmod booleans.f90 -L$dirlib -lpk2 -lblas -llapack
!  nagfor   -fpp -I $dirmod booleans.f90 -L$dirlib -lpk2 -lblas -llapack -kind=byte 
!
!  where $dirmod is ../../mod/$comp  and $dirlib is ./../../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------

   use pk2mod_m

   type(pk2_t) :: P, Q, R, X
!
!- and, or, not operators:
!
   P = reshape([.true.,.true. ,.false.,.false.],[4,1]) ! <-- a 4-vector of booleans
   Q = reshape([.true.,.false.,.true. ,.false.],[4,1]) ! <-- a 4-vector of booleans

   X = (P .or. Q) .and. (.not. P .or. .not. Q) ! <-- operation between P and Q 
                                               !     (X = exclusive or)
  
   call P%InsertInto (X,1,'c')
   call Q%InsertInto (X,2,'c')
   call X%PrintMe(msg='truth table of .xor.',dispstyle='underline',form='values')

!
!- relational operators (==, /=, <, <=, >, >=) with numeric data:
!
   P = [1  , 2  ,  3  ] !
   Q = [1.0, 1.9,  3.2] ! <-- take 3 matrices of the same shape
   R = [1.1, 2.1,  3.0] !

   call P%PrintMe(msg='Let P be: ',form='values')
   call Q%PrintMe(msg='and Q: '   ,form='values')
   call R%PrintMe(msg='and R: '   ,form='values')
   
   X = ( P <= Q )             ; call X%PrintMe(msg='then P <= Q is: '       ,form='values')
   X = ( P <= Q .and. Q < R ) ; call X%PrintMe(msg='and P <= Q & Q < R is: ',form='values')
   X = ( P == 2_Ikind )       ; call X%PrintMe(msg='and P == 2 is: '        ,form='values')


   P = reshape([(1,1),(2,0),(0,2),(4,3)],[2,2]) ! <-- a 2x2 matrix of complexes

   call P%PrintMe(msg='Let P be: ',form='values')
   
   X = ( abs(P) == 2_Ikind )  ; call X%PrintMe(msg='then abs(P) == 2 is: ',form='values')

!
!- relational operators (==, /=) with string data:
!
   P = reshape(['xlf     ','ifort   ', &       !
                'nagfor  ','gcc     ', &       ! <-- a 3x2 matrix of strings
                'nagfor  ','gfortran'],[3,2])  !
                
   call P%PrintMe(msg='Let P be: ',form='values')

   P = ( P == "nagfor" )  ; call P%PrintMe(msg='then P == "nagfor" is: ',form='values')

   write(*,'(/,a)')'Terminated'
   
end program booleans
