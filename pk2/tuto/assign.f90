program assign

!---------------------------------------------------------------------------------------------
!
!- A small demo showing how to use the pk2 library
!
!  Example showing how  
!  . to assign pk2 variables (§1, §2, §3 and §4)
!  . to copy (§5), to insert (§6), to extract (§7), to reset (§8) some elements, and to merge
!    arrays (§9)
!
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod assign.f90 -L$dirlib -lpk2 -llapack -lblas
!  ifort    -fpp -I $dirmod assign.f90 -L$dirlib -lpk2 -llapack -lblas
!  nagfor   -fpp -I $dirmod assign.f90 -L$dirlib -lpk2 -llapack -lblas -kind=byte
!
!  where $dirmod is ../mod/$comp  and $dirlib is ./../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------

   use pk2mod_m
!
!- Example of declarations of pk2 variables:
!
   type(pk2_t)              :: a, b, c, d, e   ! Five pk2 var.
   type(pk2_t)              :: v(4), w(2,4)    ! Two arrays of pk2 var.
   type(pk2_t), allocatable :: x1(:), x2(:,:)  ! Two allocatable arrays of pk2 var.
   
   type(str_t)              :: s(3), sp(2,2)   ! Two lists of strings
!
!- Caution: two kind parameters are defined in the "kindParameters.f90" module: 
!
!  . Ikind for integers
!  . Rkind for reals and complexes
!
!  Many procedures, but not all, are generic and work with i32 and i64 integer arguments. 
!  It is then recommended to always write integer arguments using _Ikind as in '2_Ikind' 

!
!- 1) Scalar assignments:
!
   a = 'hello'  ! a is a string
   a = 1        ! a is now an integer
   a = (1,2)    ! a is now a complex
   a = .true.   ! and now is a logical
!
!- after the assignment we can give a name to our variable, e.g.:
!
   a%name = 'my first pk2 variable'
!
!- We can also use the pk2-constructor to set a variable, e.g. to set a = .true. with a name
!  (optional):
!
   a = pk2_t(.true., name = 'my first pk2 variable') 
!
!- or more precisely:
!
   a = pk2_t(scalar = .true., name = 'my first pk2 variable') 
!
!- and we can print it (different options exist, see "print.f90" for more details):
!
   call a%PrintMe()      
!
!- 2) Vector assignments:
!
   a = [1, 2, 3] ! a is a 3x1 array of integers
   a = [.true., .false., .false., .true.] ! a is now a 4x1 array of logicals
!
!- or with the constructor:
!
   a = pk2_t([.true., .false., .false., .true.], name = 'my second pk2 variable')
!
!- or more precisely:
!
   a = pk2_t(vector = [.true., .false., .false., .true.], name = 'my second pk2 variable')
!
!- For string arrays, unfortunatly, fortran does not allow different character lengths for
!  array elements. 
!  Then, we have to make sure that all elements have the same number of characters:
!
   a = ['gfortran','ifort   ','nagfor  ']
!
!- Another way is to use an array of the DT str_t (defined in the module "str.f90")
!  that contains an allocatable character member:

   s(1) = 'gfortran' ; s(2) = 'ifort' ; s(3) = 'nagfor'
   a = s
   
   call a%PrintMe()
!
!- 3) Matrix assignments:
!
   a = reshape([1, 2, 3, 4, 5, 6],[2,3])
   a = reshape([1.1, 2.2, 3.3, 4.4, 5.5, 6.6],[2,3])
   a = reshape([.true., .false., .false., .true.],[2,2])
   
   sp(1,1) = 'Emma' ; sp(1,2) = 'Peel' ; sp(2,1) = 'John' ; sp(2,2) = 'Steed'
   a = pk2_t(sp, name = 'The Avengers')
   
   call a%PrintMe()
!
!- 4) Initializing (to a given type and to a given shape, name is optional):
!
   c = pk2_t (typ = ITYP, shape = [2,3], name = 'C') ! an integer 2x3 array of zeros 
   c = pk2_t (typ = RTYP, shape = [2,3], name = 'C') ! a  real    2x3 array of zeros
   c = pk2_t (typ = CTYP, shape = [2,3], name = 'C') ! a  complex 2x3 array of zeros
   c = pk2_t (typ = LTYP, shape = [2,3], name = 'C') ! a  boolean 2x3 array of .false.
   c = pk2_t (typ = STYP, shape = [2,3], name = 'C') ! a  string  2x3 array of empty strings  
!
!- 5) Copying a variable into another one: 
!
   b = reshape( ['Linda  ', 'Thorson'], [1,2] ) ! a 1x2 array of strings

   write(*,'(/,a)')'Copy b into c (it copies the matrix b but it does not erase the name of c):'
   
   c = b   
      
   call c%PrintMe()
!
!- 6) Inserting a variable into another one:
!
   write(*,'(/,a)')'Insert b (=["Linda","Thorson"]) into a at the row ("r") #3:'
   
   call b%InsertInto ( a, 3, "r" ) ! insert the 1x2 array b into a at the row #3
                                   ! (a becomes a 3x2 array)
   
   call a%PrintMe()
!
!- 7) Extracting a sub-matrix 
!     (using bound procedure ExtracSubMat or extracmat function of the pk2f module)
!   
   write(*,'(/,a)')'Extract the 1st column of a and put the result in b:'
   !call b%ExtracSubMat ( A, [1,2,3] )
   b = extracmat ( A, [1,2,3] )
   
   call b%PrintMe()

   write(*,'(/,a)')'Extract from a the sub-array [2:3][1:2] and put the result in b'   
   !call b%ExtracSubMat ( A, [2,3], [1,2] )  
   b = extracmat ( A, [2,3], [1,2] )

   call b%PrintMe()
!
!- 8) Setting a sub-matrix:
!
   b = reshape(['Mike  ', 'Gambit'],[1,2])

   write(*,'(/,a,/,a)')'Copy b = ["Mike" , "Gambit"] into a at the indices (3,1) and (3,2)', &
                       '(overwrite the 3rd row of a [Linda,Thorson] by [Mike,Gambit]):'
   call a%SetSubmat ( b, [3], [1,2] ) ! overwrite the 3rd row of A
   
   call a%PrintMe()
!
!- 9) Merging arrays (call to mergemats of the pk2f module):
!
   a = reshape([11,21,31,12,22,32],[3,2]) ; call a%PrintMe(msg='Let A be: ',form='values')
   b = reshape([13,23,33],[3,1])          ; call b%PrintMe(msg='and B be: ',form='values')
   c = reshape([41,42,43],[1,3])          ; call c%PrintMe(msg='and C be: ',form='values')
   d = [14,24,34,44]                      ; call d%PrintMe(msg='and D be: ',form='values')
   
   write(*,'(/,a)')'Then the matrix [ a , b ; c ] is'
   e = mergemats ( [a,b,c], ',;' )   ; call e%printMe(form='values')
      
   write(*,'(/,a)')'and the matrix [ [a , b ; c] , d ] is'
   e = mergemats ( [e,d], ',' )      ; call e%printMe(form='values')   

   ! Note: the comma (,) is the column separator symbol, 
   !       the semi-colon (;) is the row separator symbol

   write(*,'(/,a)')'Terminated'

end program assign
