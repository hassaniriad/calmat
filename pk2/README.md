# pk2: A Fortran Library for <ins>P</ins>olymorphic ran<ins>k</ins> <ins>2</ins> Arrays

1. [What is pk2?](#whatis)  

2. [Operations on pk2_t variables](#oper)  
   2.1. [Setters: Assignment - Move allocation](#assign)  
   2.2. [Operators](#operators)  
   2.3. [Usual functions](#func)  
   2.4. [Some Matrix manipulations (Reshaping, Merging, Extracting, Inserting, ...)](#manip)  
   2.5. [Getters (from pk2_t to intrinsics): Copy - Move allocation - Pointer association](#access)  
   
3. [Interpreting / evaluating literal expessions](#interp)  

4. [Application: the calmat project](#app) 
 
5. [Installation / Compilation](#comp)  

6. [How to add user procedures?](#userproc)   
   6.1. [Writing the procedures in a text file](#writeproc)  
   6.2. [Creating the new library](#makelib)  
   6.3. [Compiling a user program](#compilprog)  
   6.4. [Calling user procedures from a user program](#callproc)  


7. [Examples](#examples)  

8. [TODO](#todo)  

## 1. What is pk2? <a name="whatis"></a>

The `pk2` module defines a derived type named `pk2_t` that can represent rank 2 arrays of any type (integer, real, complex, logical, string):  

```fortran
   type :: pk2_t
      class    (bk2_t), allocatable :: m
      character(len=:), allocatable :: name
      integer  (Ikind)              :: typ = EMPTY, nrow = 0, ncol = 0
      ...
   contains
      ....
   end type pk2_t
```
The polymorphic member `m` can be of type `ik2_t`, `rk2_t`, `ck2_t`, `lk2_t` or `sk2_t` and these DT (defined in the module `bk2` for <ins>b</ins>ase <ins>r</ins>ank <ins>2</ins>) contain an allocatable rank 2 array `v` of integer, real, complex, logical or `str_t`, respectively (`str_t` is a DT for string), for example:

```fortran
   type, extends(bk2_t) :: rk2_t
      real(Rkind), allocatable :: v(:,:)
   contains
      ...
   end type rk2_t
   ...
   type, extends(bk2_t) :: sk2_t
      type(str_t), allocatable :: v(:,:)
   contains
      ...
   end type sk2_t
```
where `str_t` is the DT (defined in the module `str`):

```fortran
   type :: str_t
      character(len=:), allocatable :: str
      ...
   contains
      ...
   end type str_t
```
The polymorphic member `m` (and also its member `v`) is hereafter called the *container*.  The integer members `nrow` and `ncol` correspond to the dimension of the container while `typ` defines its type and may have the value of the constants (defined in the `pk2Constants` module) `ITYP`, `RTYP`, `CTYP`, `LTYP`, `STYP` or `EMPTY` (for integer, real, complex, logical, string or empty).

The `pk2` module also implements a large number of methods and usual operators and functions.

It is then possible to use only (or almost only) this type of variable to perform simple calculations without worrying about the actual type of the result.

```fortran
   program foo
      use pk2mod_m
      type(pk2_t) :: A, B, C

      A = 1 ! A is a scalar integer (actually a 1x1 matrix)
      A = 'hello' ! now A becomes a 1x1 string
      A = [.true., .false.] ! now A is a logical vector
      A = reshape([1., 2., 3., 4., 5., 6.],[2,3]) ! now A is a real matrix
      A = reshape([(1,1),(2,0),(0,2),(4,3)],[2,2]) ! now A is a 2x2 matrix of complexes

      ! operators and usual functions are overloaded:
      B = sin(2*A + 1) ! a 2x2 complex matrix
      C = abs(A) >= 2  ! a 2x2 logical matrix

      A = ['hello','world'] ! A is now a 2x1 string matrix
      B = [' ','!']         ! B is now a 2x1 string matrix

      C = trans( A + trim(B) ) ! C is now a 1x2 matrix
   end program foo
```
The `pk2` library also contains an interpreter (`pk2Interpreter` module) which can be used to evaluate literal expressions involving `pk2_t` variables ([see section 3](#interp)).

An application of this interpreter, called **Calmat**, can evaluate in Matlab/Scilab-like syntax, an expression or a set of expressions involving Operations between matrices, vectors or scalars of any type ([see section 4](#app) and the [README file in calmat/](../README.md)).

+ **Modules organisation**
   - Global constants:
      + `kindParameters.f90`:
         Defines the kind parameters `Ikind` (for integers) and `Rkind` (for reals and complexes). Their default values can be modified during the compilation.
      + `pk2Constants.f90`: Some useful constants
   - Utilities:
      + `err.f90`: An error handler
      + `str.f90`: A DT for strings
      + `util_Strings.f90`: Some useful procedures for string manipulation
      + `util_Other.f90`: Other useful procedures
      + `LapackInterface.f90`: Generic interfaces for a small set of LAPACK routines
   - Main pk2 modules:
      + `bk2.f90`: Defines the base types for the container of a `pk2_t` variables
      + `pk2.f90`: Defines the `pk2_t` DT
      + `pk2f.90`: Functions acting on `pk2_t` variables
      + `pk2Interpreter.f90`: Adapted/modified from `interpreter.f90` (github.com/ivomarb/Fortran-Expression-Evaluator) to parse and evaluate any literal expression  involving rank 2 arrays
      + `pk2mod.f90`: all in one
      + `userfunc.f90`: a driver for user procedures
      + `util/addFun.f90`: a program for adding user procedures.

## 2. Operations on `pk2_t` variables <a name="oper"></a>
### 2.1. Setters: Assignment - Move allocation <a name="assign"></a>
#### 2.1.1. Assignment
Assignment is overloaded so we can use the symbol `=` to assign to a `pk2_t` variable the value of another `pk2_t` variable or the value of an intrinsic array of rank 0, 1 or 2:

```fortran
   real(Rkind), parameter :: r0 = 1.0, r1(*) = [2.0,3.0], &
                             r2(*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])

   type(pk2_t) :: A0, A1, A2, B

   A0 = r0
   A1 = r1
   A2 = r2

   B = A2
   ...
```
**Caution**: Note however, that using the defined assignment can be problematic for large matrices. Indeed, when the evaluation of the rhs is copied into the stack (as with ifort or ifx compilers), the stack may overflow. A workarround is to increase the stack (by using `ulimit` on linux and mac os) or to add appropriate compilation options for using the heap rather than the stack (with ifort and ifx: `-heap-arrays`).

A better solution to overcome this type of problem when dealing with large matrices is to use the generic subroutine `pk2_assign` instead of the defined assignment (it's less “user-friendly” but more efficient):

```fortran
   real(Rkind), parameter :: r0 = 1.0, r1(*) = [2.0,3.0], &
                             r2(*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])

   type(pk2_t) :: A0, A1, A2, B

   call pk2_assign ( lhs = A0, rhs = r0 )
   call pk2_assign ( lhs = A1, rhs = r1 )
   call pk2_assign ( lhs = A2, rhs = r2 )

   call pk2_assign ( lhs = B, rhs = A2 )
   ...
```
See also the few examples given in `pk2/tuto/assign.f90` and `pk2/tuto/assign2.f90`.

**Constraints**:
When copying an intrinsic array to a pk2_t:

 - the **rank** of this array must be 0, 1 or 2.
 - Its **kind** must corresponds to `int32` or `int64` for integer type, `real32` or `real64` for real and complex types and to the default one for logical (we can also use the kind parameters `Ikind` (for integer) and `Rkind` (for real and complex). These kind parameters are defined in the `kindParameters` module and can be chosen at compile time, see ([see section 5](#comp)).

#### 2.1.2. Move allocation <a name="movealloc"></a>
In some situations, the right-hand side member is no longer needed after a copy. In these cases, it is more efficient to use a transfer by move allocation rather than an assignment. This is what the procedure `pk2_movealloc` does to transer data between two `pk2_t` variables or from an intrinsic array to a `pk2_t` variable.

**Constraints**:
A move allocation from an intrinsic array to a `pk2_t` variable must meet the two constraints:  

- obviously, the array must be an **allocatable** array of **rank 2** (of integers, reals, complexes, logicals or of `str_t`).  
- For numeric types its **kind** parameter must be `Ikind` (for integer) or `Rkind` (for real or complex).

Example:

```fortran
   program ex_movealloc1
      use pk2mod_m
      implicit none
      real(Rkind), allocatable :: rmat(:,:)
      type(pk2_t)              :: A, B

      A%name = 'myA' ; B%name = 'myB'

      ! Let rmat be the matrix:
      rmat = reshape([1,2,3,4,5,6],[3,2])

      ! and let A contains an integer random n x n matrix:
      call s_randi ( imin=1_Ikind, imax=20_Ikind, n=3_Ikind, m=3_Ikind, res=A )

      ! to print a pk2_t variable we use %printMe:
      ! (note: all arg. are optionals. To print only the values pass: form='values')
      call A%printMe ( msg = 'The initial A variable:')

      ! 1) Move allocation from pk2_t to pk2_t:
      ! (note: moveName is optional. If true, to%name is overwritten by from%name)
      call pk2_moveAlloc ( from = A, to = B, moveName = .false. )

      call A%printMe ( msg = 'The A variable after moveAlloc(A,B):')
      call B%printMe ( msg = 'and the B variable: ')

      ! 2) Move allocation from an intrisic to a pk2_t:
      A%name = 'myNewA'

      call pk2_moveAlloc ( from = rmat, to = A )

      call A%printMe ( msg = 'The A variable after moveAlloc(rmat,A):')
      print '(/,a,g0)','Allocated(rmat): ',allocated(rmat)
   end program ex_movealloc1
```
The output of this program is, for example, the following:


```
   The initial A variable:
   name  : myA
   type  : integer
   size  : 3 x 3
   values:  4  18  16
           17   6  18
            1   4  20

   The A variable after moveAlloc(A,B):
   name  : (no name)
   type  : empty
   size  : 0 x 0

   and the B variable:
   name  : myB
   type  : integer
   size  : 3 x 3
   values:  4  18  16
           17   6  18
            1   4  20

   The A variable after moveAlloc(rmat,A):
   name  : myNewA
   type  : real
   size  : 3 x 2
   values: 1.00000  4.00000
           2.00000  5.00000
           3.00000  6.00000

   Allocated(rmat): F
```

### 2.2. Operators <a name="operators"></a>
In what follows, by a slight abuse of language, we say that a pk2_t variable is "scalar" when we actually mean that its container is a 1x1 matrix and that is "integer", "real", etc., when its container is.

When the corresponding binary operations `a op b` make sense (`a` and `b` may be two `pk2_t` variables or one of the two is of an intrinsic type, and `op` denotes an operator):

- `a + b` is used for addition or concatenation of `a` and `b`. Not used for logicals.
- `a - b` is used for subtraction. Not used for logicals and strings.
- `a * b` is the **matrix product**. Used for integer, real or complex matrices only.
- `a / b` is used for dividing. Not used for logicals and strings. At least `a` or `b` must be scalar (i.e. 1x1 matrix or a scalar of intrinsic type)
- `a ** b` is used for exponentiation. Not used for logicals and strings. At least `a` or `b` must be scalar (1x1 matrix or a scalar of intrinsic type). Restriction: if `a` is not a 1x1 matrix `b` must be integer.
- `a .m. b` is the element-wise multiplication. Not used for logicals and strings.
- `a .d. b` is the element-wise divide. Not used for logicals and strings.
- `a .p. b` is the element-wise exponentiation. Not used for logicals and strings.
- `a .and. b` is the boolean and condition. Used only for logicals
- `a .or. b` is the boolean or condition. Used only for logicals
- `a == b`, `a /= b` are used for equality and inequality relational operations. 
- `a > b`, `a < b`, `a >= b`, `a <= b` are used for relational operations. Not used for complexes, logicals and strings.
- `a .bslash. b` is used for linear solve (in least square sense for non square system). Not used for logicals and strings.

Moreover, the unary operators `-` and `.not.` are used for negation and logical negation, respectively.

**Caution**: remember that user-defined binary operators have the lowest precedence (see e.g. [this article of Steve Lionel](https://stevelionel.com/drfortran/2021/04/03/doctor-fortran-in-order-order/)), so use parentheses where appropriate. For example `a .m. b + c` will not be interpreted as `(a .m. b) + c` but as `a .m. ( b + c)`.


### 2.3. Usual functions <a name="func"></a>
Almost all built-in functions implemented in the module `pk2f` have a subroutine version. The name of one of them (actually called by the corresponding function) is the name of the function preceded by a `s_` (for example: `y = abs(x)` can be replaced by `call s_abs(x,y)` and `y = eye(3,3)` by `call s_eye(3,3,y)`).

At the present, the list of available built-in functions is the following:

+ Elementary functions:
   - abs, sqrt, mod
   - cos, sin, tan, acos, asin, atan, acosd, asind, atand
   - sinc (cardinal sine function)
   - sinh, cosh, tanh, asinh, acosh, atanh
   - exp, log, log10, erf, erfc, gamma, factor
   - heav (Heaviside), sign (signum)
   - real, conj, imag
   - ceil, floor, nint, anint, aint
   - min, max, mean, sum, prod, cross
   - all, any
+ Matrix generation and transformation:
   - zeros, falses, ones, eye, diag
   - rand, randi, randperm
   - magic
   - colon (linearly spaced numbers)
   - meshgrid
   - trans, transp, reshape, tril, triu
   - sort<sup>*</sup>
   - trim, num2str, str2num, part, replace, convstr
   - mergemats, extracmat
+ Queries:
   - size, length, numel
   - is\_symm, is\_skew
   - sizeof, typeof
   - find
+ Linear algebra:
   - det<sup>+</sup>
   - trace, dev (deviatoric part)
   - cov, svd<sup>+</sup>, eig<sup>+</sup>
   - lu<sup>+</sup>, lusolv<sup>+</sup>, inv<sup>+</sup>, mldivide<sup>+</sup>, poldec<sup>+</sup> (polar decomposition)
+ Interpolation:
   - inttrap (trapezoidal rule)
+ Other:
   - readmat, writemat
   - date, cputime, pwd

Note: a superscript <sup>+</sup> denotes functions/subroutines that calls Lapack/Blas libraries.

### 2.4. Some Matrix manipulations <a name="manip"></a>
+ **Reshaping**  
The generic subroutine `s_reshape` and its function version `reshape` allow to reshape the container of a `pk2_t` variable:

```fortran
    program ex_reshape
       use pk2mod_m
       integer(Ikind) :: idim(2)
       type   (pk2_t) :: A, Res, Pad, Dim

       A = reshape([1,2,3,4,5,6],[2,3])
       call A%printMe(msg = 'Let A = ', form = 'values')

       ! idim(1) * idim(2) must be equal to A%nrow * A%ncol
       ! (unless "pad" is present):

       idim = [3,2]
       call s_reshape ( a=A, dim=idim, res=Res  )  ! or Res = reshape (A,idim)
       call Res%printMe(msg = 'Then reshape(A,[3,2]) is: ',form='values')

       ! when "pad" is present it must be of the same type as "A"
       ! (and only its first element is used):

       idim = [4,3] ; Pad = 0
       call s_reshape ( a=A, dim=idim, res=Res, pad=Pad  )  ! or Res = reshape(A,idim,Pad)
       call Res%printMe(msg = 'and reshape(A,[4,3],Pad) is: ',form='values')

       ! the argument "dim" can also be a pk2_t variable:

       Dim = [3,2]
       call s_reshape ( a=A, dim=Dim, res=Res  )  ! or B = reshape(A,Dim)
    end program ex_reshape
```

The output of this example gives:

```
       Let A = 1  3  5
               2  4  6

       Then reshape(A,[3,2]) is: 1  4
                                 2  5
                                 3  6

       and reshape(A,[4,3],Pad) is: 1  5  0
                                    2  6  0
                                    3  0  0
                                    4  0  0
```


+ **Merging**  
The `s_mergemats` subroutine allows to merge arrays of compatible types (for example integer and complex matrices). This subroutine is implemented in the `pk2f` module as `pk2f_subMergemats` (interfaced to `s_mergmats`) and has the following interface:

```fortran
   SUBROUTINE pk2f_subMergemats ( matrs, delim, res )
      class    (pk2_t), intent(in    ) :: matrs(:)
      character(len=*), intent(in    ) :: delim
      class    (pk2_t), intent(in out) :: res
```

The `delim` character is a sequence of "," (column delimiter) and ";" (row delimiter).  This subroutine is called by its function version `mergemats`.

   Here is an example:
   
```fortran
   program ex_mergemats
      use pk2mod_m

      version1: block
         type(pk2_t) :: s1, s2, s3, s4, s5, a
         integer     :: i

         s1 = reshape([11,21,31,12,22,32,13,23,33]*1_Ikind,[3,3])
         s2 = reshape([14,24,34]*1_Ikind,[3,1])
         s3 = reshape([15,25,35,16,26,36]*1_Ikind,[3,2])
         s4 = reshape([41,42,43]*1_Ikind,[1,3])
         s5 = reshape([44,45,46]*1_Ikind,[1,3])

         call s1%printMe(msg='1) Let s1 = ',form='values')
         call s2%printMe(msg='   and s2 = ',form='values')
         call s3%printMe(msg='   and s3 = ',form='values')
         call s4%printMe(msg='   and s4 = ',form='values')
         call s5%printMe(msg='   and s5 = ',form='values')

         ! Form the matrix [s1,s2,s3 ; s4,s5]:

         a = mergemats( [s1,s2,s3,s4,s5], delim = ",,;," )
         ! or also: call s_mergemats( [s1,s2,s3,s4,s5], delim = ",,;,", a )

         call a%printMe(msg='then [s1,s2,s3 ; s4,s5] = ',form='values')

         s1 = reshape([('ifort',i=1,9)],[3,3])
         s2 = reshape([('gfortran',i=1,3)],[3,1])
         s3 = reshape([('nagfor',i=1,4)],[1,4])

         call s1%printMe(msg='2) Let s1 = ',form='values')
         call s3%printMe(msg='   and s2 = ',form='values')
         call s3%printMe(msg='   and s3 = ',form='values')

         a = mergemats( [s1,s2,s3], delim = ',;')

         call a%printMe(msg='then [s1,s2 ; s3] = ',form='values')
      end block version1

      version2: block
         ! same but with an array of pk2_t
         type(pk2_t) :: allS(3), a
         integer     :: i

         allS(1) = reshape([('ifort',i=1,9)],[3,3])
         allS(2) = reshape([('gfortran',i=1,3)],[3,1])
         allS(3) = reshape([('nagfor',i=1,4)],[1,4])

         a = mergemats( allS, delim = ',;')
      end block version2

   end program ex_mergemats
```
The output of this program is

```
   1) Let s1 = 11  12  13
               21  22  23
               31  32  33

      and s2 = 14
               24
               34

      and s3 = 15  16
               25  26
               35  36

      and s4 = 41  42  43

      and s5 = 44  45  46

      then [s1,s2,s3 ; s4,s5] = 11  12  13  14  15  16
                                21  22  23  24  25  26
                                31  32  33  34  35  36
                                41  42  43  44  45  46

   2) Let s1 = ifort  ifort  ifort
               ifort  ifort  ifort
               ifort  ifort  ifort

      and s2 = gfortran
               gfortran
               gfortran

      and s3 = nagfor  nagfor  nagfor  nagfor

      then [s1,s2 ; s3] = ifort   ifort   ifort   gfortran
                          ifort   ifort   ifort   gfortran
                          ifort   ifort   ifort   gfortran
                          nagfor  nagfor  nagfor  nagfor
```
+ **Extracting**  
To copy some elements or a sub-matrix of a `pk2_t` variable into another `pk2_t` we can use the generic subroutine `s_extracmat` or its function version `extracmat` of the module `pk2f`.
These procedures take the original `pk2_t` variable, a set of row indices and/or a set of column indices. These indices may be stored in integers rank-1 arrays or in logicals rank-1 arrays or may also be stored in `pk2_t` variables, as illustrated below.

```fortran
    program ex_extracmat
      use pk2mod_m
      implicit none
      type(pk2_t) :: A, subA

      A = reshape([11,12,13,14,15,16,17,18,19],[3,3]) * 1_Ikind
      call A%printMe ( msg = 'Let A = ', form = 'values' )

      ex1: block
         ! Version 1: Passing indices as integers or booleans:
         integer(Ikind) :: i

         ! extract an element at given indices (i,j):
         subA = extracmat ( A, [3_Ikind], [1_Ikind] )
         call subA%printMe ( msg = '    A(3,1) = ', form = 'values' )

         ! extract an element at a given major-column index:
         subA = extracmat ( A, [3_Ikind] )
         call subA%printMe ( msg = '    A(3) = ', form = 'values' )

         ! extract a set of elements given by their indices (i,j)
         subA = extracmat ( A, [1,3]*1_Ikind, [2,3]*1_Ikind )
         call subA%printMe ( msg = '    A([1,3],[2,3]) = ', form = 'values' )

         ! another way is to give boolean arrays (mixing is possible). For example:
         subA = extracmat ( A, [.true., .false., .true.], [.false.,.true.,.true.])
         call subA%printMe ( msg = '    A([T,F,T,],[F,T,T]) = ', form = 'values' )

         ! extract a set of elements given by their major-column indices:
         subA = extracmat ( A, [1,3,4]*1_Ikind )
         call subA%printMe ( msg = '    A([1,3,4]) = ', form = 'values' )

         ! extract contiguous elements:
         subA = extracmat ( A, [(i,i=2,3)], [(i,i=1,2)] )
         call subA%printMe ( msg = '    A(2:3,1:2) = ', form = 'values' )

         ! extract a given column (i = -1 means the whole column, j = -1 means the whole row):
         subA = extracmat ( A, [-1_Ikind], [2_Ikind] )
         call subA%printMe ( msg = '    A(:,2) = ', form = 'values' )

         ! all elements in column-major ordering (same as subA = reshape(A,[n,1] where n = numel(A)):
         subA = extracmat ( A, [-1_Ikind] )
         call subA%printMe ( msg = '    A(:) = ', form = 'values' )
      end block ex1

      ex2: block
         ! Version 2: Same things but passing indices as pk2 variables:
         type(pk2_t) :: rowIndx, colIndx

         rowIndx = 3_Ikind ; colIndx = 1_Ikind
         subA = extracmat ( A, rowIndx, colIndx )

         ! etc.
      end block ex2
   end program ex_extracmat
```
This program gives the following output:

```
   Let A = 11  14  17
           12  15  18
           13  16  19

       A(3,1) = 13

       A(3) = 13

       A([1,3],[2,3]) = 14  17
                        16  19

       A([T,F,T,],[F,T,T]) = 14  17
                             16  19

       A([1,3,4]) = 11
                    13
                    14

       A(2:3,1:2) = 12  15
                    13  16

       A(:,2) = 14
                15
                16

       A(:) = 11
              12
              13
              14
              15
              16
              17
              18
              19
```

+ **Inserting**  
   The generic `pk2_insert` subroutine is designed to insert any intrinsic matrix into the container of a `pk2_t` variable (of compatible type) or to insert the container of a `pk2_t` variable into another one. Here are some examples:
   
```fortran
    program ex_insert
      use pk2mod_m
      type   (pk2_t)              :: pToinsert, pHost
      integer(Ikind), allocatable :: iToinsert(:,:)

      ! pToinsert contains a real 2x4 matrix:
      pToinsert = reshape([11,21,12,22,13,23,14,24],[2,4])*1.0_Rkind
      ! pHost contains an integer 3x4 matrix:
      pHost = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])

      call pToinsert%printMe(msg='Let pToinsert = ',form='values')
      call pHost%printMe(msg='Let pHost = ',form='values')

      print '(/,a)','1) Insert pToinsert into pHost at the 2-nd row of pHost'
      ! (the result contains a real 5x4 matrix):

      call pk2_Insert(insert = pToinsert, into = pHost, at_row = 2_Ikind)

      call pHost%printMe(msg='pHost is now: ',form='values')

      ! iToinsert is an intrinsic integer 2x1 matrix:
      iToinsert = reshape([1,2],[2,1])

      print*
      call disp ('Let iToinsert = ', iToinsert)

      print '(/,a)','2) Insert iToinsert into pHost at the 3-rd column of pHost'
      ! (the container is a real 5x5 matrix):

      call pk2_Insert(insert = iToinsert, into = pHost, at_col= 3_Ikind)

      call pHost%printMe(msg='pHost is now: ',form='values')

      ! iToinsert is an intrinsic integer 7x1 matrix:
      iToinsert = -reshape([1,2,3,4,5,6,7],[7,1])

      print*
      call disp ('Let iToinsert = ', iToinsert)

      print '(/,a)','3) Insert iToinsert into pHost at the 4-th column of pHost'
      ! (the container is a real 5x6 matrix):

      call pk2_Insert(insert = iToinsert, into = pHost, at_col= 4_Ikind)

      call pHost%printMe(msg='pHost is now: ',form='values')
    end program ex_insert
```
 
   and the corresponding output is the following:

``` 
   Let pToinsert = 11.0000  12.0000  13.0000  14.0000
                   21.0000  22.0000  23.0000  24.0000

   Let pHost = 101  102  103  104
               201  202  203  204
               301  302  303  304

   1) Insert pToinsert into pHost at the 2-nd row of pHost:

   pHost is now: 101.000  102.000  103.000  104.000
                  11.000   12.000   13.000   14.000
                  21.000   22.000   23.000   24.000
                 201.000  202.000  203.000  204.000
                 301.000  302.000  303.000  304.000

   Let iToinsert = 1
                   2

   2) Insert iToinsert into pHost at the 3-rd column of pHost:

   pHost is now: 101.000  102.000  1.000  103.000  104.000
                  11.000   12.000  2.000   13.000   14.000
                  21.000   22.000      0   23.000   24.000
                 201.000  202.000      0  203.000  204.000
                 301.000  302.000      0  303.000  304.000

   Let iToinsert = -1
                   -2
                   -3
                   -4
                   -5
                   -6
                   -7

   3) Insert iToinsert into pHost at the 4-th column of pHost

   pHost is now: 101.000  102.000  1.000  -1.000  103.000  104.000
                  11.000   12.000  2.000  -2.000   13.000   14.000
                  21.000   22.000      0  -3.000   23.000   24.000
                 201.000  202.000      0  -4.000  203.000  204.000
                 301.000  302.000      0  -5.000  303.000  304.000
```
   Notes:  

  - Row insertion: If the number of columns of the variable to be inserted (in the above examples: `pToinsert` or `iToinsert`) is smaller than that of the host variable (`pHost`), each  inserted row is completed by 0 (by .false. for logicals and by empty character for strings). On the other hand, if it is greater, elements beyond this position are ignored (the number of columns in the host variable remains unchanged).  
  - Column insertion: The same applies for column insertion as illustrated in the example 2 and 3 below.


+ **Setting elements**  
   The elements of a given array can be copied into or added to the elements of another array at given entries by using the generic bound-procedure `setSubmat`, as illustrated in the following simple example:
   
```fortran
      program ex_setsubmat
         use pk2mod_m
         implicit none
         type(pk2_t) :: a, b
         
         a = reshape([1,2,3,4,5,6,7,8,9],[3,3],order = [2,1])
         b = reshape([10,100,1000,10000],[2,2],order = [2,1])

         call a%printMe(msg='Let a = ',form='values') ;
         call b%printMe(msg='and b = ',form='values') ;

         ! add the elements of b into a at i in {2,3} and j in {1,3}:
         call a%SetSubmat(b,[2,3],[1,3], add=.true.)

         call a%printMe(msg='Then a%SetSubmat(b,[2,3],[1,3], add=.true.)',form='values')
      end program ex_setsubmat
```
   The output of this program is

```
      Let a = 1  2  3
              4  5  6
              7  8  9

      and b =   10    100
              1000  10000

      Then a%SetSubmat(b,[2,3],[1,3], add=.true.) =    1  2      3
                                                      14  5    106
                                                    1007  8  10009
```
+ **Deleting Row/Column**  
   The bound-procedure `delRows` deletes some rows or some columns or a given set of elements. Its first (input) argument, `indx`, is a rank-1 integer array which gives the indices of the rows or columns or elements to delete. The second (input) argument, `opt`, is a character(1) with three possible values: `"r"` (for row), `"c"` (for column) or `"e"` (for element).

   The following example illustrates this.
   
```fortran
     program ex_delrows
       use pk2mod_m
   	    type(pk2_t) :: a, a0

        a0 = reshape([11, 21, 31, 12, 22, 32 ,13, 23, 33]*1_Ikind,[3,3])

        a = a0   
        ! 1) Deleting rows 1 and 3 of the 3x3 matrix:
        call a%printMe ( msg = 'Let a = ', form = 'values' ) 
        call a%DelRows ( opt = 'r', indx = [1_Ikind,3_Ikind] )
        call a%printMe ( msg = 'then a%DelRows([1,3],"r") gives : ', form = 'values') 
   
        a = a0
        ! 2) Deleting columns 1 and 3 of the 3x3 matrix:
        call a%printMe ( msg = 'Let a = ', form = 'values' ) 
        call a%DelRows ( opt = 'c', indx = [1_Ikind,3_Ikind] )
        call a%printMe ( msg = 'then a%DelRows([1,3],"c") gives : ', form = 'values') 

        a = a0
        ! 3) Deleting elements 1, 3 and 5 (i.e. (1,1), (3,1) and (2,2) of the 3x3 matrix:
        call a%printMe ( msg = 'Let a = ', form = 'values' )
        call a%DelRows ( opt = 'e', indx = [1,3,5]*1_Ikind )
        call a%printMe ( msg = 'then a%DelRows([1,3,5],"e") gives : ', form = 'values')
   end program ex_delrows
```
   The output of this program is

```
    Let a = 11  12  13
            21  22  23
            31  32  33

   then a%DelRows([1,3],"r") gives : 21  22  23

    Let a = 11  12  13
            21  22  23
            31  32  33
            
    then a%DelRows([1,3],"c") gives : 12
                                      22
                                      32
    Let a = 11  12  13
            21  22  23
            31  32  33
            
    then a%DelRows([1,3,5],"e") gives : 21
                                        12
                                        32
                                        13
                                        23
                                        33
```
 
   Notes:

 - When `opt = "e"`, the elements are reshaped to a 1-column matrix (column-major ordering) and the rows given in `indx` are removed.
 - Indices out of range are ignored (thus if all indices in `indx` are out of range, the initial variable is unchanged).

### 2.5. Getters (from pk2_t to intrinsics): Copy - Move allocation - Pointer association <a name="access"></a>

If required by a user program, the container of a `pk2_t` can be retrieved from within an intrinsic array by a **copy**, a **move allocation** or by a **pointer association**.  

In all cases, we obviously have to know the **type** of the container (integer, real, complex, logical or of `str_t` type).


#### 2.5.1. Copy a `pk2_t` variable into an intrinsic array

The generic bound-procedure `%get` is used to copy the container of a `pk2_t` or one of its element into an intrinsic. Its use is subjected to the following rules:

 **Constraints**  
 
  - The copy must be **allocatable** and its **kind** has to be  `Ikind` for integer, `Rkind` for real or complex and of default type for logical and character.  
  - The **rank** of the copy must be **2**, **1** or **0**. If the rank is 1 (vector) the copy is done in major column order. If the rank is 0 (scalar) the indices of the element to be copied must be supplied to the `%get` procedure.  
  - An integer container can be copied into an integer, a real or a complex array. A real container can be copied into a real or complex array.

  Here are some examples:
  
```fortran
   program ex_copy
      use pk2mod_m
      implicit none
      integer(Ikind)              :: i1, i2, n
      integer(Ikind), allocatable :: iscal
      real   (Rkind), allocatable :: rmat(:,:), rvec(:)
      complex(Rkind), allocatable :: cvec(:)

      type   (pk2_t)              :: A, eigA

      ! Let A contains a given integer n x n matrix:
      i1 = 0 ; i2 = 10 ; n = 4
      A = randi ( imin=i1, imax=i2, n=n, m=n ) ! (or call s_randi(i1,i2,n,n,A))
      A = (A**2) - (3*A) + 1 + eye(n,n)

      ! copy the container of A into a real rank 2 array:
      call A % get ( rmat )

      ! copy the container of A into a real rank 1 array:
      call A % get ( rvec )

      ! copy the element A(i1,i2) into an integer:
      i1 = 2 ; i2 = 3
      call A % get ( i1, i2, iscal )

      ! Testing the type of a container before getting its values
      ! (note: ?TYP, EMPTY are constants defined in the pk2mod)

      call s_eig ( A, eigA )  ! (or eigA = eig(A)) compute the eigenvalues of A 

      if ( eigA % typ == RTYP ) then
         print '(/,a)','eigenvalues of A are reals'
         call eigA % get ( rvec )
         ! ... (do what you have to do with rvec)
      elseif ( eigA % typ == CTYP ) then
         print '(/,a)','eigenvalues of A are complexes'
         call eigA % get ( cvec )
         ! ... (do what you have to do with cvec)
      else
         ! ... (error)
      end if
      !...
   end program ex_copy
```

#### 2.5.4. Move allocation: from a `pk2_t` to an intrinsic array

When the rhs is no more needed, a best way (avoiding a deep copying) to get the container of a `pk2_t` variable is to use the generic subroutine `pk2_moveAlloc` (already seen in [section 2.1.2](#movealloc) to set a `pk2_t` from an intrinsic).

**Constraints**  

- The intrinsic array must be an **allocatable** array of **rank 2** of the same **type** of the container.
- For numeric types its **kind** parameter must be `Ikind` (for integer) or `Rkind` (for real or complex).

Example:

```fortran
   program ex_movealloc2
      use pk2mod_m
      implicit none
      integer(Ikind), allocatable :: imat(:,:)
      logical       , allocatable :: lmat(:,:)
      type   (pk2_t)              :: A, B

      ! Let A and B contain an integer random n x n matrix:
      A = randi ( imin=1_Ikind, imax=20_Ikind, n=3_Ikind, m=3_Ikind )
      B = A

      ! 1) Move allocation from a pk2_t to an intrinsic:
      call pk2_moveAlloc ( from = A, to = imat )

      ! 2) Move allocation with a type mismatch (error):
      call pk2_moveAlloc ( from = B, to = lmat )
   end program ex_movealloc2
```
This example produces the output:

```
   --> Error: Cannot move allocation from non-logical pk2 to logical array
   --> Returned by: pk2_moveallocPk2ToLmat

   --> ABORT (Halting Mode enabled)

   ERROR STOP
```

#### 2.5.5. Pointer association

Another way of accessing the container of a `pk2_t` variable is by pointer association which is the task of the
generic bound-procedure `%pointer`.


**Constraints**  

   - The pointer can be of **rank 0**, **1** or **2**. If the rank is **0** (scalar) the target value is the element (1,1) of the container (default) or the element given by its two indices (i,j). If the rank is **1**, the target values are the elements of the container arranged in major column order.
   - Through the pointer, the values of the target can be changed but cannot be resized (this would generate a runtime error).

Example:

```fortran
   program ex_pointer
      use pk2mod_m
      implicit none
      type   (pk2_t) :: A
      integer        :: i, j

      example1: block
         integer(Ikind), pointer :: iscal, ivec(:), imat(:,:)

         ! Let A contains an integer random n x n matrix:
         A = randi ( imin=0_Ikind, imax=200_Ikind, n=3_Ikind, m=4_Ikind )
         call A % printMe ( msg = 'Let A = ', form = 'values' )

         ! 1) Pointer to A(1,1):
         call A % pointer ( iscal )
         print '(/,a,i0)','pointer to A(1,1): ',iscal

         ! 2) Pointer to A(i,j):
         call A % pointer ( iscal, 2_Ikind, 3_Ikind )
         print '(/,a,i0)','pointer to A(2,3): ',iscal

         ! 3) Pointer to A(:):
         call A % pointer ( ivec )
         print '(/,a,*(i0,1x))','pointer to A(:): ',ivec

         ! 4) Pointer to A(:,:):
         call A % pointer ( imat )
         print '(/,a)','pointer to A(:,:): '
         do i = 1, size(imat,1)
            print '(*(i3,1x))',(imat(i,j),j=1,size(imat,2))
         end do

         !...

         ! Don't forger to nullify pointers:
         nullify(iscal,ivec,imat)
         !...
      end block example1

      example2: block
         type(str_t), pointer :: sscal, svec(:), smat(:,:)

         A = reshape(['Fortran','C++    ','Pascal ','Rust   ','BASIC  ','ALGOL  '],[2,3])
         A = trim(A)
         call A % printMe ( msg = 'Let A = ', form = 'values' )

         ! 1) Pointer to A(1,1):
         call A % pointer ( sscal )
         print '(/,a,a)','pointer to A(1,1): ',sscal%str

         ! 2) Pointer to A(i,j):
         call A % pointer ( sscal, 2_Ikind, 3_Ikind )
         print '(/,a,a)','pointer to A(2,3): ',sscal%str

         ! 3) Pointer to A(:):
         call A % pointer ( svec )
         print '(/,a,*(a,1x))','pointer to A(:): ',(svec(i)%str,i=1,size(svec))

         ! 4) Pointer to A(:,:):
         call A % pointer ( smat )
         print '(/,a)','pointer to A(:,:): '
         do i = 1, size(smat,1)
            print '(*(a7,1x))',(smat(i,j)%str,j=1,size(smat,2))
         end do

         ! Modify sscal (the element A(2,2)):
         sscal = 'COBOL'
         call A % printMe ( msg = 'A(2,2) has changed: A = ', form = 'values' )

         !...

         ! Don't forger to nullify pointers:
         nullify(sscal,svec,smat)
         !...
      end block example2
   end program ex_pointer
```

and the corresponding output is:

```
Let A =  70  175   84   53
        145   27  110  197
        192  181   73   65

pointer to A(1,1): 70

pointer to A(2,3): 110

pointer to A(:): 70 145 192 175 27 181 84 110 73 53 197 65

pointer to A(:,:): 
 70 175  84  53
145  27 110 197
192 181  73  65

Let A = Fortran  Pascal  BASIC
        C++      Rust    ALGOL

pointer to A(1,1): Fortran

pointer to A(2,2): Rust

pointer to A(:): Fortran C++ Pascal Rust BASIC ALGOL

pointer to A(:,:): 
Fortran  Pascal   BASIC
    C++    Rust   ALGOL

A(2,2) has changed: A = Fortran  Pascal  BASIC
                        C++      COBOL   ALGOL

pointer to A(:): Fortran C++ Pascal COBOL BASIC ALGOL
```

## 3. Interpreting / evaluating literal expessions <a name="interp"></a>
The excellent interpreter by Wilton P. Silva and Ivomar B. Soares ([see here](https://github.com/ivomarb/Fortran-Expression-Evaluator)) which parses and evaluates real scalar expressions involving real variables, has been modified/adapted to handle matrix expressions of any type, using Matlab-like syntax.
The resulting module is called `pk2Interpreter` and is the keystone of the calmat project (see [section 4](#app)).  

You can find in the directory `pk2/tuto` some program examples (files `using_interpreter*.f90`) showing how to use the `pk2Interpreter`.  

You could also consider using the higher-level library `calmat` for greater convenience.

## 4. Application: the `calmat` project <a name="app"></a>

Calmat is both a standalone program and a subroutine that a user code can call to evaluate an expression, a set of expresions or a script written in a language borrowed from Matlab/Scilab. For more informations, see the [README file of calmat](../README.md).

## 5. Installation / Compilation <a name="comp"></a>

The pk2 project is integrated in the calmat project. For cloning it:

```
git  clone  https://github.com/hassaniriad/calmat.git
```

### 5.1 Compiling the pk2 library (and the calmat project)
+ **Prerequisites**:  You will need a recent fortran compiler. At present, the compilers I have tested are:  
	- on macos: gfortran (>= 14), nagfor (>= Build 7213), ifort 2021.7.0
	- on linux: gfortran (>= 11.2.1), ifx (>= 2024.1.2)  
	
	(please note that several compiler bugs have been corrected, be sure you have the last release).

	I haven't tried compiling it on Windows and on macOS ARM, but if you do, I'd be very interested to know.

The makefile in the main `calmat` directory (`calmat/Makefile`) simply executes the makefile of the `pk2` library (located in `calmat/pk2/src` directory) and then runs the makefile of calmat (located in `calmat/src` directory).  

If you only want to compile the `pk2` library, just run the makefile in the `calmat/pk2/src` directory.  

These two makefiles are ready to use for ifort, ifx, gfortran and nagfor and are easy to adapt to another compiler. 


   To get help just type: `make`

+ **Setting the compiler name:** For compiling with ifort, gfortran or nagfor use the keyword `comp`, e.g.

   ```
         make comp=gfortran
   ```
+ **Setting options:** By default the flag `-O3` is used. You can use your own options by using the keyword `opt`. For example:

   ```
         make  comp=ifort  opt="-fast"
   ```
+ **Setting the kind parameters:** By default the kind parameters for integer (`Ikind`) and for real/complex (`Rkind`) are set, respectively, to `int32` and to `real64` (parameters of the `iso_fortran_env` module).  
Modify them with the flags `-DI32` (for `int32`)  or `-DI64` (for `int64`), `-DSP` (for `real32`) or `-DDP` (for `real64`) using the `kind` keyword. Example:

   ```
         make  comp=gfortran  kind="-DSP -DI32"
   ```

If the compilation is successful

- the `pk2` library (`libpk2.a` and `libpk2_d.so`) and the module files are located in `calmat/pk2/lib/$comp` and `calmat/pk2/mod/$comp`, respectively, where `$comp` is the name of the compiler used.
- the `calmat` library (`libcalmat.a` and `libcalmat_d.so`) and the module files are located in `calmat/lib/$comp` and `calmat/mod/$comp`, respectively.

  You can move them to a more appropriate directory if you wish or add the path of these libraries to your `LIBRARY_PATH`/`DYLD_LIBRARY_PATH` environment variable.

### 5.2 Compiling and linking your program with the pk2 library
You need to link your program against `pk2`, `LAPACK` and `BLAS` libraries and to add the path name of the module files of `pk2` (`-I` flag). Here are examples:

+ **If the static library is used** (`libpk2.a`), the compilation takes the form:

   ```
   ifort foo.f90 -I $modpk2 -L$libpk2 -lpk2 -llapack -lblas  -o foo 
   ```
   where `$libpk2` and `$modpk2` are the paths of the `pk2` libraries and module files.

   It may be shortened to: 

   ```
   ifort foo.f90 -lpk2 -llapack -lblas -o foo
   ```
   by setting the environment variable `LIBRARY_PATH`:
   
   ```
   export LIBRARY_PATH=/Users/me/pk2/lib/ifort
   ```
   and `CPATH` for module files (unfortunately, only ifort supports this):
   
   ```
    export CPATH=/Users/me/pk2/mod/ifort
   ```  
   
   
+ **If the shared library is used** (`libpk2_d.so`), the compilation takes the form:

   ```
   gfortran -lpk2_d -llapack -lblas -I $modcal -I $modpk2 -o foo foo.f90
   ```   

   provided that the environment variable `DYLD_LIBRARY_PATH` has also been set to:

   ```
   export DYLD_LIBRARY_PATH =/Users/me/calmat/lib/gfortran
   ```

## 6. How to add user procedures? <a name="userproc"></a>
Assume we want to add some procedures to the list of available functions of the pk2 library without without having to modify the `pk2` sources. 

#### 6.1. Writing the procedures in a text file <a name="writeproc"></a>
The first thing to do is to write (in very "basic" Fortran) theses new procedures into a text file (say `myfun.txt`) between the keywords `fortranproc` and `endfortranproc` and specifying "à la matlab" the input and output arguments as follows:

```fortran
$ cat myfun.txt

fortranproc [ out1, out2, ... ] = function_name ( in1, int2, in3, ... )
// Fortran code:
// declaration of input/output arguments (if any)
...
// declaration of local variables (if any)
...
// body of the function
endfortranproc function_name

fortranproc [ ... ] = an_other_function ( ...)
...
endfortranproc an_other_function
```

Remarks:

+ The file can contain any number of functions. 
+ The body of the function is given in very basic Fortran langage as illustrated in the examples given below.
+ The accepted types (don't worry about their kind, they'll be added automatically) for input/output arguments are: `integer`, `real`, `complexe`, `logical` and `type(str_t)` for string (don't use `character` except for local variables).
+ At present, output array argument must have an explicit shape (e.g. `y(3,2)`) or must be an allocatable.
+ By default `implicit none` will be used in the resulting Fortran module. All variables must therefore be declared.

Example:

```fortran
$ cat myfun.txt

fortranproc [ regular, x, detA, invA ] = solve3x3 ( A, b )
   real    :: A(3,3), b(3), x(3), invA(3,3), detA
   logical :: regular
!----------------------------------------------------------------------------------- 
! Solves the 3x3 linear system A*x = b using 3 cross products and 1 dot product
! Returns regular = .false. if the matrix is singular
! Computes also (if A is not singular) the inverse matrix
! If A is singular set an error message (via the error handler) and returns
!----------------------------------------------------------------------------------- 

!- local variables -----------------------------------------------------------------
   real, parameter :: TOLDET = 1e-10
!----------------------------------------------------------------------------------- 
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
!
!-   internal functions of solve3x3:
!
     function cross ( x, y ) result(z)
       real, intent(in) :: x(3), y(3)
       real             :: z(3)
       z(1) = x(2)*y(3) - x(3)*y(2)
       z(2) =-x(1)*y(3) + x(3)*y(1)
       z(3) = x(1)*y(2) - x(2)*y(1)
     end function cross

     function dot ( x, y ) result(s)
       real, intent(in) :: x(3), y(3)
       real,            :: s
       s = x(1)*y(1) + x(2)*y(2) + x(3)*y(3)
     end function dot
endfortranproc solve3x3	   
```

#### 6.2. Creating the new library <a name="makelib"></a>

Then by launching the program `addfun` (located in `bin/$comp`), e.g.

```
        addfun -i myfun.txt -o libmypk2
```

a new version of the `pk2` library (`libmypk2.a` or `libmypk2.so` if you use the option `-l dynamic`) that can be used in place of `libpk2.a` (or `libpk2_d.so`) is created.
 
 We can choose the destination directory by giving the path of this library, e.g.

```
       addfun -i myfun.txt -o mylibs/libmypk2
```
and, of course, we can safely use the path of the original `libpk2`, e.g. 

```
       addfun -i myfun.txt -o pk2/lib/$comp/libpk2
```

to replace it by this new version. 

What this program does exactly is rewrite the `userfunc` module, compile it and reform the library. 

Use `addfun -h` to get help.

Note that addfun creates also the module file `userfunc_m.mod` that can be needed during the compilation of your program (see below).  
By default this file is located in your current directory. You can specify its destination with the `-mdir` option.  
If you wish for example to overwrite the original one, enter:

```
addfun  -i myfun.txt  -o /Users/me/pk2/lib/$comp/libpk2  -mdir /Users/me/pk2/mod/$comp
```


#### 6.3. Compiling a user program <a name="compilprog"></a>

Now we can compile and link our program against this library, e.g.

```
$comp   myprog.f90   -I ../../mod/$comp/  libformyprog.a  -llapack -lblas  -o myprog.exe
```

Note: as mentioned above the module file `userfunc_m.mod` created by addfun can be needed by our program. This is the case when we wish to call our procedures without using the supplied driver (see below [§6.4](#callproc)).  
In that case, we can move `userfunc_m.mod` into `pk2/mod/$comp/` (see above [§6.2](#makelib)) or add another `-I` when compiling our program.

#### 6.4. Calling user procedures from a user program <a name="callproc"></a>
The program `addfun` records your procedures as subroutines and,  if the option `-onlysub` is not used, also as functions (of course only for procedures with at least one output argument). If you use the subroutine versions, add the prefix `s_` when calling them. 

Considere the previous example (definition of `solve3x3`)

##### a) Calling your procedures as a functions
Since a Fortran function returns a single variable, the first output argument (`regular` in the example) will be the result. The others (`x`, `detA`, `invA`) are `in/out` variables and follow the input variables in the argument list. The interface of the function `solve3x3` is then:

```fortran
function solve3x3 ( A, b, x, detA, invA ) result ( regular )
   type(pk2_t), intent(in    ) :: A, b
   type(pk2_t), intent(in out) :: x, detA, invA
   type(pk2_t)                 :: regular
end function solve3x3
```

and here is a user program that use this function:

```fortran
   ...
   type(pk2_t) :: A, b, x, reg, detA, invA
   ...
   A = reshape([1.,1.,2.,1.,2.,5.,2.,5.,14.],[3,3])
   call A % printMe ( msg = '. Let A = ', form = 'values')
   
   b = [9.,20.,54.]
   call b % printMe ( msg = '. and let b = ', form = 'values')
   
   reg = solve3x3 ( A, b, x, detA, invA ) 
   
   call x % printMe ( msg = '. Then the solution of A*x = b is: ', form = 'values')
   ...
```

##### b) Calling your procedure as a subroutine
In the subroutine version the output arguments follow simply the input ones.  
The interface of the corresponding subroutine `s_solve3x3` is then:

```fortran
subroutine s_solve3x3 ( A, b, regular, x, detA, invA )
   type(pk2_t), intent(in    ) :: A, b
   type(pk2_t), intent(in out) :: regular, x, detA, invA
end subroutine s_solve3x3
```

Thus the calling sequence is as follow:

```fortran
...
   type(pk2_t) :: A, b, x, reg, invA
   A = ...
   b = ...

   call s_solve3x3 ( A, b, reg, x, detA, invA )
```

##### c) Calling your procedure via the drivers
There are two drivers called `userProcDriver` (function version) and `s_userProcDriver` (subroutine version).  

The main advantage for using the drivers (compared to a direct call to your procedures) is:

- their interfaces do not change and are already known by `pk2`, so you do not need the `userfunc_m.mod` module file to compile your program
- you don't have to request all the outputs

The interfaces of these two drivers are:

```fortran
function userProcDriver ( fname, inputs, optOuts ) result ( firstOut )
   character(len=*),           intent(in    ) :: fname
   type     (pk2_t),           intent(in    ) :: inputs(:)
   type     (pk2_t), optional, intent(in out) :: optOuts(:)
   type     (pk2_t)                           :: firstOut
end function userProcDriver
```

and

```fortran
subroutine s_userProcDriver ( fname, inputs, firstOut, optOuts )
   character(len=*),           intent(in    ) :: fname
   type     (pk2_t),           intent(in    ) :: inputs(:)
   type     (pk2_t),           intent(in out) :: firstOut
   type     (pk2_t), optional, intent(in out) :: optOuts(:)
end subroutine s_userProcDriver
```
   
Example of use:

```fortran

   type(pk2_t) :: A, b, regular, opts(3)
   A = ...
   b = ...

   regular = userProcDriver ( fname = 'solve3x3', inputs = [A, b], optOuts  = opts )

   ! (opts(1:3) will contain x, detA and invA, respectively)
   
   ! If only x is requested:
   
   regular = userProcDriver ( fname = 'solve3x3', inputs = [A, b], optOuts = opts(1) )

   ! and if only the principal output (regular) is requested:

   regular = userProcDriver ( fname = 'solve3x3', inputs = [A, b] )
```

###### See the complete example in `pk2/tuto/adding_userFunctions.f90`

## 7. An example <a name="examples"></a>
A few other basic examples can be found in the `pk2/tuto` directory.

```fortran
program poisson2d
!- Example using the pk2 module to solve by FDM the 2d Poisson problem:
!
!     -laplacian(u) = f,   in  Omega = (0,1)x(0,1)
!                u  = g,   on  dOmega
!
!  We take here:  f(x,y) = (w1^2 + w2^2) * sin(w1*x) * cos(w2*y)
!                 g(x,y) = sin(w1*x) * cos(w2*y)
!
!  for which the exact solution is u(x,y) = sin(w1*x) * cos(w2*y).
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   ! Used pk2 variables:
   type   (pk2_t)            :: K, F, U                  ! the matrix, the rhs and the solution
   type   (pk2_t)            :: XY                       ! coordinates of the grid mesh
   type   (pk2_t)            :: Zero, X, Y, Xbnod, Ybnod ! auxiliary pk2 var.
   ! Auxiliary variables of intrinsic type:
   integer(Ikind), parameter :: nx = 71, i1 = 1, i2 = 2, i4 = 4
   integer(Ikind)            :: i, bnod(4*nx), n = nx*nx ! n: total number of nodes
   real   (Rkind)            :: a = 0.0, b = 1.0, w1 = 6.28, w2 = 6.28, h
!---------------------------------------------------------------------------------------------

   Zero = 0           ! the pk2 0
   h = (b-a) / (nx-1) ! mesh size (uniform grid with hx = hy = h)

!- Grid generation:
   X = [(a+(i-1)*h, i=1,nx)] ; XY = meshgrid ([X,X]) ! shape of XY is (nx*nx, 2) with X's in the 1st col.

!- The Matrix (penta-diagonal) of the 2d-laplacian:
   K = i4 * diag(ones(n,i1)) - diag(ones(n-i1,i1),-i1) - diag(ones(n-i1,i1),i1) &
                             - diag(ones(n-nx,i1),-nx) - diag(ones(n-nx,i1),nx)

!- The rhs: f = (w1^2 + w2^2) * sin(w1*x) * cos(w2*y):
   X = extracmat ( XY, [-i1], [i1] ) ! x coord. of the grid nodes (-1 means the whole col. 1)
   Y = extracmat ( XY, [-i1], [i2] ) ! y coord. of the grid nodes (-1 means the whole col. 2)                
   F = (h**2) * (w1**2 + w2**2) * sin(w1*X) .m. cos(w2*Y) ! note: ".m.": element-wise mult.

!- Insert the BC (adapt K and F):
!  boundary node #s 
   bnod = [ ( i    , n-nx+i, (i-1)*nx + 1, i*nx, i=1,nx ) ]
!            bottom, right , top         , left

!  boundary node coordinates:  
   Xbnod = extracmat ( XY, bnod, [i1] ) ! elements #bnod(:) of the 1st column of XY
   Ybnod = extracmat ( XY, bnod, [i2] ) ! elements #bnod(:) of the 2nd column of XY
 
!  prescribed values on the boundary (u = g = sin(w1*x) * cos(w2*y) on dOmega):
   U =  sin(w1*Xbnod) .m. cos(w2*Ybnod)  ! (note the element-wise mult)

!  Modify K and F:    
   call F % SetSubmat ( U, bnod, [i1] )              ! put U(Xbnod,Ybnod) on elt. #bnod(:) of F 
   call K % SetSubmat ( Zero, bnod, [(i,i=1,n)] )    ! put 0 on the rows #bnod(:) of K
   call K % SetSubmat ( eye(4*nx,4*nx), bnod, bnod ) ! put 1 on the diagonal elt. #bnod(:) of K

!- Solve the system (use the operator .bslah. or the function mldivide of pk2f):
   U = K .bslash. F  ! or  U = mldivide (K, F)
   
!- Print the result (in the form: X,Y,U,Uexact), using for example the pk2_writemat routine:
   call pk2_writemat &
        (                                                     &
        mergemats([XY,U,sin(w1*X) .m. cos(w2*Y)],delim=',,'), & ! the pk2 object to print
        fname = 'out_poisson2d',                              & ! name of the file
        size = .false.,                                       & ! do not print the dimensions (optional)
        format = '(4e13.5)',                                  & ! selected format (optional)
        title =                                               & ! set a header (optional)
        'Solution of -lap(u) = f, u = g on boundary \n'  //   & ! note: '\n' for new line
        'x            y            u            u_exact'      &
        )
end program poisson2d
```
## 8. TODO <a name="todo"></a>

- Extend `pk2_t` to sparse matrices. 
- Redesign the modules by using submodule feature 