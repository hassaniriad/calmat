! For compiling (or use the makefile):
!
! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran Check_pk2f.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_pk2f

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort Check_pk2f.f90 -L../../lib/ifort -lpk2 -llapack -lblas -o check_pk2f

! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor Check_pk2f.f90 -L../../lib/nagfor -lpk2 -llapack -lblas -o check_pk2f


program Check_pk2f_dec19
!
!- Test all functions of pk2f
!
   use, intrinsic :: ieee_arithmetic
   use pk2mod_m
   
   implicit none
   
   logical          :: verbos = .false.
   character(len=1) :: buf
   real             :: t1, t2, tp1, tp2
   integer          :: iter, niter = 20000
   
   call SignalHandler_SignalCatch (unit = STDOUT, title = '--> Check_pk2f_dec19:')

   call err_SetHaltingMode ( halting = .true., DisplayWarning = .false. ) ! stop if error    
   
   write(*,'(/,a,/,a)',advance = 'no') &
   'test with a long loop to check the memory usage? (no result displayed) (yes=return or "y")',&
   '(alternate: on mac os x use "leaks -atExit -- ./check_pk2f"): '
   
   read(*,'(a)') buf
   print*
   
   if (len_trim(buf) /= 0 .and. buf /= 'y') then
      verbos = .true. 
      niter = 1
   end if   
 
   call cpu_time(t1) ; tp1 = t1
       
   do iter = 1, niter
!
!-    Setting arrays of zeros, of ones, of randoms and unity:
!
      call test_ZEROS_ONES_RAND_EYE_MAGIC
!
!-    Compute min and max of arrays elements:
!    
      call test_MIN_MAX
!
!-    Compute the mean, sum and product of arrays elements:
!    
      call test_MEAN_SUM_PROD
!
!-    Get the upper-triangle and lower-triangle of an array:
!
      call test_TRIU_TRIL
!
!-    Get or Set the k-th diagonale of an array:  
!
      call test_DIAG
!
!-    Transpositions of an array
!
      call test_TRANS_TRANSP
!
!-    Compute the trace, determinant and eigenvalues and eigenvectors of square matrices   
!
      call test_TRACE_DET_EIG
!
!-    Inverse of a matrix and system solve:
!
      call test_INV_MLDIVIDE_LU
!
!-    Linearly spaced (integer or real) numbers:
!   
      call test_COLON
!
!-    Reshape an array:
!
      call test_RESHAPE
!
!-    Extraction of sub-array:
!
      call test_EXTRACMAT   
!
!-    Find indexes of an array corresponding to a given condition:
!
      call test_FIND 
!
!-    Merge arrays:
!
      call test_MERGEMATS 
!
!-    Conversion numerics to string and string to numerics:
!
      call test_NUM2STR_STR2NUM 
!
!-    Shape and length of an array
!
      call test_SIZE_NUMEL
!
!-    Testing the symmetry or the skew-symmetry 
!
      call test_ISSYMM_ISSKEW
!
!-    Cross product 
!
      call test_CROSS
!
!-    Read an array from a file
!
      call test_READMAT 
!
!-    Sort array elements
!
      call test_SORT
!
!-    Covariance matrices
!
      call test_COV_REGLIN
!
!-    SVD and polar decomposition
!
      call test_SVD_POLDEC
!
!-    LU factorization
!
      !call test_LU
          
      if (mod(iter,1000)==0) then
         call cpu_time(tp2)
         write(*,'(a,i0,a,i0,a,g0,a)',advance='no') "iterations #",iter," / ",niter, &
                                                    ". Partial cpu time: ",tp2-tp1,  &
                                                    " (hit return to continue) "
         call cpu_time(tp1)
         read*
      end if
   
   end do
   
   call cpu_time(t2)  
   print '(/,a,/,f12.5)','Ellapsed Time: ',t2-t1      

contains



!=============================================================================================   
   subroutine test_SVD_POLDEC
!=============================================================================================  

!---------------------------------------------------------------------------------------------    
!- Singular values decomposition and polar decomposition
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------
   type(pk2_t) :: A, U, S, V, R
!---------------------------------------------------------------------------------------------

!   
!- SVD
!  ---

   if (verbos) then
      print*
      print*,'****** Testing SVD *******'
      print*
   end if       
!
!- get the help: 
!   
   S = svd() ; call display(S,symb=' ')   
!
!- SVD of an integer matrix:
!   
   A = reshape([5,1,4 , 0,-5,9 , 3,7,8 , 7,3,10],[3,4])
   call display (A, msg = '. Let A')
   S = svd(A)
   call display (S, msg = '  then s = svd(A)')
   
   S = svd(A,U,V)
   if (verbos) print '(//,a)','  and S = svd(A,U,V) gives'
   call display (U, msg = '  U')
   call display (S, msg = '  S')
   call display (V, msg = '  V')
   call display (U * S * trans(V), msg = "  Verif.: . U*S*V'")
   call display (U * trans(U),     msg = "          . U*U'")
   call display (V * trans(V),     msg = "          . V*V'")
!
!- SVD of a real matrix:
!   
   A = reshape([5,1,4 , 0,-5,9 , 3,7,8 , 7,3,10],[3,4])*RONE
   call display (A, msg = '. Let A')
   S = svd(A)
   call display (S, msg = '  then s = svd(A)')
   
   S = svd(A,U,V)
   if (verbos) print '(//,a)','  and S = svd(A,U,V) gives'
   call display (U, msg = '  U')
   call display (S, msg = '  S')
   call display (V, msg = '  V')
   call display (U * S * trans(V), msg = "  Verif.: . U*S*V'")
   call display (U * trans(U),     msg = "          . U*U'")
   call display (V * trans(V),     msg = "          . V*V'")
!
!- SVD of a complex matrix:
!   
   A = reshape([5,1,4 , 0,-5,9 , 3,7,8 , 7,3,10],[3,4])*CONE + &
       reshape([2,0,7 , 0,1,-6 , 6,5,2 , 4,2,1 ],[3,4])*CIMAG
   call display (A, msg = '. Let A')
   S = svd(A)
   call display (S, msg = '  then s = svd(A)')
   
   S = svd(A,U,V)
   if (verbos) print '(//,a)','  and S = svd(A,U,V) gives'
   call display (U, msg = '  U')
   call display (S, msg = '  S')
   call display (V, msg = '  V')
   call display (U * S * trans(V), msg = "  Verif.: . U*S*V'")
   call display (U * trans(U),     msg = "          . U*U'")
   call display (V * trans(V),     msg = "          . V*V'")

!   
!- Poldec
!  ------

   if (verbos) then
      print*
      print*,'****** Testing PolDec *******'
      print*
   end if       
!
!- get the help: 
!   
   S = poldec() ; call display(S,symb=' ')   
!
!- Polar decomposition of an integer matrix:
!      
   A = reshape([5,1,4 , 0,-5,9 , 3,7,8 ],[3,3])
   call display (A, msg = '. Let A')
   R = poldec(A)
   call display (R, msg = '  then R = poldec(A)') 
   R = poldec(A,U)
   if (verbos) print '(//,a)','  and R = poldec(A,U) gives'
   call display (R, msg = '  R')
   call display (U, msg = '  U')
   call display (norm(R*U-A),             msg = "  Verif.: . || R*U-A ||")
   call display (norm(R*trans(R)-eye(R)), msg = "          . || R*R'-I || ")
   call display (norm(U*U-trans(A)*A),    msg = "          . || U^2-A'A || ")
!
!- Polar decomposition of a real matrix:
!      
   A = reshape([5,1,4 , 0,-5,9 , 3,7,8 ],[3,3])*RONE
   call display (A, msg = '. Let A')
   R = poldec(A)
   call display (R, msg = '  then R = poldec(A)') 
   R = poldec(A,U)
   if (verbos) print '(//,a)','  and R = poldec(A,U) gives'
   call display (R, msg = '  R')
   call display (U, msg = '  U')
   call display (norm(R*U-A),             msg = "  Verif.: . || R*U-A ||")
   call display (norm(R*trans(R)-eye(R)), msg = "          . || R*R'-I || ")
   call display (norm(U*U-trans(A)*A),    msg = "          . || U^2-A'A || ")
!
!- Polar decomposition of a complex matrix:
!      
   A = reshape([5,1,4 , 0,-5,9 , 3,7,8],[3,3])*CONE + &
       reshape([2,0,7 , 0,1,-6 , 6,5,2],[3,3])*CIMAG
   call display (A, msg = '. Let A')
   R = poldec(A)
   call display (R, msg = '  then R = poldec(A)') 
   R = poldec(A,U)
   if (verbos) print '(//,a)','  and R = poldec(A,U) gives'
   call display (R, msg = '  R')
   call display (U, msg = '  U')
   call display (norm(R*U-A),             msg = "  Verif.: . || R*U-A ||")
   call display (norm(R*trans(R)-eye(R)), msg = "          . || R*R'-I || ")
   call display (norm(U*U-trans(A)*A),    msg = "          . || U^2-A'A || ")
   
              
   end subroutine test_SVD_POLDEC


!=============================================================================================   
   subroutine test_COV_REGLIN
!============================================================================================= 
 
!---------------------------------------------------------------------------------------------    
!- Covariance and (simple) linear regression
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------
   type(pk2_t) :: A, B, C, coefa, coefb, sigres
!---------------------------------------------------------------------------------------------

!- Disable halting after an error is detected in any procedures of the pk2 module.

   call err_SetHaltingMode ( halting = .false. )       
!   
!- Covariance
!  ----------

   if (verbos) then
      print*
      print*,'****** Testing COV *******'
      print*
   end if       
!
!- get the help: 
!   
   c = cov() ; call display(c,symb=' ')   
!
!- covariance of an empty array (cov = NaN):
!   
   A = pk2_t()
   call display (A, msg = '. Let a')
   c = cov(a)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(a)')
   else
      call opflag%display()
   end if         
!
!- covariance of a scalar (cov = 0):
!   
   A = 1
   call display (A, msg = '. Let a')
   c = cov(a)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(a)')
   else
      call opflag%display()
   end if         
!
!- covariance of a real vector (cov = var)
!   
   A = reshape([5,1,4,7,3,10],[1,6])
   call display (A, msg = '. Let a')
   c = cov(a)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(a)')
   else
      call opflag%display()
   end if        
!
!- covariance of a complex vector (cov = var)
!   
   B = reshape([-5,2,8,-7,3,0],[1,6])*CIMAG
   A = A + B
   call display (A, msg = '. Let a')
   c = cov(a,w=IONE)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(a)')
   else
      call opflag%display()
   end if        
!
!- covariance of a real matrix:
!   
   A = reshape([5,1,4 , 0,-5,9 , 3,7,8 , 7,3,10],[3,4])
   call display (A, msg = '. Let A')
   C = cov(A)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(A)')
   else
      call opflag%display()
   end if     
!
!- covariance of a complex matrix:
!   
   A = reshape([5,1,4 , 0,-5,9 , 3,7,8 , 7,3,10],[3,4])
   B = reshape([2,3,4 ,-1,-5,0 , 6,7,4 ,-2,5,11],[3,4])*CIMAG
   A = A + B
   call display (A, msg = '. Let A')
   C = cov(A)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(A)')
   else
      call opflag%display()
   end if        
!
!- covariance of two scalars (cov = zeros(2,2)):
!   
   A = 3
   B = 6
   call display (A, msg = '. Let A')
   call display (B, msg = '  and B')
   C = cov(A,B)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(A,B)')
   else
      call opflag%display()
   end if     
!
!- covariance of two real vectors (cov is a 2x2 matrix)
!   
   A = [3, 6, 4]
   B = [7, 12, -9]
   call display (A, msg = '. Let A')
   call display (B, msg = '  and B')
   C = cov(A,B)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(A,B)')
   else
      call opflag%display()
   end if     
!
!- covariance of two complex vectors (cov is a 2x2 matrix)
!   
   A = [3, 6, 4] + [1,0,2]*CIMAG
   B = [7, 12, -9]+[4,5,6]*CIMAG
   call display (A, msg = '. Let A')
   call display (B, msg = '  and B')
   C = cov(A,B)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(A,B)')
   else
      call opflag%display()
   end if     
!
!- covariance of two matrices (treated as two vectors):
!   
   A = reshape([2,3,0,4,-9,1],[2,3])
   B = reshape([5,-4,2,4,6,9],[2,3])
   call display (A, msg = '. Let A')
   call display (B, msg = '  and B')
   C = cov(A,B)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(A,B)')
   else
      call opflag%display()
   end if     
!
!- Intentional error: two incompatible matrices:
!   
   A = reshape([2,3,0,4,-9,1],[2,3])
   B = reshape([2,3,0,4],[2,2])
   call display (A, msg = '. Let A')
   call display (B, msg = '  and B')
   if (verbos) print*,' (INTENTIONAL ERROR)'
   C = cov(A,B)
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov(A,B)')
   else
      if (verbos) then
         call opflag%display()
      else
         opflag = err_t() ! reset opflag (as opflag%display is not called)
      end if
   end if     
!
!- covariance of two matrices (with the wrapped version):
!      
   A = reshape([2,3,0,4,-9,1],[2,3])
   B = reshape([5,-4,2,4,6,9],[2,3])
   call display (A, msg = '. Let A')
   call display (B, msg = '  and B')
   C = cov([A,B])
   if (opflag%code <= 0) then   
      call display (c, msg = '  then cov([A,B])')
   else
      call opflag%display()
   end if        
!
!- Linear regression:
!  -----------------

   if (verbos) then
      print*
      print*,'****** Testing REGLIN *******'
      print*
   end if    
!
!- get the help: 
!
   c = reglin() ; call display(c,symb=' ')   
!
!- set A = -30:30 and B = A.^3:
!      
   A = colon(-30_Ikind,30_Ikind)
   B = A .p. 3_Ikind
   if (verbos) print '(//,a)','. Let x = -30:30 and y = x.^3'
   coefa = reglin(A,B,coefb,sigres)
   if (opflag%code <= 0) then   
      call display (coefa , msg = '  then a')
      call display (coefb , msg = '       b')
      call display (sigres, msg = '       sig')
   else
      call opflag%display()
   end if        

   call err_SetHaltingMode ( halting = .true., DisplayWarning = .false. ) ! stop if error    

   end subroutine test_COV_REGLIN
      
!=============================================================================================   
   subroutine test_SORT
!=============================================================================================  

!---------------------------------------------------------------------------------------------    
!- Sort array elements
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------
   type   (pk2_t)              :: A, B, I, J, Names, FNames, Birth, Death
   type   (pk2_t)              :: BI
   integer(Ikind)              :: dim
   type   (str_t)              :: s(8,4)
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing SORT *******'
      print*
   end if    
!
!- get the help: 
!   
   BI = sort()
   call display(BI,symb=' ')   

   if (verbos) write(*,'(/,a)')'Sorting a vector:'
      
   A = reshape([10,-1,3,2,9,8,3,15,4,6,5,-2],[1,12])
   
   call display (A, msg = 'Let A')
   
   ! return only the sorted array (B) in increasing order:
   B = sort(A,ord='i')
   if (opflag%code <= 0) then
      call display(B,msg = ". then B=sort(A,'i')")
   else
      call opflag%display()
   end if     

   ! return only the sorted array (B) in decreasing order:
   B = sort(A,ord='d')
   if (opflag%code <= 0) then
      call display(B,msg = ". then B=sort(A,'d')")
   else
      call opflag%display()
   end if     

   ! same but return also the original indices:
   B = sort(A,ord='d',ind=J)
   if (opflag%code <= 0) then
      call display(J,msg = ". and the initial indices are J")
      I = 1_Ikind
      call display(extracmat(A,I,J)==B,msg = ". verif. B == A(1,J)? :")
   else
      call opflag%display()
   end if     

   A = reshape(['|ba g|','|abgu|','|BaG |','|gabu|','|guAb|','|GAB |','|ba_g|','|abG |'],[1,8])
   
   call display (A, msg = 'Let A')
   
   ! return only the sorted array (B) in increasing order:
   B = sort(A,ord='i')
   if (opflag%code <= 0) then
      call display(B,msg = ". then B=sort(A,'i')")
   else
      call opflag%display()
   end if     

   ! return only the sorted array (B) in increasing order and case insensitive:
   B = sort(A,ord='i',caseinsens=.true.)
   if (opflag%code <= 0) then
      call display(B,msg = ". and B=sort(A,'i',caseinsens=.true.)")
   else
      call opflag%display()
   end if        
   
   if (verbos) write(*,'(/,a)')'Sorting a matrix:'

   A = reshape([10,-1,3,2,9,8,3,15,4,6,5,-2],[4,3])
   
   call display (A, msg = 'Let A')

   ! return only the sorted columns (default) of A in increasing order:
   B = sort(A,ord='i')
   if (opflag%code <= 0) then
      call display(B,msg = ". then sort(A,'i',dim=1)")
   else
      call opflag%display()
   end if     

   ! return only the sorted rows of A in increasing order:
   dim = 2
   B = sort(A,ord='i',dim=dim)
   if (opflag%code <= 0) then
      call display(B,msg = ". then B=sort(A,'i',dim=2)")
   else
      call opflag%display()
   end if     


   A = reshape([(1,2),(0,1),(1,1),(-1,2),(-1,1),(1,0),(3,-1),(-2,2)],[4,2])
   
   call display (A, msg = 'Let A')

   ! return only the sorted columns (default) of A in increasing magnitude order:
   B = sort(A,ord='i')
   if (opflag%code <= 0) then
      call display(B,msg = ". then sort(A,'i',dim=1)")
   else
      call opflag%display()
   end if         
   
   ! return only the sorted rows of A in increasing magnitude order:
   B = sort(A,ord='d',dim=2_Ikind)
   if (opflag%code <= 0) then
      call display(B,msg = ". then sort(A,'d',dim=2)")
   else
      call opflag%display()
   end if      

   s(1,1)%str = 'Descartes' ; s(1,2)%str = 'Rene' ; s(1,3)%str = '1596' ; s(1,4)%str = '1650'
   s(2,1)%str = 'Huygens'   ; s(2,2)%str = 'Christiaan' ; s(2,3)%str = '1629' ; s(2,4)%str = '1695'
   s(3,1)%str = 'Newton'    ; s(3,2)%str = 'Isaac'; s(3,3)%str = '1643' ; s(3,4)%str = '1727'
   s(4,1)%str = 'Leibniz'   ; s(4,2)%str = 'Gottfried'  ; s(4,3)%str = '1646' ; s(4,4)%str = '1716'
   s(5,1)%str = 'Euler'     ; s(5,2)%str = 'Leonhard'; s(5,3)%str = '1707' ; s(5,4)%str = '1783'
   s(6,1)%str = 'Coulomb'   ; s(6,2)%str = 'Charles-Augustin'; s(6,3)%str = '1736' ; s(6,4)%str = '1806' 
   s(7,1)%str = 'Laplace'   ; s(7,2)%str = 'Pierre-Simon'; s(7,3)%str = '1749' ; s(7,4)%str = '1827' 
   s(8,1)%str = 'Lagrange'  ; s(8,2)%str = 'Joseph-Louis' ; s(8,3)%str = '1736' ; s(8,4)%str = '1813'     

   A = s
   call display (A, msg = 'Let A')
   
   ! return only the sorted columns (default) of A in increasing order:
   B = sort(A,ord='i')
   if (opflag%code <= 0) then
      call display(B,msg = ". then sort(A,'i',dim=1)")
   else
      call opflag%display()
   end if         

   ! return only the sorted rows of A in increasing order:
   B = sort(A,ord='i',dim=2_Ikind)
   if (opflag%code <= 0) then
      call display(B,msg = ". then sort(A,'i',dim=2)")
   else
      call opflag%display()
   end if         

   ! return only the sorted rows of A in increasing order:
   B = extracmat(A,[-1_Ikind],[1_Ikind]) ! the 1st column of A (names)
   
   Names = sort(B,ord='i',ind=I) ! sort the names in alphabetical order
   
   J = 2_Ikind ; FNames = extracmat(A,I,J) ! put the other   
   J = 3_Ikind ; Birth  = extracmat(A,I,J) ! columns in   
   J = 4_Ikind ; Death  = extracmat(A,I,J) ! the same order
   
   B = mergemats([Names,FNames,Birth,Death],delim=',,,') ! reconstruct the array
   if (opflag%code <= 0) then
      call display(B,msg = ". The array A sorted by names")
   else
      call opflag%display()
   end if          
   
   end subroutine test_SORT

!=============================================================================================   
   subroutine test_READMAT
!============================================================================================= 

!---------------------------------------------------------------------------------------------    
!- Reading matrices
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t)              :: A, n
   type(pk2_t), allocatable :: args(:), vals(:)
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing READMAT *******'
      print*
   end if    
!
!- get the help:
!   
   A = readmat(help=.true.)
   call display(A,symb=' ')   
!
!- read in the format [ ... , ... ; ... ]:
!   
   if (verbos) write(*,'(/,a)')'reading foo1.txt:'

   A = readmat(file="files/matx/foo1.txt",comment="//")
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if     
! 
!- read in column-organized format the complete array (elements need to be all compatibles)
!   
   A = readmat(file="files/matx/foo2.txt",nrow=-1_Ikind,ncol=-1_Ikind)

   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if   
!
!- read in column-organized format the first 2 rows and 3 columns 
! 
   A = readmat(file="files/matx/foo4.txt",ncol=3_Ikind,nrow=2_Ikind)

   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if   
!
!- read in column-organized format and selecting columns:
!     
   A = readmat(file="files/matx/foo4.txt",col=[3_Ikind,1_Ikind],nrow=2_Ikind)

   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if   
!
!- read in column-organized format, with a known type:
!  
   A = readmat(file="files/matx/foo2.txt",ncol=3_Ikind,nrow=2_Ikind, typ='r')
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if  
!
!- read in column-organized format, with a known type:
!  
   A = readmat(file="files/matx/foo4.txt",ncol=-1_Ikind,nrow=2_Ikind, typ='s')
   if (opflag%code <= 0) then
      call display(A,'(with type="s"):')
   else
      call opflag%display()
   end if  
!
!- read in column-organized format, with a known type and selecting a set of columns
!  
   if (verbos) write(*,'(/,a)')'reading foo2.txt, knowing the type and selecting a set of columns:'

   A = readmat(file="files/matx/foo2.txt",col=[3_Ikind,1_Ikind],nrow=2_Ikind, typ='r')
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if  
!
!- read in column-organized format, with a conversion (to string)
!  
   if (verbos) write(*,'(/,a)')'reading 2 columns of foo4.txt and converted to string:'
   
   A = readmat(file="files/matx/foo4.txt",ncol=4_Ikind,nrow=2_Ikind, convert='s')
   if (opflag%code <= 0) then
      call display(A,'(converted to string)')
   else
      call opflag%display()
   end if  
!
!- read in column-organized format, selecting columns and with a conversion (to complexes)
!  
   if (verbos) &
   write(*,'(/,a)')'reading foo4.txt, selecting a set of columns and conversion to complexes:'
      
   A = readmat(file="files/matx/foo4.txt",col=[4,1,2]*IONE,nrow=2_Ikind, convert='c')
   if (opflag%code <= 0) then
      call display(A,'(converted to cmplx)')
   else
      call opflag%display()
   end if  
!
!- read in column-organized format until the eof, selecting columns and with a conversion (to logicals)
!  
   if (verbos) &
   write(*,'(/,a)')'reading foo5.txt, until eof, selecting a set of columns and conversion to logicals:'
   
   A = readmat(file="files/matx/foo5.txt",col=[7,6,3,4]*IONE,nrow=-1_Ikind, convert='l')
   if (opflag%code <= 0) then
      call display(A,'(converted to logicals)')
   else
      call opflag%display()
   end if  
      
!
!- Same things but with the wrapped version:
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   
   return

!
!- read in the format [ ... , ... ; ... ]:
!
   allocate(args(2)) ; allocate(vals(2)) ; n = size(args)
   args(1) = "file"                ; args(2) = "comment"  
   vals(1) = 'files/matx/foo1.txt' ; vals(2) =  '//'           

   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if      
!
!- read in column-organized format the complete array (elements need to be all compatibles)
!   
   allocate(args(3)) ; allocate(vals(3)) ; n = size(args)
   args(1) = "file"                ; args(2) = "ncol" ; args(3) = "nrow" 
   vals(1) = 'files/matx/foo2.txt' ; vals(2) = -1     ; vals(3) = -1    

   A = readmat([n,args,vals]) ; deallocate(args,vals)

   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if   
!
!- read in column-organized format the first 2 rows and 3 columns 
! 
   allocate(args(3)) ; allocate(vals(3)) ; n = size(args)
   args(1) = "file"                ; args(2) = "ncol" ; args(3) = "nrow" 
   vals(1) = 'files/matx/foo4.txt' ; vals(2) =  3     ; vals(3) = 2      
     
   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if      
!
!- read in column-organized format and selecting columns:
!  
   allocate(args(3)) ; allocate(vals(3)) ; n = size(args)
   args(1) = "file"                ; args(2) = "col" ; args(3) = "nrow" 
   vals(1) = 'files/matx/foo4.txt' ; vals(2) = [3,1] ; vals(3) = 2      
   
   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if   
!
!- read in column-organized format, with a known type:
!  
   allocate(args(4)) ; allocate(vals(4)) ; n = size(args)
   args(1) = "file"                ; args(2) = "ncol" ; args(3) = "nrow" ; args(4) = "typ"
   vals(1) = 'files/matx/foo2.txt' ; vals(2) = 3      ; vals(3) = 2      ; vals(4) = "r"

   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if  
!
!- read in column-organized format, with a known type:
!  
   allocate(args(4)) ; allocate(vals(4)) ; n = size(args)
   args(1) = "file"                ; args(2) = "ncol" ; args(3) = "nrow" ; args(4) = "typ"
   vals(1) = 'files/matx/foo4.txt' ; vals(2) = -1     ; vals(3) = 2      ; vals(4) = "s"

   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A,'(with type="s"):')
   else
      call opflag%display()
   end if  
!
!- read in column-organized format, with a known type and selecting a set of columns
!  
   if (verbos) write(*,'(/,a)')'reading foo2.txt, knowing the type and selecting a set of columns:'
   allocate(args(4)) ; allocate(vals(4)) ; n = size(args)
   args(1) = "file"                ; args(2) = "col" ; args(3) = "nrow" ; args(4) = "typ"
   vals(1) = 'files/matx/foo2.txt' ; vals(2) = [3,1] ; vals(3) = 2      ; vals(4) = "r"

   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if  
!
!- read in column-organized format, with a conversion (to string)
!  
   if (verbos) write(*,'(/,a)')'reading 2 columns of foo4.txt and converted to string:'
   allocate(args(4)) ; allocate(vals(4)) ; n = size(args)
   args(1) = "file"                ; args(2) = "ncol" ; args(3) = "nrow" ; args(4) = "convert"
   vals(1) = 'files/matx/foo4.txt' ; vals(2) = 4      ; vals(3) = 2      ; vals(4) = "s"
   
   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A,'(converted to string)')
   else
      call opflag%display()
   end if  
!
!- read in column-organized format, selecting columns and with a conversion (to complexes)
!  
   if (verbos) &
   write(*,'(/,a)')'reading foo4.txt, selecting a set of columns and conversion to complexes:'
   allocate(args(4)) ; allocate(vals(4)) ; n = size(args)
   args(1) = "file"                ; args(2) = "col"   ; args(3) = "nrow" ; args(4) = "convert"
   vals(1) = 'files/matx/foo4.txt' ; vals(2) = [4,1,2] ; vals(3) = 2      ; vals(4) = "c"
      
   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A,'(converted to cmplx)')
   else
      call opflag%display()
   end if  
!
!- read in column-organized format until the eof, selecting columns and with a conversion (to logicals)
!  
   if (verbos) &
   write(*,'(/,a)')'reading foo5.txt, until eof, selecting a set of columns and conversion to logicals:'
   allocate(args(4)) ; allocate(vals(4)) ; n = size(args)
   args(1) = "file"                ; args(2) = "col"     ; args(3) = "nrow" ; args(4) = "convert"
   vals(1) = 'files/matx/foo5.txt' ; vals(2) = [7,6,3,4] ; vals(3) =-1      ; vals(4) = "l"
   
   A = readmat([n,args,vals]) ; deallocate(args,vals)
   if (opflag%code <= 0) then
      call display(A)
   else
      call opflag%display()
   end if  

   end subroutine test_READMAT


!=============================================================================================   
   subroutine test_CROSS
!============================================================================================= 
 
!---------------------------------------------------------------------------------------------    
!- Cross product
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: x, y
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing CROSS *******'
      print*
   end if    
   
!  with integers   
   x = reshape([1,2,3]*IONE,[3,1])
   call display(x,'Let a')   
   y = reshape([0,1,2]*IONE,[3,1])
   call display(y,'and b')
   call display(cross(x,y),'then a x b')

   x = reshape([1,2,3,4,5,6]*IONE,[3,2])
   call display(x,'Let A')   
   
   y = reshape([0,1,2,3,4,5]*IONE,[3,2])
   call display(y,'and B')
   call display(cross(x,y),'then A x B')

!  with reals (or a mix)
   x = reshape([1,2,3]*RONE,[3,1])
   call display(x,'Let a')   
   y = reshape([0,1,2]*RONE,[3,1])
   call display(y,'and b')
   call display(cross(x,y),'then a x b')

   x = reshape([1,2,3,4,5,6]*IONE,[3,2])
   call display(x,'Let A')   
   
   y = reshape([0,1,2,3,4,5]*RONE,[3,2])
   call display(y,'and B')
   call display(cross(x,y),'then A x B')

!  with complexes (or a mix)
   x = reshape([1,2,3]*RONE,[3,1])
   x = x + CIMAG*x
   call display(x,'Let a')   
   y = reshape([0,1,2]*RONE,[3,1])
   call display(y,'and b')
   call display(cross(x,y),'then a x b')

   x = reshape([1,2,3,4,5,6]*(CONE+CIMAG),[3,2])
   call display(x,'Let A')   
   
   y = reshape([0,1,2,3,4,5]*RONE,[3,2])
   call display(y,'and B')
   call display(cross(x,y),'then A x B')
   
   end subroutine test_CROSS

!=============================================================================================   
   subroutine test_ISSYMM_ISSKEW
!============================================================================================= 
  
!---------------------------------------------------------------------------------------------    
!- Testing symmetry and asymmetry
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: x
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing ISSYMM and ISSKEW *******'
      print*
   end if    

!
!- ISSYMM
!   
   x = reshape([11,21,31,12,22,32]*IONE,[3,2])
   call display(x,'Let x')
   call display(is_symm(x),'then issymm(x)')

   x = reshape([11,21,31,21,22,32,31,32,33]*IONE,[3,3])
   call display(x,'Let x')
   call display(is_symm(x),'then issymm(x)')
   
   x = x * RONE
   call display(x,'Let x')
   call display(is_symm(x),'then issymm(x)')
   
   x = x + x*CIMAG
   call display(x,'Let x')
   call display(is_symm(x),'then issymm(x)')
   
   x = magic(3_Ikind) ; x = sin(x) + CIMAG*cos(x)
   x = x + trans(x)
   call display(x,'Let x')
   call display(is_symm(x),'then issymm(x)')

   x = reshape(['S11','S21','S31','S21','S22','S32','S31','S32','S33'],[3,3])
   call display(x,'Let x')
   call display(is_symm(x),'then issymm(x)')

   x = reshape(['S11 ','S21 ' ,'S31 ','S21 ','S22 ',' S32','S31 ','S32 ','S33 '],[3,3])
   call display(x,'Let x')
   call display(is_symm(x),'then issymm(x)')
   
!
!- ISSKEW
!         
   x = reshape([0,21,31,-21,0,32,-31,-32,0]*IONE,[3,3])
   call display(x,'Let x')
   call display(is_skew(x),'then isskew(x)')

   x = magic(4_Ikind)
   call display(x,'Let x')
   call display(is_skew(x),'then isskew(x)')

   x = x - trans(x)
   call display(x,'Let x')
   call display(is_skew(x),'then isskew(x)')

   x = magic(3_Ikind) ; x = sin(x) + CIMAG*cos(x)
   call display(x,'Let x')
   call display(is_skew(x),'then isskew(x)')

   x = x - trans(x)
   call display(x,'Let x')
   call display(is_skew(x),'then isskew(x)')
         
   end subroutine test_ISSYMM_ISSKEW

!=============================================================================================   
   subroutine test_SIZE_NUMEL
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!- Shape (size) and length (number of elements) of an array
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: x
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing SIZE and NUMEL *******'
      print*
   end if    
   
   x = reshape([11,21,31,12,22,32]*IONE,[3,2])
   call display(x,'Let x')
   call display(size(x),'then size(x)')
   call display(numel(x),'and numel(x)')
   
   end subroutine test_SIZE_NUMEL

!=============================================================================================   
   subroutine test_NUM2STR_STR2NUM
!=============================================================================================  
 
!---------------------------------------------------------------------------------------------    
!- Converts a number to a string and conversely (when possible)
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: x
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing NUM2STR and STR2NUM *******'
      print*
   end if    
!
!- NUM2STR   
!
   x = reshape([11,21,31,12,22,32,13,23,33]*IONE,[3,3])
   call display(x,'Let x')
   call display(num2str(x),'then num2str(x)')
   
   x = reshape([11,21,31,12,22,32,13,23,33]*(1.1_Rkind),[3,3])
   call display(x,'Let x')
   call display(num2str(x),'then num2str(x)')   
   
   x = (CONE+CIMAG)*x
   call display(x,'Let x')
   call display(num2str(x),'then num2str(x)')
   
   x = reshape([.true.,.false.,.true.,.false.],[2,2])
   call display(x,'Let x')
   call display(num2str(x),'then num2str(x)')
   
   x = reshape(['abc','de ','fgh','i  '],[2,2])
   call display(x,'Let x')
   call display(num2str(x),'then num2str(x)')

!
!- STR2NUM   
!
   x = reshape(['11  ',' 21 ','  31','  12','22  ','32  ',' 13 ','  23','33  '],[3,3])
   call display(x,'Let x')
   call display(str2num(x),'then str2num(x)')
   
   x = reshape(['11.1','21  ','31  ','12  ','22  ','32  ','13  ',' 23 ',' 3.3'],[3,3])
   call display(x,'Let x')
   call display(str2num(x),'then str2num(x)')
      
   end subroutine test_NUM2STR_STR2NUM
   
!=============================================================================================   
   subroutine test_MERGEMATS
!============================================================================================= 

!---------------------------------------------------------------------------------------------    
!- Merging a set of (type-compatible) matrices into a larger one using ',' and ';' as 
!  column/row delimiters
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: i1, i2, i3, i4, i5, r, c, l1, l2, l3, a
   integer     :: i
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing MERGEMATS *******'
      print*
   end if    
!
!- Merging integer arrays:
!
   i1 = reshape([11,21,31,12,22,32,13,23,33]*IONE,[3,3])
   i2 = reshape([14,24,34]*IONE,[3,1])
   i3 = reshape([15,25,35,16,26,36]*IONE,[3,2])
   i4 = reshape([41,42,43]*IONE,[1,3])
   i5 = reshape([44,45,46]*IONE,[1,3])
   call display(i1, msg='1) Let i1')  
   call display(i2, msg='   and i2')  
   call display(i3, msg='   and i3')  
   call display(i4, msg='   and i4')  
   call display(i5, msg='   and i5')  
   
   a = mergemats([i1,i2,i3,i4,i5],delim=',,;,')
   call display(a, msg='then [i1,i2,i3 ; i4,i5]')  
!
!- Merging real and integer arrays:
!
   r = i1 + 0.1_Rkind
   call display(r, msg='2) Let r')  
   a = mergemats([r,i2,i3,i4,i5],delim=',,;,')
   call display(a, msg='then [r,i2,i3 ; i4,i5]')  

!
!- Merging real, integer and complex arrays:
!
   c = i2 + CIMAG
   call display(c, msg='3) Let c')  
   a = mergemats([r,c,i3,i4,i5],delim=',,;,')
   call display(a, msg='then [r,c,i3 ; i4,i5]')  
!
!- Merging boolean arrays:
!
   l1 = diag ([.true.,.true.,.true.])
   call display(l1, msg='4) Let l1')  
   l2 = falses (n=3_Ikind,m=1_Ikind)
   call display(l2, msg='   and l2')  
   l3 = .not. falses (n=1_Ikind,m=4_Ikind)
   call display(l3, msg='   and l3')  
   a = mergemats([l1,l2,l3],delim=',;')
   call display(a, msg='then [l1,l2 ; l3]')  
!
!- Merging string arrays:
!   
   l1 = reshape([('ifort',i=1,9)],[3,3])
   call display(l1, msg='5) Let s1')  
   l2 = reshape([('gfortran',i=1,3)],[3,1])
   call display(l2, msg='   and s2') 
   l3 = reshape([('nagfor',i=1,4)],[1,4])
   call display(l3, msg='   and s3') 
   a = mergemats([l1,l2,l3],delim=',;')
   call display(a, msg='then [s1,s2 ; s3]')  
!
!- Merging with an empty array:
!      
   r = pk2_t()! or pk2_t(shape=[0,0],typ=EMPTY)
   call display(r, msg='6) Let v (empty)')  
   a = mergemats([l1,l2,r,l3,r],delim=',;,,')
   call display(a, msg='then [s1,s2 ; v, s3, v]')  
   
   end subroutine test_MERGEMATS
   

!=============================================================================================   
   subroutine test_EXTRACMAT
!=============================================================================================  

!---------------------------------------------------------------------------------------------    
!- Extracts elements or sub-array from a given array
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type   (pk2_t) :: a, suba, i_indx, j_indx
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing EXTRACMAT *******'
      print*
   end if    

   a = reshape([11,12,13,14,15,16,17,18,19],[3,3]) * IONE
   call display(a, msg='Let be the array A')  
!
!- Version 1) passing indices as integers or boolean:
!
!- an element at given indices (i,j):  
   suba = extracmat (a, [3_Ikind], [1_Ikind])
   call display(suba, msg='. get the element A(3,1)',symb=':')       

!- an element at a given major-column index:
   suba = extracmat (a, [3_Ikind])
   call display(suba, msg='. get the element A(3)',symb=':')    

!- set of elements given by their indices (i,j)      
   suba = extracmat (a, [1_Ikind,3_Ikind], [2_Ikind,3_Ikind])
   call display(suba, msg='. get the elements A([1,3],[2,3])',symb=':')   

!- Another way is to give i_indx and j_indx as boolean arrays. For example:
   suba = extracmat (a, [.true., .false., .true.], [.false.,.true.,.true.])
   call display(suba, msg='. get the elements A([T,F,T],[F,T,T])',symb=':')      

!- Mixing is also possible:
   suba = extracmat (a, [.true., .false., .true.], [2_Ikind,3_Ikind])
   call display(suba, msg='. get the elements A([T,F,T],[2,3])',symb=':')      
       
!- set of elements given by their major-column indices:      
   suba = extracmat (a, [1_Ikind,3_Ikind,4_Ikind])
   call display(suba, msg='. get the elements A([1,3,4])',symb=':')  

!- indices with using COLON:         
   suba = extracmat (a, [(i,i=2,3)], [(i,i=1,2)])
   call display(suba, msg='. get the sub-array A(2:3,1:2)',symb=':')  

!- extract a given column:
   suba = extracmat (a, [-1_Ikind], [2_Ikind])
   call display(suba, msg='. get the elements of column A(:,2)',symb=':')  

!- extract a given row:
   suba = extracmat (a, [2_Ikind], [-1_Ikind])
   call display(suba, msg='. get the elements of row A(2,:)',symb=':')  

!- all elements (result = a):
   suba = extracmat (a, [-1_Ikind], [-1_Ikind])
   call display(suba, msg='. get the elements A(:,:)',symb=':')  

!- all elements in column-major ordering (result = reshape(a,[n,1]):   
   suba = extracmat (a, [-1_Ikind])
   call display(suba, msg='. get the elements A(:)',symb=':')  
   
!
!- Version 2) Same things but passing indices as pk2 objects:
!
   if (verbos) print '(/a,/)','Same things but passing indices as pk2 objects:'
   
!- an element at given indices (i,j):  
   i_indx = 3_Ikind ; j_indx = 1_Ikind
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the element A(3,1)',symb=':')       
      
!- an element at a given major-column index:
   i_indx = 3_Ikind 
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the element A(3)',symb=':')    

!- set of elements given by their indices (i,j)      
   i_indx = [1_Ikind,3_Ikind] ; j_indx = [2_Ikind,3_Ikind]
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements A([1,3],[2,3])',symb=':')   

!- Another way is to give i_indx and j_indx as boolean arrays. For example:
   i_indx = [.true., .false., .true.] ; j_indx = [.false.,.true.,.true.]     
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements A([T,F,T],[F,T,T])',symb=':')      

!- The mix is also possible:
   i_indx = [.true., .false., .true.] ; j_indx = [2_Ikind,3_Ikind]     
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements A([T,F,T],[2,3])',symb=':')      
       
!- set of elements given by their major-column indices:      
   i_indx = [1_Ikind,3_Ikind,4_Ikind]
   suba = extracmat (a, i_indx)
   call display(suba, msg='. get the elements A([1,3,4])',symb=':')  

!- indices with using COLON:         
   i_indx = colon(2_Ikind,3_Ikind) ; j_indx = colon(1_Ikind,2_Ikind)
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the sub-array A(2:3,1:2)',symb=':')  

!- extract a gvien column:
   i_indx = -1_Ikind ; j_indx = 2_Ikind
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements of column A(:,2)',symb=':')  

!- extract a given row:
   i_indx = 2_Ikind ; j_indx = -1_Ikind
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements of row A(2,:)',symb=':')  

!- all elements (result = a):
   i_indx = -1_Ikind ; j_indx = i_indx
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements A(:,:)',symb=':')  

!- all elements in column-major ordering (result = reshape(a,[n,1]):   
   i_indx = -1_Ikind
   suba = extracmat (a, i_indx)
   call display(suba, msg='. get the elements A(:)',symb=':')   
   

! With real array:
   a = a * RONE
   call display(a, msg='Let be the array A')  
!- extract a gvien column:
   i_indx = -1_Ikind ; j_indx = 2_Ikind
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements of column A(:,2)',symb=':')     

! With complex array:
   a = (CONE+CIMAG)*a
   call display(a, msg='Let be the array A')  
!- extract a gvien column:
   i_indx = -1_Ikind ; j_indx = 2_Ikind
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements of column A(:,2)',symb=':')     
   
! With boolean array:
   a = a==a
   call display(a, msg='Let be the array A')  
!- extract a gvien column:
   i_indx = -1_Ikind ; j_indx = 2_Ikind
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements of column A(:,2)',symb=':')   
   
! With string array:
   a = reshape( ['one_one    ','two_one    ', 'three_one  ','one_two    ', &
                 'two_two    ','three_two  ', 'one_three  ','two_three  ', &
                 'three_three'],[3,3])
   call display(a, msg='Let be the array A')  
!- extract a given column:
   i_indx = -1_Ikind ; j_indx = 2_Ikind
   suba = extracmat (a, i_indx, j_indx)
   call display(suba, msg='. get the elements of column A(:,2)',symb=':')   
   

   end subroutine test_EXTRACMAT
   
   
!=============================================================================================   
   subroutine test_FIND
!============================================================================================= 
  
!---------------------------------------------------------------------------------------------    
!- Finds indexes of an array corresponding to a given condition 
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, suba, indx, jindx
   real(Rkind) :: x = 13.0_Rkind, y = 16.0_Rkind
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing FIND *******'
      print*
   end if    

!
!- 1) find indices of non-zero or non-false elements of an array:
!
   a = reshape([13,0,16,15,14,11],[2,3]) * IONE
   call display(a, msg='1) Let be the array A') 
   indx = find ( a )
   call display (indx, msg = 'indices (column-major ord.) where A ~= 0 are I') 
   
   suba = extracmat (a, indx)
   call display (suba, msg = 'the corresponding elements are (in column-major ord.) ') 
   
   a = a * RONE
   call display(a, msg='2) Let be the array A') 
   indx = find ( a )
   call display (indx, msg = 'indices (column-major ord.) where A ~= 0 are I') 
   
   suba = extracmat (a, indx)
   call display (suba, msg = 'the corresponding elements are (in column-major ord.) ') 
   
   a = a * CONE + a * CIMAG  
   call display(a, msg='3) Let be the array A') 
   indx = find ( a )
   call display (indx, msg = 'indices (column-major ord.) where A ~= 0 are I') 
   
   suba = extracmat (a, indx)
   call display (suba, msg = 'the corresponding elements are (in column-major ord.) ') 
   
   a = reshape([.true.,.false.,.true.,.true.,.true.,.true.],[2,3])
   call display(a, msg='4) Let be the array A') 
   indx = find ( a )
   call display (indx, msg = 'indices (column-major ord.) where A ~= F are I') 
   
!
!- 2) find indexes of an array corresponding to a given condition:
!   
   a = reshape([13,0,16,15,14,11],[2,3]) * IONE
   call display(a, msg='5) Let be the array A') 
   indx = find ( a >=  x .and. a < y )
   call display (indx, msg = 'indices (column-major ord.) where A >= 13 and A < 16 are I')   

   suba = extracmat (a, indx)
   call display (suba, msg = 'the corresponding elements are (in column-major ord.) ') 
   
   a = a * RONE
   call display(a, msg='6) Let be the array A') 
   indx = find ( a >=  x .and. a < y )
   call display (indx, msg = 'indices (column-major ord.) where A >= 13 and A < 16 are I') 

   suba = extracmat (a, indx)
   call display (suba, msg = 'the corresponding elements are (in column-major ord.) ') 
           
              
   a = reshape( ['First names:',' Last names:', &
                 'Roger       ','Smith       ', &
                 'Mary        ','King        ', &
                 'Roger       ','Moore       ', &
                 'Tony        ','Curtis      '],[2,5])
                                                                                        
   a = trim(a)
   call display(a, msg='7) Let A') 
   jindx = find ( extracmat(a,[1_Ikind],[-1_Ikind]) == 'Roger' )

   call display (jindx, msg = 'indices (of the 1st row) where the first name is "Roger"',symb=':') 

   indx = 2_Ikind
   suba = extracmat (a, indx, jindx)
   call display (suba, msg = 'the corresponding names are',symb=':')    
   
   end subroutine test_FIND


!=============================================================================================   
   subroutine test_RESHAPE
!=============================================================================================  
 
!---------------------------------------------------------------------------------------------    
!- Reshapes an array 
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, sz
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing RESHAPE *******'
      print*
   end if    

!
!- Integer arrays:
!
   a = reshape([1,2,3,4,5,6],[2,3]) * IONE
   call display(a, msg='1) Let be the array A') 
!- passing an array of integers for the shape argument:   
   a = reshape ( a, [1_Ikind,6_Ikind] )
   call display (a, msg = 'reshape(A,[1,6]) gives now A')
!- passaing a pk2 object for the shape argument:   
   sz = [2_Ikind,3_Ikind]
   a = reshape ( a, sz )
   call display (a, msg = 'reshape(A,sz) (with sz = [2,3]) gives now A')   

!
!- Same for real arrays:
!  
   a = a * RONE
   call display(a, msg='2) Let be the array A') 
!- passing an array of integers for the shape argument:   
   a = reshape ( a, [1_Ikind,6_Ikind] )
   call display (a, msg = 'reshape(A,[1,6]) gives now A')
!- passaing a pk2 object for the shape argument:      
   sz = [2_Ikind,3_Ikind]
   a = reshape ( a, sz )
   call display (a, msg = 'reshape(A,sz) (with sz = [2,3]) gives now A')   

!
!- Same for complex arrays:
!  
   a = a * (CONE + CIMAG)
   call display(a, msg='3) Let be the array A') 
!- passing an array of integers for the shape argument:   
   a = reshape ( a, [1_Ikind,6_Ikind] )
   call display (a, msg = 'reshape(A,[1,6]) gives now A')
!- passaing a pk2 object for the shape argument:      
   sz = [2_Ikind,3_Ikind]
   a = reshape ( a, sz )
   call display (a, msg = 'reshape(A,sz) (with sz = [2,3]) gives now A')         

!
!- Same for boolean arrays:
!  
   a = reshape([.true.,.false.,.true.,.false.,.true.,.false.],[2,3])
   call display(a, msg='4) Let be the array A') 
!- passing an array of integers for the shape argument:   
   a = reshape ( a, [1_Ikind,6_Ikind] )
   call display (a, msg = 'reshape(A,[1,6]) gives now A')
!- passaing a pk2 object for the shape argument:      
   sz = [2_Ikind,3_Ikind]
   a = reshape ( a, sz )
   call display (a, msg = 'reshape(A,sz) (with sz = [2,3]) gives now A')  
   
!
!- Same for string arrays:
!  
   a = reshape(['ifort   ','nagfor  ','gfortran','xlf     ','Absoft  ','f77     '],[2,3])
   call display(a, msg='5) Let be the array A') 
!- passing an array of integers for the shape argument:   
   a = reshape ( a, [1_Ikind,6_Ikind] )
   call display (a, msg = 'reshape(A,[1,6]) gives now A')
!- passaing a pk2 object for the shape argument:      
   sz = [2_Ikind,3_Ikind]
   a = reshape ( a, sz )
   call display (a, msg = 'reshape(A,sz) (with sz = [2,3]) gives now A')  
   
   end subroutine test_RESHAPE


!=============================================================================================   
   subroutine test_COLON
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!- Computes linearly spaced n-vector
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: r, p(3)
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing COLON *******'
      print*
   end if    

!
!- Version 1) passing integers or reals (--> call to pk2f_COLONi or pk2f_COLONr):
!
   r = colon (1_Ikind, 10_Ikind)  ! integers between 1 and 10 (default step=1)
   call display(r, msg = '1:10')

   r = colon (1_Ikind, 10_Ikind, step = 1_Ikind) ! int. between 1 and 10 (w/ explicit step)
   call display(r,msg = '1:1:10')
   
   r = colon (1_Ikind, 10_Ikind, step = 9_Ikind) ! int. between 1 and 10 (w/ explicit step))
   call display(r,msg = '1:9:10')
   
   r = colon (1_Ikind, 10_Ikind, step = 11_Ikind) ! int. between 1 and 10 (w/ explicit step)
   call display(r,msg = '1:11:10')
   
   r = colon (1_Ikind, 10_Ikind, step = 0_Ikind) ! int. between 1 and 10 (w/ explicit step)
   call display(r, msg = '1:0:10')   

   r = colon (1.0_Rkind, 10.0_Rkind, step = 9.0_Rkind) ! real. between 1 and 10
   call display(r, msg ='1.0:9.0:10.0')   

!
!- Version 2) an array of pk2 object (--> call to pk2f_COLONwrp):
!
   p(1) = 1_Ikind ; p(2) = 10_Ikind ; p(3) = 2_Ikind 
   
   r = colon (p(1:2)) ! int. between 1 and 10 with a step of 1 (p(3) not used)
   call display(r, msg = '1:10 (with an array)') 
    
   r = colon (p)
   call display(r, msg = '1:2:10 (with an array)') ! integers with a step of 2

   p(1) = 1_Ikind ; p(2) = 10.0_Rkind ; p(3) = 2_Ikind
   
   r = colon (p)
   call display(r, msg ='1:2:10.0 (with an array)') ! reals with step of 2

   p(1) = 1_Ikind ; p(2) = 5_Ikind ; p(3) = 0.5_Rkind 
   
   r = colon (p)
   call display(r, msg = '1:0.5:5 (with an array)') ! reals with step of 0.5
            
   end subroutine test_COLON
   
     
!=============================================================================================   
   subroutine test_INV_MLDIVIDE_LU
!=============================================================================================  

!---------------------------------------------------------------------------------------------    
!- Inverses a matrix and solves a linear system
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   integer(Ikind), parameter :: n = 5, n1 = n-1, i1 = 1, i2 = 2
   type   (pk2_t)            :: Ai, Ar, Ac, bi, Aiinv, Arinv, Acinv, x, tmp
!--------------------------------------------------------------------------------------------- 

!  Example: solve -u'' = 1 on (0,L) & u(0) = u(L) = 0, L = n-1

!  the matrix (1d-laplacian):   
   Ai = diag(i2*ones(n ,i1)    ) - &  ! 2 on the diagonal 
        diag(   ones(n1,i1), i1) - &  ! 1 on the upper-diagonale
        diag(   ones(n1,i1),-i1)      !-1 on the lower-diagonale
!  the rhs:
   bi = ones(n,i1)
!  set boundary conditions (change Ai and bi):
   tmp = reshape([IONE,IZERO], [1,2])
   call Ai%SetSubmat (tmp,[i1], [i1,i2]) ! change the first 2 elements of the first row
   tmp = reshape([IZERO,IONE], [1,2])
   call Ai%SetSubmat (tmp,[n], [n1,n]) ! change the last 2 elements of the last row
   tmp = IZERO
   call bi%SetSubmat(tmp,[i1],[i1]) ! change the first element of the rhs
   call bi%SetSubmat(tmp,[n ],[i1]) ! change the last element of the rhs
   
   if (verbos) then
      print*
      print*,'****** Testing INV, MLDIVIDE, LU AND LUSOLV *******'
      print*
   end if    
   
!
!- INV
!  ^^^
   call display(Ai,'Ai')
   Aiinv = inv(Ai)
   call display(Aiinv,'inv(Ai)')
   call display(Ai*Aiinv,'Ai*inv(Ai)')
   
   Ar = Ai*1.0_Rkind
   call display(Ar,'Ar')   
   Arinv = inv(Ar)
   call display(Arinv,'inv(Ar)')
   call display(Ar*Arinv,'Ar*inv(Ar)')

   Ac = Ar + CIMAG
   call display(Ac,'Ac')   
   Acinv = inv(Ac)
   call display(Acinv,'inv(Ac)')
   call display(Ac*Acinv,'Ac*inv(Ac)')
!
!- MLDIVIDE
!  ^^^^^^^^

!- Version 1) (--> call mldivide):
   call display(Ai,'Ai')     
   call display(bi,'bi')     
   x = mldivide(Ai,bi)
   call display(x,'x (sol. of Ai*x = bi)')
   call display(Ai*x,'Ai*x')

   call display(Ar,'Ar')     
   call display(bi,'bi')     
   x = mldivide(Ar,bi)
   call display(x,'x (sol. of Ar*x = bi)')
   call display(Ar*x,'Ar*x')

   call display(Ac,'Ac')     
   call display(bi,'bi')     
   x = mldivide(Ac,bi)
   call display(x,'x (sol. of Ac*x = bi)')
   call display(Ac*x,'Ac*x') 
   
!- Version 2) (--> using the operator .bslash.):
   call display(Ai .bslash. bi,'Ai. bslash. bi')
   call display(Ar .bslash. bi,'Ar. bslash. bi')
   call display(Ac .bslash. bi,'Ac. bslash. bi')

!- Version 3) (--> using the inverse matrix):
   call display(Aiinv * bi,'inv(Ai) * bi')
   call display(Arinv * bi,'inv(Ar) * bi')
   call display(Acinv * bi,'inv(Ac) * bi')   

!
!- LU and LUsolv (function and subroutine versions)
!  ^^^^^^^^^^^^^
   tmp = lu(Ai) ! or call s_lu(Ai,tmp)
   x = lusolv(tmp,bi) ! or call s_lusolv(tmp,bi,x)
   call display(Ai,'Ai')     
   call display(bi,'bi')     
   call display(x,'x (sol. by lu of Ai*x = bi)')
   call display(Ai*x,'Ai*x')
   
   tmp = lu(Ar) ! or call s_lu(Ar,tmp)
   x = lusolv(tmp,bi) ! or call s_lusolv(tmp,bi,x)
   call display(Ar,'Ar')     
   call display(bi,'bi')     
   call display(x,'x (sol. by lu of Ar*x = bi)')
   call display(Ar*x,'Ar*x')

   tmp = lu(Ac) ! or call s_lu(Ar,tmp)
   x = lusolv(tmp,bi) ! or call s_lusolv(tmp,bi,x)
   call display(Ac,'Ac')     
   call display(bi,'bi')     
   call display(x,'x (sol. by lu of Ac*x = bi)')
   call display(Ac*x,'Ac*x')
   
   end subroutine test_INV_MLDIVIDE_LU
   
!=============================================================================================   
   subroutine test_DIAG
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!- Gets or sets a given diagonal
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   integer  (Ikind), parameter   :: n = 4, k =-1
   integer  (Ikind)              :: i
   integer  (Ikind), allocatable :: Ivec(:), Imat(:,:)
   real     (Rkind), allocatable :: Rvec(:), Rmat(:,:)
   complex  (Rkind), allocatable :: Cvec(:), Cmat(:,:)
   type     (str_t), allocatable :: Svec(:), Smat(:,:)
   logical         , allocatable :: Lvec(:), Lmat(:,:)
   character(len=3)              :: c
   
   type     (pk2_t)                :: p, q, qI, qR, qC, qL, qS
!--------------------------------------------------------------------------------------------- 
   allocate(Ivec(n),Rvec(n),Cvec(n),Lvec(n),Svec(n))
   do i = 1, n
      Ivec(i) = i
      Rvec(i) = i
      Cvec(i) = i + CIMAG*i
      Lvec(i) = .true.
      write(c,'(i0)')i
      Svec(i)%str = 'S'//trim(c)
   end do   
   
   write(c,'(i0)') k
   
   if (verbos) then
      print*
      print*,'****** Testing Diag *******'
      print*
   end if         
   
!
!- Form the square matrix whose k-th diagonal is given
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

!  Version 1) passing an intrinsic array and an integer (--> call to pk2f_Set?diag):
   qI = diag (Ivec,k)   
   call display (qI, 'qI=diag(Ivec,'//trim(c)//')')
   qR = diag (Rvec,k)
   call display (qR, 'qR=diag(Rvec,'//trim(c)//')')
   qC = diag (Cvec,k)
   call display (qC, 'qC=diag(Cvec,'//trim(c)//')')
   qL = diag (Lvec,k)
   call display (qL, 'qL=diag(Lvec,'//trim(c)//')')   
   qS = diag (Svec,k)
   call display (qS, 'qS=diag(Svec,'//trim(c)//')')   
   

!  Version 2) passing a pk2 object and an integer (--> call to pk2f_DIAGn):
   p = Ivec
   p = diag (p,k)
   call display (p, 'diag(p,'//trim(c)//') with (p=Ivec)')
   p = Rvec
   p = diag (p,k)
   call display (p, 'diag(p,'//trim(c)//') with (p=Rvec)')
   p = Cvec
   p = diag (p,k)
   call display (p, 'diag(p,'//trim(c)//') with (p=Cvec)')
   p = Lvec
   p = diag (p,k)
   call display (p, 'diag(p,'//trim(c)//') with (p=Lvec)')
   p = Svec
   p = diag (p,k)
   call display (p, 'diag(p,'//trim(c)//') with (p=Svec)')
   
!  Version 3) passing two pk2 objects (--> call to pk2f_DIAG):
   p = Ivec ; q = k
   p = diag (p,q)
   call display (p, 'diag(p,q) (with p=Ivec and q='//trim(c)//')')
   p = Rvec ; q = k
   p = diag (p,q)
   call display (p, 'diag(p,q) (with p=Rvec and q='//trim(c)//')')   
   p = Cvec ; q = k
   p = diag (p,q)
   call display (p, 'diag(p,q) (with p=Cvec and q='//trim(c)//')')
   p = Lvec ; q = k
   p = diag (p,q)
   call display (p, 'diag(p,q) (with p=Lvec and q='//trim(c)//')')
   p = Svec ; q = k
   p = diag (p,q)
   call display (p, 'diag(p,q) (with p=Svec and q='//trim(c)//')')      

!  Version 4) passing an array of two pk2 objects (--> call to pk2f_DIAGwrp):
   p = Ivec ; q = k
   p = diag ([p,q])
   call display (p, 'diag([p,q]) (with p=Ivec and q='//trim(c)//')')
   p = Rvec ; q = k
   p = diag ([p,q])
   call display (p, 'diag([p,q]) (with p=Rvec and q='//trim(c)//')')   
   p = Cvec ; q = k
   p = diag ([p,q])
   call display (p, 'diag([p,q]) (with p=Cvec and q='//trim(c)//')')
   p = Lvec ; q = k
   p = diag ([p,q])
   call display (p, 'diag([p,q]) (with p=Lvec and q='//trim(c)//')')
   p = Svec ; q = k
   p = diag ([p,q])
   call display (p, 'diag([p,q]) (with p=Svec and q='//trim(c)//')')    
   
!
!- Extract the k-th diagonal of a matrix 
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   call qI%m%Getmat(I=Imat)
   select type(w=>qI%m) ; type is (ik2_t) ; call w%Getmat(I=Imat) ; end select      
   select type(w=>qR%m) ; type is (rk2_t) ; call w%Getmat(R=Rmat) ; end select      
   select type(w=>qC%m) ; type is (ck2_t) ; call w%Getmat(C=Cmat) ; end select      
   select type(w=>qL%m) ; type is (lk2_t) ; call w%Getmat(L=Lmat) ; end select      
   select type(w=>qS%m) ; type is (sk2_t) ; call w%Getmat(S=Smat) ; end select 
   
         
!  Version 1) passing an intrinsic array and an integer (--> call to pk2f_Get?diag):
   p = diag (Imat,k)
   call display (p, 'p=diag(Imat,'//trim(c)//')')
   p = diag (Rmat,k)
   call display (p, 'p=diag(Rmat,'//trim(c)//')')
   p = diag (Cmat,k)
   call display (p, 'p=diag(Cmat,'//trim(c)//')')
   p = diag (Lmat,k)
   call display (p, 'p=diag(Lmat,'//trim(c)//')')
   p = diag (Smat,k)
   call display (p, 'p=diag(Smat,'//trim(c)//')')
         
!  Version 2) passing a pk2 object array and an integer (--> call to pk2f_DIAGn):
   p = diag (qI,k)
   call display (p, 'diag(qI,k='//trim(c)//')')
   p = diag (qR,k)
   call display (p, 'diag(qR,k='//trim(c)//')')
   p = diag (qC,k)
   call display (p, 'diag(qC,k='//trim(c)//')')
   p = diag (qL,k)
   call display (p, 'diag(qL,k='//trim(c)//')')
   p = diag (qS,k)
   call display (p, 'diag(qS,k='//trim(c)//')')

!  Version 3) passing two pk2 objects (--> call to pk2f_DIAG):
   q = k
   p = diag (qI,q)
   call display (p, 'diag(qI,q) (with q='//trim(c)//')')
   p = diag (qR,q)
   call display (p, 'diag(qR,q) (with q='//trim(c)//')')
   p = diag (qC,q)
   call display (p, 'diag(qC,q) (with q='//trim(c)//')')
   p = diag (qL,q)
   call display (p, 'diag(qL,q) (with q='//trim(c)//')')
   p = diag (qS,q)
   call display (p, 'diag(qS,q) (with q='//trim(c)//')')

!  Version 4) passing an array of two pk2 objects (--> call to pk2f_DIAGwrp):
   q = k
   p = diag ([qI,q])
   call display (p, 'diag([qI,q]) (with q='//trim(c)//')')
   p = diag ([qR,q])
   call display (p, 'diag([qR,q]) (with q='//trim(c)//')')
   p = diag ([qC,q])
   call display (p, 'diag([qC,q]) (with q='//trim(c)//')')
   p = diag ([qL,q])
   call display (p, 'diag([qL,q]) (with q='//trim(c)//')')
   p = diag ([qS,q])
   call display (p, 'diag([qS,q]) (with q='//trim(c)//')')

   deallocate(Ivec,Rvec,Cvec,Lvec,Svec,Imat,Rmat,Cmat,Lmat,Smat)
   
   end subroutine test_DIAG

!=============================================================================================   
   subroutine test_TRACE_DET_EIG
!=============================================================================================  

!---------------------------------------------------------------------------------------------    
!- Trace, determinant and eigenvalues
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   integer  (Ikind), parameter :: n = 3
   integer  (Ikind) :: Imat(n,n) 
   real     (Rkind) :: Rmat(n,n)
   complex  (Rkind) :: Cmat(n,n)
   integer  (Ikind) :: i
    
   type     (pk2_t)              :: p, q
!--------------------------------------------------------------------------------------------- 

   Imat = 0
   do i = 1, n
      Imat(i,i) = 2
      if (i < n) Imat(i+1,i) =-1
      if (i > 1) Imat(i-1,i) =-1
   end do   
   Rmat = Imat 
   Cmat = Rmat + Rmat*CIMAG

   if (verbos) then
      print*
      print*,'****** Testing Trace, Det and Eig *******'
      print*
   end if      
!
!- TRACE
!  ^^^^^
   p = Imat
   p = trace ( a = p )
   call display ( p , msg = 'trace(a=p) (with p = Imat)')
   call opflag%display()
   p = Rmat
   p = trace ( a = p )
   call display ( p , msg = 'trace(a=p) (with p = Rmat)')
   p = Cmat
   p = trace ( a = p ) 
   call display ( p , msg = 'trace(a=p) (with p = Cmat)')
!
!- DET
!  ^^^
   p = Imat
   p = det ( a = p )
   call display ( p , msg = 'det(a=p) (with p = Imat)')
   p = Rmat
   p = det ( a = p )
   call display ( p , msg = 'det(a=p) (with p = Rmat)')
   p = Cmat
   p = det ( a = p ) 
   call display ( p , msg = 'det(a=p) (with p = Cmat)') 
!
!- EIG
!  ^^^

!  1) eigenvalues only:
   p = Imat
   p = eig ( a = p ) ! equivalent to pk2f_eig ( a = p, vec = .false. )
   call display ( p , msg = 'eig(a=p) (with p = Imat)')
   p = Rmat
   p = eig ( a = p ) ! equivalent to pk2f_eig ( a = p, vec = .false. )
   call display ( p , msg = 'eig(a=p) (with p = Rmat)')
   p = Cmat
   p = eig ( a = p ) ! equivalent to pk2f_eig ( a = p, vec = .false. ) 
   call display ( p , msg = 'eig(a=p) (with p = Cmat)') 

!  2) eigenvalues (output result of eig) and eigenvectors (output argument of eig):
   p = Imat
   p = eig ( a = p, res2 = q ) 
   call display ( p , msg = 'eig(a=p,res2=q) (with p = Imat): ')
   call display ( q , msg = 'and q: ')
   p = Rmat
   p = eig ( a = p, res2 = q ) 
   call display ( p , msg = 'eig(a=p,res2=q) (with p = Rmat): ')
   call display ( q , msg = 'and q: ')
   p = Cmat
   p = eig ( a = p, res2 = q ) 
   call display ( p , msg = 'eig(a=p,res2=q) (with p = Cmat): ')
   call display ( q , msg = 'and q: ')

   end subroutine test_TRACE_DET_EIG
   
     
!=============================================================================================   
   subroutine test_TRANS_TRANSP
!=============================================================================================  

!---------------------------------------------------------------------------------------------    
!- Transposes of arrays (of any type)
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   integer  (Ikind), parameter :: n = 3, m = 2
   integer  (Ikind) :: Imat(n,m) 
   real     (Rkind) :: Rmat(n,m)
   complex  (Rkind) :: Cmat(n,m)
   logical          :: Lmat(n,m) = .true.
   type     (str_t) :: Smat(n,m)
   character(len=1) :: ci, cj
   integer  (Ikind) :: i,j
    
   type     (pk2_t) :: p1, p
!--------------------------------------------------------------------------------------------- 

   Imat = reshape([(i,i=1,n*m)],[n,m])
   Rmat = Imat 
   Cmat = Rmat + Rmat*CIMAG
   
   do j = 1, m
      do i = 1, n
         if (i > j) Lmat(i,j) = .false.
      end do
   end do

   do j = 1, m
      write(cj,'(i0)')j
      do i = 1, n
         write(ci,'(i0)')i
         Smat(i,j)%str = 'S'//ci//cj
      end do
   end do      

   if (verbos) then
      print*
      print*,'****** Testing Transp and Trans *******'
      print*
   end if      
!
!- TRANSP (tranposition)
!  ^^^^^^
   p1 = Imat
   p = transp ( a = p1 )
   call display ( p , msg = 'transp(a=p1) (with p1 = Imat)')
   p1 = Rmat
   p = transp ( a = p1 )
   call display ( p , msg = 'transp(a=p1) (with p1 = Rmat)')
   p1 = Cmat
   p = transp ( a = p1 )
   call display ( p , msg = 'transp(a=p1) (with p1 = Cmat)')
   p1 = Lmat
   p = transp ( a = p1 )
   call display ( p , msg = 'transp(a=p1) (with p1 = Lmat)')
   p1 = Smat
   p = transp ( a = p1 )
   call display ( p , msg = 'transp(a=p1) (with p1 = Smat)')

!
!- TRANS (conjugate-tranpose)
!  ^^^^^
   p1 = Imat
   p = trans ( a = p1 )
   call display ( p , msg = 'trans(a=p1) (with p1 = Imat)')
   p1 = Rmat
   p = trans ( a = p1 )
   call display ( p , msg = 'trans(a=p1) (with p1 = Rmat)')
   p1 = Cmat
   p = trans ( a = p1 )
   call display ( p , msg = 'trans(a=p1) (with p1 = Cmat)')
   p1 = Lmat
   p = trans ( a = p1 )
   call display ( p , msg = 'trans(a=p1) (with p1 = Lmat)')
   p1 = Smat
   p = trans ( a = p1 )
   call display ( p , msg = 'trans(a=p1) (with p1 = Smat)')  

            
   end subroutine test_TRANS_TRANSP
   
   
!=============================================================================================   
   subroutine test_MEAN_SUM_PROD
!=============================================================================================  

!---------------------------------------------------------------------------------------------    
!- Computes mean, sum and prod elements of an array (of integers, reals or complexes)
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: dim1 = 1
   integer(Ikind) :: Imat(4,3) = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[4,3])
   real   (Rkind) :: Rmat(4,3) = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[4,3])
   complex(Rkind) :: Cmat(4,3) 
   
   type   (pk2_t) :: p1, p2, p3, p
!---------------------------------------------------------------------------------------------    

   Cmat = Rmat + CIMAG*Rmat
   
   if (verbos) then
      print*
      print*,'****** Testing Mean, Sum and Prod functions *******'
      print*
   end if   
!
!- MEAN
!  ^^^^
   p1 = Imat
   p = mean ( a = p1 )
   call display ( p , msg = 'mean(a=p1) (with p1 = Imat)')
   
   p1 = Rmat
   p = mean ( a = p1 )
   call display ( p , msg = 'mean(a=p1) (with p1 = Rmat)')   

   p1 = Cmat
   p = mean ( a = p1 )
   call display ( p , msg = 'mean(a=p1) (with p1 = Cmat)')   

!
!- MEAN along a given dimension
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat
   p = mean ( a = p1, dim = dim1 )
   call display ( p , msg = 'mean(a=p1,dim=1) (with p1 = Imat)')
   
   p1 = Rmat
   p = mean ( a = p1, dim = dim1 )
   call display ( p , msg = 'mean(a=p1,dim=1) (with p1 = Rmat)')   

   p1 = Cmat
   p = mean ( a = p1, dim = dim1 )
   call display ( p , msg = 'mean(a=p1,dim=1) (with p1 = Cmat)')   
!
!- MEAN of the elements for which a given condition is true
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat ; p2 = Imat /= 6
   p = mean ( a = p1, mask = p2  )
   call display ( p , msg = 'mean(a=p1,mask=p2) (with p1=Imat, p2=Imat/=6)')
   
   p1 = Rmat
   p = mean ( a = p1, mask = p2 )
   call display ( p , msg = 'mean(a=p1,mask=p2) (with p1=Rmat, p2=Imat/=6)')   

   p1 = Cmat
   p = mean ( a = p1, mask = p2 )
   call display ( p , msg = 'mean(a=p1,mask=p2) (with p1=Cmat, p2=Imat/=6)')   
!
!- MEAN along a given dimension and with a condition
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat ; p2 = Imat /= 6 ; p3 = dim1
   p = mean ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'mean(matrs=[p1,p2,p3]) (with p1=Imat,p2=Imat/=6,p3=dim1)')
   
   p1 = Rmat 
   p = mean ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'mean(matrs=[p1,p2,p3]) (with p1=Rmat,p2=Imat/=6,p3=dim1)')   

   p1 = Cmat
   p = mean ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'mean(matrs=[p1,p2,p3]) (with p1=Cmat,p2=Imat/=6,p3=dim1)')   

!
!- SUM
!  ^^^
   p1 = Imat
   p = sum ( a = p1 )
   call display ( p , msg = 'sum(a=p1) (with p1 = Imat)')
   
   p1 = Rmat
   p = sum ( a = p1 )
   call display ( p , msg = 'sum(a=p1) (with p1 = Rmat)')   

   p1 = Cmat
   p = sum ( a = p1 )
   call display ( p , msg = 'sum(a=p1) (with p1 = Cmat)')   
!
!- SUM along a given dimension
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat
   p = sum ( a = p1, dim = dim1 )
   call display ( p , msg = 'sum(a=p1,dim=1) (with p1 = Imat)')
   
   p1 = Rmat
   p = sum ( a = p1, dim = dim1 )
   call display ( p , msg = 'sum(a=p1,dim=1) (with p1 = Rmat)')   

   p1 = Cmat
   p = sum ( a = p1, dim = dim1 )
   call display ( p , msg = 'sum(a=p1,dim=1) (with p1 = Cmat)')   
!
!- SUM of the elements for which a given condition is true
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat ; p2 = Imat /= 6
   p = sum ( a = p1, mask = p2  )
   call display ( p , msg = 'sum(a=p1,mask=p2) (with p1=Imat, p2=Imat/=6)')
   
   p1 = Rmat
   p = sum ( a = p1, mask = p2 )
   call display ( p , msg = 'sum(a=p1,mask=p2) (with p1=Rmat, p2=Imat/=6)')   

   p1 = Cmat
   p = sum ( a = p1, mask = p2 )
   call display ( p , msg = 'sum(a=p1,mask=p2) (with p1=Cmat, p2=Imat/=6)')   
!
!- SUM along a given dimension and with a condition
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat ; p2 = Imat /= 6 ; p3 = dim1
   p = sum ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'sum(matrs=[p1,p2,p3]) (with p1=Imat,p2=Imat/=6,p3=dim1)')
   
   p1 = Rmat 
   p = sum ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'sum(matrs=[p1,p2,p3]) (with p1=Rmat,p2=Imat/=6,p3=dim1)')   

   p1 = Cmat
   p = sum ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'sum(matrs=[p1,p2,p3]) (with p1=Cmat,p2=Imat/=6,p3=dim1)')    

!
!- PROD
!  ^^^^
   p1 = Imat
   p = prod ( a = p1 )
   call display ( p , msg = 'prod(a=p1) (with p1 = Imat)')
   
   p1 = Rmat
   p = prod ( a = p1 )
   call display ( p , msg = 'prod(a=p1) (with p1 = Rmat)')   

   p1 = Cmat
   p = prod ( a = p1 )
   call display ( p , msg = 'prod(a=p1) (with p1 = Cmat)')   
!
!- PROD along a given dimension
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat
   p = prod ( a = p1, dim = dim1 )
   call display ( p , msg = 'prod(a=p1,dim=1) (with p1 = Imat)')
   
   p1 = Rmat
   p = prod ( a = p1, dim = dim1 )
   call display ( p , msg = 'prod(a=p1,dim=1) (with p1 = Rmat)')   

   p1 = Cmat
   p = prod ( a = p1, dim = dim1 )
   call display ( p , msg = 'prod(a=p1,dim=1) (with p1 = Cmat)')  
!
!- SUM of the elements for which a given condition is true
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat ; p2 = Imat /= 6
   p = prod ( a = p1, mask = p2  )
   call display ( p , msg = 'prod(a=p1,mask=p2) (with p1=Imat, p2=Imat/=6)')
   
   p1 = Rmat
   p = prod ( a = p1, mask = p2 )
   call display ( p , msg = 'prod(a=p1,mask=p2) (with p1=Rmat, p2=Imat/=6)')   

   p1 = Cmat
   p = prod ( a = p1, mask = p2 )
   call display ( p , msg = 'prod(a=p1,mask=p2) (with p1=Cmat, p2=Imat/=6)')   
!
!- PROD along a given dimension and with a condition
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   p1 = Imat ; p2 = Imat /= 6 ; p3 = dim1
   p = prod ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'prod(matrs=[p1,p2,p3]) (with p1=Imat,p2=Imat/=6,p3=dim1)')
   
   p1 = Rmat 
   p = prod ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'prod(matrs=[p1,p2,p3]) (with p1=Rmat,p2=Imat/=6,p3=dim1)')   

   p1 = Cmat
   p = prod ( matrs = [p1,p2,p3]  )
   call display ( p , msg = 'prod(matrs=[p1,p2,p3]) (with p1=Cmat,p2=Imat/=6,p3=dim1)')   
             
   end subroutine test_MEAN_SUM_PROD


!=============================================================================================   
   subroutine test_MIN_MAX
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!- Computes min and max elements of an array (of integers or reals)
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: dim1=1, dim2=2, i3 = 3
   integer(Ikind) :: Imat(4,3) = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[4,3])
   real   (Rkind) :: Rmat(4,3) = reshape([1,2,3,4,5,6,7,8,9,10,11,12],[4,3]), r3 = 3.0

   type   (pk2_t) :: p1, p2, p, parray(3)
!---------------------------------------------------------------------------------------------    

   if (verbos) then
      print*
      print*,'****** Testing Min and Max functions *******'
      print*
   end if   
!
!- absolute MIN 
!  ^^^^^^^^^^^^
   p1 = Imat
   p = min ( a = p1 )
   call display ( p , msg = 'min(a=p1) (with p1 = Imat)')
   
   p1 = Rmat
   p = min ( a = p1 )
   call display ( p , msg = 'min(a=p1) (with p1 = Rmat)')   
!
!- absolute min along a given dimension
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

!- Version 1) passing an integer for argument dim (--> call to pk2f_MIN):
   p1 = Imat
   p = min ( a = p1, dim = dim1 )
   call display ( p , msg = 'min(a=p1,dim=1) (with p1 = Imat)')

   p = min ( a = p1, dim = dim2 )
   call display ( p , msg = 'min(a=p1,dim=2) (with p1 = Imat)')

   p1 = Rmat
   p = min ( a = p1, dim = dim1 )
   call display ( p , msg = 'min(a=p1,dim=1) (with p1 = Rmat)')

   p = min ( a = p1, dim = dim2 )
   call display ( p , msg = 'min(a=p1,dim=2) (with p1 = Rmat)')

!- Version 2) passing an array of two pk2 objects (--> call to pk2f_MINwrp):
   parray(1) = Imat ; parray(2) = dim1
   p = min ( matrs = parray(1:2) )
   call display ( p , msg = 'min(matrs = parray) (with parray = [Imat,1] )')

   parray(1) = Imat ; parray(2) = dim2
   p = min ( matrs = parray(1:2) )
   call display ( p , msg = 'min(matrs = parray) (with parray = [Imat,2] )')

   parray(1) = Rmat ; parray(2) = dim1
   p = min ( matrs = parray(1:2) )
   call display ( p , msg = 'min(matrs = parray) (with parray = [Rmat,1] )')

   parray(1) = Rmat ; parray(2) = dim2
   p = min ( matrs = parray(1:2) )
   call display ( p , msg = 'min(matrs = parray) (with parray = [Rmat,2] )')

!
!- min of the elements for which a given condition is true
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

!- version 1) passing two pk2 objects (--> call to pk2f_MIN)
   p1 = Imat ; p2 = p1 > i3
   p = min ( a = p1, mask = p2 )
   call display ( p , msg = 'min(a=p1,mask=p2) (with p1=Imat, p2=p1>3)')

   p1 = Rmat ; p2 = p1 > r3
   p = min ( a = p1, mask = p2 )
   call display ( p , msg = 'min(a=p1,mask=p2) (with p1=Rmat, p2=p1>3.0)')

!- Version 2) passing an array of two pk2 objects (--> call to pk2f_MINwrp)
   parray(1) = Imat ; parray(2) = p1 > i3
   p = min ( matrs = parray(1:2) )
   call display ( p , msg = 'min(matrs=parray) (with parray = [Imat,p1>3])')

   parray(1) = Rmat ; parray(2) = p1 > r3
   p = min ( matrs = parray(1:2) )
   call display ( p , msg = 'min(matrs=parray) (with parray = [Rmat,p1>3.0])')

!
!- min along a given dimension and for elements for which a given condition is true
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

!- version 1) passing two pk2 objects (--> call to pk2f_MIN)
   p1 = Imat ; p2 = p1 > i3
   p = min ( a = p1, mask = p2, dim = 1_Ikind )
   call display ( p , msg = 'min(a=p1,mask=p2,dim=1) (with p1=Imat, p2=p1>3)')

   p1 = Rmat ; p2 = p1 > r3
   p = min ( a = p1, dim = 1_Ikind, mask = p2 )
   call display ( p , msg = 'min(a=p1,dim=1,mask=p2) (with p1=Rmat, p2=p1>3.0)')

!- Version 2) passing an array of three pk2 objects (--> call to pk2f_MINwrp)
!  the 1st must be the concerned matrix, the 2nd and the 3rd (in any order) the dim. and the
!  boolean condition:
!
   parray(1) = Imat ; parray(2) = p1 > i3 ; parray(3) = dim1 
   p = min ( matrs = parray )
   call display ( p , msg = 'min(matrs=parray) (with parray = [Imat,p1>3,dim1])')

   parray(1) = Rmat ; parray(2) = dim1 ; parray(3) = p1 > r3
   p = min ( matrs = parray )
   call display ( p , msg = 'min(matrs=parray) (with parray = [Rmat,dim1,p1>3.0])')

!
!- absolute MAX
!  ^^^^^^^^^^^^
   p1 = Imat
   p = max ( a = p1 )
   call display ( p , msg = 'max(a=p1) (with p1 = Imat)')
   
   p1 = Rmat
   p = max ( a = p1 )
   call display ( p , msg = 'max(a=p1) (with p1 = Rmat)')   
!
!- absolute max along a given dimension
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

!- Version 1) passing an integer for argument dim (--> call to pk2f_MAX):
   p1 = Imat
   p = max ( a = p1, dim = dim1 )
   call display ( p , msg = 'max(a=p1,dim=1) (with p1 = Imat)')

   p = max ( a = p1, dim = dim2 )
   call display ( p , msg = 'max(a=p1,dim=2) (with p1 = Imat)')

   p1 = Rmat
   p = max ( a = p1, dim = dim1 )
   call display ( p , msg = 'max(a=p1,dim=1) (with p1 = Rmat)')

   p = max ( a = p1, dim = dim2 )
   call display ( p , msg = 'max(a=p1,dim=2) (with p1 = Rmat)')

!- Version 2) passing an array of two pk2 objects (--> call to pk2f_MAXwrp):
   parray(1) = Imat ; parray(2) = dim1
   p = max ( matrs = parray(1:2) )
   call display ( p , msg = 'max(matrs = parray) (with parray = [Imat,1] )')

   parray(1) = Imat ; parray(2) = dim2
   p = max ( matrs = parray(1:2) )
   call display ( p , msg = 'max(matrs = parray) (with parray = [Imat,2] )')

   parray(1) = Rmat ; parray(2) = dim1
   p = max ( matrs = parray(1:2) )
   call display ( p , msg = 'max(matrs = parray) (with parray = [Rmat,1] )')

   parray(1) = Rmat ; parray(2) = dim2
   p = max ( matrs = parray(1:2) )
   call display ( p , msg = 'max(matrs = parray) (with parray = [Rmat,2] )')

!
!- max of the elements for which a given condition is true
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

!- version 1) passing two pk2 objects (--> call to pk2f_MAX)
   p1 = Imat ; p2 = p1 < 10_Ikind
   p = max ( a = p1, mask = p2 )
   call display ( p , msg = 'max(a=p1,mask=p2) (with p1=Imat, p2=p1<10)')
      
   p1 = Rmat ; p2 = p1 < 10.0_Rkind
   p = max ( a = p1, mask = p2 )
   call display ( p , msg = 'max(a=p1,mask=p2) (with p1=Rmat, p2=p1<10.0)')   

!- Version 2) passing an array of two pk2 objects (--> call to pk2f_MAXwrp)
   parray(1) = Imat ; parray(2) = p1 < 10_Ikind
   p = max ( matrs = parray(1:2) )
   call display ( p , msg = 'max(matrs=parray) (with parray = [Imat,p1<10])')

   parray(1) = Rmat ; parray(2) = p1 < 10.0_Rkind
   parray(3) = pk2_t(EMPTY) ! or put parray(1:2) for arg. of pk2f_max
   p = max ( matrs = parray )
   call display ( p , msg = 'max(matrs=parray) (with parray = [Rmat,p1<10.0])')
!
!- max along a given dimension and for elements for which a given condition is true
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

!- version 1) passing two pk2 objects (--> call to pk2f_MAX)
   p1 = Imat ; p2 = p1 < 10_Ikind
   p = max ( a = p1, mask = p2, dim = 1_Ikind )
   call display ( p , msg = 'max(a=p1,mask=p2,dim=1) (with p1=Imat, p2=p1<10)')

   p1 = Rmat ; p2 = p1 < 10.0_Rkind
   p = max ( a = p1, dim = 1_Ikind, mask = p2 )
   call display ( p , msg = 'max(a=p1,dim=1,mask=p2) (with p1=Rmat, p2=p1<10.0)')

!- Version 2) passing an array of three pk2 objects (--> call to pk2f_MAXwrp)
!  the 1st must be the concerned matrix, the 2nd and the 3rd (in any order) the dim. and the
!  boolean condition:
!
   parray(1) = Imat ; parray(2) = p1 < 10_Ikind ; parray(3) = dim1
   p = max ( matrs = parray )
   call display ( p , msg = 'max(matrs=parray) (with parray = [Imat,p1<10,dim1])')

   parray(1) = Rmat ; parray(2) = 1_Ikind ; parray(3) = p1 < 10.0_Rkind
   p = max ( matrs = parray )
   call display ( p , msg = 'max(matrs=parray) (with parray = [Rmat,dim1,p1<10.0])')


   end subroutine test_MIN_MAX
   
   
!=============================================================================================   
   subroutine test_ZEROS_ONES_RAND_EYE_MAGIC
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options

!---------------------------------------------------------------------------------------------    
!- Setting arrays of zeros, of falses, of ones, of randoms, of unity and magic square
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------   
   integer  (Ikind ) :: i1 = 1, i3 = 3, i5 = 5, i100 = 100
   type     (pk2_t ) :: p1, p2, p3, p4, p, parray(2)
   character(len=80) :: comp
!---------------------------------------------------------------------------------------------    

   if (verbos) then
      print*
      print*,'****** Testing Zeros, Ones, Rand, Eye, Magic functions *******'
      print*
   end if
!
!- Arrays of 0s
!  ^^^^^^^^^^^^
!- Version 1) with integers argument (--> call to pk2f_ZEROSnm):
!
   p = zeros (n = i3, m = i5) ! or pk2f_ZEROS (i3, i5)
   call display ( p , msg = 'zeros(i3,i5)')
!
!- Version 2) with two pk2 objects (--> call to pk2f_ZEROS), the shape n x m of the resulting
!  is defined by the first entries of each object:
!
   p1 = i3  ; p2 = i5
   p = zeros (a = p1, b = p2) ! or pk2f_ZEROS (p1, p2)
   call display ( p , msg = 'zeros(p1,p2)')
!
!- Version 3) with only one pk2 object (--> call to pk2f_ZEROS), the result is of the same
!  shape of the pk2 array:
!
   p = zeros (a = p) ! or pk2f_ZEROS (p)  
   call display ( p , msg = 'zeros(p)')
!
!- Version 4): with an array of two pk2 objects (--> call to pk2f_ZEROSwrp):
!
   parray(1) = p1 ; parray(2) = p2
   p = zeros (matrs = parray) ! or pk2f_ZEROS (parray)
   call display ( p , msg = 'zeros(parray)')
!
!- Version 4bis): with an array of two pk2 objects (--> call to pk2f_ZEROSwrp):
!
   p = zeros (matrs = [p1,p2]) ! or zeros ([p1,p2])
   call display ( p , msg = 'zeros([p1,p2])')
!
!- Arrays of .false.
!  ^^^^^^^^^^^^^^^^^
!- Version 1) with integers argument (--> call to pk2f_FALSESnm):
!
   p = falses (n = i3, m = i5) ! or falses (i3, i5)
   call display ( p , msg = 'falses(i3,i5)')
!
!- Version 2) with two pk2 objects (--> call to pk2f_FALSES), the shape n x m of the resulting
!  is defined by the first entries of each object:
!
   p1 = i3  ; p2 = i5
   p = falses (a = p1, b = p2) ! or falses (p1, p2)
   call display ( p , msg = 'falses(p1,p2)')
!
!- Version 3) with only one pk2 object (--> call to pk2f_FALSES), the result is of the same
!  shape of the pk2 array:
!
   p = falses (a = p) ! or falses (p)  
   call display ( p , msg = 'falses(p)')
!
!- Version 4): with an array of two pk2 objects (--> call to pk2f_FALSESwrp):
!
   parray(1) = p1 ; parray(2) = p2
   p = falses (matrs = parray) ! or falses (parray)
   call display ( p , msg = 'falses(parray)')
!
!- Version 4bis): with an array of two pk2 objects (--> call to pk2f_FALSESwrp):
!
   p = falses (matrs = [p1,p2]) ! or falses ([p1,p2])
   call display ( p , msg = 'falses([p1,p2])')

!
!- Arrays of 1s
!  ^^^^^^^^^^^^
!- Version 1) with integers argument (--> call to pk2f_ONESnm):
!
   p = ones (n = i3, m = i5) ! or ones (i3, i5)
   call display ( p , msg = 'ones(i3,i5)')
!
!- Version 2) with two pk2 objects (--> call to pk2f_ONES), the shape n x m of the resulting
!  is defined by the first entries of each object:
!
   p1 = i3  ; p2 = i5
   p = ones (a = p1, b = p2) ! or ones (p1, p2)
   call display ( p , msg = 'ones(p1,p2)')
!
!- Version 3) with only one pk2 object (--> call to pk2f_ONES), the result is of the same
!  shape of the pk2 array:
!
   p = ones (a = p) ! or ones (p)  
   call display ( p , msg = 'ones(p)')
!
!- Version 4): with an array of two pk2 objects (--> call to pk2f_ONESwrp):
!
   parray(1) = p1 ; parray(2) = p2
   p = ones (matrs = parray) ! or ones (parray)
   call display ( p , msg = 'ones(parray)')
!
!- Version 4bis): with an array of two pk2 objects (--> call to pk2f_ONESwrp):
!
   p = ones (matrs = [p1,p2]) ! or ones ([p1,p2])
   call display ( p , msg = 'ones([p1,p2])')
   
!
!- Identity matrix
!  ^^^^^^^^^^^^^^^
!- Version 1) with integers argument (--> call to pk2f_EYEnm):
!
   p = eye (n = i3, m = i5) ! or eye (i3, i5)
   call display ( p , msg = 'eye(i3,i5)')
!
!- Version 2) with two pk2 objects (--> call to pk2f_EYE), the shape n x m of the resulting
!  is defined by the first entries of each object:
!
   p1 = i3  ; p2 = i5
   p = eye (a = p1, b = p2) ! or eye (p1, p2)
   call display ( p , msg = 'eye(p1,p2)')
!
!- Version 3) with only one pk2 object (--> call to pk2f_EYE), the result is of the same
!  shape of the pk2 array:
!
   p = eye (a = p) ! or eye (p)  
   call display ( p , msg = 'eye(p)')
!
!- Version 4): with an array of two pk2 objects (--> call to pk2f_EYEwrp):
!
   parray(1) = p1 ; parray(2) = p2
   p = eye (matrs = parray) ! or eye (parray)
   call display ( p , msg = 'eye(parray)')
!
!- Version 4bis): with an array of two pk2 objects (--> call to pk2f_EYEwrp):
!
   p = eye (matrs = [p1,p2]) ! or eye ([p1,p2])
   call display ( p , msg = 'eye([p1,p2])')

!
!- Magic squares
!  ^^^^^^^^^^^^^
!- Version 1) with integer argument "n" (--> call to pk2f_MAGICn): a magic square of order n
!
   p = magic (n = i5) ! or magic (i5)
   call display ( p , msg = 'magic(5)')
!
!- Version 2) with a pk2 argument "p" (--> call to pk2f_MAGIC), the result is a magic square
!  of order n with n equal to the 1st entry of p:
!
   p1 = reshape([i5,i3,i3,i3],[2,2]) ! e.g. a 2x2 array 
   p = magic (a = p1) ! (only p1%v(1,1) will be used)
   call display ( p , msg = 'magic(p1) (with p1 = [5,3;3,3])')  
!
!- which in fact is equivalent to:
!
   p1 = i5
   p = magic (a = p1)   
   call display ( p , msg = 'magic(p1) (with p1 = i5)')  

!
!- Arrays of randoms (uniformly distributed in (0,1))
!  ^^^^^^^^^^^^^^^^^

! leak is reported (by leaks) in the intrinsic random_number
! (used in pk2_rand?) with gfortran (gcc 9.2.0) but not with
! ifort nor with nagfor

comp = trim(adjustl(compiler_version()))
if (index(util_StringCap(comp),'GCC') /= 0) then
   if (verbos) then
      write(*,'(/,a,/,a,/,a)',advance='no') &
      'WARNING: you compiled this program with gfortran and we have observed that the',&
      'intrinsic "random_number" subroutine leaks when used with this compiler (GCC 9.2.0)', &
      '(not observed with ifort and nagfor). Test the rand and randi functions anyway (y/n): '
      read(*,'(a)')buf
      if (buf == 'n') RETURN
   else
      RETURN
   end if 
end if  

!
!- Version 1) with integers argument (--> call to pk2f_RANDnm):
!
   p = rand (n = i3, m = i5) ! or rand (i3, i5)
   call display ( p , msg = 'rand(i3,i5)')
!
!- Version 2) with two pk2 objects (--> call to pk2f_RAND), the shape n x m of the resulting
!  is defined by the first entries of each object:
!
   p1 = i3  ; p2 = i5
   p = rand (a = p1, b = p2) ! or rand (p1, p2)
   call display ( p , msg = 'rand(p1,p2)')
!
!- Version 3) with only one pk2 object (--> call to pk2f_RAND), the result is of the same
!  shape of the pk2 array:
!
   p = rand (a = p) ! or rand (p)  
   call display ( p , msg = 'rand(p)')
!
!- Version 4): with an array of two pk2 objects (--> call to pk2f_RANDwrp):
!
   parray(1) = p1 ; parray(2) = p2
   p = rand (matrs = parray) ! or rand (parray)
   call display ( p , msg = 'rand(parray)')
!
!- Version 4bis): with an array of two pk2 objects (--> call to pk2f_RANDwrp):
!
   p = rand (matrs = [p1,p2]) ! or rand ([p1,p2])
   call display ( p , msg = 'rand([p1,p2])')
!
!- Arrays of integer randoms
!  ^^^^^^^^^^^^^^^^^^^^^^^^^
!- Version 1) with integers argument (--> call to pk2f_RANDInm):
!
   p = randi (imin = i5, imax = i100, n = i3, m = i5) 
   call display ( p , msg = 'randi(i5,i100,i3,i5)')
!
!- Version 2): with an array of two pk2 objects (--> call to pk2f_RANDIwrp):
!
   p1 = [i5,i100] ; p2 = [i3,i5]
   p3 = randi(matrs = [p1,p2]) ! or rand (parray)
   call display ( p3 , msg = 'randi([p1,p2])')

   end subroutine test_ZEROS_ONES_RAND_EYE_MAGIC
   

!=============================================================================================   
   subroutine test_TRIU_TRIL
!============================================================================================= 

!---------------------------------------------------------------------------------------------    
!- Get the upper-triangle of a matrix
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------   
   integer  (Ikind) :: Imat(4,4) 
   real     (Rkind) :: Rmat(4,4)
   complex  (Rkind) :: Cmat(4,4)
   logical          :: Lmat(4,4) = .true.
   type     (str_t) :: Smat(4,4)
   character(len=1) :: ci, cj
   character(len=2) :: cdiag
   integer  (Ikind) :: i,j, kdiag =-1
   
   type     (pk2_t) :: p1, p2, p, parray(2)
!---------------------------------------------------------------------------------------------    

   write(cdiag,'(i0)')kdiag
   
   Imat = reshape([(j,j=1,16)],[4,4])
   Rmat = Imat
   Cmat = Rmat + CIMAG*Rmat
   
   do j = 1, size(Smat,2)
      write(cj,'(i0)')j
      do i = 1, size(Smat,1)
         write(ci,'(i0)')i
         Smat(i,j)%str = 'S'//ci//cj
         if (i > j) Lmat(i,j) = .false.
      end do
   end do      
   
   do j = 1, size(Lmat,2)
      do i = 1, size(Lmat,1)
         if (i > j) Lmat(i,j) = .false.
      end do
   end do      

   if (verbos) then
      print*
      print*,'****** Testing Triu and Tril functions *******'
      print*
   end if
!
!- TRIU
!  ^^^^   
!
!- version 1) passing an intrinsic array and an integer (kdiag) (--> call to pk2f_Get?triu):
!   
   p = triu (matrix = Imat, k = kdiag) ! 
   call display ( p , msg = 'triu(matrix = Imat, k = '//trim(cdiag)//')')   
   p = triu (matrix = Rmat, k = kdiag) ! 
   call display ( p , msg = 'triu(matrix = Rmat, k = '//trim(cdiag)//')')
   p = triu (matrix = Cmat, k = kdiag) ! 
   call display ( p , msg = 'triu(matrix = Cmat, k = '//trim(cdiag)//')')
   p = triu (matrix = Lmat, k = kdiag) ! 
   call display ( p , msg = 'triu(matrix = Lmat, k = '//trim(cdiag)//')')
   p = triu (matrix = Smat, k = kdiag) ! 
   call display ( p , msg = 'triu(matrix = Smat, k = '//trim(cdiag)//')')
   
return   

   
!
!- version 2) passing a pk2 object (p1) and an integer (kdiag) (—-> call to pk2f_TRIUn):
!      
   p1 = Rmat
   p = triu (a = p1, k = kdiag)
   call display (p , msg = 'triu(a = p1, k = '//trim(cdiag)//')')
!
!- version 3) passing two pk2 objects (p1 and p2) (--> call to pk2f_TRIU):
!     
   p2 = kdiag
   p = triu (a = p1, b = p2)
   call display ( p , msg = 'triu(a = p1, b = p2)')
!
!- version 4) passing an array of two pk2 objects (--> call to pk2f_TRIUwrp):
!     
   parray(1) = p1 ; parray(2) = p2
   p = triu (matrs=parray)
   call display ( p , msg = 'triu(matrs = parray))')
!
!- version 4bis) an array of two pk2 objects (--> call to pk2f_TRIUwrp):
!     
   p = triu (matrs=[p1,p2])
   call display ( p , msg = 'triu(matrs = [p1,p2]))')  

!
!- TRIL
!  ^^^^ 
!
!- version 1) passing an intrinsic array and an integer (kdiag) (--> call to pk2f_GetRtril):
!   
   p = tril (matrix = Imat, k = kdiag) ! 
   call display ( p , msg = 'tril(matrix = Imat, k ='//trim(cdiag)//')')   
   p = tril (matrix = Rmat, k = kdiag) ! 
   call display ( p , msg = 'tril(matrix = Rmat, k = '//trim(cdiag)//')')
   p = tril (matrix = Cmat, k = kdiag) ! 
   call display ( p , msg = 'tril(matrix = Cmat, k = '//trim(cdiag)//')')
   p = tril (matrix = Lmat, k = kdiag) ! 
   call display ( p , msg = 'tril(matrix = Lmat, k = '//trim(cdiag)//')')
   p = tril (matrix = Smat, k = kdiag) ! 
   call display ( p , msg = 'tril(matrix = Smat, k = '//trim(cdiag)//')')

!
!- version 2) passing a pk2 object (p1) and an integer (kdiag) (—-> call to pk2f_TRILn):
!      
   p1 = Rmat
   p = tril (a = p1, k = kdiag)
   call display (p , msg = 'tril(a = p1, k = '//trim(cdiag)//')')
!
!- version 3) passing two pk2 objects (p1 and p2) (--> call to pk2f_TRIL):
!     
   p2 = kdiag
   p = tril (a = p1, b = p2)
   call display ( p , msg = 'tril(a = p1, b = p2)')
!
!- version 4) passing an array of two pk2 objects (--> call to pk2f_TRILwrp):
!     
   parray(1) = p1 ; parray(2) = p2
   p = tril (matrs=parray)
   call display ( p , msg = 'tril(matrs = parray))')
!
!- version 4bis) an array of two pk2 objects (--> call to pk2f_TRILwrp):
!     
   p = tril (matrs=[p1,p2])
   call display ( p , msg = 'tril(matrs = [p1,p2]))')   
    
   end subroutine test_TRIU_TRIL

!=============================================================================================   
   subroutine display ( a, msg, symb )
!=============================================================================================   
   type     (pk2_t),           intent(in) :: a
   character(len=*), optional, intent(in) :: msg
   character(len=1), optional, intent(in) :: symb
!---------------------------------------------------------------------------------------------    
!  Displays the variable "a".
!
!  If "msg" is present, prints "msg" and "a" in the form << msg  = {value(s) of a} >>.
!  If "symb" is present, replaces the "=" by symb.
!
!  Note: dispmodule is used.
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------   
   character(len=:), allocatable :: str, csymb
   character(len=:), allocatable :: res(:,:)
   character(len=9)              :: fmt
   integer                       :: nfmt
!---------------------------------------------------------------------------------------------    
   
   if (.not. verbos) return
   
   if (present(msg)) then
      str = msg
   else
      if (allocated(a%name)) then
         str = a%name
      else
         str = ''
      end if   
   end if
   
   if (present(symb)) then
      csymb = ' '//symb//' '
   else
      csymb = ' = '
   end if      
         
   print*

   if (a%typ == EMPTY .or. a%nrow * a%ncol == 0) then
      call disp(str // csymb // '[ ]')
      return
   end if
            
   select type (p=>a%m)
      type is (ik2_t)
         call disp(str // csymb,p%v)
      type is (rk2_t)
         call disp(str // csymb,p%v)
      type is (ck2_t)
         call disp(str // csymb,p%v)
      type is (lk2_t)
         call disp(str // csymb,p%v)
      type is (sk2_t)       
         call p%GetMat(Ch=res)
         fmt = 'a0'
         if (all(len_trim(res(1,:)) == 0)) then
            nfmt = max(1,maxval(len_trim(res)))
            write(fmt,'(a,i0)')'a',nfmt 
         end if     
         call disp(str // csymb,res,fmt=fmt)
   end select
      
   end subroutine display
   

end program Check_pk2f_dec19
