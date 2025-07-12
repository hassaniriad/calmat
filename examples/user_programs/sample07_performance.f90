program sample7_performance
!
!  A small test to explore the performance of calmat (even though performance was not an
!  objective taken into account).
!
!  We consider a slightly complicated expression:
!
!   "y = y + dt*( B*cos(A*x-(i*dt) ) .* ( 1-exp(-i*dt*abs(x+B*x)) ) .* sin( B'*x*i*dt) )"  (1)
!
!  evaluated for nsample values of i and where 
!
!  . A and B are two matrices of size n x n, 
!  . x and y are two matrices of size n x 1,
!  . dt is a given number (a 1x1 matrix).
!
!  We compare the elapsed time using 4 methods:
!
!  A) using calmat with an embedded for-loop in the expression, i.e. the expression passed to 
!     calmat is 
!
!            "for i=1:nsample, y = y + dt * ... ; endfor"
!
!  B) calmat with an external do-loop. In this version, the expression (1) is first parsed 
!     before the do-loop (as illustrated in "sample5_parseThenEvaluate.f90") 
!
!  C) a direct calculation using intrinsic variables (which will give us a reference time)
!
!  D) a direct calculation using only pk2_t variables
!---------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
   logical, parameter          :: YES = .true.,  NO = .false.
   
   integer(Ikind)              :: flowId, i, n, nsample
   real   (Rkind), allocatable :: rA(:,:), rB(:,:), rX(:,:), rY(:,:), rDt
   real   (Rkind)              :: tA, tB, tC, tD
   type   (err_t)              :: stat 
!---------------------------------------------------------------------------------------------

!
!- All of the 4 tests are conducted with the same following set of data:
!
   n = 421 ; ! size of the matrices A, B, x
   nsample = 4000 ; ! number of evaluations (number of terms in the sum)
   
   rDt = 1.0_Rkind / nsample
   rA = util_random ( n, n, stat )
   rB = util_random ( n, n, stat )
   rX = util_random ( n, 1, stat )
   
!  ==============
   MethodA: block  
!  ==============
      real(Rkind), pointer :: y_ptr(:,:)=>NULL() ! will be used to collect the computed y
!
!-    Method A: we call calmat with a for-loop in the supplied expression
!
      print '(a)','*******************************************************************'
      print '(a)','Method A: we call calmat with a for-loop in the supplied expression'
      print '(a)','*******************************************************************'

      tA = util_wtime() ! start the timer

!     A first call to calmat is used to define in the calmat workspace the involved variables.
!     Note: we take this occasion to initialize y to 0.0 (real):

      call calmat ( exprIn = 'A=%i; B=[]; x=[]; dt=[]; i=[]; nsample=[]; y=0.0;', &
                    warning = YES, welcome = NO )
   
!     Set the value of the involved variables:
      call Calmat_copy ( from = rX     , to = 'x' )
      call Calmat_copy ( from = rA     , to = 'A' )
      call Calmat_copy ( from = rB     , to = 'B' )
      call Calmat_copy ( from = nsample, to = 'nsample' )
      call Calmat_copy ( from = rDt    , to = 'dt')

!     Now parse and evaluate the expression:

      call calmat ( exprIn = "for i=1:nsample ; "                       // &
                             "     y = y + dt*(B*cos(A*x-(i*dt)).* "    // &
                             "            (1-exp(-i*dt*abs(x+B*x))).* " // &
                             "            sin(B'*x*i*dt));"             // &
                             "endfor;" )

      tA = util_wtime(tA) ! stop the timer
      
      print '(a,f9.2,a)','Elapsed time: ',tA,' seconds'

!     Print only the first 5 components of y:

      call Calmat_getPointer ( var = 'y', ptr = y_ptr ) 

      print '(a)', 'the first 5 elements of y:'
      do i = 1, min(5,n) ; print*,y_ptr(i,1) ; end do
      nullify(y_ptr)

      call calmat( exprIn = 'clear' )
   end block MethodA
   
!  ==============
   MethodB: block
!  ==============
      integer(Ikind), pointer :: i_ptr=>NULL()      ! will be used to update the value of i
      real   (Rkind), pointer :: y_ptr(:,:)=>NULL() ! will be used to collect the computed y
!
!-    Method B: we call calmat to evaluate the expression for a given i and use a do-loop
!
      print '(a)','*******************************************************************'
      print '(a)','Method B: we call calmat nsample times with an external do-loop'
      print '(a)','*******************************************************************'

      tB = util_wtime() ! start the timer

!     A first call to calmat just to define in the calmat workspace the involved variables.
!     Note: we take this occasion to initialize i to 0 (integer) and y to 0.0 (real):
!
      call calmat ( exprIn = 'A=[]; B=[]; x=[]; dt=[]; i=0; nsample=[]; y=0.0;', &
                    warning = YES, welcome = NO )
   
!     Set the involved variables:

      call Calmat_copy ( from = rX     , to = 'x' )
      call Calmat_copy ( from = rA     , to = 'A' )
      call Calmat_copy ( from = rB     , to = 'B' )
      call Calmat_copy ( from = rDt    , to = 'dt' )
   
!     Analyse the expression only once:

      call calmat ( parseOnly = flowId, exprIn = &
                "y = y + dt*(B*cos(A*x-(i*dt)).* (1-exp(-i*dt*abs(x+B*x))).* sin(B'*x*i*dt));")

!     Now evaluate the expression for each value of i:

      call Calmat_getPointer ( var = 'i', ptr = i_ptr ) 

      do i = 1, nsample
         i_ptr = i
         call Calmat ( evaluate = flowId )
      end do
   
      tB = util_wtime(tB) ! stop the timer
      
      print '(a,f9.2,a)','Elapsed time: ',tB,' seconds'

!     Print only the first 5 components of y:

      call Calmat_getPointer ( var = 'y', ptr = y_ptr ) 

      print '(a)', 'the first 5 elements of y:'
      do i = 1, min(5,n) ; print*,y_ptr(i,1) ; end do
      nullify(i_ptr, y_ptr)

      call calmat( exprIn = 'clear' )
      
   end block MethodB

!  ==============
   MethodC: block
!  ==============
!
!-    Method C: A direct calculation
!
      print '(a)','*******************************************************************'
      print '(a)','Method C: with a direct calculation'
      print '(a)','*******************************************************************'

      ! Note: matmul is too slow, I use here blas for matrix multiplication:
   
      tC = util_wtime() ! start the timer
      
      allocate(rY(n,1), source = 0.0_Rkind)
      do i = 1, nsample
         rY = rY + rDt * (                                                     &
                           prodblas(rB,cos(prodblas(rA,rX)-i*rDt))           * &
                           (1.0_Rkind - exp(-i*rDt*abs(prodblas(rB,rX)+rX))) * &
                           sin(prodblas(transpose(rB),rX)*i*rDt)               &
                         )
      end do

      tC = util_wtime(tC) ! stop the timer
      
      print '(a,f9.2,a)','Elapsed time: ',tC,' seconds'

!     Print only the first 5 components of y:

      print '(a)', 'the first 5 elements of y:'
      do i = 1, min(5,n) ; print*,rY(i,1) ; end do

   end block MethodC

!  ==============   
   MethodD: block
!  ==============
!
!-    Method D: A direct calculation with the pk2 library
!
      type(pk2_t)          :: A, B, x, y ! declare the involved variables
      real(Rkind), pointer :: y_ptr(:,:)=>NULL() ! will be used to collect the computed y
      
      print '(a)','*******************************************************************'
      print '(a)','Method D: with a direct calculation using the pk2 library'
      print '(a)','*******************************************************************'

      tD = util_wtime() ! start the timer

      A = rA ; B = rB ; x = rX ; y = 0.0_Rkind

      do i = 1, nsample
         y = y + rDt * &
        ( B*cos(A*x-i*rDt) .m. (1.0_Rkind - exp(-i*rDt*abs(x+B*x))) .m. sin(trans(B)*x*i*rDt) )
      end do
   
      tD = util_wtime(tD) ! stop the timer
      
      print '(a,f9.2,a)','Elapsed time: ',tD,' seconds'

!     Print only the first 5 components of y:

      call y%pointer(y_ptr)

      print '(a)', 'the first 5 elements of y:'
      do i = 1, min(5,n)
         print*,y_ptr(i,1)
      end do
   
      nullify(y_ptr)
      
   end block MethodD
   
   print*
   print '(a)','Absolute elapsed times, Ratio (Compared to method C):'
   print '(a,2(f9.2,a))','. Method A (Calmat with embedded loop): tA = ',tA,', tA/tC = ',tA/tC
   print '(a,2(f9.2,a))','. Method B (Calmat with external loop): tB = ',tB,', tB/tC = ',tB/tC
   print '(a,2(f9.2,a))','. Method C (Direct calculation)       : tC = ',tC,', tC/tC = ',tC/tC
   print '(a,2(f9.2,a))','. Method D (Direct calculation by pk2): tD = ',tD,', tD/tC = ',tD/tC

   print*

contains

   function prodblas ( a, b ) result(res)
   use LapackInterface_m, only: LapackInterface_gemm
   real(Rkind), intent(in) :: a(:,:), b(:,:)
   real(Rkind)             :: res(size(a,1),size(b,2))
   
   integer(Ikind) :: n1, m1, n2, m2
   
   n1 = size(a,1) ; m1 = size(a,2)
   n2 = size(b,1) ; m2 = size(b,2)
   
   if ( m1 /= n2 ) stop "In prodblas: incompatible shapes"
   
   call LapackInterface_gemm ("N", "N", n1, m2, m1, RONE, a, n1, b, n2, RZERO, res, n1)  
    
   end function prodblas
   
end program sample7_performance
