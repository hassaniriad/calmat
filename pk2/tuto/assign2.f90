program assign2

!---------------------------------------------------------------------------------------------
!
!- A small demo showing how to use the pk2 library
!
!  . Some efficient ways for assigning "large" arrays or transferring data
!  . and known issue with ifort (overflow of the stack)
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod assign2.f90 -L$dirlib -lpk2 -lblas -llapack -o assign2
!  ifort    -fpp -I $dirmod assign2.f90 -L$dirlib -lpk2 -lblas -llapack -o assign2 [-heap_arrays]
!  nagfor   -fpp -I $dirmod assign2.f90 -L$dirlib -lpk2 -lblas -llapack -kind=byte -o assign2
!
!  where $dirmod is ../mod/$comp  and $dirlib is ./../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------

   use pk2mod_m
!
!- Example of declarations of pk2 variables:
!
   type     (pk2_t)              :: a, b, c, d
   real     (Rkind), allocatable :: R(:,:)
   integer  (Ikind)              :: n = 6543, m = 9876
   character(len=1)              :: jump   
   real                          :: t1, t2


   write(*,'(/,a)')'Warning: depending of the array sizes (n and m), this example may crashes'
   write(*,'(a  )')'         (stack overflow) when compiled with intel ifort or ifx without'
   write(*,'(a  )')'         the flag option "-heap_arrays" or "-heap-arrays".'
   write(*,'(a,/)')'         The statements in question are the assignments "a = R" or "b = a"'
   write(*,'(a  )')'         An altenative is to let the stack grows unlimited. On linux/mac:'
   write(*,'(a  )')'         ulimit -s unlimited before running this program.'
   write(*,'(a)',advance='no')'Jump these statements (y/n)?: '
   read*, jump
   write(*,*)
   
   a%name = 'a' ; b%name = 'b' ; c%name = 'c' ; d%name = 'd'
!
!- Let given a "big" real array R:
!
   allocate(R(n,m), source = 1.234_Rkind)   
   
   if (jump == 'y') goto 1
!
!- We can assign to "a" this real array by using the overloaded assignment "a = R". This will
!  in fact call the generic subroutine "pk2_assign".

   call cpu_time(t1)

      a = R  ! <-- copy R into a (it calls pk2_assign)

   call cpu_time(t2) ; print*,'. cpu time for a = R         :',t2-t1
!
!- similarly we can assign the content of "a" to another pk2 variable:
!
   call cpu_time(t1)
   
      b = a  ! <-- copy a into b (it calls pk2_assign)
   
   call cpu_time(t2) ; print*,'. cpu time for b = a         :',t2-t1     
!
!  However the statement "a=R" (or "b=a") is not quite the same as "call pk2_assign( a, R )" 
!  but rather "call pk2_assign( a, (R) ): that is the rhs is first evaluated and is copied
!  onto the stack or onto the heap, depending on the compiler and its options.
!
!  In some situations the stack can overflow. A workarround is to increase the stack (by using
!  "ulimit" on linux and mac os) or to add appropriate compilation options for using the heap  
!  rather than the stack (with ifort: -heap_arrays or -heap-arrays)
!
!  Another solution (recommended), when dealing with large arrays, is to call directly the
!  subroutine "pk2_assign" instead of doing an assignment in order to avoid the overhead due
!  to a temporary copy of rhs:
!   
1  call a%destroy() ; call b%destroy() ; a%name = 'a' ; b%name = 'b'
   
   call cpu_time(t1)

      call pk2_assign ( a, R )  ! <-- copy R into a
      
   call cpu_time(t2) ; print*,'. cpu time for assign(a,R)   :',t2-t1
!
!- or little more explicit: 
!
      call pk2_assign ( lhs = a, rhs = R )
!
!- And of course this can be used between two pk2 variables
!
   call cpu_time(t1)

      call pk2_assign ( lhs = b, rhs = a )  ! <-- copy a into b

   call cpu_time(t2) ; print*,'. cpu time for assign(b,a)   :',t2-t1   
      
!
!- Another useful thing to know: when the right-hand-side is no longer needed, a more 
!  intersting solution is to transfer rhs into lhs (move allocation) by using the generic 
!  routine "pk2_moveAlloc". This is faster and the rhs is automatically deallocated:
!
   call cpu_time(t1)

      call pk2_moveAlloc ( from = R, to = c )  ! <-- from intrinsic (R) to pk2 (c)

   call cpu_time(t2) ; print*,'. cpu time for moveAlloc(R,c):',t2-t1
      
   call cpu_time(t1)

      call pk2_moveAlloc ( from = a, to = d, movename = .false. ) ! from pk2 (a) to pk2 (d)
      
   call cpu_time(t2) ; print*,'. cpu time for moveAlloc(a,d):',t2-t1

   print*   
   print*,'allocated(R): ',allocated(R)
   call a%Printme(form='novalues',msg='display size and type of a:')
   call b%Printme(form='novalues',msg='display size and type of b:')
   call c%Printme(form='novalues',msg='display size and type of c:')
   call d%Printme(form='novalues',msg='display size and type of d:')


   write(*,'(/,a)')'Terminated'

end program assign2
