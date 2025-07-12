! For compiling (or use the makefile):

! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran Check_pk2moveAlloc.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_pk2movealloc

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort Check_pk2moveAlloc.f90 -L../../lib/ifort -lpk2 -llapack -lblas -o check_pk2movealloc

! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor Check_pk2MoveAlloc.f90 -L../../lib/nagfor -lpk2 -llapack -lblas -o check_pk2movealloc

program Check_pk2moveAlloc

!
!- Test moveAlloc procedures
!
   use pk2mod_m
   use, intrinsic :: ieee_arithmetic
   
   implicit none
   
   logical           :: verbos = .false.
   character(len=99) :: buf
   real              :: t1, t2, tp1, tp2
   integer           :: i,j,k,iter, niter = 50000


   integer  (Ikind), parameter   :: n1=2, m1=3, n2=2000, m2=300
   integer  (Ikind)              :: I1(n1,m1), I2(n2,m2) 
   real     (Rkind)              :: R1(n1,m1), R2(n2,m2)
   complex  (Rkind)              :: C1(n1,m1), C2(n2,m2)
   logical                       :: L1(n1,m1), L2(n2,m2)
   type     (str_t)              :: S1(n1,m1), S2(n2,m2)
   
   call SignalHandler_SignalCatch (unit = STDOUT, title = '--> Check_pk2:')
   
   call err_SetHaltingMode ( halting = .true., DisplayWarning = .false. ) ! stop if error    
!
!- Some matrices samples:
!
   k = 0 ; do j = 1, m1; do i = 1, n1 ; k = k+1 ; I1(i,j) = k ; enddo ; enddo
   k = 0 ; do j = 1, m2; do i = 1, n2 ; k = k+1 ; I2(i,j) = k ; enddo ; enddo
   R1(:,:) = I1(:,:)*RONE ; R2(:,:) = I2(:,:)*RONE
   C1(:,:) = R1(:,:) + R1(:,:)*CIMAG ; C2(:,:) = R2(:,:) + R2(:,:)*CIMAG
   L1(:,:) = I1(:,:) <= 4 ; L2(:,:) = I2(:,:) <= 4
   do j = 1, m1 ; do i = 1, n1 ; 
      write(buf,'(i0)') I1(i,j) ; S1(i,j)%str = ('!'//trim(buf)//'!')
   enddo ; enddo
   do j = 1, m2 ; do i = 1, n2 ; 
      write(buf,'(i0)') I2(i,j) ; S2(i,j)%str = ('!'//trim(buf)//'!')
   enddo ; enddo


   write(*,'(/,a,a)',advance = 'no')'test with a long loop to check the memory usage? ', &
                                    '(and no result displayed) (yes=return): '
   read(*,'(a)') buf
   print*
   
   if (len_trim(buf) /= 0) then
      verbos = .true. 
      niter = 1
   end if   
    
   call cpu_time(t1)  ; tp1 = t1

   do iter = 1, niter
!
!-    Move allocation from a pk2 to a pk2:
!
      call test_move_from_pk2_to_pk2
!
!-    Move allocation from pk2 to an intrinsic:
!
      call test_move_from_pk2_to_intrinsic
!
!-    Move allocation from an intrinsic to a pk2:
!
      call test_move_from_intrinsic_to_pk2

      if (mod(iter,10)==0) then
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
   print '(a)',"Terminated (hit return)"   
   read*
contains



!=============================================================================================   
   subroutine test_move_from_pk2_to_intrinsic
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!  Move allocation from a pk2 to intrinsic
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------
   type     (pk2_t)              :: a, pi, pr, pc, pl, ps
   integer  (Ikind), allocatable :: Imat(:,:)
   real     (Rkind), allocatable :: Rmat(:,:)
   complex  (Rkind), allocatable :: Cmat(:,:)
   logical         , allocatable :: Lmat(:,:)
   type     (str_t), allocatable :: Smat(:,:)
   integer  (Ikind)              :: i, j
   real                          :: t1, t2
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing Move Allocation from a pk2 to an intrinsic array *******'
      print*
   end if   
      
   a = I1
   call pk2_movealloc ( from = a, to = Imat )
   if (verbos) then   
      print*
      if (allocated(Imat)) then
         print*,'movealloc from "a" to "Imat" gives Imat = '
         do i = 1, size(Imat,1) ; print*,(Imat(i,j),j=1,size(Imat,2)) ; end do
         deallocate(Imat)
      else
         stop 'Error, Imat is not allocated'
      end if
   end if   
   
   a = R1
   call pk2_movealloc ( from = a, to = Rmat )
   if (verbos) then   
      print*
      if (allocated(Rmat)) then
         print*,'movealloc from "a" to "Rmat" gives Rmat = '
         do i = 1, size(Rmat,1) ; print*,(Rmat(i,j),j=1,size(Rmat,2)) ; end do
         deallocate(Rmat)
      else
         stop 'Error, Rmat is not allocated'
      end if
   end if   
 
   a = C1
   call pk2_movealloc ( from = a, to = Cmat )
   if (verbos) then   
      print*
      if (allocated(Cmat)) then
         print*,'movealloc from "a" to "Cmat" gives Cmat = '
         do i = 1, size(Cmat,1) ; print*,(Cmat(i,j),j=1,size(Cmat,2)) ; end do
         deallocate(Cmat)
      else
         stop 'Error, Cmat is not allocated'
      end if
   end if   

   a = L1
   call pk2_movealloc ( from = a, to = Lmat )
   if (verbos) then   
      print*
      if (allocated(Lmat)) then
         print*,'movealloc from "a" to "Lmat" gives Lmat = '
         do i = 1, size(Lmat,1) ; print*,(Lmat(i,j),j=1,size(Lmat,2)) ; end do
         deallocate(Lmat)
      else
         stop 'Error, Lmat is not allocated'
      end if
   end if   

   a = S1
   call pk2_movealloc ( from = a, to = Smat )
   if (verbos) then   
      print*
      if (allocated(Smat)) then
         print*,'movealloc from "a" to "Smat" gives Smat = '
         do i = 1, size(Smat,1) ; print*,(Smat(i,j)%str,' ',j=1,size(Smat,2)) ; end do
         deallocate(Smat)
      else
         stop 'Error, Smat is not allocated'
      end if
   end if   

!
!- test move alloc when A is empty:
!   
   call pk2_movealloc ( from = a, to = Imat )
   if (verbos) then   
      print*
      if (allocated(Smat)) then
         print*,'movealloc from "a" (empty) to "Imat" gives Imat = '
         do i = 1, size(Smat,1) ; print*,(Smat(i,j)%str,' ',j=1,size(Smat,2)) ; end do
         deallocate(Smat)
      else
         print*,'movealloc from "a" (empty) to "Imat": Imat is not allocated '
      end if
   end if   

!
!- Intentional error:
!
   call err_SetHaltingMode ( halting = .false. ) ! enable continuation after an error 
   
   a = I1
   call pk2_movealloc ( from = a, to = Lmat )
   if (verbos) then
      call opflag%display(title='INTENTIONAL ERROR')
   else
      opflag = err_t() ! reset opflag (as opflag%display is not called)
   end if
   
   call err_SetHaltingMode ( halting = .true. ) ! reset halting after an error 
   
!   
!- same with "big" arrays (prefer "call assign" than "=")
!
   call cpu_time(t1)
   
      call pk2_assign(pi, I2) 
      call pk2_assign(pr, R2) 
      call pk2_assign(pc, C2)  
      call pk2_assign(pl, L2)  
      call pk2_assign(ps, S2)  
   call cpu_time(t2)
   if (verbos) then
      print*
      print*,'cpu time for assignment from intrinsics to pk2     :', t2-t1
   end if   
   
   call cpu_time(t1)   
      call pk2_movealloc ( from = pi, to = Imat )
      call pk2_movealloc ( from = pr, to = Rmat )
      call pk2_movealloc ( from = pc, to = Cmat )
      call pk2_movealloc ( from = pl, to = Lmat )
      call pk2_movealloc ( from = ps, to = Smat )
   call cpu_time(t2)
   if (verbos) then
      print*,'cpu time for move allocation from pk2 to intrinsics:', t2-t1
   end if   
   
   end subroutine test_move_from_pk2_to_intrinsic
   
   
!=============================================================================================   
   subroutine test_move_from_intrinsic_to_pk2
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!  Move allocation from a pk2 to a pk2
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------
   type     (pk2_t)              :: a, pi, pr, pc, pl, ps
   integer  (Ikind), allocatable :: Imat(:,:)
   real     (Rkind), allocatable :: Rmat(:,:)
   complex  (Rkind), allocatable :: Cmat(:,:)
   logical         , allocatable :: Lmat(:,:)
   type     (str_t), allocatable :: Smat(:,:)
   real                          :: t1, t2
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing Move Allocation from an intrinsic type array to a pk2 *******'
      print*
   end if   
      
   allocate(Imat(n1,m1)) ; Imat = I1
   allocate(Rmat(n1,m1)) ; Rmat = R1 
   allocate(Cmat(n1,m1)) ; Cmat = C1 
   allocate(Lmat(n1,m1)) ; Lmat = L1 
   allocate(Smat(n1,m1)) ; Smat = S1 
   
   a%name = 'A'
   call pk2_moveAlloc ( from = Imat, to = a )
   call display (A,msg='movealloc from "Imat" to "a"',printname=.true.)
   call pk2_movealloc ( from = Rmat, to = a )
   call display (A,msg='movealloc from "Rmat" to "a"',printname=.true.)
   call pk2_moveAlloc ( from = Cmat, to = a )
   call display (A,msg='movealloc from "Cmat" to "a"',printname=.true.)
   call pk2_moveAlloc ( from = Lmat, to = a )
   call display (A,msg='movealloc from "Lmat" to "a"',printname=.true.)
   call pk2_moveAlloc ( from = Smat, to = a )
   call display (A,msg='movealloc from "Smat" to "a"',printname=.true.)

   call pk2_moveAlloc ( from = Smat, to = a )
   call display (A,msg='movealloc from "Smat" (non allocated) to "a"',printname=.true.)

!   
!- same with "big" arrays and time comparison used for allocation of intrinsics:
!

   call cpu_time(t1)
      allocate(Imat(n2,m2)) ; Imat = I2
      allocate(Rmat(n2,m2)) ; Rmat = R2
      allocate(Cmat(n2,m2)) ; Cmat = C2  
      allocate(Lmat(n2,m2)) ; Lmat = L2  
      allocate(Smat(n2,m2)) ; Smat = S2  
   call cpu_time(t2)
   if (verbos) then
      print*
      print*,'cpu time for allocation and assignment of intrinsics:', t2-t1
   end if   
   
   a%name = 'A'
   call cpu_time(t1)   
      call pk2_movealloc ( from = Imat, to = A )
      call pk2_movealloc ( from = Rmat, to = A )
      call pk2_moveAlloc ( from = Cmat, to = A )
      call pk2_moveAlloc ( from = Lmat, to = A )
      call pk2_moveAlloc ( from = Smat, to = A )
   call cpu_time(t2)   
   if (verbos) then
      print*,'cpu time for move allocation from intrinsics to pk2 :', t2-t1
   end if   

   call cpu_time(t1)
      call pk2_assign(pi, I2) 
      call pk2_assign(pr, R2) 
      call pk2_assign(pc, C2)  
      call pk2_assign(pl, L2)  
      call pk2_assign(ps, S2)  
   call cpu_time(t2)
   if (verbos) then
      print*,'cpu time for assignment from intrinsics to pk2      :', t2-t1
   end if   
   
   
   end subroutine test_move_from_intrinsic_to_pk2
   

!=============================================================================================   
   subroutine test_move_from_pk2_to_pk2
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!  Move allocation from a pk2 to a pk2
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b, pi, pr, pc, pl, ps
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing Move Allocation from a pk2 to a pk2 *******'
      print*
   end if   
   
   a = I1 ; a%name = 'A'
   call display(a, 'Let a')
   
   b = rand(4_Ikind, 5_Ikind) ; b%name = 'B'
   call display(b, 'and b')

   call pk2_movealloc (from = a, to = b, movename = .false.)
   call display(a, 'Then after moveAlloc from "a" to "b" (with movename = .false.) a is')
   if (verbos) then
      print*
      print*,'allocated(a%m)       : ',allocated(a%m)
      print*,'allocated(a%name)    : ',allocated(a%name)
      print*,'a%typ, a%nrow, a%ncol: ',a%typ, a%nrow, a%ncol
   end if
   
   call display(b,msg='and b ',printname=.true.)

   a = rand(5_Ikind, 2_Ikind) ; a%name = 'Anew'
   call display(a, 'Let a',printname=.true.)
   
   call pk2_movealloc (from = b, to = a)
   call display(A, 'Then after moveAlloc from "b" to "a" (with movename = .true.) a is', &
                printname=.true.)
   
   call display(b,msg='and b ')
   if (verbos) then
      print*
      print*,'allocated(b%m)       : ',allocated(b%m)
      print*,'allocated(b%name)    : ',allocated(b%name)
      print*,'b%typ, b%nrow, b%ncol: ',b%typ, b%nrow, b%ncol
   end if
   
   call pk2_movealloc (from = b, to = a)
   call display(a, 'moveAlloc from "b" (empty) to "a" (with movename = .true.) gives')

!   
!- same with "big" arrays and comparison with assignments (prefer "call assign" than "=")
!
   call pk2_assign(pi, I2) 
   call pk2_assign(pr, R2) 
   call pk2_assign(pc, C2) 
   call pk2_assign(pl, L2) 
   call pk2_assign(ps, S2)       

   call cpu_time(t1)   
      call pk2_assign(a, pi) 
      call pk2_assign(a, pr) 
      call pk2_assign(a, pc) 
      call pk2_assign(a, pl) 
      call pk2_assign(a, ps)       
   call cpu_time(t2)
   if (verbos) then
      print*
      print*,'cpu time for assignment from pk2 to pk2 (v1):', t2-t1
   end if   
   
   call a%destroy()

   call cpu_time(t1)   
      a = pi
      a = pr
      a = pc
      a = pl
      a = ps
   call cpu_time(t2)
   if (verbos) then
      print*,'cpu time for assignment from pk2 to pk2 (v2):', t2-t1
   end if      

   call cpu_time(t1)        
      call pk2_movealloc ( from = pi, to = b )     
      call pk2_movealloc ( from = pr, to = b )     
      call pk2_movealloc ( from = pc, to = b )     
      call pk2_movealloc ( from = pl, to = b )     
      call pk2_movealloc ( from = ps, to = b )     
   call cpu_time(t2)   
   if (verbos) then
      print*,'cpu time for move allocation from pk2 to pk2:', t2-t1
   end if   
     
   end subroutine test_move_from_pk2_to_pk2
   

!=============================================================================================   
   subroutine display ( a , msg, symb, printname )
!=============================================================================================   
   type     (pk2_t),           intent(in) :: a
   character(len=*), optional, intent(in) :: msg
   character(len=1), optional, intent(in) :: symb
   logical         , optional, intent(in) :: printname
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
      if (present(printname)) then
         if (printname) then
            if (allocated(a%name)) then
               str = str // ' (name = '// a%name //')'
            else
               str = str // ' (no name) '
            end if
         end if
      end if
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
            print*,'nfmt=',nfmt
            write(fmt,'(a,i0)')'a',nfmt 
         end if     
         call disp(str // csymb, res, fmt = fmt)
   end select
      
   end subroutine display
   
end
