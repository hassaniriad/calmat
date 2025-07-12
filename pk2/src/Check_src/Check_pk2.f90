! For compiling (or use the makefile):

! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran check_pk2.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_pk2

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort check_pk2.f90 -L../../lib/ifort -lpk2 -llapack -lblas -o check_pk2

! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor check_pk2.f90 -L../../lib/nagfor -lpk2 -llapack -lblas -o check_pk2

program Check_pk2

!
!- Test the module pk2
!
   use pk2mod_m
   use, intrinsic :: ieee_arithmetic
   
   implicit none
   
   logical          :: verbos = .false.
   character(len=1) :: buf
   real             :: t1, t2, tp1, tp2
   integer          :: iter, niter = 10000
      
   call SignalHandler_SignalCatch (unit = STDOUT, title = '--> Check_pk2:')

   write(*,'(/,a,/,a)',advance = 'no') &
   'test with a long loop to check the memory usage? (no result displayed) (yes=return or "y")',&
   '(alternate: on mac os x use "leaks -atExit -- ./check_pk2"): '

   read(*,'(a)') buf
   print*
   
   if (len_trim(buf) /= 0) then
      verbos = .true. 
      niter = 1
   end if   
 
   call cpu_time(t1)  ; tp1 = t1

   do iter = 1, niter
!
!-    Check assignments:
!
      call test_assign
!
!-    Check addition:
!
      call test_add       
!
!-    Check subtraction:
!
      call test_minus_sub
!
!-    Check matrix product
!
      call test_mult      
!
!-    Check divide
!
      call test_div     
!
!-    Check exponentiation
!
      call test_pow        
!
!-    Check element-wise mutliplication
!
      call test_emult 
!
!-    Check element-wise divide
!
      call test_ediv  
!
!-    Check movealloc
!
      call test_movealloc
!
!-    Check copy
!
      call test_copy    
!
!-    Check .not., .and., .or.
!
      call test_not_and_or   
!
!-    Check > and >=
!
      call test_gt_ge
!
!-    Check < and <=
!
      call test_lt_le
!
!-    Check == and /=
!
      call test_eq_neq         
!
!-    Check DelRows
!
      call test_delrows   
!
!-    Check SetSubmat
!
      call test_SetSubmat1  
      call test_SetSubmat2 
!
!-    Check InsertInto
!
      call test_InsertInto
!
!-    Check pk2Insert
!
      call test_pk2Insert
!
!-    Check alloc1
!
      call test_alloc1 
!
!-    Check ExtracMat
!
      call test_ExtracSubMat
!
!-    Check ReadMat
!
      call test_ReadMat   
!
!-    Check GetMat
!
      call test_GetMat   
!
!-    Check GetMatPacked
!
      call test_GetMatPacked  
!
!-    check LU, LU2 and LUsolv
!
      call test_LU
      call test_LU2
      call test_LUsolv
!
!-    Check Pointer
!
      call test_Pointer

1     if (mod(iter,1000)==0) then
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
   subroutine test_Pointer
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!  
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type   (pk2_t)          :: a 
   integer(Ikind), pointer :: I, Ivec(:), Imat(:,:)
   real   (Rkind), pointer :: R, Rvec(:), Rmat(:,:)
   complex(Rkind), pointer :: C, Cvec(:), Cmat(:,:)
   logical       , pointer :: L, Lvec(:), Lmat(:,:)
   type   (str_t), pointer :: S, Svec(:), Smat(:,:)
   type   (err_t)          :: stat
   integer                 :: ii, jj, n = 5, m = 2
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing test_Pointer *******'
      print*
   end if   
!
!- 1) I => a(1,1), Ivec => a(:), Imat => a(:,:):
!      
   a = reshape([11,21,12,22,13,23,14,24,15,25],[n,m]) 
   call display(a, msg='Let be the array a')  
   if (verbos) & 
   print '(//,a,/)','1) I => a(1,1), I = I+1000'
      
   call a%Pointer(I)
   I = I + 1000   
   if (verbos) print*,'then I = ',I
   call display(a, msg='Now a is')  
   nullify(I)

   if (verbos) & 
   print '(//,a,/)','2) Ivec => a(:), Ivec = Ivec+1000'   
   call a%Pointer(Ivec)
   Ivec = Ivec + 1000
   if (verbos) print*,'then Ivec = ',Ivec
   call display(a, msg='Now a is')  
   nullify(Ivec)

   if (verbos) & 
   print '(//,a,/)','3) Imat => a(:,:), Imat = Imat+1000'   
   call a%Pointer(Imat)
   Imat = Imat + 1000
   if (verbos) then
      print*,'then Imat = '
      do ii = 1, size(Imat,1); print*,Imat(ii,:); enddo
   end if
   call display(a, msg='Now a is')  
   nullify(Imat)

!
!- 2) R => a(1,1), Rvec => a(:), Rmat => a(:,:):
!      
   a = reshape(RONE*[11,21,12,22,13,23,14,24,15,25],[n,m]) 
   call display(a, msg='Let be the array a')  
   if (verbos) & 
   print '(//,a,/)','1) R => a(1,1), R = R+1000'
      
   call a%Pointer(R)
   R = R + 1000   
   if (verbos) print*,'then R = ',R
   call display(a, msg='Now a is')  
   nullify(R)

   if (verbos) & 
   print '(//,a,/)','2) Rvec => a(:), Rvec = Rvec+1000'   
   call a%Pointer(Rvec)
   Rvec = Rvec + 1000
   if (verbos) print*,'then Rvec = ',Rvec
   call display(a, msg='Now a is')  
   nullify(Rvec)

   if (verbos) & 
   print '(//,a,/)','3) Rmat => a(:,:), Rmat = Rmat+1000'   
   call a%Pointer(Rmat)
   Rmat = Rmat + 1000
   if (verbos) then
      print*,'then Rmat = '
      do ii = 1, size(Rmat,1); print*,Rmat(ii,:); enddo
   end if
   call display(a, msg='Now a is')  
   nullify(Rmat)   

!
!- 3) C => a(1,1), Cvec => a(:), Cmat => a(:,:):
!      
   a = reshape(RONE*[11,21,12,22,13,23,14,24,15,25]+CIMAG*[11,21,12,22,13,23,14,24,15,25],[n,m]) 
   call display(a, msg='Let be the array a')  
   if (verbos) & 
   print '(//,a,/)','1) C => a(1,1), C = C+1000+1000i'
      
   call a%Pointer(C)
   C = C + 1000*(CONE+CIMAG)   
   if (verbos) print*,'then R = ',C
   call display(a, msg='Now a is')  
   nullify(C)

   if (verbos) & 
   print '(//,a,/)','2) Cvec => a(:), Cvec = Cvec+1000+1000i'   
   call a%Pointer(Cvec)
   Cvec = Cvec + 1000*(CONE+CIMAG)
   if (verbos) print*,'then Rvec = ',Cvec
   call display(a, msg='Now a is')  
   nullify(Cvec)

   if (verbos) & 
   print '(//,a,/)','3) Cmat => a(:,:), Cmat = Cmat+1000+1000i'   
   call a%Pointer(Cmat)
   Cmat = Cmat + 1000*(CONE+CIMAG)
   if (verbos) then
      print*,'then Cmat = '
      do ii = 1, size(Cmat,1); print*,Cmat(ii,:); enddo
   end if
   call display(a, msg='Now a is')  
   nullify(Cmat)      

!
!- 4) L => a(1,1), Lvec => a(:), Lmat => a(:,:):
!      
   a = reshape([.true.,.false.,.false.,.false.,.true.,.false.,.true.,.true.,.false.,.false.], &
               [n,m]) 
   call display(a, msg='Let be the array a')  
   if (verbos) & 
   print '(//,a,/)','1) L => a(1,1), L = .false.'
      
   call a%Pointer(L)
   L = .false.   
   if (verbos) print*,'then L = ',L
   call display(a, msg='Now a is')  
   nullify(L)

   if (verbos) & 
   print '(//,a,/)','2) Lvec => a(:), Lvec(n*m) = .true.'   
   call a%Pointer(Lvec)
   Lvec(n*m) = .true.
   if (verbos) print*,'then Lvec = ',Lvec
   call display(a, msg='Now a is')  
   nullify(Lvec)

   if (verbos) & 
   print '(//,a,/)','3) Lmat => a(:,:), Lmat(n,m) = .false.'   
   call a%Pointer(Lmat)
   Lmat(n,m) = .false.
   if (verbos) then
      print*,'then Lmat = '
      do ii = 1, size(Lmat,1); print*,Lmat(ii,:); enddo
   end if
   call display(a, msg='Now a is')  
   nullify(Lmat)       
!
!- 5) S => a(1,1), Svec => a(:), Smat => a(:,:):
!      
   a = reshape(['s11','s21','s31','s41','s51','s12','s22','s32','s42','s52'], &
               [5,2]) 
   call display(a, msg='Let be the array a')  
   if (verbos) & 
   print '(//,a,/)','1) S => a(1,1), L = "xxx"'
      
   call a%Pointer(S)
   S = 'xxx'   
   if (verbos) print*,'then S = ',S%str
   call display(a, msg='Now a is')  
   nullify(S)

   if (verbos) & 
   print '(//,a,/)','2) Svec => a(:), Svec(n*m) = Svec(n*m) + "_Y"'   
   call a%Pointer(Svec)
   Svec(n*m)%str = Svec(n*m)%str // '_Y'
   if (verbos) print*,'then Svec = ',(Svec(ii)%str,' ',ii=1,size(Svec))
   call display(a, msg='Now a is')  
   nullify(Svec)

   if (verbos) & 
   print '(//,a,/)','3) Smat => a(:,:), Smat(n,m) = Smat(n,m) + "Z"'   
   call a%Pointer(Smat)
   Smat(n,m)%str = Smat(n,m)%str // "Z"
   if (verbos) then
      print*,'then Smat = '
      do ii = 1, size(Smat,1); print*,(Smat(ii,jj)%str,' ',jj=1,size(Smat,2)); enddo
   end if
   call display(a, msg='Now a is')  
   nullify(Smat)    

   if (verbos) then
      call err_SetHaltingMode ( halting = .false. )   
      print '(//,a,/)', ansiColor_colorTxt(int(STDOUT),err_colorError,&
                                           "INTENTIONAL ERROR (Imat => a):")
      call a%Pointer(Imat,stat)
      call stat%Display()
      call err_SetHaltingMode ( halting = .true. )
   end if
        
   end subroutine test_Pointer
   
!=============================================================================================   
   subroutine test_LUSolv
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b, x, h
   type(err_t) :: stat
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing pk2_LUSolv *******'
      print*
   end if   

   a = reshape([8,5,8,1,6,4,6,2,7,8,7,7,7,3,3,7],[4,4])
   x = reshape([1,1,1,1],[4,1])*RONE ! the expected solution
   b = a*x ! the rhs
   
   x = 'hello' ! erase x

   call display(a, msg='Let a')
   call display(b, msg='and b')
     
   call pk2_LU2 ( a, h = h, stat = stat )   ! LU factorization of a
   call pk2_LUSolv ( x, h, b, stat = stat ) ! solve now a*x = b
   
   call display(x, msg='then the solution of a*x = b is',symb=':')
      
   end subroutine test_LUSolv


!=============================================================================================   
   subroutine test_LU2
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, l, u, p, h
   type(err_t) :: stat
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing pk2_LU2 *******'
      print*
   end if   

   a = reshape([1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4],[4,4])

   call display(a, msg='Let a')  
   
   call pk2_LU2 ( a, l = l, u = u, p = p, stat = stat )
   if (stat%code > 0) then
      call stat%display()
   else
      call display(l, msg='then l')
      call display(u, msg='and u')
      call display(p, msg='and p')
      call display(p * a - l * u, msg='verif.: p*a-l*u')
   end if

   a = reshape([1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4],[4,4])*RONE
   call display(a, msg='Let a')  
   call pk2_LU2 ( a, h = h, stat = stat )
   if (stat%code > 0) then
      call stat%display()
   else
      call display(h, msg='then h')
   end if
      
   end subroutine test_LU2


!=============================================================================================   
   subroutine test_LU
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, l, u, p
   type(err_t) :: stat
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing pk2_LU *******'
      print*
   end if   

   a = reshape([1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4],[4,4])

   call display(a, msg='Let a')  
   
   call pk2_LU ( a, l, u, p, stat )
   if (stat%code > 0) then
      call stat%display()
   else
      call display(l, msg='then l')
      call display(u, msg='and u')
      call display(p, msg='and p')
      call display(p * a - l * u, msg='verif.: p*a-l*u')
   end if
   
   a = reshape([1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4],[4,4])*RONE

   call display(a, msg='Let a')  
   
   call pk2_LU ( a, l, u, p, stat )
   if (stat%code > 0) then
      call stat%display()
   else
      call display(l, msg='then l')
      call display(u, msg='and u')
      call display(p, msg='and p')
      call display(p * a - l * u, msg='verif.: p*a-l*u')
   end if

   a = a*10_Ikind + a*CIMAG

   call display(a, msg='Let a')  
   
   call pk2_LU ( a, l, u, p, stat )
   if (stat%code > 0) then
      call stat%display()
   else
      call display(l, msg='then l')
      call display(u, msg='and u')
      call display(p, msg='and p')
      call display(p * a - l * u, msg='verif.: p*a-l*u')
   end if
   
   a = reshape([1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4],[5,4])

   call display(a, msg='Let a')  
   
   call pk2_LU ( a, l, u, p, stat )
   if (stat%code > 0) then
      call stat%display()
   else
      call display(l, msg='then l')
      call display(u, msg='and u')
      call display(p, msg='and p')
      call display(p * a - l * u, msg='verif.: p*a-l*u')
   end if
   
   end subroutine test_LU
   
   
!=============================================================================================   
   subroutine test_GetMat
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!  Copy a pk2 variable into an intrinsic variable or a bk2 variable
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type   (pk2_t)              :: a
   class  (bk2_t), allocatable :: b
   
   integer(Ikind), allocatable :: I(:,:)
   real   (Rkind), allocatable :: R(:,:)
   complex(Rkind), allocatable :: C(:,:), tmp(:,:)
   logical       , allocatable :: L(:,:)
   type   (str_t), allocatable :: S(:,:)
   integer(Ikind)              :: k, j
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing GetMat *******'
      print*
   end if   
!
!- 1) copy into unallocated arrays (I, R, C, L, S, b):
!   
   if (verbos) & 
   print '(//,a,/)','1) Copy into unallocated arrays'
   
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5]) 
   call display(a, msg='Let be the array a')  
   call a%GetMat(I)
   if (verbos) then
      print '(/,a)','Then a%GetMat(I) (with I unallocated) gives I ='
      do k = 1, size(I,1)
         write(*,*)(I(k,j),j=1,size(I,2))
      end do   
   end if
   
   a = reshape([11.,21.,12.,22.,13.,23.,14.,24.,15.,25.],[2,5])   
   call display(a, msg='Let be the array a')     
   call a%GetMat(R)
   if (verbos) then
      print '(/,a)','Then a%GetMat(R) (with R unallocated) gives R ='
      do k = 1, size(R,1)
         write(*,*)(R(k,j),j=1,size(R,2))
      end do   
   end if
   
   allocate(tmp, source = R + CIMAG*R) ; a = tmp ; deallocate(tmp)
   call display(a, msg='Let be the array a')        
   call a%GetMat(C)
   if (verbos) then
      print '(/,a)','Then a%GetMat(C) (with C unallocated) gives C ='
      do k = 1, size(C,1)
         write(*,*)(C(k,j),j=1,size(C,2))
      end do   
   end if
   
   a  = [.true.,.false.,.false.,.true.]
   call display(a, msg='Let be the array a')        
   call a%GetMat(L)
   if (verbos) then
      print '(/,a)','Then a%GetMat(L) (with L unallocated) gives L ='
      do k = 1, size(L,1)
         write(*,*)(L(k,j),j=1,size(L,2))
      end do   
   end if      
   
   a = pk2_t (matrix = reshape(['s11','s21','s12','s22'],[2,2]), name = 'Smat2')
   call display(a, msg='Let be the array a')           
   call a%GetMat(S)
   if (verbos) then
      print '(/,a)','Then a%GetMat(S) (with S unallocated) gives S ='
      do k = 1, size(S,1)
         write(*,*)(S(k,j)%str,' ',j=1,size(S,2))
      end do   
   end if   

!- copy into a polymorphic variable (bk2_t):

   call display(a, msg='Let be the array a')              
   call a%GetMat(b)
   a = b ! use a pk2 var to display the result
   call display(a,'Then a%GetMat(b) (with b (bk2_t) unallocated) gives b ')    
!
!- 2) copy into already allocated arrays (I, R, C, L, S, b) which have the right shape:
!
   if (verbos) &
   print '(//,a,/)','2) Copy into already allocated arrays which have the right shape'
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5]) 
   call display(a, msg='Let be the array a')  
   call a%GetMat(I)
   if (verbos) then
      print '(/,a,g0)','allocated(I):',allocated(I)
      print '(a,i0,1x,i0)','size(I): ',size(I,1),size(I,2)
      print '(/,a)','Then a%GetMat(I) (with I allocated and of same shape) gives I ='
      do k = 1, size(I,1)
         write(*,*)(I(k,j),j=1,size(I,2))
      end do   
   end if
   
   a = reshape([11.,21.,12.,22.,13.,23.,14.,24.,15.,25.],[2,5])   
   call display(a, msg='Let be the array a')     
   call a%GetMat(R)
   if (verbos) then
      print '(/,a,g0)','allocated(R):',allocated(R)
      print '(a,i0,1x,i0)','size(R): ',size(R,1),size(R,2)   
      print '(/,a)','Then a%GetMat(R) (with R allocated and of same shape) gives R ='
      do k = 1, size(R,1)
         write(*,*)(R(k,j),j=1,size(R,2))
      end do   
   end if
   
   allocate(tmp, source = R + CIMAG*R) ; a = tmp ; deallocate(tmp)
   call display(a, msg='Let be the array a')        
   call a%GetMat(C)
   if (verbos) then
      print '(/,a,g0)','allocated(C):',allocated(C)
      print '(a,i0,1x,i0)','size(C): ',size(C,1),size(C,2)      
      print '(/,a)','Then a%GetMat(C) (with C allocated and of same shape) gives C ='
      do k = 1, size(C,1)
         write(*,*)(C(k,j),j=1,size(C,2))
      end do   
   end if
   
   a  = [.true.,.false.,.false.,.true.]
   call display(a, msg='Let be the array a')        
   call a%GetMat(L)
   if (verbos) then
      print '(/,a,g0)','allocated(L):',allocated(L)
      print '(a,i0,1x,i0)','size(L): ',size(L,1),size(L,2)      
      print '(/,a)','Then a%GetMat(L) (with L allocated and of same shape) gives L ='
      do k = 1, size(L,1)
         write(*,*)(L(k,j),j=1,size(L,2))
      end do   
   end if      
   
   a = pk2_t (matrix = reshape(['s11','s21','s12','s22'],[2,2]), name = 'Smat2')
   call display(a, msg='Let be the array a')           
   call a%GetMat(S)
   if (verbos) then
      print '(/,a,g0)','allocated(S):',allocated(S)
      print '(a,i0,1x,i0)','size(S): ',size(S,1),size(S,2)      
      print '(/,a)','Then a%GetMat(S) (with S allocated and of same shape) gives S ='
      do k = 1, size(S,1)
         write(*,*)(S(k,j)%str,' ',j=1,size(S,2))
      end do   
   end if   

!- copy into a polymorphic variable (bk2_t):

   call display(a, msg='Let be the array a')              
   call a%GetMat(b)
   if (verbos) then
      print '(/,a,g0)','b%typ:',b%typ
      print '(a,i0,1x,i0)','b%nrow,b%ncol: ',b%nrow,b%ncol      
   end if
   a = b ! use a pk2 var to display the result
   call display(a,'Then a%GetMat(b) (with b allocated and of same shape) gives b') 


!
!- 3) copy into already allocated arrays (I, R, C, L, S, b) but which have not the right shape:
!
   if (verbos) &
   print '(//,a,/)','3) Copy into already allocated arrays but which have not the right shape'
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5]) 
   call display(a, msg='Let be the array a')  
   deallocate(I) ; allocate(I(1,1),source=999_Ikind)
   if (verbos) then
      print '(/,a,g0)','allocated(I):',allocated(I)
      print '(a,i0,1x,i0)','size(I): ',size(I,1),size(I,2)
   end if   
   call a%GetMat(I)
   if (verbos) then   
      print '(/,a)','Then a%GetMat(I) (with I allocated but not of the same shape) gives I ='
      do k = 1, size(I,1)
         write(*,*)(I(k,j),j=1,size(I,2))
      end do   
   end if
   
   a = reshape([11.,21.,12.,22.,13.,23.,14.,24.,15.,25.],[2,5])   
   call display(a, msg='Let be the array a')     
   deallocate(R) ; allocate(R(10,9),source=999.0_Rkind)
   if (verbos) then
      print '(/,a,g0)','allocated(R):',allocated(R)
      print '(a,i0,1x,i0)','size(R): ',size(R,1),size(R,2)   
   end if   
   call a%GetMat(R)
   if (verbos) then
      print '(/,a)','Then a%GetMat(R) (with R allocated but not of the same shape) gives R ='
      do k = 1, size(R,1)
         write(*,*)(R(k,j),j=1,size(R,2))
      end do   
   end if

   allocate(tmp, source = R + CIMAG*R) ; a = tmp ; deallocate(tmp)
   call display(a, msg='Let be the array a')        
   deallocate(C) ; allocate(C(10,9),source=999.+88*CIMAG)   
   if (verbos) then
      print '(/,a,g0)','allocated(C):',allocated(C)
      print '(a,i0,1x,i0)','size(C): ',size(C,1),size(C,2)      
   end if
   call a%GetMat(C)
   if (verbos) then
      print '(/,a)','Then a%GetMat(C) (with C allocated but not of the same shape) gives C ='
      do k = 1, size(C,1)
         write(*,*)(C(k,j),j=1,size(C,2))
      end do   
   end if
   
   a  = [.true.,.false.,.false.,.true.]
   call display(a, msg='Let be the array a')     
   deallocate(L) ; allocate(L(10,9),source=.true.)      
   if (verbos) then
      print '(/,a,g0)','allocated(L):',allocated(L)
      print '(a,i0,1x,i0)','size(L): ',size(L,1),size(L,2)      
   end if      
   call a%GetMat(L)
   if (verbos) then
      print '(/,a)','Then a%GetMat(L) (with L allocated but not of the same shape) gives L ='
      do k = 1, size(L,1)
         write(*,*)(L(k,j),j=1,size(L,2))
      end do   
   end if      
   
   a = pk2_t (matrix = reshape(['s11','s21','s12','s22'],[2,2]), name = 'Smat2')
   call display(a, msg='Let be the array a')  
   deallocate(S) ; allocate(S(10,9))            
   if (verbos) then
      print '(/,a,g0)','allocated(S):',allocated(S)
      print '(a,i0,1x,i0)','size(S): ',size(S,1),size(S,2)      
   end if   
   call a%GetMat(S)
   if (verbos) then
      print '(/,a)','Then a%GetMat(S) (with S allocated but not of the same shape) gives S ='
      do k = 1, size(S,1)
         write(*,*)(S(k,j)%str,' ',j=1,size(S,2))
      end do   
   end if   

!- copy into a polymorphic variable (bk2_t):     

   !allocate(tmp, source = R + CIMAG*R) ; a = tmp ; deallocate(tmp)

   call display(a, msg='Let be the array a')        
   b = sk2_t(n=10_Ikind,m=9_Ikind)   
   if (verbos) then
      print '(/,a,g0)','b%typ:',b%typ
      print '(a,i0,1x,i0)','b%nrow,b%ncol: ',b%nrow,b%ncol      
   end if
   call a%GetMat(b) 
  
   a = b ! use a pk2 var to display the result
   call display(a,'Then a%GetMat(b) (with b allocated but not of the same shape) gives b') 
      
   end subroutine test_GetMat

!=============================================================================================   
   subroutine test_GetMatPacked
!=============================================================================================   
!--------------------------------------------------------------------------------------------- 
!  Copy a pk2 variable into an intrinsic rank-1 variable
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------  
   type   (pk2_t)              :: a
   class  (bk2_t), allocatable :: b
   
   integer(Ikind), allocatable :: I(:)
   real   (Rkind), allocatable :: R(:)
   complex(Rkind), allocatable :: C(:), tmp(:)
   logical       , allocatable :: L(:)
   type   (str_t), allocatable :: S(:)
   integer(Ikind)              :: k, j
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing test_GetMatPacked *******'
      print*
   end if   
!
!- 1) copy into unallocated arrays (I, R, C, L, S, b):
!   
   if (verbos) & 
   print '(//,a,/)','1) Copy into unallocated arrays'
   
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5]) 
   call display(a, msg='Let be the array a')  
   call a%GetMatPacked(I)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(I) (with I unallocated) gives I ='
      do j = 1, size(I) ; write(*,*)I(j) ; end do
   end if
   
   a = reshape([11.,21.,12.,22.,13.,23.,14.,24.,15.,25.],[2,5])   
   call display(a, msg='Let be the array a')     
   call a%GetMatPacked(R)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(R) (with R unallocated) gives R ='
      do j = 1, size(R) ; write(*,*)R(j) ; end do
   end if
   
   a = 2_Ikind*a + CIMAG*a
   call display(a, msg='Let be the array a')        
   call a%GetMatPacked(C)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(C) (with C unallocated) gives C ='
      do j = 1, size(C) ; write(*,*)C(j) ; end do
   end if
   
   a  = reshape([.true.,.false.,.false.,.true.],[2,2])
   call display(a, msg='Let be the array a')        
   call a%GetMatPacked(L)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(L) (with L unallocated) gives L ='
      do j = 1, size(L) ; write(*,*)L(j) ; end do
   end if      
   
   a = pk2_t (matrix = reshape(['s11','s21','s12','s22'],[2,2]), name = 'Smat2')
   call display(a, msg='Let be the array a')           
   call a%GetMatPacked(S)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(S) (with S unallocated) gives S ='
      do j = 1, size(S) ; write(*,*)S(j)%str ; end do
   end if   

!
!- 2) copy into already allocated arrays (I, R, C, L, S, b) which have the right shape:
!
   if (verbos) &
   print '(//,a,/)','2) Copy into already allocated arrays which have the right shape'
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5]) 
   call display(a, msg='Let be the array a')  
   call a%GetMatPacked(I)
   if (verbos) then
      print '(/,a,g0)','allocated(I):',allocated(I)
      print '(a,i0)','size(I): ',size(I)
      print '(/,a)','Then a%GetMatPacked(I) (with I allocated and of same shape) gives I ='
      do j = 1, size(I) ; write(*,*)I(j) ; end do
   end if
   
   a = reshape([11.,21.,12.,22.,13.,23.,14.,24.,15.,25.],[2,5])   
   call display(a, msg='Let be the array a')     
   call a%GetMatPacked(R)
   if (verbos) then
      print '(/,a,g0)','allocated(R):',allocated(R)
      print '(a,i0)','size(R): ',size(R)
      print '(/,a)','Then a%GetMatPacked(R) (with R allocated and of same shape) gives R ='
      do j = 1, size(R) ; write(*,*)R(j) ; end do
   end if
   
   a = 2_Ikind*a + CIMAG*a
   call display(a, msg='Let be the array a')        
   call a%GetMatPacked(C)
   if (verbos) then
      print '(/,a,g0)','allocated(C):',allocated(C)
      print '(a,i0)','size(C): ',size(C)     
      print '(/,a)','Then a%GetMatPacked(C) (with C allocated and of same shape) gives C ='
      do j = 1, size(C) ; write(*,*)C(j) ; end do
   end if
   
   a  = reshape([.true.,.false.,.false.,.true.],[2,2])
   call display(a, msg='Let be the array a')        
   call a%GetMatPacked(L)
   if (verbos) then
      print '(/,a,g0)','allocated(L):',allocated(L)
      print '(a,i0)','size(L): ',size(L)    
      print '(/,a)','Then a%GetMatPacked(L) (with L allocated and of same shape) gives L ='
      do j = 1, size(L) ; write(*,*)L(j) ; end do
   end if      
   
   a = pk2_t (matrix = reshape(['s11','s21','s12','s22'],[2,2]), name = 'Smat2')
   call display(a, msg='Let be the array a')           
   call a%GetMatPacked(S)
   if (verbos) then
      print '(/,a,g0)','allocated(S):',allocated(S)
      print '(a,i0)','size(S): ',size(S)     
      print '(/,a)','Then a%GetMatPacked(S) (with S allocated and of same shape) gives S ='
      do j = 1, size(S) ; write(*,*)S(j)%str ; end do
   end if   
!
!- 3) copy into already allocated arrays (I, R, C, L, S, b) but which have not the right shape:
!
   if (verbos) &
   print '(//,a,/)','3) Copy into already allocated arrays but which have not the right shape'
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5]) 
   call display(a, msg='Let be the array a')  
   deallocate(I) ; allocate(I(1),source=999_Ikind)
   if (verbos) then
      print '(/,a,g0)','allocated(I):',allocated(I)
      print '(a,i0)','size(I): ',size(I)
   end if   
   call a%GetMatPacked(I)
   if (verbos) then   
      print '(/,a)','Then a%GetMatPacked(I) (with I allocated but not of the same shape) gives I ='
      do j = 1, size(I) ; write(*,*)I(j) ; end do
   end if
   
   a = reshape([11.,21.,12.,22.,13.,23.,14.,24.,15.,25.],[2,5])   
   call display(a, msg='Let be the array a')     
   deallocate(R) ; allocate(R(17),source=999.0_Rkind)
   if (verbos) then
      print '(/,a,g0)','allocated(R):',allocated(R)
      print '(a,i0)','size(R): ',size(R)   
   end if   
   call a%GetMatPacked(R)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(R) (with R allocated but not of the same shape) gives R ='
      do j = 1, size(R) ; write(*,*)R(j) ; end do
   end if

   a = 2_Ikind*a + CIMAG*a
   call display(a, msg='Let be the array a')        
   deallocate(C) ; allocate(C(8),source=999.+88*CIMAG)   
   if (verbos) then
      print '(/,a,g0)','allocated(C):',allocated(C)
      print '(a,i0)','size(C): ',size(C)    
   end if
   call a%GetMatPacked(C)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(C) (with C allocated but not of the same shape) gives C ='
      do j = 1, size(C) ; write(*,*)C(j) ; end do
   end if
   
   a  = reshape([.true.,.false.,.false.,.true.],[2,2])
   call display(a, msg='Let be the array a')     
   deallocate(L) ; allocate(L(13),source=.true.)      
   if (verbos) then
      print '(/,a,g0)','allocated(L):',allocated(L)
      print '(a,i0)','size(L): ',size(L)      
   end if      
   call a%GetMatPacked(L)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(L) (with L allocated but not of the same shape) gives L ='
      do j = 1, size(L) ; write(*,*)L(j) ; end do
   end if      
   
   a = pk2_t (matrix = reshape(['s11','s21','s12','s22'],[2,2]), name = 'Smat2')
   call display(a, msg='Let be the array a')  
   deallocate(S) ; allocate(S(3))            
   if (verbos) then
      print '(/,a,g0)','allocated(S):',allocated(S)
      print '(a,i0)','size(S): ',size(S)      
   end if   
   call a%GetMatPacked(S)
   if (verbos) then
      print '(/,a)','Then a%GetMatPacked(S) (with S allocated but not of the same shape) gives S ='
      do j = 1, size(S) ; write(*,*)S(j)%str ; end do
   end if    
   
   end subroutine test_GetMatPacked
   
!=============================================================================================   
   subroutine test_ReadMat
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  Read an array.
!
!  Different ways to enter an array:
!
!  1) By using square brackets and the delimiters ',' and ';' between elements of the same row
!     and between rows, respectively. The read may be done on the stdin or on any text file. 
!     The elements must have compatible types.
!
!     Example 1.1:
!         . example of a file containing the following line:
!           [ 1, 2, 3, 4.1 ; 5, 6, 7, 8 ; 9, -1, 0, 0 ]
!           This array will be interpreted as a 3 x 4 matrix of reals (because of the "4.1")
!
!         . while this one will be interpreted as a 3 x 4 matrix of complexes:
!           [ 1, 2, 3, 4.1 ; (5,1), 6, 7, 8 ; 9, -1, 0, 0 ]
!
!     The symbols '...' may be used as a break line, and comments must be added by using some
!     user symbols (the default is '#').
!
!     Example 1.2:
!         . example of a file containing the following lines ('//' are used for comments):
!
!           // this is my file where my array is as follows:
!            [ 
!              1,  2, 3, 4.1 ; ... // this is the 1st row
!              5,  6, 7, 8   ; ... // this is the 2nd row
!              9, -1, 0, 0     ... // this is the 3rd row
!            ]
!           // end of my file
!
!  2) By reading a file in column format. Each column may have a 
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   integer  (Ikind  ) :: ui, err
   character(len=100) :: msg
   
   type     (pk2_t  ) :: a
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing ReadMat *******'
      print*
   end if   
   
!- 1) Read an array with the '[' ']' brackets constructor (and ',' and ';' as delimiters):

!  on the stdin, e.g.: [ .f. , .t. ; .t. , .f. ] or [1, 2, 3 ; 4, 5.2, 6]
!
   if (verbos) then 
      write(*,'(/,a,/)')'1) USING THE [ ] CONSTRUCTOR'
      write(*,'(a)',advance='no') &
      "Enter an array using the [ ] constructor (do not forget delimiters ',' and ';'): "
      call a%ReadMe ()    
      if (opflag%code > 0) then
         call opflag%display()
      else
         call display(a,'a')
      end if
   end if   
!- on a file - Using a file name:
   if (verbos) print '(/,a,/)',". Read an array from a file (using the file name)"
   call a%ReadMe(fname='files/matx/foo1.txt',comment='//')
   if (opflag%code > 0) then
      call opflag%display()
   else
       a%name = "Array from 'foo1.txt'"
      call display(a,symb=':')
   end if
!- on a file - Using a unit number (the file must be opened before calling the %ReadMe)
   if (verbos) print '(/,a,/)',". Read an array from a file (using a unit number)"
   ui = 1
   open (unit=ui,file='files/matx/foo1.txt',status='old',action='read',iostat=err,iomsg=msg)
   if (err == 0) then  
      call a%ReadMe(unit=ui,comment='//')
      if (opflag%code > 0) then
         call opflag%display()
      else
         a%name = "Array from 'foo1.txt'"
         call display(a,symb=':')
      end if
      close(ui)
   else
      print '(a)',"Error: "//trim(msg)
   end if      
   
!- 2) Read an array without '[' ']' constructor, row after row:

!  on the stdin, e.g:
!       'Emma' 'Peel'  .f. 29
!       'John' 'Steed' .f. 34
!       'Elizabeth' 'Shepherd' .t. 25 
!       'Patrick' 'Macnee' .t. 36 
!
   if (verbos) then 
      write(*,'(/,a,/)')'2) FORMATED DATA (WITHOUT [ ]) CONTAINING VARIOUS TYPES'   
      write(*,'(a)')". Enter, row after row, an array of 4 rows and at least of 2 columns: "
      write(*,'(a)')"  (press 'return' at the end of each line. You can insert blank lines"
      write(*,'(a)')"   between rows and with the symbol '#' you can insert commented line"
      write(*,'(a)')"   or end-of-line comments. In this example only the two first columns "
      write(*,'(a)')"   will define the final array (call a%ReadMe (nrow=4, ncol=2)) so make"
      write(*,'(a)')"   sure that these two columns are of compatible types)"
      call a%ReadMe (nrow=4_Ikind, ncol=2_Ikind)    
      if (opflag%code > 0) then
         call opflag%display()
      else
         call display(a,'call a%ReadMe (nrow=3, ncol=2)',symb=':')
      end if
   end if   
!  on a file - Using a file name :
   if (verbos) print '(/,a,/)',&
   ". Read an array from a file (using the file name) and selecting the first 3 columns:"
   call a%ReadMe(fname='files/matx/foo5.txt',nrow=2_Ikind,ncol=3_Ikind)
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,"call a%ReadMe(fname='files/matx/foo5.txt',nrow=2,ncol=3)",symb=':')
   end if
   
!  on a file - Using a file name. Selecting some columns:
   if (verbos) print '(/,a,/)', &
   ". Read an array from a file (using the file name) and selecting some columns:"
   call a%ReadMe(fname='files/matx/foo5.txt',nrow=2_Ikind,colindx=[3_Ikind,1_Ikind])
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,"call a%ReadMe(fname='files/matx/foo5.txt',nrow=2,colindx=[3,1])",symb=':')
   end if

!  on a file - Using a file name. With an imposed type (complex)
   if (verbos) print '(/,a,/)', &
   ". Read an array from a file (using the file name). With an imposed type (complex):"
   call a%ReadMe(fname='files/matx/foo5.txt',nrow=2_Ikind,ncol=3_Ikind,convert='c')
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,"a%ReadMe(fname='files/matx/foo5.txt',nrow=2,ncol=3,convert='c')",symb=':')
   end if
   
!  on a file - Using a file name. With an imposed type (logical)
   if (verbos) print '(/,a,/)', &
   ". Read an array from a file (using the file name). With an imposed type (logical):"
   call a%ReadMe(fname='files/matx/foo5.txt',nrow=-1_Ikind,colindx=[3_Ikind,4_Ikind],convert='l')
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,"a%ReadMe(fname='files/matx/foo5.txt',nrow=-1,colindx=[3,4],convert='l')",symb=':')
   end if  

!  on a file - Using a file name. With an imposed type (string)
   if (verbos) print '(/,a,/,a,/)', &
   ". Read an array from a file (using the file name). With an imposed type (string)", &
   "  reading until the eof and selecting columns #3 and #4:"
   call a%ReadMe(fname='files/matx/foo5.txt',nrow=-1_Ikind,colindx=[3_Ikind,4_Ikind],convert='s')
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,"a%ReadMe(fname='files/matx/foo5.txt',nrow=-1,colindx=[3,4],convert='s')",symb=':')
   end if  
               
!  on a file - Using a file name. With an imposed type (string)
   if (verbos) print '(/,a,/,a,/)', &
   ". Read an array from a file (using the file name). With an imposed type (string)", &
   "  reading until the eof and selecting all columns:"
   call a%ReadMe(fname='files/matx/foo5.txt',nrow=-1_Ikind,ncol=-1_Ikind,convert='s')
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,"a%ReadMe(fname='files/matx/foo5.txt',nrow=-1,ncol=-1,convert='s')",symb=':')
   end if   
!  on a file - Using a file name. With an imposed type (integer)
   if (verbos) print '(/,a,/)', &
   ". Read an array from a file (using the file name). With an imposed type (integer):"
   call a%ReadMe(fname='files/matx/foo5.txt',nrow=-1_Ikind,colindx=[2_Ikind,6_Ikind],convert='i')
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,"a%ReadMe(fname='files/matx/foo5.txt',nrow=-1,colindx=[2,6],convert='i')",symb=':')
   end if      
!  (we can do the same things with the unit number...)

!  3) Writing:
   
   call a%WriteMe(fname='out',title='copy of col. 2 & 6 of foo5.txt:')
   if (opflag%code > 0) call opflag%display()
   
   call a%WriteMe(fname='out1',&
   title='Written by check_pk2 \n copy of col. 2 & 6 of foo5.txt with fmt=*(i5):',format='(*(i5))')
   if (opflag%code > 0) call opflag%display()
   
   call a%ReadMe(fname='files/matx/foo5.txt',nrow=-1_Ikind,colindx=[7_Ikind,2_Ikind],convert='r')
   call a%WriteMe(fname='out',title='copy of col. 7 & 2 converted in real with a fixed format:', &
                  format='("''",g0,"''",1x,g0)')

   call a%ReadMe(fname='files/matx/foo5.txt',nrow=-1_Ikind,colindx=[7_Ikind,2_Ikind],convert='s')
   call a%WriteMe(fname='out', quote="{}", &
                  title='enclose element of col. 7 & 2 of foo5.txt between { }')
!
!- 4) Read from a string:
!
   call a%ReadMe(fname="[1,2,3;4,5,6]")
   
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,'a%ReadMe(fname="[1,2,3;4,5,6]")',symb=':')
   end if     
   
   call a%ReadMe(fname="['one','two','three';'four','five','six']")
   if (opflag%code > 0) then
      call opflag%display()
   else
      call display(a,&
      'a%ReadMe(string="[''one'',''two'',''three'';''four'',''five'',''six'']")',symb=':')
   end if        
      
   end subroutine test_ReadMat
   

!=============================================================================================   
   subroutine test_ExtracSubMat
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!  Extract a sub-array.
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing ExtracSubMat *******'
      print*
   end if    

   a = reshape([11,12,13,14,15,16,17,18,19],[3,3]) * IONE
   call display(a, msg='Let be the array a')  

   call b%ExtracSubMat (a, [3], [1])
   call display(b, msg='. get the element a(3,1)',symb=':')       

   call b%ExtracSubMat (a, [3])
   call display(b, msg='. get the element a(3)',symb=':')    

   call b%ExtracSubMat (a, [1,3], [2,3])
   call display(b, msg='. get the elements a([1,3],[2,3])',symb=':')   

   call b%ExtracSubMat (a, [-1], [2])
   call display(b, msg='. get the elements of column a(:,2)',symb=':')  
   
   a = reshape(['M11','M21','M12','M22','M13','M23','M14','M24','M15','M25'],[2,5])   
   call display(a,'Let a') 

   call b%ExtracSubMat (a, [1], [3])
   call display(b,'a(1,3)')

   call b%ExtracSubMat (a, [-1], [1,3,5])
   call display(b,'a(:,[1,3,5])')
      
   end subroutine test_ExtracSubMat

!=============================================================================================   
   subroutine test_Alloc1
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!  Allocation or reallocation of a rank-1 array of pk2 arrays.
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type     (pk2_t), allocatable :: a(:)
   integer  (Ikind)              :: n, i
   character(len=9)              :: cnum
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing Alloc1 *******'
      print*
   end if    
   
   if (verbos) print '(/,a)','Allocate a rank 1 array of pk2:'
   n = 4
   call pk2_alloc1 (mode = 'e', t = a, n = n)
   do i = 1, n
      write(cnum,'(i0)')i
      call display(a(i),'a('//trim(cnum)//')')   
   end do
   
   if (verbos) print '(/,a)','Set each pk2:'   
   a(1) = reshape(['M11','M21','M12','M22','M13','M23','M14','M24','M15','M25'],[2,5])
   a(2) = 22_8
   a(3) = [.true.,.false.,.true.]
   a(4) = [1+CIMAG,2+CIMAG,3+CIMAG*2]
   
   do i = 1, n
      write(cnum,'(i0)')i
      call display(a(i),'a('//trim(cnum)//')')   
   end do
   
   ! extend a:
   if (verbos) print '(/,a)','Extend a to an array of 6 elements keeping the previous ones:'
   n = 6
   call pk2_alloc1 (mode = 's', t = a, n = n)
   do i = 1, n
      write(cnum,'(i0)')i
      call display(a(i),'a('//trim(cnum)//')')   
   end do
   
   if (verbos) print '(/,a)','Set a(5) and a(6):'   
   a(5) = [1.1, 2.2, 3.3]
   a(6) = reshape([1.1, 2.2, 3.3],[1,3])
   do i = 1, n
      write(cnum,'(i0)')i
      call display(a(i),'a('//trim(cnum)//')')   
   end do
               
   end subroutine test_Alloc1


!=============================================================================================   
   subroutine test_InsertInto
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!  Insert an array a into another one b at a given location (i.e. at a given row or column)
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing InsertInto *******'
      print*
   end if    
   
   a = reshape(['A11','A21','A12','A22','A13','A23','A14','A24','A15','A25'],[2,5])   
   call display(a,'Let a') 
   b = reshape(['b11','b21','b31','b12','b22','b32','b13','b23','b33','b14','b24','b34'], &
               [3,4])
   call display(b,'and b')
   call a%InsertInto(b, 2, 'r') 
   call display(b,"a%InsertInto(b,2,'r')",":")

   b = reshape(['b11','b21','b31','b12','b22','b32','b13','b23','b33','b14','b24','b34'], &
               [3,4])
   call a%InsertInto(b,2,'c') 
   call display(b,"a%InsertInto(b,2,'c')",":")


   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5])   
   call display(a,'Let a') 
   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call display(b,'and b')
   call a%InsertInto(b,2,'r') 
   call display(b,"a%InsertInto(b,2,'r')",":")

   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call a%InsertInto(b,2,'c') 
   call display(b,"a%InsertInto(b,2,'c')",":")
   
            
   end subroutine test_InsertInto
   
   
!=============================================================================================   
   subroutine test_pk2Insert
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!  Insert a pk2 into another one at a given location (i.e. at a given row or column)
!  or insert an intrinsic array into a pk2
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type   (pk2_t)              :: a, b
   integer(Ikind), allocatable :: Imat(:,:)
   real   (Rkind), allocatable :: Rmat(:,:)
   integer                     :: i, j
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing pk2Insert *******'
      print*
   end if    

   if (verbos) then
      print*,'1) Insert a pk2 into a pk2'
      print*
   end if
!
!- strings:
!      
   a = reshape(['A11','A21','A12','A22','A13','A23','A14','A24','A15','A25'],[2,5])   
   call display(a,'Let a') 
   b = reshape(['b11','b21','b31','b12','b22','b32','b13','b23','b33','b14','b24','b34'], &
               [3,4])
   call display(b,'and b')
   call pk2_Insert(insert = a, into = b, at_row = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_row = 2) gives b")
  
   b = reshape(['b11','b21','b31','b12','b22','b32','b13','b23','b33','b14','b24','b34'], &
               [3,4])
   call pk2_Insert(insert = a, into = b, at_col = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_col = 2) gives b")

!
!- integers:
!
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5])   
   call display(a,'Let a') 
   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call display(b,'and b')
   call pk2_Insert(insert = a, into = b, at_row = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_row = 2) gives b")

   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call pk2_Insert(insert = a, into = b, at_col = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_col = 2) gives b")

!
!- reals:
!
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5])*RONE
   call display(a,'Let a') 
   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])*RONE
   call display(b,'and b')
   call pk2_Insert(insert = a, into = b, at_row = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_row = 2) gives b")

   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])*RONE
   call pk2_Insert(insert = a, into = b, at_col = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_col = 2) gives b")
!
!- integers into reals:
!
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5])
   call display(a,'Let a') 
   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])*RONE
   call display(b,'and b')
   call pk2_Insert(insert = a, into = b, at_row = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_row = 2) gives b")

   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])*RONE
   call pk2_Insert(insert = a, into = b, at_col = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_col = 2) gives b")

!
!- real into integers:
!
   a = reshape([11,21,12,22,13,23,14,24,15,25],[2,5])*RONE
   call display(a,'Let a') 
   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call display(b,'and b')
   call pk2_Insert(insert = a, into = b, at_row = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_row = 2) gives b")

   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call pk2_Insert(insert = a, into = b, at_col = 2_Ikind) 
   call display(b,"pk2_Insert(insert = a, into = b, at_col = 2) gives b")
 
!
!- etc.
! 
   if (verbos) then
      print*,'2) Insert an intrinsic into a pk2'
      print*
   end if
!
!- intrinsic integer into pk2 (integer):
!    
   Imat = reshape([11,21,12,22,13,23,14,24,15,25],[2,5])
   if (verbos) then
      print*,'Let Imat:'
      do i = 1, size(Imat,1)
      print*,(Imat(i,j),j=1,size(Imat,2))
      end do
   end if
   
   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call display(b,'and b') 

   call pk2_Insert(insert = Imat, into = b, at_row = 2_Ikind) 
   call display(b,"pk2_Insert(insert = Imat, into = b, at_row = 2) gives b")

   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call pk2_Insert(insert = Imat, into = b, at_col = 2_Ikind) 
   call display(b,"pk2_Insert(insert = Imat, into = b, at_col = 2) gives b")
!
!- intrinsic integer into pk2 (real):
!    
   Imat = reshape([11,21,12,22,13,23,14,24,15,25],[2,5])
   if (verbos) then
      print*,'Let Imat:'
      do i = 1, size(Imat,1)
      print*,(Imat(i,j),j=1,size(Imat,2))
      end do
   end if
   
   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])*RONE
   call display(b,'and b') 
   call pk2_Insert(insert = Imat, into = b, at_row = 2_Ikind) 
   call display(b,"pk2_Insert(insert = Imat, into = b, at_row = 2) gives b")

   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])*RONE
   call pk2_Insert(insert = Imat, into = b, at_col = 2_Ikind) 
   call display(b,"pk2_Insert(insert = Imat, into = b, at_col = 2) gives b")

!
!- intrinsic real into pk2 (integer):
!    
   Rmat = reshape([11,21,12,22,13,23,14,24,15,25],[2,5])*RONE
   if (verbos) then
      print*,'Let Rmat:'
      do i = 1, size(Rmat,1)
      print*,(Rmat(i,j),j=1,size(Rmat,2))
      end do
   end if
   
   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call display(b,'and b') 
   call pk2_Insert(insert = Rmat, into = b, at_row = 2_Ikind) 
   call display(b,"pk2_Insert(insert = Rmat, into = b, at_row = 2) gives b")

   b = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])
   call pk2_Insert(insert = Rmat, into = b, at_col = 2_Ikind) 
   call display(b,"pk2_Insert(insert = Rmat, into = b, at_col = 2) gives b")

   end subroutine test_pk2Insert
   

!=============================================================================================   
   subroutine test_SetSubmat1
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!  Set given elements of an array b to be equal to an array a: b(indi,indj) = a
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b
   type(str_t) :: smat(2,2)
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing SetSubmat (1/2) *******'
      print*
   end if    
   
   a = reshape([11,13,12,14]*IONE,[2,2])  
   call display(a,'Let a') 
   call display(b,'and b (empty)') 
   
   call b%SetSubmat(a,[1,3]*IONE,[2,4]*IONE)
   call display(b,'. SetSubmat(b,a,[1,3],[2,4])',':') 
   
   call a%Destroy()
   call display(a,'Let a (empty)') 
   call display(b,'and b') 
   
   call b%SetSubmat(a,[1,2]*IONE,[1,2,3,4]*IONE)
   call display(b,'. SetSubmat(b,a,[1,2],[1,2,3,4])',':') 
   

   a = reshape([88_Ikind],[1,1])
   call display(a,'Let a') 
   call display(b,'and b') 
   
   call b%SetSubmat(a,[1,2]*IONE,[1,3,7]*IONE)
   call display(b,'. SetSubmat(b,a,[1,2],[1,3,7])',':') 



   call b%Destroy()
   smat(1,1)%str = 's11' ; smat(1,2)%str ='ss12' 
   smat(2,1)%str = 'sss21' ; smat(2,2)%str = 'ssss22'
   a = smat
   call display(a,'Let a') 
   call display(b,'and b (empty)') 

   call b%SetSubmat(a,[1,3]*IONE,[2,4]*IONE)
   call display(b,'. SetSubmat(b,a,[1,3],[2,4])',':') 

   a = reshape(['...'],[1,1])
   call display(a,'Let a') 
   call display(b,'and b') 

   call b%SetSubmat(a,[1,2,3]*IONE,[1,3]*IONE)
   call display(b,'. SetSubmat(b,a,[1,2,3],[1,3])',':') 
   call b%SetSubmat(a,[2]*IONE,[1,2,3,4]*IONE)
   call display(b,'. SetSubmat(b,a,[2],[1,2,3,4])',':') 
            
   end subroutine test_SetSubmat1


!=============================================================================================   
   subroutine test_SetSubmat2
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!  Add given elements of an array b with those of an array a: b(indi,indj) = b(indi,indj) + a
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b
   type(str_t) :: smat(3,3)
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing SetSubmat (2/2) *******'
      print*
   end if    
   
   a = reshape([1,2,3,4,5,6,7,8,9],[3,3],order = [2,1])
   b = reshape([10,100,1000,10000],[2,2],order = [2,1])   
   call display(a,'Let a') ; call display(b,'and b') 
   
   call a%SetSubmat(b,[2,3],[1,3], add=.true.)
   call display(a,'. SetSubmat(a,b,[2,3],[1,3],add=.true.)',':') 


   a = reshape([1,2,3,4,5,6,7,8,9]*RONE,[3,3],order = [2,1]) 
   call display(a,'Let a') ; call display(b,'and b') 
   call a%SetSubmat(b,[2,3],[1,3], add=.true.)
   call display(a,'. SetSubmat(a,b,[2,3],[1,3],add=.true.)',':') 
      
      
   a = reshape([1,2,3,4,5,6,7,8,9],[3,3],order = [2,1])
   b = b*1.0_Rkind
   call display(a,'Let a') ; call display(b,'and b') 
   call a%SetSubmat(b,[2,3],[1,3], add=.true.)
   call display(a,'. SetSubmat(a,b,[2,3],[1,3],add=.true.)',':') 
      

   a = reshape([1,2,3,4,5,6,7,8,9]+CIMAG,[3,3],order = [2,1])    
   call display(a,'Let a') ; call display(b,'and b') 
   call a%SetSubmat(b,[2,3],[1,3], add=.true.)
   call display(a,'. SetSubmat(a,b,[2,3],[1,3],add=.true.)',':') 
      

   call a%Destroy()
   call b%Destroy()
   
   smat(1,1)%str = 's11' ; smat(1,2)%str = 's12' ; smat(1,3)%str = 's13'
   smat(2,1)%str = 's21' ; smat(2,2)%str = 's22' ; smat(2,3)%str = 's23'
   smat(3,1)%str = 's31' ; smat(3,2)%str = 's32' ; smat(3,3)%str = 's33'
   
   a = smat
   call display(a,'Let a') 
   b = 'x'
   call display(b,'and b') 

   call a%SetSubmat(b,[2,3],[1,3], add=.true.)
   call display(a,'. SetSubmat(a,b,[2,3],[1,3],add=.true.)',':') 
            
   end subroutine test_SetSubmat2


!=============================================================================================   
   subroutine test_delrows
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!  Delete some rows or columns of an array
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing DelRows *******'
      print*
   end if    
   
   b = reshape([1  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ]*IONE,[3,3])
   a = b
   
   call display(a,'Let a') 
   
   call a%DelRows([1_Ikind,3_Ikind],'r')
   call display(a,'. a1 <-- Delrows(a,[1,3],r)') 
   
   a = b
   call a%DelRows([1_Ikind,3_Ikind],'c')
   call display(a,'. a2 <-- Delrows(a,[1,3],c)') 
   
   call a%DelRows([2_Ikind],'c')
   call display(a,'. a3 <-- Delrows(a2,[2],c)')  

   call a%DelRows([1_Ikind],'r')
   call display(a,'. a3 <-- Delrows(a2,[1],r)') 

   call a%DelRows([1_Ikind],'r')
   call display(a,'. a4 <-- Delrows(a3,[1],r)') 

   call a%DelRows([1_Ikind],'r')
   call display(a,'. a5 <-- Delrows(a4,[1],r)') 

   call a%DelRows([1_Ikind],'r')
   call display(a,'. a6 <-- Delrows(a5,[1],r)') 
           
   end subroutine test_delrows


!=============================================================================================   
   subroutine test_eq_neq
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!  Check equality (a == b) or non-equality (a /= b) of two arrays
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t)    :: a, b, c
   type(str_t)    :: smat(2,2)
   complex(Rkind) :: Cmat(3,3)
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing EQ and NE *******'
      print*
   end if    
   
   a = reshape([1  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ],[3,3])
   b = reshape([1.0,1.0,1.0,5.0,5.0,5.0,6.1,6.0,9.0],[3,3])

   call display(a,'Let a') ; call display(b,'and b')
   
   c = a==b ; call display(c,'. a == b')
   c = a/=b ; call display(c,'. a /= b')
   c = b==5.0_Rkind ; call display(c,'. b == 5.0')
   c = 5.0_Rkind==b ; call display(c,'. 5.0 == b')

   a = reshape([1,2,3,4,5,6,7,8,9],[3,3])
   Cmat = reshape([1,2,3,4,5,6,7,8,9]*(CONE+CIMAG),[3,3])   
   Cmat(1,1) = Cmat(1,1)-CIMAG
   Cmat(2,2) = Cmat(2,2)-5*CIMAG
   Cmat(3,3) = Cmat(3,3)-9*CIMAG
   b = Cmat

   call display(a,'Let a') ; call display(b,'and b')
   
   c = a==b ; call display(c,'. a == b')
   c = a/=b ; call display(c,'. a /= b')
   c = b==2*(1+CIMAG) ; call display(c,'. b == 2+2i')
   c = 2*(1+CIMAG)==b ; call display(c,'. 2+2i == b')
   
   smat(1,1)%str = 'gfortran' ; smat(1,2)%str = 'ifort'
   smat(2,1)%str = 'xlf'      ; smat(2,2)%str = 'nagfor'
   a = smat
   smat(1,1)%str = 'g95' ; smat(2,2)%str = 'truc'
   b = smat

   call display(a,'Let a') ; call display(b,'and b')
   
   c = a==b ; call display(c,'. a == b')
   c = a/=b ; call display(c,'. a /= b')
   c = a=='xlf' ; call display(c,". a == 'xlf'")
   c = 'xlf'==a ; call display(c,". 'xlf' == a")
   
   end subroutine test_eq_neq


!=============================================================================================   
   subroutine test_lt_le
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!   
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b, c
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing LT and LE *******'
      print*
   end if    
   
   a = reshape([1  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ]*IONE,[3,3])
   b = reshape([1.0,1.0,1.0,5.0,5.0,5.0,6.1,6.0,6.0]*RONE,[3,3])
   
   c = a < b ; call display(c,'a < b')
   c = a <= b ; call display(c,'a <= b')
   
   c = a < 6_Ikind ; call display(c,'a < 6')
   c = a <= 6_Ikind ; call display(c,'a <= 6')
   
   c = a < 6.1*RONE ; call display(c,'a < 6.1')
   c = a <= 6.1*RONE ; call display(c,'a <= 6.1')
   
   c = b < 6.1*RONE ; call display(c,'b < 6.1')
   c = b <= 6.1*RONE ; call display(c,'b <= 6.1')
   
   end subroutine test_lt_le


!=============================================================================================   
   subroutine test_gt_ge
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!   
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b, c
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing GT and GE *******'
      print*
   end if    
   
   a = reshape([1  ,2  ,3  ,4  ,5  ,6  ,7  ,8  ,9  ]*IONE,[3,3])
   b = reshape([1.0,1.0,1.0,5.0,5.0,5.0,6.1,6.0,6.0]*RONE,[3,3])
   
   c = a > b  ; call display(c,'a > b')
   c = a >= b ; call display(c,'a >= b')
   
   c = a > 6_Ikind ; call display(c,'a > 6')
   c = a >= 6_Ikind ; call display(c,'a >= 6')
   
   c = a > 6.1*RONE ; call display(c,'a > 6.1')
   c = a >= 6.1*RONE ; call display(c,'a >= 6.1')
   
   c = b > 6.1*RONE ; call display(c,'b > 6.1')
   c = b >= 6.1*RONE ;call display(c,'b >= 6.1')
   
   end subroutine test_gt_ge

!=============================================================================================   
   subroutine test_not_and_or
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!   
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b, c
   logical     :: t = .true., f = .false.
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing not, and, or *******'
      print*
   end if    
   
   a = reshape([f,f,t,t,f,f],[2,3]) 
   b = reshape([f,t,t,t,f,f],[2,3]) 
   c = .not. a
   call display(c,'~a')
   c = a .and. b
   call display(c,'a & b')
   c = a .or. b
   call display(c,'a | b')
   c = a .and. t
   call display(c,'a & .true.')
   c = a .or. t
   call display(c,'a | .true.')   
   
   end subroutine test_not_and_or


!=============================================================================================   
   subroutine test_copy
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!   
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b
   integer(Ikind) :: indi(2)=[1,3], indj(2)=[2,3]
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing bk2_CopySubmat?k2 *******'
      print*
   end if    
   
   a = reshape([1,2,3,4,5,6,7,8,9]*IONE,[3,3]) ; a%name = 'Amat'
   call a%m%CopySubmat(b%m,indi,indj)
   b%typ=b%m%typ ; b%nrow = b%m%nrow ; b%ncol = b%m%ncol
   
   call display(b,'b')
   
   end subroutine test_copy
   

!=============================================================================================   
   subroutine test_movealloc
!=============================================================================================   
!---------------------------------------------------------------------------------------------    
!   
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing movealloc *******'
      print*
   end if    

   a = [1,2,3,4,5,6]*IONE ; a%name = 'Amat'
   b = reshape([1,2,3,4]*RONE,[2,2]) ; b%name = 'Bmat'
   call display(a,'a before movealloc') ; call display(b,'b before movealloc')
   call pk2_movealloc(from=a,to=b)
   call display(a,'a after movealloc') ; call display(b,'b after movealloc')

   a = reshape(['s11','s21','s12','s22'],[2,2]) ; 
   call display(a,'a before movealloc') ; call display(b,'b before movealloc')
   call pk2_movealloc(from=a,to=b)
   call display(a,'a after movealloc') ; call display(b,'b after movealloc')

   end subroutine test_movealloc


!=============================================================================================   
   subroutine test_pow
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  check exponentiation 
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: iscal1, iscal2, Imat, res
   type(pk2_t) :: rscal1, rscal2, Rmat
   type(pk2_t) :: cscal1, cscal2, Cmat
!---------------------------------------------------------------------------------------------

   iscal1 = 2*IONE ; iscal2 = 3*IONE 
   rscal1 = (2.3)*RONE ; rscal2 = (3.4)*RONE 
   cscal1 = (2.3)*CONE+CIMAG ; cscal2 = (3.4)*CONE+CIMAG
   Imat = reshape(IONE*[2,1,0,5,6,-1,2,-1,3],[3,3]) 
   Rmat = reshape(RONE*[2,1,0,5,6,-1,2,-1,3],[3,3]) 
   Cmat = reshape(CONE*[2,1,0,5,6,-1,2,-1,3],[3,3]) 
   
   if (verbos) then
      print*
      print*,'****** Testing exponentiation *******'
      print*
   end if    
!
!- iscal1^iscal2, rscal1^rscal2, cscal1^cscal2:
!
   res = iscal1**iscal2 ; call display(res,msg='2 ^ 3')     
   res = rscal1**rscal2 ; call display(res,msg='2.3 ^ 3.4')     
   res = cscal1**cscal2 ; call display(res,msg='(2.3+i) ^ (3.4+i)')     
!
!- iscal1^Imat, iscal1^Rmat, iscal1^Cmat  and  Imat^iscal1:
!
   call display(Imat,msg='Let Imat')
   res = iscal1**Imat ; call display(res,msg='2 ^ Imat')  
   call display(Rmat,msg='Let Rmat')
   res = iscal1**Rmat ; call display(res,msg='2 ^ Rmat')  
   call display(Cmat,msg='Let Cmat')
   res = iscal1**Cmat ; call display(res,msg='2 ^ Cmat')  
   
   res = Imat**iscal1 ; call display(res,msg='Imat ^ 2')    
!
!- rscal1^Imat, rscal1^Rmat, rscal1^Cmat  and  Rmat^iscal1:
!
   call display(Imat,msg='Let Imat')
   res = rscal1**Imat ; call display(res,msg='2.3 ^ Imat')  
   call display(Rmat,msg='Let Rmat')
   res = rscal1**Rmat ; call display(res,msg='2.3 ^ Rmat')  
   call display(Cmat,msg='Let Cmat')
   res = rscal1**Cmat ; call display(res,msg='2.3 ^ Cmat')  
   
   res = Rmat**iscal1 ; call display(res,msg='Rmat ^ 2')    
!
!- cscal1^Imat, cscal1^Rmat, cscal1^Cmat  and  Cmat^iscal1:
!
   call display(Imat,msg='Let Imat')
   res = cscal1**Imat ; call display(res,msg='(2.3+i) ^ Imat')  
   call display(Rmat,msg='Let Rmat')
   res = cscal1**Rmat ; call display(res,msg='(2.3+i) ^ Rmat')  
   call display(Cmat,msg='Let Cmat')
   res = cscal1**Cmat ; call display(res,msg='(2.3+i) ^ Cmat')  
   
   res = Cmat**iscal1 ; call display(res,msg='Cmat ^ 2')    

   end subroutine test_pow
   
   
!=============================================================================================   
   subroutine test_div
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  check divide 
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t) :: a, b, c
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing divide *******'
      print*
   end if    
!
!- Int / Int:
!
   a = 2*IONE ; b = 3*IONE ;
   c = a/b ; call display(c,msg='2 / 3')     
   c = b/a ; call display(c,msg='3 / 2')     
   b = reshape(IONE*[2,1,0,5,6,-1,2,-1,3],[3,3])
   call display(b,msg='Let b')
   c = a/b ; call display(c,msg='2 / b')  
   c = b/a ; call display(c,msg='b / 2')  
!
!- Int / Real and Real / Int:
!
   a = 2*IONE ; b = 3*RONE
   c = a/b ; call display(c,msg='2 / 3.0')     
   c = b/a ; call display(c,msg='3.0 / 2')     
   b = reshape(RONE*[2,1,0,5,6,-1,2,-1,3],[3,3])
   call display(b,msg='Let b')
   c = a/b ; call display(c,msg='2 / b')  
   c = b/a ; call display(c,msg='b / 2')     
   a = reshape([2,1,0,5,6,-1,2,-1,3]*IONE,[3,3]); b = 2*RONE
   call display(a,msg='Let a')
   c = b/a ; call display(c,msg='2.0/a') 

!
!- Int / Cmplx and Cmplx / Int:
!
   a = 2*IONE ; b = 3+CIMAG
   c = a/b ; call display(c,msg='2 / (3+i)')     
   c = b/a ; call display(c,msg='(3+i) / 2')     
   b = reshape((CONE+CIMAG)*[2,1,0,5,6,-1,2,-1,3],[3,3])
   call display(b,msg='Let b')
   c = a/b ; call display(c,msg='2 / b')  
   c = b/a ; call display(c,msg='b / 2') 
   a = reshape([2,1,0,5,6,-1,2,-1,3]*IONE,[3,3]); b = 3+CIMAG
   call display(a,msg='Let a')
   c = b/a ; call display(c,msg='(3+i)/a') 
!
!- Real / Real:
!
   a = 2*RONE ; b = 3*RONE
   c = a/b ; call display(c,msg='2.0 / 3.0')     
   c = b/a ; call display(c,msg='3.0 / 2.0')     
   b = reshape(RONE*[2,1,0,5,6,-1,2,-1,3],[3,3])
   call display(b,msg='Let b')
   c = a/b ; call display(c,msg='2.0 / b')  
   c = b/a ; call display(c,msg='b / 2.0')   
!
!- Real / Cmplx and Cmplx / Real:
!
   a = 2*RONE ; b = 3+CIMAG
   c = a/b ; call display(c,msg='2.0 / (3+i)')     
   c = b/a ; call display(c,msg='(3+i) / 2.0')     
   b = reshape((CONE+CIMAG)*[2,1,0,5,6,-1,2,-1,3],[3,3])
   call display(b,msg='Let b')
   c = a/b ; call display(c,msg='2.0 / b')  
   c = b/a ; call display(c,msg='b / 2.0') 
   a = reshape([2,1,0,5,6,-1,2,-1,3]*RONE,[3,3]); b = 3+CIMAG
   call display(a,msg='Let a')
   c = b/a ; call display(c,msg='(3+i)/a')   
       
   end subroutine test_div
   
   
!=============================================================================================   
   subroutine test_ediv
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  check element-wise divide 
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t)    :: a, b, c
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing element-wise divide *******'
      print*
   end if    
!
!- Int ./ Int:
!
   a = IONE ; b = 2*IONE
   c = a .d. b ; call display(c,msg='1 ./ 2')     
   c = b .d. a ; call display(c,msg='2 ./ 1')     
   b = reshape(IONE*[1,2,3,4,5,6],[3,2])
   c = a .d. b ; call display(c,msg='1 ./ b')  
   c = b .d. a ; call display(c,msg='b ./ 1')  
   a = reshape(IONE*[11,12,13,14,15,16],[3,2])
   c = a .d. b ; call display(c,msg='a ./ b')  
   c = b .d. a ; call display(c,msg='b ./ a')  
!
!- Int ./ Real, Real ./ Int:
!
   a = IONE ; b = 2*RONE
   c = a .d. b ; call display(c,msg='1 ./ 2.0')     
   c = b .d. a ; call display(c,msg='2.0 ./ 1')     
   b = reshape(RONE*[1,2,3,4,5,6],[3,2])
   c = a .d. b ; call display(c,msg='1 ./ b')  
   c = b .d. a ; call display(c,msg='b ./ 1')  
   a = reshape(IONE*[11,12,13,14,15,16],[3,2])
   c = a .d. b ; call display(c,msg='a ./ b')  
   c = b .d. a ; call display(c,msg='b ./ a')  
!
!- Int ./ Cmplx, Cmplx ./ Int:
!
   a = IONE ; b = 2*(CONE+CIMAG)
   c = a .d. b ; call display(c,msg='1 ./ (2+2i)')     
   c = b .d. a ; call display(c,msg='(2+2i) ./ 1')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a .d. b ; call display(c,msg='1 ./ b')  
   c = b .d. a ; call display(c,msg='b ./ 1')  
   a = reshape(IONE*[11,12,13,14,15,16],[3,2])
   c = a .d. b ; call display(c,msg='a ./ b')  
   c = b .d. a ; call display(c,msg='b ./ a')  
!
!- Real ./ Cmplx, Cmplx ./ Real:
!
   a = RONE ; b = 2*(CONE+CIMAG)
   c = a .d. b ; call display(c,msg='1.0 ./ (2+2i)')     
   c = b .d. a ; call display(c,msg='(2+2i) ./ 1.0')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a .d. b ; call display(c,msg='1.0 ./ b')  
   c = b .d. a ; call display(c,msg='b ./ 1.0')  
   a = reshape(RONE*[11,12,13,14,15,16],[3,2])
   c = a .d. b ; call display(c,msg='a ./ b')  
   c = b .d. a ; call display(c,msg='b ./ a')   
!
!- Cmplx ./ Cmplx:
!
   a = (3-CIMAG) ; b = 2*(CONE+CIMAG)
   c = a .d. b ; call display(c,msg='(3-i) ./ (2+2i)')     
   c = b .d. a ; call display(c,msg='(2+2i) ./ (3-i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a .d. b ; call display(c,msg='(3-i) ./ b')  
   c = b .d. a ; call display(c,msg='b ./ (3-i)')  
   a = reshape((3-CIMAG)*[11,12,13,14,15,16],[3,2])
   c = a .d. b ; call display(c,msg='a ./ b')  
   c = b .d. a ; call display(c,msg='b ./ a')         
     
   end subroutine test_ediv
   
   
!=============================================================================================   
   subroutine test_emult
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  check element-wise multiplication 
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t)    :: a, b, c
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing element-wise multiplication *******'
      print*
   end if    
!
!- Int .* Int:
!
   a = IONE ; b = 2*IONE
   c = a .m. b ; call display(c,msg='1 .* 2')     
   b = reshape(IONE*[1,2,3,4,5,6],[3,2])
   c = a .m. b ; call display(c,msg='1. * b')  
   c = b .m. a ; call display(c,msg='b. * 1')  
   a = reshape(IONE*[11,12,13,14,15,16],[3,2])
   c = a .m. b ; call display(c,msg='a .* b')  
   c = b .m. a ; call display(c,msg='b .* a')  
!
!- Int .* Real, Real .* In.:
!
   a = IONE ; b = 2*RONE
   c = a .m. b ; call display(c,msg='1 .* 2.0')     
   c = b .m. a ; call display(c,msg='2.0 .* 1')     
   b = reshape(RONE*[1,2,3,4,5,6],[3,2])
   c = a .m. b ; call display(c,msg='1 .* b')  
   c = b .m. a ; call display(c,msg='b .* 1')  
   a = reshape(IONE*[11,12,13,14,15,16],[3,2])
   c = a .m. b ; call display(c,msg='a .* b')  
   c = b .m. a ; call display(c,msg='b .* a')  
!
!- Int .* Cmplx, Cmplx .* Int:
!
   a = IONE ; b = 2*(CONE+CIMAG)
   c = a .m. b ; call display(c,msg='1 .* (2+2i)')     
   c = b .m. a ; call display(c,msg='(2+2i) .* 1')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a .m. b ; call display(c,msg='1 .* b')  
   c = b .m. a ; call display(c,msg='b .* 1')  
   a = reshape(IONE*[11,12,13,14,15,16],[3,2])
   c = a .m. b ; call display(c,msg='a .* b')  
   c = b .m. a ; call display(c,msg='b .* a')    
!
!- Real .* Cmplx, Cmplx .* Real:
!
   a = RONE ; b = 2*(CONE+CIMAG)
   c = a .m. b ; call display(c,msg='1.0 .* (2+2i)')     
   c = b .m. a ; call display(c,msg='(2+2i) .* 1.0')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a .m. b ; call display(c,msg='1.0 .* b')  
   c = b .m. a ; call display(c,msg='b .* 1.0')  
   a = reshape(RONE*[11,12,13,14,15,16],[3,2])
   c = a .m. b ; call display(c,msg='a .* b')  
   c = b .m. a ; call display(c,msg='b .* a') 
!
!- Cmplx .* Cmplx:
!
   a = (CONE+CIMAG) ; b = 2*(CONE+CIMAG)
   c = a .m. b ; call display(c,msg='(1+i) .* (2+2i)')     
   c = b .m. a ; call display(c,msg='(2+2i) .* (1+i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a .m. b ; call display(c,msg='(1+i) .* b')  
   c = b .m. a ; call display(c,msg='b .* (1+i)')  
   a = reshape((CONE+CIMAG)*[11,12,13,14,15,16],[3,2])
   c = a .m. b ; call display(c,msg='a .* b')  
   c = b .m. a ; call display(c,msg='b .* a') 
            
   end subroutine test_emult
   
!=============================================================================================   
   subroutine test_mult
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  check array multiplication (matrix product)
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t)    :: a, b, c
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing multiplication *******'
      print*
   end if    
!
!- Int. * Int.:
!
   a = IONE ; b = 2*IONE
   c = a * b ; call display(c,msg='1*2')     
   b = reshape(IONE*[1,2,3,4,5,6],[3,2])
   c = a * b ; call display(c,msg='1*b')  
   c = b * a ; call display(c,msg='b*1')  
   a = reshape(IONE*[1,2,3,4,5,6],[2,3])
   c = a * b ; call display(c,msg='a*b')  
   c = b * a ; call display(c,msg='b*a')  
!
!- Int. * Real, Real * Int.:
!
   a = IONE ; b = 2*RONE
   c = a * b ; call display(c,msg='1*2.')     
   c = b * a ; call display(c,msg='2.*1')     
   b = reshape(RONE*[1,2,3,4,5,6],[3,2])
   c = a * b ; call display(c,msg='1*b')  
   c = b * a ; call display(c,msg='b*1')  
   a = reshape(IONE*[1,2,3,4,5,6],[2,3])
   c = a * b ; call display(c,msg='a*b')  
   c = b * a ; call display(c,msg='b*a')  
!
!- Int. * Cmplx, Cmplx * Int.:
!
   a = IONE ; b = 2*(CONE+CIMAG)
   c = a * b ; call display(c,msg='1*(2+2i)')     
   c = b * a ; call display(c,msg='(2+2i)*1')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a * b ; call display(c,msg='1*b')  
   c = b * a ; call display(c,msg='b*1')  
   a = reshape(IONE*[1,2,3,4,5,6],[2,3])
   c = a * b ; call display(c,msg='a*b')  
   c = b * a ; call display(c,msg='b*a')   
!
!- Real * Real:
!
   a = RONE ; b = 2*RONE
   c = a * b ; call display(c,msg='1.*2.')     
   c = b * a ; call display(c,msg='2.*1.')     
   b = reshape(RONE*[1,2,3,4,5,6],[3,2])
   c = a * b ; call display(c,msg='1.*b')  
   c = b * a ; call display(c,msg='b*1.')  
   a = reshape(RONE*[1,2,3,4,5,6],[2,3])
   c = a * b ; call display(c,msg='a*b')  
   c = b * a ; call display(c,msg='b*a')  
!
!- Real * Cmplx, Cmplx * Real:
!
   a = RONE ; b = 2*(CONE+CIMAG)
   c = a * b ; call display(c,msg='1.*(2+2i)')     
   c = b * a ; call display(c,msg='(2+2i)*1.')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a * b ; call display(c,msg='1.*b')  
   c = b * a ; call display(c,msg='b*1.')  
   a = reshape(RONE*[1,2,3,4,5,6],[2,3])
   c = a * b ; call display(c,msg='a*b')  
   c = b * a ; call display(c,msg='b*a')   
!
!- Cmplx * Cmplx:
!
   a = CONE+CIMAG ; b = 2*(CONE+CIMAG)
   c = a * b ; call display(c,msg='(1+i)*(2+2i)')     
   c = b * a ; call display(c,msg='(2+2i)*(1+i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[3,2])
   c = a * b ; call display(c,msg='(1+i)*b')  
   c = b * a ; call display(c,msg='b*(1+i)')  
   a = reshape((CONE+CIMAG)*[1,2,3,4,5,6],[2,3])
   c = a * b ; call display(c,msg='a*b')  
   c = b * a ; call display(c,msg='b*a')     
      
   end subroutine test_mult

!=============================================================================================   
   subroutine test_minus_sub
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  check addition
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t)    :: a, b, c
!---------------------------------------------------------------------------------------------

   if (verbos) then
      print*
      print*,'****** Testing subtraction *******'
      print*
   end if    
   
   a = reshape(RONE*[1,2,3,4,5,6],[2,3])
   c = -a ; call display(c, msg='-a')

   call pk2_subMinus ( a, c ) ; call display(c, msg='pk2_subMinus ( a, c ) (w/ c right type/size)')
   c = reshape([1,2,3,4,5,6,7,8,9],[3,3])
   call pk2_subMinus ( a, c ) ; call display(c, msg='pk2_subMinus ( a, c ) (w/ c wrong type/size)')
   c = reshape(RONE*[1,2,3,4,5,6,7,8,9],[3,3])
   call pk2_subMinus ( a, c ) ; call display(c, msg='pk2_subMinus ( a, c ) (w/ c wrong size)')

!
!- Int. - Int.:
!
   a = IONE ; b = 2*IONE
   c = a - b ; call display(c,msg='1-2')     
   b = reshape(IONE*[1,2,3,4],[2,2])
   c = a - b ; call display(c,msg='1-b')  
   c = b - a ; call display(c,msg='b-1')  
   a = reshape(IONE*[11,12,13,14],[2,2])
   c = a - b ; call display(c,msg='a-b')  
   c = b - a ; call display(c,msg='b-a')  
!
!- Int. - Real:
!
   a = IONE ; b = 2*RONE
   c = a - b ; call display(c,msg='1-2.')     
   b = reshape(RONE*[1,2,3,4],[2,2])
   c = a - b ; call display(c,msg='1-b')  
   c = b - a ; call display(c,msg='b-1')  
   a = reshape(IONE*[11,12,13,14],[2,2])
   c = a - b ; call display(c,msg='a-b')  
   c = b - a ; call display(c,msg='b-a')
!
!- Int. - CMPLX:
!
   a = IONE ; b = 2*(CONE+CIMAG)
   c = a - b ; call display(c,msg='1-(2+2i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4],[2,2])
   c = a - b ; call display(c,msg='1-b')  
   c = b - a ; call display(c,msg='b-1')  
   a = reshape(IONE*[11,12,13,14],[2,2])
   c = a - b ; call display(c,msg='a-b')  
   c = b - a ; call display(c,msg='b-a')  
!
!- Real - Real:
!
   a = RONE ; b = 2*RONE
   c = a - b ; call display(c,msg='1.-2.')     
   b = reshape(RONE*[1,2,3,4],[2,2])
   c = a - b ; call display(c,msg='1.-b')  
   c = b - a ; call display(c,msg='b-1.')  
   a = reshape(RONE*[11,12,13,14],[2,2])
   c = a - b ; call display(c,msg='a-b')  
   c = b - a ; call display(c,msg='b-a')      
   
!
!- Real + Cmplx:
!
   a = RONE ; b = 2*(CONE+CIMAG)
   c = a - b ; call display(c,msg='1.-(2+2i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4],[2,2])
   c = a - b ; call display(c,msg='1.-b')  
   c = b - a ; call display(c,msg='b-1.')  
   a = reshape(RONE*[11,12,13,14],[2,2])
   c = a - b ; call display(c,msg='a-b')  
   c = b - a ; call display(c,msg='b-a')         
!
!- Cmplx + Cmplx:
!
   a = CONE+CIMAG ; b = 2*(CONE+CIMAG)
   c = a - b ; call display(c,msg='(1+i)-(2+2i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4],[2,2])
   c = a - b ; call display(c,msg='(1+i)-b')  
   c = b - a ; call display(c,msg='b-(1+i)')  
   a = reshape((CONE+CIMAG)*[11,12,13,14],[2,2])
   c = a - b ; call display(c,msg='a-b')  
   c = b - a ; call display(c,msg='b-a')      
      
   end subroutine test_minus_sub

!=============================================================================================   
   subroutine test_add
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  check addition
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t)    :: a, b, c
   type(str_t)    :: s(2,2)
!---------------------------------------------------------------------------------------------

    s(1,1)%str = '|september|' ; s(1,2)%str = '|october|'
    s(2,1)%str = '|november|'  ; s(2,2)%str = '|december|'
   
   if (verbos) then
      print*
      print*,'****** Testing addition *******'
      print*
   end if    
!
!- Int. + Int.:
!
!  with 2 pk2 var.   
   a = IONE ; b = 2*IONE
   c = a + b ; call display(c,msg='1+2')     
   b = reshape(IONE*[1,2,3,4],[2,2])
   c = a + b ; call display(c,msg='a+b')  
   c = b + a ; call display(c,msg='b+1')  
   a = reshape(IONE*[11,12,13,14],[2,2])
   c = a + b ; call display(c,msg='a+b')  
   c = b + a ; call display(c,msg='b+a')  
!  with an int. and 1 pk2 var.
   c = IONE+b ; call display(c,msg='1+b (int. + b)')  
   c = b+IONE ; call display(c,msg='b+1 (b + int.)')  

!
!- Int. + Real:
!
   a = IONE ; b = 2*RONE
   c = a + b ; call display(c,msg='1+2.')     
   b = reshape(RONE*[1,2,3,4],[2,2])
   c = a + b ; call display(c,msg='1+b')  
   c = b + a ; call display(c,msg='b+1')  
   a = reshape(IONE*[11,12,13,14],[2,2])
   c = a + b ; call display(c,msg='a+b')  
   c = b + a ; call display(c,msg='b+a')
!  with an int. or a real +  1 pk2 var.   
   c = RONE+a ; call display(c,msg='1.0+a (real + a)')  
   c = IONE+b ; call display(c,msg='1 + b (int. + b)')     
!
!- Int. + CMPLX:
!
   a = IONE ; b = 2*(CONE+CIMAG)
   c = a + b ; call display(c,msg='1+(2+2i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4],[2,2])
   c = a + b ; call display(c,msg='1+b')  
   c = b + a ; call display(c,msg='b+1')  
   a = reshape(IONE*[11,12,13,14],[2,2])
   c = a + b ; call display(c,msg='a+b')  
   c = b + a ; call display(c,msg='b+a')  
!
!- Real + Real:
!
   a = RONE ; b = 2*RONE
   c = a + b ; call display(c,msg='1.+2.')     
   b = reshape(RONE*[1,2,3,4],[2,2])
   c = a + b ; call display(c,msg='1.+b')  
   c = b + a ; call display(c,msg='b+1.')  
   a = reshape(RONE*[11,12,13,14],[2,2])
   c = a + b ; call display(c,msg='a+b')  
   c = b + a ; call display(c,msg='b+a')
!
!- Real + Cmplx:
!
   a = RONE ; b = 2*(CONE+CIMAG)
   c = a + b ; call display(c,msg='1.+(2+2i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4],[2,2])
   c = a + b ; call display(c,msg='1.+b')  
   c = b + a ; call display(c,msg='b+1.')  
   a = reshape(RONE*[11,12,13,14],[2,2])
   c = a + b ; call display(c,msg='a+b')  
   c = b + a ; call display(c,msg='b+a')         
!
!- Cmplx + Cmplx:
!
   a = CONE+CIMAG ; b = 2*(CONE+CIMAG)
   c = a + b ; call display(c,msg='(1+i)+(2+2i)')     
   b = reshape((CONE+CIMAG)*[1,2,3,4],[2,2])
   c = a + b ; call display(c,msg='(1+i)+b')  
   c = b + a ; call display(c,msg='b+(1+i)')  
   a = reshape((CONE+CIMAG)*[11,12,13,14],[2,2])
   c = a + b ; call display(c,msg='a+b')  
   c = b + a ; call display(c,msg='b+a')        
!
!- Str + Str:
!

   a = '|2018|' ; b = '|december|' 
   c = a + b ; call display(c,msg='"|2018|"+"|december|"')   
   b = s 
   c = a + b ; call display(c,msg='"|2018|"+b')  
   c = b + a ; call display(c,msg='b+"|2018|"')  
   a = reshape(['|09|','|11|','|10|','|12|'],[2,2])
   c = a + b ; call display(c,msg='a+b')  
   c = b + a ; call display(c,msg='b+a')   
           
   call display(b+' hello',msg='b+char(=" hello")')
   
   end subroutine test_add
      
!=============================================================================================   
   subroutine test_assign
!=============================================================================================   

!---------------------------------------------------------------------------------------------    
!  check the different ways to set a pk2 variable
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------  
   type(pk2_t)    :: a
   logical        :: T = .true., F = .false.
   type(str_t)    :: s(2,2), c
   integer(Ikind) :: I
!---------------------------------------------------------------------------------------------

   c%str = 'hello'
   s(1,1)%str = 'a' ; s(1,2)%str = 'ab' ; s(2,1)%str = 'abc' ; s(2,2)%str = 'abcd'
   
   if (verbos) then
      print*
      print*,'****** Testing assignments *******'
      print*
   end if    
!
!- 1) Using the constructor:
!
   if (verbos) print '(/,a,/,a,/)','Using the constructor pk2_t:','==========================='
              
   
   ! an integer scalar (1x1):
   a = pk2_t(IONE, name = 'n') ! or pk2_t(scalar=IONE,name='n')
   call display(a)   
   ! an integer vector (3x1) (required: matrix, optional: name):
   a = pk2_t (matrix = reshape([1,2,3]*IONE,[3,1]), name = 'Ivec')
   call display(a)     
   ! an 2x2 integer array:
   a = pk2_t (matrix = reshape([1,2,3,4]*IONE,[2,2]), name = 'Imat')
   call display(a)   
   ! a real scalar (1x1):
   a = pk2_t (RONE, name = 'r') ! or pk2_t(scalar=RONE,name='r')
   call display(a)      
   ! a 2x2 real array:
   a = pk2_t (matrix = reshape([1,2,3,4]*RONE,[2,2]), name = 'Rmat')
   call display(a)

   ! a complex scalar (1x1):  
   a = pk2_t (CONE+CIMAG, name = 'c')   
   call display(a)      
   ! a 2x2 complex array:
   a = pk2_t (matrix = reshape([1,2,3,4]*(CONE+CIMAG),[2,2]), name = 'Cmat')
   call display(a)

   ! a boolean scalar (1x1):      
   a = pk2_t (.true., name = 'b')   
   call display(a)      
   ! a 2x2 boolean array:
   a = pk2_t (matrix = reshape([t,f,f,t],[2,2]), name = 'Lmat')
   call display(a)
   ! a string scalar (1x1) (initialized with a str_t variable): 
   a = pk2_t (c, name = 's1')   
   call display(a)       
   ! a 2x2 string array (initialized with a str_t variable):
   a = pk2_t (matrix = s, name = 'Smat1')
   call display(a)

   ! a string scalar (1x1) (initialized with a character variable):      
   a = pk2_t ('hello', name = 's2')   
   call display(a)        
   ! a 2x2 string array (initialized with a character array):
   a = pk2_t (matrix = reshape(['s11','s21','s12','s22'],[2,2]), name = 'Smat2')
   call display(a)

   ! an n x m null real array (required: n,m,typ, optional: name)
   a = pk2_t(name = 'Zmat', shape = [5,3], typ = RTYP)
   call display(a)
   
   ! an empty array
   a = pk2_t()
   call display(a,msg='(empty)')   
   
!
!- 2) Using assignment overload:
!

   if (verbos) print '(//,a,/,a,/)','Using assignment:','================'
                                     
   ! an integer scalar (1x1):
   a = IONE ; a%name = 'n2'  
   call display(a)    
   ! an integer vector (3x1):
   a = [1,2,3] ; a%name = 'Ivec2'
   call display(a)      
   ! an 2x2 integer array:   
   a = reshape([1,2,3,4]*IONE,[2,2]) ; a%name = 'Imat2'  
   call display(a)
   ! a real scalar (1x1):   
   a = RONE ; a%name = 'r2'  
   call display(a)     
   ! a 2x2 real array:
   a = reshape([1,2,3,4]*RONE,[2,2]) ; a%name = 'Rmat2'
   call display(a)
   ! a complex scalar (1x1):   
   a = CONE+CIMAG ; a%name = 'c2'  
   call display(a)      
   ! a 2x2 complex array:
   a = reshape([1,2,3,4]*(CONE+CIMAG),[2,2]) ; a%name = 'Cmat2'
   call display(a)
   ! a boolean scalar (1x1):   
   a = t ; a%name = 'b2'  
   call display(a)      
   ! a 2x2 boolean array:
   a = reshape([t,f,f,t],[2,2]) ; a%name = 'Lmat2'
   call display(a)
   ! a string scalar (1x1) (initialized with a str_t variable):
   a = c ; a%name = 's3'  
   call display(a)      
   ! a 2x2 string array (initialized with a str_t variable):
   a = s ; a%name = 'Smat3'
   call display(a)
   ! a string scalar (1x1) (initialized with a character variable):
   a = 'bonjour' ; a%name = 's4'  
   call display(a)     
   ! a 2x2 string array (initialized with a character array):
   a = reshape(['s11','s21','s12','s22'],[2,2]) ; a%name = 'Smat4'
   call display(a)
  ! an n x m null real matrix
   a = reshape([(0.0_Rkind,i=1,15)],[5,3]); a%name = 'Zmat2' 
   call display(a)

   end subroutine test_assign

!=============================================================================================   
   subroutine display ( a , msg, symb )
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
         call str_print ( s=p%v, unit=int(STDOUT), lhsMsg = str // csymb )
!          call p%GetMat(Ch=res)
!          fmt = 'a0'
!          if (all(len_trim(res(1,:)) == 0)) then
!             nfmt = max(1,maxval(len_trim(res)))
!             print*,'nfmt=',nfmt
!             write(fmt,'(a,i0)')'a',nfmt 
!          end if     
!          call disp(str // csymb, res, fmt = fmt)
   end select
      
   end subroutine display


end program Check_pk2
