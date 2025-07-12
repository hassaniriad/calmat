! For compiling (or use the makefile):

! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran Check_Util_Other.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_Util_Other

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort Check_Util_Other.f90 -L../../lib/ifort -lpk2 -llapack -lblas -o check_Util_Other

! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor Check_Util_Other.f90  -L../../lib/nagfor -lpk2 -llapack -lblas -o check_Util_Other

program Check_Util_Other
   use pk2mod_m
   
   implicit none
   
!---------------------------------------------------------------------------      
   type(err_t) :: stat ! for error/warning report      
!--------------------------------------------------------------------------- 

   call signalHandler_SignalCatch (unit = STDOUT, title = '--> Check_Util_Other:')
!   
!- disable automatic stop when an error occurs:
!
   call err_SetHaltingMode ( halting = .false. )
   
   call test_util_UpdateSk1

   call test_util_UpdateIk1

   call test_util_UpdateRk1

   call test_util_UpdateCk1

   call test_util_UpdateLk1

   call test_util_allocIk1

   call test_util_allocIk2

   call test_util_allocRk1

   call test_util_allocRk2

   call test_util_allocCk1

   call test_util_allocCk2

   call test_util_allocLk1

   call test_util_allocLk2

   call test_util_allocSk1

   call test_util_allocSk2
   
   call test_util_FindPosInList

   call test_util_TimeStamp

   call test_util_GetUnit
   
   call test_util_Magic
   
   call test_util_IsRealCk0

   call test_util_IsRealCk1

   call test_util_IsRealCk2
   
   call test_util_IsIntgRk0

   call test_util_IsIntgRk1

   call test_util_IsIntgRk2

   call test_util_SortIk1

   call test_util_SortRk1

   call test_util_SortCk1

   call test_util_SortSk1
   
   call test_util_SolvLin
   
   call stat%Destroy()
   
contains

!=============================================================================================
   SUBROUTINE test_util_SolvLin
!=============================================================================================

   complex(Rkind), allocatable :: Ac(:,:), Bc(:,:), Xc(:,:), AcSave(:,:), BcSave(:,:)
   real   (Rkind), allocatable :: Ar(:,:), Br(:,:), Xr(:,:), ArSave(:,:), BrSave(:,:),  &
                                  vr(:,:), vi(:,:)
   real   (Rkind)              :: rcond = 1e-8, res
   integer                     :: i, j, n = 5, m = 4, nrhs = 7
   
   
   allocate(vr(n,m),vi(n,m))
   call random_number(harvest=vr) ; call random_number(harvest=vi)
   Ac = vr*CONE + vi*CIMAG
   Ar = vr
   
   deallocate(vr,vi) ; allocate(vr(m,nrhs),vi(m,nrhs))
   call random_number(harvest=vr) ; call random_number(harvest=vi)
   Xc = vr*CONE + vi*CIMAG
   Xr = vr
   
   Bc = matmul(Ac,Xc) ; Br = matmul(Ar,Xr)
   
   AcSave = Ac ; BcSave = Bc
   ArSave = Ar ; BrSave = Br
                  
   print*
   print*,'************* Begin test_util_SolvLin *************'
   print*
   
   deallocate(Xr,Xc)
   
   print*,'1.1) for a real system (matrix and rhs modified):'
   call util_SolvLin ( Ar, Br, rcond, stat )
   call stat%display() 

   res = RZERO
   Xr = matmul(ArSave,Br)-BrSave
   do j = 1, nrhs
      do i = 1, n
         res = res + (Xr(i,j))**2
      end do
   end do
   print*,'   || A*X - B || =', sqrt(res)

   print*,'1.2) for a real system (matrix and rhs unchanged):'
   call util_SolvLin ( ArSave, BrSave, Xr, rcond, stat )
   call stat%display()

   res = RZERO
   Xr = matmul(ArSave,Xr)-BrSave
   
   do j = 1, nrhs
      do i = 1, n
         res = res + (Xr(i,j))**2
      end do
   end do
   print*,'   || A*X - B || =', sqrt(res)      
   
   
   print*,'2.1) for a complex system (matrix and rhs modified):'
   call util_SolvLin ( Ac, Bc, rcond, stat )
   call stat%display() 

   res = RZERO
   Xc = matmul(AcSave,Bc)-BcSave
   
   do j = 1, nrhs
      do i = 1, n
         res = res + abs(Xc(i,j))**2
      end do
   end do
   print*,'   || A*X - B || =', sqrt(res)

   print*,'2.2) for a complex system (matrix and rhs unchanged):'
   call util_SolvLin ( AcSave, BcSave, Xc, rcond, stat )
   call stat%display() 

   res = RZERO
   Xc = matmul(AcSave,Xc)-BcSave
   
   do j = 1, nrhs
      do i = 1, n
         res = res + abs(Xc(i,j))**2
      end do
   end do
   print*,'   || A*X - B || =', sqrt(res)      

   print*
   print*,'-------------  End test_util_SolvLin  -------------'  
   print*
         
   END SUBROUTINE test_util_SolvLin
   
!=============================================================================================
   SUBROUTINE test_util_UpdateSk1
!=============================================================================================

   type   (str_t), allocatable :: list0(:), list1(:)
   type   (str_t)              :: ins(3)
   integer(Ikind)              :: i, n, n1, pos, rem(3) = [2,4,9]
   character(len=20)           :: fmt, fmt1
   
   print*
   print*,'************* Begin test_util_UpdateSk1 *************'
   print*
   
   n = 8
   allocate(list0(n))
   list0(1)%str = 'A' ; list0(2)%str = 'B' ; list0(3)%str = 'C' ; list0(4)%str = 'D'
   list0(5)%str = 'E' ; list0(6)%str = 'F' ; list0(7)%str = 'G' ; list0(8)%str = 'H'

   ins(1)%str = 'ins1' ; ins(2)%str = 'ins2' ; ins(3)%str = 'ins3'
   
!- 1st test: ------------------------------      
   allocate(list1, source = list0)

   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(a,1x),a)"
   write(*,fmt)"- Remove from the rank-1 array of strings list1 = [ ",&
                           (list1(i)%str, i = 1, n),"]"
   ! nagfor doesn't like this line:
   !write(*,'(a,*(a,1x),a)')"- Remove from the rank-1 array of strings list1 = [ ",&
   !                        (list1(i)%str, i = 1, n),"]"
   ! (Unlimited format item can only be the last item of a format specification)
   !
   write(*,'(a,3(i0,1x),a)')"  the elements of indices: ",rem, &
                              "(indices out of range will be ignored)"
   
   call util_UpdateSk1 ( list1, EltToRemove = rem, stat = stat)

   print*
   call stat%display()
   
   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(a,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", &
                   (list1(i)%str, i = 1, n1),"]"

!- 2nd test: ------------------------------      
   print*
   list1 = list0

   write(*,fmt)"- Insert into the rank-1 array of strings list1 = [ ",&
                           (list1(i)%str, i = 1, n),"]"
   write(*,'(a,3(a,1x),a)')"  the array: [ ",(ins(i)%str,i=1,3),"]"
   write(*,'(a)'          )"  The insertion will done at the beginning of list1"

   call util_UpdateSk1 ( list1, ListToInsert = Ins, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)
   
   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(a,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", &
                   (list1(i)%str, i = 1, n1),"]"

!- 3rd test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 array of strings list1 = [ ",&
                           (list1(i)%str, i = 1, n),"]"
   write(*,'(a,3(a,1x),a)')"  the array: [ ",(ins(i)%str,i=1,3),"]"
   write(*,'(a,i0)'       )"  The insertion will start at the index #",pos

   call util_UpdateSk1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(a,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", &
                   (list1(i)%str, i = 1, n1),"]"

!- 4th test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 array of strings list1 = [ ",&
                           (list1(i)%str, i = 1, n),"]"
   write(*,'(a,3(a,1x),a)')"  the array: [ ",(ins(i)%str,i=1,3),"]"
   write(*,'(a)'          )"  The insertion will done at the end of list1"

   call util_UpdateSk1 ( list1, ListToInsert = Ins, Pos =-1_Ikind, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(a,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", &
                   (list1(i)%str, i = 1, n1),"]"

!- 5th test: ------------------------------      
   print*
   list1 = list0
   pos = n+5
   write(*,fmt)"- Insert into the rank-1 array of strings list1 = [ ",&
                           (list1(i)%str, i = 1, n),"]"
   write(*,'(a,3(a,1x),a)')"  the array: [ ",(ins(i)%str,i=1,3),"]"
   write(*,'(a,i0,a)'     )"  The insertion will start at the index #",pos," > size(list1)"
   write(*,'(a,i0,a)'     )"  (the gap will be filled by a set of empty characters)"

   call util_UpdateSk1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(a,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", &
                   (list1(i)%str, i = 1, n1),"]"

!- 6th test: ------------------------------      
   print*
   list1 = list0
   pos = 2
   write(*,fmt)"- Remove from the rank-1 array of strings list1 = [ ",&
                                 (list1(i)%str, i = 1, n),"]"
   write(*,'(a,3(i0,1x),a)' )"  the elements of indices: ",rem, &
                                 "(indices out of range will be ignored)"
                                 
   write(*,'(a,3(a,1x),a)')"  and insert the array: [ ",(ins(i)%str,i=1,3),"]"
   write(*,'(a,i0,a)'     )"  at the index #",pos," of the resulting array"// &
                              " (removal is always done first)"                                 

   call util_UpdateSk1 ( list1, ListToInsert = Ins, Pos = pos, EltToRemove = rem, &
                         stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(a,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", &
                   (list1(i)%str, i = 1, n1),"]"
   
      
   print*
   print*,'-------------  End test_util_UpdateSk1  -------------'  
   print*
   
   END SUBROUTINE test_util_UpdateSk1


!=============================================================================================
   SUBROUTINE test_util_UpdateIk1
!=============================================================================================

   integer(Ikind), allocatable :: list0(:), list1(:)
   integer(Ikind)              :: ins(3) = [101,202,303]
   integer(Ikind)              :: i, n, n1, pos, rem(3) = [2,4,9]
   character(len=20)           :: fmt, fmt1
   
   print*
   print*,'************* Begin test_util_UpdateIk1 *************'
   print*
   
   n = 8
   allocate(list0(n)) ; list0 = [(i*11,i=1,n)]

   
!- 1st test: ------------------------------      
   allocate(list1, source = list0)

   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(i0,1x),a)"
   write(*,fmt)"- Remove from the rank-1 integer array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the elements of indices: ",rem, &
                              "(indices out of range will be ignored)"
   
   call util_Updatek1 ( list1, EltToRemove = rem, stat = stat)

   print*
   call stat%display()
   
   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"


!- 2nd test: ------------------------------      
   print*
   list1 = list0

   write(*,fmt)"- Insert into the rank-1 integer array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the array: [ ",ins,"]"
   write(*,'(a)'           )"  The insertion will done at the beginning of list1"

   call util_Updatek1 ( list1, ListToInsert = Ins, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)
   
   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"
      
!- 3rd test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 integer array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the array: [ ",ins,"]"
   write(*,'(a,i0)'       )"  The insertion will start at the index #",pos

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 4th test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 integer array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the array: [ ",ins,"]"
   write(*,'(a)'          )"  The insertion will done at the end of list1"

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos =-1_Ikind, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 5th test: ------------------------------      
   print*
   list1 = list0
   pos = n+5
   write(*,fmt)"- Insert into the rank-1 integer array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the array: [ ",ins,"]"
   write(*,'(a,i0,a)'     )"  The insertion will start at the index #",pos," > size(list1)"
   write(*,'(a,i0,a)'     )"  (the gap will be filled by a set of 0)"

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 6th test: ------------------------------      
   print*
   list1 = list0
   pos = 2
   write(*,fmt)"- Remove from the rank-1 integer array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)' )"  the elements of indices: ",rem, &
                                 "(indices out of range will be ignored)"
                                 
   write(*,'(a,3(i0,1x),a)')"  and insert the array: [ ",ins,"]"
   write(*,'(a,i0,a)'     )"  at the index #",pos," of the resulting array"// &
                              " (removal is always done first)"                                 

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, EltToRemove = rem, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"
                         
   print*
   print*,'-------------  End test_util_UpdateIk1  -------------'  
   print*
   
   END SUBROUTINE test_util_UpdateIk1 


!=============================================================================================
   SUBROUTINE test_util_UpdateRk1
!=============================================================================================

   real   (Rkind), allocatable :: list0(:), list1(:)
   real   (Rkind)              :: ins(3) = [101.0_Rkind,202.0_Rkind,303.0_Rkind]
   integer(Ikind)              :: i, n, n1, pos, rem(3) = [2,4,9]
   character(len=20)           :: fmt, fmt1, fmt0, fmt2, f0 = "f5.1"
   
   fmt0 = "("//trim(f0)//",1x),a)"
   fmt2 = "(a,3("//trim(f0)//",1x),a)"

   print*
   print*,'************* Begin test_util_UpdateRk1 *************'
   print*
   
   n = 8
   allocate(list0(n)) ; list0 = real([(i*11,i=1,n)],kind=Rkind)

   
!- 1st test: ------------------------------      
   allocate(list1, source = list0)

   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//trim(fmt0)
   write(*,fmt)"- Remove from the rank-1 real array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the elements of indices: ",rem, &
                              "(indices out of range will be ignored)"
   
   call util_Updatek1 ( list1, EltToRemove = rem, stat = stat)

   print*
   call stat%display()
   
   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"


!- 2nd test: ------------------------------      
   print*
   list1 = list0

   write(*,fmt)"- Insert into the rank-1 real array list1 = [ ",list1,"]"
   write(*,fmt2)"  the array: [ ",ins,"]"
   write(*,'(a)'           )"  The insertion will done at the beginning of list1"

   call util_Updatek1 ( list1, ListToInsert = Ins, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)
   
   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"
      
!- 3rd test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 real array list1 = [ ",list1,"]"
   write(*,fmt2)"  the array: [ ",ins,"]"
   write(*,'(a,i0)'       )"  The insertion will start at the index #",pos

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 4th test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 real array list1 = [ ",list1,"]"
   write(*,fmt2)"  the array: [ ",ins,"]"
   write(*,'(a)'          )"  The insertion will done at the end of list1"

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos =-1_Ikind, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 5th test: ------------------------------      
   print*
   list1 = list0
   pos = n+5
   write(*,fmt)"- Insert into the rank-1 real array list1 = [ ",list1,"]"
   write(*,fmt2)"  the array: [ ",ins,"]"
   write(*,'(a,i0,a)'     )"  The insertion will start at the index #",pos," > size(list1)"
   write(*,'(a,i0,a)'     )"  (the gap will be filled by a set of 0)"

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 6th test: ------------------------------      
   print*
   list1 = list0
   pos = 2
   write(*,fmt)"- Remove from the rank-1 real array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)' )"  the elements of indices: ",rem, &
                                 "(indices out of range will be ignored)"
                                 
   write(*,fmt2)"  and insert the array: [ ",ins,"]"
   write(*,'(a,i0,a)'     )"  at the index #",pos," of the resulting array"// &
                              " (removal is always done first)"                                 

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, EltToRemove = rem, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"
                         
   print*
   print*,'-------------  End test_util_UpdateRk1  -------------'  
   print*
   
   END SUBROUTINE test_util_UpdateRk1 


!=============================================================================================
   SUBROUTINE test_util_UpdateCk1
!=============================================================================================

   complex(Rkind), allocatable :: list0(:), list1(:)
   complex(Rkind)              :: ins(3)
   integer(Ikind)              :: i, n, n1, pos, rem(3) = [2,4,9]
   character(len=50)           :: fmt, fmt1, fmt0, fmt2, f0 = "f5.1"
   
   fmt0 = "("//"'(',"//trim(f0)//",','"//trim(f0)//",')',1x),a)"
   fmt2 = "(a,3('(',"//trim(f0)//",','"//trim(f0)//",')',1x),a)"

   print*
   print*,'************* Begin test_util_UpdateCk1 *************'
   print*
   
   n = 8
   allocate(list0(n))
   list0 = real([(i*11,i=1,n)],kind=Rkind) + CIMAG*real([(i*11,i=1,n)],kind=Rkind)

   ins = [101.0_Rkind,202.0_Rkind,303.0_Rkind]
   ins = ins + CIMAG*ins
   
!- 1st test: ------------------------------      
   allocate(list1, source = list0)

   write(fmt,'(i0)')n
   fmt = "(a,/,'list1 = [ ',"//trim(fmt)//trim(fmt0)
   write(*,fmt)"- Remove from the rank-1 complex array ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the elements of indices: ",rem, &
                              "(indices out of range will be ignored)"
   
   call util_Updatek1 ( list1, EltToRemove = rem, stat = stat)

   print*
   call stat%display()
   
   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,/,'list1 = [',"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is:", list1,"]"


!- 2nd test: ------------------------------      
   print*
   list1 = list0

   write(*,fmt)"- Insert into the rank-1 complex array ",list1,"]"
   write(*,fmt2)"  the array: [ ",ins,"]"
   write(*,'(a)')"  The insertion will done at the beginning of list1"

   call util_Updatek1 ( list1, ListToInsert = Ins, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)
   
   write(fmt1,'(i0)')n1 ; fmt1 = "(a,/,'list1 = [',"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is:", list1,"]"
      
!- 3rd test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 complex array ",list1,"]"
   write(*,fmt2)"  the array: [ ",ins,"]"
   write(*,'(a,i0)')"  The insertion will start at the index #",pos

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,/,'list1 = [',"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is:", list1,"]"

!- 4th test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 complex array ",list1,"]"
   write(*,fmt2)"  the array: [ ",ins,"]"
   write(*,'(a)')"  The insertion will done at the end of list1"

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos =-1_Ikind, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,/,'list1 = [',"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is:", list1,"]"

!- 5th test: ------------------------------      
   print*
   list1 = list0
   pos = n+5
   write(*,fmt)"- Insert into the rank-1 complex array ",list1,"]"
   write(*,fmt2)"  the array: [ ",ins,"]"
   write(*,'(a,i0,a)')"  The insertion will start at the index #",pos," > size(list1)"
   write(*,'(a,i0,a)')"  (the gap will be filled by a set of 0)"

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,/,'list1 = [',"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is:", list1,"]"

!- 6th test: ------------------------------      
   print*
   list1 = list0
   pos = 2
   write(*,fmt)"- Remove from the rank-1 complex array ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the elements of indices: ",rem, &
                               "(indices out of range will be ignored)"
                                 
   write(*,fmt2)"  and insert the array: [ ",ins,"]"
   write(*,'(a,i0,a)')"  at the index #",pos," of the resulting array"// &
                         " (removal is always done first)"                                 

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, EltToRemove = rem, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,/,'list1 = [',"//trim(fmt1)//trim(fmt0)
   
   write(*,fmt1)"  The updated array is:", list1,"]"
                         
   print*
   print*,'-------------  End test_util_UpdateCk1  -------------'  
   print*
   
   END SUBROUTINE test_util_UpdateCk1 

!=============================================================================================
   SUBROUTINE test_util_UpdateLk1
!=============================================================================================

   logical       , allocatable :: list0(:), list1(:)
   logical                     :: ins(3) = [.true., .false., .true.]
   integer(Ikind)              :: i, n, n1, pos, rem(3) = [2,4,9]
   character(len=20)           :: fmt, fmt1
   
   print*
   print*,'************* Begin test_util_UpdateLk1 *************'
   print*
   
   n = 8
   allocate(list0(n)) ; list0 = .true.
   do i = 1, n, 2
      list0(i) = .false.
   end do   
   
!- 1st test: ------------------------------      
   allocate(list1, source = list0)

   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(g0,1x),a)"
   write(*,fmt)"- Remove from the rank-1 boolean array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)')"  the elements of indices: ",rem, &
                              "(indices out of range will be ignored)"
   
   call util_Updatek1 ( list1, EltToRemove = rem, stat = stat)

   print*
   call stat%display()
   
   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 2nd test: ------------------------------      
   print*
   list1 = list0

   write(*,fmt)"- Insert into the rank-1 boolean array = [ ",list1,"]"
   write(*,'(a,3(g0,1x),a)')"  the array: [ ",ins,"]"
   write(*,'(a)'           )"  The insertion will done at the beginning of list1"

   call util_Updatek1 ( list1, ListToInsert = Ins, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)
   
   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"
      
!- 3rd test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 boolean array list1 = [ ",list1,"]"
   write(*,'(a,3(g0,1x),a)')"  the array: [ ",ins,"]"
   write(*,'(a,i0)'       )"  The insertion will start at the index #",pos

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 4th test: ------------------------------      
   print*
   list1 = list0
   pos = 4
   write(*,fmt)"- Insert into the rank-1 boolean array list1 = [ ",list1,"]"
   write(*,'(a,3(g0,1x),a)')"  the array: [ ",ins,"]"
   write(*,'(a)'          )"  The insertion will done at the end of list1"

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos =-1_Ikind, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 5th test: ------------------------------      
   print*
   list1 = list0
   pos = n+5
   write(*,fmt)"- Insert into the rank-1 boolean array list1 = [ ",list1,"]"
   write(*,'(a,3(g0,1x),a)')"  the array: [ ",ins,"]"
   write(*,'(a,i0,a)'     )"  The insertion will start at the index #",pos," > size(list1)"
   write(*,'(a,i0,a)'     )"  (the gap will be filled by a set of .false.)"

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"

!- 6th test: ------------------------------      
   print*
   list1 = list0
   pos = 2
   write(*,fmt)"- Remove from the rank-1 boolean array list1 = [ ",list1,"]"
   write(*,'(a,3(i0,1x),a)' )"  the elements of indices: ",rem, &
                                 "(indices out of range will be ignored)"
                                 
   write(*,'(a,3(g0,1x),a)')"  and insert the array: [ ",ins,"]"
   write(*,'(a,i0,a)'     )"  at the index #",pos," of the resulting array"// &
                              " (removal is always done first)"                                 

   call util_Updatek1 ( list1, ListToInsert = Ins, Pos = pos, EltToRemove = rem, stat = stat )   

   print*
   call stat%display()

   n1 = size(list1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   write(*,fmt1)"  The updated array is: list1 = [ ", list1,"]"
                         
   print*
   print*,'-------------  End test_util_UpdateLk1  -------------'  
   print*
   
   END SUBROUTINE test_util_UpdateLk1 
   

!=============================================================================================
   SUBROUTINE test_util_allocIk1
!=============================================================================================

   integer(Ikind), allocatable :: A(:), A1(:)
   integer(Ikind)              :: n, n1, i
   character(len=50)           :: fmt1
   
   print*
   print*,'************* Begin test_util_allocIk1 *************'
   print*
   
   n = 8
   allocate(A(n)) ; A = [(i*11,i=1,n)]

!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 and assign its elements to 1" 
     
   call util_alloc ( mode = 'e', t = A1, n = n, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"

   A1 = 1
   write(*,'(a,i0)')"  . size of A1 is  : ",n1
   write(*,fmt1    )"  . and it contains: [ ",A1,"]"
   
!- 2nd test: ------------------------------      
   print*
   
   n1 = 5
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
   write(*,'(a)')"  (mode 'e')" 
     
   call util_alloc ( mode = 'e', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"


!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   n1 = n + 3
   write(*,fmt1)"- Reallocate the integer rank-1 array A1 = [ ", A1,"]"
   write(*,'(a,i0,a)')"  with a greater size: n1 = ",n1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to 0)"
   write(*,'(a)')"  (mode 's')" 
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"
   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   n1 = n - 3
   write(*,fmt1)"- Reallocate the integer rank-1 array A1 = [ ", A1,"]"
   write(*,'(a,i0,a)')"  with a smaller size: n1 = ",n1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries (elements beyond n1 are lost)"
   write(*,'(a)')"  (mode 's')" 
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"

!- 5th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   n1 = n + 3
   write(*,fmt1)"- Reallocate the integer rank-1 array A1 = [ ", A1,"]"
   write(*,'(a,i0)')"  to the zero array only if the new size (n1) is greater: n1 = ",n1
   write(*,'(a)')"  (mode 'i')" 

   call util_alloc ( mode = 'i', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"

!- 6th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   n1 = n
   write(*,fmt1)"- Reallocate the integer rank-1 array A1 = [ ", A1,"]"
   write(*,'(a,i0)')"  to the zero array only if the new size (n1) is greater: n1 = ",n1
   write(*,'(a)')"  (mode 'i')" 

   call util_alloc ( mode = 'i', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"      

!- 7th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   n1 = n-3
   write(*,fmt1)"- Reset the integer rank-1 array A1 = [ ", A1,"] to the zero array."
   write(*,'(a,i0)')"  Do not re-allocate A1 if its original size n is > n1 = ",n1
   write(*,'(a)')"  (mode 'z')" 
   
   call util_alloc ( mode = 'z', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)
   
   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"      
   
!- 8th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"
   
   n1 = n+3
   write(*,fmt1)"- Reset the integer rank-1 array A1 = [ ", A1,"] to the zero array."
   write(*,'(a,i0)')"  Do not re-allocate A1 if its original size n is > n1 = ",n1
   write(*,'(a)')"  (mode 'z')" 
   
   call util_alloc ( mode = 'z', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)
   
   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(i0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"     
                               
   print*
   print*,'-------------  End test_util_allocIk1  -------------'  
   print*
   
   END SUBROUTINE test_util_allocIk1
   

!=============================================================================================
   SUBROUTINE test_util_allocIk2
!=============================================================================================

   integer(Ikind), allocatable :: A(:,:), A1(:,:)
   integer(Ikind)              :: n, m, n1, m1, i, j
   character(len=50)           :: fmt1
   
   print*
   print*,'************* Begin test_util_allocIk2 *************'
   print*
   
   n = 3 ; m = 4
   allocate(A(n,m))
   A(1,:) = [11,12,13,14] ; A(2,:)=[21,22,23,24] ; A(3,:)=[31,32,33,34]

!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 and assign its elements to 1" 
     
   call util_alloc (mode = 'e', t = A1, n = n, m = m, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   A1 = 1
   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "
   
   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 2nd test: ------------------------------      
   print*
   
   n1 = 4 ; m1 = 2
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
     
   call util_alloc (mode = 'e', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the integer ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n + 3 ; m1 = m + 1
   
   write(*,'(a,i0,a,i0,a)')"  with a greater size: n1 x m1 = ",n1," x ",m1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to 0)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the integer ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n - 1 ; m1 = m - 2
   
   write(*,'(a,i0,a,i0,a)')"  with a smaller size: n1 x m1 = ",n1," x ",m1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries "
   write(*,'(a)')"  (but elements A1(i,j) with i > n1 or j > m1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 5th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the integer ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n - 1 ; m1 = m

   write(*,'(a,i0,a,i0,a)') &
      "  to the zero array only if one of the new sizes (n1 = ",n1," or m1 = ",m1,")"
   write(*,'(a,i0,a,i0,a)')"  is greater than the original one (n = ",n,", m = ",m,")"
   write(*,'(a)')"  (mode 'i')" 
      
   call util_alloc ( mode = 'i', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 6th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the integer ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n - 1 ; m1 = m + 2

   write(*,'(a,i0,a,i0,a)') &
      "  to the zero array only if one of the new sizes (n1 = ",n1," or m1 = ",m1,")"
   write(*,'(a,i0,a,i0,a)')"  is greater than the original one (n = ",n,", m = ",m,")"
   write(*,'(a)')"  (mode 'i')" 
      
   call util_alloc ( mode = 'i', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 7th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reset the integer ",n," x ",m," array A1:"
   
   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n - 1 ; m1 = m

   write(*,'(a,i0,a,i0,a)') &
   "  to the zero array but do not re-allocate A1 if the news sizes ([",n1,", ",m1,"])"
   write(*,'(a)')"  are less than or equal to the original ones"
   write(*,'(a)')"  (mode 'z')" 
         
   call util_alloc ( mode = 'z', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 8th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reset the integer ",n," x ",m," array A1:"
   
   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n+2  ; m1 = m

   write(*,'(a,i0,a,i0,a)') &
   "  to the zero array but do not re-allocate A1 if the news sizes ([",n1,", ",m1,"])"
   write(*,'(a)')"  are less than or equal to the original ones"
   write(*,'(a)')"  (mode 'z')" 
         
   call util_alloc ( mode = 'z', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(i3,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   
                                     
   print*
   print*,'-------------  End test_util_allocIk2  -------------'  
   print*
   
   END SUBROUTINE test_util_allocIk2
   

!=============================================================================================
   SUBROUTINE test_util_allocRk1
!=============================================================================================

   real     (Rkind ), allocatable :: A(:), A1(:)
   integer  (Ikind )              :: n, n1, i
   character(len=50)              :: fmt1
   
   print*
   print*,'************* Begin test_util_allocRk1 *************'
   print*
   
   n = 8
   allocate(A(n)) ; A = [(i*11,i=1,n)]

!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 and assign its elements to 1" 
     
   call util_alloc ( mode = 'e', t = A1, n = n, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(f5.1,1x),a)"

   A1 = 1
   write(*,'(a,i0)')"  . size of A1 is  : ",n1
   write(*,fmt1    )"  . and it contains: [",A1,"]"
   
!- 2nd test: ------------------------------      
   print*
   
   n1 = 5
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
     
   call util_alloc ( mode = 'e', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(f5.1,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [",A1,"]"


!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(f5.1,1x),a)"
   
   n1 = n + 3
   write(*,fmt1)"- Reallocate the real rank-1 array A1 = [", A1,"]"
   write(*,'(a,i0,a)')"  with a greater size: n1 = ",n1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to 0)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(f5.1,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [",A1,"]"
   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(f5.1,1x),a)"
   
   n1 = n - 3
   write(*,fmt1)"- Reallocate the real rank-1 array A1 = [", A1,"]"
   write(*,'(a,i0,a)')"  with a smaller size: n1 = ",n1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries (elements beyond n1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(f5.1,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [",A1,"]"

                         
   print*
   print*,'-------------  End test_util_allocRk1  -------------'  
   print*
   
   END SUBROUTINE test_util_allocRk1


!=============================================================================================
   SUBROUTINE test_util_allocRk2
!=============================================================================================

   real     (Rkind ), allocatable :: A(:,:), A1(:,:)
   integer  (Ikind )              :: n, m, n1, m1, i, j
   character(len=50)           :: fmt1
   
   print*
   print*,'************* Begin test_util_allocRk2 *************'
   print*
   
   n = 3 ; m = 4
   allocate(A(n,m))
   A(1,:) = [11,12,13,14] ; A(2,:)=[21,22,23,24] ; A(3,:)=[31,32,33,34]

!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 (and assign its elements to 1)" 
     
   call util_alloc ( mode = 'e', t = A1, n = n, m = m, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   A1 = 1
   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "
   
   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(f5.1,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 2nd test: ------------------------------      
   print*
   
   n1 = 4 ; m1 = 2
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
     
   call util_alloc ( mode = 'e', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(f5.1,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the real ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(f5.1,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n + 3 ; m1 = m + 1
   
   write(*,'(a,i0,a,i0,a)')"  with a greater size: n1 x m1 = ",n1," x ",m1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to 0)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(f5.1,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the real ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(f5.1,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n - 1 ; m1 = m - 2
   
   write(*,'(a,i0,a,i0,a)')"  with a smaller size: n1 x m1 = ",n1," x ",m1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries "
   write(*,'(a)')"  (but elements A1(i,j) with i > n1 or j > m1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(f5.1,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   
      
                         
   print*
   print*,'-------------  End test_util_allocRk2  -------------'  
   print*
   
   END SUBROUTINE test_util_allocRk2
   

!=============================================================================================
   SUBROUTINE test_util_allocCk1
!=============================================================================================

   complex  (Rkind ), allocatable :: A(:), A1(:)
   integer  (Ikind )              :: n, n1, i
   character(len=50)              :: fmt, fmt0, f0 = "f5.1"
   
   fmt0 = "("//"'(',"//trim(f0)//",','"//trim(f0)//",')',1x),a)"
   
   print*
   print*,'************* Begin test_util_allocCk1 *************'
   print*
   
   n = 4
   allocate(A(n)) ; A = [(i*11,i=1,n)] ; A = A + CIMAG * A

!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 (and assign its elements to 1)" 
     
   call util_alloc ( mode = 'e', t = A1, n = n, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt,'(i0)')n1
   fmt = "(a,"//trim(fmt)//trim(fmt0)

   A1 = 1
   write(*,'(a,i0)')"  . size of A1 is  : ",n1
   write(*,fmt     )"  . and it contains: [",A1,"]"
   

!- 2nd test: ------------------------------      
   print*
   
   n1 = 6
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
     
   call util_alloc ( mode = 'e', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt,'(i0)')n1
   fmt = "(a,"//trim(fmt)//trim(fmt0)

   write(*,'(a,i0)')"  . size of A1 is  : ",n1
   write(*,fmt     )"  . and it contains: [",A1,"]"


!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//trim(fmt0)
   
   n1 = n + 1
   write(*,fmt)"- Reallocate the complex rank-1 array A1 = [", A1,"]"
   write(*,'(a,i0,a)')"  with a greater size: n1 = ",n1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to 0)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt,'(i0)')n1
   fmt = "(a,"//trim(fmt)//trim(fmt0)

   write(*,'(a,i0)')"  . size of A1 is  : ",n1
   write(*,fmt     )"  . and it contains: [",A1,"]"

   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//trim(fmt0)
   
   n1 = n - 2
   write(*,fmt)"- Reallocate the complex rank-1 array A1 = [", A1,"]"
   write(*,'(a,i0,a)')"  with a smaller size: n1 = ",n1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries (elements beyond n1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt,'(i0)')n1
   fmt = "(a,"//trim(fmt)//trim(fmt0)

   write(*,'(a,i0)')"  . size of A1 is  : ",n1
   write(*,fmt     )"  . and it contains: [",A1,"]"
                            
   print*
   print*,'-------------  End test_util_allocCk1  -------------'  
   print*
   
   END SUBROUTINE test_util_allocCk1
   

!=============================================================================================
   SUBROUTINE test_util_allocCk2
!=============================================================================================

   complex  (Rkind ), allocatable :: A(:,:), A1(:,:)
   integer  (Ikind )              :: n, m, n1, m1, i, j
   character(len=50)           :: fmt1
   
   print*
   print*,'************* Begin test_util_allocCk2 *************'
   print*
   
   n = 3 ; m = 4
   allocate(A(n,m))
   A(1,:) = [11,12,13,14] ; A(2,:)=[21,22,23,24] ; A(3,:)=[31,32,33,34]
   A = A + CIMAG * A
   
!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 (and assign its elements to 1)" 
     
   call util_alloc ( mode = 'e', t = A1, n = n, m = m, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   A1 = 1
   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "
   
   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"('(',f5.1,',',f5.1,')',1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 2nd test: ------------------------------      
   print*
   
   n1 = 4 ; m1 = 2
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
     
   call util_alloc ( mode = 'e', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"('(',f5.1,',',f5.1,')',1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the complex ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"('(',f5.1,',',f5.1,')',1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n + 3 ; m1 = m + 1
   
   write(*,'(a,i0,a,i0,a)')"  with a greater size: n1 x m1 = ",n1," x ",m1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to 0)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"('(',f5.1,',',f5.1,')',1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the complex ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"('(',f5.1,',',f5.1,')',1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n - 1 ; m1 = m - 2
   
   write(*,'(a,i0,a,i0,a)')"  with a smaller size: n1 x m1 = ",n1," x ",m1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries "
   write(*,'(a)')"  (but elements A1(i,j) with i > n1 or j > m1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"('(',f5.1,',',f5.1,')',1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   
      
                         
   print*
   print*,'-------------  End test_util_allocCk2  -------------'  
   print*
   
   END SUBROUTINE test_util_allocCk2


!=============================================================================================
   SUBROUTINE test_util_allocLk1
!=============================================================================================

   logical          , allocatable :: A(:), A1(:)
   integer  (Ikind )              :: n, n1
   character(len=50)              :: fmt1
   
   print*
   print*,'************* Begin test_util_allocLk1 *************'
   print*
   
   n = 8
   allocate(A(n)) ; A = .true.

!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 and assign its elements to .true." 
     
   call util_alloc ( mode = 'e', t = A1, n = n, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"

   A1 = .true.
   write(*,'(a,i0)')"  . size of A1 is  : ",n1
   write(*,fmt1    )"  . and it contains: [ ",A1,"]"
   
!- 2nd test: ------------------------------      
   print*
   
   n1 = 5
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
     
   call util_alloc ( mode = 'e', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"


!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   n1 = n + 3
   write(*,fmt1)"- Reallocate the boolean rank-1 array A1 = [", A1,"]"
   write(*,'(a,i0,a)')"  with a greater size: n1 = ",n1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to F)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"
   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   n1 = n - 3
   write(*,fmt1)"- Reallocate the boolean rank-1 array A1 = [", A1,"]"
   write(*,'(a,i0,a)')"  with a smaller size: n1 = ",n1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries (elements beyond n1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains  : [ ",A1,"]"

                         
   print*
   print*,'-------------  End test_util_allocLk1  -------------'  
   print*
   
   END SUBROUTINE test_util_allocLk1


!=============================================================================================
   SUBROUTINE test_util_allocLk2
!=============================================================================================

   logical       , allocatable :: A(:,:), A1(:,:)
   integer(Ikind)              :: n, m, n1, m1, i, j
   character(len=50)           :: fmt1
   
   print*
   print*,'************* Begin test_util_allocLk2 *************'
   print*
   
   n = 3 ; m = 4
   allocate(A(n,m))
   A(1,:) = .true. ; A(2,:)=.false. ; A(3,:)=.true.

!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 and assign its elements to .true." 
     
   call util_alloc ( mode = 'e', t = A1, n = n, m = m, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   A1 = .true.
   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "
   
   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 2nd test: ------------------------------      
   print*
   
   n1 = 4 ; m1 = 2
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
     
   call util_alloc ( mode = 'e', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the boolean ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n + 3 ; m1 = m + 1
   
   write(*,'(a,i0,a,i0,a)')"  with a greater size: n1 x m1 = ",n1," x ",m1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to F)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the boolean ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j),j=1,m)
   end do   
   
   n1 = n - 1 ; m1 = m - 2
   
   write(*,'(a,i0,a,i0,a)')"  with a smaller size: n1 x m1 = ",n1," x ",m1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries "
   write(*,'(a)')"  (but elements A1(i,j) with i > n1 or j > m1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j),j=1,m1)
   end do   
                         
   print*
   print*,'-------------  End test_util_allocLk2  -------------'  
   print*
   
   END SUBROUTINE test_util_allocLk2


!=============================================================================================
   SUBROUTINE test_util_allocSk1
!=============================================================================================

   type     (str_t ), allocatable :: A(:), A1(:)
   integer  (Ikind )              :: n, n1, i
   character(len=50)              :: fmt1
   
   print*
   print*,'************* Begin test_util_allocSk1 *************'
   print*
   
   n = 4
   allocate(A(n))
   A(1)%str = 'a' ; A(2)%str = 'bb' ; A(3)%str = 'ccc' ; A(4)%str ='dddd'
   
!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 and assign its elements to 'zzz'" 
     
   call util_alloc ( mode = 'e', t = A1, n = n, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"

   do i = 1, n1 ; A1(i)%str = 'zzz' ; end do
   
   write(*,'(a,i0)')"  . size of A1 is  : ",n1
   write(*,fmt1    )"  . and it contains: [ ",(A1(i)%str,i=1,n1),"]"
   
!- 2nd test: ------------------------------      
   print*
   
   n1 = 5
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 
     
   call util_alloc ( mode = 'e', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   do i = 1, n1
      if ( A1(i)%str == '' ) A1(i)%str = '.'
   end do   
   write(*,fmt1    )"  . and it contains: [ ",(A1(i)%str,i=1,n1),"]"
   write(*,'(a)')"  (where for clarity we replaced empty char by '.')"


!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   write(*,fmt1)"- Reallocate the string rank-1 array A1 = [ ", (A1(i)%str,i=1,n),"]"
   n1 = n + 3
   write(*,'(a,i0,a)')"  with a greater size: n1 = ",n1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to empty char)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   do i = 1, n1
      if (A1(i)%str == '') A1(i)%str = '.'
   end do   
   write(*,fmt1    )"  . and it contains: [ ",(A1(i)%str,i=1,n1),"]"
   write(*,'(a)')"  (where for clarity we replaced empty char by '.')"
   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(fmt1,'(i0)')n ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"
   
   write(*,fmt1)"- Reallocate the string rank-1 array A1 = [ ", (A1(i)%str,i=1,n),"]"
   n1 = n - 2
   write(*,'(a,i0,a)')"  with a smaller size: n1 = ",n1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries (elements beyond n1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, stat = stat)

   print*
   call stat%display()
   
   n1 = size(A1)

   write(fmt1,'(i0)')n1 ; fmt1 = "(a,"//trim(fmt1)//"(g0,1x),a)"

   write(*,'(a,i0)')"  . size of A1 is now: ",n1
   write(*,fmt1    )"  . and it contains: [ ",(A1(i)%str,i=1,n1),"]"

                         
   print*
   print*,'-------------  End test_util_allocLk1  -------------'  
   print*
   
   END SUBROUTINE test_util_allocSk1
   
   
!=============================================================================================
   SUBROUTINE test_util_allocSk2
!=============================================================================================

   type     (str_t ), allocatable :: A(:,:), A1(:,:)
   integer  (Ikind )              :: n, m, n1, m1, i, j
   character(len=50)              :: fmt1
   
   print*
   print*,'************* Begin test_util_allocSk2 *************'
   print*
   
   n = 3 ; m = 4
   allocate(A(n,m))
   A(1,1)%str = 'a' ; A(1,2)%str = 'bb' ; A(1,3)%str = 'ccc' ; A(1,4)%str ='dddd'
   A(2,1)%str = '21'; A(2,2)%str = '22' ; A(2,3)%str = '23'  ; A(2,4)%str ='24'
   A(3,1)%str = 'A1'; A(3,2)%str = 'B2' ; A(3,3)%str = 'C3'  ; A(3,4)%str ='D4'

!- 1st test: ------------------------------      

   write(*,'(a)')"- Simple allocation of A1 (and assign its elements to 'zzz')" 
     
   call util_alloc ( mode = 'e', t = A1, n = n, m = m, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   do j = 1, m1 ; do i = 1, n1 ; A1(i,j)%str = 'zzz' ; end do ; end do
   
   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "
   
   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j)%str,j=1,m1)
   end do   

!- 2nd test: ------------------------------      
   print*
   
   n1 = 4 ; m1 = 2
   write(*,'(a)')"- Reallocate A1 with a different size without saving its previous entries" 

   call util_alloc ( mode = 'e', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is  : ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n1
      do j = 1, m1
         if (A1(i,j)%str == '') A1(i,j)%str = '.'
      end do   
      write(*,fmt1)(A1(i,j)%str,j=1,m1)
   end do   
   write(*,'(a)')"  (where for clarity we replaced empty char by '.')"

!- 3rd test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the string ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j)%str,j=1,m)
   end do   
   
   n1 = n + 3 ; m1 = m + 1
   
   write(*,'(a,i0,a,i0,a)')"  with a greater size: n1 x m1 = ",n1," x ",m1," (extension)"
   write(*,'(a)')"  and with saving its previous entries (new elements are set to empty char)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n1
      do j = 1, m1
         if (A1(i,j)%str == '') A1(i,j)%str = '.'
      end do      
      write(*,fmt1)(A1(i,j)%str,j=1,m1)
   end do   
   write(*,'(a)')"  (where for clarity we replaced empty char by '.')"
   

!- 4th test: ------------------------------      
   print*
   
   deallocate(A1) ; allocate(A1,source = A)

   write(*,'(a,i0,a,i0,a)')"- Reallocate the string ",n," x ",m," array A1:"

   write(fmt1,'(i0)')m
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n
      write(*,fmt1)(A1(i,j)%str,j=1,m)
   end do   
   
   n1 = n - 1 ; m1 = m - 2
   
   write(*,'(a,i0,a,i0,a)')"  with a smaller size: n1 x m1 = ",n1," x ",m1," (reduction)"
   write(*,'(a)')"  and with saving its previous entries "
   write(*,'(a)')"  (but elements A1(i,j) with i > n1 or j > m1 are lost)"
   
   call util_alloc ( mode = 's', t = A1, n = n1, m = m1, stat=stat)

   print*
   call stat%display()
   
   n1 = size(A1,dim=1) ; m1 = size(A1,dim=2)

   write(*,'(a,i0,a,i0)')"  . shape of A1 is now: ",n1," x ",m1
   write(*,'(a)'        )"  . and it contains: "

   write(fmt1,'(i0)')m1
   fmt1 = "(10x,"//trim(fmt1)//"(g0,1x))"
   do i = 1, n1
      write(*,fmt1)(A1(i,j)%str,j=1,m1)
   end do   
                         
   print*
   print*,'-------------  End test_util_allocSk2  -------------'  
   print*
   
   END SUBROUTINE test_util_allocSk2


!=============================================================================================
   SUBROUTINE test_util_FindPosInList
!=============================================================================================

   integer(Ikind), allocatable :: A(:)
   integer(Ikind)              :: n, i, ideb, ifin, k, posk
   real                        :: t1, t2
   
   print*
   print*,'************* Begin test_util_FindPosInList *************'
   print*
   
   n = 10000
   allocate(A(n)) ; A = [(2*i, i=1,n)]
   
   k = 4002 ; ideb = 100 ; ifin = n-200
   
!- 1st test: ------------------------------      

   write(*,'(a,i0)')"- Find the absolute position of the integer k = ",k
   write(*,'(a,i0,a,i0)')"  in the array A (assumed unordered) and between indices ", &
                       ideb," and ",ifin
                   
   call cpu_time(t1)
   
   posk = util_FindPosInUnsortedList ( A, ideb, ifin, k )  
   
   call cpu_time(t2)

   print*
   write(*,'(a,i0)')"  . the position is: posk = ",posk
   write(*,'(a,i0)')"  . verification   : A(posk) = ",A(posk)
   write(*,'(a,g0)')"  . cpu time needed: ",t2-t1   

!- 2nd test: ------------------------------      
   print*
   
   write(*,'(a,i0)')"- Find by dichotomy the absolute position of the integer k = ",k
   write(*,'(a,i0,a,i0)')"  in the array A (assumed sorted in ascending order)"// &
                         " between indices ",ideb," and ",ifin
                   
                   
   call cpu_time(t1)
   
   posk = util_FindPosInSortedList ( A, ideb, ifin, k )  
   
   call cpu_time(t2)

   print*   
   write(*,'(a,i0)')"  . the position is: posk = ",posk
   write(*,'(a,i0)')"  . verification   : A(posk) = ",A(posk)
   write(*,'(a,g0)')"  . cpu time needed: ",t2-t1   
                         
   print*
   print*,'-------------  End test_util_FindPosInList  -------------'  
   print*
   
   END SUBROUTINE test_util_FindPosInList


!=============================================================================================
   SUBROUTINE test_util_TimeStamp
!=============================================================================================

   print*
   print*,'************* Begin test_util_TimeStamp *************'
   print*
   
   write(*,'(a,/)')" Print the current YMDHMS date as a time stamp"
   
   call util_TimeStamp ( STDOUT )
                            
   print*
   print*,'-------------  End test_util_TimeStamp  -------------'  
   print*
   
   END SUBROUTINE test_util_TimeStamp


!=============================================================================================
   SUBROUTINE test_util_GetUnit
!=============================================================================================

   integer(Ikind) :: unit1, unit2
   
   print*
   print*,'************* Begin test_util_GetUnit *************'
   print*
   

!- 1st test: ------------------------------      

   write(*,'(a)')" Return a first free FORTRAN unit number"
   
   print*
   unit1 = util_GetUnit ()
   
   if (unit1 /= 0) then
      write(*,'(a,i0)')"  . returned free unit: ",unit1
      write(*,'(a)'   )"  . open a file ('toto1')"
      open(unit1, file='toto1')
   else
      write(*,'(a,i0)')"  . no free unit found"
   end if   
         
!- 2nd test: ------------------------------      

   write(*,'(/,a)')" Return a second free FORTRAN unit number"
   
   print*   
   unit2 = util_GetUnit ()

   if (unit2 /= 0) then
      write(*,'(a,i0)')"  . returned free unit: ",unit2
      write(*,'(a)'   )"  . open a file ('toto2')"
      open(unit2, file='toto2')
   else
      write(*,'(a,i0)')"  . no free unit found"
   end if   

!- 3rd test: ------------------------------      


   write(*,'(/,a,i0,a)')" Close unit #",unit2," and return a thrid free FORTRAN unit number"
   close(unit2,status='delete')
   
   print*   
   unit2 = util_GetUnit ()

   if (unit2 /= 0) then
      write(*,'(a,i0)')"  . returned free unit: ",unit2
   else
      write(*,'(a,i0)')"  . no free unit found"
   end if   
   
   close(unit1,status='delete') 
                         
   print*
   print*,'-------------  End test_util_GetUnit  -------------'  
   print*
   
   END SUBROUTINE test_util_GetUnit


!=============================================================================================
   SUBROUTINE test_util_Magic
!=============================================================================================

   integer(Ikind)              :: n, i, j
   integer(Ikind), allocatable :: mat(:,:)
   
   print*
   print*,'************* Begin test_util_Magic *************'
   print*
   

!- 1st test: ------------------------------      

   n = 4 ; allocate(mat(n,n))
   write(*,'(a,i0)')"- Create a magic square of order n = ",n
   
   
   call util_magic ( n, mat )

   write(*,'(/,a)')"  . Result:"   
   do i = 1, n
      write(*,'(*(i3,1x))')(mat(i,j),j=1,n)
   end do
               
!- 2nd test: ------------------------------      
   print*

   n = 5 ; deallocate(mat) ;  allocate(mat(n,n))
   write(*,'(a,i0)')"- Create a magic square of order n = ",n
   
   call util_magic ( n, mat )

   write(*,'(/,a)')"  . Result:"   
   do i = 1, n
      write(*,'(*(i3,1x))')(mat(i,j),j=1,n)
   end do
             

!- 3rd test: ------------------------------      
   print*

   n = 0 ; deallocate(mat) ;  allocate(mat(n,n))
   write(*,'(a,i0)')"- Create a magic square of order n = ",n
   
   call util_magic ( n, mat )

   write(*,'(/,a)')"  . Result:"   
   do i = 1, n
      write(*,'(*(i3,1x))')(mat(i,j),j=1,n)
   end do
             
   deallocate(mat)

               
   print*
   print*,'-------------  End test_util_Magic  -------------'  
   print*
   
   END SUBROUTINE test_util_Magic


!=============================================================================================
   SUBROUTINE test_util_IsRealCk0
!=============================================================================================

   complex(Rkind):: z1 = cmplx(1,2,kind=Rkind), z2 = cmplx(2,0,kind=Rkind), &
                    z3 = cmplx(1,EPS*.1,kind=Rkind)
   
   print*
   print*,'************* Begin test_util_IsRealCk0 *************'
   print*
   

!- 1st test: ------------------------------      
   write(*,'(a,"(",g0,",",g0,")",a)')"- Check if the complex number z1 = ",z1," is real"

   print*   
   if (util_IsReal(z1)) then
      write(*,'(a)')"  Yes"
   else
      write(*,'(a)')"  No"
   end if         

               
!- 2nd test: ------------------------------      
   print*

   write(*,'(a,"(",g0,",",g0,")",a)')"- Check if the complex number z2 = ",z2," is real"

   print*   
   if (util_IsReal(z2)) then
      write(*,'(a)')"  Yes"
   else
      write(*,'(a)')"  No"
   end if                 

!- 3rd test: ------------------------------      
   print*

   write(*,'(a,"(",g0,",",g0,")",a)')"- Check if the complex number z3 = ",z3," is real"

   print*   
   if (util_IsReal(z3)) then
      write(*,'(a)')"  Yes"
   else
      write(*,'(a)')"  No"
   end if              
             
   print*
   print*,'-------------  End test_util_IsRealCk0  -------------'  
   print*
   
   END SUBROUTINE test_util_IsRealCk0


!=============================================================================================
   SUBROUTINE test_util_IsRealCk1
!=============================================================================================

   complex(Rkind) :: z(4)
   integer(Ikind) :: i
   
   print*
   print*,'************* Begin test_util_IsRealCk1 *************'
   print*
   

!- 1st test: ------------------------------      
   z(1) = 1.0_Rkind
   z(2) = z(1) + CIMAG
   z(3) = CZERO
   z(4) = 2*CONE
   
   write(*,'(a,/)')"- Check if the following complex rank-1 array Z = "
   do i = 1, size(z)
      write(*,*)z(i)
   end do
   write(*,'(/,a)')"  is real"
      
   print*   
   if (util_IsReal(z)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

!- 2nd test: ------------------------------  
   print*
   z(1) = 1.0_Rkind
   z(2) = z(1) 
   z(3) = CZERO
   z(4) = 2*CONE
   
   write(*,'(a,/)')"- Check if the following complex rank-1 array Z = "
   do i = 1, size(z)
      write(*,*)z(i)
   end do
   write(*,'(/,a)')"  is real"
      
   print*   
   if (util_IsReal(z)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

!- 3rd test: ------------------------------  
   print*
   z(1) = 1.0_Rkind
   z(2) = z(1) + CIMAG*EPS*0.0001
   z(3) = CZERO
   z(4) = 2*CONE
   
   write(*,'(a,/)')"- Check if the following complex rank-1 array Z = "
   do i = 1, size(z)
      write(*,*)z(i)
   end do
   write(*,'(/,a)')"  is real"
      
   print*   
   if (util_IsReal(z)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         
             
   print*
   print*,'-------------  End test_util_IsRealC1k  -------------'  
   print*
   
   END SUBROUTINE test_util_IsRealCk1


!=============================================================================================
   SUBROUTINE test_util_IsRealCk2
!=============================================================================================

   complex(Rkind) :: z(4,2)
   integer(Ikind) :: i, j
   
   print*
   print*,'************* Begin test_util_IsRealCk2 *************'
   print*
   

!- 1st test: ------------------------------      
   z(1,1) = 1.0_Rkind
   z(2,1) = z(1,1) + CIMAG
   z(3,1) = CZERO
   z(4,1) = 2*CONE
   z(:,2) = z(:,1) * 1.5
   
   write(*,'(a,/)')"- Check if the following complex rank-2 array Z = "
   do i = 1, size(z,dim=1)
      write(*,*)(z(i,j),j=1,size(z,dim=2))
   end do
   write(*,'(/,a)')"  is real"
      
   print*   
   if (util_IsReal(z)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

!- 2nd test: ------------------------------  
   print*
   z(1,1) = 1.0_Rkind
   z(2,1) = z(1,1) 
   z(3,1) = CZERO
   z(4,1) = 2*CONE
   z(:,2) = z(:,1) * 1.5
   
   write(*,'(a,/)')"- Check if the following complex rank-2 array Z = "
   do i = 1, size(z,dim=1)
      write(*,*)(z(i,j), j=1,size(z,dim=2))
   end do
   write(*,'(/,a)')"  is real"
      
   print*   
   if (util_IsReal(z)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

!- 3rd test: ------------------------------  
   print*
   z(1,1) = 1.0_Rkind
   z(2,1) = z(1,1) + CIMAG*EPS*0.0001
   z(3,1) = CZERO
   z(4,1) = 2*CONE
   z(:,2) = z(:,1)*1.5
   write(*,'(a,/)')"- Check if the following complex rank-1 array Z = "
   do i = 1, size(z,dim=1)
      write(*,*)(z(i,j), j=1,size(z,dim=2))
   end do
   write(*,'(/,a)')"  is real"
      
   print*   
   if (util_IsReal(z)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         
             
   print*
   print*,'-------------  End test_util_IsRealCk2  -------------'  
   print*
   
   END SUBROUTINE test_util_IsRealCk2


!=============================================================================================
   SUBROUTINE test_util_IsIntgRk0
!=============================================================================================

   real(Rkind) :: x
   
   print*
   print*,'************* Begin test_util_IsIntgRk0 *************'
   print*
   

!- 1st test: ------------------------------      
   x = 1.5_Rkind
   
   write(*,'(a,g0)')"- Check if the real number x = ",x
   write(*,'(/,a)')"  is integer"
      
   print*   
   if (util_IsIntg(x)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

!- 2nd test: ------------------------------  
   print*
   x = 5.0_Rkind
   
   write(*,'(a,g0)')"- Check if the real number x = ",x
   write(*,'(/,a)')"  is integer"
      
   print*   
   if (util_IsIntg(x)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

!- 3rd test: ------------------------------  
   print*
   x = 1.0_Rkind + EPS*0.5
   write(*,'(a)')"- Check if the real number x = 1 + epsilon_machine/2"
   write(*,'(/,a)')"  is integer"
      
   print*   
   if (util_IsIntg(x)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         
             
   print*
   print*,'-------------  End test_util_IsIntgRk0  -------------'  
   print*
   
   END SUBROUTINE test_util_IsIntgRk0


!=============================================================================================
   SUBROUTINE test_util_IsIntgRk1
!=============================================================================================

   real(Rkind) :: x(4)
   
   print*
   print*,'************* Begin test_util_IsIntgRk1 *************'
   print*
   

!- 1st test: ------------------------------      
   x = [0.0, 1.5, 2.0, 3.0]
   
   write(*,'(a,4(g0,1x),a)')"- Check if the real rank-1 array X = [ ",x,"]"
   write(*,'(/,a)')"  is integer"
      
   print*   
   if (util_IsIntg(x)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

!- 2nd test: ------------------------------  
   print*
   x = [0.0, -1.0, 2.0, 3.0]
   
   write(*,'(a,4(g0,1x),a)')"- Check if the real rank-1 array X = [ ",x,"]"
   write(*,'(/,a)')"  is integer"
      
   print*   
   if (util_IsIntg(x)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

             
   print*
   print*,'-------------  End test_util_IsIntgRk1  -------------'  
   print*
   
   END SUBROUTINE test_util_IsIntgRk1   


!=============================================================================================
   SUBROUTINE test_util_IsIntgRk2
!=============================================================================================

   real   (Rkind) :: x(4,2)
   integer(Ikind) :: i, j
   
   print*
   print*,'************* Begin test_util_IsIntgRk2 *************'
   print*
   

!- 1st test: ------------------------------      
   x(:,1) = [0.0, 1.5, 2.0, 3.0] ; x(:,2) = 1.5*x(:,1)
   
   write(*,'(a,4(g0,1x),a,/)')"- Check if the following real rank-2 array X = "
   do i = 1, size(x,dim=1)
      write(*,*) (x(i,j), j=1,size(x,dim=2))
   end do   
   write(*,'(/,a)')"  is integer"
      
   print*   
   if (util_IsIntg(x)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

!- 2nd test: ------------------------------  
   print*
   x(:,1) = [0.0, -1.0, 2.0, 3.0] ; x(:,2) = 6.0*x(:,1)
   
   write(*,'(a,4(g0,1x),a,/)')"- Check if the following real rank-2 array X = "
   do i = 1, size(x,dim=1)
      write(*,*) (x(i,j),j=1,size(x,dim=2))
   end do      
   write(*,'(/,a)')"  is integer"
      
   print*   
   if (util_IsIntg(x)) then
      write(*,'(a)')"  Answer: Yes"
   else
      write(*,'(a)')"  Answer: No"
   end if         

             
   print*
   print*,'-------------  End test_util_IsIntgRk2  -------------'  
   print*
   
   END SUBROUTINE test_util_IsIntgRk2


!=============================================================================================
   SUBROUTINE test_util_SortIk1
!=============================================================================================

   integer(Ikind)              :: v(10)
   integer(Ikind), allocatable :: indx(:)
   
   print*
   print*,'************* Begin test_util_SortIk1 *************'
   print*
   

!- 1st test: ------------------------------      
   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a,10(i3,1x),a)')"- Sort in increasing order the integer rank-1 array v = [",v," ]"
   write(*,'(a)')"  and also return the original indices"

   call util_Sort ( v, 'i', stat, indx )   
   
   print*
   call stat%display()
   
   write(*,'(a,10(i3,1x),a)')"  . The sorted array: [",v," ]"
   write(*,'(a,10(i3,1x),a)')"  . The orginal ind.: [",indx," ]"
    
   
!- 2nd test: ------------------------------  
   print*

   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a,10(i3,1x),a)')"- Sort in decreasing order the integer rank-1 array v = [",v," ]"
   write(*,'(a)')"  and also return the original indices"

   call util_Sort ( v, 'd', stat, indx )   
   
   print*
   call stat%display()
   
   write(*,'(a,10(i3,1x),a)')"  . The sorted array: [",v," ]"
   write(*,'(a,10(i3,1x),a)')"  . The orginal ind.: [",indx," ]"

!- 3rd test: ------------------------------  
   print*

   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a)')"- Produce an INTENTIONAL ERROR:"

   call util_Sort ( v, 'c', stat, indx )   
   if (stat > IZERO) call stat%AddTrace('test_util_SortIk1')
   
   print*
   call stat%display(STDOUT,verb=IONE)
   
                
   print*
   print*,'-------------  End test_util_SortIk1  -------------'  
   print*
   
   END SUBROUTINE test_util_SortIk1


!=============================================================================================
   SUBROUTINE test_util_SortRk1
!=============================================================================================

   real   (Rkind)              :: v(10)
   integer(Ikind), allocatable :: indx(:)
   
   print*
   print*,'************* Begin test_util_SortRk1 *************'
   print*
   

!- 1st test: ------------------------------      
   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a)')"- Sort in increasing order the real rank-1 array "
   write(*,'(a,10(f5.1,1x),a)') "   v = [",v," ]"
   write(*,'(a)')"  and also return the original indices"

   call util_Sort ( v, 'i', stat , indx)   
   
   print*
   call stat%display()
   
   write(*,'(a,10(f5.1,1x),a)')"  . The sorted array: [",v," ]"
   write(*,'(a,10(i3,1x),a)')"  . The orginal ind.: [",indx," ]"
    
   
!- 2nd test: ------------------------------  
   print*

   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a)')"- Sort in decreasing order the real rank-1 array "
   write(*,'(a,10(f5.1,1x),a)') "   v = [",v," ]"
   write(*,'(a)')"  and also return the original indices"

   call util_Sort ( v, 'd', stat, indx )   
   
   print*
   call stat%display()
   
   write(*,'(a,10(f5.1,1x),a)')"  . The sorted array: [",v," ]"
   write(*,'(a,10(i3,1x),a)')"  . The orginal ind.: [",indx," ]"

!- 3rd test: ------------------------------  
   print*

   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a)')"- Produce an INTENTIONAL ERROR:"

   call util_Sort ( v, 'undef', stat, indx )   
   if (stat > IZERO) call stat%AddTrace('test_util_SortRk1')
   
   print*
   call stat%display()
   
                
   print*
   print*,'-------------  End test_util_SortRk1  -------------'  
   print*
   
   END SUBROUTINE test_util_SortRk1
   

!=============================================================================================
   SUBROUTINE test_util_SortCk1
!=============================================================================================

   complex(Rkind)              :: v(10)
   integer(Ikind), allocatable :: indx(:)
   integer(Ikind)              :: i
   real   (Rkind)              :: vi(10), vr(10)
   
   print*
   print*,'************* Begin test_util_SortCk1 *************'
   print*
   

!- 1st test: ------------------------------      
   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a)')"- Sort in increasing order of magnitude the complex rank-1 array v = "
   do i = 1, size(v)
      write(*,'(10x,"(",f5.1,",",f5.1,")",a,f5.1,a)') &
         v(i),"     (mag. = ",abs(v(i)),")"
   end do   
   write(*,'(a)')"  and also return the original indices"

   call util_Sort ( v, 'i', stat, indx )   
   
   print*
   call stat%display()
   
   write(*,'(a,10(f5.1,1x),a)')"  . The sorted array: v = "
   do i = 1, size(v)
      write(*,'(10x,"(",f5.1,",",f5.1,")",a,f5.1,a)') &
         v(i),"     (mag. = ",abs(v(i)),")"
   end do    
   write(*,'(a,10(i3,1x),a)')"  . The orginal ind.: [",indx," ]"
    
   
!- 2nd test: ------------------------------  
   print*

   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a)')"- Sort in decreasing order of magnitude the complex rank-1 array v = "
   do i = 1, size(v)
      write(*,'(10x,"(",f5.1,",",f5.1,")",a,f5.1,a)') &
         v(i),"     (mag. = ",abs(v(i)),")"
   end do   
   write(*,'(a)')"  and also return the original indices"


   call util_Sort ( v, 'd', stat, indx )   
   
   print*
   call stat%display()
   
   write(*,'(a,10(f5.1,1x),a)')"  . The sorted array: v = "
   do i = 1, size(v)
      write(*,'(10x,"(",f5.1,",",f5.1,")",a,f5.1,a)') &
         v(i),"     (mag. = ",abs(v(i)),")"
   end do    
   write(*,'(a,10(i3,1x),a)')"  . The orginal ind.: [",indx," ]"

!- 3rd test: ------------------------------  
   print*


   !call random_number(harvest=vr) !<-- leak reported (by leaks) with gfortran (gcc 9.2.0)
   !call random_number(harvest=vi) !    on mac os x
   
   vr = sin(v) ; vi = cos(v)*vr
   v = vr + vi * CIMAG
   
   write(*,'(a)')"- Sort in decreasing order of magnitude the complex rank-1 array v = "
   do i = 1, size(v)
      write(*,'(10x,"(",f5.1,",",f5.1,")",a,f5.2,a)') &
         v(i),"     (mag. = ",abs(v(i)),")"
   end do   
   write(*,'(a)')"  and also return the original indices"


   call util_Sort ( v, 'd', stat, indx )   
   
   print*
   call stat%display()
   
   write(*,'(a,10(f5.1,1x),a)')"  . The sorted array: v = "
   do i = 1, size(v)
      write(*,'(10x,"(",f5.1,",",f5.1,")",a,f5.2,a)') &
         v(i),"     (mag. = ",abs(v(i)),")"
   end do    
   write(*,'(a,10(i3,1x),a)')"  . The orginal ind.: [",indx," ]"
   
!- 4th test: ------------------------------  
   print*

   v = [5, -1, 2, 9, 8, -10, 7, 3, 2, 0]
   
   write(*,'(a)')"- Produce an INTENTIONAL ERROR:"

   call util_Sort ( v, 'undef', stat, indx )   
   if (stat > IZERO) call stat%AddTrace('test_util_SortCk1')
   
   print*
   call stat%display()
   
                
   print*
   print*,'-------------  End test_util_SortCk1  -------------'  
   print*
   
   END SUBROUTINE test_util_SortCk1


!=============================================================================================
   SUBROUTINE test_util_SortSk1
!=============================================================================================

   type   (str_t)              :: v(9)
   integer(Ikind), allocatable :: indx(:)
   integer(Ikind)              :: i, n
   character(len=50)           :: fmt
   
   print*
   print*,'************* Begin test_util_SortSk1 *************'
   print*
   

!- 1st test: ------------------------------      
   v(1)%str = 'nagfor' ; v(2)%str = 'gfortran' ; v(3)%str = 'xlf'
   v(4)%str = 'f77'    ; v(5)%str = 'ifort'    ; v(6)%str = 'g77'
   v(7)%str = 'g95'    ; v(8)%str = 'flang'    ; v(9)%str = 'pathscale'

   n = size(v)
   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(a,1x),a)"
  
   write(*,'(a)')"- Sort in increasing order the rank-1 array of strings"
   write(*,fmt)"     v = [ ",(v(i)%str,i=1,n),"]"
   write(*,'(a)')"  and also return the original indices"

   call util_Sort ( v, 'i', stat=stat, indx=indx )   
   
   print*
   call stat%display()
   
   write(*,fmt)"  . The sorted array: [ ",(v(i)%str,i=1,n),"]"
   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(i3,1x),a)"
   
   write(*,fmt)"  . The orginal ind.: [",indx," ]"
   
!- 2nd test: ------------------------------  
   print*
   v(1)%str = 'FORTRAN' ; v(2)%str = 'fortran' ; v(3)%str = 'C'
   v(4)%str = 'pascal'    ; v(5)%str = 'Basic'    ; v(6)%str = 'C++'
   v(7)%str = 'algol'    ; v(8)%str = 'Cobol'    ; v(9)%str = 'python'

   n = size(v)
   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(a,1x),a)"
  
   write(*,'(a)')"- Sort in increasing order and CASE SENSITIVE mode the rank-1 array of strings"
   write(*,fmt)"     v = [ ",(v(i)%str,i=1,n),"]"
   write(*,'(a)')"  and also return the original indices"

   call util_Sort ( v, 'i', stat=stat, indx=indx )   
   
   print*
   call stat%display()
   
   write(*,fmt)"  . The sorted array: [ ",(v(i)%str,i=1,n),"]"
   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(i3,1x),a)"
   
   write(*,fmt)"  . The orginal ind.: [",indx," ]"
   

!- 3rd test: ------------------------------  
   print*
   v(1)%str = 'FORTRAN' ; v(2)%str = 'fortran' ; v(3)%str = 'C'
   v(4)%str = 'pascal'    ; v(5)%str = 'Basic'    ; v(6)%str = 'C++'
   v(7)%str = 'algol'    ; v(8)%str = 'Cobol'    ; v(9)%str = 'python'

   n = size(v)
   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(a,1x),a)"
  
   write(*,'(a)')"- Sort in decreasing order and in CASE INSENSITIVE mode the rank-1 array of strings"
   write(*,fmt)"     v = [ ",(v(i)%str,i=1,n),"]"
   write(*,'(a)')"  and also return the original indices"

   call util_Sort ( v, 'd', is_CaseInsensitive=.true., stat=stat, indx=indx )   
   
   print*
   call stat%display()
   
   write(*,fmt)"  . The sorted array: [ ",(v(i)%str,i=1,n),"]"
   write(fmt,'(i0)')n
   fmt = "(a,"//trim(fmt)//"(i3,1x),a)"
   
   write(*,fmt)"  . The orginal ind.: [",indx," ]"
                   
   print*
   print*,'-------------  End test_util_SortSk1  -------------'  
   print*
   
   END SUBROUTINE test_util_SortSk1

                        
end program Check_Util_Other
   
   
