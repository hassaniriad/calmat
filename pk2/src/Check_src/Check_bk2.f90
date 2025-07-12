! For compiling (or use the makefile):
!
! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran Check_bk2.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_bk2

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort Check_bk2.f90  -L../../lib/ifort -lpk2 -llapack -lblas -o check_bk2

! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor Check_bk2.f90 -L../../lib/nagfor -lpk2 -llapack -lblas  -o check_bk2

program Check_bk2
   use bk2_m

   class(bk2_t), allocatable :: p, q, i, r, c, l, s, r1, r2
   
   type(str_t) :: str(2,2)
   
   integer(Ikind)  , allocatable :: Imat(:,:)
   real   (Rkind)  , allocatable :: Rmat(:,:)
   complex(Rkind)  , allocatable :: Cmat(:,:)
   logical         , allocatable :: Lmat(:,:)
   type     (str_t), allocatable :: Smat(:,:)
   character(len=:), allocatable :: Chmat(:,:)
   
   integer(Ikind) :: ii,jj, &
	              verb=4 ! set it to a value < 3 if you don't want the message: 'no error, no warning,...'
   
   str(1,1)%str = 'A'   ; str(1,2)%str = 'BB' 
   str(2,1)%str = 'CC'  ; str(2,2)%str = 'DDDD'  
     
   call test_assign
   call test_Transpose

   call test_getMat
   call test_Resize
   call test_DelRows
   call test_InsertInto
   
   
contains

!=============================================================================================
   subroutine test_assign   
!=============================================================================================
     
   print*
   print*,'Set an ik2 matrix (i = [1,3;2,4]): '
   i = ik2_t(matrix=reshape([1_Ikind,2_Ikind,3_Ikind,4_Ikind],[2,2]))
   call opflag%Display(STDOUT,verb=verb)
   call i%affiche()

!--------------------------------   
   print*
   print*,'Set an rk2 matrix (r = [1,3;2,4]): '   
   r = rk2_t(matrix=reshape([RONE,2*RONE,3*RONE,4*RONE],[2,2]))
   call opflag%Display(STDOUT,verb=verb)
   call r%affiche()

!--------------------------------   
   print*
   print*,'Set a ck2 matrix (c = [1+i,3+i;2+i,4+i]): '
   c = ck2_t(matrix=reshape([RONE+CIMAG,2*RONE+CIMAG,3*RONE+CIMAG,4*RONE+CIMAG],[2,2]))
   call opflag%Display(STDOUT,verb=verb)
   call c%affiche()   
   
!--------------------------------
   print*
   print*,'Set an lk2 matrix (l = [%t,%f;%f,%t]): '
   l = lk2_t(matrix=reshape([.true.,.false.,.false.,.true.],[2,2]))
   call opflag%Display(STDOUT,verb=verb)
   call l%affiche()
   
!--------------------------------
   print*
   print*,'Set an sk2 matrix (s = [A,BB;BB,DDDD]):'
   s = sk2_t(matrix=str)
   call opflag%Display(STDOUT,verb=verb)
   call s%affiche()

!--------------------------------
   print*   
   print*,'Do a polymorphic assignment: p = r: '
   p = r
   call opflag%Display(STDOUT,verb=verb)
   call p%affiche()

!--------------------------------
   print*   
   print*,'Do a polymorphic assignment: q = r: '
   q = r
   call opflag%Display(STDOUT,verb=verb)   
   call q%affiche()

!--------------------------------
   print*   
   print*,'Do r1 = r: '
   r1 = r
   call opflag%Display(STDOUT,verb=verb)   
   call r1%affiche()

!--------------------------------

   end subroutine test_assign 
       
!=============================================================================================
   subroutine test_GetMat   
!=============================================================================================
   
   if (allocated(Imat)) deallocate(Imat)
   print*   
   print*,'Do call i%GetMat(Imat) with Imat not allocated: '   
   call i%GetMat(I=Imat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Imat,dim=1)
      print*, (Imat(ii,jj),jj=1,size(Imat,dim=2))
   end do

   print*   
   print*,'Do call i%GetMat(Imat) with Imat already allocated with the right size: '   
   call i%GetMat(I=Imat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Imat,dim=1)
      print*, (Imat(ii,jj),jj=1,size(Imat,dim=2))
   end do

   if (allocated(Imat)) deallocate(Imat) ; allocate(Imat(30,20))
   print*   
   print*,'Do call i%GetMat(Imat) with Imat already allocated but without the right size: '   
   call i%GetMat(I=Imat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Imat,dim=1)
      print*, (Imat(ii,jj),jj=1,size(Imat,dim=2))
   end do

   
   print*   
   print*,'Do call r%GetMat(Rmat) : '   
   call r%GetMat(R=Rmat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Rmat,dim=1)
      print*, (Rmat(ii,jj),jj=1,size(Rmat,dim=2))
   end do

   print*   
   print*,'Do call c%GetMat(Cmat) : '   
   call c%GetMat(C=Cmat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Cmat,dim=1)
      print*, (Cmat(ii,jj),jj=1,size(Cmat,dim=2))
   end do

   print*   
   print*,'Do call l%GetMat(Lmat) : '   
   call l%GetMat(L=Lmat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Lmat,dim=1)
      print*, (Lmat(ii,jj),jj=1,size(Lmat,dim=2))
   end do

   print*   
   print*,'Do call s%GetMat(S=Smat) : '   
   call s%GetMat(S=Smat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Smat,dim=1)
      print*, (Smat(ii,jj)%str,' ',jj=1,size(Smat,dim=2))
   end do

   if (allocated(Chmat)) deallocate(Chmat)

   print*   
   print*,'Do call s%GetMat(Ch=Chmat) with Chmat not allocated: '   
   call s%GetMat(Ch=Chmat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Chmat,dim=1)
      print*, (Chmat(ii,jj),' ',jj=1,size(Chmat,dim=2))
   end do

   print*   
   print*,'Do call s%GetMat(Ch=Chmat) with Chmat allocated w/ right size and length: '   
   call s%GetMat(Ch=Chmat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Chmat,dim=1)
      print*, (Chmat(ii,jj),' ',jj=1,size(Chmat,dim=2))
   end do
   
   if (allocated(Chmat)) deallocate(Chmat) ; allocate(character(100) :: Chmat(s%nrow,s%ncol))
   
   print*   
   print*,'Do call s%GetMat(Ch=Chmat) with Chmat allocated but without the right length: '   
   call s%GetMat(Ch=Chmat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Chmat,dim=1)
      print*, (Chmat(ii,jj),' ',jj=1,size(Chmat,dim=2))
   end do
   
   if (allocated(Chmat)) deallocate(Chmat) ; allocate(character(100) :: Chmat(s%nrow+1,s%ncol))
   
   print*   
   print*,'Do call s%GetMat(Ch=Chmat) with Chmat allocated but without the right length and size: '   
   call s%GetMat(Ch=Chmat) ; call opflag%Display(STDOUT,verb=verb)
   do ii = 1, size(Chmat,dim=1)
      print*, (Chmat(ii,jj),' ',jj=1,size(Chmat,dim=2))
   end do
      
   end subroutine test_GetMat

!=============================================================================================
   subroutine test_Resize   
!=============================================================================================

   print*   
   print*,'Resize i (from 2x2 to 4x3):'   
   call i%Resize(mode='s',n=4_Ikind,m=3_Ikind) ; call opflag%Display(STDOUT,verb=verb)
   call i%affiche()

   print*   
   print*,'Resize r (from 2x2 to 4x3):'   
   call r%Resize(mode='s',n=4_Ikind,m=3_Ikind) ; call opflag%Display(STDOUT,verb=verb)
   call r%affiche()

   print*   
   print*,'Resize c (from 2x2 to 4x3):'   
   call c%Resize(mode='s',n=4_Ikind,m=3_Ikind) ; call opflag%Display(STDOUT,verb=verb)
   call c%affiche()

   print*   
   print*,'Resize l (from 2x2 to 4x3):'   
   call l%Resize(mode='s',n=4_Ikind,m=3_Ikind) ; call opflag%Display(STDOUT,verb=verb)
   call l%affiche()

   print*   
   print*,'Resize s (from 2x2 to 4x3):'   
   call s%Resize(mode='s',n=4_Ikind,m=3_Ikind) ; call opflag%Display(STDOUT,verb=verb)
   call s%affiche()   

   print*   
   print*,'Resize p (from 2x2 to 4x3):'   
   call p%Resize(mode='s',n=4_Ikind,m=3_Ikind) ; call opflag%Display(STDOUT,verb=verb)
   call p%affiche()   

   end subroutine test_Resize 
   
!=============================================================================================
   subroutine test_DelRows   
!=============================================================================================
 
   print*   
   print*,'Delete columns #1 and #3 of i: '   
   call i%DelRows(indx=[1,3],opt='c') ; call opflag%Display(STDOUT,verb=verb)
   call i%affiche()

   print*   
   print*,'Delete rows #2 and #4 of r: '   
   call r%DelRows(indx=[4,2],opt='r') ; call opflag%Display(STDOUT,verb=verb)
   call r%affiche()

   print*   
   print*,'Delete rows #2 and #4 of c: '   
   call c%DelRows(indx=[4,2],opt='r') ; call opflag%Display(STDOUT,verb=verb)
   call c%affiche()

   print*   
   print*,'Delete column #2 of l: '   
   call l%DelRows(indx=[2],opt='c') ; call opflag%Display(STDOUT,verb=verb)
   call l%affiche()

   print*   
   print*,'Delete column #2 of s: '   
   call s%DelRows(indx=[2],opt='c') ; call opflag%Display(STDOUT,verb=verb)
   call s%affiche()

   print*   
   print*,'Delete rows #2 and #4 of p: '   
   call p%DelRows(indx=[2,4],opt='c') ; call opflag%Display(STDOUT,verb=verb)
   call p%affiche()
   
   end subroutine test_DelRows

!=============================================================================================
   subroutine test_InsertInto   
!=============================================================================================

   print*   
   print*,'Insert i into p starting at column #2:'   
   call i%InsertInto(p, pnum=2_Ikind, cr='c') ; call opflag%Display(STDOUT,verb=verb)
   call p%affiche()

   print*   
   print*,'Insert p into q starting at column #2:'   
   call p%InsertInto(q, pnum=2_Ikind, cr='c') ; call opflag%Display(STDOUT,verb=verb)
   call q%affiche()

   end subroutine test_InsertInto

!=============================================================================================
   subroutine test_Transpose 
!=============================================================================================
   
   print*   
   print*,'transpose i:'   
   call i%transpose() ; call i%affiche()
   print*,'transpose r:'   
   call r%transpose() ; call r%affiche()
   print*,'transpose c:'   
   call c%transpose() ; call c%affiche()
   print*,'transpose l:'   
   call l%transpose() ; call l%affiche()
   print*,'transpose s:'   
   call s%transpose() ; call s%affiche() 
              
   end subroutine test_Transpose
      
!=============================================================================================
   subroutine test_SetSubmat   
!=============================================================================================
   
      
   end subroutine test_SetSubmat

 
end program Check_bk2
