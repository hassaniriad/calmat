!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: util_Other
!
! Description: 
! This module contains some usefull functions and subroutines (of my own or found on the web)
!---------------------------------------------------------------------------------------------
#include "error.fpp"

MODULE util_Other_m
!
!  Notes: 
!   . If you implement a new function or subroutine and, if appropriate, add in its arguments
!     a variable of DT "err_t", say "stat", with the intent(out) to report an error or a 
!     warning message to the caller. Use the defined constructor "err_t" or (better) the
!     bound-procedure %set, as follows (the 3 args are optionals):
!
!        stat = err_t (stat=code, msg="give the message here", where="name of the proc.")
!
!     (code may be > 0 (error) or < 0 (warning))
!
!     or (better): 
!
!        call stat%set (stat=code, msg="give the message here", where="name of the proc.")
!
!     (see the module err_m in the file err.f90)
!
!     If in the caller "ErrAbort" has been set to .false. by
!
!                        call SetHaltingMode ( halting = .false. )    
!           
!     then the caller may check the value of "stat%code" to see if anything went wrong, with
!     the following convention:
!
!       stat%code = 0 : no error and no warning
!       stat%code > 0 : an error occured and "stat%mesg" reports this error
!       stat%code < 0 : a warning message is given in "stat%mesg"
!
!   . If "ErrAbort" has been set to .true. (default value) and if stat%code = IERROR (> 0), 
!     the execution  will be stopped by the constructor "err_t" after the printing of the 
!     passed message on the standard output.
!---------------------------------------------------------------------------------------------
   
   use util_Strings_m

   implicit none
   
!  Content:
!  ^^^^^^^

!  . util_Alloc (generic subroutine):
!        Allocate or reallocate (resizing) of a rank 1 or 2 arrays of different types

!  . util_Eig (generic subroutine): (need LAPACK)
!        Computes eigenvalues and eigenvectors of matrices of different types   
         
!  . util_FindPosInUnsortedList (generic function):
!        Find the absolute position of a given entry (integer or character) in an unordered list

!  . util_FindPosInSortedList (recursive function):
!        Find the absolute position of a given entry (integer) in an ordered list

!  . util_GetUnit (function):                  
!        Returns a free FORTRAN unit number (author: John Burkardt)

!  . util_Horner (subroutine):                  
!        Evaluation of a polynomial and its derivative(s) by Horner's method

!  . util_Igemm (generic subroutine): (adapted BLAS routine DGEMM)
!        Performs a matrix-matrix operations c := alpha*op( a )*op( b ) + beta*c
!        for a and b integer matrices and for alpha, beta scalars of different types

!  . util_InvMats (generic subroutine): (need LAPACK)
!        Computes the inverse of matrices of different types

!  . util_isIncluded (function):
!        Returns .true. if the elements of a first integer array are all present in a 
!        second one

!  . util_IsIntg (generic function):
!        See if a given real or complex array of rank 0, 1 or 2 can be represented as an 
!        integer one

!  . util_IsReal (function):
!        See if a given complex array of rank 0, 1 or 2 can be represented as a real one

!  . util_Magic (subroutine):
!        Creates a magic square

!  . util_MatrixNorm (function):
!        Computes usual matricial norms of a matrix

!  . util_pwd (function):
!        Returns the current directory

!  . util_Random (generic function)
!        Function version of util_SubRandom

!  . util_RandPerm (subroutine):
!        Returns a vector containing a random permutation of {1,...,n} or 
!        a sample of unique integers of {1,...,n}

!  . util_RemoveDuplicate (subroutine):
!        Removes duplicate entries in a rank-1 integer array

!  . util_Reshape (generic subroutine):
!        Reshapes arrays of different types according to a given shape

!  . util_ScalarPow (generic subroutine)
!        Raises any number type to any power

!  . util_SolvLin (generic subroutine): (need LAPACK)
!        Solves a linear systems of different types

!  . util_Sort (generic subroutine): (adapted from LAPACK routine DLASRT)
!        Sorts an integer rank-1 array of different types

!  . util_SubRandom (generic subroutine)
!        Returns a matrix of uniformly distributed random numbers in the interval (0,1)
!         or a matrix of random integer numbers in a given interval 

!  . util_timestamp (subroutine):
!        Prints the current date and time (author: John Burkardt)

!  . util_Trapeze (subroutine):
!        Evaluate a 1d integral by the trapezoidal rule

!  . util_Updatek1 (generic subroutine):
!        Remove and/or insert elements in a rank 1 array of different types

!  . util_wtime (function):
!        Computes the elapsed wall clock time

!  Define some interfaces:
!  ^^^^^^^^^^^^^^^^^^^^^^

   interface util_Alloc ! (subroutines)
     module procedure util_allocIk1 , util_allocIk2, &
                      util_allocRk1 , util_allocRk2, &
                      util_allocCk1 , util_allocCk2, &
                      util_allocLk1 , util_allocLk2, &
                      util_allocSk1 , util_allocSk2  
   end interface util_Alloc

   interface util_Eig ! (subroutines)
      module procedure util_EigRsym, util_EigCherm, & ! <- sym. or hermitian matrices
                       util_EigRgen, util_EigCgen     ! <- general matrices
   end interface   

   interface util_Horner ! (subroutines)
      module procedure util_Horner1d, util_Horner2d, util_Horner3d  
   end interface
      
   interface util_Igemm ! (subroutines)
      module procedure util_IgemmI, util_IgemmR, util_IgemmC  
   end interface

   interface util_InvMats ! (subroutines)
      module procedure util_InvRMats1, util_InvRMats2, &
                       util_InvCMats1, util_InvCMats2, &
                       util_InvIMats
   end interface
      
   interface util_IsIntg ! (functions)
      module procedure util_IsIntgR, util_IsIntgC
   end interface util_IsIntg   

   interface util_PolDec ! (subroutines)
     module procedure util_PolDecI, util_PolDecR, util_PolDecC
   end interface util_PolDec

   interface util_Random ! (functions)
     module procedure util_RandomI , util_RandomR
   end interface util_Random

   interface util_Reshape ! (subroutines)
      module procedure util_ReshapeI, util_ReshapeR, util_ReshapeC, &
                       util_ReshapeL, util_ReshapeS  
   end interface

   interface util_ScalarPow ! (subroutines)
      module procedure util_ScalarPowI, util_ScalarPowR, util_ScalarPowC
   end interface 
   
   interface util_SolvLin ! (subroutines)
      module procedure util_SolvLinR1, util_SolvLinR2, &
                       util_SolvLinC1, util_SolvLinC2 
   end interface   
      
   interface util_Sort ! (subroutines)
      module procedure util_SortIk1, util_SortRk1, util_SortCk1, util_SortSk1   
   end interface
   
   interface util_SubRandom ! (subroutines)
     module procedure util_SubRandomI , util_SubRandomR
   end interface util_SubRandom

   interface util_Svd ! (subroutines)
     module procedure util_SvdR, util_SvdC
   end interface util_Svd

   interface util_Sinc ! (elemental function)
     module procedure util_SincI, util_SincR, util_SincC
   end interface util_Sinc
      
   interface util_Updatek1 ! (subroutines)
     module procedure util_UpdateSk1, util_UpdateIk1, util_UpdateRk1, &
                      util_UpdateCk1, util_UpdateLk1
   end interface util_Updatek1
   
   interface util_pow ! (subroutines)
      procedure util_IntgPowIntg, util_RealPowIntg, util_CplxPowIntg, & ! m^n, x^n, z^n
                util_IntgPowReal, util_RealPowReal, util_CplxPowReal, & ! m^r, x^r, z^r
                util_IntgPowCplx, util_RealPowCplx, util_CplxPowCplx    ! m^c, x^c, z^c            
   end interface util_pow
   
   interface util_FindPosInUnsortedList
      module procedure util_FindPosInUnsortedListIntg, util_FindPosInUnsortedListStr, &
                       util_FindPosInUnsortedListChar
   end interface util_FindPosInUnsortedList
  
CONTAINS

!=============================================================================================
   FUNCTION util_pwd ( stat ) result ( res )
!=============================================================================================
   use iso_c_binding
   type     (err_t)             , intent(in out) :: stat
   character(len=:), allocatable                 :: res
!---------------------------------------------------------------------------------------------   
!  Gets Path of Current Working Directory
!
!  Note: unfortunately, the function/subroutine fortran getcwd are not portable. 
!        As a workaround, we use here the interoperablity with C to call the corresponding C
!        function (adapted from a solution found on www.tek-tips.com).
!--------------------------------------------------------------------------- R.H. 07/18, 01/19
 
!- local variables ---------------------------------------------------------------------------
   character(len=*             ), parameter   :: HERE = 'util_pwd'   
   character(kind=c_char, len=:), allocatable :: dir
   integer  (c_long            )              :: i, ldir, iguess, lguess, nguess, err
   type     (c_ptr             )              :: buf
!---------------------------------------------------------------------------------------------    

   interface
      function c_pwd (dir, ldir) bind(c, name = "getcwd") result(r)
         use iso_c_binding
         integer  (c_size_t), value, intent(in    ) :: ldir
         character(c_char  ),        intent(   out) :: dir(*)
         type     (c_ptr   )                        :: r
      end function
   end interface
!
!- Find a length large enough:
!
   nguess = 5 ; lguess = 0 ; err = 1
   
   do iguess = 1, nguess
      lguess = lguess + MDSTR
      allocate(character(len=lguess) :: dir)
      buf = c_pwd (dir, lguess)
      if ( .not. c_associated(buf) ) then
         deallocate(dir)
         cycle
      else
         err = 0
         exit   
      end if   
   end do
   
   if ( err == 0 ) then
!
!-    Find the terminal null_char:
!   
      ldir = 0
      do i = 1, lguess
         if ( dir(i:i) == c_null_char ) then
            ldir = i ; exit
         end if   
      enddo
      if ( ldir > 0 ) then
         dir(ldir:) = ' ' ; res = trim(adjustl(dir))
         return
      else   
         err = 1
      end if   
   end if   
   
   if ( err /= 0 ) then
      call stat%Set ( stat = UERROR, where = HERE, &
                       msg = 'The length of the current directory is too long' )
      return
   end if   
   
   END FUNCTION util_pwd
      
   
!=============================================================================================
   SUBROUTINE util_UpdateSk1 ( List, EltToRemove, ListToInsert, Pos, stat ) 
!=============================================================================================
   type   (str_t), allocatable, intent(in out) :: List(:)
   integer(Ikind), optional   , intent(in    ) :: EltToRemove(:)      
   type   (str_t), optional   , intent(in    ) :: ListToInsert(:)
   integer(Ikind), optional   , intent(in    ) :: Pos
   type   (err_t)             , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  . If "EltToRemove" is present: 
!      Removes from the rank 1 array "List" elements whose indices are given in "EltToremove".
!      Indices out of range are ignored.
!
!  . If "ListToInsert" is present: 
!      Inserts into the array "List" the elements of the array "ListToInsert". 
!      This insertion starts at the the index "Pos".
!      If "Pos" is not present, "ListToInsert" starts at the begining of the list (Pos = 1).
!      If "Pos" is -1, "ListToInsert" is inserted at the end of the list (Pos = size(List)+1).
!
!  . If "EltToRemove" and "ListToInsert" are both present, removal is firts done.
!
!  Examples:
!
!   1) List = {'A', 'B', 'C', 'D'} 
!
!       call util_UpdateS1k ( List, EltToRemove = [2, 4, 5])
!
!       gives List = {'A', 'C'}
!
!       (indice #5 is out of range, thus ignored)
!
!
!   2) List = {'A', 'B', 'C', 'D'}, Ins = {'F', 'G', 'H'}
!
!      . call util_UpdateS1k ( List, ListoInsert = Ins )
!     or call util_UpdateS1k ( List, ListoInsert = Ins, Pos = 1 )
!
!      give List = { 'F', 'G', 'H',   'A', 'B', 'C', 'D'}
!
!      . call util_UpdateS1k ( List, ListoInsert = Ins, Pos = 3 )
!
!      gives List = { 'A', 'B',   'F', 'G', 'H',    'C', 'D'}
!
!      . call util_UpdateS1k ( List, ListoInsert = Ins, Pos =-1 )
!     or call util_UpdateS1k ( List, ListoInsert = Ins, Pos = 5 )
!
!      give List = { 'A', 'B', 'C', 'D',    'F', 'G', 'H' }
!
!      . call util_UpdateS1k ( List, ListoInsert = Ins, Pos = 8 )
!
!      gives List = { 'A', 'B', 'C', 'D', '', '', '',   'F', 'G', 'H' }
!
!
!   3) List = {'A', 'B', 'C', 'D'}, Ins = {'F', 'G', 'H'}
!
!      . call util_UpdateS1k ( List, EltToRemove = [2, 4, 5], ListoInsert = Ins )
!
!      gives List = {'F', 'G', 'H',  'A', 'C'}  (removal before insertion)
!
!      . call util_UpdateS1k ( List, , EltToRemove = [2, 4, 5], ListoInsert = Ins, Pos = 8 )
!
!      gives List = { 'A', 'C',   '', '', '', '','',     'F', 'G', 'H' }
!-----------------------------------------------------------------------------------R.H. 07/18 

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_UpdateSk1'
   integer  (Ikind)              :: i, p1, p2, ipos, nkeep, nold, nins, nnew, err
   integer  (Ikind)              :: lkeep(size(List))
   type     (str_t), allocatable :: tmp(:)
!---------------------------------------------------------------------------------------------   
   
   if ( .not. allocated(List) ) allocate(List(0))
   
   if ( present(EltToremove) ) then
      
      nkeep = 0
      do i = 1, size(List)
         if ( any(EltToRemove == i) ) cycle
         nkeep = nkeep + 1
         lkeep(nkeep) = i
      end do   

      if ( nkeep == 0 ) then
         deallocate(List) ; allocate(List(0))
      else                    
         allocate(tmp(nkeep), stat = err)
         if ( err /= 0 ) then
            call stat%set (IERROR, HERE, 'Allocation failure for array "tmp"')
            return
         end if   
         
         tmp(1:nkeep) = List(lkeep(1:nkeep))
         call move_alloc (from = tmp, to = List)
      end if   
   end if
   
   if ( present(ListToInsert) ) then
      nold = size(List)   
      nins = size(ListToInsert) ; if ( nins == 0 ) return
      
      ipos = 1 ; if ( present(Pos) ) ipos = Pos
   
      if ( ipos == -1 ) ipos = nold + 1
      
      nnew = nins + max(nold, ipos-1)
             
      allocate(tmp(nnew), stat = err)
      if ( err /= 0 ) then
         call stat%set (UERROR, HERE, 'Allocation failure for array "tmp"')
         return
      end if   

      p1 =      1 ; p2 = min(ipos-1,nold) ; tmp(p1:p2) = List(p1:p2)
      p1 = p2 + 1 ; p2 = ipos-1           ; do i=p1,p2 ; tmp(i)%str = ''; end do      
      p1 = p2 + 1 ; p2 = ipos + nins - 1  ; tmp(p1:p2) = ListToInsert(1:nins)
      p1 = p2 + 1 ; p2 = nnew             ; tmp(p1:p2) = List(ipos:nold)      
      
      call move_alloc (from = tmp, to = List)      
   end if
   
   END SUBROUTINE util_UpdateSk1


!=============================================================================================
   SUBROUTINE util_UpdateIk1 ( List, EltToRemove, ListToInsert, Pos, stat ) 
!=============================================================================================
   integer(Ikind), allocatable, intent(in out) :: List(:)
   integer(Ikind), optional   , intent(in    ) :: EltToRemove(:)      
   integer(Ikind), optional   , intent(in    ) :: ListToInsert(:)
   integer(Ikind), optional   , intent(in    ) :: Pos
   type   (err_t)             , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  See description of util_UpdateSk1
!-----------------------------------------------------------------------------------R.H. 07/18 

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_UpdateIk1'
   integer  (Ikind)              :: i, p1, p2, ipos, nkeep, nold, nins, nnew, err
   integer  (Ikind)              :: lkeep(size(List))
   integer  (Ikind)              :: zero
   integer  (Ikind), allocatable :: tmp(:)
!---------------------------------------------------------------------------------------------   

   zero = IZERO
   
#include "include/util_updatek1.inc"  
   
   END SUBROUTINE util_UpdateIk1
   

!=============================================================================================
   SUBROUTINE util_UpdateRk1 ( List, EltToRemove, ListToInsert, Pos, stat ) 
!=============================================================================================
   real   (Rkind), allocatable, intent(in out) :: List(:)
   integer(Ikind), optional   , intent(in    ) :: EltToRemove(:)      
   real   (Rkind), optional   , intent(in    ) :: ListToInsert(:)
   integer(Ikind), optional   , intent(in    ) :: Pos
   type   (err_t)             , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  See description of util_UpdateSk1
!-----------------------------------------------------------------------------------R.H. 07/18 

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_UpdateRk1'
   integer  (Ikind)              :: i, p1, p2, ipos, nkeep, nold, nins, nnew, err
   integer  (Ikind)              :: lkeep(size(List))
   real     (Rkind)              :: zero
   real     (Rkind), allocatable :: tmp(:)
!---------------------------------------------------------------------------------------------   

   zero = RZERO

#include "include/util_updatek1.inc"  
   
   END SUBROUTINE util_UpdateRk1


!=============================================================================================
   SUBROUTINE util_UpdateCk1 ( List, EltToRemove, ListToInsert, Pos, stat ) 
!=============================================================================================
   complex(Rkind), allocatable, intent(in out) :: List(:)
   integer(Ikind), optional   , intent(in    ) :: EltToRemove(:)      
   complex(Rkind), optional   , intent(in    ) :: ListToInsert(:)
   integer(Ikind), optional   , intent(in    ) :: Pos
   type   (err_t)             , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  See description of util_UpdateSk1
!-----------------------------------------------------------------------------------R.H. 07/18 

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_UpdateCk1'
   integer  (Ikind)              :: i, p1, p2, ipos, nkeep, nold, nins, nnew, err
   integer  (Ikind)              :: lkeep(size(List))
   complex  (Rkind)              :: zero
   complex  (Rkind), allocatable :: tmp(:)
!---------------------------------------------------------------------------------------------   

   zero = CZERO

#include "include/util_updatek1.inc"  
   
   END SUBROUTINE util_UpdateCk1


!=============================================================================================
   SUBROUTINE util_UpdateLk1 ( List, EltToRemove, ListToInsert, Pos, stat ) 
!=============================================================================================
   logical       , allocatable, intent(in out) :: List(:)
   integer(Ikind), optional   , intent(in    ) :: EltToRemove(:)      
   logical       , optional   , intent(in    ) :: ListToInsert(:)
   integer(Ikind), optional   , intent(in    ) :: Pos
   type   (err_t)             , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  See description of util_UpdateS1k
!-----------------------------------------------------------------------------------R.H. 07/18 

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_UpdateLk1'
   integer  (Ikind)              :: i, p1, p2, ipos, nkeep, nold, nins, nnew, err
   integer  (Ikind)              :: lkeep(size(List))
   logical                       :: zero
   logical         , allocatable :: tmp(:)
!---------------------------------------------------------------------------------------------   

   zero = LZERO
   
#include "include/util_updatek1.inc"  
   
   END SUBROUTINE util_UpdateLk1

   
!=============================================================================================
   SUBROUTINE util_allocIk1 ( mode, t, n, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode
   integer  (Ikind),              intent(in    ) :: n
   integer  (Ikind), allocatable, intent(in out) :: t(:)
   type     (err_t),              intent(in out) :: stat   
   integer  (Ikind), optional   , intent(in    ) :: const   
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-1 integer array t. It is specially useful   
!  when t is already allocated but needs to be extended.
! 
! * When t is not allocated, it is simply allocated to the zero array of size n whatever the
!   value of 'mode' is. Otherwise:
!   
! * If mode = 's' ('save') (we want an array of size n formed by the first n entries of t)
!
!   t is first saved in a temporary array, de-allocated and re-allocated to the new size (n). 
!   The saved entries are then copied back to t.
!
!   Warning: if the new size n is less than the old one, the part of t that is located beyond
!            n will be definitively lost. 
!
! * If mode = 'a' ('allocate'), (we want an array of a size n)
!
!   . if size(t) == n:  (t has the required size)
!            t is unchanged
!   . else 
!            t is de-allocated and re-allocated to the zero (or const) array of size n
!
! * If mode = 'e' ('erase'), (we want an array of a size n initialized to 0)
!
!   . if size(t) == n:  (t has the required size)
!            t is simply reset to 0
!   . else 
!            t is de-allocated and re-allocated to the zero (or const) array of size n
! 
! * If mode = 'i' ('increase'), (we want an array with a size at least equal to n)
!
!   . if size(t) >= n: (there is enough space in t)
!            t is unchanged
!   . else 
!            t is de-allocated and re-allocated to the zero (or const) array of size n
!
! * If mode = 'z' ('zero') (we want an array of zeros (or const) with a size at least equal to n)
!
!   . if size(t) >= n: (there is enough space in t)
!            t is reset to 0
!   . else 
!            t is de-allocated and re-allocated to the zero (or const) array of size n
!
!  Inputs
!
!   mode: 's', 'e', 'i' or 'z'
!      n: the (new) size of t
!      t: array of integers to be allocated or re-allocated with the new size n
!
!---------------------------------R.H. 12/16, 12/17 (use of move_alloc), 09/21 ('i','z' modes)
 
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocIk1'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, constant, err
   integer  (Ikind), allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 
   
   if ( present(const) ) then ; constant = const ; else ; constant = IZERO ; end if   
   if ( present(reallocated) ) reallocated = .false.
      
#include "include/util_allock1.inc"  
      
   END SUBROUTINE util_allocIk1


!=============================================================================================
   SUBROUTINE util_allocIk2 ( mode, t, n, m, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode
   integer  (Ikind),              intent(in    ) :: n, m
   integer  (Ikind), allocatable, intent(in out) :: t(:,:)
   type     (err_t),              intent(in out) :: stat     
   integer  (Ikind), optional   , intent(in    ) :: const    
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-2 integer array t. It is specially useful   
!  when t is already allocated but needs to be extended.
! 
! * When t is not allocated, it is simply allocated to the zero array of shape n x m whatever
!   the value of 'mode' is. Otherwise:
!   
! * If mode = 's' ('save') (we want an array of size n x m formed by the first entries of t)
!
!   t is first saved in a temporary array, de-allocated and re-allocated to the new shape. 
!   The saved entries are then copied back to t.
!
!   Warning: if one of the new sizes (n or m) is less than its old value, the part of t that
!            is located beyond this size will be definitively lost. 
!
! * If mode = 'a' ('allocate'), (we want an array of shape n x m)
!
!   . if size(t) == [n,m]:  (t has the required shape)
!            t is unchanged
!   . else 
!            t is de-allocated and re-allocated to the zero n x m array
!
! * If mode = 'e' ('erase'), (we want an array of shape n x m initialized to 0)
!
!   . if size(t) == [n,m]:  (t has the required shape)
!            t is simply reset to 0
!   . else 
!            t is de-allocated and re-allocated to the zero n x m array
! 
! * If mode = 'i' ('increase'), (we want an array with sizes at least equal to n and m)
!
!   . if size(t,1) >= n & size(t,2) >= m (there is enough space in t)
!            t is unchanged
!   . else 
!            t is de-allocated and re-allocated to the zero n x m array
!
! * If mode = 'z' ('zero') (we want an array of zeros with sizes at least equal to n and m)
!
!   . if size(t,1) >= n & size(t,2) >= m (there is enough space in t)
!            t is reset to 0
!   . else 
!            t is de-allocated and re-allocated to the zero n x m array
!
!  Inputs
!
!   mode: 'a', 's', 'e', 'i' or 'z'
!      n: the (new) size of t
!      t: array of integers to be allocated or re-allocated with the new size n
!
!---------------------------------R.H. 12/16, 12/17 (use of move_alloc), 09/21 ('i','z' modes)
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocIk2'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, mm, err
   integer  (Ikind)              :: constant
   integer  (Ikind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 
   
   if ( present(const) ) then ; constant = const ; else ; constant = IZERO ; end if   
   if ( present(reallocated) ) reallocated = .false.
   
#include "include/util_allock2.inc"  
      
   END SUBROUTINE util_allocIk2


!=============================================================================================
   SUBROUTINE util_allocRk1 ( mode, t, n, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode   
   integer  (Ikind),              intent(in    ) :: n
   real     (Rkind), allocatable, intent(in out) :: t(:)
   type     (err_t),              intent(in out) :: stat
   real     (Rkind), optional   , intent(in    ) :: const 
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-1 real array t. It is specially useful   
!  when t is already allocated but needs to be extended.
!
!  See the description of util_allocIk1
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocRk1'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, err
   real     (Rkind)              :: constant
   real     (Rkind), allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 

   if ( present(const) ) then ; constant = const ; else ; constant = RZERO ; end if   
   if ( present(reallocated) ) reallocated = .false.
   
#include "include/util_allock1.inc"  

   END SUBROUTINE util_allocRk1


!=============================================================================================
   SUBROUTINE util_allocRk2 ( mode, t, n, m, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode      
   integer  (Ikind),              intent(in    ) :: n, m
   real     (Rkind), allocatable, intent(in out) :: t(:,:)
   type     (err_t),              intent(in out) :: stat
   real     (Rkind), optional   , intent(in    ) :: const   
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-2 real array t. It is specially useful   
!  when t is already allocated but needs to be extended.
!
!  See the description of util_allocIk2
!---------------------------------------------------------------------------------------------
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocRk2'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, mm, err
   real     (Rkind)              :: constant
   real     (Rkind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( present(const) ) then ; constant = const ; else ; constant = RZERO ; end if   
   if ( present(reallocated) ) reallocated = .false.
   
#include "include/util_allock2.inc"     
      
   END SUBROUTINE util_allocRk2


!=============================================================================================
   SUBROUTINE util_allocCk1 ( mode, t, n, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode   
   integer  (Ikind),              intent(in    ) :: n
   complex  (Rkind), allocatable, intent(in out) :: t(:)
   type     (err_t),              intent(in out) :: stat
   complex  (Rkind), optional   , intent(in    ) :: const
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-1 complex array t. It is specially useful   
!  when t is already allocated but needs to be extended.
!
!  See the description of util_allocIk1
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocCk1'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, err
   complex  (Rkind)              :: constant
   complex  (Rkind), allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 

   if ( present(const) ) then ; constant = const ; else ; constant = CZERO ; end if   
   if ( present(reallocated) ) reallocated = .false.
    
#include "include/util_allock1.inc"  

   END SUBROUTINE util_allocCk1


!=============================================================================================
   SUBROUTINE util_allocCk2 ( mode, t, n, m, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode      
   integer  (Ikind),              intent(in    ) :: n, m
   complex  (Rkind), allocatable, intent(in out) :: t(:,:)
   type     (err_t),              intent(in out) :: stat
   complex  (Rkind), optional   , intent(in    ) :: const   
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-2 complex array t. It is specially useful   
!  when t is already allocated but needs to be extended.
!
!  See the description of util_allocIk2
!---------------------------------------------------------------------------------------------
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocCk2'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, mm, err
   complex  (Rkind)              :: constant
   complex  (Rkind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 
   
   if ( present(const) ) then ; constant = const ; else ; constant = CZERO ; end if   
   if ( present(reallocated) ) reallocated = .false.
   
#include "include/util_allock2.inc"     
      
   END SUBROUTINE util_allocCk2


!=============================================================================================
   SUBROUTINE util_allocLk1 ( mode, t, n, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode   
   integer  (Ikind),              intent(in    ) :: n
   logical         , allocatable, intent(in out) :: t(:)
   type     (err_t),              intent(in out) :: stat
   logical         , optional   , intent(in    ) :: const
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-1 logical array t. It is specially useful   
!  when t is already allocated but needs to be extended.
!
!  See the description of util_allocIk1
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocLk1'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, err
   logical                       :: constant
   logical         , allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 

   if ( present(const) ) then ; constant = const ; else ; constant = LZERO ; end if   
   if ( present(reallocated) ) reallocated = .false.
   
#include "include/util_allock1.inc"  
      
   END SUBROUTINE util_allocLk1


!=============================================================================================
   SUBROUTINE util_allocLk2 ( mode, t, n, m, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode      
   integer  (Ikind),              intent(in    ) :: n, m
   logical         , allocatable, intent(in out) :: t(:,:)
   type     (err_t),              intent(in out) :: stat
   logical         , optional   , intent(in    ) :: const
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-2 logical array t. It is specially useful   
!  when t is already allocated but needs to be extended.
!
!  See the description of util_allocIk2
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocLk2'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, mm, err
   logical                       :: constant
   logical         , allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( present(const) ) then ; constant = const ; else ; constant = LZERO ; end if   
   if ( present(reallocated) ) reallocated = .false.
   
#include "include/util_allock2.inc"     
      
   END SUBROUTINE util_allocLk2


!=============================================================================================
   SUBROUTINE util_allocSk1 ( mode, t, n, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode   
   integer  (Ikind),              intent(in    ) :: n
   type     (str_t), allocatable, intent(in out) :: t(:)
   type     (err_t),              intent(in out) :: stat   
   character(len=*), optional   , intent(in    ) :: const
   logical         , optional   , intent(   out) :: reallocated
!---------------------------------------------------------------------------------------------
!  This procedure allocates or re-allocates the rank-1 string array t. It is specially useful   
!  when t is already allocated but needs to be extended.
!
!  See the description of util_allocIk1
!---------------------------------------------------------------------------------------------
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocSk1'
   character(len=:), allocatable :: msg, constant
   integer  (Ikind)              :: i, nn, err
   type     (str_t), allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 

   err = 0

   if ( present(const) ) then ; constant = const ; else ; constant = '' ; end if   
   if ( present(reallocated) ) reallocated = .false.
   
   if ( .not. allocated(t) ) then
   
      allocate(t(n), stat=err)
      if ( err /= 0 ) then
         msg = "Allocation failure for array 't' [1]"  
      else
         do i = 1, n ; t(i)%str = constant ; end do
      end if

   else if ( mode == 'a' ) then

      if ( size(t) /= n ) then
         deallocate(t)
         allocate(t(n), stat=err)
         if ( err /= 0 ) then
            msg = "Allocation failure for array 't' [2]"  
         else      
            do i = 1, n ; t(i)%str = constant ; end do
         end if         
      else
         return
      end if
      
   else if ( mode == 'e' ) then

      if ( size(t) /= n ) then
         deallocate(t)
         allocate(t(n), stat=err)
         if ( err /= 0 ) then
            msg = "Allocation failure for array 't' [3]"  
         else      
            do i = 1, n ; t(i)%str = constant ; end do
         end if         
      else
         do i = 1, n ; t(i)%str = constant ; end do
         return
      end if
  
   else if ( mode == 's' ) then

      allocate(tmp(n), stat=err)
      if ( err /= 0 ) then
         msg = " Allocation failure for array 'tmp'"
      else          
         do i = 1, n ; tmp(i)%str = constant ; end do
         nn = min(n,size(t,kind=Ikind))
         tmp(1:nn) = t(1:nn)
         call move_alloc (from = tmp, to = t)
      end if

   else if ( mode == 'i' ) then
      if ( size(t) < n ) then
         deallocate(t)
         allocate(t(n), stat=err)
         if ( err /= 0 ) then
            msg = "Allocation failure for array 't' [4]"  
         else
            do i = 1, n ; t(i)%str = constant ; end do
         end if
      else
         return
      end if  

   else if ( mode == 'z' ) then
      if ( size(t) < n ) then
         deallocate(t)
         allocate(t(n), stat=err)
         if ( err /= 0 ) then
            msg = "Allocation failure for array 't' [5]"  
         else
            do i = 1, n ; t(i)%str = constant ; end do
         end if
      else
         do i = 1, size(t) ; t(i)%str = constant ; end do
         return
      end if
      
   else
      call stat%set ( UERROR, HERE,                                                       &
                      "Invalid mode '" // mode // "' (must be 'a', 'e', 's', 'i' or 'z')" )
      return
   end if          

   if ( err /= 0 ) then      
      call stat%set (IERROR, HERE, msg) ; return
   end if           
   
   if ( present(reallocated) ) reallocated = .true.
      
   END SUBROUTINE util_allocSk1


!=============================================================================================
   SUBROUTINE util_allocSk2 ( mode, t, n, m, stat, const, reallocated )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode      
   integer  (Ikind),              intent(in    ) :: n, m
   type     (str_t), allocatable, intent(in out) :: t(:,:)
   type     (err_t),              intent(in out) :: stat   
   character(len=*), optional   , intent(in    ) :: const
   logical         , optional   , intent(   out) :: reallocated
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the rank-2 string array t. It is specially useful   
!  when t is already allocated but needs to be extended.
!
!  See the description of util_allocIk2
!---------------------------------------------------------------------------------------------
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_allocSk2'
   character(len=:), allocatable :: msg, constant
   integer  (Ikind)              :: i, j, nn, mm, err
   type     (str_t), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   err = 0
   
   if ( present(const) ) then ; constant = const ; else ; constant = '' ; end if   
   if ( present(reallocated) ) reallocated = .false.
            
   if (.not. allocated(t)) then
      
      allocate(t(n,m), stat=err)
      if ( err /= 0 ) then
         msg = "Allocation failure for array 't' [1]"  
      else
         do j = 1, m ; do i = 1, n ; t(i,j)%str = constant ; end do ; end do
      end if

   else if ( mode == 'a' ) then

      if ( size(t,dim=1) /= n .or. size(t,dim=2) /= m ) then 
         deallocate(t)
         allocate(t(n,m), stat=err)      
         if ( err /= 0 ) then
            msg = "Allocation failure for array 't' [2]"  
         else
            do j = 1, m ; do i = 1, n ; t(i,j)%str = constant ; end do ; end do
         endif
      else
         return
      end if
            
   else if ( mode == 'e' ) then

      if ( size(t,dim=1) /= n .or. size(t,dim=2) /= m ) then 
         deallocate(t)
         allocate(t(n,m), stat=err)      
         if ( err /= 0 ) then
         msg = "Allocation failure for array 't' [3]"  
         else
            do j = 1, m ; do i = 1, n ; t(i,j)%str = constant ; end do ; end do
         endif
      else
         do j = 1, m ; do i = 1, n ; t(i,j)%str = constant ; end do ; end do
         return
      end if       
         
   else if ( mode == 's' ) then   
   
      allocate(tmp(n,m), stat=err)
      
      if ( err /= 0 ) then
         msg = " Allocation failure for array 'tmp'"
      else          
         do j = 1, m ; do i = 1, n ; tmp(i,j)%str = constant ; end do ; end do
         nn = min(n,size(t,1,kind=Ikind))
         mm = min(m,size(t,2,kind=Ikind))                                               
         tmp(1:nn,1:mm) = t(1:nn,1:mm)
         call move_alloc (from = tmp, to = t)
      end if      
      
   else if ( mode == 'i' ) then
         
      if ( size(t,1) < n .or. size(t,2) < m ) then
         deallocate(t)
         allocate(t(n,m), stat=err)
         if ( err /= 0 ) then
         msg = "Allocation failure for array 't' [4]"  
         else
            do j = 1, m ; do i = 1, n ; t(i,j)%str = constant ; end do ; end do
         end if
      else
         return
      end if  

   else if ( mode == 'z' ) then
   
      if ( size(t,1) < n .or. size(t,2) < m ) then
         deallocate(t)
         allocate(t(n,m), stat=err)
         if ( err /= 0 ) then
         msg = "Allocation failure for array 't' [5]"  
         else
            do j = 1, m ; do i = 1, n ; t(i,j)%str = constant ; end do ; end do
         end if
      else
         do j = 1, size(t,2) ; do i = 1, size(t,1) ; t(i,j)%str = constant ; end do ; end do
         return
      end if
      
   else
      call stat%set ( UERROR, HERE,                                                        &
                       "Invalid mode '" // mode // "' (must be 'a', 'e', 's', 'i' or 'z')" )
      return
   end if          

   if ( err /= 0 ) then      
      call stat%set (IERROR, HERE, msg) ; return
   end if       

   if ( present(reallocated) ) reallocated = .true. 
            
   END SUBROUTINE util_allocSk2


!=============================================================================================
   pure FUNCTION util_isIncluded ( list1, list2 ) result ( is_included )
!=============================================================================================
   integer(Ikind), intent(in) :: list1(:), list2(:) 
   logical                    :: is_included
!---------------------------------------------------------------------------------------------
!  Returns .true. if the elements of list1 are all present in the list2
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------

   do i = 1, size(list1)
      if ( .not. any(list2 == list1(i)) ) then
         is_included = .false.
         return
      end if
   end do

   is_included = .true.
   
   END FUNCTION util_isIncluded    


!=============================================================================================
   pure SUBROUTINE util_removeDuplicate ( list, newList, newSize ) 
!=============================================================================================
   integer(Ikind), intent(in    ) :: list(:)
   integer(Ikind), intent(in out) :: newList(size(list))
   integer(Ikind), intent(   out) :: newSize
!---------------------------------------------------------------------------------------------
!  Removes duplicate entries in "list". The result is stored in newList(1:newSize).
!
!  Source: https://rosettacode.org/wiki/Remove_duplicate_elements#Fortran
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------

   if ( size(list) < 1 ) then
      newSize = 0 ; return
   end if
   
   newSize = 1 ;  newList(newSize) = list(newSize)
   
   do i = 2, size(list)
      if ( any( newList(1:newSize) == list(i) ) ) cycle
      newSize = newSize + 1
      newList(newSize) = list(i)
   end do

   END SUBROUTINE util_removeDuplicate    
   
   
!=============================================================================================
   FUNCTION util_FindPosInUnsortedListIntg ( list, ideb, ifin, k ) result ( pos )
!=============================================================================================
   integer(Ikind), intent(in) :: ideb, ifin, k, list(*)
   integer(Ikind)             :: pos
!---------------------------------------------------------------------------------------------
!  Finds the absolute position of the integer "k" in the unordered array "list": 
!
!         list(pos) = k
!
!  Search is done for ideb <= pos <= ifin.
!
!  Returns 0 if k is not present in the sub-array list(ideb:ifin)
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------

!
!- brute force
!
   pos = 0
   do i = ideb, ifin
      if ( list(i) == k ) then
         pos = i
         exit
      end if
   end do

   END FUNCTION util_FindPosInUnsortedListIntg    
   

!=============================================================================================
   FUNCTION util_FindPosInUnsortedListStr ( list, ideb, ifin, str, is_caseIns ) result ( pos )
!=============================================================================================
   type     (str_t),           intent(in) :: list(*)
   integer  (Ikind),           intent(in) :: ideb, ifin
   character(len=*),           intent(in) :: str
   logical         , optional, intent(in) :: is_caseIns
   integer  (Ikind)                       :: pos
!---------------------------------------------------------------------------------------------
!  Finds the absolute position of the "str" in the unordered array "list" of str_t: 
!
!            list(pos)%str = str.
!
!  Search is done for ideb <= pos <= ifin.
!
!  If "is_caseIns" is present and .true. the result is case insensitive. 
!
!  Returns 0 if str is not present in the sub-array list(ideb:ifin)
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   integer(Ikind) :: i
   logical        :: is_insens
!---------------------------------------------------------------------------------------------

   if ( present(is_caseIns) ) then
      is_insens = is_caseIns
   else
      is_insens = .false.
   end if
   
   pos = 0
   
   if ( is_insens ) then
      do i = ideb, ifin
         if ( allocated(list(i)%str) ) then
            if ( util_StringLow(trim(adjustl(str))) == &
                 util_StringLow(trim(adjustl(list(i)%str))) ) then
               pos = i
               exit
            end if   
         end if   
      end do
   else
      do i = ideb, ifin
         if ( allocated(list(i)%str) ) then
            if ( trim(adjustl(str)) == trim(adjustl(list(i)%str)) ) then
               pos = i
               exit
            end if   
         end if   
      end do
   end if

   END FUNCTION util_FindPosInUnsortedListStr    


!=============================================================================================
   FUNCTION util_FindPosInUnsortedListChar ( list, ideb, ifin, str, is_caseIns ) result ( pos )
!=============================================================================================
   character(len=*),           intent(in) :: list(*)
   integer  (Ikind),           intent(in) :: ideb, ifin
   character(len=*),           intent(in) :: str
   logical         , optional, intent(in) :: is_caseIns
   integer  (Ikind)                       :: pos
!---------------------------------------------------------------------------------------------
!  Finds the absolute position of the "str" in the unordered array "list" of character: 
!
!            list(pos)%str = str.
!
!  Search is done for ideb <= pos <= ifin.
!
!  If "is_caseIns" is present and .true. the result is case insensitive. 
!
!  Returns 0 if str is not present in the sub-array list(ideb:ifin)
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   integer(Ikind) :: i
   logical        :: is_insens
!---------------------------------------------------------------------------------------------

   if ( present(is_caseIns) ) then
      is_insens = is_caseIns
   else
      is_insens = .false.
   end if
   
   pos = 0
   
   if ( is_insens ) then
      do i = ideb, ifin
         if ( util_StringLow(trim(adjustl(str))) == &
              util_StringLow(trim(adjustl(list(i)))) ) then
            pos = i
            exit
         end if   
      end do
   else
      do i = ideb, ifin
         if ( trim(adjustl(str)) == trim(adjustl(list(i))) ) then
            pos = i
            exit
         end if   
      end do
   end if

   END FUNCTION util_FindPosInUnsortedListChar
   
   
!=============================================================================================
   RECURSIVE FUNCTION util_FindPosInSortedList ( list, ideb, ifin, k ) result ( pos )
!=============================================================================================
   integer(Ikind), intent(in) :: ideb, ifin, k, list(*)
   integer(Ikind)             :: pos
!---------------------------------------------------------------------------------------------
!  Finds the absolute position of the integer k in the array list, assumed sorted in ascending 
!  order: find pos st list(pos) = k.
!
!  Search is done for ideb <= pos <= ifin.
!
!  Returns 0 if k is not present in the sub-array list(ideb:ifin)
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   integer (Ikind) :: i1, i2, mil
!---------------------------------------------------------------------------------------------

!
!- sorted list: proceed by dichotomy
!
   pos = 0   ; if ( k  > list(ifin) .or. k < list(ideb) ) return  
   pos = ideb; if ( k == list(ideb)                     ) return  
   pos = ifin; if ( k == list(ifin)                     ) return
   pos = 0   ; if ( ifin - ideb <= 1                    ) return
        
   mil = (ideb + ifin)/2; 
 
   pos = mil ; if ( k == list(mil)                      ) return

   if ( k < list(mil) ) then
      i1 = ideb; i2 = mil
   else
      i1 = mil; i2 = ifin
   end if      
        
   pos = util_FindPosInSortedList ( list, i1, i2, k )
    
   END FUNCTION util_FindPosInSortedList    

   
!=============================================================================================
   FUNCTION util_wtime ( t0 ) result ( t )
!=============================================================================================
   real(Rkind), intent(in), optional :: t0
   real(Rkind)                       :: t
!---------------------------------------------------------------------------------------------
!  Computes the elapsed wall clock time
!  Usage:
!       real(Rkind) :: t0
!       t0 = util_wtime()
!       .... (part of the code to be timed) ....
!       print*,'Elapsed time: ',util_wtime(t0),' seconds'
!---------------------------------------------------------------------------------- R.H. 12/21

!- local variables: --------------------------------------------------------------------------
   integer(i64) :: nPeriode, nPeriode_sec, nPeriode_max
!---------------------------------------------------------------------------------------------
   
   if ( present(t0) ) then
      call system_clock ( count=nPeriode, count_rate=nperiode_sec, count_max=nPeriode_max )
      t = real(nPeriode,Rkind) / real(nperiode_sec,Rkind) - t0
      if ( t < RZERO ) t = t  + real(nPeriode_max,Rkind) / real(nperiode_sec,Rkind)
   else
      call system_clock ( count=nPeriode, count_rate=nperiode_sec)
      t = real(nPeriode,Rkind) / real(nperiode_sec,Rkind)
   end if

   
   END FUNCTION util_wtime
   
   
!=============================================================================================
   SUBROUTINE util_TimeStamp ( funit )
!=============================================================================================
   integer(Ikind), intent(in) :: funit
!---------------------------------------------------------------------------------------------                   
!  TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!   Example:
!
!     31 May 2001   9:45:54.872 AM
!
!    Licensing:
!
!     This code is distributed under the GNU LGPL license.
!
!    Modified:
!
!     18 May 2013
!
!    Author:
!
!     John Burkardt
!
!    Parameters:
!
!     None
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   character(len=8 ) :: ampm
   integer  (Ikind ) :: d, h, m, mm, n, s, y
   integer           :: values(8)
  
   character(len=9), parameter, dimension(12) :: month =   (/     &
        'January  ', 'February ', 'March    ', 'April    ',       &
        'May      ', 'June     ', 'July     ', 'August   ',       &
        'September', 'October  ', 'November ', 'December ' /)  
!---------------------------------------------------------------------------------------------                   

   call date_and_time ( values = values )

   y  = values(1); m  = values(2); d  = values(3); h  = values(5)
   n  = values(6); s  = values(7); mm = values(8)

   if ( h < 12 ) then
      ampm = 'AM'
   else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
         ampm = 'Noon'
      else
         ampm = 'PM'
      end if
   else
      h = h - 12
      if ( h < 12 ) then
         ampm = 'PM'
      else if ( h == 12 ) then
         if ( n == 0 .and. s == 0 ) then
            ampm = 'Midnight'
         else
            ampm = 'AM'
         end if
      end if
   end if

   write ( funit, '(3x,i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
          d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

   return
  
   END SUBROUTINE util_TimeStamp
   

!=============================================================================================
   FUNCTION util_GetUnit () result ( iunit )
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only:  input_unit, output_unit, error_unit
   integer(Ikind) :: iunit  
!---------------------------------------------------------------------------------------------                   
!   GET_UNIT returns a free FORTRAN unit number.
!
!   Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!   Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!   Modified:
!
!    18 September 2005
!
!   Author:
!
!    John Burkardt
!
!   Parameters:
!
!    Output, integer, the free unit number.
!---------------------------------------------------------------------------------------------                   


!- local variables: --------------------------------------------------------------------------
   integer(Ikind)       :: i,ios
   logical              :: lopen
!---------------------------------------------------------------------------------------------                   
 
   iunit = 0

   do i = 1, 99

      !if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then
      if ( i /= input_unit .and. i /= output_unit .and. i /= error_unit ) then

         inquire ( unit = i, opened = lopen, iostat = ios )

         if ( ios == 0 ) then
            if ( .not. lopen ) then
               iunit = i
               return
            end if
         end if

      end if

   end do

   return
  
   END FUNCTION util_GetUnit   


!=============================================================================================    
   SUBROUTINE util_magic ( n, mat )  
!=============================================================================================       
   integer(Ikind), intent(in    ) :: n
   integer(Ikind), intent(in out) :: mat(:,:)
!---------------------------------------------------------------------------------------------   
!  Creates a magic square of order n
!
!     Recopied and modified from 
!           Scilab (http://www.scilab.org/)
!     Algorithms for magic squares taken from
!           mathematical recreations and essays, 12th ed.,
!           by W. W. Rouse Ball and H. S. M. Coxeter
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   integer(Ikind) :: m, mm, i, j, k, i1, j1, t, im, jm, m1, m2
   integer(Ikind) :: v(n/2)
!---------------------------------------------------------------------------------------------   
   
   mat = 0
      
   if ( mod(n,4_Ikind) == 0 ) then
!
!-    double even order:
!
      k = 1
      do i = 1, n
         do j = 1, n
            mat(i,j) = k
            if ( mod(i,4_Ikind)/2_Ikind == mod(j,4_Ikind)/2_Ikind ) mat(i,j) = n*n+1 - k
            k = k+1
         end do
      end do
      
      return
      
   end if
         
   if ( mod(n,2_Ikind) == 0 ) m = n/2_Ikind
   if ( mod(n,2_Ikind) /= 0 ) m = n
!
!- odd order or upper corner of even order
!
   
   i = 1
   j = (m+1)/2
   mm = m*m
   do k = 1, mm
      mat(i,j) = k
      i1 = i-1 ; j1 = j+1
      if ( i1 < 1 ) i1 = m
      if ( j1 > m ) j1 = 1
      if ( mat(i1,j1) /= 0) then
         i1 = i+1 ;  j1 = j
      end if   
      i = i1 ; j = j1
   end do
         
   if ( mod(n,2_Ikind) /= 0 ) return
     
!
!- rest of even order
!
   t = m*m
   do i = 1, m
      do j = 1, m
         im = i+m ; jm = j+m
         mat(i ,jm) = mat(i,j) + 2*t
         mat(im,j ) = mat(i,j) + 3*t
         mat(im,jm) = mat(i,j) + t
      end do
   end do

   m1 = (m-1)/2
   if ( m1 == 0 ) return
 
   do j = 1, m1
      v = mat(1:m,j) ; mat(1:m,j) = mat(m+1:n,j) ; mat(m+1:n,j) = v
   end do
   
   m1 = (m+1)/2 ; m2 = m1 + m
   
   i = mat(m1,1 ) ; mat(m1,1 ) = mat(m2,1 ) ; mat(m2,1 ) = i
   i = mat(m1,m1) ; mat(m1,m1) = mat(m2,m1) ; mat(m2,m1) = i
   
   m1 = n+1-(m-3)/2
   
   if ( m1 > n ) return
   
   do j = m1, n
      v = mat(1:m,j) ; mat(1:m,j) = mat(m+1:n,j) ; mat(m+1:n,j) = v
   end do
      
   END SUBROUTINE util_magic


!=============================================================================================   
   FUNCTION util_MatrixNorm ( norm, stat, Imat, Rmat, Cmat ) result(res)
!=============================================================================================   
   character(len=1),           intent(in    ) :: norm   
   type     (err_t), optional, intent(in out) :: stat
   integer  (Ikind), optional, intent(in    ) :: Imat(:,:)
   real     (Rkind), optional, intent(in    ) :: Rmat(:,:)
   complex  (Rkind), optional, intent(in    ) :: Cmat(:,:)
   real     (Rkind)                           :: res
!---------------------------------------------------------------------------------------------
!  Computes the matricial norm of Imat, Rmat or Cmat
!
!  Possible choices:
!
!     . norm = 'm': computes the largest absolute value (max(abs(a(i,j)))   
!     . norm = '1': computes the 1 norme (maximum column sum)
!     . norm = 'i': computes the infinity norm (maximum row sum)
!     . norm = 'f': computes the Frobenius's norm (square root of sum of squares)  
!
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'util_MatrixNorm'
   integer  (Ikind)            :: typ
!---------------------------------------------------------------------------------------------

   res = 0.0

   if      ( present(Imat) ) then
      typ = 1
   else if ( present(Rmat) ) then
      typ = 2
   else if ( present(Cmat) ) then
      typ = 3
   else
      stat = err_t (UERROR, HERE, 'Missing the matrix!')
      return
   end if
   
   select case (norm)
      case ('m')
         select case (typ)
            case (1)
               res = maxval( abs(Imat) )
            case (2)
               res = maxval( abs(Rmat) )
            case (3)   
               res = maxval( abs(Cmat) )
         end select
         
      case ('1')
         select case (typ)
            case (1)
               if ( size(Imat,1) == 1 .or. size(Imat) == 1 ) then
                  res = sum( abs(real(Imat,kind=Rkind)) )
               else
                  res = maxval( sum(abs(real(Imat,kind=Rkind)),1) )
               end if   
            case (2)
               if ( size(Rmat,1) == 1 .or. size(Rmat) == 1 ) then
                  res = sum( abs(Rmat) )
               else
                  res = maxval( sum(abs(Rmat),1) )
               end if   
            case (3)
               if ( size(Cmat,1) == 1 .or. size(Cmat) == 1 ) then
                  res = sum( abs(Cmat) )
               else
                  res = maxval( sum(abs(Cmat),1) )
               end if   
         end select
               
      case ('i') 
         select case (typ)
            case (1)
               if ( size(Imat,1) == 1 .or. size(Imat) == 1 ) then
                  res = maxval( abs(Imat) )
               else         
                  res = maxval( sum( abs(Imat), 2 ) )
               end if   
            case (2)
               if ( size(Rmat,1) == 1 .or. size(Rmat) == 1 ) then
                  res = maxval( abs(Rmat) )
               else         
                  res = maxval( sum( abs(Rmat), 2 ) )
               end if   
            case (3)
               if ( size(Cmat,1) == 1 .or. size(Cmat) == 1 ) then
                  res = maxval( abs(Cmat) )
               else         
                  res = maxval( sum( abs(Cmat), 2 ) )
               end if   
         end select
         
      case ('f') 
         select case (typ)
            case (1)
               res = sqrt( sum( (real(Imat,kind=Rkind))**2 ) )
            case (2)
               res = sqrt( sum( (Rmat)**2 ) )
            case (3)
               res = sqrt( sum( (abs(Cmat))**2 ) ) 

         end select
         
      case default
         call stat%set (UERROR, HERE, "Unknown norm ('" // norm // "')")
         return
   end select
   
   END FUNCTION util_MatrixNorm


!=============================================================================================   
   pure FUNCTION util_IsReal ( z ) result ( is_real ) 
!=============================================================================================   
   complex(Rkind), intent(in) :: z(..)
   logical                    :: is_real
!---------------------------------------------------------------------------------------------    
!----------------------------------------------------------------------------R.H. 04/18, 08/24       

   select rank ( z )
      rank ( 0 )
         is_real = ( aimag(z) == RZERO )
      rank ( 1 )
         is_real = ( .not. any( aimag(z) /= RZERO ) )
      rank ( 2 )
         is_real = ( .not. any( aimag(z) /= RZERO ) )
   end select
   
   END FUNCTION util_IsReal
   

!=============================================================================================   
   pure FUNCTION util_IsIntgR ( x ) result ( is_intg )
!=============================================================================================   
   real   (Rkind), intent(in) :: x(..)
   logical                    :: is_intg
!---------------------------------------------------------------------------------------------
!  Returns true if the elements of x are whole numbers (and don't overflow)
!----------------------------------------------------------------------------R.H. 04/18, 08/24       

   select rank ( x )
      rank ( 0 )
         is_intg = ( anint(x) == x .and. abs(x) <= real(huge(Ikind)-1,Rkind) )
      rank ( 1 )
         is_intg = ( .not. any( anint(x) /=x ) .and. &
                     .not. any( abs(x) > real(huge(Ikind)-1,Rkind) ) )
      rank ( 2 )
         is_intg = ( .not. any( anint(x) /=x ) .and. &
                     .not. any( abs(x) > real(huge(Ikind)-1,Rkind) ) )
   end select      
   
   END FUNCTION util_IsIntgR
   
   
!=============================================================================================   
   pure FUNCTION util_IsIntgC ( z ) result ( is_intg ) 
!=============================================================================================   
   complex(Rkind), intent(in) :: z(..)
   logical                    :: is_intg
!---------------------------------------------------------------------------------------------
!  Returns true if the elements of x are whole numbers
!----------------------------------------------------------------------------R.H. 04/18, 08/24       

   select rank ( z )
      rank ( 0 )
         if ( aimag(z) /= RZERO ) then
            is_intg = .false.
         else
            is_intg = util_IsIntgR ( real(z,Rkind) )
         end if
      rank ( 1 ) 
         if ( any( aimag(z) /= RZERO ) ) then
            is_intg = .false.
         else
            is_intg = util_IsIntgR ( real(z,Rkind) )
         end if
      rank ( 2 ) 
         if ( any( aimag(z) /= RZERO ) ) then
            is_intg = .false.
         else
            is_intg = util_IsIntgR ( real(z,Rkind) )
         end if
   end select  
   
   END FUNCTION util_IsIntgC
   

!=============================================================================================   
   SUBROUTINE util_SubRandomR ( n, m, Rmat, stat )
!=============================================================================================   
   integer  (Ikind),              intent(in    ) :: n, m
   real     (Rkind), allocatable, intent(in out) :: Rmat(:,:)   
   type     (err_t),              intent(in out) :: stat
!---------------------------------------------------------------------------------------------      
!  Returns a n x m matrix of uniformly distributed random numbers in the interval (0,1).
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   character(len=* ), parameter   :: HERE = 'util_SubRandomR'  
   integer                        :: time(8), nsize
   integer          , allocatable :: seed(:) 
   integer          , save        :: mem = 0 
!---------------------------------------------------------------------------------------------      

   if ( n > 0 .and. m > 0 ) then
      call util_alloc ( mode='a', t=Rmat, n=n, m=m, stat=stat )
      error_TraceNreturn(stat, HERE)

      call date_and_time(values=time)
      call random_seed(size=nsize) ; allocate(seed(1:nsize))
      mem = min(mem+1, 1000)      
      seed(1) = time(8)+mem ; seed(2:nsize) = time(1)*time(8)
      call random_seed(put=seed)
      call random_number(harvest=Rmat)

   else
      call util_alloc ( mode='a', t=Rmat, n=IZERO, m=IZERO, stat=stat )
      error_TraceNreturn(stat, HERE)
   
   end if
   
   END SUBROUTINE util_SubRandomR


!=============================================================================================   
   FUNCTION util_RandomR ( n, m, stat ) result ( Rmat )
!=============================================================================================   
   integer  (Ikind),              intent(in    ) :: n, m
   type     (err_t),              intent(in out) :: stat     
   real     (Rkind), allocatable                 :: Rmat(:,:)   
!---------------------------------------------------------------------------------------------      

!- local variables ---------------------------------------------------------------------------     
   character(len=* ), parameter :: HERE = 'util_Random'  
!---------------------------------------------------------------------------------------------      

   call util_SubRandomR ( n, m, Rmat, stat )
   error_TraceNreturn(stat, HERE)
      
   END FUNCTION util_RandomR
   

!=============================================================================================   
   SUBROUTINE util_SubRandomI ( imin, imax, n, m, Imat, stat )
!=============================================================================================   
   integer  (Ikind),              intent(in    ) :: imin, imax, n, m
   integer  (Ikind), allocatable, intent(in out) :: Imat(:,:)    
   type     (err_t),              intent(in out) :: stat  
!---------------------------------------------------------------------------------------------      
!  Returns a n x m matrix of random integer numbers in the interval [imin,imax]
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter   :: HERE = 'util_SubRandomI'  
   real     (Rkind), allocatable :: Rmat(:,:)
!---------------------------------------------------------------------------------------------      

   if ( n > 0 .and. m > 0 ) then
      call util_SubRandomR ( n, m, Rmat, stat )
      error_TraceNreturn(stat, HERE)
   
      call util_alloc ( mode='a', t=Imat, n=n, m=m, stat=stat )
      error_TraceNreturn(stat, HERE)      
      
      Imat = nint(Rmat * (imax-imin) + imin,kind=Ikind)
   else
      call util_alloc ( mode='a', t=Imat, n=IZERO, m=IZERO, stat=stat )
      error_TraceNreturn(stat, HERE)
   end if
               
   END SUBROUTINE util_SubRandomI


!=============================================================================================   
   FUNCTION util_RandomI ( imin, imax, n, m, stat ) result ( Imat )
!=============================================================================================   
   integer  (Ikind),              intent(in    ) :: imin, imax, n, m
   type     (err_t),              intent(in out) :: stat
   integer  (Ikind), allocatable                 :: Imat(:,:)      
!---------------------------------------------------------------------------------------------      

!- local variables ---------------------------------------------------------------------------     
   character(len=* ), parameter   :: HERE = 'util_RandomI'  
!---------------------------------------------------------------------------------------------      

   call util_SubRandomI ( imin, imax, n, m, Imat, stat )
   error_TraceNreturn(stat, HERE)
     
   END FUNCTION util_RandomI


!=============================================================================================   
   SUBROUTINE util_RandPerm ( n, k, perm, stat )
!=============================================================================================   
   integer  (Ikind),              intent(in    ) :: n
   integer  (Ikind), optional   , intent(in    ) :: k
   integer  (Ikind), allocatable, intent(in out) :: perm(:)
   type     (err_t),              intent(in out) :: stat
!---------------------------------------------------------------------------------------------      
!  Returns
!  . if k is not present: a vector containing a random permutation of {1,...,n} 
!  . if k is present; a sample of k unique integers of {1,...,n} 
!
!  Source: Rosetta Code (the  Knuth shuffle algorithm)
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'util_RandPerm'  
   integer  (Ikind)              :: i, j, t
   integer  (Ikind), allocatable :: tmp(:)
   real     (Rkind)              :: r
!---------------------------------------------------------------------------------------------      

   tmp = [(i,i=1,n)] 
!
!- Shuffle the set {1,2,...,n}:
!   
   do i = n, 2, -1
      call random_number(r)
      j = int(r * i) + 1
      t      = tmp(j)
      tmp(j) = tmp(i)
      tmp(i) = t
   end do      
   
   if ( present(k) ) then
!
!-    Random sampling of k out of n = the subset of the first k elements of the shuffled set
!     (not: if k <= 0 => the sample is empty, if k >=n => the sample is the whole set):
!
      j = min(k,n)
      
      call util_alloc ( mode='a', t=perm, n=j, stat=stat )
      error_TraceNreturn(stat, HERE)
      
      perm(1:j) = tmp(1:j)
      
   else
!
!-    Random permutation = shuffled set:
!   
      call move_alloc (from = tmp, to = perm)
   end if      
      
   END SUBROUTINE util_RandPerm  


!=============================================================================================
   SUBROUTINE util_SortIk1 ( d, ord, stat, indx )
!=============================================================================================
   integer  (Ikind),                        intent(in out) :: d(:)
   character(len=1),                        intent(in    ) :: ord
   type     (err_t),                        intent(in out) :: stat
   integer  (Ikind), allocatable, optional, intent(   out) :: indx(:)   
!--------------------------------------------------------------------------------------------- 
!  Sorts the integer rank-1 array "v" in increasing (ord='i') or in decreasing (ord='d') order
!
!  Inputs:
!  . v  : (modified) The array to sort.
!  . ord: 'i' (or 'I') for increasing order or 'd' (or 'D') for decreasing order.
!
!  Outputs:
!  . v   : The sorted array.
!  . indx: (optional) The original indices.
!
!  Source: this is a modified copy of LAPACK routine DLASRT adapted here for integer array
!          and where "indx" was added.
!-----------------------------------------------------------------------------------R.H. 10/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_SortIk1'
   integer  (Ikind), parameter   :: SELECT = 20
   integer  (Ikind)              :: dir, endd, i, j, n, start, stkpnt, stack(2,32), estat, &
                                    i1, i2, i3, imnmx, itmp
   integer  (Ikind)              :: d1, d2, d3, dmnmx, tmp
   integer  (Ikind), allocatable :: indxd(:)
!--------------------------------------------------------------------------------------------- 

   n = size(d)
   
#include "include/util_sort.inc" 

   if (present(indx)) call move_alloc (from = indxd, to = indx)
      
   END SUBROUTINE util_SortIk1


!=============================================================================================
   SUBROUTINE util_SortRk1 ( d, ord, stat, indx )
!=============================================================================================
   real     (Rkind),                        intent(in out) :: d(:)
   character(len=1),                        intent(in    ) :: ord
   type     (err_t),                        intent(in out) :: stat   
   integer  (Ikind), allocatable, optional, intent(   out) :: indx(:)
!--------------------------------------------------------------------------------------------- 
!  Sorts the real rank-1 array "v" in increasing (ord='i') or in decreasing (ord='d') order
!
!  Inputs:
!  . v  : (modified) The array to sort.
!  . ord: 'i' (or 'I') for increasing order or 'd' (or 'D') for decreasing order.
!
!  Outputs:
!  . v   : The sorted array.
!  . indx: (optional) The original indices.
!
!  Source: this is a modified copy of LAPACK routine DLASRT where "indx" was added.
!-----------------------------------------------------------------------------------R.H. 10/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_SortRk1'
   integer  (Ikind), parameter   :: SELECT = 20
   integer  (Ikind)              :: dir, endd, i, j, n, start, stkpnt, stack(2,32), estat, &
                                    i1, i2, i3, imnmx, itmp
   real     (Rkind)              :: d1, d2, d3, dmnmx, tmp
   integer  (Ikind), allocatable :: indxd(:)
!--------------------------------------------------------------------------------------------- 

   n = size(d)
   
#include "include/util_sort.inc" 

   if (present(indx)) call move_alloc (from = indxd, to = indx)
      
   END SUBROUTINE util_SortRk1


!=============================================================================================
   SUBROUTINE util_SortCk1 ( z, ord, stat, indx )
!=============================================================================================
   complex  (Rkind),                        intent(in out) :: z(:)
   character(len=1),                        intent(in    ) :: ord
   type     (err_t),                        intent(in out) :: stat   
   integer  (Ikind), allocatable, optional, intent(   out) :: indx(:)
!--------------------------------------------------------------------------------------------- 
!  Sorts the complex rank-1 array "v" in increasing (ord='i') or in decreasing (ord='d') 
!  magnitude order
!
!  Inputs:
!  . v  : (modified) The array to sort.
!  . ord: 'i' (or 'I') for increasing order or 'd' (or 'D') for decreasing order.
!
!  Outputs:
!  . v   : The sorted array.
!  . indx: (optional) The original indices.
!
!  Source: this is a modified copy of LAPACK routine DLASRT where "indx" was added.
!-----------------------------------------------------------------------------------R.H. 10/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_SortCk1'
   integer  (Ikind), parameter   :: SELECT = 20
   integer  (Ikind)              :: dir, endd, i, j, n, start, stkpnt, stack(2,32), estat, &
                                    i1, i2, i3, imnmx, itmp
   real     (Rkind)              :: d1, d2, d3, dmnmx, tmp
   real     (Rkind), allocatable :: d(:)
   integer  (Ikind), allocatable :: indxd(:)
!--------------------------------------------------------------------------------------------- 

   n = size(z)
   
   allocate(d(n)) ; d = abs(z)
   
#include "include/util_sort.inc" 

   z = z(indxd)
   
   if (present(indx)) call move_alloc (from = indxd, to = indx)
      
   END SUBROUTINE util_SortCk1


!=============================================================================================
   SUBROUTINE util_SortSk1 ( d, ord, is_CaseInsensitive, stat, indx )
!=============================================================================================
   type     (str_t),                        intent(in out) :: d(:)
   character(len=1),                        intent(in    ) :: ord
   logical         ,              optional, intent(in    ) :: is_CaseInsensitive
   type     (err_t),                        intent(in out) :: stat      
   integer  (Ikind), allocatable, optional, intent(   out) :: indx(:)
!--------------------------------------------------------------------------------------------- 
!  Sorts the string rank-1 array "v" in increasing (ord='i') or in decreasing order (ord='d')
!
!  Inputs:
!  . v                 : (modified) The array to sort.
!  . ord               : 'i' (or 'I') for increasing or 'd' (or 'D') for decreasing order.
!  . is_CaseInsensitive: (optional) .true. for case insensitive.
!
!  Outputs:
!  . v   : The sorted array.
!  . indx: (optional) The original indices.
!
!  Source: this is a modified copy of LAPACK routine DLASRT adapted here for string array
!          (DT str_t) and where "indx" was added.
!-----------------------------------------------------------------------------------R.H. 10/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_SortSk1'
   integer  (Ikind), parameter   :: SELECT = 20
   integer  (Ikind)              :: dir, endd, i, j, n, start, stkpnt, stack(2,32), estat, &
                                    i1, i2, i3, imnmx, itmp
   character(len=:), allocatable :: d1, d2, d3, dmnmx, tmp, d1l, d2l, d3l, dmnmxl
   logical                       :: is_ci
   integer  (Ikind), allocatable :: indxd(:)
!--------------------------------------------------------------------------------------------- 

   if ( present(is_CaseInsensitive) ) then
      is_ci = is_CaseInsensitive
   else
      is_ci = .false.
   end if   
!            
!- Test the input parameters.
!
   n = size(d)
         
   dir = -1
   d1 = util_stringlow ( ord )
   if      ( d1 == 'd' ) then
      dir = 0
   else if ( d1 == 'i' ) then
      dir = 1
   else
      call stat%set ( UERROR, HERE, "Invalid value '"// ord // &
                                    "' for << ord >> (must be 'i' or 'd' only)" )
      return
   end if

   allocate(indxd(n), stat = estat)   
   if ( estat /= 0 ) then
      call stat%set (IERROR, HERE, "Allocation failure for array 'indxd'")
      return
   end if        

   do i = 1, n
      if ( .not. allocated(d(i)%str) ) d(i)%str = ''
      indxd(i) = i
   end do   
!
!- Quick return if possible
!
   if ( n <= 1 ) then
      if ( present(indx) ) call move_alloc (from = indxd, to = indx)
      return
   end if
   
   stkpnt = 1
   stack( 1, 1 ) = 1
   stack( 2, 1 ) = n

10 continue
      start = stack( 1, stkpnt )
      endd  = stack( 2, stkpnt )
      stkpnt = stkpnt - 1
      if ( endd-start.le.select .and. endd-start.gt.0 ) then
!
!        do insertion sort on d( start:endd )
!
         if ( dir.eq.0 ) then
!
!           sort into decreasing order
!
            if ( is_ci ) then
               do 30 i = start + 1, endd
                  do 20 j = i, start + 1, -1
                     if ( lgt ( util_stringlow(d(j)%str),  util_stringlow(d(j-1)%str) ) ) then
                        dmnmx      = (d(j  )%str) ; imnmx      = indxd(j  )
                        d(j  )%str = (d(j-1)%str) ; indxd(j  ) = indxd(j-1)
                        d(j-1)%str = dmnmx        ; indxd(j-1) = imnmx
                     else
                        go to 30
                     end if
20                continue
30             continue
            else
               do 31 i = start + 1, endd
                  do 21 j = i, start + 1, -1
                     if ( lgt ( d(j)%str,  d(j-1)%str ) ) then
                        dmnmx      = (d(j  )%str) ; imnmx      = indxd(j  )
                        d(j  )%str = (d(j-1)%str) ; indxd(j  ) = indxd(j-1)
                        d(j-1)%str = dmnmx        ; indxd(j-1) = imnmx
                     else
                        go to 31
                     end if
21                continue
31             continue
            end if
            
         else
!
!           sort into increasing order
!
            if ( is_ci ) then
               do 50 i = start + 1, endd
                  do 40 j = i, start + 1, -1
                     if ( llt ( util_stringlow(d(j)%str),  util_stringlow(d(j-1)%str) ) ) then
                        dmnmx      = (d(j  )%str) ; imnmx      = indxd(j  )
                        d(j  )%str = (d(j-1)%str) ; indxd(j  ) = indxd(j-1)
                        d(j-1)%str = dmnmx        ; indxd(j-1) = imnmx
                     else
                        go to 50
                     end if
40                continue
50             continue
            else
               do 51 i = start + 1, endd
                  do 41 j = i, start + 1, -1
                     if ( llt ( d(j)%str,  d(j-1)%str ) ) then
                        dmnmx      = (d(j  )%str) ; imnmx      = indxd(j  )
                        d(j  )%str = (d(j-1)%str) ; indxd(j  ) = indxd(j-1)
                        d(j-1)%str = dmnmx        ; indxd(j-1) = imnmx
                     else
                        go to 51
                     end if
41                continue
51             continue
            end if
         end if

      else if ( endd-start.gt.select ) then
!
!        partition d( start:endd ) and stack parts, largest one first
!
!        choose partition entry as median of 3
!
         d1 = (d( start )%str) ; i1 = indxd(start)
         if ( is_ci ) then ; d1l = util_stringlow(d1) ; else ; d1l = d1 ; end if
         
         d2 = (d( endd  )%str) ; i2 = indxd(endd )
         if ( is_ci ) then ; d2l = util_stringlow(d2) ; else ; d2l = d2 ; end if
         
         i = ( start+endd ) / 2
         
         d3 = (d( i )%str) ; i3 = indxd(i)
         if ( is_ci ) then ; d3l = util_stringlow(d3) ; else ; d3l = d3 ; end if   
         
         if ( llt (d1l, d2l) ) then
            if ( llt (d3l, d1l) ) then
               dmnmx = d1 ; imnmx = i1
            else if( d3.lt.d2 ) then
               dmnmx = d3 ; imnmx = i3
            else
               dmnmx = d2 ; imnmx = i2
            end if
         else
            if ( llt (d3l, d2l) ) then
               dmnmx = d2 ; imnmx = i2
            else if ( llt (d3l, d1l) ) then
               dmnmx = d3 ; imnmx = i3
            else
               dmnmx = d1 ; imnmx = i1
            end if
         end if
         
         dmnmxl = util_stringlow ( dmnmx )

         if ( dir.eq.0 ) then
!
!           sort into decreasing order
!
            i = start - 1
            j = endd + 1
60          continue
            if ( is_ci ) then
70             continue
               j = j - 1
               if ( llt (util_stringlow(d(j)%str), dmnmxl) ) &
                  go to 70
80             continue
               i = i + 1
               if ( lgt (util_stringlow(d(i)%str), dmnmxl) ) &
                  go to 80
            else
71             continue
               j = j - 1
               if( llt (d(j)%str, dmnmx) ) &
                  go to 71
81             continue
               i = i + 1
               if( lgt (d(i)%str, dmnmx) ) &
                  go to 81
            end if
                              
            if ( i.lt.j ) then
               tmp        = d( i )%str ; itmp     = indxd(i)
               d( i )%str = d( j )%str ; indxd(i) = indxd(j)
               d( j )%str = tmp        ; indxd(j) = itmp
               go to 60
            end if
            if ( j-start.gt.endd-j-1 ) then
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = start
               stack( 2, stkpnt ) = j
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = j + 1
               stack( 2, stkpnt ) = endd
            else
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = j + 1
               stack( 2, stkpnt ) = endd
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = start
               stack( 2, stkpnt ) = j
            end if
         else
!
!           sort into increasing order
!
            i = start - 1
            j = endd + 1
90          continue

            if ( is_ci ) then
100            continue
               j = j - 1
               if ( lgt (util_stringlow(d(j)%str), dmnmxl) ) &
                  go to 100
110            continue
               i = i + 1
               if ( llt (util_stringlow(d(i)%str), dmnmxl) ) &
                  go to 110
            else
101            continue
               j = j - 1
               if ( lgt (d(j)%str, dmnmx) ) &
                  go to 101
111            continue
               i = i + 1
               if ( llt (d(i)%str, dmnmx) ) &
                  go to 111
            end if      
                              
            if ( i.lt.j ) then
               tmp        = d( i )%str ; itmp     = indxd(i)
               d( i )%str = d( j )%str ; indxd(i) = indxd(j)
               d( j )%str = tmp        ; indxd(j) = itmp
               go to 90
            end if
            if ( j-start.gt.endd-j-1 ) then
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = start
               stack( 2, stkpnt ) = j
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = j + 1
               stack( 2, stkpnt ) = endd
            else
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = j + 1
               stack( 2, stkpnt ) = endd
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = start
               stack( 2, stkpnt ) = j
            end if
         end if
      end if
      if ( stkpnt.gt.0 ) &
         go to 10

   if ( present(indx) ) call move_alloc (from = indxd, to = indx)

   END SUBROUTINE util_SortSk1
   

!=============================================================================================    
   SUBROUTINE util_horner1d ( c, x, p, dp )
!=============================================================================================     
   real(Rkind), intent (in    ) :: c(0:), x
   real(Rkind), intent (   out) :: p, dp
!---------------------------------------------------------------------------------------------    
!  Evaluation of a polynomial of a real variable (and its derivative)
!                   p(x) = sum ( c(i) * x(1)^i )
!  with the Horner method's
!  (adapted from Rosetta Code)
!--------------------------------------------------------------------------------------------- 
   integer(Ikind) :: i, ndeg
!---------------------------------------------------------------------------------------------
  
   ndeg = ubound(c,1)
   
   p = c(ndeg) ; dp = RZERO
   
   do i = ndeg-1, 0, -1
      dp = dp * x + p
      p  = p  * x + c(i)
   end do

   END SUBROUTINE util_horner1d
   

!=============================================================================================    
   SUBROUTINE util_horner2d ( c, x, p, dp )
!=============================================================================================     
   real(Rkind), intent (in    ) :: c(0:,0:), x(:)
   real(Rkind), intent (   out) :: p, dp(:)
!---------------------------------------------------------------------------------------------    
!  Evaluation of a polynomial of 2 real variables (and its gradient)
!               p(x) = sum ( c(i,j) * x(1)^i * x(2)^j )
!  with the Horner method's
!--------------------------------------------------------------------------------------------- 
   integer(Ikind) :: j
   real   (Rkind) :: b(0:ubound(c,2)), dpj, yj
!---------------------------------------------------------------------------------------------
      
   dp = RZERO ; yj = RONE
   
   do j = 0, ubound(c,2)
      call util_horner1d ( c(0:,j), x(1), b(j), dpj ) 
      dp(1) = dp(1) + dpj * yj
      yj = yj * x(2)
   end do
   
   call util_horner1d ( b, x(2), p, dp(2) ) 

   END SUBROUTINE util_horner2d
   

!=============================================================================================    
   SUBROUTINE util_horner3d ( c, x, p, dp )
!=============================================================================================     
   real(Rkind), intent (in    ) :: c(0:,0:,0:), x(:)
   real(Rkind), intent (   out) :: p, dp(:)
!---------------------------------------------------------------------------------------------    
!  Evaluation of a polynomial of 3 real variables (and its gradient)
!         p(x) = sum ( c(i,j,k) * x(1)^i * x(2)^j * x(3)^k )
!  with the Horner method's
!--------------------------------------------------------------------------------------------- 
   integer(Ikind) :: k
   real   (Rkind) :: b(0:ubound(c,3)), dpk(2), zk
!---------------------------------------------------------------------------------------------
      
   dp = RZERO ; zk = RONE 
   
   do k = 0, ubound(c,3)
      call util_horner2d ( c(0:,0:,k), x(1:2), b(k), dpk )
      dp(1:2) = dp(1:2) + dpk * zk
      zk = zk * x(3)
   end do

   call util_horner1d ( b, x(3), p, dp(3) ) 
   
   END SUBROUTINE util_horner3d      
   
            
!=============================================================================================
   SUBROUTINE util_IgemmI (transa, transb, alpha, a, b, beta, c, stat)
!=============================================================================================
   integer  (Ikind), intent(in    ) :: alpha, beta
   character(len=1), intent(in    ) :: transa, transb
   integer  (Ikind), intent(in    ) :: a(:,:), b(:,:)
   integer  (Ikind), intent(in out) :: c(:,:)
   type     (err_t), intent(in out) :: stat   
!--------------------------------------------------------------------------------------------- 
!  Performs one of the matrix-matrix operations
!
!                  c := alpha*op( a )*op( b ) + beta*c
!
!  where  op( x ) is one of
!
!                  op( x ) = x   or   op( x ) = x^T  
!
!  "alpha" and "beta" are integer scalars, and "a", "b" and "c" are integer matrices, with
!  "op( a )" an integer m by k matrix, "op( b )" an integer k by n matrix and "c" an integer
!  m by n matrix.
!
!  Inputs:
!  . transa, transb: 1-char strings (valid values are 'n', 't' or 'c')
!  . alpha, beta   : integer scalars
!  . a             : a m x k integer matrix
!  . b             : a ? x n integer matrix
!  . c             : (modified) a ? x ? integer matrix. Its shape must conform with the shape
!                    of "a" and "b" and the values given for "transa" and "transb"
!  Outputs:
!  . c             : the updated matrix
!
!  Source: this is a modified copy of BLAS routine dgemm adapted here for integers matrices
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=* ), parameter   :: HERE = 'util_IgemmI'
   integer  (Ikind )              :: k, i, info, j, l, temp, m, n, mc, nc
   logical                        :: nota,notb
   character(len=: ), allocatable :: msg
   character(len=99)              :: cnum
!--------------------------------------------------------------------------------------------- 
      
   m = size(a,dim=1) ; k = size(a,dim=2) ; n = size(b,dim=2)
!
!- set nota and notb as true if a and b are not transposed: 
!
   nota = (util_StringLow(transa) == 'n') ; notb = (util_StringLow(transb) == 'n')
!
!- Test the input parameters:
!
   info = 0
   if ( (.not. nota) .and. util_StringLow(transa)/='c' &
                     .and. util_StringLow(transa)/='t' ) then
      info = 1
      msg = "Invalid value ('" // transa // "') for << TransA >> (must be 'n', 't' or 'c')"
   else if ( (.not. notb) .and. util_StringLow(transb)/='c' &
                          .and. util_StringLow(transb)/='t' ) then
      info = 2
      msg = "Invalid value ('" // transb // "') for << TransB >> (must be 'n', 't' or 'c')"
   else if ( m < 0 ) then
      info = 3
      msg = "Negative value for dimension #1 of << A >>"
   else if ( n < 0 ) then
      info = 4
      msg = "Negative value for dimension #2 of << B >>"      
   else if ( k < 0 ) then
      info = 5   
      msg = "Negative value for dimension #2 of << A >>"
   end if

   if ( info /= 0 ) then
      stat = err_t (UERROR, HERE, msg) ; return
   end if
   
   if ( notb .and. nota ) then
      if ( size(b,1) /= k ) then
         info = 6
         msg = "in A*B, number of rows of B must be equal to the number of column of A"
      else
         mc = m ; nc = n
      end if
   else if ( notb .and. (.not. nota) ) then
      if ( size(b,1) /= m ) then
         info = 7
         msg = "in A^T*B, number of rows of B must be equal to the number of rows of A"
      else
         mc = k ; nc = n
      end if
   else if ( (.not. notb) .and. nota ) then
      if ( n /= k ) then
         info = 8
         msg = "in A*B^T, number of columns of B must be equal to the number of columns of A"
      else
         mc = m ; nc = size(b,1)
      end if
   else if ( (.not. notb) .and. (.not. nota) ) then   
      if ( n /= m ) then
         info = 9
         msg = "in A^T*B^T, number of columns of B must be equal to the number of rows of A"         
      else
         mc = k ; nc = size(b,1)
      end if   
   end if   
   
   if ( mc /= size(c,dim=1) .or. nc /= size(c,dim=2) ) then
      info = 10
      write(cnum,'(a,i0,a,i0,a)')"[",mc,",",nc,"]"
      msg = "the shape of C := alpha*op( A )*op( A ) + beta*C must be: "//trim(cnum)
   end if
                  
   if ( info /= 0 ) then
      call stat%set ( UERROR, HERE, 'Incompatible shape: '//trim(msg) ) ; return
   end if  
!
!- Quick return if possible:
!
   if ( (m==0) .or. (n==0) .or. (((alpha==IZERO) .or. (k==0)) .and. (beta==IONE)) ) return
!
!- And if alpha == zero:
!
   if ( alpha == IZERO ) then
      if ( beta == IZERO ) then
         do j = 1, n
            do i = 1, m
               c(i,j) = IZERO
            end do
         end do      
      else
         do j = 1, n
            do i = 1, m
               c(i,j) = beta*c(i,j)
            end do
         end do      
      end if
      return
   end if
!
!- Start the operations:
!
   if ( notb ) then
      if ( nota ) then
!
!        form  C := alpha*A*B + beta*C:
!
         do j = 1, n
            if ( beta == IZERO ) then
               do i = 1, m
                  c(i,j) = IZERO
               end do
            else if ( beta /= IONE ) then
               do i = 1, m
                  c(i,j) = beta*c(i,j)
               end do
            end if
            do l = 1, k
               temp = alpha*b(l,j)
               do i = 1, m
                  c(i,j) = c(i,j) + temp*a(i,l)
               end do
            end do
         end do
      else
!
!        Form  C := alpha*(A^T)*B + beta*C:
!
         do j = 1, n
            do i = 1, m
               temp = IZERO
                do l = 1, k
                   temp = temp + a(l,i)*b(l,j)
                end do
                if ( beta == IZERO ) then
                   c(i,j) = alpha*temp
                else
                   c(i,j) = alpha*temp + beta*c(i,j)
                end if
            end do            
         end do
         
      end if
      
   else
      if ( nota ) then
!
!        form  C := alpha*A*(B^T) + beta*C:
!
         do j = 1, n
            if ( beta == IZERO ) then
               do i = 1, m
                  c(i,j) = IZERO
               end do
            else if ( beta /= IONE ) then
               do i = 1, m
                  c(i,j) = beta*c(i,j)
               end do
            end if
            do l = 1, k
               temp = alpha*b(j,l)
               do i = 1, m
                  c(i,j) = c(i,j) + temp*a(i,l)
               end do
            end do
         end do
         
      else
!
!        form  C := alpha*(A^T)*(B^T) + beta*C:
!
         do j = 1, n
            do i = 1, m
               temp = IZERO
               do l = 1, k
                  temp = temp + a(l,i)*b(j,l)
               end do
               if ( beta == IZERO ) then
                   c(i,j) = alpha*temp
               else
                   c(i,j) = alpha*temp + beta*c(i,j)
               end if
            end do
         end do
      end if
   end if

   END SUBROUTINE util_IgemmI


!=============================================================================================
   SUBROUTINE util_IgemmR (transa, transb, alpha, a, b, beta, c, stat)
!=============================================================================================
   real     (Rkind), intent(in    ) :: alpha, beta
   character(len=1), intent(in    ) :: transa, transb
   integer  (Ikind), intent(in    ) :: a(:,:), b(:,:)
   real     (Rkind), intent(in out) :: c(:,:)
   type     (err_t), intent(in out) :: stat   
!--------------------------------------------------------------------------------------------- 
!  Same as util_igemmI but with alpha and beta reals (c must be real in input)
!
!  Source: this is a modified copy of BLAS routine dgemm adapted here for integers matrices
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=* ), parameter   :: HERE = 'util_IgemmR'
   integer  (Ikind )              :: k, i, info, j, l, m, n, mc, nc !, temp !bug 01/24
   real     (Rkind )              :: temp 
   logical                        :: nota,notb
   character(len=: ), allocatable :: msg
   character(len=99)              :: cnum
!--------------------------------------------------------------------------------------------- 
      
   m = size(a,dim=1) ; k = size(a,dim=2) ; n = size(b,dim=2)
!
!- set nota and notb as true if a and b are not transposed:
!
   nota = (util_StringLow(transa) == 'n') ; notb = (util_StringLow(transb) == 'n')
!
!- Test the input parameters:
!
   info = 0
   if ( (.not. nota) .and. util_StringLow(transa)/='c' &
                     .and. util_StringLow(transa)/='t' ) then
      info = 1
      msg = "Invalid value ('" // transa // "') for << TransA >> (must be 'n', 't' or 'c')"
   else if ( (.not. notb) .and. util_StringLow(transb)/='c' &
                          .and. util_StringLow(transb)/='t' ) then
      info = 2
      msg = "Invalid value ('" // transb // "') for << TransB >> (must be 'n', 't' or 'c')"
   else if ( m < 0 ) then
      info = 3
      msg = "Negative value for dimension #1 of << A >>"
   else if ( n < 0 ) then
      info = 4
      msg = "Negative value for dimension #2 of << B >>"      
   else if ( k < 0 ) then
      info = 5   
      msg = "Negative value for dimension #2 of << A >>"
   end if

   if ( info /= 0 ) then
      stat = err_t (UERROR, HERE, msg) ; return
   end if
   
   if ( notb .and. nota ) then
      if ( size(b,1) /= k ) then
         info = 6
         msg = "in A*B, number of rows of B must be equal to the number of column of A"
      else
         mc = m ; nc = n
      end if
   else if ( notb .and. (.not. nota) ) then
      if ( size(b,1) /= m ) then
         info = 7
         msg = "in A^T*B, number of rows of B must be equal to the number of rows of A"
      else
         mc = k ; nc = n
      end if
   else if ( (.not. notb) .and. nota ) then
      if ( n /= k ) then
         info = 8
         msg = "in A*B^T, number of columns of B must be equal to the number of columns of A"
      else
         mc = m ; nc = size(b,1)
      end if
   else if ( (.not. notb) .and. (.not. nota) ) then   
      if ( n /= m ) then
         info = 9
         msg = "in A^T*B^T, number of columns of B must be equal to the number of rows of A"         
      else
         mc = k ; nc = size(b,1)
      end if   
   end if   
   
   if ( mc /= size(c,dim=1) .or. nc /= size(c,dim=2) ) then
      info = 10
      write(cnum,'(a,i0,a,i0,a)')"[",mc,",",nc,"]"
      msg = "the shape of C := alpha*op( A )*op( A ) + beta*C must be: "//trim(cnum)
   end if
                  
   if ( info /= 0 ) then
      call stat%set ( UERROR, HERE, 'Incompatible shape: '//trim(msg) ) ; return
   end if  
!
!- Quick return if possible:
!
   if ( (m==0) .or. (n==0) .or. (((alpha==RZERO) .or. (k==0)) .and. (beta==RONE)) ) return
!
!- And if alpha == zero:
!
   if ( alpha == RZERO ) then
      if ( beta == RZERO ) then
         do j = 1, n
            do i = 1, m
               c(i,j) = RZERO
            end do
         end do      
      else
         do j = 1, n
            do i = 1, m
               c(i,j) = beta*c(i,j)
            end do
         end do      
      end if
      return
   end if
!
!- Start the operations:
!
   if ( notb ) then
      if ( nota ) then
!
!        form  C := alpha*A*B + beta*C:
!
         do j = 1, n
            if ( beta == RZERO ) then
               do i = 1, m
                  c(i,j) = RZERO
               end do
            else if ( beta /= RONE ) then
               do i = 1, m
                  c(i,j) = beta*c(i,j)
               end do
            end if
            do l = 1, k
               temp = alpha*b(l,j)
               do i = 1, m
                  c(i,j) = c(i,j) + temp*a(i,l)
               end do
            end do
         end do
      else
!
!        Form  C := alpha*(A^T)*B + beta*C:
!
         do j = 1, n
            do i = 1, m
               temp = RZERO
                do l = 1, k
                   temp = temp + a(l,i)*b(l,j)
                end do
                if ( beta == RZERO ) then
                   c(i,j) = alpha*temp
                else
                   c(i,j) = alpha*temp + beta*c(i,j)
                end if
            end do            
         end do
         
      end if
      
   else
      if ( nota ) then
!
!        form  C := alpha*A*(B^T) + beta*C:
!
         do j = 1, n
            if ( beta == RZERO ) then
               do i = 1, m
                  c(i,j) = RZERO
               end do
            else if ( beta /= RONE ) then
               do i = 1, m
                  c(i,j) = beta*c(i,j)
               end do
            end if
            do l = 1, k
               temp = alpha*b(j,l)
               do i = 1, m
                  c(i,j) = c(i,j) + temp*a(i,l)
               end do
            end do
         end do
         
      else
!
!        form  C := alpha*(A^T)*(B^T) + beta*C:
!
         do j = 1, n
            do i = 1, m
               temp = RZERO
               do l = 1, k
                  temp = temp + a(l,i)*b(j,l)
               end do
               if ( beta == RZERO ) then
                   c(i,j) = alpha*temp
               else
                   c(i,j) = alpha*temp + beta*c(i,j)
               end if
            end do
         end do
      end if
   end if

   END SUBROUTINE util_IgemmR
   

!=============================================================================================
   SUBROUTINE util_IgemmC (transa, transb, alpha, a, b, beta, c, stat)
!=============================================================================================
   complex  (Rkind), intent(in    ) :: alpha, beta
   character(len=1), intent(in    ) :: transa, transb
   integer  (Ikind), intent(in    ) :: a(:,:), b(:,:)
   complex  (Rkind), intent(in out) :: c(:,:)
   type     (err_t), intent(in out) :: stat   
!--------------------------------------------------------------------------------------------- 
!  Same as util_igemmI but with alpha and beta complexes (c must be complex in input)
!
!  Source: this is a modified copy of BLAS routine dgemm adapted here for integers matrices
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=* ), parameter   :: here = 'util_IgemmC'
   integer  (Ikind )              :: k, i, info, j, l, m, n, mc, nc !, temp !bug 01/24
   complex  (Rkind )              :: temp 
   logical                        :: nota,notb
   character(len=: ), allocatable :: msg
   character(len=99)              :: cnum
!--------------------------------------------------------------------------------------------- 
      
   m = size(a,dim=1) ; k = size(a,dim=2) ; n = size(b,dim=2)
!
!- set nota and notb as true if a and b are not transposed:
!
   nota = (util_StringLow(transa) == 'n') ; notb = (util_StringLow(transb) == 'n')
!
!- Test the input parameters:
!
   info = 0
   if ( (.not. nota) .and. util_StringLow(transa)/='c' &
                     .and. util_StringLow(transa)/='t' ) then
      info = 1
      msg = "Invalid value ('" // transa // "') for << TransA >> (must be 'n', 't' or 'c')"
   else if ( (.not. notb) .and. util_StringLow(transb)/='c' &
                          .and. util_StringLow(transb)/='t' ) then
      info = 2
      msg = "Invalid value ('" // transb // "') for << TransB >> (must be 'n', 't' or 'c')"
   else if ( m < 0 ) then
      info = 3
      msg = "Negative value for dimension #1 of << A >>"
   else if ( n < 0 ) then
      info = 4
      msg = "Negative value for dimension #2 of << B >>"      
   else if ( k < 0 ) then
      info = 5   
      msg = "Negative value for dimension #2 of << A >>"
   end if

   if ( info /= 0 ) then
      stat = err_t (UERROR, HERE, msg) ; return
   end if
   
   if ( notb .and. nota ) then
      if ( size(b,1) /= k ) then
         info = 6
         msg = "in A*B, number of rows of B must be equal to the number of column of A"
      else
         mc = m ; nc = n
      end if
   else if ( notb .and. (.not. nota) ) then
      if ( size(b,1) /= m ) then
         info = 7
         msg = "in A^T*B, number of rows of B must be equal to the number of rows of A"
      else
         mc = k ; nc = n
      end if
   else if ( (.not. notb) .and. nota ) then
      if ( n /= k ) then
         info = 8
         msg = "in A*B^T, number of columns of B must be equal to the number of columns of A"
      else
         mc = m ; nc = size(b,1)
      end if
   else if ( (.not. notb) .and. (.not. nota) ) then   
      if ( n /= m ) then
         info = 9
         msg = "in A^T*B^T, number of columns of B must be equal to the number of rows of A"         
      else
         mc = k ; nc = size(b,1)
      end if   
   end if   
   
   if ( mc /= size(c,dim=1) .or. nc /= size(c,dim=2) ) then
      info = 10
      write(cnum,'(a,i0,a,i0,a)')"[",mc,",",nc,"]"
      msg = "the shape of C := alpha*op( A )*op( A ) + beta*C must be: "//trim(cnum)
   end if
                  
   if ( info /= 0 ) then
      call stat%set ( UERROR, HERE, 'Incompatible shape: '//trim(msg) ) ; return
   end if  
!
!- Quick return if possible:
!
   if ( (m==0) .or. (n==0) .or. (((alpha==CZERO) .or. (k==0)) .and. (beta==CONE)) ) return
!
!- And if alpha == zero:
!
   if ( alpha == CZERO ) then
      if ( beta == CZERO ) then
         do j = 1, n
            do i = 1, m
               c(i,j) = CZERO
            end do
         end do      
      else
         do j = 1, n
            do i = 1, m
               c(i,j) = beta*c(i,j)
            end do
         end do      
      end if
      return
   end if
!
!- Start the operations:
!
   if ( notb ) then
      if ( nota ) then
!
!        form  C := alpha*A*B + beta*C:
!
         do j = 1, n
            if ( beta == CZERO ) then
               do i = 1, m
                  c(i,j) = CZERO
               end do
            else if ( beta /= CONE ) then
               do i = 1, m
                  c(i,j) = beta*c(i,j)
               end do
            end if
            do l = 1, k
               temp = alpha*b(l,j)
               do i = 1, m
                  c(i,j) = c(i,j) + temp*a(i,l)
               end do
            end do
         end do
      else
!
!        Form  C := alpha*(A^T)*B + beta*C:
!
         do j = 1, n
            do i = 1, m
               temp = CZERO
                do l = 1, k
                   temp = temp + a(l,i)*b(l,j)
                end do
                if ( beta == CZERO ) then
                   c(i,j) = alpha*temp
                else
                   c(i,j) = alpha*temp + beta*c(i,j)
                end if
            end do            
         end do
         
      end if
      
   else
      if ( nota ) then
!
!        form  C := alpha*A*(B^T) + beta*C:
!
         do j = 1, n
            if ( beta == CZERO ) then
               do i = 1, m
                  c(i,j) = CZERO
               end do
            else if ( beta /= CONE ) then
               do i = 1, m
                  c(i,j) = beta*c(i,j)
               end do
            end if
            do l = 1, k
               temp = alpha*b(j,l)
               do i = 1, m
                  c(i,j) = c(i,j) + temp*a(i,l)
               end do
            end do
         end do
         
      else
!
!        form  C := alpha*(A^T)*(B^T) + beta*C:
!
         do j = 1, n
            do i = 1, m
               temp = CZERO
               do l = 1, k
                  temp = temp + a(l,i)*b(j,l)
               end do
               if ( beta == CZERO ) then
                   c(i,j) = alpha*temp
               else
                   c(i,j) = alpha*temp + beta*c(i,j)
               end if
            end do
         end do
      end if
   end if

   END SUBROUTINE util_IgemmC   


!=============================================================================================
   SUBROUTINE util_ReshapeI (a, shape, res, stat, pad)
!=============================================================================================
   integer(Ikind),              intent(in    ) :: a(:,:)
   integer(Ikind),              intent(in    ) :: shape(2)
   integer(Ikind), allocatable, intent(in out) :: res(:,:)
   type   (err_t),              intent(in out) :: stat
   integer(Ikind), optional   , intent(in    ) :: pad
!--------------------------------------------------------------------------------------------- 
!  Reshapes the integer array "a"  to correspond to "shape". 
!  If necessary, the new array may be padded 
!  . with "pad" if present
!  . with 0
!
!  Note: for relatively small arrays, the intrinsic reshape is used. However, for large array
!        (size > ysize) explicit do loops are used to avoid unnecessary temporaries.
!-----------------------------------------------------------------------------------R.H. 11/19 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: here = 'util_ReshapeI'
   integer  (Ikind), parameter :: ysize = 0 
   integer  (Ikind)            :: na, ma, ia, ja, nr, mr, ir, jr, kr, szr
   integer  (Ikind)            :: defpad(1,1)
!--------------------------------------------------------------------------------------------- 
      
   defpad(1,1) = IZERO

#include "include/util_reshape.inc" 
   
   END SUBROUTINE util_ReshapeI   


!=============================================================================================
   SUBROUTINE util_ReshapeR (a, shape, res, stat, pad)
!=============================================================================================
   real   (Rkind),              intent(in    ) :: a(:,:)
   integer(Ikind),              intent(in    ) :: shape(2)
   real   (Rkind), allocatable, intent(in out) :: res(:,:)
   type   (err_t),              intent(in out) :: stat
   real   (Rkind), optional   , intent(in    ) :: pad
!--------------------------------------------------------------------------------------------- 
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: here = 'util_ReshapeR'
   integer  (Ikind), parameter :: ysize = 0 
   integer  (Ikind)            :: na, ma, ia, ja, nr, mr, ir, jr, kr, szr
   real     (Rkind)            :: defpad(1,1)   
!--------------------------------------------------------------------------------------------- 
      
   defpad(1,1) = RZERO

#include "include/util_reshape.inc" 
   
   END SUBROUTINE util_ReshapeR


!=============================================================================================
   SUBROUTINE util_ReshapeC (a, shape, res, stat, pad)
!=============================================================================================
   complex(Rkind),              intent(in    ) :: a(:,:)
   integer(Ikind),              intent(in    ) :: shape(2)
   complex(Rkind), allocatable, intent(in out) :: res(:,:)
   type   (err_t),              intent(in out) :: stat
   complex(Rkind), optional   , intent(in    ) :: pad
!--------------------------------------------------------------------------------------------- 
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: here = 'util_ReshapeC'
   integer  (Ikind), parameter :: ysize = 0 
   integer  (Ikind)            :: na, ma, ia, ja, nr, mr, ir, jr, kr, szr
   complex  (Rkind)            :: defpad(1,1)
!--------------------------------------------------------------------------------------------- 
      
   defpad(1,1) = CZERO

#include "include/util_reshape.inc" 
   
   END SUBROUTINE util_ReshapeC


!=============================================================================================
   SUBROUTINE util_ReshapeL (a, shape, res, stat, pad)
!=============================================================================================
   logical       ,              intent(in    ) :: a(:,:)
   integer(Ikind),              intent(in    ) :: shape(2)
   logical       , allocatable, intent(in out) :: res(:,:)
   type   (err_t),              intent(in out) :: stat
   logical       , optional   , intent(in    ) :: pad
!--------------------------------------------------------------------------------------------- 
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: here = 'util_ReshapeL'
   integer  (Ikind), parameter :: ysize = 0 
   integer  (Ikind)            :: na, ma, ia, ja, nr, mr, ir, jr, kr, szr
   logical                     :: defpad(1,1)
!--------------------------------------------------------------------------------------------- 
      
   defpad(1,1) = LZERO

#include "include/util_reshape.inc" 
   
   END SUBROUTINE util_ReshapeL


!=============================================================================================
   SUBROUTINE util_ReshapeS (a, shape, res, stat, pad)
!=============================================================================================
   type   (str_t),              intent(in    ) :: a(:,:)
   integer(Ikind),              intent(in    ) :: shape(2)
   type   (str_t), allocatable, intent(in out) :: res(:,:)
   type   (err_t),              intent(in out) :: stat
   type   (str_t), optional   , intent(in    ) :: pad
!--------------------------------------------------------------------------------------------- 
!--------------------------------------------------------------------------------------------- 
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'util_ReshapeS'
   integer  (Ikind), parameter :: ysize = 0 
   integer  (Ikind)            :: na, ma, ia, ja, nr, mr, ir, jr, kr, szr
   type     (str_t)            :: defpad(1,1)
!--------------------------------------------------------------------------------------------- 
      
   defpad(1,1)%str = ''

   if ( present(pad )) defpad(1,1) = pad

   nr = shape(1) ; mr = shape(2)
   
   if ( nr == 0 .or. mr == 0 ) then
      allocate(res(0,0))
      return
   end if
      
   if ( size(a) < ysize ) then
!
!-    when matrix size is not too big, use intrinsic reshape:
!   
      res = reshape ( a, shape, pad=defpad )
   else
!
!-    otherwise use do loops:
!   
      call util_alloc ( mode='e', t=res, n=nr, m=mr, stat=stat, const=defpad(1,1)%str )
      error_TraceNreturn(stat, HERE)

      !allocate(res(nr,mr), source = defpad(1,1), stat = err)
      !if ( err /= 0 ) then
      !   stat = err_t (stat = IERROR, msg = HERE // " Allocation failure")         
      !   return
      !end if   
      
      na = size(a,1) ; ma = size(a,2) ; szr = nr*mr
      
      ir = 1 ; jr = 1 ; kr = 0
      
      do ja = 1, ma
         do ia = 1, na
            kr = kr + 1 ; if (kr > szr) return
            
            if ( allocated(a(ia,ja)%str) ) then
               res(ir,jr)%str = (a(ia,ja)%str)
            else
               res(ir,jr)%str = ''
            end if
            
            if ( ir < nr ) then
!
!-             the next element will be on the same column # jr and at the row # ir:
!            
               ir = ir + 1
            else
!
!-             the next element will be at the 1st row of the next column # jr:
!
               ir = 1
               jr = jr + 1
            end if
            
         end do
      end do
      
   end if

   END SUBROUTINE util_ReshapeS


!=============================================================================================
   SUBROUTINE util_InvImats ( I, res, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_getrf, LapackInterface_getri
   integer(Ikind),              intent(in    ) :: I(:,:)
   real   (Rkind), allocatable, intent(in out) :: res(:,:)
   type   (err_t),              intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Inverses the integer square matrix I
!
!  The result is the real matrix res. The input matrix I is first converted and copied in a
!  real temporary array.
!
!  external routines:
!     . ?getrf (lapack)
!     . ?getri (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_InvImats'
   integer  (Ikind)              :: err     
!- needed by LAPACK routines ?getrf and ?getri -----------------------------------------------
   integer                       :: nrow, ncol, lwork, info
   real     (Rkind)              :: rw(1)
   integer         , allocatable :: ipiv(:)
   real     (Rkind), allocatable :: rwork(:)
!---------------------------------------------------------------------------------------------   

   err = 0
!
!- Make a copy (in res) of the input matrix:
!  
   nrow = size(I,dim=1) ; ncol = size(I,dim=2)
   
   if ( nrow /= ncol .or. nrow == 0 ) then
      call stat%set ( UERROR, HERE, 'The matrix must be square' )
      return
   end if
   
   allocate(ipiv(nrow), stat = err)
   if ( err /= 0 ) then
      call stat%set ( IERROR, HERE, 'Allocation failure for "ipiv"' )
      return
   end if
!
!- Allocate/reallocate res only if needed:
!
   call util_alloc ( 'a', res, int(nrow,Ikind), int(nrow,Ikind), stat )
   error_TraceNreturn(stat, HERE)   
         
   res(:,:) = real(I,kind=Rkind) 
        
   call LapackInterface_getrf ( nrow, nrow, res, nrow, ipiv, info )

   if ( info > 0 ) then
      call stat%set ( UERROR,HERE,'<< inv(a) >> could not be computed (singular matrix)' )
      return
   end if
   
   if ( nrow >= 100 ) then
!
!-    for a "large" matrix compute the optimal size lwork of the work array:
!     (otherwise, set lwork = nrow)
!      
      lwork = -1
      call LapackInterface_getri (nrow, res, nrow, ipiv, rw, lwork, info)
      lwork = int(rw(1))
   else
      lwork = nrow
   end if   
            
   allocate(rwork(lwork), stat = err)

   if ( err == 0 ) then
      call LapackInterface_getri (nrow, res, nrow, ipiv, rwork, lwork, info)
      if ( info > 0 ) then
         call stat%set ( IERROR, HERE, '<< inv(a) >>  matrix inversion failed in ?getri!' )
         return   
      end if
   else      
      call stat%set ( IERROR, HERE, 'Allocation failure for "rwork"' ) ; return
   end if
            
   END SUBROUTINE util_InvImats


!=============================================================================================
   SUBROUTINE util_InvRmats1 ( R, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_getrf, LapackInterface_getri
   real   (Rkind),                        intent(in out) :: R(:,:)   
   type   (err_t),                        intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Inverses the real square matrix R 
!
!  R is overwritten by the result.
!
!  external routines:
!     . ?getrf (lapack)
!     . ?getri (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_InvRmats1'
   integer  (Ikind)              :: err     
!- needed by LAPACK routines ?getrf and ?getri -----------------------------------------------
   integer                       :: nrow, ncol, lwork, info
   real     (Rkind)              :: rw(1)
   integer         , allocatable :: ipiv(:)
   real     (Rkind), allocatable :: rwork(:)
!---------------------------------------------------------------------------------------------   

   err = 0

   nrow = size(R,dim=1) ; ncol = size(R,dim=2)  
   
   if ( nrow /= ncol .or. nrow == 0 ) then
      call stat%set (UERROR, HERE, 'The matrix must be square')
      return   
   end if
   
   allocate(ipiv(nrow), stat = err)
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for "ipiv"') ; return
   end if
   
   call LapackInterface_getrf ( nrow, nrow, R, nrow, ipiv, info )

   if ( info > 0 ) then
      call stat%set (UERROR,HERE,'<< inv(a) >> could not be computed (singular matrix)')
      return
   end if

   if ( nrow >= 100 ) then
!
!-    for a "large" matrix compute the optimal size lwork of the work array:
!     (otherwise, set lwork = n)
!      
      lwork = -1
      call LapackInterface_getri (nrow, R, nrow, ipiv, rw, lwork, info)
      lwork = int(rw(1))
   else
      lwork = nrow
   end if   
            
   allocate(rwork(lwork), stat = err)
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for "rwork"') ; return
   end if

   call LapackInterface_getri (nrow, R, nrow, ipiv, rwork, lwork, info)
   if ( info > 0 ) then
      call stat%set (IERROR, HERE, '<< inv(a) >>  matrix inversion failed in ?getri!')
      return   
   end if
            
   END SUBROUTINE util_InvRmats1
   

!=============================================================================================
   SUBROUTINE util_InvRmats2 ( R, res, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_getrf, LapackInterface_getri
   real   (Rkind),              intent(in    ) :: R(:,:)   
   real   (Rkind), allocatable, intent(in out) :: res(:,:)   
   type   (err_t),              intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Inverses the real square matrix R 
!
!  R is unchanged and the result is stored in res (but this will require a prior copy of R)
!
!  external routines:
!     . ?getrf (lapack)
!     . ?getri (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_InvRmats2'
   integer  (Ikind)              :: err     
!- needed by LAPACK routines ?getrf and ?getri -----------------------------------------------
   integer                       :: nrow, ncol, lwork, info
   real     (Rkind)              :: rw(1)
   integer         , allocatable :: ipiv(:)
   real     (Rkind), allocatable :: rwork(:)
!---------------------------------------------------------------------------------------------   

   err = 0

   nrow = size(R,dim=1) ; ncol = size(R,dim=2)  
   
   if ( nrow /= ncol .or. nrow == 0 ) then
      call stat%set (UERROR, HERE, 'The matrix must be square')
      return   
   end if
   
   allocate(ipiv(nrow), stat = err)
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for "ipiv"') ; return
   end if
!
!- Allocate/reallocate res only if needed:
!   
   call util_alloc ( 'a', res, int(nrow,Ikind), int(nrow,Ikind), stat )
   error_TraceNreturn(stat, HERE)
   
   res = R
   call LapackInterface_getrf ( nrow, nrow, res, nrow, ipiv, info )

   if ( info > 0 ) then
      call stat%set (UERROR,HERE,'<< inv(a) >> could not be computed (singular matrix)')
      return
   end if

   if ( nrow >= 100 ) then
!
!-    for a "large" matrix compute the optimal size lwork of the work array:
!     (otherwise, set lwork = n)
!      
      lwork = -1
      call LapackInterface_getri (nrow, res, nrow, ipiv, rw, lwork, info)
      lwork = int(rw(1))
   else
      lwork = nrow
   end if   
            
   allocate(rwork(lwork), stat = err)
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for "rwork"') ; return
   end if
   
   call LapackInterface_getri (nrow, res, nrow, ipiv, rwork, lwork, info)
   if ( info > 0 ) then
      call stat%set (IERROR, HERE, '<< inv(a) >>  matrix inversion failed in ?getri!')
      return   
   end if
            
   END SUBROUTINE util_InvRmats2
      

!=============================================================================================
   SUBROUTINE util_InvCmats1 ( C, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_getrf, LapackInterface_getri
   complex(Rkind),                        intent(in out) :: C(:,:)
   type   (err_t),                        intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Inverses the complex square matrix C
!
!  C is overwritten by the result.
!
!  external routines:
!     . ?getrf (lapack)
!     . ?getri (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'util_InvCmats1'
   integer  (Ikind)              :: err      
!- needed by LAPACK routines ?getrf and ?getri -----------------------------------------------
   integer                       :: nrow, ncol, lwork, info
   complex  (Rkind)              :: cw(1)   
   integer         , allocatable :: ipiv(:)
   complex  (Rkind), allocatable :: cwork(:)
!---------------------------------------------------------------------------------------------   
   
   err = 0
!
!- Need to make a copy (in cmat) of the input matrix (lapack overwrite it):
!           
   nrow = size(C,dim=1) ; ncol = size(C,dim=2)  
   
   if ( nrow /= ncol .or. nrow == 0 ) then
      call stat%set (UERROR, HERE, 'The matrix must be square')
      return
   end if
   
   allocate(ipiv(nrow), stat = err)
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for "ipiv"')
      return
   end if   
   
   call LapackInterface_getrf ( nrow, nrow, C, nrow, ipiv, info )
       
   if ( info > 0 ) then
      call stat%set (UERROR, HERE, '<< inv(a) >> could not be computed (singular matrix)')
      return
   end if
   
   if ( nrow >= 100 ) then
!
!-    for a "large" matrix compute the optimal size lwork of the work array:
!     (otherwise, set lwork = nrow)
!      
      lwork =-1
      call LapackInterface_getri (nrow, C, nrow, ipiv, cw, lwork, info)
      lwork = int(cw(1))
   else
      lwork = nrow
   end if   
            
   allocate(cwork(lwork), stat = err)
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for "cwork"') ; return
   end if

   call LapackInterface_getri (nrow, C, nrow, ipiv, cwork, lwork, info)
   if ( info > 0 ) then
      call stat%set (IERROR, HERE, '<< inv(a) >> matrix inversion failed in dgetri!')
      return
   end if       
            
   END SUBROUTINE util_InvCmats1


!=============================================================================================
   SUBROUTINE util_InvCmats2 ( C, res, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_getrf, LapackInterface_getri
   complex(Rkind),              intent(in    ) :: C(:,:)
   complex(Rkind), allocatable, intent(in out) :: res(:,:)
   type   (err_t),              intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Inverses the complex square matrix C
!
!  C is unchanged and the result is stored in res (but this will require a prior copy of R)
!
!  external routines:
!     . ?getrf (lapack)
!     . ?getri (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'util_InvCmats2'
   integer  (Ikind)              :: err      
!- needed by LAPACK routines ?getrf and ?getri -----------------------------------------------
   integer                       :: nrow, ncol, lwork, info
   complex  (Rkind)              :: cw(1)   
   integer         , allocatable :: ipiv(:)
   complex  (Rkind), allocatable :: cwork(:)
!---------------------------------------------------------------------------------------------   
   
   err = 0
!
!- Need to make a copy (in cmat) of the input matrix (lapack overwrite it):
!           
   nrow = size(C,dim=1) ; ncol = size(C,dim=2)  
   
   if ( nrow /= ncol .or. nrow == 0 ) then
      call stat%set (UERROR, HERE, 'The matrix must be square')
      return
   end if
   
   allocate(ipiv(nrow), stat = err)
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for "ipiv"')
      return
   end if   
!
!- Allocate/reallocate res only if needed:
!   
   call util_alloc ( 'a', res, int(nrow,Ikind), int(nrow,Ikind), stat )
   error_TraceNreturn(stat, HERE)         
   
   res = C 
   call LapackInterface_getrf ( nrow, nrow, res, nrow, ipiv, info )
       
   if ( info > 0 ) then
      call stat%set (UERROR, HERE, '<< inv(a) >> could not be computed (singular matrix)')
      return
   end if
   
   if ( nrow >= 100 ) then
!
!-    for a "large" matrix compute the optimal size lwork of the work array:
!     (otherwise, set lwork = nrow)
!      
      lwork =-1
      call LapackInterface_getri (nrow, res, nrow, ipiv, cw, lwork, info)
      lwork = int(cw(1))
   else
      lwork = nrow
   end if   
            
   allocate(cwork(lwork), stat = err)
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for "cwork"') ; return
   end if

   call LapackInterface_getri (nrow, res, nrow, ipiv, cwork, lwork, info)
   if ( info > 0 ) then
      call stat%set (IERROR, HERE, '<< inv(a) >> matrix inversion failed in dgetri!')               
      return
   end if   
            
   END SUBROUTINE util_InvCmats2


!=============================================================================================
   SUBROUTINE util_SolvLinR1 ( A, B, rcond, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_gesv, LapackInterface_gelsy
   real   (Rkind),              intent(in out) :: A(:,:)
   real   (Rkind), allocatable, intent(in out) :: B(:,:)
   real   (Rkind), optional   , intent(in    ) :: rcond
   type   (err_t),              intent(in out) :: stat      
!---------------------------------------------------------------------------------------------
!  Solves the linear system A * X = B where A and B are real matrices
!  
!  A is OVERWRITTEN by details of its factorization.
!  B is OVERWRITTEN by the solutions (must be allocatable and may be resized)
!
!  rcond is used to determine the effective rank of A (for over/underdetermined systems)
!
!  external routines:
!     . ?gesv (lapack) for square systems
!     . ?gesy (lapack) for overdetermined or underdetermined systems 
!
!  Example: A is an nA x mA matrix and B is an nB x nRhs matrix (=> nB = nA)
!           then the solution is a mA x nRhs matrix stored in B thus B is resized (when 
!           nA /= mA) 
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_SolvLinR1'   
   real     (Rkind), parameter   :: RCOND_ = 0.01_Rkind
   integer  (Ikind)              :: err
!- needed by LAPACK routine ?gesv ------------------------------------------------------------
   integer                       :: nA, mA, nB, nRhs, info, lwork, rank, ldB
   real     (Rkind)              :: rc       
   integer         , allocatable :: Piv(:)
   real     (Rkind), allocatable :: rwork(:)
!---------------------------------------------------------------------------------------------   
      
   err = 0
   
   nA = size(A,dim=1) ; mA = size(A,dim=2) ; 
   nB = size(B,dim=1) ; nRhs = size(B,dim=2)
         
   if ( nB /= nA ) then
      call stat%set (UERROR, HERE, 'Shapes of the matrix and the rhs do not conform') 
      return
   end if

   ldB = max(mA,nB)
   
   if ( ldB /= nB ) then
      call util_alloc ( 's', B, int(ldB,Ikind), int(nrhs,Ikind), stat )
      error_TraceNreturn(stat, HERE)
   end if         

   allocate(Piv(mA), source = 0, stat = err)
   
   if ( err /= 0 ) then
      call stat%set (UERROR, HERE, 'Allocation failure for Piv') 
      return
   end if   
    
   
   if ( nA == mA ) then
!
!-    For square systems:
!     ------------------
!      
      call LapackInterface_gesv ( nA, nRhs, A, nA, Piv, B, ldB, info )

      if ( info /= 0 ) then
         if ( info > 0 ) then
            call stat%set (UERROR, HERE, 'Returned by ?gelsv: The solution could not be '//&
                                         ' computed (singular matrix)')
         else
            call stat%set (UERROR, HERE, 'Returned by ?gelsv: The '//util_intTochar(-info)//& 
                                         '-th argument had an illegal value')             
         end if
         return
      end if      
                    
   else
!   
!-    For rectangular systems:
!     -----------------------
! 
      if ( present(rcond) ) then
         rc = rcond
      else
         rc = RCOND_
      end if
      
      lwork =-1
      allocate(rwork(1))
      if ( err == 0 ) then
         call LapackInterface_gelsy ( nA, mA, nRhs, A, nA, B, ldB, Piv, &
                                      rc, rank, rwork, lwork, info )
                                      
         if ( info == 0 ) then
            lwork = int(rwork(1))
            deallocate(rwork) ; allocate(rwork(lwork), stat = err)
            if ( err /= 0 ) then
               call stat%set ( IERROR, HERE, 'Allocation failure for rwork' ) 
               return
            end if                 
         else
            call stat%set ( UERROR, HERE, 'Returned by ?gelsy: The '//util_intTochar(info)//& 
                                         '-th argument had an illegal value' )             
            return         
         end if
      end if
      
      call LapackInterface_gelsy ( nA, mA, nRhs, A, nA, B, ldB, Piv, &
                                   rc, rank, rwork, lwork, info )
      if ( info /= 0 ) then
         call stat%set ( UERROR, HERE, 'Returned by ?gelsy: The '//util_intTochar(-info)// & 
                                      '-th argument had an illegal value' )             
         return         
      end if
      
      if ( mA < ldB ) then
         call util_alloc ( 's', B, int(mA,Ikind), int(nRhs,Ikind), stat )
         error_TraceNreturn(stat, HERE)
      end if             
      
   end if
   
   END SUBROUTINE util_SolvLinR1   
      

!=============================================================================================
   SUBROUTINE util_SolvLinR2 ( A, B, X, rcond, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_gesv, LapackInterface_gelsy
   real   (Rkind),              intent(in    ) :: A(:,:), B(:,:)
   real   (Rkind), allocatable, intent(in out) :: X(:,:)
   real   (Rkind), optional   , intent(in    ) :: rcond
   type   (err_t),              intent(in out) :: stat      
!---------------------------------------------------------------------------------------------
!  Solves the linear system A * X = B where A, B and X are real matrices
!  
!  A and B are UNCHANGED (this will require a prior copies of A and B)
!
!  rcond is used to determine the effective rank of A (for over/underdetermined systems)
!
!  external routines:
!     . ?gesv (lapack) for square systems
!     . ?gesy (lapack) for overdetermined or underdetermined systems 
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_SolvLinR2'   
   real     (Rkind), parameter   :: RCOND_ = 0.01_Rkind
   integer  (Ikind)              :: err
!- needed by LAPACK routine ?gesv ------------------------------------------------------------
   integer                       :: i, j, nA, mA, nB, nRhs, info, lwork, rank, ldb
   real     (Rkind)              :: rc       
   integer         , allocatable :: Piv(:)
   real     (Rkind), allocatable :: rwork(:), Mat(:,:), Rhs(:,:)
!---------------------------------------------------------------------------------------------   
      
   err = 0
   
   nA = size(A,dim=1) ; mA = size(A,dim=2) ; 
   nB = size(B,dim=1) ; nRhs = size(B,dim=2)
         
   if ( nB /= nA ) then
      call stat%set (UERROR, HERE, 'Shapes of the matrix and the rhs do not conform') 
      return
   end if
!
!- Make a copy (in Mat, Rhs) of the input system (lapack overwrite them)
!   
   ldB = max(mA,nB)
                   allocate(Mat(nA ,mA  ), source = A, stat = err)
   if ( err == 0 ) allocate(Rhs(ldb,nRhs),             stat = err)
   if ( err == 0 ) allocate(Piv(mA      ), source = 0, stat = err)

   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for Mat, Rhs or Piv')
      return
   end if
   
   do j = 1, nRhs
      do i = 1, nB
         Rhs(i,j) = B(i,j)
      end do
   end do     
    
   if ( nA == mA ) then
!
!-    For square systems:
!     ------------------
!      
      call LapackInterface_gesv ( nA, nRhs, Mat, nA, Piv, Rhs, ldB, info )

      if ( info /= 0 ) then
         if ( info > 0 ) then
            call stat%set ( UERROR, HERE, 'Returned by ?gelsv: The solution could not be '//&
                                          ' computed (singular matrix)' )
         else
            call stat%set ( UERROR, HERE, 'Returned by ?gelsv: The '//util_intTochar(-info)//& 
                                          '-th argument had an illegal value' )             
         end if
         return
      end if      
      
      call move_alloc ( from = Rhs, to = X )
                    
   else
!   
!-    For rectangular systems:
!     -----------------------
! 
      if ( present(rcond) ) then
         rc = rcond
      else
         rc = RCOND_
      end if
      
      lwork =-1
      allocate(rwork(1))
      if ( err == 0 ) then
         call LapackInterface_gelsy ( nA, mA, nRhs, Mat, nA, Rhs, ldB, Piv, &
                                      rc, rank, rwork, lwork, info )
                                      
         if ( info == 0 ) then
            lwork = int(rwork(1))
            deallocate(rwork) ; allocate(rwork(lwork), stat = err)
            if ( err /= 0 ) then
               call stat%set ( IERROR, HERE, 'Allocation failure for rwork' ) 
               return
            end if                 
         else
            call stat%set ( UERROR, HERE, 'Returned by ?gelsy: The '//util_intTochar(info)//& 
                                          '-th argument had an illegal value' )             
            return         
         end if
      end if
      
      call LapackInterface_gelsy ( nA, mA, nRhs, Mat, nA, Rhs, ldB, Piv, &
                                   rc, rank, rwork, lwork, info )
      if ( info /= 0 ) then
         call stat%set ( UERROR, HERE, 'Returned by ?gelsy: The '//util_intTochar(-info)//& 
                                       '-th argument had an illegal value' )             
         return         
      end if
!
!-    Allocate/reallocate X if needed only:
!
      call util_alloc ( 'a', X, int(mA,Ikind), int(nRhs,Ikind), stat )
      error_TraceNreturn(stat, HERE)

      do j = 1, nRhs
         do i = 1, mA
            X(i,j) = Rhs(i,j)
         end do
      end do           
      
   end if
   
   END SUBROUTINE util_SolvLinR2
   
   
!=============================================================================================
   SUBROUTINE util_SolvLinC1 ( A, B, rcond, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_gesv, LapackInterface_gelsy
   complex(Rkind),              intent(in out) :: A(:,:)
   complex(Rkind), allocatable, intent(in out) :: B(:,:)
   real   (Rkind), optional   , intent(in    ) :: rcond
   type   (err_t),              intent(in out) :: stat      
!---------------------------------------------------------------------------------------------
!  Solves the linear system A * X = B where a and b are complex matrices
!  
!  A is OVERWRITTEN by details of its factorization.
!  B is OVERWRITTEN by the solutions (must be allocatable and may be resized)
!
!  rcond is used to determine the effective rank of A (for over/underdetermined systems)
!
!  external routines:
!     . ?gesv (lapack) for square systems
!     . ?gesy (lapack) for overdetermined or underdetermined systems 
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_SolvLinC1'   
   real     (Rkind), parameter   :: RCOND_ = 0.01_Rkind
   integer  (Ikind)              :: err
!- needed by LAPACK routine ?gesv ------------------------------------------------------------
   integer                       :: nA, mA, nB, nRhs, info, lwork, rank, ldb
   real     (Rkind)              :: rc       
   integer         , allocatable :: Piv(:)
   real     (Rkind), allocatable :: rwork(:)
   complex  (Rkind), allocatable :: cwork(:)
!---------------------------------------------------------------------------------------------   
      
   err = 0
   
   nA = size(A,dim=1) ; mA = size(A,dim=2)  

   nB = size(B,dim=1) ; nRhs = size(B,dim=2)
         
   if ( nB /= nA ) then
      call stat%set (UERROR, HERE, 'Shapes of the matrix and the rhs do not conform') 
      return
   end if

   ldB = max(mA,nB)
   
   if ( ldB /= nB ) then
      call util_alloc ( 's', B, int(ldB,Ikind), int(nRhs,Ikind), stat )
      error_TraceNreturn(stat, HERE)
   end if           

   allocate(Piv(mA), source = 0, stat = err)
   
   if ( err /= 0 ) then
      call stat%set (UERROR, HERE, 'Allocation failure for Piv') 
      return
   end if   
   
   if ( nA == mA ) then
!
!-    For square systems:
!     ------------------
! 
      call LapackInterface_gesv ( na, nrhs, A, na, Piv, B, ldb, info )

      if ( info /= 0 ) then
         if ( info > 0 ) then
            call stat%set ( UERROR, HERE, 'Returned by ?gelsv: The solution could not be '//&
                                          ' computed (singular matrix)' )
         else
            call stat%set ( UERROR, HERE, 'Returned by ?gelsv: The '//util_intTochar(-info)//& 
                                          '-th argument had an illegal value' )             
         end if
         return
      end if      
                    
   else
!   
!-    For rectangular systems:
!     -----------------------
!
      if ( present(rcond) ) then
         rc = rcond
      else
         rc = 0.01_Rkind
      end if

      lwork =-1
      allocate(cwork(1))
      allocate(rwork(2*nA), stat = err)
      if ( err == 0 ) then
         call LapackInterface_gelsy ( nA, mA, nRhs, A, nA, B, ldB, Piv, &
                                      rc, rank, cwork, lwork, rwork, info )
         if ( info == 0 ) then
            lwork = int(cwork(1))
            deallocate(cwork) ; allocate(cwork(lwork), stat = err)
         else
            call stat%set ( UERROR, HERE, 'Returned by ?gelsy: The '//util_intTochar(info)//& 
                                          '-th argument had an illegal value' )             
            return         
         end if
      end if
      if ( err /= 0 ) then
         call stat%set ( IERROR, HERE, 'Allocation failure for rwork or cwork' ) 
         return
      end if      
      
      call LapackInterface_gelsy ( nA, mA, nRhs, A, nA, B, ldB, Piv, &
                                   rc, rank, cwork, lwork, rwork, info )
      if ( info /= 0 ) then
         call stat%set ( UERROR, HERE, 'Returned by ?gelsy: The '//util_intTochar(-info)//& 
                                       '-th argument had an illegal value' )             
         return         
      end if

      if ( mA < ldB ) then
         call util_alloc ( 's', B, int(mA,Ikind), int(nRhs,Ikind), stat )
         error_TraceNreturn(stat, HERE)
      end if       
      
   end if
   
   END SUBROUTINE util_SolvLinC1   


!=============================================================================================
   SUBROUTINE util_SolvLinC2 ( A, B, X, rcond, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_gesv, LapackInterface_gelsy
   complex(Rkind),              intent(in    ) :: A(:,:), B(:,:)
   complex(Rkind), allocatable, intent(in out) :: X(:,:)
   real   (Rkind), optional   , intent(in    ) :: rcond
   type   (err_t),              intent(in out) :: stat      
!---------------------------------------------------------------------------------------------
!  Solves the linear system A * X = B where A, B and X are complex matrices
!  
!  A and B are UNCHANGED (this will require a prior copies of A and B)
!
!  rcond is used to determine the effective rank of A (for over/underdetermined systems)
!
!  external routines:
!     . ?gesv (lapack) for square systems
!     . ?gesy (lapack) for overdetermined or underdetermined systems 
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'util_SolvLinC2'   
   real     (Rkind), parameter   :: RCOND_ = 0.01_Rkind
   integer  (Ikind)              :: err
!- needed by LAPACK routine ?gesv ------------------------------------------------------------
   integer                       :: i, j, nA, mA, nB, nRhs, info, lwork, rank, ldB
   real     (Rkind)              :: rc       
   integer         , allocatable :: Piv(:)
   real     (Rkind), allocatable :: rwork(:)
   complex  (Rkind), allocatable :: cwork(:), Mat(:,:), Rhs(:,:)
!---------------------------------------------------------------------------------------------   
      
   err = 0
   
   nA = size(A,dim=1) ; mA = size(A,dim=2)  

   nB = size(B,dim=1) ; nRhs = size(B,dim=2)
         
   if ( nB /= nA ) then
      call stat%set (UERROR, HERE, 'Shapes of the matrix and the rhs do not conform') 
      return
   end if
!
!- Make a copy (in Mat, Rhs) of the input system (lapack overwrite them)
!   
   ldB = max(mA,nB)
                   allocate(Mat(nA ,mA  ), source = A, stat = err)
   if ( err == 0 ) allocate(Rhs(ldb,nRhs),             stat = err)
   if ( err == 0 ) allocate(Piv(mA      ), source = 0, stat = err)

   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for Mat, Rhs or Piv')
      return
   end if
   
   do j = 1, nRhs
      do i = 1, nB
         Rhs(i,j) = B(i,j)
      end do
   end do     
            
   if ( nA == mA ) then
!
!-    For square systems:
!     ------------------
! 
      call LapackInterface_gesv ( nA, nRhs, Mat, nA, Piv, Rhs, ldb, info )

      if ( info /= 0 ) then
         if ( info > 0 ) then
            call stat%set ( UERROR, HERE, 'Returned by ?gelsv: The solution could not be '//&
                                          ' computed (singular matrix)' )
         else
            call stat%set ( UERROR, HERE, 'Returned by ?gelsv: The '//util_intTochar(-info)//& 
                                          '-th argument had an illegal value' )             
         end if
         return
      end if      
      
      call move_alloc ( from = Rhs, to = X )
                    
   else
!   
!-    For rectangular systems:
!     -----------------------
!
      if ( present(rcond) ) then
         rc = rcond
      else
         rc = 0.01_Rkind
      end if

      lwork =-1
      allocate(cwork(1))
      allocate(rwork(2*nA), stat = err)
      if ( err == 0 ) then
         call LapackInterface_gelsy ( nA, mA, nRhs, Mat, nA, Rhs, ldB, Piv, &
                                      rc, rank, cwork, lwork, rwork, info )
         if ( info == 0 ) then
            lwork = int(cwork(1))
            deallocate(cwork) ; allocate(cwork(lwork), stat = err)
         else
            call stat%set ( UERROR, HERE, 'Returned by ?gelsy: The '//util_intTochar(info)//& 
                                          '-th argument had an illegal value' )             
            return         
         end if
      end if
      if ( err /= 0 ) then
         call stat%set ( IERROR, HERE, 'Allocation failure for rwork or cwork' ) 
         return
      end if      
      
      call LapackInterface_gelsy ( nA, mA, nRhs, Mat, nA, Rhs, ldB, Piv, &
                                   rc, rank, cwork, lwork, rwork, info )
      if ( info /= 0 ) then
         call stat%set ( UERROR, HERE, 'Returned by ?gelsy: The '//util_intTochar(-info)//& 
                                       '-th argument had an illegal value' )             
         return         
      end if

      call util_alloc ( 'a', X, int(mA,Ikind), int(nRhs,Ikind), stat )
      error_TraceNreturn(stat, HERE)

      do j = 1, nRhs
         do i = 1, mA
            X(i,j) = Rhs(i,j)
         end do
      end do          
      
   end if
   
   END SUBROUTINE util_SolvLinC2   
  
      
!=============================================================================================
   SUBROUTINE util_EigRsym ( Rmat, compute_eigvec, eigval, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_syheev
   real(Rkind), intent(in out) :: Rmat(:,:)
   logical    , intent(in    ) :: compute_eigvec   
   real(Rkind), intent(in out) :: eigval(:)
   type(err_t), intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Simple driver to LAPACK ?syev (ssyev or dsyev):
!
!  Computes the eigenvalues (eigval) and the eigenvectors (if compute_eigvec = .true.) of the
!  real symmetric matrix Rmat
!
!  Caution: . the caller must check the symmetry of Rmat
!           . Rmat is modified (and will contain the eigenvectors if requested)
!
!  Examples :  call util_EigRsym (A, .false., eigval, flag),   
!              call util_EigRsym (A, .true. , eigval, flag)
!
!  external routines:
!     . ?SYEV (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'util_EigRsym'          
   integer  (Ikind )              :: err
   character(len=99)              :: cnum
!- needed by LAPACK routine ?syev ------------------------------------------------------------
   integer                        :: n, lda, lwork, info
   real     (Rkind ), allocatable :: work(:)
   character(len=1 )              :: jobz
!---------------------------------------------------------------------------------------------   

   lda = size(Rmat,1) ; n = size(Rmat,2)
   
   if ( size(eigval) < n ) then
      call stat%set (UERROR, HERE, 'The size of "eigval" must be >= n = size(Rmat,2)')
      return
   end if
    
   jobz = 'N' ; if ( compute_eigvec ) jobz = 'V'   
!
!- workspace query:
!
   lwork =-1 ; info = 0

   call LapackInterface_syheev ( jobz, 'L', n, Rmat, lda, eigval, eigval, lwork, info )

   lwork = max((8*Rkind+2)*n,nint(eigval(1)))

   allocate(work(lwork), stat = err)
 
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array "work"')
      return
   end if   
!
!- eigenvalues (and eigenvectors) query:
!   
   call LapackInterface_syheev ( jobz, 'L', n, Rmat, lda, eigval, work, lwork, info )

   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call stat%set (UERROR, HERE,'The '//trim(cnum)//'-th argument had an illegal value.')    
      return  
   end if
   
   if ( info > 0 ) then
      write(cnum,'(i0)') info   
      call stat%set (UERROR, HERE, &                                           
              'The algorithm failed to converge; '//trim(cnum)//' off-diagonal' //     &
              'elements of an intermediate tridiagonal form did not converge to zero.')
      return
   end if  
      
   END SUBROUTINE util_EigRsym


!=============================================================================================
   SUBROUTINE util_EigCherm ( Cmat, compute_eigvec, eigval, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_syheev
   complex(Rkind), intent(in out) :: Cmat(:,:)
   logical       , intent(in    ) :: compute_eigvec   
   real   (Rkind), intent(in out) :: eigval(:)
   type   (err_t), intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Simple driver to LAPACK ?heev (cheev or zheev): 
!
!  Computes the eigenvalues (eigval) and the eigenvectors (if compute_eigvec = .true.) of the
!  complex hermitian matrix Cmat
!
!  Caution: . the caller must check that Cmat is hermitian
!           . Cmat is modified (and will contain the eigenvectors, if requested)
!
!  Examples :  call util_EigCherm (A, .false., eigval, flag),   
!              call util_EigCherm (A, .true. , eigval, flag)
!
!  external routines:
!     . ?HEEV (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = '(in util_EigCherm)'          
   integer  (Ikind )              :: err
   character(len=99)              :: cnum
!- needed by LAPACK routine ?heev: -----------------------------------------------------------
   integer                        :: n, lda, lwork, info
   real     (Rkind ), allocatable :: rwork(:)
   complex  (Rkind ), allocatable :: work(:)
   complex  (Rkind )              :: tmp(1)
   character(len=1 )              :: jobz
!---------------------------------------------------------------------------------------------   

   lda = size(Cmat,1) ; n = size(Cmat,2)
   
   if ( size(eigval) < n ) then
      call stat%set (UERROR, HERE, 'The size of "eigval" must be >= n = size(Cmat,2)')
      return
   end if
    
   jobz = 'N' ; if (compute_eigvec) jobz = 'V'  
!
!- workspace query:
!
   lwork =-1 ; info = 0
   
   allocate(rwork(3*n-2), stat = err) 
   
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array "rwork"')
      return
   end if         
     
   call LapackInterface_syheev ( jobz, 'L', n, Cmat, lda, eigval, tmp, lwork, rwork, info )

   lwork = max((8*Rkind+1)*n,nint(real(tmp(1))))

   allocate(work(lwork), stat = err)
 
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array "lwork"')
      return
   end if   
!
!- eigenvalues (and eigenvectors) query:
!   
   call LapackInterface_syheev ( jobz, 'L', n, Cmat, lda, eigval, work, lwork, rwork, info )

   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call stat%set (UERROR, HERE,'The '//trim(cnum)//'-th argument had an illegal value.')    
      return
   end if
   
   if ( info > 0 ) then
      write(cnum,'(i0)') info
      call stat%set (UERROR, HERE,                                                          &
                   'The algorithm failed to converge; '//trim(cnum)//' off-diagonal' //     & 
                   'elements of an intermediate tridiagonal form did not converge to zero.' )
      return
   end if  

   END SUBROUTINE util_EigCherm


!=============================================================================================
   SUBROUTINE util_EigRgen ( Rmat, wreal, wimag, stat, vright )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_geev
   real(Rkind),           intent(in out) :: Rmat(:,:)
   real(Rkind),           intent(in out) :: wreal(:), wimag(:)
   type(err_t),           intent(in out) :: stat
   real(Rkind), optional, intent(in out) :: vright(:,:) 
!---------------------------------------------------------------------------------------------
!  Simple driver to LAPACK ?geev (sgeev or dgeev): 
!
!  Computes the eigenvalues (wreal, wimag) and the right eigenvectors (if "vright" is present)
!  of the non-symmetric real square matrix Rmat. 
!  The right eigenvector v(j) of Rmat satisfies
!
!                         Rmat * v(j) = lambda(j) * v(j) 
!
!  where lambda(j) is its eigenvalue.
!
!  Notes (extract from dgeev):
!
!  . wreal and wimag contain the real and imaginary parts, respectively, of the computed 
!    eigenvalues.  Complex conjugate pairs of eigenvalues appear consecutively with the 
!    eigenvalue having the positive imaginary part first.
!
!  . The right eigenvectors v(j) are stored one after another in the columns of vright, in the
!    same order as their eigenvalues.
!
!    If the j-th eigenvalue is real, then 
!
!                     v(j)   = vright(:,j),  the j-th column of vright.
!
!    If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, then
!
!                     v(j)   = vright(:,j) + i*vright(:,j+1) 
!    and
!                     v(j+1) = vright(:,j) - i*vright(:,j+1).
!
!  Caution: on exit Rmat is modified
!  ---------------------------------
!
!  external routines:
!     . ?GEEV (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'util_EigRgen'          
   integer  (Ikind )              :: err
   character(len=99)              :: cnum
!- needed by LAPACK routine ?geev: -----------------------------------------------------------
   integer                        :: n, lda, ldvr, ldvl, lwork, info
   real     (Rkind ), allocatable :: vr(:,:), vl(:,:), work(:)
   character(len=1 )              :: jobvr, jobvl
!---------------------------------------------------------------------------------------------   

   lda = size(Rmat,1) ; n = size(Rmat,2) 
   
   if ( size(wreal) < n .or. size(wimag) < n ) then
      call stat%set(UERROR,HERE,'The size of "wreal" and "wimag" must be >= n = size(Rmat,2)')
      return
   end if
   
   jobvr = 'N'
   if ( present(vright) ) then
      ldvr = size(vright,1)
      if ( ldvr < n .or. size(vright,2) /= n ) then
         call stat%set (UERROR, HERE,                                                        &
                       'The shape of "vright" must be ldvr x n with ldvr >= n = size(Rmat,2)')
         return
      end if
      jobvr = 'V' 
   else
      ldvr = 1
      allocate(vr(ldvr,1))   
   end if       
   
   jobvl = 'N' ; ldvl = 1 ; allocate(vl(ldvl,1))        
!
!- workspace query:
!
   lwork =-1 ; info = 0

   if ( present(vright) ) then
      call LapackInterface_geev ( jobvl, jobvr, n, Rmat, lda, wreal, wimag, vl, ldvl, &
                                  vright, ldvr, wreal, lwork, info )
   else
      call LapackInterface_geev ( jobvl, jobvr, n, Rmat, lda, wreal, wimag, vl, ldvl, &
                                  vr, ldvr, wreal, lwork, info )
   end if
   
   lwork = max((8*Rkind+2)*n,nint(wreal(1)))
   
   allocate(work(lwork), stat = err)
 
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array work')
      return
   end if   
!
!- eigenvalues (and eigenvectors) query:
!   
   if ( present(vright) ) then
      call LapackInterface_geev ( jobvl, jobvr, n, Rmat, lda, wreal, wimag, vl, ldvl, &
                                  vright, ldvr, work, lwork, info )
   else
      call LapackInterface_geev ( jobvl, jobvr, n, Rmat, lda, wreal, wimag, vl, ldvl, &
                                  vr, ldvr, work, lwork, info )
   end if   
   
   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call stat%set (UERROR, HERE, 'The '//trim(cnum)//'-th argument had an illegal value.')    
      return
   end if
   
   if ( info > 0 ) then
      call stat%set (UERROR, HERE,                                               &
                     'The QR algorithm failed to compute all the eigenvalues,'// &
                     'and no eigenvectors have been computed'                    )          
      return
   end if     
      
   END SUBROUTINE util_EigRgen
   

!=============================================================================================
   SUBROUTINE util_EigCgen ( Cmat, eigval, stat, vright )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_geev
   complex(Rkind),           intent(in out) :: Cmat(:,:)
   complex(Rkind),           intent(in out) :: eigval(:)
   type   (err_t),           intent(in out) :: stat
   complex(Rkind), optional, intent(in out) :: vright(:,:)
!---------------------------------------------------------------------------------------------
!  Simple driver to LAPACK ?geev (cgeev or zgeev): 
!
!  Computes the eigenvalues ("eigval") and the right eigenvectors (if "vright" is present) of 
!  the non-symmetric complex square matrix Cmat. 
!
!  The right eigenvector v(j) of Cmat satisfies
!
!                         Cmat * v(j) = lambda(j) * v(j) 
!
!  where lambda(j) is its eigenvalue.
!
!  Caution: on exit Cmat is modified
!  ---------------------------------
!
!  external routines:
!     . ?GEEV (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'util_EigCgen'          
   integer  (Ikind )              :: err
   character(len=99)              :: cnum
!- needed by LAPACK routine ?geev: -----------------------------------------------------------
   integer                        :: n, lda, ldvr, ldvl, lwork, info
   real     (Rkind ), allocatable :: rwork(:)
   complex  (Rkind ), allocatable :: work(:), vr(:,:), vl(:,:)
   character(len=1 )              :: jobvr, jobvl
!---------------------------------------------------------------------------------------------   

   lda = size(Cmat,1) ; n = size(Cmat,2)
   
   if ( size(eigval) < n ) then
      call stat%set (UERROR, HERE, 'The size of "eigval" must be >= n = size(Cmat,2)')
      return
   end if
      
   jobvr = 'N'
   if ( present(vright) ) then
      ldvr = size(vright,1)
      if ( ldvr < n .or. size(vright,2) /= n ) then
         call stat%set (UERROR, HERE,                                                        &
                        'The size of "vright" must be ldvr x n with ldvr >= n = size(Cmat,2)')
         return
      end if
      jobvr = 'V' 
   else
      ldvr = 1
      allocate(vr(ldvr,1))   
   end if       
   
   jobvl = 'N' ; ldvl = 1 ; allocate(vl(ldvl,1))         
!
!- workspace query:
!
   lwork =-1 ; info = 0
   allocate(rwork(2*n), stat = err)   
   
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array rwork')
      return
   end if         
   
   if ( present(vright) ) then
      call LapackInterface_geev( jobvl, jobvr, n, Cmat, lda, eigval, vl, ldvl, vright, ldvr, &
                                 eigval, lwork, rwork, info )
   else
      call LapackInterface_geev( jobvl, jobvr, n, Cmat, lda, eigval, vl, ldvl, vr    , ldvr, &
                                 eigval, lwork, rwork, info )
   end if
      
   lwork = max((8*Rkind+1)*n,nint(real(eigval(1))))
   
   allocate(work(lwork), stat = err)
   
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array lwork')
      return
   end if   
!
!- eigenvalues (and eigenvectors) query:
!   
   if ( present(vright) ) then
      call LapackInterface_geev( jobvl, jobvr, n, Cmat, lda, eigval, vl, ldvl, vright, ldvr, &
                                 work, lwork, rwork, info )
   else
      call LapackInterface_geev( jobvl, jobvr, n, Cmat, lda, eigval, vl, ldvl, vr    , ldvr, &
                                 work, lwork, rwork, info ) 
   end if  

   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call stat%set (UERROR, HERE, 'The '//trim(cnum)//'-th argument had an illegal value.')  
      return  
   end if
   
   if ( info > 0 ) then
      call stat%set (UERROR, HERE,                                               &
                     'The QR algorithm failed to compute all the eigenvalues,'// &
                     'and no eigenvectors have been computed'                    )    
      return       
   end if  
   
   END SUBROUTINE util_EigCgen


!=============================================================================================
   SUBROUTINE util_SvdR ( A, S, stat, saveA, U, VT )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_gesvd
   real(Rkind), target,   intent(in out) :: A(:,:)
   real(Rkind),           intent(in out) :: S(:)
   type(err_t),           intent(in out) :: stat
   logical    , optional, intent(in    ) :: saveA
   real(Rkind), optional, intent(in out) :: U(:,:), VT(:,:)
!---------------------------------------------------------------------------------------------
!  Simple driver to LAPACK ?gesvd (sgesvd or dgesvd): 
!
!  If saveA is present and .true., A is unchanged (copy of A is needed)
!
!  Notes (extract from dgesvd):
!
!  Computes the singular value decomposition (SVD) of a real m-by-n matrix Rmat, optionally
!  computing the left and/or right singular vectors. The SVD is written
!                         A = U * SIGMA * transpose(V)
!  where SIGMA is an m-by-n matrix which is zero except for its min(m,n) diagonal elements, 
!  U is an m-by-m orthogonal matrix, and V is an n-by-n orthogonal matrix.  
!  The diagonal elements of SIGMA are the singular values of Rmat; they are real and 
!  non-negative, and are returned in descending order.  The first min(m,n) columns of
!  U and V are the left and right singular vectors of A.
!
!  Note that the routine returns V**T, not V.
!
!  external routines:
!     . ?GESVD (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'util_SvdR'          
   integer  (Ikind )              :: err
   character(len=99)              :: cnum
   logical                        :: saveA_
!- needed by LAPACK routine ?gesvd: ----------------------------------------------------------
   integer          , parameter   :: nb = 64
   integer                        :: m, n, lda, ldu, ldvt, lwork, info
   real     (Rkind )              :: udum(1,1), vdum(1,1), wdum(1)
   real     (Rkind ), allocatable :: work(:)
   real     (Rkind ), pointer     :: AA(:,:) => NULL()
   character(len=1 )              :: jobu, jobvt
!---------------------------------------------------------------------------------------------   

   m = size(A,1) ; n = size(A,2) ; lda = m ; ldu = m ; ldvt = n
   
   if ( size(S) < min(n,m) ) then
      call stat%set (UERROR,HERE,'The size of "S" must be >= min(shape(A))')
      return
   end if

   if ( present(U) ) then
      if ( size(U,1) < ldu .or. size(U,2) /= m ) then
         call stat%set (UERROR, HERE,                                              &
                       'The shape of "U" must be ldu x m with ldu >= m = size(A,1)')
         return
      end if
      jobu = 'A'
   else
      jobu = 'N'
   end if

   if ( present(VT) ) then
      if ( size(VT,1) < ldvt .or. size(VT,2) /= n ) then
         call stat%set (UERROR, HERE,                                                 &
                       'The shape of "VT" must be ldvt x n with ldvt >= n = size(A,2)')
         return
      end if
      jobvt = 'A'
   else
      jobvt = 'N'
   end if

   if ( present(saveA) ) then ; saveA_ = saveA ; else ; saveA_ = .false. ; end if
!
!- Work with a copy of the input matrix if saveA =.true.:
!
   if ( saveA_ ) then   
      allocate(AA, source = A)
   else
      AA => A
   end if
!
!- workspace query:
!
   lwork =-1 ; info = 0

   if ( present(U) .and. present(VT) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, U, ldu, VT, ldvt, &
                                   wdum, lwork, info )
   else if ( present(U) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, U, ldu, Vdum, ldvt, &
                                   wdum, lwork, info )
   else if ( present(VT) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, Udum, ldu, VT, ldvt, &
                                   wdum, lwork, info )
   else
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, Udum, ldu, vdum, ldvt, &
                                   wdum, lwork, info )
   end if

   lwork = max(m+4*n+nb*(m+n), nint(wdum(1)))
      
   allocate(work(lwork), stat = err)
 
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array work')
      if ( saveA_ ) then ; deallocate(AA) ; else ; nullify (AA) ; end if
      return
   end if   
!
!- singular values (and singular vectors) query:
!   
   if ( present(U) .and. present(VT) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, U, ldu, VT, ldvt, &
                                   work, lwork, info )
   else if ( present(U) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, U, ldu, Vdum, ldvt, &
                                   work, lwork, info )
   else if ( present(VT) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, Udum, ldu, VT, ldvt, &
                                   wdum, lwork, info )
   else
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, Udum, ldu, Vdum, ldvt, &
                                   work, lwork, info )
   end if  

   if ( saveA_ ) then ; deallocate(AA) ; else ; nullify (AA) ; end if
   
   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call stat%set (UERROR, HERE, 'The '//trim(cnum)//'-th argument had an illegal value.')    
      return
   end if
   
   if ( info > 0 ) then
      call stat%set (UERROR, HERE, 'The ?BDSQR algorithm failed to converge' )          
      return
   end if     
      
   END SUBROUTINE util_SvdR


!=============================================================================================
   SUBROUTINE util_SvdC ( A, S, stat, saveA, U, VT )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_gesvd
   complex(Rkind), target,   intent(in out) :: A(:,:)
   real   (Rkind),           intent(in out) :: S(:)
   type   (err_t),           intent(in out) :: stat
   logical       , optional, intent(in    ) :: saveA
   complex(Rkind), optional, intent(in out) :: U(:,:), VT(:,:)     
!---------------------------------------------------------------------------------------------
!  Simple driver to LAPACK ?gesvd (cgesvd or zgesvd): 
!
!  If saveA is present and .true., A is unchanged (copy of A is needed)
!
!  external routines:
!     . ?GESVD (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'util_SvdC'          
   integer  (Ikind )              :: err
   character(len=99)              :: cnum
   logical                        :: saveA_
!- needed by LAPACK routine ?gesvd: ----------------------------------------------------------
   integer          , parameter   :: nb = 64
   integer                        :: m, n, lda, ldu, ldvt, lwork, info
   complex  (Rkind )              :: udum(1,1), vdum(1,1), wdum(1)
   real     (Rkind ), allocatable :: rwork(:)
   complex  (Rkind ), allocatable :: work(:)
   complex  (Rkind ), pointer     :: AA(:,:) => NULL()
   character(len=1 )              :: jobu, jobvt
!---------------------------------------------------------------------------------------------   

   m = size(A,1) ; n = size(A,2) ; lda = m ; ldu = m ; ldvt = n
   
   if ( size(S) < min(n,m) ) then
      call stat%set (UERROR,HERE,'The size of "S" must be >= min(shape(A))')
      return
   end if

   if ( present(U) ) then
      if ( size(U,1) < ldu .or. size(U,2) /= m ) then
         call stat%set (UERROR, HERE,                                               &
                        'The shape of "U" must be ldu x m with ldu >= m = size(A,1)')
         return
      end if
      jobu = 'A'
   else
      jobu = 'N'
   end if

   if ( present(VT) ) then
      if ( size(VT,1) < ldvt .or. size(VT,2) /= n ) then
         call stat%set (UERROR, HERE,                                                 &
                       'The shape of "VT" must be ldvt x n with ldvt >= n = size(A,2)')
         return
      end if
      jobvt = 'A'
   else
      jobvt = 'N'
   end if

   allocate(rwork(5*min(m,n)), stat = err)
   
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array rwork')
      return
   end if   

   if ( present(saveA) ) then ; saveA_ = saveA ; else ; saveA_ = .false. ; end if
!
!- Work with a copy of the input matrix if saveA =.true.:
!
   if ( saveA_ ) then   
      allocate(AA, source = A)
   else
      AA => A
   end if
!
!- workspace query:
!
   lwork =-1 ; info = 0     
     
   if ( present(U) .and. present(VT) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, U, ldu, VT, ldvt, &
                                   wdum, lwork, rwork, info )
   else if ( present(U) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, U, ldu, Vdum, ldvt, &
                                   wdum, lwork, rwork, info )
   else if ( present(VT) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, Udum, ldu, VT, ldvt, &
                                   wdum, lwork, rwork, info )
   else
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, Udum, ldu, Vdum, ldvt, &
                                   wdum, lwork, rwork, info )
   end if

   lwork = max(m+4*n+nb*(m+n), int(wdum(1)))
      
   allocate(work(lwork), stat = err)
 
   if ( err /= 0 ) then
      call stat%set (IERROR, HERE, 'Allocation failure for array work')
      if ( saveA_ ) then ; deallocate(AA) ; else ; nullify (AA) ; end if
      return
   end if   
!
!- singular values (and singular vectors) query:
!   
   if ( present(U) .and. present(VT) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, U, ldu, VT, ldvt, &
                                   work, lwork, rwork, info )
   else if ( present(U) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, U, ldu, Vdum, ldvt, &
                                   work, lwork, rwork, info )
   else if ( present(VT) ) then
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, Udum, ldu, VT, ldvt, &
                                   work, lwork, rwork, info )
   else
      call LapackInterface_gesvd ( jobu, jobvt, m, n, AA, lda, S, Udum, ldu, Vdum, ldvt, &
                                   work, lwork, rwork, info )
   end if  

   if ( saveA_ ) then ; deallocate(AA) ; else ; nullify (AA) ; end if
   
   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call stat%set (UERROR, HERE, 'The '//trim(cnum)//'-th argument had an illegal value.')    
      return
   end if
   
   if ( info > 0 ) then
      call stat%set (UERROR, HERE, 'The ?BDSQR algorithm failed to converge' )          
      return
   end if     
      
   END SUBROUTINE util_SvdC
      

!=============================================================================================         
   SUBROUTINE util_PoldecI ( F, R, U, stat )
!============================================================================================= 
   integer(Ikind),           intent(in    ) :: F(:,:)
   real   (Rkind), optional, intent(in out) :: R(:,:), U(:,:)
   type   (err_t),           intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Computes the Polar decomposition of a real square matrix
!  Version using the SVD of F
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'util_PoldecR'
   real     (Rkind), allocatable :: SS(:), UU(:,:), VVt(:,:), FF(:,:)
   integer  (Ikind)              :: i, j, n, m, err 
!---------------------------------------------------------------------------------------------

#include "include/util_poldec.inc"  

   END SUBROUTINE util_PoldecI
   
    
!=============================================================================================         
   SUBROUTINE util_PoldecR ( F, R, U, stat )
!============================================================================================= 
   real   (Rkind),           intent(in    ) :: F(:,:)
   real   (Rkind), optional, intent(in out) :: R(:,:), U(:,:)
   type   (err_t),           intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Computes the Polar decomposition of a real square matrix
!  Version using the SVD of F
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'util_PoldecR'
   real     (Rkind), allocatable :: SS(:), UU(:,:), VVt(:,:), FF(:,:)
   integer  (Ikind)              :: i, j, n, m, err 
!---------------------------------------------------------------------------------------------

#include "include/util_poldec.inc"  
         
   END SUBROUTINE util_PoldecR
      

!=============================================================================================         
   SUBROUTINE util_PoldecC ( F, R, U, stat )
!============================================================================================= 
   complex(Rkind),           intent(in    ) :: F(:,:)
   complex(Rkind), optional, intent(in out) :: R(:,:), U(:,:)
   type   (err_t),           intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Computes the Polar decomposition of a real square matrix
!  Version using the SVD of F
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'util_PoldecC'
   real     (Rkind), allocatable :: SS(:)
   complex  (Rkind), allocatable :: UU(:,:), VVt(:,:), FF(:,:)
   integer  (Ikind)              :: i, j, n, m, err 
!---------------------------------------------------------------------------------------------


#include "include/util_poldec.inc" 
          
   END SUBROUTINE util_PoldecC   
   
   
!=============================================================================================         
   SUBROUTINE util_Trapeze ( ix, iy, rx, ry, res, stat )
!=============================================================================================         
   integer(Ikind), optional, intent(in    ) :: ix(:), iy(:)
   real   (Rkind), optional, intent(in    ) :: rx(:), ry(:)
   real   (Rkind),           intent(   out) :: res
   type   (err_t),           intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'util_Trapeze'            
   integer  (Ikind)            :: i, n, err 
!---------------------------------------------------------------------------------------------
 
   res = RZERO
   err = 0
   
   if ( present(ix) ) then
      n = size(ix)
      if ( present(iy) ) then
         if ( size(iy) /= n ) then
            err = 3
         else
            do i = 1, n-1
               res = res + HALF * ( ix(i+1)-ix(i) ) * ( iy(i)+iy(i+1) )
            end do
         end if
      else if ( present(ry) ) then
         if ( size(ry) /= n ) then
            err = 3
         else
            do i = 1, n-1
               res = res + HALF * ( ix(i+1)-ix(i) ) * ( ry(i)+ry(i+1) )
            end do
         end if
      else
         err = 2
      end if
   else if ( present(rx) ) then
      n = size(rx)
      if ( present(iy) ) then
         if ( size(iy) /= n ) then
            err = 3
         else
            do i = 1, n-1
               res = res + HALF * ( rx(i+1)-rx(i) ) * ( iy(i)+iy(i+1) )
            end do
         end if
      else if ( present(ry) ) then
         if ( size(ry) /= n ) then
            err = 3
         else      
            do i = 1, n-1
               res = res + HALF * ( rx(i+1)-rx(i) ) * ( ry(i)+ry(i+1) )
            end do
         end if
      else
         err = 1
      end if
   end if   
   
   if ( err == 0 ) then
      return
   else if ( err == 1 ) then
      call stat%set (UERROR, HERE, 'Missing ix or rx')
   else if ( err == 2 ) then
      call stat%set (UERROR, HERE, 'Missing iy or ry')
   else if ( err == 3 ) then
      call stat%set (UERROR, HERE, 'ix/rx and iy/ry must have the same size')
   end if
   
   END SUBROUTINE util_Trapeze
   

!=============================================================================================
   elemental FUNCTION util_sincR ( x ) result ( y )
!=============================================================================================
   real(Rkind), intent(in) :: x
   real(Rkind)             :: y
!---------------------------------------------------------------------------------------------       
!
!--------------------------------------------------------------------------------------------- 
      
!- local variables ---------------------------------------------------------------------------
   real(Rkind), parameter :: SIXTH = RONE / 6.0e0_Rkind, TWENTIETH = 0.05e0_Rkind
!---------------------------------------------------------------------------------------------       

   if ( abs(x) > EPS ) then
      y = sin(x) / x
   else
      y = RONE - SIXTH * x * ( x * ( RONE - TWENTIETH * x * x ) )
   end if
   
   END FUNCTION util_sincR
   

!=============================================================================================
   elemental FUNCTION util_sincC ( x ) result ( y )
!=============================================================================================
   complex(Rkind), intent(in) :: x
   complex(Rkind)             :: y
!---------------------------------------------------------------------------------------------       
!
!--------------------------------------------------------------------------------------------- 
      
!- local variables ---------------------------------------------------------------------------
   real(Rkind), parameter :: SIXTH = RONE / 6.0e0_Rkind, TWENTIETH = 0.05e0_Rkind
!---------------------------------------------------------------------------------------------       

   if ( abs(x) > EPS ) then
      y = sin(x) / x
   else
      y = RONE - SIXTH * x * ( x * ( RONE - TWENTIETH * x * x ) )
   end if
   
   END FUNCTION util_sincC
   

!=============================================================================================
   elemental FUNCTION util_sincI ( x ) result ( y )
!=============================================================================================
   integer(Ikind), intent(in) :: x
   real   (Rkind)             :: y
!---------------------------------------------------------------------------------------------       
!
!--------------------------------------------------------------------------------------------- 
      
!- local variables ---------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------       

   if ( x /= 0 ) then
      y = sin(real(x,Rkind)) / real(x,Rkind)
   else
      y = RONE
   end if
   
   END FUNCTION util_sincI
   
         
!=============================================================================================
   SUBROUTINE util_IntgPowIntg ( base, expo, res, typres )
!=============================================================================================
   use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_POSITIVE_INF   
   integer(Ikind), intent(in    ) :: base, expo
   real   (Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base and expo integers
!
!  Note: the result is stored in the REAL res (as it can overflow in integer arithmetics) but
!        if typres= ITYP means that res can be safely converted to an integer
!---------------------------------------------------------------------------------------------      

   if ( base == IZERO ) then
      if ( expo < 0 ) then
         ! Divide by zero (m^i with m = 0 and i < 0) => res = Inf:
         res = ieee_value(RONE, IEEE_POSITIVE_INF) ; typres = RTYP
      else if ( expo == IZERO ) then
         res = RONE  ; typres = ITYP
      else   
         res = RZERO ; typres = ITYP
      end if      
   else if ( base == IONE ) then
      res = RONE ; typres = ITYP
   else if ( expo == IZERO ) then
      res = RONE ; typres = ITYP
   else
      res = real(base,kind=Rkind) ** expo
      if ( expo >= IZERO .and. abs(res) < huge(Ikind) ) then
         typres = ITYP
      else 
         typres = RTYP
      end if                
   end if  
       
   END SUBROUTINE util_IntgPowIntg

!=============================================================================================
   SUBROUTINE util_RealPowIntg ( base, expo, res, typres )
!=============================================================================================
   real   (Rkind), intent(in    ) :: base
   integer(Ikind), intent(in    ) :: expo
   real   (Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base real and expo integer
!
!  Note: the result res is assumed REAL but if typres = ITYP (for some particular cases) means
!        that res can be safely converted to an integer
!--------------------------------------------------------------------------------------------- 
   
   if ( util_isIntg(base) ) then
      call util_IntgPowIntg ( int(base,Ikind), expo, res, typres )
   else if ( expo == IZERO ) then
      res = RONE ; typres = ITYP
   else
      res = base ** expo ; typres = RTYP 
   end if

   END SUBROUTINE util_RealPowIntg

!=============================================================================================
   SUBROUTINE util_CplxPowIntg ( base, expo, res, typres )
!=============================================================================================
   complex(Rkind), intent(in    ) :: base
   integer(Ikind), intent(in    ) :: expo
   complex(Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base complex and expo integer
!
!  Note: the result res is assumed COMPLEX but if typres = ITYP or if typres = RTYP (for some 
!        particular cases) means that res can be safely converted to an integer or to a real,
!        respectively
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind), parameter :: Two = 2*IONE     
   real   (Rkind)            :: rtmp
!--------------------------------------------------------------------------------------------- 
   
   if ( util_isReal(base) ) then
      call util_RealPowIntg ( real(base,Rkind), expo, rtmp, typres )
      res = rtmp
   else if ( expo == IZERO ) then
      res = CONE ; typres = ITYP
   else if ( real(base,Rkind) == RZERO ) then
      ! base is a pure imaginary number (base = i*b), compute b^expo:
      call util_RealPowIntg ( aimag(base), expo, rtmp, typres )
      if ( mod(expo,Two) == IZERO ) then
         ! expo is even, the result is real (res = +/- b^expo) :
         if ( mod(expo/Two,Two) == IZERO ) then
            res = rtmp
         else
            res =-rtmp
         end if
      else 
         !  expo is odd, the result is a pure imaginary number (res = +/- i * b^expo):
         typres = CTYP
         if ( mod((expo+IONE)/Two,Two) == IZERO ) then
            res =-CIMAG * rtmp
         else
            res = CIMAG * rtmp
         end if
      end if
   else
      ! all other cases:
      res = base ** expo ; typres = CTYP
   end if

   END SUBROUTINE util_CplxPowIntg

!=============================================================================================
   SUBROUTINE util_IntgPowReal ( base, expo, res, typres )
!=============================================================================================
   use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_POSITIVE_INF  
   integer(Ikind), intent(in    ) :: base
   real   (Rkind), intent(in    ) :: expo
   complex(Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base integer and expo real
!
!  Note: the result res is assumed COMPLEX but if typres = ITYP or if typres = RTYP (for some 
!        particular cases) means that res can be safely converted to an integer or to a real,
!        respectively
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------      
   real(Rkind) :: rtmp
!--------------------------------------------------------------------------------------------- 
      
   if ( base == IZERO ) then
      if ( expo < 0 ) then
         ! Divide by zero (m^r with m = 0 and r < 0) => res = Inf:
         res = ieee_value(RONE, IEEE_POSITIVE_INF) ; typres = RTYP
      else if ( expo == RZERO ) then
         res = CONE  ; typres = ITYP
      else   
         res = CZERO ; typres = ITYP
      end if      
   else if ( base == IONE ) then
      res = CONE ; typres = ITYP
   else if ( expo == RZERO ) then
      res = CONE ; typres = ITYP
   else if ( util_isIntg(expo) ) then
      ! expo is a whole number, compute base**int(expo):
      call util_IntgPowIntg ( base, int(expo,Ikind), rtmp, typres )
      res = rtmp
   else if ( base > IZERO ) then
      res = real(base,kind=Rkind) ** expo  ; typres = RTYP
   else
      res = cmplx(base,kind=Rkind) ** expo ; typres = CTYP
   end if
   
   END SUBROUTINE util_IntgPowReal

!=============================================================================================
   SUBROUTINE util_RealPowReal ( base, expo, res, typres )
!=============================================================================================
   real   (Rkind), intent(in    ) :: base, expo
   complex(Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base and expo reals
!
!  Note: the result res is assumed COMPLEX but if typres = ITYP or if typres = RTYP (for some 
!        particular cases) means that res can be safely converted to an integer or to a real,
!        respectively
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------      
   real(Rkind) :: rtmp
!--------------------------------------------------------------------------------------------- 

   if ( util_isIntg(base) ) then
      call util_IntgPowReal ( int(base,Ikind), expo, res, typres )
   else if ( util_isIntg(expo) ) then
      call util_RealPowIntg ( base, int(expo,Ikind), rtmp, typres )
      res = rtmp
   else if ( base > RZERO ) then
      res = base ** expo ; typres = RTYP
   else
      res = cmplx(base,kind=Rkind) ** expo ; typres = CTYP
   end if       
   
   END SUBROUTINE util_RealPowReal

!=============================================================================================
   SUBROUTINE util_CplxPowReal ( base, expo, res, typres )
!=============================================================================================
   complex(Rkind), intent(in    ) :: base
   real   (Rkind), intent(in    ) :: expo
   complex(Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base complex and expo real
!
!  Note: the result res is assumed COMPLEX but if typres = ITYP or if typres = RTYP (for some 
!        particular cases) means that res can be safely converted to an integer or to a real,
!        respectively
!--------------------------------------------------------------------------------------------- 
   
   if ( util_isReal(base) ) then
      call util_RealPowReal ( real(base,Rkind), expo, res, typres )
   else if ( util_isIntg(expo) ) then
      call util_CplxPowIntg ( base, int(expo,Ikind), res, typres )
   else
      res = base ** expo ; typres = CTYP
   end if       

   END SUBROUTINE util_CplxPowReal   

!=============================================================================================
   SUBROUTINE util_IntgPowCplx ( base, expo, res, typres )
!=============================================================================================
   use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_POSITIVE_INF
   integer(Ikind), intent(in    ) :: base
   complex(Rkind), intent(in    ) :: expo
   complex(Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base integer and expo complex
!
!  Note: the result res is assumed COMPLEX but if typres = ITYP or if typres = RTYP (for some 
!        particular cases) means that res can be safely converted to an integer or to a real,
!        respectively
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------      
   real(Rkind) :: rtmp
!--------------------------------------------------------------------------------------------- 

   if ( base == IZERO ) then
      if ( real(expo) < 0 ) then
         ! Divide by zero (m^c with m = 0 and real(c) < 0) => res = Inf:
         res = ieee_value(RONE, IEEE_POSITIVE_INF) ; typres = RTYP
      else if ( expo == CZERO ) then
         res = CONE  ; typres = ITYP
      else   
         res = CZERO ; typres = ITYP
      end if      
   else if ( base == IONE ) then
      res = CONE ; typres = ITYP 
   else if ( expo == CZERO ) then
      res = CONE ; typres = ITYP 
   else if ( util_isIntg(expo) ) then
      ! expo is a whole number, compute base**int(expo):
      call util_IntgPowIntg ( base, int(expo,Ikind), rtmp, typres )
      res = rtmp
   else if ( util_isReal(expo) ) then
      ! expo is a real number, compute base**real(expo):
      call util_IntgPowReal ( base, real(expo,Rkind), res, typres )
   else if ( base > IZERO ) then
      res = cmplx(base,kind=Rkind) ** expo ; typres = CTYP
   end if

   END SUBROUTINE util_IntgPowCplx

!=============================================================================================
   SUBROUTINE util_RealPowCplx ( base, expo, res, typres )
!=============================================================================================
   real   (Rkind), intent(in    ) :: base
   complex(Rkind), intent(in    ) :: expo
   complex(Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base and expo reals
!
!  Note: the result res is assumed COMPLEX but if typres = ITYP or if typres = RTYP (for some 
!        particular cases) means that res can be safely converted to an integer or to a real,
!        respectively
!--------------------------------------------------------------------------------------------- 

   if ( util_isIntg(base) ) then
      call util_IntgPowCplx ( int(base,Ikind), expo, res, typres )
   else if ( util_isReal(expo) ) then
      call util_RealPowReal ( base, real(expo,Rkind), res, typres )
   else 
      res = base ** expo ; typres = CTYP
   end if       
   
   END SUBROUTINE util_RealPowCplx

!=============================================================================================
   SUBROUTINE util_CplxPowCplx ( base, expo, res, typres )
!=============================================================================================
   complex(Rkind), intent(in    ) :: base, expo
   complex(Rkind), intent(   out) :: res
   integer       , intent(   out) :: typres
!---------------------------------------------------------------------------------------------       
!  Computes base**expo for base and expo complexes
!
!  Note: the result res is assumed COMPLEX but if typres = ITYP or if typres = RTYP (for some 
!        particular cases) means that res can be safely converted to an integer or to a real,
!        respectively
!--------------------------------------------------------------------------------------------- 

   if ( util_isReal(base) ) then
      call util_RealPowCplx ( real(base,Rkind), expo, res, typres )
   else if ( util_isReal(expo) ) then
      call util_CplxPowReal ( base, real(expo,Rkind), res, typres )
   else 
      res = base ** expo ; typres = CTYP
   end if       
   
   END SUBROUTINE util_CplxPowCplx
   
   
!=============================================================================================         
   SUBROUTINE util_ScalarPowScalar(res, typres, stat, ibase, rbase, cbase, iexpo, rexpo, cexpo)
!=============================================================================================
   complex(Rkind),           intent(   out) :: res
   integer(Ikind),           intent(   out) :: typres     
   type   (err_t),           intent(in out) :: stat          
   integer(Ikind), optional, intent(in    ) :: ibase, iexpo
   real   (Rkind), optional, intent(in    ) :: rbase, rexpo
   complex(Rkind), optional, intent(in    ) :: cbase, cexpo
!--------------------------------------------------------------------------------------------- 
!  Raises a number "?base" (? in {i, r, c}) to a number "?expo" (? in {i, r, c})
!  The result is returned into the complex number "res". Its actual type is "typres" and can
!  be ITYP (=1) for integer, RTYP (=2) for real and CTYP (=3) for complex.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'util_ScalarPowScalar'          
!--------------------------------------------------------------------------------------------- 

   if ( present(iexpo) ) then
!
!-    1) the exponent is integer
!            
      call util_ScalarPowI (iexpo, res, typres, stat, ibase, rbase, cbase)
                           
   else if ( present(rexpo) ) then
!
!-    2) the exponent is real
!
      call util_ScalarPowR (rexpo, res, typres, stat, ibase, rbase, cbase)
                        
   else if ( present(cexpo) ) then
!
!-    3) the exponent is complex
!
      call util_ScalarPowC (cexpo, res, typres, stat,  ibase, rbase, cbase)
   else
      call stat%set (UERROR,HERE,'Missing arg. "?expo" (must be integer, real or complex)')
      return
   end if 
      
   END SUBROUTINE util_ScalarPowScalar
   

!=============================================================================================         
   SUBROUTINE util_ScalarPowI (iexpo, res, typres, stat, ibase, rbase, cbase)
!=============================================================================================  
   integer(Ikind),           intent(in    ) :: iexpo
   complex(Rkind),           intent(   out) :: res
   integer(Ikind),           intent(   out) :: typres         
   type   (err_t),           intent(in out) :: stat 
   integer(Ikind), optional, intent(in    ) :: ibase
   real   (Rkind), optional, intent(in    ) :: rbase
   complex(Rkind), optional, intent(in    ) :: cbase
!--------------------------------------------------------------------------------------------- 
!  Raises a number "?base" (? in {i,r,c}) to an integer number "iexpo"
!  The result is returned into the complex number "res". Its actual type is "typres" and can
!  be ITYP (=1) for integer, RTYP (=2) for real and CTYP (=3) for complex.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'util_ScalarPowI'          
   real     (Rkind)            :: tmp
!--------------------------------------------------------------------------------------------- 
           
   res = CZERO; typres = IZERO
   
   if ( present(ibase) ) then
!   
!-    the base is integer
!         
      if ( ibase == IZERO ) then
         if ( iexpo < 0 ) then
            call stat%set (UERROR, HERE, 'Divide by zero (m^n with m = 0 and n < 0)')
            return
         else if ( iexpo == IZERO ) then
            res = IONE  ; typres = ITYP ; return
         else   
            res = IZERO ; typres = ITYP ; return
         end if      
      else
         tmp = real(ibase,kind=Rkind) ** iexpo
         res = tmp
         if ( iexpo >= IZERO .and. abs(tmp) < huge(Ikind) ) then
            typres = ITYP
         else
            typres = RTYP
         end if      
         
         !if (iexpo >= IZERO) then
         !   res = ibase ** iexpo ; typres = ITYP ; return
         !else
         !   res = real(ibase,kind=Rkind) ** iexpo ; typres = RTYP ; return
         !end if                   
      end if   
      
   else if ( present(rbase) ) then           
!
!-    the base is real
!           
      if ( rbase == RZERO ) then
         if ( iexpo < IZERO ) then
            call stat%set (UERROR, HERE, 'Divide by zero (r^n with r = 0.0 and n < 0)')
            return
         else if ( iexpo == IZERO ) then
            res = IONE ; typres = ITYP ; return
         else
            res = IZERO ; typres = ITYP ; return
         end if
      else         
         res = rbase ** iexpo
      end if   
      
   else if ( present(cbase) ) then
!
!-    the base is complex
!              
      if ( cbase == CZERO ) then
         if ( iexpo < IZERO ) then
            call stat%set (UERROR, HERE, 'Divide by zero (z^n with z = (0,0) and n < 0)')
            return
         else if ( iexpo == IZERO ) then
            res = IONE ; typres = ITYP ; return
         else
            res = IZERO ; typres = ITYP ; return
         end if
      else             
         res = cbase ** iexpo
      end if   

   else         
      call stat%set (UERROR,HERE,'Missing arg. "?base" (must be integer, real or complex)')
      return            
   end if          

   if ( aimag(res) /= 0 ) then
      typres = CTYP
   else if ( aint(real(res)) /= real(res) ) then
      typres = RTYP
   else
      if ( abs(real(res)) < huge(Ikind) ) then
         typres = ITYP
      else
         typres = RTYP
      end if      
   end if         
      
   END SUBROUTINE util_ScalarPowI


!=============================================================================================         
   SUBROUTINE util_ScalarPowR (rexpo, res, typres, stat, ibase, rbase, cbase )
!=============================================================================================            
   real   (Rkind),           intent(in    ) :: rexpo
   complex(Rkind),           intent(   out) :: res
   integer(Ikind),           intent(   out) :: typres
   type   (err_t),           intent(in out) :: stat
   integer(Ikind), optional, intent(in    ) :: ibase
   real   (Rkind), optional, intent(in    ) :: rbase
   complex(Rkind), optional, intent(in    ) :: cbase
!--------------------------------------------------------------------------------------------- 
!  Raises a number "?base" (? in {i,r,c}) to a real number "rexpo"
!  The result is returned into the complex number "res". Its actual type is "typres" and can
!  be ITYP (=1) for integer, RTYP (=2) for real and CTYP (=3) for complex.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'util_ScalarPowR'          
!--------------------------------------------------------------------------------------------- 
   
   res = CZERO ; typres = IZERO
   
   if ( present(ibase) ) then
!                  
!-    the base is integer
!                                 
      if ( ibase == IZERO ) then
         if ( rexpo < 0 ) then
            call stat%set (UERROR, HERE, 'Divide by zero (n^r with n = 0 and r < 0)')
            return
         else if ( rexpo == RZERO ) then
            res = CONE  ; typres = ITYP ; return
         else   
            res = CZERO ; typres = ITYP ; return
         end if      
      else if ( ibase > IZERO ) then
         res = ibase ** rexpo
      else
         res = cmplx(ibase,kind=Rkind) ** rexpo
      end if      

   else if ( present(rbase) ) then   
!
!-    the base is real
!               
      if ( rbase == RZERO ) then
         if ( rexpo < RZERO ) then
            call stat%set (UERROR, HERE, 'Divide by zero (s^r with s = 0.0 and r < 0)')
            return
         else if ( rexpo == RZERO ) then
            res = CONE  ; typres = ITYP ; return
         else   
            res = CZERO ; typres = ITYP ; return
         end if      
      else if ( rbase > RZERO ) then
         res = rbase ** rexpo
      else
         res = cmplx(rbase,kind=Rkind) ** rexpo
      end if       
      
   else if ( present(cbase) ) then   
!
!-    the base is complex                  
!
      if ( cbase == CZERO ) then
         if ( rexpo < RZERO ) then
            call stat%set (UERROR, HERE, 'Divide by zero (z^r with z = (0,0) and r < 0)')
            return
         else if ( rexpo == RZERO ) then
            res = CONE  ; typres = ITYP ; return
         else   
            res = CZERO ; typres = ITYP ; return
         end if      
      else      
         res = cbase ** rexpo
      end if   
   else
      call stat%set (UERROR,HERE,'Missing arg. "?base" (must be integer, real or complex)')
      return                  
   end if           
   
   if ( aimag(res) /= 0 ) then
      typres = CTYP
   else if ( aint(real(res)) /= real(res) .or. abs(real(res)) > real(IMAX-1,Rkind) ) then
      typres = RTYP
   else
      typres = ITYP
   end if            
    
   END SUBROUTINE util_ScalarPowR
   
   
!=============================================================================================         
   SUBROUTINE util_ScalarPowC (cexpo, res, typres, stat, ibase, rbase, cbase)
!=============================================================================================            
   complex(Rkind),           intent(in    ) :: cexpo
   complex(Rkind),           intent(   out) :: res
   integer(Ikind),           intent(   out) :: typres
   type   (err_t),           intent(in out) :: stat
   integer(Ikind), optional, intent(in    ) :: ibase
   real   (Rkind), optional, intent(in    ) :: rbase
   complex(Rkind), optional, intent(in    ) :: cbase
!--------------------------------------------------------------------------------------------- 
!  Raises a number "?base" (? in {i,r,c}) to a complex number "cexpo"
!  The result is returned into the complex number "res". Its actual type is "typres" and can
!  be ITYP (=1) for integer, RTYP (=2) for real and CTYP (=3) for complex.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'util_ScalarPowC'          
!--------------------------------------------------------------------------------------------- 
   
   res = CZERO ; typres = IZERO
   
   if ( present(ibase) ) then
!                  
!-    the base is integer
!                                 
      if ( ibase == IZERO ) then
         if ( real(cexpo) < 0 ) then
            call stat%set (UERROR, HERE,'Divide by zero (n^z with n = 0 and real(z) < 0)')
            return
         else if ( cexpo == CZERO ) then
            res = CONE  ; typres = ITYP ; return
         else   
            res = CZERO ; typres = ITYP ; return
         end if      
      else
         res = ibase ** cexpo
      end if

   else if ( present(rbase) ) then
!                  
!-    the base is real
!                               
      if ( rbase == RZERO ) then
         if ( real(cexpo) < 0 ) then
            call stat%set (UERROR, HERE, 'Divide by zero (r^z with r = 0.0 and real(z) < 0)')
            return
         else if ( cexpo == CZERO ) then
            res = CONE ; typres = ITYP ; return
         else
            res = CZERO ; typres = ITYP ; return
         end if
      else    
         res = rbase ** cexpo
      end if   
 
   else if ( present(cbase) ) then
!            
!-    the base is complex
!
      if ( cbase == CZERO ) then
         if ( real(cexpo) < 0 ) then
            call stat%set (UERROR, HERE, 'Divide by zero (c^z with c = (0,0) and Re(z) < 0)')
            return            
         else if ( cexpo == CZERO ) then
            res = CONE ; typres = ITYP ; return
         else
            res = CZERO ; typres = ITYP ; return
         end if
      else    
         res = cbase ** cexpo
      end if   
   else
      call stat%set (UERROR,HERE,'Missing arg. "?base" (must be integer, real or complex)')
      return
   end if    
 
   if ( aimag(res) /= 0 ) then
      typres = CTYP
   else if ( aint(real(res)) /= real(res) .or. abs(real(res)) > real(IMAX-1,Rkind) ) then
      typres = RTYP
   else
      typres = ITYP
   end if            
  
   END SUBROUTINE util_ScalarPowC    


!=============================================================================================         
   FUNCTION util_isAllocated ( x ) result( res )
!=============================================================================================         
   class(*), optional, intent(in) :: x(..)
   logical                        :: res
!--------------------------------------------------------------------------------------------- 
!  Returns .true. if x is allocated (works even when x is not an allocatable)
!  Examples:  returns .true. for util_isAllocI ( 1 ), util_isAllocI ( [1,2] ), ...
!
!  Solution given by mEm in: fortran-lang.discourse.group/t/
!  how-to-have-a-generic-interface-for-both-allocatable-and-non-allocatable-dummy/6114/17
!--------------------------------------------------------------------------------------------- 

   if ( present(x) ) then ; res = .true. ; else ; res = .false. ; end if

   END FUNCTION util_isAllocated
   
   
END MODULE util_Other_m

