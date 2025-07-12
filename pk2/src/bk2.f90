!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: bk2
!
! Description: 
! This module defines the DT bk2_t (for Base rank-2 array) and its extensions. 
! It serves to define the polymorphic component of pk2_t DT (see module pk2).
!---------------------------------------------------------------------------------------------
#include "error.fpp"

MODULE bk2_m

   use kindParameters_m
   use pk2Constants_m
   use util_m
      
   implicit none   
!
!- Error / warning handler:
!
   type(err_t) :: opflag
!
!- Base type for rank-2 arrays:
!   
   type :: bk2_t
      integer(Ikind) :: nrow = 0, ncol = 0, typ = EMPTY      
   contains
      procedure, pass :: affiche    => bk2_AfficheBk2
      procedure, pass :: PrintMe    => bk2_PrintBk2
      procedure, pass :: Destroy    => bk2_DestroyBk2  
      procedure, pass :: GetMat     => bk2_GetMatxBk2          
      procedure, pass :: Resize     => bk2_ResizevBk2      
      procedure, pass :: InsertInto => bk2_InsertBk2                                               
      procedure, pass :: SetSubmat  => bk2_SetSubmatBk2 
      procedure, pass :: CopySubmat => bk2_CopySubmatBk2      
      procedure, pass :: movealloc  => bk2_moveallocBk2 
      procedure, pass :: Is_symm    => bk2_IsSymmBk2 
      procedure, pass :: copyv      => bk2_CopyvBk2      
      procedure, pass :: transpose  => bk2_TransposeBk2  
      
      procedure, pass :: bk2_DelRowsBk2i32
      procedure, pass :: bk2_DelRowsBk2i64
      generic, public :: DelRows => bk2_DelRowsBk2i32, &
                                    bk2_DelRowsBk2i64                            
   end type bk2_t
!
!- Concret type for integer rank-2 array:   
!
   type, extends(bk2_t) :: ik2_t
      integer(Ikind), allocatable :: v(:,:)
   contains
      procedure, pass :: affiche    => bk2_AfficheIk2
      procedure, pass :: PrintMe    => bk2_PrintIk2
      procedure, pass :: destroy    => bk2_DestroyIk2
      procedure, pass :: Resize     => bk2_ResizevIk2
      procedure, pass :: InsertInto => bk2_InsertIk2   
      procedure, pass :: SetSubmat  => bk2_SetSubmatIk2 
      procedure, pass :: CopySubmat => bk2_CopySubmatIk2
      procedure, pass :: movealloc  => bk2_moveallocIk2
      procedure, pass :: Is_symm    => bk2_IsSymmIk2
      procedure, pass :: copyv      => bk2_CopyvIk2
      procedure, pass :: transpose  => bk2_TransposeIk2  
      procedure, pass :: bk2_DelRowsBk2i32 => bk2_DelRowsIk2i32
      procedure, pass :: bk2_DelRowsBk2i64 => bk2_DelRowsIk2i64
      !final :: bk2_finalizeIk2       
   end type ik2_t
!
!- Concret type for real rank-2 array:   
!   
   type, extends(bk2_t) :: rk2_t
      real(Rkind), allocatable :: v(:,:)
   contains   
      procedure, pass :: affiche    => bk2_AfficheRk2  
      procedure, pass :: PrintMe    => bk2_PrintRk2
      procedure, pass :: destroy    => bk2_DestroyRk2   
      procedure, pass :: Resize     => bk2_ResizevRk2 
      procedure, pass :: InsertInto => bk2_InsertRk2          
      procedure, pass :: SetSubmat  => bk2_SetSubmatRk2  
      procedure, pass :: CopySubmat => bk2_CopySubmatRk2      
      procedure, pass :: movealloc  => bk2_moveallocRk2
      procedure, pass :: Is_symm    => bk2_IsSymmRk2  
      procedure, pass :: copyv      => bk2_CopyvRk2  
      procedure, pass :: transpose  => bk2_TransposeRk2 
      procedure, pass :: bk2_DelRowsBk2i32 => bk2_DelRowsRk2i32
      procedure, pass :: bk2_DelRowsBk2i64 => bk2_DelRowsRk2i64
     ! final :: bk2_finalizeRk2                       
   end type rk2_t
!
!- Concret type for complex rank-2 array:   
!
   type, extends(bk2_t) :: ck2_t
      complex(Rkind), allocatable :: v(:,:)
   contains   
      procedure, pass :: affiche    => bk2_AfficheCk2  
      procedure, pass :: PrintMe    => bk2_PrintCk2
      procedure, pass :: destroy    => bk2_DestroyCk2
      procedure, pass :: Resize     => bk2_ResizevCk2                    
      procedure, pass :: InsertInto => bk2_InsertCk2          
      procedure, pass :: SetSubmat  => bk2_SetSubmatCk2  
      procedure, pass :: CopySubmat => bk2_CopySubmatCk2
      procedure, pass :: movealloc  => bk2_moveallocCk2     
      procedure, pass :: Is_symm    => bk2_IsSymmCk2
      procedure, pass :: copyv      => bk2_CopyvCk2 
      procedure, pass :: transpose  => bk2_TransposeCk2   
      procedure, pass :: bk2_DelRowsBk2i32 => bk2_DelRowsCk2i32
      procedure, pass :: bk2_DelRowsBk2i64 => bk2_DelRowsCk2i64
   end type ck2_t
!
!- Concret type for boolean rank-2 array:   
!
   type, extends(bk2_t) :: lk2_t
      logical, allocatable :: v(:,:)
   contains   
      procedure, pass :: affiche    => bk2_AfficheLk2 
      procedure, pass :: PrintMe    => bk2_PrintLk2
      procedure, pass :: destroy    => bk2_DestroyLk2        
      procedure, pass :: Resize     => bk2_ResizevLk2           
      procedure, pass :: InsertInto => bk2_InsertLk2                 
      procedure, pass :: SetSubmat  => bk2_SetSubmatLk2       
      procedure, pass :: CopySubmat => bk2_CopySubmatLk2
      procedure, pass :: movealloc  => bk2_moveallocLk2      
      procedure, pass :: Is_symm    => bk2_IsSymmLk2  
      procedure, pass :: copyv      => bk2_CopyvLk2 
      procedure, pass :: transpose  => bk2_TransposeLk2  
      procedure, pass :: bk2_DelRowsBk2i32 => bk2_DelRowsLk2i32
      procedure, pass :: bk2_DelRowsBk2i64 => bk2_DelRowsLk2i64
   end type lk2_t
!
!- Concret type for string rank-2 array:   
!
   type, extends(bk2_t) :: sk2_t
      type(str_t), allocatable :: v(:,:)
   contains   
      procedure, pass :: affiche    => bk2_AfficheSk2   
      procedure, pass :: PrintMe    => bk2_PrintSk2
      procedure, pass :: destroy    => bk2_DestroySk2 
      procedure, pass :: GetMatChar => bk2_GetMatxChar
      procedure, pass :: Resize     => bk2_ResizevSk2   
      procedure, pass :: InsertInto => bk2_InsertSk2              
      procedure, pass :: SetSubmat  => bk2_SetSubmatSk2       
      procedure, pass :: CopySubmat => bk2_CopySubmatSk2
      procedure, pass :: movealloc  => bk2_moveallocSk2      
      procedure, pass :: Is_symm    => bk2_IsSymmSk2                  
      procedure, pass :: copyv      => bk2_CopyvSk2
      procedure, pass :: transpose  => bk2_TransposeSk2   
      
      procedure, pass :: bk2_DelRowsBk2i32 => bk2_DelRowsSk2i32
      procedure, pass :: bk2_DelRowsBk2i64 => bk2_DelRowsSk2i64
   end type sk2_t
!
!- Defined constructors:
!   
   interface ik2_t
      module procedure bk2_ConstructorIk2
   end interface    
     
   interface rk2_t
      module procedure bk2_ConstructorRk2
   end interface      

   interface ck2_t
      module procedure bk2_ConstructorCk2
   end interface      

   interface lk2_t
      module procedure bk2_ConstructorLk2
   end interface       

   interface sk2_t
      module procedure bk2_ConstructorSk2
   end interface    
!
!- Assignment overload:
!   
   interface assignment(=)               ! normalement pas necessaires
      module procedure bk2_AssignFromIk2 ! mais si je ne les mets pas
      module procedure bk2_AssignFromRk2 ! j'observe du leakage avec gfortran
      module procedure bk2_AssignFromCk2 ! (mais pas avec ifort ni nagfor)
      module procedure bk2_AssignFromLk2 ! A ECLAIRCIR
      module procedure bk2_AssignFromSk2
   end interface
!
!- Generic move allocation procedures:
!   
   interface bk2_MoveAlloc
      module procedure bk2_moveallocBk2  
      module procedure bk2_moveallocImatToBk2
      module procedure bk2_moveallocRmatToBk2
      module procedure bk2_moveallocCmatToBk2
      module procedure bk2_moveallocLmatToBk2
      module procedure bk2_moveallocSmatToBk2
   end interface
!
!- Generic assignment procedures:
!   
   interface bk2_Assign
      module procedure bk2_AssignFromI32, bk2_AssignFromIvec32, bk2_AssignFromImat32, &
                       bk2_AssignFromI64, bk2_AssignFromIvec64, bk2_AssignFromImat64, &
                       bk2_AssignFromRsp, bk2_AssignFromRvec32, bk2_AssignFromRmat32, & 
                       bk2_AssignFromRdp, bk2_AssignFromRvec64, bk2_AssignFromRmat64, & 
                       bk2_AssignFromCsp, bk2_AssignFromCvec32, bk2_AssignFromCmat32, &
                       bk2_AssignFromCdp, bk2_AssignFromCvec64, bk2_AssignFromCmat64, &
                       bk2_AssignFromL  , bk2_AssignFromLvec  , bk2_AssignFromLmat  , &
                       bk2_AssignFromS  , bk2_AssignFromSvec  , bk2_AssignFromSmat  , &
                       bk2_AssignFromCH , bk2_AssignFromCHvec , bk2_AssignFromCHmat 
   end interface  
   
   type(ik2_t) :: dumIk2
   type(rk2_t) :: dumRk2
   type(ck2_t) :: dumCk2
   type(lk2_t) :: dumLk2
   type(sk2_t) :: dumSk2
      
CONTAINS

!=============================================================================================
   SUBROUTINE bk2_reallocIfNeeded ( a, typ, nrow, ncol )
!=============================================================================================
   class  (bk2_t), allocatable, intent(in out) :: a
   integer(Ikind),              intent(in    ) :: typ, nrow, ncol
!--------------------------------------------------------------------------------------------- 
!  Allocates or reallocates the object a if its type is not typ or if the shape of its member
!  %v is not [nrow, ncol]
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'bk2_reallocIfNeeded'
   logical                     :: allocate_a, allocate_v
   integer                     :: err
!--------------------------------------------------------------------------------------------- 

   allocate_a = .false. ; allocate_v = .false. ; err = 0
   
   select case  ( allocated(a) )
      case ( .true. )
         if ( a%typ /= typ ) then
            deallocate(a)
            allocate_a = .true.
            allocate_v = .true.
         else if ( a%nrow /= nrow .or. a%ncol /= ncol ) then
            allocate_v = .true.
         end if
      case ( .false. )
         allocate_a = .true.
         allocate_v = .true.
   end select 

! if ( .not. allocate_v ) then
!    print*,'bk2_reallocIfNeeded: NO allocation: the object is of the good type and good shape:',a%typ,a%nrow,a%ncol
! else if ( .not. allocate_a ) then
!    print*,'bk2_reallocIfNeeded: allocation of the container: [a%nrow,a%ncol]/=[nrow,ncol]',a%nrow,a%ncol,nrow,ncol
! else
!    if (allocated(a)) then
!       print*,'bk2_reallocIfNeeded: reallocation of the object: typ /= a%typ:',typ,a%typ
!     else
!       print*,'bk2_reallocIfNeeded: allocation of the object: typ /= a%typ:',typ,0
!     endif
! end if




   if ( allocate_a ) then
      select case ( typ )
         case ( ITYP )
            allocate(ik2_t :: a)
            select type ( a )
               type is ( ik2_t )
                  allocate(a%v(nrow,ncol), stat = err)
            end select
         case ( RTYP )
            allocate(rk2_t :: a)
            select type ( a )
               type is ( rk2_t )
                  allocate(a%v(nrow,ncol), stat = err)
            end select
         case ( CTYP )
            allocate(ck2_t :: a)
            select type ( a )
               type is ( ck2_t )
                  allocate(a%v(nrow,ncol), stat = err)
            end select
         case ( LTYP )
            allocate(lk2_t :: a)
            select type ( a )
               type is ( lk2_t )
                  allocate(a%v(nrow,ncol), stat = err)
            end select
         case ( STYP )
            allocate(sk2_t :: a)
            select type ( a )
               type is ( sk2_t )
               allocate(a%v(nrow,ncol), stat = err)
            end select
         case ( EMPTY )
            allocate(bk2_t :: a)
            a%typ = typ ; a%nrow = IZERO ; a%ncol = IZERO
            return
      end select
      
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure')
         return
      end if
 
      a%typ = typ ; a%nrow = nrow ; a%ncol = ncol
      
   else if ( allocate_v ) then
      select type ( a )
         type is ( ik2_t ) 
            if ( allocated(a%v) ) deallocate(a%v)
            allocate(a%v(nrow,ncol), stat = err)
         type is ( rk2_t )
            if ( allocated(a%v) ) deallocate(a%v)
            allocate(a%v(nrow,ncol), stat = err)
         type is ( ck2_t )
            if ( allocated(a%v) ) deallocate(a%v)
            allocate(a%v(nrow,ncol), stat = err)
         type is ( lk2_t )
            if ( allocated(a%v) ) deallocate(a%v)
            allocate(a%v(nrow,ncol), stat = err)
         type is ( sk2_t )
            if ( allocated(a%v) ) deallocate(a%v)
            allocate(a%v(nrow,ncol), stat = err)
         type is ( bk2_t )
            return
      end select

      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure')
         return
      end if
      
      a%nrow = nrow ; a%ncol = ncol
      
   end if
   
   END SUBROUTINE bk2_reallocIfNeeded
   
    
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Converting ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!  . bk2_ConvertToIk2: from lk2 to ik2, from rk2 to ik2
!  . bk2_ConvertToRk2: from lk2 to rk2, from ik2 to rk2
!  . bk2_ConvertToCk2: from lk2 to ck2, from ik2 to ck2, from rk2 to ck2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_ConvertToIk2 ( a )
!=============================================================================================
   class(bk2_t), allocatable, intent(in out) :: a
!--------------------------------------------------------------------------------------------- 
!  Converts from
!  . rk2_t to ik2_t  or
!  . ck2_t to ik2_t  or
!  . lk2_t to ik2_t
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_ConvertToIk2'
   integer  (Ikind), allocatable :: tmp(:,:) 
   character(len=:), allocatable :: msg
!--------------------------------------------------------------------------------------------- 

   if ( a%typ == ITYP ) return
   
   if ( a%typ == EMPTY .or. a%nrow == 0 .or. a%ncol == 0 ) then
      a = ik2_t()
      return
   end if
   
   select type (a)
      type is (rk2_t)
         tmp = int(a%v,kind=Ikind)
      type is (ck2_t)
         tmp = int(a%v,kind=Ikind)
      type is (lk2_t)  
         tmp = merge(tsource = IONE, fsource = IZERO, mask = a%v)
      class default
         if ( a%typ == STYP ) then
            call opflag%Set(UERROR,HERE,'Cannot convert from string to integer type')
            return
         end if
   end select
   
   if ( allocated(a) ) deallocate(a) ; allocate(ik2_t :: a)
   select type (a)
      type is (ik2_t)
         call move_alloc (from = tmp, to = a%v)
         a%typ = ITYP ; a%nrow = size(a%v,1) ; a%ncol = size(a%v,2)
   end select      
   
   END SUBROUTINE bk2_ConvertToIk2  


!=============================================================================================
   SUBROUTINE bk2_ConvertToRk2 ( a )
!=============================================================================================
   class(bk2_t), allocatable, intent(in out) :: a
!--------------------------------------------------------------------------------------------- 
!  Converts from
!  . ik2_t to rk2_t  or
!  . ck2_t to rk2_t  or
!  . lk2_t to rk2_t
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_ConvertToRk2'
   real     (Rkind), allocatable :: tmp(:,:) 
   character(len=:), allocatable :: msg
!--------------------------------------------------------------------------------------------- 

   if ( a%typ == RTYP ) return
   
   if ( a%typ == EMPTY .or. a%nrow == 0 .or. a%ncol == 0 ) then
      a = rk2_t()
      return
   end if
   
   select type (a)
      type is (ik2_t)
         tmp = real(a%v,kind=Rkind)
      type is (ck2_t)
         tmp = real(a%v,kind=Rkind)
      type is (lk2_t)
         tmp = merge(tsource = RONE, fsource = RZERO, mask = a%v)
      class default
         if ( a%typ == STYP ) then
            call opflag%Set(UERROR,HERE,'Cannot convert from string to real type')
            return
         end if
   end select
   
   if ( allocated(a) )  deallocate(a) ; allocate(rk2_t :: a)
   select type (a)
      type is (rk2_t)
         call move_alloc (from = tmp, to = a%v)
         a%typ = RTYP ; a%nrow = size(a%v,1) ; a%ncol = size(a%v,2)
   end select      
   
   END SUBROUTINE bk2_ConvertToRk2  


!=============================================================================================
   SUBROUTINE bk2_ConvertToCk2 ( a )
!=============================================================================================
   class(bk2_t), allocatable, intent(in out) :: a
!--------------------------------------------------------------------------------------------- 
!  Converts from 
!  . ik2_t to ck2_t  or 
!  . rk2_t to ck2_t  or
!  . lk2_t to ck2_t
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_ConvertToCk2'
   complex  (Rkind), allocatable :: tmp(:,:) 
!--------------------------------------------------------------------------------------------- 

   if ( a%typ == CTYP ) return
   
   if ( a%typ == EMPTY .or. a%nrow == 0 .or. a%ncol == 0 ) then
      a = ck2_t()
      return
   end if
   
   select type (a)
      type is (ik2_t)
         tmp = cmplx(a%v,kind=Rkind)
      type is (rk2_t)
         tmp = cmplx(a%v,kind=Rkind)
      type is (lk2_t)
         tmp = merge(tsource = CONE, fsource = CZERO, mask = a%v)
      class default
         if (a%typ == STYP) then
            call opflag%Set(UERROR,HERE,'Cannot convert from string to complex type')
            return
         end if         
   end select
   
   if ( allocated(a) ) deallocate(a) ; allocate(ck2_t :: a)
   select type (a)
      type is (ck2_t)
         call move_alloc (from = tmp, to = a%v)
         a%typ = CTYP ; a%nrow = size(a%v,1) ; a%ncol = size(a%v,2)
   end select      
      
   END SUBROUTINE bk2_ConvertToCk2             
         
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Transposition +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!  . bk2_TransposeBk2
!  . bk2_TransposeIk2
!  . bk2_TransposeRk2
!  . bk2_TransposeCk2
!  . bk2_TransposeLk2
!  . bk2_TransposeSk2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================   
   SUBROUTINE bk2_TransposeBk2 ( self )
!=============================================================================================      
   class(bk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  swaps self%nrow and self%ncol
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind) :: n
!---------------------------------------------------------------------------------------------

   n = self%nrow ; self%nrow = self%ncol ; self%ncol = n
   
   END SUBROUTINE bk2_TransposeBk2


!=============================================================================================   
   SUBROUTINE bk2_TransposeIk2 ( self )
!=============================================================================================      
   class(ik2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  swaps self%nrow and self%ncol and transposes self%v
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind)              :: n
   integer(Ikind), allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------

   n = self%nrow ; self%nrow = self%ncol ; self%ncol = n
   
   if ( allocated(self%v) ) then
      tmp = transpose(self%v) ; call move_alloc(from = tmp, to = self%v)
   end if
   
   END SUBROUTINE bk2_TransposeIk2


!=============================================================================================   
   SUBROUTINE bk2_TransposeRk2 ( self )
!=============================================================================================      
   class(rk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  swaps self%nrow and self%ncol and transposes self%v
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind)              :: n
   real   (Rkind), allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------

   n = self%nrow ; self%nrow = self%ncol ; self%ncol = n
   
   if ( allocated(self%v) ) then
      tmp = transpose(self%v) ; call move_alloc(from = tmp, to = self%v)
   end if
   
   END SUBROUTINE bk2_TransposeRk2


!=============================================================================================   
   SUBROUTINE bk2_TransposeCk2 ( self )
!=============================================================================================      
   class(ck2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  swaps self%nrow and self%ncol and transposes self%v
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind)              :: n
   complex(Rkind), allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------

   n = self%nrow ; self%nrow = self%ncol ; self%ncol = n
   
   if ( allocated(self%v) ) then
      tmp = transpose(self%v) ; call move_alloc(from = tmp, to = self%v)
   end if
   
   END SUBROUTINE bk2_TransposeCk2


!=============================================================================================   
   SUBROUTINE bk2_TransposeLk2 ( self )
!=============================================================================================      
   class(lk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  swaps self%nrow and self%ncol and transposes self%v
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind)              :: n
   logical       , allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------

   n = self%nrow ; self%nrow = self%ncol ; self%ncol = n
   
   if ( allocated(self%v) ) then
      tmp = transpose(self%v) ; call move_alloc(from = tmp, to = self%v)
   end if
   
   END SUBROUTINE bk2_TransposeLk2


!=============================================================================================   
   SUBROUTINE bk2_TransposeSk2 ( self )
!=============================================================================================      
   class(sk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  swaps self%nrow and self%ncol and transposes self%v
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind)              :: n
   type   (str_t), allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------

   n = self%nrow ; self%nrow = self%ncol ; self%ncol = n
   
   if ( allocated(self%v) ) then
      tmp = transpose(self%v) ; call move_alloc(from = tmp, to = self%v)
   end if
   
   END SUBROUTINE bk2_TransposeSk2
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Copy ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!  . bk2_CopyvBk2 (dummy)
!  . bk2_CopyvIk2
!  . bk2_CopyvRk2
!  . bk2_CopyvCk2
!  . bk2_CopyvLk2
!  . bk2_CopyvSk2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================   
   SUBROUTINE bk2_CopyvBk2 ( self, target ) 
!=============================================================================================      
   class(bk2_t), intent(in    ) :: self
   class(bk2_t), intent(in out) :: target
!---------------------------------------------------------------------------------------------
!  Dummy subroutine
!-----------------------------------------------------------------------------------R.H. 10/19

   END SUBROUTINE bk2_CopyvBk2


!=============================================================================================   
   SUBROUTINE bk2_CopyvIk2 ( self, target )  ! bk2 = ik2
!=============================================================================================    
   class(ik2_t), intent(in    ) :: self
   class(bk2_t), intent(in out) :: target
!---------------------------------------------------------------------------------------------
!  Makes a copy of self%v into target%v
!-----------------------------------------------------------------------------------R.H. 10/19

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_CopyvIk2'
!---------------------------------------------------------------------------------------------

   select type (target)
      type is (ik2_t)
         target%v = self%v 
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types')
         return
   end select
               
   END SUBROUTINE bk2_CopyvIk2


!=============================================================================================   
   SUBROUTINE bk2_CopyvRk2 ( self, target )  ! bk2 = rk2
!=============================================================================================    
   class(rk2_t), intent(in    ) :: self
   class(bk2_t), intent(in out) :: target
!---------------------------------------------------------------------------------------------
!  same as bk2_CopyvIk2
!-----------------------------------------------------------------------------------R.H. 10/19

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_CopyvRk2'
!---------------------------------------------------------------------------------------------

   select type (target)
      type is (rk2_t)
         target%v = self%v 
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types')
         return  
   end select
               
   END SUBROUTINE bk2_CopyvRk2
   

!=============================================================================================   
   SUBROUTINE bk2_CopyvCk2 ( self, target )  ! bk2 = ck2
!=============================================================================================    
   class(ck2_t), intent(in    ) :: self
   class(bk2_t), intent(in out) :: target
!---------------------------------------------------------------------------------------------
!  same as bk2_CopyvIk2
!-----------------------------------------------------------------------------------R.H. 10/19

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_CopyvCk2'
!---------------------------------------------------------------------------------------------

   select type (target)
      type is (ck2_t)
         target%v = self%v 
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types')
         return 
   end select
               
   END SUBROUTINE bk2_CopyvCk2


!=============================================================================================   
   SUBROUTINE bk2_CopyvLk2 ( self, target )  ! bk2 = lk2
!=============================================================================================    
   class(lk2_t), intent(in    ) :: self
   class(bk2_t), intent(in out) :: target
!---------------------------------------------------------------------------------------------
!  same as bk2_CopyvIk2
!-----------------------------------------------------------------------------------R.H. 10/19

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_CopyvLk2'
!---------------------------------------------------------------------------------------------

   select type (target)
      type is (lk2_t)
         target%v = self%v 
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types')
         return
   end select
               
   END SUBROUTINE bk2_CopyvLk2


!=============================================================================================   
   SUBROUTINE bk2_CopyvSk2 ( self, target )  ! bk2 = sk2
!=============================================================================================    
   class(sk2_t), intent(in    ) :: self
   class(bk2_t), intent(in out) :: target
!---------------------------------------------------------------------------------------------
!  same as bk2_CopyvIk2
!-----------------------------------------------------------------------------------R.H. 10/19

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_CopyvSk2'
!---------------------------------------------------------------------------------------------

   select type (target)
      type is (sk2_t)
         target%v = self%v 
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types')
         return
   end select
               
   END SUBROUTINE bk2_CopyvSk2

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Move allocation +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!  . bk2_moveallocBk2  (bk2 --> bk2)
!  . bk2_moveallocIk2  (ik2 --> bk2)
!  . bk2_moveallocRk2  (rk2 --> bk2)
!  . bk2_moveallocCk2  (ck2 --> bk2)
!  . bk2_moveallocLk2  (lk2 --> bk2)
!  . bk2_moveallocSk2  (sk2 --> bk2)
!  . bk2_moveallocImatToBk2 (Imat --> bk2)
!  . bk2_moveallocRmatToBk2 (Rmat --> bk2)
!  . bk2_moveallocCmatToBk2 (Cmat --> bk2)
!  . bk2_moveallocLmatToBk2 (Lmat --> bk2)
!  . bk2_moveallocSmatToBk2 (Smat --> bk2)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
   
!=============================================================================================   
   SUBROUTINE bk2_moveallocIk2 ( from, to )  ! ik2 --> bk2
!=============================================================================================      
   class(ik2_t),              intent(in out) :: from
   class(bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from "from%v" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------    
   logical :: allocate_to
!---------------------------------------------------------------------------------------------    

   allocate_to = .true.

   if ( allocated(to) ) then  
      if ( same_type_as(to,from) ) then
         allocate_to = .false.
      else
         deallocate(to)
      end if
   end if
   
   if ( allocate_to ) allocate(ik2_t::to)
   
   to%typ = from%typ ; to%nrow = from%nrow ; to%ncol = from%ncol
   
   select type (to)
      type is (ik2_t)
         call move_alloc (from=from%v, to=to%v)
   end select
         
   from%nrow = IZERO ; from%ncol = IZERO
   
   END SUBROUTINE bk2_moveallocIk2     
    

!=============================================================================================   
   SUBROUTINE bk2_moveallocRk2 ( from, to )  ! rk2 --> bk2
!=============================================================================================      
   class(rk2_t),              intent(in out) :: from
   class(bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from "from%v" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------    
   logical :: allocate_to
!---------------------------------------------------------------------------------------------    

   allocate_to = .true.

   if ( allocated(to) ) then
      if ( same_type_as(to,from) ) then
         allocate_to = .false.
      else
         deallocate(to)
      end if
   end if
   
   if ( allocate_to ) allocate(rk2_t::to)
   
   to%typ = from%typ ; to%nrow = from%nrow ; to%ncol = from%ncol
   
   select type (to)
      type is (rk2_t)
         call move_alloc (from=from%v, to=to%v)
   end select
         
   from%nrow = IZERO ; from%ncol = IZERO
   
   END SUBROUTINE bk2_moveallocRk2      
   

!=============================================================================================   
   SUBROUTINE bk2_moveallocCk2 ( from, to )  ! ck2 --> bk2
!=============================================================================================      
   class(ck2_t),              intent(in out) :: from
   class(bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from "from%v" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------    
   logical :: allocate_to
!---------------------------------------------------------------------------------------------    

   allocate_to = .true.

   if ( allocated(to) ) then
      if ( same_type_as(to,from) ) then
         allocate_to = .false.
      else
         deallocate(to)
      end if
   end if
   
   if ( allocate_to ) allocate(ck2_t::to)
   
   to%typ = from%typ ; to%nrow = from%nrow ; to%ncol = from%ncol
   
   select type (to)
      type is (ck2_t)
         call move_alloc (from=from%v, to=to%v)
   end select
         
   from%nrow = IZERO ; from%ncol = IZERO
   
   END SUBROUTINE bk2_moveallocCk2      
      
      
!=============================================================================================   
   SUBROUTINE bk2_moveallocLk2 ( from, to )  ! lk2 --> bk2
!=============================================================================================      
   class(lk2_t),              intent(in out) :: from
   class(bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from "from%v" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------    
   logical :: allocate_to
!---------------------------------------------------------------------------------------------    

   allocate_to = .true.

   if ( allocated(to) ) then
      if ( same_type_as(to,from) ) then
         allocate_to = .false.
      else
         deallocate(to)
      end if
   end if
   
   if ( allocate_to ) allocate(lk2_t::to)
   
   to%typ = from%typ ; to%nrow = from%nrow ; to%ncol = from%ncol
   
   select type (to)
      type is (lk2_t)
         call move_alloc (from=from%v, to=to%v)
   end select
         
   from%nrow = IZERO ; from%ncol = IZERO
   
   END SUBROUTINE bk2_moveallocLk2      
      

!=============================================================================================   
   SUBROUTINE bk2_moveallocSk2 ( from, to )  ! sk2 --> bk2
!=============================================================================================      
   class(sk2_t),              intent(in out) :: from
   class(bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from "from%v" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------    
   logical :: allocate_to
!---------------------------------------------------------------------------------------------    

   allocate_to = .true.
   
   if ( allocated(to) ) then
      if ( same_type_as(to,from) ) then
         allocate_to = .false.
      else
         deallocate(to)
      end if
   end if
   
   if ( allocate_to ) allocate(sk2_t::to)
   
   to%typ = from%typ ; to%nrow = from%nrow ; to%ncol = from%ncol
   
   select type (to)
      type is (sk2_t)
         call move_alloc (from=from%v, to=to%v)
   end select
         
   from%nrow = IZERO ; from%ncol = IZERO
   
   END SUBROUTINE bk2_moveallocSk2      
      

!=============================================================================================   
   SUBROUTINE bk2_moveallocBk2 ( from, to )  ! bk2 --> bk2
!=============================================================================================      
   class(bk2_t),              intent(in out) :: from
   class(bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from "from%v" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------    
   logical :: allocate_to
!---------------------------------------------------------------------------------------------    
   
   allocate_to = .true.
   
   if ( allocated(to) ) then
      if ( same_type_as(to,from) ) then
         allocate_to = .false.
      else
         deallocate(to)
      end if
   end if
      
   select type (from)
      type is (ik2_t)
         if ( allocate_to ) allocate(ik2_t::to)
         select type (to) ; type is (ik2_t) ; call move_alloc(from%v, to%v) ;end select      
      type is (rk2_t)
         if ( allocate_to ) allocate(rk2_t::to)
         select type (to) ; type is (rk2_t) ; call move_alloc(from%v, to%v) ;end select      
      type is (ck2_t)
         if ( allocate_to ) allocate(ck2_t::to)
         select type (to) ; type is (ck2_t) ; call move_alloc(from%v, to%v) ;end select      
      type is (lk2_t)
         if ( allocate_to ) allocate(lk2_t::to)
         select type (to) ; type is (lk2_t) ; call move_alloc(from%v, to%v) ;end select      
      type is (sk2_t)
         if ( allocate_to ) allocate(sk2_t::to)
         select type (to) ; type is (sk2_t) ; call move_alloc(from%v, to%v) ;end select 
      class default
         if ( allocated(to) ) deallocate(to)
         from%nrow = IZERO ; from%ncol = IZERO
         return
   end select      

   to%typ = from%typ ; to%nrow = from%nrow ; to%ncol = from%ncol

   from%nrow = IZERO ; from%ncol = IZERO
   
   END SUBROUTINE bk2_moveallocBk2     


!=============================================================================================   
   SUBROUTINE bk2_moveallocBk20 ( from, to )  ! bk2 --> bk2
!=============================================================================================      
   class(bk2_t),              intent(in out) :: from
   class(bk2_t), allocatable, intent(   out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation 
!-----------------------------------------------------------------------------------R.H. 12/18
      
   select type (from)
      type is (ik2_t)
         allocate(ik2_t::to)
         select type (to)
            type is (ik2_t)
               call move_alloc(from%v, to%v)
         end select      
      type is (rk2_t)
         allocate(rk2_t::to)
         select type (to)
            type is (rk2_t)
               call move_alloc(from%v, to%v)
         end select      
      type is (ck2_t)
         allocate(ck2_t::to)      
         select type (to)
            type is (ck2_t)
               call move_alloc(from%v, to%v)
         end select      
      type is (lk2_t)
         allocate(lk2_t::to)      
         select type (to)
            type is (lk2_t)
               call move_alloc(from%v, to%v)
         end select      
      type is (sk2_t)
         allocate(sk2_t::to)      
         select type (to)
            type is (sk2_t)
               call move_alloc(from%v, to%v)
         end select      

   end select      

   to%typ = from%typ ; to%nrow = from%nrow ; to%ncol = from%ncol
            
   from%typ = EMPTY ; from%nrow = IZERO ; from%ncol = IZERO
   
   END SUBROUTINE bk2_moveallocBk20   


!=============================================================================================   
   SUBROUTINE bk2_moveallocImatToBk2 ( from, to )  ! Imat --> bk2
!=============================================================================================     
   integer(Ikind), allocatable, intent(in out) :: from(:,:) 
   class  (bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from the integer matrix "from" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

   if ( allocated(to) ) then
      if ( .not. same_type_as(to,dumIk2) ) then
         deallocate(to) ; allocate(ik2_t::to)
      end if
   else
      allocate(ik2_t::to)
   end if
   
   to%typ = ITYP
   select type (to)
      type is (ik2_t)
         if ( allocated(from) ) then
            to%nrow = size(from,1) ; to%ncol = size(from,2)
            call move_alloc(from, to%v) 
         else
            to%nrow = 0 ; to%ncol = 0 
            if ( allocated(to%v) ) deallocate(to%v)
         end if
   end select  
         
   END SUBROUTINE bk2_moveallocImatToBk2


!=============================================================================================   
   SUBROUTINE bk2_moveallocRmatToBk2 ( from, to )  ! Rmat --> bk2
!=============================================================================================     
   real (Rkind), allocatable, intent(in out) :: from(:,:) 
   class(bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from the real matrix "from" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

   if ( allocated(to) ) then
      if ( .not. same_type_as(to,dumRk2) ) then
         deallocate(to) ; allocate(rk2_t::to)
      end if
   else
      allocate(rk2_t::to)
   end if
   
   to%typ = RTYP
   select type (to)
      type is (rk2_t)
         if ( allocated(from) ) then
            to%nrow = size(from,1) ; to%ncol = size(from,2)
            call move_alloc(from, to%v) 
         else
            to%nrow = 0 ; to%ncol = 0 
            if ( allocated(to%v) ) deallocate(to%v)
         end if
   end select  
         
   END SUBROUTINE bk2_moveallocRmatToBk2


!=============================================================================================   
   SUBROUTINE bk2_moveallocCmatToBk2 ( from, to )  ! Cmat --> bk2
!=============================================================================================     
   complex(Rkind), allocatable, intent(in out) :: from(:,:) 
   class  (bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from the complex matrix "from" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

   if ( allocated(to) ) then
      if ( .not. same_type_as(to,dumCk2) ) then
         deallocate(to) ; allocate(ck2_t::to)
      end if
   else
      allocate(ck2_t::to)
   end if
   
   to%typ = CTYP
   select type (to)
      type is (ck2_t)
         if ( allocated(from) ) then
            to%nrow = size(from,1) ; to%ncol = size(from,2)
            call move_alloc(from, to%v) 
         else
            to%nrow = 0 ; to%ncol = 0 
            if ( allocated(to%v) ) deallocate(to%v)
         end if
   end select  
         
   END SUBROUTINE bk2_moveallocCmatToBk2


!=============================================================================================   
   SUBROUTINE bk2_moveallocLmatToBk2 ( from, to )  ! Lmat --> bk2
!=============================================================================================     
   logical       , allocatable, intent(in out) :: from(:,:) 
   class  (bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from the logical matrix "from" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

   if ( allocated(to) ) then
      if ( .not. same_type_as(to,dumLk2) ) then
         deallocate(to) ; allocate(lk2_t::to)
      end if
   else
      allocate(lk2_t::to)
   end if
   
   to%typ = LTYP
   select type (to)
      type is (lk2_t)
         if ( allocated(from) ) then
            to%nrow = size(from,1) ; to%ncol = size(from,2)
            call move_alloc(from, to%v)
         else
            to%nrow = 0 ; to%ncol = 0 
            if ( allocated(to%v) ) deallocate(to%v)
         end if
   end select  
            
   END SUBROUTINE bk2_moveallocLmatToBk2
   

!=============================================================================================   
   SUBROUTINE bk2_moveallocSmatToBk2 ( from, to )  ! Smat --> bk2
!=============================================================================================     
   type (str_t), allocatable, intent(in out) :: from(:,:) 
   class(bk2_t), allocatable, intent(in out) :: to
!---------------------------------------------------------------------------------------------
!  Moves allocation from the string matrix "from" to "to%v"
!-----------------------------------------------------------------------------------R.H. 12/18

   if ( allocated(to) ) then
      if ( .not. same_type_as(to,dumSk2) ) then
         deallocate(to) ; allocate(sk2_t::to)
      end if
   else
      allocate(sk2_t::to)
   end if
   
   to%typ = STYP
   select type (to)
      type is (sk2_t)
         if ( allocated(from) ) then
            to%nrow = size(from,1) ; to%ncol = size(from,2)
            call move_alloc(from, to%v)
         else
            to%nrow = 0 ; to%ncol = 0 
            if ( allocated(to%v) ) deallocate(to%v)
         end if
   end select  
            
   END SUBROUTINE bk2_moveallocSmatToBk2
       
       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Assignments +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!  . bk2_AssignFromIk2  (bk2 <-- ik2)
!  . bk2_AssignFromRk2  (bk2 <-- rk2)
!  . bk2_AssignFromCk2  (bk2 <-- ck2)
!  . bk2_AssignFromLk2  (bk2 <-- lk2)
!  . bk2_AssignFromSk2  (bk2 <-- sk2)
!  . bk2_AssignFromIvec32, bk2_AssignFromIvec64 (bk2 <-- Ivec)
!  . bk2_AssignFromRvec32, bk2_AssignFromRvec64 (bk2 <-- Rvec)
!  . bk2_AssignFromCvec32, bk2_AssignFromCvec64 (bk2 <-- Cvec)
!  . bk2_AssignFromLvec                         (bk2 <-- Lvec)
!  . bk2_AssignFromSvec                         (bk2 <-- Svec)
!  . bk2_AssignFromCHvec                        (bk2 <-- CHvec)
!  . bk2_AssignFromImat32, bk2_AssignFromImat64 (bk2 <-- Imat)
!  . bk2_AssignFromRmat32, bk2_AssignFromRmat64 (bk2 <-- Rmat)
!  . bk2_AssignFromCmat32, bk2_AssignFromCmat64 (bk2 <-- Cmat)
!  . bk2_AssignFromLmat                         (bk2 <-- Lmat)
!  . bk2_AssignFromSmat                         (bk2 <-- Smat)
!  . bk2_AssignFromCHmat                        (bk2 <-- CHmat)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
!=============================================================================================   
   SUBROUTINE bk2_AssignFromIk2 ( lhs, rhs ) 
!=============================================================================================      
   type (ik2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- ik2_t
!
!  Note (10/19): when lhs is of the same type as rhs and when lhs%v is already allocated and 
!                has the same shape as rhs%v, overhead due to deallocation/allocation is 
!                avoided (call to rhs%Copyv).
!----------------------------------------------------------------------------R.H. 03/18, 10/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromIk2' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( .not. allocated(lhs) ) then
      allocate(lhs, source = rhs, stat = err)
      if ( err /= 0  ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure [1]')  
         return    
      end if
   end if
   
   select type (lhs)
      type is (ik2_t)      
!
!-       lhs is of the same type as rhs
!      
         if ( .not. allocated(rhs%v) ) then
!
!-          rhs%v is not allocated. Deallocate lhs%v if allocated:
!         
            if ( allocated(lhs%v) ) deallocate(lhs%v) ; lhs%nrow = 0 ; lhs%ncol = 0
            
            return
         end if   
         
         if ( lhs%nrow == rhs%nrow .and. lhs%ncol == rhs%ncol .and. allocated(lhs%v) ) then
!
!-          lhs%v has the same shape as rhs%v. Copy rhs%v into lhs%v:
!
            call rhs%Copyv (lhs)
            
            return
         end if
   end select
!
!- The types of lhs and rhs are not the same or their shapes are not the same:
!   
   deallocate(lhs) ; allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure [2]')
      return
   end if
   
   END SUBROUTINE bk2_AssignFromIk2


!=============================================================================================   
   SUBROUTINE bk2_AssignFromRk2 ( lhs, rhs ) 
!=============================================================================================      
   type (rk2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rk2_t
!----------------------------------------------------------------------------R.H. 03/18, 10/19
  
!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'bk2_AssignFromRk2' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   if ( .not. allocated(lhs) ) then
      allocate(lhs, source = rhs, stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure [1]')
         return
      end if
   end if
  
   select type (lhs)
      type is (rk2_t)
!
!-       lhs is of the same type as rhs
!      
         if ( .not. allocated(rhs%v) ) then
!
!-          rhs%v is not allocated. Deallocate lhs%v if allocated:
!         
            if ( allocated(lhs%v) ) deallocate(lhs%v) ; lhs%nrow = 0 ; lhs%ncol = 0
            
            return
         end if   
         
         if ( lhs%nrow == rhs%nrow .and. lhs%ncol == rhs%ncol .and. allocated(lhs%v) ) then
!
!-          lhs%v has the same shape as rhs%v. Copy rhs%v into lhs%v:
!
            call rhs%Copyv (lhs)
            
            return
         end if
   end select
!
!- The types of lhs and rhs are not the same or their shapes are not the same:
!   
   deallocate(lhs) ; allocate(lhs, source = rhs, stat = err)

   if ( err /=  0) then
      call opflag%Set(IERROR, HERE, 'Allocation failure [2]')
      return
   end if
            
   END SUBROUTINE bk2_AssignFromRk2
   

!=============================================================================================   
   SUBROUTINE bk2_AssignFromCk2 ( lhs, rhs ) 
!=============================================================================================      
   type (ck2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- ck2_t
!----------------------------------------------------------------------------R.H. 03/18, 10/19
  
!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_AssignFromCk2' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( .not. allocated(lhs) ) then
      allocate(lhs, source = rhs, stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure [1]')
         return
      end if
   end if
  
   select type (lhs)
      type is (ck2_t)
!
!-       lhs is of the same type as rhs
!            
         if ( .not. allocated(rhs%v) ) then
!
!-          rhs%v is not allocated. Deallocate lhs%v if allocated:
!         
            if ( allocated(lhs%v) ) deallocate(lhs%v) ; lhs%nrow = 0 ; lhs%ncol = 0
            
            return
         end if   
   
         if ( lhs%nrow == rhs%nrow .and. lhs%ncol == rhs%ncol .and. allocated(lhs%v) ) then
!
!-          lhs%v has the same shape as rhs%v. Copy rhs%v into lhs%v:
!
            call rhs%Copyv (lhs)
            
            return
         end if
   end select
!
!- The types of lhs and rhs are not the same or their shapes are not the same:
!   
   deallocate(lhs) ; allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure [2]')
      return
   end if
         
   END SUBROUTINE bk2_AssignFromCk2


!=============================================================================================   
   SUBROUTINE bk2_AssignFromLk2 ( lhs, rhs ) 
!=============================================================================================      
   type (lk2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- lk2_t
!----------------------------------------------------------------------------R.H. 03/18, 10/19
  
!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_AssignFromLk2' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( .not. allocated(lhs) ) then
      allocate(lhs, source = rhs, stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure [1]')
         return
      end if
   end if
  
   select type (lhs)
      type is (lk2_t)
!
!-       lhs is of the same type as rhs
!               
         if ( .not. allocated(rhs%v) ) then
!
!-          rhs%v is not allocated. Deallocate lhs%v if allocated:
!         
            if ( allocated(lhs%v))  deallocate(lhs%v) ; lhs%nrow = 0 ; lhs%ncol = 0
            
            return
         end if   

         if (lhs%nrow == rhs%nrow .and. lhs%ncol == rhs%ncol .and. allocated(lhs%v)) then
!
!-          lhs%v has the same shape as rhs%v. Copy rhs%v into lhs%v:
!
            call rhs%Copyv (lhs)
            
            return
         end if
   end select
!
!- The types of lhs and rhs are not the same or their shapes are not the same:
!   
   deallocate(lhs) ; allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure [2]')
      return
   end if
         
   END SUBROUTINE bk2_AssignFromLk2
   

!=============================================================================================   
   SUBROUTINE bk2_AssignFromSk2 ( lhs, rhs ) 
!=============================================================================================      
   type (sk2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- sk2_t
!----------------------------------------------------------------------------R.H. 03/18, 10/19
  
!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_AssignFromSk2' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( .not. allocated(lhs) ) then
      allocate(lhs, source = rhs, stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure [1]')
         return
      end if
   end if
  
   select type (lhs)
      type is (sk2_t)
!
!-       lhs is of the same type as rhs
!               
         if ( .not. allocated(rhs%v) ) then
!
!-          rhs%v is not allocated. Deallocate lhs%v if allocated:
!         
            if ( allocated(lhs%v) ) deallocate(lhs%v) ; lhs%nrow = 0 ; lhs%ncol = 0
            
            return
         end if   

         if ( lhs%nrow == rhs%nrow .and. lhs%ncol == rhs%ncol .and. allocated(lhs%v) ) then
!
!-          lhs%v has the same shape as rhs%v. Copy rhs%v into lhs%v:
!
            call rhs%Copyv (lhs)
            
            return
         
         end if
   end select
!
!- The types of lhs and rhs are not the same or their shapes are not the same:
!   
   deallocate(lhs) ; allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure [2]')
      return
   end if
         
   END SUBROUTINE bk2_AssignFromSk2
   

!=============================================================================================   
   SUBROUTINE bk2_AssignFromIk2_old ( lhs, rhs )
!=============================================================================================      
   type (ik2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(   out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- ik2_t
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter :: HERE = '(in bk2_AssignFromIk2)'       
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) call opflag%set(stat = IERROR, msg = HERE // ' Allocation failure')
   
   END SUBROUTINE bk2_AssignFromIk2_old
   
   
!=============================================================================================   
   SUBROUTINE bk2_AssignFromRk2_old ( lhs, rhs )
!=============================================================================================      
   type (rk2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(   out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rk2_t
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter :: HERE = '(in bk2_AssignFromRk2)'       
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------
   
   allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) call opflag%set(stat = IERROR, msg = HERE // ' Allocation failure')
   
   END SUBROUTINE bk2_AssignFromRk2_old
   
   
!=============================================================================================   
   SUBROUTINE bk2_AssignFromCk2_old ( lhs, rhs )
!=============================================================================================      
   type (ck2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(   out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- ck2_t
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter :: HERE = '(in bk2_AssignFromCk2)'       
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------
   
   allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) call opflag%set(stat = IERROR, msg = HERE // ' Allocation failure')
   
   END SUBROUTINE bk2_AssignFromCk2_old
   
   
!=============================================================================================   
   SUBROUTINE bk2_AssignFromLk2_old ( lhs, rhs )
!=============================================================================================      
   type (lk2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(   out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- lk2_t
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter :: HERE = '(in bk2_AssignFromLk2)'       
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------
   
   allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) call opflag%set(stat = IERROR, msg = HERE // ' Allocation failure')
   
   END SUBROUTINE bk2_AssignFromLk2_old


!=============================================================================================   
   SUBROUTINE bk2_AssignFromSk2_old ( lhs, rhs )
!=============================================================================================      
   type (sk2_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(   out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rk2_t
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter :: HERE = '(in bk2_AssignFromSk2)'       
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------   

   allocate(lhs, source = rhs, stat = err)

   if ( err /= 0 ) call opflag%set(stat = IERROR, msg = HERE // ' Allocation failure')
   
   END SUBROUTINE bk2_AssignFromSk2_old
   

!=============================================================================================   
   SUBROUTINE bk2_AssignFromI32 ( lhs, rhs ) 
!=============================================================================================      
   integer( i32 ),              intent(in    ) :: rhs
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- i
!
!  Note (12/19): when lhs is of the same type as rhs and when lhs%v is already allocated and 
!                has the same size as rhs (i.e. n x 1 matrix with n = size(rhs)) avoid 
!                overhead due to deallocation/allocation.
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromI32' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ik2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(1,1) = rhs
            
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ik2_t::lhs)
   select type (lhs)
      type is (ik2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then
            lhs%v(1,1) = rhs
            lhs%typ = ITYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromI32


!=============================================================================================   
   SUBROUTINE bk2_AssignFromI64 ( lhs, rhs ) 
!=============================================================================================      
   integer( i64 ),              intent(in    ) :: rhs
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- i
!  (see bk2_AssignFromI32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromI64' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ik2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(1,1) = rhs
               !if ( Ikind /= i64 ) call opflag%Set(WARNING, HERE, &
               !                   'Conversion from i'//i2a(i64)//' to i'//i2a(Ikind))                             
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ik2_t::lhs)
   select type (lhs)
      type is (ik2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then
            lhs%v(1,1) = rhs
            lhs%typ = ITYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
         !if ( Ikind /= i64 ) call opflag%Set(WARNING, HERE, &
         !                         'Conversion from i'//i2a(i64)//' to i'//i2a(Ikind))  
   end select
   
   END SUBROUTINE bk2_AssignFromI64


!=============================================================================================   
   SUBROUTINE bk2_AssignFromRsp ( lhs, rhs ) 
!=============================================================================================      
   real ( rSP ),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- r
!  (see bk2_AssignFromI32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromRsp' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (rk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(1,1) = rhs
            
               return
            end if
      end select
!
!-    lhs%v is not real or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(rk2_t::lhs)
   select type (lhs)
      type is (rk2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then
            lhs%v(1,1) = rhs
            lhs%typ = RTYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromRsp


!=============================================================================================   
   SUBROUTINE bk2_AssignFromRdp ( lhs, rhs ) 
!=============================================================================================      
   real ( rDP ),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- r
!  (see bk2_AssignFromI32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromRdp' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (rk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(1,1) = rhs
            
               return
            end if
      end select
!
!-    lhs%v is not real or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(rk2_t::lhs)
   select type (lhs)
      type is (rk2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then
            lhs%v(1,1) = rhs
            lhs%typ = RTYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromRdp


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCsp ( lhs, rhs ) 
!=============================================================================================      
   complex( rSP ),              intent(in    ) :: rhs
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- c
!  (see bk2_AssignFromI32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCsp' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ck2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(1,1) = rhs
            
               return
            end if
      end select
!
!-    lhs%v is not complex or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ck2_t::lhs)
   select type (lhs)
      type is (ck2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then
            lhs%v(1,1) = rhs
            lhs%typ = CTYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCsp


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCdp ( lhs, rhs ) 
!=============================================================================================      
   complex(rDP  ),              intent(in    ) :: rhs
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- c
!  (see bk2_AssignFromI32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCdp' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ck2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(1,1) = rhs
            
               return
            end if
      end select
!
!-    lhs%v is not complex or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ck2_t::lhs)
   select type (lhs)
      type is (ck2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then
            lhs%v(1,1) = rhs
            lhs%typ = CTYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCdp


!=============================================================================================   
   SUBROUTINE bk2_AssignFromL ( lhs, rhs ) 
!=============================================================================================      
   logical     ,              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- bool
!  (see bk2_AssignFromI32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromL' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (lk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(1,1) = rhs
            
               return
            end if
      end select
!
!-    lhs%v is not logical or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(lk2_t::lhs)
   select type (lhs)
      type is (lk2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then
            lhs%v(1,1) = rhs
            lhs%typ = LTYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromL


!=============================================================================================   
   SUBROUTINE bk2_AssignFromS ( lhs, rhs ) 
!=============================================================================================      
   type (str_t),              intent(in    ) :: rhs
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- string
!  (see bk2_AssignFromI32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromS' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (sk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               call lhs%v(1,1)%assign ( rhs = rhs ) ! RH 09/22
               !lhs%v(1,1) = rhs ! RH 09/22
               
!                if ( allocated(rhs%str) ) then
!                   lhs%v(1,1)%str = (rhs%str)
!                else
!                   lhs%v(1,1)%str = ''
!                end if
!                
            
               return
            end if
      end select
!
!-    lhs%v is not str_t or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(sk2_t::lhs)
   select type (lhs)
      type is (sk2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then

            call lhs%v(1,1)%assign ( rhs = rhs ) ! RH 09/22
            !lhs%v(1,1) = rhs ! RH 09/22
            
!             if (allocated(rhs%str)) then
!                lhs%v(1,1)%str = (rhs%str)
!             else
!                lhs%v(1,1)%str = ''
!             end if
!             
!             if ( rhs%has_color ) then ! RH 09/22
!                if ( allocated(rhs%strcol) ) lhs%v(1,1)%strcol = (rhs%strcol) 
!                lhs%v(1,1)%has_color = .true.
!             end if
            
            lhs%typ = STYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromS


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCH ( lhs, rhs ) 
!=============================================================================================      
   character(len=*),              intent(in    ) :: rhs
   class    (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- string
!  (see bk2_AssignFromI32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCH' 
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (sk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == 1 .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               call lhs%v(1,1)%assign ( rhs ) ! RH 09/22
               
               !lhs%v(1,1)%str = rhs
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not 1 x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(sk2_t::lhs)
   select type (lhs)
      type is (sk2_t)
         allocate(lhs%v(1,1), stat = err)
         if ( err == 0 ) then
            call lhs%v(1,1)%assign ( rhs ) ! RH 09/22
            !lhs%v(1,1)%str = rhs
            lhs%typ = STYP ; lhs%nrow = 1 ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCH
   

!=============================================================================================   
   SUBROUTINE bk2_AssignFromIvec32 ( lhs, rhs ) 
!=============================================================================================      
   integer(i32  ),              intent(in    ) :: rhs(:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- ivec
!
!  Note (12/19): when lhs is of the same type as rhs and when lhs%v is already allocated and 
!                has the same size as rhs (i.e. n x 1 matrix with n = size(rhs)) avoid 
!                overhead due to deallocation/allocation.
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromIvec32' 
   integer  (Ikind)            :: n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ik2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v )) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,1) = rhs(:)
            
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not n x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ik2_t::lhs)
   select type (lhs)
      type is (ik2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            lhs%v(:,1) = rhs(:)
            lhs%typ = ITYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromIvec32


!=============================================================================================   
   SUBROUTINE bk2_AssignFromIvec64 ( lhs, rhs ) 
!=============================================================================================      
   integer(i64  ),              intent(in    ) :: rhs(:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- ivec
!  (see bk2_AssignFromIvec32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromIvec64' 
   integer  (Ikind)            :: n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ik2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,1) = rhs(:)
            
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not n x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ik2_t::lhs)
   select type (lhs)
      type is (ik2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            lhs%v(:,1) = rhs(:)
            lhs%typ = ITYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromIvec64


!=============================================================================================   
   SUBROUTINE bk2_AssignFromRvec32 ( lhs, rhs ) 
!=============================================================================================      
   real (rSP  ),              intent(in    ) :: rhs(:)
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rvec
!  (see bk2_AssignFromIvec32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromRvec32' 
   integer  (Ikind)            :: n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (rk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,1) = rhs(:)
            
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not n x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(rk2_t::lhs)
   select type (lhs)
      type is (rk2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            lhs%v(:,1) = rhs(:)
            lhs%typ = RTYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromRvec32


!=============================================================================================   
   SUBROUTINE bk2_AssignFromRvec64 ( lhs, rhs ) 
!=============================================================================================      
   real (rDP  ),              intent(in    ) :: rhs(:)
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rvec
!  (see bk2_AssignFromIvec32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromRvec64' 
   integer  (Ikind)            :: n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (rk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,1) = rhs(:)
            
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not n x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(rk2_t::lhs)
   select type (lhs)
      type is (rk2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            lhs%v(:,1) = rhs(:)
            lhs%typ = RTYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromRvec64


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCvec32 ( lhs, rhs ) 
!=============================================================================================      
   complex(rSP  ),              intent(in    ) :: rhs(:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rvec
!  (see bk2_AssignFromIvec32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCvec32' 
   integer  (Ikind)            :: n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ck2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,1) = rhs(:)
            
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not n x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ck2_t::lhs)
   select type (lhs)
      type is (ck2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            lhs%v(:,1) = rhs(:)
            lhs%typ = CTYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCvec32


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCvec64 ( lhs, rhs ) 
!=============================================================================================      
   complex(rDP  ),              intent(in    ) :: rhs(:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rvec
!  (see bk2_AssignFromIvec32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCvec64' 
   integer  (Ikind)            :: n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ck2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,1) = rhs(:)
            
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not n x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ck2_t::lhs)
   select type (lhs)
      type is (ck2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            lhs%v(:,1) = rhs(:)
            lhs%typ = CTYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCvec64


!=============================================================================================   
   SUBROUTINE bk2_AssignFromLvec ( lhs, rhs ) 
!=============================================================================================      
   logical       ,              intent(in    ) :: rhs(:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rvec
!  (see bk2_AssignFromIvec32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromLvec' 
   integer  (Ikind)            :: n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (lk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,1) = rhs(:)
            
               return
            end if
      end select
!
!-    lhs%v is not integer or its shape is not n x 1. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(lk2_t::lhs)
   select type (lhs)
      type is (lk2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            lhs%v(:,1) = rhs(:)
            lhs%typ = LTYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromLvec   


!=============================================================================================   
   SUBROUTINE bk2_AssignFromSvec ( lhs, rhs ) 
!=============================================================================================      
   type (str_t),              intent(in    ) :: rhs(:)
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- svec
!  (see bk2_AssignFromIvec32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromSvec' 
   integer  (Ikind)            :: i, n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (sk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               do i = 1, n
                  
                  call lhs%v(i,1)%assign ( rhs(i) ) ! RH 09/22
!                   if ( allocated(rhs(i)%str) ) then
!                      lhs%v(i,1)%str = (rhs(i)%str)
!                   else
!                      lhs%v(i,1)%str = ''
!                   end if
!                   
!                   if ( rhs(i)%has_color ) then ! RH 09/22
!                      if ( allocated(rhs(i)%strcol) ) lhs%v(i,1)%strcol = (rhs(i)%strcol) 
!                      lhs%v(i,1)%has_color = .true.
!                   else
!                      lhs%v(i,1)%has_color = .false.
!                   end if
                  
               end do
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(sk2_t::lhs)
   select type (lhs)
      type is (sk2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            do i = 1, n
            
               call lhs%v(i,1)%assign ( rhs(i) ) ! RH 09/22
               
!                if ( allocated(rhs(i)%str) ) then
!                   lhs%v(i,1)%str = (rhs(i)%str)
!                else
!                   lhs%v(i,1)%str = ''
!                end if   
!                
!                if ( rhs(i)%has_color ) then ! RH 09/22
!                   if ( allocated(rhs(i)%strcol) ) lhs%v(i,1)%strcol = (rhs(i)%strcol) 
!                   lhs%v(i,1)%has_color = .true.
!                end if
            
            end do   
            lhs%typ = STYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromSvec


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCHvec ( lhs, rhs ) 
!=============================================================================================      
   character(len=*),              intent(in    ) :: rhs(:)
   class    (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- Chvec
!  (see bk2_AssignFromIvec32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCHvec' 
   integer  (Ikind)            :: i, n, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs) 
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (sk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == 1 .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               do i = 1, n
                  lhs%v(i,1)%str = rhs(i)
                  lhs%v(i,1)%has_color = .false.
               end do
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(sk2_t::lhs)
   select type (lhs)
      type is (sk2_t)
         allocate(lhs%v(n,1), stat = err)
         if ( err == 0 ) then
            do i = 1, n
               lhs%v(i,1)%str = rhs(i)
            end do
            lhs%typ = STYP ; lhs%nrow = n ; lhs%ncol = 1
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCHvec


!=============================================================================================   
   SUBROUTINE bk2_AssignFromImat32 ( lhs, rhs ) 
!=============================================================================================      
   integer(i32  ),              intent(in    ) :: rhs(:,:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- imat
!
!  Note (10/19): when lhs is of the same type as rhs and when lhs%v is already allocated and 
!                has the same shape as rhs avoid overhead due to deallocation/allocation.
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromImat32' 
   integer  (Ikind)            :: n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ik2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,:) = rhs(:,:)
            
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ik2_t::lhs)
   select type (lhs)
      type is (ik2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            lhs%v(:,:) = rhs(:,:)
            lhs%typ = ITYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromImat32
   

!=============================================================================================   
   SUBROUTINE bk2_AssignFromImat64 ( lhs, rhs ) 
!=============================================================================================      
   integer(i64  ),              intent(in    ) :: rhs(:,:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- imat
!  (see bk2_AssignFromImat32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromImat64' 
   integer  (Ikind)            :: n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ik2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,:) = rhs(:,:)
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ik2_t::lhs)
   select type (lhs)
      type is (ik2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            lhs%v(:,:) = rhs(:,:)
            lhs%typ = ITYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromImat64


!=============================================================================================   
   SUBROUTINE bk2_AssignFromRmat32 ( lhs, rhs ) 
!=============================================================================================      
   real (rSP  ),              intent(in    ) :: rhs(:,:)
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rmat
!  (see bk2_AssignFromImat32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromRmat32' 
   integer  (Ikind)            :: n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (rk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,:) = rhs(:,:)
            
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(rk2_t::lhs)
   select type (lhs)
      type is (rk2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            lhs%v(:,:) = rhs(:,:)
            lhs%typ = RTYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromRmat32


!=============================================================================================   
   SUBROUTINE bk2_AssignFromRmat64 ( lhs, rhs ) 
!=============================================================================================      
   real (rDP  ),              intent(in    ) :: rhs(:,:)
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- rmat
!  (see bk2_AssignFromImat32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromRmat64' 
   integer  (Ikind)            :: n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (rk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,:) = rhs(:,:)
            
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(rk2_t::lhs)
   select type (lhs)
      type is (rk2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            lhs%v(:,:) = rhs(:,:)
            lhs%typ = RTYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromRmat64


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCmat32 ( lhs, rhs ) 
!=============================================================================================      
   complex(rSP  ),              intent(in    ) :: rhs(:,:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- cmat
!  (see bk2_AssignFromImat32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCmat32' 
   integer  (Ikind)            :: n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ck2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,:) = rhs(:,:)
            
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ck2_t::lhs)
   select type (lhs)
      type is (ck2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            lhs%v(:,:) = rhs(:,:)
            lhs%typ = CTYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCmat32


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCmat64 ( lhs, rhs ) 
!=============================================================================================      
   complex(rDP  ),              intent(in    ) :: rhs(:,:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- cmat
!  (see bk2_AssignFromImat32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCmat64' 
   integer  (Ikind)            :: n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (ck2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,:) = rhs(:,:)
            
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(ck2_t::lhs)
   select type (lhs)
      type is (ck2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            lhs%v(:,:) = rhs(:,:)
            lhs%typ = CTYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCmat64
   

!=============================================================================================   
   SUBROUTINE bk2_AssignFromLmat ( lhs, rhs ) 
!=============================================================================================      
   logical       ,              intent(in    ) :: rhs(:,:)
   class  (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- lmat
!  (see bk2_AssignFromImat32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromLmat' 
   integer  (Ikind)            :: n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (lk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               lhs%v(:,:) = rhs(:,:)
            
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(lk2_t::lhs)
   select type (lhs)
      type is (lk2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            lhs%v(:,:) = rhs(:,:)
            lhs%typ = LTYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromLmat


!=============================================================================================   
   SUBROUTINE bk2_AssignFromSmat ( lhs, rhs ) 
!=============================================================================================      
   type (str_t),              intent(in    ) :: rhs(:,:)
   class(bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- smat
!  (see bk2_AssignFromImat32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromSmat' 
   integer  (Ikind)            :: i, j, n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (sk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               do j = 1, m
                  do i = 1, n
                     
                     call lhs%v(i,j)%assign ( rhs(i,j) ) ! RH 09/22
                     
!                      if ( allocated(rhs(i,j)%str) ) then
!                         lhs%v(i,j)%str = (rhs(i,j)%str)
!                      else
!                         lhs%v(i,j)%str = ''
!                      end if
!                      
!                      if ( lhs%v(i,j)%has_color ) then ! RH 09/22
!                         if ( allocated(rhs(i,j)%strcol) ) lhs%v(i,j)%strcol = (rhs(i,j)%strcol) 
!                         lhs%v(i,j)%has_color = .true.
!                      else
!                         lhs%v(i,j)%has_color = .false.
!                      end if
                  
                  end do
               end do
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(sk2_t::lhs)
   select type (lhs)
      type is (sk2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            do j = 1, m
               do i = 1, n
                  
                  call lhs%v(i,j)%assign ( rhs(i,j) ) ! RH 09/22
!                   if ( allocated(rhs(i,j)%str) ) then
!                      lhs%v(i,j)%str = (rhs(i,j)%str)
!                   else
!                      lhs%v(i,j)%str = ''
!                   end if
! 
!                   if ( lhs%v(i,j)%has_color ) then ! RH 09/22
!                      if ( allocated(rhs(i,j)%strcol) ) lhs%v(i,j)%strcol = (rhs(i,j)%strcol) 
!                      lhs%v(i,j)%has_color = .true.
!                   end if
                  
               end do
            end do   
            lhs%typ = STYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromSmat


!=============================================================================================   
   SUBROUTINE bk2_AssignFromCHmat ( lhs, rhs ) 
!=============================================================================================      
   character(len=*),              intent(in    ) :: rhs(:,:)
   class    (bk2_t), allocatable, intent(in out) :: lhs
!---------------------------------------------------------------------------------------------
!  Assignment bk2_t <-- chmat
!  (see bk2_AssignFromImat32)
!-----------------------------------------------------------------------------------R.H. 12/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'bk2_AssignFromCHmat' 
   integer  (Ikind)            :: i,j, n, m, err
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   n = size(rhs,1) ; m = size(rhs,2)
   
   if ( allocated(lhs) ) then
      select type (lhs)
         type is (sk2_t)      
!
!-          lhs%v and rhs are of the same type
!               
            if ( lhs%nrow == n .and. lhs%ncol == m .and. allocated(lhs%v) ) then
!
!-             and are of the same shape. Copy rhs into lhs%v:
!
               do j = 1, m
                  do i = 1, n
                     call lhs%v(i,j)%assign ( rhs(i,j) ) ! RH 09/22
                  end do
               end do
               return
            end if
      end select
!
!-    the type of lhs%v or its shape differ from those of rhs. Deallocate it:
! 
      deallocate(lhs)
   end if
   
   allocate(sk2_t::lhs)
   select type (lhs)
      type is (sk2_t)
         allocate(lhs%v(n,m), stat = err)
         if ( err == 0 ) then
            do j = 1, m
               do i = 1, n
                  call lhs%v(i,j)%assign ( rhs(i,j) ) ! RH 09/22
               end do
            end do   
            lhs%typ = STYP ; lhs%nrow = n ; lhs%ncol = m
         else   
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
   end select
   
   END SUBROUTINE bk2_AssignFromCHmat
   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Finalizers ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!  . bk2_finalizeIk2 
!  . bk2_finalizeRk2  
!  . bk2_finalizeCk2 
!  . bk2_finalizeLk2 
!  . bk2_finalizeSk2 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
!=============================================================================================
   SUBROUTINE bk2_finalizeIk2 ( self ) ! utile ?
!=============================================================================================
   type(ik2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------
   
   if ( allocated(self%v) ) deallocate(self%v)
   
   END SUBROUTINE bk2_finalizeIk2


!=============================================================================================
   SUBROUTINE bk2_finalizeRk2 ( self ) ! utile ?
!=============================================================================================
   type(rk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------

   if ( allocated(self%v) ) deallocate(self%v)
   
   END SUBROUTINE bk2_finalizeRk2
   

!=============================================================================================
   SUBROUTINE bk2_finalizeCk2 ( self ) ! utile ?
!=============================================================================================
   type(ck2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------

   if ( allocated(self%v) ) deallocate(self%v)
   
   END SUBROUTINE bk2_finalizeCk2


!=============================================================================================
   SUBROUTINE bk2_finalizeLk2 ( self ) ! utile ?
!=============================================================================================
   type(lk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------

   if ( allocated(self%v) ) deallocate(self%v)
   
   END SUBROUTINE bk2_finalizeLk2


!=============================================================================================
   SUBROUTINE bk2_finalizeSk2 ( self ) ! utile ?
!=============================================================================================
   type(lk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------

   if ( allocated(self%v) ) deallocate(self%v)
   
   END SUBROUTINE bk2_finalizeSk2


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Symmetry check ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_IsSymmIk2
! . bk2_IsSymmRk2
! . bk2_IsSymmCk2
! . bk2_IsSymmLk2
! . bk2_IsSymmSk2
! . bk2_IsSymmBk2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
      
!=============================================================================================   
   SUBROUTINE bk2_IsSymmIk2 ( self, is_sym ) 
!=============================================================================================   
   class  (ik2_t), intent(in    ) :: self
   logical       , intent(   out) :: is_sym
!---------------------------------------------------------------------------------------------          
!  Returns .true. if "self" is symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------    
      
   is_sym = .false.
   
   if ( self%nrow /= self%ncol ) return
   
   do j = 1, self%ncol
      do i = 1, j-1
         if ( self%v(i,j) /= self%v(j,i) ) return
      end do
   end do
   
   is_sym = .true.
                                 
   END SUBROUTINE bk2_IsSymmIk2


!=============================================================================================   
   SUBROUTINE bk2_IsSymmRk2 ( self, is_sym )
!=============================================================================================   
   class  (rk2_t), intent(in    ) :: self
   logical       , intent(   out) :: is_sym
!---------------------------------------------------------------------------------------------          
!  Returns .true. if "self" is strictely symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------    
      
   is_sym = .false.
   
   if ( self%nrow /= self%ncol ) return
   
    do j = 1, self%ncol
       do i = 1, j-1
          if ( abs(self%v(i,j) - self%v(j,i)) /= RZERO ) return
       end do
    end do

   is_sym = .true.
                                 
   END SUBROUTINE bk2_IsSymmRk2


!=============================================================================================   
   SUBROUTINE bk2_IsSymmCk2 ( self, is_sym )
!=============================================================================================   
   class  (ck2_t), intent(in    ) :: self
   logical       , intent(   out) :: is_sym
!---------------------------------------------------------------------------------------------          
!  Returns .true. if "self" is strictely symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------    

   is_sym = .false.
   
   if ( self%nrow /= self%ncol ) return
   
    do j = 1, self%ncol
       if ( aimag(self%v(j,j)) /= RZERO ) return
       do i = 1, j-1
          if ( abs(self%v(i,j) - conjg(self%v(j,i))) /= RZERO ) return
       end do
    end do

   is_sym = .true.
                                 
   END SUBROUTINE bk2_IsSymmCk2
   

!=============================================================================================   
   SUBROUTINE bk2_IsSymmLk2 ( self, is_sym )
!=============================================================================================   
   class  (lk2_t), intent(in    ) :: self
   logical       , intent(   out) :: is_sym
!---------------------------------------------------------------------------------------------          
!  Returns .true. if "self" is symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------    
      
   is_sym = .false.
   
   if ( self%nrow /= self%ncol ) return
   
   do j = 1, self%ncol
      do i = 1, j-1
         if ( self%v(i,j) .neqv. self%v(j,i) ) return
      end do
   end do
   
   is_sym = .true.
                                 
   END SUBROUTINE bk2_IsSymmLk2


!=============================================================================================   
   SUBROUTINE bk2_IsSymmSk2 ( self, is_sym )
!=============================================================================================   
   class  (sk2_t), intent(in    ) :: self
   logical       , intent(   out) :: is_sym
!---------------------------------------------------------------------------------------------          
!  Returns .true. if "self" is symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------    
      
   is_sym = .false.
   
   if ( self%nrow /= self%ncol ) return
   
   do j = 1, self%ncol
      do i = 1, j-1
         
         if ( self%v(i,j) /= self%v(j,i) ) return ! RH 09/22 (use "/=" of str_t)
         
         !if ( (self%v(i,j)%str) /= (self%v(j,i)%str) ) return
      end do
   end do
   
   is_sym = .true.
                                 
   END SUBROUTINE bk2_IsSymmSk2


!=============================================================================================   
   SUBROUTINE bk2_IsSymmBk2 ( self, is_sym )
!=============================================================================================   
   class  (bk2_t), intent(in    ) :: self
   logical       , intent(   out) :: is_sym
!---------------------------------------------------------------------------------------------          
!  Dymmy function
!-----------------------------------------------------------------------------------R.H. 10/18          
      
   is_sym = .false.
                                    
   END SUBROUTINE bk2_IsSymmBk2

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Set sub-matrix ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_SetSubmatIk2
! . bk2_SetSubmatRk2
! . bk2_SetSubmatCk2
! . bk2_SetSubmatLk2
! . bk2_SetSubmatSk2
! . bk2_SetSubmatBk2 (dummy)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
   
!=============================================================================================
   SUBROUTINE bk2_SetSubmatIk2 ( self, b, indi, indj, add )
!=============================================================================================
   class    (ik2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
   logical         , optional   , intent(in    ) :: add
!--------------------------------------------------------------------------------------------- 
!  Assigns the elements (i,j) of "b" to those of the integer matrix "self" where (i,j) belongs
!  the sets "indi" and "indj":
!
!                        b%v(indi,indj) = self%v
!
!  If "indj" is not present, the indices in "indi" correspond to column-major ordering.
!
!  Modified 04/19: 
!  if "add" is present and true, sum up the relevant elements of "self" and "b":
!
!                        b%v(indi,indj) = b%v(indi,indj) + self%v
!----------------------------------------------------------------------------R.H. 06/18, 04/19
   
!- local variables ----------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetSubmatIk2'
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( .not. allocated(b) ) then
      allocate(ik2_t::b) 
   else if ( b%typ == EMPTY ) then
      deallocate(b) ; allocate(ik2_t::b) 
   end if   
   
   select type (b)
      type is (ik2_t)
         call bk2_SetMatIk2ijI (matrix = self%v, indi = indi, indj = indj, add = add, res = b)     
      type is (rk2_t)
         call bk2_SetMatRk2ijI (matrix = self%v, indi = indi, indj = indj, add = add, res = b)     
      type is (ck2_t)
         call bk2_SetMatCk2ijI (matrix = self%v, indi = indi, indj = indj, add = add, res = b)           
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types') ; return     
   end select             
   
   END SUBROUTINE bk2_SetSubmatIk2

         
!=============================================================================================
   SUBROUTINE bk2_SetSubmatRk2 ( self, b, indi, indj, add )
!=============================================================================================
   class    (rk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
   logical         , optional   , intent(in    ) :: add   
!--------------------------------------------------------------------------------------------- 
!  Assigns the elements (i,j) of "b" to those of the real matrix "self" where (i,j) belongs
!  the sets "indi" and "indj":
!
!                        b%v(indi,indj) = self%v
!
!  If "indj" is not present, the indices in "indi" correspond to column-major ordering.
!  If "b" is a non-empty integer matrix (ik2), it's first converted to a real one (rk2)
!
!  Modified 04/19: 
!  if "add" is present and true, sum up the relevant elements of "self" and "b":
!
!                        b%v(indi,indj) = b%v(indi,indj) + self%v
!----------------------------------------------------------------------------R.H. 06/18, 04/19  
 
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetSubmatRk2'
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   if ( .not. allocated(b) ) then
      allocate(rk2_t::b) 
   else if ( b%typ == EMPTY ) then
      deallocate(b) ; allocate(rk2_t::b) 
   else if ( b%typ == ITYP ) then
      call bk2_ConvertToRk2 ( b )
   end if   

   select type (b)
      type is (rk2_t)
         call bk2_SetMatRk2ijR (matrix = self%v, indi = indi, indj = indj, add = add, res = b)     
      type is (ck2_t)
         call bk2_SetMatCk2ijR (matrix = self%v, indi = indi, indj = indj, add = add, res = b)     
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types') ; return  
   end select          
   
   END SUBROUTINE bk2_SetSubmatRk2   


!=============================================================================================
   SUBROUTINE bk2_SetSubmatCk2 ( self, b, indi, indj, add )
!=============================================================================================
   class    (ck2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
   logical         , optional   , intent(in    ) :: add   
!--------------------------------------------------------------------------------------------- 
!  Assigns the elements (i,j) of "b" to those of the complex matrix "self" where (i,j) belongs
!  the sets "indi" and "indj":
!
!                        b%v(indi,indj) = self%v
!
!  If "indj" is not present, the indices in "indi" correspond to column-major ordering.
!  If "b" is a non-empty non-complex matrix (ik2 or rk2), it's first converted to a complex one
!
!  Modified 04/19: 
!  if "add" is present and true, sum up the relevant elements of "self" and "b":
!
!                        b%v(indi,indj) = b%v(indi,indj) + self%v
!----------------------------------------------------------------------------R.H. 06/18, 04/19  
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetSubmatCk2'
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   if ( .not. allocated(b) ) then
      allocate(ck2_t::b)
   else if ( b%typ == EMPTY ) then
      deallocate(b) ; allocate(ck2_t::b)
   else if ( b%typ == ITYP .or. b%typ == RTYP ) then
      call bk2_ConvertToCk2 ( b )
   end if   

   select type (b)
      type is (ck2_t)
         call bk2_SetMatCk2ijC (matrix = self%v, indi = indi, indj = indj, add = add, res = b)     
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types') ; return  
   end select       
      
   END SUBROUTINE bk2_SetSubmatCk2   


!=============================================================================================
   SUBROUTINE bk2_SetSubmatLk2 ( self, b, indi, indj, add )
!=============================================================================================
   class    (lk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
   logical         , optional   , intent(in    ) :: add   
!--------------------------------------------------------------------------------------------- 
!  Assigns the elements (i,j) of "b" to those of the boolean matrix "self" where (i,j) belongs
!  the sets "indi" and "indj":
!
!                        b%v(indi,indj) = self%v
!
!  If "indj" is not present, the indices in "indi" correspond to column-major ordering.
!
!  Note: "add" is not used for logical type.
!----------------------------------------------------------------------------R.H. 06/18, 04/19  
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetSubmatLk2'
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   if ( .not. allocated(b) ) then
      allocate(lk2_t::b)
   else if ( b%typ == EMPTY ) then
      deallocate(b) ; allocate(lk2_t::b)
   end if
   
   select type (b)
      type is (lk2_t)
         call bk2_SetMatLk2ijL (matrix = self%v, indi = indi, indj = indj, res = b)        
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types') 
         return     
   end select       
        
   END SUBROUTINE bk2_SetSubmatLk2
   

!=============================================================================================
   SUBROUTINE bk2_SetSubmatSk2 ( self, b, indi, indj, add )
!=============================================================================================
   class    (sk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
   logical         , optional   , intent(in    ) :: add   
!--------------------------------------------------------------------------------------------- 
!  Assigns the elements (i,j) of "b" to those of the string matrix "self" where (i,j) belongs
!  the sets "indi" and "indj":
!
!                        b%v(indi,indj) = self%v
!
!  If "indj" is not present, the indices in "indi" correspond to column-major ordering.
!
!  Modified 04/19: 
!  if "add" is present and true, concatenate the relevant elements of "self" and "b":
!
!                        b%v(indi,indj) = b%v(indi,indj) // self%v
!----------------------------------------------------------------------------R.H. 06/18, 04/19  
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetSubmatSk2'
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   if ( .not. allocated(b) ) then
      allocate(sk2_t::b)
   else if ( b%typ == EMPTY ) then
      deallocate(b) ; allocate(sk2_t::b)
   end if

   select type (b)
      type is (sk2_t)
         call bk2_SetMatSk2ijS (matrix = self%v, indi = indi, indj = indj, add = add, res = b)        
      class default
         call opflag%Set(UERROR, HERE, 'Incompatible types')  
         return    
   end select       
        
   END SUBROUTINE bk2_SetSubmatSk2
   

!=============================================================================================
   SUBROUTINE bk2_SetSubmatBk2 ( self, b, indi, indj, add )
!=============================================================================================
   class    (bk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
   logical         , optional   , intent(in    ) :: add   
!--------------------------------------------------------------------------------------------- 
!  Dummy subroutine
!-----------------------------------------------------------------------------------R.H. 06/18
   
   END SUBROUTINE bk2_SetSubmatBk2


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Copy sub-matrix +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_CopySubmatIk2
! . bk2_CopySubmatRk2
! . bk2_CopySubmatCk2
! . bk2_CopySubmatLk2
! . bk2_CopySubmatSk2
! . bk2_CopySubmatBk2 (dummy)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_CopySubmatIk2 ( self, res, indi, indj )
!=============================================================================================
   class    (ik2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(   out) :: res
   integer  (Ikind), optional   , intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
!--------------------------------------------------------------------------------------------- 
!  Makes a copy of the submatrix self%v(indi,indj) into res%v:
!
!                        res%v(i,j) = self%v(indi(i),indj(j))
!
!  If only "indi" (or "indj") is present, the indices correspond to column-major ordering.
!  If "indi" and "indj" are not present, res = self.
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ----------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_CopySubmatIk2'
   character(len=99)           :: emsg
   integer  (Ikind)            :: i, j, k, indk, ni, nj, minind, maxind, typ, err
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   if ( .not. present(indi) .and. .not. present(indj) ) then
      res = self
   else
      typ = ITYP
      allocate(ik2_t :: res)
      select type (res)
         type is (ik2_t)
#include "include/bk2_copysubmat.inc"         
      end select
   end if
   
   if ( opflag%code > 0 ) res = bk2_t()
     
   END SUBROUTINE bk2_CopySubmatIk2


!=============================================================================================
   SUBROUTINE bk2_CopySubmatRk2 ( self, res, indi, indj )
!=============================================================================================
   class    (rk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(   out) :: res
   integer  (Ikind), optional   , intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
!--------------------------------------------------------------------------------------------- 
!  Makes a copy of the submatrix self%v(indi,indj) in res%v:
!
!                        res%v(i,j) = self%v(indi(i),indj(j))
!
!  If only "indi" (or "indj") is present, the indices correspond to column-major ordering.
!  If "indi" and "indj" are not present, res = self.
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ----------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_CopySubmatRk2'
   character(len=99)           :: emsg
   integer  (Ikind)            :: i, j, k, indk, ni, nj, minind, maxind, typ, err
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   if ( .not. present(indi) .and. .not. present(indj) ) then
      res = self
   else
      typ = RTYP
      allocate(rk2_t :: res)
      select type (res)
         type is (rk2_t)
#include "include/bk2_copysubmat.inc"
      end select
   end if

   if ( opflag%code > 0 ) res = bk2_t()

   END SUBROUTINE bk2_CopySubmatRk2


!=============================================================================================
   SUBROUTINE bk2_CopySubmatCk2 ( self, res, indi, indj )
!=============================================================================================
   class    (ck2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(   out) :: res
   integer  (Ikind), optional   , intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
!--------------------------------------------------------------------------------------------- 
!  Makes a copy of the submatrix self%v(indi,indj) in res%v:
!
!                        res%v(i,j) = self%v(indi(i),indj(j))
!
!  If only "indi" (or "indj") is present, the indices correspond to column-major ordering.
!  If "indi" and "indj" are not present, res = self.
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ----------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_CopySubmatCk2'
   character(len=99)           :: emsg
   integer  (Ikind)            :: i, j, k, indk, ni, nj, minind, maxind, typ, err
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   if ( .not. present(indi) .and. .not. present(indj) ) then
      res = self
   else
      typ = CTYP
      allocate(ck2_t :: res)
      select type (res)
         type is (ck2_t)
#include "include/bk2_copysubmat.inc"
      end select
   end if

   if ( opflag%code > 0 ) res = bk2_t()
      
   END SUBROUTINE bk2_CopySubmatCk2
   

!=============================================================================================
   SUBROUTINE bk2_CopySubmatLk2 ( self, res, indi, indj )
!=============================================================================================
   class    (lk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(   out) :: res
   integer  (Ikind), optional   , intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
!--------------------------------------------------------------------------------------------- 
!  Makes a copy of the submatrix self%v(indi,indj) in res%v:
!
!                        res%v(i,j) = self%v(indi(i),indj(j))
!
!  If only "indi" (or "indj") is present, the indices correspond to column-major ordering.
!  If "indi" and "indj" are not present, res = self.
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ----------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_CopySubmatLk2'
   character(len=99)           :: emsg
   integer  (Ikind)            :: i, j, k, indk, ni, nj, minind, maxind, typ, err
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   if ( .not. present(indi) .and. .not. present(indj) ) then
      res = self
   else
      typ = LTYP
      allocate(lk2_t :: res)
      select type (res)
         type is (lk2_t)
#include "include/bk2_copysubmat.inc"
      end select
   end if

   if ( opflag%code > 0 ) res = bk2_t()

   END SUBROUTINE bk2_CopySubmatLk2


!=============================================================================================
   SUBROUTINE bk2_CopySubmatSk2 ( self, res, indi, indj )
!=============================================================================================
   class    (sk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(   out) :: res
   integer  (Ikind), optional   , intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
!--------------------------------------------------------------------------------------------- 
!  Makes a copy of the submatrix self%v(indi,indj) in res%v: 
!
!                        res%v(i,j) = self%v(indi(i),indj(j))
!
!  If only "indi" (or "indj") is present, the indices correspond to column-major ordering.
!  If "indi" and "indj" are not present, res = self.
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ----------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_CopySubmatSk2' 
   character(len=99)             :: emsg
   type     (str_t), allocatable :: tmp(:)
   integer  (Ikind)              :: i, j, ii, jj, ni, nj, minind, maxind, err
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   if ( present(indi) .and. present(indj) ) then  
      minind = minval(indi) ; maxind = maxval(indi)
      
      if ( minind < 1 ) then
         write(emsg,'(a,i0,a)') "index '",minind,"' below lower bound of 1"                                     
         call opflag%Set(UERROR, HERE, emsg)      
         return
      end if

      if ( maxind > self%nrow ) then
         write(emsg,'(a,i0,a,i0)') &
                    "index '",maxind,"' of dimension #1 above upper bound of ",self%nrow                                     
         call opflag%Set(UERROR, HERE, emsg)      
         return
      end if

      minind = minval(indj) ; maxind = maxval(indj)
      
      if ( minind < 1 ) then
         write(emsg,'(a,i0,a)') "index '",minind,"' below lower bound of 1"                                     
         call opflag%Set(UERROR, HERE, emsg)      
         return
      end if

      if ( maxind > self%ncol ) then
         write(emsg,'(a,i0,a,i0)') &
                    "index '",maxind,"' of dimension #2 above upper bound of ",self%ncol                                     
         call opflag%Set(UERROR, HERE, emsg)      
         return
      end if
    
      !res = sk2_t ( matrix = self%v(indi,indj) ) ! unfortunetly I gave up to use this 
                                                  ! because of an issue with some distribution
                                                  ! of gfortran that occurs when res%v
                                                  ! contains an allocatable component
                                                  ! (memory leaks)
      ni = size(indi) ; nj = size(indj)
      
      if ( ni == 0 .or. nj == 0 ) return
      
      allocate(sk2_t::res)                        
      select type (res)
         type is (sk2_t)
            allocate(res%v(ni,nj), stat = err)
            if ( err == 0 ) then
               do j = 1, nj
                  jj = indj(j)
                  do i = 1, ni
                     ii = indi(i)
                     
                     call res%v(i,j)%assign ( rhs = self%v(ii,jj) ) ! RH 09/22
                     
!                      if ( allocated( self%v(ii,jj)%str ) ) then
!                         res%v(i,j)%str = (self%v(ii,jj)%str)
!                      else
!                         res%v(i,j)%str = ''
!                      end if
!                      if ( self%v(ii,jj)%has_color ) then  ! RH 09/22
!                         if ( allocated( self%v(ii,jj)%strcol ) ) &
!                            res%v(i,j)%strcol = (self%v(ii,jj)%strcol)
!                         res%v(i,j)%has_color = .true.
!                      end if
                     
                  end do
               end do
               res%typ = STYP ; res%nrow = ni ; res%ncol = nj
            end if
      end select      
      
      if ( err > 0 ) then
         res = bk2_t()
         call opflag%Set(UERROR, HERE, "Allocation failure") 
         return     
      end if       
      
   else if ( present(indi) .or. present(indj) ) then
   
      if ( present(indi) ) then
         ni = size(indi)
         minind = minval(indi)
         maxind = maxval(indi)
      else   
         ni = size(indj)
         minind = minval(indj) 
         maxind = maxval(indj)
      end if
      
      if ( ni == 0 ) return
      
      if ( minind < 1 ) then
         write(emsg,'(a,i0,a)') "index '",minind,"' below lower bound of 1"                                     
         call opflag%Set(UERROR, HERE, emsg)      
         return
      end if

      if ( maxind > self%nrow * self%ncol ) then
         write(emsg,'(a,i0,a,i0)') &
               "index '",maxind,"' of dimension #1 above upper bound of ",self%nrow*self%ncol                                     
         call opflag%Set(UERROR, HERE, emsg)      
         return
      end if
      
      allocate(tmp(self%nrow * self%ncol), stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure')   
         return   
      end if
               
      ii = 0
      do j = 1, self%ncol
         do i = 1, self%nrow
            ii = ii + 1
            
            call tmp(ii)%assign ( rhs = self%v(i,j) ) ! RH 09/22
!             if ( allocated(self%v(i,j)%str) ) then
!                tmp(ii)%str = (self%v(i,j)%str)
!             else
!                tmp(ii)%str = ''
!             end if
         end do
      end do
                               
      allocate(sk2_t::res)
      
      select type (res)
         type is (sk2_t)
            if ( self%nrow == 1 ) then
               allocate(res%v(1,ni))
               if ( present(indi) ) then
                  do i = 1, ni
                     call res%v(1,i)%assign ( rhs = tmp(indi(i)) ) ! RH 09/22
!                     res%v(1,i)%str = (tmp(indi(i))%str)
                  end do
               else      
                  do i = 1, ni
                     call res%v(1,i)%assign ( rhs = tmp(indj(i)) ) ! RH 09/22
!                     res%v(1,i)%str = (tmp(indj(i))%str)
                  end do
               end if
               res%typ = STYP ; res%nrow = 1 ; res%ncol = ni
            else
               allocate(res%v(ni,1))
               if ( present(indi) ) then
                  do i = 1, ni
                     call res%v(i,1)%assign ( rhs = tmp(indi(i)) ) ! RH 09/22
!                     res%v(i,1)%str = (tmp(indi(i))%str)
                  end do
               else      
                  do i = 1, ni
                     call res%v(i,1)%assign ( rhs = tmp(indj(i)) ) ! RH 09/22
!                     res%v(i,1)%str = (tmp(indj(i))%str)
                  end do
               end if
               res%typ = STYP ; res%nrow = ni ; res%ncol = 1
            end if   
      end select       
            
   else     
      res = self
   end if    
     
   END SUBROUTINE bk2_CopySubmatSk2
      

!=============================================================================================
   SUBROUTINE bk2_CopySubmatBk2 ( self, res, indi, indj )
!=============================================================================================
   class    (bk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(   out) :: res
   integer  (Ikind), optional   , intent(in    ) :: indi(:)
   integer  (Ikind), optional   , intent(in    ) :: indj(:)
!--------------------------------------------------------------------------------------------- 
!  Dummy subroutine
!--------------------------------------------------------------------------------------------- 

   END SUBROUTINE bk2_CopySubmatBk2     


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Delete rows/columns +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_DelRowsIk2i32, bk2_DelRowsIk2i64  (given indices as i32 or i64)
! . bk2_DelRowsRk2i32, bk2_DelRowsRk2i64  (  "      "     "  "   "  " )
! . bk2_DelRowsCk2i32, bk2_DelRowsCk2i64  (  "      "     "  "   "  " )
! . bk2_DelRowsLk2i32, bk2_DelRowsLk2i64  (  "      "     "  "   "  " )
! . bk2_DelRowsSk2i32, bk2_DelRowsSk2i64  (  "      "     "  "   "  " )
! . bk2_DelRowsBk2i32, bk2_DelRowsBk2i64  (  "      "     "  "   "  " )
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_DelRowsIk2i32 ( self, indx, opt )
!=============================================================================================
   class    (ik2_t), intent(in out) :: self
   integer  (i32  ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  . If opt == 'r' or 'c':
!     The rows or columns #s are given in indx(:)
!     If all indices in "indx" are out of range, self is unchanged
!  . If opt == 'e':
!     The matrix is reshaped to a n x 1 matrix and the rows given in "indx" are removed.
!     Note that if all indices in "indx" are out of range, self is unchanged but reshaped.
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsIk2i32'
   integer  (Ikind)              :: i, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   integer  (Ikind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

#include "include/bk2_delrows.inc"
                  
   END SUBROUTINE bk2_DelRowsIk2i32
   
       
!=============================================================================================
   SUBROUTINE bk2_DelRowsIk2i64 ( self, indx, opt )
!=============================================================================================
   class    (ik2_t), intent(in out) :: self
   integer  (i64  ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsIk2i64'
   integer  (Ikind)              :: i, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   integer  (Ikind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

#include "include/bk2_delrows.inc"
                  
   END SUBROUTINE bk2_DelRowsIk2i64
      

!=============================================================================================
   SUBROUTINE bk2_DelRowsRk2i32 ( self, indx, opt )
!=============================================================================================
   class    (rk2_t), intent(in out) :: self
   integer  (i32 ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsRk2i32'
   integer  (Ikind)              :: i, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   real     (Rkind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

#include "include/bk2_delrows.inc"
                  
   END SUBROUTINE bk2_DelRowsRk2i32


!=============================================================================================
   SUBROUTINE bk2_DelRowsRk2i64 ( self, indx, opt )
!=============================================================================================
   class    (rk2_t), intent(in out) :: self
   integer  (i64 ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsRk2i64'
   integer  (Ikind)              :: i, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   real     (Rkind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

#include "include/bk2_delrows.inc"
                  
   END SUBROUTINE bk2_DelRowsRk2i64


!=============================================================================================
   SUBROUTINE bk2_DelRowsCk2i32 ( self, indx, opt )
!=============================================================================================
   class    (ck2_t), intent(in out) :: self
   integer  (i32  ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsCk2i32'
   integer  (Ikind)              :: i, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   complex  (Rkind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
#include "include/bk2_delrows.inc"
                  
   END SUBROUTINE bk2_DelRowsCk2i32


!=============================================================================================
   SUBROUTINE bk2_DelRowsCk2i64 ( self, indx, opt )
!=============================================================================================
   class    (ck2_t), intent(in out) :: self
   integer  (i64  ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsCk2i64'
   integer  (Ikind)              :: i, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   complex  (Rkind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
#include "include/bk2_delrows.inc"
                  
   END SUBROUTINE bk2_DelRowsCk2i64


!=============================================================================================
   SUBROUTINE bk2_DelRowsLk2i32 ( self, indx, opt )
!=============================================================================================
   class    (lk2_t), intent(in out) :: self
   integer  (i32  ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsLk2'
   integer  (Ikind)              :: i, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   logical         , allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
#include "include/bk2_delrows.inc"

   END SUBROUTINE bk2_DelRowsLk2i32


!=============================================================================================
   SUBROUTINE bk2_DelRowsLk2i64 ( self, indx, opt )
!=============================================================================================
   class    (lk2_t), intent(in out) :: self
   integer  (i64  ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsLk2i64'
   integer  (Ikind)              :: i, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   logical         , allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
#include "include/bk2_delrows.inc"

   END SUBROUTINE bk2_DelRowsLk2i64


!=============================================================================================
   SUBROUTINE bk2_DelRowsSk2i32 ( self, indx, opt )
!=============================================================================================
   class    (sk2_t), intent(in out) :: self
   integer  (i32  ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsSk2i32'
   integer  (Ikind)              :: i, j, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   type     (str_t), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   nkeep = 0
   
   select case (opt)
      case ('r')         
         do i = 1, self%nrow
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1 ; lkeep(nkeep) = i
         end do   

         if ( nkeep == 0 ) then
            call self%Destroy () ; return
         end if
            
         allocate(tmp(nkeep,self%ncol), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')   
            return   
         end if   
         
         do j = 1, self%ncol
            do i = 1, nkeep
               
               call tmp(i,j)%assign ( rhs = self%v(lkeep(i),j) ) ! RH 09/22
!                if (allocated(self%v(lkeep(i),j)%str)) then
!                   tmp(i,j)%str = (self%v(lkeep(i),j)%str)
!                else
!                   tmp(i,j)%str = ''
!                end if      
            end do
         end do      
         self%nrow = nkeep
         
      case ('c')
         do i = 1, self%ncol
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1 ; lkeep(nkeep) = i   
         end do   
         if ( nkeep == 0 ) then
            call self%Destroy ; return
         end if
               
         allocate(tmp(self%nrow,nkeep), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')      
            return
         end if  
         
         do j = 1, nkeep
            do i = 1, self%nrow
               call tmp(i,j)%assign ( rhs = self%v(i,lkeep(j)) ) ! RH 09/22
               
!                if (allocated(self%v(i,lkeep(j))%str)) then
!                   tmp(i,j)%str = (self%v(i,lkeep(j))%str)
!                else
!                   tmp(i,j)%str = ''
!                end if
             end do
         end do            
         self%ncol = nkeep

      case ('e') 
            
         nm = self%nrow * self%ncol   
         self%v = reshape(self%v,[nm,IONE])
            
         do i = 1, nm
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1
            lkeep(nkeep) = i
         end do   
         
         if ( nkeep == 0 ) then
            call self%Destroy ; return
         end if
               
         allocate(tmp(nkeep,1), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')    
            return  
         end if  
         
         do i = 1, nkeep
            call tmp(i,1)%assign ( rhs = self%v(lkeep(i),1) ) ! RH 09/22
            
!             if (allocated(self%v(lkeep(i),1)%str)) then
!                tmp(i,1)%str = (self%v(lkeep(i),1)%str)
!             else
!                tmp(i,1)%str = ''
!             end if
         end do         
         !tmp(1:nkeep,1) = self%v(lkeep(1:nkeep),1)
         self%nrow = nkeep ; self%ncol = 1
               
      case default
         call opflag%Set(UERROR, HERE, &      
                         "Unknown option opt = "//opt//" (must be 'r', 'c' or 'e')")  
         return       
   end select
     
   call move_alloc (from = tmp, to = self%v)
                  
   END SUBROUTINE bk2_DelRowsSk2i32


!=============================================================================================
   SUBROUTINE bk2_DelRowsSk2i64 ( self, indx, opt )
!=============================================================================================
   class    (sk2_t), intent(in out) :: self
   integer  (i64  ), intent(in    ) :: indx(:)
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsSk2i64'
   integer  (Ikind)              :: i, j, nm, err, nkeep, lkeep(self%nrow * self%ncol)
   type     (str_t), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   nkeep = 0
   
   select case (opt)
      case ('r')         
         do i = 1, self%nrow
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1 ; lkeep(nkeep) = i
         end do   

         if ( nkeep == 0 ) then
            call self%Destroy () ; return
         end if
            
         allocate(tmp(nkeep,self%ncol), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')     
            return 
         end if   
         
         do j = 1, self%ncol
            do i = 1, nkeep
               
               call tmp(i,j)%assign ( rhs = self%v(lkeep(i),j) ) ! RH 09/22
!                if (allocated(self%v(lkeep(i),j)%str)) then
!                   tmp(i,j)%str = (self%v(lkeep(i),j)%str)
!                else
!                   tmp(i,j)%str = ''
!                end if      
            end do
         end do      
         self%nrow = nkeep
         
      case ('c')
         do i = 1, self%ncol
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1 ; lkeep(nkeep) = i   
         end do   
         if ( nkeep == 0 ) then
            call self%Destroy ; return
         end if
               
         allocate(tmp(self%nrow,nkeep), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')  
            return    
         end if  
         
         do j = 1, nkeep
            do i = 1, self%nrow
               
               call tmp(i,j)%assign ( rhs = self%v(i,lkeep(j)) ) ! RH 09/22
               
!                if (allocated(self%v(i,lkeep(j))%str)) then
!                   tmp(i,j)%str = (self%v(i,lkeep(j))%str)
!                else
!                   tmp(i,j)%str = ''
!                end if
            end do
         end do            
         !tmp(:,1:nkeep) = self%v(:,lkeep(1:nkeep))
         self%ncol = nkeep

      case ('e') 
            
         nm = self%nrow * self%ncol   
         self%v = reshape(self%v,[nm,IONE])
            
         do i = 1, nm
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1
            lkeep(nkeep) = i
         end do   
         
         if ( nkeep == 0 ) then
            call self%Destroy ; return
         end if
               
         allocate(tmp(nkeep,1), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')     
            return 
         end if  
         
         do i = 1, nkeep
            call tmp(i,1)%assign ( rhs = self%v(lkeep(i),1) ) ! RH 09/22
!             if (allocated(self%v(lkeep(i),1)%str)) then
!                tmp(i,1)%str = (self%v(lkeep(i),1)%str)
!             else
!                tmp(i,1)%str = ''
!             end if
         end do         
         self%nrow = nkeep ; self%ncol = 1
               
      case default
         call opflag%Set(UERROR, HERE, &      
                         "Unknown option opt = "//opt//" (must be 'r', 'c' or 'e')")   
         return      
   end select
     
   call move_alloc (from = tmp, to = self%v)
                  
   END SUBROUTINE bk2_DelRowsSk2i64
   

!=============================================================================================
   SUBROUTINE bk2_DelRowsBk2i32 ( self, indx, opt )
!=============================================================================================
   class    (bk2_t), intent(in out) :: self
   integer  (i32  ), intent(in    ) :: indx(:)   
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsBk2i32'
   integer  (Ikind)              :: i, nm, nkeep
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   nkeep = 0
   
   select case (opt)
      case ('r')         
         do i = 1, self%nrow
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1
         end do   

         if ( nkeep == 0 ) then
            call self%Destroy () ; return
         end if
            
         self%nrow = nkeep
         
      case ('c')
         do i = 1, self%ncol
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1   
         end do   
         if ( nkeep == 0 ) then
            call self%Destroy ; return
         end if
               
         self%ncol = nkeep

      case ('e') 
         nm = self%nrow * self%ncol               
         do i = 1, nm
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1
         end do   
         
         if ( nkeep == 0 ) then
            call self%Destroy ; return
         end if
         
         self%nrow = nkeep ; self%ncol = 1
               
      case default
         call opflag%Set(UERROR, HERE, &      
                         "Unknown option opt = "//opt//" (must be 'r', 'c' or 'e')")
         return         
   end select
                       
   END SUBROUTINE bk2_DelRowsBk2i32


!=============================================================================================
   SUBROUTINE bk2_DelRowsBk2i64 ( self, indx, opt )
!=============================================================================================
   class    (bk2_t), intent(in out) :: self
   integer  (i64  ), intent(in    ) :: indx(:)   
   character(len=1), intent(in    ) :: opt
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt = 'r') or columns (if opt = 'c') or elements (if opt = 'e') of 
!  the matrix self%v.
!  See description of bk2_DelRowsIk2i32
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DelRowsBk2i64'
   integer  (Ikind)              :: i, nm, nkeep
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   nkeep = 0
   
   select case (opt)
      case ('r')         
         do i = 1, self%nrow
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1
         end do   

         if ( nkeep == 0 ) then
            call self%Destroy () ; return
         end if
            
         self%nrow = nkeep
         
      case ('c')
         do i = 1, self%ncol
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1   
         end do   
         if ( nkeep == 0 ) then
            call self%Destroy ; return
         end if
               
         self%ncol = nkeep

      case ('e') 
         nm = self%nrow * self%ncol               
         do i = 1, nm
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1
         end do   
         
         if ( nkeep == 0 ) then
            call self%Destroy ; return
         end if
         
         self%nrow = nkeep ; self%ncol = 1
               
      case default
         call opflag%Set(UERROR, HERE, &      
                         "Unknown option opt = "//opt//" (must be 'r', 'c' or 'e')")
         return         
   end select
                       
   END SUBROUTINE bk2_DelRowsBk2i64


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Insert sub-matrix +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_InsertIintoIk2 (Imat into rk2)
! . bk2_InsertIintoRk2 (Imat into rk2)
! . bk2_InsertIintoCk2 (Imat into ck2)
!
! . bk2_InsertRintoRk2 (Rmat into rk2)
! . bk2_InsertRintoCk2 (Rmat into ck2)
!
! . bk2_InsertCintoCk2 (Cmat into ck2)
!
! . bk2_InsertLintoLk2 (Lmat into lk2)
! . bk2_InsertSintoSk2 (Smat into sk2)
!
! . bk2_InsertIk2 (ik2 into bk2)
! . bk2_InsertRk2 (rk2 into bk2)
! . bk2_InsertCk2 (ck2 into bk2)
! . bk2_InsertLk2 (lk2 into bk2)
! . bk2_InsertSk2 (sk2 into bk2)
! . bk2_InsertBk2 (dummy)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_InsertIintoIk2 ( a, matrix, pnum, cr )   
!=============================================================================================
   type     (ik2_t), intent(in out) :: a
   integer  (Ikind), intent(in    ) :: matrix(:,:)
   integer  (Ikind), intent(in    ) :: pnum
   character(len=1), intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the integer matrix "matrix" into the ik2_t matrix "a".
!
!  Description:
!
!  . If cr = 'r', the insertion into "a" of the rows of "matrix" starts at the row # "pnum"
!       (with  1<= pnum <= a%nrow+1).
!       If the number of columns of "matrix" (mi) is less than a%ncol (ma), each inserted row
!       is completed by 0. On the contrary, if mi > ma, elements of "matrix" beyond this
!       position are ignored (number of columns is unchanged).
!
!       The resulting dimensions of the matrix "a" is then (na + ni) x ma where na is the
!       initial number of rows in "a" and ni is the number of rows in "matrix".
!
!  . Else, if cr = 'c', the insertion into "a" of the columns of "matrix" starts at the column
!       # "pnum".(with 1 <= pnum <= a%ncol+1).
!       If the number of rows of "matrix" (ni) is less than a%nrow (na), each inserted  
!       column is completed by 0. On the contrary, if ni > na, elements of "matrix" beyond  
!       this position are ignored (number of rows is unchanged).
!
!       The resulting dimensions of the matrix "a" is then na x (ma + mi) where ma is the
!       initial number of columns in "a" and mi is the number of columns in "matrix".
!
!  Example:
!                   | a11 a12 a13 a14 |                     | M11 M12 M13 M14 M15 |
!             a%v = | a21 a22 a23 a24 | (3x4)      matrix = |                     |  (2x5)
!                   | a31 a32 a33 a34 |                     | M21 M22 M23 M24 M25 |
!
!     if cr = 'r' and pnum = 2:
!
!                            | a11 a12 a13 a14 |
!                            | --------------- |
!                            | M11 M12 M13 M14 |
!                      a%v = | M21 M22 M23 M24 |    (3+2) x 4
!                            | --------------- |
!                            | a21 a22 a23 a24 |
!                            | a31 a32 a33 a34 |
!
!     if cr = 'c' and pnum = 2:
!
!                       | a11 |M11 M12 M13 M14 M15| a12 a13 a14 |
!                       | a21 |M21 M22 M23 M24 M25| a22 a23 a24 |  3 x (4+5)
!                       | a31 | 0   0   0   0   0 | a32 a33 a34 |
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_InsertIintoIk2'
   integer  (Ikind)              :: na, ma, ni, mi, minai, p1, p2, err, zero, typ
   integer  (Ikind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = IZERO ; typ = ITYP

#include "include/bk2_insertmat.inc"
      
   END SUBROUTINE bk2_InsertIintoIk2


!=============================================================================================
   SUBROUTINE bk2_InsertIintoRk2 ( a, matrix, pnum, cr )   
!=============================================================================================
   type     (rk2_t), intent(in out) :: a
   integer  (Ikind), intent(in    ) :: matrix(:,:)
   integer  (Ikind), intent(in    ) :: pnum
   character(len=1), intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the integer matrix "matrix" into the rk2_t matrix "a".
!
!  Description: see description of bk2_InsertIintoIk2
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_InsertIintoRk2'
   integer  (Ikind)              :: na, ma, ni, mi, minai, p1, p2, err, typ
   real     (Rkind), allocatable :: tmp(:,:)
   real     (Rkind)              :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = RZERO ; typ = RTYP

#include "include/bk2_insertmat.inc"
   
   END SUBROUTINE bk2_InsertIintoRk2


!=============================================================================================
   SUBROUTINE bk2_InsertRintoRk2 ( a, matrix, pnum, cr )   
!=============================================================================================
   type     (rk2_t), intent(in out) :: a
   real     (Rkind), intent(in    ) :: matrix(:,:)
   integer  (Ikind), intent(in    ) :: pnum
   character(len=1), intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the real matrix "matrix" into the rk2_t matrix "a".
!
!  Description: see description of bk2_InsertIintoIk2
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_InsertRintoRk2'
   integer  (Ikind)              :: na, ma, ni, mi, minai, p1, p2, err, typ
   real     (Rkind), allocatable :: tmp(:,:)
   real     (Rkind)              :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = RZERO ; typ = RTYP

#include "include/bk2_insertmat.inc"
   
   END SUBROUTINE bk2_InsertRintoRk2
   

!=============================================================================================
   SUBROUTINE bk2_InsertIintoCk2 ( a, matrix, pnum, cr )   
!=============================================================================================
   type     (ck2_t), intent(in out) :: a
   integer  (Ikind), intent(in    ) :: matrix(:,:)
   integer  (Ikind), intent(in    ) :: pnum
   character(len=1), intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the integer matrix "matrix" into the ck2_t matrix "a".
!
!  Description: see description of bk2_InsertIintoIk2
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_InsertIintoCk2'
   integer  (Ikind)              :: na, ma, ni, mi, minai, p1, p2, err, typ
   complex  (Rkind), allocatable :: tmp(:,:)
   complex  (Rkind)              :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = CZERO ; typ = CTYP

#include "include/bk2_insertmat.inc"
   
   END SUBROUTINE bk2_InsertIintoCk2


!=============================================================================================
   SUBROUTINE bk2_InsertRintoCk2 ( a, matrix, pnum, cr )   
!=============================================================================================
   type     (ck2_t), intent(in out) :: a
   real     (Rkind), intent(in    ) :: matrix(:,:)
   integer  (Ikind), intent(in    ) :: pnum
   character(len=1), intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the real matrix "matrix" into the ck2_t matrix "a".
!
!  Description: see description of bk2_InsertIintoIk2
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_InsertRintoCk2'
   integer  (Ikind)              :: na, ma, ni, mi, minai, p1, p2, err, typ
   complex  (Rkind), allocatable :: tmp(:,:)
   complex  (Rkind)              :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = CZERO ; typ = CTYP

#include "include/bk2_insertmat.inc"
   
   END SUBROUTINE bk2_InsertRintoCk2


!=============================================================================================
   SUBROUTINE bk2_InsertCintoCk2 ( a, matrix, pnum, cr )   
!=============================================================================================
   type     (ck2_t), intent(in out) :: a
   complex  (Rkind), intent(in    ) :: matrix(:,:)
   integer  (Ikind), intent(in    ) :: pnum
   character(len=1), intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the complex matrix "matrix" into the ck2_t matrix "a".
!
!  Description: see description of bk2_InsertIintoIk2
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_InsertCintoCk2'
   integer  (Ikind)              :: na, ma, ni, mi, minai, p1, p2, err, typ
   complex  (Rkind), allocatable :: tmp(:,:)
   complex  (Rkind)              :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = CZERO ; typ = CTYP

#include "include/bk2_insertmat.inc"
   
   END SUBROUTINE bk2_InsertCintoCk2


!=============================================================================================
   SUBROUTINE bk2_InsertLintoLk2 ( a, matrix, pnum, cr )   
!=============================================================================================
   type     (lk2_t), intent(in out) :: a
   logical         , intent(in    ) :: matrix(:,:)
   integer  (Ikind), intent(in    ) :: pnum
   character(len=1), intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the boolean matrix "matrix" into the lk2_t matrix "a".
!
!  Description: see description of bk2_InsertIintoIk2
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_InsertLintoLk2'
   integer  (Ikind)              :: na, ma, ni, mi, minai, p1, p2, err, typ
   logical         , allocatable :: tmp(:,:)
   logical                       :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = LZERO ; typ = LTYP

#include "include/bk2_insertmat.inc"
   
   END SUBROUTINE bk2_InsertLintoLk2
   

!=============================================================================================
   SUBROUTINE bk2_InsertSintoSk2 ( a, matrix, pnum, cr )   
!=============================================================================================
   type     (sk2_t), intent(in out) :: a
   type     (str_t), intent(in    ) :: matrix(:,:)
   integer  (Ikind), intent(in    ) :: pnum
   character(len=1), intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the string matrix "matrix" into the sk2_t matrix "a".
!
!  Description: see description of bk2_InsertIintoIk2
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_InsertSintoSk2'
   integer  (Ikind)              :: na, ma, ni, mi, p1, p2, err, i, j, minai
   type     (str_t), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   na = a%nrow ; ni = size(matrix,dim=1) 
   ma = a%ncol ; mi = size(matrix,dim=2) 

   if ( a%typ == EMPTY .or. na*ma == 0 ) then
      if ( allocated(a%v) ) deallocate(a%v) ; allocate(a%v(ni,mi), source = matrix, stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure for array "a%v"')  
         return    
      end if
      a%nrow = ni ; a%ncol = mi ; a%typ = STYP
      return
   end if      

   if ( cr == 'r' ) then
   
      if ( pnum < 1 .or. pnum > na+1 ) then
         call opflag%Set(UERROR, HERE, "Row # outside of expected range")
         return
      end if
!
!-    Insert the ni rows of "matrix" into self%v starting at the row # "pnum":
!     (note: if mi < ma, each inserted rows are completed by 0, if mi > ma, elements beyond
!      ma are ignored)
!
      allocate(tmp(na+ni,ma), stat = err)    
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')      
         return
      end if
      
      do j = 1, ma
         do i = 1, na+ni
            tmp(i,j)%str = ''
         end do
      end do      
      minai = min(ma,mi)
      p1 =      1 ; p2 = pnum - 1 
      do j = 1, ma
         do i = p1, p2
            
            call tmp(i,j)%assign ( rhs = a%v(i,j) ) ! RH 09/22
!            if ( allocated(a%v(i,j)%str) ) tmp(i,j)%str = (a%v(i,j)%str)
         end do
      end do
            
      p1 = p2 + 1 ; p2 = p2 + ni
      do j = 1, minai
         do i = p1, p2
            call tmp(i,j)%assign ( rhs = matrix(i-p1+1,j) ) ! RH 09/22
!            if (allocated(matrix(i-p1+1,j)%str)) tmp(i,j)%str = (matrix(i-p1+1,j)%str)
         end do
      end do
            
      p1 = p2 + 1 ; p2 = na + ni 
      do j = 1, ma
         do i = p1, p2
            call tmp(i,j)%assign ( rhs = a%v(i-ni,j) ) ! RH 09/22
!            if (allocated(a%v(i-ni,j)%str)) tmp(i,j)%str = (a%v(i-ni,j)%str)
         end do
      end do      
      
      call move_alloc (from = tmp, to = a%v)
      
      a%nrow = na + ni
   
   else if ( cr == 'c' ) then
      
      if ( pnum < 1 .or. pnum > ma+1 ) then
         call opflag%Set(UERROR, HERE, "Column # outside expected range")
         return
      end if      
!
!-    Insert the mi columns of "matrix" into self%v starting at the column # "pnum":
!     (note: if ni < na, each inserted column are completed by 0, if ni > na, elements beyond
!      na are ignored)

      allocate(tmp(na,ma+mi), stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')   
         return   
      end if
      
      do j = 1, ma+mi
         do i = 1, na
            tmp(i,j)%str = ''
         end do
      end do                    
      
      minai = min(na,ni)
      p1 =      1 ; p2 = pnum - 1
      do j = p1, p2
         do i = 1, na
            call tmp(i,j)%assign ( rhs = a%v(i,j) ) ! RH 09/22
!            if ( allocated(a%v(i,j)%str) ) tmp(i,j)%str = (a%v(i,j)%str)
         end do
      end do
            
      p1 = p2 + 1 ; p2 = p2 + mi 
      do j = p1, p2
         do i = 1, minai
            call tmp(i,j)%assign ( rhs = matrix(i,j-p1+1) ) ! RH 09/22
!            if (allocated(matrix(i,j-p1+1)%str)) tmp(i,j)%str = (matrix(i,j-p1+1)%str)
         end do
      end do
            
      p1 = p2 + 1 ; p2 = ma + mi   
      do j = p1, p2
         do i = 1, na
            call tmp(i,j)%assign ( rhs = a%v(i,j-mi) ) ! RH 09/22
!            if (allocated(a%v(i,j-mi)%str)) tmp(i,j)%str = (a%v(i,j-mi)%str)
         end do
      end do      
      
      call move_alloc (from = tmp, to = a%v)
      
      a%ncol = ma + mi
      
   else
      call opflag%Set(UERROR,HERE,"Invalid option cr ="//cr//" (must be 'c' or 'r')")
      return
   end if
   
   END SUBROUTINE bk2_InsertSintoSk2

   
!=============================================================================================
   SUBROUTINE bk2_InsertIk2 ( self, b, pnum, cr )
!=============================================================================================
   class    (ik2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: pnum
   character(len=1),              intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of ik2 matrix "self" into the matrix "b". 
!
!  Description (see also bk2_InsertIintoIk2): the insertion is done
!  . at the row # pnum if cr = 'r' (with  1<= pnum <= b%nrow+1)
!  . else at the column # pnum (with 1 <= pnum <= b%ncol+1)
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ----------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_InsertIk2'
   logical                     :: b_is_empty
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   b_is_empty = .false.
   if ( .not. allocated(b) ) then
      b_is_empty = .true.
   else
      if ( b%typ == EMPTY ) b_is_empty = .true.
   end if      
!
!- If b is empty then b = self:
!      
   if ( b_is_empty ) then
      b = ik2_t ( matrix = self%v )      
      return
   end if  

   if ( cr /= 'r' .and. cr /= 'c' ) then
      call opflag%Set(UERROR,HERE,"Invalid option cr ="//cr//" (must be 'c' or 'r')")
      return
   end if
!
!- Insert self into b:
!
   select type (b)
      type is (ik2_t)
         call bk2_InsertIintoIk2 (b, self%v, pnum, cr) ; error_TraceNreturn(opflag, HERE) 
      type is (rk2_t)
         call bk2_InsertIintoRK2 (b, self%v, pnum, cr) ; error_TraceNreturn(opflag, HERE) 
      type is (ck2_t)
         call bk2_InsertIintoCk2 (b, self%v, pnum, cr) ; error_TraceNreturn(opflag, HERE) 
      class default
         call opflag%Set(UERROR, HERE, "Incompatible types")
         return
   end select             
   
   END SUBROUTINE bk2_InsertIk2
   

!=============================================================================================
   SUBROUTINE bk2_InsertRk2 ( self, b, pnum, cr )
!=============================================================================================
   class    (rk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: pnum
   character(len=1),              intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of the rk2 matrix "self" into the matrix "b". 
!
!  Description (see also bk2_InsertIintoIk2): the insertion is done
!  . at the row # pnum if cr = 'r' (with  1<= pnum <= b%nrow+1)
!  . else at the column # pnum (with 1 <= pnum <= b%ncol+1)
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ----------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_InsertRk2'
   logical                     :: b_is_empty   
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   b_is_empty = .false.
   if ( .not. allocated(b) ) then
      b_is_empty = .true.
   else
      if ( b%typ == EMPTY ) b_is_empty = .true.
   end if      
!
!- If b is empty then b = self:
!      
   if ( b_is_empty ) then
      b = rk2_t ( matrix = self%v )      
      return
   end if  
   
   if ( cr /= 'r' .and. cr /= 'c' ) then
      call opflag%Set(UERROR,HERE,"Invalid option cr ="//cr//" (must be 'c' or 'r')")
      return
   end if
!
!- If b is an ik2_t matrix, convert it into an rk2_t one:
!    
   if ( b%typ == ITYP ) call bk2_ConvertToRk2 ( b )
!
!- Insert self into b:
!   
   select type (b)
      type is (rk2_t)
         call bk2_InsertRintoRk2 (b, self%v, pnum, cr) ; error_TraceNreturn(opflag, HERE) 
      type is (ck2_t)
         call bk2_InsertRintoCk2 (b, self%v, pnum, cr) ; error_TraceNreturn(opflag, HERE)
      class default
         call opflag%Set(UERROR, HERE, "Incompatible types")
         return
   end select             
   
   END SUBROUTINE bk2_InsertRk2


!=============================================================================================
   SUBROUTINE bk2_InsertCk2 ( self, b, pnum, cr )
!=============================================================================================
   class    (ck2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: pnum
   character(len=1),              intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of ck2 matrix "self" into the matrix "b". 
!
!  Description (see also bk2_InsertIintoIk2): the insertion is done
!  . at the row # pnum if cr = 'r' (with  1<= pnum <= b%nrow+1)
!  . else at the column # pnum (with 1 <= pnum <= b%ncol+1)
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'bk2_InsertCk2'
   logical                     :: b_is_empty
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   b_is_empty = .false.
   if ( .not. allocated(b) ) then
      b_is_empty = .true.
   else
      if ( b%typ == EMPTY ) b_is_empty = .true.
   end if      
!
!- If b is empty then b = self:
!      
   if ( b_is_empty ) then
      b = ck2_t ( matrix = self%v )      
      return
   end if  

   if ( cr /= 'r' .and. cr /= 'c' ) then
      call opflag%Set(UERROR,HERE,"Invalid option cr ="//cr//" (must be 'c' or 'r')")
      return
   end if
!
!- If b is an ik2_t matrix or an rk2_t matrix, convert it into a ck2_t one
!    
   if ( b%typ == ITYP .or. b%typ == RTYP ) call bk2_ConvertToCk2 ( b )
!
!- Insert self into b:
!   
   select type (b)
      type is (ck2_t)
         call bk2_InsertCintoCk2 (b, self%v, pnum, cr) ; error_TraceNreturn(opflag, HERE) 
      class default
         call opflag%Set(UERROR, HERE, "Incompatible types")
         return
   end select             
   
   END SUBROUTINE bk2_InsertCk2
   

!=============================================================================================
   SUBROUTINE bk2_InsertLk2 ( self, b, pnum, cr )
!=============================================================================================
   class    (lk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: pnum
   character(len=1),              intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of lk2 matrix "self" into the matrix "b". 
!
!  Description (see also bk2_InsertIintoIk2): the insertion is done
!  . at the row # pnum if cr = 'r' (with  1<= pnum <= b%nrow+1)
!  . else at the column # pnum (with 1 <= pnum <= b%ncol+1)
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_InsertLk2'
   logical                     :: b_is_empty
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   b_is_empty = .false.
   if ( .not. allocated(b) ) then
      b_is_empty = .true.
   else
      if ( b%typ == EMPTY ) b_is_empty = .true.
   end if      
!
!- If b is empty then b = self:
!      
   if ( b_is_empty ) then
      b = lk2_t ( matrix = self%v )      
      return
   end if  

   if ( cr /= 'r' .and. cr /= 'c' ) then
      call opflag%Set(UERROR,HERE,"Invalid option cr ="//cr//" (must be 'c' or 'r')")
      return
   end if
!
!- Insert self into b:
!   
   select type (b)
      type is (lk2_t)
         call bk2_InsertLintoLk2 (b, self%v, pnum, cr) ; error_TraceNreturn(opflag, HERE) 
      class default
         call opflag%Set(UERROR, HERE, "Incompatible types")
         return
   end select             
   
   END SUBROUTINE bk2_InsertLk2


!=============================================================================================
   SUBROUTINE bk2_InsertSk2 ( self, b, pnum, cr )
!=============================================================================================
   class    (sk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: pnum
   character(len=1),              intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Inserts the rows or the columns of sk2 matrix "self" into the matrix "b". 
!
!  Description (see also bk2_InsertIintoIk2): the insertion is done
!  . at the row # pnum if cr = 'r' (with  1<= pnum <= b%nrow+1)
!  . else at the column # pnum (with 1 <= pnum <= b%ncol+1)
!-----------------------------------------------------------------------------------R.H. 07/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_InsertSk2'
   logical                     :: b_is_empty
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   b_is_empty = .false.
   if ( .not. allocated(b) ) then
      b_is_empty = .true.
   else
      if ( b%typ == EMPTY ) b_is_empty = .true.
   end if      
!
!- If b is empty then b = self:
!      
   if ( b_is_empty ) then
      b = sk2_t ( matrix = self%v )      
      return
   end if  

   if ( cr /= 'r' .and. cr /= 'c' ) then
      call opflag%Set(UERROR,HERE,"Invalid option cr ="//cr//" (must be 'c' or 'r')")
      return
   end if
!
!- Insert self into b:
!   
   select type (b)
      type is (sk2_t)
         call bk2_InsertSintoSk2 (b, self%v, pnum, cr) ; error_TraceNreturn(opflag, HERE) 
      class default
         call opflag%Set(UERROR, HERE, "Incompatible types")
         return
   end select             
   
   END SUBROUTINE bk2_InsertSk2


!=============================================================================================
   SUBROUTINE bk2_InsertBk2 ( self, b, pnum, cr )
!=============================================================================================
   class    (bk2_t),              intent(in    ) :: self
   class    (bk2_t), allocatable, intent(in out) :: b
   integer  (Ikind),              intent(in    ) :: pnum
   character(len=1),              intent(in    ) :: cr
!--------------------------------------------------------------------------------------------- 
!  Dummy procedure
!-----------------------------------------------------------------------------------R.H. 07/18
   
   if ( .not. allocated(b) ) b = bk2_t (nrow = IZERO, ncol = IZERO, typ = EMPTY)

   END SUBROUTINE bk2_InsertBk2
               

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!§ Resize ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! . bk2_ResizevIk2
! . bk2_ResizevRk2
! . bk2_ResizevCk2
! . bk2_ResizevLk2
! . bk2_ResizevSk2
! . bk2_ResizevBk2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
               
!=============================================================================================
   SUBROUTINE bk2_ResizevIk2 ( self, mode, n, m )
!=============================================================================================
   class    (ik2_t), intent(in out) :: self
   character(len=1), intent(in    ) :: mode   
   integer  (Ikind), intent(in    ) :: n, m
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the ik2 matrix "self". It is specially useful when  
!  "self" is already allocated but needs to be extended. 
!
! * If mode = 's', "self" is saved in a temporary var., de-allocated and re-allocated with the 
!   new size (n,m). The saved entries are then copied back to "self".
!
!  Warning: if one of the new sizes (n or m) is less than its old value, the part of "self" 
!           that is located beyond this size will be definitively lost. 
!
! * If mode = 'e' (e for 'erase'), "self" is simply de-allocated and re-allocated without a  
!   copy of its possible previous values.
!
!
!  Inputs
!
!     mode: 's' or 'e'
!      n,m: the (new) sizes of "self"
!     self: an ik2 matrix to be allocated or re-allocated with the new sizes (n,m)
!
!---------------------------------------------------------R.H. 12/16, 12/17 (use of move_alloc)
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_ResizevIk2'
   integer  (Ikind)              :: nn, mm, err, typ, zero
   integer  (Ikind), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = IZERO ; typ = ITYP

!$$$$$ à voir si ca:
   call util_alloc ( mode, self%v, n, m, opflag, zero )
   error_TraceNreturn(opflag, HERE)
   self%nrow = n ; self%ncol = m ; self%typ = ITYP
!$$$$$ est mieux que ça:            
#include "include/bk2_resizev.inc"
         
   END SUBROUTINE bk2_ResizevIk2
   

!=============================================================================================
   SUBROUTINE bk2_ResizevRk2 ( self, mode, n, m )
!=============================================================================================
   class    (rk2_t), intent(in out) :: self
   character(len=1), intent(in    ) :: mode   
   integer  (Ikind), intent(in    ) :: n, m
!--------------------------------------------------------------------------------------------- 
!  See description of bk2_ResizevIk2
!---------------------------------------------------------R.H. 12/16, 12/17 (use of move_alloc)
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_ResizevRk2'
   integer  (Ikind)              :: nn, mm, err, typ
   real     (Rkind), allocatable :: tmp(:,:)
   real     (Rkind)              :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = RZERO ; typ = RTYP
            
#include "include/bk2_resizev.inc"

   END SUBROUTINE bk2_ResizevRk2


!=============================================================================================
   SUBROUTINE bk2_ResizevCk2 ( self, mode, n, m )
!=============================================================================================
   class    (ck2_t), intent(in out) :: self
   character(len=1), intent(in    ) :: mode   
   integer  (Ikind), intent(in    ) :: n, m
!--------------------------------------------------------------------------------------------- 
!  See description of bk2_ResizevIk2
!---------------------------------------------------------R.H. 12/16, 12/17 (use of move_alloc)
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_ResizevCk2'
   integer  (Ikind)              :: nn, mm, err, typ
   complex  (Rkind), allocatable :: tmp(:,:)
   complex  (Rkind)              :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = CZERO ; typ = CTYP
            
#include "include/bk2_resizev.inc"
      
   END SUBROUTINE bk2_ResizevCk2


!=============================================================================================
   SUBROUTINE bk2_ResizevLk2 ( self, mode, n, m )
!=============================================================================================
   class    (lk2_t), intent(in out) :: self
   character(len=1), intent(in    ) :: mode   
   integer  (Ikind), intent(in    ) :: n, m
!--------------------------------------------------------------------------------------------- 
!  See description of bk2_ResizevIk2
!---------------------------------------------------------R.H. 12/16, 12/17 (use of move_alloc)
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_ResizevLk2'
   integer  (Ikind)              :: nn, mm, err, typ
   logical         , allocatable :: tmp(:,:)
   logical                       :: zero
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = LZERO ; typ = LTYP
            
#include "include/bk2_resizev.inc"
      
   END SUBROUTINE bk2_ResizevLk2


!=============================================================================================
   SUBROUTINE bk2_ResizevSk2 ( self, mode, n, m )
!=============================================================================================
   class    (sk2_t), intent(in out) :: self
   character(len=1), intent(in    ) :: mode   
   integer  (Ikind), intent(in    ) :: n, m
!--------------------------------------------------------------------------------------------- 
!  See description of bk2_ResizevIk2
!---------------------------------------------------------R.H. 12/16, 12/17 (use of move_alloc)
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_ResizevSk2'
   integer  (Ikind)              :: nn, mm, i, j, err, typ
   type     (str_t), allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return ! call opflag%set ()
            
   typ = STYP
            
   if ( mode == 'e' .or. self%typ == EMPTY ) then
      self%nrow = n ; self%ncol = m ; self%typ = typ
      if ( allocated(self%v) ) deallocate(self%v)      
      allocate(self%v(n,m), stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure for array "self%v"')
         return
      else
         do j = 1, m ; do i = 1, n ; self%v(i,j)%str = '' ; end do ; end do
      end if               
      return            
   end if
      
   if ( mode == 's' ) then
   
      allocate(tmp(n,m), stat=err)
            
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure for array "tmp"')  
         return    
      end if    

      do j = 1, m ; do i = 1, n ; tmp(i,j)%str = '' ; end do ; end do

      nn = min(n,self%nrow) ; mm = min(m,self%ncol)    

      do j = 1, mm
         do i = 1, nn
            call tmp(i,j)%assign ( rhs = self%v(i,j) ) ! RH 09/22
!            tmp(i,j)%str = (self%v(i,j)%str)
         end do
      end do 
                                                             
      call move_alloc (from = tmp, to = self%v)
      
      self%nrow = n ; self%ncol = m
   
   else
      call opflag%Set(UERROR, HERE, &
                               "Invalid mode << "//mode//" >> (must be << e >> or << s >>)") 
      return        
   end if          
      
   END SUBROUTINE bk2_ResizevSk2


!=============================================================================================
   SUBROUTINE bk2_ResizevBk2 ( self, mode, n, m )
!=============================================================================================
   class    (bk2_t), intent(in out) :: self
   character(len=1), intent(in    ) :: mode   
   integer  (Ikind), intent(in    ) :: n, m
!--------------------------------------------------------------------------------------------- 
!  See description of bk2_ResizevIk2
!--------------------------------------------------------------------------------------------- 
      
   self%nrow = n ; self%ncol = m
            
   END SUBROUTINE bk2_ResizevBk2


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!§ Set type and shape ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! . bk2_SetMatIk2nm  ( n x m null matrix)
! . bk2_SetMatRk2nm  ( n x m null matrix)
! . bk2_SetMatCk2nm  ( n x m null matrix)
! . bk2_SetMatLk2nm  ( n x m falses matrix)
! . bk2_SetMatSk2nm  ( n x m blank matrix)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!$$$$$ DEVRAIT ETRE INOUT !$$$$$

!=============================================================================================      
   SUBROUTINE bk2_SetMatIk2nm ( n, m, res )
!=============================================================================================      
   integer(Ikind), intent(in    ) :: n, m
   type   (ik2_t), intent(   out) :: res   
!---------------------------------------------------------------------------------------------
!  Sets "res" to ik2 type and initializes it to the n x m null matrix.
!---------------------------------------------------------------------R.H. 06/18, 04/19, 11/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatIk2nm'    
   integer  (Ikind)            :: err    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   allocate(res%v(n,m), source = IZERO, stat = err)
   if ( err == 0 ) then
      res%nrow = n ; res%ncol = m ; res%typ = Ityp
   else
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure') 
      return     
   end if
      
   END SUBROUTINE bk2_SetMatIk2nm


!=============================================================================================      
   SUBROUTINE bk2_SetMatRk2nm ( n, m, res )
!=============================================================================================      
   integer(Ikind), intent(in    ) :: n, m
   type   (rk2_t), intent(   out) :: res   
!---------------------------------------------------------------------------------------------
!  Sets "res" to rk2 type and initializes it to the n x m null matrix.
!---------------------------------------------------------------------R.H. 06/18, 04/19, 11/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatRk2nm'      
   integer  (Ikind)            :: err  
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   allocate(res%v(n,m), source = RZERO, stat = err)
   if ( err == 0 ) then
      res%nrow = n ; res%ncol = m ; res%typ = Rtyp
   else
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure')    
      return  
   end if
      
   END SUBROUTINE bk2_SetMatRk2nm
   

!=============================================================================================      
   SUBROUTINE bk2_SetMatCk2nm ( n, m, res )
!=============================================================================================      
   integer(Ikind), intent(in    ) :: n, m
   type   (ck2_t), intent(   out) :: res   
!---------------------------------------------------------------------------------------------
!  Sets "res" to ck2 type and initializes it to the n x m null matrix.
!---------------------------------------------------------------------R.H. 06/18, 04/19, 11/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatCk2nm'      
   integer  (Ikind)            :: err  
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   allocate(res%v(n,m), source = CZERO, stat = err)
   if ( err == 0 ) then
      res%nrow = n ; res%ncol = m ; res%typ = Ctyp
   else
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure') 
      return     
   end if
      
   END SUBROUTINE bk2_SetMatCk2nm


!=============================================================================================      
   SUBROUTINE bk2_SetMatLk2nm ( n, m, res )
!=============================================================================================      
   integer(Ikind), intent(in    ) :: n, m
   type   (lk2_t), intent(   out) :: res   
!---------------------------------------------------------------------------------------------
!  Sets "res" to lk2 type and initializes it to the n x m false matrix.
!---------------------------------------------------------------------R.H. 06/18, 04/19, 11/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatLk2nm'      
   integer  (Ikind)            :: err  
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   allocate(res%v(n,m), source = LZERO, stat = err)
   if ( err == 0 ) then
      res%nrow = n ; res%ncol = m ; res%typ = Ltyp
   else
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure') 
      return     
   end if
      
   END SUBROUTINE bk2_SetMatLk2nm


!=============================================================================================      
   SUBROUTINE bk2_SetMatSk2nm ( n, m, res )
!=============================================================================================      
   integer(Ikind), intent(in    ) :: n, m
   type   (sk2_t), intent(   out) :: res   
!---------------------------------------------------------------------------------------------
!  Sets "res" to sk2 type and initializes it to the n x m "blank" matrix.
!---------------------------------------------------------------------R.H. 06/18, 04/19, 11/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatSk2nm'      
   integer  (Ikind)            :: i, j, err  
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   allocate(res%v(n,m), stat = err)
   if ( err == 0 ) then
      res%nrow = n ; res%ncol = m ; res%typ = Styp
      do j = 1, m
         do i = 1, n
            res%v(i,j)%str = ''
         end do
      end do
   else
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure')  
      return    
   end if
      
   END SUBROUTINE bk2_SetMatSk2nm
   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Initialisation to a given matrix ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_SetMatIk2I (ik2 <-- Imat, or ik2(nxm) <-- Imat(1,1), or ik2 <-- ik2 + Imat)
!
! . bk2_SetMatRk2I (rk2 <-- Imat, or rk2(nxm) <-- Imat(1,1) or rk2 <-- rk2 + Imat)
! . bk2_SetMatRk2R (rk2 <-- Rmat, or rk2(nxm) <-- Rmat(1,1) or rk2 <-- Rk2 + Rmat)

! . bk2_SetMatCk2I (ck2 <-- Imat, or ck2(nxm) <-- Imat(1,1) or ck2 <-- ck2 + Imat)
! . bk2_SetMatCk2R (ck2 <-- Rmat, or ck2(nxm) <-- Rmat(1,1) or ck2 <-- ck2 + Rmat)
! . bk2_SetMatCk2C (ck2 <-- Cmat, or ck2(nxm) <-- Cmat(1,1) or ck2 <-- ck2 + Cmat)

! . bk2_SetMatLk2L (lk2 <-- Lmat, or lk2(nxm) <-- Lmat(1,1))
! . bk2_SetMatSk2S (sk2 <-- Smat, or sk2(nxm) <-- Smat(1,1) or sk2 <-- sk2 // Smat)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================      
   SUBROUTINE bk2_SetMatIk2I ( matrix, res, n, m, add )  ! ik2 <-- Imat
!=============================================================================================      
   integer(Ikind),           intent(in    ) :: matrix(:,:)
   type   (ik2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: n, m
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets the elements of the ik2 array "res" to those of the integer matrix "matrix".
!
!  If n, m are present "matrix" must be 1 x 1 and "res" is set to the matrix whose n x m 
!  elements are matrix(1,1).
!
!  If "add" is present and .true., the elements of "res" and "matrix" are summed up and put 
!  back to "res".
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatIk2I'        
   integer  (Ikind)            :: nn, mm, typ, err
   logical                     :: cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typ = ITYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setmat.inc" 

   END SUBROUTINE bk2_SetMatIk2I


!=============================================================================================      
   SUBROUTINE bk2_SetMatRk2I ( matrix, res, n, m, add )  ! rk2 <-- Imat
!=============================================================================================      
   integer(Ikind),           intent(in    ) :: matrix(:,:)
   type   (rk2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: n, m
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets the elements of the rk2 array "res" to those of the integer matrix "matrix".
!  (see description of bk2_SetMatIk2I)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatRk2I'        
   integer  (Ikind)            :: nn, mm, typ, err
   logical                     :: cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typ = RTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setmat.inc" 

   END SUBROUTINE bk2_SetMatRk2I


!=============================================================================================      
   SUBROUTINE bk2_SetMatRk2R ( matrix, res, n, m, add )  ! rk2 <-- Rmat
!=============================================================================================      
   real   (Rkind),           intent(in    ) :: matrix(:,:)
   type   (rk2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: n, m
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets the elements of the rk2 array "res" to those of the real matrix "matrix".
!  (see description of bk2_SetMatIk2I)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatRk2R'        
   integer  (Ikind)            :: nn, mm, typ, err
   logical                     :: cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typ = RTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setmat.inc" 

   END SUBROUTINE bk2_SetMatRk2R


!=============================================================================================      
   SUBROUTINE bk2_SetMatCk2I ( matrix, res, n, m, add )  ! ck2 <-- Imat
!=============================================================================================      
   integer(Ikind),           intent(in    ) :: matrix(:,:)
   type   (ck2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: n, m
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets the elements of the ck2 array "res" to those of the integer matrix "matrix".
!  (see description of bk2_SetMatIk2I)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatCk2I'        
   integer  (Ikind)            :: nn, mm, typ, err
   logical                     :: cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typ = CTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setmat.inc" 

   END SUBROUTINE bk2_SetMatCk2I


!=============================================================================================      
   SUBROUTINE bk2_SetMatCk2R ( matrix, res, n, m, add )  ! ck2 <-- Rmat
!=============================================================================================      
   real   (Rkind),           intent(in    ) :: matrix(:,:)
   type   (ck2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: n, m
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets the elements of the ck2 array "res" to those of the real matrix "matrix".
!  (see description of bk2_SetMatIk2I)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatCk2R'        
   integer  (Ikind)            :: nn, mm, typ, err
   logical                     :: cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   typ = CTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setmat.inc" 

   END SUBROUTINE bk2_SetMatCk2R


!=============================================================================================      
   SUBROUTINE bk2_SetMatCk2C ( matrix, res, n, m, add )  ! ck2 <-- Cmat
!=============================================================================================      
   complex(Rkind),           intent(in    ) :: matrix(:,:)
   type   (ck2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: n, m
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets the elements of the ck2 array "res" to those of the complex matrix "matrix".
!  (see description of bk2_SetMatIk2I)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatCk2C'        
   integer  (Ikind)            :: nn, mm, typ, err
   logical                     :: cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typ = CTYP   
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setmat.inc" 

   END SUBROUTINE bk2_SetMatCk2C


!=============================================================================================      
   SUBROUTINE bk2_SetMatLk2L ( res, matrix, n, m, add )  ! lk2 <-- Lmat
!=============================================================================================      
   type   (lk2_t),           intent(in out) :: res   
   logical       ,           intent(in    ) :: matrix(:,:)
   integer(Ikind), optional, intent(in    ) :: n, m
   logical       , optional, intent(in    ) :: add      
!---------------------------------------------------------------------------------------------
!  Sets the elements of the lk2 array "res" to those of the logical matrix "matrix".
!  (see description of bk2_SetMatIk2I)
!  Note: "add" has no effect here. 
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatLk2L'        
   integer  (Ikind)            :: nn, mm, err, typ
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   typ = LTYP
      
   err = 0
!
!- case 3: (type A(1:n,1:m) =  matrix(1,1))
!   
   if ( present(n) .and. present(m) ) then  
      if ( size(matrix) /= 1 ) then
         res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
         call opflag%Set(UERROR, HERE, &      
                       'Invalid data (if data = {matrix, n, m} --> size(matrix)=1) (case #3)')
         return
      endif
      if ( allocated(res%v) ) then
         if ( res%nrow /= n .or. res%ncol /= m ) then
            deallocate(res%v)
            allocate(res%v(n,m), source = matrix(1,1), stat = err)
         end if
      else
         allocate(res%v(n,m), source = matrix(1,1), stat = err)
      end if
      
      if ( err == 0 ) then
         res%nrow = n ; res%ncol = m ; res%typ = typ
      end if   
!
!- case 2: (type A = matrix)
!   
   else   
      nn = size(matrix,dim=1) ; mm = size(matrix,dim=2)

!note: il suffit de faire : res%v = matrix (lhs realloc assumed)

      if ( allocated(res%v) ) then
         if ( res%nrow /= nn .or. res%ncol /= mm ) then
            deallocate(res%v)
            allocate(res%v(nn,mm), source = matrix, stat = err)
         end if
      else
         allocate(res%v(nn,mm), source = matrix, stat = err)
      end if

      !if ( allocated(res%v) ) deallocate(res%v)
      !allocate(res%v(nn,mm), source = matrix, stat = err)
      
      if ( err == 0 ) then
         res%nrow = nn ; res%ncol = mm ; res%typ = typ
      end if   
      
   end if
   
   if ( err /= 0 ) then
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure')
      return
   end if

   END SUBROUTINE bk2_SetMatLk2L


!=============================================================================================      
   SUBROUTINE bk2_SetMatSk2S ( res, matrix, n, m, indi, indj, add )  ! sk2 <-- Smat
!=============================================================================================      
   type   (sk2_t),           intent(in out) :: res   
   type   (str_t),           intent(in    ) :: matrix(:,:)
   integer(Ikind), optional, intent(in    ) :: n, m
   integer(Ikind), optional, intent(in    ) :: indi(:), indj(:)
   logical       , optional, intent(in    ) :: add      
!---------------------------------------------------------------------------------------------
!  Sets the elements of the sk2 array "res" to those of the string matrix "matrix".
!  (see description of bk2_SetMatIk2I)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_SetMatSk2S'        
   integer  (Ikind)              :: nn, mm, err, i, j
   logical                       :: cumul
   character(len=:), allocatable :: m11   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   err = 0
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if 
!
!- case 3: (type A(1:n,1:m) =  matrix(1,1))
!   
   if ( present(n) .and. present(m) ) then  
      if ( size(matrix) /= 1 ) then
         res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
         call opflag%Set(UERROR, HERE, &      
                     'Invalid data (if data = {matrix, n, m} --> size(matrix)=1) (case #3)')
         return
      endif
      
      m11 = ''
      if ( allocated(matrix(1,1)%str) ) m11 = (matrix(1,1)%str)
      
      if ( cumul .and. allocated(res%v) ) then
         if ( n == res%nrow .and. m == res%ncol ) then
            do j = 1, m
               do i = 1, n
                  call str_concate ( a = res%v(i,j), b = m11, res = res%v(i,j) ) ! RH 09/22
!                   if ( allocated(res%v(i,j)%str) ) then
!                      res%v(i,j)%str = (res%v(i,j)%str) // m11
!                   else
!                      res%v(i,j)%str = m11
!                   end if
               end do
            end do         
         else
            call opflag%Set(UERROR, HERE, 'Incompatible shape (case #3.2)')      
            return
         end if   
      else
         if ( allocated(res%v) ) then
            if ( res%nrow /= n .or. res%ncol /= m ) then
               deallocate(res%v)
               allocate(res%v(n,m), stat = err)
            end if
         else
            allocate(res%v(n,m), stat = err)
         end if
      
         !if ( allocated(res%v) ) deallocate(res%v)
         !allocate(res%v(n,m), stat = err)
         
         if ( err == 0 ) then
            res%nrow = n ; res%ncol = m ; res%typ = STYP
            do j = 1, m
               do i = 1, n
                  call res%v(i,j)%assign ( rhs = m11 ) ! RH 09/22
!                  res%v(i,j)%str = m11 !
               end do
            end do            
         end if
      end if   
!
!- case 2: (type A = matrix)
!   
   else 
      nn = size(matrix,dim=1) ; mm = size(matrix,dim=2)
      
      if ( cumul .and. allocated(res%v) ) then
         if ( nn == res%nrow .and. mm == res%ncol ) then
            do j = 1, mm
               do i = 1, nn
                  call str_concate ( a=res%v(i,j), b=matrix(i,j), res=res%v(i,j) ) ! RH 09/22
!                   if ( allocated(matrix(i,j)%str) ) then
!                      m11 = (matrix(i,j)%str)
!                   else
!                      m11 = ''
!                   end if
!                   if ( allocated(res%v(i,j)%str) ) then
!                      res%v(i,j)%str = (res%v(i,j)%str) // m11
!                   else
!                      res%v(i,j)%str = m11
!                   end if
               end do
            end do            
         else
            call opflag%Set(UERROR, HERE, 'Incompatible shape (case #2)')
            return 
         end if                    
      else
         if ( allocated(res%v) ) then
            if ( res%nrow /= nn .or. res%ncol /= mm ) then
               deallocate(res%v)
               allocate(res%v(nn,mm), stat = err)
            end if
         else
            allocate(res%v(nn,mm), stat = err)
         end if
      
         !if ( allocated(res%v) ) deallocate(res%v)
         !allocate(res%v(nn,mm), stat = err)
         
         if ( err == 0 ) then
            res%nrow = nn ; res%ncol = mm ; res%typ = STYP
            do j = 1, mm
               do i = 1, nn
                  call res%v(i,j)%assign ( rhs = matrix(i,j) ) ! RH 09/22
!                   if ( allocated(matrix(i,j)%str) ) then
!                      res%v(i,j)%str = (matrix(i,j)%str)
!                   else  
!                      res%v(i,j)%str = ''
!                   end if    
               end do
            end do      
         end if
      end if   
   end if     
   
   if ( err /= 0 ) then
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure')
      return   
   end if            

   END SUBROUTINE bk2_SetMatSk2S
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Set some elements +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_SetMatIk2ijI (ik2(i,j) <-- Imat or ik2(i,j) <-- ik2(i,j) + Imat)
!
! . bk2_SetMatRk2ijI (rk2(i,j) <-- Imat or rk2(i,j) <-- rk2(i,j) + Imat)
! . bk2_SetMatRk2ijR (rk2(i,j) <-- Rmat or rk2(i,j) <-- rk2(i,j) + Rmat)
!
! . bk2_SetMatCk2ijI (ck2(i,j) <-- Imat or ck2(i,j) <-- ck2(i,j) + Imat)
! . bk2_SetMatCk2ijR (ck2(i,j) <-- Rmat or ck2(i,j) <-- ck2(i,j) + Rmat)
! . bk2_SetMatCk2ijC (ck2(i,j) <-- Cmat or ck2(i,j) <-- ck2(i,j) + Cmat)
!
! . bk2_SetMatLk2ijL (lk2(i,j) <-- Lmat)
!
! . bk2_SetMatSk2ijS (sk2(i,j) <-- Smat or sk2(i,j) <-- sk2(i,j) // Smat)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
   
!=============================================================================================      
   SUBROUTINE bk2_SetMatIk2ijI ( matrix, indi, res, indj, add )  ! ik2(i,j) <-- Imat
!=============================================================================================      
   integer(Ikind),           intent(in    ) :: matrix(:,:)
   integer(Ikind),           intent(in    ) :: indi(:)
   type   (ik2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: indj(:)
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets some of the elements of the ik2 array "res" to those of the integer matrix "matrix"
!
!  . If "indj" is not present: 
!
!   "matrix" must be an n-vector (i.e. 1 x n or n x 1 matrix) and "indi" contains the indexes
!   in column-major ordering. 
!
!   On input, "res" may either be empty (the result is then also an n-vector) or an existing
!   matrix of a size that must conform with "indi".
!
!  . If "indj" is present: 
!
!   "res" is a nn x mm matrix, with nn = maxval(indi) and mm = maxval(indj), whose elements at
!   indexes given by "indi" and "indj" are those of the matrix "matrix". The shape of "matrix"
!   must conform with the given indexes, i.e. shape(matrix) = [size(indi),size(indj)].
!
!   If "res" is not empty on input, it will be resized if needed.
!
!  Modified 04/19: 
!  if "add" is present and true, sum up the relevant elements of "res" and "matrix":
!
!                        res%v(indi,indj) = res%v(indi,indj) + matrix(i,j)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatIk2ijI'        
   integer  (Ikind)            :: nn, mm, ni, mi, na, ma, err, i, j, k, kg, sizea, typ
   integer  (Ikind)            :: zero, r
   logical                     :: is_scalar, cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = IZERO ; typ = ITYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setij.inc" 

   END SUBROUTINE bk2_SetMatIk2ijI


!=============================================================================================      
   SUBROUTINE bk2_SetMatRk2ijI ( matrix, indi, res, indj, add )  ! rk2(i,j) <-- Imat
!=============================================================================================      
   integer(Ikind),           intent(in    ) :: matrix(:,:)
   integer(Ikind),           intent(in    ) :: indi(:)
   type   (rk2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: indj(:)
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets some of the elements of the rk2 array "res" to those of the integer matrix "matrix"
!  (see description of bk2_SetMatIk2ijI)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatRk2ijI'        
   integer  (Ikind)            :: nn, mm, ni, mi, na, ma, err, i, j, k, kg, sizea, typ
   real     (Rkind)            :: zero, r
   logical                     :: is_scalar, cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = RZERO ; typ = RTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setij.inc" 

   END SUBROUTINE bk2_SetMatRk2ijI


!=============================================================================================      
   SUBROUTINE bk2_SetMatRk2ijR ( matrix, indi, res, indj, add )  ! rk2(i,j) <-- Rmat
!=============================================================================================      
   real   (Rkind),           intent(in    ) :: matrix(:,:)
   integer(Ikind),           intent(in    ) :: indi(:)
   type   (rk2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: indj(:)
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets some of the elements of the rk2 array "res" to those of the real matrix "matrix"
!  (see description of bk2_SetMatIk2ijI)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatRk2ijR'        
   integer  (Ikind)            :: nn, mm, ni, mi, na, ma, err, i, j, k, kg, sizea, typ
   real     (Rkind)            :: zero, r
   logical                     :: is_scalar, cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = RZERO ; typ = RTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setij.inc" 

   END SUBROUTINE bk2_SetMatRk2ijR
   

!=============================================================================================      
   SUBROUTINE bk2_SetMatCk2ijI ( matrix, indi, res, indj, add )  ! ck2(i,j) <-- Imat
!=============================================================================================      
   integer(Ikind),           intent(in    ) :: matrix(:,:)
   integer(Ikind),           intent(in    ) :: indi(:)
   type   (ck2_t),           intent(in out) :: res      
   integer(Ikind), optional, intent(in    ) :: indj(:)
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets some of the elements of the ck2 array "res" to those of the integer matrix "matrix"
!  (see description of bk2_SetMatIk2ijI)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatCk2ijI'        
   integer  (Ikind)            :: nn, mm, ni, mi, na, ma, err, i, j, k, kg, sizea, typ
   complex  (Rkind)            :: zero, r
   logical                     :: is_scalar, cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = CZERO ; typ = CTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setij.inc" 

   END SUBROUTINE bk2_SetMatCk2ijI


!=============================================================================================      
   SUBROUTINE bk2_SetMatCk2ijR ( matrix, indi, res, indj, add )  ! ck2(i,j) <-- Rmat
!=============================================================================================      
   real   (Rkind),           intent(in    ) :: matrix(:,:)
   integer(Ikind),           intent(in    ) :: indi(:)
   type   (ck2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: indj(:)
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets some of the elements of the ck2 array "res" to those of the real matrix "matrix"
!  (see description of bk2_SetMatIk2ijI)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatCk2ijR'        
   integer  (Ikind)            :: nn, mm, ni, mi, na, ma, err, i, j, k, kg, sizea, typ
   complex  (Rkind)            :: zero, r
   logical                     :: is_scalar, cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = CZERO ; typ = CTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setij.inc" 

   END SUBROUTINE bk2_SetMatCk2ijR
   

!=============================================================================================      
   SUBROUTINE bk2_SetMatCk2ijC ( matrix, indi, res, indj, add )  ! ck2(i,j) <-- Cmat
!=============================================================================================      
   complex(Rkind),           intent(in    ) :: matrix(:,:)
   integer(Ikind),           intent(in    ) :: indi(:)
   type   (ck2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: indj(:)
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets some of the elements of the ck2 array "res" to those of the complex matrix "matrix"
!  (see description of bk2_SetMatIk2ijI)
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatCk2ijC'        
   integer  (Ikind)            :: nn, mm, ni, mi, na, ma, err, i, j, k, kg, sizea, typ
   complex  (Rkind)            :: zero, r
   logical                     :: is_scalar, cumul
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = CZERO ; typ = CTYP
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
#include "include/bk2_setij.inc" 

   END SUBROUTINE bk2_SetMatCk2ijC     


!=============================================================================================      
   SUBROUTINE bk2_SetMatLk2ijL ( matrix, indi, res, indj, add )
!=============================================================================================      
   logical       ,           intent(in    ) :: matrix(:,:)
   integer(Ikind),           intent(in    ) :: indi(:)
   type   (lk2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: indj(:)
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets some of the elements of the lk2 array "res" to those of the logical matrix "matrix"
!  (see description of bk2_SetMatIk2ijI)
!  Note: "add" has no effect here. 
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_SetMatLk2ijL'        
   integer  (Ikind)            :: nn, mm, ni, mi, na, ma, err, i, j, k, kg, sizea, typ
   logical                     :: is_scalar, zero
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = LZERO ; typ = LTYP
   
   err = 0
!
!- case 5: (type A(indi,indj) = matrix)
!   
   if ( present(indj) ) then
      nn = maxval(indi) ; mm = maxval(indj) ; ni = size(indi) ; mi = size(indj)
      na = size(matrix,dim=1) ; ma = size(matrix,dim=2) ; is_scalar = (na*ma == 1)
      
      if ( .not. is_scalar .and. (ni /= na .or. mi /= ma) ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes (case #5)')
         return
      end if   
      
      if ( allocated(res%v) ) then
         if ( nn > res%nrow .or. mm > res%ncol ) then ! check if "res" needs to be resized
            nn = max(nn,res%nrow) ; mm = max(mm,res%ncol)
            call res%Resize ( 's', nn, mm)
            error_TraceNreturn(opflag, HERE)
         else
            nn = res%nrow ; mm = res%ncol   
         end if   
      else 
         allocate(res%v(nn,mm), source = zero, stat = err)
      end if   
      
      if ( err == 0 ) then            
         res%nrow = nn ; res%ncol = mm ; res%typ = typ
         if ( is_scalar ) then
            !res%v(indi,indj) = matrix(1,1) ! nagfor complains when subscript indi (or indj)
                                            ! has duplicate value. I use do loops:
            do j = 1, mi
               do i = 1, ni
                  res%v(indi(i),indj(j)) = matrix(1,1)
               end do
            end do      
         else
            !res%v(indi,indj) = matrix ! same
            do j = 1, mi
               do i = 1, ni
                  res%v(indi(i),indj(j)) = matrix(i,j)
               end do
            end do                 
         end if
      end if
!
!- case 4: (type A(indi) = matrix --> "matrix" must be an n-vector)
!          
   else
      na = size(matrix,dim=1) ; ma = size(matrix,dim=2)  
      
      if ( na /= 1 .and. ma /= 1 ) then
         call opflag%Set(UERROR, HERE, &
                                 'Incompatible shapes (case #4.0): the rhs must be a vector')
         return         
      end if
       
      sizea = na*ma ; is_scalar = (sizea == 1) ; nn = maxval(indi) ; ni = size(indi)
      
      if ( .not. is_scalar .and. sizea /= ni ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes (case #4.1)')
         return
      end if
      
      if ( .not. allocated(res%v) ) then 
!
!-       "res" is empty on input. The resulting matrix will be a nn x 1 or 1 x nn matrix:
!
         if ( is_scalar ) then
            allocate(res%v(nn,1), source = zero, stat = err)
            if ( err == 0 ) then
               res%nrow = nn ; res%ncol = 1 ; res%typ = typ
               !res%v(indi,1) = matrix(1,1)
               do i = 1, ni ; res%v(indi(i),1) = matrix(1,1) ; end do
            end if   
         else if ( na == ni .and. ma == 1 ) then
            allocate(res%v(nn,1), source = zero, stat = err)
            if ( err == 0 ) then
               res%nrow = nn ; res%ncol = 1 ; res%typ = typ
               !res%v(indi,1) = matrix(:,1)
               do i = 1, ni ; res%v(indi(i),1) = matrix(i,1) ; end do
            end if   
         else if ( ma == ni .and. na == 1 ) then
            allocate(res%v(1,nn), source = zero, stat = err)
            if ( err == 0 ) then
               res%nrow = 1 ; res%ncol = nn ; res%typ = typ
               !res%v(1,indi) = matrix(1,:)
               do i = 1, ni ; res%v(1,indi(i)) = matrix(1,i) ; end do
            end if   
         end if
      
      else if ( res%nrow == 1 ) then 
!
!-       "res" is a 1 x ? matrix. The result will be a 1 x nn matrix (resize it if necessary):
!
         if ( res%ncol < nn ) call res%Resize ('s', IONE, nn )
         error_TraceNreturn(opflag, HERE)

         if ( is_scalar ) then
            !res%v(1,indi) = matrix(1,1)         
            do i = 1, ni ; res%v(1,indi(i)) = matrix(1,1) ; end do
         else if ( na == 1 ) then
            !res%v(1,indi) = matrix(1,:)
            do i = 1, ni ; res%v(1,indi(i)) = matrix(1,i) ; end do
         else 
            !res%v(1,indi) = matrix(:,1)
            do i = 1, ni ; res%v(1,indi(i)) = matrix(i,1) ; end do
         end if
      
      else if ( res%ncol == 1 ) then 
!
!-       "res" is a ? x 1 matrix. The result will be a 1 x nn matrix (resize it if necessary):

         if ( res%nrow < nn ) call res%Resize ( 's', nn, IONE )
         error_TraceNreturn(opflag, HERE)

         if ( is_scalar ) then
            do i = 1, ni ; res%v(indi(i),1) = matrix(1,1) ; end do   
         else if ( na == 1 ) then
            !res%v(indi,1) = matrix(1,:)
            do i = 1, ni ; res%v(indi(i),1) = matrix(1,i); end do 
         else
            !res%v(indi,1) = matrix(:,1)
            do i = 1, ni ; res%v(indi(i),1) = matrix(i,1) ; end do 
         end if
         
      else  
!      
!-       "res" is n x m, copy the value of "matrix" into the elements of res%v
!        located at their natural positions (column-major ordering) given in "indi":
                          
         if ( res%nrow * res%ncol < nn ) then
            call opflag%Set(UERROR, HERE, 'Incompatible shapes (case #4.2)')
            return
         end if                          

         do k = 1, ni
            kg = indi(k)
            i = mod(kg,res%nrow) ; if ( i == 0 ) i = res%nrow ; j = (kg - i)/res%nrow + 1
            if ( is_scalar ) then
               res%v(i,j) = matrix(1,1)
            else if ( na == 1 ) then
               res%v(i,j) = matrix(1,k)
            else
               res%v(i,j) = matrix(k,1)
            end if
         end do         
      end if   
   end if

   if ( err /= 0 ) then
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure')
      return
   end if           
      
   END SUBROUTINE bk2_SetMatLk2ijL
    

!=============================================================================================      
   SUBROUTINE bk2_SetMatSk2ijS ( matrix, indi, res, indj, add ) ! sk2(i,j) <-- S
!=============================================================================================    
   type   (str_t),           intent(in    ) :: matrix(:,:)
   integer(Ikind),           intent(in    ) :: indi(:)
   type   (sk2_t),           intent(in out) :: res   
   integer(Ikind), optional, intent(in    ) :: indj(:)
   logical       , optional, intent(in    ) :: add   
!---------------------------------------------------------------------------------------------
!  Sets some of the elements of the sk2 array "res" to those of the string matrix "matrix"
!  (see description of bk2_SetMatIk2ijI)
!
!  Modified 04/19: if "add" is present and .true., the relevant elements of "res" are 
!  concatened with those of "matrix".
!----------------------------------------------------------------------------R.H. 06/18, 04/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_SetMatSk2ijS'        
   integer  (Ikind)              :: nn, mm, ni, mi, na, ma, err, i, j, k, kg, sizea, jj
   logical                       :: is_scalar, cumul
   character(len=:), allocatable :: m11
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   if ( present(add) ) then
      cumul = add
   else
      cumul = .false.
   end if         
   
   err = 0
!
!- case 5: (type A(indi,indj) = matrix)
!   
   if ( present(indj) ) then
      nn = maxval(indi) ; mm = maxval(indj) ; ni = size(indi) ; mi = size(indj)
      na = size(matrix,dim=1) ; ma = size(matrix,dim=2) ; is_scalar = (na*ma == 1)
      
      if ( .not. is_scalar .and. (ni /= na .or. mi /= ma) ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes (case #5)')
         return
      end if   
      
      if ( allocated(res%v) ) then
         if ( nn > res%nrow .or. mm > res%ncol ) then ! check if "res" needs to be resized
            nn = max(nn,res%nrow) ; mm = max(mm,res%ncol)
            call res%Resize ( 's', nn, mm)
            error_TraceNreturn(opflag, HERE)
         else
            nn = res%nrow ; mm = res%ncol   
         end if   
      else 
         allocate(res%v(nn,mm), stat = err)
         do j = 1, mm
            do i = 1, nn
               res%v(i,j)%str = ''
            end do
         end do      
      end if   
      
      if ( err == 0 ) then            
         res%nrow = nn ; res%ncol = mm ; res%typ = STYP
         if ( is_scalar ) then
            if (allocated(matrix(1,1)%str)) then
               m11 = (matrix(1,1)%str)
            else
               m11 = ''
            end if      
            if (cumul) then
               do j = 1, mi
                  jj = indj(j)
                  do i = 1, ni
                     call str_concate ( a   = res%v(indi(i),jj), b = m11, &  ! RH 09/22
                                        res = res%v(indi(i),jj)           )
!                      if (allocated(res%v(indi(i),jj)%str)) then
!                         res%v(indi(i),jj)%str = (res%v(indi(i),jj)%str) // m11
!                      else
!                         res%v(indi(i),jj)%str = m11
!                      end if   
                  end do
               end do
            else
               do j = 1, mi
                  jj = indj(j)
                  do i = 1, ni
                     call res%v(indi(i),jj)%assign ( rhs = m11 ) ! RH 09/22
!                     res%v(indi(i),jj)%str = m11
                  end do
               end do
            end if
            !res%v(indi,indj) = matrix(1,1)
         else
            if ( cumul ) then
               do j = 1, mi
                  jj = indj(j)
                  do i = 1, ni
                     call str_concate ( a   = res%v(indi(i),jj), b = matrix(i,j), &  ! RH 09/22
                                        res = res%v(indi(i),jj)                   )
                                        
!                      if (allocated(matrix(i,j)%str)) then
!                         m11 = (matrix(i,j)%str)
!                      else
!                         m11 = ''
!                      end if
!                      if (allocated(res%v(indi(i),jj)%str)) then      
!                         res%v(indi(i),jj)%str = (res%v(indi(i),jj)%str) // m11
!                      else
!                         res%v(indi(i),jj)%str = m11
!                      end if     
                  end do
               end do
            else
               do j = 1, mi
                  jj = indj(j)
                  do i = 1, ni
                     call res%v(indi(i),jj)%assign ( rhs = matrix(i,j) ) ! RH 09/22
!                      if (allocated(matrix(i,j)%str)) then
!                         res%v(indi(i),jj)%str = (matrix(i,j)%str)
!                      else
!                         res%v(indi(i),jj)%str = ''
!                      end if     
                  end do
               end do
            end if
            !res%v(indi,indj) = matrix
         end if
      end if
!
!- case 4: (type A(indi) = matrix --> "matrix" must be an n-vector)
!          
   else 
      nn = maxval(indi) ; ni = size(indi) ; na = size(matrix,dim=1) ; ma = size(matrix,dim=2)

      sizea = na*ma ; is_scalar = (sizea == 1)
      
      if ( .not. is_scalar .and. sizea /= ni ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes (case #4.1)')
         return
      end if
      
      if ( .not. allocated(res%v) ) then 
!
!-       "res" is empty on input. The resulting matrix will be a nn x 1 or 1 x nn matrix:
!
         if ( is_scalar ) then
            allocate(res%v(nn,1), stat = err)
            do i = 1, nn ; res%v(i,1)%str = '' ; end do       
            if ( err == 0 ) then
               res%nrow = nn ; res%ncol = 1 ; res%typ = STYP
               if ( allocated(matrix(1,1)%str) ) then
                  m11 = (matrix(1,1)%str)
               else
                  m11 = ''
               end if      
               do i = 1, ni
                  call res%v(indi(i),1)%assign ( rhs = m11 ) ! RH 09/22
!                  res%v(indi(i),1)%str = m11
               end do
            end if   
         else if ( na == ni .and. ma == 1 ) then
            allocate(res%v(nn,1), stat = err)
            do i = 1, nn ; res%v(i,1)%str = '' ; end do                          
            if ( err == 0 ) then
               res%nrow = nn ; res%ncol = 1 ; res%typ = STYP
               do i = 1, ni
                  call res%v(indi(i),1)%assign ( rhs = matrix(i,1) ) ! RH 09/22
!                  if (allocated(matrix(i,1)%str)) res%v(indi(i),1)%str = (matrix(i,1)%str)
               end do      
            end if   
         else if ( ma == ni .and. na == 1 ) then
            allocate(res%v(1,nn), stat = err)
            do i = 1, nn ; res%v(1,i)%str = '' ; end do                        
            if ( err == 0 ) then
               res%nrow = 1 ; res%ncol = nn ; res%typ = STYP
               do i = 1, ni
                  call res%v(1,indi(i))%assign ( rhs = matrix(1,i) ) ! RH 09/22
!                  if (allocated(matrix(1,i)%str)) res%v(1,indi(i))%str = (matrix(1,i)%str)
               end do   
            end if   
         end if
      
      else if ( res%nrow == 1 ) then 
!
!-       "res" is a 1 x ? matrix. The result will be a 1 x nn matrix (resize it if necessary):
!
         if ( res%ncol < nn ) call res%Resize ('s', IONE, nn )
         error_TraceNreturn(opflag, HERE)

         if ( is_scalar ) then
            if ( allocated(matrix(1,1)%str) ) then
               m11 = (matrix(1,1)%str)
            else
               m11 = ''
            end if
            if ( cumul ) then
               do i = 1, ni  
                  call str_concate ( a   = res%v(1,indi(i)), b = m11, &  ! RH 09/22
                                     res = res%v(1,indi(i))           )
!                   if ( allocated(res%v(1,indi(i))%str) ) then  
!                      res%v(1,indi(i))%str = (res%v(1,indi(i))%str) // m11
!                   else
!                      res%v(1,indi(i))%str = m11
!                   end if   
               end do  
            else
               do i = 1, ni      
                  call res%v(1,indi(i))%assign ( rhs = m11 ) ! RH 09/22
!                  res%v(1,indi(i))%str = m11
               end do  
            end if                
            !res%v(1,indi) = matrix(1,1)
         else if ( na == 1 ) then
            if ( cumul ) then
               do i = 1, ni
                  call str_concate ( a   = res%v(1,indi(i)), b = matrix(1,i), &  ! RH 09/22
                                     res = res%v(1,indi(i))                   )

!                   if (allocated(matrix(1,i)%str)) then
!                      m11 = (matrix(1,i)%str)
!                   else
!                      m11 = ''
!                   end if
!                   if ( allocated(res%v(1,indi(i))%str) ) then
!                      res%v(1,indi(i))%str = res%v(1,indi(i))%str // m11
!                   else
!                      res%v(1,indi(i))%str = m11
!                   end if
               end do         
            else
               do i = 1, ni
                  call res%v(1,indi(i))%assign ( rhs = matrix(1,i) ) ! RH 09/22
!                   if ( allocated(matrix(1,i)%str) ) then
!                      res%v(1,indi(i))%str = (matrix(1,i)%str)
!                   else
!                      res%v(1,indi(i))%str = ''
!                   end if
               end do         
            end if
         else 
            if ( cumul ) then
               do i = 1, ni
                  call str_concate ( a   = res%v(1,indi(i)), b = matrix(i,1), &  ! RH 09/22
                                     res = res%v(1,indi(i))                   )

!                   if ( allocated(matrix(i,1)%str) ) then
!                      m11 = (matrix(i,1)%str)
!                   else
!                      m11 = ''
!                   end if
!                   if ( allocated(res%v(1,indi(i))%str) ) then
!                      res%v(1,indi(i))%str = (res%v(1,indi(i))%str) // m11
!                   else
!                      res%v(1,indi(i))%str = m11
!                   end if
               end do
            else   
               do i = 1, ni
                  call res%v(1,indi(i))%assign ( rhs = matrix(i,1) ) ! RH 09/22
!                   if ( allocated(matrix(i,1)%str) ) then
!                      res%v(1,indi(i))%str = (matrix(i,1)%str)
!                   else
!                      res%v(1,indi(i))%str = ''
!                   end if
               end do
            end if
         end if
      
      else if ( res%ncol == 1 ) then 
!
!-       "res" is a ? x 1 matrix. The result will be a 1 x nn matrix (resize it if necessary):

         if ( res%nrow < nn ) call res%Resize ( 's', nn, IONE )
         error_TraceNreturn(opflag, HERE)

         if ( is_scalar ) then
            if ( allocated(matrix(1,1)%str) ) then
               m11 = (matrix(1,1)%str)
            else
               m11 = ''
            end if
            if ( cumul ) then
               do i = 1, ni
                  call str_concate ( a   = res%v(1,indi(i)), b = m11, &  ! RH 09/22
                                     res = res%v(1,indi(i))           )
!                   if ( allocated(res%v(1,indi(i))%str) ) then
!                      res%v(1,indi(i))%str = (res%v(1,indi(i))%str) // m11
!                   else   
!                      res%v(1,indi(i))%str = m11
!                   end if   
               end do   
            else
               do i = 1, ni
                  call res%v(1,indi(i))%assign ( rhs = m11 ) ! RH 09/22
!                  res%v(1,indi(i))%str = m11
               end do   
            end if                     
            !res%v(1,indi) = matrix(1,1)
         else if ( na == 1 ) then
            if ( cumul ) then
               do i = 1, ni
                  call str_concate ( a   = res%v(indi(i),1), b = matrix(1,i), &  ! RH 09/22
                                     res = res%v(indi(i),1)                   )

!                   if ( allocated(matrix(1,i)%str) ) then
!                      m11 = (matrix(1,i)%str)
!                   else
!                      m11 = ''
!                   end if
!                   if ( allocated(res%v(indi(i),1)%str) ) then  
!                      res%v(indi(i),1)%str = (res%v(indi(i),1)%str) // m11
!                   else
!                      res%v(indi(i),1)%str = m11
!                   end if
               end do   
            else        
               do i = 1, ni
                  call res%v(indi(i),1)%assign ( rhs = matrix(1,i) ) ! RH 09/22
!                   if (allocated(matrix(1,i)%str)) then
!                      res%v(indi(i),1)%str = (matrix(1,i)%str)
!                   else
!                      res%v(indi(i),1)%str = ''
!                   end if
               end do        
            end if
            !res%v(indi,1) = matrix(1,:)
         else
            if ( cumul ) then
               do i = 1, ni
                  call str_concate ( a   = res%v(indi(i),1), b = matrix(i,1), &  ! RH 09/22
                                     res = res%v(indi(i),1)                   )

!                   if ( allocated(matrix(i,1)%str) ) then
!                      m11 = (matrix(i,1)%str)
!                   else
!                      m11 = ''
!                   end if
!                   if ( allocated(res%v(indi(i),1)%str) ) then
!                      res%v(indi(i),1)%str = (res%v(indi(i),1)%str) // m11
!                   else
!                      res%v(indi(i),1)%str = m11
!                   end if
               end do
            else   
               do i = 1, ni
                  call res%v(indi(i),1)%assign ( rhs = matrix(i,1) ) ! RH 09/22
!                   if (allocated(matrix(i,1)%str)) then
!                      res%v(indi(i),1)%str = (matrix(i,1)%str)
!                   else
!                      res%v(indi(i),1)%str = ''
!                   end if
               end do
            end if
         end if
         
      else  
!      
!-       "res" is n x m, copy the value of "matrix" into the elements of res%v
!        located at their natural positions (column-major ordering) given in "indi":
                          
         if ( res%nrow * res%ncol < nn ) then
            call opflag%Set(UERROR, HERE, 'Incompatible shapes (case #4.2)')
            return
         end if

         m11 = ''
         if ( is_scalar .and. allocated(matrix(1,1)%str) ) m11 = (matrix(1,1)%str)

         if ( cumul ) then
            do k = 1, ni
               kg = indi(k)
               i = mod(kg,res%nrow) ; if ( i == 0 ) i = res%nrow ; j = (kg - i)/res%nrow + 1
               if ( is_scalar ) then
                  call str_concate ( a = res%v(i,j), b = m11, res = res%v(i,j) ) ! RH 09/22
!                   if ( allocated(res%v(i,j)%str) ) then
!                      res%v(i,j)%str = (res%v(i,j)%str) // m11
!                   else
!                      res%v(i,j)%str = m11
!                   end if   
               else if ( na == 1 ) then
                  if ( allocated(matrix(1,k)%str) ) then
                     m11 = (matrix(1,k)%str)
                  else
                     m11 = ''
                  end if
                  call str_concate ( a = res%v(i,j), b = m11, res = res%v(i,j) ) ! RH 09/22
!                   if ( allocated(res%v(i,j)%str) ) then     
!                      res%v(i,j)%str = (res%v(i,j)%str) // m11
!                   else
!                      res%v(i,j)%str = m11
!                   end if     
               else
                  if ( allocated(matrix(k,1)%str) ) then
                     m11 = (matrix(k,1)%str)
                  else
                     m11 = ''
                  end if
                  call str_concate ( a = res%v(i,j), b = m11, res = res%v(i,j) ) ! RH 09/22
!                   if ( allocated(res%v(i,j)%str) ) then    
!                      res%v(i,j)%str = (res%v(i,j)%str) // m11
!                   else
!                      res%v(i,j)%str = m11
!                   end if     
               end if
            end do     
         else
            do k = 1, ni
               kg = indi(k)
               i = mod(kg,res%nrow) ; if ( i == 0 ) i = res%nrow ; j = (kg - i)/res%nrow + 1
               if ( is_scalar ) then
                  call res%v(i,j)%assign ( rhs = m11 ) ! RH 09/22
!                  res%v(i,j)%str = m11
               else if ( na == 1 ) then
                  call res%v(i,j)%assign ( rhs = matrix(1,k) ) ! RH 09/22
!                   if (allocated(matrix(1,k)%str)) then
!                      res%v(i,j)%str = (matrix(1,k)%str)
!                   else
!                      res%v(i,j)%str = ''
!                   end if     
               else
                  call res%v(i,j)%assign ( rhs = matrix(k,1) ) ! RH 09/22
!                   if (allocated(matrix(k,1)%str)) then
!                      res%v(i,j)%str = (matrix(k,1)%str)
!                   else
!                      res%v(i,j)%str = ''
!                   end if     
               end if
            end do     
         end if         
      end if        
   end if     
   
   if ( err /= 0 ) then
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set(IERROR, HERE, 'Allocation failure')
      return
   end if            

   END SUBROUTINE bk2_SetMatSk2ijS


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Constructors ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_ConstructorIk2
! . bk2_ConstructorRk2
! . bk2_ConstructorCk2
! . bk2_ConstructorLk2
! . bk2_ConstructorSk2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================      
   FUNCTION bk2_ConstructorIk2 ( matrix, n, m, indi, indj ) result ( res )
!=============================================================================================      
   integer(Ikind), optional, intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: n, m
   integer(Ikind), optional, intent(in) :: indi(:), indj(:)
   type   (ik2_t)                       :: res   
!---------------------------------------------------------------------------------------------
!  Constructor for ik2 type
!----------------------------------------------------------------------------R.H. 04/18, 11/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_ConstructorIk2'        
!---------------------------------------------------------------------------------------------

   if ( present(matrix) .and. present(indi) ) then
      call bk2_SetMatIk2ijI (matrix=matrix, indi=indi, indj=indj,res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(matrix) ) then
      call bk2_SetMatIk2I (matrix=matrix, n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)      
   else if ( present(n) .and. present(m) ) then
      call bk2_SetMatIk2nm (n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)      
   else
      call opflag%Set(UERROR, HERE, 'Bad choice of arguments')
      return
   end if
 
   END FUNCTION bk2_ConstructorIk2
   

!=============================================================================================      
   FUNCTION bk2_ConstructorRk2 ( matrix, n, m, indi, indj ) result ( res )
!=============================================================================================      
   real   (Rkind), optional, intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: n, m
   integer(Ikind), optional, intent(in) :: indi(:), indj(:)
   type   (rk2_t)                       :: res   
!---------------------------------------------------------------------------------------------
!  Constructor for rk2 type
!----------------------------------------------------------------------------R.H. 04/18, 11/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_ConstructorRk2'        
!---------------------------------------------------------------------------------------------

   if ( present(matrix) .and. present(indi) ) then
      call bk2_SetMatRk2ijR (matrix=matrix, indi=indi, indj=indj, res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(matrix) ) then
      call bk2_SetMatRk2R (matrix=matrix, n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(n) .and. present(m) ) then
      call bk2_SetMatRk2nm (n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)
   else
      call opflag%Set(UERROR, HERE, 'Bad choice of arguments')
      return
   end if

   END FUNCTION bk2_ConstructorRk2
   

!=============================================================================================      
   FUNCTION bk2_ConstructorCk2 ( matrix, n, m, indi, indj ) result ( res )
!=============================================================================================      
   complex(Rkind), optional, intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: n, m
   integer(Ikind), optional, intent(in) :: indi(:), indj(:)
   type   (ck2_t)                       :: res   
!---------------------------------------------------------------------------------------------
!  Constructor for ck2 type
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_ConstructorCk2'        
!---------------------------------------------------------------------------------------------

   if ( present(matrix) .and. present(indi) ) then
      call bk2_SetMatCk2ijC (matrix=matrix, indi=indi, indj=indj, res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(matrix) ) then
      call bk2_SetMatCk2C (matrix=matrix, n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(n) .and. present(m) ) then
      call bk2_SetMatCk2nm (n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)
   else
      call opflag%Set(UERROR, HERE, 'Bad choice of arguments')
      return
   end if

   END FUNCTION bk2_ConstructorCk2


!=============================================================================================      
   FUNCTION bk2_ConstructorLk2 ( matrix, n, m, indi, indj ) result ( res )
!=============================================================================================      
   logical       , optional, intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: n, m
   integer(Ikind), optional, intent(in) :: indi(:), indj(:)
   type   (lk2_t)                       :: res   
!---------------------------------------------------------------------------------------------
!  Constructor for lk2 type
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_ConstructorLk2'        
!---------------------------------------------------------------------------------------------

   if ( present(matrix) .and. present(indi) ) then
      call bk2_SetMatLk2ijL (matrix=matrix, indi=indi, indj=indj, res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(matrix) ) then
      call bk2_SetMatLk2L (matrix=matrix, n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(n) .and. present(m) ) then
      call bk2_SetMatLk2nm (n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)
   else
      call opflag%Set(UERROR, HERE, 'Bad choice of arguments')
      return
   end if

   END FUNCTION bk2_ConstructorLk2
   

!=============================================================================================      
   FUNCTION bk2_ConstructorSk2 ( matrix, n, m, indi, indj ) result ( res )
!=============================================================================================      
   type   (str_t), optional, intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: n, m
   integer(Ikind), optional, intent(in) :: indi(:), indj(:)
   type   (sk2_t)                       :: res   
!---------------------------------------------------------------------------------------------
!  Constructor for sk2 type
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_ConstructorSk2'        
!---------------------------------------------------------------------------------------------

   if ( present(matrix) .and. present(indi) ) then
      call bk2_SetMatSk2ijS (matrix=matrix, indi=indi, indj=indj, res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(matrix) ) then
      call bk2_SetMatSk2S (matrix=matrix, n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)
   else if ( present(n) .and. present(m) ) then
      call bk2_SetMatSk2nm (n=n, m=m, res=res)
      error_TraceNreturn(opflag, HERE)
   else
      call opflag%Set(UERROR, HERE, 'Bad choice of arguments')
      return
   end if
   
   END FUNCTION bk2_ConstructorSk2


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Destructors +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_DestroyIk2
! . bk2_DestroyRk2
! . bk2_DestroyCk2
! . bk2_DestroyLk2
! . bk2_DestroySk2
! . bk2_DestroyBk2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_DestroyIk2 ( self )
!=============================================================================================
   class(ik2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18
   
   self%nrow = 0 ; self%ncol = 0 ; self%typ = EMPTY
   if ( allocated(self%v) ) deallocate(self%v)
      
   END SUBROUTINE bk2_DestroyIk2


!=============================================================================================
   SUBROUTINE bk2_DestroyRk2 ( self )
!=============================================================================================
   class(rk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18
   
   self%nrow = 0 ; self%ncol = 0 ; self%typ = EMPTY
   if ( allocated(self%v) ) deallocate(self%v)
      
   END SUBROUTINE bk2_DestroyRk2


!=============================================================================================
   SUBROUTINE bk2_DestroyCk2 ( self )
!=============================================================================================
   class(ck2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18
   
   self%nrow = 0 ; self%ncol = 0 ; self%typ = EMPTY
   if ( allocated(self%v) ) deallocate(self%v)
      
   END SUBROUTINE bk2_DestroyCk2


!=============================================================================================
   SUBROUTINE bk2_DestroyLk2 ( self )
!=============================================================================================
   class(lk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18
   
   self%nrow = 0 ; self%ncol = 0 ; self%typ = EMPTY
   if ( allocated(self%v) ) deallocate(self%v)
      
   END SUBROUTINE bk2_DestroyLk2


!=============================================================================================
   SUBROUTINE bk2_DestroySk2 ( self )
!=============================================================================================
   class(sk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18
   
   self%nrow = 0 ; self%ncol = 0 ; self%typ = EMPTY
   if ( allocated(self%v) ) deallocate(self%v)
      
   END SUBROUTINE bk2_DestroySk2


!=============================================================================================
   SUBROUTINE bk2_DestroyBk2 ( self )
!=============================================================================================
   class(bk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18
   
   self%nrow = 0 ; self%ncol = 0 ; self%typ = EMPTY
      
   END SUBROUTINE bk2_DestroyBk2


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Getters +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_GetMatxBk2  (bk2 --> Imat, Rmat, Cmat, Lmat, Smat, CHmat)
! . bk2_GetMatxIk2  (ik2 --> Imat)
! . bk2_GetMatxRk2  (rk2 --> Rmat)
! . bk2_GetMatxCk2  (ck2 --> Cmat)
! . bk2_GetMatxLk2  (lk2 --> Lmat)
! . bk2_GetMatxSk2  (sk2 --> Smat)
! . bk2_GetMatxChar (sk2 --> CHmat)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_GetMatxBk2 ( self, I, R, C, L, S, Ch )
!=============================================================================================   
   class    (bk2_t),                        intent(in    ) :: self
   integer  (Ikind), optional, allocatable, intent(in out) :: I (:,:)
   real     (Rkind), optional, allocatable, intent(in out) :: R (:,:)
   complex(  Rkind), optional, allocatable, intent(in out) :: C (:,:)
   logical         , optional, allocatable, intent(in out) :: L (:,:)
   type     (str_t), optional, allocatable, intent(in out) :: S (:,:)
   character(len=:), optional, allocatable, intent(in out) :: Ch(:,:)   
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_GetMatxBk2'        
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   select type (self)
      type is (ik2_t)
         if ( .not. present(I) ) then
            call opflag%Set(UERROR, HERE, 'Ouput argument "I" needed')
            return
         end if
         call bk2_GetMatxIk2 (self,I)
      type is (rk2_t)
         if ( .not. present(R) ) then
            call opflag%Set(UERROR, HERE, 'Ouput argument "R" needed')
            return
         end if
         call bk2_GetMatxRk2 (self,R)
      type is (ck2_t)
         if ( .not. present(C) ) then
            call opflag%Set(UERROR, HERE, 'Ouput argument "C" needed')
            return
         end if
         call bk2_GetMatxCk2 (self,C)
      type is (lk2_t)
         if ( .not. present(L) ) then
            call opflag%Set(UERROR, HERE, 'Ouput argument "L" needed')
            return
         end if
         call bk2_GetMatxLk2 (self,L)
      type is (sk2_t)
         if ( .not. present(S) .and. .not. present(Ch) ) then
            call opflag%Set(UERROR, HERE, 'Ouput argument "S" or "Ch" needed')
            return
         end if
         if ( present(S)  ) call bk2_GetMatxSk2 (self,S) 
         if ( present(Ch) ) call bk2_GetMatxChar(self,Ch) 
   end select
    
   error_TraceNreturn(opflag, HERE)
    
   END SUBROUTINE bk2_GetMatxBk2 
   
   
!=============================================================================================
   SUBROUTINE bk2_GetMatxIk2 ( self, res )
!=============================================================================================   
   class  (ik2_t),              intent(in    ) :: self
   integer(Ikind), allocatable, intent(in out) :: res(:,:) ! make it inout
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_GetMatxIk2'        
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

! (08/24): make res inout and let the compiler to reallocate the lhs if needed
!    allocate(res(self%nrow,self%ncol), source = self%v, stat = err)
!    
!    if ( err /= 0 ) then
!       call opflag%Set(IERROR, HERE, 'Allocation failure')
!       return
!    end if
   res = self%v
   
   END SUBROUTINE bk2_GetMatxIk2     


!=============================================================================================
   SUBROUTINE bk2_GetMatxRk2 ( self, res )
!=============================================================================================   
   class(rk2_t),              intent(in    ) :: self
   real (Rkind), allocatable, intent(in out) :: res(:,:)
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_GetMatxRk2'        
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

! (08/24): make res inout and let the compiler to reallocate the lhs if needed
!    allocate(res(self%nrow,self%ncol), source = self%v, stat = err)
!    
!    if ( err /= 0 ) then
!       call opflag%Set(IERROR, HERE, 'Allocation failure')
!       return
!    end if
   res = self%v
             
   END SUBROUTINE bk2_GetMatxRk2     


!=============================================================================================
   SUBROUTINE bk2_GetMatxCk2 ( self, res )
!=============================================================================================   
   class  (ck2_t),              intent(in    ) :: self
   complex(Rkind), allocatable, intent(in out) :: res(:,:)
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_GetMatxCk2'        
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

! (08/24): make res inout and let the compiler to reallocate the lhs if needed
!    allocate(res(self%nrow,self%ncol), source = self%v, stat = err)
!    
!    if ( err /= 0 ) then
!       call opflag%Set(IERROR, HERE, 'Allocation failure')
!       return
!    end if
   res = self%v
             
   END SUBROUTINE bk2_GetMatxCk2    
   

!=============================================================================================
   SUBROUTINE bk2_GetMatxLk2 ( self, res )
!=============================================================================================   
   class(lk2_t),              intent(in    ) :: self
   logical     , allocatable, intent(in out) :: res(:,:)
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_GetMatxLk2'        
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

! (08/24): make res inout and let the compiler to reallocate the lhs if needed
!    allocate(res(self%nrow,self%ncol), source = self%v, stat = err)
!    
!    if ( err /= 0 ) then
!       call opflag%Set(IERROR, HERE, 'Allocation failure')
!       return
!    end if
   res = self%v
             
   END SUBROUTINE bk2_GetMatxLk2   

   
!=============================================================================================
   SUBROUTINE bk2_GetMatxSk2 ( self, res )
!=============================================================================================   
   class(sk2_t),              intent(in    ) :: self
   type (str_t), allocatable, intent(in out) :: res(:,:)
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'bk2_GetMatxSk2'                
   integer  (Ikind)            :: i, j, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
    
!    allocate(res(self%nrow,self%ncol), stat = err)
! 
!    if ( err /= 0 ) then
!       call opflag%Set(IERROR, HERE, 'Allocation failure')
!       return
!    end if

   if ( allocated(res) ) then
      if ( size(res,1) /= self%nrow .or. size(res,2) /= self%ncol ) then
         deallocate(res) ; allocate(res(self%nrow,self%ncol), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if      
      end if
   else
      allocate(res(self%nrow,self%ncol), stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure')
         return
      end if
   end if   
   
   do j = 1, self%ncol
      do i = 1, self%nrow
         res(i,j) = self%v(i,j)
!          if ( allocated(self%v(i,j)%str) ) then
!             res(i,j)%str = (self%v(i,j)%str)
!           else
!             res(i,j)%str = ''
!           end if    
      end do
    end do        
          
   END SUBROUTINE bk2_GetMatxSk2  


!=============================================================================================
   SUBROUTINE bk2_GetMatxChar ( self, res )
!=============================================================================================   
   class    (sk2_t),              intent(in    ) :: self
   character(len=:), allocatable, intent(in out) :: res(:,:)
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'bk2_GetMatxSk2'                
   integer  (Ikind)            :: i, j, maxlen, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   maxlen = 1
   do j = 1, self%ncol
      do i = 1, self%nrow
         if ( allocated(self%v(i,j)%str) ) then
            maxlen = max(maxlen,len_trim(self%v(i,j)%str,kind=Ikind))
         end if   
      end do
   end do      
   
   if ( allocated(res) ) then
      if ( size(res,1)/=self%nrow .or. size(res,2)/=self%ncol .or. len(res)/=maxlen ) then
         deallocate(res)
         allocate(character(len=maxlen) :: res(self%nrow,self%ncol), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure')
            return
         end if
      end if
   else  
      allocate(character(len=maxlen) :: res(self%nrow,self%ncol), stat = err)
      if ( err /= 0 ) then
         call opflag%Set(IERROR, HERE, 'Allocation failure')
         return
      end if
   end if
            
   do j = 1, self%ncol
      do i = 1, self%nrow
         if ( allocated(self%v(i,j)%str) ) then
            res(i,j) = (self%v(i,j)%str)
          else
            res(i,j) = repeat(' ', maxlen) !''
          end if    
      end do
    end do        
          
   END SUBROUTINE bk2_GetMatxChar    
      
      
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Operators (algebraic) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_Minus (-bk2)
! . bk2_Sub   (bk2 - bk2)
! . bk2_Add   (bk2 + bk2)
! . bk2_Mult  (bk2 * bk2)
! . bk2_Div   (bk2 / bk2)
! . bk2_Pow   (bk2 ^ bk2)
! . bk2_ePow  (bk2 .^ bk2)
! . bk2_eMult (bk2 .* bk2)
! . bk2_eDiv  (bk2 ./ bk2)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
              
!=============================================================================================
   SUBROUTINE bk2_Minus ( a, b )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a
   class(bk2_t), allocatable, intent(in out) :: b
!---------------------------------------------------------------------------------------------
!  Numeric negation (b =-a)
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_Minus'                
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   select type (a)
      type is (ik2_t)
         call bk2_reallocIfNeeded ( b, ITYP, a%nrow, a%ncol )
         error_TraceNreturn(opflag, HERE)
         select type (b)
            type is (ik2_t)
               b%v = -a%v
         end select
      type is (rk2_t)
         call bk2_reallocIfNeeded ( b, RTYP, a%nrow, a%ncol )
         error_TraceNreturn(opflag, HERE)
         select type (b)
            type is (rk2_t)
               b%v = -a%v
         end select
      type is (ck2_t)
         call bk2_reallocIfNeeded ( b, CTYP, a%nrow, a%ncol )
         error_TraceNreturn(opflag, HERE)
         select type (b)
            type is (ck2_t)
               b%v = -a%v
         end select
      type is (lk2_t)
         call opflag%Set(UERROR,HERE,'<< b = -a >> with << a >> boolean: not defined (use ~)')
         return
      type is (sk2_t)
         call opflag%Set(UERROR,HERE,'<< b = -a >> with << a >> string: not defined' ) 
         return
   end select
   
   END SUBROUTINE bk2_Minus
   
!=============================================================================================
   SUBROUTINE bk2_Minus0 ( a, b )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a
   class(bk2_t), allocatable, intent(in out) :: b
!---------------------------------------------------------------------------------------------
!  Numeric negation (b =-a)
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind)              :: err
   integer(Ikind), allocatable :: tmpi(:,:)       
   real   (Rkind), allocatable :: tmpr(:,:)       
   complex(Rkind), allocatable :: tmpc(:,:)       
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   err = 0   
   select type (a)
      type is (ik2_t)
         !b = ik2_t(matrix = -a%v) ! (to be avoided?)
         !allocate(ik2_t::tmp) ; select type (tmp) ; type is (ik2_t) ; tmp%v = -a%v ; end select
         allocate(tmpi(a%nrow,a%ncol), stat = err)
         if ( err == 0 ) then
            tmpi(:,:) = -a%v(:,:)
            call bk2_moveAllocImatToBk2 ( from = tmpi, to = b )
            return
         end if
      type is (rk2_t)
         allocate(tmpr(a%nrow,a%ncol), stat = err)
         if ( err == 0 ) then
            tmpr(:,:) = -a%v(:,:)
            call bk2_moveAllocRmatToBk2 ( from = tmpr, to = b )
            return
         end if
      type is (ck2_t)
         allocate(tmpc(a%nrow,a%ncol), stat = err)
         if ( err == 0 ) then
            tmpc(:,:) = -a%v(:,:)
            call bk2_moveAllocCmatToBk2 ( from = tmpc, to = b )
            return
         end if
      type is (lk2_t)
         call opflag%Set(UERROR, 'bk2_Minus', &
                                  '<< b = -a >> with << a >> boolean: not defined (use ~)')
         return
      type is (sk2_t)
         call opflag%Set(UERROR, 'bk2_Minus', &
                                  '<< b = -a >> with << a >> string: not defined' ) 
         return
   end select

   if ( err /= 0 ) then
      call opflag%Set(IERROR, 'bk2_Minus', 'Allocation failure')
      return
   end if
   
   END SUBROUTINE bk2_Minus0
   
!=============================================================================================
   SUBROUTINE bk2_Sub ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Subtraction of two bk2 matrices (c = a - b)
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'bk2_Sub'
   logical         , parameter   :: minus = .false.          
   integer  (Ikind)              :: na, ma, nb, mb, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   na = a%nrow ; ma = a%ncol ; nb = b%nrow ; mb = b%ncol
      
   if ( na * ma /= 1 .and. nb * mb /= 1 ) then
      if ( na /= nb .or. ma /= mb ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes for << - >>')
         return
      end if
   end if
   
   err = 0
   
   select type (a)
   
      type is (ik2_t)
         select type (b)
            type is (ik2_t)
               call bk2_AddII ( a%v, minus, b%v, c )
            type is (rk2_t)
               call bk2_AddIR ( a%v, minus, b%v, c )                
            type is (ck2_t)
               call bk2_AddIC ( a%v, minus, b%v, c )                
            class default
               err = 1               
         end select

      type is (rk2_t)
         select type (b)
            type is (ik2_t)
               call bk2_AddRI ( a%v, minus, b%v, c )
            type is (rk2_t)
               call bk2_AddRR ( a%v, minus, b%v, c )                
            type is (ck2_t)
               call bk2_AddRC ( a%v, minus, b%v, c )                
            class default
               err = 1               
         end select

      type is (ck2_t)
         select type (b)
            type is (ik2_t)
               call bk2_AddCI ( a%v, minus, b%v, c )                
            type is (rk2_t)
               call bk2_AddCR ( a%v, minus, b%v, c )                
            type is (ck2_t)
               call bk2_AddCC ( a%v, minus, b%v, c )                
            class default
               err = 1               
         end select
                  
      class default
            err = 1               
      
   end select    

   error_TraceNreturn(opflag, HERE)
   
   if ( err == 1 ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << - >>')
      return
   end if
         
   END SUBROUTINE bk2_Sub
      
   
!=============================================================================================
   SUBROUTINE bk2_Add ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Addition of two bk2 matrices (c = a + b)
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_Add'
   logical         , parameter   :: plus = .true.          
   integer  (Ikind)              :: na, ma, nb, mb, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   na = a%nrow ; ma = a%ncol ; nb = b%nrow ; mb = b%ncol
      
   if ( na * ma /= 1 .and. nb * mb /= 1 ) then
      if ( na /= nb .or. ma /= mb ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes for << + >>')
         return
      end if
   end if
   
   err = 0
      
   select type (a)
   
      type is (ik2_t)
         select type (b)
            type is (ik2_t)
               call bk2_AddII ( a%v, plus, b%v, c )
            type is (rk2_t)
               call bk2_AddIR ( a%v, plus, b%v, c )
            type is (ck2_t)
               call bk2_AddIC ( a%v, plus, b%v, c )           
            class default
               err = 1               
         end select

      type is (rk2_t)
         select type (b)
            type is (ik2_t)
               call bk2_AddIR ( b%v, plus, a%v, c )
            type is (rk2_t)
               call bk2_AddRR ( a%v, plus, b%v, c )
            type is (ck2_t)
               call bk2_AddRC ( a%v, plus, b%v, c )
            class default
               err = 1               
         end select
      
      type is (ck2_t)
         select type (b)
            type is (ik2_t)
               call bk2_AddIC ( b%v, plus, a%v, c )
            type is (rk2_t)
               call bk2_AddRC ( b%v, plus, a%v, c )
            type is (ck2_t)
               call bk2_AddCC ( a%v, plus, b%v, c )
            class default
               err = 1               
         end select

      type is (lk2_t)
         err = 1
         
      type is (sk2_t)
         select type (b)
            type is (sk2_t)
               call bk2_AddSS ( a%v, b%v, c )
            class default
               err = 1
         end select
                  
      class default
            err = 1               
      
   end select

   error_TraceNreturn(opflag, HERE)
   
   if ( err == 1 ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << + >>')
      return
   end if
      
   END SUBROUTINE bk2_Add


!=============================================================================================
   SUBROUTINE bk2_Mult ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Multiplication (matrix product) of two bk2 matrices (c = a * b)
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter   :: HERE = 'bk2_Mult'      
   integer  (Ikind)              :: na, ma, nb, mb, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   na = a%nrow ; ma = a%ncol ; nb = b%nrow ; mb = b%ncol
  
   if ( na * ma /= 1 .and. nb * mb /= 1 ) then
      if ( ma /= nb ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes for << * >>')
         return
      end if
   end if
   
   err = 0
   
   select type (a)
   
      type is (ik2_t)
         select type (b)
            type is (ik2_t)
                call bk2_MultII ( a%v, b%v, commute =.false., res = c )
            type is (rk2_t)
                call bk2_MultIR ( a%v, b%v, commute =.false., res = c )
            type is (ck2_t)
                call bk2_MultIC ( a%v, b%v, commute =.false., res = c )
                
            class default
               err = 1               
         end select

      type is (rk2_t)
         select type (b)
            type is (ik2_t)
                call bk2_MultIR ( b%v, a%v, commute =.true., res = c )
            type is (rk2_t)
                call bk2_MultRR ( a%v, b%v, commute =.false., res = c )
            type is (ck2_t)
                call bk2_MultRC ( a%v, b%v, commute =.false., res = c )
            class default
               err = 1               
         end select

      type is (ck2_t)
         select type (b)
            type is (ik2_t)
                call bk2_MultIC ( b%v, a%v, commute =.true., res = c )
            type is (rk2_t)
                call bk2_MultRC ( b%v, a%v, commute =.true., res = c )
            type is (ck2_t)
                call bk2_MultCC ( a%v, b%v, commute =.false., res = c )
            class default
               err = 1               
         end select

      class default
            err = 1               
      
   end select    
   
   error_TraceNreturn(opflag, HERE)
   
   if ( err == 1 ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << * >>')
      return
   end if
      
   END SUBROUTINE bk2_Mult


!=============================================================================================
   SUBROUTINE bk2_Div ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Division of two bk2 matrices (c = a / b)
!  Expected cases: at least one of the two matrices is 1x1
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'bk2_Div'              
   integer  (Ikind)              :: err
   class    (bk2_t), allocatable :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
               
   if ( a%nrow * a%ncol /= 1 .and. b%nrow * b%ncol /= 1 ) then
      call opflag%Set(UERROR, HERE, &
                       '<< a / b >> not defined for two matrices (a or b must be scalar)') 
      return
   end if
   
   err = 0

   select type (a)
   
      type is (ik2_t)
         select type (b)
            type is (ik2_t)
                call bk2_DivII ( A1 = a%v, A2 = b%v, res = c )                    !i/i 
            type is (rk2_t)
                call bk2_DivIR ( A1 = a%v, A2 = b%v, res = c, commute = .false. ) !i/r 
            type is (ck2_t)
                call bk2_DivIC ( A1 = a%v, A2 = b%v, res = c, commute = .false. ) !i/z
            class default
               err = 1               
         end select

      type is (rk2_t)
         select type (b)
            type is (ik2_t)
                call bk2_DivIR ( A1 = b%v, A2 = a%v, res = c, commute = .true. )  !r/i 
            type is (rk2_t)
                call bk2_DivRR ( A1 = a%v, A2 = b%v, res = c )                    !r/r
            type is (ck2_t)
                call bk2_DivRC ( A1 = a%v, A2 = b%v, res = c, commute = .false. ) !r/z
            class default
               err = 1               
         end select

      type is (ck2_t)
         select type (b)
            type is (ik2_t)
                call bk2_DivIC ( A1 = b%v, A2 = a%v, res = c, commute = .true. ) !z/i 
            type is (rk2_t)
                call bk2_DivRC ( A1 = b%v, A2 = a%v, res = c, commute = .true. ) !z/r 
            type is (ck2_t)
                call bk2_DivCC ( A1 = a%v, A2 = b%v, res = c )                   !z/z
            class default
               err = 1               
         end select

      class default
            err = 1               
      
   end select    
   
   error_TraceNreturn(opflag, HERE)
   
   if ( err == 1 ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << / >>')
      return
   end if
      
   END SUBROUTINE bk2_Div
   

!=============================================================================================
   SUBROUTINE bk2_Pow ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  (very limited) Exponentiation of two bk2 matrices (c = a ^ b)
!
!  Note: at the present, at least one of the two matrices must be scalar (ie a 1x1 matrix) and
!        the only considered cases are:
!        . if b is 1x1:  c%v = a%v ^ b%v(1,1) with 
!                        - b%v(1,1) can only be INTEGER
!                        - and a%v is SQUARE (integer, real or complex)
!        . if a is 1x1:  c%v = a%v(1,1) ^ b%v  gives the same result than a%v(1,1) .^ b%v
!
!  Versions:
!  Initial: 04/18. Modified (totally!): 08/24
!----------------------------------------------------------------------------R.H. 04/18, 08/24

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter   :: HERE = 'bk2_Pow'                     
   integer  (Ikind)              :: na, ma, ta, nb, mb, tb, nc, mc, tc, nama, nbmb, nexpo, j
   real     (Rkind), allocatable :: Rinv(:,:)
   complex  (Rkind), allocatable :: Cinv(:,:)
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
!
!- Determine the shape (nc,mc) of the result c:
!
   na = a%nrow ; ma = a%ncol ; nama = na*ma ; ta = a%typ
   nb = b%nrow ; mb = b%ncol ; nbmb = nb*mb ; tb = b%typ

   tc = IZERO
   
   if ( ta < ITYP .or. ta > CTYP .or. tb < ITYP .or. tb > CTYP ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << a ^ b >>')
      return      
   else if ( nama == 0 .or. nbmb == 0 ) then
      c = bk2_t()
      call opflag%Set ( WARNING, HERE, &
                     '<< c = a ^ b >> with << a >> or << b >> empty (--> c = [ ])' )
      return      
   else if ( nama /= 1 .and. nbmb /= 1 ) then
      call opflag%Set ( UERROR, HERE, '<< a ^ b >> not defined for two matrices, ' // &
                                      'at least one of them must be scalar (1x1)'     ) 
      return
      
   else if ( nama == 1 ) then
   
      ! case with a scalar (c = a(1,1) .^ b): set tc=-1 (bk2_epow will be called)
      nc = nb ; mc = mb ; tc = -IONE 
      
   else
   
      ! cases with b scalar:
      
      if ( tb /= ITYP ) then
         call opflag%Set ( UERROR, HERE, & 
            '<< a ^ b >> is limited to integer power (b) when the matrix (a) is not 1x1' )
         return
      else if ( na /= ma ) then
         call opflag%Set ( UERROR, HERE, &
         '<< a ^ b >> is limited to square matrix (a) when the power (b) is scalar (integer)')
         return
      else
         ! c = a ^ b(1,1)
         nc = na ; mc = nc ; tc = max(RTYP,a%typ)
         select type ( b )
            type is ( ik2_t ) ; nexpo = b%v(1,1)
         end select 
         
         ! quick return for particular cases (nexpo = 0 or 1):
         
         if ( nexpo == IZERO ) then
            ! a^0 => c = I 
            call bk2_reallocIfNeeded ( c, ITYP, nc, mc )
            error_TraceNreturn(opflag, HERE)
            select type ( c )
               type is ( ik2_t )
                  c%v(:,:) = IZERO
                  do j = 1, nc
                     c%v(j,j) = IONE
                  end do
            end select
            return
         else if ( nexpo == IONE ) then
            ! a^1 => c = a
            call bk2_reallocIfNeeded ( c, a%typ, nc, mc )
            call a%copyv ( c )
            error_TraceNreturn(opflag, HERE)
            return
         end if
      
      end if
   end if
   
   if ( tc == -IONE ) then
!
!-    Compute a(1,1) ^ b   as   a(1,1) .^ b:
!
      call bk2_ePow ( a, b, c ) ; error_TraceNreturn(opflag, HERE)
      return
   endif
!
!- Compute c = a ^ b(1,1):
!
   call bk2_reallocIfNeeded ( c, tc, nc, mc ) ; error_TraceNreturn(opflag, HERE)
   
   select type ( a )
   
      type is ( ik2_t )
         select type ( c )
            type is ( rk2_t )          
               if ( nexpo >= 0 ) then
                  c%v(:,:) = a%v
                  do j = 1, nexpo-1
                     c%v(:,:) = matmul(c%v, a%v)
                  end do
               else
                  call util_InvMats ( I = a%v, res = Rinv, stat = opflag )
                  error_TraceNreturn(opflag, HERE)
                  c%v(:,:) = Rinv
                  do j = 1, abs(nexpo)-1
                     c%v(:,:) = matmul(c%v, Rinv)
                  end do
               end if    
               if ( util_isIntg(c%v) ) tc = ITYP
         end select
         
         if ( tc == ITYP ) call bk2_convertToIk2 ( c )

      type is ( rk2_t )
         select type ( c )
            type is ( rk2_t )          
               if ( nexpo >= 0 ) then
                  c%v(:,:) = a%v
                  do j = 1, nexpo-1
                     c%v(:,:) = matmul(c%v, a%v)
                  end do
               else
                  call util_InvMats ( R = a%v, res = Rinv, stat = opflag )
                  error_TraceNreturn(opflag, HERE)
                  c%v(:,:) = Rinv
                  do j = 1, abs(nexpo)-1
                     c%v(:,:) = matmul(c%v, Rinv)
                  end do
               end if              
         end select

      type is ( ck2_t )
         select type ( c )
            type is ( ck2_t )          
               if ( nexpo >= 0 ) then
                  c%v(:,:) = a%v
                  do j = 1, nexpo-1
                     c%v(:,:) = matmul(c%v, a%v)
                  end do
               else
                  call util_InvMats ( C = a%v, res = Cinv, stat = opflag )
                  error_TraceNreturn(opflag, HERE)
                  c%v(:,:) = Cinv
                  do j = 1, abs(nexpo)-1
                     c%v(:,:) = matmul(c%v, Cinv)
                  end do
               end if              
         end select
   end select
               
   END SUBROUTINE bk2_Pow


!=============================================================================================
   SUBROUTINE bk2_ePow ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation (element-wise) of two bk2 matrices (c = a .^ b)
!
!  Versions:
!  Initial: 04/18. Modified (totally!): 08/24
!----------------------------------------------------------------------------R.H. 04/18, 08/24

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter   :: HERE = 'bk2_ePow'                     
   integer  (Ikind)              :: na, ma, ta, nb, mb, tb, nc, mc, tc, nama, nbmb, tres, i, j
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
!
!- Determine the shape (nc,mc) of the result c:
!
   na = a%nrow ; ma = a%ncol ; nama = na*ma ; nb = b%nrow ; mb = b%ncol ; nbmb = nb*mb

   if ( nama == 0 .or. nbmb == 0 ) then
      c = bk2_t()
      call opflag%Set ( WARNING, HERE, &
                     '<< c = a .^ b >> with << a >> or << b >> empty (--> c = [ ])' )
      return      
   else if ( nama /= 1 .and. nbmb /= 1 ) then
      if ( na /= nb .or. ma /= mb ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes for << .^ >>')
         return
      else
         ! c = a .^ b:
         nc = na ; mc = ma
      end if
   else if ( nama == 1 ) then
      ! c = a(1,1) .^ b
      nc = nb ; mc = mb
   else
      ! c = a .^ b(1,1)
      nc = na ; mc = ma
   end if
!
!- Determine the type (tc) of the result c (is assumed real only when the power (b) is integer
!  and the base (a) is integer or real. Otherwise the result is assumed complex):
! 
   ta = a%typ ; tb = b%typ 
   if ( ta < ITYP .or. ta > CTYP .or. tb < ITYP .or. tb > CTYP ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << .^ >>')
      return
   else if ( tb == ITYP .and. ta <= RTYP ) then
      tc = RTYP 
   else
      tc = CTYP
   end if
!
!- (Re)allocate c if needed:
!
   call bk2_reallocIfNeeded ( c, tc, nc, mc ) ; error_TraceNreturn(opflag, HERE)
!
!- Now compute c and determine its actual type:
!
   tc = ITYP
   
   select type ( b )
      type is (ik2_t) 
         ! ---------------------
         ! the power is integer:
         ! ---------------------
         select type ( a )
            type is (ik2_t)
               ! the base is integer (i^i):
               select type ( c )
                  type is ( rk2_t )
#include "include/bk2_epow.inc"
               end select
            type is (rk2_t)  
               ! the base is real (r^i):
               select type ( c )
                  type is ( rk2_t )
#include "include/bk2_epow.inc"
               end select
            type is (ck2_t)  
               ! the base is complex (c^i):
               select type ( c )
                  type is ( ck2_t )
#include "include/bk2_epow.inc"
               end select
         end select
      type is (rk2_t) 
         ! ------------------
         ! the power is real:
         ! ------------------
         select type ( a )
            type is (ik2_t)
               ! the base is integer (i^r):
               select type ( c )
                  type is ( ck2_t )
#include "include/bk2_epow.inc"
               end select
            type is (rk2_t)  
               ! the base is real (r^r):
               select type ( c )
                  type is ( ck2_t )
#include "include/bk2_epow.inc"
               end select
            type is (ck2_t)  
               ! the base is complex (c^r):
               select type ( c )
                  type is ( ck2_t )
#include "include/bk2_epow.inc"
               end select
         end select               
      type is (ck2_t) 
         ! ---------------------
         ! the power is complex:
         ! ---------------------
         select type ( a )
            type is (ik2_t)
               ! the base is integer (i^c):
               select type ( c )
                  type is ( ck2_t )
#include "include/bk2_epow.inc"
               end select
            type is (rk2_t)  
               ! the base is real (r^c):
               select type ( c )
                  type is ( ck2_t )
#include "include/bk2_epow.inc"
               end select
            type is (ck2_t)  
               ! the base is complex (c^c):
               select type ( c )
                  type is ( ck2_t )
#include "include/bk2_epow.inc"
               end select
         end select
   end select
!
!- See if c can be converted (if tc < c%typ):
!
   if ( tc == ITYP ) then
      call bk2_convertToIk2 ( c )
   else if ( tc == RTYP .and. c%typ == CTYP ) then
      call bk2_convertToRk2 ( c )
   end if
               
   END SUBROUTINE bk2_ePow
   
   
!=============================================================================================
   SUBROUTINE bk2_eMult ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Multiplication (element-wise) of two bk2 matrices (c = a .* b)
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_eMult'                              
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
            
   if ( a%nrow * a%ncol /= 1 .and. b%nrow * b%ncol /= 1 ) then
      if ( a%nrow /= b%nrow .or. a%ncol /= b%ncol ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes for << .* >>')
         return
      end if
   end if

   err = 0

   select type (a)
   
      type is (ik2_t)
         select type (b)
            type is (ik2_t)
                call bk2_eMultII (a%v, b%v, c)                
            type is (rk2_t)
                call bk2_eMultIR (a%v, b%v, c)                
            type is (ck2_t)
                call bk2_eMultIC (a%v, b%v, c)                
            class default
               err = 1               
         end select

      type is (rk2_t)
         select type (b)
            type is (ik2_t)
                call bk2_eMultIR (b%v, a%v, c)                
            type is (rk2_t)
                call bk2_eMultRR (a%v, b%v, c)                
            type is (ck2_t)
                call bk2_eMultRC (a%v, b%v, c)                
            class default
               err = 1               
         end select

      type is (ck2_t)
         select type (b)
            type is (ik2_t)
                call bk2_eMultIC (b%v, a%v, c)                
            type is (rk2_t)
                call bk2_eMultRC (b%v, a%v, c)                
            type is (ck2_t)
                call bk2_eMultCC (a%v, b%v, c)                
            class default
               err = 1               
         end select

      class default
            err = 1               
      
   end select    

   error_TraceNreturn(opflag, HERE)
   
   if ( err == 1 ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << .* >>')
      return
   end if
               
   END SUBROUTINE bk2_eMult
   
   
!=============================================================================================
   SUBROUTINE bk2_eDiv ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Divide (element-wise) of two bk2 matrices (c = a ./ b)
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'bk2_eDiv'                                      
   integer  (Ikind)              :: err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if ( a%nrow * a%ncol /= 1 .and. b%nrow * b%ncol /= 1 ) then
      if ( a%nrow /= b%nrow .or. a%ncol /= b%ncol ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes for << ./ >>')
         return
      end if
   end if

   err = 0
   
   select type (a)
   
      type is (ik2_t)
         select type (b)
            type is (ik2_t)
                call bk2_eDivII ( A1 = a%v, A2 = b%v, res = c, commute =.false. ) !i/i
            type is (rk2_t)
                call bk2_eDivIR ( A1 = a%v, A2 = b%v, res = c, commute =.false. ) !i/r
            type is (ck2_t)
                call bk2_eDivIC ( A1 = a%v, A2 = b%v, res = c, commute =.false. ) !i/c
            class default
               err = 1               
         end select

      type is (rk2_t)
         select type (b)
            type is (ik2_t)
                call bk2_eDivIR ( A1 = b%v, A2 = a%v, res = c, commute =.true.  ) !r/i
            type is (rk2_t)
                call bk2_eDivRR ( A1 = a%v, A2 = b%v, res = c, commute =.false. ) !r/r
            type is (ck2_t)
                call bk2_eDivRC ( A1 = a%v, A2 = b%v, res = c, commute =.false. ) !r/c
            class default
               err = 1               
         end select

      type is (ck2_t)
         select type (b)
            type is (ik2_t)
                call bk2_eDivIC ( A1 = b%v, A2 = a%v, res = c, commute =.true. ) ! c/i
            type is (rk2_t)
                call bk2_eDivRC ( A1 = b%v, A2 = a%v, res = c, commute =.true. ) ! c/r
            type is (ck2_t)
                call bk2_eDivCC ( A1 = a%v, A2 = b%v, res = c, commute =.false. ) !c/c
            class default
               err = 1               
         end select

      class default
            err = 1               
      
   end select    
   
   error_TraceNreturn(opflag, HERE)
   
   if ( err == 1 ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << ./ >>')
      return
   end if
   
         
   END SUBROUTINE bk2_eDiv
   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Operators (relational) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_GT (bk2 > bk2)
! . bk2_GE (bk2 >= bk2)
! . bk2_LT (bk2 < bk2)
! . bk2_LE (bk2 <= bk2)
! . bk2_EQ (bk2 == bk2)
! . bk2_NE (bk2 /= bk2)
! . bk2_eqsk2 (sk2 == sk2)
! . bk2_neqsk2 (sk2 /= sk2)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_GT ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a > b
!  
!  Modified 12/19: avoid expressions using the constructor like c = lk2_t(a%v > b%v) 
!                  which can produce a stack overflow (e.g. with ifort)
!----------------------------------------------------------------------------R.H. 04/18, 12/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_GT'                                               
   integer  (Ikind)              :: nama, nbmb, n, m, i, j
   character(len=:), allocatable :: stra, strb
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return ! call opflag%set ()
!
!- Quick return if incompatible shapes or types:
!
   if ( (a%typ /= STYP .or. b%typ /= STYP) .and. (a%typ > RTYP .or. b%typ > RTYP) ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for " > "')
      return
   end if

   nama = a%nrow * a%ncol ; nbmb = b%nrow * b%ncol 

   if ( nama /= 1 .and. nbmb /= 1 ) then
      if ( a%nrow /= b%nrow .or. a%ncol /= b%ncol ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes for " > "')
         return
      end if
   end if
!
!- (Re)allocate c if needed:
!   
   n = max(a%nrow,b%nrow) ; m = max(a%ncol,b%ncol)
   
   call bk2_reallocIfNeeded ( c, LTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
!
!- Compute c:
!   
   select type ( c )
      type is ( lk2_t ) 
         select type ( a )
         
            ! for numerics:
            type is ( ik2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_gt.inc"
                  type is ( rk2_t )
#include "include/bk2_gt.inc"
               end select ! type(b)
            type is ( rk2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_gt.inc"
                  type is ( rk2_t )
#include "include/bk2_gt.inc"
               end select ! type(b)
               
            ! for strings:
            type is ( sk2_t )
               select type ( b )
                  type is ( sk2_t )
                     if ( nama == 1 ) then
                        call a%v(1,1) % getStr ( stra )
                        do j = 1, m
                           do i = 1, n
                              call b%v(i,j) % getStr ( strb )
                              c%v(i,j) = lgt(stra, strb)
                           end do
                        end do      
                     else if ( nbmb == 1 ) then
                        call b%v(1,1) % getStr ( strb )
                        do j = 1, m
                           do i = 1, n
                              call a%v(i,j) % getStr ( stra )
                              c%v(i,j) = lgt(stra, strb)
                           end do
                        end do      
                     else
                        do j = 1, m
                           do i = 1, n
                              call a%v(i,j) % getStr ( stra )
                              call b%v(i,j) % getStr ( strb )
                              c%v(i,j) = lgt(stra, strb)
                           end do
                        end do      
                     end if       
               end select ! type(b)            
         end select ! type(a)
   end select ! type(c)
            
   END SUBROUTINE bk2_GT

   
!=============================================================================================
   SUBROUTINE bk2_GE ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a >= b
!
!  Modified 12/19: avoid expressions using the constructor like c = lk2_t(a%v >= b%v) 
!                  which can produce a stack overflow (e.g. with ifort)
!----------------------------------------------------------------------------R.H. 04/18, 12/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'bk2_GE'                                                      
   integer  (Ikind)              :: n, m, nama, nbmb, i, j
   character(len=:), allocatable :: stra, strb
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
!
!- Quick return if incompatible shapes or types:
!
   if ( (a%typ /= STYP .or. b%typ /= STYP) .and. (a%typ > RTYP .or. b%typ > RTYP) ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for " >= "')
      return
   end if

   nama = a%nrow * a%ncol ; nbmb = b%nrow * b%ncol 

   if ( nama /= 1 .and. nbmb /= 1 ) then
      if ( a%nrow /= b%nrow .or. a%ncol /= b%ncol ) then
         call opflag%Set(UERROR, HERE, 'Incompatible shapes for " >= "')
         return
      end if
   end if
!
!- (Re)allocate c if needed:
!   
   n = max(a%nrow,b%nrow) ; m = max(a%ncol,b%ncol)
   
   call bk2_reallocIfNeeded ( c, LTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
!
!- Compute c:
!   
   select type ( c )
      type is ( lk2_t ) 
         select type ( a )
         
            ! for numerics:
            type is ( ik2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_ge.inc"
                  type is ( rk2_t )
#include "include/bk2_ge.inc"
               end select ! type(b)
            type is ( rk2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_ge.inc"
                  type is ( rk2_t )
#include "include/bk2_ge.inc"
               end select ! type(b)
               
            ! for strings:
            type is ( sk2_t )
               select type ( b )
                  type is ( sk2_t )
                     if ( nama == 1 ) then
                        call a%v(1,1) % getStr ( stra )
                        do j = 1, m
                           do i = 1, n
                              call b%v(i,j) % getStr ( strb )
                              c%v(i,j) = lge(stra, strb)
                           end do
                        end do      
                     else if ( nbmb == 1 ) then
                        call b%v(1,1) % getStr ( strb )
                        do j = 1, m
                           do i = 1, n
                              call a%v(i,j) % getStr ( stra )
                              c%v(i,j) = lge(stra, strb)
                           end do
                        end do      
                     else
                        do j = 1, m
                           do i = 1, n
                              call a%v(i,j) % getStr ( stra )
                              call b%v(i,j) % getStr ( strb )
                              c%v(i,j) = lge(stra, strb)
                           end do
                        end do      
                     end if       
               end select ! type(b)            
         end select ! type(a)
   end select ! type(c)   
         
   END SUBROUTINE bk2_GE
   

!=============================================================================================
   SUBROUTINE bk2_LT ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a < b  (<=> c = b > a)
!-----------------------------------------------------------------------------------R.H. 04/18

   call bk2_GT ( b, a, c )

   error_TraceNreturn(opflag, 'bk2_LT')
         
   END SUBROUTINE bk2_LT


!=============================================================================================
   SUBROUTINE bk2_LE ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a < b  (<=> c = b > a)
!-----------------------------------------------------------------------------------R.H. 04/18

   call bk2_GE ( b, a, c )

   error_TraceNreturn(opflag, 'bk2_LE')
         
   END SUBROUTINE bk2_LE


!=============================================================================================
   SUBROUTINE bk2_EQ ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == b
!
!  Note: if shapes or types of a and b do not conform, set c = .false.
!
!  Modified 12/19: avoid expressions using the constructor like c = lk2_t(a%v == b%v) 
!                  which can produce a stack overflow (e.g. with ifort)
!----------------------------------------------------------------------------R.H. 04/18, 12/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_EQ'     
   integer  (Ikind)            :: na, ma, nb, mb, n, m, nama, nbmb, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   na = a%nrow ; ma = a%ncol ; nb = b%nrow ; mb = b%ncol
  
   nama = na*ma ; nbmb = nb*mb   
!
!- Quick return with c%v the 1x1 .false. if incompatible shapes or incompatibles types:
!   
   if ( nama /= 1 .and. nbmb /= 1 ) then
      if ( na /= nb .or. ma /= mb ) then
         call bk2_assign ( c, .false. )
         return 
      end if   
   end if

   err = 1
   if ( ITYP <= a%typ .and. a%typ <= CTYP .and. ITYP <= b%typ .and. b%typ <= CTYP ) err = 0
   if ( a%typ == LTYP .and. b%typ == LTYP ) err = 0    
   if ( a%typ == STYP .and. b%typ == STYP ) err = 0  
   if ( err == 1 ) then
      call bk2_assign ( c, .false. )
      return 
   end if   
!
!- otherwise, c%v is an n x m boolean array:     
!   
   n = max(na,nb) ; m = max(ma,mb)
   call bk2_reallocIfNeeded ( c, LTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type ( c )
      type is ( lk2_t )
         select type ( a )
   
            type is ( ik2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_eq.inc"
                  type is ( rk2_t )
#include "include/bk2_eq.inc"
                  type is ( ck2_t )
#include "include/bk2_eq.inc"
               end select
               
            type is ( rk2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_eq.inc"
                  type is ( rk2_t )
#include "include/bk2_eq.inc"
                  type is ( ck2_t )
#include "include/bk2_eq.inc"
               end select

            type is ( ck2_t )
               select type ( b )         
                  type is ( ik2_t )
#include "include/bk2_eq.inc"
                  type is ( rk2_t )
#include "include/bk2_eq.inc"
                  type is ( ck2_t )
#include "include/bk2_eq.inc"
               end select

            type is ( lk2_t )
               select type ( b )
                  type is ( lk2_t )
                     if ( nama == 1 ) then
                        c%v(:,:) = (a%v(1,1) .eqv. b%v(:,:))
                     else if ( nbmb == 1 ) then
                        c%v(:,:) = (a%v(:,:) .eqv. b%v(1,1))
                     else
                        c%v(:,:) = (a%v(:,:) .eqv. b%v(:,:))
                     end if    
               end select

            type is ( sk2_t )
               select type ( b )
                  type is ( sk2_t )
                     call bk2_eqsk2 ( a, b, c%v )
               end select
               
         end select  ! (type a) 
   end select ! (type c)
      
   END SUBROUTINE bk2_EQ


!=============================================================================================
   SUBROUTINE bk2_NE ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!
!  Note: if shapes or types of a and b do not conform, set c = .true.
!
!  Modified 12/19: avoid expressions using the constructor like c = lk2_t(a%v /= b%v) 
!                  which can produce a stack overflow (e.g. with ifort)
!----------------------------------------------------------------------------R.H. 04/18, 12/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_NE'     
   integer  (Ikind)            :: na, ma, nb, mb, n, m, nama, nbmb, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   na = a%nrow ; ma = a%ncol ; nb = b%nrow ; mb = b%ncol
  
   nama = na*ma ; nbmb = nb*mb
!
!- Quick return with c%v the 1x1 .true. if incompatible shapes or incompatibles types:
!   
   if ( nama /= 1 .and. nbmb /= 1 ) then
      if ( na /= nb .or. ma /= mb ) then
         call bk2_assign ( c, .true. ) 
         return 
      end if   
   end if

   err = 1
   if ( ITYP <= a%typ .and. a%typ <= CTYP .and. ITYP <= b%typ .and. b%typ <= CTYP ) err = 0
   if ( a%typ == LTYP .and. b%typ == LTYP ) err = 0    
   if ( a%typ == STYP .and. b%typ == STYP ) err = 0  
   if ( err == 1 ) then
      call bk2_assign ( c, .true. ) 
      return 
   end if   
!
!- otherwise, c%v is an n x m boolean array:     
!   
   n = max(na,nb) ; m = max(ma,mb)
   call bk2_reallocIfNeeded ( c, LTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type ( c )
      type is ( lk2_t )
      
         select type ( a )
            type is ( ik2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_ne.inc"
                  type is ( rk2_t )
#include "include/bk2_ne.inc"
                  type is ( ck2_t )
#include "include/bk2_ne.inc"
               end select

            type is ( rk2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_ne.inc"
                  type is ( rk2_t )
#include "include/bk2_ne.inc"
                  type is ( ck2_t )
#include "include/bk2_ne.inc"
               end select

            type is ( ck2_t )
               select type ( b )
                  type is ( ik2_t )
#include "include/bk2_ne.inc"
                  type is ( rk2_t )
#include "include/bk2_ne.inc"
                  type is ( ck2_t )
#include "include/bk2_ne.inc"
               end select

            type is ( lk2_t )
               select type ( b )
                  type is (lk2_t)
                     if ( nama == 1 ) then
                        c%v(:,:) = ( a%v(1,1) .neqv. b%v )
                     else if ( nbmb == 1 ) then
                        c%v(:,:) = ( a%v .neqv. b%v(1,1) )
                     else
                        c%v(:,:) = ( a%v .neqv. b%v )
                     end if 
               end select

            type is (sk2_t)
               select type (b)
                  type is (sk2_t)
                     call bk2_neqsk2 ( a, b,  c%v )
               end select
         
         end select ! (type a)
   end select ! (type c)  
      
   END SUBROUTINE bk2_NE
   

!=============================================================================================
   SUBROUTINE bk2_eqsk2 ( a, b, res )
!=============================================================================================
   type   (sk2_t), intent(in    ) :: a, b
   logical       , intent(in out) :: res(:,:)
!---------------------------------------------------------------------------------------------
!  Boolean condition res%v = (a == b) for two sk2 var
!
!  . a%v and b%v are of the same shape or at least one of them is 1x1
!  . res must be a logical array with a shape of [max(na,nb), max(ma,mb)]
!  
!  Warning: THE CALLER MUST CHECK THE SHAPES
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------           
   integer  (Ikind)              :: na, ma, nb, mb, i, j
   character(len=:), allocatable :: sa, sb
!---------------------------------------------------------------------------------------------
 
   if ( opflag%code > IZERO ) return ! call opflag%set ()
  
   na = a%nrow ; ma = a%ncol ; nb = b%nrow ; mb = b%ncol
      
   if ( na == 1 .and. ma == 1 ) then
      call a%v(1,1)%getStr ( sa )
      do j = 1, mb
         do i = 1, nb
            call b%v(i,j)%getStr ( sb  )
            res(i,j) = ( sa == sb )
         end do
      end do
   else if ( nb == 1 .and. mb == 1 ) then
      call b%v(1,1)%getStr ( sb )
      do j = 1, ma
         do i = 1, na
            call a%v(i,j)%getStr ( sa )
            res(i,j) = ( sa == sb )
         end do
      end do
   else if ( nb == na .and. mb == ma ) then
      do j = 1, ma
         do i = 1, na
            call a%v(i,j)%getStr ( sa )
            call b%v(i,j)%getStr ( sb )
            res(i,j) = ( sa == sb )
         end do
      end do
   end if
   
   END SUBROUTINE bk2_eqsk2
      

!=============================================================================================
   SUBROUTINE bk2_neqsk2 ( a, b, res )
!=============================================================================================
   type   (sk2_t), intent(in    ) :: a, b
   logical       , intent(in out) :: res(:,:)
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b for two sk2 var.
!
!  . a%v and b%v are of the same shape or at least one of them is 1x1
!  . res must be a logical array with a shape of [max(na,nb), max(ma,mb)]
!  
!  Warning: THE CALLER MUST CHECK THE SHAPES
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------           
   integer  (Ikind)              :: na, ma, nb, mb, i, j
   character(len=:), allocatable :: sa, sb
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   na = a%nrow ; ma = a%ncol ; nb = b%nrow ; mb = b%ncol
   
   if ( na == 1 .and. ma == 1 ) then
      call a%v(1,1)%getstr ( sa )
      do j = 1, mb
         do i = 1, nb
            call b%v(i,j)%getstr ( sb )
            res(i,j) = ( sa /= sb )
         end do
      end do
   else if ( nb == 1 .and. mb == 1 ) then
      call b%v(1,1)%getstr ( sb )
      do j = 1, ma
         do i = 1, na
            call a%v(i,j)%getstr ( sa )
            res(i,j) = ( sa /= sb )
         end do
      end do
   else if ( nb == na .and. mb == ma ) then
      res(:,:) = .false.      
      do j = 1, ma
         do i = 1, na
            call a%v(i,j)%getstr ( sa )
            call b%v(i,j)%getstr ( sb )
            res(i,j) = ( sa /= sb )
         end do
      end do
   end if
   
   END SUBROUTINE bk2_neqsk2   
         
   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Operators (boolean) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_AND (bk2 & bk2)
! . bk2_OR  (bk2 | bk2)
! . bk2_NOT (~bk2)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_AND ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Boolean operation c = a & b
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_AND'         
   integer  (Ikind)              :: nama, nbmb, n, m
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
     
   nama = a%nrow * a%ncol ; nbmb = b%nrow * b%ncol
   
   if ( nama /= 1 .and. nbmb /= 1 ) then
      if ( a%nrow /= b%nrow .or. a%ncol /= b%ncol ) then
         call opflag%Set(UERROR, HERE,'Incompatible shapes for << & >> (and)')
         return
      end if
   end if
   
   if ( a%typ /= LTYP .or. b%typ /= LTYP ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << & >> (and)')
      return
   end if      
   
   n = max(a%nrow,b%nrow) ; m = max(a%ncol,b%ncol)
   call bk2_reallocIfNeeded ( c, LTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
         
   select type ( c )
      type is ( lk2_t )
         select type ( a )
            type is ( lk2_t )
               select type ( b )
                  type is ( lk2_t )
                     if ( nama == 1 ) then
                        c%v(:,:) = ( a%v(1,1) .and. b%v )
                     else if ( nbmb == 1 ) then
                        c%v(:,:) = ( a%v .and. b%v(1,1) )
                     else
                        c%v(:,:) = ( a%v .and. b%v )
                     end if   
               end select    
         end select    
   end select    
         
   END SUBROUTINE bk2_AND


!=============================================================================================
   SUBROUTINE bk2_OR ( a, b, c )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a, b
   class(bk2_t), allocatable, intent(in out) :: c
!---------------------------------------------------------------------------------------------
!  Boolean operation c = a | b
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_OR'                   
   integer  (Ikind)              :: nama, nbmb, n, m
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   nama = a%nrow * a%ncol ; nbmb = b%nrow * b%ncol
   
   if ( nama /= 1 .and. nbmb /= 1 ) then
      if ( a%nrow /= b%nrow .or. a%ncol /= b%ncol ) then
         call opflag%Set(UERROR, HERE,'Incompatible shapes for << | >> (or)')
         return
      end if
   end if
   
   if ( a%typ /= LTYP .or. b%typ /= LTYP ) then
      call opflag%Set(UERROR, HERE, 'Incompatible types for << | >> (or)')
      return
   end if      
   
   n = max(a%nrow,b%nrow) ; m = max(a%ncol,b%ncol)
   call bk2_reallocIfNeeded ( c, LTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
         
   select type ( c )
      type is ( lk2_t )
         select type ( a )
            type is ( lk2_t )
               select type ( b )
                  type is ( lk2_t )
                     if ( nama == 1 ) then
                        c%v(:,:) = ( a%v(1,1) .or. b%v )
                     else if ( nbmb == 1 ) then
                        c%v(:,:) = ( a%v .or. b%v(1,1) )
                     else
                        c%v(:,:) = ( a%v .or. b%v )
                     end if   
               end select    
         end select    
   end select    
      
   END SUBROUTINE bk2_OR
   

!=============================================================================================
   SUBROUTINE bk2_NOT ( a, b )
!=============================================================================================
   class(bk2_t),              intent(in    ) :: a
   class(bk2_t), allocatable, intent(in out) :: b
!---------------------------------------------------------------------------------------------
!  Logical negation b = .not. a
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_NOT'                   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   if ( a%typ /= LTYP ) then
      call opflag%Set ( UERROR, HERE, '<< b = ~a >> with << a >> non-boolean' )
      return
   end if
         
   call bk2_reallocIfNeeded ( b, LTYP, a%nrow, a%ncol ) ; error_TraceNreturn(opflag, HERE)
   
   select type ( b ) 
      type is ( lk2_t )
         select type ( a )
            type is (lk2_t )
               b%v(:,:) = .not. a%v        
         end select
   end select
      
   END SUBROUTINE bk2_NOT


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Prints ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_PrintIk2
! . bk2_PrintRk2
! . bk2_PrintCk2
! . bk2_PrintLk2
! . bk2_PrintSk2
! . bk2_PrintBk2
! . bk2_AfficheIk2
! . bk2_AfficheRk2
! . bk2_AfficheCk2
! . bk2_AfficheLk2
! . bk2_AfficheSk2
! . bk2_AfficheBk2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================   
   SUBROUTINE bk2_PrintIk2 ( self, msg, dispstyle, unit, digmax )
!=============================================================================================   
   use dispmodule ; use disp_i8mod
   class    (ik2_t),           intent(in) :: self
   character(len=*), optional, intent(in) :: msg, dispstyle
   integer  (Ikind), optional, intent(in) :: unit, digmax
!---------------------------------------------------------------------------------------------
!  Prints an ik2 array (call to disp from dispmodule)
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=:), allocatable :: str, styl
   integer                       :: uo
!---------------------------------------------------------------------------------------------    

   if ( present(unit) ) then
      uo = int(unit)
   else
      uo = int(STDOUT)
   end if      

   if ( present(msg) ) then
      str = msg
   else
      str = ''
   end if
                     
   if ( self%typ == EMPTY .or. self%nrow * self%ncol == 0 .or. .not. allocated(self%v) ) then
      write(uo,'(a)')str // ' (empty)'
      return
   end if
      
   if ( present(dispstyle) ) then
      styl = dispstyle
   else
      styl = 'left'
   end if      

   call disp (str, self%v, unit = uo, zeroas = '0', style = styl)
   
   END SUBROUTINE bk2_PrintIk2
   

!=============================================================================================   
   SUBROUTINE bk2_PrintRk2 ( self, msg, dispstyle, unit, digmax )
!=============================================================================================   
   use dispmodule ; use disp_i8mod
   class    (rk2_t),           intent(in) :: self
   character(len=*), optional, intent(in) :: msg, dispstyle
   integer  (Ikind), optional, intent(in) :: unit, digmax
!---------------------------------------------------------------------------------------------
!  Prints an rk2 array (call to disp from dispmodule)
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=:), allocatable :: str, styl
   integer                       :: uo, digm
!---------------------------------------------------------------------------------------------    

   if ( present(unit) ) then
      uo = int(unit)
   else
      uo = int(STDOUT)
   end if      

   if ( present(msg) ) then
      str = msg
   else
      str = ''
   end if
            
   if ( self%typ == EMPTY .or. self%nrow * self%ncol == 0 .or. .not. allocated(self%v) ) then
      write(uo,'(a)')str // ' (empty)'
      return
   end if
      
   if ( present(dispstyle) ) then
      styl = dispstyle
   else
      styl = 'left'
   end if       
   
   if ( present(digmax) ) then
      digm = int(digmax)
   else
      digm = 6
   end if         
   
   call disp (str, self%v, unit = uo, zeroas = '0', style = styl, digmax = digm)
   
   END SUBROUTINE bk2_PrintRk2
   

!=============================================================================================   
   SUBROUTINE bk2_PrintCk2 ( self, msg, dispstyle, unit, digmax )
!=============================================================================================   
   use dispmodule ; use disp_i8mod
   class    (ck2_t),           intent(in) :: self
   character(len=*), optional, intent(in) :: msg, dispstyle
   integer  (Ikind), optional, intent(in) :: unit, digmax
!---------------------------------------------------------------------------------------------
!  Prints a ck2 array (call to disp from dispmodule)
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=:), allocatable :: str, styl
   integer                       :: uo, digm
!---------------------------------------------------------------------------------------------    
         
   if ( present(unit) ) then
      uo = int(unit)
   else
      uo = int(STDOUT)
   end if      

   if ( present(msg) ) then
      str = msg
   else
      str = ''
   end if
         
   if ( self%typ == EMPTY .or. self%nrow * self%ncol == 0 .or. .not. allocated(self%v) ) then
      write(uo,'(a)')str // ' (empty)'
      return
   end if
   
   if ( present(dispstyle) ) then
      styl = dispstyle
   else
      styl = 'left'
   end if      

   if ( present(digmax) ) then
      digm = int(digmax)
   else
      digm = 6
   end if      
         
   call disp (str, self%v, unit = uo, style = styl, digmax = digm)
   
   END SUBROUTINE bk2_PrintCk2
   

!=============================================================================================   
   SUBROUTINE bk2_PrintLk2 ( self, msg, dispstyle, unit, digmax )
!=============================================================================================   
   use dispmodule ; use disp_i8mod 
   class    (lk2_t),           intent(in) :: self
   character(len=*), optional, intent(in) :: msg, dispstyle
   integer  (Ikind), optional, intent(in) :: unit, digmax
!---------------------------------------------------------------------------------------------
!  Prints an lk2 array (call to disp from dispmodule)
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=:), allocatable :: str, styl
   integer                       :: uo
!---------------------------------------------------------------------------------------------    
            
   if ( present(unit) ) then
      uo = int(unit)
   else
      uo = int(STDOUT)
   end if      

   if ( present(msg) ) then
      str = msg
   else
      str = ''
   end if
         
   if ( self%typ == EMPTY .or. self%nrow * self%ncol == 0 .or. .not. allocated(self%v) ) then
      write(uo,'(a)')str // ' (empty)'
      return
   end if
   
   if ( present(dispstyle) ) then
      styl = dispstyle
   else
      styl = 'left'
   end if      
   
   call disp (str, self%v, unit = uo, style = styl)
   
   END SUBROUTINE bk2_PrintLk2
   

!=============================================================================================   
   SUBROUTINE bk2_PrintSk2 ( self, msg, dispstyle, unit, digmax )
!=============================================================================================   
   use dispmodule ; use disp_i8mod
   class    (sk2_t),           intent(in) :: self
   character(len=*), optional, intent(in) :: msg, dispstyle
   integer  (Ikind), optional, intent(in) :: unit, digmax
!---------------------------------------------------------------------------------------------
!  Prints an sk2 array (call to disp from dispmodule)
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=:), allocatable :: str, styl !, res(:,:)
   integer                       :: uo, lenmax, m 
   character(len=9)              :: fmt
!---------------------------------------------------------------------------------------------    
            
   if ( present(unit) ) then
      uo = int(unit)
   else
      uo = int(STDOUT)
   end if      

   if ( present(msg) ) then
      str = msg
   else
      str = ''
   end if
         
   if ( self%typ == EMPTY .or. self%nrow * self%ncol == 0 .or. .not. allocated(self%v) ) then
      write(uo,'(a)')str // ' (empty)'
      return
   end if
   
   if ( present(dispstyle) ) then
      styl = dispstyle
   else
      styl = 'left'
   end if      

   call str_print ( s = self%v, unit = uo, lhsMsg = str )
   
   
!    call self%GetMatChar(res)
! 
!    m = self%ncol
!    fmt = 'a0'                                
!    if (all(len_trim(res(1,:)) == 0) .or. &   ! dispmodule crashes if the elements
!        all(len_trim(res(:,m)) == 0)   ) then ! of the first row or the last column
!       lenmax = max(1,maxval(len_trim(res)))  ! are all of 0 lenght. Then I call it 
!       write(fmt,'(a,i0)')'a',lenmax          ! with a format 'Aw' with w based on the
!    end if                                    ! max. element lengths.
!    
!    call disp (str, res, unit = uo, style = styl, fmt = fmt)
   
   END SUBROUTINE bk2_PrintSk2
   

!=============================================================================================   
   SUBROUTINE bk2_PrintBk2 ( self, msg, dispstyle, unit, digmax )
!=============================================================================================   
   class    (bk2_t),           intent(in) :: self
   character(len=*), optional, intent(in) :: msg, dispstyle
   integer  (Ikind), optional, intent(in) :: unit, digmax
!---------------------------------------------------------------------------------------------
!  Prints a bk2 array
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=:), allocatable :: str
   integer                       :: uo
!---------------------------------------------------------------------------------------------    

   if ( present(msg) ) then
      str = msg
   else 
      str = ''
   end if
      
   if ( present(unit) ) then
      uo = int(unit)
   else
      uo = int(STDOUT)
   end if      
   
   if ( self%typ == EMPTY .or. self%nrow * self%ncol == 0 ) then
      write(uo,'(a)')str // ' (empty)'
   else   
      write(uo,'(a,i0,a,i0,a)')str // '(nrow = ',self%nrow, ', ncol = ',self%ncol,')'
   end if
                           
   END SUBROUTINE bk2_PrintBk2
   

!=============================================================================================   
   SUBROUTINE bk2_AfficheIk2 ( self )
!=============================================================================================   
   class(ik2_t), intent(in) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------              
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------
   
   if ( self%nrow * self%ncol == 0 ) then
      print*,' (empty matrix)'
   else   
      do i = 1, self%nrow
         print*,(self%v(i,j),j=1,self%ncol)
      end do
   end if
      
   END SUBROUTINE bk2_AfficheIk2


!=============================================================================================   
   SUBROUTINE bk2_AfficheRk2 ( self )
!=============================================================================================   
   class(rk2_t), intent(in) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------              
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------

   if ( self%nrow * self%ncol == 0 ) then
      print*,' (empty matrix)'
   else     
      do i = 1, self%nrow
         print '(*(g0,1x))',(self%v(i,j),j=1,self%ncol)
      end do
   end if
      
   END SUBROUTINE bk2_AfficheRk2


!=============================================================================================   
   SUBROUTINE bk2_AfficheCk2 ( self )
!=============================================================================================   
   class(ck2_t), intent(in) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------              
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------

   if ( self%nrow * self%ncol == 0 ) then
      print*,' (empty matrix)'
   else     
      do i = 1, self%nrow
         print*,(self%v(i,j),j=1,self%ncol)
      end do
   end if
      
   END SUBROUTINE bk2_AfficheCk2


!=============================================================================================   
   SUBROUTINE bk2_AfficheLk2 ( self )
!=============================================================================================   
   class(lk2_t), intent(in) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------              
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------

   if ( self%nrow * self%ncol == 0 ) then
      print*,' (empty matrix)'
   else     
      do i = 1, self%nrow
         print*,(self%v(i,j),j=1,self%ncol)
      end do
   end if
      
   END SUBROUTINE bk2_AfficheLk2


!=============================================================================================   
   SUBROUTINE bk2_AfficheSk2 ( self )
!=============================================================================================   
   class(sk2_t), intent(in) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 03/18

!- local variables ---------------------------------------------------------------------------              
   integer  (Ikind) :: i, j, k, maxlen, nbk
   type     (str_t) :: tmp(self%nrow,self%ncol)
   character(len=:), allocatable :: line 
!---------------------------------------------------------------------------------------------

   if ( self%nrow * self%ncol == 0 ) then
      print*,' (empty matrix)'
   else        
      maxlen = 0
      do j = 1, self%ncol
         do i = 1, self%nrow
            if ( allocated(self%v(i,j)%str) ) then
               tmp(i,j)%str = (self%v(i,j)%str)
            else
               tmp(i,j)%str = ''
            end if
            maxlen = max(maxlen,len(tmp(i,j)%str,kind=Ikind))
         end do
      end do
   
      do i = 1, self%nrow
         line = '!'
         do j = 1, self%ncol
            line = line // (tmp(i,j)%str) // ' '
            nbk = maxlen - len(tmp(i,j)%str)
            do k = 1, nbk
               line = line // ' '
            end do
         end do
         line = line // '!'
         print*,line
      end do      
   end if
   
   END SUBROUTINE bk2_AfficheSk2


!=============================================================================================   
   SUBROUTINE bk2_AfficheBk2 ( self )
!=============================================================================================   
   class(bk2_t), intent(in) :: self
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 03/18
      
   if ( self%typ == EMPTY ) then
      print*,'(empty matrix)'
   else   
      print*,'typ, nrow, ncol:',self%typ, self%nrow, self%ncol
   end if
         
   END SUBROUTINE bk2_AfficheBk2


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Addition ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_AddII  (bk2 = Imat + Imat)
! . bk2_AddIR  (bk2 = Imat + Rmat)
! . bk2_AddRI  (bk2 = Rmat + Imat)
! . bk2_AddIC  (bk2 = Imat + Cmat)
! . bk2_AddCI  (bk2 = Cmat + Imat)
! . bk2_AddRR  (bk2 = Rmat + Rmat)
! . bk2_AddRC  (bk2 = Rmat + Cmat)
! . bk2_AddCR  (bk2 = Cmat + Rmat)
! . bk2_AddCC  (bk2 = Cmat + Cmat)
! . bk2_AddSS  (bk2 = Smat + Smat)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 


!=============================================================================================
   SUBROUTINE bk2_AddII ( A1, s, A2, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s
   integer(Ikind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A2, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddII'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, ITYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (ik2_t)
#include "include/bk2_addNsub.inc"
   end select
   
   END SUBROUTINE bk2_AddII


!=============================================================================================
   SUBROUTINE bk2_AddIR ( A1, s, A2, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s   
   real   (Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A1, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddIR'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, RTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (rk2_t)
#include "include/bk2_addNsub.inc"
   end select

   END SUBROUTINE bk2_AddIR


!=============================================================================================
   SUBROUTINE bk2_AddRI ( A1, s, A2, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s   
   integer(Ikind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A1, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddRI'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, RTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (rk2_t)
#include "include/bk2_addNsub.inc"
   end select

   END SUBROUTINE bk2_AddRI


!=============================================================================================
   SUBROUTINE bk2_AddIC ( A1, s, A2, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s   
   complex(Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A1, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddIC'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (ck2_t)
#include "include/bk2_addNsub.inc"
   end select

   END SUBROUTINE bk2_AddIC


!=============================================================================================
   SUBROUTINE bk2_AddCI ( A1, s, A2, res )
!=============================================================================================
   complex(Rkind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s   
   integer(Ikind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A1, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddCI'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (ck2_t)
#include "include/bk2_addNsub.inc"
   end select

   END SUBROUTINE bk2_AddCI


!=============================================================================================
   SUBROUTINE bk2_AddRR ( A1, s, A2, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s   
   real   (Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A1, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddRR'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, RTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (rk2_t)
#include "include/bk2_addNsub.inc"
   end select
    
   END SUBROUTINE bk2_AddRR
   
   
!=============================================================================================
   SUBROUTINE bk2_AddRC ( A1, s, A2, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s   
   complex(Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A1, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddRC'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (ck2_t)
#include "include/bk2_addNsub.inc"
   end select

   END SUBROUTINE bk2_AddRC


!=============================================================================================
   SUBROUTINE bk2_AddCR ( A1, s, A2, res )
!=============================================================================================
   complex(Rkind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s   
   real   (Rkind),              intent(in    ) :: A2(:,:)   
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A1, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddCR'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (ck2_t)
#include "include/bk2_addNsub.inc"
   end select

   END SUBROUTINE bk2_AddCR


!=============================================================================================
   SUBROUTINE bk2_AddCC ( A1, s, A2, res )
!=============================================================================================
   complex(Rkind),              intent(in    ) :: A1(:,:)
   logical       ,              intent(in    ) :: s   
   complex(Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  If s = true: computes A1 + A2, else: computes A1 - A1, and puts the result into res%m
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddCC'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)

   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ; error_TraceNreturn(opflag, HERE)
   
   select type(res)
      type is (ck2_t)
#include "include/bk2_addNsub.inc"
   end select

   END SUBROUTINE bk2_AddCC


!=============================================================================================
   SUBROUTINE bk2_AddSS ( A1, A2, res )
!=============================================================================================
   type (str_t),              intent(in    ) :: A1(:,:)
   type (str_t),              intent(in    ) :: A2(:,:)
   class(bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Adds the two string arrays A1 and A2 and puts the result in res%m
!  CAUTION: the caller must check the shape compatibility
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_AddSS'      
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2 , n, m
   character(len=:), allocatable :: s
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; m1 = size(A1,2) ; n1m1 = n1*m1
   n2 = size(A2,1) ; m2 = size(A2,2) ; n2m2 = n2*m2
   
   n = max(n1,n2) ; m = max(m1,m2)
   
   call bk2_reallocIfNeeded ( res, STYP, n, m ) ; error_TraceNreturn(opflag, HERE)
    
   select type ( res )
      type is ( sk2_t )
         if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
            call str_concate ( a = A1, b = A2, res = res%v ) ! RH 09/22               
         else if ( n1m1 == 1 ) then
            if ( allocated(A1(1,1)%str) ) then
               s = (A1(1,1)%str)
            else
               s = ''
            end if
            call str_concate ( a = s, b = A2, res = res%v ) ! RH 09/22
         else if ( n2m2 == 1 ) then
            if ( allocated(A2(1,1)%str) ) then
               s = (A2(1,1)%str)
            else
               s = ''
            end if
            call str_concate ( a = A1, b = s, res = res%v ) ! RH 09/22
         end if   
   end select                     

   END SUBROUTINE bk2_AddSS
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Multiplication (element-wise) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_eMultII  (bk2 = Imat .* Imat)
! . bk2_eMultIR  (bk2 = Imat .* Rmat)
! . bk2_eMultIC  (bk2 = Imat .* Cmat)
! . bk2_eMultRR  (bk2 = Rmat .* Rmat)
! . bk2_eMultRC  (bk2 = Rmat .* Cmat)
! . bk2_eMultCC  (bk2 = Cmat .* Cmat)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_eMultII ( A1, A2, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   integer(Ikind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Mutliplies element-wise the two integer arrays A1 and A2 and puts the result in res%v
!  Caution: THE CALLER MUST CHECK THE SHAPES
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'bk2_eMultII'      
   real     (Rkind), allocatable :: R2(:,:)
   integer  (Ikind), allocatable :: Ires(:,:)
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()
!
!- We first do the product in reals:
!
   R2 = A2
   call bk2_eMultIR ( A1, R2, res )
!
!- See if the result will overflow in integers:
!      
   select type ( res )
      type is ( rk2_t )
         if ( any( abs(res%v) > real(huge(Ikind)-1,Rkind) ) ) then
            return
         else
            Ires = int(res%v,kind=Ikind)
         end if
   end select
!
!- Otherwise convert res to integers:
!      
   deallocate(res) ; allocate(ik2_t :: res)
   select type ( res )
      type is ( ik2_t )
         res%typ = ITYP ; res%nrow = size(Ires,1) ; res%ncol = size(Ires,2)
         call move_alloc ( from = Ires, to = res%v )
   end select
      
   END SUBROUTINE bk2_eMultII


!=============================================================================================
   SUBROUTINE bk2_eMultIR ( A1, A2, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   real   (Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Mutliplies element-wise the integer array A1 and the real array A2 and puts the result in 
!  res%v
!  Caution: THE CALLER MUST CHECK THE SHAPES
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_eMultIR'      
   integer  (Ikind)            :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
         
   call bk2_reallocIfNeeded ( res, RTYP, n, m ) ; error_TraceNreturn(opflag, HERE)  

   select type ( res )
      type is ( rk2_t )
#include "include/bk2_emult.inc"
   end select

   END SUBROUTINE bk2_eMultIR


!=============================================================================================
   SUBROUTINE bk2_eMultIC ( A1, A2, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Mutliplies element-wise the integer array A1 and the real array A2 and puts the result in 
!  res%v
!  Caution: THE CALLER MUST CHECK THE SHAPES
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_eMultIC'      
   integer  (Ikind)            :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
         
   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ; error_TraceNreturn(opflag, HERE)  

   select type ( res )
      type is ( ck2_t )
#include "include/bk2_emult.inc"
   end select

   END SUBROUTINE bk2_eMultIC
   

!=============================================================================================
   SUBROUTINE bk2_eMultRR ( A1, A2, res )
!=============================================================================================
   real (Rkind),              intent(in    ) :: A1(:,:)
   real (Rkind),              intent(in    ) :: A2(:,:)
   class(bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Mutliplies element-wise the two real arrays A1 and A2 and puts the result in res%v
!  Caution: THE CALLER MUST CHECK THE SHAPES
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_eMultRR'      
   integer  (Ikind)            :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
         
   call bk2_reallocIfNeeded ( res, RTYP, n, m ) ; error_TraceNreturn(opflag, HERE)  

   select type ( res )
      type is ( rk2_t )
#include "include/bk2_emult.inc"
   end select
   
   END SUBROUTINE bk2_eMultRR
   
   
!=============================================================================================
   SUBROUTINE bk2_eMultRC ( A1, A2, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Mutliplies element-wise the real array A1 and the complex array A2 and puts the result in 
!  res%m
!  Caution: THE CALLER MUST CHECK THE SHAPES
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_eMultRC' 
   integer  (Ikind)            :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
         
   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ; error_TraceNreturn(opflag, HERE)  

   select type ( res )
      type is ( ck2_t )
#include "include/bk2_emult.inc"
   end select

   END SUBROUTINE bk2_eMultRC


!=============================================================================================
   SUBROUTINE bk2_eMultCC ( A1, A2, res )
!=============================================================================================
   complex(Rkind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Mutliplies element-wise the two complexes array A1 and A2 and puts the result in res%m
!  Caution: THE CALLER MUST CHECK THE SHAPES
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_eMultCC' 
   integer  (Ikind)            :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
         
   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ; error_TraceNreturn(opflag, HERE)  

   select type ( res )
      type is ( ck2_t )
#include "include/bk2_emult.inc"
   end select
   
   END SUBROUTINE bk2_eMultCC
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Multiplication (matrix product) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_MultII  (bk2 = Imat1 * Imat2  or  Imat2 * Imat1)  (version using ?gemm of BLAS)
! . bk2_MultIR  (bk2 = Imat  * Rmat   or  Rmat  * Imat)   ("       "      "    "  "   )     
! . bk2_MultIC  (bk2 = Imat  * Cmat   or  Cmat  * Imat)   ("       "      "    "  "   )
! . bk2_MultRR  (bk2 = Rmat1 * Rmat2  or  Rmat2 * Rmat1)  ("       "      "    "  "   )
! . bk2_MultRC  (bk2 = Rmat  * Cmat   or  Cmat  * Rmat)   ("       "      "    "  "   )
! . bk2_MultCC  (bk2 = Cmat1 * Cmat2  or  Cmat2 * Cmat1)  ("       "      "    "  "   )
!
! . bk2_MultII0  (version using matmul)
! . bk2_MultIR0  ("       "     "     )
! . bk2_MultIC0  ("       "     "     )
! . bk2_MultRR0  ("       "     "     )
! . bk2_MultRC0  ("       "     "     )
! . bk2_MultCC0  ("       "     "     )
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_MultII ( I, J, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: I(:,:)
   integer(Ikind),              intent(in    ) :: J(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the two integers arrays I and J and puts the result in res%m
!  . If commute == .false.: I * J
!  . If commute == .true. : J * I
!
!  The multiplication is done in reals. If each element of the result is not greater than
!  huge(int), save the result into integer array
! 
!  Version using Blas routine ?gemm
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultII'            
!- For Blas ?gemm: ---------------------------------------------------------------------------
   integer                     :: err  
   real   (Rkind), allocatable :: A(:,:), B(:,:)
   integer(Ikind), allocatable :: Ires(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
!
!- Copy I and J into real matrices A and B:
!      
   allocate(A(size(I,1),size(I,2)), B(size(J,1),size(J,2)), stat = err) 
   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure for array "A" or "B"')
      return
   end if   
   A(:,:) = real(I(:,:),kind=Rkind) ; B(:,:) = real(J(:,:),kind=Rkind)
!
!- Compute A * B or B * A:
!   
   call bk2_MultRR ( A, B, commute, res )
!
!- See if the result will overflow in integers:
!      
   select type ( res )
      type is ( rk2_t )
         if ( any( abs(res%v) > real(huge(Ikind)-1,Rkind) ) ) then
            return
         else
            Ires = int(res%v(:,:),kind=Ikind)
         end if
   end select
!
!- Otherwise convert res to integers:
!      
   deallocate(res) ; allocate(ik2_t :: res)
   select type ( res )
      type is ( ik2_t )
         res%typ = ITYP ; res%nrow = size(Ires,1) ; res%ncol = size(Ires,2)
         call move_alloc ( from = Ires, to = res%v )
   end select
   
   END SUBROUTINE bk2_MultII


!=============================================================================================
   SUBROUTINE bk2_MultIR ( I, B, commute, res )
!=============================================================================================
   use LapackInterface_m, only: lapackinterface_gemm
   integer(Ikind),              intent(in    ) :: I(:,:)
   real   (Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the integer arrays I and the real array B and puts the result in res%m
!  . If commute == .false.: I * B
!  . If commute == .true. : B * I
!
!  Version using Blas routine ?gemm
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultIR'            
!- For Blas ?gemm: ---------------------------------------------------------------------------
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
   real   (Rkind)              :: zero, one       
   real   (Rkind), allocatable :: A(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = RZERO ; one = RONE ; typres = RTYP
!
!- Copy the array I into the real array A:
!
   allocate(A(size(I,1),size(I,2)), stat = err) 
   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure for array "A"')
      return
   end if   
   A(:,:) = real(I(:,:),kind=Rkind)
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( rk2_t )
#include "include/bk2_mult_by_blas.inc"
   end select
      
   END SUBROUTINE bk2_MultIR


!=============================================================================================
   SUBROUTINE bk2_MultIC ( I, B, commute, res )
!=============================================================================================
   use LapackInterface_m, only: lapackinterface_gemm
   integer(Ikind),              intent(in    ) :: I(:,:)
   complex(Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the integer array I and the complex array B and puts the result in res%m
!  . If commute == .false.: I * B
!  . If commute == .true. : B * I
!
!  Version using Blas routine ?gemm
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultIC'        
!- For Blas ?gemm: ---------------------------------------------------------------------------
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
   complex(Rkind)              :: zero, one       
   complex(Rkind), allocatable :: A(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = CZERO ; one = CONE ; typres = CTYP
!
!- Copy the array I into the complex array A:
!   
   allocate(A(size(I,1),size(I,2)), stat = err) 
   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure for array "A"')
      return
   end if   
   A(:,:) = cmplx(I(:,:),kind=Rkind)
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_mult_by_blas.inc"
   end select
       
   END SUBROUTINE bk2_MultIC


!=============================================================================================
   SUBROUTINE bk2_MultRR ( A, B, commute, res )
!=============================================================================================
   use LapackInterface_m, only: lapackinterface_gemm
   real   (Rkind),              intent(in    ) :: A(:,:)
   real   (Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the two reals arrays A and B and puts the result in res%m
!  . If commute == .false.: A * B
!  . If commute == .true. : B * A
!
!  Version using Blas routine ?gemm
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultRR'   
!- For Blas ?gemm: ---------------------------------------------------------------------------
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
   real   (Rkind)              :: zero, one       
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   zero = RZERO ; one = RONE ; typres = RTYP
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( rk2_t )
#include "include/bk2_mult_by_blas.inc"
   end select
      
   END SUBROUTINE bk2_MultRR


!=============================================================================================
   SUBROUTINE bk2_MultRC ( R, B, commute, res )
!=============================================================================================
   use LapackInterface_m, only: lapackinterface_gemm
   real   (Rkind),              intent(in    ) :: R(:,:)
   complex(Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the real array R and the complex array B and puts the result in res%m
!  . If commute == .false.: R * B
!  . If commute == .true. : B * R
!
!  Version using Blas routine ?gemm
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultRC'          
!- For Blas ?gemm:----------------------------------------------------------------------------                                          
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
   complex(Rkind)              :: zero, one    
   complex(Rkind), allocatable :: A(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   zero = CZERO ; one = CONE ; typres = CTYP
!
!- Copy R into the complex array A:
!
   allocate(A(size(R,1),size(R,2)), stat = err) 
   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure for array "A"')
      return
   end if   
   A(:,:) = cmplx(R(:,:),kind=Rkind)
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_mult_by_blas.inc"
   end select
   
   END SUBROUTINE bk2_MultRC


!=============================================================================================
   SUBROUTINE bk2_MultCC ( A, B, commute, res )
!=============================================================================================
   use LapackInterface_m, only: lapackinterface_gemm
   complex(Rkind),              intent(in    ) :: A(:,:)
   complex(Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the two complex arrays A and B and puts the result in res%m
!  . If commute == .false.: A * B
!  . If commute == .true. : B * A
!
!  Version using Blas routine ?gemm
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_MultCC'          
!- For Blas ?gemm: ---------------------------------------------------------------------------
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
   complex(Rkind)              :: zero, one 
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   zero = CZERO ; one = CONE ; typres = CTYP
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_mult_by_blas.inc"
   end select

   END SUBROUTINE bk2_MultCC


!=============================================================================================
   SUBROUTINE bk2_MultII0 ( I, J, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: I(:,:)
   integer(Ikind),              intent(in    ) :: J(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the two integers arrays I and J and puts the result in res%m
!  . If commute == .false.: I * J
!  . If commute == .true. : J * I
!
!  Version using matmul
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultII0'          
   integer                     :: err  
   real   (Rkind), allocatable :: A(:,:), B(:,:)
   integer(Ikind), allocatable :: Ires(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
!
!- Copy A1 and A2 into real matrices A and B:
!      
   allocate(A(size(I,1),size(I,2)), B(size(J,1),size(J,2)), stat = err) 
   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure for array "A" or "B"')
      return
   end if   
   A(:,:) = real(I,kind=Rkind) ; B(:,:) = real(J,kind=Rkind)
!
!- Compute A * B or B * A:
!   
   call bk2_MultRR0 ( A, B, commute, res )
!
!- See if the result will overflow in integers:
!      
   select type ( res )
      type is ( rk2_t )
         if ( any( abs(res%v) > real(huge(Ikind)-1,Rkind) ) ) then
            return
         else
            Ires = int(res%v(:,:),kind=Ikind)
         end if
   end select
!
!- Otherwise convert res to integers:
!      
   deallocate(res) ; allocate(ik2_t :: res)
   select type ( res )
      type is ( ik2_t )
         res%typ = ITYP ; res%nrow = size(Ires,1) ; res%ncol = size(Ires,2)
         call move_alloc ( from = Ires, to = res%v )
   end select
      
   END SUBROUTINE bk2_MultII0


!=============================================================================================
   SUBROUTINE bk2_MultIR0 ( A, B, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A(:,:)
   real   (Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the integer array A and the real array B and puts the result in res%m
!  . If commute == .false.: A * B
!  . If commute == .true. : B * A
!
!  Version using matmul
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultIR0'            
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typres = RTYP
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( rk2_t )
#include "include/bk2_mult_by_matmul.inc"
   end select
   
   END SUBROUTINE bk2_MultIR0


!=============================================================================================
   SUBROUTINE bk2_MultIC0 ( A, B, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A(:,:)
   complex(Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the integer array A and the complex array B and puts the result in res%m
!  . If commute == .false.: A * B
!  . If commute == .true. : B * A
!
!  Version using matmul
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultIC0'                              
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typres = CTYP
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_mult_by_matmul.inc"
   end select
   
   END SUBROUTINE bk2_MultIC0


!=============================================================================================
   SUBROUTINE bk2_MultRR0 ( A, B, commute, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A(:,:)
   real   (Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the two real arrays A and B and puts the result in res%m
!  . If commute == .false.: A * B
!  . If commute == .true. : B * A
!
!  Version using matmul
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultRR0'                              
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typres = RTYP
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( rk2_t )
#include "include/bk2_mult_by_matmul.inc"
   end select
   
   END SUBROUTINE bk2_MultRR0


!=============================================================================================
   SUBROUTINE bk2_MultRC0 ( A, B, commute, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A(:,:)
   complex(Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the real arrays A and the complex array B and puts the result in res%m
!  . If commute == .false.: A * B
!  . If commute == .true. : B * A
!
!  Version using matmul
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultRC0'                              
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typres = CTYP
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_mult_by_matmul.inc"
   end select
   
   END SUBROUTINE bk2_MultRC0
      

!=============================================================================================
   SUBROUTINE bk2_MultCC0 ( A, B, commute, res )
!=============================================================================================
   complex(Rkind),              intent(in    ) :: A(:,:)
   complex(Rkind),              intent(in    ) :: B(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Multiplies the two complex arrays A and B and puts the result in res%m
!  . If commute == .false.: A * B
!  . If commute == .true. : B * A
!
!  Version using matmul
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_MultCC0'                              
   integer                     :: n1, m1, n2, m2, n1m1, n2m2, nres, mres, typres, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
   
   typres = CTYP
!
!- Check dimensions and allocate/reallocate res if needed:
!   
#include "include/bk2_mult_prepare.inc"
!
!- Proceed:
!
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_mult_by_matmul.inc"
   end select
   
   END SUBROUTINE bk2_MultCC0


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Divide (element-wise) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_eDivII  (bk2 = Imat1 ./ Imat2  or  Imat2 ./ Imat1)
! . bk2_eDivIR  (bk2 = Imat  ./ Rmat   or  Rmat  ./ Imat)
! . bk2_eDivIC  (bk2 = Imat  ./ Cmat   or  Cmat  ./ Imat)
! . bk2_eDivRR  (bk2 = Rmat1 ./ Rmat2  or  Rmat2 ./ Rmat1)
! . bk2_eDivRC  (bk2 = Rmat  ./ Cmat   or  Cmat  ./ Rmat)
! . bk2_eDivCC  (bk2 = Cmat1 ./ Cmat2  or  Cmat2 ./ Cmat1)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_eDivII ( A1, A2, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   integer(Ikind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Divides element-wise the two integers arrays A1 and A2 and puts the result in res%v
!  . If commute == .false.: A1 ./ A2
!  . If commute == .true. : A2 ./ A1
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_eDivII'                              
   integer  (Ikind)              :: n, m, n1, m1, n2, m2, n1m1, n2m2, i, j
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
   
   call bk2_reallocIfNeeded ( res, RTYP, n, m ) ;  error_TraceNreturn(opflag, HERE)
   
   n1m1 = n1*m1 ; n2m2 = n2*m2
   
   select type ( res )
      type is ( rk2_t )
            
         select case ( commute )
         
         case ( .false. )
            if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
               do j = 1, m1
                  do i = 1, n1
                     res%v(i,j) = real(A1(i,j),Rkind) / real(A2(i,j),Rkind)
                  end do
               end do         
            else if ( n1m1 == 1 ) then 
               res%v(:,:) = real(A1(1,1),Rkind) / A2
            else if ( n2m2 == 1 ) then
               res%v(:,:) = A1 / real(A2(1,1),Rkind)
            end if
            
         case ( .true. )
            if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
               do j = 1, m1
                  do i = 1, n1
                     res%v(i,j) = real(A2(i,j),Rkind) / real(A1(i,j),Rkind)
                  end do
               end do                        
            else if ( n1m1 == 1 ) then 
               res%v(:,:) = A2 / real(A1(1,1),Rkind)
            else if ( n2m2 == 1 ) then
               res%v(:,:) = real(A2(1,1),Rkind) / A1
            end if
         end select
   
   end select
         
   END SUBROUTINE bk2_eDivII


!=============================================================================================
   SUBROUTINE bk2_eDivIR ( A1, A2, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   real   (Rkind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Divides element-wise integer arrays A1 and the real array A2 and puts the result in res%v
!  . If commute == .false.: A1 ./ A2
!  . If commute == .true. : A2 ./ A1
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_eDivIR'                              
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
   
   call bk2_reallocIfNeeded ( res, RTYP, n, m ) ;  error_TraceNreturn(opflag, HERE)
   
   select type ( res )
      type is ( rk2_t )
#include "include/bk2_ediv.inc"
   end select
      
   END SUBROUTINE bk2_eDivIR


!=============================================================================================
   SUBROUTINE bk2_eDivIC ( A1, A2, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Divides element-wise the integer array  A1 and the complex array A2 and puts the result in 
!  res%v
!  . If commute == .false.: A1 ./ A2
!  . If commute == .true. : A2 ./ A1
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_eDivIC'                              
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
   
   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ;  error_TraceNreturn(opflag, HERE)
   
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_ediv.inc"
   end select
      
   END SUBROUTINE bk2_eDivIC


!=============================================================================================
   SUBROUTINE bk2_eDivRR ( A1, A2, commute, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A1(:,:)
   real   (Rkind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Divides element-wise the two real arrays A1 and A2 and puts the result in res%v
!  . If commute == .false.: A1 ./ A2
!  . If commute == .true. : A2 ./ A1
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_eDivRR'                              
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
   
   call bk2_reallocIfNeeded ( res, RTYP, n, m ) ;  error_TraceNreturn(opflag, HERE)
   
   select type ( res )
      type is ( rk2_t )
#include "include/bk2_ediv.inc"
   end select
         
   END SUBROUTINE bk2_eDivRR
         

!=============================================================================================
   SUBROUTINE bk2_eDivRC ( A1, A2, commute, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Divides element-wise the real array A1 and the complex array A2 and puts the result in res%v
!  . If commute == .false.: A1 ./ A2
!  . If commute == .true. : A2 ./ A1
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_eDivRC'                              
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
   
   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ;  error_TraceNreturn(opflag, HERE)
   
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_ediv.inc"
   end select    
     
   END SUBROUTINE bk2_eDivRC


!=============================================================================================
   SUBROUTINE bk2_eDivCC ( A1, A2, commute, res )
!=============================================================================================
   complex(Rkind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Divides element-wise the two complexes A1 and A2 and puts the result in res%v
!  . If commute == .false.: A1 ./ A2
!  . If commute == .true. : A2 ./ A1
!  Caution: the caller must check the shapes
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_eDivCC'                              
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, n, m
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; n2 = size(A2,1) ; n = max(n1,n2)
   m1 = size(A1,2) ; m2 = size(A2,2) ; m = max(m1,m2)
   
   call bk2_reallocIfNeeded ( res, CTYP, n, m ) ;  error_TraceNreturn(opflag, HERE)
   
   select type ( res )
      type is ( ck2_t )
#include "include/bk2_ediv.inc"
   end select 
      
   END SUBROUTINE bk2_eDivCC


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Divide ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_DivII  (bk2 = Imat1 / Imat2  or  Imat2 / Imat1)
! . bk2_DivIR  (bk2 = Imat  / Rmat   or  Rmat  / Imat)
! . bk2_DivIC  (bk2 = Imat  / Cmat   or  Cmat  / Imat)
! . bk2_DivRR  (bk2 = Rmat1 / Rmat2  or  Rmat2 / Rmat1)
! . bk2_DivRC  (bk2 = Rmat  / Cmat   or  Cmat  / Rmat)
! . bk2_DivCC  (bk2 = Cmat1 / Cmat2  or  Cmat2 / Cmat1)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 


!=============================================================================================
   SUBROUTINE bk2_DivII ( A1, A2, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   integer(Ikind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Computes A1/A2 where A1 and A2 are two integer arrats. 
!  The result (res%v) is real.
!
!  Only the two following cases are expected:
!      . A1 is 1x1 and A2 is square: res%v = A1(1,1)*inv(A2) 
!      . A2 is 1x1                 : res%v = A1 / A2(1,1)
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'bk2_DivII'                              
   integer  (Ikind)            :: n1m1, n2m2
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1m1 = size(A1) ; n2m2 = size(A2)
      
   if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      call opflag%Set(UERROR, HERE, &
                       '<< a / b >> not defined for two matrices (a or b must be scalar)') 
      return
   end if
 
   if ( n1m1 == 1 .and. n2m2 /= 1 ) then
      call bk2_Invmats ( I = A2, res = res ) ; error_TraceNreturn(opflag, HERE)
      select type (res)
         type is (rk2_t)
            res%v(:,:) = res%v * A1(1,1)
      end select      
   else
      call bk2_edivII ( A1, A2, .false., res ) ; error_TraceNreturn(opflag, HERE)
   end if
             
   END SUBROUTINE bk2_DivII


!=============================================================================================
   SUBROUTINE bk2_DivIR ( A1, A2, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   real   (Rkind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Computes A1/A2 or A2/A1 where A1 is an integer array and A2 is a real array. 
!  The result (res%v) is real.
!
!  Only the following cases are expected:
!    If commute == .false.: 
!        . A1 is 1x1 and A2 is square: res%v = A1(1,1)*inv(A2) 
!        . A2 is 1x1                 : res%v = A1 / A2(1,1)
!    If commute == .true.:
!       . A2 is 1x1 and A1 is square: res%v = A2(1,1)*inv(A1) 
!       . A1 is 1x1                 : res%v = A2 / A1(1,1)
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_DivIR'                              
   integer  (Ikind)            :: n1m1, n2m2
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1m1 = size(A1) ; n2m2 = size(A2)
      
   if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      call opflag%Set(UERROR, HERE, &
                       '<< a / b >> not defined for two matrices (a or b must be scalar)') 
      return
   end if
 
   if ( .not. commute ) then
      if ( n1m1 == 1 .and. n2m2 /= 1 ) then
         call bk2_Invmats ( R = A2, res = res ) ; error_TraceNreturn(opflag, HERE)
         select type (res)
            type is (rk2_t)
               res%v(:,:) = res%v * A1(1,1)
         end select      
      else
         call bk2_edivIR ( A1, A2, .false., res ) ; error_TraceNreturn(opflag, HERE)
      end if
   else
      if ( n2m2 == 1 .and. n1m1 /= 1 ) then
         call bk2_Invmats ( I = A1, res = res ) ; error_TraceNreturn(opflag, HERE)
         select type (res)
            type is (rk2_t)
               res%v(:,:) = res%v * A2(1,1)
         end select           
      else
         call bk2_edivIR ( A1, A2, .true., res ) ; error_TraceNreturn(opflag, HERE)
      end if      
   end if   
                
   END SUBROUTINE bk2_DivIR


!=============================================================================================
   SUBROUTINE bk2_DivIC ( A1, A2, commute, res )
!=============================================================================================
   integer(Ikind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Computes A1/A2 or A2/A1 where A1 is an integer array and A2 a complex array.
!  The result (res%v) is complex.
!  (considered cases: see description of bk2_DivIR)
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DivIC'                              
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2
   class   ( bk2_t), allocatable :: invA1
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; m1 = size(A1,2) ; n1m1 = n1*m1
   n2 = size(A2,1) ; m2 = size(A2,2) ; n2m2 = n2*m2
      
   if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      call opflag%Set(UERROR, HERE, &
                        '<< a / b >> not defined for two matrices (a or b must be scalar)') 
      return
   end if
 
   if ( .not. commute ) then
      if ( n1m1 == 1 .and. n2m2 /= 1 ) then
         call bk2_Invmats ( C = A2, res = res ) ; error_TraceNreturn(opflag, HERE)
         select type (res)
            type is (ck2_t)
               res%v(:,:) = res%v * A1(1,1)
         end select      
      else
         call bk2_edivIC ( A1, A2, .false., res ) ; error_TraceNreturn(opflag, HERE)
      end if
   else
      if ( n2m2 == 1 .and. n1m1 /= 1 ) then
         call bk2_Invmats ( I = A1, res = invA1 ) ; error_TraceNreturn(opflag, HERE)
         call bk2_reallocIfNeeded ( res, CTYP, n1, m1 ) ; error_TraceNreturn(opflag, HERE)
         select type (res)
            type is (ck2_t)
               select type (invA1)
                  type is (rk2_t)
                     res%v(:,:) = invA1%v * A2(1,1)
               end select      
         end select           
      else
         call bk2_edivIC ( A1, A2, .true., res ) ; error_TraceNreturn(opflag, HERE)
      end if      
   end if   
             
   END SUBROUTINE bk2_DivIC      
                         

!=============================================================================================
   SUBROUTINE bk2_DivRR ( A1, A2, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A1(:,:)
   real   (Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Computes A1/A2 where A1 and A2 are two real arrays.
!  The result (res%v) is real.
!  (considered cases: see description of bk2_DivII)
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_DivRR'                              
   integer  (Ikind)            :: n1m1, n2m2
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1m1 = size(A1) ; n2m2 = size(A2)
      
   if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      call opflag%Set(UERROR, HERE, &
                       '<< a / b >> not defined for two matrices (a or b must be scalar)') 
      return
   end if
 
   if ( n1m1 == 1 .and. n2m2 /= 1 ) then
      call bk2_Invmats ( R = A2, res = res ) ; error_TraceNreturn(opflag, HERE)
      select type (res)
         type is (rk2_t)
            res%v(:,:) = res%v * A1(1,1)
      end select      
   else
      call bk2_edivRR ( A1, A2, .false., res ) ; error_TraceNreturn(opflag, HERE)
   end if
             
   END SUBROUTINE bk2_DivRR   


!=============================================================================================
   SUBROUTINE bk2_DivRC ( A1, A2, commute, res )
!=============================================================================================
   real   (Rkind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   logical       ,              intent(in    ) :: commute
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Computes A1/A2 or A2/A1 where A1 is a ral array and A2 a complex array.
!  The result (res%v) is complex.
!  (considered cases: see description of bk2_DivIR)
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'bk2_DivRC'
   integer  (Ikind)              :: n1, m1, n2, m2, n1m1, n2m2, err
   class    (bk2_t), allocatable :: invA1
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1 = size(A1,1) ; m1 = size(A1,2) ; n1m1 = n1*m1
   n2 = size(A2,1) ; m2 = size(A2,2) ; n2m2 = n2*m2
      
   if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      call opflag%Set(UERROR, HERE, &
                       '<< a / b >> not defined for two matrices (a or b must be scalar)') 
      return
   end if
 
   if ( .not. commute ) then
      if ( n1m1 == 1 .and. n2m2 /= 1 ) then
         call bk2_Invmats ( C = A2, res = res ) ; error_TraceNreturn(opflag, HERE)
         select type (res)
            type is (ck2_t)
               res%v(:,:) = res%v * A1(1,1)
         end select      
      else
         call bk2_edivRC ( A1, A2, .false., res ) ; error_TraceNreturn(opflag, HERE)
      end if
   else
      if ( n2m2 == 1 .and. n1m1 /= 1 ) then
         call bk2_Invmats ( R = A1, res = invA1 ) ; error_TraceNreturn(opflag, HERE)
         call bk2_reallocIfNeeded ( res, CTYP, n1, m1 ) ; error_TraceNreturn(opflag, HERE)
         select type (res)
            type is (ck2_t)
               select type (invA1)
                  type is (rk2_t)
                     res%v(:,:) = invA1%v * A2(1,1)
               end select      
         end select           
      else
         call bk2_edivRC ( A1, A2, .true., res ) ; error_TraceNreturn(opflag, HERE)
      end if      
   end if   
             
   END SUBROUTINE bk2_DivRC    
      

!=============================================================================================
   SUBROUTINE bk2_DivCC ( A1, A2, res )
!=============================================================================================
   complex(Rkind),              intent(in    ) :: A1(:,:)
   complex(Rkind),              intent(in    ) :: A2(:,:)
   class  (bk2_t), allocatable, intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Computes A1/A2 where A1 and A2 are two complex arrays.
!  The result (res%v) is complex.
!  (considered cases: see description of bk2_DivII)
!-----------------------------------------------------------------------------------R.H. 12/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'bk2_DivCC'
   integer  (Ikind)            :: n1m1, n2m2
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   n1m1 = size(A1) ; n2m2 = size(A2)
      
   if ( n1m1 /= 1 .and. n2m2 /= 1 ) then
      call opflag%Set(UERROR, HERE, &
                       '<< a / b >> not defined for two matrices (a or b must be scalar)') 
      return
   end if
 
   if ( n1m1 == 1 .and. n2m2 /= 1 ) then
      call bk2_Invmats ( C = A2, res = res ) ; error_TraceNreturn(opflag, HERE)
      select type (res)
         type is (ck2_t)
            res%v(:,:) = res%v * A1(1,1)
      end select      
   else
      call bk2_edivCC (A1, A2, .false., res) ; error_TraceNreturn(opflag, HERE)
   end if

   END SUBROUTINE bk2_DivCC    
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Exponentiation ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_Powmats   (bk2 = Kmat1 ^  Kmat2,  K = I, R, C)
! . bk2_ePowmats  (bk2 = Kmat1 .^ Kmat2,  K = I, R, C)
! . bk2_MatPowInt (bk2 = Kmat  ^  nexpo,  K = I, R, C)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_Powmats ( res, I1, R1, C1, I2, R2, C2 )
!=============================================================================================
   class  (bk2_t), allocatable, intent(in out) :: res
   integer(Ikind), optional,    intent(in    ) :: I1(:,:), I2(:,:)
   real   (Rkind), optional,    intent(in    ) :: R1(:,:), R2(:,:)
   complex(Rkind), optional,    intent(in    ) :: C1(:,:), C2(:,:)
!---------------------------------------------------------------------------------------------
!!! NO MORE USED
!
!  Computes Mat1 ^ Mat2, puts the result in res%v where Mat? can be I?, R?, C?
!  At least one of the two matrices must be scalar (ie a 1x1 matrix) and the considered cases
!  are:
!
!  . Mat2 is 1x1:  res%v = Mat1 ^ scal  
!                  and scal = Mat2(1,1) can only be INTEGER and Mat1 any of the 3 types
!
!  . Mat1 is 1x1:  res%v = scal ^ Mat2  (= scal .^ Mat2) 
!                  where scal = Mat1(1,1) and Mat2 are of any of the 3 types
!
!  Example :  c = bk2_Powmats (R1 = a%v, I2 = b%v)
!
!  WARNING: THE CALLER MUST CHECK THAT AT LEAST ONE OF THE TWO MATRICES IS 1 X 1
!
!  Modified (11/19): statements like res = ik2_t(matrix = I1(1,1)**I2) were deleted
!----------------------------------------------------------------------------R.H. 04/18, 11/19

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'bk2_Powmats'                                             
   integer  (Ikind)            :: n1m1, n2m2, typ1, typ2
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
      
   if      ( present(I1) ) then
      typ1 = ITYP
   else if ( present(R1) ) then
      typ1 = RTYP
   else if ( present(C1) ) then
      typ1 = CTYP
   else
      call opflag%Set ( UERROR, HERE, &
           'First matrix argument not present (must be integer, real or complex)' )
      return
   end if            

   if      ( present(I2) ) then
      typ2 = ITYP
   else if ( present(R2) ) then
      typ2 = RTYP
   else if ( present(C2) ) then
      typ2 = CTYP
   else
      call opflag%Set ( UERROR, HERE, &
           'Second matrix argument not present (must be integer, real or complex)' )
      return
   end if            

   select case ( typ1 )
   
      case ( ITYP )
         n1m1 = size(I1)
         select case ( typ2 )
         
            case ( ITYP )               
               n2m2 = size(I2)
               if ( n1m1 == 1 ) then
                  !if (all(I2 >= 0)) then  !! a revoir 
                  if ( all(I2 > 0) ) then
                     call bk2_reallocIfNeeded ( res, ITYP, size(I2,1), size(I2,2) )
                     error_TraceNreturn(opflag, HERE)   
                     select type ( res )
                        type is ( ik2_t )
                           res%v(:,:) = I1(1,1) ** I2
                     end select      
                  else
                     call bk2_reallocIfNeeded ( res, RTYP, size(I2,1), size(I2,2) )
                     error_TraceNreturn(opflag, HERE)   
                     select type ( res )
                        type is ( rk2_t )
                           res%v(:,:) = real(I1(1,1),kind=Rkind) ** I2
                     end select      
                  end if                  
               else if ( n2m2 == 1 ) then
                  call bk2_MatPowInt ( I = I1, nexpo = I2(1,1), res = res )
                  error_TraceNreturn(opflag, HERE)
               end if
               
            case ( RTYP )
               n2m2 = size(R2)
               if ( n1m1 == 1 ) then
                  call bk2_reallocIfNeeded ( res, RTYP, size(R2,1), size(R2,2) )
                  error_TraceNreturn(opflag, HERE)   
                  select type ( res )
                     type is ( rk2_t )
                        res%v(:,:) = real(I1(1,1),kind=Rkind) ** R2
                  end select      
               else if ( n2m2 == 1 ) then
                  call opflag%set ( UERROR, HERE, &
                                    'Real power of integer matrix not implemented' )
                  return
               end if
               
            case ( CTYP )
               n2m2 = size(C2)
               if ( n1m1 == 1 ) then
                  call bk2_reallocIfNeeded ( res, CTYP, size(C2,1), size(C2,2) )
                  error_TraceNreturn(opflag, HERE)   
                  select type ( res )
                     type is ( ck2_t )
                     res%v(:,:) = I1(1,1) ** C2
                  end select
               else if ( n2m2 == 1 ) then
                  call opflag%set ( UERROR, HERE, &
                                    'Complex power of integer matrix not implemented' )
                  return
               end if            
         end select      
 
      case ( RTYP )
         n1m1 = size(R1)
         select case ( typ2 )
         
            case ( ITYP )    
               n2m2 = size(I2)
               if ( n1m1 == 1 ) then
                  call bk2_reallocIfNeeded ( res, RTYP, size(I2,1), size(I2,2) )
                  error_TraceNreturn(opflag, HERE)                  
                  select type ( res )
                     type is ( rk2_t )
                        res%v(:,:) = R1(1,1) ** I2
                  end select
               else if ( n2m2 == 1 ) then
                  call bk2_MatPowInt ( R = R1, nexpo = I2(1,1), res = res )
                  error_TraceNreturn(opflag, HERE)
               end if
               
            case ( RTYP )    
               n2m2 = size(R2)
               if ( n1m1 == 1 ) then
                  call bk2_reallocIfNeeded ( res, RTYP, size(R2,1), size(R2,2) )
                  error_TraceNreturn(opflag, HERE)                  
                  select type ( res )
                     type is ( rk2_t )
                        res%v(:,:) = R1(1,1) ** R2
                  end select
               else if ( n2m2 == 1 ) then
                  call opflag%set ( UERROR, HERE, &
                                    'Real power of a real matrix not implemented' ) 
                  return
               end if
               
            case ( CTYP )
               n2m2 = size(C2)
               if ( n1m1 == 1 ) then
                  call bk2_reallocIfNeeded ( res, CTYP, size(C2,1), size(C2,2) )
                  error_TraceNreturn(opflag, HERE)                  
                  select type ( res )
                     type is ( ck2_t )
                        res%v(:,:) = R1(1,1) ** C2
                  end select
               else if ( n2m2 == 1 ) then
                  call opflag%set ( UERROR, HERE, &
                                   'Complex power of a real matrix not implemented' ) 
                  return
               end if
         end select      

      case ( CTYP )
         n1m1 = size(C1)
         select case ( typ2 )
         
            case ( ITYP )    
               n2m2 = size(I2)
               if ( n1m1 == 1 ) then
                  call bk2_reallocIfNeeded ( res, CTYP, size(I2,1), size(I2,2) )
                  error_TraceNreturn(opflag, HERE)                  
                  select type ( res )
                     type is ( ck2_t )
                        res%v(:,:) = C1(1,1) ** I2
                  end select
               else if ( n2m2 == 1 ) then
                  call bk2_MatPowInt ( C = C1, nexpo = I2(1,1), res = res )
                  error_TraceNreturn(opflag, HERE)
               end if
               
            case ( RTYP )    
               n2m2 = size(R2)
               if ( n1m1 == 1 ) then
                  call bk2_reallocIfNeeded ( res, CTYP, size(R2,1), size(R2,2) )
                  error_TraceNreturn(opflag, HERE)                  
                  select type ( res )
                     type is ( ck2_t )
                        res%v(:,:) = C1(1,1) ** R2
                  end select                  
               else
                  call opflag%set ( UERROR, HERE, &
                                   'Real power of a complex matrix not implemented' )
                  return
               end if
               
            case ( CTYP )
               n2m2 = size(C2)
               if ( n1m1 == 1 ) then
                  call bk2_reallocIfNeeded ( res, CTYP, size(C2,1), size(C2,2) )
                  error_TraceNreturn(opflag, HERE)                  
                  select type ( res )
                     type is ( ck2_t )
                        res%v(:,:) = C1(1,1) ** C2
                  end select
               else
                  call opflag%set ( UERROR, HERE, &
                                   'Complex power of a complex matrix not implemented' )
                  return 
               end if
         end select     
          
   end select          
            
   END SUBROUTINE bk2_Powmats  

   
!=============================================================================================
   SUBROUTINE bk2_MatPowInt ( nexpo, res, I, R, C )
!=============================================================================================
   use util_m, only: util_Invmats
   integer(Ikind),              intent(in    ) :: nexpo
   class  (bk2_t), allocatable, intent(in out) :: res
   integer(Ikind), optional,    intent(in    ) :: I(:,:)
   real   (Rkind), optional,    intent(in    ) :: R(:,:)
   complex(Rkind), optional,    intent(in    ) :: C(:,:)
!---------------------------------------------------------------------------------------------
!!! NO MORE  USED (was used by bk2_Powmats)
!
!  Computes the integer power of a SQUARE matrix I, R or C, puts the result in res%v
!
!  Example :  call bk2_MatPowInt (n, res, I = Imat)
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'bk2_MatPowInt'     
   integer  (Ikind)              :: typ, n, m, j, err
   complex  (Rkind), allocatable :: Cinv(:,:)
   real     (Rkind), allocatable :: Rinv(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()
    
   err = 0
   
   n = 0 ; m = 0
   
   if ( present(I) ) then
      n = size(I,dim=1) ; m = size(I,dim=2) ; typ = ITYP
      if (n /= m) err = 5
   else if ( present(R) ) then
      n = size(R,dim=1) ; m = size(R,dim=2) ; typ = RTYP
      if (n /= m) err = 5
   else if ( present(C) ) then
      n = size(C,dim=1) ; m = size(C,dim=2) ; typ = CTYP    
      if (n /= m) err = 5
   else
      err = 4
   end if           
   
   if ( err /= 0 ) then
      if ( err == 4 ) then
         call opflag%Set(IERROR,HERE,'Matrix not present (must be integer, real or complex)')
         return
      else if ( err == 5 ) then
         call opflag%Set(UERROR, HERE, 'The matrix must be square')
         return
      end if
   end if   
       
   select case ( typ )
   
      case ( ITYP )

         if ( n == 1 ) then
            call bk2_reallocIfNeeded ( res, ITYP, n, m )
            error_TraceNreturn(opflag, HERE)   
            select type ( res )
               type is ( ik2_t )
                  res%v(1,1) = I(1,1) ** nexpo
            end select
         else
            if ( nexpo >= 0 ) then
               call bk2_reallocIfNeeded ( res, ITYP, n, m )
               error_TraceNreturn(opflag, HERE)          
               select type ( res )
                  type is ( ik2_t )
                     res%v(:,:) = I
                     do j = 1, nexpo-1
                        res%v(:,:) = matmul(res%v, I)
                     end do
               end select
            else
               call bk2_reallocIfNeeded ( res, RTYP, n, m )
               error_TraceNreturn(opflag, HERE)    
               call util_InvMats ( I = I, res = Rinv, stat = opflag )
               error_TraceNreturn(opflag, HERE)
               select type ( res )
                  type is ( rk2_t )  
                     res%v(:,:) = Rinv
                     do j = 1, abs(nexpo)-1
                        res%v(:,:) = matmul(res%v, Rinv)
                     end do
               end select
            end if  
         end if    

      case ( RTYP )

         call bk2_reallocIfNeeded ( res, RTYP, n, m )
         error_TraceNreturn(opflag, HERE)   
         
         if ( n == 1 ) then
            select type ( res )
               type is ( rk2_t )
                  res%v(1,1) = R(1,1) ** nexpo
            end select
         else
            if ( nexpo >= 0 ) then
               select type ( res )
                  type is ( rk2_t )
                     res%v(:,:) = R
                     do j = 1, nexpo-1
                        res%v(:,:) = matmul(res%v, R)
                     end do
               end select
            else
               call util_InvMats ( R = R, res = Rinv, stat = opflag )
               error_TraceNreturn(opflag, HERE)
               select type ( res )
                  type is ( rk2_t )
                     res%v(:,:) = Rinv
                     do j = 1, abs(nexpo)-1
                        res%v(:,:) = matmul(res%v, Rinv)
                     end do
               end select
            end if  
         end if    
                              
      case ( CTYP )

         call bk2_reallocIfNeeded ( res, CTYP, n, m )
         error_TraceNreturn(opflag, HERE)   

         if ( n == 1 ) then
            select type ( res )
               type is ( ck2_t )
                  res%v(1,1) = C(1,1) ** nexpo
            end select
         else
            if ( nexpo >= 0 ) then
               select type ( res )
                  type is ( ck2_t )
                     res%v(:,:) = C
                     do j = 1, nexpo-1
                        res%v(:,:) = matmul(res%v, C)
                     end do
               end select
            else
               call util_invMats ( C = C, res = Cinv, stat = opflag )
               error_TraceNreturn(opflag, HERE)
               select type ( res )
                  type is ( ck2_t )
                     res%v(:,:) = Cinv
                     do j = 1, nexpo-1
                        res%v(:,:) = matmul(res%v, Cinv)
                     end do
               end select
            end if  
         end if    
         
   end select    
   
   END SUBROUTINE bk2_MatPowInt
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Inversion and Determinant +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_Invmats   (bk2 = Kmat^(-1),  K = I, R, C)  (using LAPACK ?getrf and ?getri)
! . bk2_Detmats   (bk2 = det(Kmat),  K = I, R, C)  (using LAPACK ?getrf)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE bk2_Invmats ( res, I, R, C )
!=============================================================================================
   class  (bk2_t), allocatable, intent(in out) :: res
   integer(Ikind), optional   , intent(in    ) :: I(:,:)
   real   (Rkind), optional   , intent(in    ) :: R(:,:)
   complex(Rkind), optional   , intent(in    ) :: C(:,:)
!---------------------------------------------------------------------------------------------
!  Inverses the square matrix I, R or C, puts the result in res%m
!
!  Example :  call bk2_Invmats (I = a%v, res=res)
!
!  external routines:
!     . ?getrf, ?getri (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'bk2_Invmats'          
   real     (Rkind), allocatable :: tmpr(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   if ( present(I) ) then
      call bk2_reallocIfNeeded ( res, RTYP, size(I,1), size(I,2) )
      error_TraceNreturn(opflag, HERE)   
      select type ( res )
         type is ( rk2_t )
            call util_InvImats ( I, res%v, opflag ) ; error_TraceNreturn(opflag, HERE)
      end select
   else if ( present(R) ) then
      call bk2_assign ( res, R ) ; error_TraceNreturn(opflag, HERE)
      select type (res)
         type is (rk2_t)
            call util_Invmats ( res%v, opflag  ) ; error_TraceNreturn(opflag, HERE)
      end select
   else if ( present(C) ) then
      call bk2_assign ( res, C ) ; error_TraceNreturn(opflag, HERE)
      select type (res)
         type is (ck2_t)
            call util_Invmats ( res%v, opflag  ) ; error_TraceNreturn(opflag, HERE)
      end select
   else
      call opflag%Set(UERROR, HERE, 'The matrix  must be integer, real or complex' )
   end if   
   
   END SUBROUTINE bk2_Invmats


!=============================================================================================
   SUBROUTINE bk2_Detmats ( res, I, R, C )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_getrf
   use util_m           , only: util_IsIntg
   class  (bk2_t),           allocatable, intent(in out) :: res
   integer(Ikind), optional,              intent(in    ) :: I(:,:)
   real   (Rkind), optional,              intent(in    ) :: R(:,:)
   complex(Rkind), optional,              intent(in    ) :: C(:,:)
!---------------------------------------------------------------------------------------------
!  Computes the determinant of the square matrix I, R or C, puts the result in res%m
!
!  Example :  call bk2_Detmats (deta, I = a%v)
!
!  external routines:
!     . ?getrf (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------           
   character(len=* ), parameter   :: HERE = 'bk2_Detmats'   
   integer  (Ikind )              :: typ, j, err
   real     (Rkind )              :: tmpr(1,1)
   complex  (Rkind )              :: tmpc(1,1)
!- needed by lapack routines ?getrf ----------------------------------------------------------
   integer                        :: n, m, info   
   integer          , allocatable :: ipiv(:)
   real     (Rkind ), allocatable :: rmat(:,:)
   complex  (Rkind ), allocatable :: cmat(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   err = 0
!
!- Need to make a copy (in rmat or cmat) of the input matrix (lapack overwrite it):
!         
   if ( present(I) ) then
      n = size(I,dim=1) ; m = size(I,dim=2)
      if ( n /= m .or. n == 0 ) then
         err = 5
      else
         allocate(rmat(n,n), ipiv(n), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for "rmat" or "ipiv"')
            return
         end if   
         rmat(:,:) = real (I(:,:),kind=Rkind)
         typ = ITYP      
      end if         
   else if ( present(R) ) then
      n = size(R,dim=1) ; m = size(R,dim=2)  
      if ( n /= m .or. n == 0 ) then
         err = 5
      else
         allocate(rmat(n,n), source = R, stat = err)
         if ( err == 0 ) allocate(ipiv(n), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for "rmat" or "ipiv"')
            return
         end if         
         typ = RTYP      
      end if          
   else if ( present(C) ) then
      n = size(C,dim=1) ; m = size(C,dim=2)     
      if ( n /= m .or. n == 0 ) then
         err = 5
      else
         allocate(cmat(n,n), source = C, stat = err)
         if ( err == 0 ) allocate(ipiv(n), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure for "cmat" or "ipiv"')
            return
         end if     
         typ = CTYP    
      end if       
   else
      err = 4
   end if           
   
   if ( err /= 0 ) then
      if ( err == 4 ) then
            call opflag%Set(UERROR, HERE, &
                             'The matrix argument must be integer, real or complex')
            return
      else if ( err == 5 ) then
            call opflag%Set(UERROR, HERE, 'The matrix must be square')
            return
      end if
   end if   
   
   select case (typ)
   
      case (ITYP,RTYP)

         call LapackInterface_getrf ( n, n, rmat, n, ipiv, info )
         
         tmpr(1,1) = RONE
         do j = 1, n
            tmpr(1,1) = tmpr(1,1)*rmat(j,j)
            if (ipiv(j) /= j) tmpr(1,1) =-tmpr(1,1)
         end do   
         
         if ( typ == ITYP ) then
            if ( abs(tmpr(1,1)) < huge(Ikind) .and. util_IsIntg(tmpr(1,1)) ) then
               res = ik2_t(matrix = int(tmpr,kind=Ikind))
            else
               res = rk2_t(matrix = tmpr)
            end if   
         else
            res = rk2_t(matrix = tmpr)
         end if      
      
      case (CTYP)

         call LapackInterface_getrf ( n, n, cmat, n, ipiv, info )

         tmpc(1,1) = CONE
         do j = 1, n
            tmpc(1,1) = tmpc(1,1)*cmat(j,j)
            if ( ipiv(j) /= j ) tmpc(1,1) =-tmpc(1,1)
         end do   
         
         res = ck2_t(matrix = tmpc)
 
   end select    
         
   END SUBROUTINE bk2_Detmats


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Cross product +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
! . bk2_crossIJRC  (bk2 = Imat1 x Kmat2,   K = I, R, C)
! . bk2_crossRISC  (bk2 = Rmat1 x Kmat2,   K = I, R, C)  
! . bk2_crossCIRD  (bk2 = Cmat1 x Kmat2    K = I, R, C)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================         
   SUBROUTINE bk2_crossIJRC ( I, n, m, J, R, C, res )
!=============================================================================================            
   integer(Ikind),              intent(in    ) :: I(:,:)
   integer(Ikind),              intent(in    ) :: n, m
   integer(Ikind), optional,    intent(in    ) :: J(:,:)
   real   (Rkind), optional,    intent(in    ) :: R(:,:)
   complex(Rkind), optional,    intent(in    ) :: C(:,:)
   class  (bk2_t), allocatable, intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Computes the cross product of the integer matrix I with K where K is one of the three 
!  matrices I (integer), R (real) or C (complex)
!
!  The shape (n,m) of I and K may be 
!       . n = 1 and m = 3
!  or
!       . n = 3 and m > 0
!
!  Warning: these sizes are not checked here, the caller must check them
!---------------------------------------------------------------------------------------------
   
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_crossIJRC'
   integer  (Ikind) :: cs, k, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   cs = 0 
   if ( present(J) ) cs = cs+1
   if ( present(R) ) cs = cs+2
   if ( present(C) ) cs = cs+3
   
   if ( cs /= 1 .and. cs /= 2 .and. cs /= 3 ) then
      call opflag%Set(UERROR, HERE, &
                      'at least one and only one of the matrices J, R, C must be present.')
      return
   end if
      
   select case (cs)
      case (1)  ! I x J
         allocate(ik2_t :: res)
         select type (res)
            type is (ik2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                   allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = I(1,2)*J(1,3) - I(1,3)*J(1,2)
                  res%v(1,2) = I(1,3)*J(1,1) - I(1,1)*J(1,3)
                  res%v(1,3) = I(1,1)*J(1,2) - I(1,2)*J(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = ITYP
               else   
                  do k = 1, m
                     res%v(1,k) = I(2,k)*J(3,k) - I(3,k)*J(2,k)
                     res%v(2,k) = I(3,k)*J(1,k) - I(1,k)*J(3,k)
                     res%v(3,k) = I(1,k)*J(2,k) - I(2,k)*J(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = ITYP
               end if
         end select    

      case (2) ! I x R
         allocate(rk2_t :: res)
         select type (res)
            type is (rk2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                  allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = I(1,2)*R(1,3) - I(1,3)*R(1,2)
                  res%v(1,2) = I(1,3)*R(1,1) - I(1,1)*R(1,3)
                  res%v(1,3) = I(1,1)*R(1,2) - I(1,2)*R(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = RTYP
               else   
                  do k = 1, m
                     res%v(1,k) = I(2,k)*R(3,k) - I(3,k)*R(2,k)
                     res%v(2,k) = I(3,k)*R(1,k) - I(1,k)*R(3,k)
                     res%v(3,k) = I(1,k)*R(2,k) - I(2,k)*R(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = RTYP
               end if
         end select    

      case (3) ! I x C
         allocate(ck2_t :: res)
         select type (res)
            type is (ck2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                  allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = I(1,2)*C(1,3) - I(1,3)*C(1,2)
                  res%v(1,2) = I(1,3)*C(1,1) - I(1,1)*C(1,3)
                  res%v(1,3) = I(1,1)*C(1,2) - I(1,2)*C(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = CTYP
               else   
                  do k = 1, m
                     res%v(1,k) = I(2,k)*C(3,k) - I(3,k)*C(2,k)
                     res%v(2,k) = I(3,k)*C(1,k) - I(1,k)*C(3,k)
                     res%v(3,k) = I(1,k)*C(2,k) - I(2,k)*C(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = CTYP
               end if
         end select    
         
   end select
   
   END SUBROUTINE bk2_crossIJRC


!=============================================================================================         
   SUBROUTINE bk2_crossRISC (R, n, m, I, S, C, res )
!=============================================================================================            
   real   (Rkind),              intent(in    ) :: R(:,:)
   integer(Ikind),              intent(in    ) :: n, m   
   integer(Ikind), optional,    intent(in    ) :: I(:,:)
   real   (Rkind), optional,    intent(in    ) :: S(:,:)
   complex(Rkind), optional,    intent(in    ) :: C(:,:)
   class  (bk2_t), allocatable, intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Computes the cross product of the real matrix R with K where K is one of the three 
!  matrices I (integer), S (real) or C (complex)
!  (see bk2_crossIJRC)
!---------------------------------------------------------------------------------------------
   
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_crossRISC'
   integer  (Ikind)            :: cs, k, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   cs = 0 
   if ( present(I) ) cs = cs+1
   if ( present(S) ) cs = cs+2
   if ( present(C) ) cs = cs+3
      
   if ( cs /= 1 .and. cs /= 2 .and. cs /= 3 ) then
      call opflag%Set(UERROR, HERE, &
                      'at least one and only one of the matrices I, S, C must be present.')
      return
   end if

   select case (cs)
      case (1) ! R x I
         allocate(rk2_t :: res)
         select type (res)
            type is (rk2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                  allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = R(1,2)*I(1,3) - R(1,3)*I(1,2)
                  res%v(1,2) = R(1,3)*I(1,1) - R(1,1)*I(1,3)
                  res%v(1,3) = R(1,1)*I(1,2) - R(1,2)*I(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = RTYP
               else   
                  do k = 1, m
                     res%v(1,k) = R(2,k)*I(3,k) - R(3,k)*I(2,k)
                     res%v(2,k) = R(3,k)*I(1,k) - R(1,k)*I(3,k)
                     res%v(3,k) = R(1,k)*I(2,k) - R(2,k)*I(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = RTYP
               end if
         end select    

      case (2) ! R x R
         allocate(rk2_t :: res)
         select type (res)
            type is (rk2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                  allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = R(1,2)*S(1,3) - R(1,3)*S(1,2)
                  res%v(1,2) = R(1,3)*S(1,1) - R(1,1)*S(1,3)
                  res%v(1,3) = R(1,1)*S(1,2) - R(1,2)*S(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = RTYP
               else   
                  do k = 1, m
                     res%v(1,k) = R(2,k)*S(3,k) - R(3,k)*S(2,k)
                     res%v(2,k) = R(3,k)*S(1,k) - R(1,k)*S(3,k)
                     res%v(3,k) = R(1,k)*S(2,k) - R(2,k)*S(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = RTYP
               end if
         end select    

      case (3) ! R x C
         allocate(ck2_t :: res)
         select type (res)
            type is (ck2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                  allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = R(1,2)*C(1,3) - R(1,3)*C(1,2)
                  res%v(1,2) = R(1,3)*C(1,1) - R(1,1)*C(1,3)
                  res%v(1,3) = R(1,1)*C(1,2) - R(1,2)*C(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = CTYP
               else   
                  do k = 1, m
                     res%v(1,k) = R(2,k)*C(3,k) - R(3,k)*C(2,k)
                     res%v(2,k) = R(3,k)*C(1,k) - R(1,k)*C(3,k)
                     res%v(3,k) = R(1,k)*C(2,k) - R(2,k)*C(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = CTYP
               end if
         end select    
    end select    

   END SUBROUTINE bk2_crossRISC


!=============================================================================================         
   SUBROUTINE bk2_crossCIRD (C, n, m, I, R, D, res )
!=============================================================================================            
   complex(Rkind),              intent(in    ) :: C(:,:)
   integer(Ikind),              intent(in    ) :: n, m
   integer(Ikind), optional,    intent(in    ) :: I(:,:)
   real   (Rkind), optional,    intent(in    ) :: R(:,:)
   complex(Rkind), optional,    intent(in    ) :: D(:,:)
   class  (bk2_t), allocatable, intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Computes the cross product of the complex matrix C with K where K is one of the three 
!  matrices I (integer), R (real) or D (complex)
!  (see bk2_crossIJRC)
!---------------------------------------------------------------------------------------------
   
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'bk2_crossCIRD'
   integer  (Ikind)            :: cs, k, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set ()

   cs = 0 
   if ( present(I) ) cs = cs+1
   if ( present(R) ) cs = cs+2
   if ( present(D) ) cs = cs+3
      
   if ( cs /= 1 .and. cs /= 2 .and. cs /= 3 ) then
      call opflag%Set(UERROR, HERE, &
                      'at least one and only one of the matrices I, R, D must be present.')
      return
   end if

   select case (cs)
      case (1) ! C x I
         allocate(ck2_t :: res)
         select type (res)
            type is (ck2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                  allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = C(1,2)*I(1,3) - C(1,3)*I(1,2)
                  res%v(1,2) = C(1,3)*I(1,1) - C(1,1)*I(1,3)
                  res%v(1,3) = C(1,1)*I(1,2) - C(1,2)*I(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = CTYP
               else   
                  do k = 1, m
                     res%v(1,k) = C(2,k)*I(3,k) - C(3,k)*I(2,k)
                     res%v(2,k) = C(3,k)*I(1,k) - C(1,k)*I(3,k)
                     res%v(3,k) = C(1,k)*I(2,k) - C(2,k)*I(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = CTYP
               end if
         end select  
           
      case (2) ! C x R
         allocate(ck2_t :: res)
         select type (res)
            type is (ck2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                  allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = C(1,2)*R(1,3) - C(1,3)*R(1,2)
                  res%v(1,2) = C(1,3)*R(1,1) - C(1,1)*R(1,3)
                  res%v(1,3) = C(1,1)*R(1,2) - C(1,2)*R(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = CTYP
               else   
                  do k = 1, m
                     res%v(1,k) = C(2,k)*R(3,k) - R(3,k)*R(2,k)
                     res%v(2,k) = C(3,k)*R(1,k) - R(1,k)*R(3,k)
                     res%v(3,k) = C(1,k)*R(2,k) - R(2,k)*R(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = CTYP
               end if
         end select    

      case (3) ! C x D   
         allocate(ck2_t :: res)
         select type (res)
            type is (ck2_t)
               if ( n == 1 ) then
                  allocate(res%v(1,3), stat = err)
               else
                  allocate(res%v(3,m), stat = err)
               end if
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for res%v')
                  return
               end if   
               if ( n == 1 ) then
                  res%v(1,1) = C(1,2)*D(1,3) - C(1,3)*C(1,2)
                  res%v(1,2) = C(1,3)*D(1,1) - C(1,1)*C(1,3)
                  res%v(1,3) = C(1,1)*D(1,2) - C(1,2)*C(1,1)
                  res%nrow = 1 ; res%ncol = 3 ; res%typ = CTYP
               else   
                  do k = 1, m
                     res%v(1,k) = C(2,k)*D(3,k) - C(3,k)*D(2,k)
                     res%v(2,k) = C(3,k)*D(1,k) - C(1,k)*D(3,k)
                     res%v(3,k) = C(1,k)*D(2,k) - C(2,k)*D(1,k)
                  end do
                  res%nrow = 3 ; res%ncol = m ; res%typ = CTYP
               end if
         end select    
   end select    

   END SUBROUTINE bk2_crossCIRD

      
END MODULE bk2_m
