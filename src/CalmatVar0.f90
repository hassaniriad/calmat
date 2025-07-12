!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Module CalmatVar
!
! This module contains the definition of a DT for a variable
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Date: 07/20
!---------------------------------------------------------------------------------------------

MODULE CalmatVar

   use CalmatConstants
   use pk2mod, only : pk2_t, pk2_Assign, pk2_MoveAlloc, opflag, str_t, err_t, err_MoveAlloc
      
   implicit none

   private
   public :: CalmatVar_GetPtrsToVals, CalmatVar_GetNamesOfVars, CalmatVar_alloc1, &
             CalmatVar_MoveAlloc, CalmatVar_Set
   
   type :: var_t
      !private
      type   (pk2_t) :: v
      integer(Iprec) :: status   = G_FREE
      integer(Iprec) :: firstIns = 0
      integer(Iprec) :: lastIns  = 0
      integer(Iprec) :: objId    = 0, cmpObjId = 0
   contains
      procedure, pass :: Destroy           => CalmatVar_Destroy
      procedure, pass :: Reset             => CalmatVar_Reset
 
      procedure, pass :: GetStatus         => CalmatVar_GetStatus
      procedure, pass :: GetFirstIns       => CalmatVar_GetFirstIns
      procedure, pass :: GetLastIns        => CalmatVar_GetLastIns
      procedure, pass :: GetObjId          => CalmatVar_GetObjId
      procedure, pass :: GetcmpObjId       => CalmatVar_GetcmpObjId
      procedure, pass :: GetName           => CalmatVar_GetName
      procedure, pass :: GetVal            => CalmatVar_GetVal
      procedure, pass :: GetPtrToVal       => CalmatVar_GetPtrToVal
      procedure, pass :: GetValDim         => CalmatVar_GetValDim
      
      procedure, pass :: SetValByMoveAlloc => CalmatVar_SetValByMoveAlloc
      procedure, pass :: SetValByCopy      => CalmatVar_SetValByCopy
      procedure, pass :: SetName           => CalmatVar_SetName
      procedure, pass :: SetStatus         => CalmatVar_SetStatus
      procedure, pass :: SetFirstIns       => CalmatVar_SetFirstIns
      procedure, pass :: SetLastIns        => CalmatVar_SetLastIns
      procedure, pass :: SetObjId          => CalmatVar_SetObjId
      procedure, pass :: SetCmpObjId       => CalmatVar_SetCmpObjId
      
      procedure, pass :: CalmatVar_SetValSubMat1
      procedure, pass :: CalmatVar_SetValSubMat2
      
      generic, public :: SetValSubMat      => CalmatVar_SetValSubMat1, CalmatVar_SetValSubMat2
      
   end type var_t

   interface CalmatVar_movealloc
      module procedure CalmatVar_moveallocVarToVar, CalmatVar_moveallocPk2ToVar
   end interface
   
   interface var_t
      module procedure CalmatVar_fSetToI, CalmatVar_fSetToR , &
                       CalmatVar_fSetToC, CalmatVar_fSetToL , &
                       CalmatVar_fSetToS, CalmatVar_fSetToEmpty
   end interface

   public :: var_t
   
   interface CalmatVar_Set
      module procedure CalmatVar_SetToI, CalmatVar_SetToR , &
                       CalmatVar_SetToC, CalmatVar_SetToL , &
                       CalmatVar_SetToS, CalmatVar_SetToEmpty
   end interface
        

CONTAINS


!=============================================================================================
   FUNCTION CalmatVar_fSetToEmpty (name, status, firstIns, lastIns, objId, cmpObjId) result (res)
!=============================================================================================
   character(len=*), optional, intent(in) :: name
   integer  (Iprec), optional, intent(in) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t)                       :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   call CalmatVar_SetToEmpty (name, res, status, firstIns, lastIns, objId, cmpObjId)
   
   END FUNCTION CalmatVar_fSetToEmpty


!=============================================================================================
   FUNCTION CalmatVar_fSetToI (val, name, status, firstIns, lastIns, objId, cmpObjId) result (res)
!=============================================================================================
   integer  (Iprec),           intent(in) :: val
   character(len=*), optional, intent(in) :: name
   integer  (Iprec), optional, intent(in) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t)                       :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   call CalmatVar_SetToI (val, name, res, status, firstIns, lastIns, objId, cmpObjId)
   
   END FUNCTION CalmatVar_fSetToI


!=============================================================================================
   FUNCTION CalmatVar_fSetToR (val, name, status, firstIns, lastIns, objId, cmpObjId) result (res)
!=============================================================================================
   real     (Rprec),           intent(in) :: val
   character(len=*), optional, intent(in) :: name
   integer  (Iprec), optional, intent(in) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t)                       :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   call CalmatVar_SetToR (val, name, res, status, firstIns, lastIns, objId, cmpObjId)
   
   END FUNCTION CalmatVar_fSetToR


!=============================================================================================
   FUNCTION CalmatVar_fSetToC (val, name, status, firstIns, lastIns, objId, cmpObjId) result (res)
!=============================================================================================
   complex  (Rprec),           intent(in) :: val
   character(len=*), optional, intent(in) :: name
   integer  (Iprec), optional, intent(in) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t)                       :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   call CalmatVar_SetToC (val, name, res, status, firstIns, lastIns, objId, cmpObjId)
   
   END FUNCTION CalmatVar_fSetToC


!=============================================================================================
   FUNCTION CalmatVar_fSetToL (val, name, status, firstIns, lastIns, objId, cmpObjId) result (res)
!=============================================================================================
   logical         ,           intent(in) :: val
   character(len=*), optional, intent(in) :: name
   integer  (Iprec), optional, intent(in) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t)                       :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   call CalmatVar_SetToL (val, name, res, status, firstIns, lastIns, objId, cmpObjId)
   
   END FUNCTION CalmatVar_fSetToL


!=============================================================================================
   FUNCTION CalmatVar_fSetToS (val, name, status, firstIns, lastIns, objId, cmpObjId) result (res)
!=============================================================================================
   type     (str_t),           intent(in) :: val
   character(len=*), optional, intent(in) :: name
   integer  (Iprec), optional, intent(in) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t)                       :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   call CalmatVar_SetToS (val, name, res, status, firstIns, lastIns, objId, cmpObjId)
   
   END FUNCTION CalmatVar_fSetToS


!=============================================================================================
   SUBROUTINE CalmatVar_SetToEmpty (name, res, status, firstIns, lastIns, objId, cmpObjId)
!=============================================================================================
   character(len=*), optional, intent(in    ) :: name
   integer  (Iprec), optional, intent(in    ) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t),           intent(in out) :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
   type(pk2_t) :: tmp
!---------------------------------------------------------------------------------------------   

   tmp = pk2_t()
   
   if (present(name)) then
      call pk2_movealloc ( from = tmp, to = res%v, movename = .true. )
   else
      call pk2_movealloc ( from = tmp, to = res%v, movename = .false. )
   end if
   
   if (present(status)  ) res%status   = status
   if (present(firstIns)) res%firstIns = firstIns
   if (present(lastIns) ) res%lastIns  = lastIns
   if (present(objId)   ) res%objId    = objId
   if (present(cmpObjId)) res%cmpObjId = cmpObjId
   
   END SUBROUTINE CalmatVar_SetToEmpty


!=============================================================================================
   SUBROUTINE CalmatVar_SetToI (v, name, res, status, firstIns, lastIns, objId, cmpObjId)
!=============================================================================================
   integer  (Iprec),           intent(in    ) :: v
   character(len=*), optional, intent(in    ) :: name
   integer  (Iprec), optional, intent(in    ) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t),           intent(in out) :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
   type(pk2_t) :: tmp
!---------------------------------------------------------------------------------------------   

print*,'in CalmatVar_SetToI'
#include "include/Calmat_Set.inc"
   
   END SUBROUTINE CalmatVar_SetToI


!=============================================================================================
   SUBROUTINE CalmatVar_SetToR (v, name, res, status, firstIns, lastIns, objId, cmpObjId)
!=============================================================================================
   real     (Rprec),           intent(in    ) :: v
   character(len=*), optional, intent(in    ) :: name
   integer  (Iprec), optional, intent(in    ) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t),           intent(in out) :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
   type(pk2_t) :: tmp
!---------------------------------------------------------------------------------------------   

#include "include/Calmat_Set.inc"
   
   END SUBROUTINE CalmatVar_SetToR


!=============================================================================================
   SUBROUTINE CalmatVar_SetToC (v, name, res, status, firstIns, lastIns, objId, cmpObjId)
!=============================================================================================
   complex  (Rprec),           intent(in    ) :: v
   character(len=*), optional, intent(in    ) :: name
   integer  (Iprec), optional, intent(in    ) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t),           intent(in out) :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
   type(pk2_t) :: tmp
!---------------------------------------------------------------------------------------------   

#include "include/Calmat_Set.inc"
   
   END SUBROUTINE CalmatVar_SetToC


!=============================================================================================
   SUBROUTINE CalmatVar_SetToL (v, name, res, status, firstIns, lastIns, objId, cmpObjId)
!=============================================================================================
   logical         ,           intent(in    ) :: v
   character(len=*), optional, intent(in    ) :: name
   integer  (Iprec), optional, intent(in    ) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t),           intent(in out) :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
   type(pk2_t) :: tmp
!---------------------------------------------------------------------------------------------   

#include "include/Calmat_Set.inc"
   
   END SUBROUTINE CalmatVar_SetToL


!=============================================================================================
   SUBROUTINE CalmatVar_SetToS (v, name, res, status, firstIns, lastIns, objId, cmpObjId)
!=============================================================================================
   type     (str_t),           intent(in    ) :: v
   character(len=*), optional, intent(in    ) :: name
   integer  (Iprec), optional, intent(in    ) :: status, firstIns, lastIns, objId, cmpObjId
   type     (var_t),           intent(in out) :: res   
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
   type(pk2_t) :: tmp
!---------------------------------------------------------------------------------------------   

#include "include/Calmat_Set.inc"
   
   END SUBROUTINE CalmatVar_SetToS
   

!=============================================================================================   
   SUBROUTINE CalmatVar_Destroy ( self )
!=============================================================================================   
   class(var_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------   
   
   self%status   = G_FREE
   self%firstIns = 0
   self%lastIns  = 0
   self%objId    = 0
   self%cmpObjId = 0
   
   call self%v%Destroy()
   
   END SUBROUTINE CalmatVar_Destroy
   

!=============================================================================================   
   SUBROUTINE CalmatVar_Reset ( self, name, status, firstIns, lastIns, objId, cmpObjId )
!=============================================================================================   
   class    (var_t), intent(in out) :: self
   character(len=*), intent(in    ) :: name
   integer  (Iprec), intent(in    ) :: status, firstIns, lastIns, objId, cmpObjId
!---------------------------------------------------------------------------------------------
!  Resets name, status, firstIns, lastIns, objId and cmpObjId members
!---------------------------------------------------------------------------------------------

   self%v%name   = trim(adjustl(name))
   self%status   = status
   self%firstIns = firstIns
   self%lastIns  = lastIns
   self%objId    = objId
   self%cmpObjId = cmpObjId
      
   END SUBROUTINE CalmatVar_Reset


!=============================================================================================
   SUBROUTINE CalmatVar_SetValByMoveAlloc ( self, val, movename, flag )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   logical       , intent(in    ) :: movename
   type   (pk2_t), intent(in out) :: val
   type   (err_t), intent(in out) :: flag      
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   call pk2_MoveAlloc ( from = val, to = self%v, movename = movename, stat = flag )
      
   END SUBROUTINE CalmatVar_SetValByMoveAlloc

   
!=============================================================================================
   SUBROUTINE CalmatVar_SetValByCopy ( self, val, copyname, flag )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   logical       , intent(in    ) :: copyname
   type   (pk2_t), intent(in    ) :: val
   type   (err_t), intent(in out) :: flag   
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   call pk2_Assign ( lhs = self%v, rhs = val )

   if (opflag%code /= 0) call err_MoveAlloc (from = opflag, to = flag)! (flag from pk2_Assign)
   
   if (copyname) self%v%name = val%name
   
   END SUBROUTINE CalmatVar_SetValByCopy


!=============================================================================================
   SUBROUTINE CalmatVar_SetValSubMat1 ( self, val, indi, flag )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   integer(Iprec), intent(in    ) :: indi(:)
   type   (pk2_t), intent(in    ) :: val
   type   (err_t), intent(in out) :: flag
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   call self%v%SetSubmat ( val, indi )
   
   if (opflag%code /= 0) call err_MoveAlloc (from = opflag, to = flag)! (flag from %SetSubmat)
   
   END SUBROUTINE CalmatVar_SetValSubMat1


!=============================================================================================
   SUBROUTINE CalmatVar_SetValSubMat2 ( self, val, indi, indj, flag )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   integer(Iprec), intent(in    ) :: indi(:), indj(:)
   type   (pk2_t), intent(in    ) :: val
   type   (err_t), intent(in out) :: flag   
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   call self%v%SetSubmat ( val, indi, indj )

   if (opflag%code /= 0) call err_MoveAlloc (from = opflag, to = flag)! (flag from %SetSubmat)
   
   END SUBROUTINE CalmatVar_SetValSubMat2


!=============================================================================================
   SUBROUTINE CalmatVar_SetName ( self, name )
!=============================================================================================
   class    (var_t), intent(in out) :: self
   character(len=*), intent(in    ) :: name
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   self%v%name = name
      
   END SUBROUTINE CalmatVar_SetName


!=============================================================================================
   SUBROUTINE CalmatVar_SetStatus ( self, status )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   integer(Iprec), intent(in    ) :: status
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   self%status = status
   
   END SUBROUTINE CalmatVar_SetStatus
   
   
!=============================================================================================
   SUBROUTINE CalmatVar_SetFirstIns ( self, firstIns )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   integer(Iprec), intent(in    ) :: firstIns
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   self%firstIns = firstIns
   
   END SUBROUTINE CalmatVar_SetFirstIns


!=============================================================================================
   SUBROUTINE CalmatVar_SetLastIns ( self, lastIns )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   integer(Iprec), intent(in    ) :: lastIns
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   self%lastIns = lastIns
   
   END SUBROUTINE CalmatVar_SetLastIns
   

!=============================================================================================
   SUBROUTINE CalmatVar_SetObjId ( self, objId )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   integer(Iprec), intent(in    ) :: objId
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   self%objId = objId
   
   END SUBROUTINE CalmatVar_SetObjId


!=============================================================================================
   SUBROUTINE CalmatVar_SetCmpObjId ( self, cmpObjId )
!=============================================================================================
   class  (var_t), intent(in out) :: self
   integer(Iprec), intent(in    ) :: cmpObjId
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   self%cmpObjId = cmpObjId
   
   END SUBROUTINE CalmatVar_SetCmpObjId
   

!=============================================================================================
   FUNCTION CalmatVar_GetStatus ( self ) result ( status )
!=============================================================================================
   class  (var_t), intent(in) :: self
   integer(Iprec)             :: status
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   status = self%status
   
   END FUNCTION CalmatVar_GetStatus


!=============================================================================================
   FUNCTION CalmatVar_GetFirstIns ( self ) result ( firstIns )
!=============================================================================================
   class  (var_t), intent(in) :: self
   integer(Iprec)             :: firstIns
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   firstIns = self%firstIns
   
   END FUNCTION CalmatVar_GetFirstIns


!=============================================================================================
   FUNCTION CalmatVar_GetLastIns ( self ) result ( lastIns )
!=============================================================================================
   class  (var_t), intent(in) :: self
   integer(Iprec)             :: lastIns
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   lastIns = self%lastIns
   
   END FUNCTION CalmatVar_GetLastIns


!=============================================================================================
   FUNCTION CalmatVar_GetObjId ( self ) result ( objId )
!=============================================================================================
   class  (var_t), intent(in) :: self
   integer(Iprec)             :: objId
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   objId = self%objId
   
   END FUNCTION CalmatVar_GetObjId


!=============================================================================================
   FUNCTION CalmatVar_GetCmpObjId ( self ) result ( cmpObjId )
!=============================================================================================
   class  (var_t), intent(in) :: self
   integer(Iprec)             :: cmpObjId
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   cmpObjId = self%cmpObjId
   
   END FUNCTION CalmatVar_GetCmpObjId


!=============================================================================================
   FUNCTION CalmatVar_GetName ( self ) result ( name )
!=============================================================================================
   class    (var_t), intent(in)  :: self
   character(len=:), allocatable :: name
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   if (allocated(self%v%name)) then
      name = self%v%name
   else
      name = ''
   end if
   
   END FUNCTION CalmatVar_GetName


!=============================================================================================
   FUNCTION CalmatVar_GetVal ( self ) result ( v )
!=============================================================================================
   class(var_t), intent(in) :: self
   type (pk2_t)             :: v
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   call pk2_Assign ( v, self%v )
      
   END FUNCTION CalmatVar_GetVal


!=============================================================================================
   FUNCTION CalmatVar_GetValDim ( self ) result ( res )
!=============================================================================================
   class  (var_t), intent(in) :: self
   integer(Iprec)             :: res(3)
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   res(1) = self%v%nrow ; res(2) = self%v%ncol ; res(3) = self%v%typ
      
   END FUNCTION CalmatVar_GetValDim

!=============================================================================================
   FUNCTION CalmatVar_GetPtrToVal ( self ) result ( vptr )
!=============================================================================================
   class(var_t), target , intent(in) :: self
   type (pk2_t), pointer             :: vptr
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   vptr => self%v
      
   END FUNCTION CalmatVar_GetPtrToVal


!=============================================================================================      
   FUNCTION CalmatVar_GetPtrsToVals ( vars, i1, i2, flag ) result ( ptrs )
!=============================================================================================      
   type   (var_t), target , intent(in    ) :: vars(:)
   integer(Iprec),          intent(in    ) :: i1, i2
   type   (err_t),          intent(in out) :: flag
   type   (pk2_t), pointer                 :: ptrs(:)
   
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = '(in CalmatVar_GetPtrsVals)'
!--------------------------------------------------------------------------------------------- 

   nullify(ptrs)
   
   if (i1 < 1 .or. i2 > size(vars)) then
      flag = err_t (stat = G_IERROR, msg = HERE // ' i1 < 1 .or. i2 > size(vars)')
   else
      ptrs => vars(i1:i2)%v
   end if
      
   END FUNCTION CalmatVar_GetPtrsToVals
   

!=============================================================================================      
   SUBROUTINE CalmatVar_GetNamesOfVars ( vars, varNames )
!=============================================================================================      
   type(var_t),              intent(in    ) :: vars(:)
   type(str_t), allocatable, intent(in out) :: varNames(:)
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------      
   integer(Iprec) :: nvar, i
!---------------------------------------------------------------------------------------------   

   nvar = size(vars)
   if (allocated(varNames)) then
      if (size(varNames) < nvar) then
         deallocate(varNames) ; allocate(varNames(nvar))
      end if
   else
      allocate(varNames(nvar))
   end if
   
   do i = 1, nvar
      if (allocated(vars(i)%v%name)) then
         varNames(i)%str = (vars(i)%v%name)
      else
         varNames(i)%str = ''
      end if
   end do
   
   do i = nvar+1, size(varNames)
      varNames(i)%str = ''
   end do
   
   END SUBROUTINE CalmatVar_GetNamesOfVars


!=============================================================================================
   SUBROUTINE CalmatVar_MoveAllocPk2ToVar ( from, to, movename, stat )
!=============================================================================================
   type   (pk2_t), intent(in out) :: from
   type   (var_t), intent(in out) :: to
   logical       , intent(in    ) :: movename
   type   (err_t), intent(in out) :: stat      
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   call pk2_MoveAlloc ( from = from, to = to%v, movename = movename, stat = stat )
      
   END SUBROUTINE CalmatVar_MoveAllocPk2ToVar


!=============================================================================================
   SUBROUTINE CalmatVar_MoveAllocVarToVar ( from, to, movename, stat )
!=============================================================================================
   type   (var_t), intent(in out) :: from
   type   (var_t), intent(in out) :: to
   logical       , intent(in    ) :: movename
   type   (err_t), intent(in out) :: stat      
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   call pk2_MoveAlloc ( from = from%v, to = to%v, movename = movename, stat = stat )
      
   END SUBROUTINE CalmatVar_MoveAllocVarToVar


!=============================================================================================
   SUBROUTINE CalmatVar_alloc1 (mode, t, n, stat)
!=============================================================================================
   character(len=1),              intent(in    ) :: mode
   integer  (Iprec),              intent(in    ) :: n
   type     (var_t), allocatable, intent(in out) :: t(:)
   type     (err_t), optional   , intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the 1d obj_t array t. It is specially useful when  
!  t is already allocated but needs to be extended.
! 
! * If mode = 's' (s for 'save'), t is saved in a temporary array, de-allocated and re-allocated 
!   with the new size (n). The saved entries are then copied back to t.
!
!   Warning: if the new size n is less than the old one, the part of t located beyond n will be
!            definitively lost. 
!
! * If mode = 'e' (e for 'erase'), t is simply de-allocated and re-allocated without a copy of 
!   its possible previous values.
! 
!
!  Inputs
!
!   mode: 's' or 'e'
!      n: the (new) size of t
!      t: array of obj_t to be allocated or re-allocated with the new size n
!
!-----------------------------------------------------------------------------------R.H. 05/19
 
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = '(in CalmatVar_alloc1)'
   integer  (Iprec)              :: nn, err
   type     (var_t), allocatable :: tmp(:)
   type     (err_t)              :: flag
!--------------------------------------------------------------------------------------------- 

   flag = err_t ()
   
   if (present(stat)) stat = flag

   if (.not. allocated(t)) then

      allocate(t(n), stat=err)
      if (err /= 0) &
         flag = err_t (msg = HERE //" Allocation failure for array t", stat = G_IERROR) 

   else if (mode == 'e') then

      if (allocated(t)) deallocate(t)
      allocate(t(n), stat=err)
      if (err /= 0) &
         flag = err_t (msg = HERE //" Allocation failure for array t", stat = G_IERROR) 
           
   else if (mode == 's') then                      
   
      allocate(tmp(n), stat=err)
      if (err /= 0) then
         flag = err_t (msg = HERE //" Allocation failure for array tmp", stat = G_IERROR) 
      else   
                                         
         nn = min(n,size(t,kind=Iprec))
         tmp(1:nn) = t(1:nn)
      
         call move_alloc (from = tmp, to = t)
      end if   
         
   else
      flag = err_t ( stat = G_UERROR, msg = HERE // &
                     " Invalid mode << "//mode//" >> (must be << e >> or << s >>)" ) 
   end if 
   
   if (present(stat)) then
      call err_MoveAlloc (from = flag, to = stat)
   else
      if (flag%code > 0) call flag%display(abort=.true.)
   end if      
            
   END SUBROUTINE CalmatVar_alloc1
   
      
END MODULE CalmatVar
            