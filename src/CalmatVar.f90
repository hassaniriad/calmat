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

MODULE CalmatVar_m

   use CalmatConstants_m
   
   use pk2_m, only : pk2_t, pk2_Assign
   use pk2Constants_m, only: IZERO  
   use str_m, only: str_t
   use err_m, only: err_t, err_MoveAlloc
    
   implicit none

   private
   public :: var_t, CalmatVar_GetNamesOfVars, CalmatVar_alloc1
   
   type, extends(pk2_t) :: var_t
      integer(Ikind) :: status   = G_FREE
      integer(Ikind) :: firstIns = 0
      integer(Ikind) :: lastIns  = 0
      integer(Ikind) :: objId    = 0, cmpObjId = 0
      integer(Ikind) :: deleteAt =-1
   contains
      procedure, pass :: Destroy           => CalmatVar_Destroy
      procedure, pass :: Reset             => CalmatVar_Reset
      procedure, pass :: GetName           => CalmatVar_GetName
      
      procedure, pass :: CalmatVar_AssignFromVarToVar
      generic :: assignment(=) => CalmatVar_AssignFromVarToVar ! overload pk2 assign
      
   end type var_t

!
!- Defined constructor:
!   
   interface var_t
      module procedure CalmatVar_Constructor
   end interface

CONTAINS


!=============================================================================================
   FUNCTION CalmatVar_Constructor ( p, status, first, last, obj, cmpObj, del, flag ) result ( res )
!=============================================================================================
   type   (pk2_t), optional, intent(in    ) :: p
   integer(Ikind), optional, intent(in    ) :: status, first, last, obj, cmpObj, del
   type   (err_t), optional, intent(   out) :: flag
   type   (var_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  var_t constructor 
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables: --------------------------------------------------------------------------   
   type(err_t) :: flagerr
!---------------------------------------------------------------------------------------------      
   
   if ( present(p)        ) call pk2_assign ( res, p, flagerr )   
   if ( allocated(p%name) ) res%name = (p%name)
   
   if ( present(status) ) res%status   = status
   if ( present(first)  ) res%firstIns = first
   if ( present(last)   ) res%lastIns  = last
   if ( present(obj)    ) res%objId    = obj
   if ( present(cmpObj) ) res%cmpObjId = cmpObj
   if ( present(del)    ) res%deleteAt = del

   res%constr = .true.
   
   if ( flagerr > IZERO .and. present(flag) ) flag = flagerr

   END FUNCTION CalmatVar_Constructor


!=============================================================================================
   SUBROUTINE CalmatVar_AssignFromVarToVar ( lhs, rhs ) 
!=============================================================================================
   class(var_t), intent(in out) :: lhs
   class(var_t), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------
   
   call pk2_Assign ( lhs, rhs )
   if ( allocated(rhs%name) ) lhs%name = (rhs%name)
   
   lhs%status   = rhs%status
   lhs%firstIns = rhs%firstIns
   lhs%lastIns  = rhs%lastIns
   lhs%objId    = rhs%objId
   lhs%cmpObjId = rhs%cmpObjId
   lhs%deleteAt = rhs%deleteAt
   
   END SUBROUTINE CalmatVar_AssignFromVarToVar

   
!=============================================================================================   
   SUBROUTINE CalmatVar_Destroy ( self )
!=============================================================================================   
   class(var_t), intent(in out) :: self
!-----------------------------------------------------------------------------------R.H. 07/20       
!
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------   

   call self%pk2_t%Destroy()

   self%status   = G_FREE
   self%firstIns = 0
   self%lastIns  = 0
   self%objId    = 0
   self%cmpObjId = 0
   self%deleteAt =-1
      
   END SUBROUTINE CalmatVar_Destroy
   

!=============================================================================================   
   SUBROUTINE CalmatVar_Reset ( self, name, status, firstIns, lastIns, objId, cmpObjId )
!=============================================================================================   
   class    (var_t), intent(in out) :: self
   character(len=*), intent(in    ) :: name
   integer  (Ikind), intent(in    ) :: status, firstIns, lastIns, objId, cmpObjId
!---------------------------------------------------------------------------------------------
!  Resets name, status, firstIns, lastIns, objId and cmpObjId members
!-----------------------------------------------------------------------------------R.H. 07/20       

   self%name     = trim(adjustl(name))
   !self%constr   = .false.
   self%status   = status
   self%firstIns = firstIns
   self%lastIns  = lastIns
   self%objId    = objId
   self%cmpObjId = cmpObjId
   self%deleteAt =-1
      
   END SUBROUTINE CalmatVar_Reset


!=============================================================================================
   FUNCTION CalmatVar_GetName ( self ) result ( name )
!=============================================================================================
   class    (var_t), intent(in)  :: self
   character(len=:), allocatable :: name
!--------------------------------------------------------------------------------------------- 
!  Copies the members "%name" of "self" to a character variable
!--------------------------------------------------------------------------------------------- 

   if ( allocated(self%name) ) then
      name = self%name
   else
      name = ''
   end if
   
   END FUNCTION CalmatVar_GetName
   

!=============================================================================================      
   SUBROUTINE CalmatVar_GetNamesOfVars ( vars, varNames )
!=============================================================================================      
   type(var_t),              intent(in    ) :: vars(:)
   type(str_t), allocatable, intent(in out) :: varNames(:)
!---------------------------------------------------------------------------------------------
!  Copies the members "%name" of a var_t array into a str_t (string) array
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------      
   integer(Ikind) :: nvar, i
!---------------------------------------------------------------------------------------------   

   nvar = size(vars)
   if ( allocated(varNames) ) then
      if ( size(varNames) < nvar ) then
         deallocate(varNames) ; allocate(varNames(nvar))
      end if
   else
      allocate(varNames(nvar))
   end if
   
   do i = 1, nvar
      if ( allocated(vars(i)%name) ) then
         varNames(i)%str = (vars(i)%name)
      else
         varNames(i)%str = ''
      end if
   end do
   
   do i = nvar+1, size(varNames)
      varNames(i)%str = ''
   end do
   
   END SUBROUTINE CalmatVar_GetNamesOfVars


!=============================================================================================
   SUBROUTINE CalmatVar_alloc1 (mode, t, n, stat)
!=============================================================================================
   character(len=1),              intent(in    ) :: mode
   integer  (Ikind),              intent(in    ) :: n
   type     (var_t), allocatable, intent(in out) :: t(:)
   type     (err_t), optional   , intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the 1d var_t array t. It is specially useful when  
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
   character(len=*), parameter   :: HERE = 'CalmatVar_alloc1'
   integer  (Ikind)              :: nn, err
   type     (var_t), allocatable :: tmp(:)
   type     (err_t)              :: flag
!--------------------------------------------------------------------------------------------- 

   flag = err_t ()
   
   if ( present(stat) ) stat = flag

   if ( .not. allocated(t) ) then

      allocate(t(n), stat=err)
      if ( err /= 0 ) &
         call flag%set ( msg="Allocation failure for array t", where=HERE, stat=G_IERROR ) 

   else if ( mode == 'e' ) then

      if ( allocated(t) ) deallocate(t)
      allocate(t(n), stat=err)
      if ( err /= 0 ) &
         call flag%set ( msg="Allocation failure for array t", where=HERE, stat=G_IERROR ) 
           
   else if ( mode == 's' ) then                      
   
      allocate(tmp(n), stat=err)
      if ( err /= 0 ) then
         call flag%set (msg="Allocation failure for array tmp", where=HERE, stat=G_IERROR ) 
      else   
                                         
         nn = min(n,size(t,kind=Ikind))
         tmp(1:nn) = t(1:nn)
      
         call move_alloc ( from = tmp, to = t )
      end if   
         
   else
      call flag%set ( stat = G_UERROR, where = HERE, msg = &
                      "Invalid mode << "//mode//" >> (must be << e >> or << s >>)" ) 
   end if 
   
   if ( present(stat) ) then
      call err_MoveAlloc ( from = flag, to = stat )
   else
      if ( flag > IZERO ) call flag%display(abort=.true.)
   end if      
            
   END SUBROUTINE CalmatVar_alloc1
   
      
END MODULE CalmatVar_m
            
