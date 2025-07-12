#include "error.fpp"

MODULE CalmatObject_m

! A faire: passer flagerr en argument des procedures (plutot que var module opflag)

   use kindParameters_m , only: Ikind
   use pk2Constants_m   , only: IZERO, IONE
   use err_m            , only: err_t, err_MoveAlloc
   use CalmatConstants_m, only: G_IERROR, G_UERROR, G_FREE
   
   implicit none
   
   private
   public :: obj_t, CalmatObject_alloc1
!
!- A DT for a "structure" (an object made of a collection of pk2 variables):
!   
   type :: obj_t
      character(len=:), allocatable :: name           ! The name of the structure

      integer  (Ikind)              :: status   = G_FREE
      integer  (Ikind)              :: firstIns = 0
      integer  (Ikind)              :: lastIns  = 0
      integer  (Ikind)              :: deleteAt =-IONE
      
      integer  (Ikind), allocatable :: varId(:)       ! The #s of variables (stored in G_vars)
                                                      ! that form the structure
                                                     
      integer  (Ikind)              :: nCmp    = 0, & ! Actual number of such variables
                                       nCmpMax = 10   ! Initial maximum number of variables
                                       
      logical                       :: constr = .false.      
   contains

      procedure, pass :: PrintMe => CalmatObject_PrintMe
      
      procedure, pass :: InsertAComponent => CalmatObject_InsertAComponent
            
      procedure, pass :: RemoveAComponent => CalmatObject_RemoveAComponentByNum
            
      procedure, pass :: Destroy => CalmatObject_DestroyObj
      procedure, pass :: Frees   => CalmatObject_FreesObj
      
      procedure, pass :: SetStatus   => CalmatObject_SetStatus
      procedure, pass :: SetFirstIns => CalmatObject_SetFirstIns
      procedure, pass :: SetLastIns  => CalmatObject_SetLastIns

      procedure, pass :: GetName     => CalmatObject_GetName
      procedure, pass :: GetStatus   => CalmatObject_GetStatus
      procedure, pass :: GetFirstIns => CalmatObject_GetFirstIns
      procedure, pass :: GetLastIns  => CalmatObject_GetLastIns
      procedure, pass :: GetCmpNum   => CalmatObject_GetCmpNum
      procedure, pass :: GetVarId    => CalmatObject_GetVarId
      procedure, pass :: GetAllVarId => CalmatObject_GetAllVarId
      
      
   end type obj_t
   
   interface obj_t
      module procedure CalmatObject_constructor
   end interface   
   
CONTAINS

 
!=============================================================================================
   SUBROUTINE CalmatObject_DestroyObj ( self )
!=============================================================================================
   class(obj_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Destroy the object "self"
!---------------------------------------------------------------------------------------------

   if ( allocated(self%varId) ) deallocate(self%varId)
   if ( allocated(self%name ) ) deallocate(self%name )
   
   self%status = G_FREE ; self%firstIns = 0 ; self%lastIns = 0 ; self%deleteAt =-IONE
   
   self%nCmp = 0 ; self%nCmpMax = 10 ; self%constr  = .false. 
   
   END SUBROUTINE CalmatObject_DestroyObj


!=============================================================================================
   SUBROUTINE CalmatObject_FreesObj ( self )
!=============================================================================================
   class(obj_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Resets some members of the object "self"
!---------------------------------------------------------------------------------------------

   self%status = G_FREE ; self%firstIns = 0 ; self%lastIns = 0 ; self%deleteAt =-IONE
   self%constr  = .false. 
   
   END SUBROUTINE CalmatObject_FreesObj

   
!=============================================================================================
   FUNCTION CalmatObject_constructor ( nameObj, status, first, last, del, flagerr ) result ( res )
!=============================================================================================
   character(len=*), intent(in    ) :: nameObj
   integer  (Ikind), intent(in    ) :: status, first, last, del
   type     (err_t), intent(in out) :: flagerr
   type     (obj_t)                 :: res   
!---------------------------------------------------------------------------------------------
!  Creates an empty new structure of name "nameObj" 
!
!  Note: To avoid frequent reallocations whenever a component must be added to the structure, 
!  the members %varId is first allocated with a maximum of %nCmpMax. This maximum number of
!  components will be increased if necessary (see CalmatObject_InsertAComponent).
!
!-----------------------------------------------------------------------------------R.H. 05/19     
   
!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'CalmatObject_constructor'  
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   allocate(res%varId(res%nCmpMax), stat = err)
   
   if ( err /= 0 ) then
      call flagerr%set ( stat = G_IERROR, where = HERE, &
                          msg = 'Allocation failure for "components"' )
      return
   end if
   
   res%name     =  nameObj
   res%constr   = .true.
   res%status   = status
   res%firstIns = first
   res%lastIns  = last
   res%deleteAt = del
   
   END FUNCTION CalmatObject_constructor


!=============================================================================================
   SUBROUTINE CalmatObject_SetStatus ( self, status )
!=============================================================================================
   class  (obj_t), intent(in out) :: self
   integer(Ikind), intent(in    ) :: status
!--------------------------------------------------------------------------------------------- 
!  Sets the status code of the object self
!--------------------------------------------------------------------------------------------- 

   self%status = status
   
   END SUBROUTINE CalmatObject_SetStatus


!=============================================================================================
   SUBROUTINE CalmatObject_SetFirstIns ( self, firstIns )
!=============================================================================================
   class  (obj_t), intent(in out) :: self
   integer(Ikind), intent(in    ) :: firstIns
!--------------------------------------------------------------------------------------------- 
!  Sets the # of first instruction of the flow where this object is defined 
!--------------------------------------------------------------------------------------------- 

   self%firstIns = firstIns
   
   END SUBROUTINE CalmatObject_SetFirstIns


!=============================================================================================
   SUBROUTINE CalmatObject_SetLastIns ( self, lastIns )
!=============================================================================================
   class  (obj_t), intent(in out) :: self
   integer(Ikind), intent(in    ) :: lastIns
!--------------------------------------------------------------------------------------------- 
!  Sets the # of last instruction of the flow where this object is defined 
!--------------------------------------------------------------------------------------------- 

   self%lastIns = lastIns
   
   END SUBROUTINE CalmatObject_SetLastIns


!=============================================================================================
   FUNCTION CalmatObject_GetName ( self ) result ( name)
!=============================================================================================
   class    (obj_t), intent(in)  :: self
   character(len=:), allocatable :: name
!--------------------------------------------------------------------------------------------- 
!  Gets the name of the object self
!--------------------------------------------------------------------------------------------- 

   name = (self%name)
   
   END FUNCTION CalmatObject_GetName


!=============================================================================================
   FUNCTION CalmatObject_GetStatus ( self ) result ( status )
!=============================================================================================
   class  (obj_t), intent(in) :: self
   integer(Ikind)             :: status
!--------------------------------------------------------------------------------------------- 
!  Gets the status code of the object self
!--------------------------------------------------------------------------------------------- 

   status = self%status
   
   END FUNCTION CalmatObject_GetStatus


!=============================================================================================
   FUNCTION CalmatObject_GetFirstIns ( self ) result ( firstIns )
!=============================================================================================
   class  (obj_t), intent(in) :: self
   integer(Ikind)             :: firstIns
!--------------------------------------------------------------------------------------------- 
!  Gets the # of the first instruction of the flow(s) where this object was defined
!--------------------------------------------------------------------------------------------- 

   firstIns = self%firstIns
   
   END FUNCTION CalmatObject_GetFirstIns


!=============================================================================================
   FUNCTION CalmatObject_GetLastIns ( self ) result ( lastIns )
!=============================================================================================
   class  (obj_t), intent(in) :: self
   integer(Ikind)             :: lastIns
!--------------------------------------------------------------------------------------------- 
!  Gets the # of the last instruction of the flow(s) where this object was defined
!--------------------------------------------------------------------------------------------- 

   lastIns = self%lastIns
   
   END FUNCTION CalmatObject_GetLastIns


!=============================================================================================
   FUNCTION CalmatObject_GetCmpNum ( self ) result ( n )
!=============================================================================================
   class  (obj_t), intent(in) :: self
   integer(Ikind)             :: n
!--------------------------------------------------------------------------------------------- 
!  Gets the number of components of self
!--------------------------------------------------------------------------------------------- 

   n = self%nCmp
   
   END FUNCTION CalmatObject_GetCmpNum   


!=============================================================================================
   FUNCTION CalmatObject_GetVarId ( self, icmp ) result ( varId )
!=============================================================================================
   class  (obj_t), intent(in) :: self
   integer(Ikind), intent(in) :: icmp
   integer(Ikind)             :: varId
!--------------------------------------------------------------------------------------------- 
!  Gets the variable Id of the component #icmp of self
!--------------------------------------------------------------------------------------------- 

   varId = self%varId(icmp)
   
   END FUNCTION CalmatObject_GetVarId


!=============================================================================================
   FUNCTION CalmatObject_GetAllVarId ( self ) result ( varId )
!=============================================================================================
   class  (obj_t), intent(in)  :: self
   integer(Ikind), allocatable :: varId(:)
!--------------------------------------------------------------------------------------------- 
!  Gets the variables Id of the components of self
!--------------------------------------------------------------------------------------------- 

   varId = self%varId
   
   END FUNCTION CalmatObject_GetAllVarId

   
!=============================================================================================
   SUBROUTINE CalmatObject_PrintMe (self)
!=============================================================================================
   class(obj_t), intent(in) :: self
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------   
   integer   (Ikind) :: i, ncmp
   character(len=99) :: msg
!---------------------------------------------------------------------------------------------
   
   print*,'in CalmatObject_PrintMe'
   if ( allocated(self%name) ) print*,'object name: ',self%name
   
   if ( allocated(self%varId) ) then   
      ncmp = 0
      do i = 1, self%nCmp
         if ( self%varId(i) /= 0 ) ncmp = ncmp + 1
      end do   
      print*,'nbr of components: ',ncmp
      print*,'components:'
      do i = 1, self%nCmp
         if ( self%varId(i) /= 0 ) then
            write(msg,'(a,i0,a)')" (=> var(#",self%varId(i),"))"
         end if
      end do 
   end if     
   
   END SUBROUTINE CalmatObject_PrintMe


!=============================================================================================
   SUBROUTINE CalmatObject_RemoveAComponentByNum (self, varId, flagerr)
!=============================================================================================
   class  (obj_t), intent(in out) :: self
   integer(Ikind), intent(in    ) :: varId
   type   (err_t), intent(in out) :: flagerr
!--------------------------------------------------------------------------------------------- 
!  Removes from the object "self" a component given by its variable identifier ("varId") 
!-----------------------------------------------------------------------------------R.H. 05/19     

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatObject_RemoveAComponentByNum'
   integer  (Ikind)            :: i, ncmp
!--------------------------------------------------------------------------------------------- 

!
!- Remove the variable varId and count the remaining component:
!
   ncmp = 0
   do i = 1, self%nCmp
      if ( self%varId(i) == varId ) then
         self%varId(i) = 0
      elseif ( self%varId(i) /= 0 ) then
         ncmp = ncmp + 1
      end if
   end do   
!
!- If any, the object is empty, deallocate its name:  
!
   if ( ncmp == 0 ) then
      if ( allocated(self%name) ) then
         deallocate(self%name)
         self%nCmp = 0         
      else
         call flagerr%set ( stat = G_IERROR, where = HERE, msg = &
                            'An existing structure does not have a name' )
      end if
   end if
   
   END SUBROUTINE CalmatObject_RemoveAComponentByNum
   
    
!=============================================================================================
   SUBROUTINE CalmatObject_InsertAComponent (self, varId, cmpId, flagerr)
!=============================================================================================
   class    (obj_t), intent(in out) :: self
   integer  (Ikind), intent(in    ) :: varId
   integer  (Ikind), intent(   out) :: cmpId
   type     (err_t), intent(in out) :: flagerr
!--------------------------------------------------------------------------------------------- 
!  Adds the variable # "varId" to the components of the structure "self".
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'CalmatObject_InsertAComponent'
   integer  (Ikind), allocatable :: tmp(:)
   integer  (Ikind)              :: i, nCmpOld, nCmpNew, nCmpMaxNew, err
!--------------------------------------------------------------------------------------------- 

   cmpId = 0
   
   if ( .not. allocated(self%varId) ) then
      call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                        'Could not insert a component in a non-existing structure' )
      return
   end if
      
   nCmpOld = self%nCmp
!
!- First see if "varId" is already a component of "self". If yes return:
!   
   do i = 1, nCmpOld
      if ( self%varId(i) == varId ) then
         cmpId = i
         return
      end if
   end do
!
!- Find a free component # in [1,nCmpMax]. If not, resize.
!            
   cmpId = 0
   do i = 1, nCmpOld
      if ( self%varId(i) == 0 ) then
         cmpId = i
         exit
      end if
   end do
          
   if ( cmpId == 0 ) then
      nCmpNew = nCmpOld + 1
      cmpId    = nCmpNew
   else
      nCmpNew = nCmpOld   
   end if   
   
   if ( cmpId <= self%nCmpMax ) then
      self%nCmp     = nCmpNew
      self%varId(i) = varId
      return
   end if      
        
   nCmpMaxNew = self%nCmpMax + 10
        
   allocate(tmp(nCmpMaxNew), stat = err)
   if ( err /= 0 ) then
      call flagerr%set ( stat=G_IERROR, where=HERE, msg='Allocation failure for "tmp"' )
      return
   end if
   
   tmp(1:nCmpOld) = self%varId
      
   call move_alloc (from = tmp, to = self%varId)
   
   self%nCmp    = nCmpNew
   self%nCmpMax = nCmpMaxNew     
               
   END SUBROUTINE CalmatObject_InsertAComponent


!=============================================================================================
   SUBROUTINE CalmatObject_alloc1 (mode, t, n, stat)
!=============================================================================================
   character(len=1),              intent(in    ) :: mode
   integer  (Ikind),              intent(in    ) :: n
   type     (obj_t), allocatable, intent(in out) :: t(:)
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
   character(len=*), parameter   :: HERE = 'CalmatObject_alloc1'
   integer  (Ikind)              :: nn, err
   type     (obj_t), allocatable :: tmp(:)
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
         call flag%set ( msg="Allocation failure for array tmp", where=HERE, stat=G_IERROR ) 
      else   
                                         
         nn = min(n,size(t,kind=Ikind))
         tmp(1:nn) = t(1:nn)
      
         call move_alloc (from = tmp, to = t)
      end if   
         
   else
      call flag%set ( stat = G_UERROR, where = HERE, msg = &
                      " Invalid mode << "//mode//" >> (must be << e >> or << s >>)" ) 
   end if 
   
   if ( present(stat) ) then
      call err_MoveAlloc (from = flag, to = stat)
   else
      if ( flag > IZERO ) call flag%display(abort=.true.)
   end if      
            
   END SUBROUTINE CalmatObject_alloc1
   
   
END MODULE CalmatObject_m