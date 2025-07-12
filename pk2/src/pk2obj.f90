!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: pk2Obj
!
! Description: 
! This module defines a structure (DT pk2obj_t) made of several pk2 members.
!----------------------------------------------------------------------------R.H. 05/19, 10/19     

MODULE pk2Obj_m

   use pk2_m
   
!   use pk2_m 
!   use util_m, only: util_alloc
      
   implicit none
   
   private
   public :: pk2obj_t, pk2Obj_alloc1
   
   integer(Ikind), private, parameter :: ADDCMP = 10
   
   type :: pk2obj_t
      !private
      character(len=:), allocatable :: name              ! Name of the object
      integer  (Ikind)              :: ncmp = 0          ! Number of available components
      integer  (Ikind)              :: ncmpMax = ADDCMP  ! Maximum number of components
      type     (pk2_t), allocatable :: cmp(:)            ! The set of pk2 omponents
      logical         , allocatable :: usedCmp(:)        ! .false. for free components
      logical                       :: constr = .false.  ! .true. if initialized by the constr.  
!
!-    Notes: 
!     . Each component must have a name (cmp(:)%name cannot be empty or not allocated).
!     . The structure may have no name (%name can be empty or not allocated) but only if 
!       %ncmp = 1 (and in this case the object is considered to be similar to the pk2 variable 
!       cmp(1)).
!     . To avoid frequent reallocations whenever a component must be added to the structure, 
!       the members %cmp and %usedCmp are first allocated with a maximum length of %nCmpMax. 
!       This maximum number of components will be increased by ADDCMP whenever necessary.
!
   contains
      private
!
!-    Defined assignments:
!   
      procedure, pass :: pk2Obj_Assign      ! pk2Obj = pk2Obj
      procedure, pass :: pk2Obj_AssignToPk2 ! pk2Obj = pk2
      generic, public :: assignment (=) => pk2Obj_Assign, pk2Obj_AssignToPk2
!
!-    For inserting components into a structure:
!      
      procedure, pass :: pk2Obj_InsertAComponent ! insert a given component
      procedure, pass :: pk2Obj_InsertComponents ! insert a set of components
      generic, public :: InsertComp => pk2Obj_InsertAComponent, pk2Obj_InsertComponents
!
!-    For deleting a component from a structure:
!   
      procedure, pass, public :: RemoveComp => pk2Obj_RemoveAComponent
!
!-    For transfering the components of a structure to a set of pk2 variables
!     (and then destroying the structure):
!      
      procedure, pass, public :: TransferTopk2 => pk2Obj_TransferTopk2 
!
!-    For destroying a structure;
!      
      procedure, pass, public :: Destroy => pk2Obj_Destroy  
!
!-    For printing a structure:
!
      procedure, pass, public :: PrintMe => pk2Obj_Print
!
!-    Some accessors and mutators:
!      
      procedure, pass, public :: GetName => pk2obJ_GetName
      procedure, pass, public :: SetName => pk2obJ_SetName
      procedure, pass, public :: GetAcmp => pk2obJ_GetAcmp
      procedure, pass, public :: SetACmp => pk2Obj_InsertAComponent
          
   end type pk2obj_t
!
!- Defined constructors:
!   
   interface pk2obj_t
      module procedure pk2Obj_constructor0, pk2Obj_constructor1, pk2Obj_constructor2
   end interface   

CONTAINS


!=============================================================================================
   FUNCTION pk2Obj_GetName ( self ) result ( res )
!=============================================================================================
   class    (pk2obj_t),             intent(in) :: self
   character(len=:   ), allocatable            :: res
!---------------------------------------------------------------------------------------------
!  Sets the name of the structure 
!-----------------------------------------------------------------------------------R.H. 10/19 

   res = '' ; if (allocated(self%name)) res = self%name
   
   END FUNCTION pk2Obj_GetName
   

!=============================================================================================
   SUBROUTINE pk2Obj_SetName ( self, objName )
!=============================================================================================
   class    (pk2obj_t), intent(in out) :: self
   character(len=*   ), intent(in    ) :: objName
!---------------------------------------------------------------------------------------------
!  Sets the name of the structure 
!-----------------------------------------------------------------------------------R.H. 10/19 

   self%name = objName
   
   END SUBROUTINE pk2Obj_SetName


!=============================================================================================
   FUNCTION pk2Obj_GetAcmp ( self, cmpName) result ( res )
!=============================================================================================
   class    (pk2obj_t), intent(in) :: self
   character(len=*   ), intent(in) :: cmpName
   type     (pk2_t   )             :: res
!---------------------------------------------------------------------------------------------
!  Gets the component whose name is cmpName (if not present return an empty pk2) 
!-----------------------------------------------------------------------------------R.H. 10/19 

!- local variables ---------------------------------------------------------------------------    
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------
   
   do i = 1, self%ncmp
      if (self%cmp(i)%name == trim(adjustl(cmpName))) then
         res = self%cmp(i)
         return
      end if   
   end do      

   res%name = trim(adjustl(cmpName))
         
   END FUNCTION pk2Obj_GetAcmp
   

!=============================================================================================
   FUNCTION pk2Obj_constructor0 ( objName ) result ( res )
!=============================================================================================   
   character(len=*   ), intent(in) :: objName   
   type     (pk2obj_t)             :: res   
!---------------------------------------------------------------------------------------------
!  Initializes an empty structure object of name "objName"
!-----------------------------------------------------------------------------------R.H. 10/19     

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter   :: HERE = 'pk2Obj_constructor0'  
   integer  (Ikind)              :: err
!---------------------------------------------------------------------------------------------

   allocate(res%cmp(res%ncmpMax), res%usedCmp(res%ncmpMax), stat = err)
   
   if (err /= 0) then
      call opflag%set ( stat = IERROR, msg = HERE //' Allocation failure' )  
      call res%Destroy()
      return
   end if

   res%ncmp    = 0   
   res%name    = objName
   res%usedCmp = .false.
   res%constr  = .true.
   
   END FUNCTION pk2Obj_constructor0

   
!=============================================================================================
   FUNCTION pk2Obj_constructor1 ( compValues, compNames, objName ) result ( res )
!=============================================================================================
   use util_Strings_m, only: util_OrdinalAbbrev
   type     (pk2_t   ), intent(in) :: compValues(:)
   type     (str_t   ), intent(in) :: compNames (:)
   character(len=*   ), intent(in) :: objName
   type     (pk2obj_t)             :: res   
!---------------------------------------------------------------------------------------------
!  Initializes a structure object with the given set "compValues" of pk2 variables.
!  The names of the corresponding components are given by "compNames" and the name of the 
!  structure is "objName"
!-----------------------------------------------------------------------------------R.H. 05/19     
   
!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter   :: HERE = 'pk2Obj_constructor1'  
   integer  (Ikind)              :: ncomp, i, err
   character(len=:), allocatable :: name
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! opflag = err_t ()
   
   ncomp = size(compValues)
   
   if (size(compNames) /= ncomp) then
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'Number of components and number of component names are not equal')  
      return
   end if
   
   if (ncomp > res%ncmpMax) res%ncmpMax = ncomp + ADDCMP
   
   allocate(res%cmp    (res%ncmpMax),                   stat = err)
   allocate(res%usedCmp(res%ncmpMax), source = .false., stat = err)       

   if (err /= 0) then
      call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure')  
      call res%Destroy()
      return
   end if
         
   do i = 1, ncomp
      if (allocated(compNames(i)%str)) then
         name = trim(compNames(i)%str)
      else
         name = ''
      end if
      if (len_trim(name) == 0) then
         call opflag%set ( stat = UERROR, where = HERE, msg =  &
                          ' The '//trim(util_OrdinalAbbrev(i))//' component name is empty' )  
         call res%Destroy()
         return
      end if     
      res%cmp    (i)      = compValues(i)
      res%cmp    (i)%name = name
      res%usedCmp(i)      = .true.       
   end do   

   res%name   = trim(objName)
   res%ncmp   = ncomp   
   res%constr = .true.
   
   END FUNCTION pk2Obj_constructor1


!=============================================================================================
   FUNCTION pk2Obj_constructor2 ( sourceObj, targetObjName ) result ( targetObj )
!=============================================================================================
   type     (pk2obj_t), intent(in) :: sourceObj
   character(len=*   ), intent(in) :: targetObjName
   type     (pk2obj_t)             :: targetObj   
!---------------------------------------------------------------------------------------------
!  Initializes the components of a structure object with those of another one.
!-----------------------------------------------------------------------------------R.H. 05/19     
   
!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter   :: HERE = 'pk2Obj_constructor2'  
   integer  (Ikind)              :: err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! opflag = err_t ()
   
   if (len_trim(targetObjName) == 0 .and. sourceObj%ncmp > 1) then
!
!-    a structure with more than one component must have a name
!   
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'A non-empty string expected for a structure name')  
      return
   end if
         
   allocate(targetObj%cmp    (sourceObj%ncmpMax), source = sourceObj%cmp    , stat = err)
   allocate(targetObj%usedCmp(sourceObj%ncmpMax), source = sourceObj%usedCmp, stat = err)

   if (err /= 0) then
      call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure')  
      call targetObj%Destroy()
      return
   end if
   
   targetObj%name    = targetObjName
   targetObj%ncmp    = sourceObj%ncmp   
   targetObj%constr  = .true.
   targetObj%ncmpMax = sourceObj%ncmpMax
   
   END FUNCTION pk2Obj_constructor2
   

!=============================================================================================
   SUBROUTINE pk2Obj_Assign ( lhs, rhs )
!=============================================================================================
   class(pk2obj_t), intent(in out) :: lhs
   type (pk2obj_t), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Replaces all members of "lhs" (except %name) by those of "rhs".
!
!  Note:
!  The rhs%name member is also copied to lhs%name only if the rhs was initialized by the 
!  constructor (for expression like: lhs = pk2obj_t(...,name = 'foo'))
!-----------------------------------------------------------------------------------R.H. 05/19       

!- local variables --------------------------------------------------------------------------- 
!---------------------------------------------------------------------------------------------
   
   if (opflag%code > 0) return !opflag = err_t ()

   if (allocated(rhs%name) .and. rhs%constr) lhs%name = (rhs%name)
   
   lhs%ncmp    = rhs%ncmp
   lhs%ncmpMax = rhs%ncmpMax 
   
   if (allocated(lhs%cmp)    ) deallocate(lhs%cmp    )
   if (allocated(lhs%usedCmp)) deallocate(lhs%usedCmp)
   
   if (allocated(rhs%cmp)    ) allocate(lhs%cmp    (lhs%ncmpMax), source = rhs%cmp    )
   if (allocated(rhs%usedCmp)) allocate(lhs%usedCmp(lhs%ncmpMax), source = rhs%usedCmp)
   
   END SUBROUTINE pk2Obj_Assign


!=============================================================================================
   SUBROUTINE pk2Obj_AssignToPk2 ( lhs, rhs )
!=============================================================================================
   class(pk2obj_t), intent(   out) :: lhs
   type (pk2_t   ), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Assigns the pk2_t "rhs" to the structure "lhs" 
!
!  Notes:
!  . lhs will be a structure with only one component,
!  . the name of this component will be rhs%name (this name must not be empty)
!  . lhs will have no structure name.
!-----------------------------------------------------------------------------------R.H. 05/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2Obj_AssignToPk2'
   integer  (Ikind)            :: err 
!---------------------------------------------------------------------------------------------
   
   if (opflag%code > 0) return !opflag = err_t ()

   err = 1
   if (allocated(rhs%name)) then
      if (len_trim(rhs%name) /= 0) err = 0
   end if
   
   if (err /= 0) then
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       "In an assignment << pk2obj = pk2 >>, pk2 must have a name" )  
      return
   end if  
    
   allocate(lhs%cmp    (lhs%ncmpMax),                   stat = err) 
   allocate(lhs%usedCmp(lhs%ncmpMax), source = .false., stat = err)
   
   if (err /= 0) then
      call opflag%set (stat = IERROR, where = HERE, msg = 'Allocation failure')  
      call lhs%Destroy()
      return
   end if          

   lhs%ncmp            = 1
   lhs%name            = '' ! no structure name    
   lhs%cmp    (1)      = rhs
   lhs%cmp    (1)%name = rhs%name
   lhs%usedCmp(1)      = .true.
   
   END SUBROUTINE pk2Obj_AssignToPk2


!=============================================================================================
   SUBROUTINE pk2Obj_InsertAComponent ( self, compValue, compName )
!=============================================================================================
   class    (pk2obj_t),           intent(in out) :: self
   type     (pk2_t   ),           intent(in    ) :: compValue
   character(len=*   ), optional, intent(in    ) :: compName
!---------------------------------------------------------------------------------------------
!  Adds the pk2_t "compValue" as a new component of the structure "self". 
!
!  The name of this component is 
!  . "compName" if "compName" is present,
!  . "compValue%name" otherwise.
!
!  Notes:
!  . If a component already has this name, its value is overwritten by "compValue".
!  . If not, a free component # is sought among the "self%ncmp" available components.
!  . If no free component # is found, the total number of available components (self%ncmp) is 
!    increased by 1. 
!  . If this number is greater than "self%ncmpMax", "self%cmp" and "self%usedCmp" are resized 
!    with a maximum length (self%ncmpMax) increased by ADDCMP.
!-----------------------------------------------------------------------------------R.H. 05/19     

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2Obj_InsertComponent'
   integer  (Ikind)              :: i, icmp
   type     (str_t)              :: name
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! opflag = err_t ()
   
   if (.not. allocated(compValue%m)) return
   
   name%str = ''
   if (present(compName)) then
      name%str = compName
   else if (allocated(compValue%name)) then
      name%str = compValue%name
   end if
         
   if (len_trim(name%str) == 0) then   
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                        "The new component to be added has no name" )
      return
   end if   
   
   if (self%ncmp == 0) then
      self = pk2Obj_constructor1 ( [compValue], [name], '' )
      return
   end if
!
!- First see if "compName" is the name of a component of "self". 
!  If yes, replace its values and return.
!  If no, get a free # (1 <= icmp <= ncmp) where to insert this new component.
!   
   icmp = 0
   do i = 1, self%ncmp
      if (allocated(self%cmp(i)%name)) then
         if (self%cmp(i)%name == name%str) then
!
!-          this component has the same name: replace its value and return
!           
            self%cmp    (i)      = compValue
            self%cmp    (i)%name = name%str
            self%usedCmp(i)      = .true. 
            return
         end if
      end if
      if (icmp == 0 .and. .not. self%usedCmp(i)) icmp = i
   end do
      
   if (icmp /= 0) then
!
!-    Insert (compValue,compName) at the free component #icmp :
!
      self%cmp    (icmp)      = compValue
      self%cmp    (icmp)%name = name%str
      self%usedCmp(icmp)      = .true. 
      return
   end if
!
!- No free component, increase the number of components by 1 and if needed resize self%cmp(:)
!         
   icmp = self%ncmp + 1
      
   if (icmp > self%ncmpMax) then
!
!-    new maximum size:
!   
      self%ncmpMax = self%ncmpMax + ADDCMP
!
!-    resize self%cmp and self%usedCmp:
!
      call pk2_alloc1 ( 's', self%cmp, self%ncmpMax, opflag )
      if (opflag%code /= 0) return
      call util_alloc ( 's', self%usedCmp, self%ncmpMax, stat = opflag)
      if (opflag%code /= 0) return      
   end if   
!
!- Insert the component at position #icmp:
!
   self%cmp    (icmp)      = compValue
   self%cmp    (icmp)%name = name%str
   self%usedCmp(icmp)      = .true. 
!
!- New number of available component:
!
   self%ncmp = icmp   
      
   END SUBROUTINE pk2Obj_InsertAComponent


!=============================================================================================
   SUBROUTINE pk2Obj_InsertComponents ( self, compValues )
!=============================================================================================
   use util_Strings_m, only: util_OrdinalAbbrev
   class(pk2obj_t),           intent(in out) :: self
   type (pk2_t   ),           intent(in    ) :: compValues(:)
!---------------------------------------------------------------------------------------------
!  Adds the set of pk2_t variables "compValues(:)" as new components of the structure "self" 
!-----------------------------------------------------------------------------------R.H. 05/19     

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2Obj_InsertComponents'
   integer  (Ikind)              :: i, nadd, err
   type     (str_t), allocatable :: names(:)
   character(len=:), allocatable :: objname
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! opflag = err_t ()
   
   nadd = size(compValues)
   
   if (nadd == 0) return
   
   allocate(names(nadd))
   
   err = 0
   
   do i = 1, nadd
      if (allocated(compValues(i)%name)) then
         names(i)%str = compValues(i)%name
      else
         names(i)%str = ''
      end if
      if (len_trim(names(i)%str) == 0) then
         err = i
         exit
      end if   
   end do         
   
   if (err /= 0) then
      call opflag%set ( stat = UERROR, where = HERE, msg =  &
                       "The "// trim(util_OrdinalAbbrev(i))//" component has no name" )  
      return
   end if   
               
   if (self%ncmp == 0) then 
      objname = ''
      if (allocated(self%name)) objname = self%name
      if (len_trim(objname) == 0 .and. nadd > 1) then
         call opflag%set ( stat = UERROR, where = HERE, msg =  &
                          "A structure with more than one component must have a name" )
         return
      end if
      self = pk2Obj_constructor1 ( compValues, names, objname )
      return
   end if
   
   do i = 1, nadd
      call pk2Obj_InsertAComponent ( self, compValues(i), names(i)%str )
   end do
     
   END SUBROUTINE pk2Obj_InsertComponents

   
!=============================================================================================
   SUBROUTINE pk2Obj_RemoveAComponent ( self, nameComp )
!=============================================================================================
   class    (pk2obj_t), intent(in out) :: self
   character(len=*   ), intent(in    ) :: nameComp
!---------------------------------------------------------------------------------------------
!  Removes the component of name "nameComp" from the structure "self" 
!
!  Note: the freed space is left vacant (available for later insertion): the NUMBERING of the
!        remaining components is NOT AFFECTED.
!-----------------------------------------------------------------------------------R.H. 05/19     

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! opflag = err_t ()
   
   if (.not. allocated(self%cmp)) return
!
!- Find the component to remove:
!   
   do i = 1, self%nCmp
      if (allocated(self%cmp(i)%name)) then
         if (self%cmp(i)%name == trim(nameComp)) then
            call self%cmp(i)%Destroy() 
            self%usedCmp(i) = .false.
            exit
         end if   
      end if
   end do
   
   END SUBROUTINE pk2Obj_RemoveAComponent
         

!=============================================================================================
   SUBROUTINE pk2Obj_TransferTopk2 ( self, a )
!=============================================================================================
   class(pk2obj_t),              intent(in out) :: self
   type (pk2_t   ), allocatable, intent(in out) :: a(:)
!---------------------------------------------------------------------------------------------
!  Destroys "self" after transferring its components into a set of pk2 variables 
!
!  Note: The name of the i-th pk2 variable will be self%name // '.' // self%cmp(i)%name
!-----------------------------------------------------------------------------------R.H. 05/19     

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2Obj_TransferTopk2'
   integer  (Ikind)              :: i, err
   character(len=:), allocatable :: nameObj, nameCmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! opflag = err_t ()
      
   if (allocated(a)) deallocate(a) ; allocate(a(self%ncmp), stat = err)
   
   if (err /= 0) then
      call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure' )
      return
   end if   
   
   nameObj = ''
   
   if (allocated(self%name)) nameObj = (self%name) // '.'      
 
   do i = 1, self%ncmp
      nameCmp = (self%cmp(i)%name)
      call pk2_movealloc (from = self%cmp(i), to = a(i))
      a(i)%name = (nameObj) // (nameCmp)
   end do
   
   call self%Destroy()
   
   END SUBROUTINE pk2Obj_TransferTopk2
   

!=============================================================================================
   SUBROUTINE pk2Obj_Destroy ( self )
!=============================================================================================
   class(pk2obj_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------          
!   Destroy "self"
!-----------------------------------------------------------------------------------R.H. 05/19     
   
   self%ncmp = 0 ; self%constr = .false. ; self%ncmpMax = ADDCMP
   
   if (allocated(self%name   )) deallocate(self%name   )
   if (allocated(self%cmp    )) deallocate(self%cmp    )
   if (allocated(self%usedCmp)) deallocate(self%usedCmp)
        
   END SUBROUTINE pk2Obj_Destroy
         

!=============================================================================================
   SUBROUTINE pk2Obj_Print ( self, opt, unit )
!=============================================================================================
   class  (pk2obj_t),           intent(in) :: self
   integer(Ikind   ),           intent(in) :: opt
   integer(Ikind   ), optional, intent(in) :: unit
!---------------------------------------------------------------------------------------------          
!
!-----------------------------------------------------------------------------------R.H. 05/19     
   
!- local variables ---------------------------------------------------------------------------           
   integer   (Ikind)              :: i, lmax, uo, nused
   character(len=: ), allocatable :: name
   character(len=99)              :: sz, fmt, cnum
   character(len=13)              :: typ
   logical                        :: is_obj
!---------------------------------------------------------------------------------------------

   if (present(unit)) then
      uo = unit
   else
      uo = STDOUT
   end if    
   
   if (self%ncmp == 0 .and. .not. allocated(self%name)) then
      write(uo,*)'Empty structure'
      return   
   end if   
   
   name ='' ; if (allocated(self%name)) name = self%name
   
   if (len_trim(name) == 0 .and. self%ncmp > 1) then
      call opflag%set ( stat = UERROR, where = 'in pk2Obj_Print', &
                         msg = 'A structure with no name' )
      return
   end if   

   if (len_trim(name) == 0 .and. self%ncmp == 1) then
      is_obj = .false.
   else
      is_obj = .true.
   end if      

   lmax = 0 ; nused = 0
   do i = 1, self%ncmp
      if (allocated(self%cmp(i)%name)) lmax = max(lmax,len_trim(self%cmp(i)%name))
      if (self%usedCmp(i)) nused = nused + 1
   end do   
   lmax = lmax + 3
   write(fmt,'(a,i0,a)')'(a',lmax,',a)'   
      
   if (is_obj) then
      write(uo,'(/,a)') name //' (structure):'
      write(uo,'(a,i0)')'- Allocated with a max. number of components: ',self%ncmpMax
      write(uo,'(a,i0)')'- Largest index used                        : ',self%ncmp  
      write(uo,'(a,i0)')'- Number of components actually used        : ',nused     
   else
      write(uo,'(/,a)')'(pk2 variable):'
   end if
      
   if (opt == 1) then
      if (is_obj) then
         write(uo,'(/,a,i0,a)')'- List (values) of the ',nused,' used components:'                   
         do i = 1, self%ncmp
            if (self%usedCmp(i)) call self%cmp(i)%PrintMe(form = 'values', &
                                                          unit = unit    , &
                                                          msg  = '  '//self%cmp(i)%name//' = ')
         end do
      else
         if (self%usedCmp(1)) call self%cmp(1)%PrintMe ( form = 'values', unit = unit)
      end if   
   else
      write(uo,'(/,a,i0,a)')'- List (size and type) of the ',nused,' used components:'             
      do i = 1, self%ncmp
         if (.not. self%usedCmp(i)) cycle
         write(sz,'(i0,a,i0)')self%cmp(i)%nrow,' x ',self%cmp(i)%ncol
         if (self%cmp(i)%typ == EMPTY) then
            typ = ' (empty)'
         else if (self%cmp(i)%typ == ITYP) then
            typ = ' of integers'
         else if (self%cmp(i)%typ == RTYP) then
            typ = ' of reals'   
         else if (self%cmp(i)%typ == CTYP) then
            typ = ' of complexes'   
         else if (self%cmp(i)%typ == LTYP) then
            typ = ' of logicals'   
         else if (self%cmp(i)%typ == STYP) then
            typ = ' of strings'   
         else
            typ = ' (unknowns)'
         end if       
         
         name = ''
         if (allocated(self%cmp(i)%name)) name = self%cmp(i)%name

         write(cnum,'(a,i0,a)')'  (#',i,')'
         
         if (is_obj) then  
            write(uo,fmt)'  .'// name, ': ' // trim(sz) // typ // trim(cnum)
         else
            write(uo,*)
            write(uo,'(a)') name// ': ' // trim(sz) // trim(typ) // trim(cnum)
         end if   
      end do
   end if          
     
   END SUBROUTINE pk2Obj_Print


!=============================================================================================
   SUBROUTINE pk2Obj_alloc1 ( mode, t, n, stat )
!=============================================================================================
   character(len=1   ),              intent(in    ) :: mode
   integer  (Ikind   ),              intent(in    ) :: n
   type     (pk2obj_t), allocatable, intent(in out) :: t(:)
   type     (err_t   ), optional   , intent(   out) :: stat
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the 1d pk2obj_t array t. It is specially useful   
!  when t is already allocated but needs to be extended.
! 
! * If mode = 's' (s for 'save'), t is saved in a temporary array, de-allocated and re-
!   allocated with the new size (n). The saved entries are then copied back to t.
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
!      t: array of pk2obj_t to be allocated or re-allocated with the new size n
!
!-----------------------------------------------------------------------------------R.H. 05/19
 
!- local variables ---------------------------------------------------------------------------  
   character(len=*   ), parameter   :: HERE = 'pk2Obj_alloc1'
   integer  (Ikind   )              :: nn, err
   type     (pk2obj_t), allocatable :: tmp(:)
   type     (err_t   )              :: flag
!--------------------------------------------------------------------------------------------- 

   flag = err_t ()
   
   if (present(stat)) stat = flag

   if (.not. allocated(t)) then

      allocate(t(n), stat=err)
      if (err /= 0) &
           call flag%set (where = HERE, msg = "Allocation failure for array t", stat = IERROR) 

   else if (mode == 'e') then

      if (allocated(t)) deallocate(t)
      allocate(t(n), stat=err)
      if (err /= 0) &
           call flag%set (where = HERE, msg = "Allocation failure for array t", stat = IERROR ) 
           
   else if (mode == 's') then                      
   
      allocate(tmp(n), stat=err)
      if (err /= 0) then
         call flag%set (where = HERE, msg = "Allocation failure for array tmp", stat = IERROR) 
      else   
                                         
         nn = min(n,size(t,kind=Ikind))
         tmp(1:nn) = t(1:nn)
      
         call move_alloc (from = tmp, to = t)
      end if   
         
   else
      call flag%set ( stat = UERROR,  where = HERE, msg = &
                     "Invalid mode << "//mode//" >> (must be << e >> or << s >>)" ) 
   end if 
   
   if (present(stat)) then
      stat = flag
      return
   else
      if (flag%code > 0) call flag%display(abort=.true.)
   end if      
         
   END SUBROUTINE pk2Obj_alloc1

         
END MODULE pk2Obj_m