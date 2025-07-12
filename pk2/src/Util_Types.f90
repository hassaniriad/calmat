! No more used
!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: Util_Types
!
! Description: defines usefull derived types
!---------------------------------------------------------------------------------------------

MODULE Util_Types_m

   use KindParameters_m, only: Ikind

   implicit none
   
   private
   public :: str_t, err_t, err_SetHaltingMode, err_GetHaltingMode
!
!- A usefull type to define a list of strings of different lengths:
!   
   type :: str_t
      character(len=:), allocatable :: str
   contains
      procedure, pass(lhs) :: util_types_StrEqStr       ! str_t = str_t
      procedure, pass(lhs) :: util_types_StrEqChar      ! str_t = char
      procedure, pass(rhs) :: util_types_allocCharEqStr ! allocchar  = str_t
      
      generic, public :: assignment (=) => util_types_StrEqStr, util_types_StrEqChar, &
                                           util_types_allocCharEqStr
      !final :: util_FinalizeStr
   end type str_t
!
!- For passing message error or warning
!
   type :: err_t
      integer  (Ikind)              :: code = 0 
      character(len=:), allocatable :: mesg
   contains
      procedure :: Display => err_DisplayErr   
      procedure :: Destroy => err_DestroyErr
      !final :: util_FinalizeErr
   end type err_t   

   interface err_t
      module procedure err_SetErr
   end interface
!
!- If "ErrAbort" is set to .true. by the caller, any encountered error in any procedure will
!  immediately cause the stop of execution after a message printing on the standard output.
!  Otherwise, if "ErrAbort" is set to .false., it is the responsability of the caller to test
!  the error code returned by this procedure.
!
   logical :: err_Halting = .true.

CONTAINS

!=============================================================================================
   SUBROUTINE err_SetHaltingMode ( halting )
!=============================================================================================   
   logical, intent(in) :: halting
!---------------------------------------------------------------------------------------------   
   
   err_Halting = halting
   
   END SUBROUTINE err_SetHaltingMode


!=============================================================================================
   FUNCTION err_GetHaltingMode ( ) result ( halting )
!=============================================================================================   
   logical :: halting
!---------------------------------------------------------------------------------------------   
   
   halting = err_Halting 
   
   END FUNCTION err_GetHaltingMode


!=============================================================================================
   SUBROUTINE util_types_StrEqStr ( lhs, rhs )
!=============================================================================================
   class(str_t), intent(   out) :: lhs
   type (str_t), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 10/19       
   
   if (allocated(rhs%str)) lhs%str = (rhs%str)
   
   END SUBROUTINE util_types_StrEqStr 


!=============================================================================================
   SUBROUTINE util_types_StrEqChar ( lhs, rhs )
!=============================================================================================
   class    (str_t), intent(   out) :: lhs
   character(len=*), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 10/19       
   
   lhs%str = rhs
   
   END SUBROUTINE util_types_StrEqChar 


!=============================================================================================
   SUBROUTINE util_types_allocCharEqStr ( lhs, rhs )
!=============================================================================================
   character(len=:), allocatable, intent(   out) :: lhs
   class    (str_t),              intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 10/19       
   
   if (allocated(rhs%str)) then
      lhs = (rhs%str)
   else
      lhs = ''
   end if      
   
   END SUBROUTINE util_types_allocCharEqStr 


!=============================================================================================
   SUBROUTINE util_FinalizeStr ( self )
!=============================================================================================
   type(str_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Finalizer
!---------------------------------------------------------------------------------------------
        
   if (allocated(self%str)) deallocate(self%str)
     
   end SUBROUTINE util_FinalizeStr


!=============================================================================================
   FUNCTION err_SetErr ( msg, stat ) result ( res )
!=============================================================================================
   character(len=*), optional, intent(in) :: msg
   integer  (Ikind), optional, intent(in) :: stat
   type     (err_t)                       :: res
!---------------------------------------------------------------------------------------------
!  Constructor for err_t type
!
!  . if "msg" is present, set res%mesg = msg, else res%msg remains unallocated
!
!  . if "stat" is present, set res%code = stat, else set res%code = 0            
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   logical :: halting
!---------------------------------------------------------------------------------------------
   
   res%code = 0
   
   if (present(msg)) res%mesg = trim(msg)

   if (present(stat)) res%code = stat
   
   if (res%code > 0) then
      halting = err_GetHaltingMode()
      if (halting) call err_DisplayErr ( res, abort = halting )
   end if
   
   END FUNCTION err_SetErr


!=============================================================================================
   SUBROUTINE err_DisplayErr ( self, unit, verb, delete, abort, title ) 
!=============================================================================================
   use Constants_m, only: STDOUT
   class    (err_t),           intent(in out) :: self
   integer  (Ikind), optional, intent(in    ) :: unit
   integer  (Ikind), optional, intent(in    ) :: verb
   logical         , optional, intent(in    ) :: delete, abort
   character(len=*), optional, intent(in    ) :: title
!---------------------------------------------------------------------------------------------
!  Prints self
!
!  By convention: 
!  . self%code = 0 means no error and no warning
!  . self%code < 0 corresponds to a warning
!  . self%code = 1 corresponds to an internal error
!  . sefl%code > 1 corresponds to a user error
!
!  Optional inputs:
!
!  . unit  : the logical unit of the output file. If not present, STDOUT is used.
!
!  . verb  : if self%code = 0 and self%mesg is not allocated (or is empty), nothing is printed
!            unless "verb" is present and positive and in this case the message 
!                         'no error, no warning, no message'
!            is printed.
!
!  . delete: by default "self" is deleted unless "delete" is present and equal to .false.  
!
!  . abort : when present and equal to .true., the run is stopped after the print if self%code
!            corresponds to an error (self%code > 0).
!            When not present, GetHaltingMode is used.
!
!  . title : a title to print before the warning/error message.
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   integer  (Ikind)              :: un, errcode
   character(len=:), allocatable :: titre   
   logical                       :: nomsg, del, halting
!---------------------------------------------------------------------------------------------

   un    = STDOUT ; if (present(unit)  ) un    = unit
   del   = .true. ; if (present(delete)) del   = delete
   titre = ' '    ; if (present(title) ) titre = title
      
   nomsg = .true.
   if (allocated(self%mesg)) then
      if (len_trim(self%mesg) /= 0) nomsg = .false.
   end if            
   
   errcode = self%code
      
   select case (errcode)
      case (0)
         if (nomsg) then
            if (present(verb)) then
               if (verb > 0) then
                  write(un,'(/,a)')titre
                  write(un,'(a,/)')'--> no error, no warning, no message'
               end if   
            end if   
         else   
            write(un,'(/,a)')titre
            write(un,'(a,/)')'--> message: '//trim(self%mesg)
         end if

      case (:-1)
         if (nomsg) then
            write(un,'(/,a)')titre
            write(un,'(a,/)')'--> Warning (with no message)'
         else
            write(un,'(/,a)')titre
            write(un,'(a,/)')'--> Warning: '//trim(self%mesg)
         end if

      case (1:)
         if (nomsg) then
            if (errcode == 1) then
               write(un,'(/,a)')titre
               write(un,'(a,/)')'--> Internal error (with no message)'
            else
               write(un,'(/,a)')titre
               write(un,'(a,/)')'--> Error (with no message)'
            end if
         else
            if (errcode == 1) then
               write(un,'(/,a)')titre
               write(un,'(a,/)')'--> Internal error: '//trim(self%mesg)
            else
               write(un,'(/,a)')titre
               write(un,'(a,/)')'--> Error: '//trim(self%mesg)
            end if
         end if
                        
   end select
   
   if (del) call self%Destroy()

   if (errcode > 0) then
      if (present(abort)) then
         halting = abort
      else
         halting = err_GetHaltingMode()
      end if
      if (halting) then
         write(un,'(a,/)') '--> ABORT (requested)'
         stop
      end if
   end if
   
   END SUBROUTINE err_DisplayErr
   
   
!=============================================================================================
   SUBROUTINE err_DestroyErr ( self ) 
!=============================================================================================
   class(err_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Destroys self
!---------------------------------------------------------------------------------------------

   if (allocated(self%mesg)) deallocate(self%mesg) ; self%code = 0

   END SUBROUTINE err_DestroyErr 


!=============================================================================================
   SUBROUTINE err_FinalizeErr ( self ) 
!=============================================================================================
   type(err_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Finalizer
!---------------------------------------------------------------------------------------------

   if (allocated(self%mesg)) deallocate(self%mesg)

   END SUBROUTINE err_FinalizeErr 

   
END MODULE Util_Types_m
