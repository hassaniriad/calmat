! Copyright 2018-2024  Riad Hassani, Universite Cote d'Azur
!
! This file is part of pk2 library.
!
! The  pk2 library  is free software: you can redistribute it and/or modify it under the terms
! of  the  GNU  General  Public  License  as published by the Free Software Foundation, either 
! version 3 of the License, or (at your option) any later version.
!
! The pk2 library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
! without  even the implied warranty of  MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE. 
! See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with the pk2 library.
! If not, see <https://www.gnu.org/licenses/>. 

!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: err
!
! Description: defines a usefull type for error handling
!---------------------------------------------------------------------------------------------

MODULE err_m

   use, intrinsic :: iso_fortran_env, only: output_unit
   
   use ansiColor_m,      only: ansiColor_start   
   use kindParameters_m, only: Ikind
   use pk2constants_m  , only: IERROR, WARNING, IONE, NLT
   
   implicit none
   
   private
   public :: err_t, err_SetHaltingMode, err_GetHaltingMode, err_GetWarningMode, &
             err_MoveAlloc, err_ncall, err_colorError, err_colorWarning, err_colorNormal, &
             err_resetHaltingMode
!
!- A DT defining a flag (for passing message error or warning)
!
   type :: err_t
   
      integer  (Ikind)              :: code = 0
      
      character(len=:), allocatable :: mesg,  &
                                       trace, &
                                       loc
                                       
      logical                       :: has_mesg  = .false., &
                                       has_trace = .false., &
                                       has_loc   = .false.                                       
   contains
   
      procedure :: Set      => err_Set
      procedure :: Display  => err_DisplayErr 
      procedure :: Destroy  => err_DestroyErr 
      procedure :: AddTrace => err_AddTrace 
      procedure :: AddMsg   => err_AddMsg 
      !final :: err_FinalizeErr            
!
!-    Operator overload:
!      
      procedure, pass(a) :: err_Gt
      procedure, pass(a) :: err_GtIntg
      procedure, pass(a) :: err_IntgGt
      generic, public :: operator(>) => err_Gt, err_GtIntg, err_IntgGt 

      procedure, pass(a) :: err_Ge
      procedure, pass(a) :: err_GeIntg
      procedure, pass(a) :: err_IntgGe
      generic, public :: operator(>=) => err_Ge, err_GeIntg, err_IntgGe

      procedure, pass(a) :: err_Lt
      procedure, pass(a) :: err_LtIntg
      procedure, pass(a) :: err_IntgLt
      generic, public :: operator(<) => err_Lt, err_LtIntg, err_IntgLt

      procedure, pass(a) :: err_Le
      procedure, pass(a) :: err_LeIntg
      procedure, pass(a) :: err_IntgLe
      generic, public :: operator(<=) => err_Le, err_LeIntg, err_IntgLe

      procedure, pass(a) :: err_Eq
      procedure, pass(a) :: err_EqIntg
      procedure, pass(a) :: err_IntgEq
      generic, public :: operator(==) => err_Eq, err_EqIntg, err_IntgEq

      procedure, pass(a) :: err_Ne
      procedure, pass(a) :: err_NeIntg
      procedure, pass(a) :: err_IntgNe
      generic, public :: operator(/=) => err_Ne, err_NeIntg, err_IntgNe            
      
   end type err_t 
!
!- Constructor
!
   interface err_t
      module procedure err_constructor
   end interface
!
!- The default values of the following variables controlling the handler mode can be modified 
!  by the user by calling the "err_SetHaltingMode" subroutine. 
!
!  . err_Halting : controls halting or continuation after an error. 
!                  If set to .true., the error will cause halting as soon as err_t is 
!                  invoqued to signal this error.
!                  Otherwise, it is the caller responsability to test the returned error code.
!
!  . err_IeeeHalt: controls halting or continuation after an ieee exception (default = false)
!
!  . err_DispWarn: if this variable is set to .false. warning messages are not printed when
!                  the flag is displayed.
!
   logical          :: err_Halting  = .true., err_saveHalting =.true.
   logical          :: err_IeeeHalt = .false.      
   logical          :: err_DispWarn = .true.    
!
!- The following variables can also be modified by calling the same subroutine: 
! 
!  . err_Unit: unit used to print error and warning message.
!
!  . err_colorError, err_colorWarning, err_colorNormal: colors of the corresponding messages
!    (on stdout)
!
   integer  (Ikind) :: err_Unit = output_unit

   character(len=9) :: err_colorError   = ansiColor_start // '1;91m', & !'r_bh', &
                       err_colorWarning = ansiColor_start // '1;92m', & !'g_bh', &
                       err_colorNormal  = ansiColor_start // '1;94m'    !'b_bh'
                          
   integer(Ikind) :: err_ncall(2) = 0
!
!- Operator overload:
!
   !interface operator(==)    
   !   module procedure err_Eq, err_EqIntg, err_IntgEq 
   !end interface

   !interface operator(/=)    
   !   module procedure err_Ne, err_NeIntg, err_IntgNe
   !end interface

   !interface operator(>)    
   !   module procedure err_Gt, err_GtIntg, err_IntgGt 
   !end interface

   !interface operator(>=)    
   !   module procedure err_Ge, err_GeIntg, err_IntgGe
   !end interface

   !interface operator(<)    
   !   module procedure err_Lt, err_LtIntg, err_IntgLt 
   !end interface

   !interface operator(<=)    
   !   module procedure err_Le, err_LeIntg, err_IntgLe
   !end interface
     

CONTAINS

!=============================================================================================
   SUBROUTINE err_SetHaltingMode ( halting, ieee_halting, DisplayWarning, unit, &
                                   errorColor, warnColor, normalColor           )
!=============================================================================================
   use, intrinsic :: ieee_exceptions, only: ieee_set_halting_mode, ieee_support_halting, &
                     ieee_underflow, ieee_overflow, ieee_divide_by_zero, ieee_invalid
   use ansiColor_m, only: ansiColor_getAnsiCode   
   logical         , optional, intent(in) :: halting, ieee_halting, DisplayWarning
   integer  (Ikind), optional, intent(in) :: unit
   character(len=*), optional, intent(in) :: errorColor, warnColor, normalColor
!---------------------------------------------------------------------------------------------
!  This subroutine can be called to modify the halting mode, to control the printing of 
!  warning messages, the unit used to print messages and the corresponding colors. 
!
!  Inputs:
!
!  . halting       : (optional) When present, the given value overwrite the default value of 
!                    the module variable "err_Halting". 
!                    When this value is .true. any invocation of the err_t constructor with
!                    %code > 0 will cause halting. 
!
!  . ieee_halting  : (optional) When present and .true., any ieee exception will cause halting. 
!                    When not present "err_halting" is used.
!
!  . DisplayWarning: (optional) When present, the given value overwrite the default value of 
!                    the module variable "err_DispWarn".
!                    When this value is .true. any warning message (%code < 0) will be printed
!                    when %display is invocated.
!
!  . unit          : (optional) The unit used to print out error and warning message.
!                    When present, the given value overwrite the default value of the module
!                    variable "err_Unit".
!                    
!  . errorColor, warnColor, normalColor : color codes for error, warning and normal messages
!                    (when stdout or sdterr are used).
!                    Theses codes can be ansi codes or aliases in the form: 'c[t]' where c in 
!                    {'k', 'r', 'b', 'y', 'g', 'c', 'm'} (for black, red, blue, yellow, green, 
!                    cyan, magenta) is the color and t in {'b', 'h', 'bh'} (for bold, high,
!                    bold and high) is the texture and/or the intensity. Example : rbh
!                    
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------
   character(len=9) :: color
   logical          :: ieee_halting_
!---------------------------------------------------------------------------------------------
   
   if ( present(halting) ) then
      err_Halting     = halting
      err_saveHalting = halting
   end if
   
   if ( present(DisplayWarning) ) err_DispWarn = DisplayWarning
   
   if ( present(unit) ) then
      if ( unit /= 5 ) err_Unit = unit
   end if

   if ( present(ieee_halting) ) then
      ieee_halting_ = ieee_halting
   else
      ieee_halting_ = err_IeeeHalt
   end if

!  cannot use the following on MacOS/Silicon (2025):
!   if ( present(ieee_halting) ) then   
!      call ieee_set_halting_mode (ieee_all , halting = ieee_halting) 
!   else
!      call ieee_set_halting_mode (ieee_all , halting = err_IeeeHalt) 
!   end if
!  workaround: verify first if it's supported:
!! 01/2026: Unfortunately with flang I get: undefined symbols for architecture arm64:"_fedisableexcept"
!!   if ( ieee_support_halting(ieee_underflow) ) &
!!      call ieee_set_halting_mode ( ieee_underflow, halting = ieee_halting_ )
!!   if ( ieee_support_halting(ieee_overflow) ) &
!!      call ieee_set_halting_mode ( ieee_overflow, halting = ieee_halting_ )
!!   if ( ieee_support_halting(ieee_divide_by_zero) ) &
!!      call ieee_set_halting_mode ( ieee_divide_by_zero, halting = ieee_halting_ )
!!   if ( ieee_support_halting(ieee_invalid) ) &
!!      call ieee_set_halting_mode ( ieee_invalid, halting = ieee_halting_ )
   
   if ( present(errorColor) ) then
      color = errorColor
      if ( color(1:2) /= ansiColor_start ) color = ansiColor_getAnsiCode ( color )
      if ( len_trim(color) > 0 ) err_colorError = color
   end if

   if ( present(warnColor) ) then
      color = warnColor
      if ( color(1:2) /= ansiColor_start ) color = ansiColor_getAnsiCode ( color )
      if ( len_trim(color) > 0 ) err_colorWarning = color
   end if
   
   if ( present(normalColor) ) then
      color = normalColor
      if ( color(1:2) /= ansiColor_start ) color = ansiColor_getAnsiCode ( color )
      if ( len_trim(color) > 0 ) err_colorNormal = color
   end if
      
   END SUBROUTINE err_SetHaltingMode


!=============================================================================================
   pure FUNCTION err_GetHaltingMode ( ) result ( halting )
!=============================================================================================   
   logical :: halting
!---------------------------------------------------------------------------------------------   
   
   halting = err_Halting 
   
   END FUNCTION err_GetHaltingMode
   

!=============================================================================================
   pure FUNCTION err_GetWarningMode ( ) result ( warning )
!=============================================================================================   
   logical :: warning
!---------------------------------------------------------------------------------------------   
   
   warning = err_DispWarn 
   
   END FUNCTION err_GetWarningMode
   
   
!=============================================================================================
   FUNCTION err_constructor ( stat, where, msg ) result ( res )
!=============================================================================================
   integer  (Ikind), optional, intent(in) :: stat
   character(len=*), optional, intent(in) :: msg, where
   type     (err_t)                       :: res
!---------------------------------------------------------------------------------------------
!  Constructor
!---------------------------------------------------------------------------------------------

   call err_set ( res, stat, where, msg ) 
   
   END FUNCTION err_constructor


!=============================================================================================
   SUBROUTINE err_set ( self, stat, where, msg ) 
!=============================================================================================
   class    (err_t),           intent(   out) :: self
   integer  (Ikind), optional, intent(in    ) :: stat
   character(len=*), optional, intent(in    ) :: msg, where
!---------------------------------------------------------------------------------------------
!  Setter
!
!  . if "msg" is present, set self%mesg = msg and self%has_mesg = .true.
!
!  . if "stat" is present, set self%code = stat, else set res%code = 0  
!
!  . if "where" is present, set sel%loc = where and self%has_loc = .true.       
!---------------------------------------------------------------------------------------------

   if ( present(stat) ) self%code = stat
   
   if ( present(msg) ) then
      self%mesg = trim(msg)
      if ( len_trim(msg) > 0 ) self%has_mesg = .true.
   end if
      
   if ( present(where) ) then
      self%loc = trim(where)   
      if ( len_trim(where) > 0 ) self%has_loc = .true.
   end if         

   if ( self%code > 0 ) then
      if ( err_halting ) call err_DisplayErr ( self, unit = err_unit )
      err_ncall(1) = err_ncall(1) + 1
   else if ( self%code == WARNING ) then
      if ( err_DispWarn ) call err_DisplayErr ( self, unit = err_unit )
      err_ncall(2) = err_ncall(2) + 1
   end if
   
   END SUBROUTINE err_set
   
   
!=============================================================================================
   pure SUBROUTINE err_AddTrace ( self, trace ) 
!=============================================================================================
   class    (err_t), intent(in out) :: self
   character(len=*), intent(in    ) :: trace
!---------------------------------------------------------------------------------------------
!  Sets self%trace = trace and self_has_trace = .true.
!---------------------------------------------------------------------------------------------

   if ( len_trim(trace) > 0 ) then
      if ( self%has_trace ) then
         self%trace = self%trace // ' <<- ' // trim(adjustl(trace))
      else
         self%trace = trim(adjustl(trace))
         self%has_trace = .true.
      end if
   end if
   
   END SUBROUTINE err_AddTrace
   

!=============================================================================================
   pure SUBROUTINE err_AddMsg ( self, msg, before, newline ) 
!=============================================================================================
   class    (err_t),           intent(in out) :: self
   character(len=*),           intent(in    ) :: msg
   logical         , optional, intent(in    ) :: before, newline
!---------------------------------------------------------------------------------------------
!  Adds a message
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   logical :: bef, newl
!---------------------------------------------------------------------------------------------

   if ( present(before) ) then
      bef = before
   else
      bef = .false.
   end if
   if ( present(newline) ) then
      newl = newline
   else
      newl = .false.
   end if
   
   if ( len_trim(msg) > 0 ) then
      if ( self%has_mesg ) then
         if ( newl ) then
            if ( bef ) then
               self%mesg = trim(msg) // NLT // self%mesg
            else
               self%mesg = self%mesg // NLT // trim(msg)
            end if
         else
            if ( bef ) then
               self%mesg = msg // self%mesg
            else
               self%mesg = self%mesg // msg
            end if
         end if         
      else
         self%mesg = trim(msg)
         self%has_mesg = .true.
      end if
   end if
   
   END SUBROUTINE err_AddMsg   
   
   
!=============================================================================================
   SUBROUTINE err_DisplayErr ( self, unit, verb, delete, abort, title, trace ) 
!=============================================================================================
   use ansiColor_m, color => ansiColor_colorTxt
   class    (err_t),           intent(in out) :: self
   integer  (Ikind), optional, intent(in    ) :: unit
   integer  (Ikind), optional, intent(in    ) :: verb
   logical         , optional, intent(in    ) :: delete, abort, trace
   character(len=*), optional, intent(in    ) :: title
!---------------------------------------------------------------------------------------------
!  Prints self
!
!  By convention: 
!  . self%code = 0 means no error and no warning
!  . self%code < 0 corresponds to a warning
!  . self%code = IERROR (>0) corresponds to an internal error
!  . sefl%code = UERROR (>0) corresponds to a user error
!
!  Optional inputs:
!
!  . unit  : the logical unit of the output file. If not present, the value of the module
!            variable "err_unit" is used.
!
!  . verb  : if verb is present and
!            . verb <= 0: NOTHING IS PRINTED AND SELF IS NOT DELETED
!            . verb > 0 and self%code /= 0 the error or warning message is printed               
!            . moreover if self%code = 0 and self%mesg is not allocated the message
!              'no error, no warning, no message' is printed if verb > 3  
!            if verb is not present, the default value 1 is used
!
!  . delete: by default "self" is deleted unless "delete" is present and equal to .false.  
!
!  . abort : when present and equal to .true., the run is stopped after the print if self%code
!            corresponds to an error (self%code > 0).
!            When not present, err_halting is used.
!
!  . title : a title to print before the warning/error message.
!
!  . trace : when present and equal to .true. print the traceback (self%trace) if any.
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   integer  (Ikind)              :: un, errcode, verbose
   character(len=:), allocatable :: titre   
   logical                       :: nomsg, del, traceback
   logical         , save        :: firstTime = .true.
   character(len=9), save        :: colorErr, colorWarn, colorOk
!---------------------------------------------------------------------------------------------

   if ( firstTime .or. colorErr /= err_colorError ) then
      if ( err_colorError(1:2) /= ansiColor_start ) &
         err_colorError = ansiColor_getAnsiCode ( err_colorError )
      colorErr = err_colorError
   end if

   if ( firstTime .or. colorWarn /= err_colorWarning ) then
      if ( err_colorWarning(1:2) /= ansiColor_start ) &
         err_colorWarning = ansiColor_getAnsiCode ( err_colorWarning )
      colorWarn = err_colorWarning
   end if

   if ( firstTime .or. colorOk /= err_colorNormal ) then
      if ( err_colorNormal(1:2) /= ansiColor_start ) &
         err_colorNormal = ansiColor_getAnsiCode ( err_colorNormal ) 
      colorOk = err_colorNormal
   end if

   firstTime = .false.
      
   if ( present(verb) ) then
      if ( verb <= 0 ) return ! nothing to do
      verbose = verb
   else
      verbose = IONE
   end if

   un        = err_Unit ; if ( present(unit)   ) un        = unit
   del       = .true.   ; if ( present(delete) ) del       = delete
   titre     = ''       ; if ( present(title)  ) titre     = title
   traceback = .true.   ; if ( present(trace)  ) traceback = trace
   
   nomsg = .true.
   if ( allocated(self%mesg) ) then
      if ( len_trim(self%mesg) /= 0 ) nomsg = .false.
   end if        

   if ( self%has_loc ) then
      if ( .not. allocated(self%loc) ) then
         self%has_loc = .false.
      else
         if ( len_trim(self%loc) == 0 ) self%has_loc = .false.
      end if
   end if 
      
   if ( self%has_trace ) then
      if ( .not. allocated(self%trace) ) then
         self%has_trace = .false.
      else
         if ( len_trim(self%trace) == 0 ) self%has_trace = .false.
      end if
   end if 
   
   errcode = self%code
      
   select case ( errcode )
      case ( 0 ) ! no error
         if ( nomsg ) then
            if ( verbose > 3 ) then
               if ( present(title) ) write(un,'(/,a)') color( int(un),colorOk,titre )
               write(un,'(a,/)')color( int(un),colorOk,'--> no error, no warning, no message' )
            end if   
         else   
            if ( present(title) ) write(un,'(/,a)')color(int(un),colorOk,titre)
            write(un,'(a,/)')color(int(un),colorOk,'--> message: '//trim(self%mesg))
         end if

      case ( :-1 ) ! warning or information 
         !if (err_DispWarn) then
            if ( nomsg ) then
               if ( present(title) ) write(un,'(/,a)')color(int(un),colorWarn,titre)
               if ( errcode == WARNING ) &
                  write(un,'(a,/)')color(int(un),colorWarn,'--> Warning (with no message)')
            else
               if ( present(title) ) write(un,'(/,a)')color(int(un),colorWarn,titre)
               if ( errcode == WARNING ) then
                  write(un,'(a)')color(int(un),colorWarn,'--> Warning: '//trim(self%mesg))
               else
                  write(un,'(a)')color(int(un),colorWarn,'--> '//trim(self%mesg))
               end if
               if ( self%has_loc ) &
                  write(un,'(a)')color(int(un),colorWarn,'--> Returned by: '//trim(self%loc))
               if ( self%has_trace .and. traceback ) &
                  call err_printTrace ( int(un), colorErr, self%trace, lineMaxLength=80 )
                  !write(un,'(a)')color(int(un),colorWarn,'--> Traceback: '//trim(self%trace))
               write(un,'(a)')' '   
            end if
         !end if

      case ( 1: ) ! error
         if ( nomsg ) then
            if ( errcode == IERROR ) then
               if ( present(title) ) write(un,'(/,a)') color(int(un),colorErr,titre)
               write(un,'(a,/)') color(int(un),colorErr,'--> Internal error (with no message)')
            else
               if ( present(title) ) write(un,'(/,a)') color(int(un),colorErr,titre)
               write(un,'(a,/)') color(int(un),colorErr,'--> Error (with no message)')
            end if
         else
            if ( errcode == IERROR ) then
               if ( present(title) ) write(un,'(/,a)') color(int(un),colorErr,titre)
               write(un,'(a)') color(int(un),colorErr,'--> Internal error: '//trim(self%mesg))
            else
               if ( present(title) ) write(un,'(/,a)') color(int(un),colorErr,titre)
               write(un,'(a)') color(int(un),colorErr,'--> Error: '// trim(self%mesg)) 
            end if
            if ( self%has_loc ) &
               write(un,'(a)') color(int(un),colorErr,'--> Returned by: '//trim(self%loc)) 
            if ( self%has_trace .and. traceback ) &
               !call err_printTrace ( int(un), colorErr, self%trace, lineMaxLength=80 )
               write(un,'(a)') color(int(un),colorErr,'--> Traceback: '//trim(self%trace)) 
            write(un,'(a)')' '   
         end if
   end select
   
   if ( del ) call self%Destroy()

   if ( errcode > 0 ) then
      if ( present(abort) ) then
         if ( abort ) then
            write(un,'(a,/)') color(int(un),colorErr,'--> ABORT (requested)')
            error stop
         end if
      else if ( err_halting ) then
         write(un,'(a,/)') color(int(un),colorErr,'--> ABORT (Halting Mode enabled)')
         error stop
      end if
   end if
   
   END SUBROUTINE err_DisplayErr
   

!=============================================================================================
   SUBROUTINE err_printTrace ( un, col, trace, lineMaxLength )
!=============================================================================================
   use ansiColor_m, color => ansiColor_colorTxt
   integer         , intent(in) :: un, lineMaxLength
   character(len=*), intent(in) :: col, trace
!---------------------------------------------------------------------------------------------
! à revoir, ne marche pas toujours bien
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: msg1  = '--> Traceback: ', msg2 = repeat(' ',len(msg1))
   integer  (Ikind)              :: n, npos, i, j, istart
   character(len=:), allocatable :: msg, str, str0, split(:)
   integer  (Ikind), allocatable :: pos(:)
!---------------------------------------------------------------------------------------------

   n = len(trace) + len(msg1)
   if ( n < lineMaxLength ) then
      write(un,'(a)')  color( un, col, msg1 // trace )
   else
      call err_FindSubstring1 ( loc=pos, n=npos, str=trace, substr='<<-' )   
      allocate(character(len=n)::split(npos+1))
      if ( npos == 0 ) then
         split(1) = trace
      else
         split(1) = trim(adjustl(trace(:pos(1)-1))) // ' <<- '
      end if
      do i = 2, npos
         split(i) = trim(adjustl(trace(pos(i-1)+3:pos(i)-1))) // ' <<- '
      end do
      split(npos+1) = trim(adjustl(trace(pos(npos)+3:)))
  
      msg = msg1 ; istart = 1
      do while ( istart <= npos+1 )
         str0 = msg ; str = msg
         do i = istart, npos+1
            str = str // trim(split(i)) // ' '
            if ( len(str) > lineMaxLength ) then
               if ( i == istart ) str0 = str
               j = i+1
               exit
            else
               str0 = str
               j = i+1
            end if
         end do
         write(un,'(a)') color( un, col, str0 ) 
         msg = msg2
         istart = j
      end do   
         
   end if
   
   END SUBROUTINE err_printTrace
   

!=============================================================================================
   pure SUBROUTINE err_MoveAlloc ( from, to ) 
!=============================================================================================
   type(err_t), intent(in out) :: from, to
!---------------------------------------------------------------------------------------------
!  Move from "from" to "to"
!---------------------------------------------------------------------------------------------
      
   to%code      = from%code      ; from%code      = 0 
   to%has_trace = from%has_trace ; from%has_trace = .false.
   to%has_loc   = from%has_loc   ; from%has_loc   = .false.
   to%has_mesg  = from%has_mesg  ; from%has_mesg  = .false.
   
   if ( allocated(from%mesg ) ) call move_alloc (from = from%mesg , to = to%mesg )
   if ( allocated(from%trace) ) call move_alloc (from = from%trace, to = to%trace)
   if ( allocated(from%loc  ) ) call move_alloc (from = from%loc  , to = to%loc  )

   END SUBROUTINE err_MoveAlloc 

   
!=============================================================================================
   pure SUBROUTINE err_DestroyErr ( self ) 
!=============================================================================================
   class(err_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Destroys self
!---------------------------------------------------------------------------------------------

   self%code = 0
   if ( allocated(self%mesg ) ) deallocate(self%mesg ) ; self%has_mesg  = .false.
   if ( allocated(self%trace) ) deallocate(self%trace) ; self%has_trace = .false.
   if ( allocated(self%loc  ) ) deallocate(self%loc  ) ; self%has_loc   = .false.

   END SUBROUTINE err_DestroyErr 


!=============================================================================================
   pure SUBROUTINE err_FinalizeErr ( self ) 
!=============================================================================================
   type(err_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Finalizer
!---------------------------------------------------------------------------------------------

   self%code = 0
   if ( allocated(self%mesg ) ) deallocate(self%mesg ) ; self%has_mesg  = .false.
   if ( allocated(self%trace) ) deallocate(self%trace) ; self%has_trace = .false.
   if ( allocated(self%loc  ) ) deallocate(self%loc  ) ; self%has_loc   = .false.

   END SUBROUTINE err_FinalizeErr 


!=============================================================================================
   pure FUNCTION err_Eq ( a, b ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a, b
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code == b%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code == b%code)
   
   END FUNCTION err_Eq
   

!=============================================================================================
   pure FUNCTION err_EqIntg ( a, i ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code == i
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code == i)
   
   END FUNCTION err_EqIntg


!=============================================================================================
   pure FUNCTION err_IntgEq ( i, a ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code == i
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code == i)
      
   END FUNCTION err_IntgEq


!=============================================================================================
   pure FUNCTION err_Ne ( a, b ) result( res )
!=============================================================================================
   class(err_t), intent(in) :: a, b
   logical                  :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code == b%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code /= b%code)
   
   END FUNCTION err_Ne
   

!=============================================================================================
   pure FUNCTION err_NeIntg ( a, i ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code == i
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code /= i)
   
   END FUNCTION err_NeIntg


!=============================================================================================
   pure FUNCTION err_IntgNe ( i, a ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code /= i
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code /= i)
      
   END FUNCTION err_IntgNe


!=============================================================================================
   pure FUNCTION err_Gt ( a, b ) result( res )
!=============================================================================================
   class(err_t), intent(in) :: a, b
   logical                  :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code > b%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code > b%code)
   
   END FUNCTION err_Gt
   

!=============================================================================================
   pure FUNCTION err_GtIntg ( a, i ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code > i
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code > i)
   
   END FUNCTION err_GtIntg


!=============================================================================================
   pure FUNCTION err_IntgGt ( i, a ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if i > a%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (i > a%code)
      
   END FUNCTION err_IntgGt


!=============================================================================================
   pure FUNCTION err_Ge ( a, b ) result( res )
!=============================================================================================
   class(err_t), intent(in) :: a, b
   logical                  :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code >= b%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code >= b%code)
   
   END FUNCTION err_Ge
   

!=============================================================================================
   pure FUNCTION err_GeIntg ( a, i ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code >= i
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code >= i)
   
   END FUNCTION err_GeIntg


!=============================================================================================
   pure FUNCTION err_IntgGe ( i, a ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if i >= a%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (i >= a%code)
      
   END FUNCTION err_IntgGe


!=============================================================================================
   pure FUNCTION err_Lt ( a, b ) result( res )
!=============================================================================================
   class(err_t), intent(in) :: a, b
   logical                  :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code < b%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code < b%code)
   
   END FUNCTION err_Lt
   

!=============================================================================================
   pure FUNCTION err_LtIntg ( a, i ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code < i
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code < i)
   
   END FUNCTION err_LtIntg


!=============================================================================================
   pure FUNCTION err_IntgLt ( i, a ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if i < a%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (i < a%code)
      
   END FUNCTION err_IntgLt


!=============================================================================================
   pure FUNCTION err_Le ( a, b ) result( res )
!=============================================================================================
   class(err_t), intent(in) :: a, b
   logical                  :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code <= b%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code <= b%code)
   
   END FUNCTION err_Le
   

!=============================================================================================
   pure FUNCTION err_LeIntg ( a, i ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if a%code <= i
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (a%code <= i)
   
   END FUNCTION err_LeIntg


!=============================================================================================
   pure FUNCTION err_IntgLe ( i, a ) result( res )
!=============================================================================================
   class  (err_t), intent(in) :: a
   integer(Ikind), intent(in) :: i
   logical                    :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if i <= a%code
!-----------------------------------------------------------------------------------R.H. 12/19      

   res = (i <= a%code)
      
   END FUNCTION err_IntgLe


!=============================================================================================
   SUBROUTINE err_FindSubstring1 ( loc, n, str, substr )
!=============================================================================================  
   character(len=*),              intent(in    ) :: str, substr
   integer  (Ikind),              intent(   out) :: n   
   integer  (Ikind), allocatable, intent(   out) :: loc(:)
!--------------------------------------------------------------------------------------------- 
!  Finds in the string "str" the occurences (n) of the substring "substr". 
!  Stores its different locations in the array "loc". 
!-----------------------------------------------------------------------------------R.H. 01/18 

!- local variables ---------------------------------------------------------------------------
   integer  (Ikind )              :: len_substr, len_str, p, lind1
   integer  (Ikind ), allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 
   
   n = 0

   len_str = len(str) ; len_substr = len(substr)   
   
   if ( len_str == 0 .or. len_substr == 0 ) return

   allocate(loc(len_str), source=0_Ikind)  

   lind1 = 1
   do while ( lind1 <= len_str )
      p = index(str(lind1:len_str),substr)
      if ( p == 0 ) exit
      n = n + 1
      loc(n) = lind1 + p - 1
      lind1 = loc(n) + len_substr
   end do
   
   allocate(tmp(n))
   
   tmp = loc(1:n) ; call move_alloc(from=tmp, to=loc)  
            
   END SUBROUTINE err_FindSubstring1   


!=============================================================================================      
   SUBROUTINE err_resetHaltingMode ( reset, stat )
!=============================================================================================   
   logical    ,           intent(in) :: reset
   type(err_t), optional, intent(in) :: stat
!---------------------------------------------------------------------------------------------
!  Changes temporarily the halting mode
!
!  - If reset is .false.: 
!       . if stat is present    : set the halting mode to .false.
!       . if stat is not present: set the halting mode to .true. 
!  - If reset is .true.: 
!       restore the halting  mode to its initial value (i.e. err_saveHalting)
!---------------------------------------------------------------------------------------------
   
   if ( reset ) then
      err_Halting = err_saveHalting
   else
      if ( present(stat) ) then
         err_Halting= .false.
      else
         err_Halting = .true.
      end if
   end if
   
   END SUBROUTINE err_resetHaltingMode   
   
   
END MODULE err_m
