!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: ansiColor
!
! Description: 
!---------------------------------------------------------------------------------------------

MODULE ansiColor_m
!
!- Some ANSI color codes (see ANSI-color-codes.h on gist.github.com/RabaDabaDoba )
!   
   private
   public :: ansiColor_colorTxt, ansiColor_getAnsiCode, ansiColor_start, ansiColor_clear, &
             ansiColor_isATeleTypeWriter, ansiColor_write

   character(len=2), parameter :: ansiColor_start = achar(27) // '['
   character(len=4), parameter :: ansiColor_clear = ansiColor_start // '0m'
   
CONTAINS
   

!=============================================================================================
   FUNCTION ansiColor_getAnsiCode ( userCode ) result ( ansiCode )
!=============================================================================================
   character(len=*), intent(in)  :: userCode
   character(len=:), allocatable :: ansiCode
!---------------------------------------------------------------------------------------------
!  Conversion from user code color to ansi code color
!
!  A valid user code color must start with 
!
!                         'k', 'r', 'b', 'y', 'g', 'c', 'm' 
!
!  (for black, red, blue, yellow, green, cyan, magenta)
!  
!  The following codes for the texture and the color intensity may be added:
!  - 'b' for bold, 
!  - 'h' for high intensity,
!  - 'U' for underlined text,
!  - 'B' for background color.
!
!  Examples: rhb, rbh, r_bh, ... (for high intensity red in bold), 
!            rhbB, rB_bh, ... (bakcground high intensity bold)
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=:), allocatable :: user
!---------------------------------------------------------------------------------------------

   ansiCode = ''
   
   user = trim(adjustl(userCode))
   
   if ( len(user) == 0) return

   if ( index(user(2:),'U') /= 0 ) then
      ! Underlined text (e.g. rU --> 4;31, rhU --> 4;91)
      if ( index(user(2:),'h') /= 0 ) then
         ! high intensity
         ansiCode = '4;9'
      else 
         ! regular
         ansiCode = '4;3'
      end if
   else
      if ( index(user(2:),'b') /= 0 ) then
         ! bold text (e.g. rb --> 1;31, rbh --> 1;91, rbB --> 1;41, rbhB --> 1;101)
         ansiCode = '1;'
      else
         ! regular text (e.g. r --> 0;31, rh --> 0;91, rB --> 0;41, rhB --> 0;101)
         ansiCode = '0;'
      end if
      
      if ( index(user(2:),'B') /= 0 .and. index(user(2:),'h') /= 0 ) then
         ! background and high intensity (e.g. rhB, rbhB)
         ansiCode = ansiCode // '10'
      else if ( index(user(2:),'B') /= 0 ) then
         ! background without high intensity (e.g. rB, rbB)
         ansiCode = ansiCode // '4'
      else if ( index(user(2:),'h') /= 0 ) then
         ! high intensity without background (e.g. rh, rbh)
         ansiCode = ansiCode // '9'
      else
         ! without high intensity and without background (e.g. r, rb) 
         ansiCode = ansiCode // '3'
      end if
   end if
   
   ! append the index color:
   select case ( user(1:1) )
      case ('k') ! black
         ansiCode = ansiCode // '0'
      case ('r') ! red
         ansiCode = ansiCode // '1'
      case ('g') ! green
         ansiCode = ansiCode // '2'
      case ('y') ! yellow
         ansiCode = ansiCode // '3'
      case ('b') ! blue
         ansiCode = ansiCode // '4'
      case ('m') ! magenta
         ansiCode = ansiCode // '5'
      case ('c') ! cyan
         ansiCode = ansiCode // '6'
      case ('w') ! white
         ansiCode = ansiCode // '7'
      case default     
         ansiCode = ''
   end select   
   
   if ( len(ansiCode) > 0 ) ansiCode = ansiColor_start // ansiCode // 'm'
   
   END FUNCTION ansiColor_getAnsiCode   
   

!=============================================================================================
   FUNCTION ansiColor_colorTxt ( unit, ansiColorCode, str ) result ( res )
!=============================================================================================
   integer         , intent(in)  :: unit
   character(len=*), intent(in)  :: ansiColorCode   
   character(len=*), intent(in)  :: str
   character(len=:), allocatable :: res
!---------------------------------------------------------------------------------------------
!  (adapted from an answer of sigma on stackoverflow about isatty function)
!---------------------------------------------------------------------------------------------

   if ( ansiColor_isATeleTypeWriter(unit) .and. len_trim(ansiColorCode) /= 0 ) then
      res = trim(adjustl(ansiColorCode)) // str // ansiColor_clear
   else
      res = str
   end if
   
   END FUNCTION ansiColor_colorTxt

   
!=============================================================================================
   FUNCTION ansiColor_isATeleTypeWriter ( unit ) result ( res )
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only:  output_unit, error_unit
   use, intrinsic :: iso_c_binding, only: c_int
   integer, intent(in) :: unit
   logical             :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if the logical unit is a tty
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   integer(c_int) :: cunit
!---------------------------------------------------------------------------------------------

   interface
      function isattyf(fd) bind(C, name = 'isatty')
         use, intrinsic :: iso_c_binding, only: c_int
         integer(c_int)        :: isattyf
         integer(c_int), value :: fd
      end function
   end interface
   
   if ( unit == output_unit ) then
      cunit = 1
   else if ( unit == error_unit ) then
      cunit = 2
   else if ( unit == 1 .or. unit == 2 ) then
      cunit = 11
   else
      cunit = unit
   end if
   
   res = isattyf(cunit) > 0

   END FUNCTION ansiColor_isATeleTypeWriter


!=============================================================================================   
   SUBROUTINE ansiColor_write ( unit, char, color, fmt, advance )
!=============================================================================================
   integer         ,           intent(in) :: unit
   character(len=*),           intent(in) :: char, color
   character(len=*), optional, intent(in) :: fmt, advance
!---------------------------------------------------------------------------------------------
!  Writes the string "char" on the unit "unit" with the color "color" if "unit" corresponds to
!  a tty and if "color" is a valid code color.
!
!  Note: "color" must be an ansi color code or a user one (see ansiColor_getAnsiCode)
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=:), allocatable :: form, adv, col
!---------------------------------------------------------------------------------------------

   if ( present(fmt) ) then
      form = fmt
   else
      form = '(a)'
   end if
   
   if ( present(advance) ) then
      adv = advance
   else
      adv = 'yes'
   end if
   
   if ( ansiColor_isATeleTypeWriter(unit) ) then

      col = trim(adjustl(color))
   
      if ( len(col) > 2 ) then
         if ( col(1:2) /= ansiColor_start ) col = ansiColor_getAnsiCode (col)
      else
         col = ansiColor_getAnsiCode (col)
      end if
            
      if ( len_trim(col) /= 0 ) then
         write(unit,form,advance=adv) col // char // ansiColor_clear  
      else
         write(unit,form,advance=adv) char
      end if
   else
      write(unit,form,advance=adv) char
   end if   
   
   END SUBROUTINE ansiColor_write
      
END MODULE ansiColor_m

