!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: str
!
! Description: defines a usefull type for a list of strings of different lengths
!
! Version 10/2019
! Modified
! - 07/2021 (R.H.): operators "==", "/=" 
! - 09/2022 (R.H.): added a member for colored string, print color string, operator "+" (//) 
!---------------------------------------------------------------------------------------------

MODULE str_m

   use ansiColor_m
   
   implicit none
   
   private
   public :: str_t, str_color, str_colorChar, str_print, str_concate, str_moveAlloc

   type :: str_t
      ! when part of the string is colored, strcol contains ansi codes while str is always a
      ! copy without these codes. For non-colored string, has_color must be set to .false.
      character(len=:), allocatable :: str
      character(len=:), allocatable :: strcol
      logical                       :: has_color = .false.
   contains
      procedure, pass(lhs) :: str_assignFromStr       ! str_t = str_t
      procedure, pass(lhs) :: str_assignFromChar      ! str_t = char
      procedure, pass(rhs) :: getStr => str_getString           ! allocchar = str_t
      
      generic, public :: assign => str_assignFromStr , str_assignFromChar
      
      generic, public :: assignment (=) => & 
                           !str_assignFromStr,  & !< prefer intrinsic "=" (commented 08/24)
                           str_assignFromChar,  &
                           getStr

      procedure, pass(lhs) :: str_StrEqStr     ! str_t == str_t
      procedure, pass(lhs) :: str_StrEqChar    ! str_t == char
      procedure, pass(rhs) :: str_CharEqStr    ! char  == str_t

      generic, public :: operator (==) => str_StrEqStr ,  &
                                          str_StrEqChar,  &
                                          str_CharEqStr

      procedure, pass(lhs) :: str_StrNeStr     ! str_t /= str_t
      procedure, pass(lhs) :: str_StrNeChar    ! str_t /= char
      procedure, pass(rhs) :: str_CharNeStr    ! char  /= str_t

      generic, public :: operator (/=) => str_StrNeStr ,  &
                                          str_StrNeChar,  &
                                          str_CharNeStr

      procedure, pass(a) :: str_StrPlusStr     ! str_t + str_t
      procedure, pass(a) :: str_StrPlusChar    ! str_t + char
      procedure, pass(b) :: str_CharPlusStr    ! char  + str_t

      generic, public :: operator (+) => str_StrPlusStr ,  &
                                         str_StrPlusChar,  &
                                         str_CharPlusStr

      procedure, pass(self) :: length => str_length
      procedure, pass(self) :: printMe => str_printMe

      procedure, pass(self) :: getMyColor => str_getMyColor

      !final :: str_FinalizeStr ! causes problem with gfortran...
   end type str_t

   type :: objstr_t
      character(len=:), allocatable :: name
      type     (str_t), allocatable :: cmp(:)
      integer                       :: ncmp
   end type objstr_t
   
   interface str_concate
      ! concatenation
      module procedure str_concateStrStr,  & ! a str_t with a str_t
                       str_concateStrChar, & ! a str_t with a character string
                       str_concateCharStr    ! a character string with a str_t
   end interface
   
   interface str_moveAlloc
      ! move allocation
      module procedure str_transfertFromStrToStr      , &  ! from str_t to str_t
                       str_transfertFromStrToAllocChar, &  ! from str_t to character
                       str_transfertFromAllocCharToStr     ! from character to str_t
   end interface

   interface str_color
      ! create a colored str_t
      module procedure str_color1, & ! from a str_t
                       str_color2    ! from a character string
   end interface

   interface str_colorChar
      ! create a colored character string
      module procedure str_colorChar1, & ! from a str_t
                       str_colorChar2    ! from a character string
   end interface
   
   interface str_print
      ! print a str_t objects
      module procedure str_printk0, & ! a str_t scalar
                       str_printk1, & ! a rank 1 array of str_t
                       str_printk2    ! a rank 2 array of str_t
   end interface 

   interface str_pad
      ! append spaces 
      module procedure str_padChar, str_padStr
   end interface     
   
CONTAINS

!=============================================================================================
   SUBROUTINE str_FinalizeStr ( self )
!=============================================================================================
   type(str_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Finalizer
!---------------------------------------------------------------------------------------------
        
   if ( allocated(self%str)    ) deallocate(self%str)
   
   if ( allocated(self%strcol) ) deallocate(self%strcol)
   self%has_color = .false.
     
   END SUBROUTINE str_FinalizeStr


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Assignments: ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE str_assignFromStr ( lhs, rhs )
!=============================================================================================
   class(str_t), intent(in out) :: lhs
   type (str_t), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 10/19       
      
   if ( allocated(rhs%str) ) then
      lhs%str = (rhs%str)
   else
      lhs%str = ''
   end if
   
   if ( rhs%has_color ) then 
      if ( allocated(rhs%strcol) ) lhs%strcol = (rhs%strcol)
      lhs%has_color = .true.
   end if
   
   END SUBROUTINE str_assignFromStr 


!=============================================================================================
   SUBROUTINE str_assignFromChar ( lhs, rhs )
!=============================================================================================
   class    (str_t), intent(   out) :: lhs
   character(len=*), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 10/19       
   
   lhs%str = rhs
   
   END SUBROUTINE str_assignFromChar


!=============================================================================================
   SUBROUTINE str_getString ( lhs, rhs )
!=============================================================================================
   character(len=:), allocatable, intent(   out) :: lhs
   class    (str_t),              intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 10/19       
   
   if ( allocated(rhs%str) ) then
      lhs = (rhs%str)
   else
      lhs = ''
   end if
   
   END SUBROUTINE str_getString 


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Move allocation (transfert): ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE str_transfertFromStrToStr ( from, to )
!=============================================================================================
   type(str_t), intent(in out) :: from
   type(str_t), intent(   out) :: to
!---------------------------------------------------------------------------------------------
!  Transferts the str_t from to the str_t to
!-----------------------------------------------------------------------------------R.H. 09/22     
      
   call move_alloc ( from = from%str, to = to%str )
      
   if ( from%has_color ) then
      call move_alloc ( from = from%strcol, to = to%strcol )
      to  %has_color = .true.
      from%has_color = .false.
   end if
   
   END SUBROUTINE str_transfertFromStrToStr 
   
   
!=============================================================================================
   SUBROUTINE str_transfertFromStrToAllocChar ( from, to )
!=============================================================================================
   type     (str_t),              intent(in out) :: from
   character(len=:), allocatable, intent(   out) :: to
!---------------------------------------------------------------------------------------------
!  Transferts from%str to the allocatable character to
!  Resets all other members of from to their defaults values
!-----------------------------------------------------------------------------------R.H. 09/22     
      
   call move_alloc ( from = from%str, to = to )
   
   from%has_color = .false.
   if ( allocated(from%strcol) ) deallocate(from%strcol)

   END SUBROUTINE str_transfertFromStrToAllocChar 
   

!=============================================================================================
   SUBROUTINE str_transfertFromAllocCharToStr ( from, to )
!=============================================================================================
   character(len=:), allocatable, intent(in out) :: from
   type     (str_t),              intent(   out) :: to
!---------------------------------------------------------------------------------------------
!  Transferts the allocatable character from to the from%str
!-----------------------------------------------------------------------------------R.H. 09/22     
      
   call move_alloc ( from = from, to = to%str )

   END SUBROUTINE str_transfertFromAllocCharToStr 
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Comparisons: ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
         
!=============================================================================================
   elemental FUNCTION str_StrEqStr ( lhs, rhs ) result( is_same )
!=============================================================================================
   class(str_t), intent(in) :: lhs
   type (str_t), intent(in) :: rhs
   logical                  :: is_same
!---------------------------------------------------------------------------------------------
!  Boolean condition is_same = (lhs%str == rhs%str)
!-----------------------------------------------------------------------------------R.H. 07/21       

   if ( allocated(lhs%str) .and. allocated(rhs%str) ) then
      is_same = trim(adjustl(lhs%str)) == trim(adjustl(rhs%str))
   else if ( .not. allocated(lhs%str) .and. .not. allocated(rhs%str) ) then
      is_same = .true.
   else
      is_same = .false.
   end if
      
   END FUNCTION str_StrEqStr


!=============================================================================================
   elemental FUNCTION str_StrEqChar ( lhs, string ) result( is_same )
!=============================================================================================
   class    (str_t), intent(in) :: lhs
   character(len=*), intent(in) :: string
   logical                      :: is_same
!---------------------------------------------------------------------------------------------
!  Boolean condition is_same = (lhs%str == string)
!-----------------------------------------------------------------------------------R.H. 07/21       

   if ( allocated(lhs%str) ) then
      is_same = trim(adjustl(lhs%str)) == trim(adjustl(string))
   else if ( len(string) == 0 ) then
      is_same = .true.
   else
      is_same = .false.
   end if
      
   END FUNCTION str_StrEqChar


!=============================================================================================
   elemental FUNCTION str_CharEqStr ( string, rhs ) result( is_same )
!=============================================================================================
   character(len=*), intent(in) :: string
   class    (str_t), intent(in) :: rhs
   logical                      :: is_same
!---------------------------------------------------------------------------------------------
!  Boolean condition is_same = (string == rhs%str)
!-----------------------------------------------------------------------------------R.H. 07/21       

   is_same = str_StrEqChar ( rhs, string )
      
   END FUNCTION str_CharEqStr


!=============================================================================================
   elemental FUNCTION str_StrNeStr ( lhs, rhs ) result( is_diff )
!=============================================================================================
   class(str_t), intent(in) :: lhs
   type (str_t), intent(in) :: rhs
   logical                  :: is_diff
!---------------------------------------------------------------------------------------------
!  Boolean condition is_idff = (lhs%str /= rhs%str)
!-----------------------------------------------------------------------------------R.H. 07/21       

   is_diff = .not. ( str_StrEqStr (lhs,rhs) )
      
   END FUNCTION str_StrNeStr


!=============================================================================================
   elemental FUNCTION str_StrNeChar ( lhs, string ) result( is_diff )
!=============================================================================================
   class    (str_t), intent(in) :: lhs
   character(len=*), intent(in) :: string
   logical                      :: is_diff
!---------------------------------------------------------------------------------------------
!  Boolean condition is_idff = (lhs%str /= string)
!-----------------------------------------------------------------------------------R.H. 07/21       

   is_diff = .not. ( str_StrEqChar (lhs,string) )
      
   END FUNCTION str_StrNeChar


!=============================================================================================
   elemental FUNCTION str_CharNeStr ( string, rhs ) result( is_diff )
!=============================================================================================
   character(len=*), intent(in) :: string
   class    (str_t), intent(in) :: rhs   
   logical                      :: is_diff
!---------------------------------------------------------------------------------------------
!  Boolean condition is_idff = (string /= rhs%str)
!-----------------------------------------------------------------------------------R.H. 07/21       

   is_diff = .not. ( str_StrEqChar (rhs,string) )
      
   END FUNCTION str_CharNeStr


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Concatenations: +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   elemental FUNCTION str_strPlusStr ( a, b ) result( res )
!=============================================================================================
   class(str_t), intent(in) :: a, b
   type (str_t)             :: res
!---------------------------------------------------------------------------------------------
!  a+b: concatenates 'a' and 'b' 
!       (--> res%str = a%str // b%str  and  res%strcol = a%strcol // b%strcol)
!-----------------------------------------------------------------------------------R.H. 09/22       

   call str_concateStrStr ( a, b, res )
      
   END FUNCTION str_strPlusStr   


!=============================================================================================
   elemental FUNCTION str_strPlusChar ( a, char ) result( res )
!=============================================================================================
   class    (str_t), intent(in) :: a
   character(len=*), intent(in) :: char
   type     (str_t)             :: res
!---------------------------------------------------------------------------------------------
!  a+char: concatenates 'a' with a character 
!          (--> res%str = a%str // char  and  res%strcol = a%strcol // char)
!-----------------------------------------------------------------------------------R.H. 09/22       

   call str_concateStrChar ( a, char, res )
   
   END FUNCTION str_strPlusChar  


!=============================================================================================
   elemental FUNCTION str_charPlusStr ( char, b ) result( res )
!=============================================================================================
   character(len=*), intent(in) :: char
   class    (str_t), intent(in) :: b   
   type     (str_t)             :: res
!---------------------------------------------------------------------------------------------
!  char+b: concatenates a character with 'b' 
!          (--> res%str = char // b%str  and  res%strcol = char // b%strcol)
!-----------------------------------------------------------------------------------R.H. 09/22       

   call str_concateCharStr ( char, b, res )
   
   END FUNCTION str_charPlusStr 
   
   
!=============================================================================================
   elemental SUBROUTINE str_concateStrStr ( a, b, res )
!=============================================================================================
   class(str_t), intent(in    ) :: a, b
   type (str_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  a+b: concatenates 'a' and 'b' 
!       (--> res%str = a%str // b%str  and  resc%strcol = a%strcol // b%strcol)
!-----------------------------------------------------------------------------------R.H. 09/22       

   if ( allocated(a%str) .and. allocated(b%str) ) then
      res%str = a%str // b%str
      if ( allocated(a%strcol) .and. allocated(b%strcol) ) then
         res%strcol = a%strcol // b%strcol
         res%has_color = .true.
      else if ( allocated(a%strcol) ) then
         res%strcol = a%strcol // b%str
         res%has_color = .true.
      else if ( allocated(b%strcol) ) then
         res%strcol = a%str // b%strcol
         res%has_color = .true.
      end if
   else if ( allocated(a%str) ) then
      res%str = a%str
      if ( allocated(a%strcol) ) then
         res%strcol = a%strcol 
         res%has_color = .true.
      end if      
   else if ( allocated(b%str) ) then
      res%str = b%str
      if ( allocated(b%strcol) ) then
         res%strcol = b%strcol 
         res%has_color = .true.
      end if      
   end if
      
   END SUBROUTINE str_concateStrStr   


!=============================================================================================
   elemental SUBROUTINE str_concateStrChar ( a, b, res )
!=============================================================================================
   class    (str_t), intent(in    ) :: a
   character(len=*), intent(in    ) :: b
   type     (str_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  a+char: concatenates 'a' with a character 
!          (-->resc%str = a%str // char  and  res%strcol = a%strcol // char)
!-----------------------------------------------------------------------------------R.H. 09/22       

   if ( allocated(a%str) ) then
      res%str = a%str // b
      if ( allocated(a%strcol) ) then
         res%strcol = a%strcol // b
         res%has_color = .true.
      end if
   else
      res%str = b
   end if
      
   END SUBROUTINE str_concateStrChar  
   

!=============================================================================================
   elemental SUBROUTINE str_concateCharStr ( a, b, res )
!=============================================================================================
   character(len=*), intent(in    ) :: a
   class    (str_t), intent(in    ) :: b   
   type     (str_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  char+b: concatenates char with 'b'  
!          (--> res%str = char // b%str  and  res%strcol = char // b%strcol)
!-----------------------------------------------------------------------------------R.H. 09/22       

   if ( allocated(b%str) ) then
      res%str = a // b%str 
      if ( allocated(b%strcol) ) then
         res%strcol = a // b%strcol 
         res%has_color = .true.
      end if
   else
      res%str = a
   end if
      
   END SUBROUTINE str_concateCharStr     
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Length: +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!=============================================================================================
   elemental FUNCTION str_length ( self ) result ( res )
!=============================================================================================
   class(str_t), intent(in) :: self 
   integer                  :: res
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 09/22       

   if ( allocated(self%str) ) then
      res = len(self%str)
   else
      res = 0
   end if
      
   END FUNCTION str_length  
   
   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Coloring: +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 


!=============================================================================================
   FUNCTION str_getMyColor ( self ) result( res )
!=============================================================================================   
   class    (str_t), intent(in)  :: self
   character(len=:), allocatable :: res
!---------------------------------------------------------------------------------------------
!  Returns the ANSI code used in self
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   res = ''
   if ( self%has_color ) res = self%strcol( 1 : index(self%strcol,'m') )
   
   END FUNCTION str_getMyColor
   
         
!=============================================================================================
   FUNCTION str_color1 ( str, userColorCode ) result( res )
!=============================================================================================   
   class    (str_t), intent(in) :: str
   character(len=*), intent(in) :: userColorCode
   type     (str_t)             :: res
!---------------------------------------------------------------------------------------------
!  Sets res%str = str%str and if the user color code (userColorCode) is a valid code sets also 
!  res%strcol to str%str but coded according to the corresponding ansi code.
!
!  Note: A valid user color code is either an ANSI color code or a character string beginning
!        with one of the following:
!                              'r', 'b', 'k', 'y', 'c', 'm' 'g'
!       (for black, blue, cyan, green, magenta, red and yellow) and can be followed by 'b' for
!        bold 'h' for high intensity, 'U' for underlined, 'B' for background (in any order)
!
!  Example: userColorCode = 'rb'  --> bold red
!                         = 'rbh' --> high intensity bold red
!                         = 'rU'  --> red and underlined
!                         = 'rhU' --> high intensity red and underlined
!                         = 'rB'  --> backgr. red
!                         = 'rhB' --> backgr. high intensity red
!
!-----------------------------------------------------------------------------------R.H. 09/22       

!- local variables: --------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   res = str_color2 ( str%str, userColorCode )
   
   END FUNCTION str_color1   


!=============================================================================================
   FUNCTION str_color2 ( char, userColorCode ) result( res )
!=============================================================================================   
   character(len=*), intent(in) :: char, userColorCode
   type     (str_t)             :: res
!---------------------------------------------------------------------------------------------
!  Same as str_color1 but for character ('char') input
!-----------------------------------------------------------------------------------R.H. 09/22       

!- local variables: --------------------------------------------------------------------------
   character(len=:), allocatable :: color
!---------------------------------------------------------------------------------------------

   res%str = char
 
   color = trim(adjustl(userColorCode))
   
   if ( len(color) > 2 ) then
      if ( color(1:2) /= ansiColor_start ) color = ansiColor_getAnsiCode (color)
   else
      color = ansiColor_getAnsiCode (color)
   end if
   
   if ( len_trim(color) /= 0 ) then
      res%strcol = color // char // ansiColor_clear
      res%has_color = .true.
   end if
   
   END FUNCTION str_color2


!=============================================================================================
   FUNCTION str_colorChar1 ( str, userColorCode ) result( res )
!=============================================================================================   
   class    (str_t), intent(in) :: str
   character(len=*), intent(in) :: userColorCode
   character(len=:), allocatable :: res
!---------------------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------R.H. 09/22       

!- local variables: --------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------

   res = str_colorChar2 ( str%str, userColorCode )
   
   END FUNCTION str_colorChar1   
   
!=============================================================================================
   FUNCTION str_colorChar2 ( char, userColorCode ) result( res )
!=============================================================================================   
   character(len=*), intent(in)  :: char, userColorCode
   character(len=:), allocatable :: res
!---------------------------------------------------------------------------------------------
!  Same as str_colorChar1 but for character ('char') input
!-----------------------------------------------------------------------------------R.H. 09/22       

!- local variables: --------------------------------------------------------------------------
   character(len=:), allocatable :: color
!---------------------------------------------------------------------------------------------

   res = char
 
   color = trim(adjustl(userColorCode))
   
   if ( len(color) > 2 ) then
      if ( color(1:2) /= ansiColor_start ) color = ansiColor_getAnsiCode (color)
   else
      color = ansiColor_getAnsiCode (color)
   end if
            
   if ( len_trim(color) /= 0 ) res = color // char // ansiColor_clear
   
   END FUNCTION str_colorChar2


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Printings: ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE str_printMe ( self, unit, fmt, advance )
!============================================================================================= 
   class    (str_t),           intent(in) :: self
   integer         ,           intent(in) :: unit  
   character(len=*), optional, intent(in) :: fmt, advance
!---------------------------------------------------------------------------------------------
!  Prints the content of self with the format fmt (optional) and on the given unit.
!  If self contains a colored string (self%strcol allocated) check first if the unit corresponds
!  to a tty before printing the member self%strcol. Otherwise prints only self%str.
!-----------------------------------------------------------------------------------R.H. 09/22       

!- local variables: --------------------------------------------------------------------------
   character(len=:), allocatable :: form, adv
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
      
   if ( self%has_color .and. allocated(self%strcol) ) then
      if ( ansiColor_isATeleTypeWriter(unit) ) then
         write(unit,form,advance=adv) self%strcol
      else if ( allocated(self%str) ) then
         write(unit,form,advance=adv) self%str
      endif
   else
      if ( allocated(self%str) ) write(unit,form,advance=adv) self%str
   end if

   END SUBROUTINE str_printMe
   

!=============================================================================================
   SUBROUTINE str_printk2 ( unit, s, lhsMsg, title, underline, sepCol, sepRow, justify, &
                            colorTitle, colorLhsMsg, colorUnderline, nblkBefore, nblkAfter )
!=============================================================================================
   integer         ,           intent(in) :: unit
   type     (str_t),           intent(in) :: s(:,:)
   character(len=*), optional, intent(in) :: lhsMsg, title, underline, sepCol, sepRow, &
                                             justify, colorTitle, colorLhsMsg, colorUnderline
   integer         , optional, intent(in) :: nblkBefore, nblkAfter
!---------------------------------------------------------------------------------------------  
!  Writes on the unit "unit" the contents of the rank 2 string array "s"
!
!  The optional arguments are:
!
!  . lhsMsg:
!       a message to print at the lhs of the array
!  . title:
!       will be printed before the array and centered
!  . underline: 
!       character(s) used to underline the title
!  . sepCol: 
!       character(s) used as column delimiter
!  . sepRow: 
!       character(s) used as row delimiter
!  . justify: 
!       a combination of 'l', 'r', 'c' characters to indicate how the column have to be
!       justified. For example if justify = 'cclr' the first and second columns will be
!        centered while the third and fourth will be left and right justified, respectively. 
!       Note:
!         - If justify is not given, columns are left justified by default.
!         - If justify is given but with a length < size(s,2), the remaining columns are left
!           justified 
!  . colorTitle, colorLhsMsg, colorUnderline: 
!      code color for the title, the lhs message and the underline characters
!  . nblkBefore: 
!      number of blank lines before the print
!  . nblkAfter: 
!      number of blank lines after the print
!
!  Example:
!     call str_printk2 ( 6, s, lhsMsg = "s = ", title = "my title", underline = "-", &
!                        sepCol = ", " , sepRow = "; ", justify = 'llll')
!
!---------------------------------------------------------------------------------- R.H. 09/22   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: sepColDefault = '  '
   type     (str_t)              :: title_, lhsMsg_, underl
   character(len=:), allocatable :: usymb_, sepCol_, sepRow_, justify_, &
                                    shift1, shift2, buf
   integer                       :: i, j, n, m, justTitle
   integer                       :: lenMsg, lenTitle, lenSepCol, lenSepRow, lenJust, &
                                    lenTab, lenUsymb
   integer         , allocatable :: lenCol(:), justCol(:)
   logical         , allocatable :: with_color(:,:)
   logical                       :: isatty, w_col
!---------------------------------------------------------------------------------------------   
   
   n = size(s,1) ; m = size(s,2)
   
   if ( present(lhsMsg) ) then
      lenMsg = len(lhsMsg)      
      if ( present(colorLhsMsg) ) then
         lhsMsg_ = str_color ( lhsMsg, colorLhsMsg )
      else
         lhsMsg_%str = lhsMsg
      end if
   else
      lhsMsg_%str = ''
      lenMsg = 0
   end if

   if ( present(title) ) then
      lenTitle = len(title)
      if ( present(colorTitle) ) then
         title_ = str_color ( title ,colorTitle )
      else
         title_%str = title
      end if
   else
      title_%str= ''
      lenTitle = 0
   end if
   
   justTitle = 2 ! centered title
   if ( present(underline) ) then
      usymb_ = underline  ; lenUsymb = len(usymb_)
   else
      usymb_ = ''
      lenUsymb = 0
   end if   
!
!- Symbol for column delimiter (if no delimiter given, set the delimiter to its default):
!
   if ( present(sepCol) ) then
      sepCol_ = sepCol ; lenSepCol = len(sepCol)
   else
      sepCol_ = ''; lenSepCol = 0
   end if
   if ( lenSepCol == 0 ) then
      sepCol_   = sepColDefault
      lenSepCol = len(sepColDefault)
   end if
!
!- Symbol for row delimiter (no delimiter if only one row):
!   
   if ( present(sepRow) .and. n /= 1 ) then
      sepRow_ = sepRow ; lenSepRow = len(sepRow)
   else
      sepRow_ = ''; lenSepRow = 0
   end if   
   
   if ( present(justify) ) then
      justify_ = justify
      lenJust = len(justify)
   else
      justify_ = ''
      lenJust = 0
   end if   
!   
!- Store in lenCol the length of each column, determine which of them have to be justified
!  and which entries are in color:
!
   allocate(lenCol(m), justCol(m), source = 0)
   allocate(with_color(n,m), source = .false.)

   do j = 1, m
   
      do i = 1, n
         if ( allocated(s(i,j)%str) ) lenCol(j) = max( lenCol(j), len(s(i,j)%str) )
         if ( s(i,j)%has_color .and. allocated(s(i,j)%strcol) ) with_color(i,j) = .true.
      end do
      
      if ( j <= lenJust ) then
         if ( justify_(j:j) == 'r' ) then
            justCol(j) = 1
         else if ( justify_(j:j) == 'c' ) then
            justCol(j) = 2
         end if
      end if
      
   end do
!
!- The total length of the printed table:
!
   lenTab = sum(lenCol) + lenSepCol*(m-1) + lenSepRow + lenMsg
!
!-  When the table is shorter than the title, add a shift in order to center the table:
!
   if ( lenTitle > lenTab ) then
      shift1 = repeat(' ',(lenTitle-lenTab)/2)
   else
      shift1 = ''
   end if
!
!- When a lhs message is present (1st row), add a shift to align the row with the 1st:
!
   shift2 = repeat(' ',lenMsg) 
!
!- Print the title and the underline:
!
   isatty = ansiColor_isATeleTypeWriter(unit)

   if ( present(nblkBefore) ) then
      do i = 1, nblkBefore
         write(unit,*)
      end do
   end if
   
   if ( present(title) ) then
      w_col = title_%has_color .and. isatty
      write(unit,'(a)') str_pad ( title_, lenTab, justTitle, w_col, colorTitle )
   end if

   if ( lenUsymb > 0 ) then
      if ( lenUsymb == 1 ) then
         underl%str = repeat ( usymb_, max(lenTab,lenTitle) )
      else
         underl%str = '' ; j = 1 
         do i = 1, max(lenTab,lenTitle)
            underl%str = underl%str // usymb_(j:j)
            j = j+1 ; if ( j >  lenUsymb ) j = 1
         end do
      end if
      if ( present(colorUnderline) ) underl = str_color ( underl, colorUnderline )
      w_col = underl%has_color .and. isatty
      write(unit,'(a)') str_pad ( underl, lenTab, justTitle, w_col )
   end if
!
!- Print the table (in the case of colored text, use color only if the unit is a tty):
!
   with_color = with_color .and. isatty
   
   do i = 1, n
   
      buf = str_pad ( s(i,m), lenCol(m), justCol(m), with_color(i,m) ) // sepRow_
      do j = m-1,1,-1
         buf = str_pad ( s(i,j), lenCol(j), justCol(j), with_color(i,j) ) // sepCol_ // buf
      end do
      
      if ( i == 1 ) then
         w_col = lhsMsg_%has_color .and. isatty
         
!-       I don't know why the following write causes a bug with intel ifx:
         
         !write(unit,'(*(a))') shift1, str_pad(lhsMsg_,0,0,w_col), &
         !( str_pad ( s(i,j), lenCol(j), justCol(j), with_color(i,j) ), sepCol_, j=1,m-1 ), &
         !  str_pad ( s(i,m), lenCol(m), justCol(m), with_color(i,m) ), sepRow_
         
         ! Workarround:
         
         buf = shift1 // str_pad(lhsMsg_,0,0,w_col) // buf
      else 
         if ( i == n ) sepRow_ = ''

!-       I don't know why the following write causes a bug with intel ifx:

         !write(unit,'(*(a))') shift1, shift2 , &
         !( str_pad ( s(i,j), lenCol(j), justCol(j), with_color(i,j) ), sepCol_, j=1,m-1 ), &
         !  str_pad ( s(i,m), lenCol(m), justCol(m), with_color(i,m) ), sepRow_
           
         ! Workarround:
         
         buf = shift1 // shift2 // buf
      end if
      
      write(unit,'(a)') buf

   end do

   if ( present(nblkAfter) ) then
      do i = 1, nblkAfter
         write(unit,*)
      end do
   end if
      
   END SUBROUTINE str_printk2

!=============================================================================================
   SUBROUTINE str_printk1 ( unit, s, lhsMsg, title, underline, sepCol, sepRow, justify, &
                            colorTitle, colorLhsMsg, colorUnderline, nblkBefore, nblkAfter )
!=============================================================================================
   integer         ,           intent(in) :: unit
   type     (str_t),           intent(in) :: s(:)
   character(len=*), optional, intent(in) :: lhsMsg, title, underline, sepCol, sepRow, &
                                             justify, colorTitle, colorLhsMsg, colorUnderline
   integer         , optional, intent(in) :: nblkBefore, nblkAfter
!--------------------------------------------------------------------------------------------- 
!  Description: see str_printk2
!  Note: obviously "sepcol" is not needed here, it is left for consistency with str_printk2
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------
   character(len=:), allocatable :: usymb_, sepRow_, justify_, shift1, shift2
   type     (str_t)              :: title_, lhsMsg_, underl
   integer                       :: i, j, n, justCol, justTitle
   integer                       :: lenMsg, lenTitle, lenSepRow, lenJust, lenTab, lenUsymb
   integer                       :: lenCol
   logical         , allocatable :: with_color(:)
   logical                       :: isatty, w_col
!---------------------------------------------------------------------------------------------   
   
   n = size(s)
   
   if ( present(lhsMsg) ) then
      lenMsg = len(lhsMsg)      
      if ( present(colorLhsMsg) ) then
         lhsMsg_ = str_color ( lhsMsg, colorLhsMsg )
      else
          lhsMsg_%str = lhsMsg
      end if
   else
      lenMsg = 0
      lhsMsg_%str = ''
   end if

   if ( present(title) ) then
      lenTitle = len(title)
      if ( present(colorTitle) ) then
         title_ = str_color ( title ,colorTitle )
      else
         title_%str = title
      end if
   else
      lenTitle = 0
   end if
   
   justTitle = 2 ! centered title
   if ( present(underline) ) then
      usymb_ = underline ; lenUsymb = len(usymb_)
   else
      usymb_ = '' ; lenUsymb = 0
   end if   
   
   if ( present(sepRow) .and. n /= 1 ) then
      sepRow_ = sepRow ; lenSepRow = len(sepRow)
   else
      sepRow_ = '' ; lenSepRow = 0
   end if   

   if ( present(justify) ) then
      justify_ = justify
      lenJust = len(justify)
   else
      lenJust = 0
      justify_ =''
   end if

   lenCol = 0   
   allocate(with_color(n), source=.false.)
   
   do i = 1, n
      if ( allocated(s(i)%str) ) lenCol = max( lenCol, len(s(i)%str) )
      if ( s(i)%has_color .and. allocated(s(i)%strcol) ) with_color(i) = .true.
   end do    
   
   justCol = 0
   if ( lenJust > 0) then
      if ( justify_(1:1) == 'c' ) then
         justCol = 2
      else if ( justify_(1:1) == 'r' ) then
         justCol = 1
      else
         justCol = 0
      end if
   end if
   
   lenTab = lenCol + lenSepRow + lenMsg

   if ( lenTitle > lenTab ) then
      shift1 = repeat(' ',(lenTitle-lenTab)/2)
   else
      shift1 = ''
   end if

   shift2 = repeat(' ',lenMsg)

   isatty = ansiColor_isATeleTypeWriter(unit)

   if ( present(nblkBefore) ) then
      do i = 1, nblkBefore
         write(unit,*)
      end do
   end if
   
   if ( present(title) ) then
      w_col = title_%has_color .and. isatty
      write(unit,'(a)') str_pad ( title_, lenTab, justTitle, w_col, colorTitle )
   end if
   
   if ( lenUsymb > 0 ) then
      if ( lenUsymb == 1 ) then
         underl%str = repeat ( usymb_, max(lenTab,lenTitle) )
      else
         underl%str = '' ; j = 1 
         do i = 1, max(lenTab,lenTitle)
            underl%str = underl%str // usymb_(j:j)
            j = j+1 ; if ( j >  lenUsymb ) j = 1
         end do
      end if
      if ( present(colorUnderline) ) underl = str_color ( underl, colorUnderline )
      write(unit,'(a)') str_pad ( underl, lenTab, justTitle, w_col )
   end if

   with_color = with_color .and. isatty

   do i = 1, n
      if ( i == 1 ) then
         w_col = lhsMsg_%has_color .and. isatty
         write(unit,'(a)') shift1 // str_pad( lhsMsg_, 0, 0, w_col) // &
                           str_pad ( s(i), lenCol, justCol, with_color(i) ) // sepRow_
      else 
         if ( i == n ) sepRow_ = ''
         write(unit,'(a)') shift1 // shift2 // &
                           str_pad ( s(i), lenCol, justCol, with_color(i) ) // sepRow_
      end if
   end do

   if ( present(nblkAfter) ) then
      do i = 1, nblkAfter
         write(unit,*)
      end do
   end if   
   
   END SUBROUTINE str_printk1

!=============================================================================================
   SUBROUTINE str_printk0 ( unit, s, lhsMsg, title, underline, sepCol, sepRow, justify, &
                            colorTitle, colorLhsMsg, colorUnderline, nblkBefore, nblkAfter )
!=============================================================================================
   integer         ,           intent(in) :: unit
   type     (str_t),           intent(in) :: s
   character(len=*), optional, intent(in) :: lhsMsg, title, underline, sepCol, sepRow, &
                                             justify, colorTitle, colorLhsMsg, colorUnderline
   integer         , optional, intent(in) :: nblkBefore, nblkAfter
!--------------------------------------------------------------------------------------------- 
!  Description: see str_printk2
!  Note: obviously "sepcol", "seprow" and  "justify" are not needed here, they are left for  
!        consistency with str_printk2 and str_printk1
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------
   character(len=:), allocatable :: usymb_, shift
   type     (str_t)              :: lhsMsg_, title_, underl
   integer                       :: i, j, lenMsg, lenTitle, lenTab, lenUsymb, justTitle, lenCol
   logical                       :: isatty, with_color, w_col
!---------------------------------------------------------------------------------------------   
   
   if ( present(lhsMsg) ) then
      lenMsg = len(lhsMsg)      
      if ( present(colorLhsMsg) ) then
         lhsMsg_ = str_color ( lhsMsg, colorLhsMsg )
      else
          lhsMsg_%str = lhsMsg
      end if
   else
      lenMsg = 0
      lhsMsg_%str = ''
   end if

   if ( present(title) ) then
      lenTitle = len(title)
      if ( present(colorTitle) ) then
         title_ = str_color ( title ,colorTitle )
      else
         title_%str = title
      end if
   else
      lenTitle = 0
      title_%str = ''
   end if
   
   justTitle = 2 ! centered title
   if ( present(underline) ) then
      usymb_ = underline ; lenUsymb = len(usymb_)
   else
      usymb_ = '' ; lenUsymb = 0
   end if   
   
   lenCol = 0   
   with_color = .false.
   
   if ( allocated(s%str) ) lenCol = len(s%str)
   if ( s%has_color .and. allocated(s%strcol) ) with_color = .true.
   
   lenTab = lenCol + lenMsg

   if ( lenTitle > lenTab ) then
      shift = repeat(' ',(lenTitle-lenTab)/2)
   else
      shift = ''
   end if

   isatty = ansiColor_isATeleTypeWriter(unit)

   if ( present(nblkBefore) ) then
      do i = 1, nblkBefore
         write(unit,*)
      end do
   end if  
      
   if ( present(title) ) then
      w_col = title_%has_color .and. isatty
      write(unit,'(a)') str_pad ( title_, lenTab, justTitle, w_col, colorTitle )
   end if
   
   if ( lenUsymb > 0 ) then
      if ( lenUsymb == 1 ) then
         underl%str = repeat ( usymb_, max(lenTab,lenTitle) )
      else
         underl%str = '' ; j = 1 
         do i = 1, max(lenTab,lenTitle)
            underl%str = underl%str // usymb_(j:j)
            j = j+1 ; if ( j >  lenUsymb ) j = 1
         end do
      end if
      if ( present(colorUnderline) ) underl = str_color ( underl, colorUnderline )
      write(unit,'(a)') str_pad ( underl, lenTab, justTitle, w_col )
   end if

   with_color = isatty .and. with_color 
   w_col      = isatty .and. lhsMsg_%has_color
   
   write(unit,'(a)') shift // str_pad(lhsMsg_,0,0,w_col) // str_pad(s,lenCol,0,with_color)

   if ( present(nblkAfter) ) then
      do i = 1, nblkAfter
         write(unit,*)
      end do
   end if   
      
   END SUBROUTINE str_printk0
   
   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Padding: ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   FUNCTION str_padChar ( char, n, pos ) result( res )
!=============================================================================================
   character(len=*), intent(in)  :: char
   integer         , intent(in)  :: n, pos
   character(len=:), allocatable :: res
   
!- local variables: --------------------------------------------------------------------------
   integer :: lsin, m, ml, mr
!---------------------------------------------------------------------------------------------   
   
   lsin = len(adjustl(char))
   
   m = max(0,n - lsin)
   if ( pos == 0 ) then
      ! left justified
      ml = 0 ; mr = m
   else if ( pos == 1 ) then
      ! right justified
      ml = m ; mr = 0
   else if ( pos == 2 ) then
      ! centered
      ml = m / 2
      mr = m - ml
   else
      ! invalid pos value (= left justified)
      ml = 0 ; mr = m      
   end if
   
   res = repeat(' ',ml) // (char) // repeat(' ',mr)
   
   END FUNCTION str_padChar  
   
!=============================================================================================
   FUNCTION str_padStr ( s, n, pos, with_color, colorPad ) result ( res )
!=============================================================================================
   type     (str_t),           intent(in)  :: s
   integer         ,           intent(in)  :: n, pos
   logical         ,           intent(in)  :: with_color
   character(len=*), optional, intent(in) :: colorPad
   character(len=:), allocatable          :: res
!---------------------------------------------------------------------------------------------   
!
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------
   integer                       :: lsin, m, ml, mr
   character(len=:), allocatable :: leftPad, rightPad
!---------------------------------------------------------------------------------------------   
   
   if ( allocated(s%str) ) then
      lsin = len(adjustl(s%str))
   else
      res = repeat(' ',n)
      return
   end if
   
   m = max(0,n - lsin)
   
   if ( pos == 0 ) then
      ! left justified
      ml = 0 ; mr = m
   else if ( pos == 1 ) then
      ! right justified
      ml = m ; mr = 0
   else if ( pos == 2 ) then
      ! centered
      ml = m / 2
      mr = m - ml
   else
      ! invalid pos value (= left justified)
      ml = 0 ; mr = m
   end if
   
   leftPad = repeat(' ',ml) ; rightPad = repeat(' ',mr)

   if ( with_color .and. s%has_color .and. allocated(s%strcol) ) then
      if ( present(colorPad) ) then
         leftPad  = str_colorChar ( leftPad , colorPad )
         rightPad = str_colorChar ( rightPad, colorPad )
         res = leftPad //  s%strcol // rightPad
      else
         res = leftPad // s%strcol // rightPad
      end if
   else
      res = leftPad // s%str    // rightPad
   end if
   
   END FUNCTION str_padStr  
        

!=============================================================================================
   FUNCTION str_i2a(i) result(a)
!=============================================================================================
   integer         , intent(in)  :: i
   character(len=:), allocatable :: a
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------
   character(len=range(i)+2) :: tmp
!---------------------------------------------------------------------------------------------   

   write(tmp,'(i0)') i ;  a = trim(tmp)

   END FUNCTION str_i2a     
   
END MODULE str_m