!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: util_Strings
!
! Description: 
! This module contains some usefull functions and subroutines (of my own or found on the web)
!---------------------------------------------------------------------------------------------

#include "error.fpp"

MODULE util_Strings_m
!
!  Notes: 
!   . If you implement a new function or subroutine and, if appropriate, add in its arguments
!     a variable of DT "err_t", say "stat", with the intent(out) to report an error or a 
!     warning message to the caller. Use the defined constructor "err_t" as follows:
!
!         stat = err_t (stat = IERROR, msg = "give the message here")
!     or
!         stat = err_t (stat = WARNING, msg = "give the message here")
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

   use iso_c_binding

   use kindParameters_m
   use pk2Constants_m
   use err_m
   use str_m
      
   implicit none      
!
!  List:
!  ^^^^
!
!  .  util_GetLine (function): 
!         Reads a complete line (end-of-record terminated) from a file

!  .  util_GetRecord (function): 
!         Reads a complete record (end-of-record terminated) from a file

!  . util_CountTokens (function):
!         Counts the number of tokens in a string and optionally returns these tokens.

!  . util_readStringBinFile (function)
!         Reads a string of unknown length from an unformatted file

!  .  util_FindSubstring1 (subroutine): 
!         Finds in a string the occurences of a substring

!  .  util_FindSubstring2 (subroutine): 
!         Finds in a string the occurences of a substring that are not enclosed by a given 
!         pairs of symbols.

!  .  util_ReplaceSubstring1 (function): 
!         Replaces in a string the occurences of a substring by another one

!  .  util_ReplaceSubstring2 (function): 
!         Replaces in a string the occurences of a substring by another one only if not 
!         enclosed by a given set of symbols

!  .  util_IsBalanced1 (function):
!         Sees if a given pair of opening/closing symbols are balanced in a given string

!  .  util_IsBalanced2 (function):
!         Same as util_IsBalanced1 but for a set of pairs of opening/closing symbols

!  .  util_IsEnclosed (function):
!         Sees if a given character of a given string is enclosed by a given pair of symbols

!  .  util_SplitString1 (subroutine):
!         Splits a string into substrings wherever a given delimiter appears 

!  .  util_SplitString2 (subroutine):
!         Same as util_SplitString but for a set of delimiters

!  .  util_FirstOccurence (function):
!         Returns the position in a string of the first occurence of any characters of a set
!         that are not enclosed between a given pair of symbols 

!  .  util_RemoveSpaces1 (function):
!         Removes all occurences of spaces from a string

!  .  util_RemoveSpaces2 (function):
!         Removes all occurences of spaces from a string that are not enclosed by quotation 
!         marks

!  .  util_RemoveZeros (function):
!         Compactes a string-number by removing trailing zeros 

!  .  util_StringLow (function):
!         Sets a string in lowercase

!  .  util_StringCap (function):
!         Capitalizes a string

!  .  util_CharLow (subroutine):
!         Sets a single character in lowercase

!  .  util_CharCap (subroutine):
!         Capitalizes a single character

!  .  util_RemoveChar (function):
!         Removes a given character from a string

!  .  util_intToChar (function):
!        Integer to string conversion

!  .  util_OrdinalAbbrev (function):
!        Ordinal number abbreviations

!  .  util_BarreDefilement (subroutine):
!        Prints a scroll bar

!  .  util_ScrollingMessage (subroutine):
!        Prints a scrolling message

!  .  util_getLhsNRhs (subroutine)
!        Extracts from a string the parts located on either side of a given symbol

   interface util_intToChar
      module procedure util_int32ToChar, util_int64ToChar
   end interface util_intToChar
      
CONTAINS
  
!=============================================================================================
   FUNCTION util_GetLine ( unit, stat ) result ( line )   
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only: iostat_eor
   integer  (Ikind),              intent(in    ) :: unit
   type     (err_t),              intent(in out) :: stat
   character(len=:), allocatable                 :: line
!---------------------------------------------------------------------------------------------
!  Reads a complete line (end-of-record terminated) from a file.
!
!  Source: solution proposed on intel forum by IanH Sun
!  (software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/385790)
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*  ), parameter   :: HERE = 'util_GetLine'
   integer  (Ikind  ), parameter   :: LG = 3000
   integer  (Ikind  )              :: size, iostat
   character(len=256)              :: buf
   character(len=LG )              :: iomsg
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return

   line = ''

   do
      read(unit,'(a)', advance='no', iostat=iostat, iomsg=iomsg, size=size) buf
      
      !flush(unit)

      ! 12/2019: The next line (if (iostat == 126) iostat = 0) is a workarround to a bug with
      ! ifort 19.1.0 ("A non-advancing READ immediately following a non-advancing WRITE...)"
      ! In the meantime, I reset iostat.
      ! 03/2020: No more usefull with ifort.19.1.1 (bug corrected)
      
      !!if (iostat == 126) iostat = 0 
     
      if ( iostat > 0 ) then
         stat = err_t ( UERROR, HERE, trim(iomsg) ) ; return
      end if   

      line = line // buf(:size)
          
      if ( iostat < 0 ) then
         if ( iostat /= iostat_eor ) then
            stat = err_t ( EOF, HERE, trim(iomsg) ) ; return
         end if
         return
      end if

   end do
   
   END FUNCTION util_GetLine 
      

!=============================================================================================
   FUNCTION util_GetRecord0 ( unit, stat, comments, rmblk, nlines, firstl, Rec0 ) result( Rec )
!=============================================================================================
   integer  (Ikind),                        intent(in    ) :: unit
   type     (err_t),                        intent(in out) :: stat 
   character(len=*), optional,              intent(in    ) :: comments(:)
   logical         , optional,              intent(in    ) :: rmblk
   integer  (Ikind), optional,              intent(   out) :: nlines, firstl
   character(len=:), optional, allocatable, intent(   out) :: Rec0   
   character(len=:),           allocatable                 :: Rec   
!---------------------------------------------------------------------------------------------
!  Reads a complete record (end-of-record terminated) from a file.
!
!  Inputs:
!  . unit               : unit file
!  . comments (optional): comments(1) is the symbol used for a comment, comments(2:3) are
!                         the pair of symbols used for a commented block, e.g. 
!                         comments(1) = '//', comments(2) = '/*', comments(3) = '*/'
!  . rmblk    (optional): spaces are removed if rmblk=.true.
!
!  Outputs:
!  . stat             : error handler
!  . nlines (optional): number of lines read
!  . firstl (optional): the first line # (non-blank or non-commented)
!  . rec              : the record
!-----------------------------------------------------------------------------------R.H. 12/16  

!- local variables --------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'util_GetRecord'
   character(len=:), allocatable :: segm0, segm, com, beginComBlk, endComBlk
   integer  (Ikind), allocatable :: loc(:)
   character(len=3)              :: last
   integer  (Ikind)              :: plast, pend, i, n, numl, lsegm, lbeginComBlk, lendComBlk
   logical                       :: rm, is_first
   type     (err_t)              :: flag
   logical         , SAVE        :: is_comBlk = .false.
!--------------------------------------------------------------------------------------------- 
   
   if ( stat%code > IZERO ) return
!
!- Define the symbol used for comments:
!
   if ( present(comments) ) then
      com         = trim(adjustl(comments(1)))
      beginComBlk = trim(adjustl(comments(2)))
      endComBlk   = trim(adjustl(comments(3)))
      lbeginComBlk = len(beginComBlk)
      lendComBlk   = len(endComBlk)
   else
      lbeginComBlk = 0
      lendComBlk = 0
   end if
!
!- Spaces will be removed (default) from "Rec" if "rm" is .true.:
!
   if ( present(rmblk) ) then
      rm = rmblk
   else
      rm = .true.
   end if         
         
   Rec = '' ; if ( present(Rec0) ) Rec0 = ''
      
   last = '...' ! line break symbols
   
   numl = 0 ; if ( present(nlines) ) nlines = 0
   
   is_first = .true. ; if ( present(firstl) ) firstl = 0
   
   if ( stat%code < IZERO ) stat = err_t()
      
   do while ( last == '...' )      
!
!-    read a new line:
!            
      segm0 = util_GetLine ( unit, flag )

      segm = adjustl( segm0 ) 
      
      numl = numl + 1

      if ( present(nlines) ) nlines = numl
!
!-    return, if a problem has occurred: 
!          
      if ( flag > IZERO ) then
         call err_MoveAlloc ( from = flag, to = stat )
         call stat%AddTrace ( HERE )
         return
      end if
!
!-    cycle if it's a blank line or if the eof is reached: return if the read was on stdin, 
!     otherwise exit the loop:

      if ( len_trim(segm) == 0 ) then 
         if ( unit == STDIN ) return
         if ( flag%code < 0 ) then ! if EOF reached
            call err_MoveAlloc ( from = flag, to = stat ) 
            exit
         endif 
         cycle 
      end if  
!      
!-    cycle if it's a commented line (and return if the read was on stdin):
!
      if ( present(comments) ) then
         ! For a single commented line (starting with com):
         if ( index(adjustl(segm),com) == 1 ) then 
            if ( unit == STDIN ) return
            cycle
         end if       
      
         ! For a commented block (starting with beginComBlk and ending with endComBlk):
         if ( .not. is_comBlk ) &
            is_comBlk = ( lbeginComBlk > 0 .and. index(adjustl(segm),beginComBlk) == 1 )

         if ( is_comBlk ) then
            i = index(segm, endComBlk, back=.true.)
            is_comBlk = ( i == 0 )
            if ( .not. is_comBlk ) segm = segm(i+lendComBlk:)
            if ( is_comBlk .or. ( .not. is_comBlk .and. len_trim(segm) == 0 ) ) then
               if ( unit == STDIN ) return
               cycle
            end if        
         end if
      end if
      
      if ( present(firstl) .and. is_first ) then
         firstl = numl ! the first non-blank or non-commented # line
         is_first = .false.
      end if
!
!-    Remove possible end comments (i.e. from symbol "com" until the end of the line, if this
!     symbol is not between quotes or double quotes):
!
      if ( present(comments) ) then
         call util_FindSubstring1 ( loc, n, segm, 1_Ikind, len(segm,Ikind), com, stat )
         error_TraceNreturn(stat, HERE)      

         do i = 1, n
            if ( util_IsEnclosed ( segm, loc(i), "'", "'", stat ) .or. &
                 util_IsEnclosed ( segm, loc(i), '"', '"', stat ) ) then
               cycle
            else
               segm = segm(1:loc(i)-1)
               exit
            end if   
         end do
      end if         
!
!-    Remove possible trailing blanks:
!               
      if ( rm ) segm = trim(segm)
!
!-    See if the last 3 characters are the sequence "..." (indicating a line break)
!                
      last = '' ; pend = len(segm) ; plast = index(segm,'...',back=.true.)
      
      if ( plast /= 0 ) then
         if ( len_trim(segm(plast+3:)) == 0 ) then
            last = '...'
            pend = plast - 1
         end if
      end if
      
      Rec = Rec // segm(:pend)

      if ( present(Rec0) ) then
         if ( numl > 0 ) then
            Rec0 = Rec0 // NL // segm0
         else
            Rec0 = Rec0 // segm0
         end if
      end if
      
   end do  
!
!- Remove spaces and tabs from the resulting string (if not between quotes):
!   
   if ( rm ) Rec = trim( util_RemoveSpaces2 ( adjustl(Rec), opt='nqt', stat=flag ) )       

   END FUNCTION util_GetRecord0
   
   
!=============================================================================================
   FUNCTION util_GetRecord ( unit, stat, comments, contSymb, rmblk, nlines, firstl, Rec0 ) &
            result( Rec )
!=============================================================================================
   integer  (Ikind),                        intent(in    ) :: unit
   type     (err_t),                        intent(in out) :: stat 
   character(len=*), optional,              intent(in    ) :: comments(:)
   character(len=*), optional,              intent(in    ) :: contSymb(:)
   logical         , optional,              intent(in    ) :: rmblk
   integer  (Ikind), optional,              intent(   out) :: nlines, firstl
   character(len=:), optional, allocatable, intent(   out) :: Rec0   
   character(len=:),           allocatable                 :: Rec   
!---------------------------------------------------------------------------------------------
!  Reads a complete record (end-of-record terminated) from a file.
!
!  Inputs:
!  . unit               : unit file
!  . comments (optional): comments(1) is the symbol used for a comment, comments(2:3) are
!                         the pair of symbols used for a commented block, e.g. 
!                         comments(1) = '//', comments(2) = '/*', comments(3) = '*/'
!  . rmblk    (optional): spaces are removed if rmblk=.true.
!
!  Outputs:
!  . stat             : error handler
!  . nlines (optional): number of lines read
!  . firstl (optional): the first line # (non-blank or non-commented)
!  . rec              : the record
!-----------------------------------------------------------------------------------R.H. 12/16  

!- local variables --------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'util_GetRecord'
   character(len=:), allocatable :: segm0, segm, com, beginComBlk, endComBlk, contSymb_(:)
   integer  (Ikind), allocatable :: loc(:)
   integer  (Ikind)              :: plast, pend, i, n, numl, lsegm, lbeginComBlk, lendComBlk
   logical                       :: rm, is_first, continueRead
   type     (err_t)              :: flag
   logical         , SAVE        :: is_comBlk = .false.
   integer  (Ikind), allocatable :: lenSymb(:)
   integer  (Ikind)              :: nContSymb
!--------------------------------------------------------------------------------------------- 
   
   if ( stat%code > IZERO ) return   
!
!- Define the symbol used for comments:
!
   if ( present(comments) ) then
      com         = trim(adjustl(comments(1)))
      beginComBlk = trim(adjustl(comments(2)))
      endComBlk   = trim(adjustl(comments(3)))
      lbeginComBlk = len(beginComBlk)
      lendComBlk   = len(endComBlk)
   else
      lbeginComBlk = 0
      lendComBlk = 0
   end if
   
   if ( present(contSymb) ) then
      nContSymb = size(contSymb)
      allocate(lenSymb(nContSymb))
      contSymb_ = contSymb
      do i = 1, nContSymb
         lenSymb(i) = len_trim(contSymb_(i))
      end do
   else
      nContSymb = 0
   end if
!
!- Spaces will be removed (default) from "Rec" if "rm" is .true.:
!
   if ( present(rmblk) ) then
      rm = rmblk
   else
      rm = .true.
   end if         
         
   Rec = '' ; if ( present(Rec0) ) Rec0 = ''
         
   numl = 0 ; if ( present(nlines) ) nlines = 0
   
   is_first = .true. ; if ( present(firstl) ) firstl = 0
   
   if ( stat%code < IZERO ) stat = err_t()

   continueRead = .true.
      
   do while ( continueRead )      
!
!-    read a new line:
!            
      segm0 = util_GetLine ( unit, flag )

      segm = adjustl( segm0 ) 
      
      numl = numl + 1

      if ( present(nlines) ) nlines = numl
!
!-    return, if a problem has occurred: 
!          
      if ( flag > IZERO ) then
         call err_MoveAlloc ( from = flag, to = stat )
         call stat%AddTrace ( HERE )
         return
      end if
!
!-    cycle if it's a blank line or if the eof is reached: return if the read was on stdin, 
!     otherwise exit the loop:

      if ( len_trim(segm) == 0 ) then 
         if ( unit == STDIN ) return
         if ( flag%code < 0 ) then ! if EOF reached
            call err_MoveAlloc ( from = flag, to = stat ) 
            exit
         endif 
         cycle 
      end if  
!      
!-    cycle if it's a commented line (and return if the read was on stdin):
!
      if ( present(comments) ) then
         ! For a single commented line (starting with com):
         if ( index(adjustl(segm),com) == 1 ) then 
            if ( unit == STDIN ) return
            cycle
         end if       
      
         ! For a commented block (starting with beginComBlk and ending with endComBlk):
         if ( .not. is_comBlk ) &
            is_comBlk = ( lbeginComBlk > 0 .and. index(adjustl(segm),beginComBlk) == 1 )

         if ( is_comBlk ) then
            i = index(segm, endComBlk, back=.true.)
            is_comBlk = ( i == 0 )
            if ( .not. is_comBlk ) segm = segm(i+lendComBlk:)
            if ( is_comBlk .or. ( .not. is_comBlk .and. len_trim(segm) == 0 ) ) then
               if ( unit == STDIN ) return
               cycle
            end if        
         end if
      end if
      
      if ( present(firstl) .and. is_first ) then
         firstl = numl ! the first non-blank or non-commented # line
         is_first = .false.
      end if
!
!-    Remove possible end comments (i.e. from symbol "com" until the end of the line, if this
!     symbol is not between quotes or double quotes):
!
      if ( present(comments) ) then
         call util_FindSubstring1 ( loc, n, segm, 1_Ikind, len(segm,Ikind), com, stat )
         error_TraceNreturn(stat, HERE)      

         do i = 1, n
            if ( util_IsEnclosed ( segm, loc(i), "'", "'", stat ) .or. &
                 util_IsEnclosed ( segm, loc(i), '"', '"', stat ) ) then
               cycle
            else
               segm = segm(1:loc(i)-1)
               exit
            end if   
         end do
      end if         
!
!-    Remove possible trailing blanks:
!               
      if ( rm ) segm = trim(segm)
!
!-    Check for continuation line:
!           
      continueRead = .false.
      
      if ( nContSymb > 0 ) then
         pend = len(segm)
         do i = 1, ncontSymb
            plast = index(segm,contSymb_(i),back=.true.)
            if ( plast /= 0 ) then
               if ( len_trim(segm(plast+lensymb(i):)) == 0 ) then
                  continueRead = .true.
                  pend = plast - 1
                  exit
               end if
            end if
         end do
         Rec = Rec // segm(:pend)
      else
         Rec = Rec // segm
      end if
      
      if ( present(Rec0) ) then
         if ( numl > 0 ) then
            Rec0 = Rec0 // NL // segm0
         else
            Rec0 = Rec0 // segm0
         end if
      end if
      
   end do  
!
!- Remove spaces and tabs from the resulting string (if not between quotes):
!   
   if ( rm ) Rec = trim( util_RemoveSpaces2 ( adjustl(Rec), opt='nqt', stat=flag ) )       

   END FUNCTION util_GetRecord
   

!=============================================================================================
   SUBROUTINE util_ReadTextFile ( fileName, stat, Recs, comment, rmblk, nlines )
!=============================================================================================
   character(len=*),              intent(in    ) :: fileName
   type     (err_t),              intent(in out) :: stat   
   type     (str_t), allocatable, intent(   out) :: Recs(:)      
   character(len=*), optional,    intent(in    ) :: comment
   logical         , optional,    intent(in    ) :: rmblk
   integer  (Ikind), optional,    intent(   out) :: nlines
!---------------------------------------------------------------------------------------------
!  Reads the lines of a text file. The results are stored in the arrays recs.
!  - Parts of text to the right of the symbol "comment" are ignored.
!  - Empty lines are ignored.
!  - Spaces are removed if rmblk = .true.
!  - The number of lines actually read is given by nlines.
!-----------------------------------------------------------------------------------R.H. 12/16  

!- local variables --------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'util_ReadTextFile'
   integer  (Ikind), parameter   :: incremSize = 10
   integer  (Ikind)              :: currentSize = incremSize, newSize, usedSize, nlinesRead, n
   integer  (Ikind)              :: u, iostat, err
   character(len=LGSTR)          :: iomsg
   character(len=:), allocatable :: rec, com(:)
   type     (str_t), allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 

   if ( stat%code > IZERO ) return
       
   open ( newunit = u, file = fileName, action = 'read', iostat = iostat, iomsg = iomsg )

   if ( iostat /= 0 ) then
      stat = err_t ( stat = UERROR, where = HERE, msg = 'Unable to open the file "' // &
                     fileName // '"' // NLT // 'IOMSG: '//trim(iomsg) )
      return
   end if
   
   ! Allocate the array "recs" with an initial size of "currentSize":

   allocate(recs(currentSize), stat = err )

   if ( err /= 0 ) then
      stat = err_t(stat = IERROR, where = HERE, msg = 'Allocation failure for array "recs"')
      close(u)
      return
   end if
   
   if ( present(comment) ) then
      allocate(character(len=len_trim(comment))::com(3))
      com(1) = comment
   else 
      allocate(character(len=1)::com(3))
      com(1) = '#'
   end if
   com(2) = '' ; com(3) = ''
   
   usedSize = 0 ; nlinesRead = 0
   
   ! Start reading the file:
   
   do 
      ! Read a new record from the file
      ! - do not read the part of the line to the right of the symbol "com"
      ! - if rmblk = .false. => do not delete spaces (but ignore empty lines)
      
      rec = util_GetRecord ( unit = u, stat = stat, comments = com,  &
                             rmblk = rmblk, nlines = n, contSymb = ['...'] )
                             
      ! Terminate if an error occurred or if the end-of-file was reached:     
                      
      if ( stat /= IZERO ) exit

      ! Number of lines actually read (can be useful to signal an error during reading):
      
      nlinesRead = nlinesRead + n 
      
      ! Store "rec" in the array "recs" (first check that it is large enough): 
      
      usedSize = usedSize + 1       
      
      if ( usedSize > currentSize ) then
         ! The size of "recs" must be increased. Increase it by "incremSize":

         newSize = currentSize + incremSize

         allocate(tmp(newSize), stat=err)
         
         if ( err /= 0 ) then
            stat = err_t ( stat = IERROR, where = HERE, &
                            msg = 'Allocation failure for local array "tmp" (1)' )
            exit
         end if
            
         tmp(1:currentSize) = recs(1:currentSize)
         call move_alloc (from = tmp, to = recs)
         currentSize = newSize
      end if

      recs(usedSize)%str = rec
      
   end do
   
   if ( present(nlines) ) nlines = nlinesRead
   
   close(u)
   
   if ( stat /= IZERO ) then
      if ( stat /= EOF ) return
      stat = err_t()
   end if
      
   ! Resize the array "recs" to its useful size ("usedSize"):
   
   if ( usedSize < currentSize ) then
      allocate(tmp(usedSize), stat=err)
      
      if ( err /= 0 ) then
         stat = err_t ( stat = IERROR, where = HERE, &
                         msg = 'Allocation failure for local array "tmp" (2)' )
         return
      end if
      
      tmp(1:usedSize) = recs(1:usedSize)
      call move_alloc ( from = tmp, to = recs )
   end if
                                     
   END SUBROUTINE util_ReadTextFile
   
   
!=============================================================================================
   FUNCTION util_readStringBinFile ( lunit ) result( str )
!=============================================================================================
   integer  (Ikind), intent(in)  :: lunit
   character(len=:), allocatable :: str
!--------------------------------------------------------------------------------------------- 
!   Reads a string of an unknown length from an unformatted file
!
!   The file must be opened (logical unit: lunit) and the specifiers form and access must be
!   set to 'unformatted' and 'sequential' (not 'stream'), respectively.
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------   
   integer(Ikind) :: i, n, stat
!--------------------------------------------------------------------------------------------- 

   n = 100
   do 
      str = repeat(' ',n)
      read(lunit,iostat=stat) (str(i:i), i = 1, n)
      if ( stat /= 0 ) exit
      n = n * 10
      backspace(lunit)
   end do
   str = trim(str)
   
   END FUNCTION util_readStringBinFile
   

!=============================================================================================
   SUBROUTINE util_getLhsNRhs ( str, symb, lhs, rhs, pos )
!=============================================================================================
   character(len=*),              intent(in    ) :: str, symb
   character(len=:), allocatable, intent(in out) :: lhs, rhs
   integer  (Ikind), optional   , intent(in out) :: pos
!--------------------------------------------------------------------------------------------- 
!  Extracts from the string "str" the substrings "lhs" and "rhs" located at the left and at
!  the right of the substring "symb", respectively.
!  Optionally returns the position ("pos") of "symb" in "str".
!  If "symb" is not present in "str", it returns lhs = str and rhs = ''
!
!  Example:
!  .  if str is 'myvar = myvalue' and symb is '=', then lhs = 'myvar' and rhs = 'myvalue'
!  .  if str is 'a >= b' and symb is '>=', then lhs = 'a' and rhs = 'b'
!  .  if str is 'a + b' and symb is '=', then lhs = 'a + b' and rhs = ''  
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------   
   integer(Ikind) :: p, lsymb
!--------------------------------------------------------------------------------------------- 

   p = index(str,symb)
   
   if ( p > 0 ) then
      lsymb = len_trim(symb)
      lhs = trim(adjustl(str(:p-1))) ; rhs = trim(adjustl(str(p+lsymb:)))
   else
      lhs = trim(adjustl(str)) ; rhs = ''
   end if
   
   if (present(pos)) pos = p
   
   END SUBROUTINE util_getLhsNRhs
   
      
!=============================================================================================
   FUNCTION util_CountTokens (str, delims, BlkToken, stat, opcl, tokens, pos) result( ntokens )   
!=============================================================================================
   character(len=*),                        intent(in    ) :: str
   character(len=*),                        intent(in    ) :: delims
   logical         ,                        intent(in    ) :: BlkToken
   type     (err_t),                        intent(in out) :: stat   
   character(len=*),              optional, intent(in    ) :: opcl   
   type     (str_t), allocatable, optional, intent(   out) :: tokens(:,:)
   integer  (Ikind), allocatable, optional, intent(   out) :: pos(:)
   integer  (Ikind)                                        :: ntokens
!---------------------------------------------------------------------------------------------                   
!  Counts the number of tokens in the string "str" and optionally returns these tokens.
!
!  Inputs:
!  . str     : the string to parse.
!  . delims  : any set of 1-character delimiters (may be ' ').
!  . BlkToken: if .true. blanks between two delimiters are considered as tokens (except if
!              the delimiter is itself a blank space ' ').
!  . opcl    : (optional) an even-length string containing the opening/closing symbols.
!
!  Outputs:
!  . ntokens: the number of tokens found.
!  . tokens : (optional) the ntokens tokens stored in tokens(:,1) and the first delimiter
!             that follows each tokens stored in tokens(:,2) (possibly empty).
!  . pos    : (optional) the positions of the delimiters.
!  . stat   : error status and error message.
!
!  Notes: 
!  . Tokens are parts of "str" that are delimited by any 1-char symbol of "delims". 
!  . The occurence of one of these symbols that is enclosed between pair of symbols given in
!    in "opcl" is not considered as a delimiter.
!
!  Warnings: 
!  . Contiguous delimiters are counted as one occurence (see examples 7 to 9 below).
!  . For string tokens, the use of single quotation marks (') is not recommended, prefer the 
!    use of double quotation marks (").
!  
!  Examples: in these examples and for clarity, the beginning and the end of the resulting 
!            tokens are marked by the symbol '|'. In examples 1-9 only one delimiter (' ' or 
!            ',') is considered. Example 10 show the case with two delimiters (';,')
!
!  . 1) If str = ' 11  22    33   44   55   '  
!       - with delims = ' '
!   
!       Then ntokens = 5
!       and  tokens(:,1)%str = [ |11| |22| |33| |44| |55| ]
!            tokens(:,2)%str = [ | |  | |  | |  | |  | |  ]
!
!  . 2) If str = ' 11, 22,   33 , 44 , 55,  '  
!       - with delims = ',' 
!       - and BlkToken = .false.
!
!       Then ntokens = 5
!       and  tokens(:,1)%str = [ |11| |22| |33| |44| |55| ]
!            tokens(:,2)%str = [ |,|  |,|  |,|  |,|  |,|  ]
!
!  . 3) If str = ' 11, 22,   33 , 44 , 55,   '
!       - with delims = ',' 
!       - and BlkToken = .true.
!
!       Then ntokens = 6
!       and  tokens(:,1)%str = [ |11| |22| |33| |44| |55| |   |]
!            tokens(:,2)%str = [ |,|  |,|  |,|  |,|  |,|  |,|  ]
!
!  . 4) If str = ' a1  b2  "c is  3 "  "d is 4" e5  (1, 2) ' 
!       - with delims = ' ' 
!       - and opcl = '""()'
!
!       Then ntokens = 6
!       and  tokens(:,1)%str = [ |a1|  |b2|  |"c is 3 "|  |"d is 4"|  |e5|  |(1, 2)| ]
!            tokens(:,2)%str = [ | |   | |   | |          | |         | |   | |      ]
!
!  . 5) If str = ' 1 , 2.2 , ( 3, 3 ) , .true. , "array name" '
!       - with delims = ',' 
!       - and opcl = '""()'
!
!       Then ntokens = 5
!       and  tokens(:,1)%str = [ [1| |2.2|  |( 3, 3 )|  |.true.|  |"array name"| ]
!            tokens(:,2)%str = [ |,|  |,|   |,|         |,|       ||             ]

!  . 6) If str = ' "hello world!"    "John Steed"  "Emma Peel" ' 
!       - with delims = ' '
!       - opcl = '""'
! 
!       Then ntokens = 3
!       and  tokens(:,1)%str = [ |"hello world!"|  |"John Steed"|  |"Emma Peel"| ]
!            tokens(:,2)%str = [ | |               | |             ||            ]
!
!  . 7) If str = ' X1, X2 ,,,, X3, X4, X5 '
!       - with delims = ','
!
!       Then ntokens = 5
!       and  tokens(:,1)%str = [ |X1|  |X2|  |X3|  |X4|  |X5| ]
!            tokens(:,2)%str = [ |,|   |,|   |,|   |,|   ||   ]
!
!  . 8) If str = ' X1, X2 ,,  ,,, X3, X4, X5 ' 
!       - with delims = ',' 
!       - and BlkToken = .false.
!
!       Then ntokens = 5
!       and  tokens(:,1)%str = [ |X1|  |X2|  |X3|  |X4|  |X5| ]
!            tokens(:,2)%str = [ |,|   |,|   |,|   |,|   ||   ]
!
!  . 9) If str = ' X1, X2 ,,  ,,, X3, X4, X5 ' 
!       - with delims = ',' 
!       - and BlkToken = .true.
!
!       Then ntokens = 6
!       and  tokens(:,1)%str = [ |X1|  |X2|  |  |  |X3|  |X4|  |X5| ]
!            tokens(:,2)%str = [ |,|   |,|   |,|   |,|   |,|   ||   ]
!
!  .10) If str = 'a = 11 , A = read("foo.txt",0) ; b = a + 33,, c=44, d = [1,2;3,4]'
!       - with delims = ',;'
!       - and BlkToken = .false.  
!       - and opcl = '""()[]'
!
!       Then ntokens = 5 and
!       tokens(:,1)%str=[ |a = 11| |A = read("foo.txt",0)| |b = a + 33| |c=44|  |d=[1,2;3,4]| ]
!       tokens(:,2)%str=[ |,|      |;|                     |,|          |,|     ||            ]
!
!-----------------------------------------------------------------------------------R.H. 01/19                   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_CountTokens'
   character(len=1)              :: ch
   integer  (Ikind)              :: i, kpair, kop, kcl, lstr, nopcl, p1, p2, itok, ltok, &
                                    ndelim, err
   logical         , allocatable :: is_diff(:)
   integer  (Ikind), allocatable :: pdelim(:), numdel(:), opclstat(:)
!--------------------------------------------------------------------------------------------- 

   if ( stat%code > IZERO ) return
   
   ntokens = 0
               
   lstr = len(str)
!
!- Return with ntokens = 0 if the string "str" is empty:
!   
   if ( lstr == 0 ) then
      if ( present(tokens) ) allocate(tokens(0,0))
      if ( present(pos)    ) allocate(pos(0)     )
      return
   end if   
!
!- Check if the number of characters in "opcl" is an even number:
!
   nopcl = 0 ; if ( present(opcl) ) nopcl = len_trim(opcl)

   if ( modulo(nopcl,2_Ikind) /= 0 ) then
      stat = err_t ( stat = IERROR, where = HERE, msg = &
                     'Number of characters in opcl must be even (opcl='//trim(opcl)//')' )
      return
   end if      
!
!- number of pairs of open/close symbols
!   
   nopcl = nopcl / 2
!
!- Allocate the open/close status markers (is_open(k)=.true. if the k-th left symbol is met)
!  and allocate and set is_diff(k) = .false. if the same symbol is used in the k-th pair for 
!  opening and closing (e.g. " or ' or |):
!   
   allocate(is_diff (nopcl), source = .false., stat = err)  
   allocate(opclstat(nopcl), source = 0_Ikind, stat = err)  
   if ( err /= 0 ) then
      stat = err_t ( stat = IERROR, where = HERE,                        &
                      msg = "Allocation failure for is_diff or opclstat" ) 
      return
   end if      
      
   do kpair = 1, nopcl
      kcl = 2*kpair ; kop = kcl - 1
      if ( opcl(kop:kop) /= opcl(kcl:kcl) ) is_diff(kpair) = .true.
   end do  
!
!- Iniitialize the number of delimiters and their positions in "str":
!   
   ndelim = 0 ; allocate(pdelim(lstr), source = 0_Ikind, stat = err)
   if ( err /= 0 ) then
      stat = err_t (stat = IERROR, where = HERE, msg = "Allocation failure for pdelim") 
      return
   end if      
!
!- Find the positions of the delimiters that are not between open/close symbols:
!    
   p1 = 1
   do i = 1, lstr
      
      ch = str(i:i)

      if ( index(delims,ch) == 0 ) then
!
!-       "ch" is not a delimiter see if this is an open/close symbol, if yes set the status
!        accordingly: 
!            
         do kpair = 1, nopcl
            kcl = 2*kpair ; kop = kcl - 1
            if ( is_diff(kpair) ) then
               if ( ch == opcl(kop:kop) ) opclstat(kpair) = opclstat(kpair) + 1
               if ( ch == opcl(kcl:kcl) ) opclstat(kpair) = opclstat(kpair) - 1
            else
               if ( ch == opcl(kop:kop) ) then
                  if ( opclstat(kpair) == 1 ) then
                     opclstat(kpair) = 0
                  else
                     opclstat(kpair) = 1
                  end if
               end if
            end if               
         end do     
                
      else   
!
!-       If "ch" is not enclosed by an open/close pair ("opclstat" are all 0) then "ch" is 
!        a delimiter":
!
         if ( all(opclstat <= 0) ) then            
            p2 = i-1
            if ( p2 >= p1 ) then
!
!-             if str(p1:p2) is not empty or if blank token is allowed, increase "ntokens":
!      
               if ( len_trim(str(p1:p2)) /= 0 .or. BlkToken ) ntokens = ntokens + 1
                              
            end if   
            p1 = p2+2
!
!-          save its positions:
!            
            ndelim = ndelim + 1 ; pdelim(ndelim) = i
         end if   
      end if            
   end do
   
   if ( any(opclstat /= 0) ) then
      stat = err_t ( stat = WARNING, where = HERE, &
                      msg = "At least one of the pairs of symbols << " // opcl //  &
                           " >> is unbalanced in the expression << "//str//" >>" )
   end if   
!
!- See if the remaining sub-string after the last delimiter is a blank text or not. If not, 
!  this is a token. Else, this is a (blank) token if not empty and allowed by the caller:
!     
   if ( len_trim(str(p1:)) /= 0 .or. (len(str(p1:)) /= 0 .and. BlkToken) ) &
      ntokens = ntokens + 1 
!
!- If requested, form the ntokens tokens:
!         
   if ( present(tokens) ) then
   
      allocate(tokens(ntokens,2),                 stat = err)
      allocate(numdel(ntokens  ), source = IZERO, stat = err)
      
      if ( err /= 0 ) then
         stat = err_t ( stat = IERROR, where = HERE,                     &
                         msg = "Allocation failure for tokens or numdel" ) 
         return
      end if      
      
      itok = 0
      p1 = 1
      do i = 1, ndelim
         p2 = pdelim(i)-1
         if ( p2 >= p1 ) then
            ltok = len_trim(str(p1:p2))
            if ( ltok /= 0 .or. BlkToken ) then
               itok = itok + 1
               if ( ltok == 0 ) then
                  tokens(itok,1)%str = adjustl(str(p1:p2))
               else
                  tokens(itok,1)%str = trim(adjustl(str(p1:p2)))
               end if
               numdel(itok) = i
            end if   
         end if   
         p1 = p2 + 2     
      end do  
      ltok = len_trim(str(p1:))
      if ( ltok /= 0 .or. (len(str(p1:)) /= 0 .and. BlkToken) ) then
         itok = itok + 1
         if ( ltok == 0 ) then
            tokens(itok,1)%str = adjustl(str(p1:))
         else   
            tokens(itok,1)%str = trim(adjustl(str(p1:)))
         end if
         if ( itok > 1 ) then
            i = numdel(itok-1)
            if ( i < ndelim ) numdel(itok) = ndelim
         end if   
      end if     
!
!-    save the first delimiter that follows each tokens:
!      
      do itok = 1, ntokens
         i = numdel(itok)
         if ( i /= 0 ) then    
            tokens(itok,2)%str = str(pdelim(i):pdelim(i))
         else
            tokens(itok,2)%str = ''
         end if
      end do
               
   end if   
!
!- If requested, save the positions of the delimiters:
!
   if ( present(pos) ) then
      if ( ndelim /= 0 ) then
         allocate(pos(ndelim), source = pdelim(1:ndelim))
      else
         allocate(pos(0))
      end if
   end if   
         
   END FUNCTION util_CountTokens

            
!=============================================================================================
   SUBROUTINE util_FindSubstring1 ( loc, n, str, lind, rind, substr, stat )
!=============================================================================================  
   integer  (Ikind),              intent(in    ) :: lind, rind
   character(len=*),              intent(in    ) :: str, substr
   integer  (Ikind),              intent(   out) :: n   
   integer  (Ikind), allocatable, intent(   out) :: loc(:)
   type     (err_t),              intent(in out) :: stat   
!--------------------------------------------------------------------------------------------- 
!  Finds in the string "str" and between indices "lind" and "rind", the occurences (n) of the 
!  substring "substr". Stores its different locations in the array "loc". 
!-----------------------------------------------------------------------------------R.H. 01/18 

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter   :: HERE = 'util_FindSubstring1'
   integer  (Ikind )              :: err, len_substr, len_str, p, lind1
   integer  (Ikind ), allocatable :: tmp(:)
   character(len=20)              :: lcnum, rcnum
!--------------------------------------------------------------------------------------------- 

   if ( stat%code > IZERO ) return
   
   n = 0 ; err = 0

   len_str = len(str) ; len_substr = len(substr)   
   
   if ( len_str == 0 .or. len_substr == 0 ) return

   if ( lind > rind ) then
      write(lcnum,'(i0)')lind ; write(rcnum,'(i0)')rind
      stat = err_t ( stat = UERROR, where = HERE,                &
                     msg = 'Left index (= '           //         &
                     trim(lcnum) // ') > Right index (= ' //     &
                     trim(rcnum) // ') for str = ' // trim(str)  )
      return
   end if
   
   if ( rind < len_str ) len_str = rind
   
   allocate(loc(len_str - lind + 1), source=0_Ikind, stat=err)
      
   if ( err /= 0 ) then
      stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for array "loc"')
      return  
   end if   

   lind1 = lind
   do while ( lind1 <= len_str )
      p = index(str(lind1:len_str),substr)
      if ( p == 0 ) exit
      n = n + 1
      loc(n) = lind1 + p - 1
      lind1 = loc(n) + len_substr
   end do
   
   allocate(tmp(n), stat=err)
   
   if ( err /= 0 ) then
      stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for array "tmp"')
      return   
   end if   
   
   tmp = loc(1:n) ; call move_alloc(from=tmp, to=loc)  
            
   END SUBROUTINE util_FindSubstring1
   

!=============================================================================================
   SUBROUTINE util_FindSubstring2 ( loc, n, str, lind, rind, substr, opcl, stat )
!=============================================================================================  
   integer  (Ikind),              intent(in    ) :: lind, rind
   character(len=*),              intent(in    ) :: str, substr, opcl
   integer  (Ikind),              intent(   out) :: n   
   integer  (Ikind), allocatable, intent(   out) :: loc(:)
   type     (err_t),              intent(in out) :: stat   
!--------------------------------------------------------------------------------------------- 
!  Same as util_FindSubstring1 except that the locations of "substr" surrounded by a pair of 
!  symbols given in "opcl" are not taken into account.
!-----------------------------------------------------------------------------------R.H. 03/19 

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter   :: HERE = 'util_FindSubstring2'
   integer  (Ikind )              :: err, len_substr, len_str, p, lind1, k, k1, k2, pos, lopcl
   integer  (Ikind ), allocatable :: tmp(:)
   character(len=20)              :: lcnum, rcnum
   logical                        :: is_enclosed
!--------------------------------------------------------------------------------------------- 

   if ( stat%code > IZERO ) return
   
   lopcl = len_trim(opcl)
   
   if ( modulo(lopcl,2_Ikind) /= 0 ) then
      stat = err_t ( stat = IERROR, where = HERE, msg =                                  &
                     'Number of characters in opcl must be even (opcl='//trim(opcl)//')' )
      return
   end if      
      
   lopcl = lopcl / 2      
   
   n = 0 ; err = 0

   len_str = len(str) ; len_substr = len(substr)   
   
   if ( len_str == 0 .or. len_substr == 0 ) return

   if ( lind > rind ) then
      write(lcnum,'(i0)')lind ; write(rcnum,'(i0)')rind
      stat = err_t ( stat = UERROR, where = HERE,                &
                     msg = ' Left index (= ' //                  &
                     trim(lcnum) // ') > Right index (= ' //     &
                     trim(rcnum) // ') for str = ' // trim(str) )
      return
   end if
   
   if ( rind < len_str ) len_str = rind
   
   allocate(loc(len_str - lind + 1), source=0_Ikind, stat=err)
      
   if ( err /= 0 ) then
      stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for array "loc"')
      return  
   end if      

   lind1 = lind
   do while ( lind1 <= len_str )
   
      p = index(str(lind1:len_str),substr) ! position in the substring 
      
      if ( p == 0 ) exit

      pos = lind1 + p - 1 ! position in the entire string "str"

      is_enclosed = .false.
      do k = 1, lopcl
         k2 = 2*k ; k1 = k2 - 1
         is_enclosed = util_IsEnclosed (str, pos, opcl(k1:k1), opcl(k2:k2), stat)
         error_TraceNreturn(stat, HERE) 
         if ( is_enclosed ) exit
      end do        
      
      if ( .not. is_enclosed ) then      
         n = n + 1
         loc(n) = pos
      end if
         
      lind1 = pos + len_substr ! the next substring to examine start at lind1
   end do
   
   allocate(tmp(n), stat=err)
   
   if ( err /= 0 ) then
      stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for array "tmp"')
      return   
   end if   
   
   tmp = loc(1:n) ; call move_alloc(from=tmp, to=loc)  
            
   END SUBROUTINE util_FindSubstring2            


!=============================================================================================
   FUNCTION util_ReplaceSubstring1 ( str, remove, replace, nrep ) result ( newstr )
!=============================================================================================
   character(len=*),             intent(in    ) :: str, remove, replace
   character(len=:), allocatable                :: newstr
   integer  (Ikind), optional,   intent(   out) :: nrep
!---------------------------------------------------------------------------------------------                   
!   Replaces all the occurences of the substring "remove" by substring "replace" in the string
!   "str"
!
!   Inputs:
!    . str    : the original string 
!    . remove : the substring to remove
!    . replace: the substring to insert in place of remove
!
!   Outputs:
!    . newtr: the new string
!    . nrep : (optional) number of replacements done
!
!   External procedures: none
!
!   Notes: 
!
!   . unused trailing spaces are assumed to be removed from "str", "remove" and "replace".
!
!   . the simple solution 
!
!              newstr = adjustl(str) ; lrm = len(remove)
!      
!              p = index(newstr,remove)
!              do while (p /= 0) 
!                 newstr = newstr(1:p-1) // replace // trim(newstr(p+lrm:))
!                 p = index(newstr,remove)
!              end do
!
!     can not be used if the substring "replace" to insert in place of "remove" contains
!     "remove", e.g.: str = 'z+sin(x)*y', remove = 'sin(', replace = 'sind('. Indeed, it
!     will produce in this case an infinite loop. I propose the solution below
! 
!-----------------------------------------------------------------------------------R.H. 01/18                   

!- local variables: --------------------------------------------------------------------------
   integer(Ikind) :: p, p1, p2, lold, lrm, n
!---------------------------------------------------------------------------------------------
   
   lold = len(str)       ! length of the original string
   lrm  = len(remove)    ! length of the substring to remove from "str"
   
   if ( lrm > 0 ) then   
      p = index(str,remove) ! position of the first occurence of "remove"
   else
      p = 0
   end if      
   
   if ( p == 0 ) then
      if ( present(nrep) ) nrep = 0
      newstr = (str) ! adjustl(trim(str))
      return ! if not present, exit with newstr = str
   else
      newstr = ''
   end if   
   
   p1 = 1 ; n = 0
   do while ( p /= 0 ) 
      n = n + 1
      p2 = p1 + p - 2
      newstr = newstr // str(p1 : p2) // replace
      p1 = p1 + p + lrm - 1
      if ( p1 > lold ) exit
      p = index(str(p1:),remove)
   end do
   
   if ( p1 <= lold ) newstr = newstr // str(p1:lold)
   
   if ( present(nrep) ) nrep = n
   
   END FUNCTION util_ReplaceSubstring1


!=============================================================================================
   FUNCTION util_ReplaceSubstring2 ( str, remove, replace, opcl, stat, nrep ) result ( newstr )
!=============================================================================================
   character(len=*),             intent(in    ) :: str, remove, replace
   character(len=*),             intent(in    ) :: opcl
   type     (err_t),             intent(in out) :: stat    
   integer  (Ikind), optional,   intent(   out) :: nrep     
   character(len=:), allocatable                :: newstr
!---------------------------------------------------------------------------------------------
!   Replaces any occurence of the substring "remove" by substring "replace" in the input
!   string "str" under the condition that this occurence is not enclosed by one of the
!   opening/closing symbol (1 char.) pairs given in opcl (i.e. "remove" is not replaced if it
!   appears between the two symbols opcl(2*i-1:2*i-1) and opcl(2*i:2*i) for at least one index 
!   i in {1,..,len(opcl)/2})
!
!   Inputs:
!    . str    : the original string
!    . remove : the substring to remove
!    . replace: the substring to insert in place of remove
!    . opcl   : a 0-length or an even-length string containing the opening/closing symbols
!    . stat   : status error object
!
!   Outputs:
!    . stat : status error object
!    . nrep : (optional) number of replacements done
!    . newtr: the new string
!
!   External procedures:
!    . util_ReplaceSubstring1
!    . util_FindSubstring1
!    . util_IsEnclosed
!
!   Examples:
!    . replacesubstr2 ( 'sin(x)', 'sin', 'asin', '' ) --> 'asin(x)'
!
!    . replacesubstr2 ( 'x=[1;2;3]; y=5', ';', ',', '[]' ) -->  'x=[1;2;3], y=5'
!        (the ";" are still present inside [ ] but not outside)
!
!    . replacesubstr2 ( ' "a*b" * c {**} d', '*', '{*}', '{}""' ) --> ' "a*b" {*} c {**} d'
!        (* is replaced by {*} but not inside " " and not inside the existing { })
!-----------------------------------------------------------------------------------R.H. 01/18                   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_ReplaceSubstring2'
   integer  (Ikind)              :: i, p, q, lold, lrm, nloc, lopcl, k, k1, k2, n
   logical                       :: is_enclosed
   integer  (Ikind), allocatable :: loc(:)   
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return

   if ( present(nrep) ) nrep = 0 

   lrm = len(remove) ! length of the substring to remove from "string"
   
   if ( lrm == 0 ) then
      newstr = (str) ! adjustl(trim(str))
      return
   end if
   
   lopcl = len_trim(opcl)
   
   if ( lopcl == 0 ) then
      newstr = util_ReplaceSubstring1 ( str, remove, replace, nrep )
      return
   end if
     
   if ( modulo(lopcl,2_Ikind) /= 0 ) then
      stat = err_t ( stat = IERROR, where = HERe, msg =                                  &
                     'Number of characters in opcl must be even (opcl='//trim(opcl)//')' )
      return
   end if      
      
   lopcl = lopcl / 2   

   lold = len(str)    ! length of the original string      
      
   call util_FindSubstring1 (loc, nloc, str, 1_Ikind, lold, remove, stat)
   
   error_TraceNreturn(stat, HERE) 
   
   if ( nloc == 0 ) then
      newstr = (str) ! adjustl(trim(str))
      return
   end if
   
   newstr = ''
   n = 0
   p = -lrm+1
   
   do i = 1, nloc
      
      q = loc(i)
            
      is_enclosed = .false.
      do k = 1, lopcl
         k2 = 2*k ; k1 = k2 - 1
         is_enclosed = util_IsEnclosed (str, q, opcl(k1:k1), opcl(k2:k2), stat)
         error_TraceNreturn(stat, HERE) 
         if ( is_enclosed ) exit
      end do        
      
      if ( is_enclosed ) then
         newstr = newstr // str(p+lrm:q+lrm-1)   
      else   
         newstr = newstr // str(p+lrm:q-1) // replace
         n = n + 1
      end if
      p = q
   end do      
            
   if ( q <= lold ) newstr = newstr // str(q+lrm:lold)
   
   if ( present(nrep) ) nrep = n
   
   END FUNCTION util_ReplaceSubstring2


!=============================================================================================
   FUNCTION util_IsBalanced1 ( str, op, cl ) result ( is_balanced )
!=============================================================================================
   character(len=*), intent(in    ) :: str
   character(len=1), intent(in    ) :: op, cl
   logical                          :: is_balanced
!--------------------------------------------------------------------------------------------- 
!  Determines whether the opening/closing symbols "op" and "cl" (in this order) are balanced
!  in the string "str".
!
!   Inputs:
!    . str: the string to analyze
!    . op : opening symbol (1-char)
!    . cl : closing symbol (1-char)
!
!   Outputs:
!    . is_balanced: is .true. if the symbols are balanced
!
!   External procedures: none
!
!   Notes:
!    . Source: adapted (slightly modified) from rosettacode.org/wiki 
!    . If op and cl are identical (e.g. op=cl=" or op=cl='), is_balanced = .true. if they are
!      in even numbers 
!-----------------------------------------------------------------------------------R.H. 02/18 

!- local variables ---------------------------------------------------------------------------    
   integer(Ikind) :: i, a
!--------------------------------------------------------------------------------------------- 
    
   is_balanced = .true.
   
   a = 0   
   if ( op == cl)  then
      do i = 1, len_trim(str)
         if (str(i:i) == op) a = a + 1
      end do
      if ( modulo(a,2_Ikind) /= 0 ) is_balanced = .false.
   else
      do i = 1, len_trim(str)
         if ( str(i:i) == op ) a = a + 1
         if ( str(i:i) == cl ) a = a - 1
         is_balanced = is_balanced .and. (a >= 0)
      end do
      is_balanced = is_balanced .and. (a == 0)
   end if

   END FUNCTION util_IsBalanced1


!=============================================================================================
   FUNCTION util_IsBalanced2 ( str, opcl, stat ) result ( is_balanced )
!=============================================================================================
   character(len=*), intent(in    ) :: str, opcl
   type     (err_t), intent(in out) :: stat      
   logical                          :: is_balanced
!--------------------------------------------------------------------------------------------- 
!  Same as util_IsBalanced1 but for a set of pairs of opening/closing symbols given in the
!  string "opcl" (the pairs are [opcl(1:1), opcl(2:2)], [opcl(3:3), opcl(4:4)], and so on).
!
!   Inputs:
!    . str : the string to analyze
!    . opcl: a 0-length or an even-length string containing the opening/closing symbols
!
!   Outputs:
!    . is_balanced: is .true. if the symbols are balanced
!    . stat       : status error object
!
!   External procedures:
!    . util_IsBalanced1
!-----------------------------------------------------------------------------------R.H. 02/18 

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_IsBalanced2' 
   integer  (Ikind)              :: j, p1, p2, lopcl
!--------------------------------------------------------------------------------------------- 

   if ( stat%code > IZERO ) return

   is_balanced = .true.
   
   lopcl = len_trim(opcl)
   
   if ( lopcl == 0 ) return
      
   if ( modulo(lopcl,2_Ikind) /= 0 ) then
      stat = err_t ( stat = IERROR, where = HERE, msg =                                   &
                     ' Number of characters in opcl must be even (opcl='//trim(opcl)//')' )
      return
   end if   
      
   lopcl = lopcl / 2

   do j = 1, lopcl
      p2 = 2*j ; p1 = p2 - 1
      is_balanced = is_balanced .and. util_IsBalanced1 ( str, opcl(p1:p1), opcl(p2:p2) )
   end do   
      
   END FUNCTION util_IsBalanced2


!=============================================================================================
   FUNCTION util_IsEnclosed ( str, p, substr1, substr2, stat ) result ( is_enclosed )
!=============================================================================================
   character(len=*), intent(in    ) :: str, substr1, substr2
   integer  (Ikind), intent(in    ) :: p
   type     (err_t), intent(in out) :: stat   
   logical                          :: is_enclosed
!--------------------------------------------------------------------------------------------- 
!  See if the "p" th (p > 1) character of the string "str" is enclosed between substrings 
!  "substr1" and "substr2"
!
!   Inputs:
!    . str             : the string
!    . p               : the position in "str" of the character to be analysed
!    . substr1, substr2: the two substrings 
!
!   Outputs:
!    . is_enclosed: is .true. if the p-th character is enclosed between "substr1" and "substr2"
!    . stat       : status error object
!
!   External procedures:
!    . util_FindSubstring1
!
!   Examples:
!
!        1)   str = ((a(b)c)d(e)f)                 2)  str = --abcdef***
!                           |                                     |
!                          p=9                                   p=6
!
!             substr1 = (                             substr1 = --
!             substr2 = )                             substr2 = ***
!
!             is_enclosed = .true.                    is_enclosed = .true.
!
!        3)   str = "abc" + "ef"                   4)  str = "abc + ef"
!                         |                                       |
!                        p=7                                     p=6
!
!             substr1 = substr2 = "                    substr1 = substr2 = " 
!
!             is_enclosed = .false.                    is_enclosed = .true.
!-----------------------------------------------------------------------------------R.H. 01/18 

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_IsEnclosed'
   character(len=:), allocatable :: left, right, s1, s2
   integer  (Ikind), allocatable :: loc(:)
   integer  (Ikind)              :: lleft , nleft1 , nleft2 , nleft
   integer  (Ikind)              :: lright, nright1, nright2, nright
!--------------------------------------------------------------------------------------------- 

   if ( stat%code > IZERO ) return
      
   is_enclosed = .false.
   
   if ( p <= 1 ) return
   
   s1 = trim(adjustl(substr1)) ; s2 = trim(adjustl(substr2))
!
!- occurence of "substr1" in the left part of the "str":
!   
   left = trim(adjustl(str(:p-1))) ; lleft = len_trim(left)  

   call util_FindSubstring1 (loc, nleft1, left, 1_Ikind, lleft, s1, stat)   

   error_TraceNreturn(stat, HERE) 
!
!- occurence of "substr1" in the right part of the "str":
!  
   right = trim(adjustl(str(p+1:))) ; lright = len_trim(right)  

   call util_FindSubstring1 (loc, nright1, right, 1_Ikind, lright, s1, stat)  
   
   error_TraceNreturn(stat, HERE) 
!
!- case substr1 = substr2:
!
   if ( s1 == s2 ) then
     
      if ( modulo(nleft1,2_Ikind) /= 0 .and. modulo(nright1,2_Ikind) /= 0 ) &
         is_enclosed = .true.
!
!- case string1 <> string2:
!  
   else      
!
!-    occurence of "substr2" in the left part of the "str":
!            
      call util_FindSubstring1 (loc, nleft2, left, 1_Ikind, lleft, s2, stat)
      
      error_TraceNreturn(stat, HERE) 
      
      nleft = nleft1 - nleft2
!
!-    occurence of "substr2" in the right part of the "str":
!      
      call util_FindSubstring1 (loc, nright2, right, 1_Ikind, lright, s2, stat)
      
      error_TraceNreturn(stat, HERE) 

      nright = nright1 - nright2      
   
      if (nleft == -nright .and. nleft > 0) is_enclosed = .true.
   end if
   
   if ( allocated(loc) ) deallocate(loc)
         
   END FUNCTION util_IsEnclosed


!=============================================================================================
   SUBROUTINE util_SplitString1 ( Rec, delim, opcl, subRec, stat, nsubrec )
!=============================================================================================
   character(len=*),              intent(in    ) :: Rec, delim, opcl
   type     (str_t), allocatable, intent(in out) :: subRec(:)
   type     (err_t),              intent(in out) :: stat   
   integer  (Ikind),              intent(   out) :: nsubrec      
!---------------------------------------------------------------------------------------------
!  Splits the record "Rec" if formed of multiple parts separated by the delimiter "delim".
!  Occurences of "delim" between two consecutive symbols defined in "opcl" are not considered
!  as delimiters.
!
!  Inputs:
!     . Rec  : the string to split
!     . delim: the delimiter symbol (e.g. "," or ";")
!     . opcl : a string formed by the pairs of symbols in which "delim" are not considered
!              (e.g. "[]()"). The length of opcl must be an even number.
!
!  Inputs/Output:
!     . subRec : the list of substrings (subRec(i)%str)
!     . stat   : status error object
!
!  Output:
!     . nsubrec: the number of substrings
!
!  CAUTION: subRec (if allocated) is not re-sized if its initial size is greater than nsubrec,
!           so the caller must use nsubrec and NOT size(subRec)
!
!  External procedures:
!     . util_FindSubstring1
!     . util_IsEnclosed
!
!  Examples:
!         1) the delimiter is ";" and opcl the string formed by the opening/closing pairs 
!           " ", ' ' and[ ], that is :  opcl = ""''[]
!
!           Then if the record is the string
!           
!                x = 4 ; names = ['camef2018' ; 'adeli3p8' ; 'other'] ; pi = acos(-1)
!
!           it will be splitted in 3 parts
!
!           subRec(1)%str = "x = 4"
!           subRec(2)%str = "names = ['camef2018' ; 'adeli3p8' ; 'other']"
!           subRec(3)%str = "pi = acos(-1)"
!
!        2) the delimiter is "," and opcl = ""''()[]
!
!           Then if the record is the string
!
!               a = myfile, b = [1,2;3,4], c = mesh(file=dmesh, fmt='gmsh') 
!
!           it will be splitted in 3 parts
!
!           subRec(1)%str = "a = myfile"
!           subRec(2)%str = "b = [1,2;3,4]"
!           subRec(3)%str = "c = mesh(file=dmesh, fmt='gmsh')"
!-----------------------------------------------------------------------------------R.H. 02/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_SplitString1'  
   integer  (Ikind)              :: i, j, n, p1, p2, lopcl, err, lrec
   integer  (Ikind), allocatable :: loc(:), sep(:)
   logical                       :: is_enclosed
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return
   
   nsubrec = 0
   
   err = 0
   
   lrec = len_trim(Rec)
   
   if ( lrec == 0 ) then
      nsubrec = 1
      if ( allocated(subrec) ) then
         if ( size(subrec) < nsubrec ) deallocate(subrec)
      end if
      if ( .not. allocated(subrec) ) allocate(subrec(nsubrec))
      subrec(1)%str = ''
      return
   end if   
   
   lopcl = len_trim(opcl)

   if ( modulo(lopcl,2_Ikind) /= 0 ) then
      stat = err_t ( stat = IERROR, where = HERE, msg =                                   &
                     ' Number of characters in opcl must be even (opcl='//trim(opcl)//')' )
      return
   end if      
      
   lopcl = lopcl / 2
!
!- localize all occurences of "delim" in the record "Rec":
!
   call util_FindSubstring1 (loc, n, trim(Rec), 1_Ikind, lrec, delim, stat)
   
   error_TraceNreturn(stat, HERE) 
   
   if ( n > 0 ) allocate(sep(n), source=0_Ikind, stat=err)
   
   if ( err /= 0 ) then
      stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for array "sep"')
      return
   end if   
!
!- for each of them, see if it is enclosed by one of the pairs given in opcl.
!  If not, store its position in "sep"
!
   do i = 1, n
   
      is_enclosed = .false.
      do j = 1, lopcl
         p2 = 2*j ; p1 = p2 - 1
         is_enclosed = is_enclosed .or. &
                  util_IsEnclosed ( trim(Rec), loc(i), opcl(p1:p1), opcl(p2:p2), stat ) 
         error_TraceNreturn(stat, HERE) 
      end do    
   
      if ( is_enclosed ) cycle
         
      nsubrec = nsubrec + 1
      sep(nsubrec) = loc(i)
   end do
   nsubrec = nsubrec + 1
!
!- Allocate (or re-allocate subrec only if its size is less than nsubrec):
!   
   if ( allocated(subrec) ) then
      if ( size(subrec) < nsubrec ) deallocate(subrec)
   end if
   if ( .not. allocated(subrec) ) then
      allocate(subrec(nsubrec),stat=err)
      if ( err /= 0 ) then
         stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for "subrec"')
         return
      end if
   end if   
!
!- then split the record "Rec" in "nsubrec" pieces:
!     
   p1 = 1
   do i = 1, nsubrec
      if ( i < nsubrec ) then
         p2 = sep(i)-1
      else
         p2 = lrec
      end if
      subrec(i)%str = rec(p1:p2) 
      p1 = p2 + 2     
   end do  
      
   END SUBROUTINE util_SplitString1


!=============================================================================================
   SUBROUTINE util_SplitString2 ( Rec, delims, opcl, subRec, locsep, stat, nsubRec )
!=============================================================================================
   character(len=*),              intent(in    ) :: Rec, opcl, delims
   type     (str_t), allocatable, intent(in out) :: subRec(:)
   integer  (Ikind), allocatable, intent(in out) :: locsep(:)
   type     (err_t),              intent(in out) :: stat  
   integer  (Ikind)             , intent(   out) :: nsubRec       
!---------------------------------------------------------------------------------------------
!  Same as util_SpliString but for a set of delimiters (of 1 char length) given in "delims"
!  (e.g. delims = ',;')
!
!  CAUTION: subRec and locsep (if allocated) are not re-sized if their initial sizes are 
!           greater than nsubrec, so the caller must use nsubrec and NOT size(subRec) or
!           size(locsep)
!
!-----------------------------------------------------------------------------------R.H. 02/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'util_SplitString2'  
   integer  (Ikind)              :: i, j, p1, p2, lopcl, err, lrec
   integer  (Ikind), allocatable :: sep(:)
   logical                       :: is_enclosed
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return

   nsubrec = 0
   
   err = 0
    
   lrec = len_trim(Rec)
   
   if ( lrec == 0 ) then
      nsubrec = 1
      if ( allocated(subrec) ) then
         if ( size(subrec) < nsubrec ) deallocate(subrec)
      end if
      if ( .not. allocated(subrec) ) allocate(subrec(nsubrec))
      SubRec(1)%str = ''
      return
   end if   
   
   lopcl = len_trim(opcl)

   if ( modulo(lopcl,2_Ikind) /= 0 ) then
      stat = err_t ( stat = IERROR, where = HERe, msg =                                   &
                     ' Number of characters in opcl must be even (opcl='//trim(opcl)//')' )
      return
   end if      
      
   lopcl = lopcl / 2
!
!- localize all occurences of "delims(i:i)" in the record "Rec" that are not enclosed by one
!  of the pairs given in "opcl". Store these positions in "sep":
!
   allocate(sep(lrec), source = 0_Ikind, stat = err)

   if ( err /= 0 ) then
      stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for array "sep"')
      return
   end if   

   nsubrec = 0 
   do i = 1, lrec
      if ( index(delims,Rec(i:i)) /= 0 ) then
         is_enclosed = .false.
         do j = 1, lopcl
            p2 = 2*j ; p1 = p2 - 1
            is_enclosed = is_enclosed .or. &
                       util_IsEnclosed ( trim(Rec), i, opcl(p1:p1), opcl(p2:p2), stat ) 
            error_TraceNreturn(stat, HERE) 
         end do    
         
         if ( is_enclosed ) cycle
         
         nsubrec = nsubrec + 1
         sep(nsubrec) = i         
      end if   
   end do
   nsubrec = nsubrec + 1
!
!- Allocate subrec and locsep or reallocate them (only if their sizes are less than nsubrec);
!   
   if ( allocated(subrec) ) then
      if ( size(subrec) < nsubrec ) deallocate(subrec)
   end if
   if ( .not. allocated(subrec) ) then
      allocate(subrec(nsubrec),stat=err)
      if ( err /= 0 ) then
         stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for "subrec"')
         return
      end if
   end if   

   if ( allocated(locsep) ) then
      if ( size(locsep) < nsubrec ) deallocate(locsep)
   end if
   if ( .not. allocated(locsep) ) then
      allocate(locsep(nsubrec),source = 0_Ikind,stat=err)
      if ( err /= 0 ) then
         stat = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure for "locsep"')
         return
      end if
   else
      locsep = 0_Ikind
   end if   
!
!- Split now the record "Rec" in "nsubrec" pieces:
!            
   p1 = 1
   j = 0
   do i = 1, nsubrec
      subrec(i)%str = ''
      if ( i < nsubrec ) then
         p2 = sep(i)-1
      else
         p2 = lrec
      end if
      !if (len_trim(rec(p1:p2)) /= 0) then  ! commented 03/04/19
         j = j + 1
         subrec(j)%str = rec(p1:p2)
         locsep(j) = sep(i)
      !end if    
      p1 = p2 + 2     
   end do  
   nsubrec = j
         
   END SUBROUTINE util_SplitString2
   
   
!=============================================================================================
   FUNCTION util_FirstOccurence ( str, chars, sopen, sclose, stat, back ) result ( pos )
!=============================================================================================   
   character(len=*),           intent(in    ) :: str, chars, sopen, sclose
   type     (err_t),           intent(   out) :: stat  
   logical         , optional, intent(in    ) :: back
   integer  (Ikind)                           :: pos
!--------------------------------------------------------------------------------------------- 
!  This function returns the position in the string "str" of the first occurence of any of the
!  characters given in "chars" that are not enclosed between symbols "sopen" (at left) and 
!  "sclose" (at right). It returns 0 if none of these characters are present in "str"
!
!  If "back" is present and true, it returns the last occurrence rather than the first.
!
!    
!  External procedures:
!     . util_IsEnclosed
!
!
!  Examples:
!   
!     Let  chars = '+-'   and   sopen = '(',   sclose = ')'
!
!     -------------------------------------------|--------------------------------------------  
!                 with back = .false.            |            with back = .true.
!     -------------------------------------------|--------------------------------------------  
!     1)        str = '(a + b) - (c + f) + g'    |     str = '(a + b) - (c + f) + g'
!                              |                 |                              | 
!                           pos = 9              |                          pos = 19 
!     -------------------------------------------|--------------------------------------------  
!     2)        str = '((a+b)-(c+f)) + g'        |     str = '((a+b)-(c+f)) + g'
!                                    |           |                          |
!                                pos = 15        |                      pos = 15
!     -------------------------------------------|--------------------------------------------  
! 
!  Note: if no enclosing condition is required, simply set sopen = sclose = '' (empty string)
!
!-----------------------------------------------------------------------------------R.H. 06/18
   
!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter   :: HERE = 'util_FirstOccurence'
   integer  (Ikind), allocatable :: charsloc(:)
   logical         , allocatable :: is_enclosed(:)
   logical                       :: lback
   integer  (Ikind)              :: i, nloc, lstr, i1, i2, i3, err
!---------------------------------------------------------------------------------------------    

   if ( stat%code > IZERO ) return

   pos = 0
         
   lback = .false.
   if ( present(back) ) lback = back

   lstr = len_trim(str)

   nloc = 0
   do i = 1, lstr
      if ( index(chars,str(i:i)) /= 0 ) nloc = nloc + 1
   end do
   
   allocate(is_enclosed(nloc), source = .false., stat = err)
   allocate(charsloc   (nloc),                   stat = err)
   
   if ( err /= 0 ) then
      stat = err_t ( stat = IERROR, where = HERe, msg =                               &
                     "Allocation failure for array 'is_enclosed' or array 'charsloc'" )
      return
   end if
      
   nloc = 0  
   do i = 1, lstr
      if ( index(chars,str(i:i)) /= 0 ) then
         nloc = nloc + 1
         charsloc(nloc) = i
         is_enclosed(nloc) = util_IsEnclosed ( trim(str), i, sopen, sclose, stat )
         error_TraceNreturn(stat, HERE) 
      end if
   end do
   
   if ( lback ) then
      i1 = nloc ; i2 = 1 ; i3 =-1
      pos = 0 !pos = 1
   else
      i1 = 1 ; i2 = nloc ; i3 = 1
      pos = 0 !pos = lstr
   end if
      
   do i = i1, i2, i3
      if ( .not. is_enclosed(i) ) then
         pos = charsloc(i)
         exit
      end if
   end do
   
   END FUNCTION util_FirstOccurence 
   
   
!=============================================================================================
   FUNCTION util_RemoveSpaces1 ( str, rm_non_printable ) result( res )
!=============================================================================================
   character(len=*       ),           intent(in) :: str
   logical                , optional, intent(in) :: rm_non_printable
   character(len=len(str))                       :: res
!---------------------------------------------------------------------------------------------                   
!   Removes spaces, tabs, and control characters in string "str"
!   Source: George Benthien (http://www.gbenthien.net)
!
!   Initial version: 04/17 (moved to util_RemoveSpaces1_old)
!   Modified: 05/24 (remove all non-printable characters if rm_non_printable = .true. or not
!                    present)
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   character(len=1)             :: ch
   character(len=len_trim(str)) :: outstr
   integer  (Ikind)             :: i, k, ich1, ich2, ich, lstr
!---------------------------------------------------------------------------------------------

   res = adjustl(str) ; lstr = len_trim(res)
!
!- If rm_non_printable is .true. or not present: 
!  - keep only printable characters (i.e. those for which ascii codes are in [33,127])
!  Otherwise: 
!  - keep those for which ascii codes are >= 33
!
   ich1 = 33 ; ich2 = 127
   if ( present(rm_non_printable) ) then
      if ( .not. rm_non_printable ) ich2 = 255
   end if
   
   outstr = ' '; k = 0
   do i = 1, lstr
      ch = res(i:i) ; ich = iachar(ch)
      if ( ich >= ich1 .and. ich <= ich2 ) then
         k = k + 1 ; outstr(k:k) = ch
      end if
   end do

   res = adjustl(outstr)

   END FUNCTION util_RemoveSpaces1
   

!=============================================================================================
   FUNCTION util_RemoveSpaces1_old ( str ) result( res )
!=============================================================================================
   character(len=*       ), intent(in) :: str
   character(len=len(str))             :: res
!---------------------------------------------------------------------------------------------                   
!   Removes spaces, tabs, and control characters in string "str"
!   Source: George Benthien (http://www.gbenthien.net)
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   character(len=1)             :: ch
   character(len=len_trim(str)) :: outstr
   integer  (Ikind)             :: i, k, lenstr
!---------------------------------------------------------------------------------------------

   res = adjustl(str) ; lenstr = len_trim(res)
   
   outstr = ' '; k = 0
   do i = 1, lenstr
      ch = res(i:i)
      
      if ( ch == ' ' ) cycle
      
      select case ( iachar(ch) )    
         case(0:32)  ! space, tab, or control character 
            cycle 
                  
         case(33:)  
            k = k + 1
            outstr(k:k) = ch
         
      end select
   end do

   res = adjustl(outstr)

   END FUNCTION util_RemoveSpaces1_old
   
   
!=============================================================================================
   FUNCTION util_RemoveSpaces2 ( strin, stat, opt, rm_non_printable ) result( strout )
!=============================================================================================
   character(len=*         ),           intent(in    ) :: strin
   type     (err_t         ),           intent(in out) :: stat            
   character(len=*         ), optional, intent(in    ) :: opt
   logical                  , optional, intent(in    ) :: rm_non_printable
   character(len=len(strin))                           :: strout
!---------------------------------------------------------------------------------------------                   
!   Removes spaces, tabs, and control characters in string str
!
!   Original: 01/18
!   Modified: 05/24 (remove all non-printable characters if rm_non_printable = .true. or not
!                    present)
!
!   If "opt" is present with the value 
!
!     . 'nsqt': removes them only if they are not in parts of "strin" enclosed between simple
!               quotation marks  (i.e. parts enclosed by quotes (') are not modified)
!
!     . 'ndqt': removes them only if they are not in parts of "strin" enclosed between double
!               quotation marks  (i.e. parts enclosed by quotes (") are not modified)
!
!     . 'nqt' : removes them only if they are not in parts of "strin" enclosed between simple
!               or double quotation marks  (i.e. parts enclosed by quotes (") or (') are not
!               modified)
!
!   External procedures:
!     . util_RemoveSpaces1
!
!
!   Examples:
!     . util_RemoveSpaces2(' "hello world", he said')            gives '"helloworld",hesaid'
!
!     . util_RemoveSpaces2(' "hello world", he said', opt='nqt') gives '"hello world",hesaid'
!
!   Warnings: 
!     . no check is done for unbalanced quotes    
!
!     . mixing quotes and double quotes in the same string, like in this one
!
!          file="/hassani/m.in", F = "x + sin(y) * a", t = 'foot' + ' ball'
!
!       is considered (but not recommended!) and will give with opt = 
!
!    no opt (all removed)      :  file="/hassani/m.in",F="x+sin(y)*a",t='foot'+'ball' 
!    'nqt'  (kept if in ' or "):  file="/hassani/m.in",F="x + sin(y) * a",t='foot'+' ball'
!    'nsqt' (kept if in ')     :  file="/hassani/m.in",F="x+sin(y)*a",t='foot'+' ball'
!    'ndqt' (kept if in ")     :  file="/hassani/m.in",F="x + sin(y) * a",t='foot'+'ball'
!
!-----------------------------------------------------------------------------------R.H. 01/18                   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_RemoveSpaces2'
   character(len=1)              :: ch
   character(len=:), allocatable :: mopt
   integer  (Ikind)              :: i, k, lstr, ich1, ich2, ich, ndl, ndr, nsl, nsr, rm, iopt
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return
!
!- If rm_non_printable is .true. or not present: 
!  - keep only printable characters (i.e. those for which ascii codes are in [33,127])
!  Otherwise: 
!  - keep those for which ascii codes are >= 33
!
   ich1 = 32 ; ich2 = 128
   if ( present(rm_non_printable) ) then
      if ( .not. rm_non_printable ) ich2 = 256
   end if  
   
   if ( present(opt) ) then
      mopt = trim(adjustl(opt)) ; iopt = 0
      select case (mopt)
         case ('')
            iopt = 0
         case('nsqt')
            if ( index(strin,"'") /= 0 ) iopt = 1
         case('ndqt')      
            if ( index(strin,'"') /= 0 ) iopt = 2
         case('nqt')      
            if ( index(strin,'"') /= 0 .or. index(strin,"'" ) /= 0) iopt = 3
         case default
            stat = err_t (stat = IERROR, where = HERE, msg = 'Unknown option, opt = ' // opt)
            return
      end select
   else
      iopt = 0   
   end if           
               
   if ( iopt == 0 ) then
      strout = util_RemoveSpaces1 ( strin )
      return
   end if
                   
   strout = ''
   
   lstr = len_trim(strin)

   nsl = 0 ; nsr = 0 ; ndl = 0 ; ndr = 0
   do i = 1, lstr
      ch = strin(i:i)
      if ( (iopt == 1 .or. iopt == 3) .and. ch == "'" ) nsr = nsr + 1
      if ( (iopt == 2 .or. iopt == 3) .and. ch == '"' ) ndr = ndr + 1
   end do

   k = 0 ; rm = 1
   do i = 1, lstr
   
      ch = strin(i:i)

      if ( ch == "'" .and. (iopt == 1 .or. iopt == 3) ) then
         nsl = nsl + 1 ; nsr = nsr - 1
         rm = 0 ; if ( modulo(nsl,2_Ikind) == 0 .and. modulo(nsr,2_Ikind) == 0 ) rm = 1
      end if

      if ( ch == '"' .and. (iopt == 2 .or. iopt == 3) ) then
         ndl = ndl + 1 ; ndr = ndr - 1
         rm = 0 ; if ( modulo(ndl,2_Ikind) == 0 .and. modulo(ndr,2_Ikind) == 0 ) rm = 1
      end if

      if ( rm == 1 ) then
         ich = iachar(ch)
         if ( ich <= ich1 .or. ich >= ich2 ) cycle !!(05/24)
      end if
      
      k = k + 1 ; strout(k:k) = ch
   end do

   strout = adjustl(strout)

   END FUNCTION util_RemoveSpaces2


!=============================================================================================
   FUNCTION util_RemoveSpaces2_old ( strin, stat, opt ) result( strout )
!=============================================================================================
   character(len=*         ),           intent(in    ) :: strin
   type     (err_t         ),           intent(in out) :: stat            
   character(len=*         ), optional, intent(in    ) :: opt
   character(len=len(strin))                           :: strout
!---------------------------------------------------------------------------------------------                   
!   Removes spaces, tabs, and control characters in string str
!
!   If "opt" is present with the value 
!
!     . 'nsqt': removes them only if they are not in parts of "strin" enclosed between simple
!               quotation marks  (i.e. parts enclosed by quotes (') are not modified)
!
!     . 'ndqt': removes them only if they are not in parts of "strin" enclosed between double
!               quotation marks  (i.e. parts enclosed by quotes (") are not modified)
!
!     . 'nqt' : removes them only if they are not in parts of "strin" enclosed between simple
!               or double quotation marks  (i.e. parts enclosed by quotes (") or (') are not
!               modified)
!
!   External procedures:
!     . util_RemoveSpaces1
!
!
!   Examples:
!     . util_RemoveSpaces2(' "hello world", he said')            gives '"helloworld",hesaid'
!
!     . util_RemoveSpaces2(' "hello world", he said', opt='nqt') gives '"hello world",hesaid'
!
!   Warnings: 
!     . no check is done for unbalanced quotes    
!
!     . mixing quotes and double quotes in the same string, like in this one
!
!          file="/hassani/m.in", F = "x + sin(y) * a", t = 'foot' + ' ball'
!
!       is considered (but not recommended!) and will give with opt = 
!
!    no opt (all removed)      :  file="/hassani/m.in",F="x+sin(y)*a",t='foot'+'ball' 
!    'nqt'  (kept if in ' or "):  file="/hassani/m.in",F="x + sin(y) * a",t='foot'+' ball'
!    'nsqt' (kept if in ')     :  file="/hassani/m.in",F="x+sin(y)*a",t='foot'+' ball'
!    'ndqt' (kept if in ")     :  file="/hassani/m.in",F="x + sin(y) * a",t='foot'+'ball'
!
!-----------------------------------------------------------------------------------R.H. 01/18                   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_RemoveSpaces2'
   character(len=1)              :: ch
   character(len=:), allocatable :: mopt
   integer  (Ikind)              :: i, k, lenstr, ich, ndl, ndr, nsl, nsr, rm, iopt
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return

   if ( present(opt) ) then
      mopt = trim(adjustl(opt)) ; iopt = 0
      select case (mopt)
         case ('')
            iopt = 0
         case('nsqt')
            if ( index(strin,"'") /= 0 ) iopt = 1
         case('ndqt')      
            if ( index(strin,'"') /= 0 ) iopt = 2
         case('nqt')      
            if ( index(strin,'"') /= 0 .or. index(strin,"'" ) /= 0) iopt = 3
         case default
            stat = err_t (stat = IERROR, where = HERE, msg = 'Unknown option, opt = ' // opt)
            return
      end select
   else
      iopt = 0   
   end if             
               
   if ( iopt == 0 ) then
      strout = util_RemoveSpaces1 ( strin )
      return
   end if
                   
   strout = ''
   
   lenstr = len_trim(strin)

   nsl = 0 ; nsr = 0 ; ndl = 0 ; ndr = 0
   do i = 1, lenstr
      ch = strin(i:i)
      if ( (iopt == 1 .or. iopt == 3) .and. ch == "'" ) nsr = nsr + 1
      if ( (iopt == 2 .or. iopt == 3) .and. ch == '"' ) ndr = ndr + 1
   end do

   k = 0 ; rm = 1
   do i = 1, lenstr
   
      ch = strin(i:i)

      if ( ch == "'" .and. (iopt == 1 .or. iopt == 3) ) then
         nsl = nsl + 1 ; nsr = nsr - 1
         rm = 0 ; if ( modulo(nsl,2_Ikind) == 0 .and. modulo(nsr,2_Ikind) == 0 ) rm = 1
      end if

      if ( ch == '"' .and. (iopt == 2 .or. iopt == 3) ) then
         ndl = ndl + 1 ; ndr = ndr - 1
         rm = 0 ; if ( modulo(ndl,2_Ikind) == 0 .and. modulo(ndr,2_Ikind) == 0 ) rm = 1
      end if

      ich = iachar(ch)
      if ( ich >= 0 .and. ich <= 32 .and. rm == 1 ) cycle
   
      k = k + 1 ; strout(k:k) = ch
   end do

   strout = adjustl(strout)

   END FUNCTION util_RemoveSpaces2_old
   
   
!=============================================================================================
   FUNCTION util_RemoveZeros ( strnum, stat, ndecimals ) result ( res )
!=============================================================================================
   character(len=*          ),           intent(in    ) :: strnum
   type     (err_t          ),           intent(in out) :: stat     
   integer  (Ikind          ), optional, intent(in    ) :: ndecimals       
   character(len=len(strnum))                           :: res
!---------------------------------------------------------------------------------------------
!  Compactes the string-number "str" by removing trailing zeros 
!
!  Examples:  
!  - if the string-number is '123.450000e+006' or '123.4500000', all '0' after the last non-
!    zero decimal are removed. The resulting strings are '123.45e+6' or '123.45', respectively
!  - if "strnum" is '123.000' the result is '123'
!  - if "strnum" is '.0000' the resutlt is '0'
!
!  If "ndecimals" is present and > 0, decimals beyond the ndecimals+1 th decimal are ignored. 
!
!  Example: if "strnum" is '123.450006700D-089' and ndecimals is 2, then res = '123.45e-89'  
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   character(len=* ), parameter   :: HERE = 'util_RemoveZeros'
   character(len=: ), allocatable :: string
   integer  (Ikind ), parameter   :: LG = 3000
   integer  (Ikind )              :: n, pdot, pexp, pend, p, plast, iexpo, err
   character(len=10)              :: expo
   character(len=LG)              :: iomsg
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return

   string = adjustl(strnum) ; res = string
!
!- if "string" is not a string-number return with res = string:
!
   n = 0 ; pend = len_trim(string)
   
   do p = 1, pend
      select case ( string(p:p) )
         case('0':'9','D','d','E','e','.','-','+')
            n = n + 1
      end select
   end do         
   if ( n /= pend ) return

   string = util_StringLow ( string )  ! put "string" in lowercase   
                  
   pdot = index(string,'.') ! position of the decimal point
      
   if ( pdot == 0 ) return  ! if "string" doesn't contain '.' exit with res = string      
!
!- find the position "pexp" of the exponent symbol ('e' or 'd'):
!      
   pexp = index(string,'e')
   if ( pexp == 0 ) pexp = index(string,'d')
!
!- if presents, removes trailing in the exponent part and put the result in "expo"
!  if not presents, put the empty string in "expo" and set pexp to pend + 1:
!
   expo = ''; 
   if ( pexp == 0 ) then
      pexp = pend + 1
   else
      read(string(pexp+1:pend), *, iostat=err, iomsg=iomsg) iexpo
      if ( err /= 0 ) then
         stat = err_t ( stat = IERROR, where = HERE, msg = 'IOMSG: '//trim(iomsg) )
         return
      end if   
      if ( abs(iexpo) > 0 ) write(expo,'(a,i0)') string(pexp:pexp),iexpo
   end if      
!
!- remove trailing '0's in the mantissa:
!   
   res = ''; res = string(1:pdot-1)

   plast = pexp-1
!
!- keep only the "ndecimals" first decimals if required:
!
   if ( present(ndecimals) ) then
      if ( ndecimals > 0 ) plast = min(pdot + ndecimals,plast) 
   end if

   do p = plast, pdot+1, -1
      if ( string(p:p) /= '0' ) then
         res = string(1:p)
         exit
      end if
   end do

   res = trim(res)//expo ! add the exponent part

   if ( len_trim(res) == 0 ) res = '0' 

   END FUNCTION util_RemoveZeros
   

!=============================================================================================
   FUNCTION util_StringLow ( string ) result ( res )
!=============================================================================================
   character(len=*          ), intent(in ) :: string
   character(len=len(string))              :: res
!---------------------------------------------------------------------------------------------  
!  Lowercases a string.
!  External routine used: util_CharLow
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   character         :: c
   integer  (kind=4) :: i
!---------------------------------------------------------------------------------------------                   
  
   res = ''
   do i = 1, len_trim(string)
      c = string(i:i)
      call util_CharLow ( c )
      res(i:i) = c
   end do
  
   END FUNCTION util_StringLow  
   

!=============================================================================================
   FUNCTION util_StringLow2 ( strin, stat, opt ) result( strout )
!=============================================================================================
   character(len=*         ),           intent(in    ) :: strin
   type     (err_t         ),           intent(in out) :: stat            
   character(len=*         ), optional, intent(in    ) :: opt
   character(len=len(strin))                           :: strout
!---------------------------------------------------------------------------------------------                   
!  Lowercases a string.
!
!   If "opt" is present with the value 
!
!     . 'nsqt': lowercases only the characters that are not enclosed between simple
!               quotation marks 
!
!     . 'ndqt': lowercases only the characters that are not enclosed between double
!               quotation marks
!
!     . 'nqt' : lowercases only the characters that are not enclosed between simple or
!               double quotation marks 
!
!   External procedures:
!     . util_StringLow
!
!-----------------------------------------------------------------------------------R.H. 01/18                   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_StringLow2'
   character(len=1)              :: ch
   character(len=:), allocatable :: mopt
   integer  (Ikind)              :: i, k, lenstr, ndl, ndr, nsl, nsr, iopt
   logical                       :: conv
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return

   if ( present(opt) ) then
      mopt = trim(adjustl(opt)) ; iopt = 0
      select case (mopt)
         case ('')
            iopt = 0
         case('nsqt')
            if ( index(strin,"'") /= 0 ) iopt = 1
         case('ndqt')      
            if ( index(strin,'"') /= 0 ) iopt = 2
         case('nqt')      
            if ( index(strin,'"') /= 0 .or. index(strin,"'" ) /= 0) iopt = 3
         case default
            stat = err_t (stat = IERROR, where = HERE, msg = 'Unknown option, opt = ' // opt)
            return
      end select
   else
      iopt = 0   
   end if             
               
   if ( iopt == 0 ) then
      strout = util_StringLow ( strin )
      return
   end if
                   
   strout = ''
   
   lenstr = len_trim(strin)

   nsl = 0 ; nsr = 0 ; ndl = 0 ; ndr = 0
   do i = 1, lenstr
      ch = strin(i:i)
      if ( (iopt == 1 .or. iopt == 3) .and. ch == "'" ) nsr = nsr + 1
      if ( (iopt == 2 .or. iopt == 3) .and. ch == '"' ) ndr = ndr + 1
   end do

   k = 0 ; conv = .true.
   do i = 1, lenstr
   
      ch = strin(i:i)

      if ( ch == "'" .and. (iopt == 1 .or. iopt == 3) ) then
         nsl = nsl + 1 ; nsr = nsr - 1 ; conv = .false. 
         if ( modulo(nsl,2_Ikind) == 0 .and. modulo(nsr,2_Ikind) == 0 ) conv = .true.
      end if

      if ( ch == '"' .and. (iopt == 2 .or. iopt == 3) ) then
         ndl = ndl + 1 ; ndr = ndr - 1 ; conv = .false.
         if ( modulo(ndl,2_Ikind) == 0 .and. modulo(ndr,2_Ikind) == 0 ) conv = .true.
      end if

      if ( conv ) call util_CharLow ( ch )
      k = k + 1 ; strout(k:k) = ch
   end do

   END FUNCTION util_StringLow2
   
   
!=============================================================================================
   FUNCTION util_StringCap ( string ) result ( res )
!=============================================================================================
   character(len=*          ), intent(in ) :: string
   character(len=len(string))              :: res  
!---------------------------------------------------------------------------------------------                   
!  Capitalizes a string.
!  External routine used: util_CharCap
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   character         :: c
   integer  (kind=4) :: i
!---------------------------------------------------------------------------------------------                   
  
   res = ''
   do i = 1, len(trim(string))
      c = string(i:i)
      call util_CharCap ( c )
      res(i:i) = c
   end do
  
   END FUNCTION util_StringCap  
   

!=============================================================================================
   FUNCTION util_StringCap2 ( strin, stat, opt ) result( strout )
!=============================================================================================
   character(len=*         ),           intent(in    ) :: strin
   type     (err_t         ),           intent(in out) :: stat            
   character(len=*         ), optional, intent(in    ) :: opt
   character(len=len(strin))                           :: strout
!---------------------------------------------------------------------------------------------                   
!  Lowercases a string.
!
!   If "opt" is present with the value 
!
!     . 'nsqt': lowercases only the characters that are not enclosed between simple
!               quotation marks 
!
!     . 'ndqt': lowercases only the characters that are not enclosed between double
!               quotation marks
!
!     . 'nqt' : lowercases only the characters that are not enclosed between simple or
!               double quotation marks 
!
!   External procedures:
!     . util_StringLow
!
!-----------------------------------------------------------------------------------R.H. 01/18                   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'util_StringCap2'
   character(len=1)              :: ch
   character(len=:), allocatable :: mopt
   integer  (Ikind)              :: i, k, lenstr, ndl, ndr, nsl, nsr, iopt
   logical                       :: conv
!---------------------------------------------------------------------------------------------

   if ( stat%code > IZERO ) return

   if ( present(opt) ) then
      mopt = trim(adjustl(opt)) ; iopt = 0
      select case (mopt)
         case ('')
            iopt = 0
         case('nsqt')
            if ( index(strin,"'") /= 0 ) iopt = 1
         case('ndqt')      
            if ( index(strin,'"') /= 0 ) iopt = 2
         case('nqt')      
            if ( index(strin,'"') /= 0 .or. index(strin,"'" ) /= 0) iopt = 3
         case default
            stat = err_t (stat = IERROR, where = HERE, msg = 'Unknown option, opt = ' // opt)
            return
      end select
   else
      iopt = 0   
   end if             
               
   if ( iopt == 0 ) then
      strout = util_StringLow ( strin )
      return
   end if
                   
   strout = ''
   
   lenstr = len_trim(strin)

   nsl = 0 ; nsr = 0 ; ndl = 0 ; ndr = 0
   do i = 1, lenstr
      ch = strin(i:i)
      if ( (iopt == 1 .or. iopt == 3) .and. ch == "'" ) nsr = nsr + 1
      if ( (iopt == 2 .or. iopt == 3) .and. ch == '"' ) ndr = ndr + 1
   end do

   k = 0 ; conv = .true.
   do i = 1, lenstr
   
      ch = strin(i:i)

      if ( ch == "'" .and. (iopt == 1 .or. iopt == 3) ) then
         nsl = nsl + 1 ; nsr = nsr - 1 ; conv = .false. 
         if ( modulo(nsl,2_Ikind) == 0 .and. modulo(nsr,2_Ikind) == 0 ) conv = .true.
      end if

      if ( ch == '"' .and. (iopt == 2 .or. iopt == 3) ) then
         ndl = ndl + 1 ; ndr = ndr - 1 ; conv = .false.
         if ( modulo(ndl,2_Ikind) == 0 .and. modulo(ndr,2_Ikind) == 0 ) conv = .true.
      end if

      if ( conv ) call util_CharCap ( ch )
      k = k + 1 ; strout(k:k) = ch
   end do

   END FUNCTION util_StringCap2
   
   
!=============================================================================================
   SUBROUTINE util_CharLow ( ch )
!=============================================================================================
   character, intent(in out) :: ch
!---------------------------------------------------------------------------------------------                     
!  Lowercases a single character
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   integer(kind=4) :: itemp
!---------------------------------------------------------------------------------------------                   

   itemp = iachar ( ch )

   if ( 65 <= itemp .and. itemp <= 90 ) ch = achar ( itemp + 32 )

   return

   END SUBROUTINE util_CharLow
   

!=============================================================================================
   SUBROUTINE util_CharCap ( ch )
!=============================================================================================
   character, intent(in out) :: ch
!---------------------------------------------------------------------------------------------                     
!  Capitalizes a single character.
!
!  Discussion:
!
!     Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!     which guarantee the ASCII collating sequence.
!
!  Licensing:
!
!     This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!     19 July 1998
!
!  Author:
!
!     John Burkardt
!
!  Parameters:
!
!     Input/output, character CH, the character to capitalize.
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   integer(kind=4) :: itemp
!---------------------------------------------------------------------------------------------                   

   itemp = iachar ( ch )

   if ( 97 <= itemp .and. itemp <= 122 ) ch = achar ( itemp - 32 )

   return

   END SUBROUTINE util_CharCap
   

!=============================================================================================
   FUNCTION util_RemoveChar ( str, chr ) result ( res )
!=============================================================================================
   character(len=*       ), intent(in) :: str
   character(len=*       ), intent(in) :: chr
   character(len=len(str))             :: res
!---------------------------------------------------------------------------------------------                   
!  Removes a single character ("chr") in a string ("str")
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   character(len=1)             :: ch
   character(len=len_trim(str)) :: outstr
   integer  (Ikind)             :: i, k, lenstr
!---------------------------------------------------------------------------------------------
   
   res = adjustl(str) ; lenstr = len_trim(res)
   
   outstr = ' '; k=0
   do i = 1, lenstr
      ch = res(i:i)
      if ( ch == chr ) cycle
      k = k + 1
      outstr(k:k) = ch
   end do

   res = adjustl(outstr)

   END FUNCTION util_RemoveChar      
   

!=============================================================================================
   FUNCTION util_OrdinalAbbrev ( i ) result ( abbrev )
!=============================================================================================
   integer  (Ikind),             intent(in) :: i
   character(len=:), allocatable            :: abbrev   
!---------------------------------------------------------------------------------------------
!  Ordinal number abbreviations
!----------------------------------------------------------------------------------- R.H.12/18

!- local variables: --------------------------------------------------------------------------
   integer  (Ikind) :: l
   character(len=1) :: u, d
   character(len=3) :: symb
!---------------------------------------------------------------------------------------------

   abbrev = util_intToChar ( i )
   l = len(abbrev)
   u = abbrev(l:l)

   if ( l > 1 ) then
      d = abbrev(l-1:l-1)
   else
      d = ' '
   end if

   if ( d /= '1') then
      if ( u == '1') then
         symb = '-st'
      else if ( u == '2' ) then
         symb = '-nd'
      else if ( u == '3' ) then
          symb = '-rd'
      else
          symb = '-th'
      end if
   else
      symb = '-th'
   end if

   abbrev = trim(abbrev)//symb

   END FUNCTION util_OrdinalAbbrev


!=============================================================================================   
   FUNCTION util_int32ToChar ( i ) result( res )
!=============================================================================================
   integer  ( i32 ),             intent(in) :: i
   character(len=:), allocatable            :: res
!---------------------------------------------------------------------------------------------
!  Converts integer to string (from Vladimir F., https://stackoverflow.com)
!----------------------------------------------------------------------------------- R.H.04/22

!- local variables: --------------------------------------------------------------------------
   character(len=range(i)+2) :: tmp
!---------------------------------------------------------------------------------------------

   write(tmp,'(i0)') i ;  res = trim(tmp)
  
  END FUNCTION util_int32ToChar       


!=============================================================================================   
   FUNCTION util_int64ToChar ( i ) result( res )
!=============================================================================================
   integer  ( i64 ),             intent(in) :: i
   character(len=:), allocatable            :: res
!---------------------------------------------------------------------------------------------
!  Converts integer to string (from Vladimir F., https://stackoverflow.com)
!----------------------------------------------------------------------------------- R.H.04/22

!- local variables: --------------------------------------------------------------------------
   character(len=range(i)+2) :: tmp
!---------------------------------------------------------------------------------------------

   write(tmp,'(i0)') i ;  res = trim(tmp)
  
  END FUNCTION util_int64ToChar     


!=============================================================================================
   SUBROUTINE util_BarreDefilement0 ( i, n, di, sHead, cDone, cRemain, nsymb )
!=============================================================================================
   integer         ,           intent(in) :: di, i, n
   character(len=*), optional, intent(in) :: sHead
   character(len=1), optional, intent(in) :: cDone, cRemain      
   integer         , optional, intent(in) :: nsymb   
!---------------------------------------------------------------------------------------------
!  Affiche (sur stderr) une barre de defilement au cours d'un calcul dont le volume est connu 
!  a l'avance (une boucle par exemple). 
!
!  Inputs:
!
!  . i       : Numero de l'element en cours dans la boucle (1 <= i <= n).
!  . n       : Nombre d'elements dans la boucle (n >= i).
!  . di      : Frequence de l'affichage (en % : 1 <= di <= 100).
!  . sHead   : (option). Commentaire a placer au debut (sera suivi de ": ").
!  . cDone   : (option) Symbole (character*1) a utiliser pour la partie deja traitee (1 a i).
!              S'il est absent, le caracter "=" sera utilise par defaut.
!  . cRemain : (option) Symbole (character*1) a utiliser pour la partie restante (i+1 a n).
!              S'il est absent, le caracter blanc (" ") sera utilise sauf si "cDone" est
!              lui meme un blanc, auquel cas le character "o" sera utilise.
!  . nsymb   : (option) Nombre total de symboles a utiliser dans la barre. Si absent ou nul,
!              le nombre par defaut (DEFNSYMB) sera utilise.
!              Attention: la taille de la console doit etre >= nsymb+15
!
!  Exemples :
!
!        n = 200
!        do i = 1, n
!           call sleep(1)
!           call util_BarreDefilement ( i, n, 10, &
!                                       cDone = "=", cRemain = "#" ) ! options
!        end do
!
!        produira un affichage tous les 10% sous la forme :
!
!                40%  (/)  [ ====================############################# ]
!
!        alors que 
!
!           call util_BarreDefilement ( i, n, 10, &
!                                       cDone = "=" ) ! options
!        produira :
!
!                40%  (/)  [ ====================                               ]
!
!        et que 
!
!           call util_BarreDefilement ( i, n, 10,  &
!                                cDone = "=", cRemain = "#", sHead = "In progress" ) ! options
!
!        produira :
!
!                In progress: 40%  (/)  [ ====================############################# ]
!
! 
!  La barre contiendra un total de ntsymb symboles "cDone" quand 100 % du calcul sera atteint
!  (ntsymb = nsymb si present ou DEFNSYMB sinon).
!
!  source : inspire au depart par une reponse de Steve Lionel trouvee sur un forum intel : 
!           https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-
!           os-x/topic/270155
!
!!!!!! A revoir (cf les changements dans ptovk3d_ascii.f90) !!!!!!
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   integer         , parameter         :: DEFNSYMB = 50 ! defaut pour le nombre de symboles 
   character(len=3), parameter         :: wheel(4) = ['(|)','(/)','(-)','(\)']

   integer                             :: isymb   
   real                                :: tprint
   character(len=3)                    :: percents
   character(len=:), allocatable       :: bar

   logical         ,              SAVE :: first = .true.
   integer         ,              SAVE :: nprint, ntsymb
   character(len=1),              SAVE :: symbDone, symbRemain   
   character(len=:), allocatable, SAVE :: sHeader
   
!---------------------------------------------------------------------------------------------
         
   if ( first ) then
!
!-    Si premiere fois : definir le commentaire a placer en debut de barre, les symboles a 
!     a utiliser et leur nombre total
!      
      first  = .false.
      nprint = 0
!
!-    Le commentaire (ou chaine vide) :
!      
      if ( present(sHead) ) then
         sHeader = char(13) // sHead // ': '
      else
         sHeader = char(13)
      end if   
!
!-    Le symbole de progression :    
!  
      if ( present(cDone) ) then
         symbDone = cDone
      else
         symbDone = '='
      end if    
!
!-    Le symbole pour la partie restante :
!    
      if ( present(cRemain) ) then
         symbRemain = cRemain 
      else
         symbRemain = ' '
      end if      
!
!-    Si identiques, modifier symbRemain :
!   
      if ( symbRemain == symbDone ) then
         if ( symbRemain /= ' ' ) then
            symbRemain = ' '
         else
            symbRemain = 'o'
         end if
      end if      
!
!-    Le nombre total de symboles dans la barre :
!            
      if ( present(nsymb) ) then
         ntsymb = nsymb
         if ( ntsymb == 0 ) ntsymb = DEFNSYMB
      else
         ntsymb = DEFNSYMB
      end if    
      
   end if   
      
   tprint = real(n*nprint*di) / 100.0e0
      
   if ( i >= tprint .or. i == n ) then
      nprint = nprint + 1

      isymb = nint( real(i*ntsymb)/real(n) )
      
      write(percents,'(i3)') floor( ( 100.0*real(i) ) / real(n) )

      bar = repeat(symbRemain, 15+ntsymb)
      
!     1  4 6  9 13 13+1                                     13+ntsymb 15+ntsymb
!     |  | |  |  | |                                                | |
!     v  v v  v  v v                                                v v
!     100%  (/)  [ ====================############################## ]
!                  ^                  ^                             ^
!                  |                  |                             |
!                  1    ...         isymb           ...        ntsymb
 
      bar(1         : 6        ) = trim(percents) // '%  '
      bar(7         : 9        ) = wheel(mod(nprint,4)+1)
      bar(10        : 13       ) = '  [ '
      bar(14        : 13+isymb ) = repeat(symbDone, isymb)
      bar(14+ntsymb : 15+ntsymb) = ' ]'
      
      write(STDERR,'(a)',advance = 'no') sHeader // bar ; flush(STDERR)

      if ( i == n ) then
         write(STDERR,*) ; flush(STDERR)
      end if   
   end if

   END SUBROUTINE util_BarreDefilement0
   

!=============================================================================================
   SUBROUTINE util_BarreDefilement ( i, n, di, sLead, sTrail, cDone, cRemain, nsymb, unit )
!=============================================================================================
   integer         ,           intent(in) :: di, i, n
   character(len=*), optional, intent(in) :: sLead, sTrail
   character(len=1), optional, intent(in) :: cDone, cRemain      
   integer         , optional, intent(in) :: nsymb, unit
!---------------------------------------------------------------------------------------------
!  Affiche (sur stdout) une barre de defilement au cours d'un calcul dont le volume est connu 
!  a l'avance (une boucle par exemple). 
!
!  Inputs:
!
!  . i       : Numero de l'element en cours dans la boucle (1 <= i <= n).
!
!  . n       : Numero du dernier element de la boucle (n >= i).
!
!  . di      : Frequence de l'affichage (en % : 1 <= di <= 100).
!
!  . sLead   : (option). Commentaire (fixe) a placer en debut de barre (sera suivi de ": ").
!
!  . sTrail  : (option). Commentaire (eventuellement changeant) a placer en fin de barre.
!
!  . cDone   : (option) Symbole (character*1) a utiliser pour la partie deja traitee (1 a i).
!                       S'il est absent, le caracter "=" sera utilise par defaut.
!
!  . cRemain : (option) Symbole (character*1) a utiliser pour la partie restante (i+1 a n).
!                       S'il est absent, le caracter blanc (" ") sera utilise sauf si "cDone" 
!                       est lui meme un blanc, auquel cas le character "o" sera utilise.
!
!  . nsymb   : (option) Nombre total de symboles a utiliser dans la barre. Si absent ou nul,
!                       le nombre par defaut (DEFNSYMB) sera utilise.
!                       Attention: la taille de la console doit etre >= nsymb+15
!                       (voire superieure si presence de sTrail).
!  Exemples :
!
!        n = 200
!        do i = 1, n
!           call sleep(1)
!           call BarreDefilement ( i, n, 10, &
!                                  cDone = "=", cRemain = "#" ) ! options
!        end do
!
!        produira un affichage tous les 10% sous la forme :
!
!                40%  (/)  [ ====================############################# ]
!
!        alors que 
!
!           call BarreDefilement ( i, n, 10, &
!                                  cDone = "=" ) ! options
!        produira :
!
!                40%  (/)  [ ====================                               ]
!
!        et que 
!
!           call BarreDefilement ( i, n, 10,  &
!                                cDone = "=", cRemain = "#", sLead = "In progress" ) ! options
!
!        produira :
!
!                In progress: 40%  (/)  [ ====================############################# ]
!
! 
!  La barre contiendra un total de ntsymb symboles "cDone" quand 100 % du calcul sera atteint
!  (ntsymb = nsymb si present ou DEFNSYMB sinon).
!
!  source : inspire d'une reponse de Steve Lionel trouvee sur un forum intel : 
!           https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-
!           os-x/topic/270155
!----------------------------------------------------------------------------------------- R.H.

!- local variables: --------------------------------------------------------------------------
   integer         , parameter         :: DEFNSYMB = 50 ! defaut pour le nombre de symboles 
   character(len=3), parameter         :: wheel(4) = ['(|)','(/)','(-)','(\)']

   integer                             :: isymb, m
   real                                :: tprint
   character(len=3)                    :: percents
   character(len=:), allocatable       :: bar, sT

   integer         ,              SAVE :: nprint, ntsymb, sTlen, uo
   character(len=1),              SAVE :: symbDone, symbRemain      
   character(len=:), allocatable, SAVE :: sL
!---------------------------------------------------------------------------------------------
                     
   if ( i == 1 ) then
!
!-    Si premiere fois : definir le commentaire a placer en debut de barre, les symboles a 
!     a utiliser et leur nombre total
!      
      nprint = 0
!
!-    Le commentaire (ou chaine vide) en debut de barre (fixe) :
!      
      if ( present(sLead) ) then
         sL = char(13) // sLead // ': '
      else
         sL = char(13)
      end if   
      
      if ( present(sTrail) ) then
         sTlen = len_trim(sTrail)
      else
         sTlen = 1
      end if      
!
!-    Le symbole de progression :    
!  
      if ( present(cDone) ) then
         symbDone = cDone
      else
         symbDone = '='
      end if    
!
!-    Le symbole pour la partie restante :
!    
      if ( present(cRemain) ) then
         symbRemain = cRemain 
      else
         symbRemain = ' '
      end if      
!
!-    Si identiques, modifier symbRemain :
!   
      if ( symbRemain == symbDone ) then
         if ( symbRemain /= ' ' ) then
            symbRemain = ' '
         else
            symbRemain = 'o'
         end if
      end if      
!
!-    Le nombre total de symboles dans la barre :
!            
      if ( present(nsymb) ) then
         ntsymb = nsymb
         if ( ntsymb == 0 ) ntsymb = DEFNSYMB
      else
         ntsymb = DEFNSYMB
      end if    
            
      if ( present(unit) ) then
         uo = unit
      else 
         uo = STDOUT    
      end if  
   end if   
!
!- Commentaire a placer en fin de barre (ou chaine vide) :
!
   if ( present(sTrail) ) then
      m = sTlen - len_trim(sTrail)
      if ( m > 0 ) then
         sT = sTrail // repeat(" ",m)
      else
         sTlen = len_trim(sTrail)      
         sT = sTrail   
      end if   
   else
      sT = repeat(" ",sTlen)
   end if         
         
   tprint = real(n*nprint*di) / 100.0e0
   
   if ( util_isATeleTypeWriter ( uo ) ) then
      if ( i >= tprint .or. i == n ) then
         nprint = nprint + 1

         isymb = nint( real(i*ntsymb)/real(n) )
      
         write(percents,'(i3)') floor( ( 100.0*real(i) ) / real(n) )

         bar = repeat(symbRemain, 15+ntsymb)
      
!        1  4 6  9 13 13+1                                     13+ntsymb 15+ntsymb
!        |  | |  |  | |                                                | |
!        v  v v  v  v v                                                v v
!        100%  (/)  [ ====================############################## ]
!                     ^                  ^                             ^
!                     |                  |                             |
!                     1    ...         isymb           ...        ntsymb
 
         bar(1         : 6        ) = trim(percents) // '%  '
         bar(7         : 9        ) = wheel(mod(nprint,4)+1)
         bar(10        : 13       ) = '  [ '
         bar(14        : 13+isymb ) = repeat(symbDone, isymb)
         bar(14+ntsymb : 15+ntsymb) = ' ]'
      
         write(uo,'(a)',advance = 'no') sL // bar // ' ' // sT ; flush(uo)

         if ( i == n ) then
            write(uo,*) ; flush(uo)
         end if  
      end if
   else if ( i == n ) then
      write(uo,'(a)') sL(2:) // '100% '// wheel(1) //'  [ ' // &
                      repeat(symbDone,ntsymb) // ' ]' // ' ' // sT 
   end if

   END SUBROUTINE util_BarreDefilement
   

!=============================================================================================
   recursive SUBROUTINE util_ScrollingMessage ( message, delaytime, unit )
!=============================================================================================   
   character(len=*), intent(in)           :: message
   real            , intent(in)           :: delaytime
   integer         , intent(in), optional :: unit
!---------------------------------------------------------------------------------------------
!  Prints (on stdout) a scrolling message
!
!  Inputs:
!  . message  : the message to display
!  . delaytime: delay time (in seconds) between two character prints
!-----------------------------------------------------------------------------------R.H. 01/18 

!- local variables: --------------------------------------------------------------------------
   integer(c_int32_t), parameter :: ONESEC = 100000_c_int32_t
   integer(c_int32_t)            :: delay
   integer                       :: nlen, i, uo
!---------------------------------------------------------------------------------------------
   
   interface ! interface found in http://computer-programming-forum.com (by Tobias Burnu)
      subroutine usleep (useconds) bind(C)
         use iso_c_binding
         implicit none
         integer(c_int32_t), value :: useconds
      end subroutine
   end interface   
   
   if ( present(unit) ) then
      uo = unit
   else
      uo = int(STDOUT)
   end if
   
   if ( util_isATeleTypeWriter ( uo ) ) then
      delay = int(ONESEC * delaytime, kind=c_int32_t)
      nlen = len(message)
      do i = 1, nlen
         call usleep ( delay )
         if ( i == nlen ) then
            write(uo,'(a1)') message(i:i)
            flush(uo)         
         else
            write(uo,'(a1)',advance='no') message(i:i)
            flush(uo)
         end if
      end do
   else
      write(uo,'(a)') message
   end if
   
   END SUBROUTINE util_ScrollingMessage
       

!=============================================================================================
   FUNCTION util_isATeleTypeWriter ( unit ) result ( res )
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only:  output_unit, error_unit
   use, intrinsic :: iso_c_binding, only: c_int
   integer, intent(in) :: unit
   logical             :: res
!---------------------------------------------------------------------------------------------
!  Returns .true. if the logical unit "unit" corresponds to a tty
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

   END FUNCTION util_isATeleTypeWriter
   
     
END MODULE util_Strings_m
