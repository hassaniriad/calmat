!
! This file is part of the calmat library.
!
! The  calmat library  is  free software: you can  redistribute  it and/or modify it under the 
! terms of the GNU General Public License as published by the Free Software Foundation, either 
! version 3 of the License, or (at your option) any later version.
!
! The  calmat library  is  distributed  in the hope that it  will  be useful, but  WITHOUT ANY 
! WARRANTY;  without even the implied warranty of  MERCHANTABILITY or FITNESS FOR A PARTICULAR
! PURPOSE. 
! See the GNU General Public License for more details.
!
! You should  have received  a copy of the  GNU General Public License  along with the  calmat 
! library. If not, see <https://www.gnu.org/licenses/>. 


!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite Cote d'Azur
!
! Date: 11/18
! Modified: 03/19 - 01/20 (for-block and if-block) - 05/20 (exec command)
!---------------------------------------------------------------------------------------------
#include "error.fpp"

MODULE Calmat_m
 
   use CalmatGlobal_m
   use CalmatUtil_m
   use CalmatCmd_m
   use CalmatSetNGet_m
   use CalmatVersion_m 

   implicit none
   
   logical :: first = .true., warn = G_OFF, welc = G_OFF, dispr = G_OFF

   !private
   public :: calmat
!--------------------------------------------------------------------------------------------- 

CONTAINS   

!=============================================================================================
   RECURSIVE SUBROUTINE Calmat ( fileIn , exprIn , parseOnly, evaluate, &
                                 warning, welcome, dispRes  , stat      )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: fileIn, exprIn
   integer  (Ikind), optional, intent(in    ) :: evaluate
   logical         , optional, intent(in    ) :: warning, welcome, dispRes
   type     (err_t), optional, intent(in out) :: stat
   integer  (Ikind), optional, intent(   out) :: parseOnly
!---------------------------------------------------------------------------------------------
!  There are few ways to use this routine depending on your needs (see examples above)
!
!  Inputs:
!  ======
!
!  . stat (optional): controls the behaviour of calmat when an error occurs as follows:
!    - when stat is present, calmat returns in stat the code and the error message (the 
!      decision rests with the caller)
!    - when is not present, the error message will be printed and calmat will stop immediately
!
!  . warning (optional): controls the display of warnings:
!    - when warning = .false. warning messages are not printed
!    - when warning = .true. warning  messages are printed
!    Default: equivalent to warning = .false.
!
!  . welcome (optional): prints a welcome message if welcome = .true.
!    Default: equivalent to welcome = .true.
!
!  . dispRes (optional): controls the displays of the results 
!    - when dispRes = .true. the results of instructions not ended by a ';' are printed
!    Default: disRes = G_disp (defined in the module CalmatGlobal)
!
!  . fileIn (optional): parse and execute the instructions contained in the file fileIn
!
!  . exprIn (optional): parse and execute the expression given by exprIn
!
!  . parseOnly (optional): if presents it means that only the parsing is requested.
!
!  . evaluate (optional): execute and evaluate the set of instructions corresponding to the
!                         flow #evaluate. This flow corresponds to the analysis performed 
!                         during a previous call to calmat (with option "parseOnly")
!
!    Note: (1) when neither fileIn nor exprIn are present, calmat runs in interactive or in  
!              batch mode according to the command line arguments used
!          (2) fileIn and exprIn cannot both be present
!
!  Outputs:
!  =======
!
!  . parseOnly (optional): contains the flow # corresponding to the parsing.
!
!  . stat (optional): error status with the convention : 
!                     + stat%code > 0 for an error
!                     + stat%code < 0 for a warning 
!                     and stat%mesg gives the corresponding message
!
!  . G_vars, G_objs (module variables): the list of variables and objects
!
!  Note: variables beginning with with 'G_' are global and defined in the module CalmatGlobal
!
!  Examples
!  ========
!
!  . Case 1 - No argument supplied (interactive/batch modes):  
!    -----------------------------
!
!    cat foo1.f90:
!
!    use calmat_m
!    call calmat()
!    end
!
!    ./foo1                                <== use calmat in interactive mode
!    ./foo1 -i laplacian2d -o lap2d.out    <== use calmat in batch mode
!
!  . Case 2 - A file name is supplied (evaluate several expressions):
!    --------------------------------
!
!    cat foo2.f90:
!
!    use calmat_m
!    call calmat ( fileIn = "laplacian2d" )
!    end
!
!    ./foo2  <== execute the instructions of a given script (equivalent to batch mode above)
!
!  . Case 3 - A string is supplied (evaluate one or few expressions): 
!    -----------------------------
!    
!    cat foo3.f90:
!
!    use calmat_m
!    call calmat ( exprIn = "x = 0:0.2:1; y=sin(%pi*x)" )
!    end
!
!    ./foo3  <== parse and evaluate a given expression
!
!  . Case 4 - Parse then evaluate
!    ----------------------------
!
!    cat foo4.f90:
!
!    use calmat_m
!    ! parse only:
!    call calmat ( exprIn = "x = 0:0.2:1; y=sin(%pi*x)", parseOnly = flowId )
!    ...
!    ! now evaluate:
!    call calmat ( evaluate = flowId )
!    ...
!    ! freed:
!    call calmat_delFlows()
!    ...
!    end
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'Calmat'
   integer  (Ikind), parameter :: ReadWhole = -1
!--------------------------------------------------------------------------------------------- 

!
!- Initialize predefined variables, list of variables, ..., and user's settings
!   
   call Calmat_Initialize ( warning, welcome, dispRes, stat ) 

   G_disp = dispr
!
!- See what we have to evaluate:
!   
   if ( .not. present(fileIn) .and. .not. present(exprIn) .and. .not. present(evaluate) ) then
!
!-    Case 1: set G_verb = 1 (by default print error/warning message) and get the command
!             line arguments:
!   
      G_verb = IONE

      call Calmat_CmdLineArgs ()
      
   else
!
!-    Case 2 or 3: set G_verb = 0 (do not print error/warning message (unless halting=.true.
!                  or warning=.true.) the caller must check stat) and set G_dispPrompt=.false.
!                  (do not prompt for user input):
!
      G_verb       = IZERO 
      G_dispPrompt = .false.
      
      if ( present(fileIn) ) then
!
!-       Case 2: open the input file and initialize the flow:
!
         if ( len_trim(fileIn) /= 0 ) then
            G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = fileIn )
         else
            call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Empty file name' )
         end if
         
      else if ( present(exprIn) ) then
!
!-       Case 3: store the input expression in the flow:
!      
         G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, expression = exprIn )
         
      end if
      
      if ( G_flagerr > IZERO ) call CalmatUtil_Error ( abort = .true., empty = .false. )      
   end if   
!
!- Print a welcome message:
!      
   call CalmatUtil_welcome ( welc )
!
!- Start:
!
   if ( present(parseOnly) ) then

      parseOnly = G_curflow
   
      call Calmat_read    ( flowId = G_curflow, nLine = ReadWhole ) 
      
      call Calmat_parse   ( flowId = G_curflow )
            
   else if ( present(evaluate) ) then
   
      G_task = G_CONTINUE
      
      call Calmat_execute ( flowId = evaluate )
      
   else
   
      do while ( G_task /= G_STOP )   

         call Calmat_read     ( flowId = G_curflow )
      
         call Calmat_parse    ( flowId = G_curflow )

         call Calmat_execute  ( flowId = G_curflow )

         call Calmat_delFlows ( flowId = G_curflow )       
      
      end do
      
   end if
   
   if ( present(stat) ) then
      call err_moveAlloc ( from = G_flagerr, to = stat )
      if ( stat > IZERO) call stat%AddTrace(HERE)
   end if
      
   G_task = G_CONTINUE   

   END SUBROUTINE Calmat

      
!=============================================================================================
   SUBROUTINE Calmat_read ( flowId, nLine )
!=============================================================================================
   integer(Ikind),           intent(in    ) :: flowId
   !integer(Ikind),           intent(   out) :: start
   integer(Ikind), optional, intent(in    ) :: nLine
!--------------------------------------------------------------------------------------------- 
!  Reads a new record from the current flow #flowId
!---------------------------------------------------------------------------------- R.H. 12/18

!- local variables: --------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'Calmat_read', OPCL = '""()[]' !//"''"
   integer  (Ikind ), parameter   :: NESTED = 1000
   character(len=: ), allocatable :: rec, rec0
   type     (str_t ), allocatable :: SubRec(:,:)
   integer  (Ikind )              :: ui, nSubRec, numl, i, nread, iread, nL, err
   integer  (Ikind )              :: nfor, nif, nwait, nfortproc
   character(len=30)              :: waitingfor(NESTED)
   logical                        :: ignore
!--------------------------------------------------------------------------------------------- 

   if ( G_task /= G_CONTINUE ) return
   
!print*,'*** Calmat_read ***'
!print*,'flowId =',flowId, 'ui = ',G_flows(flowId)%unit,'G_dispPrompt= ',G_dispPrompt 
   
   nwait = 0 ; nif = 0 ; nfor = 0 ; nfortproc = 0 ; err = 0 ; ignore = .false.
   
   !start = 1
   G_flows(flowId)%start = 1 !19/04/24
   
   if ( present(nLine) ) then
      nL = nLine
   else
      nL = G_nLine
   end if
   
   ui = G_flows(flowId)%unit
      
   if ( ui == STDIN .or. ui == IZERO ) then
!
!-    On stdin read only one record at a time:
!   
      nread = 1
   else
!
!-    Otherwise
!
      if ( nL == -1 ) then
!
!-       read the whole file:
!
         nread = 0
      else if ( nL == 0 ) then
!
!-       read as many lines as possible without having to resize the stack:
!
         nread = G_flows(flowId)%maxnStack - G_flows(flowId)%nStack
      else
!
!-       read a maximum of "nL" lines at a time:
!
         nread = nL
      end if
   end if

   iread = 0

   do   
!
!-    print the prompt symbol(s):
!
      if ( G_dispPrompt .and. ui == STDIN .and. G_uo == STDOUT ) then
         if ( nwait == 0 ) then
            call G_prompt%printMe ( unit=int(G_uo), advance='no' )
         else
            call G_prompt%printMe ( unit=int(G_uo), advance='no',                &
                                    fmt='(a,"['//trim(waitingfor(nwait))//'] ")' )
         end if
         flush(G_uo)
      end if      
!
!-    fetch the next record from this file:
!     
      if ( ui /=  IZERO ) then 
         rec = util_GetRecord ( unit=ui, stat=G_flagerr, comments=['//','/*','*/'], &
                                rmblk=.false., nlines = numl, contSymb=['...','&  '] ) 
      else
         rec = G_flows(flowId)%expression
         numl = 1
      end if
      
      rec0 = rec    
!
!-    see if a block construct has not been completed while the eof has been reached:
! 
      !if ( ui /= IZERO .and. ui /= STDIN .and. G_flagerr%code < 0 ) then
      if ( ui == IZERO .or. (ui /= STDIN .and. G_flagerr%code < 0) ) then
         if ( nwait > 0 ) then
            if ( ui == IZERO ) then
               call G_flagerr%set ( stat = G_UERROR, where = HERE, msg =  &
               'An "end'//trim(waitingfor(nwait))//'" missing in the expression' )
            else
               call G_flagerr%set ( stat = G_UERROR, where = HERE, msg =  &
               'The EOF was reached while an "end'//trim(waitingfor(nwait))//'" was expected')
            end if
            err = 2 ; exit
         end if  
      end if      
!
!-    update the current line number:
!
      if ( ui == STDIN .and. G_flows(flowId)%curLine == IMAX ) G_flows(flowId)%curLine = 0
      
      G_flows(flowId)%curLine = G_flows(flowId)%curLine + numl  
!
!-    make a copy of this record on the ouput file (if not stdout):
!   
      !!if ( G_uo /= STDOUT .and. len_trim(rec) > 0 ) &  ! usefull? (05/24)
      !!     write(G_uo,'(a)') G_prompt%str //trim(rec)  
!
!-    See if an error has occured during the read or if the eof has been reached. 
!   
      if ( G_flagerr /= IZERO ) then
   
         if ( G_flagerr > IZERO ) then
!
!-          an error has occured
!      
            call G_flagerr%AddTrace(HERE); err = 3 ; exit
         else
!
!-          the end-of-file has been reached (close the script if the read was not on stdin)
!      
            G_flagerr = err_t()

            if ( ui /= STDIN .and. ui /= IZERO ) then
               close(G_flows(flowId)%unit)
               G_flows(flowId)%unit = -1 !! 0 !!-1 means "closed" !04/24
               exit
            end if 
 
         end if
      end if   
!
!-    Replace simple quotation marks (') by double quotation marks (") except those used for
!     transposition:
!
      call pk2Interpreter_ConvertQuotes ( rec, G_flagerr )

      if ( G_flagerr > IZERO ) then
         call G_flagerr%AddTrace(HERE); err = 1 ; exit
      end if      
!
!-    Split the record if formed by tokens separated by the delimiters ',;' (multi-records).
!     (note: ',' and ';' enclosed between a pair of symbols given in "opcl" are not considered
!     as delimiters of records):
!   
      if ( nfortproc > 0 .or. ignore ) then
         ! do not split the record when this is a line of a user fortran procedure (it will be
         ! ignored anyway) 
         nSubRec = 1
         if ( .not. allocated(subRec) ) allocate(subRec(1,2))
         subRec(1,1) = rec ; subRec(1,2) = ''
      else
         nSubRec = util_CountTokens ( rec, delims = ',;', BlkToken =.false., opcl = OPCL,  &
                                      tokens = subRec, stat = G_flagerr )
      end if
      
      ignore = .false.
      
      if ( G_flagerr > IZERO ) then   
         call G_flagerr%AddTrace(HERE); err = 1 ; exit
      end if   

      if ( nSubRec /= 0 ) then
!
!-       Save them into the stack:
!      
         do i = 1, nSubRec

            if ( index(subRec(i,1)%str, 'fortranproc') == 1 ) then
               if ( G_dispPrompt .and. (ui == STDIN .or. ui == IZERO) ) then 
                  err = 1
                  call G_flagerr%set ( stat=G_UERROR, where=HERE, msg = &
                                 'A Fortran function cannot be defined in interactive mode' )
                  exit
               end if
            
               nfortproc = nfortproc + 1
               
               ignore = .true.
                           
            else if ( util_RemoveSpaces1(subRec(i,1)%str) == 'endfortranproc' ) then

               nfortproc = nfortproc - 1
               if ( nfortproc < 0 ) then
                  err = 1
                  call G_flagerr%set ( msg = 'Unexpected "endfortranproc"', &
                                       stat = G_UERROR, where = HERE )
                  exit
               end if

               ignore = .true.
            
            end if
            
            if ( nfortproc > 0 ) exit
            
            if ( util_StringLow(subRec(i,1)%str) == 'quit' .or. &
                 util_StringLow(subRec(i,1)%str) == 'exit' ) then
               subRec(i,1)%str = 'quit'
            
            else if ( index(subRec(i,1)%str, 'for ') == 1 ) then
               nwait = nwait + 1
               if ( nwait > NESTED ) then ; err = 4 ; exit ; end if
               waitingfor(nwait) = 'for        '
               nfor = nfor + 1
            
            else if ( util_RemoveSpaces1(subRec(i,1)%str) == 'endfor' ) then
               if ( nwait == 0 ) then
                  err = 1
               else if ( nwait > 0 ) then
                  if ( waitingfor(nwait) /= 'for        ' ) err = 1
               end if
               
               if ( err == 1 ) then
                  call G_flagerr%set ( msg='Unexpected "endfor"', stat=G_UERROR, where=HERE )
                  exit
               end if

               subRec(i,1)%str = 'endfor'
               nwait = nwait - 1
            
            else if ( index(subRec(i,1)%str, 'if ') == 1 ) then
               nwait = nwait + 1
               if ( nwait > NESTED ) then ; err = 4 ; exit ; end if
               waitingfor(nwait) = 'if         '
               nif = nif + 1
            
            else if ( util_RemoveSpaces1(subRec(i,1)%str) == 'endif' ) then
               if ( nwait == 0 ) then
                  err = 1
               else if ( nwait > 0 ) then
                  if ( waitingfor(nwait) /= 'if         ' ) err = 1
               end if
               
               if ( err == 1 ) then
                  call G_flagerr%set ( msg='Unexpected "endif"', stat=G_UERROR, where=HERE )
                  exit
               end if
            
               subRec(i,1)%str = 'endif'
               nwait = nwait - 1
                                                         
            end if
         
         end do     
         
         if ( err == 4 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE,                          &
                                  msg = 'Too many number of nested blocs (if, for, ...)' )
            err = 1
         end if
         
         if ( err /= 0 ) exit 

         if ( nfortproc > 0 .or. ignore) cycle     

         call G_flows(flowId)%insertRecordsIntoStack ( SubRec, flowId, G_flagerr )
      
         if ( G_flagerr > IZERO ) then
            call G_flagerr%AddTrace(HERE); err = 1 ; exit
         end if        
      
      else
!
!-       Empty record
!
         if ( ui /= IZERO ) cycle         
      end if
   
      if ( nwait == 0 ) then
!
!-       for FOR or IF instructions add informations into the stack
!      
         if ( nif /= 0 .or. nfor /= 0 ) then
            call G_flows(flowId)%completeStack ( nfor, nif, G_flagerr )
            if ( G_flagerr > IZERO ) then
               call G_flagerr%AddTrace(HERE); err = 1 ; exit
            end if  
         end if 
            
      end if

      iread = iread + 1
      
      if ( nread == 0 ) then
         cycle ! read until the eof
      else if ( iread >= nread .and. nwait == 0 ) then
         exit  ! or read "nread" lines (as long as the blocks are complete)
      end if

   end do
   
   if ( err /= 0 ) then
      if ( err == 1 ) then
         call CalmatUtil_Error (empty = .true.                  , &
                                rec   = rec0                    , &
                                file  = G_flows(flowId)%fileName, &
                                line  = G_flows(flowId)%curLine   )
      else if ( err == 2 ) then
         call CalmatUtil_Error (empty = .true.                  , &
                                rec   = rec0                    , &
                                file  = G_flows(flowId)%fileName  )
      else if ( err == 3 ) then
         call CalmatUtil_Error (empty = .true.)
      end if

   end if

!debug: 
!call CalmatUtil_printFlows (flowId)
   
   END SUBROUTINE Calmat_read


!=============================================================================================
   RECURSIVE SUBROUTINE Calmat_parse ( flowId )!, start )
!=============================================================================================
   integer(Ikind), intent(in    ) :: flowId
   !integer(Ikind), intent(in out) :: start
!---------------------------------------------------------------------------------------------    
!  Parse the instructions of the stack of the current flow
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = "Calmat_parse"
   character(len=*), parameter   :: OPCL = '()[]{}""'!   //"''"
   character(len=1), parameter   :: S1 = char(192), S2 = char(193)
   character(len=2), parameter   :: OPERS(4) = ["=="  , "~="  , ">="  , "<="  ]
   character(len=2), parameter   :: TMPOP(4) = [S1//S1, S1//S2, S2//S1, S2//S2]
   integer  (Ikind), parameter   :: WHOLE =-1
   integer  (Ikind)              :: nrepl(size(OPERS)), i, nsides, lstr, ins, err
   character(len=:), allocatable :: lhs, str
   integer  (Ikind), allocatable :: poseq(:)
!---------------------------------------------------------------------------------------------    
   
   if ( G_task /= G_CONTINUE ) return
   
!print*,'*** Calmat_parse ***'
!
!- Parse each instruction of the stack:
!      
   ASSOCIATE ( stack => G_flows(flowId)%stack )
   
   !!do ins = start, G_flows(flowId)%nStack !!!19/04/24
   do ins = G_flows(flowId)%start, G_flows(flowId)%nStack !19/04/24

      G_ins = G_ins + 1
      
      stack(ins)%globalInsNum = G_ins
               
      stack(ins)%parsed = .true.

      str = trim(adjustl(stack(ins)%rec)) ; lstr = len_trim(str)

      if ( lstr == 0 ) then
!
!-       Empty instruction
!   
         stack(ins)%rhs = ''
         stack(ins)%lhs = ''
         stack(ins)%nlhs = 0

         cycle
   
      else if ( stack(ins)%blk == G_FORBLOCK ) then
!
!-       First instruction of a block FOR: extract the counter name and parse the bounds 
!   
         call Calmat_ParseFor ( stack(ins) )
         
         if ( G_flagerr > IZERO ) then
            call G_flagerr%AddTrace(HERE) ; exit
         end if
         
         cycle
      
      else if ( stack(ins)%blk == G_IFBLOCK ) then
!
!-       First instruction of a block IF: parse each branch condition 
!   
         call Calmat_ParseIf ( stack(ins), flowId )
         
         if ( G_flagerr > IZERO ) then
            call G_flagerr%AddTrace(HERE) ; exit
         end if
         
         cycle

      else if ( stack(ins)%blk < 0 ) then
!
!-       Last instruction of a block construct
!
         cycle
         
      else if ( stack(ins)%blk == G_EXECBLOCK ) then
!
!-       "exec" instruction
!
         call Calmat_ParseExec ( stack(ins) )

         if ( G_flagerr > IZERO ) then
            call G_flagerr%AddTrace(HERE) ; exit
         end if

         if ( stack(ins)%breakpt == 0 ) then   
!
!-          Read the instructions contained in the script (caution: we read the entire file):
!
            call Calmat_read  ( flowId = stack(ins)%nextFlow, nLine = WHOLE )!start = st, nLine = WHOLE )
            if ( G_task /= G_CONTINUE ) return  
!
!-          Parse now this flow of instructions (caution: the entire file is parsed. If an 
!           error is detected during this phase no evaluation will be performed even for 
!           instructions preceding this error): 

            call Calmat_parse ( flowId = stack(ins)%nextFlow )!, start = st )
            if ( G_task /= G_CONTINUE ) return  
            
         else
!
!-          The script name itself must be evaluated. Set a breakpoint:
!         
            stack(ins)%breakpt = ins
            if ( ins == G_flows(flowId)%nStack ) stack(ins)%breakpt = 0
            exit
            
         end if
         
      else if ( index(str,'clear') == 1 ) then

         call Calmat_ParseClear ( str )

         if ( G_flagerr > IZERO ) then
            call G_flagerr%AddTrace(HERE) ; exit
         end if
         
      end if
!
!-    Replace all occurences of relational operators ('==', '~=', '>=', '<=') in "rec" by one
!     of the characters given in TMPOP:
!
      do i = 1, size(OPERS)
         str = util_ReplaceSubstring1 (str, OPERS(i), TMPOP(i), nrepl(i))
      end do
!
!-    Find (if present) the occurence(s) of '=' that are not between ()[]{}""'':
!
      nsides = util_CountTokens ( str = str, delims = "=", BlkToken = .false., opcl = OPCL, &
                                  pos = poseq, stat = G_flagerr )
                   
      if ( G_flagerr > IZERO ) then
         call G_flagerr%AddTrace(HERE) ; exit
      end if
   
      lhs = '' ; err = 0
!
!-    If more than two sides: error
!      
      if ( nsides > 2 .or. size(poseq) > 1 ) err = 1
!
!-    If "str" begins or ends with '=': error
!   
      if ( size(poseq) == 1 ) then
         if ( poseq(1) <= 1    ) err = 1
         if ( poseq(1) >= lstr ) err = 1
      end if   
   
      if ( err /= 0 ) then
!
!-       Exit with error if there are more than two sides in the expression
!   
         call G_flagerr%set ( stat = UERROR, where = HERE, msg =  &
                              "Expression << "//stack(ins)%rec//" >> has an error" )
         exit
      end if
!
!-    Reconstruct the initial string "str" (put the removed relational operators back):
!
      do i = 1, size(OPERS)
         if ( nrepl(i) /= 0 ) str = util_ReplaceSubstring1 (str, TMPOP(i), OPERS(i))
      end do      
!
!-    Store the expression of the rhs and the lhs:
!
      if ( nsides == 1 ) then
         stack(ins)%rhs = str 
         stack(ins)%lhs = ''
      else
         stack(ins)%rhs = trim(adjustl(str(poseq(1)+1:)))
         stack(ins)%lhs = trim(adjustl(str(:poseq(1)-1)))
      end if
!
!-    Tokenize the rhs:
!
      call CalmatCmd_Tokenizor ( stack(ins) )

      if ( G_flagerr > IZERO ) then
         call G_flagerr%AddTrace(HERE) ; exit
      end if
!
!-    Extract the name of the variable(s) invoqued in the lhs:
!
      call Calmat_lhsAnalysis ( stack(ins) )

      if ( G_flagerr > IZERO ) then
         call G_flagerr%AddTrace(HERE) ; exit
      end if

   end do

   if ( G_flagerr > IZERO ) then
      G_flows(flowId)%CurLine = stack(ins)%numl
      call CalmatUtil_Error (empty = .true.                  , &
                             rec   = stack(ins)%rec          , &
                             file  = G_flows(flowId)%fileName, &
                             line  = G_flows(flowId)%CurLine   )
      return
   end if   
      
   END ASSOCIATE ! end of association stack => G_flows(flow+Id)%stack

!print*,'*** End Calmat_parse ***'
   
   END SUBROUTINE Calmat_parse
   
   
!=============================================================================================
   RECURSIVE SUBROUTINE Calmat_execute ( flowId )!, start )
!=============================================================================================
   integer(Ikind), intent(in    ) :: flowId
   !integer(Ikind), intent(in out) :: start
!---------------------------------------------------------------------------------------------                   
!  Executes the set of instructions present in the stack of the flow #flowId
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'Calmat_execute'
   integer  (Ikind)            :: i, ins
!---------------------------------------------------------------------------------------------

!print*,'Calmat_execute', start-1, G_flows(flowId)%nstack

   if ( G_task /= G_CONTINUE ) return

   ins = G_flows(flowId)%start - 1 !! ins = start - 1 !19/04/24

   do while ( ins < G_flows(flowId)%nstack )
   
      ins = ins + 1

      call Calmat_InstructionType ( ins )
      
      if ( G_flagerr > IZERO ) then
         call G_flagerr%AddTrace(HERE) ; exit
      end if
      
      if ( G_task /= G_CONTINUE ) return
      
   end do
   
   if ( G_flagerr > IZERO ) then
      G_flows(flowId)%CurLine = G_flows(flowId)%stack(ins)%numl
      
      call CalmatUtil_Error (empty = .true.                        , &
                             rec   = G_flows(flowId)%stack(ins)%rec, &
                             file  = G_flows(flowId)%fileName      , &
                             line  = G_flows(flowId)%CurLine         )      
      return
   end if   
!
!- Exit if requested:
!   
   if ( G_task == G_STOP ) return
!
!- Change the status of the added variables from "temporary" to "used"
!
   do i = 1, G_nvar
      if ( abs(G_vars(i)%status) == G_TEMPORARY ) G_vars(i)%status = G_USED
   end do   
   
   do i = 1, G_nobj
      if ( abs(G_objs(i)%status) == G_TEMPORARY ) G_objs(i)%status = G_USED
   end do
   
   CONTAINS
   
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   ! Internal procedures of Calmat_execute:
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE Calmat_InstructionType ( ins )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      integer(Ikind), intent(in out) :: ins
   !------------------------------------------------------------------------------------------                 
   !  
   !------------------------------------------------------------------------------------------                  

   !- local variables: -----------------------------------------------------------------------
      character(len=*), parameter :: HERE = 'Calmat_InstructionType'
      integer  (Ikind)            :: insType
   !------------------------------------------------------------------------------------------                  

      ASSOCIATE ( stack => G_flows(flowId)%stack )
   
      insType = stack(ins)%blk

      select case ( insType )

         case ( 0 )
!
!-          This is a single instruction:
!            
            call CalmatCmd_Evaluator ( stack(ins) ) 

            if_error_trace_and_RETURN ( G_flagerr, HERE )
                                                                         
            if ( CalmatCmd_forloopbreak ) return
               
         case ( G_FORBLOCK ) 
!
!-          This is a FOR block construct:
!                     
            call Calmat_ForConstruct ( ins ) 

            if_error_trace_and_RETURN ( G_flagerr, HERE )
            
            if ( G_task /= G_CONTINUE ) return
            
            ins = ins + stack(ins)%nlink
                                 
         case ( G_IFBLOCK )
!
!-          This is an IF block construct:
!                     
            call Calmat_IfConstruct ( ins )

            if_error_trace_and_RETURN ( G_flagerr, HERE )
            
            if ( G_task /= G_CONTINUE ) return
            
            ins = ins + stack(ins)%nlink

         case ( G_EXECBLOCK )

            call Calmat_ExecConstruct ( ins )

            if_error_trace_and_RETURN ( G_flagerr, HERE )
            
         case ( -G_FORBLOCK, -G_IFBLOCK )
!
!-          This is a closing-block keyword ("endfor" or "endif")
!                                      
         case default
         
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='Unknown instruction type' )
            return

      end select

      END ASSOCIATE ! end of the association stack => G_flows(flowId)%stack

      END SUBROUTINE Calmat_InstructionType
   

   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE Calmat_ExecConstruct ( ins )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      integer(Ikind), intent(in) :: ins
   !------------------------------------------------------------------------------------------                  
   !  Executes the exec command
   !------------------------------------------------------------------------------------------              

   !- local variables: -----------------------------------------------------------------------
      character(len=*), parameter   :: HERE = 'Calmat_ExecConstruct'
      integer  (Ikind), parameter   :: WHOLE =-1      
      integer  (Ikind)              :: newflow
      character(len=:), allocatable :: fileName
   !------------------------------------------------------------------------------------------              

      ASSOCIATE ( stack => G_flows(flowId)%stack )

      if ( stack(ins)%breakpt == 0 .and. stack(ins)%nextFlow /= 0 ) then
         !st = 1
         G_flows(stack(ins)%nextFlow)%start = 1 !19/04/24
         call Calmat_execute ( stack(ins)%nextFlow )!, st )
         if ( G_task /= G_CONTINUE ) return
      else
         call CalmatCmd_Exec ( stack(ins)%rec, fileName = fileName )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
               
         newflow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = fileName )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
                        
         call Calmat_read    ( flowId = newflow, nLine = WHOLE )!, start = st, nLine = WHOLE )  
         if ( G_task /= G_CONTINUE ) return
               
         call Calmat_parse   ( flowId = newflow )!, start = st )
         if ( G_task /= G_CONTINUE ) return
               
         call Calmat_execute ( flowId = newflow )!, start = st )
         if ( G_task /= G_CONTINUE ) return

         call G_flows(newflow)%destroyFlow ( G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )

         !start = stack(ins)%breakpt + 1
         G_flows(flowId)%start = stack(ins)%breakpt + 1 !/04/24
         
         call Calmat_parse ( flowId = flowId )!, start = start )
         if ( G_task /= G_CONTINUE ) return
      end if

      END ASSOCIATE ! end of the association stack => G_flows(flowId)%stack

      END SUBROUTINE Calmat_ExecConstruct


   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE Calmat_IfConstruct ( ins )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      integer(Ikind), intent(in) :: ins  
   !------------------------------------------------------------------------------------------              
   !  Executes an IF block
   !------------------------------------------------------------------------------------------              

   !- local variables: -----------------------------------------------------------------------
      character(len=*), parameter :: HERE = 'Calmat_IfConstruct'
      integer  (Ikind)            :: trueBranch, curIns
   !------------------------------------------------------------------------------------------              

      if ( G_task /= G_CONTINUE ) return
   
      ASSOCIATE ( stack => G_flows(flowId)%stack )
!
!-    Determine the branch to go (evaluate the branch conditions):
!
      call Calmat_IfCond ( stack(ins), trueBranch )
   
      if ( G_flagerr > IZERO ) then
         call G_flagerr%AddTrace(HERE)      
         call CalmatUtil_Error (empty = .true.                  , &
                                rec   = stack(ins)%rec          , &
                                file  = G_flows(flowId)%fileName, &
                                line  = stack(ins)%numl           )  
         return
      end if
!
!-    Exit if all conditions are false (trueBranch = 0):
!
      if ( trueBranch == 0 ) return
!
!-    First instruction Id of this branch
!            
      curIns = stack(ins)%startBr(trueBranch)
!
!-    Scroll through all the instructions of this branch
!       
      do 

         call Calmat_InstructionType ( curIns )         
         if_error_trace_and_EXIT ( G_flagerr, HERE )
         
         if ( G_task /= G_CONTINUE ) return
      
         curIns = curIns + 1
                    
         if ( curIns > stack(ins)%endBr(trueBranch) ) exit
            
      end do
   
      if ( G_flagerr > IZERO ) then
         G_flows(flowId)%CurLine = stack(curIns)%numl
         call CalmatUtil_Error (empty = .true.                  , &
                                rec   = stack(curIns)%rec       , &
                                file  = G_flows(flowId)%fileName, &
                                line  = G_flows(flowId)%CurLine   )
         return
      end if
   
      END ASSOCIATE ! end of the association stack => G_flows(flowId)%stack   
               
      END SUBROUTINE Calmat_IfConstruct


   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE Calmat_ForConstruct ( ins )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      integer(Ikind), intent(in) :: ins
   !------------------------------------------------------------------------------------------                
   !  Executes a FOR block 
   !------------------------------------------------------------------------------------------                

   !- local variables: -----------------------------------------------------------------------
      character(len=*), parameter :: HERE = 'Calmat_ForConstruct'
      integer  (Ikind)            :: curIns, counter
      integer  (Ikind)            :: initVal, endVal, stepVal, lastIns
   !------------------------------------------------------------------------------------------                

      if ( G_task /= G_CONTINUE ) return
      
      ASSOCIATE ( stack => G_flows(flowId)%stack )
!
!-    Evaluate the bounds of the for block
!
      call Calmat_ForBounds ( stack(ins), initVal, endVal, stepVal ) 
   
      if ( G_flagerr > IZERO ) then
         call G_flagerr%AddTrace(HERE)
         call CalmatUtil_Error (empty = .true.                  , &
                                rec   = stack(ins)%rec          , &
                                file  = G_flows(flowId)%fileName, &
                                line  = stack(ins)%numl           )  
         return
      end if
   
      lastIns = ins + stack(ins)%nlink ! last instruction Id of the For block
!
!-    Start the loop:
!      
      do counter = initVal, endVal, stepVal
!
!-       store the counter value in G_vars:
!
         G_vars(stack(ins)%cIvar) = counter 
      
         curIns = ins + 1 ! first instruction Id of the FOR block      
!
!-       Scroll through all the instructions of this block:
!       
         do
      
            call Calmat_InstructionType ( curIns ) 
            if_error_trace_and_EXIT(G_flagerr, HERE)
                        
            if ( G_task /= G_CONTINUE ) return
         
            curIns = curIns + 1
         
            if ( curIns > lastIns ) exit
         
         end do

         if ( G_flagerr > IZERO ) then
            G_flows(flowId)%CurLine = stack(curIns)%numl
            call CalmatUtil_Error (empty = .true.                  , &
                                   rec   = stack(curIns)%rec       , &
                                   file  = G_flows(flowId)%fileName, &
                                   line  = G_flows(flowId)%CurLine   )
      
            return
         end if
      
         if ( CalmatCmd_forloopbreak ) then
            CalmatCmd_forloopbreak = .false.
            exit
         end if
            
      end do    
   
      END ASSOCIATE ! end of the association stack => G_flows(flowId)%stack
               
      END SUBROUTINE Calmat_ForConstruct

   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   ! end of internal procedures of Calmat_execute
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 
   END SUBROUTINE Calmat_execute


!=============================================================================================
   SUBROUTINE Calmat_ForBounds ( instr, initVal, endVal, stepVal )
!=============================================================================================   
   type   (ins_t), intent(in out) :: instr
   integer(Ikind), intent(   out) :: initVal, endVal, stepVal
!---------------------------------------------------------------------------------------------                   
!  Evaluates the bounds of a for block construct
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'Calmat_ForBounds'
   type     (pk2_t), allocatable :: outval(:)
   integer  (Ikind), allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------
   
   initVal = instr%cVal(1) ; endVal = instr%cVal(2) ; stepVal = instr%cVal(3)
   
   if ( instr%cHdl(1)%initialized ) then      
      call pk2Interpreter_Evaluator ( handle  = instr%cHdl(1)   , &
                                      vars    = G_vars(1:G_nvar), &
                                      valExpr = outval          , &
                                      flagerr = G_flagerr       )

      if_error_trace_and_RETURN ( G_flagerr, HERE )                                      

      if ( outval(1)%typ /= ITYP ) then
         call G_flagerr%set ( stat = UERROR, where = HERE, &
                               msg = 'Bounds and increment of a for loop must be integers' )
         return
      end if
      
      call outval(1)%GetMat (tmp, G_flagerr)
      if_error_trace_and_RETURN ( G_flagerr, HERE )                                      
                  
      initVal = tmp(1,1)
   end if
   
   if ( instr%cHdl(2)%initialized ) then      
      call pk2Interpreter_Evaluator ( handle  = instr%cHdl(2)   , &
                                      vars    = G_vars(1:G_nvar), &
                                      valExpr = outval          , &
                                      flagerr = G_flagerr       )

      if_error_trace_and_RETURN ( G_flagerr, HERE )                                      
                                      
      if ( outval(1)%typ /= ITYP ) then
         call G_flagerr%set ( stat = UERROR, where = HERE, &
                               msg = 'Bounds and increment of a for loop must be integers' )
         return
      end if

      call outval(1)%GetMat (tmp, G_flagerr)
      if_error_trace_and_RETURN ( G_flagerr, HERE )                                      
                           
      endVal = tmp(1,1)  
   end if
   
   if ( instr%cHdl(3)%initialized ) then      
      call pk2Interpreter_Evaluator ( handle  = instr%cHdl(3)   , &
                                      vars    = G_vars(1:G_nvar), &
                                      valExpr = outval          , &
                                      flagerr = G_flagerr       )

      if_error_trace_and_RETURN ( G_flagerr, HERE )                                      
                                      
      if ( outval(1)%typ /= ITYP ) then
         call G_flagerr%set ( stat = UERROR, where = HERE, &
                               msg = 'Bounds and increment of a for loop must be integers' )
         return
      end if
         
      call outval(1)%GetMat (tmp, G_flagerr)
      if_error_trace_and_RETURN ( G_flagerr, HERE )                                      
                           
      stepVal = tmp(1,1)   
   end if

   END SUBROUTINE Calmat_ForBounds
      

!=============================================================================================
   SUBROUTINE Calmat_IfCond ( instr, trueBranch )
!============================================================================================= 
   type   (ins_t), intent(in out) :: instr
   integer(Ikind), intent(   out) :: trueBranch
!--------------------------------------------------------------------------------------------- 
!  Evaluates the branch conditions of an IF-ELSEIF-ELSE-ENDIF block
!  It returns in "trueBranch" the # of the branch for which the condition is true (0 if any).
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------    
   character(len=*), parameter   :: HERE = 'Calmat_IfCond'
   integer  (Ikind)              :: k
   type     (pk2_t), allocatable :: outval(:)
   logical         , allocatable :: tmp(:,:)
!--------------------------------------------------------------------------------------------- 
      
   trueBranch = 0
   
   do k = 1, instr%nBr
      if ( len_trim(instr%tExpr(k)%str) /= 0 ) then
         call pk2Interpreter_Evaluator ( handle  = instr%tHdl(k)   , &
                                         vars    = G_vars(1:G_nvar), &
                                         valExpr = outval          , &
                                         flagerr = G_flagerr       )

         if_error_trace_and_RETURN ( G_flagerr, HERE )                                         

         call outval(1)%GetMat (tmp, G_flagerr)
         if_error_trace_and_RETURN ( G_flagerr, HERE )                                                  

! modified 05/24:         
!          if ( tmp(1,1) ) then 
!             trueBranch = k
!             return
!          end if

         if ( all(tmp) ) then
            trueBranch = k
            return
         end if
! end modified 05/24
                  
      else
         trueBranch = k
      end if
   end do
      
   END SUBROUTINE Calmat_IfCond
      
   
!=============================================================================================
   RECURSIVE SUBROUTINE Calmat_Initialize ( warning, welcome, dispRes, stat )
!=============================================================================================
   logical    , optional, intent(in    ) :: warning, welcome, dispRes
   type(err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
! Initializes 
! . the predefined variables and the user's ones (list G_vars),
! . the output unit,
! . the G_dispPrompt value to .true. (read from the stdin),
! . the list of available functions of the interpreter,
! . the default parameters (initialized or read from the file '.calmatdefaults').
!--------------------------------------------------------------------------- R.H. 11/18, 02/19

!- local variables: -------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'Calmat_Initialize'  
   real     (Rkind)              :: nan, inf
   integer  (Ikind)              :: i, j, n
   integer  (Ikind), allocatable :: indx(:)
   type     (err_t)              :: flag
   logical                       :: halting, warn_save, disp_save
   character(len=:), allocatable :: msg
!--------------------------------------------------------------------------------------------- 
!print*,HERE

!
!- Error handling mode:
!
   if ( present(stat) ) then
      halting = G_OFF ! automatic halting will be disabled
   else
      halting = G_ON  ! automatic halting will be enabled
   end if
!
!- Warning Display:
!   
   if ( present(warning) ) then
      warn = warning
   else
      if ( first ) warn = G_OFF
   end if
      
   call err_SetHaltingMode ( halting = halting, unit = STDOUT, DisplayWarning = warn ) 
!
!- Display or not the result. By default G_disp is used (initialized in CalmatGlobal_m)
!   
   if ( present(dispRes) ) then
      dispr = dispRes
   else
      if ( first ) dispr = G_disp
   end if
!
!- Print or not a welcome message:  
! 
   if ( present(welcome) ) then
      welc = welcome
   else
       if ( G_init ) welc = G_OFF
   endif 

   first = .false.
!
!- Return, if not the first call to calmat (already initialized):
!
   if ( G_init ) return
   
   G_init = .true.
!
!- Get the version ID, compilation date and compilation options:
!
   call CalmatVersion_versionID ()
!
!- See if a default settings file exists in the current directory or in the home directory. 
!  Otherwise, create such a file (in the home directory) with fixed default values:
!     
   call CalmatUtil_defaultSettings ( G_flagerr )
   
    if ( G_flagerr > IZERO ) then
       call G_flagerr%AddTrace(HERE)     
       call CalmatUtil_Error (empty = .true.)
       return
    endif 

   call signalHandler_SignalCatch &
        ( unit = G_uo, title = '--> Calmat Info:', farewellMsg = G_farewell )

   G_ins = 0   
!
!- Initialize a set of flows (will be resized if needed):
!
   call CalmatUtil_AllocFlow ( IZERO, G_flagerr )
   
   if ( G_flagerr > IZERO ) then
      call G_flagerr%AddTrace(HERE)
      call CalmatUtil_Error (abort = .true., empty = .false.)
      return
   end if
!
!- Initialize a set of G_maxnVar variables (will be resized if needed):
!
   G_maxnVar = G_incnVar
   allocate(G_vars(G_maxnVar), G_varNames(G_maxnVar))
!
!- Allocate G_maxnObj variables (will be resized if needed):
!
   G_maxnObj = G_incnObj
   allocate(G_objs(G_maxnObj), G_objNames(G_maxnObj))

   G_nVar = 0 ; G_nObj = 0 
!
!- The variable #1 ('ans') will contain the last result (only if this result is not explicitly 
!  assigned to a specified variable): 
!  
   n = 0       
   !n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(name = "ans"), status = G_USED )
!
!- Set some predefined variables. These variables cannot be modified by the user:
!
   nan = ieee_value(nan, IEEE_QUIET_NAN) ; inf = ieee_value(inf, IEEE_POSITIVE_INF)

   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(CIMAG, name = "%i"   ), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(PI   , name = "%pi"  ), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(NEPER, name = "%e"   ), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(EPS  , name = "%eps" ), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(IMAX , name = "%imax"), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(RMIN , name = "%rmin"), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(RMAX , name = "%rmax"), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(LZERO, name = "%f"   ), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(LONE , name = "%t"   ), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(inf  , name = "%inf" ), status = G_PROTECTED )
   n = n + 1 ; G_vars(n) = var_t ( p = pk2_t(nan  , name = "%nan" ), status = G_PROTECTED )
!
!- be quiet! nan and inf are not your enemies here:
!
   call ieee_set_flag (ieee_all, flag_value = G_QUIET)
   
   do i = 1, n
      G_varNames(i)%str = (G_vars(i)%GetName())
   end do
   
   G_nvar = n
!
!- Set the output unit to stdout:
!
   G_uo = STDOUT
!
!- The default value of "G_dispPrompt" is .true. (i.e. read from the stdin):  
!       
   G_dispPrompt = .true.
!
!- List of available functions and available operators:
!
   call pk2Interpreter_FuncAndOperList ( )   

   n = 0 ; allocate(indx(NFC))
   
   do i = NFC+1, NFC+nuserf
      do j = 1, NFC
         if ( FuncList(i)%str == FuncList(j)%str ) then
            n = n + 1
            indx(n) = i
         end if
      end do
   end do
   
   if ( n > 0 ) then
      if ( n == 1 ) then
         msg = "The built-in '" // FuncList(indx(1))%str // "' function is overwritten "// &
               " by its user version"
      else
         msg = "The following built-in functions are overwritten by their user versions:"
         do i = 1, n
            msg = msg // NLT // ". " // FuncList(indx(i))%str
         end do
      end if
      call G_flagerr%set ( stat = G_WARNING, where = HERE, msg = msg )
      call CalmatUtil_Error ( empty = .false. )
   end if
!
!- sort the list of functions in alphabetical order:
!   
   n = NFC+nuserf
   
   allocate(G_FuncNames(n), G_FuncDescr(n)) ; G_FuncNames = FuncList   
   
   call util_Sort(G_FuncNames, 'i', is_CaseInsensitive = .true., indx = indx, stat = G_flagerr) 

   if ( G_flagerr > IZERO ) then   
      call G_flagerr%AddTrace(HERE)
      call CalmatUtil_Error (abort = .true., empty = .false.)
      return
   end if
   
   do i = 1, n
      if ( allocated(FuncDesc(indx(i))%str) ) then
         G_FuncDescr(i)%str = FuncDesc(indx(i))%str
      else
         G_FuncDescr(i)%str = ''
      end if
   end do
!
!- List of available calmat commands:
!
   call CalmatCmd_CmdList ( )     
!
!- sort this list of commands in alphabetical order:
!   
   allocate(G_CmdNames(NCMD), G_CmdDescr(NCMD)) ; G_CmdNames  = CmdList

   call util_Sort(G_CmdNames , 'i', is_CaseInsensitive = .true., indx = indx, stat = G_flagerr) 

   if ( G_flagerr > IZERO ) then   
      call G_flagerr%AddTrace(HERE)
      call CalmatUtil_Error (abort = .true., empty = .false.)
      return
   end if

   do i = 1, NCMD
      if ( allocated(CmdDesc(indx(i))%str) ) then
         G_CmdDescr(i)%str = CmdDesc(indx(i))%str
      else
         G_CmdDescr(i)%str = ''
      end if   
   end do      
!
!- Calmat interprets its own configuration file (!):
!   
   warn_save = warn ; disp_save = dispr

   call Calmat ( fileIn  = G_filedef, warning = .true., welcome = .false., &
                 dispRes = .false.  , stat    =  flag                      )

   if ( flag > IZERO ) then
      call flag%AddMsg ( before = .true., newline = .true., &
                      msg ='Stop reading the settings file because of the following error:' )
      call flag%display()       
   end if 
! 
!- restore user options (potentially modified by the above call to calmat):
!   
   warn = warn_save ; dispr = disp_save
   
   if ( present(welcome) ) then ; welc = welcome ; else ; welc = G_welcome ; end if
   
   call err_SetHaltingMode ( halting = halting, unit = STDOUT, DisplayWarning = warn )  
   
   G_disp = dispr
    
   END SUBROUTINE Calmat_Initialize
   
   
!=============================================================================================        
   SUBROUTINE Calmat_ResetAll
!=============================================================================================

!- local variables: -------------------------------------------------------------------------- 
!---------------------------------------------------------------------------------------------

   if ( allocated(G_flows    ) ) deallocate(G_flows    )
   if ( allocated(G_vars     ) ) deallocate(G_vars     )
   if ( allocated(G_objs     ) ) deallocate(G_objs     )
   if ( allocated(G_varNames ) ) deallocate(G_varNames )
   if ( allocated(G_objNames ) ) deallocate(G_objNames )
   if ( allocated(G_FuncNames) ) deallocate(G_FuncNames)
   if ( allocated(G_FuncDescr) ) deallocate(G_FuncDescr)
   if ( allocated(G_CmdNames ) ) deallocate(G_CmdNames )
   if ( allocated(G_CmdDescr ) ) deallocate(G_CmdDescr )

   G_flagerr   = err_t()
      
   G_curFlow    = IZERO         
   G_maxnFlow   = IZERO   
   G_nVar       = IZERO      
   G_maxnVar    = IZERO
   G_nObj       = IZERO        
   G_maxnObj    = IZERO 
   G_ins        = IZERO   
   G_verb       = IONE 
   G_start      = IONE
   G_task       = G_CONTINUE
   G_uo         = STDOUT 
   G_nLine      = 100
   
   G_init       = .false.
   G_disp       = .true.
   G_dispPrompt = .true.      
   
   first = .true. ; warn = G_OFF ; welc = G_OFF ; dispr = G_OFF
                                   
   END SUBROUTINE Calmat_ResetAll
   

!=============================================================================================
   SUBROUTINE Calmat_lhsAnalysis ( instr )
!=============================================================================================
   type(ins_t), intent(in out) :: instr
!---------------------------------------------------------------------------------------------
!  Analyzes the lhs (expression given in instr%lhs) of the instruction "instr"
!
!  Note: a valid instruction corresponding to an assignment may be of the form
!
!          lhs = rhs                        (assignment of a single lhs)
!
!          lhs(expr) = rhs                  (assignment of a portion of a single lhs. 
!                                            expr is the expression of the indice(s))
!
!          lhs(expr1,expr2) = rhs           (assignment of a portion of a single lhs. 
!                                            expr1 & expr2 are the expressions of the indices)
!
!          [lhs1, lhs2, ...] = rhs          (assignment of multiple lhs)
!
!          [lhs1(expr1), lhs2, ...] = rhs   (assignment of multiple lhs)
!          
!          ... 
!         
!  The result of the analysis is stored as follows:
!
!  . instr%nlhs   : the number of lhs
!
!  and for the lhs #i:
!
!  . instr%lhsNames(i): name of the variable of this lhs
!  . instr%lhsVarId(i): corresponding variable Id (in the list G_vars)
!  . instr%lhsIndex(i): expression of the indices (when this is a submatrix assignment)
!
!  The name(s) of the lhs are added to the variables list (G_vars) with a "temporary" status
!  meaning that these variables will be removed from the list if a user error occurs before
!  the assignment. 
!---------------------------------------------------------------------------------------------

!- local variables: -------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'Calmat_lhsAnalysis'
   character(len=:), allocatable :: str, objName, varName, indExpr
   type     (str_t), allocatable :: tok(:,:)
   integer  (Ikind)              :: lstr, lp, rp, i, j, iv, n, dot, varId, objId, cmpId
   logical                       :: is_bal, is_new
!---------------------------------------------------------------------------------------------

   lstr = len_trim(instr%lhs)

   if ( lstr == 0 ) then
!
!-    Case without lhs
!   
      if ( instr%assignment ) then
!
!-       The result will be assigned to "ans":
!      
         instr%nlhs = 1
         if ( .not. allocated(instr%lhsVarId) ) then
            allocate(instr%lhsNames(1), instr%lhsVarId(1), instr%lhsIndex(1))
         else if ( size(instr%lhsVarId) < 1 ) then
            deallocate(instr%lhsNames, instr%lhsVarId, instr%lhsIndex)
            allocate(instr%lhsNames(1), instr%lhsVarId(1), instr%lhsIndex(1))
         end if       
         instr%lhsNames(1)%str = 'ans' ; 
      else
!
!-       Not an assignable result:
!      
         instr%nlhs = 0
         if ( .not. allocated(instr%lhsVarId) ) then
            allocate(instr%lhsVarId(0), instr%lhsNames(0), instr%lhsIndex(0))
         end if
         return
      end if
      
   else
!
!-    Case with one or more lhs:
! 
      is_bal = util_IsBalanced2 ( instr%lhs, "()[]", G_flagerr )
   
      if ( .not. is_bal ) then
         call G_flagerr%set( stat = UERROR, where = HERE, msg = 'Invalid expression << ' // &
                         trim(instr%lhs)//' >> (brackets or parentheses are unbalanced)' )
         return
      end if
!
!-    Find the number of lhs and save their expressions into "%lhsNames(:)":
!   
      lp = index(instr%lhs,'[') ; rp = index(instr%lhs,']',back = .true.)
   
      if ( lp /= 1    ) lp = 0
      if ( rp /= lstr ) rp = 0
   
      if ( lp == 0 .and. rp == 0 ) then
!
!-       There are no brackets in the string "lhs" --> only one variable in the lhs:
!
         n = 1           
      
         instr%nlhs = 1
         if ( .not. allocated(instr%lhsVarId) ) then
            allocate(instr%lhsNames(1), instr%lhsVarId(1), instr%lhsIndex(1))
         else if ( size(instr%lhsVarId) < 1 ) then
            deallocate(instr%lhsNames, instr%lhsVarId, instr%lhsIndex)
            allocate(instr%lhsNames(1), instr%lhsVarId(1), instr%lhsIndex(1))
         end if
            
         instr%lhsNames(1)%str = (instr%lhs)
               
      else if ( lp*rp == 0 .or. rp-lp <= 1 .or. lp > 1 .or. rp < lstr ) then
            
         call G_flagerr%set ( stat = UERROR, where = HERE, msg = 'Invalid expression << ' &
                              //trim(instr%lhs)//' >> (brackets are unbalanced or empty)' )
         return
      else
!
!-       There are brackets in the string "lhs" --> possibly several variables in the lhs:
!     
         str = trim(adjustl(instr%lhs(lp+1:rp-1)))
      
         n = util_CountTokens ( str = str, delims = ",", BlkToken = .false., opcl = "()", &
                                tokens = tok, stat = G_flagerr )
         
         if ( G_flagerr > IZERO ) then ; call G_flagerr%AddTrace(HERE) ; return ; end if
         
         instr%nlhs = n
         if ( .not. allocated(instr%lhsVarId) ) then
            allocate(instr%lhsNames(n), instr%lhsVarId(n), instr%lhsIndex(n))
         else if ( size(instr%lhsVarId) < n ) then
            deallocate(instr%lhsNames, instr%lhsVarId, instr%lhsIndex)
            allocate(instr%lhsNames(n), instr%lhsVarId(n), instr%lhsIndex(n))
         end if

         do i = 1, n
            instr%lhsNames(i)%str = (tok(i,1)%str)
         end do
             
      end if
   end if
!
!- Case where the result is a structure (to be re-worked):
!   
   if ( instr%rhsNCmp > 0 ) then
      if ( instr%nlhs > 1 ) then      
         ! probablement a revoir
         call G_flagerr%set(stat = UERROR, where = HERE, msg = 'Only one ouput object allowed')
         return
      end if
      n = instr%rhsNCmp
      instr%nlhs = n
      varName = (instr%lhsNames(1)%str)      
      deallocate(instr%lhsNames   , instr%lhsVarId   , instr%lhsIndex   )
      allocate  (instr%lhsNames(n), instr%lhsVarId(n), instr%lhsIndex(n))

      do i = 1, n
         instr%lhsNames(i)%str = varName // '.' // (instr%rhsCmps(i)%str)
      end do
!      
!-    when an existing object has the same name, mark all the variables corresponding to its
!     actual components as to be deleted (overwritten by assignment):
      
      call CalmatUtil_FindObj (varName, objId)
      if ( objId /= 0 ) then
         do i = 1, G_objs(objId)%ncmp
            varId = G_objs(objId)%varId(i)
            if ( varId /= 0 ) then
               do j = 1, n
                  if ( G_vars(varId)%GetName() == instr%lhsNames(j)%str ) then
                     varId =-1
                     exit
                  end if
               end do
               if ( varId > 0 ) G_vars(varId)%status = G_DELETE 
            end if
         end do
         G_objs(objId)%status = G_DELETE
      end if
      
   end if
!
!- Tokenize each lhs and put the result into %lhsHdl
!   
   do i = 1, instr%nlhs
   
      lstr = len_trim(instr%lhsNames(i)%str)
      lp = index(instr%lhsNames(i)%str, '(')
      rp = index(instr%lhsNames(i)%str, ')', back =.true.)
      
      if ( lp == 0 .and. rp == 0 ) then
!
!-       there are no parentheses in the string lhs(i)%str
!           
         n = 1
         varName = (instr%lhsNames(i)%str) ! variable name
         indExpr = ''                      ! no indices expression
         
      else if (lp*rp == 0 .or. rp-lp <= 1 .or. lp == 1 .or. rp < lstr) then
            
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Invalid expression << ' &
            //trim(instr%lhsNames(i)%str)//' >> (parentheses are unbalanced or empty)' )
         return
      else
!
!-       lhsNames(i) contains '(' and ')' --> assignment of a submatrix. 
!        Extract the name of the variable and the expressions corresponding to the indices: 
!
         n = 2
         varName = trim( adjustl( instr%lhsNames(i)%str(:lp-1) ) )     ! variable name
         indExpr = trim( adjustl( instr%lhsNames(i)%str(lp+1:rp-1) ) ) ! indices expression
            
      end if 

      dot = index(varName,'.')
      
      if ( dot==1 .or. dot==len_trim(varName) .or. dot/=index(varName,'.',back=.true. )) then
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg =  &
         'The name of a variable should not begin or end with a "."'// NLT // &
         'and should not contain more than one "."' )
         return
      end if
!
!-    Store the variable name and indexe expression:
!
      instr%lhsNames(i)%str = (varName)
      instr%lhsIndex(i)%str = (indExpr)
!
!-    add the name of this lhs into the list of variables (with a temporary status if not 
!     already present):
!
      call CalmatUtil_AddVar ( varName = varName, varStat = G_TEMPORARY , & ! in
                               varId   = varId  , is_new  = is_new      , & ! out
                               flagerr = G_flagerr                        ) ! out

      if_error_trace_and_RETURN ( G_flagerr, HERE )                                         

      instr%lhsVarId(i) = varId
!
!-    If the name "varName" is already used as an object name, mark this object as to be
!     deleted (will be deleted after the evaluation of the rhs):
!
      call CalmatUtil_FindObj (varName, objId)
      
      if ( objId /= 0 ) then
         G_objNames(objId)%str = ''
         G_objs(objId)%status = G_DELETE
         G_objs(objId)%deleteAt = instr%globalInsNum
         do j = 1, G_objs(objId)%ncmp
            iv = G_objs(objId)%varId(j)
            G_varNames(iv)%str = ''
            G_vars(iv)%status = G_DELETE
            G_vars(iv)%deleteAt = instr%globalInsNum
         end do
      end if
      
      if ( G_vars(varId)%status == G_DELETE ) then
         G_varNames(varId)%str  = (G_vars(varId)%name)
         G_vars(varId)%status   = G_TEMPORARY
         G_vars(varId)%deleteAt =-IONE
      end if
            
      if ( dot /= 0 ) then
!
!-       The name of this lhs contains a dot ("."), this is a component of an object. 
!        Extract its name:
!            
         objName = instr%lhsNames(i)%str(1:dot-1)
!
!-       Add this name into the list of objects (with a temporary status if not already 
!        present):
!
         call CalmatUtil_AddObj ( objName = objName, objStat = G_TEMPORARY, & ! in
                                  objId   = objId  , is_new  = is_new     , & ! out
                                  flagerr = G_flagerr                       ) ! out
         if_error_trace_and_RETURN ( G_flagerr, HERE )                                         
!
!-       Insert the variable # into the components of this object (#objId)
!    
         call G_objs(objId)%InsertAComponent ( varId, cmpId, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )                                         
!
!-       Mark this variable (#varId) as associated with the component #cmpId of this object
!                                    
         G_vars(varId)%objId    = objId
         G_vars(varId)%cmpObjId = cmpId
!
!-       If the name "objName" is already used as a variable name, mark this variable as to be
!        deleted (will be deleted after the evaluation of the rhs). 
!
         call CalmatUtil_FindVar ( varName = objName, varId = varId)
         
         if ( varId /= 0 ) then
            G_varNames(varId)%str = ''
            G_vars(varId)%status   = G_DELETE
            G_vars(varId)%deleteAt = instr%globalInsNum
         end if

         if ( G_objs(objId)%status == G_DELETE ) then
            G_objNames(objId)%str  = (G_objs(objId)%name)
            G_objs(objId)%status   = G_TEMPORARY
            G_objs(objId)%deleteAt = -IONE
            do j = 1, G_objs(objId)%ncmp
               iv = G_objs(objId)%varId(j)
               G_varNames(iv)%str  = (G_vars(iv)%name)
               G_vars(iv)%status   = G_TEMPORARY
               G_vars(iv)%deleteAt = -IONE
            end do
         end if
      
      end if
           
   end do

   END SUBROUTINE Calmat_lhsAnalysis
                       

!=============================================================================================
   SUBROUTINE Calmat_ParseExec ( instr )
!=============================================================================================
   type   (ins_t), intent(in out) :: instr
!---------------------------------------------------------------------------------------------    
!---------------------------------------------------------------------------------- R.H. 05/20

!- local variables: --------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'Calmat_ParseExec'
   integer  (Ikind )              :: newflow, l, lp, rp
   character(len=: ), allocatable :: fileName
   character(len=99)              :: cnum
!---------------------------------------------------------------------------------------------    

   lp = index(instr%rec,'(') ; rp = index(instr%rec,')',back=.true.)
      
   if ( lp == 0 .and. rp == 0 ) then
      fileName = trim(adjustl(instr%rec(5:)))
   else if ( lp < rp ) then
      fileName = trim(adjustl(instr%rec(lp+1:rp-1)))
   else
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' ) 
      return  
   end if
   
   l = len_trim(fileName)

   if ( l == 0 ) then
      call G_flagerr%set ( stat = UERROR, where = HERE, msg = &
                  'A file name is required in the expression << '//trim(instr%rec)// ' >>' )
      return
   end if   

   if ( ((fileName(1:1)=='"' .and. fileName(l:l)=='"') .or. & 
         (fileName(1:1)=="'" .and. fileName(l:l)=="'")) .and. index(fileName,'+')==0 ) then
!
!-    The file name of the script is known at the parsing phase. The instructions it contains
!     can be read and parsed before execution. 
!   
      fileName = fileName(2:l-1)
      
      newflow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = fileName )
      if_error_trace_and_RETURN ( G_flagerr, HERE )                                         
      
      write(cnum,'(i0)')newflow
      instr%rec      = 'change_to_flow_#'//trim(cnum)//'_('//fileName//')'
      instr%nextFlow = newflow
      instr%breakpt  = 0
            
   else
!
!-    The file name need to be evaluated. The instructions of the script will be read and 
!     parsed during the execution.
!
      instr%breakpt = 1
   end if         

   END SUBROUTINE Calmat_ParseExec
   
   
!=============================================================================================
   SUBROUTINE Calmat_ParseFor ( instr )
!=============================================================================================
   type   (ins_t), intent(in out) :: instr
!---------------------------------------------------------------------------------------------    
!  Parses a FOR block: extract the counter name and the bound expressions.
!
!  Note: except for the ending keyword "endfor", the same Matlab/Scilab syntax of the "for" 
!        block is used, i.e.
!
!             for counter_name = expr1:expr2
!                 ...
!             endfor
!
!        or
!
!             for counter_name = expr1:expr3:expr2
!                 ...
!             endfor
!
!        where expr? are mathematical expressions giving the bounds (expr1, expr2) and the
!        step (expr3).
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables: --------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'Calmat_ParseFor',  &
                                    DIGITS = '+-0123456789'
   integer  (Ikind)              :: p, i, iostat, err
   character(len=:), allocatable :: str
   logical                       :: is_new, is_notNumber(3)
!---------------------------------------------------------------------------------------------    

   err = 0
   
   p = index(instr%rec,"for ")
   
   str = trim(adjustl(instr%rec(4:))) ; p = index(str,"=")
   
   if ( p == 0 ) then
!
!-    Error: no "=" found in the string  
!
      call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                           msg  = 'Missing "=" in << '//instr%rec//' >>' )
      return
   end if
   
   if ( p - index(str,'=',back=.true.) /= 0 ) then
!
!-    Error: more than one "=" found
!   
      call G_flagerr%set ( stat = UERROR, where = HERE, & 
                            msg = "Expression << "//instr%rec//" >> has an error" )
      return
   end if
!
!- The counter name:
!   
   instr%cName = trim(adjustl(str(:p-1)))
   
   str = str(p+1:) ; p = index(str,':')
   
   if ( p == 0 ) then
!
!-    Error: no ":" found in the string
!   
      call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                           msg  = 'Missing ":" in << '//instr%rec//' >>' )
      return
   end if
!
!- Determine the expression of the bounds:
!   
   instr%cExpr(1) = trim(adjustl(str(:p-1)))  ! exp. of the 1st bound
   
   is_notNumber = .true.
   
   if ( len_trim(instr%cExpr(1)%str) >  0) then
      ! If the exp. starts with one of the characters of DIGITS, try to read it as an integer
      ! If a read error occurs, this exp. will be considered for evaluation
      if ( index(DIGITS,instr%cExpr(1)%str(1:1)) /= 0 ) then
         read(instr%cExpr(1)%str,*,iostat=iostat) i
         if ( iostat == 0 ) then
            instr%cVal(1) = i 
            is_notNumber(1) = .false.
         end if  
      end if
   else
      err = 1
   end if
   
   if ( err /= 0 ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                           'Integer expected for the lower bound in << '//instr%rec//' >>' )
      return
   end if
      
   str = str(p+1:)   ! remaining exp.
   
   p = index(str,':')
   if ( p == 0 ) then
      ! no other ":" symbol => the step is 1 and str is the exp. of the 2nd bound
      instr%cExpr(2) = trim(adjustl(str(p+1:))) 
      instr%cExpr(3) = ''
      instr%cVal(3) = 1      
      is_notNumber(3) = .false.
   else
      ! there is another ":" symbol
      instr%cExpr(2) = trim(adjustl(str(p+1:))) ! exp. of 2nd bound
      instr%cExpr(3) = trim(adjustl(str(:p-1))) ! exp. of the step
      
      if ( len_trim(instr%cExpr(3)%str) > 0 ) then
         if ( index(DIGITS,instr%cExpr(3)%str(1:1)) /= 0 ) then
            read(instr%cExpr(3)%str,*,iostat=iostat) i
            if ( iostat == 0 ) then
               instr%cVal(3) = i
               is_notNumber(3) = .false.
            end if
         end if
      else
         err = 1
      end if
      
      if ( err /= 0 ) then
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg =  &
                              'Integer expected for the increment in << '//instr%rec//' >>' )
         return
      end if
         
   end if
   
   if ( len_trim(instr%cExpr(2)%str) > 0 ) then
      if ( index(DIGITS,instr%cExpr(2)%str(1:1)) /= 0 ) then
         read(instr%cExpr(2)%str,*,iostat=iostat) i
         if ( iostat == 0 ) then
            instr%cVal(2) = i
            is_notNumber(2) = .false.
         end if
      end if
   else
      err = 1
   end if
   
   if ( err /= 0 ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                           'Integer expected for the upper bound in << '//instr%rec//' >>' )
      return
   end if   
!
!- Add the counter name in the list of variables (with a temporary status if not already 
!  present): 
!  
   call CalmatUtil_AddVar ( varName = instr%cName, &
                            varStat = G_TEMPORARY, & 
                            varId   = instr%cIvar, &
                            is_new  = is_new     , &
                            flagerr = G_flagerr  )
   if_error_trace_and_RETURN ( G_flagerr, HERE )                                         
!
!- Call the token analyzer if the bounds are expressions and store the analysis in %cHdl:
!   
   do i = 1, 3
      if ( is_notNumber(i) ) then
         call pk2Interpreter_Tokenizor ( expr     = instr%cExpr(i)%str  , & 
                                         varNames = G_varNames(1:G_nvar), & 
                                         handle   = instr%cHdl(i)       , &
                                         flagerr  = G_flagerr            )
         if_error_trace_and_RETURN ( G_flagerr, HERE )                                         
      else
         instr%cHdl(i)%initialized = .false.   
      end if
   end do
   
   END SUBROUTINE Calmat_ParseFor


!=============================================================================================
   SUBROUTINE Calmat_ParseIf ( instr, flowId )
!=============================================================================================
   type   (ins_t), intent(in out) :: instr
   integer(Ikind), intent(in    ) :: flowId
!---------------------------------------------------------------------------------------------    
!  Parses each branch condition of an IF block.
!
!  Note: except for the ending keyword "endif" (or "end if"), the same Matlab/Scilab syntax of
!  the "if" block is used, i.e.
!
!            if expr1 then
!               ...
!            elseif expr2 then
!               ...
!            elseif exprN then
!               ...
!            ...
!            else
!               ...
!            endif
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables: --------------------------------------------------------------------------      
   character(len=*), parameter   :: HERE = 'Calmat_ParseIf'
   integer  (Ikind)              :: p, i, j, nbr
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------    
   
   p = index(instr%rec,"if ")

   str = trim(adjustl(instr%rec(p+3:)))
   
   p = index(str,'then') 
   if ( p == 0 .or. p - index(str,'then',back=.true.) /= 0 ) then
      call G_flagerr%set ( stat = UERROR, where = HERE, &
                            msg = 'Keyword "then" omited in an "if" statement' )
      return
   end if
!
!- number of branches:
!   
   nbr = instr%nBr
!
!- Extract the conditional expression of each branch (put it in %tExpr) and initialize
!  their values to .false.:
!   
   if ( allocated(instr%tExpr) ) then
      if ( size(instr%tExpr) < nbr ) deallocate(instr%tExpr, instr%tHdl, instr%tVal)
   end if
   
   if ( .not. allocated(instr%tExpr) ) &
      allocate(instr%tExpr(nbr), instr%tHdl(nbr), instr%tVal(nbr))

   instr%tVal(1:nbr) = .false.
   
   instr%tExpr(1)%str = (str(:p-1)) 
   
   do i = 2, nbr
      j = instr%startBr(i)-1
      p = index(G_flows(flowId)%stack(j)%rec,"elseif ")
      if ( p > 0 ) then
         str = trim(adjustl(G_flows(flowId)%stack(j)%rec(p+7:)))
         p = index(str,'then') 
         if ( p == 0 .or. p - index(str,'then',back=.true.) /= 0 ) then
            call G_flagerr%set ( stat = UERROR, where = HERE, msg = &
                                 'Keyword "then" omited in an "elseif" statement' )
            return
         end if
         instr%tExpr(i)%str = (str(:p-1)) 
      else
         p = index(G_flows(flowId)%stack(j)%rec,"else")
         if ( p > 0 ) instr%tExpr(i)%str = '' 
      end if
   end do
!
!- Tokenize each conditional expression (put it in %tHdl):
!   
   do i = 1, nbr
      if ( len_trim(instr%tExpr(i)%str) /= 0 ) then
         call pk2Interpreter_Tokenizor ( expr     = instr%tExpr(i)%str  , &
                                         varNames = G_varNames(1:G_nvar), & 
                                         handle   = instr%tHdl(i)       , &
                                         flagerr  = G_flagerr            )
         if_error_trace_and_RETURN ( G_flagerr, HERE )                                         
      end if
   end do
         
   END SUBROUTINE Calmat_ParseIf


!=============================================================================================
   SUBROUTINE Calmat_ParseClear ( expr )
!=============================================================================================
   character(len=*), intent(in) :: expr
!---------------------------------------------------------------------------------------------  
!---------------------------------------------------------------------------------------------  


!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'Calmat_ParseClear'   
   integer  (Ikind)              :: i, ntok, IdObj, IdVar, IdCmp
   character(len=:), allocatable :: msg
   type     (str_t), allocatable :: tokens(:,:)
!---------------------------------------------------------------------------------------------    
   
   ntok = util_CountTokens ( str    = expr  , delims = ' ' , BlkToken =.false., &
                             tokens = tokens, stat = G_flagerr   )
   if_error_trace_and_RETURN ( G_flagerr, HERE )                                         
   
   if ( ntok == 1 ) then
!
!-    Case where all variables will have to be cleared
!   
      do IdVar = 1, size(G_vars) 
!
!-       If this variable is protected, unused or defined after the present instruction: cycle
!
         if ( G_vars(IdVar)%status   == G_PROTECTED     .or. &
              G_vars(IdVar)%status   == G_USERPROTECTED .or. &
              G_vars(IdVar)%status   == G_FREE          .or. &
              G_vars(IdVar)%firstIns >= G_ins                ) cycle
!
!-       This variable has been defined before "clear": remove it from the current list of
!        variable names:

         G_varNames(IdVar)%str = '' !commented(05/24)
         G_vars(IdVar)%status = G_TEMPORARY !added(05/24)
         
      end do
      
      do IdObj = 1, size(G_objs)

!-       If this object is protected, unused or defined after the present instruction: cycle
!
         if ( G_objs(IdObj)%status   == G_PROTECTED     .or. &
              G_objs(IdObj)%status   == G_USERPROTECTED .or. &
              G_objs(IdObj)%status   == G_FREE          .or. &
              G_objs(IdObj)%firstIns >= G_ins                ) cycle
!
!-       This variable has been defined before "clear": remove it from the current list of
!        variable names:

         G_objNames(IdObj)%str = '' !commented(05/24)
         G_objs(IdObj)%status = G_TEMPORARY !added(05/24)

      end do
            
   else
!
!-    Case where only a set of variables will be cleared (their names are given in 
!     "tokens(2:ntok,1)"):
!      
      do i = 2, ntok
!
!-       See if this is the name of an object (present in the list G_objs):
!
         call CalmatUtil_FindObj ( tokens(i,1)%str, IdObj )
         
         if ( IdObj /= 0 ) then        
!
!-          If this object is protected, unused or defined after the present instruction: cycle
!
            if ( G_objs(IdObj)%status   == G_PROTECTED     .or. &
                 G_objs(IdObj)%status   == G_USERPROTECTED .or. &
                 G_objs(IdObj)%status   == G_FREE          .or. &
                 G_objs(IdObj)%firstIns >= G_ins                ) cycle
!         
!-          This object has been defined before "clear": remove it from the current list of
!           object names:

            G_objNames(IdObj)%str = '' !commented(05/24)
            G_objs(IdObj)%status = G_TEMPORARY !added(05/24)
!
!-          and remove its components from the current list of variable nams:
!            
            do IdCmp = 1, G_objs(IdObj)%nCmp
               Idvar = G_objs(IdObj)%varId(IdCmp)
               if ( IdVar /= 0 ) then
                  G_varNames(IdVar)%str = '' !commented(05/24)
                  G_vars(IdVar)%status = G_TEMPORARY !added(05/24)
               end if
            end do
            
            cycle
            
         end if   
!
!-       See if this is the name of a variable (present in the list G_vars):
!
         call CalmatUtil_FindVar ( varName = tokens(i,1)%str, varId = IdVar ) 
         
         if ( IdVar /= 0 ) then         
!
!-          If this variable is protected, unused or defined after the present instruction: cycle
!
            if ( G_vars(IdVar)%status   == G_PROTECTED     .or. &
                 G_vars(IdVar)%status   == G_USERPROTECTED .or. &
                 G_vars(IdVar)%status   == G_FREE          .or. &
                 G_vars(IdVar)%firstIns >= G_ins                ) cycle
!
!-          This variable has been defined before "clear": remove it from the current list of
!           variable names:
            
            G_varNames(IdVar)%str = '' !commented(05/24)
            G_vars(IdVar)%status = G_TEMPORARY !added(05/24)
            
         end if   
         
      end do   

   end if
   
   if ( allocated(msg) ) call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                                         'Protected variables cannot be deleted ('//msg//')' )
           
   END SUBROUTINE Calmat_ParseClear


!=============================================================================================
   SUBROUTINE Calmat_delFlows ( flowId )
!=============================================================================================
   integer(Ikind), optional, intent(in out) :: flowId
!---------------------------------------------------------------------------------------------                   
!  Destroys all flows except the flow #flowId (if presents).
!  Destroys the stack of the flow #flowId.
!  Sets task = G_STOP if the file of this flow is closed.
!  Resets the global number of instructions (G_ins)
!---------------------------------------------------------------------------------------------                   

!- local variables ---------------------------------------------------------------------------  
   integer(Ikind) :: i, flowId_
   type   (err_t) :: flag
!---------------------------------------------------------------------------------------------   

   if ( G_task == G_STOP ) return
   
   if ( present(flowId) ) then
      flowId_ = flowId
   else
      flowId_ = 0
   end if
      
   if ( flowId_ > 0 ) then
      !!if ( G_flows(flowId)%unit == -1 ) G_task = G_STOP !!!04/24
      if ( G_flows(flowId)%unit <= 0 ) G_task = G_STOP !!!04/24      
   end if
    
   do i = 1, G_maxnFlow
   
      if ( G_flows(i)%used .and. i /= flowId_ ) then
         
         call G_flows(i)%destroyFlow ( flag )
         
         if ( flag > IZERO ) then
            if ( G_flagerr > IZERO ) G_flagerr%mesg = G_flagerr%mesg // NL // flag%mesg
            call G_flagerr%AddTrace('Calmat_delFlows')
            call CalmatUtil_Error (abort = .true., empty = .false.)
            return
         end if
         
      end if         
   end do
   
   if ( flowId_ > 0 ) call G_flows(flowId)%destroyStack ( flag )
   
   if ( flag > IZERO ) then
      if ( G_flagerr > IZERO ) G_flagerr%mesg = G_flagerr%mesg // NL // flag%mesg
      call G_flagerr%AddTrace('Calmat_delFlows')
      call CalmatUtil_Error (abort = .true., empty = .false.)
      return
   end if
   
   if ( G_task /= G_STOP ) G_task = G_CONTINUE
   
   G_ins = 0 ! reset the global number of instructions
   
   do i = 1, G_nVar
      G_vars(i)%firstIns = IZERO
      G_vars(i)%lastIns  = IZERO
   end do
   
   do i = 1, G_nobj
      G_objs(i)%firstIns = IZERO
      G_objs(i)%lastIns  = IZERO
   end do

   !!06/25:
   if ( G_task == G_STOP .and. G_dispPrompt ) then
      G_task = G_CONTINUE
      G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = '' )
      if ( G_flagerr > IZERO ) call G_flagerr%AddTrace("Calmat_delFlows")
   end if
   
   END SUBROUTINE Calmat_delFlows


!=============================================================================================
   SUBROUTINE Calmat_CmdLineArgs
!=============================================================================================
   use f90getopt
!--------------------------------------------------------------------------------------------- 
!  Command line arguments are:
!  -h              : display help message
!  -i < filename > : execute the script "filename"
!  -o < filename > : write all results in "filename" instead of stdout
!  -b              : batch mode (-i must be used) 
!
!  Note: it calls getopt from the f90getopt module developped by Hani Andreas Ibrahim 
!        (https://github.com/haniibrahim/f90getopt)
!---------------------------------------------------------------------------------- R.H. 11/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'CmdLineArgs'
   integer  (Ikind)              :: err
   character(LGSTR)              :: iomsg   
   type     (str_t)              :: h(23), f
   character(len=:), allocatable :: input, output
   character                     :: opt
   logical                       :: batch, casei, caseo   
!--------------------------------------------------------------------------------------------- 

   if ( G_task /= G_CONTINUE ) return

   input = '' ; output = '' ; batch = .false.
   
   casei = .false. ; caseo = .false. 
   
   do
      opt = getopt( "hbi:o:", stat = iomsg, who = HERE)
      if ( trim(iomsg) /= 'ok' ) G_flagerr = err_t (stat=IERROR, where=HERE, msg=trim(iomsg))
      call CalmatUtil_Error ( abort = .true., empty = .false. )
      if ( G_task == G_STOP ) return
         
      select case ( trim(adjustl(opt)) )

         case (char(0))
            exit

         case ( 'i' )
            if ( casei ) then
               call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                     msg = "Option '-i' used more than once")
            else
               casei = .true. 
               input = optarg
!!               G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = input )
!!               if ( G_flagerr > IZERO ) call G_flagerr%AddTrace(HERE)
            end if
            
            call CalmatUtil_Error ( abort = .true., empty = .false. )
            if ( G_task == G_STOP ) return
            
         case ( 'o' )
            if ( caseo ) then
               call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                     msg = "Option '-o' used more than once")
            else
               caseo = .true. 
               output = optarg
               G_uo = util_GetUnit ()
               if ( G_uo /= 0 ) then
                  open (unit   = G_uo   , file   = optarg, status = 'replace',  &
                        action = 'write', iostat = err   , iomsg  = iomsg    )

                  if ( err /= 0 ) &
                     call G_flagerr%set ( where = HERE, msg =                              &
                                               "Failed to open '"//trim(optarg)//"'"//NL// &
                                               "    IOMSG: "//trim(iomsg),                 &
                                         stat = G_UERROR                                   )
                     
               else
                  call G_flagerr%set (stat=G_IERROR, where=HERE, msg="No free unit available")  
               end if
            end if
            
            call CalmatUtil_Error ( abort = .true., empty = .false. )
            if ( G_task == G_STOP ) return            
                                             
         case ( 'b' )
            G_dispPrompt = .false.
            batch = .true.
            
         case ('h')
            f = str_color('calmat',G_ColorHelp)
            h( 1)%str = 'Usage: '
            h( 2)%str = '          '+f+' [-i file1] [-o file2] [-b] [-h]'            
            h( 3)%str = ' '
            h( 4)%str = 'or better (on linux and mac osx) with the readline wrapper:'
            h( 5)%str = '          rlwrap calmat [-i file1] [-o file2] [-b] [-h]' 
            h( 6)%str = ' '
            h( 7)%str = 'where the options are'
            h( 8)%str = ' '
            h( 9)%str = '  -i file1: '
            h(10)%str = '     read and execute the commands of the script "file1"' 
            h(11)%str = '     and then prompt the user for new commands (from the'
            h(12)%str = '     standard input)'
            h(13)%str = ' '
            h(14)%str = '  -o file2: '
            h(15)%str = '     all outputs are redirected into the file "file2"'  
            h(16)%str = '     (otherwise the standard output is used)'
            h(17)%str = ' '
            h(18)%str = '  -b: '
            h(19)%str = '     batch mode (must be used in conjonction with -i).' 
            h(20)%str = '     If -o is not used, the outputs are written to the'
            h(21)%str = '     file "file1".out'
            h(22)%str = ' '
!
!-          print help and exit with G_task = G_STOP (end)
!            
            call str_print ( unit = int(G_uo), s = h, underline='-',                        &
                             title = ' === help for calmat version '//G_VERSIONID//' === ', &
                             colorTitle = G_colorTitle, nblkBefore = 1                      )
                             
            G_task = G_STOP
            return            
            
      end select
   end do   

   if ( batch ) then
!
!-    Batch mode. Check that an input file name was given and open the output file:
!   
      G_dispPrompt = .false.

      if (len_trim(input) == 0) then
         iomsg = 'Batch mode (option -b) requires an input script file (option -i file1)'
         G_flagerr = err_t ( stat = G_UERROR, where = HERE, msg = trim(iomsg) )
      else if (len_trim(output) == 0) then
         output = trim(input) // '.out'
         G_uo = util_GetUnit ()
         if (G_uo /= 0) then
            open (unit   = G_uo   , file   = output, status = 'replace',  &
                  action = 'write', iostat = err   , iomsg  = iomsg    )
            if ( err /= 0 ) then
               G_uo = STDERR
               call G_flagerr%set ( where = HERE,                                      &
                                     msg = "Failed to open '"//trim(output)//"'"//NL// &
                                          "    IOMSG: "//trim(iomsg),                  &
                                    stat = G_UERROR )
            end if
         else
            G_uo = STDERR
            call G_flagerr%set ( stat=G_IERROR, where=HERE, msg="No free unit available" )  
         end if
      end if   

      call CalmatUtil_Error ( abort = .true., empty = .false. )
      if ( G_task == G_STOP ) return
      
   end if  

   if (len_trim(input) == 0) then
!
!-    No input file supplied, set G_dispPrompt to .true. and the current file to the stdin:
!
      G_dispPrompt = .true.

      G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = '' ) 

      if ( G_flagerr > IZERO ) call G_flagerr%AddTrace(HERE)
      
      call CalmatUtil_Error ( abort = .true., empty = .false. )
      if ( G_task == G_STOP ) return           
!
!-    If no output file is given, set it to stdout:
!
      if ( len_trim(output) == 0 ) G_uo = STDOUT

   else if (batch) then
!
!-     An input file is given and batch mode is choosen:
!
      G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = input )
      if ( G_flagerr > IZERO ) call G_flagerr%AddTrace(HERE)

      call CalmatUtil_Error ( abort = .true., empty = .false. )
      if ( G_task == G_STOP ) return           
               
   else 
!
!-    An input file is given and interactive mode is choosen. The flow #1 will correspond to
!     stdin, the flow #2 to the given file and the current flow is set to 2:
!     
      G_dispPrompt = .true.



      G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = input )
      if ( G_flagerr > IZERO ) call G_flagerr%AddTrace(HERE)

      call CalmatUtil_Error ( abort = .true., empty = .false. )
      if ( G_task == G_STOP ) return           

   end if   
   
   if (len_trim(output) == 0) then
!
!-    No output file, set it to the stdout:
!
      G_uo = STDOUT
   end if      
!
!- Call again SignalHandler_SignalCatch if G_uo /= STDOUT:
!
   if (G_uo /= STDOUT) &
      call SignalHandler_SignalCatch (unit = G_uo, title = '--> Calmat Info:')
      
   END SUBROUTINE Calmat_CmdLineArgs
   
         
!=============================================================================================
   SUBROUTINE Calmat_CmdLineArgs_v1
!=============================================================================================
   use f90getopt
!--------------------------------------------------------------------------------------------- 
!  Command line arguments are:
!  -h              : display help message
!  -i < filename > : execute the script "filename"
!  -o < filename > : write all results in "filename" instead of stdout
!  -b              : batch mode (-i must be used) 
!
!  Note: it calls getopt from the f90getopt module developped by Hani Andreas Ibrahim 
!        (https://github.com/haniibrahim/f90getopt)
!---------------------------------------------------------------------------------- R.H. 11/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'CmdLineArgs'
   integer  (Ikind)              :: err
   character(LGSTR)              :: iomsg   
   type     (str_t)              :: h(17), f 
   character(len=:), allocatable :: input, output
   character                     :: opt
   logical                       :: batch, casei, caseo   
!--------------------------------------------------------------------------------------------- 

   if ( G_task /= G_CONTINUE ) return

   input = '' ; output = '' ; batch = .false. 
   
   casei = .false. ; caseo = .false. 
   
   do
      opt = getopt( "hbi:o:", stat = iomsg, who = HERE)
      if ( trim(iomsg) /= 'ok' ) G_flagerr = err_t (stat=IERROR, where=HERE, msg=trim(iomsg))
      call CalmatUtil_Error ( abort = .true., empty = .false. )
      if ( G_task == G_STOP ) return
      
      select case ( trim(adjustl(opt)) )

         case ( char(0) )
            exit

         case ( 'i' )
            if ( casei ) then
               call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                     msg = "Option '-i' used more than once")
            else
               casei = .true. 
               input = optarg
               G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = input )
               if ( G_flagerr > IZERO ) call G_flagerr%AddTrace(HERE)
               !!!batch = .true.
            end if
            
            call CalmatUtil_Error ( abort = .true., empty = .false. )
            if ( G_task == G_STOP ) return
                        
         case ( 'o' )
            if ( caseo ) then
               call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                     msg = "Option '-o' used more than once")
            else
               caseo = .true. 
               output = optarg
               G_uo = util_GetUnit ()
               if ( G_uo /= 0 ) then
                  open (unit   = G_uo   , file   = optarg, status = 'replace',  &
                        action = 'write', iostat = err   , iomsg  = iomsg    )

                  if ( err /= 0 ) &
                     call G_flagerr%set ( where = HERE, msg =                              &
                                               "Failed to open '"//trim(optarg)//"'"//NL// &
                                               "    IOMSG: "//trim(iomsg),                 &
                                         stat = G_UERROR                                   )
                     
               else
                  call G_flagerr%set (stat=G_IERROR, where=HERE, msg="No free unit available")  
               end if
            end if
            
            call CalmatUtil_Error ( abort = .true., empty = .false. )
            if ( G_task == G_STOP ) return

         case ( 'b' )
            !!!
            batch = .true.
            
         case ( 'h' )
            f = str_color('calmat',G_ColorHelp)
            h( 1) = 'Usage: '
            h( 2) = '          '+f+' [-i file1] [-o file2] [-h]'
            h( 3) = ' '
            h( 4) = 'or better (on linux and mac osx) with the readline wrapper:'
            h( 5) = '          rlwrap '+f+' [-i file1] [-o file2] [-h]' 
            h( 6) = ' '
            h( 7) = 'where the options are'
            h( 8) = ' '
            h( 9) = '  -i file1: '
            h(10) = '    Read and execute the commands of the script "file1" (batch mode).' 
            h(11) = '    If not present the standart input is used.'
            h(12) = ' '
            h(13) = '  -o file2: '
            h(14) = '    When -i is used, outputs are written into the file "file2"'  
            h(15) = '    otherwise the file "file1.out" is used.'
            h(16) = '    When -i is not used, the standart output is used.'
            h(17) = ''
!
!-          print help and exit with G_task = G_STOP (end)
!            
            call str_print ( unit = int(G_uo), s = h, underline='-',                        &
                             title = ' === help for calmat version '//G_VERSIONID//' === ', &
                             colorTitle = G_colorTitle, nblkBefore = 1                      )
                             
            G_task = G_STOP
            return
            
      end select
   end do   
   
   if ( batch ) then
!
!-    Batch mode. Set G_dispPrompt to .false. and open the output file:
!   
      G_dispPrompt = .false.
      
      if ( len_trim(output) == 0 ) then
         output = trim(input) // '.out'
         G_uo = util_GetUnit ()
         if ( G_uo /= 0 ) then
            open (unit   = G_uo   , file   = output, status = 'replace',  &
                  action = 'write', iostat = err   , iomsg  = iomsg    )
            if ( err /= 0 ) then
               G_uo = STDERR
               call G_flagerr%set ( where = HERE,                                      &
                                     msg = "Failed to open '"//trim(output)//"'"//NL// &
                                          "    IOMSG: "//trim(iomsg),                  &
                                    stat = G_UERROR )
            end if
         else
            G_uo = STDERR
            call G_flagerr%set ( stat=G_IERROR, where=HERE, msg="No free unit available" )  
         end if
      end if   

      call CalmatUtil_Error ( abort = .true., empty = .false. )
      if ( G_task == G_STOP ) return
      
   else
!
!-    Set G_dispPrompt to .true.:
!
      G_dispPrompt = .true.
!
!-    Set the current file to stdin:
!
      G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = '' ) 
      if ( G_flagerr > IZERO ) call G_flagerr%AddTrace(HERE)
      
      call CalmatUtil_Error ( abort = .true., empty = .false. )
      if ( G_task == G_STOP ) return           
!
!-    If no output file is given, set it to stdout:
!
      if ( len_trim(output) == 0 ) G_uo = STDOUT
   end if      
!
!- Call again SignalHandler_SignalCatch if G_uo /= STDOUT:
!
   if ( G_uo /= STDOUT ) call SignalHandler_SignalCatch &
                         ( unit = G_uo, title = '--> Calmat Info:', farewellMsg = G_farewell )

   END SUBROUTINE Calmat_CmdLineArgs_v1
     
      
END MODULE Calmat_m
