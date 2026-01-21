!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Module CalmatCmd
!
! This module contains a set of calmat commands (see CalmatCmd_CmdList for the list)
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Date: 11/18
! Modified: 03/19
!---------------------------------------------------------------------------------------------

#include "error.fpp"

MODULE CalmatCmd_m

   use signalHandler_m
   use CalmatGlobal_m
   use CalmatUtil_m
   
   implicit none
    
   integer(Ikind), parameter :: NCMD = 32, &      ! number of available commands
                                MAXCMP = 10       ! max. number of components (for an object)
   
   type   (str_t)            :: CmdList(NCMD), &      ! Command names
                                CmdDesc(NCMD), &      ! Their short description
                                CmdCmps(MAXCMP,NCMD)  ! Component names (for an object result)
   integer(Ikind)            :: CmdType(NCMD)         ! The type of the result (var or object)

   logical :: CalmatCmd_forloopbreak = .false.
            
CONTAINS


!=============================================================================================
   SUBROUTINE CalmatCmd_CmdList ()
!=============================================================================================
!--------------------------------------------------------------------------------------------- 
!  Initialize the list of available command (CmdList) and their short descriptions (CmdDesc).
!
!  The meaning of the integer (k) stored in "CmdType" is as follows: 
!  . k < 0: the command does not produce an assignable output
!  . k = 0: the output can be assigned to a regular variable (or a set of variables)
!  . k > 0: the output can be assigned to an object and k gives its number of components
!
!  Note: some of these commands are just referenced for the help
!---------------------------------------------------------------------R.H. 11/18, 01/19, 03/20

   CmdList( 1) = 'help'  ; CmdDesc( 1) = 'returns help'
   CmdType( 1) =-1
   
   CmdList( 2) = 'clear' ; CmdDesc( 2) = 'deletes variables'
   CmdType( 2) =-1
   
   CmdList( 3) = 'list'  ; CmdDesc( 3) = 'lists variables, functions/commands, operators'
   CmdType( 3) = 0
   
   CmdList( 4) = 'disp'  ; CmdDesc( 4) = 'displays a variable'
   CmdType( 4) =-1
   
   CmdList( 5) = 'exec'  ; CmdDesc( 5) = 'executes a script'
   CmdType( 5) =-1
   
   CmdList( 6) = 'pwd'   ; CmdDesc( 6) = 'returns the current directory name (linux command)'
   CmdType( 6) = 0
   
   CmdList( 7) = 'cd'    ; CmdDesc( 7) = 'changes the current directory (linux command) '
   CmdType( 7) =-1

   CmdList( 8) = 'ls'    ; CmdDesc( 8) = 'lists the directory contents (linux command) '
   CmdType( 8) = 0
   
   CmdList( 9) = 'plot'  ; CmdDesc( 9) = 'plots a set of points in 2D (call to gnuplot)'
   CmdType( 9) =-1
   
   CmdList(10) = 'plot3d'; CmdDesc(10) = 'plots a set of points in 3D (call to gnuplot)'
   CmdType(10) =-1
      
   CmdList(11) = 'surf'  ; CmdDesc(11) = 'plots a surface (call to gnuplot)'
   CmdType(11) =-1
   
   CmdList(12) = 'format'; CmdDesc(12) = 'sets the format for displaying decimal numbers'
   CmdType(12) =-1
   
   CmdList(13) = 'quit'  ; CmdDesc(13) = 'quits the program (same as exit)'
   CmdType(13) =-1
   
   CmdList(14) = 'break' ; CmdDesc(14) = 'terminates the execution of a for loop'
   CmdType(14) =-1
   
   CmdList(15) = 'for'   ; CmdDesc(15) = 'keyword for a for loop'
   CmdType(15) =-1
   
   CmdList(16) = 'endfor'; CmdDesc(16) = '(or end for) keyword for the end of a for loop'
   CmdType(16) =-1
   
   CmdList(17) = 'if'    ; CmdDesc(17) = 'keyword for an if statement (conditional branch)'
   CmdType(17) =-1
   
   CmdList(18) = 'endif' ; CmdDesc(18) = '(or end if) keyword for the end of an if statement'
   CmdType(18) =-1
   
   CmdList(19) = 'else'  ; CmdDesc(19) = 'keyword for the else of an if construct'
   CmdType(19) =-1
   
   CmdList(20) = 'elseif'; CmdDesc(20) = 'keyword for the elseif of an if construct'
   CmdType(20) =-1
   
   CmdList(21) = 'mesh'  ; CmdDesc(21) = 'reads a mesh (in progress...)'
   CmdType(21) = 2
   CmdCmps(1,21) = 'lnods' ; CmdCmps(2,21) = 'coord'

   CmdList(22) = 'more'  ; CmdDesc(22) = 'views a given file (linux command) '
   CmdType(22) =-1

   CmdList(23) = 'tail'  ; CmdDesc(23) = 'displays the last part of a file (linux command) '
   CmdType(23) =-1

   CmdList(24) = 'linux' ; CmdDesc(24) = 'allows access to all linux commands '
   CmdType(24) =-1

   CmdList(25) = 'cp'    ; CmdDesc(25) = 'copies files (linux command) '
   CmdType(25) =-1

   CmdList(26) = 'mv'    ; CmdDesc(26) = 'moves files (linux command) '
   CmdType(26) =-1

   CmdList(27) = 'rm'    ; CmdDesc(27) = 'removes files (linux command) '
   CmdType(27) =-1

   CmdList(28) = 'exit'  ; CmdDesc(28) = 'quits the program (same as quit)'
   CmdType(28) =-1

   CmdList(29) = 'set'  ; CmdDesc(29) = 'sets or changes a default variable'
   CmdType(29) =-1

   CmdList(30) = 'welcome'; CmdDesc(30) = 'prints the welcome message'
   CmdType(30) =-1

   CmdList(31) = 'eval' ; CmdDesc(31) = 'evaluates a string containing a single expression'
   CmdType(31) = 0

   CmdList(32) = 'exists' ; CmdDesc(32) = 'checks if a variable exists'
   CmdType(32) = 0
               
   END SUBROUTINE CalmatCmd_CmdList
   

!=============================================================================================
   SUBROUTINE CalmatCmd_FindCmd ( str, IdCmd )
!=============================================================================================
   character(len=*),              intent(in    ) :: str
   integer  (Ikind),              intent(   out) :: IdCmd
!--------------------------------------------------------------------------------------------- 
!  See if string "str" starts with the name of a command (in the list "CmdList"). 
!  If so, returns its #. Otherwise returns IdCmd = 0.
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------
   integer(Ikind) :: i, n, pBlk, pPar
!--------------------------------------------------------------------------------------------- 

   IdCmd = 0
 
   pBlk = index(str,' ') ; pPar = index(str,'(')
   
   if ( pBlk /= 0 .and. pPar /= 0 ) then
      n = min(pBlk,pPar) - 1
   else if ( pBlk /= 0 ) then
      n = pBlk - 1
   else if ( pPar /= 0 ) then
      n = pPar - 1
   else
      n = -1
   end if
   
   if ( n == -1 ) n = len_trim(str)

   do i = 1, size(CmdList)
      if ( str(1:n) == CmdList(i)%str ) then
         IdCmd = i
         exit
      end if
   end do   
   
   END SUBROUTINE CalmatCmd_FindCmd
   

!=============================================================================================
   SUBROUTINE CalmatCmd_Tokenizor ( instr )
!=============================================================================================
   type(ins_t), intent(in out) :: instr
!---------------------------------------------------------------------------------------------
!  Analyses the rhs "instr%rhs"
!
!  Note: the rhs may be 
!   (1) a mathematical expression (like 'sin(a*x) + b')
!   (2) a calmat command (like 'plot(x,3*x+b)' or 'clear a b c')
!
!   For (1) pk2Interpreter_Tokenizor is called to tokenize the expression given in "instr%rhs". 
!           The result is stored into a handle: instr%rhsHdl.
!
!   For (2) ... 
!           Save also the name of the components if the result of the command is an object.
!-----------------------------------------------------------------------------------R.H. 04/20

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatCmd_Tokenizor'
!--------------------------------------------------------------------------------------------- 

   call CalmatCmd_FindCmd ( instr%rhs, instr%IdCmd )

   if ( instr%IdCmd == 0 ) then
      call pk2Interpreter_Tokenizor ( expr     = instr%rhs           , & ! in
                                      varNames = G_varNames(1:G_nvar), & ! in
                                      handle    = instr%rhsHdl       , & ! out
                                      flagerr =  G_flagerr           )   ! out
      instr%rhsNcmp = 0
      instr%assignment = .true.                                      
   else
      call CalmatCmd_AnalyzeCmd ( instr )
   end if   
   
   if ( G_flagerr > IZERO ) call G_flagerr%AddTrace(HERE)

   END SUBROUTINE CalmatCmd_Tokenizor 
   

!=============================================================================================
   SUBROUTINE CalmatCmd_AnalyzeCmd ( instr )
!=============================================================================================
   type(ins_t), intent(in out) :: instr
!--------------------------------------------------------------------------------------------- 
!-----------------------------------------------------------------------------------R.H. 01/20

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatCmd_AnalyzeCmd'
   integer  (Ikind)            :: i, ncmp, err
!--------------------------------------------------------------------------------------------- 
   
   ncmp = CmdType(instr%IdCmd)
   
   if ( ncmp > 0 ) then
      err = 0
      if ( .not. allocated(instr%rhsCmps) ) then
         allocate(instr%rhsCmps(ncmp), stat = err)
      else if ( ncmp > size(instr%rhsCmps) ) then
         deallocate(instr%rhsCmps, stat = err)
         if ( err == 0 ) allocate(instr%rhsCmps(ncmp), stat = err)
      end if
      
      if ( err /= 0 ) then
          call G_flagerr%set ( stat = G_IERROR, where = HERE, msg = 'Allocation failure')
          return
      end if

      do i = 1, ncmp
         instr%rhsCmps(i)%str = ( CmdCmps(i,instr%IdCmd)%str )
      end do
      instr%rhsNcmp = ncmp
   else
      instr%rhsNcmp = 0
   end if
      
   instr%assignment = ( CmdType(instr%IdCmd) >= 0 )

   END SUBROUTINE CalmatCmd_AnalyzeCmd
   
   
!=============================================================================================
   SUBROUTINE CalmatCmd_Evaluator ( instr )
!=============================================================================================
   type(ins_t), intent(in out) :: instr
!--------------------------------------------------------------------------------------------- 
!-----------------------------------------------------------------------------------R.H. 01/20

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatCmd_Evaluator'
   type     (pk2_t), allocatable :: outval(:)
   integer  (Ikind)              :: nlhs
!--------------------------------------------------------------------------------------------- 

   call ieee_set_flag (ieee_all, flag_value = G_QUIET)

   nlhs = max(1_Ikind, instr%nlhs)

   if ( instr%idCmd == 0 ) then
!
!-    The instruction is not a calmat command. Call the pk2interpreter to evaluate it:
!      
      call pk2Interpreter_Evaluator ( handle  = instr%rhsHdl    , &
                                      vars    = G_vars(1:G_nvar), &
                                      valExpr = outval          , &
                                      flagerr = G_flagerr       , &
                                      nanswer = nlhs            )                                      
      if_error_trace_and_RETURN ( G_flagerr, HERE )
!
!-    if the expression was just like "a", do not assign the result to "ans"
!     (in this case pk2Interpreter returns 'a' in outval(1)%name): 
!             
      if ( allocated(outval(1)%name) ) then
         if ( len(instr%lhs) == 0 ) instr%assignment = .false.
      end if 

   else
!
!-    The instruction is a calmat command:
!
      call CalmatCmd_Driver ( instr, nlhs, outval )
   end if
!
!- Check for error:
!   
   if_error_trace_and_RETURN ( G_flagerr, HERE )
   
   if ( G_task == G_STOP ) return
!
!- Assign the result and print it if requested:
!   
   call CalmatUtil_AssignAndPrint ( instr, nlhs, outval, G_flagerr )
   if_error_trace_and_RETURN ( G_flagerr, HERE )
       
   END SUBROUTINE CalmatCmd_Evaluator

      
!=============================================================================================
   SUBROUTINE CalmatCmd_Driver ( instr, nlhs, outval )
!=============================================================================================
   type     (ins_t),              intent(in out) :: instr
   integer  (Ikind),              intent(in    ) :: nlhs
   type     (pk2_t), allocatable, intent(   out) :: outval(:)
!--------------------------------------------------------------------------------------------- 
!  Calls the suited command for the instructions # ins of the stack
!---------------------------------------------------------------------R.H. 11/18, 01/19, 01/20

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatCmd_Driver'
!--------------------------------------------------------------------------------------------- 

   allocate(outval(nlhs))
     
   select case (instr%IdCmd)
   
   case ( 1 ) ! help
 
      if ( instr%verb > 0 ) call CalmatCmd_Help ( expr = instr%rhs )
      instr%verb = 0 ! nothing left to display (already displayed by CalmatCmd_Help)
      
   case ( 2 ) ! clear
      call CalmatCmd_Clear (expr = instr%rhs, clearout = outval(1), gins = instr%globalInsNum)
      instr%verb = 0 ! nothing to display
      
   case ( 3 ) ! list
      call CalmatCmd_List ( expr = instr%rhs, listout = outval(1) ) 
      instr%verb = 0 ! nothing left to display (already displayed by CalmatCmd_List)
         
      !!if (instr%verb /= 0) instr%verb = 2 ! force the whole result to be printed

   case ( 4 ) ! disp
      call CalmatCmd_Disp ( expr = instr%rhs, dispout = outval(1) )
      instr%verb = 0 ! nothing to display ("expr" is already displayed by CalmatCmd_Disp)

   case ( 5 ) ! exec
      call CalmatCmd_Exec ( expr = instr%rhs, execout = outval(1) )
      instr%verb = 0 ! nothing to display
      
   case ( 6 ) ! pwd
      call CalmatCmd_pwd ( expr = instr%rhs, pwdout = outval(1) )

   case ( 7 ) ! cd
      call CalmatCmd_chdir ( expr =  instr%rhs, cdout = outval(1) )
      instr%verb = 0 ! nothing to display

   case ( 8 ) ! ls
      call CalmatCmd_ls ( expr = instr%rhs, lsout = outval(1) )
      if (instr%verb /= 0) instr%verb = 2 ! force the whole result to be printed

   case ( 9 ) ! plot
      call CalmatCmd_Plot ( expr = instr%rhs, flowId = instr%flowId, plotout = outval(1) )       
      instr%verb = 0 ! nothing to display

   case(  10, 11 ) ! plot3d or surf
      call CalmatCmd_Plot3d ( expr = instr%rhs, flowId = instr%flowId, plotout = outval(1) )       
      instr%verb = 0 ! nothing to display
      
   case ( 12 ) ! format
      call CalmatCmd_Format ( expr = instr%rhs, formatout = outval(1) )
      instr%verb = 0 ! nothing to display

   case ( 13, 28 ) ! quit (or exit)
      G_task = G_STOP

   case ( 14 ) ! break
      CalmatCmd_forloopbreak = .true.
      instr%verb = 0 ! nothing to display

   case ( 21 ) ! mesh
      deallocate(outval)
      call CalmatCmd_Mesh ( expr = instr%rhs, meshout = outval )

   case ( 22 ) ! more
      call CalmatCmd_more ( expr =  instr%rhs, moreout = outval(1) )
      instr%verb = 0 

   case ( 23 ) ! tail
      call CalmatCmd_tail ( expr =  instr%rhs, tailout = outval(1) )
      instr%verb = 0 

   case ( 24 ) ! linux
      call CalmatCmd_linux ( expr =  instr%rhs, linuxout = outval(1) )
      instr%verb = 0 

   case ( 25 ) ! cp
      call CalmatCmd_cp ( expr =  instr%rhs, cpout = outval(1) )
      instr%verb = 0 

   case ( 26 ) ! mv
      call CalmatCmd_mv ( expr =  instr%rhs, mvout = outval(1) )
      instr%verb = 0 

   case ( 27 ) ! rm
      call CalmatCmd_rm ( expr =  instr%rhs, rmout = outval(1) )
      instr%verb = 0 

   case ( 29 ) ! set
      call CalmatCmd_set ( expr =  instr%rhs, setout = outval(1) )
      instr%verb = 0           

   case ( 30 ) ! welcome message
      call CalmatCmd_Welcome ( )
      instr%verb = 0        

   case ( 31 ) ! eval
      call CalmatCmd_Eval ( expr = instr%rhs, evalout = outval(1) )

   case ( 32 ) ! exists
      call CalmatCmd_Exists ( expr = instr%rhs, res = outval(1) )
                        
   case default
      call G_flagerr%set (stat = UERROR, where = HERE, msg = 'Unknown command')
      return

   end select
   
   if_error_trace_and_RETURN ( G_flagerr, HERE )
                                      
   END SUBROUTINE CalmatCmd_Driver
         
   
!=============================================================================================
   SUBROUTINE CalmatCmd_Help ( expr ) 
!=============================================================================================
   character(len=*), optional, intent(in) :: expr
!--------------------------------------------------------------------------------------------- 
!  Command "help"
!
!  Displays help
!
!  Input:
!  . expr: (optional) a string like help('name') or help("name") where name is the name of
!          a command or a function.
!          If expr is not present, returns the help of the "help" command itself
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatCmd_Help'
   character(len=:), allocatable :: str, title
   type     (str_t)              :: h(6), f
   integer  (Ikind)              :: lstr, lp, rp, err 
   type     (pk2_t), allocatable :: tmp(:)
   type     (pk2_t)              :: helpout
!--------------------------------------------------------------------------------------------- 
   
   G_flagerr = err_t()
         
   f = str_color('help',G_ColorHelp)
   h(1) = 'Usage: '+f+'("name") or '+f+' name'
   h(2) = 'where << name >> is the name of a command or a function' 
   h(3) = ''
   h(4) = 'Examples:'
   h(5) = '. '+f+' pwd'
   h(6) = '. '+f+'("pwd")' 
             
   if ( present(expr) ) then

      lp = index(Expr,'(') ; rp = index(Expr,')',back=.true.)
      
      if ( lp == 0 .and. rp == 0 ) then
         if ( len_trim(adjustl(expr)) > 4 ) then
            str = trim(adjustl(expr(5:)))
         else
            str = ''
         end if
      else if ( lp < rp ) then
         str = Expr(lp+1:rp-1) ! remove parentheses
         lstr = len_trim(str)
         if (lstr /= 0) then
            err = 0
            if ( str(1   :1   ) /= '"' .and. str(1   :1   ) /= "'" ) err = 1
            if ( str(lstr:lstr) /= '"' .and. str(lstr:lstr) /= "'" ) err = 1
            if ( err == 1 ) then
               call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                                   "error in expression << "// expr //" >>. Type 'help'" )
               return
            end if
            str = str(2:lstr-1) ! remove quotation marks
         end if 
      else
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' ) 
         return  
      end if
       
      select case ( str )
         case ( 'help' )
            helpout = h
            title = ' === help for the command "help" === '
         case ( 'clear' )   
            call CalmatCmd_Clear ( clearout = helpout )
            title = ' === help for the command "clear" === '
         case ( 'list' )
            call CalmatCmd_List  ( listout = helpout )
            title = ' === help for the command "list" === :'
         case ( 'exec' )
            call CalmatCmd_Exec  ( execout = helpout ) 
            title = ' === help for the command "exec" === '
         case ( 'pwd' )
            call CalmatCmd_pwd   ( pwdout = helpout ) 
            title = ' === help for the command "pwd" === '
         case ( 'cd' )
            call CalmatCmd_chdir ( cdout = helpout ) 
            title = 'help for the command "cd":'
         case ( 'ls' )
            call CalmatCmd_ls    ( lsout = helpout ) 
            title = ' === help for the command "ls" === '
         case ( 'more' )
            call CalmatCmd_more ( moreout = helpout ) 
            title = ' === help for the command "more" === '
         case ( 'tail' )
            call CalmatCmd_tail ( tailout = helpout ) 
            title = ' === help for the command "tail" === '
         case ( 'linux' )
            call CalmatCmd_linux ( linuxout = helpout ) 
            title = ' === help for the command "linux" === '
         case ( 'cp' )
            call CalmatCmd_cp ( cpout = helpout ) 
            title = ' === help for the command "cp" === '
         case ( 'mv' )
            call CalmatCmd_mv ( mvout = helpout ) 
            title = ' === help for the command "mv" === '
         case ( 'rm' )
            call CalmatCmd_rm ( rmout = helpout ) 
            title = ' === help for the command "rm" === '

         case ( 'plot' )
            call CalmatCmd_Plot  ( plotout = helpout ) 
            title = ' === help for the command "plot" === '
         case ( 'plot3d', 'surf' )
            call CalmatCmd_Plot3d( plotout = helpout ) 
            title = ' === help for the command "plot3d" or "surf" === '
         case ( 'format' )
            call CalmatCmd_Format  ( formatout = helpout )
            title = ' === help for the command "format" === '
         case ( 'disp' )
            call CalmatCmd_Disp  ( dispout = helpout )
            title = ' === help for the command "disp" === '
         case ( 'exit', 'quit' )
            call CalmatCmd_Quit  ( quitout = helpout )
            title = ' === help for the command "quit" or "exit" === '
         case ( 'for', 'endfor' )
            call CalmatCmd_For  ( forout = helpout )
            title = ' === help for the keywords "for/endfor" === '
         case ( 'if', 'then', 'elseif', 'else', 'endif' )
            call CalmatCmd_If  ( ifout = helpout )
            title = ' === help for the keywords "if-then/elseif-then/else/endif" === '
         case ( 'break' )
            call CalmatCmd_Break  ( breakout = helpout )
            title = ' === help for the command "break" === '
         case ( 'welcome' )
            call CalmatCmd_Welcome  ( welcout = helpout )
            title = ' === help for the command "welcome" === '
         case ( 'set' )
            call CalmatCmd_Set  ( setout = helpout )
            title = ' === help for the command "set" === '
         case ( 'color' )
            call CalmatCmd_color ( colout = helpout )
            title = ' === help for color codes === '
         case ( 'mesh' )
            call CalmatCmd_Mesh  ( meshout = tmp )
            call pk2_movealloc ( from = tmp(1), to = helpout)
            title = ' === help for the command "mesh" === '
         case ( 'eval' )
            call CalmatCmd_Eval  ( evalout = helpout )
            title = ' === help for the command "eval" === '    
         case ( 'exists' )
            call CalmatCmd_Exists  ( res = helpout )
            title = ' === help for the command "exists" === '                        
         case ( '' )
            helpout = h  
            title = ' === help for the command "help" === '
         
         case default
            allocate(tmp(1))
            if ( str == "'" ) then
               tmp(1) = "trans"
            else if ( str == ".'" ) then
               tmp(1) = "transp"
            else 
               tmp(1) = str
            end if
            title = ' === help for the function/operator "'//trim(str)//'" === '
            !to do: call userfunc_help ( str, helpout )
            !to do: if ( helpout%typ == EMPTY ) &
            helpout = pk2f_HELP ( tmp )
      end select
   else
      helpout = h  
      title = ' === help for the command "help" === '
   end if

            
   ! display requested help:
   
   select type ( p=>helpout%m )
      type is ( sk2_t )
         call str_print ( unit = int(G_uo), s = p%v, underline='-', title = title, &
                          colorTitle = G_colorTitle, nblkBefore = 1, nblkAfter = 1 )
   end select
   
   END SUBROUTINE CalmatCmd_Help


!=============================================================================================
   SUBROUTINE CalmatCmd_Mesh ( expr, meshout )
!=============================================================================================
   character(len=*),              optional, intent(in    ) :: expr
   type     (pk2_t), allocatable,           intent(   out) :: meshout(:)
!---------------------------------------------------------------------------------------------
!... under work ....
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=* ), parameter   :: HERE = 'CalmatCmd_Mesh'
   !character(LGSTR )              :: iomsg   
   !integer  (Ikind )              :: i, ui, n, lp, rp, err
   type     (str_t )              :: h(3), kwd
   !character(len=: ), allocatable :: filename, code 
   !type     (pk2_t ), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------

   if ( .not. present(Expr) ) then
      allocate(meshout(1))            

      kwd = str_color('mesh',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> read a mesh'
      h(2) = ' '
      h(3) = 'Syntax: ' + kwd + '("file.msh")'

!       h(1)%str = '<< '// ansiColor_colorTxt (unit=int(G_uo), colorCode=colorHelp, &
!                  str='mesh') // " >> read a mesh"      
!       h(2)%str = ' '
!       h(3)%str = 'Syntax: mesh("file.msh")'
!       h(4)%str = " "
      meshout(1) = h
      return
   end if   
   
   allocate(meshout(2))
   meshout(1) = [1,2,3] ; meshout(1)%name = 'lnods'
   meshout(2) = [0.,1.,2.] ; meshout(2)%name = 'coord'


   END SUBROUTINE CalmatCmd_Mesh


!=============================================================================================
   SUBROUTINE CalmatCmd_Exec ( expr, execout, fileName )
!=============================================================================================
   character(len=*),              optional, intent(in    ) :: expr
   type     (pk2_t),              optional, intent(   out) :: execout
   character(len=:), allocatable, optional, intent(   out) :: fileName
!--------------------------------------------------------------------------------------------- 
!  Command "exec"
!
!  Determines the name of a script file in the expression "expr".
!
!  If present, this expression must be:  'exec(filename)' or 'exec filename' where 
!  << filename >> is a string (enclosed by quotes or double quotes) or a variable name or
!  even an expression involving variables (concatenation). Nothing is returned in "execout".
!  
!  If "expr" is not present, returns the help in "execout".
!
!  Examples:
!
!  . exec("script1")
!  . exec "script1"
!  . exec(DIR + "/script1")
!  . exec DIR + "script1"
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatCmd_Exec'
   integer  (Ikind)              :: lp, rp, err
   type     (str_t)              :: h(6), kwd
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------
         
   if ( .not. present(Expr) ) then
      kwd = str_color ('exec',G_ColorHelp)
      h(1) = '<< ' + kwd + " >> execute a script"      
      h(2) = ' '
      h(3) = 'Syntax: ' + kwd + '("myscript.m")'
      h(4) = '        ' + kwd + ' "myscript.m"'
      h(5) = '        ' + kwd + '(MYDIR + "/myscript.m")'
      h(6) = '        ' + kwd + ' MYDIR + "/myscript.m"'
      execout = h
      return
   end if   

   if ( .not. present(fileName) ) then
      call G_flagerr%set ( stat=G_IERROR, where=HERE, msg='Missing the "fileName" argument' ) 
      return
   end if
   
   err = 0
      
   lp = index(Expr,'(') ; rp = index(Expr,')',back=.true.)
      
   if ( lp == 0 .and. rp == 0 ) then
      fileName = trim(adjustl(expr(5:)))
   else if ( lp < rp ) then
      fileName = expr(lp+1:rp-1)
   else
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' ) 
      return  
   end if
   
   if ( len_trim(fileName) == 0 ) then
      call G_flagerr%set ( stat = UERROR, where = HERE, msg = HERE // &
                        'A file name is required in the expression << '//trim(expr)// ' >>' )
      return
   end if   
!
!- Evaluate 'fileName' to get the name of the script file (call the interpreter):
!
   call pk2Interpreter_driver ( expr     = fileName            , &
                                vars    = G_vars(1:G_nvar)     , &
                                valexpr  = valexpr             , &
                                flagerr  = G_flagerr           )
   if_error_trace_and_RETURN ( G_flagerr, HERE )

   call pk2_moveAlloc ( from = valexpr(1), to = fileName, stat  = G_flagerr  )
   if_error_trace_and_RETURN ( G_flagerr, HERE )

   if ( .not. allocated(fileName) ) then
      err = 1
   else
      if ( len_trim(fileName) == 0 ) err = 1
   end if
   
   if ( err == 1 ) h(1)%str = 'A valid file name is required'
   
   if ( G_flagerr > IZERO ) then
      if ( err == 1 ) then
         h(1)%str = G_flagerr%mesg // NL // '--> ' // (h(1)%str)
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = h(1)%str )
      end if   
      return
   else if ( err == 1 ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = h(1)%str )
      return
   end if
               
   END SUBROUTINE CalmatCmd_Exec
   
   
!=============================================================================================   
   SUBROUTINE CalmatCmd_Clear ( expr, clearout, gins )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t), optional, intent(   out) :: clearout
   integer  (Ikind), optional, intent(in    ) :: gins
!--------------------------------------------------------------------------------------------- 
!  Command "clear"
!
!  Deletes all variables or a set of them
!
!  If "expr" is present, it must start by 'clear' followed by the list of variables (blank
!  delimited). If the list of variables is empty, all variables are deleted.
!
!  If "expr" is not present, returns help into the pk2 variable clearout.
!---------------------------------------------------------------------------------- R.H. 11/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatCmd_Clear'   
   integer  (Ikind)              :: i, k, ntok, IdObj, IdVar
   character(len=:), allocatable :: msg
   type     (str_t), allocatable :: tokens(:,:)
   type     (str_t)              :: h(7), kwd
!---------------------------------------------------------------------------------------------    
      
   if ( .not. present(expr) ) then
      kwd = str_color ('clear',G_ColorHelp)
      h(1) = "<< " + kwd + " >> deletes all variables or a set of them"            
      h(2) = " "
      h(3) = "Syntax: " + kwd + ", " + kwd + " a, " + kwd + " a b c"
      h(4) = " "
      h(5) = ". " + kwd + "      : delete all variables (except protected ones)"
      h(6) = ". " + kwd + " a    : delete the variable a"
      h(7) = ". " + kwd + " a b c: delete the variables a b and c"
      clearout = h
      return
   end if   

   if ( .not. present(gins) ) then
      call G_flagerr%set ( stat = G_IERROR, where = HERE, msg = 'Missing argument "gins"' )
      return
   end if   

   ntok = util_CountTokens ( str    = expr  , delims = ' ' , BlkToken =.false., &
                             tokens = tokens, stat = G_flagerr   )
   if_error_trace_and_RETURN ( G_flagerr, HERE )
           
   if ( ntok == 1 ) then
!
!-    Clear all variables:
!   
      do IdVar = 1, size(G_vars)
!
!-       If this variable is protected, unused or defined after the present instruction: cycle
!         
         if ( G_vars(IdVar)%status   == G_PROTECTED     .or. &
              G_vars(IdVar)%status   == G_USERPROTECTED .or. &
              G_vars(IdVar)%status   == G_FREE          .or. &
              G_vars(IdVar)%firstIns >  gins                 ) then
             
            cycle
   
         else if ( G_vars(IdVar)%lastIns <= gins ) then
!
!-          this variable is only defined before this instruction: destroy it (but DO NOT
!           RENUMBER the remaining variables and DO NOT CHANGE "G_nvar", the freed space
!           will be used by a future variable) and remove it from the list of variable
!           names (normally already removed by Calmat_ParseClear). See also if this 
!           variable is a component of a structure:   
!                          
            k = G_vars(IdVar)%objId
            
            if ( k /= 0 ) then               
!
!-             this variable is a component of the object #k. Remove it from this object:
!               
               call G_objs(k)%RemoveAComponent ( IdVar, G_flagerr )
               if_error_trace_and_RETURN ( G_flagerr, HERE )
                                                   
               if ( G_objs(k)%nCmp == 0 ) call G_objs(k)%Frees()
                   
            end if   

            call G_vars(IdVar)%Destroy ()   
            G_varNames(IdVar) = ''               

         else
!
!-          this variable is also redefined after this instruction. Empty the contents
!           of its value but keep its name: 
!                         
            G_vars(IdVar) = pk2_t ( typ = EMPTY, name = (G_vars(IdVar)%GetName()) )
            G_vars(IdVar)%status = -G_TEMPORARY

         end if
      end do
      
      !!G_vars(1)%name = 'ans'
      
      return
      
   else
!
!-    Clear only the set of variables whose names are given in "tokens(2:ntok,1)" 
!      
      do i = 2, ntok
!
!-       1) See if this is the name of an object (present in the list G_objs):
!
         call CalmatUtil_FindObj ( tokens(i,1)%str, IdObj )
         
         if ( IdObj /=  0) then        

            if ( G_objs(IdObj)%status == G_FREE .or. G_objs(IdObj)%firstIns > gins ) then
!
!-             this object is unused or defined after this clear instruction: cycle
!            
               cycle
            end if

            if ( G_objs(IdObj)%lastIns <= gins ) then
!
!-             this object is only defined before this clear instruction: destroy it
!     
               call CalmatUtil_RemoveObj ( IdObj, G_flagerr ) 
               if_error_trace_and_RETURN ( G_flagerr, HERE )
               
            else 
!
!-             this object is also redefined after this clear instruction: destroy its 
!              components but keep it name and the names of its components that are redefined 
!              after this instruction:
!
               do k = 1, G_objs(IdObj)%nCmp
                  IdVar = G_objs(IdObj)%varId(k)
                  if ( IdVar == 0 ) cycle
                  if ( G_vars(IdVar)%lastIns <= gins ) then
                     call G_objs(IdObj)%RemoveAComponent ( IdVar, G_flagerr )               
                     if ( G_flagerr > IZERO ) return
                     call G_vars(IdVar)%Destroy ()   
                     G_varNames(IdVar) = ''               
                  else             
                     G_vars(IdVar) = pk2_t ( typ = EMPTY, name = (G_vars(IdVar)%GetName()) )
                     G_vars(IdVar)%status = -G_TEMPORARY
                  end if
               end do

            end if
            
            cycle
         end if   
!
!-       2) See if this is the name of a variable (present in the list G_vars):
!
         call CalmatUtil_FindVar ( varName = tokens(i,1)%str, varId = IdVar ) 

         if ( IdVar /= 0 ) then 

            if ( G_vars(IdVar)%status == G_PROTECTED .or.  &
                 G_vars(IdVar)%status == G_USERPROTECTED   ) then 
!
!-             the variable is protected. Set an error message and cycle:
!            
               if ( .not. allocated(msg) ) then
                  msg = trim(adjustl(tokens(i,1)%str))
               else
                  msg = msg // ', ' // trim(adjustl(tokens(i,1)%str))
               end if      
               cycle
               
            else if ( G_vars(IdVar)%status == G_FREE .or. G_vars(IdVar)%firstIns > gins ) then
!
!-             the variable is unused or is defined after the present instruction: cycle
!            
               cycle
            end if 

            if ( G_vars(IdVar)%lastIns <= gins ) then
!
!-             this variable is only defined before this instruction: destroy it (but DO NOT
!              RENUMBER the remaining variables and DO NOT CHANGE "G_nvar", the freed space
!              will be used by a future variable) and remove it from the list of variable
!              names (normally already removed by Calmat_ParseClear). See also if this 
!              variable is a component of a structure:   
!                           
               k = G_vars(IdVar)%objId
            
               if ( k /= 0 ) then               
!
!-                this variable is a component of the object #k. Remove it from this object:
!               
                  call G_objs(k)%RemoveAComponent ( IdVar, G_flagerr )
                  if_error_trace_and_RETURN ( G_flagerr, HERE )
                  
                  if ( G_objs(k)%nCmp == 0 ) call G_objs(k)%Frees()
                   
               end if   

               call G_vars(IdVar)%Destroy ()   
               G_varNames(IdVar) = ''               

            else
!
!-             this variable is also redefined after this instruction. Empty the contents
!              of its value but keep its name: 
!             
               G_vars(IdVar) = pk2_t ( typ = EMPTY, name = (G_vars(IdVar)%GetName()) )
               G_vars(IdVar)%status = -G_TEMPORARY
            end if
            
         end if   
         
      end do   
      !!G_vars(1)%name = 'ans'

   end if
   
   if ( allocated(msg) ) call G_flagerr%set ( stat = G_UERROR, where = HERE, msg =       &
                                     'Protected variables cannot be deleted ('//msg//')' )
           
   END SUBROUTINE CalmatCmd_Clear


!=============================================================================================
   SUBROUTINE CalmatCmd_List ( Expr, listout )
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
   character(len=*), optional, intent(in    ) :: Expr
   type     (pk2_t),           intent(   out) :: listout
!---------------------------------------------------------------------------------------------
!  Command "list"
!
!  If "Expr" is present, it must be: 
!
!  . list('vars'), list(), list or list vars
!  . list('operators'), list('op'), list operators or list op
!  . list('functions'), list('fun'), list functions or list fun
!  . list('commands'), list('cmd'), list commands or list cmd
!
!  and this routine displays the resulting information.
!
!  If "Expr" is not present, the routine returns the help in "listout".
!---------------------------------------------------------------------------------- R.H. 11/18

!- local variables: --------------------------------------------------------------------------
   character(len=* ), parameter         :: HERE = 'CalmatCmd_List'
   integer                              :: i, n, lp, rp, err, n1, n2, j, iv, d, ll(3), ntot
   character(len=99)                    :: typ(G_nvar)
   character(len=: ), allocatable       :: opt, str, name, title
   type     (str_t ), allocatable       :: tmp(:,:)
   type     (str_t)                     :: h(15), kwd
   type     (str_t ), allocatable, save :: func(:,:), ufunc(:)
   integer  (Ikind ), allocatable       :: indx(:)

!---------------------------------------------------------------------------------------------
   
   if ( .not. present(Expr) ) then
      kwd = str_color ('list',G_ColorHelp)
      h( 1) = "<< "+ kwd + " >> can give the list of"                  
      h( 2) = ". all current variables (including structures)"
      h( 3) = ". current structures only"
      h( 4) = ". available built-in and user functions"
      h( 5) = ". available operators"
      h( 6) = ". available commands"
      h( 7) = ". informations about compiler version, options and date"
      h( 8) = " "
      h( 9) = "Syntax: "+kwd+"  or  "+kwd+"()    or  "+kwd+"('vars')  or  "+kwd+" vars"
      h(10) = "        "+kwd+"('structures')     or  "+kwd+"('str')   or  "+kwd+" str"
      h(11) = "        "+kwd+"('functions')      or  "+kwd+"('fun')   or  "+kwd+" fun"
      h(12) = "        "+kwd+"('user functions') or  "+kwd+"('ufun')  or  "+kwd+" ufun"
      h(13) = "        "+kwd+"('commands')       or  "+kwd+"('cmd')   or  "+kwd+" cmd"   
      h(14) = "        "+kwd+"('operators')      or  "+kwd+"('op')    or  "+kwd+" op"
      h(15) = "        "+kwd+"('compiler')       or  "+kwd+"('comp')  or  "+kwd+" comp"
      listout = h
      return
   end if   
   
   opt = ''
   
   err = 0
      
   lp = index(Expr,'(') ; rp = index(Expr,')',back=.true.)
   if ( lp == 0 .and. rp == 0 ) then
      if ( len_trim(adjustl(expr)) > 4 ) then
         opt = trim(adjustl(expr(5:)))
      else
         opt = 'vars'
      end if
   else if ( lp < rp ) then
      opt = trim(adjustl(Expr(lp+1:rp-1)))
      i = len_trim(opt)
      if ( i == 0 ) then
         opt = 'vars'
      else if ( i >= 2 ) then
         opt = opt(2:i-1)  !removes quotation marks
      end if      
   else
      err = 1   
   end if
   
   opt = trim(adjustl(opt))
      
   if (err == 0) then
      
      write(G_uo,*)
      
      select case ( opt )
      
         case ( 'vars' )

            n = 0
            do i = 1, G_nVar
               if ( G_vars(i)%status /= G_FREE   .and. &
                    G_vars(i)%status /= G_HIDDEN .and. &
                    G_vars(i)%objId  == 0              ) then
                  if ( len_trim(G_vars(i)%GetName()) /= 0 ) n = n + 1
               end if
            end do
            do i = 1, G_nObj
               if ( G_objs(i)%status /= G_FREE   .and. &
                    G_objs(i)%status /= G_HIDDEN       ) n = n + G_objs(i)%nCmp + 1
            end do   
            
            n = n + 1
            
            allocate(tmp(n,7),stat=err)
            if ( err /= 0 ) then
               call G_flagerr%set(stat=G_IERROR,where=HERE,msg="Allocation failure for 'tmp'")
               return
            end if   
            
            n = 0
            do i = 1, G_nVar
               if ( G_vars(i)%status == G_FREE .or. G_vars(i)%status == G_HIDDEN ) cycle                    
               name = G_vars(i)%GetName()
               if (len_trim(name) == 0) cycle

               if ( G_vars(i)%typ == EMPTY ) then
                  typ(i) = 'empty'
               else if ( G_vars(i)%typ == ITYP ) then   
                  typ(i) = 'integer'
               else if ( G_vars(i)%typ == RTYP ) then   
                  typ(i) = 'real'
               else if ( G_vars(i)%typ == CTYP ) then   
                  typ(i) = 'complex'
               else if ( G_vars(i)%typ == LTYP ) then   
                  typ(i) = 'logical'
               else if ( G_vars(i)%typ == STYP ) then   
                  typ(i) = 'string'
               end if
              
               if ( G_vars(i)%objId == 0 ) then
                  n = n + 1
                  tmp(n,1)%str = trim(name)
                  tmp(n,2)%str = trim(typ(i))
                  tmp(n,3)%str = util_intToChar(G_vars(i)%nrow)
                  tmp(n,4)%str = 'x'
                  tmp(n,5)%str = util_intToChar(G_vars(i)%ncol)
                  if ( G_vars(i)%status == G_PROTECTED .or. &
                       G_vars(i)%status == G_USERPROTECTED  ) then
                     tmp(n,6)%str = "protected"
                                   
                  else if ( G_vars(i)%status == G_USED ) then
                       tmp(n,6)%str = 'user'                       
                  
                  else
                     tmp(n,6)%str = ''
                  end if
                  tmp(n,7)%str = '(#'//util_intToChar(i)//')'
               end if
            end do   
                  
            n = n + 1
                                  
            do i = 1, G_nObj
               if ( G_objs(i)%status == G_FREE .or. G_objs(i)%status == G_HIDDEN ) cycle
               n = n + 1
               tmp(n,1)%str = trim(G_objs(i)%name)
               tmp(n,2)%str = "structure"
               tmp(n,6)%str = '(#'//util_intToChar(i)//')'                     
               do j = 1, G_objs(i)%nCmp
                  iv = G_objs(i)%varId(j)
                  if ( iv /= 0 ) then
                     n = n + 1
                     str = trim(adjustl(G_vars(iv)%GetName()))
                     d = index(str,'.')
                     tmp(n,1)%str = '  '//str(d:)
                     tmp(n,2)%str = trim(typ(iv))
                     tmp(n,3)%str = util_intToChar(G_vars(iv)%nrow)
                     tmp(n,4)%str = 'x'
                     tmp(n,5)%str = util_intToChar(G_vars(iv)%ncol)
                     tmp(n,6)%str = ''
                     tmp(n,7)%str = '(=> #'//util_intToChar(iv)//')'
                  end if
               end do
            end do    
                        
            title = ' === Current variables (name, type, size, status, #) === '
            call str_print ( unit=int(G_uo), s=tmp, underline='-', title=title, &
                             colorTitle=G_colorTitle, justify='llrcllc' )
            listout = tmp
            
         case ( 'structures', 'str' )

            n = 0
            do i = 1, G_nObj
               if ( G_objs(i)%status /= G_FREE .and. G_objs(i)%status /= G_HIDDEN ) &
                    n = n + G_objs(i)%nCmp + 1
            end do   
                        
            allocate(tmp(n,7),stat=err)
            if ( err /= 0 ) then
               call G_flagerr%set(stat=G_IERROR,where=HERE,msg="Allocation failure for 'tmp'")
               return
            end if   
        
            n = 0
            do i = 1, G_nObj
               if ( G_objs(i)%status == G_FREE .or. G_objs(i)%status == G_HIDDEN ) cycle
               n = n + 1
               tmp(n,1)%str = trim(G_objs(i)%name)
               tmp(n,2)%str = "structure"
               tmp(n,6)%str = '(#'//util_intToChar(i)//')'                     
               do j = 1, G_objs(i)%nCmp
                  iv = G_objs(i)%varId(j)
                  if ( iv /= 0 ) then
                     name = G_vars(iv)%GetName()
                     if ( len_trim(name) == 0 ) cycle
                     if ( G_vars(iv)%typ == EMPTY ) then
                        typ(j) = 'empty'
                     else if ( G_vars(iv)%typ == ITYP ) then   
                        typ(j) = 'integer'
                     else if ( G_vars(iv)%typ == RTYP ) then   
                        typ(j) = 'real'
                     else if ( G_vars(iv)%typ == CTYP ) then   
                        typ(j) = 'complex'
                     else if ( G_vars(iv)%typ == LTYP ) then   
                        typ(j) = 'logical'
                     else if ( G_vars(iv)%typ == STYP ) then   
                        typ(j) = 'string'
                     end if   
                     n = n + 1
                     str = trim(adjustl(name))
                     d = index(str,'.')
                     tmp(n,1)%str = '  '//str(d:)
                     tmp(n,2)%str = trim(typ(j))
                     tmp(n,3)%str = util_intToChar(G_vars(iv)%nrow)
                     tmp(n,4)%str = 'x'
                     tmp(n,5)%str = util_intToChar(G_vars(iv)%ncol)
                     tmp(n,6)%str = ''
                     tmp(n,7)%str = '(=> #'//util_intToChar(iv)//')'
                  end if
               end do
            end do 

            title = ' === Current objects (component names, type, size, #) === '
            call str_print ( unit=int(G_uo), s=tmp, underline='-', title=title, &
                             colorTitle=G_colorTitle, justify='llrclc' )
            listout = tmp
            
         case ( 'operators', 'op ')
!
!-          print the available operators
!
            allocate(tmp(size(OperList),2), stat = err)
            if ( err /= 0 ) then
               call G_flagerr%set(stat=G_IERROR,where=HERE,msg="Allocation failure for 'tmp'")
               return
            end if   

            do i = 1, size(OperList)
               tmp(i,1) = str_color ( OperList(i), G_colorHelp )
            end do
            tmp(:,2) = OperDesc

            title = ' === list of available operators === '
            call str_print ( unit=int(G_uo), s=tmp, underline='-', title=title, & 
                             colorTitle=G_colorTitle )

            listout = tmp

         case ( 'functions', 'fun' )
!
!-          print the available functions names
!
            if ( .not. allocated(func) ) then
               ntot = NFC + 2 + max(1_Ikind,nuserf)
               allocate(func(ntot,2))
               func(1:NFC,1) = FuncList(1:NFC)
               func(NFC+1,1) = '' ; func(NFC+1,2) = ''
               if ( nuserf > 0 ) then
                  func(NFC+2,1) = "User's:" ; func(NFC+2,2) = ''
                  func(NFC+3:NFC+nuserf+2,1) = FuncList(NFC+1:NFC+nuserf) 
               else
                  func(NFC+2,1) = "User's:" ; func(NFC+2,2) = ''
                  func(NFC+3,1) = "(none)"
                  func(NFC+3,2) = ''
               end if
                             
               call util_Sort ( func(1:NFC,1), 'i', is_CaseInsensitive = .true., &
                                indx = indx, stat = G_flagerr ) 
               do i = 1, NFC
                  if ( allocated(FuncDesc(indx(i))%str) ) then
                     func(i,2)%str = FuncDesc(indx(i))%str
                  else
                     func(i,2)%str = ''
                  end if
                  func(i,1) = str_color ( func(i,1), G_colorHelp )
               end do

               call util_Sort ( func(NFC+3:NFC+nuserf+2,1), 'i', is_CaseInsensitive = .true., &
                                stat = G_flagerr ) 
               do i = NFC+3, ntot
                  func(i,2)%str =''
                  func(i,1) = str_color ( func(i,1), G_colorHelp )
               end do
            end if
            
            title = ' === list of available functions (alphabetical order) ==='
            call str_print ( unit=int(G_uo), s=func, underline='-', title=title, & 
                             colorTitle=G_colorTitle )
            listout = func

         case ( 'user functions', 'ufun' )

            ufunc = FuncList(NFC+1:NFC+nuserf)
            call util_Sort(ufunc, 'i', is_CaseInsensitive = .true., stat = G_flagerr) 

            do i = 1, nuserf
               ufunc(i) = str_color ( ufunc(i), G_colorHelp )
            end do
               
            title = ' === list of user functions (alphabetical order) ==='
            call str_print ( unit=int(G_uo), s=ufunc, underline='-', title=title, & 
                             colorTitle=G_colorTitle )
            listout = ufunc
   
         case ( 'commands', 'cmd' )
!
!-          print the available commands names
!
            allocate(tmp(size(G_CmdNames),2))
            if ( err /= 0 ) then
               call G_flagerr%set(stat=G_IERROR,where=HERE,msg="Allocation failure for 'tmp'")
               return
            end if   
            do i = 1, size(G_CmdNames)
               tmp(i,1) = str_color ( G_CmdNames(i), G_colorHelp )
            end do
            tmp(:,2) = G_CmdDescr

            title = ' === list of available commands === '
            call str_print ( unit=int(G_uo), s=tmp, underline='-', title=title, & 
                             colorTitle=G_colorTitle )
            listout = tmp

         case ( 'compiler', 'comp' )
!
!-          print compiler informations
!
            h(1)%str = compiler_version()

            if ( len_trim(G_compilOPts) /= 0 ) then
               h(2)%str = G_compilOPts
            else   
               h(2)%str = compiler_options()
            end if
            if ( len_trim(G_compilDate) /= 0 ) then
               h(3)%str = G_compilDate
            else   
               h(3)%str = ''
            end if            
             
            ll(1) = max(1,len_trim(h(1)%str)/40)
            ll(2) = max(1,len_trim(h(2)%str)/40)
            ll(3) = max(1,len_trim(h(3)%str)/40)
            
            allocate(tmp(ll(1)+ll(2)+ll(3),2))
            n1 = 1
            do i = 1, ll(1)
               if ( i == 1 ) then
                  tmp(1,1)%str = 'Compiler version:'
               else   
                  tmp(i,1)%str = ''
               end if
               n2 = n1 + 40
               if ( n2 > len_trim(h(1)%str) .or. i == ll(1)) n2 = len_trim(h(1)%str )
               tmp(i,2)%str = h(1)%str(n1:n2)
               n1 = n2+1
            end do    
            n1 = 1
            do i = 1, ll(2)
               j = ll(1)+i
               if ( i == 1 ) then
                  tmp(j,1)%str = 'Compiler options:'
               else   
                  tmp(j,1)%str = ''
               end if
               n2 = n1 + 40
               if ( n2 > len_trim(h(2)%str) .or. i == ll(2)) n2 = len_trim(h(2)%str )
               tmp(j,2)%str = h(2)%str(n1:n2)
               n1 = n2+1
            end do    
            n1 = 1
            do i = 1, ll(3)
               j = ll(1)+ll(2)+i
               if ( i == 1 ) then
                  tmp(j,1)%str = 'Compilation date:'
               else   
                  tmp(j,1)%str = ''
               end if
               n2 = n1 + 40
               if ( n2 > len_trim(h(3)%str) .or. i == ll(3)) n2 = len_trim(h(3)%str )
               tmp(j,2)%str = h(3)%str(n1:n2)
               n1 = n2+1
            end do                                  

            title = ' === Compiler informations === '
            call str_print ( unit=int(G_uo), s=tmp, underline='-', title=title, & 
                             colorTitle=G_colorTitle )
            listout = tmp

         case default
            err = 1
            
      end select
   end if
   
   if ( err == 1 ) then                  
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
         'Unknown option for command << list >>. Valid options are:'                    //NLT// &
         ' . list      or list vars           or list()       or list("vars")'          //NLT// &
         ' . list str  or list structures     or list("str")  or list("structures")'    //NLT// &
         ' . list op   or list operators      or list("op")   or list("operators")'     //NLT// &
         ' . list cmd  or list commands       or list("cmd")  or list("commands")'      //NLT// &
         ' . list fun  or list functions      or list("fun")  or list("functions")'     //NLT// &         
         ' . list ufun or list user functions or list("ufun") or list("user functions")'//NLT// &         
         ' . list comp or list compilers      or list("comp") or list("compiler")'              )                
   end if

   END SUBROUTINE CalmatCmd_List   


!=============================================================================================   
   SUBROUTINE CalmatCmd_pwd ( expr, pwdout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr 
   type     (pk2_t),           intent(   out) :: pwdout
!---------------------------------------------------------------------------------------------  
!  Command "pwd"
!  
!  Returns the Path of Current Working Directory
!
!  If "expr" is not present, returns help in the pk2 variable pwdout
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatCmd_pwd'   
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      pwdout = pwd ( help = .true. )    ! PK2F_PWD

   else if ( expr == 'pwd' .or. expr == 'pwd()' ) then
!
!-    if the expression is just 'pwd' or 'pwd()' get the path directly:
!
      pwdout = util_pwd ( G_flagerr )
      
   else   
!
!-    otherwise (e.g. pwd()+"/mysubdir") use the interpreter:
!
      call pk2Interpreter_driver ( expr     = expr                , &
                                   vars     = G_vars(1:G_nvar)    , &
                                   valexpr  = valexpr             , &
                                   flagerr  = G_flagerr           )
      if_error_trace_and_RETURN ( G_flagerr, HERE )
       
      call pk2_moveAlloc ( from = valexpr(1), to = pwdout, stat = G_flagerr )
      
   end if

   if_error_trace_and_RETURN ( G_flagerr, HERE )
         
   END SUBROUTINE CalmatCmd_pwd


!=============================================================================================   
   SUBROUTINE CalmatCmd_chdir ( expr, cdout )
!=============================================================================================
   use iso_c_binding, only: c_int, c_null_char
   character(len=*), optional, intent(in    ) :: expr 
   type     (pk2_t),           intent(   out) :: cdout
!---------------------------------------------------------------------------------------------    
!  Command "cd"
!
!  Changes the current directory
!
!  If "expr" is not present, returns help in the pk2 variable cdout
!
!  Note: unfortunately, the function/subroutine fortran chdir are not portable. 
!        As a workaround, we use here the interoperablity with C to call the corresponding C
!        function (solution found on Stackoverflow).
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatCmd_chdir'   
   character(len=2)              :: cd
   type     (str_t)              :: h(7), kwd
   character(len=:), allocatable :: dir, name
   integer                       :: ldir, i
   integer  (c_int)              :: c_stat
   type     (pk2_t), allocatable :: pk2dir(:)
   logical                       :: to_interprete
!---------------------------------------------------------------------------------------------    

   interface
       function c_chdir (path) bind(c, name = "chdir") result(r)
         use iso_c_binding
         character(kind=c_char), intent(in) :: path(*)
         integer  (c_int      )             :: r
       end function
   end interface
        
   if ( .not. present(expr) ) then
      kwd = str_color('cd',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> changes the current directory (linux command)'
      h(2) = ' '
      h(3) = 'Syntax: ' + kwd + ' [directory]'
      h(4) = ' '
      h(5) = 'Examples:'
      h(6) = '  . ' + kwd
      h(7) = '  . ' + kwd + ' /home/users/me/tmp/src'
      cdout = h
      return
   end if   
      
   cd = trim(adjustl(expr(1:2))) ; dir = trim(adjustl(expr(3:)))   
   
   if ( trim(expr) == 'cd' .or. (cd == 'cd' .and. dir == '~') ) then
!
!-    Cases "cd" or "cd ~". Get the home directory:
!   
      call get_environment_variable (name = 'HOME', length = ldir)
      deallocate(dir) ; allocate(character(len=ldir) :: dir)
      call get_environment_variable (name = 'PWD', value = dir)
      dir = (trim(adjustl(dir)))
      
   else
!
!-    See if the expression after "cd" contains the character '+' (concatenation) or if it is 
!     a variable name. If so, set "to_interprete" to .true.:
!     
      to_interprete = .false.
      if ( index(dir,'+') /= 0 ) then
         to_interprete = .true.
      else   
         do i = 1, G_nvar
            if ( G_vars(i)%status == G_FREE ) cycle
            name = (G_vars(i)%GetName())
            if ( len_trim(name) /= 0 ) then
               if ( dir == trim(adjustl(name)) ) then 
                  to_interprete = .true.
                  exit
               end if   
            end if
         end do
      end if   
!
!-    If "to_interprete" is .true. call the interpreter to evaluate the name of the directory:
! 
      if ( to_interprete ) then
          
         call pk2Interpreter_driver ( expr     = dir                 , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = pk2dir              , &
                                      flagerr  = G_flagerr           )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
       
         call pk2_moveAlloc ( from = pk2dir(1), to = dir, stat = G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )

      end if
      
   end if   
!
!- Change directory:
!   
   c_stat =  c_chdir ( dir//c_null_char )
   
   if ( c_stat /= 0 ) call G_flagerr%set ( stat = G_UERROR, where = HERE, msg =            &
                                          "Could not find the directory '"//trim(dir)//"'" ) 
   
   END SUBROUTINE CalmatCmd_chdir
   

!=============================================================================================   
   SUBROUTINE CalmatCmd_ls ( expr, lsout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(   out) :: lsout
!---------------------------------------------------------------------------------------------
!  Command "ls"    
!
!  Returns the list directory contents
!
!  If "expr" is not present, returns help in the pk2 variable lsout
!
!  Note: it uses the execute_command_line
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'CalmatCmd_ls'
   type     (str_t)              :: h(7), kwd   
   integer  (Ikind)              :: err, u, n, i
   character(len=:), allocatable :: line   
   character(len=1)              :: dummyCh
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('ls',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> lists the directory contents (linux command)'
      h(2) = ' '
      h(3) = 'Syntax: '+kwd+' [directory]'
      h(4) = ' '
      h(5) = 'Examples:'      
      h(6) = '  . '+kwd
      h(7) = '  . '+kwd+' /home/users/me/tmp/src'
      lsout = h
      return
   end if
   
   call CalmatUtil_ExecuteCommandLine ( expr//' > .calmat_ls_tmp#12#123.out', G_flagerr )    
   if_error_trace_and_RETURN ( G_flagerr, HERE )
   
   u = util_GetUnit () ; open(unit = u,file = '.calmat_ls_tmp#12#123.out')
   
   n = 0
   do 
      read(u,'(a1)',iostat=err) dummyCh
      if ( err /= 0 ) exit
      n = n + 1
   end do   
   rewind(u)
   
   lsout = pk2_t (typ = STYP, shape = [n,1_Ikind])
   
   select type ( p=>lsout%m )
      type is (sk2_t)
         do i = 1, n
            line = util_GetLine (unit = u, stat = G_flagerr) 
            if ( G_flagerr /= IZERO ) exit
            p%v(i,1)%str = trim(adjustl(line))
         end do   
   end select
   
   close(u,status = 'delete')
      
   END SUBROUTINE CalmatCmd_ls


!=============================================================================================   
   SUBROUTINE CalmatCmd_more ( expr, moreout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(   out) :: moreout
!---------------------------------------------------------------------------------------------
!  Command "more"    
!
!  If "expr" is not present, returns help in the pk2 variable moreout
!
!  Note: it uses the execute_command_line
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(3), kwd
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('more',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> shows the content of a file (linux command)'
      h(2) = ' '
      h(3) = 'Syntax: '+kwd+' fileName'
      moreout = h
      return
   end if
      
   call CalmatUtil_ExecuteCommandLine ( expr, G_flagerr )    
   if_error_trace_and_RETURN ( G_flagerr, "CalmatCmd_more" )
         
   END SUBROUTINE CalmatCmd_more


!=============================================================================================   
   SUBROUTINE CalmatCmd_tail ( expr, tailout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(   out) :: tailout
!---------------------------------------------------------------------------------------------
!  Command "more"    
!
!  If "expr" is not present, returns help in the pk2 variable tailout
!
!  Note: it uses the execute_command_line
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(3), kwd  
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('tail',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> displays the last part of a file (linux command)'
      h(2) = ' '
      h(3) = 'Syntax: '+kwd+' fileName'
      tailout = h
      return
   end if
      
   call CalmatUtil_ExecuteCommandLine ( expr, G_flagerr ) 
   if_error_trace_and_RETURN ( G_flagerr, "CalmatCmd_tail" )

   END SUBROUTINE CalmatCmd_tail


!=============================================================================================   
   SUBROUTINE CalmatCmd_linux ( expr, linuxout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(   out) :: linuxout
!---------------------------------------------------------------------------------------------
!  Command "linux"    
!
!  If "expr" is not present, returns help in the pk2 variable linuxout
!
!  Note: it uses the execute_command_line
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'CalmatCmd_linux'
   type     (str_t)              :: h(3), kwd
   character(len=:), allocatable :: str
   integer                       :: lstr, lp  
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('linux',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> acces to linux commands'
      h(2) = ' '
      h(3) = 'Syntax: '+kwd+'("command") or linux command'
      linuxout = h
      return
   end if
   
   str = adjustl(expr(6:)) ; lstr = len_trim(str)
   
   lp = index(str,'(') 
   if ( lp /= 0 ) then
      if ( str(lstr:lstr) /= ')' ) then
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' )
         return
      end if
      str = str(lp+1:lstr-1)
      lstr = len_trim(str)       
   end if
   
   lp = index(str,'"')
   if ( lp == 1 ) then
      if ( str(lstr:lstr) /= '"' ) then
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced quotes (")' )
         return
      end if
      str = str(lp+1:lstr-1)
   else
      lp = index(str,"'")
      if ( lp == 1 ) then
         if ( str(lstr:lstr) /= "'" ) then
            call G_flagerr%set (stat = G_UERROR, where = HERE, msg = "Unbalanced quotes (')")
            return
         end if
         str = str(lp+1:lstr-1)
      end if       
   end if
         
   call CalmatUtil_ExecuteCommandLine ( str, G_flagerr ) 
   if_error_trace_and_RETURN ( G_flagerr, HERE )

   END SUBROUTINE CalmatCmd_linux


!=============================================================================================   
   SUBROUTINE CalmatCmd_cp ( expr, cpout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(   out) :: cpout
!---------------------------------------------------------------------------------------------
!  Command "cp"    
!
!  If "expr" is not present, returns help in the pk2 variable cpout
!
!  Note: it uses the execute_command_line
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   type     (str_t)              :: h(7), kwd 
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('cp',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> copies files (linux command)'
      h(2) = ' '
      h(3) = 'Syntax: ' + kwd + ' source_file target_file'
      h(4) = ' '
      h(5) = 'Note: this is the linux '+kwd+' command with the option "-i" (request'
      h(6) = '      confirmation before attempting to overwrite an existing file).'
      kwd = str_color('linux cp',G_ColorHelp)
      h(7) = '      Use the command "'+kwd+'" to access the other options'
      cpout = h
      return
   end if
!
!- Request confirmation (add -i):
!
   str = expr
      
   if ( index(str,' -i ') == 0 ) str = 'cp -i ' // str(3:)
         
   call CalmatUtil_ExecuteCommandLine ( str, G_flagerr ) 
   if_error_trace_and_RETURN ( G_flagerr, "CalmatCmd_cp" )

   END SUBROUTINE CalmatCmd_cp


!=============================================================================================   
   SUBROUTINE CalmatCmd_mv ( expr, mvout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(   out) :: mvout
!---------------------------------------------------------------------------------------------
!  Command "mv"    
!
!  If "expr" is not present, returns help in the pk2 variable mvout
!
!  Note: it uses the execute_command_line
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   type     (str_t)              :: h(7), kwd  
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('mv',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> moves files (linux command)'
      h(2) = ' '
      h(3) = 'Syntax: '+ kwd +' source target'
      h(4) = ' '
      h(5) = 'Note: this is the linux '+kwd+' command with the option "-i" (request'
      h(6) = '      confirmation before attempting to overwrite an existing file).'
      kwd = str_color('linux mv',G_ColorHelp)      
      h(7) = '      Use the command "'+kwd+'" to access the other options'
      mvout = h
      return
   end if
!
!- Request confirmation (add -i):
!
   str = expr
      
   if ( index(str,' -i ') == 0 ) str = 'mv -i ' // str(3:)
         
   call CalmatUtil_ExecuteCommandLine ( str, G_flagerr ) 
   if_error_trace_and_RETURN ( G_flagerr, "CalmatCmd_mv" )

   END SUBROUTINE CalmatCmd_mv


!=============================================================================================   
   SUBROUTINE CalmatCmd_rm ( expr, rmout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(   out) :: rmout
!---------------------------------------------------------------------------------------------
!  Command "rm"    
!
!  If "expr" is not present, returns help in the pk2 variable rmout
!
!  Note: it uses the execute_command_line
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   type     (str_t)              :: h(7), kwd   
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('rm',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> removes files (linux command)'
      h(2) = ' '
      h(3) = 'Syntax: '+kwd+' file1 ...'
      h(4) = ' '
      h(5) = 'Note: this is the linux '+kwd+' command with the option "-i" (request'
      h(6) = '      confirmation before attempting to remove each file).'
      kwd = str_color('linux rm',G_ColorHelp)
      h(7) = '      Use the command "'+kwd+'" to access the other options'
      rmout = h
      return
   end if
!
!- Request confirmation (add -i):
!
   str = expr
      
   if ( index(str,' -i ') == 0 ) str = 'rm -i ' // str(3:)
         
   call CalmatUtil_ExecuteCommandLine ( str, G_flagerr ) 
   if_error_trace_and_RETURN ( G_flagerr, "CalmatCmd_rm" )

   END SUBROUTINE CalmatCmd_rm


!=============================================================================================   
   SUBROUTINE CalmatCmd_Plot ( expr, flowId, plotout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr 
   integer  (Ikind), optional, intent(in    ) :: flowId
   type     (pk2_t),           intent(   out) :: plotout
!---------------------------------------------------------------------------------------------    
!  Command "plot"
!  Very basic way to make plot (gnuplot invocation)
!---------------------------------------------------------------------------------- R.H. 02/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*    ), parameter   :: HERE = 'CalmatCmd_Plot'
   character(len=:    ), allocatable :: str, gpscript, gpfig, tmpfile, styl, buf, legend, &
                                        gpcmd, gpterm, title, font, cmd
   type     (str_t    ), allocatable :: plot(:,:), tokens(:,:)
   type     (str_t    )              :: h(14), kwd
   integer  (Ikind    )              :: lp, rp, nplot, ntok, narg, i, j, n1, m1, n2, m2,  &
                                        nl, up, ugpscript
   integer                           :: cmdstat, exitstat, err
   type     (pk2_t    )              :: tmp(2)
   type     (pk2_t    ), allocatable :: valexpr(:)
   character(len=99   )              :: cnum
   character(len=LGSTR)              :: cmdmsg
!---------------------------------------------------------------------------------------------    
   integer  (Ikind    ), save        :: ngpfig = 0   
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('plot',G_ColorHelp)
      h( 1) = '<< ' + kwd + ' >> 2D plot '
      h( 2) = ' '
      h( 3) = 'Syntax: '+kwd+' ( x1 [, y1] [,line_spec] [, x2, y2 [,line_spec] )'
      h( 4) = ' '
      h( 5) = ' '
      h( 6) = 'Examples:'      
      h( 7) = '  . '+kwd+'(x1)'
      h( 8) = '  . '+kwd+'(x1,y1)'
      h( 9) = '  . '+kwd+'(x1,y1,"-r")'
      h(10) = '  . '+kwd+'(x1,y1,"-r" , x2,y2,"ok")'
      h(11) = '  . '+kwd+'(x1,y1,"-r" , x2,y2,"ok" , title = "my title")'
      h(12) = ' '
      h(13) = '(Note: plots are generated by the command-line program gnuplot. '
      h(14) = '       The figures are persistent but not interactive... need to be improved)'
      plotout = h
      return
   end if
   
   gpcmd  = G_gnuplot(1)%str
   gpterm = G_gnuplot(2)%str
   font   = G_gnuplot(3)%str
   
   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if ( lp * rp == 0 .or. rp <= lp ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' )
      return
   end if
   
   if ( rp - lp <= 1 ) then
      call G_flagerr%set ( stat = G_WARNING, where = HERE, &
                            msg = 'Nothing to plot (at least one argument expected)' )
      return
   end if
   
   str = trim(adjustl(expr(lp+1:rp-1))) ! remove parentheses   
   
!
!- Determine the number of graphs (delimited by a ';'):
!
   !nplot = util_CountTokens (str, delims = ';', BlkToken =.false., opcl = '[]()""', &
   !                          tokens = plot, stat = G_flagerr)
   call howManyGraph ( )
   if_error_trace_and_RETURN ( G_flagerr, HERE )
!
!- Open a gnuplot script:
!         
   ugpscript = util_GetUnit ()
   
   if ( ugpscript == 0 ) then
      call G_flagerr%set ( stat = G_IERROR, where = HERE,  msg = "No free unit available" )
      return
   end if
   
   gpscript = '.tmp?_?gpscript?'
   open(unit = ugpscript, file = gpscript, status = 'replace')

   write(ugpscript,'(a)')'reset'

   gpfig = ''
   
   if ( .not. G_dispPrompt ) then
!
!-    in batch mode, save the plot in png file (with a name 'name of the script'_fig#.png)
!
      ngpfig = ngpfig + 1
      write(cnum,'(i0)')ngpfig
      
      if ( present(flowId) ) then
         if ( flowId > 0 .and. flowId <= G_maxnFlow ) then
            gpfig = trim(G_flows(flowId)%fileName)//'_fig'//trim(cnum)//'.png'
         else
            call G_flagerr%set ( stat = G_IERROR, where = HERE, msg = 'Bad value flow #' )
            close(ugpscript, status = 'delete')
            return
         end if
      else 
          call G_flagerr%set ( stat = G_IERROR, where = HERE, msg = 'Missing flow #' )
          close(ugpscript, status = 'delete')
          return
      end if
      
      write(ugpscript,'(a)')'set terminal pngcairo enhanced font "'//trim(font)//'"'
      write(ugpscript,'(a)')"set output '"//trim(gpfig)//"'"
   else
      write(ugpscript,'(a)')'set terminal '//trim(gpterm)//' persist font "'//trim(font)//'"'
   end if

   write(ugpscript,'(a)')'set grid'   
!
!- Loop over the n graphs:
!   
   title = '' ; cmd = ''
   do i = 1, nplot

      ntok = util_CountTokens (plot(i,1)%str, delims = ',', BlkToken =.false., &
                               opcl = '[]()""', tokens = tokens, stat = G_flagerr)
      if_error_trace_and_EXIT ( G_flagerr, HERE )

      if ( index(tokens(1,1)%str, 'title') == 1 ) then
         j = index(plot(i,1)%str,'=')
         title = tokens(1,1)%str(j+1:)
         call pk2Interpreter_driver ( expr     = title               , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr            )
         if_error_trace_and_EXIT ( G_flagerr, HERE )
         
         if ( valexpr(1)%typ /= STYP ) then
            call G_flagerr%set( stat = G_UERROR, where = HERE, msg = 'title is not a string' )
            exit
         end if
         
         title = ''
         select type ( p=>valexpr(1)%m )
            type is (sk2_t) ;  title = p%v(1,1)
         end select

         if ( ntok > 1 ) then 
            font = tokens(2,1)%str
            call pk2Interpreter_driver ( expr     = font                , &
                                         vars     = G_vars(1:G_nvar)    , &
                                         valexpr  = valexpr             , &
                                         flagerr  = G_flagerr            )
            if_error_trace_and_EXIT ( G_flagerr, HERE )
         
            if ( valexpr(1)%typ /= ITYP ) then
               call G_flagerr%set(stat=G_UERROR, where=HERE, msg='font size is not an integer')
               exit
            end if
         
            font = ''
            select type ( p=>valexpr(1)%m )
               type is (ik2_t) ; font = util_intToChar(p%v(1,1))
            end select
         end if
         cycle 
      end if        
!
!-    Determine the line or symbol style:
!         
      if ( index(tokens(ntok,1)%str,'"') /= 0 ) then
         buf = tokens(ntok,1)%str
         narg = ntok - 1         
         nl = len_trim(buf)
         if ( buf(1:1) == '"' .and. buf(nl:nl) == '"' ) then
            buf = buf(2:nl-1) 
            styl = ' w l lw 2 '
               
            if ( index(buf,'-' ) /= 0 ) styl = ' w l lw 2 dt 1'
            if ( index(buf,':' ) /= 0 ) styl = ' w l lw 2 dt 3'
            if ( index(buf,'--') /= 0 ) styl = ' w l lw 2 dt 2'
            if ( index(buf,'-.') /= 0 ) styl = ' w l lw 2 dt 4'
               
            if ( index(buf,'o') /= 0 ) styl = ' w p pt  7 ps 1 '
            if ( index(buf,'+') /= 0 ) styl = ' w p pt  1 ps 2 '
            if ( index(buf,'x') /= 0 ) styl = ' w p pt  2 ps 1 '
            if ( index(buf,'*') /= 0 ) styl = ' w p pt  3 ps 1 '
            if ( index(buf,'d') /= 0 ) styl = ' w p pt 28 ps 1 '
            if ( index(buf,'s') /= 0 ) styl = ' w p pt 35 ps 1 '
            if ( index(buf,'v') /= 0 ) styl = ' w p pt 11 ps 1 '
            if ( index(buf,'^') /= 0 ) styl = ' w p pt  9 ps 1 '
               
            if ( index(buf,'b') /= 0 ) styl = trim(styl) // ' lc rgb "blue" '
            if ( index(buf,'r') /= 0 ) styl = trim(styl) // ' lc rgb "red" '
            if ( index(buf,'g') /= 0 ) styl = trim(styl) // ' lc rgb "green" '
            if ( index(buf,'y') /= 0 ) styl = trim(styl) // ' lc rgb "yellow" '
            if ( index(buf,'k') /= 0 ) styl = trim(styl) // ' lc rgb "black" '
            if ( index(buf,'m') /= 0 ) styl = trim(styl) // ' lc rgb "dark-magenta" '
            if ( index(buf,'c') /= 0 ) styl = trim(styl) // ' lc rgb "dark-cyan" '
            if ( index(buf,'w') /= 0 ) styl = trim(styl) // ' lc rgb "white" '
         else
            call G_flagerr%set ( stat=G_UERROR, where = HERE, msg = 'invalid line spec.' )
            exit
         end if      
      else
         styl = ' w l lw 2 '
         narg = ntok
      end if

      if ( narg == 1 ) then
!
!-       Case "plot(y)"
!        --------------

!
!-       Evaluate y (= tmp(2)):
!      
         call pk2Interpreter_driver ( expr     = tokens(1,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr            )
         if_error_trace_and_EXIT ( G_flagerr, HERE )
         
         call pk2_movealloc ( from = valexpr(1), to = tmp(2),  stat = G_flagerr )
         if_error_trace_and_EXIT ( G_flagerr, HERE )
         
         n1 = tmp(2)%nrow ; m1 = tmp(2)%ncol ; n2 = n1 ; m2 = m1
         
         if ( n1 /= 1 .and. m1 /= 1 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'variables of "plot" must be vectors' )
            exit
         end if
         
         if ( tmp(2)%typ > RTYP ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'variables of "plot" must be integers or reals' )
            exit
         end if
!
!-       and set x = 1:size(y)  (= tmp(1)):
!         
         tmp(1) = colon ( IONE, n1*m1, IONE ) ! PK2F_COLONI
         if ( m1 == 1 ) tmp(1) = RESHAPE (tmp(1), [n1,m1]) ! pk2f_RESHAPEnm
         
         legend = trim(tokens(1,1)%str)
         
      else if ( narg == 2 ) then
!
!-       Case "plot(x,y)"
!        ---------------

!
!-       Evaluate x (= tmp(1)):
!       
         call pk2Interpreter_driver ( expr     = tokens(1,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )                              
         if_error_trace_and_EXIT ( G_flagerr, HERE )

         call pk2_movealloc ( from = valexpr(1), to = tmp(1), stat = G_flagerr )
         if_error_trace_and_EXIT ( G_flagerr, HERE )
         
         n1 = tmp(1)%nrow ; m1 = tmp(1)%ncol 
         
         if ( n1 /= 1 .and. m1 /= 1 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Variables of "plot" must be vectors' )
            exit
         end if
         
         if ( tmp(1)%typ > RTYP ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Variables of "plot" must be integers or reals' )
            exit
         end if
!
!-       Evaluate y (= tmp(2)):
!         
         call pk2Interpreter_driver ( expr     = tokens(2,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )
         if_error_trace_and_EXIT ( G_flagerr, HERE )

         call pk2_movealloc ( from = valexpr(1), to = tmp(2), stat = G_flagerr )
         if_error_trace_and_EXIT ( G_flagerr, HERE )

         n2 = tmp(2)%nrow ; m2 = tmp(2)%ncol 
         
         if ( n2 /= 1 .and. m2 /= 1 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Variables of "plot" must be vectors' )
            exit
         end if
         
         if ( tmp(2)%typ > RTYP ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Variables of "plot" must be integers or reals' )
            exit
         end if
         
         if ( n1 * m1 /= n2 * m2 ) then
            call G_flagerr%set (stat = G_UERROR, where = HERE,&
                                 msg = 'Incompatible size for "plot"' )
            exit
         end if
         
         legend = trim(tokens(2,1)%str)
         
      else
         call G_flagerr%set ( stat = G_UERROR, where = HERE,                  &
                               msg = 'Expression << '//trim(expr)// ' >> ' // &
                                     'has an error (see the help of "plot")'  )
         exit
      end if   
      
      if ( n2 /= n1 ) tmp(2) = RESHAPE (tmp(2), [n1,m1]) ! PK2F_RESHAPENM
!
!-    Write the x and y values in a tmp file (#i):
!      
      write(cnum,'(i0)')i
      tmpfile = trim(gpscript)//'.#'//trim(cnum)
      
      up = util_GetUnit ()
      
      if ( up == 0 ) then
         call G_flagerr%set ( stat = G_IERROR, where = HERE, msg = "No free unit available" )
         exit
      end if
         
      open(unit = up, file = tmpfile, status = 'replace')

      select type ( p => tmp(1)%m )
         type is (ik2_t) 
            select type ( q => tmp(2)%m )
               type is (ik2_t)
                  if ( n1 == 1 ) then
                     do j = 1, m1
                        write(up,'(2(i0,1x))')p%v(1,j),q%v(1,j)
                     end do
                  else
                     do j = 1, n1
                        write(up,'(2(i0,1x))')p%v(j,1),q%v(j,1)
                     end do
                  end if
               type is (rk2_t) 
                  if ( n1 == 1 ) then
                     do j = 1, m1
                        write(up,'(i0,1x,g0)')p%v(1,j),q%v(1,j)
                     end do
                  else
                     do j = 1, n1
                        write(up,'(i0,1x,g0)')p%v(j,1),q%v(j,1)
                     end do
                  end if
            end select  
         type is (rk2_t) 
            select type ( q => tmp(2)%m )
               type is (ik2_t)
                  if ( n1 == 1 ) then
                     do j = 1, m1
                        write(up,'(g0,1x,i0)')p%v(1,j),q%v(1,j)
                     end do
                  else
                     do j = 1, n1
                        write(up,'(g0,1x,i0)')p%v(j,1),q%v(j,1)
                     end do
                  end if
               type is (rk2_t) 
                  if ( n1 == 1 ) then
                     do j = 1, m1
                        write(up,'(2(g0,1x))')p%v(1,j),q%v(1,j)
                     end do
                  else
                     do j = 1, n1
                        write(up,'(2(g0,1x))')p%v(j,1),q%v(j,1)
                     end do
                  end if
            end select    
      end select 
      
      close(up)
!
!-    Add the corresponding gnuplot command in "str":
!      
      if ( len_trim(cmd) == 0 ) then
         cmd = 'p "'//trim(tmpfile)
      else
         cmd = trim(cmd) // ', "' //trim(tmpfile)
      end if
      
      cmd = trim(cmd) // '"'// trim(styl)//' title "'//trim(legend)//'"'
      
   end do
   
   if ( G_flagerr > IZERO ) then
      close(ugpscript, status = 'delete')
      return
   end if

   if ( len_trim(title) /= 0 ) then
      if ( len_trim(font) /= 0 ) then
         write(ugpscript,'(a)')'set title "'//title//'" font "Verdana,'//font//'"'
      else
         write(ugpscript,'(a)')'set title "'//title//'"'
      end if      
   end if
   
   write(ugpscript,'(a)')trim(cmd)   
      
   close(ugpscript)
!
!- Call now gnuplot:
!
   cmdmsg = ''
   buf = gpcmd//' '//trim(gpscript)//'>&'//trim(gpscript)//'out'
   call execute_command_line (buf, exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg)
                               
   if ( exitstat /= 0 .or. cmdstat /= 0 ) then
      str = 'Problem during gnuplot execution. '
      if ( len_trim(cmdmsg) > 0 ) then
         str = str // 'Message returned by execute_command_line:'//NLT//trim(cmdmsg)//NLT// &
            '(check if gnuplot is installed and if its path in ".calmatdefaults" is correct)'
         call G_flagerr%set( stat=G_UERROR, where = HERE, msg = str )
      else
         up = util_GetUnit ()
         if ( up == 0 ) then
            call G_flagerr%set ( stat=G_IERROR, where=HERE,  msg="No free unit available" )
         else
            open(unit = up, file=trim(gpscript)//'out', iostat = err)
            buf = ''
            do
               read(up,'(a)',iostat = err) cmdmsg
               if ( err /= 0 ) exit
               buf = trim(buf) // char(10) // trim(cmdmsg)
            end do
            close(up)
            if ( len_trim(buf) > 0 ) str = str // 'Message returned by gnuplot: '//trim(buf)
            call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = str )
         end if
      end if
   else
      if ( .not. G_dispPrompt ) write(G_uo,'(/,a,/)') 'Figure saved in: '//trim(gpfig)   
   end if
!
!- Clean up (destroy tmp(:), delete gnuplot script and tmp files):
!
   call tmp(1)%Destroy()
   call tmp(2)%Destroy()
   
   if ( G_flagerr > IZERO ) return

   up = util_GetUnit ()
   if ( up == 0 ) then
      call G_flagerr%set ( stat = G_IERROR, where = HERE,  msg = "No free unit available" )
      return
   end if   
   
   do i = 1, nplot
      write(cnum,'(i0)')i
      tmpfile = trim(gpscript)//'.#'//trim(cnum)
      open(unit = up, file = tmpfile)
      close(up, status = 'delete')
   end do
   
   open(unit = up, file = gpscript)
   close(up, status = 'delete')   
      
   open(unit = up, file = trim(gpscript)//'out')
   close(up, status = 'delete')   

   contains

   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      subroutine howManyGraph
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

      type   (str_t), allocatable :: tok0(:,:)
      integer(Ikind)              :: ntok0, i
      
      ntok0 = util_CountTokens (str, delims = ',', BlkToken =.false., opcl = '[]()""', &
                                tokens = tok0, stat = G_flagerr)
      if ( G_flagerr > IZERO ) return
      
      plot = tok0
      
      nplot = 1 
      plot(nplot,1) = tok0(1,1)
      do i = 2, ntok0
         if ( tok0(i,1)%str(1:1) == '"' ) then
            plot(nplot,1) = plot(nplot,1) + ',' + tok0(i,1)
            plot(nplot,2) = ';'
         else if ( plot(nplot,2) == ';' ) then
            nplot = nplot + 1
            plot(nplot,1) = tok0(i,1) 
         else
            plot(nplot,1) = plot(nplot,1) + ',' + tok0(i,1)
            plot(nplot,2) = ';'
         end if
      end do
      
      end subroutine howManyGraph
      
   END SUBROUTINE CalmatCmd_Plot


!=============================================================================================   
   SUBROUTINE CalmatCmd_Plot3d ( expr, flowId, plotout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr 
   integer  (Ikind), optional, intent(in    ) :: flowId
   type     (pk2_t),           intent(   out) :: plotout
!---------------------------------------------------------------------------------------------    
!  Command "plot3d"
!  Very basic way to make plot (gnuplot invocation)
!---------------------------------------------------------------------------------- R.H. 02/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*    ), parameter   :: HERE = 'CalmatCmd_Plot3d'
   character(len=:    ), allocatable :: str, gpscript, gpfig, tmpfile, styl, buf, legend, &
                                        gpcmd, gpterm, title, font, cmd
   type     (str_t    ), allocatable :: plot(:,:), tokens(:,:)
   type     (str_t    )              :: h(21), kwd1, kwd2
   integer  (Ikind    )              :: lp, rp, nplot, ntok, narg, i, j, n1, m1, n2, m2,  &
                                        nl, up, ugpscript, n3, m3
   integer                           :: cmdstat, exitstat, err
   type     (pk2_t    )              :: tmp(3)
   type     (pk2_t    ), allocatable :: valexpr(:)
   character(len=99   )              :: cnum
   character(len=LGSTR)              :: cmdmsg
   logical                           :: surf   
!---------------------------------------------------------------------------------------------  
   integer  (Ikind    ), save        :: ngpfig = 0  
!---------------------------------------------------------------------------------------------    
   
   if ( .not. present(expr) ) then
      kwd1 = str_color('plot3d',G_ColorHelp)
      kwd2 = str_color('surf'  ,G_ColorHelp)
      h( 1) = '<< ' + kwd1 + ' >> plots a set of points in 3D'
      h( 2) = '<< ' + kwd2 + ' >> plots a surface in 3D'
      h( 3) = ' '
      h( 4) = 'Syntax: . '+kwd1+' ( x1, y1, z1 [,line_spec] [, x2, y2, z2 [,line_spec] )'
      h( 5) = '        . '+kwd2+' ( x1, y1, z1 [,line_spec] [, x2, y2, z2 [,line_spec] )'
      h( 6) = ' '
      h( 7) = ' '
      h( 8) = 'Examples:'      
      h( 9) = '  . '+kwd1+'(x1,y1,z1)'
      h(10) = '  . '+kwd1+'(x1,y1,z1,"-r")'
      h(11) = '  . '+kwd1+'(x1,y1,z1,"or" , x2,y2,z2,"xb")'
      h(12) = '  . t =0:%pi/50:10*%pi; x = sin(t) ; y = cos(t) ;' 
      h(13) = '    '+kwd1+'(x,y,t,"--c", title="a helix")'
      h(14) = ' '
      h(15) = '  . '+kwd2+'(x1,y1,z1)'
      h(16) = '  . '+kwd2+'(x1,y1,z1,"-r")'
      h(17) = '  . '+kwd2+'(x1,y1,z1,"or" , x2,y2,z2,"xb")'
      h(18) = '  . A = readmat(file="mysurf",nrow=-1,ncol=-1); '+kwd2+'(A(:,1),A(:,2),A(:,3))'
      h(19) = ' '
      h(20) = '(Note: plots are generated by the command-line program gnuplot. '
      h(21) = '       The figures are persistent but not interactive... need to be improved)'
      plotout = h
      return
   end if
   
   surf = .false.
   if ( index(expr,'surf') /= 0 ) surf = .true.
   
   gpcmd  = G_gnuplot(1)%str
   gpterm = G_gnuplot(2)%str
   font   = G_gnuplot(3)%str
   
   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if ( lp * rp == 0 .or. rp <= lp ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' )
      return
   end if
   
   if ( rp - lp <= 1 ) then
      call G_flagerr%set ( stat = G_WARNING, where = HERE, &
                            msg = 'Nothing to plot (at least one argument expected)' )
      return
   end if
   
   str = trim(adjustl(expr(lp+1:rp-1))) ! remove parentheses   
!
!- Determine the number of graphs (delimited by a ';'):
!
   !nplot = util_CountTokens (str, delims = ';', BlkToken =.false., opcl = '[]()""', &
   !                          tokens = plot, stat = G_flagerr)
   call howManyGraph ( )

   if_error_trace_and_RETURN ( G_flagerr, HERE )
!
!- Open a gnuplot script:
!         
   ugpscript = util_GetUnit ()

   if ( ugpscript == 0 ) then
      call G_flagerr%set ( stat = G_IERROR, where = HERE, msg = "No free unit available" )
      return
   end if
   
   gpscript = '.tmp?_?gpscript?'
   open(unit = ugpscript, file = gpscript, status = 'replace')

   write(ugpscript,'(a)')'reset'

   gpfig = '' 
                               
   if ( .not. G_dispPrompt ) then
!
!-    in batch mode, save the plot in png file (with a name 'name of the script'_fig#.png)
!
      ngpfig = ngpfig + 1
      write(cnum,'(i0)')ngpfig

      err = 0
      if ( present(flowId) ) then
         if ( flowId > 0 .and. flowId <= G_maxnFlow ) then
            gpfig = trim(G_flows(flowId)%fileName)//'_fig'//trim(cnum)//'.png'
         else
            err = 1
         end if
      else 
         err = 1
      end if
      if ( err /= 0 ) then
         call G_flagerr%set ( stat = G_IERROR, where = HERE, &
                               msg = 'Missing flow # or invalid value' )
         close(ugpscript, status = 'delete')
         return
      end if
      
      write(ugpscript,'(a)')"set terminal pngcairo enhanced font '"//trim(font)//"'"
      write(ugpscript,'(a)')"set output '"//trim(gpfig)//"'"
   else
      write(ugpscript,'(a)')'set terminal '//trim(gpterm)//' persist font "'//trim(font)//'"' 
   end if

   write(ugpscript,'(a)')'set grid'      
!
!- Loop over the n graphs:
!   
   title = '' ; cmd = ''
   do i = 1, nplot
      
      ntok = util_CountTokens (plot(i,1)%str, delims = ',', BlkToken =.false.,   &
                               opcl = '[]()""', tokens = tokens, stat = G_flagerr)
      if_error_trace_and_EXIT ( G_flagerr, HERE )
!
!-    For title:
!      
      if ( index(tokens(1,1)%str, 'title') == 1 ) then
         j = index(plot(i,1)%str,'=')
         title = tokens(1,1)%str(j+1:)
         call pk2Interpreter_driver ( expr     = title               , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr            )
         if_error_trace_and_EXIT ( G_flagerr, HERE )
         
         if ( valexpr(1)%typ /= STYP ) then
            call G_flagerr%set( stat=G_UERROR, where=HERE, msg='Title must be a string' )
            exit
         end if
         
         title = ''
         select type ( p => valexpr(1)%m )
            type is (sk2_t) ;  title = p%v(1,1)
         end select

         if ( ntok > 1 ) then 
            font = tokens(2,1)%str
            call pk2Interpreter_driver ( expr     = font                , &
                                         vars     = G_vars(1:G_nvar)    , &
                                         valexpr  = valexpr             , &
                                         flagerr  = G_flagerr            )
            if_error_trace_and_EXIT ( G_flagerr, HERE )
         
            if ( valexpr(1)%typ /= ITYP ) then
               call G_flagerr%set ( stat = G_UERROR, where=HERE, &
                                     msg = 'Font size must be an integer' )
               exit
            end if
         
            font = ''
            select type ( p => valexpr(1)%m )
               type is (ik2_t) ; font = util_intToChar(p%v(1,1))
            end select
         end if
         cycle 
      end if             
!
!-    Determine the line or symbol style:
!         
      if ( index(tokens(ntok,1)%str,'"') /= 0 ) then
         buf = tokens(ntok,1)%str
         narg = ntok - 1         
         nl = len_trim(buf)
         if ( buf(1:1) == '"' .and. buf(nl:nl) == '"' ) then
            buf = buf(2:nl-1) 
            styl = ' w l lw 2 '
               
            if ( index(buf,'-'  ) /= 0) styl = ' w l lw 2 dt 1'
            if ( index(buf,':'  ) /= 0) styl = ' w l lw 2 dt 3'
            if ( index(buf,'--' ) /= 0) styl = ' w l lw 2 dt 2'
            if ( index(buf,'-.' ) /= 0) styl = ' w l lw 2 dt 4'
                
            if ( index(buf,'o' ) /= 0) styl = ' w p pt  7 ps 1 '
            if ( index(buf,'+' ) /= 0) styl = ' w p pt  1 ps 2 '
            if ( index(buf,'x' ) /= 0) styl = ' w p pt  2 ps 1 '
            if ( index(buf,'*' ) /= 0) styl = ' w p pt  3 ps 1 '
            if ( index(buf,'d' ) /= 0) styl = ' w p pt 28 ps 1 '
            if ( index(buf,'s' ) /= 0) styl = ' w p pt 35 ps 1 '
            if ( index(buf,'v' ) /= 0) styl = ' w p pt 11 ps 1 '
            if ( index(buf,'^' ) /= 0) styl = ' w p pt  9 ps 1 '
               
            if ( index(buf,'b' ) /= 0) styl = trim(styl) // ' lc rgb "blue" '
            if ( index(buf,'r' ) /= 0) styl = trim(styl) // ' lc rgb "red" '
            if ( index(buf,'g' ) /= 0) styl = trim(styl) // ' lc rgb "green" '
            if ( index(buf,'y' ) /= 0) styl = trim(styl) // ' lc rgb "yellow" '
            if ( index(buf,'k' ) /= 0) styl = trim(styl) // ' lc rgb "black" '
            if ( index(buf,'m' ) /= 0) styl = trim(styl) // ' lc rgb "dark-magenta" '
            if ( index(buf,'c' ) /= 0) styl = trim(styl) // ' lc rgb "dark-cyan" '
            if ( index(buf,'w' ) /= 0) styl = trim(styl) // ' lc rgb "white" '
         else
            call G_flagerr%set ( stat=G_UERROR, where = HERE, msg = 'invalid line spec.' )
            exit
         end if      
      else
         styl = ' w l lw 2 '
         narg = ntok
      end if

      if ( narg == 3 ) then
!
!-       Case "plot(x,y,z)"
!        ---------------
!
!-       Evaluate x (= tmp(1)):
!      
         call pk2Interpreter_driver ( expr     = tokens(1,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )
         if_error_trace_and_EXIT ( G_flagerr, HERE )

         call pk2_movealloc ( from = valexpr(1), to = tmp(1), stat = G_flagerr )
         if_error_trace_and_EXIT ( G_flagerr, HERE )

         n1 = tmp(1)%nrow ; m1 = tmp(1)%ncol 
         
         if ( n1 /= 1 .and. m1 /= 1 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Variables of "plot3d" or "surf" must be vectors' )
            exit
         end if
         
         if ( tmp(1)%typ > RTYP ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                                 'Variables of "plot3d" or "surf" must be integers or reals' )
            exit
         end if
!
!-       Evaluate y (= tmp(2)):
!
         call pk2Interpreter_driver ( expr     = tokens(2,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )
         if_error_trace_and_EXIT ( G_flagerr, HERE )

         call pk2_movealloc ( from = valexpr(1), to = tmp(2), stat = G_flagerr )
         if_error_trace_and_EXIT ( G_flagerr, HERE )
         
         n2 = tmp(2)%nrow ; m2 = tmp(2)%ncol 
         
         if ( n2 /= 1 .and. m2 /= 1 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Variables of "plot3d" or "surf" must be vectors' )
            exit
         end if
         
         if ( tmp(2)%typ > RTYP ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                               'Variables of "plot3d" or "surf" must be integers or reals' )
            exit
         end if
         
         if ( n1 * m1 /= n2 * m2 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Incompatible size for "plot3d" or "surf"' )
            exit
         end if
!
!-       Evaluate z (= tmp(3)):
!
         call pk2Interpreter_driver ( expr     = tokens(3,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )
         if_error_trace_and_EXIT ( G_flagerr, HERE )

         call pk2_movealloc ( from = valexpr(1), to = tmp(3), stat = G_flagerr )
         if_error_trace_and_EXIT ( G_flagerr, HERE )

         n3 = tmp(3)%nrow ; m3 = tmp(3)%ncol 
         
         if ( n3 /= 1 .and. m3 /= 1 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Variables of "plot3d" or "surf" must be vectors' )
            exit
         end if
         
         if ( tmp(3)%typ > RTYP ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                                'Variables of "plot3d" or "surf" must be integers or reals' )
            exit
         end if
         
         if ( n1 * m1 /= n3 * m3 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                 msg = 'Incompatible size for "plot3d" or "surf"')
            exit
         end if
         
         legend = trim(tokens(3,1)%str)
         
      else
         call G_flagerr%set ( stat = G_UERROR, where = HERE,                      &
                               msg = 'Expression << '//trim(expr)// ' >> ' //     &
                              'has an error (see the help of "plot3d" or "surf")' )
         exit
      end if   
      
      if ( n2 /= n1 ) tmp(2) = RESHAPE (tmp(2), [n1,m1]) ! PK2F_RESHAPENM
      if ( n3 /= n1 ) tmp(3) = RESHAPE (tmp(3), [n1,m1]) ! PK2F_RESHAPENM
      
      if ( surf ) then
         write(ugpscript,'(a,i0,a,i0,a)')'set dgrid3d ',50,',',50,' '!!', 3' !04/24
         write(ugpscript,'(a)')'set contour'
      end if   
!
!-    Write the x, y and z values in a tmp file (#i):
!      
      write(cnum,'(i0)')i
      tmpfile = trim(gpscript)//'.#'//trim(cnum)
      
      up = util_GetUnit ()
      
      if ( up == 0 ) then
         call G_flagerr%set ( stat = G_IERROR, where = HERE, msg = "No free unit available" )
         exit
      end if

      open(unit = up, file = tmpfile, status = 'replace')
      
      select type ( p => tmp(1)%m )
         type is (ik2_t) 
            select type ( q => tmp(2)%m )
               type is (ik2_t)
                  select type ( r => tmp(3)%m )
                     type is (ik2_t)  !i,i,i
                        if ( n1 == 1 ) then
                           do j = 1, m1
                              write(up,'(3(i0,1x))')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(3(i0,1x))')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                     type is (rk2_t) !i,i,r
                        if ( n1 == 1 ) then
                           do j = 1, m1
                              write(up,'(i0,1x,i0,1x,g0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(i0,1x,i0,1x,g0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                  end select  
               type is (rk2_t)
                  select type ( r => tmp(3)%m )
                     type is (ik2_t)     !i,r,i
                        if ( n1 == 1 ) then
                           do j = 1, m1
                              write(up,'(i0,1x,g0,1x,i0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(i0,1x,g0,1x,i0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                     type is (rk2_t) !i,r,r
                        if ( n1 == 1 ) then
                           do j = 1, m1
                              write(up,'(i0,1x,g0,1x,g0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(i0,1x,g0,1x,g0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                  end select  
            end select
               
         type is (rk2_t) 
            select type ( q => tmp(2)%m )
               type is (ik2_t)
                  select type ( r => tmp(3)%m )
                     type is (ik2_t)   !r,i,i
                        if ( n1 == 1 ) then
                           do j = 1, m1
                              write(up,'(g0,1x,i0,1x,i0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(g0,1x,i0,1x,i0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                     type is (rk2_t) !r,i,r
                        if ( n1 == 1 ) then
                           do j = 1, m1
                              write(up,'(g0,1x,i0,1x,g0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(g0,1x,i0,1x,g0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                  end select  
               type is (rk2_t)
                  select type ( r => tmp(3)%m )
                     type is (ik2_t)  !r, r, i
                        if ( n1 == 1 ) then
                           do j = 1, m1
                              write(up,'(g0,1x,g0,1x,i0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(g0,1x,g0,1x,i0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                     type is (rk2_t) !r,r,r
                        if ( n1 == 1 ) then
                           do j = 1, m1
                              write(up,'(g0,1x,g0,1x,g0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(g0,1x,g0,1x,g0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                  end select  
            end select
      end select 
      
      close(up)
!
!-    Add the corresponding gnuplot command in "cmd":
!      
      if ( len_trim(cmd) == 0 ) then
         cmd = 'sp "'//trim(tmpfile)
      else
         cmd = trim(cmd) // ', "' //trim(tmpfile)
      end if
      
      cmd = trim(cmd) // '"'// trim(styl)//' title "'//trim(legend)//'"'
      
   end do
   
   if ( G_flagerr > IZERO ) then
      close(ugpscript, status = 'delete')
      return
   end if

   if ( len_trim(title) /= 0 ) then
      if ( len_trim(font) /= 0 ) then
         write(ugpscript,'(a)')'set title "'//title//'" font "Verdana,'//font//'"'
      else
         write(ugpscript,'(a)')'set title "'//title//'"'
      end if      
   end if
         
   write(ugpscript,'(a)')trim(cmd)   
   close(ugpscript)
!
!- Call now gnuplot:
!
   cmdmsg = ''
   buf = gpcmd//' '//trim(gpscript)//'>&'//trim(gpscript)//'out'
   call execute_command_line (buf, exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg)
                    
   if ( exitstat /= 0 .or. cmdstat /= 0 ) then
      str = 'Problem during gnuplot execution'
      if ( len_trim(cmdmsg) > 0 ) then
         str = str // 'Message returned by execute_command_line:'//NLT//trim(cmdmsg)//NLT// &
            '(check if gnuplot is installed and if its path in ".calmatdefaults" is correct)'
         call G_flagerr%set ( stat=G_UERROR, where = HERE, msg = str // ': '//trim(cmdmsg) )
      else
         up = util_GetUnit ()
         if ( up == 0 ) then
            call G_flagerr%set ( stat = G_IERROR, where = HERE, &
                                  msg = "No free unit available" )
         else
            open(unit = up, file=trim(gpscript)//'out', iostat = err)
            buf = ''
            do
               read(up,'(a)',iostat = err) cmdmsg
               if ( err /= 0 ) exit
               buf = trim(buf) // char(10) // trim(cmdmsg)
            end do
            close(up)
            if ( len_trim(buf) > 0 ) str = str // '. Message returned by gnuplot: '//trim(buf)
            call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = str )
         end if
      end if
   else
      if ( .not. G_dispPrompt ) write(G_uo,'(/,a,/)') 'Figure saved in: '//trim(gpfig)   
   end if      
!
!- Clean up (destroy tmp(:), delete gnuplot script and tmp files):
!
   call tmp(1)%Destroy()
   call tmp(2)%Destroy()
   call tmp(3)%Destroy()
   
   if ( G_flagerr > IZERO ) return

   up = util_GetUnit ()
   if ( up == 0 ) then
      call G_flagerr%set ( stat = G_IERROR, where = HERE, msg = "No free unit available" )
      return
   end if      

   do i = 1, nplot
      write(cnum,'(i0)')i
      tmpfile = trim(gpscript)//'.#'//trim(cnum)
      open(unit = up, file = tmpfile)
      close(up, status = 'delete')
   end do
   
   open(unit = up, file = gpscript)
   close(up, status = 'delete')   

   open(unit = up, file = trim(gpscript)//'out')
   close(up, status = 'delete')   

   contains
   
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      subroutine howManyGraph
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

      type   (str_t), allocatable :: tok0(:,:)
      integer(Ikind)              :: ntok0, i, n
      
      ntok0 = util_CountTokens (str, delims = ',', BlkToken =.false., opcl = '[]()""', &
                                tokens = tok0, stat = G_flagerr)
      if ( G_flagerr > IZERO ) return
      
      plot = tok0
      
      nplot = 1 
      plot(nplot,1) = tok0(1,1) ; n = 1
      do i = 2, ntok0
         if ( tok0(i,1)%str(1:1) == '"' ) then
            plot(nplot,1) = plot(nplot,1) + ',' + tok0(i,1)
            plot(nplot,2) = ';'
         else if ( plot(nplot,2) == ';' ) then
            nplot = nplot + 1
            plot(nplot,1) = tok0(i,1) ; n = 1
         else
            plot(nplot,1) = plot(nplot,1) + ',' + tok0(i,1) ; n = n + 1
            if ( n == 3 ) plot(nplot,2) = ';'
         end if
      end do
      
      end subroutine howManyGraph    
        
   END SUBROUTINE CalmatCmd_Plot3d


!=============================================================================================   
   SUBROUTINE CalmatCmd_Format ( expr, formatout )
!=============================================================================================   
   character(len=*), optional, intent(in    ) :: expr 
   type     (pk2_t), optional, intent(   out) :: formatout
!---------------------------------------------------------------------------------------------    
!  Command "format" (setting the default format for displaying decimal numbers)
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'CalmatCmd_Format'
   integer  (Ikind)              :: lstr, err
   type     (str_t)              :: h(6), kwd
   character(len=:), allocatable :: str
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------       
   logical         , save        :: firstime = .true.
   integer  (Ikind), save        :: digmaxdefault
!---------------------------------------------------------------------------------------------    
   
   if ( firstime ) then
      digmaxdefault = G_digmax ! save the default number
      firstime = .false.
   end if   
   
   if ( .not. present(expr) ) then
      kwd = str_color('format',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >> sets the default format for displaying decimal numbers'
      h(2) = ' '
      h(3) = 'Syntax: '+kwd+'(n), '+kwd+'(), '+kwd
      h(4) = ' '
      h(5) = '. '+kwd+'(n): set the number of significant digits of decimal numbers to n'      
      h(6) = '. '+kwd+'() or '+kwd+': reset the default number of digits (n = '+ &
             util_intToChar(digmaxdefault)+')'  
      formatout = h
      return
   end if

   if ( len_trim(expr) > 6 ) then
      str = trim(adjustl(expr(7:))) ; lstr = len_trim(str)
      
      if ( str(1:1) /= '(' .or. str(lstr:lstr) /= ')' ) then
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = "Expression has an error" )
         return
      end if 
         
      str = trim(adjustl(str(2:lstr-1))) ! remove parentheses
               
      if ( len_trim(str) == 0 ) then
         G_digmax = digmaxdefault
         return
      end if       
   else
      G_digmax = digmaxdefault
      return
   end if
   
   call pk2Interpreter_driver ( expr     = str                 , &
                                vars     = G_vars(1:G_nvar)    , &
                                valexpr  = valexpr             , &
                                flagerr  = G_flagerr           )
 
   if ( G_flagerr < IZERO ) then
      call G_flagerr%display (unit = G_uo)
   else if ( G_flagerr > IZERO ) then
      call G_flagerr%AddTrace(HERE)
      return
   end if

   err = 0
   
   select type ( p => valexpr(1)%m )
      type is (ik2_t)
         if ( p%nrow == 1 .and. p%ncol == 1 ) then
            if ( p%v(1,1) > 0 .and. p%v(1,1) < 90 ) then
               G_digmax = p%v(1,1) ; return
            else
               err = -1
            end if 
         else
            err = 1
        end if
      type is (rk2_t)
         if ( p%nrow == 1 .and. p%ncol == 1 ) then
            if ( util_IsIntg(p%v(1,1)) ) then
               if ( p%v(1,1) > 0 .and. p%v(1,1) < 90 ) then
                  G_digmax = int(p%v(1,1),kind=Ikind) ; return
               else
                  err = -1
               end if 
            else
               err = 1
            end if
         else
            err = 1
        end if        
      class default
         err = 1
   end select             

   if ( err ==-1 ) then
      call G_flagerr%set ( stat = G_WARNING, where = HERE, msg = &
                          'Number of digits (n) must be between 1 and 89 in << format(n) >>' )    
   else if ( err == 1 ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                          'An expected integer (between 1 and 89) for n in << format(n) >>' )    
      return  
   end if
      
   END SUBROUTINE CalmatCmd_Format
   

!=============================================================================================   
   SUBROUTINE CalmatCmd_Disp ( expr, dispout )
!=============================================================================================   
   character(len=*), optional, intent(in    ) :: expr 
   type     (pk2_t), optional, intent(   out) :: dispout
!---------------------------------------------------------------------------------------------    
!  Command "disp" (displaying on the output unit G_uo any data)
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'CalmatCmd_Disp'
   integer  (Ikind)              :: lp, rp, ntok, i, j
   type     (str_t), allocatable :: tokens(:,:)
   type     (str_t)              :: h(7), kwd
   character(len=:), allocatable :: str
   type     (pk2_t)              :: tmp(2)
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------    
   
   if ( .not. present(expr) ) then
      kwd = str_color('disp',G_ColorHelp)
      h(1) = '<< ' + kwd + ' >>  displays a value '
      h(2) = ' '
      h(3) = 'Syntax: '+kwd+' ( a [,message] )'
      h(4) = ' '
      h(5) = 'Examples:'      
      h(6) = '  . '+kwd+'(x1)'
      h(7) = '  . '+kwd+'(x1,"x1 is: ")'
      dispout = h
      return
   end if
   
   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if ( lp * rp == 0 .or. rp <= lp ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' )
      return
   end if
   
   str = trim(adjustl(expr(lp+1:rp-1))) ! remove parentheses   

   ntok = util_CountTokens (str, delims = ',', BlkToken =.false., opcl = '[]()""', &
                            tokens = tokens, stat = G_flagerr)
   if_error_trace_and_RETURN ( G_flagerr, HERE )

   if ( ntok > 2 ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE,                  &
                            msg = 'Expression << '//trim(expr)// ' >> ' // &
                                  'has an error (see the help of "disp")'  )
      return
   end if
                         
   do i = 1, ntok

      call pk2Interpreter_driver ( expr     = tokens(i,1)%str     , &
                                   vars     = G_vars(1:G_nvar)    , &
                                   valexpr  = valexpr             , &
                                   flagerr  = G_flagerr           )
 
      if ( G_flagerr < IZERO ) then
         call G_flagerr%display (unit = G_uo)
      else if (G_flagerr > IZERO) then
         call G_flagerr%AddTrace(HERE) ; return
      end if
      
      call pk2_moveAlloc ( from = valexpr(1), to = tmp(i), stat = G_flagerr )
      if_error_trace_and_RETURN ( G_flagerr, HERE )
      
   end do
   
   if ( tmp(1)%typ == STYP ) then
      select type ( p => tmp(1)%m )
         type is (sk2_t)
            do j = 1, p%ncol
               do i = 1, p%nrow
                  if ( allocated(p%v(i,j)%str) ) &
                     p%v(i,j)%str = util_ReplaceSubstring1 ( p%v(i,j)%str, '\n', char(10) )
               end do
            end do
      end select   
   end if
   
   if ( ntok == 2 ) then
      
      if ( tmp(2)%typ /= STYP ) then
         call G_flagerr%set ( stat = G_UERROR, where = HERE,                  &
                               msg = 'Expression << '//trim(expr)// ' >> ' // &
                                     'has an error (see the help of "disp")'  )
         return
      end if
      
      select type ( p => tmp(2)%m )
         type is (sk2_t)
            if (allocated(p%v(1,1)%str)) &
               str = util_ReplaceSubstring1 ( p%v(1,1)%str, '\n', char(10) )
      end select
      call CalmatUtil_display ( a = tmp(1), msg = str, symb = '', whole =.true. )
   else
      call CalmatUtil_display ( a = tmp(1), msg = '' , symb = '', whole = .true. )
   end if
      
   call tmp(1)%destroy() ; call tmp(2)%destroy()
      
   END SUBROUTINE CalmatCmd_Disp


!=============================================================================================   
   SUBROUTINE CalmatCmd_Quit ( quitout )
!=============================================================================================   
   type(pk2_t), intent(out) :: quitout
!---------------------------------------------------------------------------------------------    
!  Returns the help of the command "quit" or "exit" (quit calmat)
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(3), kwd1, kwd2
!---------------------------------------------------------------------------------------------    

   kwd1 = str_color('quit',G_ColorHelp)
   kwd2 = str_color('exit',G_ColorHelp)

   h(1) = '<< ' + kwd1 + ' >> or ' + '<< ' + kwd2 + ' >> quits the program '
   h(2) = ' '
   h(3) = 'Syntax: '+kwd1 + ', '+kwd2
   quitout = h
   
   END SUBROUTINE CalmatCmd_Quit   


!=============================================================================================   
   SUBROUTINE CalmatCmd_For ( forout )
!=============================================================================================   
   type(pk2_t), intent(out) :: forout
!---------------------------------------------------------------------------------------------    
!  Returns the help of the keywords "for/endfor" (for loop)
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(15), kwd1, kwd2
!---------------------------------------------------------------------------------------------    

   kwd1 = str_color('for',G_ColorHelp)
   kwd2 = str_color('endfor',G_ColorHelp)

   h( 1) = '<< ' + kwd1 + ' >>, '+'<< ' + kwd2 + ' >> keywords used for a for loop construct'
   h( 2) = ' '
   h( 3) = 'Syntax: ' + kwd1 + ' counter = start:end'
   h( 4) = '            ....'
   h( 5) = '        ' + kwd2
   h( 6) = ' '      
   h( 7) = '        ' + kwd1 + ' counter = start:step:end'
   h( 8) = '            ....'
   h( 9) = '        ' + kwd2
   h(10) = ' '   
   h(11) = 'Example:'      
   h(12) = '  x = 0;'
   h(13) = '  '+kwd1+' i = 1:2:100'
   h(14) = '      x = x + i;'
   h(15) = '  '+kwd2   
   forout = h
   
   END SUBROUTINE CalmatCmd_For
   

!=============================================================================================   
   SUBROUTINE CalmatCmd_If ( ifout )
!=============================================================================================   
   type(pk2_t), intent(out) :: ifout
!---------------------------------------------------------------------------------------------    
!  Returns the help of the keywords "if-then/elseif-then/else/endif"
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(23), kwd1, kwd2, kwd3, kwd4, kwd5
!---------------------------------------------------------------------------------------------    

   kwd1 = str_color('if',G_ColorHelp)
   kwd2 = str_color('then',G_ColorHelp)
   kwd3 = str_color('elseif',G_ColorHelp)
   kwd4 = str_color('else',G_ColorHelp)
   kwd5 = str_color('endif',G_ColorHelp)

   h( 1) = '<< '+kwd1+', '+kwd2+', '+kwd3+', '+kwd4+', '+kwd5+' >> keywords used for '
   h( 2) = 'if construct (conditional execution)'
   h( 3) = ' '
   h( 4) = 'Syntax: '+kwd1+' condition '+kwd2
   h( 5) = '            ....'
   h( 6) = '        '+kwd5
   h( 7) = ' '      
   h( 8) = '        '+kwd1+' condition '+kwd2
   h( 9) = '            ....'
   h(10) = '        '+kwd4
   h(11) = '            ....'
   h(12) = '        '+kwd5
   h(13) = ' '         
   h(14) = '        '+kwd1+' condition1 '+kwd2
   h(15) = '            ....'
   h(16) = '        '+kwd3+' condition2 '+kwd2
   h(17) = '            ....'
   h(18) = '        ....'
   h(19) = '        '+kwd3+' conditionN '+kwd2
   h(20) = '            ....'
   h(21) = '        '+kwd4
   h(22) = '            ....'
   h(23) = '        '+kwd5
   ifout = h
   
   END SUBROUTINE CalmatCmd_If


!=============================================================================================   
   SUBROUTINE CalmatCmd_Break ( breakout )
!=============================================================================================   
   type(pk2_t), intent(out) :: breakout
!---------------------------------------------------------------------------------------------    
!  Returns the help of the command "break" (exit from a for loop)
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(13), kwd
!---------------------------------------------------------------------------------------------    

   kwd = str_color('break',G_ColorHelp)

   h( 1) = '<< ' + kwd + ' >> keyword used to exit from a for loop '
   h( 2) = ' '
   h( 3) = 'Syntax: '+kwd
   h( 4) = ' '   
   h( 5) = 'Example:'      
   h( 6) = '  x = 0;'
   h( 7) = '  '+str_color('for',G_ColorHelp)+' i = 1:100'
   h( 8) = '      x = x + 1.5*'+str_color('rand',G_ColorHelp)+'();'
   h( 9) = '      '+str_color('if',G_ColorHelp)+' x > 3 '+str_color('then',G_ColorHelp)
   h(10) = '         '+str_color('disp',G_ColorHelp)+'(i)'
   h(11) = '         '+kwd
   h(12) = '      '+str_color('endif',G_ColorHelp)
   h(13) = '  '+str_color('endfor',G_ColorHelp)   
   breakout = h
   
   END SUBROUTINE CalmatCmd_Break
   
   
!=============================================================================================   
   SUBROUTINE CalmatCmd_Welcome ( welcout )
!============================================================================================= 
   type(pk2_t), optional, intent(out) :: welcout  
!---------------------------------------------------------------------------------------------    
! 
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(3), kwd 
!---------------------------------------------------------------------------------------------    

   if ( present(welcout) ) then
      kwd = str_color('welcome',G_ColorHelp)

      h(1) = '<< ' + kwd + ' >> prints the welcome message '
      h(2) = ' '
      h(3) = 'Syntax: '+kwd

      welcout = h
   else
      call CalmatUtil_welcome ( )
   end if
   
   END SUBROUTINE CalmatCmd_Welcome
      

!=============================================================================================   
   SUBROUTINE CalmatCmd_Eval ( expr, evalout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(   out) :: evalout
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'CalmatCmd_Eval'
   type     (str_t)              :: h(6), kwd   
   integer  (Ikind)              :: lp, rp
   character(len=:), allocatable :: str   
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
       kwd = str_color('eval',G_ColorHelp)
       h(1) = '<< ' + kwd + ' >> Evaluates a string containing a single expression'
       h(2) = ' '
       h(3) = 'Syntax: '+kwd+'(expr)'
       h(4) = ' '
       h(5) = 'Examples:'      
       h(6) = '  . '+kwd+'("x+y")'
       evalout = h
      return
   end if

   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if ( lp * rp == 0 .or. rp <= lp ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' )
      return
   end if
   
   str = trim(adjustl(expr(lp+1:rp-1))) ! remove parentheses
!
!- First evaluate str (the result is a string):
!
   call pk2Interpreter_driver ( expr     = str                 , &
                                vars     = G_vars(1:G_nvar)    , &
                                valexpr  = valexpr             , &
                                flagerr  = G_flagerr           )
   if_error_trace_and_RETURN ( G_flagerr, HERE )
   
   if ( valexpr(1)%typ /= STYP ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                            msg = 'Argument of << eval >> must be of string type' )
      return
   end if      
   
   call pk2_movealloc ( from = valexpr(1), to = str )
!
!- Now evaluate the expression contained in str:
!
   call pk2Interpreter_driver ( expr     = str                 , &
                                vars     = G_vars(1:G_nvar)    , &
                                valexpr  = valexpr             , &
                                flagerr  = G_flagerr           )
   if_error_trace_and_RETURN ( G_flagerr, HERE )
      
   call pk2_movealloc ( from = valexpr(1), to = evalout, movename = .false. )   
   
   END SUBROUTINE CalmatCmd_Eval


!=============================================================================================   
   SUBROUTINE CalmatCmd_Exists ( expr, res )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t),           intent(in out) :: res
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'CalmatCmd_Exists'
   type     (str_t)              :: h(7), kwd   
   integer  (Ikind)              :: lp, rp, i, j, k, nrow, ncol
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
       kwd = str_color('exists',G_ColorHelp)
       h(1) = '<< ' + kwd + ' >> Checks existence of variable (returns F or T)'
       h(2) = ' '
       h(3) = 'Syntax: '+kwd+'(name)  or '+kwd+'([name1,name2,...])'
       h(4) = ' '
       h(5) = 'Examples:'      
       h(6) = '  . '+kwd+'("x")'
       h(7) = '  . '+kwd+'(["x", "y"; "u", "v"])'
       res = h
      return
   end if

   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if ( lp * rp == 0 .or. rp <= lp ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' )
      return
   end if   
!
!- First evaluate str (the result is a string):
!
   call pk2Interpreter_driver ( expr     = trim(adjustl(expr(lp+1:rp-1))), &
                                vars     = G_vars(1:G_nvar)              , &
                                valexpr  = valexpr                       , &
                                flagerr  = G_flagerr                       )
   if_error_trace_and_RETURN ( G_flagerr, HERE )
      
   if ( valexpr(1)%typ /= STYP ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                            msg = 'Argument of << exists >> must be of string type' )
      return
   end if
   
   nrow = valexpr(1)%nrow ; ncol = valexpr(1)%ncol
   
   if ( res%typ /= LTYP .or. res%nrow /= nrow .or. res%ncol /= ncol ) then
      res = pk2_t ( typ = LTYP, shape = [nrow,ncol], stat = G_flagerr )
      if_error_trace_and_RETURN ( G_flagerr, HERE )
   end if
   
   select type ( p => valexpr(1)%m )
      type is ( sk2_t )
         select type ( q => res%m )
            type is ( lk2_t )
               do j = 1, ncol
                  do i = 1, nrow
                     q%v(i,j) = .false.
                     do k = 1, G_nvar
                        if ( allocated(G_vars(k)%name) ) then
                           if ( G_vars(k)%name == p%v(i,j)%str ) then
                              q%v(i,j) = .true.
                              exit
                           end if
                        end if
                     end do
                  end do
               end do
         end select
   end select   
      
   END SUBROUTINE CalmatCmd_Exists
      
   
!=============================================================================================   
   SUBROUTINE CalmatCmd_Color ( colout )
!=============================================================================================   
   type(pk2_t), intent(out) :: colout
!---------------------------------------------------------------------------------------------    
!  Help for color codes
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(15), k, r, b, y, g, c, m
!---------------------------------------------------------------------------------------------    

   k = str_color('blac','k') + str_color('k','kU')
   r = str_color('r','rU') + str_color('ed','r') 
   b = str_color('b','bU') + str_color('lue','b') 
   y = str_color('y','yU') + str_color('ellow','y') 
   g = str_color('g','gU') + str_color('reen','g') 
   c = str_color('c','cU') + str_color('yan','c') 
   m = str_color('m','mU') + str_color('agenta','m') 
   
   h( 1) = 'The following convention is used for color codes:'
   h( 2) = '   '   
   h( 3) = ' . A color code is a string beginning with one of the characters:'
   h( 4) = '            "k",  "r",  "b",  "y",  "g",  "c",  "m"' 
   h( 5) = '   for '+k+', '+r+', '+b+', '+y+', '+g+', '+c+', '+m + NL
   h( 6) = ' . Texture and color intensity may be added by appending the following'
   h( 7) = '   characters:'
   h( 8) = '   - "b" for bold'
   h( 9) = '   - "h" for high intensity'
   h(10) = '   - "U" for underlined text'
   h(11) = '   - "B" for background color' // NL
   h(12) = ' . All other characters will be ignored' // NL
   h(13) = 'Examples:'
   r = str_color('(bold text in intense red)','rhb')
   h(14) = ' . "rhb", "rbh", "r_bh", ... '+r
   r = str_color('(bold text and background in intense red)','rhbB')
   h(15) = ' . "rhbB", "rB_bh", ...      '+r
   
   colout = h

   END SUBROUTINE CalmatCmd_Color
   
   
!=============================================================================================   
   SUBROUTINE CalmatCmd_Set ( expr, setout )
!=============================================================================================   
   character(len=*), optional, intent(in    ) :: expr 
   type     (pk2_t), optional, intent(   out) :: setout
!---------------------------------------------------------------------------------------------    
!  Sets a default variable
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'CalmatCmd_Set'
   type     (str_t)              :: h(32), kwd
   integer  (Ikind)              :: lp, rp, p, varId
   character(len=:), allocatable :: arg, val
   type     (pk2_t), allocatable :: outval (:)  
   type     (str_t), allocatable :: s2(:,:)
   integer  (Ikind), allocatable :: i1(:), i2(:,:)   
!---------------------------------------------------------------------------------------------    

   if ( .not. present(expr) ) then
      kwd = str_color('set',G_ColorHelp)

      h( 1) = '<< ' + kwd + ' >> command used to change a default property or to define'
      h( 2) = '          a new protected variable'
      h( 3) = ' '
      h( 4) = 'Syntax: . '+kwd+'(property_name , property_value)'
      h( 5) = '        . '+kwd+'(%variable_name, variable_value)'
      h( 6) = ' '   
      h( 7) = 'Note: you can also define your prefered settings in a more convenient  '   
      h( 8) = '      way in the defaut file '//G_FDEF//'. '  
      h( 9) = '      To reset this file, simply delete it (it will be re-written at the'
      h(10) = '      next use of calmat'
      h(11) = ' '   
      h(12) = 'Examples:'      
      h(13) = ' . set( prompt.symbol, "myPrompt->" ) // changes the prompt'
      h(14) = ' . set( %gold, (sqrt(5)+1)/2 ) // set a user-protected variable (cannot be'
      h(15) = '                               // overwritten by an assignment, only editable'
      h(16) = '                               // by the command "set")'
      h(17) = ' '
      h(18) = 'The current list of editable properties is:'
      h(19) = ' . prompt           : prompt symbol (e.g. "(calmat ->)")'
      h(20) = ' . display.maxDigits: number of digits when printing result on screen (e.g. 8)'
      h(21) = ' . display.portion  : max size when writing an array on screen (e.g. [10,10])'
      h(22) = ' . color.prompt     : color of the propmt (e.g. "b_hb")'
      h(23) = ' . color.help       : color used for keyword when printing help (e.g. "c_b")'
      h(24) = ' . color.error      : color used for reporting an error (e.g. "r_b")'
      h(25) = ' . color.warning    : color used for reporting a warning (e.g. "g_b")'
      h(26) = ' . color.title      : color used for title when printing help (e.g. "c_b")'
      h(27) = ' . message.welcome  : print the banner and the welcome message ("yes"/"no")'
      h(28) = ' . message.farewell : farewell message to print after a Ctrl-C (e.g. "bye")'
      h(29) = ' . gnuplot.path     : gnuplot path (e.g. "/usr/local/bin/")'
      h(30) = ' . gnuplot.terminal : gnuplot terminal (e.g. "qt")'
      h(31) = ' . gnuplot.font     : gnuplot font and font size (e.g. "Verdana, 12")'
      h(32) = ' . error.traceback  : report the full traceback of an error ("yes"/"no")'
       
      setout = h
      return
   end if

   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if ( lp * rp == 0 .or. rp <= lp ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Unbalanced parentheses' )
      return
   end if
   
   if ( rp - lp <= 1 ) return
   
   arg = trim( adjustl( expr(lp+1:rp-1) ) ) ! remove parentheses
   p = index(arg,',')
   if ( p == 0 ) then
      call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'A comma is missing' )
      return
   end if
   
   val = arg(p+1:) ; arg = arg(:p-1) 

   call pk2Interpreter_driver ( expr     = val             , &
                                vars     = G_vars(1:G_nvar), &
                                valexpr  = outval          , &
                                flagerr  = G_flagerr         )
   if_error_trace_and_RETURN ( G_flagerr, HERE )
   
   if ( .not. allocated(outval) ) return
!
!- Set a user protected variable:
!
   if ( arg(1:1) == '%' ) then
!
!-    See if a variable of this name already exists:
!
      call CalmatUtil_FindVar ( varName = arg, varId = varId )
   
      if ( varId == 0 ) then
!
!-       It is a new variable, check its name, get a varId and store its name:
!      
         call CalmatUtil_CheckName ( arg, G_flagerr ) 
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         
         varId = CalmatUtil_GetAFreeNumVar ( G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         
         if ( varId > G_nVar ) G_nVar = varId 
         G_varNames(varId)  = arg 
         G_vars(varId)%name = arg
      else
!
!-       Error: it is an internally protected variable:
!      
         if ( G_vars(varId)%status == G_PROTECTED ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, msg =                       &
                                'The value of a protected variable can not be changed ' //  &
                                '(<< ' //  trim(arg) // ' >>)'                              )
         end if
      end if 
!
!-    Store its value and set its status as "user protected" (can be modified only by the 
!     set command): 
!     
      call pk2_movealloc ( from = outval(1), to = G_vars(varId), movename = .false. )
      G_vars(varId)%status = G_USERPROTECTED
   
      return
   
   end if
!
!- Change a predefined property:
!
   select case ( arg )
      
      case ( "prompt" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' ) 
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_prompt = str_color ( s2(1,1)%str, G_prompt%getMyColor() )

      case ( "display.maxDigits" ) 
         if ( outval(1)%typ /= ITYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='Integer value expected' )
            return
         end if
         call outval(1)%getMat ( i2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_digmax = i2(1,1)
         
      case ( "display.portion" ) 
         if ( outval(1)%typ /= ITYP .or. outval(1)%nrow * outval(1)%ncol < 2 ) then
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'An array of two integers is expected' )
            return
         end if
         call outval(1)%GetMatPacked ( i1, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_maxdisp = i1
         
      case ( "message.welcome" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         if ( s2(1,1) == 'yes' ) then
            G_welcome = .true.
         else
            G_welcome = .false.
         end if
         
      case ( "message.farewell" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_farewell = s2(1,1)%str
         call signalHandler_SignalCatch &
                      ( unit = G_uo, title = '--> Calmat Info:', farewellMsg = G_farewell )
                      
      case ( "error.traceback" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         if ( s2(1,1) == 'yes' ) then
            G_traceback = .true.
         else
            G_traceback = .false.
         end if    
             
      case ( "gnuplot.path" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_gnuplot(1) = s2(1,1)%str
         
      case ( "gnuplot.terminal" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_gnuplot(2) = s2(1,1)%str
         
      case ( "gnuplot.font" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_gnuplot(3) = s2(1,1)%str
         
      case ( "color.prompt" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_prompt = str_color ( G_prompt, s2(1,1)%str )
               
      case ( "color.error" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         err_colorError = ansiColor_getAnsiCode ( s2(1,1)%str )

      case ( "color.warning" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         err_colorWarning = ansiColor_getAnsiCode ( s2(1,1)%str ) 

      case ( "color.normal" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         err_colorNormal = ansiColor_getAnsiCode ( s2(1,1)%str ) 
         
      case ( "color.help" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_colorHelp = ansiColor_getAnsiCode ( s2(1,1)%str )  
         pk2f_helpcol = G_ColorHelp
         
      case ( "color.title" ) 
         if ( outval(1)%typ /= STYP ) then
            call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='String value expected' )
            return
         end if
         call outval(1)%getMat ( s2, G_flagerr )
         if_error_trace_and_RETURN ( G_flagerr, HERE )
         G_colorTitle = ansiColor_getAnsiCode ( s2(1,1)%str ) 
         
      case default
         call G_flagerr%set (stat = G_UERROR, where = HERE, &
                              msg = 'Unknown property name (<< '//arg//' >>)' )
         
   end select   

   END SUBROUTINE CalmatCmd_Set 

         
END MODULE CalmatCmd_m
