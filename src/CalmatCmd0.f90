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

MODULE CalmatCmd

   use CalmatGlobal
   use CalmatUtil
   
   implicit none
   
   integer(Iprec), parameter :: NCMD = 27, &      ! number of available commands
                                MAXCMP = 10       ! max. number of components (for an object)
   
   type   (str_t)            :: CmdList(NCMD), &      ! Command names
                                CmdDesc(NCMD), &      ! Their short description
                                CmdCmps(MAXCMP,NCMD)  ! Component names (for an object result)
   integer(Iprec)            :: CmdType(NCMD)         ! The type of the result (var or object)

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
!  . k = 0: the output must be assigned to a regular variable (or a set of variables)
!  . k > 0: the output must be assigned to an object and k gives its number of components
!
!  Note: some of these commands are just referenced for the help
!---------------------------------------------------------------------R.H. 11/18, 01/19, 03/20

   CmdList( 1) = 'help'  ; CmdDesc( 1) = 'returns help'
   CmdType( 1) = 0
   
   CmdList( 2) = 'clear' ; CmdDesc( 2) = 'deletes variables'
   CmdType( 2) =-1
   
   CmdList( 3) = 'list'  ; CmdDesc( 3) = 'lists variables, functions/commands, operators'
   CmdType( 3) = 0
   
   CmdList( 4) = 'disp'  ; CmdDesc( 4) = 'displays a variable'
   CmdType( 4) =-1
   
   CmdList( 5) = 'exec'  ; CmdDesc( 5) = 'executes a script'
   CmdType( 5) =-1
   
   CmdList( 6) = 'pwd'   ; CmdDesc( 6) = 'returns the current directory name (linux command) ' ! !à revoir pour pouvoir faire pwd + 'qqchose'
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
   CmdType(14) =-1
   
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
   
   END SUBROUTINE CalmatCmd_CmdList
   

!=============================================================================================
   SUBROUTINE CalmatCmd_FindCmd ( str, IdCmd )
!=============================================================================================
   character(len=*),              intent(in    ) :: str
   integer  (Iprec),              intent(   out) :: IdCmd
!--------------------------------------------------------------------------------------------- 
!  See if string "str" starts with the name of a command (in the list "CmdList"). 
!  If so, returns its #. Otherwise returns IdCmd = 0.
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------
   integer(Iprec) :: i, n, posp, posb
!--------------------------------------------------------------------------------------------- 

   IdCmd = 0
   
   posp = index(str,'(') ; posb = index(str,' ')
   
   if (posb /= 0 .and. posp /= 0) then
      n = min(posb,posp) - 1
   elseif (posb /= 0) then
      n = posb - 1
   elseif (posp /= 0) then
      n = posp - 1
   else
      n = -1
   end if
   
   if (n == -1) n = len_trim(str)

   do i = 1, size(CmdList)
      if (str(1:n) == CmdList(i)%str) then
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
   integer  (Iprec)            :: IdCmd, ncmp, i
!--------------------------------------------------------------------------------------------- 
      
   call CalmatCmd_FindCmd ( instr%rhs, IdCmd )

   if (IdCmd == 0) then
      call pk2Interpreter_Tokenizor ( expr     = instr%rhs           , & ! in
                                      varNames = G_varNames(1:G_nvar), & ! in
                                      !vars     = G_vars(1:G_nvar)    , & ! in
                                      handle    = instr%rhsHdl        , & ! out
                                      flagerr =  G_flagerr           ) ! out
      instr%rhsNcmp = 0
      instr%assignment = .true.                                      
   else

      instr%IdCmd = IdCmd
      
      call instr%rhsHdl%alloc (1_iprec, G_flagerr)
      
      if (G_flagerr%code > 0) return
      
      instr%rhsHdl%noperation    = 1       
      instr%rhsHdl%operations(1) = IdCmd
      instr%rhsHdl%initialized   = .false.
      
      if (CmdType(IdCmd) > 0) then
         ncmp = CmdType(IdCmd)
         if (.not. allocated(instr%rhsCmps)) then
            allocate(instr%rhsCmps(ncmp))
         else if (ncmp > size(instr%rhsCmps)) then
            deallocate(instr%rhsCmps)
            allocate(instr%rhsCmps(ncmp))
         end if
         do i = 1, ncmp
            instr%rhsCmps(i)%str = (CmdCmps(i,IdCmd)%str)
         end do
         instr%rhsNcmp = ncmp
      else
         instr%rhsNcmp = 0
      end if
      
      instr%assignment = (CmdType(IdCmd) >= 0)
      
   end if   

   END SUBROUTINE CalmatCmd_Tokenizor 
   

!=============================================================================================
   SUBROUTINE CalmatCmd_Evaluator ( instr )
!=============================================================================================
   type(ins_t), intent(in out) :: instr
!--------------------------------------------------------------------------------------------- 
!-----------------------------------------------------------------------------------R.H. 01/20

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatCmd_Evaluator'
   type     (pk2_t), allocatable :: outval(:)
   character(len=:), allocatable :: msg
   integer  (Iprec)              :: nlhs
   logical                       :: assignable
!--------------------------------------------------------------------------------------------- 

   call ieee_set_flag (ieee_all, flag_value = .false.)

   nlhs = max(1_Iprec, instr%nlhs)
      
   if (instr%rhsHdl%initialized) then
  ! if (instr%idCmd == 0) then
      
      call pk2Interpreter_Evaluator ( handle  = instr%rhsHdl    , &
                                      vars    = G_vars(1:G_nvar), &
                                      valExpr = outval          , &
                                      flagerr = G_flagerr       , &
                                      nanswer = nlhs            )                                  
      assignable = .true.
   else
      call CalmatCmd_Driver ( instr, nlhs, outval, assignable, msg )
   end if
   
   if (G_flagerr%code > 0 .or. G_task == G_STOP) return
!
!- Assign the result and print it if requested:
!   
   call CalmatUtil_AssignAndPrint ( instr, assignable, nlhs, msg, outval, G_flagerr )
       
   END SUBROUTINE CalmatCmd_Evaluator

      
!=============================================================================================
   SUBROUTINE CalmatCmd_Driver ( instr, nlhs, outval, assignable, msg )
!=============================================================================================
   type     (ins_t),              intent(in out) :: instr
   integer  (Iprec),              intent(in    ) :: nlhs
   type     (pk2_t), allocatable, intent(   out) :: outval(:)
   logical         ,              intent(   out) :: assignable
   character(len=:), allocatable, intent(   out) :: msg   
!--------------------------------------------------------------------------------------------- 
!  Calls the suited command for the instructions # ins of the stack
!---------------------------------------------------------------------R.H. 11/18, 01/19, 01/20

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = '(in CalmatCmd_Driver)'
   integer  (Iprec)              :: IdCmd
!--------------------------------------------------------------------------------------------- 

   allocate(outval(nlhs))

   IdCmd = instr%rhsHdl%operations(1)
   !IdCmd = instr%IdCmd
   
   assignable = .true.
   
   select case (IdCmd)
   
   case(1) ! help
      call CalmatCmd_Help (expr = instr%rhs, helpout = outval(1), msg = msg)
      if (instr%verb /= 0) instr%verb = 2 ! force the whole result to be printed
      
   case(2) ! clear
      call CalmatCmd_Clear (expr = instr%rhs, clearout = outval(1), gins = instr%globalInsNum)
      instr%verb = 0 ! nothing to display
      assignable = .false.
      
   case(3) ! list
      call CalmatCmd_List (expr = instr%rhs, listout = outval(1), msg = msg)    
      if (instr%verb /= 0) instr%verb = 2 ! force the whole result to be printed

   case(4) ! disp
      call CalmatCmd_Disp (expr = instr%rhs, dispout = outval(1))
      instr%verb = 0 ! nothing to display ("expr" is already displayed by CalmatCmd_Disp)
      assignable = .false.

   case(5) ! exec
      call CalmatCmd_Exec (expr = instr%rhs, execout = outval(1))
      instr%verb = 0 ! nothing to display
      assignable = .false.
      
   case(6) ! pwd
      call CalmatCmd_pwd (dum = instr%rhs, pwdout = outval(1))

   case(7) ! cd
      call CalmatCmd_chdir (expr =  instr%rhs, cdout = outval(1))
      instr%verb = 0 ! nothing to display
      assignable = .false.

   case(8) ! ls
      call CalmatCmd_ls (expr = instr%rhs, lsout = outval(1))
      if (instr%verb /= 0) instr%verb = 2 ! force the whole result to be printed

   case(9) ! plot
      call CalmatCmd_Plot (expr = instr%rhs, flowId = instr%flowId, plotout = outval(1))       
      instr%verb = 0 ! nothing to display
      assignable = .false.

   case(10) ! plot3d
      call CalmatCmd_Plot3d (expr = instr%rhs, flowId = instr%flowId, plotout = outval(1))       
      instr%verb = 0 ! nothing to display
      assignable = .false.

   case(11) ! surf
      call CalmatCmd_Plot3d (expr = instr%rhs, flowId = instr%flowId, plotout = outval(1))       
      instr%verb = 0 ! nothing to display
      assignable = .false.
      
   case(12) ! format
      call CalmatCmd_Format (expr = instr%rhs, formatout = outval(1))
      instr%verb = 0 ! nothing to display
      assignable = .false.

   case(13) ! quit (or exit)
      G_task = G_STOP

   case(14) ! break
      CalmatCmd_forloopbreak = .true.
      instr%verb = 0 ! nothing to display
      assignable = .false.

   case(21) ! mesh
      deallocate(outval)
      call CalmatCmd_Mesh (expr = instr%rhs, meshout = outval)

   case(22) ! more
      call CalmatCmd_more (expr =  instr%rhs, moreout = outval(1))
      instr%verb = 0 
      assignable = .false.

   case(23) ! tail
      call CalmatCmd_tail (expr =  instr%rhs, tailout = outval(1))
      instr%verb = 0 
      assignable = .false.

   case(24) ! linux
      call CalmatCmd_linux (expr =  instr%rhs, linuxout = outval(1))
      instr%verb = 0 
      assignable = .false.

   case(25) ! cp
      call CalmatCmd_cp (expr =  instr%rhs, cpout = outval(1))
      instr%verb = 0 
      assignable = .false.

   case(26) ! mv
      call CalmatCmd_mv (expr =  instr%rhs, mvout = outval(1))
      instr%verb = 0 
      assignable = .false.

   case(27) ! rm
      call CalmatCmd_rm (expr =  instr%rhs, rmout = outval(1))
      instr%verb = 0 
      assignable = .false.
         
   case default
      assignable = .false.
      G_flagerr = err_t (stat = UERROR, msg = HERE//' Unknown command')

   end select
                                      
   END SUBROUTINE CalmatCmd_Driver
         
   
!=============================================================================================
   SUBROUTINE CalmatCmd_Help (expr, helpout, msg)
!=============================================================================================
   character(len=*),              optional, intent(in    ) :: expr
   type     (pk2_t),                        intent(   out) :: helpout
   character(len=:), allocatable, optional, intent(   out) :: msg   
!--------------------------------------------------------------------------------------------- 
!  Command "help"
!
!  Returns help
!
!  Input:
!  . expr: (optional) a string like help('name') or help("name") where name is the name of
!          a command or a function.
!          If expr is not present, returns the help of the "help" command itself
!  Ouput:
!  . helpout: the corresponding help stored as a pk2_t variable
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables ---------------------------------------------------------------------------
   character(len=:), allocatable :: str
   type     (str_t)              :: h(6)   
   integer  (Iprec)              :: lstr, lp, rp, err 
   type     (pk2_t), allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 
   
   G_flagerr = err_t()
   
   h(1) = 'Usage: help("name") or help name'
   h(2) = 'where << name >> is the name of a command or a function' 
   h(3) = ''
   h(4) = 'Examples:'
   h(5) = '. help pwd'
   h(6) = '. help("pwd")' 
   
   if (present(expr)) then

      lp = index(Expr,'(') ; rp = index(Expr,')',back=.true.)
      
      if (lp == 0 .and. rp == 0) then
         str = trim(adjustl(expr(5:)))
      else if (lp < rp) then
         str = Expr(lp+1:rp-1) ! remove parentheses
         lstr = len_trim(str)
         if (lstr /= 0) then
            err = 0
            if (str(1   :1   ) /= '"' .and. str(1   :1   ) /= "'") err = 1
            if (str(lstr:lstr) /= '"' .and. str(lstr:lstr) /= "'") err = 1
            if (err == 1) then
               G_flagerr = err_t (stat = G_UERROR,  &
                                   msg = "error in expression << "// expr //" >>. Type 'help'")
               return
            end if
            str = str(2:lstr-1) ! remove quotation marks
         end if 
      else
         G_flagerr = err_t(stat = G_UERROR, msg = 'Unbalanced parentheses') 
         return  
      end if
   
         
      select case (str)
         case ('help')
            helpout = h
            if (present(msg)) msg = 'help for the command "help"'
         case ('clear')   
            call CalmatCmd_Clear ( clearout = helpout )
            if (present(msg)) msg = 'help for the command "clear"'
         case('list')
            call CalmatCmd_List  ( listout = helpout )
            if (present(msg)) msg = 'help for the command "list"'
         case('exec')
            call CalmatCmd_Exec  ( execout = helpout ) 
            if (present(msg)) msg = 'help for the command "exec"'
         case('pwd')
            call CalmatCmd_pwd   ( pwdout = helpout ) 
            if (present(msg)) msg = 'help for the command "pwd"'
         case('cd')
            call CalmatCmd_chdir ( cdout = helpout ) 
            if (present(msg)) msg = 'help for the command "cd"'
         case('ls')
            call CalmatCmd_ls    ( lsout = helpout ) 
            if (present(msg)) msg = 'help for the command "ls"'
         case('more')
            call CalmatCmd_more ( moreout = helpout ) 
            if (present(msg)) msg = 'help for the command "more"'
         case('tail')
            call CalmatCmd_tail ( tailout = helpout ) 
            if (present(msg)) msg = 'help for the command "tail"'
         case('linux')
            call CalmatCmd_linux ( linuxout = helpout ) 
            if (present(msg)) msg = 'help for the command "linux"'
         case('cp')
            call CalmatCmd_cp ( cpout = helpout ) 
            if (present(msg)) msg = 'help for the command "cp"'
         case('mv')
            call CalmatCmd_mv ( mvout = helpout ) 
            if (present(msg)) msg = 'help for the command "cp"'
         case('rm')
            call CalmatCmd_rm ( rmout = helpout ) 
            if (present(msg)) msg = 'help for the command "cp"'

         case('plot')
            call CalmatCmd_Plot  ( plotout = helpout ) 
            if (present(msg)) msg = 'help for the command "plot"'
         case('plot3d','surf')
            call CalmatCmd_Plot3d( plotout = helpout ) 
            if (present(msg)) msg = 'help for the command "plot3d" or "surf"'
         case('format')
            call CalmatCmd_Format  ( formatout = helpout )
            if (present(msg)) msg = 'help for the command "format"'
         case('disp')
            call CalmatCmd_Disp  ( dispout = helpout )
            if (present(msg)) msg = 'help for the command "disp"'
         case('exit','quit')
            call CalmatCmd_Quit  ( quitout = helpout )
            if (present(msg)) msg = 'help for the command "quit" or "exit"'
         case('for','endfor')
            call CalmatCmd_For  ( forout = helpout )
            if (present(msg)) msg = 'help for the command "for/endfor"'
         case('if','then','elseif','else','endif')
            call CalmatCmd_If  ( ifout = helpout )
            if (present(msg)) msg = 'help for the command "if-then/elseif-then/else/endif"'
         case('break')
            call CalmatCmd_Break  ( breakout = helpout )
            if (present(msg)) msg = 'help for the command "break"'
         case('mesh')
            call CalmatCmd_Mesh  ( meshout = tmp )
            call pk2_movealloc ( from = tmp(1), to = helpout)
            if (present(msg)) msg = 'help for the command "mesh"'

         case('')
            helpout = h  
            if (present(msg)) msg = 'help for the command "help"'
         
         case default
            helpout = str
            if (present(msg)) msg = 'help for the function/operator "'//trim(str)//'"'               
            helpout = pk2f_HELP ([helpout])
      end select
   else
      helpout = h  
      if (present(msg)) msg = 'help for the command "help"'       
   end if
                    
   END SUBROUTINE CalmatCmd_Help


!=============================================================================================
   SUBROUTINE CalmatCmd_Mesh ( expr, meshout )
!=============================================================================================
   character(len=*),              optional, intent(in    ) :: expr
   type     (pk2_t), allocatable,           intent(   out) :: meshout(:)
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=* ), parameter   :: HERE = '(in CalmatCmd_Mesh)'
   character(LGSTR )              :: iomsg   
   integer  (Iprec )              :: i, ui, n, lp, rp, err
   type     (str_t )              :: h(7)
   character(len=: ), allocatable :: filename   
   type     (pk2_t ), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------
           
   if (.not. present(Expr)) then
      allocate(meshout(1))   
      h(1)%str = '<< mesh >> read a mesh'
      h(2)%str = ' '
      h(3)%str = 'Syntax: mesh("file.msh")'
      h(4)%str = " "
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
   character(len=*), parameter   :: HERE = '(in CalmatCmd_Exec)'
   integer  (Iprec)              :: lp, rp, err
   type     (str_t)              :: h(7)
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------
         
   if (.not. present(Expr)) then
      h(1)%str = '<< exec >> execute a script'
      h(2)%str = ' '
      h(3)%str = 'Syntax: exec("script.m")'
      h(4)%str = '        exec "script.m"'
      h(5)%str = '        exec(MYDIR + "/script.m")'
      h(6)%str = '        exec MYDIR + "/script.m"'
      h(7)%str = " "
      execout = h
      return
   end if   

   if (.not. present(fileName)) then
      G_flagerr = err_t(stat = G_IERROR, msg = HERE //' Missing the "fileName" argument') 
      return
   end if
   
   err = 0
      
   lp = index(Expr,'(') ; rp = index(Expr,')',back=.true.)
      
   if (lp == 0 .and. rp == 0) then
      fileName = trim(adjustl(expr(5:)))
   else if (lp < rp) then
      fileName = Expr(lp+1:rp-1)
   else
      G_flagerr = err_t(stat = G_UERROR, msg = 'Unbalanced parentheses') 
      return  
   end if
   
   if (len_trim(fileName) == 0) then
      G_flagerr = err_t( stat = UERROR, msg = HERE // &
                        ' A file name is required in the expression << '//trim(expr)// ' >>' )
      return
   end if   
!
!- Evaluate 'fileName' to get the name of the script file (call the interpreter):
!
   call pk2Interpreter_driver ( expr     = fileName            , &
                                vars     = G_vars(1:G_nvar)    , &
                                varNames = G_varNames(1:G_nvar), &
                                valexpr  = valexpr             , &
                                flagerr  = G_flagerr           )

                                   
   if (G_flagerr%code /= 0) return
   
   call pk2_moveAlloc ( from = valexpr(1), to = fileName, stat  = G_flagerr  )

   if (G_flagerr%code /= 0) return
   
   if (.not. allocated(fileName)) then
      err = 1
   else
      if (len_trim(fileName) == 0) err = 1
   end if
   
   if (err == 1) &
      h(1)%str = HERE // ' A valid file name is required'
   
   if (G_flagerr%code > 0) then
      if (err == 1) then
         h(1)%str = G_flagerr%mesg // NL // '--> ' // (h(1)%str)
         G_flagerr = err_t (stat = G_UERROR, msg = (h(1)%str))
      end if   
      return
   else if (err == 1) then
      G_flagerr = err_t (stat = G_UERROR, msg = (h(1)%str))
      return
   end if
               
   END SUBROUTINE CalmatCmd_Exec
   
   
!=============================================================================================   
   SUBROUTINE CalmatCmd_Clear ( expr, clearout, gins )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr
   type     (pk2_t), optional, intent(   out) :: clearout
   integer  (Iprec), optional, intent(in    ) :: gins
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
   integer  (Iprec)              :: i, k, ntok, IdObj, IdVar
   character(len=:), allocatable :: msg
   type     (str_t), allocatable :: h(:), tokens(:,:)
!---------------------------------------------------------------------------------------------    
      
   if (.not. present(expr)) then
      allocate(h(7))
      h(1)%str = "<< clear >> delete all variables or a set of them "
      h(2)%str = " "
      h(3)%str = "Syntax: clear, clear a, clear a b c"
      h(4)%str = " "
      h(5)%str = ". clear      : delete all variables (except protected ones)"
      h(6)%str = ". clear a    : delete the variable a"
      h(7)%str = ". clear a b c: delete the variables a b and c"
      clearout = h
      return
   end if   

   if (.not. present(gins)) then
      G_flagerr = err_t(stat = G_IERROR, msg = HERE // ' Missing argument "gins"')
      return
   end if   

   ntok = util_CountTokens ( str    = expr  , delims = ' ' , BlkToken =.false., &
                             tokens = tokens, who    = HERE, stat = G_flagerr   )
   
   if (G_flagerr%code /= 0) return
           
   if (ntok == 1) then
!
!-    Clear all variables:
!   
      do IdVar = 1, size(G_vars)
!
!-       If this variable is protected, unused or defined after the present instruction: cycle
!         
         if (G_vars(IdVar)%status   == G_PROTECTED .or. &
             G_vars(IdVar)%status   == G_FREE      .or. &
             G_vars(IdVar)%firstIns >  gins           ) then
             
            cycle
   
         else if (G_vars(IdVar)%lastIns <= gins) then
!
!-          this variable is only defined before this instruction: destroy it (but DO NOT
!           RENUMBER the remaining variables and DO NOT CHANGE "G_nvar", the freed space
!           will be used by a future variable) and remove it from the list of variable
!           names (normally already removed by Calmat_ParseClear). See also if this 
!           variable is a component of a structure:   
!                          
            k = G_vars(IdVar)%objId
            
            if (k /= 0) then               
!
!-             this variable is a component of the object #k. Remove it from this object:
!               
               call G_objs(k)%RemoveAComponent ( IdVar, G_flagerr )
               
               if (G_flagerr%code > 0) return
                                    
               if (G_objs(k)%nCmp == 0) call G_objs(k)%Frees()
                   
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
         
         if (IdObj /= 0) then        

            if (G_objs(IdObj)%status == G_FREE .or. G_objs(IdObj)%firstIns > gins) then
!
!-             this object is unused or defined after this clear instruction: cycle
!            
               cycle
            end if

            if (G_objs(IdObj)%lastIns <= gins) then
!
!-             this object is only defined before this clear instruction: destroy it
!     
               call CalmatUtil_RemoveObj ( IdObj, G_flagerr ) ; if (G_flagerr%code > 0) return
            else 
!
!-             this object is also redefined after this clear instruction: destroy its 
!              components but keep it name and the names of its components that are redefined 
!              after this instruction:
!
               do k = 1, G_objs(IdObj)%nCmp
                  IdVar = G_objs(IdObj)%varId(k)
                  if (IdVar == 0) cycle
                  if (G_vars(IdVar)%lastIns <= gins) then
                     call G_objs(IdObj)%RemoveAComponent ( IdVar, G_flagerr )               
                     if (G_flagerr%code > 0) return
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
         call CalmatUtil_FindVar ( tokens(i,1)%str, IdVar ) 
         
         if (IdVar /= 0) then 

            if (G_vars(IdVar)%status == G_PROTECTED) then 
!
!-             the variable is protected. Set an error message and cycle:
!            
               if (.not. allocated(msg)) then
                  msg = trim(adjustl(tokens(i,1)%str))
               else
                  msg = msg // ', ' // trim(adjustl(tokens(i,1)%str))
               end if      
               cycle
               
            else if (G_vars(IdVar)%status == G_FREE .or. G_vars(IdVar)%firstIns > gins) then
!
!-             the variable is unused or is defined after the present instruction: cycle
!            
               cycle
            end if 

            if (G_vars(IdVar)%lastIns <= gins) then
!
!-             this variable is only defined before this instruction: destroy it (but DO NOT
!              RENUMBER the remaining variables and DO NOT CHANGE "G_nvar", the freed space
!              will be used by a future variable) and remove it from the list of variable
!              names (normally already removed by Calmat_ParseClear). See also if this 
!              variable is a component of a structure:   
!                           
               k = G_vars(IdVar)%objId
            
               if (k /= 0) then               
!
!-                this variable is a component of the object #k. Remove it from this object:
!               
                  call G_objs(k)%RemoveAComponent ( IdVar, G_flagerr )
               
                  if (G_flagerr%code > 0) return
                  
                  if (G_objs(k)%nCmp == 0) call G_objs(k)%Frees()
                   
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
   
   if (allocated(msg)) G_flagerr = err_t( stat = G_UERROR, msg = &
                                         'protected variables cannot be deleted ('//msg//')' )
           
   END SUBROUTINE CalmatCmd_Clear


!=============================================================================================
   SUBROUTINE CalmatCmd_List ( Expr, listout, msg )
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
   character(len=*),              optional, intent(in    ) :: Expr
   type     (pk2_t),                        intent(   out) :: listout
   character(len=:), allocatable, optional, intent(   out) :: msg
!---------------------------------------------------------------------------------------------
!  Command "list"
!
!  If "Expr" is present, it must be: 
!
!  . list('vars'), list(), list, 
!  . list('operators'), list('op'),
!  . list('functions'), list('fun'),
!  . list('commands'), list('cmd')
!
!  and this routine returns the corresponding list in the pk2 variable "listout"
!
!  If "Expr" is not present, the routine returns the help in "listout".
!---------------------------------------------------------------------------------- R.H. 11/18

!- local variables: --------------------------------------------------------------------------
   character(len=* ), parameter   :: HERE = '(in CalmatCmd_List)'
   integer                        :: i, n, lp, rp, err, n1, n2, j, iv, d, ll(3)
   character(len=99)              :: buf, typ(G_nvar)
   character(len=: ), allocatable :: opt, str, name
   type     (str_t ), allocatable :: tmp(:,:), h(:)
!---------------------------------------------------------------------------------------------
      
   opt = ''
   
   if (.not. present(Expr)) then
      allocate(h(15))
      h( 1)%str = "<< list >> can give the list of "
      h( 2)%str = ". all current variables (including structures)"
      h( 3)%str = ". current structures only"
      h( 4)%str = ". available functions and commands"
      h( 5)%str = ". available operators"
      h( 6)%str = ". informations about compiler version & options"
      h( 7)%str = " "
      h( 8)%str = "Syntax: list  or  list()   or  list('vars'),"
      h( 9)%str = "        list('structures') or  list('str')"
      h(10)%str = "        list('functions')  or  list('fun')"
      h(11)%str = "        list('commands')   or  list('cmd')"      
      h(12)%str = "        list('operators')  or  list('op')"
      h(13)%str = "        list('compiler')   or  list('comp')"
      h(14)%str = ""
      h(15)%str = "(list, list() and list('vars') are equivalent)"
      listout = h
      return
   end if   
   
   err = 0
      
   lp = index(Expr,'(') ; rp = index(Expr,')',back=.true.)
   if (lp == 0 .and. rp == 0) then
      opt = 'vars'
   else if (lp < rp) then
      opt = trim(adjustl(Expr(lp+1:rp-1)))
      i = len_trim(opt)
      if (i == 0) then
         opt = 'vars'
      else if (i >= 2) then
         opt = opt(2:i-1)  !removes quotation marks
      end if      
   else
      err = 1   
   end if
   
   opt = trim(adjustl(opt))
      
   if (err == 0) then
      select case (opt)
      
         case ('vars')
                     
            n = 0
            do i = 1, G_nVar
               if (G_vars(i)%status /= G_FREE .and. G_vars(i)%objId == 0) then
                  if (len_trim(G_vars(i)%GetName()) /= 0) n = n + 1
               end if
            end do
            
            do i = 1, G_nObj
               if (G_objs(i)%status /= G_FREE) n = n + G_objs(i)%nCmp + 1
            end do   
            
            n = n + 1
            
            listout = pk2_t (typ = STYP, shape = [n,6])
            
            select type (p=>listout%m)
               type is (sk2_t)
                  n = 0
                  do i = 1, G_nVar
                     if (G_vars(i)%status == G_FREE) cycle
                     
                     name = G_vars(i)%GetName()
                     if (len_trim(name) == 0) cycle

                     if (G_vars(i)%typ == EMPTY) then
                        typ(i) = 'empty'
                     else if (G_vars(i)%typ == ITYP) then   
                        typ(i) = 'integer'
                     else if (G_vars(i)%typ == RTYP) then   
                        typ(i) = 'real'
                     else if (G_vars(i)%typ == CTYP) then   
                        typ(i) = 'complex'
                     else if (G_vars(i)%typ == LTYP) then   
                        typ(i) = 'logical'
                     else if (G_vars(i)%typ == STYP) then   
                        typ(i) = 'string'
                     end if
                     if (G_vars(i)%objId == 0) then
                        n = n + 1
                        p%v(n,1)%str = trim(name)
                        p%v(n,2)%str = trim(typ(i))
                        write(buf,'(i0)')G_vars(i)%nrow
                        p%v(n,3)%str = trim(adjustr(buf))
                        p%v(n,4)%str = 'x'
                        write(buf,'(i0)')G_vars(i)%ncol
                        p%v(n,5)%str = trim(adjustl(buf))
                        write(buf,'(i0)')i
                        p%v(n,6)%str = '(#'//trim(adjustl(buf))//')'
                     end if
                  end do   
                  
                  n = n + 1
                                    
                  do i = 1, G_nObj
                     if (G_objs(i)%status == G_FREE) cycle
                     n = n + 1
                     p%v(n,1)%str = trim(G_objs(i)%name)
                     p%v(n,2)%str = "structure"
                     write(buf,'(i0)')i
                     p%v(n,6)%str = '(#'//trim(adjustl(buf))//')'                     
                     do j = 1, G_objs(i)%nCmp
                        iv = G_objs(i)%varId(j)
                        if (iv /= 0) then
                           n = n + 1
                           str = trim(adjustl(G_vars(iv)%GetName()))
                           d = index(str,'.')
                           p%v(n,1)%str = '  '//str(d:)
                           p%v(n,2)%str = trim(typ(iv))
                           write(buf,'(i0)')G_vars(iv)%nrow
                           p%v(n,3)%str = trim(adjustr(buf))
                           p%v(n,4)%str = 'x'
                           write(buf,'(i0)')G_vars(iv)%ncol
                           p%v(n,5)%str = trim(adjustl(buf))
                           write(buf,'(i0)')iv
                           p%v(n,6)%str = '(=> #'//trim(adjustl(buf))//')'
                        end if
                     end do
                  end do    
            end select
                        
            if (present(msg)) msg = 'Current variables (name, type, size)'
      
         case ('structures','str')

            n = 0
            do i = 1, G_nObj
               if (G_objs(i)%status /= G_FREE) n = n + G_objs(i)%nCmp + 1
            end do   
                        
            listout = pk2_t (typ = STYP, shape = [n,6])
         
            select type (p=>listout%m)
               type is (sk2_t)
                  n = 0
                  do i = 1, G_nObj
                     if (G_objs(i)%status == G_FREE) cycle
                     n = n + 1
                     p%v(n,1)%str = trim(G_objs(i)%name)
                     p%v(n,2)%str = "structure"
                     write(buf,'(i0)')i
                     p%v(n,6)%str = '(#'//trim(adjustl(buf))//')'                     
                     do j = 1, G_objs(i)%nCmp
                        iv = G_objs(i)%varId(j)
                        if (iv /= 0) then
                           name = G_vars(iv)%GetName()
                           if (len_trim(name) == 0) cycle
                           if (G_vars(iv)%typ == EMPTY) then
                              typ(j) = 'empty'
                           else if (G_vars(iv)%typ == ITYP) then   
                              typ(j) = 'integer'
                           else if (G_vars(iv)%typ == RTYP) then   
                              typ(j) = 'real'
                           else if (G_vars(iv)%typ == CTYP) then   
                              typ(j) = 'complex'
                           else if (G_vars(iv)%typ == LTYP) then   
                              typ(j) = 'logical'
                           else if (G_vars(iv)%typ == STYP) then   
                              typ(j) = 'string'
                           end if   
                           n = n + 1
                           str = trim(adjustl(name))
                           d = index(str,'.')
                           p%v(n,1)%str = '  '//str(d:)
                           p%v(n,2)%str = trim(typ(j))
                           write(buf,'(i0)')G_vars(iv)%nrow
                           p%v(n,3)%str = trim(adjustr(buf))
                           p%v(n,4)%str = 'x'
                           write(buf,'(i0)')G_vars(iv)%ncol
                           p%v(n,5)%str = trim(adjustl(buf))
                           write(buf,'(i0)')iv
                           p%v(n,6)%str = '(=> #'//trim(adjustl(buf))//')'
                        end if
                     end do
                  end do 
            end select     
            
            if (present(msg)) msg = 'Current objects (component names, type, size)'
            
         
         case ('operators','op')
!
!-          print the available operators
!
            allocate(tmp(size(OperList),2), stat = err)
            if (err /= 0) then
               G_flagerr = err_t (stat = G_IERROR, msg = HERE//" Allocation failure for 'tmp'")
               return
            end if   
            tmp(:,1) = OperList ; tmp(:,2) = OperDesc
            listout = tmp
            if (present(msg)) msg = 'list of available operators'
         
         case ('functions','fun')
!
!-          print the available functions names
!
            allocate(tmp(size(G_FuncNames),2))
            if (err /= 0) then
               G_flagerr = err_t (stat = G_IERROR, msg = HERE//" Allocation failure for 'tmp'")
               return
            end if   
            tmp(:,1) = G_FuncNames ; tmp(:,2) = G_FuncDescr
            listout = tmp
            if (present(msg)) msg = 'list of available functions (alphabetical order)'
            
         case ('commands','cmd')
!
!-          print the available commands names
!
            allocate(tmp(size(G_CmdNames),2))
            if (err /= 0) then
               G_flagerr = err_t (stat = G_IERROR, msg = HERE//" Allocation failure for 'tmp'")
               return
            end if   
            tmp(:,1) = G_CmdNames ; tmp(:,2) = G_CmdDescr
            listout = tmp
            if (present(msg)) msg = 'list of available commands'
            
         case ('compiler','comp')
!
!-          print compiler informations
!
            allocate(h(3))
            
            h(1)%str = compiler_version()
            if (len_trim(G_COPTS) /= 0) then
               h(2)%str = G_COPTS
            else   
               h(2)%str = compiler_options()
            end if
            if (len_trim(G_CDATE) /= 0) then
               h(3)%str = G_CDATE
            else   
               h(3)%str = ''
            end if
             
            ll(1) = max(1,len_trim(h(1)%str)/40)
            ll(2) = max(1,len_trim(h(2)%str)/40)
            ll(3) = max(1,len_trim(h(3)%str)/40)
            
            allocate(tmp(ll(1)+ll(2)+ll(3),2))
            n1 = 1
            do i = 1, ll(1)
               if (i==1) then
                  tmp(1,1)%str = 'Compiler version:'
               else   
                  tmp(i,1)%str = ''
               end if
               n2 = n1 + 40
               if (n2 > len_trim(h(1)%str) .or. i == ll(1)) n2 = len_trim(h(1)%str)
               tmp(i,2)%str = h(1)%str(n1:n2)
               n1 = n2+1
            end do    
            n1 = 1
            do i = 1, ll(2)
               j = ll(1)+i
               if (i==1) then
                  tmp(j,1)%str = 'Compiler options:'
               else   
                  tmp(j,1)%str = ''
               end if
               n2 = n1 + 40
               if (n2 > len_trim(h(2)%str) .or. i == ll(2)) n2 = len_trim(h(2)%str)
               tmp(j,2)%str = h(2)%str(n1:n2)
               n1 = n2+1
            end do    
            n1 = 1
            do i = 1, ll(3)
               j = ll(1)+ll(2)+i
               if (i==1) then
                  tmp(j,1)%str = 'Compilation date:'
               else   
                  tmp(j,1)%str = ''
               end if
               n2 = n1 + 40
               if (n2 > len_trim(h(3)%str) .or. i == ll(3)) n2 = len_trim(h(3)%str)
               tmp(j,2)%str = h(3)%str(n1:n2)
               n1 = n2+1
            end do                                  
            listout = tmp
            if (present(msg)) msg = 'Compiler informations'
            
         case default
            err = 1
      end select
   end if
   
   if (err == 1) then                  
      G_flagerr = err_t ( stat = G_UERROR, &
                          msg = 'Unknown option for command << list >>. '         // NL // &
                                '           Valid options are:'                   // NL // &
                                '           . list, list() or list("vars")'       // NL // &
                                '           . list("str")  or list("structures")' // NL // &
                                '           . list("op")   or list("operators")'  // NL // &
                                '           . list("cmd")  or list("commands")'   // NL // &
                                '           . list("fun")  or list("functions")'  // NL // &         
                                '           . list("comp") or list("compiler")'            )                
   end if
   
   END SUBROUTINE CalmatCmd_List   


!=============================================================================================   
   SUBROUTINE CalmatCmd_pwd ( dum, pwdout )
!=============================================================================================
   use iso_c_binding
   character(len=*), optional, intent(in    ) :: dum 
   type     (pk2_t),           intent(   out) :: pwdout
!---------------------------------------------------------------------------------------------  
!  Command "pwd"
!  
!  Returns the name of the current directory
!
!  If "dum" is not present, returns help in the pk2 variable pwdout
!
!  Note: unfortunately, the function/subroutine fortran getcwd are not portable. 
!        As a workaround, we use here the interoperablity with C to call the corresponding C
!        function (adapted from a solution found on www.tek-tips.com).
!---------------------------------------------------------------------------------- R.H. 01/19

!- local variables ---------------------------------------------------------------------------
   character(len=*             ), parameter   :: HERE = '(in CalmatCmd_pwd)'   
   type     (str_t             ), allocatable :: h(:)
   character(kind=c_char, len=:), allocatable :: dir
   integer  (c_long            )              :: i, ldir, iguess, lguess, nguess, err
   type     (c_ptr             )              :: buf
!---------------------------------------------------------------------------------------------    

   interface
      function c_pwd (dir, ldir) bind(c, name = "getcwd") result(r)
         use iso_c_binding
         integer  (c_size_t), value, intent(in    ) :: ldir
         character(c_char  ),        intent(   out) :: dir(*)
         type     (c_ptr   )                        :: r
      end function
   end interface
    
   if (.not. present(dum)) then
      allocate(h(4))
      h(1)%str = "<< pwd >> return the name of the current directory (linux command)"
      h(2)%str = " "
      h(3)%str = "Syntax: s = pwd"
      h(4)%str = " "
      pwdout = h
      return
   end if   

!
!- Find a length large enough:
!
   nguess = 5 ; lguess = 0 ; err = 1
   
   do iguess = 1, nguess
      lguess = lguess + MDSTR
      allocate(character(len=lguess) :: dir)
      buf = c_pwd (dir, lguess)
      if (.not. c_associated(buf)) then
         deallocate(dir)
         cycle
      else
         err = 0
         exit   
      end if   
   end do
   
   if (err == 0) then
!
!-    Find the terminal null_char:
!   
      ldir = 0
      do i = 1, lguess
         if (dir(i:i) == c_null_char) then
            ldir = i ; exit
         end if   
      enddo
      if (ldir > 0) then
         dir(ldir:) = ' ' ; pwdout = trim(adjustl(dir))
         return
      else   
         err = 1
      end if   
   end if   
   
   if (err /= 0) then
      G_flagerr = err_t (stat = G_UERROR, &
                          msg = HERE //' The length of the current directory is too long')
      return
   end if   
         
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
   type     (str_t), allocatable :: h(:)!, dir(:,:)
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
        
   if (.not. present(expr)) then
      allocate(h(7))
      h(1)%str = '<< cd >> change the current directory (linux command)'
      h(2)%str = ' '
      h(3)%str = 'Syntax: cd [directory]'
      h(4)%str = ' '
      h(5)%str = 'Examples:'
      h(6)%str = '  . cd'
      h(7)%str = '  . cd /home/users/me/tmp/src'
      cdout = h
      return
   end if   
      
   cd = trim(adjustl(expr(1:2))) ; dir = trim(adjustl(expr(3:)))   
   
   if (trim(expr) == 'cd' .or. (cd == 'cd' .and. dir == '~')) then
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
      if (index(dir,'+') /= 0) then
         to_interprete = .true.
      else   
         do i = 1, G_nvar
            if (G_vars(i)%status == G_FREE) cycle
            name = (G_vars(i)%GetName())
            if (len_trim(name) /= 0) then
               if (dir == trim(adjustl(name))) then 
                  to_interprete = .true.
                  exit
               end if   
            end if
         end do
      end if   
!
!-    If "to_interprete" is .true. call the interpreter to evaluate the name of the directory:
! 
      if (to_interprete) then
          
         call pk2Interpreter_driver ( expr     = dir                 , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      varNames = G_varNames(1:G_nvar), &
                                      valexpr  = pk2dir              , &
                                      flagerr  = G_flagerr           )

         if (G_flagerr%code > 0) return
       
         call pk2_moveAlloc ( from = pk2dir(1), to = dir, stat = G_flagerr )
         
         if (G_flagerr%code > 0) return

      end if
      
   end if   
!
!- Change directory:
!   
   c_stat =  c_chdir ( dir//c_null_char )
   
   if (c_stat /= 0) G_flagerr = err_t (stat = G_UERROR, &
                              msg = "Could not find the directory '"//trim(dir)//"'") 
   
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
   type     (str_t), allocatable :: h(:)   
   integer  (Iprec)              :: err, u, n, i
   character(len=:), allocatable :: line   
   character(len=1)              :: dummyCh
!---------------------------------------------------------------------------------------------    

   if (.not. present(expr)) then
      allocate(h(7))
      h(1)%str = '<< ls >> list the directory contents (linux command)'
      h(2)%str = ' '
      h(3)%str = 'Syntax: ls [directory]'
      h(4)%str = ' '
      h(5)%str = 'Examples:'      
      h(6)%str = '  . ls'
      h(7)%str = '  . ls /home/users/me/tmp/src'
      lsout = h
      return
   end if
      
   call execute_command_line (expr//' > .calmat_ls_tmp#12#123.out')
   
   u = util_GetUnit () ; open(unit = u,file = '.calmat_ls_tmp#12#123.out')
   
   n = 0
   do 
      read(u,'(a1)',iostat=err) dummyCh
      if (err /= 0) exit
      n = n + 1
   end do   
   rewind(u)
   
   lsout = pk2_t (typ = STYP, shape = [n,1_Iprec])
   
   select type (p=>lsout%m)
      type is (sk2_t)
         do i = 1, n
            line = util_GetLine (unit = u, who = HERE, stat = G_flagerr) 
            if (G_flagerr%code /= 0) exit
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
   type(str_t) :: h(4)   
!---------------------------------------------------------------------------------------------    

   if (.not. present(expr)) then
      h(1)%str = '<< more >> view the content of a file  (linux command)'
      h(2)%str = ' '
      h(3)%str = 'Syntax: more fileName'
      h(4)%str = ' '
      moreout = h
      return
   end if
      
   call CalmatUtil_ExecuteCommandLine ( expr, G_flagerr ) 
         
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
   type(str_t) :: h(4)   
!---------------------------------------------------------------------------------------------    

   if (.not. present(expr)) then
      h(1)%str = '<< tail >> display the last part of a file (linux command)'
      h(2)%str = ' '
      h(3)%str = 'Syntax: tail fileName'
      h(4)%str = ' '
      tailout = h
      return
   end if
      
   call CalmatUtil_ExecuteCommandLine ( expr, G_flagerr ) 
         
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
   type     (str_t)              :: h(4)   
   character(len=:), allocatable :: str
   integer                       :: lstr, lp  
!---------------------------------------------------------------------------------------------    

   if (.not. present(expr)) then
      h(1)%str = '<< linux >> acces to linux commands'
      h(2)%str = ' '
      h(3)%str = 'Syntax: linux("command") or linux command'
      h(4)%str = ' '
      linuxout = h
      return
   end if
   
   str = adjustl(expr(6:)) ; lstr = len_trim(str)
   
   lp = index(str,'(') 
   if (lp /= 0) then
      if (str(lstr:lstr) /= ')') then
         G_flagerr = err_t (stat = G_UERROR, msg = 'Unbalanced parentheses')
         return
      end if
      str = str(lp+1:lstr-1)
      lstr = len_trim(str)       
   end if
   
   lp = index(str,'"')
   if (lp == 1) then
      if (str(lstr:lstr) /= '"') then
         G_flagerr = err_t (stat = G_UERROR, msg = 'Unbalanced quotes (")')
         return
      end if
      str = str(lp+1:lstr-1)
   else
      lp = index(str,"'")
      if (lp == 1) then
         if (str(lstr:lstr) /= "'") then
            G_flagerr = err_t (stat = G_UERROR, msg = "Unbalanced quotes (')")
            return
         end if
         str = str(lp+1:lstr-1)
      end if       
   end if
         
   call CalmatUtil_ExecuteCommandLine ( str, G_flagerr ) 
         
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
   type     (str_t)              :: h(8)   
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------    

   if (.not. present(expr)) then
      h(1)%str = '<< cp >> copy files (linux command)'
      h(2)%str = ' '
      h(3)%str = 'Syntax: cp source_file target_file'
      h(4)%str = ' '
      h(5)%str = 'Note: this is the linux cp command with the option "-i" (request'
      h(6)%str = '      confirmation before attempting to overwrite an existing file).'
      h(7)%str = '      Use the command "linux cp" to access the other options'
      h(8)%str = ' '
      cpout = h
      return
   end if
!
!- Request confirmation (add -i):
!
   str = expr
      
   if (index(str,' -i ') == 0) str = 'cp -i ' // str(3:)
         
   call CalmatUtil_ExecuteCommandLine ( str, G_flagerr ) 
         
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
   type     (str_t)              :: h(8)   
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------    

   if (.not. present(expr)) then
      h(1)%str = '<< mv >> move files (linux command)'
      h(2)%str = ' '
      h(3)%str = 'Syntax: mv source target'
      h(4)%str = ' '
      h(5)%str = 'Note: this is the linux mv command with the option "-i" (request'
      h(6)%str = '      confirmation before attempting to overwrite an existing file).'
      h(7)%str = '      Use the command "linux mv" to access the other options'
      h(8)%str = ' '
      mvout = h
      return
   end if
!
!- Request confirmation (add -i):
!
   str = expr
      
   if (index(str,' -i ') == 0) str = 'mv -i ' // str(3:)
         
   call CalmatUtil_ExecuteCommandLine ( str, G_flagerr ) 
           
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
   type     (str_t)              :: h(8)   
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------    

   if (.not. present(expr)) then
      h(1)%str = '<< rm >> remove files (linux command)'
      h(2)%str = ' '
      h(3)%str = 'Syntax: rm file1 ...'
      h(4)%str = ' '
      h(5)%str = 'Note: this is the linux rm command with the option "-i" (request'
      h(6)%str = '      confirmation before attempting to remove each file).'
      h(7)%str = '      Use the command "linux rm" to access the other options'
      h(8)%str = ' '
      rmout = h
      return
   end if
!
!- Request confirmation (add -i):
!
   str = expr
      
   if (index(str,' -i ') == 0) str = 'rm -i ' // str(3:)
         
   call CalmatUtil_ExecuteCommandLine ( str, G_flagerr ) 
           
   END SUBROUTINE CalmatCmd_rm


!=============================================================================================   
   SUBROUTINE CalmatCmd_Plot ( expr, flowId, plotout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr 
   integer  (Iprec), optional, intent(in    ) :: flowId
   type     (pk2_t),           intent(   out) :: plotout
!---------------------------------------------------------------------------------------------    
!  Command "plot"
!  Very basic way to make plot (gnuplot invocation)
!---------------------------------------------------------------------------------- R.H. 02/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*    ), parameter   :: HERE = 'CalmatCmd_Plot'
   character(len=:    ), allocatable :: str, gpscript, gpfig, tmpfile, styl, buf, legend, &
                                        gpcmd, gpterm
   type     (str_t    ), allocatable :: plot(:,:), tokens(:,:), h(:)
   integer  (Iprec    )              :: lp, rp, nplot, ntok, narg, i, j, n1, m1, n2, m2,  &
                                        nl, up, ugpscript
   integer                           :: cmdstat, exitstat, err
   type     (pk2_t    )              :: tmp(2)
   type     (pk2_t    ), allocatable :: valexpr(:)
   character(len=99   )              :: cnum
   character(len=LGSTR)              :: cmdmsg
!---------------------------------------------------------------------------------------------    
   integer  (Iprec    ), save        :: ngpfig = 0   
!---------------------------------------------------------------------------------------------    

   if (.not. present(expr)) then
      allocate(h(16))
      h( 1)%str = '<< plot >> 2D plot '
      h( 2)%str = ' '
      h( 3)%str = 'Syntax: plot ( x1 [, y1] [,line_spec] [; x2, y2 [,line_spec] )'
      h( 4)%str = ' '
      h( 5)%str = "Note: "
      h( 6)%str = "    semicolons (';') are used as delimiters in case of several graphs" 
      h( 7)%str = "    (see example below)"
      h( 8)%str = ' '
      h( 9)%str = 'Examples:'      
      h(10)%str = '  . plot(x1)'
      h(11)%str = '  . plot(x1,y1)'
      h(12)%str = '  . plot(x1,y1,"-r")'
      h(13)%str = '  . plot(x1,y1,"-r" ; x2,y2,"ok")'
      h(14)%str = ' '
      h(15)%str = '(Note: plots are generated by the command-line program gnuplot. '
      h(16)%str = '       The figures are persistent but not interactive)'
      plotout = h
      return
   end if
   
   gpcmd  = G_default(3)%str
   gpterm = G_default(4)%str

   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if (lp * rp == 0 .or. rp <= lp) then
      G_flagerr = err_t (stat = G_UERROR, msg = 'Unbalanced parentheses')
      return
   end if
   
   if (rp - lp <= 1) then
      G_flagerr = err_t (stat = G_WARNING, &
                          msg = 'Nothing to plot (at least one argument expected)')
      return
   end if
   
   str = trim(adjustl(expr(lp+1:rp-1))) ! remove parentheses   
!
!- Determine the number of graphs (delimited by a ';'):
!
   nplot = util_CountTokens (str, delims = ';', BlkToken =.false., opcl = '[]()""', &
                             tokens = plot, who = HERE, stat = G_flagerr)
   if (G_flagerr%code > 0) return
!
!- Open a gnuplot script:
!         
   ugpscript = util_GetUnit ()
   
   if (ugpscript == 0) then
      G_flagerr = err_t (stat = G_IERROR,  msg = HERE // " No free unit available")
      return
   end if
   
   gpscript = '.tmp?_?gpscript?'
   open(unit = ugpscript, file = gpscript, status = 'replace')

   write(ugpscript,'(a)')'reset'
   write(ugpscript,'(a)')'set terminal '//trim(gpterm)//' persist'
   write(ugpscript,'(a)')'set font "Verdana,10"'
   
   if (.not. G_prompt) then
!
!-    in batch mode, save the plot in png file (with a name 'name of the script'_fig#.png)
!
      ngpfig = ngpfig + 1
      write(cnum,'(i0)')ngpfig
      
      if (present(flowId)) then
         if (flowId > 0 .and. flowId <= G_maxnFlow) then
            gpfig = trim(G_flows(flowId)%fileName)//'_fig'//trim(cnum)//'.png'
         else
            G_flagerr = err_t ( stat = G_IERROR, &
                                 msg = '(in '//HERE//') Bad value flow #' )
            close(ugpscript, status = 'delete')
            return
         end if
      else 
          G_flagerr = err_t ( stat = G_IERROR, &
                               msg = '(in '//HERE//') Missing flow #' )
          close(ugpscript, status = 'delete')
          return
      end if
      
      write(ugpscript,'(a)')"set terminal pngcairo enhanced font 'Verdana,10'"
      write(ugpscript,'(a)')"set output '"//trim(gpfig)//"'"
   end if

   write(ugpscript,'(a)')'set grid'   
!
!- Loop over the n graphs:
!   
   do i = 1, nplot
      
      ntok = util_CountTokens (plot(i,1)%str, delims = ',', BlkToken =.false., opcl = '[]()""', &
                               tokens = tokens, who = HERE, stat = G_flagerr)
                               
      if (G_flagerr%code > 0) exit
!
!-    Determine the line or symbol style:
!         
      if (index(tokens(ntok,1)%str,'"') /= 0) then
         buf = tokens(ntok,1)%str
         narg = ntok - 1         
         nl = len_trim(buf)
         if (buf(1:1) == '"' .and. buf(nl:nl) == '"') then
            buf = buf(2:nl-1) 
            styl = ' w l lw 2 '
               
            if (index(buf,'-' ) /= 0) styl = ' w l lw 2 dt 1'
            if (index(buf,':' ) /= 0) styl = ' w l lw 2 dt 3'
            if (index(buf,'--') /= 0) styl = ' w l lw 2 dt 2'
            if (index(buf,'-.') /= 0) styl = ' w l lw 2 dt 4'
               
            if (index(buf,'o') /= 0) styl = ' w p pt  7 ps 1 '
            if (index(buf,'+') /= 0) styl = ' w p pt  1 ps 2 '
            if (index(buf,'x') /= 0) styl = ' w p pt  2 ps 1 '
            if (index(buf,'*') /= 0) styl = ' w p pt  3 ps 1 '
            if (index(buf,'d') /= 0) styl = ' w p pt 28 ps 1 '
            if (index(buf,'s') /= 0) styl = ' w p pt 35 ps 1 '
            if (index(buf,'v') /= 0) styl = ' w p pt 11 ps 1 '
            if (index(buf,'^') /= 0) styl = ' w p pt  9 ps 1 '
               
            if (index(buf,'b') /= 0) styl = trim(styl) // ' lc rgb "blue" '
            if (index(buf,'r') /= 0) styl = trim(styl) // ' lc rgb "red" '
            if (index(buf,'g') /= 0) styl = trim(styl) // ' lc rgb "green" '
            if (index(buf,'y') /= 0) styl = trim(styl) // ' lc rgb "yellow" '
            if (index(buf,'k') /= 0) styl = trim(styl) // ' lc rgb "black" '
            if (index(buf,'m') /= 0) styl = trim(styl) // ' lc rgb "dark-magenta" '
            if (index(buf,'c') /= 0) styl = trim(styl) // ' lc rgb "dark-cyan" '
            if (index(buf,'w') /= 0) styl = trim(styl) // ' lc rgb "white" '
         else
            G_flagerr = err_t(stat=G_UERROR, msg = 'invalid line spec.')
            exit
         end if      
      else
         styl = ' w l lw 2 '
         narg = ntok
      end if

      if (narg == 1) then
!
!-       Case "plot(y)"
!        --------------

!
!-       Evaluate y (= tmp(2)):
!      
         call pk2Interpreter_driver ( expr     = tokens(1,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      varNames = G_varNames(1:G_nvar), &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr            )
 
         if (G_flagerr%code > 0) exit
         
         call pk2_movealloc ( from = valexpr(1), to = tmp(2),  stat = G_flagerr )
         
         if (G_flagerr%code > 0) exit
         
         n1 = tmp(2)%nrow ; m1 = tmp(2)%ncol ; n2 = n1 ; m2 = m1
         
         if (n1 /= 1 .and. m1 /= 1) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'variables of "plot" must be vectors')
            exit
         end if
         
         if (tmp(2)%typ > RTYP) then
            G_flagerr = err_t( stat = G_UERROR, &
                                msg = 'variables of "plot" must be integers or reals' )
            exit
         end if
!
!-       and set x = 1:size(y)  (= tmp(1)):
!         
         tmp(1) = pk2f_COLONi ( IONE, n1*m1, IONE )
         if (m1 == 1) tmp(1) = pk2f_RESHAPEnm (tmp(1), [n1,m1])
         
         legend = trim(tokens(1,1)%str)
         
      else if (narg == 2) then
!
!-       Case "plot(x,y)"
!        ---------------

!
!-       Evaluate x (= tmp(1)):
!       
         call pk2Interpreter_driver ( expr     = tokens(1,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      varNames = G_varNames(1:G_nvar), &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )
                                                     
         if (G_flagerr%code > 0) exit

         call pk2_movealloc ( from = valexpr(1), to = tmp(1), stat = G_flagerr )

         if (G_flagerr%code > 0) exit
         
         n1 = tmp(1)%nrow ; m1 = tmp(1)%ncol 
         
         if (n1 /= 1 .and. m1 /= 1) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'variables of "plot" must be vectors')
            exit
         end if
         
         if (tmp(1)%typ > RTYP) then
            G_flagerr = err_t( stat = G_UERROR, &
                                msg = 'variables of "plot" must be integers or reals')
            exit
         end if
!
!-       Evaluate y (= tmp(2)):
!         
         call pk2Interpreter_driver ( expr     = tokens(2,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      varNames = G_varNames(1:G_nvar), &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )
                                                                             
         if (G_flagerr%code > 0) exit

         call pk2_movealloc ( from = valexpr(1), to = tmp(2), stat = G_flagerr )

         if (G_flagerr%code > 0) exit

         n2 = tmp(2)%nrow ; m2 = tmp(2)%ncol 
         
         if (n2 /= 1 .and. m2 /= 1) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'variables of "plot" must be vectors')
            exit
         end if
         
         if (tmp(2)%typ > RTYP) then
            G_flagerr = err_t( stat = G_UERROR, &
                                msg = 'variables of "plot" must be integers or reals')
            exit
         end if
         
         if (n1 * m1 /= n2 * m2) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'incompatible size for "plot"')
            exit
         end if
         
         legend = trim(tokens(2,1)%str)
         
      else
         G_flagerr = err_t( stat = G_UERROR, msg = 'Expression << '//trim(expr)// '>>' // &
                            ' has an error (see the help of "plot")')
         exit
      end if   
      
      if (n2 /= n1) tmp(2) = pk2f_RESHAPEnm (tmp(2), [n1,m1])
!
!-    Write the x and y values in a tmp file (#i):
!      
      write(cnum,'(i0)')i
      tmpfile = trim(gpscript)//'.#'//trim(cnum)
      
      up = util_GetUnit ()
      
      if (up == 0) then
         G_flagerr = err_t (stat = G_IERROR,  msg = HERE // " No free unit available")
         exit
      end if
         
      open(unit = up, file = tmpfile, status = 'replace')

      select type (p=>tmp(1)%m)
         type is (ik2_t) 
            select type (q=>tmp(2)%m)
               type is (ik2_t)
                  if (n1 == 1) then
                     do j = 1, m1
                        write(up,'(2(i0,1x))')p%v(1,j),q%v(1,j)
                     end do
                  else
                     do j = 1, n1
                        write(up,'(2(i0,1x))')p%v(j,1),q%v(j,1)
                     end do
                  end if
               type is (rk2_t) 
                  if (n1 == 1) then
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
            select type (q=>tmp(2)%m)
               type is (ik2_t)
                  if (n1 == 1) then
                     do j = 1, m1
                        write(up,'(g0,1x,i0)')p%v(1,j),q%v(1,j)
                     end do
                  else
                     do j = 1, n1
                        write(up,'(g0,1x,i0)')p%v(j,1),q%v(j,1)
                     end do
                  end if
               type is (rk2_t) 
                  if (n1 == 1) then
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
      if (i == 1) then
         str = 'p "'//trim(tmpfile)
      else
         str = trim(str) // ', "' //trim(tmpfile)
      end if
      
      str = trim(str) // '"'// trim(styl)//' title "'//trim(legend)//'"'
      
   end do
   
   if (G_flagerr%code > 0) then
      close(ugpscript, status = 'delete')
      return
   end if

   write(ugpscript,'(a)')trim(str)   
   close(ugpscript)
!
!- Call now gnuplot:
!
   cmdmsg = ''
   buf = gpcmd//' '//trim(gpscript)//'>&'//trim(gpscript)//'out'
   call execute_command_line (buf, exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg)
                               
   if (exitstat /= 0 .or. cmdstat /= 0) then
      str = 'Problem during gnuplot execution'
      if (len_trim(cmdmsg) > 0) then
         G_flagerr = err_t (stat=G_UERROR, msg = str // ': '//trim(cmdmsg))
      else
         up = util_GetUnit ()
         if (up == 0) then
            G_flagerr = err_t (stat = G_IERROR,  msg = HERE // " No free unit available")
         else
            open(unit = up, file=trim(gpscript)//'out', iostat = err)
            buf = ''
            do
               read(up,'(a)',iostat = err) cmdmsg
               if (err /= 0) exit
               buf = trim(buf) // char(10) // trim(cmdmsg)
            end do
            close(up)
            if (len_trim(buf) > 0) str = str // '. Message returned by gnuplot: '//trim(buf)
            G_flagerr = err_t (stat = G_UERROR, msg = str)
         end if
      end if
   else
      if (.not. G_prompt) write(G_uo,'(/,a,/)') 'Figure saved in: '//trim(gpfig)   
   end if
!
!- Clean up (destroy tmp(:), delete gnuplot script and tmp files):
!
   call tmp(1)%Destroy()
   call tmp(2)%Destroy()
   
   if (G_flagerr%code > 0) return

   up = util_GetUnit ()
   if (up == 0) then
      G_flagerr = err_t (stat = G_IERROR,  msg = HERE // " No free unit available")
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
      
   END SUBROUTINE CalmatCmd_Plot


!=============================================================================================   
   SUBROUTINE CalmatCmd_Plot3d ( expr, flowId, plotout )
!=============================================================================================
   character(len=*), optional, intent(in    ) :: expr 
   integer  (Iprec), optional, intent(in    ) :: flowId
   type     (pk2_t),           intent(   out) :: plotout
!---------------------------------------------------------------------------------------------    
!  Command "plot3d"
!  Very basic way to make plot (gnuplot invocation)
!---------------------------------------------------------------------------------- R.H. 02/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*    ), parameter   :: HERE = 'CalmatCmd_Plot3d'
   character(len=:    ), allocatable :: str, gpscript, gpfig, tmpfile, styl, buf, legend, &
                                        gpcmd, gpterm
   type     (str_t    ), allocatable :: plot(:,:), tokens(:,:), h(:)
   integer  (Iprec    )              :: lp, rp, nplot, ntok, narg, i, j, n1, m1, n2, m2,  &
                                        nl, up, ugpscript, n3, m3
   integer                           :: cmdstat, exitstat, err
   type     (pk2_t    )              :: tmp(3)
   type     (pk2_t    ), allocatable :: valexpr(:)
   character(len=99   )              :: cnum
   character(len=LGSTR)              :: cmdmsg
   logical                           :: surf   
!---------------------------------------------------------------------------------------------  
   integer  (Iprec    ), save        :: ngpfig = 0  
!---------------------------------------------------------------------------------------------    
   
   if (.not. present(expr)) then
      allocate(h(23))
      h( 1)%str = '<< plot3d >> plot a set of points in 3D'
      h( 2)%str = '<< surf >>   plot a surface in 3D'
      h( 3)%str = ' '
      h( 4)%str = 'Syntax: . plot3d ( x1, y1, z1 [,line_spec] [; x2, y2, z2 [,line_spec] )'
      h( 5)%str = '        . surf ( x1, y1, z1 [,line_spec] [; x2, y2, z2 [,line_spec] )'
      h( 6)%str = ' '
      h( 7)%str = "Note: "
      h( 8)%str = "    semicolons (';') are used as delimiters in case of" 
      h( 9)%str = "    several graphs (see example below)"
      h(10)%str = ' '
      h(11)%str = 'Examples:'      
      h(12)%str = '  . plot3d(x1,y1,z1)'
      h(13)%str = '  . plot3d(x1,y1,z1,"-r")'
      h(14)%str = '  . plot3d(x1,y1,z1,"or" ; x2,y2,z2,"xb")'
      h(15)%str = '  . t =0:%pi/50:10*%pi; x = sin(t) ; y = cos(t) ; plot3d(x,y,t,"--c")' 
      h(16)%str = ' '
      h(17)%str = '  . surf(x1,y1,z1)'
      h(18)%str = '  . surf(x1,y1,z1,"-r")'
      h(19)%str = '  . surf(x1,y1,z1,"or" ; x2,y2,z2,"xb")'
      h(20)%str = '  . A = readmat(file="mysurf",nrow=-1,ncol=-1); surf(A(:,1),A(:,2),A(:,3))'
      h(21)%str = ' '
      h(22)%str = '(Note: plots are generated by the command-line program gnuplot. '
      h(23)%str = '       The figures are persistent but not interactive)'
      plotout = h
      return
   end if
   
   surf = .false.
   if (index(expr,'surf') /= 0) surf = .true.
   
   gpcmd  = G_default(3)%str
   gpterm = G_default(4)%str

   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if (lp * rp == 0 .or. rp <= lp) then
      G_flagerr = err_t (stat = G_UERROR, msg = 'Unbalanced parentheses')
      return
   end if
   
   if (rp - lp <= 1) then
      G_flagerr = err_t (stat = G_WARNING, &
                          msg = 'Nothing to plot (at least one argument expected)')
      return
   end if
   
   str = trim(adjustl(expr(lp+1:rp-1))) ! remove parentheses   
!
!- Determine the number of graphs (delimited by a ';'):
!
   nplot = util_CountTokens (str, delims = ';', BlkToken =.false., opcl = '[]()""', &
                             tokens = plot, who = HERE, stat = G_flagerr)
   if (G_flagerr%code > 0) return
!
!- Open a gnuplot script:
!         
   ugpscript = util_GetUnit ()

   if (ugpscript == 0) then
      G_flagerr = err_t (stat = G_IERROR,  msg = HERE // " No free unit available")
      return
   end if
   
   gpscript = '.tmp?_?gpscript?'
   open(unit = ugpscript, file = gpscript, status = 'replace')

   write(ugpscript,'(a)')'reset'
   write(ugpscript,'(a)')'set terminal '//trim(gpterm)//' persist'
   write(ugpscript,'(a)')'set font "Verdana,10"'
   
   if (.not. G_prompt) then
!
!-    in batch mode, save the plot in png file (with a name 'name of the script'_fig#.png)
!
      ngpfig = ngpfig + 1
      write(cnum,'(i0)')ngpfig

      err = 0
      if (present(flowId)) then
         if (flowId > 0 .and. flowId <= G_maxnFlow) then
            gpfig = trim(G_flows(flowId)%fileName)//'_fig'//trim(cnum)//'.png'
         else
            err = 1
         end if
      else 
         err = 1
      end if
      if (err /= 0) then
         G_flagerr = err_t ( stat = G_IERROR, &
                              msg = '(in '//HERE//') Missing flow # or invalid value' )
         close(ugpscript, status = 'delete')
         return
      end if
      
      write(ugpscript,'(a)')"set terminal pngcairo enhanced font 'Verdana,10'"
      write(ugpscript,'(a)')"set output '"//trim(gpfig)//"'"
   end if

   write(ugpscript,'(a)')'set grid'      
!
!- Loop over the n graphs:
!   
   do i = 1, nplot
      
      ntok = util_CountTokens (plot(i,1)%str, delims = ',', BlkToken =.false., opcl = '[]()""', &
                               tokens = tokens, who = HERE, stat = G_flagerr)
                               
      if (G_flagerr%code > 0) exit
!
!-    Determine the line or symbol style:
!         
      if (index(tokens(ntok,1)%str,'"') /= 0) then
         buf = tokens(ntok,1)%str
         narg = ntok - 1         
         nl = len_trim(buf)
         if (buf(1:1) == '"' .and. buf(nl:nl) == '"') then
            buf = buf(2:nl-1) 
            styl = ' w l lw 2 '
               
            if (index(buf,'-' ) /= 0) styl = ' w l lw 2 dt 1'
            if (index(buf,':' ) /= 0) styl = ' w l lw 2 dt 3'
            if (index(buf,'--') /= 0) styl = ' w l lw 2 dt 2'
            if (index(buf,'-.') /= 0) styl = ' w l lw 2 dt 4'
               
            if (index(buf,'o') /= 0) styl = ' w p pt  7 ps 1 '
            if (index(buf,'+') /= 0) styl = ' w p pt  1 ps 2 '
            if (index(buf,'x') /= 0) styl = ' w p pt  2 ps 1 '
            if (index(buf,'*') /= 0) styl = ' w p pt  3 ps 1 '
            if (index(buf,'d') /= 0) styl = ' w p pt 28 ps 1 '
            if (index(buf,'s') /= 0) styl = ' w p pt 35 ps 1 '
            if (index(buf,'v') /= 0) styl = ' w p pt 11 ps 1 '
            if (index(buf,'^') /= 0) styl = ' w p pt  9 ps 1 '
               
            if (index(buf,'b') /= 0) styl = trim(styl) // ' lc rgb "blue" '
            if (index(buf,'r') /= 0) styl = trim(styl) // ' lc rgb "red" '
            if (index(buf,'g') /= 0) styl = trim(styl) // ' lc rgb "green" '
            if (index(buf,'y') /= 0) styl = trim(styl) // ' lc rgb "yellow" '
            if (index(buf,'k') /= 0) styl = trim(styl) // ' lc rgb "black" '
            if (index(buf,'m') /= 0) styl = trim(styl) // ' lc rgb "dark-magenta" '
            if (index(buf,'c') /= 0) styl = trim(styl) // ' lc rgb "dark-cyan" '
            if (index(buf,'w') /= 0) styl = trim(styl) // ' lc rgb "white" '
         else
            G_flagerr = err_t(stat=G_UERROR, msg = 'invalid line spec.')
            exit
         end if      
      else
         styl = ' w l lw 2 '
         narg = ntok
      end if

      if (narg == 3) then
!
!-       Case "plot(x,y,z)"
!        ---------------
!
!-       Evaluate x (= tmp(1)):
!      
         call pk2Interpreter_driver ( expr     = tokens(1,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      varNames = G_varNames(1:G_nvar), &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )

         if (G_flagerr%code > 0) exit

         call pk2_movealloc ( from = valexpr(1), to = tmp(1), stat = G_flagerr )

         if (G_flagerr%code > 0) exit

         n1 = tmp(1)%nrow ; m1 = tmp(1)%ncol 
         
         if (n1 /= 1 .and. m1 /= 1) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'variables of "plot" must be vectors')
            exit
         end if
         
         if (tmp(1)%typ > RTYP) then
            G_flagerr = err_t( stat = G_UERROR, &
                                msg = 'variables of "plot" must be integers or reals')
            exit
         end if
!
!-       Evaluate y (= tmp(2)):
!
         call pk2Interpreter_driver ( expr     = tokens(2,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      varNames = G_varNames(1:G_nvar), &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )
          
         if (G_flagerr%code > 0) exit

         call pk2_movealloc ( from = valexpr(1), to = tmp(2), stat = G_flagerr )

         if (G_flagerr%code > 0) exit
         
         n2 = tmp(2)%nrow ; m2 = tmp(2)%ncol 
         
         if (n2 /= 1 .and. m2 /= 1) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'variables of "plot" must be vectors')
            exit
         end if
         
         if (tmp(2)%typ > RTYP) then
            G_flagerr = err_t( stat = G_UERROR, &
                                msg = 'variables of "plot" must be integers or reals')
            exit
         end if
         
         if (n1 * m1 /= n2 * m2) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'incompatible size for "plot"')
            exit
         end if
!
!-       Evaluate z (= tmp(3)):
!
         call pk2Interpreter_driver ( expr     = tokens(3,1)%str     , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      varNames = G_varNames(1:G_nvar), &
                                      valexpr  = valexpr             , &
                                      flagerr  = G_flagerr           )
         
         if (G_flagerr%code > 0) exit

         call pk2_movealloc ( from = valexpr(1), to = tmp(3), stat = G_flagerr )

         if (G_flagerr%code > 0) exit

         n3 = tmp(3)%nrow ; m3 = tmp(3)%ncol 
         
         if (n3 /= 1 .and. m3 /= 1) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'variables of "plot" must be vectors')
            exit
         end if
         
         if (tmp(3)%typ > RTYP) then
            G_flagerr = err_t( stat = G_UERROR, &
                                msg = 'variables of "plot" must be integers or reals')
            exit
         end if
         
         if (n1 * m1 /= n3 * m3) then
            G_flagerr = err_t(stat = G_UERROR, msg = 'incompatible size for "plot"')
            exit
         end if
         
         legend = trim(tokens(3,1)%str)
         
      else
         G_flagerr = err_t( stat = G_UERROR, msg = 'Expression << '//trim(expr)// '>>' // &
                            ' has an error (see the help of "plot")')
         exit
      end if   
      
      if (n2 /= n1) tmp(2) = pk2f_RESHAPEnm (tmp(2), [n1,m1])
      if (n3 /= n1) tmp(3) = pk2f_RESHAPEnm (tmp(3), [n1,m1])
      
      if (surf) then
         write(ugpscript,'(a,i0,a,i0,a)')'set dgrid3d ',50,',',50,', 3'
         write(ugpscript,'(a)')'set contour'
      end if   
!
!-    Write the x, y and z values in a tmp file (#i):
!      
      write(cnum,'(i0)')i
      tmpfile = trim(gpscript)//'.#'//trim(cnum)
      
      up = util_GetUnit ()
      
      if (up == 0) then
         G_flagerr = err_t (stat = G_IERROR,  msg = HERE // " No free unit available")
         exit
      end if

      open(unit = up, file = tmpfile, status = 'replace')
      
      select type (p=>tmp(1)%m)
         type is (ik2_t) 
            select type (q=>tmp(2)%m)
               type is (ik2_t)
                  select type (r=>tmp(3)%m)
                     type is (ik2_t)  !i,i,i
                        if (n1 == 1) then
                           do j = 1, m1
                              write(up,'(3(i0,1x))')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(3(i0,1x))')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                     type is (rk2_t) !i,i,r
                        if (n1 == 1) then
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
                  select type (r=>tmp(3)%m)
                     type is (ik2_t)     !i,r,i
                        if (n1 == 1) then
                           do j = 1, m1
                              write(up,'(i0,1x,g0,1x,i0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(i0,1x,g0,1x,i0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                     type is (rk2_t) !i,r,r
                        if (n1 == 1) then
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
            select type (q=>tmp(2)%m)
               type is (ik2_t)
                  select type (r=>tmp(3)%m)
                     type is (ik2_t)   !r,i,i
                        if (n1 == 1) then
                           do j = 1, m1
                              write(up,'(g0,1x,i0,1x,i0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(g0,1x,i0,1x,i0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                     type is (rk2_t) !r,i,r
                        if (n1 == 1) then
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
                  select type (r=>tmp(3)%m)
                     type is (ik2_t)  !r, r, i
                        if (n1 == 1) then
                           do j = 1, m1
                              write(up,'(g0,1x,g0,1x,i0)')p%v(1,j),q%v(1,j),r%v(1,j)
                           end do
                        else
                           do j = 1, n1
                              write(up,'(g0,1x,g0,1x,i0)')p%v(j,1),q%v(j,1),r%v(j,1)
                           end do
                        end if
                     type is (rk2_t) !r,r,r
                        if (n1 == 1) then
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
!-    Add the corresponding gnuplot command in "str":
!      
      if (i == 1) then
         str = 'sp "'//trim(tmpfile)
      else
         str = trim(str) // ', "' //trim(tmpfile)
      end if
      
      str = trim(str) // '"'// trim(styl)//' title "'//trim(legend)//'"'
      
   end do
   
   if (G_flagerr%code > 0) then
      close(ugpscript, status = 'delete')
      return
   end if
      
   write(ugpscript,'(a)')trim(str)   
   close(ugpscript)
!
!- Call now gnuplot:
!
   cmdmsg = ''
   buf = gpcmd//' '//trim(gpscript)//'>&'//trim(gpscript)//'out'
   call execute_command_line (buf, exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg)
                               
   if (exitstat /= 0 .or. cmdstat /= 0) then
      str = 'Problem during gnuplot execution'
      if (len_trim(cmdmsg) > 0) then
         G_flagerr = err_t (stat=G_UERROR, msg = str // ': '//trim(cmdmsg))
      else
         up = util_GetUnit ()
         if (up == 0) then
            G_flagerr = err_t (stat = G_IERROR,  msg = HERE // " No free unit available")
         else
            open(unit = up, file=trim(gpscript)//'out', iostat = err)
            buf = ''
            do
               read(up,'(a)',iostat = err) cmdmsg
               if (err /= 0) exit
               buf = trim(buf) // char(10) // trim(cmdmsg)
            end do
            close(up)
            if (len_trim(buf) > 0) str = str // '. Message returned by gnuplot: '//trim(buf)
            G_flagerr = err_t (stat = G_UERROR, msg = str)
         end if
      end if
   else
      if (.not. G_prompt) write(G_uo,'(/,a,/)') 'Figure saved in: '//trim(gpfig)   
   end if      
!
!- Clean up (destroy tmp(:), delete gnuplot script and tmp files):
!
   call tmp(1)%Destroy()
   call tmp(2)%Destroy()
   call tmp(3)%Destroy()
   
   if (G_flagerr%code > 0) return

   up = util_GetUnit ()
   if (up == 0) then
      G_flagerr = err_t (stat = G_IERROR,  msg = HERE // " No free unit available")
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
   integer  (Iprec)              :: lstr, err
   type     (str_t), allocatable :: h(:)   
   character(len=:), allocatable :: str
   character(len=9)              :: cnum
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------       
   logical         , save        :: firstime = .true.
   integer  (Iprec), save        :: digmaxdefault
!---------------------------------------------------------------------------------------------    
   
   if (firstime) then
      digmaxdefault = G_digmax ! save the default number
      firstime = .false.
   end if   
   
   if (.not. present(expr)) then
      write(cnum,'(i0)') digmaxdefault
      allocate(h(6))
      h(1) = '<< format >> set the default format for displaying decimal numbers'
      h(2) = ' '
      h(3) = 'Syntax: format(n), format(), format'
      h(4) = ' '
      h(5) = '. format(n): set the number of significant digits of decimal numbers to n'      
      h(6) = '. format() or format: reset the default number of digits (n = '//trim(cnum)//')'      
      formatout = h
      return
   end if

   if (len_trim(expr) > 6) then
      str = trim(adjustl(expr(7:))) ; lstr = len_trim(str)
      
      if (str(1:1) /= '(' .or. str(lstr:lstr) /= ')') then
         G_flagerr = err_t (stat = G_UERROR, msg = "Expression has an error")
         return
      end if 
         
      str = trim(adjustl(str(2:lstr-1))) ! remove parentheses
               
      if (len_trim(str) == 0) then
         G_digmax = digmaxdefault
         return
      end if       
   else
      G_digmax = digmaxdefault
      return
   end if
   
   call pk2Interpreter_driver ( expr     = str                 , &
                                vars     = G_vars(1:G_nvar)    , &
                                varNames = G_varNames(1:G_nvar), &
                                valexpr  = valexpr             , &
                                flagerr  = G_flagerr           )
 
   if (G_flagerr%code < 0) then
      call G_flagerr%display (unit = G_uo)
   else if (G_flagerr%code > 0) then
      return
   end if

   err = 0
   
   select type (p=>valexpr(1)%m)
      type is (ik2_t)
         if (p%nrow == 1 .and. p%ncol == 1) then
            if (p%v(1,1) > 0 .and. p%v(1,1) < 90) then
               G_digmax = p%v(1,1) ; return
            else
               err = -1
            end if 
         else
            err = 1
        end if
      type is (rk2_t)
         if (p%nrow == 1 .and. p%ncol == 1) then
            if (util_IsIntg(p%v(1,1))) then
               if (p%v(1,1) > 0 .and. p%v(1,1) < 90) then
                  G_digmax = int(p%v(1,1),kind=Iprec) ; return
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

   if (err ==-1) then
      G_flagerr = err_t ( stat = G_WARNING, msg = &
                          'Number of digits (n) must be between 1 and 89 in << format(n) >>' )    
   else if (err == 1) then
      G_flagerr = err_t ( stat = G_UERROR, msg = &
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
   integer  (Iprec)              :: lp, rp, ntok, i
   type     (str_t), allocatable :: tokens(:,:), h(:)
   character(len=:), allocatable :: str
   type     (pk2_t)              :: tmp(2)
   type     (pk2_t), allocatable :: valexpr(:)
!---------------------------------------------------------------------------------------------    
   
   if (.not. present(expr)) then
      allocate(h(7))
      h( 1)%str = '<< disp >> display a value '
      h( 2)%str = ' '
      h( 3)%str = 'Syntax: disp ( a [,message] )'
      h( 4)%str = ' '
      h( 5)%str = 'Examples:'      
      h( 6)%str = '  . disp(x1)'
      h( 7)%str = '  . disp(x1,"x1 is: ")'
      dispout = h
      return
   end if
   
   lp = index(expr,'(') ; rp = index(expr,')',back=.true.)
   
   if (lp * rp == 0 .or. rp <= lp) then
      G_flagerr = err_t (stat = G_UERROR, msg = 'Unbalanced parentheses')
      return
   end if
   
   str = trim(adjustl(expr(lp+1:rp-1))) ! remove parentheses   

   ntok = util_CountTokens (str, delims = ',', BlkToken =.false., opcl = '[]()""', &
                            tokens = tokens, who = HERE, stat = G_flagerr)
   if (G_flagerr%code > 0) return

   if (ntok > 2) then
      G_flagerr = err_t( stat = G_UERROR, msg = 'Expression << '//trim(expr)// '>>' // &
                         ' has an error (see the help of "disp")')
      return
   end if
                         
   do i = 1, ntok

      call pk2Interpreter_driver ( expr     = tokens(i,1)%str     , &
                                   vars     = G_vars(1:G_nvar)    , &
                                   !varNames = G_varNames(1:G_nvar), &
                                   valexpr  = valexpr             , &
                                   flagerr  = G_flagerr           )
 
      if (G_flagerr%code < 0) then
         call G_flagerr%display (unit = G_uo)
      else if (G_flagerr%code > 0) then
         return
      end if
      
      call pk2_moveAlloc ( from = valexpr(1), to = tmp(i), stat = G_flagerr )
      
      if (G_flagerr%code > 0) return
      
   end do
      
   if (ntok == 2) then
      
      if (tmp(2)%typ /= STYP) then
         G_flagerr = err_t( stat = G_UERROR, msg = 'Expression << '//trim(expr)// '>>' // &
                          ' has an error (see the help of "disp")')
         return
      end if
      
      select type (p=>tmp(2)%m)
         type is (sk2_t)
            if (allocated(p%v(1,1)%str)) str = p%v(1,1)%str
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
!  Command "quit" or "exit" (quit calmat)
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(3)
!---------------------------------------------------------------------------------------------    
   
   h(1)%str = '<< exit >> or << quit >> quit the program '
   h(2)%str = ' '
   h(3)%str = 'Syntax: exit, quit'
   quitout = h
   
   END SUBROUTINE CalmatCmd_Quit   


!=============================================================================================   
   SUBROUTINE CalmatCmd_For ( forout )
!=============================================================================================   
   type(pk2_t), intent(out) :: forout
!---------------------------------------------------------------------------------------------    
!  Command "for/endfor" (for loop)
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(15)
!---------------------------------------------------------------------------------------------    
   
   h( 1)%str = '<< for >>, << endfor >> keywords used for a for loop construct'
   h( 2)%str = ' '
   h( 3)%str = 'Syntax: for counter = start:end'
   h( 4)%str = '            ....'
   h( 5)%str = '        endfor'
   h( 6)%str = ' '      
   h( 7)%str = '        for counter = start:step:end'
   h( 8)%str = '            ....'
   h( 9)%str = '        endfor'
   h(10)%str = ' '   
   h(11)%str = 'Example:'      
   h(12)%str = '  x = 0;'
   h(13)%str = '  for i = 1:2:100'
   h(14)%str = '      x = x + i;'
   h(15)%str = '  endfor'   
   forout = h
   
   END SUBROUTINE CalmatCmd_For
   

!=============================================================================================   
   SUBROUTINE CalmatCmd_If ( ifout )
!=============================================================================================   
   type(pk2_t), intent(out) :: ifout
!---------------------------------------------------------------------------------------------    
!  Command "if-then/elseif-then/else/endif"
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(23)
!---------------------------------------------------------------------------------------------    
   
   h( 1)%str = '<< if >>, << then >>, << elseif >>, << else >>, << endif >> keywords'
   h( 2)%str = 'used for an if construct (conditional execution)'
   h( 3)%str = ' '
   h( 4)%str = 'Syntax: if condition then'
   h( 5)%str = '            ....'
   h( 6)%str = '        endif'
   h( 7)%str = ' '      
   h( 8)%str = '        if condition then'
   h( 9)%str = '            ....'
   h(10)%str = '        else'
   h(11)%str = '            ....'
   h(12)%str = '        endif'
   h(13)%str = ' '         
   h(14)%str = '        if condition1 then'
   h(15)%str = '            ....'
   h(16)%str = '        elseif condition2 then'
   h(17)%str = '            ....'
   h(18)%str = '        ....'
   h(19)%str = '        elseif conditionN then'
   h(20)%str = '            ....'
   h(21)%str = '        else'
   h(22)%str = '            ....'
   h(23)%str = '        endif'
   ifout = h
   
   END SUBROUTINE CalmatCmd_If


!=============================================================================================   
   SUBROUTINE CalmatCmd_Break ( breakout )
!=============================================================================================   
   type(pk2_t), intent(out) :: breakout
!---------------------------------------------------------------------------------------------    
!  Command "break" (exit from a for loop)
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(13)
!---------------------------------------------------------------------------------------------    
   
   h( 1)%str = '<< break >> keyword used to terminate a for loop '
   h( 2)%str = ' '
   h( 3)%str = 'Syntax: break'
   h( 4)%str = ' '   
   h( 5)%str = 'Example:'      
   h( 6)%str = '  x = 0;'
   h( 7)%str = '  for i = 1:100'
   h( 8)%str = '      x = x + 1.5*rand();'
   h( 9)%str = '      if x > 3 then'
   h(10)%str = '         disp(i)'
   h(11)%str = '         break'
   h(12)%str = '      endif'
   h(13)%str = '  endfor'   
   breakout = h
   
   END SUBROUTINE CalmatCmd_Break
   
       
END MODULE CalmatCmd
