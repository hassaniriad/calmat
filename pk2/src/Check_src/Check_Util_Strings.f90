! For compiling (or use the makefile):

! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran Check_Util_Strings.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_Util_Strings

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort Check_Util_Strings.f90 -L../../lib/ifort -lpk2 -llapack -lblas -o check_Util_Strings

! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor Check_Util_Strings.f90  -L../../lib/nagfor -lpk2 -llapack -lblas -o check_Util_Strings

program Check_Util_Strings

   use pk2mod_m
   
   implicit none
   
!---------------------------------------------------------------------------      
   type(err_t) :: stat ! for error/warning report   
!---------------------------------------------------------------------------   

   call SignalHandler_SignalCatch (unit = STDOUT, title = '--> Check_Util_Strings:')
!   
!- disable automatic stop when an error occurs:
!
   call err_SetHaltingMode ( halting = .false. )
      
   call test_util_ScrollingMessage (1_Ikind)
   print*,'return to continue'; read*

   call test_util_GetLine
   print*,'return to continue'; read*

   call test_util_FindSubstring1
   print*,'return to continue'; read*
   
   call test_util_CountTokens
   print*,'return to continue'; read*

   call test_util_ReplaceSubstring1
   print*,'return to continue'; read*

   call test_util_ReplaceSubstring2
   print*,'return to continue'; read*

   call test_util_IsBalanced1
   print*,'return to continue'; read*

   call test_util_IsBalanced2
   print*,'return to continue'; read*

   call test_util_IsEnclosed
   print*,'return to continue'; read*

   call test_util_SplitString1
   print*,'return to continue'; read*

   call test_util_SplitString2
   print*,'return to continue'; read*

   call test_util_FirstOccurence
   print*,'return to continue'; read*

   call test_util_RemoveSpaces1
   print*,'return to continue'; read*

   call test_util_RemoveSpaces2
   print*,'return to continue'; read*

   call test_util_RemoveZeros
   print*,'return to continue'; read*

   call test_util_StringLow
   print*,'return to continue'; read*

   call test_util_StringLow2
   print*,'return to continue'; read*
   
   call test_util_StringCap
   print*,'return to continue'; read*

   call test_util_StringCap2
   print*,'return to continue'; read*

   call test_util_BarreDefilement   
   print*,'return to continue'; read*
   
   call test_util_ScrollingMessage (2_Ikind)   

   call stat%Destroy()

contains

!=============================================================================================
   SUBROUTINE test_util_ScrollingMessage (num)
!=============================================================================================
   integer(Ikind) :: num
   type   (str_t) :: msg(3)
   integer(Ikind) :: i, n
   real           :: delay
   
   if (num == 1) then
      print*
      print*,'************* Begin test_util_ScrollingMessage *************'
      print*
      msg(1)%str = "-> Hello world! welcome to this test program"
      msg(2)%str = "-> We first test the routine util_ScrollingMessage"
      msg(3)%str = "-> which shows a scrolling message"
      n = 3
      delay = 0.1
   else
      msg(1)%str = "-> Tests terminated"
      msg(2)%str = "-> Exiting now ......            G O O D B Y E !           "
      n = 2
      delay = 0.4
   end if   

   
   do i = 1, n
      call util_ScrollingMessage ( msg(i)%str, delay )
   end do 

   print*
   if (num == 1) print*,'-------------  End test_util_ScrollingMessage  -------------'  
   print*
   
   END SUBROUTINE test_util_ScrollingMessage  

!=============================================================================================
   SUBROUTINE test_util_BarreDefilement 
!=============================================================================================

   integer :: j, n, di
   integer(c_int32_t) :: delay = 10000_c_int32_t
   
   interface ! found in http://computer-programming-forum.com (by Tobias Burnu)
      subroutine usleep (useconds) bind(C)
         use iso_c_binding
         implicit none
         integer(c_int32_t), value :: useconds
      end subroutine
   end interface   

   print*
   print*,'************* Begin test_util_BarreDefilement *************'
   print*   
   
   n = 100 ; di = 5
   
   do j = 1, n-1
      call usleep ( delay )
      call util_BarreDefilement ( j, n, di, cDone = "=", cRemain = "o", nsymb = 35, &
                                  sLead = "(a 1st bar)", sTrail = "... in progress" )
   end do
   call util_BarreDefilement ( n, n, di, sTrail = " Done" )

   print*
   
   do j = 1, n-1
      call usleep ( 2*delay )
      call util_BarreDefilement ( j, n, di, cDone = "X", cRemain = "-", nsymb = 45, &
                                  sLead = "(a 2nd one)", sTrail = "... in progress" )
   end do
   call util_BarreDefilement ( n, n, di, sTrail = " Done" )
   
   print*
   print*,'-------------  End test_util_BarreDefilement  -------------'  
   print*
   
   END SUBROUTINE test_util_BarreDefilement  

!=============================================================================================
   SUBROUTINE test_util_GetLine 
!=============================================================================================

   integer  (Ikind)              :: unit = 9
   character(len=:), allocatable :: line

   open(unit,file='truc') ; write(unit,*)'hello world' ; close(unit)
   
   print*
   print*,'************* Begin test_util_GetLine *************'
   print*   
   
   open(unit,file='truc')

   line = util_GetLine (unit = unit, stat = stat)
   
   print*,'The line read on the unit 9 (file truc) is: ',line

   print*
   call stat%display(STDOUT,verb=IONE)
   print*
      
   print*
   print*,'-------------  End test_util_GetLine  -------------'  
   print*
   
   close(unit,status='delete')
   
   END SUBROUTINE test_util_GetLine  

!=============================================================================================
   SUBROUTINE test_util_FindSubstring1
!=============================================================================================

   integer  (Ikind)              :: lind, rind
   character(len=:), allocatable :: str, substr
   integer  (Ikind)              :: n
   integer  (Ikind), allocatable :: loc(:)

   print*
   print*,'************* Begin test_util_FindSubstring1 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   
   

   str = 'De gustibus et coloribus non est disputandum'
   substr = 'us'
   
   lind = 1 ; rind = len(str)
   
   write(*,'(a)')"- Find all occurences of the substring <<"//substr//">> in the string:"
   write(*,'(a)')"   <<"//str//">>"
   write(*,'(a,1x,i0,1x,a,1x,i0,a)')"  that lies between the positions ",lind," and ",rind,": "
   
   call util_FindSubstring1 (loc, n, str, lind, rind, substr, stat)  
   
   print*
   call stat%display(STDOUT,verb=IONE)
   print*
      
   if (n /= 0) then
      write(*,'(a,10(i0,1x))')"  Found positions: ",loc
   else
      print*,"  No occurence"
   end if      

!--------------------------------   
   print*
   lind = 30 ; rind = 2
   write(*,'(a)')"- Find all occurences of the substring <<"//substr//">> in the string:"
   write(*,'(a)')"   <<"//str//">>"
   write(*,'(a,1x,i0,1x,a,1x,i0,a)')"  that lies between the positions ",lind," and ",rind, &
   ansiColor_colorTxt(int(STDOUT),ansiColor_getAnsiCode(err_colorError)," (INTENTIONAL ERROR)")   
   call util_FindSubstring1 (loc, n, str, lind, rind, substr, stat)  
   print*

   call stat%display(STDOUT,verb=IONE)
   print*
      
   print*
   print*,'-------------  End test_util_FindSubstring1  -------------'  
   print*

   END SUBROUTINE test_util_FindSubstring1

!=============================================================================================
   SUBROUTINE test_util_CountTokens
!=============================================================================================

   character(len=*), parameter   :: HERE = 'test_util_CountTokens'
   character(len=:), allocatable :: str, dlm, opcl
   type     (str_t), allocatable :: tok(:,:)
   integer  (Ikind)              :: i, n
   logical                       :: Blk
   
   print*
   print*,'************* Begin test_util_CountTokens *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   opcl = ''
   str = ' 11 22 33 44 55  ' ; dlm = ' ' 

   print*,'Count in a string the number of tokens delimited by a given set of separators'

   n = util_CountTokens (str, delims=dlm, BlkToken=.false., tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>> (none)'
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   opcl = '""()[]'//"''" ! do not search delimiters enclosed between any pair of these symboles
   
   str = '  1   2.2   (3 , 3)    [.false. ,  .true.]   "array name"     ' ; dlm = ' ' 
   n = util_CountTokens (str, delims=dlm, BlkToken=.true., opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*
   
   Blk = .false.
   str = '  1 ,  2.2  , (3 , 3),  ,   [.false. ,  .true.] ,,   "array name"  ,  ' ; dlm = ',' 
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   Blk = .true.
   str = '  1 ,  2.2  , (3 , 3),  ,   [.false. ,  .true.] ,,   "array name"  ,  ' ; dlm = ',' 
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   Blk = .true.
   str = '  1 ,  2.2  , (3 , 3),  ,   [.false. ,  .true.] ,,   "array name"  ,' ; dlm = ',' 
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   Blk = .true.
   str = '  1 ,  2.2  , (3 , 3),  ,   [.false. ,  .true.] ,,   "array name"  , ' ; dlm = ' ' 
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   Blk = .false.
   str = '  1   2.2;, (3 , 3)     [.false. ,  .true.] ;   "array name"   ' ; dlm = ' ,;' 
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)
   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   Blk = .false.
   str = ' a = 11 , A=read("truc.txt",fmt="ascii") ; b = a + 33,; c = 44, d = [1,2;3,4] ; e=9.81' ; dlm = ',;' 
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   Blk = .true.
   str = ' X1, X2 ,,  ,,, X3, X4, X5 ' ; dlm = ',;'
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   Blk = .false.
   str = 'u = [1,5;3,4] ; exec("truc/machin") , i=0' ; dlm = ',;'
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*

   Blk = .false.
   str = "u = [1,5;3,4]' ; exec(truc/machin) , i=0" ; dlm = ',;' ; opcl = '""()[]'
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*   

   Blk = .false.
   str = "truc(machin(x),1)" ; dlm = ',' ; opcl = '()'
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)
   
   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*   

   Blk = .false.
   str = 'truc(name="machin(x,0)",opt="val", n=3), u = [[5,4];a;[-1,0]], h="hello"'; dlm = ',;'
   opcl = '""()[]'
   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)

   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do
   print*   
   
   print*,'The following normally PRODUCES A ' // &
           ansiColor_colorTxt(int(STDOUT),ansiColor_getAnsiCode(err_colorWarning),"WARNING") // &               
           ' (unbalanced parenthesis):'
   Blk = .false.
   str = 'a(=1'; dlm = '=' ; opcl = '""()[]'

   n = util_CountTokens (str, delims=dlm, BlkToken=Blk, opcl=opcl, tokens=tok, stat=stat)
                                
   print*
   print '(a)','. The string is : <<'//str//'>>'
   print '(a)','. The set of delimiters is: <<'//dlm//'>>'
   print '(a)','. The pairs of symbols where delimiters are not sought.: <<'//opcl//'>>'  
   print '(a,g0)','. Tokens may be blank?: ',Blk
   if (dlm == ' ' .and. Blk) then
      print '(a)',&
      '  (regardless of its value, it will not be considered since the delimiter is itself blank)'
   end if
   print '(a,i0)','. The number of tokens is: ',n
   print '(a)','. The tokens are: '
   do i = 1, n
      print '(a,i0,a)','  #',i, &
            ': <<'//tok(i,1)%str//'>> (followed by the separator : <<'//tok(i,2)%str//'>>)'
   end do

   call stat%display(STDOUT,verb=IONE)
   
      
   END SUBROUTINE test_util_CountTokens
   
!=============================================================================================
   SUBROUTINE test_util_ReplaceSubstring1
!=============================================================================================

   character(len=:), allocatable :: str, rem, rep

   print*
   print*,'************* Begin test_util_ReplaceSubstring1 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   
   

   str = ' sin(a*x) + f(sin(x)) + sin(x)**2 = 0 '
   rem = 'sin'
   rep = 'sind'
   
   write(*,'(a)')"- Replace in the string <<"//str//">>"
   write(*,'(a)')"  all occurences of the substring <<" // rem // &
                     ">> by the substring <<"//rep//">>"
   
   str = util_ReplaceSubstring1 ( str, rem, rep )  
   
   write(*,'(a)')"  The new string is: <<"//trim(str)//">>"

   write(*,'(/,a)')"- Then delete all <<*>> (replace them by <<>>)"
   str = util_ReplaceSubstring1 ( str, '*', '' )  
   write(*,'(a)')"  The new string is: <<"//trim(str)//">>"

   write(*,'(/,a)')"- Restore <<sin>> in place of <<sind>>"
   str = util_ReplaceSubstring1 ( str, rep, rem )  
   write(*,'(a)')"  The new string is: <<"//trim(str)//">>"
   
   print*
   call stat%display(STDOUT,verb=IONE)
   print*
         
   print*
   print*,'-------------  End test_util_ReplaceSubstring1  -------------'  
   print*
   
   END SUBROUTINE test_util_ReplaceSubstring1     
      
!=============================================================================================
   SUBROUTINE test_util_ReplaceSubstring2
!=============================================================================================

   character(len=:), allocatable :: str, rem, rep, opcl

   print*
   print*,'************* Begin test_util_ReplaceSubstring2 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   
   

   str = ' x=[1;2;3] ; y=5 ; txt = "Hello world! Have a nice day ; Bye" ; z = 8 ;'
   rem = ';'
   rep = ','
   opcl = '[]""'
   
   write(*,'(a)')"- Replace in the string <<"//str//">>"
   write(*,'(a)')"  any occurence of the substring <<" // rem // &
                    ">> by the substring <<"//rep//">> provided they are not enclosed by one"
   write(*,'(a)')"  of these opening/closing symbol pairs: "//opcl
     
   str = util_ReplaceSubstring2 ( str, rem, rep, opcl, stat )   
   
   write(*,'(a)')"  The new string is: << "//trim(str)//" >>"
   
   print*
   call stat%display(STDOUT,verb=IONE)
   print*
         
   print*
   print*,'-------------  End test_util_ReplaceSubstring2  -------------'  

   print*
   
   END SUBROUTINE test_util_ReplaceSubstring2 
      

!=============================================================================================
   SUBROUTINE test_util_IsBalanced1
!=============================================================================================

   character(len=:), allocatable :: str
   character(len=1)              :: op, cl
   logical                       :: is_bal

   print*
   print*,'************* Begin test_util_IsBalanced1 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str = '( sin(cos(f(h(x)))) + 4*(a*(b*(c+d))) ) * ( f(g(x)) + k )'
   op = '(' 
   cl = ')'
         
   write(*,'(a)')"- See if the opening symbols <<"//op//            &
                 ">> are balanced by the closing symbols <<"//cl//  &
                 ">> in the string:"
   write(*,'(a)')"   <<"//str//">>"
      
   is_bal = util_IsBalanced1 ( str, op, cl ) 

   print*
   if (is_bal) then
      write(*,'(a)')'  Yes'
   else
      write(*,'(a)')'  No'
   end if            

!--------------------------------
   print*
   str = " 'hello', he said. A = B' "
   op = "'" 
   cl = "'"

   write(*,'(a)')"- See if the opening symbols <<"//op//            &
                 ">> are balanced by the closing symbols <<"//cl//  &
                 ">> in the string:"
   write(*,'(a)')"   <<"//str//">>"
     
   is_bal = util_IsBalanced1 ( str, op, cl ) 

   print*
   if (is_bal) then
      write(*,'(a)')'  Yes'
   else
      write(*,'(a)')'  No'
   end if            
      
   print*
   print*,'------------  End test_util_IsBalanced1  --------------'  
   print*
   
   END SUBROUTINE test_util_IsBalanced1


!=============================================================================================
   SUBROUTINE test_util_IsBalanced2
!=============================================================================================

   character(len=:), allocatable :: str, opcl
   logical                       :: is_bal

   print*
   print*,'************* Begin test_util_IsBalanced2 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str = '[ sin(cos(f(h(x)))) + 4*(a*(b*(c+d))) ] * ( f(g(x)) + k )'
   opcl = '()[]' 
         
   write(*,'(a)')"- See if in the string: <<"//str//">>"
   write(*,'(a)')"  all opening/closing pairs <<"//opcl//">> are balanced"
      
   is_bal = util_IsBalanced2 ( str, opcl, stat ) 

   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   if (is_bal) then
      write(*,'(a)')'  Yes'
   else
      write(*,'(a)')'  No'
   end if            

!--------------------------------
   print*
   str = " 'hello', he said. {A = B'} "
   opcl = "''{}" 

   write(*,'(a)')"- See if in the string: <<"//str//">>"
   write(*,'(a)')"  all opening/closing pairs <<"//opcl//">> are balanced"
     
   is_bal = util_IsBalanced2 ( str, opcl, stat) 

   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   if (is_bal) then
      write(*,'(a)')'  Yes'
   else
      write(*,'(a)')'  No'
   end if            
      
   print*
   print*,'------------  End test_util_IsBalanced2  --------------'  
   print*
	
   END SUBROUTINE test_util_IsBalanced2


!=============================================================================================
   SUBROUTINE test_util_IsEnclosed
!=============================================================================================

   character(len=:), allocatable :: str, sub1, sub2
   logical                       :: is_enc
   integer  (Ikind)              :: p

   print*
   print*,'************* Begin test_util_IsEnclosed *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str = 'f77 xlf g77 ifort nagfor gfortran flang'
   sub1 = 'xlf'
   sub2 = 'gfortran' 
   p = 13       
         
   write(*,'(a)')"- See if in the string: <<"//str//">>"
   write(*,'(a,i0,a)')"  the ",p,"-th character (i.e. "//str(p:p)// &
                      ") is enclosed between <<"// sub1//">> and <<"//sub2//">>" 
      
   is_enc = util_IsEnclosed ( str, p, sub1, sub2, stat )  

   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   if (is_enc) then
      write(*,'(a)')'  Yes'
   else
      write(*,'(a)')'  No'
   end if            

!--------------------------------
   print*
   str = '((a(b)c)d(e)f)'
   sub1 = '('
   sub2 = ')' 
   p = 9
         
   write(*,'(a)')"- See if in the string: <<"//str//">>"
   write(*,'(a,i0,a)')"  the ",p,"-th character (i.e. "//str(p:p)// &
                      ") is enclosed between <<"// sub1//">> and <<"//sub2//">>" 
      
   is_enc = util_IsEnclosed ( str, p, sub1, sub2, stat )  

   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   if (is_enc) then
      write(*,'(a)')'  Yes'
   else
      write(*,'(a)')'  No'
   end if            

!--------------------------------
   print*
   str = '"abc" + "ef"'
   sub1 = '"'
   sub2 = '"' 
   p = 7
         
   write(*,'(a)')"- See if in the string: <<"//str//">>"
   write(*,'(a,i0,a)')"  the ",p,"-th character (i.e. "//str(p:p)// &
                      ") is enclosed between <<"// sub1//">> and <<"//sub2//">>" 
      
   is_enc = util_IsEnclosed ( str, p, sub1, sub2, stat )  

   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   if (is_enc) then
      write(*,'(a)')'  Yes'
   else
      write(*,'(a)')'  No'
   end if            

!--------------------------------
   print*
   str = '"abc + ef"'
   sub1 = '"'
   sub2 = '"' 
   p = 6
         
   write(*,'(a)')"- See if in the string: <<"//str//">>"
   write(*,'(a,i0,a)')"  the ",p,"-th character (i.e. "//str(p:p)// &
                      ") is enclosed between <<"// sub1//">> and <<"//sub2//">>" 
      
   is_enc = util_IsEnclosed ( str, p, sub1, sub2, stat )  

   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   if (is_enc) then
      write(*,'(a)')'  Yes'
   else
      write(*,'(a)')'  No'
   end if            

      
   print*
   print*,'------------  End test_util_IsEnclosed  --------------'  
   print*
   
   END SUBROUTINE test_util_IsEnclosed


!=============================================================================================
   SUBROUTINE test_util_SplitString1
!=============================================================================================

   character(len=:), allocatable :: str, delim, opcl
   type     (str_t), allocatable :: substr(:)   
   integer  (Ikind)              :: i, n

   print*
   print*,'************* Begin test_util_SplitString1 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str = 'x = 4 ; names = ["camef2018" ; "adeli3p8" ; "flexlit"] ; pi = acos(-1)'
   opcl = '""'//"''[]"
   delim = ";"
         
   write(*,'(a,/)')"- Split the string: "
   write(*,'(a,/)')"    <<"//str//">>"
   write(*,'(a)')"  if formed of multiple parts separated by the delimiter <<"// &
                  delim//">>. Do not considere"
   write(*,'(a)')"  as delimiter those enclosed by one of the opening/closing pairs: "//opcl
      
   call util_SplitString1 ( str, delim, opcl, substr, stat, n )
   
   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   write(*,'(a,i0,a)')"  Splitted in the following ",n," sub-strings:"
   do i = 1, n
      write(*,'(a)')"   . <<"//substr(i)%str//">>"
   end do

!--------------------------------   
   print*
   str = 'a = myfile, b = [1,2;3,4], c = mesh(file=dmesh, fmt="gmsh")'
   opcl = '""'//"''[]()"
   delim = ","
         
   write(*,'(a,/)')"- Split the string: "
   write(*,'(a,/)')"    <<"//str//">>"
   write(*,'(a)')"  if formed of multiple parts separated by the delimiter <<"// &
                  delim//">>. Do not considere"
   write(*,'(a)')"  as delimiter those enclosed by one of the opening/closing pairs: "//opcl
      
   call util_SplitString1 ( str, delim, opcl, substr, stat, n )
   
   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   write(*,'(a,i0,a)')"  Splitted in the following ",n," sub-strings:"
   do i = 1, n
      write(*,'(a)')"   . <<"//substr(i)%str//">>"
   end do
      
   print*
   print*,'------------  End test_util_SplitString1  --------------'  
   print*
   
   END SUBROUTINE test_util_SplitString1


!=============================================================================================
   SUBROUTINE test_util_SplitString2
!=============================================================================================

   character(len=:), allocatable :: str, delims, opcl
   type     (str_t), allocatable :: substr(:)   
   integer  (Ikind), allocatable :: locsep(:)   
   integer  (Ikind)              :: i, n

   print*
   print*,'************* Begin test_util_SplitString2 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str = 'x = 4 ; names = ["camef2018" ; "adeli3p8" ; "flexlit"] , pi = acos(-1)'
   opcl = '""'//"''[]"
   delims = ";,"
         
   write(*,'(a,/)')"- Split the string: "
   write(*,'(a,/)')"    <<"//str//">>"
   write(*,'(a)')"  if formed of multiple parts separated by the delimiter(s) <<"//delims// &
                 ">>. Do not considere"
   write(*,'(a)')"  as delimiter those enclosed by one of the opening/closing pairs: "//opcl
      
   call util_SplitString2 ( str, delims, opcl, substr, locsep, stat, n )      

   print*
   call stat%display(STDOUT,verb=1_Ikind)
   print*
   
   write(*,'(a,i0,a)')"  Splitted in the following ",n," sub-strings:"
   do i = 1, n
      write(*,'(a)')"   . <<"//substr(i)%str//">>"
   end do

   print*
   print*,'------------  End test_util_SplitString2  --------------'  
   print*

   END SUBROUTINE test_util_SplitString2


!=============================================================================================
   SUBROUTINE test_util_FirstOccurence
!=============================================================================================

   character(len=:), allocatable :: str, chars
   character(len=1)              :: op, cl
   integer  (Ikind)              :: p

   print*
   print*,'************* Begin test_util_FirstOccurence *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str  = '(a + b) - (c + f) + g' 
   chars = '+-'
   op = '(' ; cl = ')'
         
   write(*,'(a)')"- Return the position in the string <<"//str//">>"
   write(*,'(a)')"  of the first occurence of any of the characters <<"//chars//">> that"
   write(*,'(a)')"  are not enclosed between symbols <<"//op//& 
                 ">> (at left) and <<"//cl//">> (at right):"
      
   p = util_FirstOccurence ( str, chars, op, cl, stat, .false. )

   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   if (p /= 0) then
      write(*,'(a,i0)')"  . the position is: ",p
      write(*,'(a)'   )"  . the character at this position is: <<"//str(p:p)//">>"
      write(*,'(a)'   )"  . the left part of the string is: <<"//str(:p-1)//">>"
      write(*,'(a)'   )"  . the right part of the string is: <<"//str(p+1:)//">>"
   else
      write(*,'(a)')'none'
   end if


!--------------------------------   
   print*
   str  = '((a+b)-(c+f))( + g)' 
   chars = '+-'
   op = '(' ; cl = ')'
         
   write(*,'(a)')"- Return the position in the string <<"//str//">>"
   write(*,'(a)')"  of the first occurence of any of the characters <<"//chars//">> that"
   write(*,'(a)')"  are not enclosed between symbols <<"//op//& 
                 ">> (at left) and <<"//cl//">> (at right):"
      
   p = util_FirstOccurence ( str, chars, op, cl, stat, .false. )

   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   if (p /= 0) then
      write(*,'(a,i0)')"  . the position is: ",p
      write(*,'(a)'   )"  . the character at this position is: <<"//str(p:p)//">>"
      write(*,'(a)'   )"  . the left part of the string is: <<"//str(:p-1)//">>"
      write(*,'(a)'   )"  . the right part of the string is: <<"//str(p+1:)//">>"
   else
      write(*,'(a)'   )"   none"
   end if

!--------------------------------   
   print*
   str  = '(a + b) - (c + f) + g - k*lambda' 
   chars = '+-'
   op = '(' ; cl = ')'
         
   write(*,'(a)')"- Return the position in the string <<"//str//">>"
   write(*,'(a)')"  of the LAST occurence of any of the characters <<"//chars//">> that"
   write(*,'(a)')"  are not enclosed between symbols <<"//op//& 
                 ">> (at left) and <<"//cl//">> (at right) and :"
      
   p = util_FirstOccurence ( str, chars, op, cl, stat, .true. )

   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   if (p /= 0) then
      write(*,'(a,i0)')"  . the position is: ",p
      write(*,'(a)'   )"  . the character at this position is: <<"//str(p:p)//">>"
      write(*,'(a)'   )"  . the left part of the string is: <<"//str(:p-1)//">>"
      write(*,'(a)'   )"  . the right part of the string is: <<"//str(p+1:)//">>"
   else
      write(*,'(a)')'none'
   end if

   print*
   print*,'------------  End test_util_FirstOccurence  --------------'  
   print*

   END SUBROUTINE test_util_FirstOccurence


!=============================================================================================
   SUBROUTINE test_util_RemoveSpaces1
!=============================================================================================

   character(len=:), allocatable :: str

   print*
   print*,'************* Begin test_util_RemoveSpaces1 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str  = '(a + b) - (c + f) + g' 
   write(*,'(a)')" Remove spaces, tabs, and control characters in the string <<"//str//">>"

   str = util_RemoveSpaces1 ( str )
   print*

   write(*,'(a)')" The new string is: <<"//trim(str)//">>"
      
   print*
   print*,'------------  End test_util_RemoveSpaces1  --------------'  
   print*

   END SUBROUTINE test_util_RemoveSpaces1


!=============================================================================================
   SUBROUTINE test_util_RemoveSpaces2
!=============================================================================================

   character(len=:), allocatable :: str

   print*
   print*,'************* Begin test_util_RemoveSpaces2 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str  = 'file="/hassani/m.in", F = "x + sin(y) * a", t = '//"'   foot ' + ' ball'"
   write(*,'(a)')"- Remove spaces, tabs, and control characters in the string:"
   write(*,'(/,a,/)')"     <<"//str//">>"
   write(*,'(a)')"  only if they are not in a part enclosed between simple quotation marks"
   write(*,'(a)')"  (i.e. parts enclosed between quotes (') are not modified)"

   str = util_RemoveSpaces2 ( str, stat, 'nsqt' )
   
   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   write(*,'(a)')"  The new string is: <<"//trim(str)//">>"

!--------------------------------   
   print*
   str  = 'file="/hassani/m.in", F = "x + sin(y) * a", t = '//"'   foot ' + ' ball'"
   write(*,'(a)')"- Remove spaces, tabs, and control characters in the string:"
   write(*,'(/,a,/)')"     <<"//str//">>"
   write(*,'(a)')"  only if they are not in a part enclosed between double quotation marks"
   write(*,'(a)')'  (i.e. parts enclosed between quotes (") are not modified)'

   str = util_RemoveSpaces2 ( str, stat, 'ndqt' )
   
   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   write(*,'(a)')"  The new string is: <<"//trim(str)//">>"

!--------------------------------   
   print*
   str  = 'file="/hassani/m.in", F = "x + sin(y) * a", t = '//"'   foot ' + ' ball'"
   write(*,'(a)')"- Remove spaces, tabs, and control characters in the string:"
   write(*,'(/,a,/)')"     <<"//str//">>"
   write(*,'(a)')"  only if they are not in a part enclosed between simple or double quotation"
   write(*,'(a)')'  marks (i.e. parts enclosed between quotes (" or '//"') are not modified)"

   str = util_RemoveSpaces2 ( str, stat, 'nqt' )
   
   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   write(*,'(a)')"  The new string is: <<"//trim(str)//">>"
      
   print*
   print*,'------------  End test_util_RemoveSpaces2  --------------'  
   print*

   END SUBROUTINE test_util_RemoveSpaces2


!=============================================================================================
   SUBROUTINE test_util_RemoveZeros
!=============================================================================================

   character(len=:), allocatable :: str
   integer  (Ikind)              :: nd

   print*
   print*,'************* Begin test_util_RemoveZeros *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str  = '.0000'
   write(*,'(a)')"- Compactes the string-number <<"//str//">> by removing trailing zeros"

   str = util_RemoveZeros ( strnum=str, stat = stat )  
    
   print*
   call stat%display(STDOUT,verb=IONE)
   print*

   write(*,'(a)')"  The new string-number is: <<"//trim(str)//">>"

!--------------------------------   
   print*
   str  = '123.000'
   write(*,'(a)')"- Compactes the string-number <<"//str//">> by removing trailing zeros"

   str = util_RemoveZeros ( strnum=str, stat = stat )  
    
   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   write(*,'(a)')"  The new string-number is: <<"//trim(str)//">>"

!--------------------------------   
   print*
   str  = '123.4500000'
   write(*,'(a)')"- Compactes the string-number <<"//str//">> by removing trailing zeros"

   str = util_RemoveZeros ( strnum=str, stat = stat )  
    
   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   write(*,'(a)')"  The new string-number is: <<"//trim(str)//">>"

!--------------------------------   
   print*
   str  = '123.450000e+006'
   write(*,'(a)')"- Compactes the string-number <<"//str//">> by removing trailing zeros"

   str = util_RemoveZeros ( strnum=str, stat = stat )  
    
   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   write(*,'(a)')"  The new string-number is: <<"//trim(str)//">>"

!--------------------------------   
   print*
   str  = '123.450006700D-089'
   nd = 2
   write(*,'(a)')"- Compactes the string-number <<"//str//">> by removing trailing zeros"
   write(*,'(a,i0,a)')"  and ignore the decimals that are beyond the ",nd+1,"-th decimal"
   
   str = util_RemoveZeros ( strnum=str, ndecimals = nd, stat = stat )  
    
   print*
   call stat%display(STDOUT,verb=IONE)
   print*
   
   write(*,'(a)')"  The new string-number is: <<"//trim(str)//">>"

   print*
   print*,'------------  End test_util_RemoveZeros  --------------'  
   print*

   END SUBROUTINE test_util_RemoveZeros


!=============================================================================================
   SUBROUTINE test_util_StringLow
!=============================================================================================

   character(len=:), allocatable :: str

   print*
   print*,'************* Begin test_util_StringLow *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str  = 'De Gustibus Et Coloribus Non Est Disputandum'
   write(*,'(a)')" Lowercase the string: <<"//str//">>:"

   str = util_StringLow ( str )  
    
   print*
   write(*,'(a)')" The new string-number is: <<"//trim(str)//">>"

   print*
   print*,'------------  End test_util_StringLow  --------------'  
   print*


   END SUBROUTINE test_util_StringLow
   
   
!=============================================================================================
   SUBROUTINE test_util_StringLow2
!=============================================================================================

   character(len=:), allocatable :: str

   print*
   print*,'************* Begin test_util_StringLow2 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str  = 'De '//"'Gustibus'"//' Et "Coloribus" Non Est '//"'Disputandum'"
   write(*,'(a)')" Lowercase the parts of the string: <<"//str//">>"
   write(*,'(a)')" that are not between simple or double quotes"

   str = util_StringLow2 ( str, stat, 'nqt' )  
    
   print*
   write(*,'(a)')" The new string-number is: <<"//trim(str)//">>"

   print*
   print*,'------------  End test_util_StringLow2  --------------'  
   print*


   END SUBROUTINE test_util_StringLow2   


!=============================================================================================
   SUBROUTINE test_util_StringCap
!=============================================================================================

   character(len=:), allocatable :: str

   print*
   print*,'************* Begin test_util_StringCap *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str  = 'De Gustibus Et Coloribus Non Est Disputandum'
   write(*,'(a)')" Capitalize the string: <<"//str//">>:"

   str = util_StringCap ( str )  
   
   print*
   write(*,'(a)')" The new string-number is: <<"//trim(str)//">>"

   print*
   print*,'------------  End test_util_StringCap  --------------'  
   print*

   END SUBROUTINE test_util_StringCap


!=============================================================================================
   SUBROUTINE test_util_StringCap2
!=============================================================================================

   character(len=:), allocatable :: str

   print*
   print*,'************* Begin test_util_StringCap2 *************'
   print*   
   print*,'NB: in the following outputs we highlight a string by using a "<<" and a ">>"'
   print*,'    to mark its beginning and its end.'
   print*   

   str  = 'De '//"'Gustibus'"//' Et "Coloribus" Non Est '//"'Disputandum'"
   write(*,'(a)')" Capitalize the parts of the string: <<"//str//">>"
   write(*,'(a)')" that are not between simple or double quotes"

   str = util_StringCap2 ( str, stat, 'nqt' )  
    
   print*
   write(*,'(a)')" The new string-number is: <<"//trim(str)//">>"

   print*
   print*,'------------  End test_util_StringCap2  --------------'  
   print*

   END SUBROUTINE test_util_StringCap2   
   
end program Check_Util_Strings
   
   
