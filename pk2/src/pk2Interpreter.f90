!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Module: pk2Interpreter
!
! Description: 
! ^^^^^^^^^^^
! pk2Interpreter  is  a  parser  that  can  evaluate  any string  expression  involving rank-2 
! arrays (of integers, reals, complexes, logicals or strings types). 
! It uses the derived type "pk2" (see the pk2 module)  which  defines any type of matrix and a 
! large set of operators. 
!
! The  available  set  of functions is  defined  in  a  separated  module "pk2f" and it can be 
! easily enriched.
!
! Notes:
! ^^^^^
! This module  is  a modified version  of  the very good fortran parser "interpreter.f90" that 
! evaluates a string containing a scalar expression of reals. "interpreter.f90" was written by
! Wilton P. Silva and Ivomar B. Soares and is available on 
!           http://zeus.df.ufcg.edu.br/labfit/functionparser.htm
!
! Ref.: Wilton P. Silva et al,  UM AVALIADOR DE EXPRESSÍES EM FORTRAN,  Ciencia  &  Engenharia,
!       14 (1): 95-99, 2005.
!
! Although heavily  redesigned,  I have tried  to preserve  as  much as  possible the  initial 
! organization of "interpreted.f90". Some procedures are tagged by "Adapted" (minor additions)  
! or by "Modified" (rewritten procedures). 
! At the end of the module are located additional procedures. 
!
! Usage: (see also pk2/app/tuto for more details and examples)
! ^^^^^
!
! Usage 1
! -------
! If we have to evaluate an expression only once, we may use the driver pk2interpreter_Driver
! as in the following first example:
! 
!        call pk2Interpreter_Driver ( expr    = 'a*(x+b*sin(x))', &
!                                     vars    = vars            , &
!                                     valExpr = valExpr         , & 
!                                     flagerr = flagerr         )
!
! where the array "vars" contains  the variable values and names (a pk2_t array), "valExpr" is 
! the result (a pk2_t array), and "flagerr" an error flag (an err_t variable). By default, the
! size of "valExpr" is 1 (only one answer) but some functions may have more outputs (sometime
! optionals) as in this second example:
!
!        call pk2Interpreter_Driver ( expr    = 'sort(A)', &
!                                     vars    = vars     , &
!                                     valExpr = valExpr  , & 
!                                     flagerr = flagerr  , &
!                                     nanswer = 2        )
!
! where "nanswer = 2" indicates that 2 answers are expected (the sorted array and the original
! indices in this example). In this example, the size of "valExpr" is therefore 2.
!
! It is recommended  to  check  the  error  flag  to  know  if something went wrong during the 
! analysis or during  the evaluation phase. It  can be done simply by  calling flagerr%display
!
!           if (flagerr%code > 0) call flagerr%display( [...options] )
!
! Notes: 1) If one wants to work  with only a subset of the variables "vars" without modifying
!           this array,  one can  provide an array of strings (a str_t array) of the same size
!           as  "vars"  into  which  the  names of the variables  are  copied  and  for  which  
!           the  elements  corresponding to the undesirable variables are left empty (i.e. the
!           string ''):
!
!           call pk2Interpreter_Driver ( expr     = 'sort(A)' , &
!                                        vars     = vars      , &
!                                        varNames = myvarNames, &
!                                        valExpr  = valExpr   , & 
!                                        flagerr  = flagerr   , &
!                                        nanswer  = 2         )
!
!        2) There are a number of limitations to a valid variable name: 
!           - it must be composed solely of  printable  characters (ascii codes 33 to 127, all 
!             others will be disregarded), some of which are excluded, such as '$', '&', etc
!           - it must not start with a digit 
!           - it cannot be the name of an  existing  function  (see the the list "FuncList" in 
!             subroutine pk2Interpreter_FuncAndOperList)
!           All these constraints must be checked beforehand by the caller.
!
! Usage 2
! -------
! If one has to evaluate the same expression several times, another way, more efficient, is to 
! first analyze the expression by calling pk2interpreter_Tokenizor only once 
!
!       call pk2Interpreter_Tokenizor ( expr    = 'a*(x+b*sin(x))', &
!                                       vars    = vars            , &
!                                       handle  = handle          , &
!                                       flagerr = flagerr         )      
!
! This subroutine parses the expression and outputs the result in the structure "handle". 
!
! Note: for this  first step  only the names  of the  variables are  needed. If such a list is 
!       already available (say e.g. varNames) at the caller level, an alternative can be used:
!
!       call pk2Interpreter_Tokenizor ( expr     = 'a*(x+b*sin(x))', &
!                                       varNames = varNames        , &
!                                       handle   = handle          , &
!                                       flagerr  = flagerr         )      
! 
! Now, since the expression  has been parsed, the expression can be evaluated as many times as
! desired (provided that the list of variable names has not been modified) by supplying the
! output of the tokenizor ("handle") to the evaluator:
! 
!       call pk2Interpreter_Evaluator ( handle, vars, valExpr, flagerr[, nanswer] )
!
! Riad Hassani, Jun. 2018, Nov. 2019
! Apr. 2024: %involvedVar added to hdl_t
!---------------------------------------------------------------------------------------------

#include "error.fpp"

MODULE pk2Interpreter_m
   
   use pk2_m, i2a => util_intToChar
   use pk2f_m
   
   use userfunc_m
   
   implicit none
!
!- A DT designed to contain the analysis made by the token analyzer and to prepare for the
!  evaluation:
!   
   type :: hdl_t
      logical                       :: initialized = .false., allocated = .false.
      integer  (Ikind)              :: size = 0
      character(len=:), allocatable :: expr
      type     (str_t), allocatable :: stokens(:),     &
                                       csep   (:)
      integer  (Ikind), allocatable :: operations(:),  &
                                       narg      (:)
      integer  (Ikind)              :: noperat = 0, &
                                       ntokens = 0, &
                                       nscalar = 0, &
                                       nsdata  = 20
      type     (pk2_t), allocatable :: scalars(:),     & 
                                       pdata  (:),     &
                                       sdata  (:)
      type     (err_t)              :: flag
      type     (str_t), allocatable :: involvedVar(:) ! 04/24
   contains
      procedure, pass, public :: Destroy => pk2Interpreter_DestroyHdl 
      procedure, pass, public :: Alloc   => pk2Interpreter_AllocHdl 
      procedure, pass, public :: Init    => pk2Interpreter_InitHdl 
   end type hdl_t
   
   integer  (Ikind), public, parameter    :: NFC = 102, & ! number of usual functions
                                             NOP = 32, &  ! number of operators
                                             NFU = 100    ! max number of user's functions
   integer  (Ikind), public               :: nuserf = 0                               
   type     (str_t), public , save        :: FuncList(NFC+NFU), & ! List of function names
                                             FuncDesc(NFC+NFU), & ! Their short descriptions
                                             OperList(NOP),     & ! list of operators names
                                             OperDesc(NOP)        ! Their short descriptions
   character(len=1), public , save        :: OperCode(NOP)        ! Operator codes
                                             
   type     (str_t), public , save        :: ForbidSq(NOP)
   logical         , private, save        :: StartEnd(NOP,2)
   
   logical         , private, save        :: firsttime = .true.                                

! To add a function follow these steps:
!  0) increase NFC (eg. NFC = 103)
!  1) add its name in the function list FuncList (cf. pk2Interpreter_FuncAndOperList)
!  2) add a call to this function in the select case block of pk2Interpreter_EvalExpr_v2
!  3) code this function in the module pk2f.f90 or another one

! The following ASCII characters have no chance to appear in the user's expression and will be 
! used temporarily to transform the initial expression.
!             SOP and SCL: used to enclose a given part in the expression
!        S01, S02 and SO3: used to mark a given character in the expression

   character(len=1), parameter :: SOP = char(192), SCL = char(193), &
                                  S01 = char(194), S02 = char(195), S03 = char(196)    
!
! And the following will be used in place of operators that have more than one character:
!
   character(len=1), private, parameter :: ELEMMUL   = char(1), & ! used for .*
                                           ELEMDIV   = char(2), & !          ./
                                           ELEMPOW   = char(3), & !          .^
                                           EQUAL     = char(4), & !          ==
                                           NOTEQ     = char(5), & !          ~=
                                           LESSOREQ  = char(6), & !          <=
                                           GREATOREQ = char(7), & !          >=
                                           ELEMTRANS = char(8)    !          .'
                                           
   character(len=*), private, parameter :: ELEMS = ELEMMUL//ELEMDIV//ELEMPOW//ELEMTRANS, &
                                           RELAT = EQUAL//NOTEQ//LESSOREQ//GREATOREQ//'><'
                          
   interface pk2Interpreter_FindId
      module procedure pk2Interpreter_FindIdStr, pk2Interpreter_FindIdChar1 
   end interface                               

      
CONTAINS   


!= Modified (riad) ===========================================================================
   SUBROUTINE pk2Interpreter_TokensAnalyzer ( myExpr, myVarNames, handle, flagerr )
!=============================================================================================
   character(len=*), intent(in    ) :: myExpr
   type     (str_t), intent(in    ) :: myVarNames(:)
   type     (hdl_t), intent(in out) :: handle
   type     (err_t), intent(in out) :: flagerr   
!---------------------------------------------------------------------------------------------        
!  This subroutine scans the expression "myExpr" string storing its basic elements
!
!  The positions in the string "myExpr" of all operators and delimiters present in the global
!  list "OperList" are first determined, then the string "myExpr" is splited into "ntokens" 
!  tokens (stored into "handle%stokens")
!
!  Example:
!
!   Let
!           myExpr = "-x * y + 30.12e-2 * sin ( x . / y )"   (blanks inserted for clarity)
!
!   The operators and delimiters involved in this string are first localized and marked into
!   the temporary string "str" between SOP and SCL characters (represented here by { and }):
!
!           str = "{-} x {*} y {+} 30.12e-2 {*} sin {(} x {. /} y {)}"
!
!   Thus "myExpr" will be splitted as follows
!     
!           ntokens = 13
!
!           stokens(1)%str = "-"           stokens( 8)%str = "sin"
!           stokens(2)%str = "x"           stokens( 9)%str = "("
!           stokens(3)%str = "*"           stokens(10)%str = "x"
!           stokens(4)%str = "y"           stokens(11)%str = "./"
!           stokens(5)%str = "+"           stokens(12)%str = "y"
!           stokens(6)%str = "30.12e-2"    stokens(13)%str = ")" 
!           stokens(7)%str = "*"       
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2Interpreter_TokensAnalyzer'                                   
!- local variables used/modified by the internal procedures of pk2Interpreter_TokensAnalyzer--
   integer  (Ikind)              :: isAddSub, isMulDiv, isPower, isOr, isAnd, isRelat, &
                                    ntokens, itoke, nscalar
   character(len=1)              :: opAddSub(4), opMulDiv(7), opPower(2), opOr(1), opAnd(1), &
                                    opRelat(6)
   character(len=:), allocatable :: toke
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return
!
!- Initialize the function and operator lists (only during the very first call):
!   
   if ( firsttime ) then
      call userProcList ( nfunc = nuserf ) 
      if ( nuserf > NFU ) then
        flagerr = err_t ( stat = UERROR, where = HERE, &
                          msg = 'Maximum Number of user functions exceeded')
        return
      end if
      call pk2Interpreter_FuncAndOperList ( ) 
      firsttime = .false.
   end if 
!
!- Store myExpr into handle%expr (the latter may be modified):
!
   handle%expr = myExpr
!
!- Do some conversions in the expression to analyze and check for basic syntax errors:
!      
   call pk2Interpreter_Convert ( handle%expr, flagerr )
   error_TraceNreturn(flagerr, HERE)
!
!- Split now this expression in tokens:
!   
   call pk2Interpreter_SplitInTokens ( handle, ntokens, flagerr )
   error_TraceNreturn(flagerr, HERE)
!
!- Start parsing (follow Wilton P. Silva and Ivomar B. Soares procedures):
!
   isAddSub = 1; isMulDiv = 1; isPower = 1; isOr = 1; isAnd = 1; isRelat = 1; nscalar = 1

   itoke = 1 ; toke = handle%stokens(itoke)%str  

   call pk2Interpreter_Or ()
   
   handle%noperat = handle%noperat - 1
!
!- Check if undefined variables are signaled:
!      
   call pk2Interpreter_UndefVar ()  

   handle%initialized = .true.
   handle%expr        = myExpr
   handle%nscalar     = nscalar
   
   if ( flagerr%code /= 0 ) then
      call flagerr%AddTrace(HERE)
      handle%flag = flagerr
   end if

   CONTAINS

   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   ! Internal procedures of pk2Interpreter_TokensAnalyzer:
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! The following subroutines call themselves recursively to build the expression to be  
   ! parsed based on an algorithm called Recursive Descendent Parsing
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !- Added (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE pk2Interpreter_Or ( )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------
   !  Enter description here
   !------------------------------------------------------------------------------------------  

      call pk2Interpreter_And ( )
   
      if ( flagerr%code > 0 ) return
   
      do while ( toke == '|' )

         opOr(isOr) = toke
         isOr = isOr + 1
         
         itoke = itoke + 1 ;  toke = handle%stokens(itoke)%str
      
         call pk2Interpreter_And ( )
    
         select case ( opOr(isOr-1) )

            case ( '|' ) ! .or. operator
               isOr                              = isOr - 1
               handle%operations(handle%noperat) = 3
               handle%noperat                    = handle%noperat + 1            

         end select
      
      end do

      END SUBROUTINE pk2Interpreter_Or
      

   !- Added (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE pk2Interpreter_And ( )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------
   !  Enter description here
   !------------------------------------------------------------------------------------------  
 
      call pk2Interpreter_Relat ( )
   
      if ( flagerr%code > 0 ) return
   
      do while ( toke == '&' )

         opAnd(isAnd) = toke
         isAnd = isAnd + 1
         
         itoke = itoke + 1 ;  toke = handle%stokens(itoke)%str
      
         call pk2Interpreter_Relat ( )
    
         select case ( opAnd(isAnd-1) )

            case ( '&' ) ! .and. operator
               isAnd                             = isAnd - 1
               handle%operations(handle%noperat) = 10
               handle%noperat                    = handle%noperat + 1            

         end select
      
      end do

      END SUBROUTINE pk2Interpreter_And
      

   !- Added (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE pk2Interpreter_Relat ( ) 
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------
   !  Enter description here
   !------------------------------------------------------------------------------------------  
 
      call pk2Interpreter_AddSub ( )
   
      if ( flagerr%code > 0 ) return
   
      !do while ( toke == '>' .or. toke == '>=' .or.  toke == '==' .or. &
      !           toke == '<' .or. toke == '<=' .or.  toke == '~='      )

      do while ( toke == '<' .or. toke == LESSOREQ  .or. toke == EQUAL .or. &
                 toke == '>' .or. toke == GREATOREQ .or. toke == NOTEQ      )

         opRelat(isRelat) = toke
         isRelat = isRelat + 1
         
         itoke = itoke + 1 ;  toke = handle%stokens(itoke)%str
      
         call pk2Interpreter_AddSub ( )

         select case ( opRelat(isRelat-1) )

            !case ('==') 
            case ( EQUAL ) 
               isRelat                           = isRelat - 1
               handle%operations(handle%noperat) = 14
               handle%noperat                    = handle%noperat + 1            
            !case ('~=') 
            case ( NOTEQ ) 
               isRelat                           = isRelat - 1
               handle%operations(handle%noperat) = 15
               handle%noperat                    = handle%noperat + 1            

            case ( '<' ) 
               isRelat                           = isRelat - 1
               handle%operations(handle%noperat) = 16
               handle%noperat                    = handle%noperat + 1            

            case ( '>' ) 
               isRelat                           = isRelat - 1
               handle%operations(handle%noperat) = 17
               handle%noperat                    = handle%noperat + 1            

            !case ('<=') 
            case ( LESSOREQ ) 
               isRelat                           = isRelat - 1
               handle%operations(handle%noperat) = 18
               handle%noperat                    = handle%noperat + 1            

            !case ('>=') 
            case ( GREATOREQ ) 
               isRelat                           = isRelat - 1
               handle%operations(handle%noperat) = 19
               handle%noperat                    = handle%noperat + 1            

         end select
      
      end do

      END SUBROUTINE pk2Interpreter_Relat  
      
         
   !- Adapted (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE pk2Interpreter_AddSub ( )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------
   !  Enter description here
   !------------------------------------------------------------------------------------------  
 
      call pk2Interpreter_MulDiv ( )
   
      if ( flagerr%code > 0 ) return
   
      do while ( toke == '+' .or. toke == '-' )

         opAddSub(isAddSub) = toke
         isAddSub = isAddSub + 1
         
         itoke = itoke + 1 ;  toke = handle%stokens(itoke)%str
      
         call pk2Interpreter_MulDiv ( )
    
         isAddSub = isAddSub - 1
         
         select case ( opAddSub(isAddSub) )
      
            case ('+')
               handle%operations(handle%noperat) = 1
               handle%noperat                    = handle%noperat + 1

            case ('-')
               handle%operations(handle%noperat) = 2
               handle%noperat                    = handle%noperat + 1           

         end select
      
      end do

      END SUBROUTINE pk2Interpreter_AddSub


   !- Adapted (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE pk2Interpreter_MulDiv ( )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------
   !  Enter description here
   !------------------------------------------------------------------------------------------  

      call pk2Interpreter_Unary ( )

      if ( flagerr%code > 0 ) return
    
      do while ( toke == '*' .or. toke == ELEMMUL .or. &
                 toke == '/' .or. toke == ELEMDIV .or. & 
                 toke == '\'                           )       
                   
         opMulDiv(isMulDiv) = toke
      
         isMulDiv = isMulDiv + 1
         
         itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
         
         call pk2Interpreter_Unary ( )

         isMulDiv = isMulDiv - 1    

         select case ( opMulDiv(isMulDiv) )
            case ( '*' )
               handle%operations(handle%noperat) = 6
               handle%noperat                    = handle%noperat + 1 
                  
            case ( '/' )
               handle%operations(handle%noperat) = 7
               handle%noperat                    = handle%noperat + 1

            case ( ELEMMUL ) ! element-wise multiplication
               handle%operations(handle%noperat) = 8
               handle%noperat                    = handle%noperat + 1          
            
            case ( ELEMDIV ) ! element-wise divide
               handle%operations(handle%noperat) = 9
               handle%noperat                    = handle%noperat + 1                           

            case ( '\' ) ! operator \ (solve linear system)
               handle%operations(handle%noperat) = 11
               handle%noperat                    = handle%noperat + 1   
         end select
      end do

      END SUBROUTINE pk2Interpreter_MulDiv
   

   !- Adapted (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE pk2Interpreter_Unary ( ) 
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------
   !  Enter description here
   !------------------------------------------------------------------------------------------  

      if ( toke == '-' ) then ! '-'
         itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
      
         call pk2Interpreter_Pow ( )
      
         handle%operations(handle%noperat) = 4
         handle%noperat                    = handle%noperat + 1
         
      else if ( toke == '+' ) then ! '+', nothing to do
         itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
      
         call pk2Interpreter_Pow ( )

               
      else if ( toke == '~' ) then ! operator .not. 
         itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
      
         call pk2Interpreter_Pow ( )
      
         handle%operations(handle%noperat) = 5
         handle%noperat                    = handle%noperat + 1   
         
      else
         call pk2Interpreter_Pow ( )
      end if

      if ( flagerr%code > 0 ) return
   
      END SUBROUTINE pk2Interpreter_Unary


   !- Adapted (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE pk2Interpreter_Pow ( )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------
   !  Enter description here
   !------------------------------------------------------------------------------------------

      call pk2Interpreter_Functions ( )
   
      if ( flagerr%code >  0) return

      do while ( toke == "^" .or. toke == ELEMPOW .or. & ! ^ or .^
                 toke == "'" .or. toke == ELEMTRANS    ) ! ' or .'
             
         opPower(isPower) = toke ; isPower = isPower + 1
         
         itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
                      
         select case ( opPower(isPower-1) )
            case ( "^" )

               call pk2Interpreter_Functions ( )
               
               isPower                           = isPower - 1    
               handle%operations(handle%noperat) = 12
               handle%noperat                    = handle%noperat + 1

            case ( ELEMPOW )   ! element-wise power
                           
               call pk2Interpreter_Functions ( )
                           
               isPower                           = isPower - 1    
               handle%operations(handle%noperat) = 13
               handle%noperat                    = handle%noperat + 1

            case ( "'" )   ! conjugate transpose

               isPower                           = isPower - 1    
               handle%operations(handle%noperat) = 28
               handle%noperat                    = handle%noperat + 1    

            case ( ELEMTRANS )   ! transpose

               isPower                           = isPower - 1    
               handle%operations(handle%noperat) = 29
               handle%noperat                    = handle%noperat + 1    

         end select
      end do   
   
      END SUBROUTINE pk2Interpreter_Pow

                     
   !- Modified (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      RECURSIVE SUBROUTINE pk2Interpreter_Functions ( ) 
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------
   !  Enter description here
   !------------------------------------------------------------------------------------------

   !- local variables ------------------------------------------------------------------------
      character(len=*), parameter :: ALPH = &
                                     'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' 
      integer  (Ikind)            :: Id, itoke1, itoke0, noperation0
   !------------------------------------------------------------------------------------------   

   !
   !- If "toke" is followed by a '(' it must be the name of a function or the name of a 
   !  variable (--> extraction of a submatrix) unless "toke" is itself a '('.
   !      
      if ( itoke < ntokens ) then
         if ( handle%stokens(itoke+1)%str == '(' ) then
      
            if ( toke == '(' ) then
               call pk2Interpreter_Brackets ( )
               return
            end if   
   !
   !-       Search within the list "FuncList" of function names:
   !
            Id = pk2Interpreter_FindId ( toke, FuncList, NFC+nuserf, IONE, -IONE )
   
            if ( Id /= 0 ) then    
               itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
               
               itoke0 = itoke ; noperation0 = handle%noperat
            
               call pk2Interpreter_Brackets ( )
                        
               if ( itoke > itoke0 + 1 .and. handle%noperat /= noperation0 ) &
                  handle%narg(handle%noperat) = handle%narg(handle%noperat) + 1

               handle%operations(handle%noperat) = NOP + Id
               handle%noperat                    = handle%noperat + 1
               
               return
            end if
   !
   !-       Search within the list "myVarNames" of variable names:
   !
            Id = pk2Interpreter_FindId ( toke, myVarNames, IONE, size(myVarNames), IONE )   
   
            itoke1 = itoke 
            itoke  = itoke + 1
            itoke0 = itoke ; noperation0 = handle%noperat
            toke   = handle%stokens(itoke)%str
               
            call pk2Interpreter_Brackets ( )
            
            if ( Id /= 0 ) then
               handle%operations (handle%noperat) = NFC + NFU + NOP + Id
               handle%involvedVar(handle%noperat) = myVarNames(Id) ! 04/24
            else         
   !
   !-          There is an error: '(' not preceeded by the name of a prexisting variable
   !           See if this due to a syntax error or to a not defined variable:
   !
               if ( index(ALPH,handle%stokens(itoke1)%str(1:1)) == 0 ) then
   !
   !-             syntax error (the 1st character is not a letter)          
   !
                  flagerr = err_t ( stat = UERROR, where ='pk2Interpreter_Functions', &
                                     msg = 'Expression has an error'                  )
                  return
               else            
   !               
   !-             undefined variable, continue anyway but set operations < 0
   !              (this error will be handled by pk2Interpreter_UndefVar):
   !
                  handle%operations(handle%noperat) = -itoke1
               end if
            end if
            
            if ( itoke > itoke0 + 1 .and. handle%noperat /= noperation0 ) &
               handle%narg(handle%noperat) = handle%narg(handle%noperat) + 1

            handle%noperat = handle%noperat + 1
            return

         end if
      end if
   
      handle%narg(handle%noperat) = 0
               
      call pk2Interpreter_Brackets ( )
        
      if ( flagerr%code > 0 ) return
        
      END SUBROUTINE pk2Interpreter_Functions     
   
   
   !- Adapted (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      RECURSIVE SUBROUTINE pk2Interpreter_Brackets ( ) 
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------            
   !  Enter description here
   !------------------------------------------------------------------------------------------            

   !- local variables ------------------------------------------------------------------------          
      integer(Ikind) :: nvirg, npvir
      type   (str_t) :: sep
   !------------------------------------------------------------------------------------------            
   
      if ( toke == '(' ) then

         itoke = itoke + 1 ; toke = handle%stokens(itoke)%str

         call pk2Interpreter_Or ( )

   !     for multiple arguments (delimited by ";" or ","):
         nvirg = 0 ; npvir = 0 
         sep%str=' '
         do while (  toke == ',' .or. toke == ';' )
            if ( toke == ',' ) then
               nvirg   = nvirg + 1
               sep%str = trim(sep%str) // ','
            end if   
            if ( toke == ';' ) then
               npvir   = npvir + 1
               sep%str = trim(sep%str) // ';'
            end if
            itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
         
            call pk2Interpreter_Or ( )
         end do

         call pk2Interpreter_Brackets ( )
      
         handle%narg(handle%noperat) = handle%narg(handle%noperat) + nvirg + npvir
      
         handle%csep(handle%noperat) = sep

         if ( toke /= ')' ) then  
            flagerr = err_t ( stat = UERROR, where = 'pk2Interpreter_Brackets', msg =  & 
                             'Invalid token (unknown variable or not a number): << '// &
                              toke //' >> '                                      )
            return
         end if                    
            
         if ( itoke < ntokens ) then
            itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
         end if
         
      
         if ( toke == '(' ) then
            flagerr =  err_t ( stat = UERROR, where = 'pk2Interpreter_Brackets', &
                                msg = 'Invalid token: << '//toke//' >>')   
            return
         end if    

      else if ( toke /= ')' ) then

         call pk2Interpreter_RecogVars ( )
      
         if ( flagerr%code > 0 ) return
   
      end if
   
      END SUBROUTINE pk2Interpreter_Brackets
   
   
   !- Modified (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      SUBROUTINE pk2Interpreter_RecogVars ( )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------      
   !  Identifies if the current token is the name of a pre-existing variable or if this is a 
   !  scalar (number, logical or string), then
   !  . if this is a variable: get its # (Id) and set the operation # to NFC + NOP + Id
   !  . if this is a scalar  : read its value and set the operation # to 0.
   !------------------------------------------------------------------------------------------      

   !- local variables ------------------------------------------------------------------------           
      integer  (Ikind)              :: Id, nl, ierr
      real     (Rkind)              :: areal
      logical                       :: is_intg
      character(len=:), allocatable :: tl
   !------------------------------------------------------------------------------------------      

   !
   !- Normally, the following test is useless here (if such error occurs, it will be 
   !  intercepted by the routine identifica):
   !       
      Id = pk2Interpreter_FindId ( toke, OperCode, IONE, size(OperCode), IONE )
      if ( Id /= 0 ) then   
          flagerr = err_t ( stat = UERROR, where = 'pk2Interpreter_RecogVars', &
                             msg = 'Expression has an error'                   )
          return    
      end if 
        
      Id = pk2Interpreter_FindId ( toke, myVarNames, IONE, size(myVarNames), IONE )

      if ( Id /= 0 ) then
   !
   !-    This is the name of a preexisting variable. 
   !     Set the operation # to (NFC + NOP + NFU) + Id:
   !   
         handle%operations (handle%noperat) = (NFC + NOP + NFU) + Id
         handle%involvedVar(handle%noperat) = myVarNames(Id) ! 04/24
   
      else   
   !
   !-    This is a scalar (number, logical or string). Set the operation # to 0:
   !
         handle%operations(handle%noperat) = 0
   !
   !-    and read it and put its value in the "scalars" array:
   !       
         if ( toke(1:1) /= '"' ) then
   !
   !-       This is a logical or a number. Try to read it as a number:
   !
            read(toke, *, iostat = ierr) areal
        
            if ( ierr /= 0 ) then
   !
   !-          This is not a number. May be a logical (if not, there is an error):
   !         
               tl = util_StringLow(toke)

               if ( tl == '.true.' .or. tl == '.t.' ) then
                  handle%scalars(nscalar) = .true.  
               else if ( tl == '.false.' .or. tl == '.f.' ) then
                  handle%scalars(nscalar) = .false.
               else                
   !
   !-             Undefined variable. Continue anyway but set operations(noperation) < 0
   !              (the error will be signaled at the end of pk2Interpreter_TokensAnalyzer):
   !
                  handle%operations(handle%noperat) =-itoke
               
               end if
             
            else
   !
   !-          This is a number. See if this number is an integer or not:
   !
               is_intg = .true.
         
               tl = util_StringLow(toke)

               if ( index(tl,'e') /= 0 .or. index(tl,'d') /= 0 .or. index(tl,'.') /= 0 ) &
                  is_intg = .false.
         
               if ( tl == 'nan'                                                        ) &
                  is_intg = .false.
               
               if ( tl == 'inf'       .or. tl == '-inf'       .or. tl == '+inf'        ) &
                  is_intg = .false.
            
               if ( tl == 'infinity'  .or. tl == '-infinity'  .or. tl == '+infinity'   ) &
                  is_intg = .false.            
         
               if ( is_intg .and. areal <= IMAX ) then
   !
   !-             This is an integer number:
   !          
                  handle%scalars(nscalar) = int(areal,kind=Ikind) ! (note: "=" overloaded)      
               else
   !
   !-             This is a real number:    
   !  
                  handle%scalars(nscalar) = areal ! (note: "=" overloaded)
               end if
            end if
         else
   !
   !-       This is a string (it starts with "):
   !   
            nl = len(toke)
      
            if ( nl > 2 ) then
               handle%scalars(nscalar) = toke(2:nl-1)  ! remove the quotes
            else
               handle%scalars(nscalar) = ''            ! empty string 
            end if      
         
         end if
   
         nscalar = nscalar + 1
      end if
   
      handle%noperat = handle%noperat + 1

      if ( itoke < ntokens ) then
         itoke = itoke + 1 ; toke = handle%stokens(itoke)%str
      end if
                
      END SUBROUTINE pk2Interpreter_RecogVars


   !- Added (riad) =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      SUBROUTINE pk2Interpreter_UndefVar ( )
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   !------------------------------------------------------------------------------------------   
   !  Reports any undefined variable (routine to be called after the tokens analyzer)
   !
   !  Note:
   !     As constructed in routines pk2Interpreter_RecogVars and pk2Interpreter_Functions,
   !     a negative entry of "operations" 
   !                               iop = handle%noperat(i) < 0
   !     indicates an undefined variable present in the operations. The name of this 
   !     variable is retrevied by
   !                               handle%stokens(-iop)%str 
   !--------------------------------------------------------------------------------R.H. 05/19

   !- local variables ------------------------------------------------------------------------
      character(len=:), allocatable :: msg, tok
      integer  (Ikind)              :: i, j, iop, jop, n
      logical                       :: is_done  
   !------------------------------------------------------------------------------------------   

      if ( any(handle%operations(1:handle%noperat) < 0) ) then
         msg = '' ; n = 0
         do i = 1, handle%noperat
            iop = handle%operations(i)
            if ( iop < 0  ) then
               tok = handle%stokens(-iop)%str
               is_done = .false.
               do j = 1, i-1
                  jop = handle%operations(j)
                  if ( jop < 0 ) then
                     if ( handle%stokens(-jop)%str == tok ) then
                        is_done = .true.
                        exit
                     end if   
                  end if
               end do      
               if ( is_done ) cycle
               if (n /= 0) msg = msg // ', '
               n = n + 1
               msg = msg // "<< " // tok // " >>"
            end if
         end do
         if ( n == 1 ) then
            msg = 'The variable or constant ' // trim(msg) // ' is not defined' 
         else
            msg = 'The variables or constants '// trim(msg) // ' are not defined'
         end if   
         flagerr = err_t (stat = UERROR+10, where = 'pk2Interpreter_UndefVar', msg = msg)
      end if
   
      END SUBROUTINE pk2Interpreter_UndefVar
      
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   ! end of internal procedures of pk2Interpreter_TokensAnalyzer
   !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
         
   END SUBROUTINE pk2Interpreter_TokensAnalyzer


!= Modified (riad) ===========================================================================  
   SUBROUTINE pk2Interpreter_EvalExpr_v1 ( handle, vars, answer, flagerr, nanswer ) 
!============================================================================================= 
   type   (hdl_t),              intent(in out) :: handle
   class  (pk2_t),              intent(in    ) :: vars(:)
   type   (pk2_t), allocatable, intent(   out) :: answer(:)
   type   (err_t),              intent(in out) :: flagerr   
   integer(Ikind), optional,    intent(in    ) :: nanswer
!---------------------------------------------------------------------------------------------    
!  This routine evaluate the expression supplied after its analysis by the tokens analyzer
!
!  Inputs:
!
!  . handle : the analysis of the expression done by the tokens analyzer
!  . vars   : the pk2 array containing the values of the variables
!  . nAnswer: (optional) expected number of answers (default: 1)
!
!  Outputs:
!
!  . answer : a pk2 array corresponding to the result(s)
!  . flagerr: error/warning status with the following convention:
!             . flagErr%code = 0: no error and no warning
!             . flagErr%code > 0: an error has occured 
!             . flagErr%code < 0: a warning message is available
!             . and flagErr%mesg gives the corresponding error or warning message.
!
!  Note: this version uses function calls and operator overloads. 
!        Another version (pk2Interpreter_EvalExpr_v2) is given below, I think more efficient
!        since it avoids assignments and corresponding temporary copies.
!
!  05/25: Version NO LONGER MAINTAINED (use pk2Interpreter_EvalExpr_v2)
!---------------------------------------------------------------------------------------------    

!- local variables --------------------------------------------------------------------------- 
   character(len=* ), parameter   :: HERE = 'pk2Interpreter_EvalExpr_v1'
   integer  (Ikind )              :: st, dt
   integer  (Ikind )              :: i, iop, k, k0, ivar, nvars, nans, nsdata, nsused, err
   character(len=99)              :: cnum
!---------------------------------------------------------------------------------------------    

   !flagerr = err_t ()
   if ( flagerr%code > IZERO ) return
   
   if (.not. handle%initialized) then
      flagerr = err_t ( stat = UERROR, where = HERE,                  &
                         msg = 'The handle must first be initialized.')  
      return
   end if   
   
   if (present(nanswer)) then
      nans = max(1_Ikind,nanswer)
   else
      nans = 1                    
   end if      
   
   nsdata = nans - 1 ! number of secondary data expected
   nsused = 0        ! actually used
   
   nvars = size(vars)
            
   if (nsdata > size(handle%sdata)) then
!
!-    handle%sdata was pre-allocated to an insufficient size. Resize it:
!   
      deallocate(handle%sdata) ; allocate(handle%sdata(nsdata), stat = err)
      if (err /= 0) then
         flagerr = err_t (stat = IERROR, where = HERE, msg = 'Allocation failure')  
         return
      end if         
      do i = 1, nsdata
         handle%sdata(i)%name = ''
      end do
   end if     
   
   !print*,'stokens: | ',(handle%stokens(i)%str,' | ',i=1,handle%ntokens)
   !print*,'op.    :',(handle%operations(i), i=1,handle%noperat)
   !print*,'narg   :',(handle%narg(i)      , i=1,handle%noperat)
   !print*,'csep   :',(handle%csep(i)%str  , i=1,handle%noperat)
!
!- Initialize the internal flag "opflag" (used in procedures of bk2, pk2, pk2f modules and
!  will serve to report any error or warning):
!   
   if ( opflag%code > IZERO ) return ! opflag = err_t ()
!
!- Start performing the "handle%noperat" operations:
!   
   associate ( narg => handle%narg, pdata => handle%pdata, sdata => handle%sdata )
   
      st = 0_Ikind ; dt = 1_Ikind
      
      do i = 1, handle%noperat 
      
         iop = handle%operations(i)
      
         if ( st - narg(i) + 1 <= 0 ) then
            call flagerr%Set(UERROR, HERE, 'Expression has an error')
            return
         end if   
            
         select case ( iop )      
!
!-       numeric or string data:
!
         case ( 0 )
            st = st + 1
            pdata(st) = handle%scalars(dt)
            dt = dt + 1
!
!-       arithmetic or boolean operations (from 1 to nop):
!                        
         case ( 1 )
            pdata(st-1) = pdata(st-1) + pdata(st) 
            st = st - 1 
            
         case ( 2 )
            pdata(st-1) = pdata(st-1) - pdata(st) 
            st = st - 1

         case ( 3 )
            pdata(st-1) = pdata(st-1) .or. pdata(st) ! .or. operator
            st = st - 1
      
         case ( 4 )
            pdata(st) = -pdata(st)
            
         case ( 5 )
            pdata(st) = .not. pdata(st) ! .not. operator
                        
         case ( 6 )
            pdata(st-1) = pdata(st-1) * pdata(st)  !  matrix-matrix product
            st = st - 1

         case ( 7 )
            pdata(st-1) = pdata(st-1) / pdata(st)  ! mat/scal or scal/mat 
            st = st - 1                                           ! (i.e. mat of inverses) 

         case ( 8 )
            pdata(st-1) = pdata(st-1) .m. pdata(st) ! element-wise mult. 
            st = st - 1

         case ( 9 )
            pdata(st-1) = pdata(st-1) .d. pdata(st)  ! element-wise divide 
            st = st - 1 
                        
         case ( 10 )
            pdata(st-1) = pdata(st-1) .and. pdata(st) ! .and. operator
            st = st - 1       

         case ( 11 )
            pdata(st-1) = pdata(st-1) .bslash. pdata(st) ! lin. syst. solve
            st = st - 1       
            
         case ( 12 )
            pdata(st-1) = pdata(st-1) ** pdata(st)
            st = st - 1       

         case ( 13 )
            pdata(st-1) = pdata(st-1) .p. pdata(st) ! element-wise expo.
            st = st - 1       
!
!-       relational operators (<--> functions evaluations)
!
         case ( 14 )
            pdata(st-1) = pdata(st-1) .eq. pdata(st)
            st = st - 1       

         case ( 15 )
            pdata(st-1) = pdata(st-1) .ne. pdata(st)
            st = st - 1       

         case ( 16 )
            pdata(st-1) = pdata(st-1) .lt. pdata(st)
            st = st - 1       

         case ( 17 )
            pdata(st-1) = pdata(st-1) .gt. pdata(st)
            st = st - 1       

         case ( 18 )
            pdata(st-1) = pdata(st-1) .le. pdata(st)
            st = st - 1       

         case ( 19 )
            pdata(st-1) = pdata(st-1) .ge. pdata(st)
            st = st - 1   
         
         case ( 28 )
            pdata(st) = trans (a=pdata(st))

         case ( 29 )
            pdata(st) = transp (a=pdata(st))             
!
!-       Function evaluations. Functions that have exactly 0 argument:
!
         case ( NOP+75 )
            if (narg(i) /= 0) then
               call flagerr%Set(UERROR, HERE, '0 argument expected for << pwd >>')
               return
            else           
               st = st + 1
               pdata(st) = pwd ( )
           end if

         case ( NOP+76 )
            if (narg(i) /= 0) then
               call flagerr%Set(UERROR, HERE, '0 argument expected for << cputime >>')
               return
            else           
               st = st + 1
               pdata(st) = cputime ( )
           end if         
!
!-       Function evaluations. Functions that have exactly 1 pk2_t input argument: 
!         
         case ( NOP+7:NOP+53, NOP+80, NOP+81, NOP+68, NOP+89, NOP+96, NOP+98:NOP+101)
            if (narg(i) /= 1) then
               call flagerr%Set(UERROR, HERE, &
                  '1 argument expected for the function << '//FuncList(iop-nop)%str //' >>' )
               return 
            else
               if      (iop == NOP+ 7) then
                  pdata(st) = sin    ( a=pdata(st) )
               else if (iop == NOP+ 8) then
                  pdata(st) = cos    ( a=pdata(st) )
               else if (iop == NOP+ 9) then
                  pdata(st) = tan    ( a=pdata(st) )
               else if (iop == NOP+10) then
                  pdata(st) = asin   ( a=pdata(st) )
               else if (iop == NOP+11) then
                  pdata(st) = acos   ( a=pdata(st) )
               else if (iop == NOP+12) then
                  pdata(st) = atan   ( a=pdata(st) )
               else if (iop == NOP+13) then 
                  pdata(st) = sind   ( a=pdata(st) )
               else if (iop == NOP+14) then 
                  pdata(st) = cosd   ( a=pdata(st) )
               else if (iop == NOP+15) then 
                  pdata(st) = tand   ( a=pdata(st) )
               else if (iop == NOP+16) then 
                  pdata(st) = asind  ( a=pdata(st) )
               else if (iop == NOP+17) then 
                  pdata(st) = acosd  ( a=pdata(st) )
               else if (iop == NOP+18) then 
                  pdata(st) = atand  ( a=pdata(st) )
               else if (iop == NOP+19) then
                  pdata(st) = sinh   ( a=pdata(st) )
               else if (iop == NOP+20) then
                  pdata(st) = cosh   ( a=pdata(st) )
               else if (iop == NOP+21) then
                  pdata(st) = tanh   ( a=pdata(st) )
               else if (iop == NOP+22) then 
                  pdata(st) = log    ( a=pdata(st) )
               else if (iop == NOP+23) then 
                  pdata(st) = log10  ( a=pdata(st) )
               else if (iop == NOP+24) then 
                  pdata(st) = exp   ( a=pdata(st) )
               else if (iop == NOP+25) then 
                  pdata(st) = sqrt   ( a=pdata(st) )
               else if (iop == NOP+26) then 
                  pdata(st) = abs    ( a=pdata(st) )
               else if (iop == NOP+27) then 
                  pdata(st) = erf    ( a=pdata(st) )
               else if (iop == NOP+28) then 
                  pdata(st) = erfc   ( a=pdata(st) )
               else if (iop == NOP+29) then 
                  pdata(st) = gamma  ( a=pdata(st) )
               else if (iop == NOP+30) then 
                  pdata(st) = factor ( a=pdata(st) )
               else if (iop == NOP+31) then 
                  pdata(st) = nint   ( a=pdata(st) )
               else if (iop == NOP+32) then 
                  pdata(st) = anint  ( a=pdata(st) )
               else if (iop == NOP+33) then 
                  pdata(st) = aint   ( a=pdata(st) )
               else if (iop == NOP+34) then 
                  pdata(st) = floor  ( a=pdata(st) )
               else if (iop == NOP+35) then 
                  pdata(st) = ceil   ( a=pdata(st) )
               else if (iop == NOP+36) then 
                  pdata(st) = sign   ( a=pdata(st) )
               else if (iop == NOP+37) then 
                  pdata(st) = heav   ( a=pdata(st) )
               else if (iop == NOP+38) then 
                  pdata(st) = real   ( a=pdata(st) )
               else if (iop == NOP+39) then 
                  pdata(st) = imag   ( a=pdata(st) )
               else if (iop == NOP+40) then 
                  pdata(st) = conj   ( a=pdata(st) )
               else if (iop == NOP+41) then 
                  pdata(st) = trace  ( a=pdata(st) )
               else if (iop == NOP+42) then 
                  pdata(st) = trans  ( a=pdata(st) )
               else if (iop == NOP+43) then 
                  pdata(st) = transp ( a=pdata(st) )
               else if (iop == NOP+44) then 
                  pdata(st) = det    ( a=pdata(st) )
               else if (iop == NOP+45) then 
                  pdata(st) = inv    ( a=pdata(st) )
               else if (iop == NOP+47) then 
                  pdata(st) = numel  ( a=pdata(st) )
               else if (iop == NOP+48) then 
                  pdata(st) = find   ( a=pdata(st) )
               else if (iop == NOP+49) then 
                  pdata(st) = num2str( a=pdata(st) )
               else if (iop == NOP+50) then 
                  pdata(st) = str2num( a=pdata(st) )
               else if (iop == NOP+51) then 
                  pdata(st) = magic  ( a=pdata(st) )
               else if (iop == NOP+52) then 
                  pdata(st) = is_symm ( a=pdata(st) )
               else if (iop == NOP+53) then 
                  pdata(st) = is_skew ( a=pdata(st) )
               else if (iop == NOP+80) then 
                  pdata(st) = trim   ( a=pdata(st) )
               else if (iop == NOP+81) then 
                  pdata(st) = length ( a=pdata(st) )
               else if (iop == NOP+46) then
!
!-                caution: this function has a variable number of outputs:
!               
                  if ( nsdata == 0 ) then
                     pdata(st) = size ( a=pdata(st) )
                  else 
                     pdata(st) = size ( a=pdata(st), res2=sdata(1) )
                     nsused = max(nsused,1_Ikind)
                  end if                     
               else if ( iop == NOP+68 ) then
!
!-                caution: this function has a variable number of outputs:
!                              
                  if ( nsdata == 0 ) then
                     pdata(st) = eig ( a=pdata(st) )
                  else
                     pdata(st) = eig ( a=pdata(st), res2=sdata(1) )
                     nsused = max(nsused,1_Ikind)
                  end if
               else if ( iop == NOP+89 ) then
!
!-                caution: this function has a variable number of outputs:
!                                             
                  if ( nsdata == 0 ) then
                     pdata(st) = lu ( a=pdata(st) )
                  else if ( nsdata == 1 ) then
                     pdata(st) = lu ( a=pdata(st), res2 = sdata(1) ) 
                     nsused = max(nsused,IONE)
                  else
                     pdata(st) = lu ( a=pdata(st), res2 = sdata(1), res3 = sdata(2) )
                     nsused = max(nsused,2_Ikind)
                  end if  

              else if ( iop == NOP+96 ) then 
                  pdata(st) = dev    (a=pdata(st))
               else if ( iop == NOP+98 ) then 
                  pdata(st) = sizeof (a=pdata(st))                  
               else if ( iop == NOP+99 ) then 
                  pdata(st) = typeof (a=pdata(st))   
                  
               else if ( iop == NOP+100 ) then
!
!-                caution: this function has a variable number of outputs:
!                                             
                  if ( nsdata == 0 ) then
                     pdata(st) = svd (a=pdata(st))
                  else if ( nsdata == 1 ) then
                     sdata(1) = svd (a=pdata(st), u=pdata(st))
                     nsused = max(nsused,1_Ikind)
                  else
                     sdata(1) = svd (a=pdata(st), u=pdata(st), v=sdata(2))
                     nsused = max(nsused,2_Ikind)
                  end if                 
                  
               else if ( iop == NOP+101 ) then
!
!-                caution: this function has a variable number of outputs:
!                                        
                  if ( nsdata == 0 ) then
                     pdata(st) = poldec (F=pdata(st))
                  else 
                     pdata(st) = poldec (F=pdata(st), U=sdata(1))
                     nsused = max(nsused,1_Ikind)
                  end if      
               end if
            end if            
!
!-       Function evaluations. Functions that have exactly 2 pk2_t input arguments:
!         
         case ( NOP+1:NOP+6, NOP+55, NOP+57, NOP+86, NOP+90, NOP+92, NOP+95, NOP+97)
            if (narg(i) /= 2) then
               call flagerr%Set(UERROR, HERE, &
                  '2 arguments expected for the function << '//FuncList(iop-nop)%str//' >>' )
               return   
            else               
               if      ( iop == NOP+1 ) then
                  pdata(st-1) = pk2_Eq  ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+2 ) then 
                  pdata(st-1) = pk2_Ne  ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+3 ) then 
                  pdata(st-1) = pk2_Lt  ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+4 ) then 
                  pdata(st-1) = pk2_Gt  ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+5 ) then 
                  pdata(st-1) = pk2_Le  ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+6 ) then 
                  pdata(st-1) = pk2_Ge  ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+55 ) then
                  pdata(st-1) = cross   ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+57 ) then 
                  pdata(st-1) = mldivide( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+86 ) then 
                  pdata(st-1) = mod     ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+90 ) then 
                  pdata(st-1) = lusolv  ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+92 ) then
!
!-                caution: this function has a variable number of outputs:
!                 
                  if ( nsdata == 0 ) then
                     pdata(st-1) = reglin ( x=pdata(st-1), y=pdata(st) )
                  else if ( nsdata == 1 ) then
                     pdata(st-1) = reglin ( x=pdata(st-1), y=pdata(st), b=sdata(1) )
                     nsused = max(nsused,1_Ikind)
                  else
                     pdata(st-1) = reglin ( x=pdata(st-1), y=pdata(st), b=sdata(1),   &
                                                                        sig=sdata(2)  )
                     nsused = max(nsused,2_Ikind)
                  end if   
               else if ( iop == NOP+95 ) then
                  pdata(st-1) = part    ( a=pdata(st-1), b=pdata(st) )
               else if ( iop == NOP+97 ) then
                  pdata(st-1) = inttrap ( a=pdata(st-1), b=pdata(st) )
               end if
               st = st - 1          
            end if
!
!-       Function evaluations. Functions with a number of pk2_t input arguments determined at 
!        run-time (optional arguments or variable number of arguments):  
!
         case ( NOP+54 )
            k0 = st-narg(i)+1
            pdata(k0) = norm ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+56 )
            k0 = st-narg(i)+1
            pdata(k0) = reshape ( matrs=[(pdata(k),k=k0,st)] )
            st = k0
         
         case ( NOP+58 )
            k0 = st-narg(i)+1
            pdata(k0) = zeros ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+59 )
            k0 = st-narg(i)+1
            pdata(k0) = falses ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+60 )
            k0 = st-narg(i)+1
            pdata(k0) = ones ( matrs=[(pdata(k),k=k0,st)] )
            st = k0
            
         case ( NOP+61 )   
            k0 = st-narg(i)+1
            pdata(k0) = eye ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+62 )
            k0 = st-narg(i)+1
            pdata(k0) = rand ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+63 )
            k0 = st-narg(i)+1
            pdata(k0) = diag ( matrs=[(pdata(k),k=k0,st)] )
            st = k0
            
         case ( NOP+64 )
            k0 = st-narg(i)+1
            pdata(k0) = tril ( matrs=[(pdata(k),k=k0,st)] )
            st = k0
            
         case ( NOP+65 )
            k0 = st-narg(i)+1
            pdata(k0) = triu ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+66 )
            k0 = st-narg(i)+1
            pdata(k0) = colon ( matrs=[(pdata(k),k=k0,st)] )
            st = k0
            
         case ( NOP+67 )
            k0 = st-narg(i)+1
            pdata(k0) = date ( matrs=[(pdata(k),k=k0,st)] )
            st = k0
                
         case ( NOP+69 )
            k0 = st-narg(i)+1
            pdata(k0) = mergemats ( matrs=[(pdata(k),k=k0,st)], delim=handle%csep(i)%str)
            st = k0

         case ( NOP+70 )    
            k0 = st-narg(i)+1
            pdata(k0) = min ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+71 )     
            k0 = st-narg(i)+1
            pdata(k0) = max ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+72 )   
            k0 = st-narg(i)+1
            pdata(k0) = sum ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+73 )     
            k0 = st-narg(i)+1
            pdata(k0) = mean ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+74 )      
            k0 = st-narg(i)+1
            pdata(k0) = prod ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+77 )
            k0 = st-narg(i)+1
            pdata(k0) = pk2f_HELP ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+78 )
            k0 = st-narg(i)+1
            pdata(k0) = readmat ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+79 )
            k0 = st-narg(i)+1
            pdata(k0) = writemat ( matrs=[(pdata(k),k=k0,st)] )
            st = k0
            
         case ( NOP+82 )
            k0 = st-narg(i)+1
            pdata(k0) = meshgrid ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+83 )
!
!-          caution: this function has a variable number of outputs (depending of nsdata):
!         
            k0 = st-narg(i)+1
            if ( nsdata == 0 ) then
               pdata(k0) = sort ( matrs=[(pdata(k),k=k0,st)] )
            else   
               pdata(k0) = sort ( matrs=[(pdata(k),k=k0,st)], res2=sdata(1) )
               nsused = max(nsused,1_Ikind)
            end if   
            st = k0
                          
         case ( NOP+84 )
            k0 = st-narg(i)+1
            pdata(k0) = all ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+85 )
            k0 = st-narg(i)+1
            pdata(k0) = any ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+87 )
            k0 = st-narg(i)+1
            pdata(k0) = randi ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+88 )
            k0 = st-narg(i)+1
            pdata(k0) = randperm ( matrs=[(pdata(k),k=k0,st)] )
            st = k0
            
         case ( NOP+91 )
            k0 = st-narg(i)+1
            pdata(k0) = cov ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+93 )
            k0 = st-narg(i)+1
            pdata(k0) = convstr ( matrs=[(pdata(k),k=k0,st)] )
            st = k0

         case ( NOP+94 )
!
!-          caution: this function has a variable number of outputs (depending of nsdata):
!         
            k0 = st-narg(i)+1
            if ( nsdata == 0 ) then
               pdata(k0) = replace ( matrs=[(pdata(k),k=k0,st)] )
            else   
               pdata(k0) = replace ( matrs=[(pdata(k),k=k0,st)], res2=sdata(1) )
               nsused = max(nsused,1_Ikind)
            end if   
            st = k0
!
!-       Values of a variable (or a subset (sub-matrix) of them):
!                              
         case ( NOP+NFC+1: )         
            ivar = iop-(NFC+NOP)
            
            if ( ivar > nvars .or. ivar <= 0 ) then
               flagerr = err_t ( stat = IERROR, where = HERE, msg = 'Wrong variable Id' )
               return
            else if ( handle%involvedVar(i)%str /= vars(ivar)%name ) then
!
!-             For the case where the list of variables has changed since the analysis,
!              try to find again this variable in the list:
!
               flagerr = err_t ( stat = WARNING, where = HERE, &
                                  msg = 'The list of variables has changed' )
               ivar = pk2Interpreter_FindVarId ( handle%involvedVar(i)%str, vars )
               if ( ivar == 0 ) then
                  flagerr = err_t ( stat = UERROR, where = HERE, msg = 'Unknown variable "'//&
                                    handle%involvedVar(i)%str//'"' )
                  return
               else
                  handle%operations(i) = NOP+NFC+ivar
               end if
            end if   
            
            if ( narg(i) == 0 ) then ! e.g. a
               st = st + 1
               pdata(st) = vars(ivar) 
            else if ( narg(i) == 1 ) then ! e.g. a(m), a(3:2:15), a(:)
               pdata(st) = extracmat ( vars(ivar), pdata(st) ) 
            else if ( narg(i) == 2)  then ! e.g. a(m,n), a(:,21), a(:,:)
               pdata(st-1) = extracmat ( vars(ivar), pdata(st-1), pdata(st) )    
               st = st - 1               
            end if
!
!-       Internal error:
!                              
         case default
            flagerr = err_t (stat = IERROR, where = HERE, msg = HERE)
            return
            
         end select

         if (opflag%code > 0) then
            call opflag%AddTrace(HERE)
            call err_moveAlloc (from = opflag, to = flagerr)
            return
         end if   

      end do
      
      if ( nsused /= nsdata ) then
         allocate( answer(0) )
         write(cnum,'(i0)')nsused+1
         flagerr%mesg = &
         "The maximum number of outputs ("//trim(cnum)//") of the invoqued function "// &
         "differs from the requested one (" 
         write(cnum,'(i0)')nsdata+1
         flagerr%mesg = flagerr%mesg // trim(cnum)//")."
         call flagerr%Set(UERROR, HERE, flagerr%mesg)
         return
      end if   
               
      allocate( answer(nsused+1) )
      
      call pk2_moveAlloc ( from = pdata(1), to = answer(1), movename = .false. )
      if (opflag%code > 0) then
         call opflag%AddTrace(HERE)
         call err_moveAlloc (from = opflag, to = flagerr)
         return
      end if
      
      do i = 1, nsused
         call pk2_moveAlloc ( from = sdata(i), to = answer(i+1), movename = .false. )
         if (opflag%code > 0) then
            call opflag%AddTrace(HERE)
            call err_moveAlloc (from = opflag, to = flagerr)
            return
         end if
      end do
   
   end associate ! ( narg => handle%narg, pdata => handle%pdata )
!
!- If the expression was just like 'a', where "a" is one of the variables vars(:), then set 
!  the name of the answer (answer(1)%name) to the name of the corresponding variable:
!
   if ( nsdata == 0 .and. handle%noperat == 1 ) then
      iop = handle%operations(1) ; ivar = iop - (NFC+NOP)
      if ( ivar > 0 .and. handle%narg(1) == 0 ) answer(1)%name = vars(ivar)%name
   end if   
              
   END SUBROUTINE pk2Interpreter_EvalExpr_v1


!= Modified (riad) ===========================================================================  
   SUBROUTINE pk2Interpreter_EvalExpr_v2 ( handle, vars, answer, flagerr, nanswer )
!============================================================================================= 
   type   (hdl_t),              intent(in out) :: handle
   class  (pk2_t),              intent(in    ) :: vars(:)
   type   (pk2_t), allocatable, intent(in out) :: answer(:)
   type   (err_t),              intent(in out) :: flagerr   
   integer(Ikind), optional,    intent(in    ) :: nanswer
!---------------------------------------------------------------------------------------------    
!  This routine evaluate the expression supplied after its analysis by the tokens analyzer
!
!  Inputs:
!  ------
!
!  . handle : the analysis of the expression done by the tokens analyzer
!  . vars   : the pk2 array containing the values of the variables
!  . nAnswer: (optional) expected number of answers (default: 1)
!
!  Outputs:
!  -------
!
!  . answer : a pk2 array corresponding to the result(s)
!  . flagerr: error/warning status with the following convention:
!             . flagErr%code = 0: no error and no warning
!             . flagErr%code > 0: an error has occured 
!             . flagErr%code < 0: a warning message is available
!             . and flagErr%mesg gives the corresponding error or warning message.
!
!  Note: in this second version and in order to avoid the overhead of temporary copies and 
!        assignments I make use of subroutines instead of functions.
!---------------------------------------------------------------------------------------------    

!- local variables --------------------------------------------------------------------------- 
   character(len=* ), parameter   :: HERE = 'pk2Interpreter_EvalExpr_v2'
   integer  (Ikind )              :: st, dt, assignto
   integer  (Ikind )              :: i, iop, k, k0, ivar, nvars, nans, nsdata, nsused, err, nres2
   character(len=99)              :: cnum
   type     (pk2_t ), save        :: res
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return

   if ( .not. handle%initialized ) then
      call flagerr%Set(UERROR, HERE, 'The handle must first be initialized.') 
      return
   end if   
   
   if ( present(nanswer) ) then
      nans = max(1_Ikind,nanswer)
   else
      nans = 1                    
   end if      
   
   nsdata = nans - 1 ! number of secondary data expected
   nsused = 0        ! actually used
   
   nvars = size(vars)
            
   if ( nsdata > size(handle%sdata) ) then
!
!-    handle%sdata was pre-allocated with an insufficient size. Resize it:
!   
      deallocate(handle%sdata) ; allocate(handle%sdata(nsdata), stat = err)
      if ( err /= 0 ) then
         call flagerr%Set(IERROR, HERE, 'Allocation failure') 
         return
      end if         
      do i = 1, nsdata
         handle%sdata(i)%name = ''
      end do
   end if     
   
    ! print*,'stokens: | ',(handle%stokens(i)%str,' | ',i=1,handle%ntokens)
    ! print*,'op.    :',(handle%operations(i), i=1,handle%noperat)
    ! print*,'narg   :',(handle%narg(i)      , i=1,handle%noperat)
    ! print*,'csep   :',(handle%csep(i)%str  , i=1,handle%noperat)
!
!- Initialize the internal flag "opflag" (used in procedures of bk2, pk2, pk2f modules and
!  will serve to report any error or warning):
!   
   if ( opflag%code > IZERO ) return ! opflag = err_t ()
!
!- Start performing the "handle%noperat" operations:
!   
   ASSOCIATE ( narg => handle%narg, pdata => handle%pdata, sdata => handle%sdata )
   
      st = 0_Ikind ; dt = 1_Ikind
      
      do i = 1, handle%noperat 
      
         iop = handle%operations(i)
      
         if ( st - narg(i) + 1 <= 0 ) then
            call flagerr%Set(UERROR, HERE, 'Expression has an error') 
            return
         end if   
            
         assignto = st-1
         
         select case ( iop )  
!
!-       numeric, logical or string data:
!
         case ( 0 )
            st = st + 1 ; assignto = IZERO
            call pk2_assign ( lhs=pdata(st), rhs=handle%scalars(dt) )
            dt = dt + 1
!
!-       arithmetic or boolean operations (from 1 to nop):
!                        
         case ( 1 ) ! a + b (addition)
            call pk2_subAdd (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1 
            
         case ( 2 ) ! a - b (subtraction)
            call pk2_subSub (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1

         case ( 3 ) ! a | b (or)
            call pk2_subOr (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1
      
         case ( 4 ) ! -a (minus)
            assignto = st
            call pk2_subMinus (a=pdata(st), res=res)
            
         case ( 5 ) ! ~a (not)
            assignto = st
            call pk2_subNot (a=pdata(st), res=res)
                        
         case ( 6 ) !  a * b (matrix-matrix product)
            call pk2_subMult (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1

         case ( 7 ) ! a / b (divide: mat/scal or scal/mat (mat of inverses))
            call pk2_subDiv (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1                                            

         case ( 8 ) ! a .* b (element-wise mutliplication)
            call pk2_subEMult (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1

         case ( 9 ) ! a ./ b (element-wise divide)
            call pk2_subEDiv (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1 
                        
         case ( 10 ) ! a & b (and)
            call pk2_subAnd (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       

         case ( 11 ) ! lin. syst. solve
            call pk2_smldivide (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       
            
         case ( 12 ) ! a^b (exponentiation)
            call pk2_subPow (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       

         case ( 13 ) ! a.^b (element-wise exponentiation)
            call pk2_subEPow (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       
!
!-       relational operators:
!
         case ( 14 ) ! a == b
            call pk2_subEq (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       

         case ( 15 ) ! a /= b 
            call pk2_subNe (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       

         case ( 16 )  ! a < b
            call pk2_subLt (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       

         case ( 17 ) ! a > b
            call pk2_subGt (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       

         case ( 18 ) ! a <= b
            call pk2_subLe (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       

         case ( 19 ) ! a >= b
            call pk2_subGe (a=pdata(st-1), b=pdata(st), res=res)
            st = st - 1       

         case ( 28 ) ! ' (conj. transpose)
            assignto = st
            call s_trans   (a=pdata(st), res=res, conj=.true.)

         case ( 29 ) ! .' (transpose)
            assignto = st
            call s_trans   (a=pdata(st), res=res, conj=.false.)

!
!-       Function evaluations. Functions that have exactly 0 input argument:
!
         case ( NOP+75 )
            if ( narg(i) /= 0 ) then
               call flagerr%Set(UERROR, HERE, '0 argument expected for << pwd >>') 
               return
            else           
               st = st + 1 ; assignto = st
               call s_pwd (res)
           end if

         case ( NOP+76 )
            if ( narg(i) /= 0 ) then
               call flagerr%Set(UERROR, HERE, '0 argument expected for << cputime >>') 
               return
            else           
               st = st + 1 ; assignto = st
               call s_CPUTIME (res)
           end if         
!
!-       Function evaluations. Functions that have exactly 1 pk2_t input argument: 
!         
         case ( NOP+7:NOP+53, NOP+80, NOP+81, NOP+68, NOP+89, NOP+96, NOP+98:NOP+102 )
            assignto = st
            if ( narg(i) /= 1 ) then
               call flagerr%Set(UERROR, HERE, & 
                  '1 argument expected for the function << '//FuncList(iop-NOP)%str //' >>' ) 
               return                          
            else
               if      ( iop == NOP+ 7 ) then
                  call s_SIN     (a=pdata(st), res=res)
               else if ( iop == NOP+ 8 ) then
                  call s_COS     (a=pdata(st), res=res)
               else if ( iop == NOP+ 9 ) then
                  call s_TAN     (a=pdata(st), res=res)
               else if ( iop == NOP+10 ) then
                  call s_ASIN    (a=pdata(st), res=res)
               else if ( iop == NOP+11 ) then
                  call s_ACOS    (a=pdata(st), res=res)
               else if ( iop == NOP+12 ) then
                  call s_ATAN    (a=pdata(st), res=res)
               else if ( iop == NOP+13 ) then 
                  call s_SIND    (a=pdata(st), res=res)
               else if ( iop == NOP+14 ) then 
                  call s_COSD    (a=pdata(st), res=res)
               else if ( iop == NOP+15 ) then 
                  call s_TAND    (a=pdata(st), res=res)
               else if ( iop == NOP+16 ) then 
                  call s_ASIND   (a=pdata(st), res=res)
               else if ( iop == NOP+17 ) then 
                  call s_ACOSD   (a=pdata(st), res=res)
               else if ( iop == NOP+18 ) then 
                  call s_ATAND   (a=pdata(st), res=res)
               else if ( iop == NOP+19 ) then
                  call s_SINH    (a=pdata(st), res=res)
               else if ( iop == NOP+20 ) then
                  call s_COSH    (a=pdata(st), res=res)
               else if ( iop == NOP+21 ) then
                  call s_TANH    (a=pdata(st), res=res)
               else if ( iop == NOP+22 ) then 
                  call s_LOG     (a=pdata(st), res=res)
               else if ( iop == NOP+23 ) then 
                  call s_LOG10   (a=pdata(st), res=res)
               else if ( iop == NOP+24 ) then 
                  call s_EXP    (a=pdata(st), res=res)
               else if ( iop == NOP+25 ) then 
                  call s_SQRT    (a=pdata(st), res=res)
               else if ( iop == NOP+26 ) then 
                  call s_ABS     (a=pdata(st), res=res)
               else if ( iop == NOP+27 ) then 
                  call s_ERF     (a=pdata(st), res=res)
               else if ( iop == NOP+28 ) then 
                  call s_ERFC    (a=pdata(st), res=res)
               else if ( iop == NOP+29 ) then 
                  call s_GAMMA   (a=pdata(st), res=res)
               else if ( iop == NOP+30 ) then 
                  call s_FACTOR  (a=pdata(st), res=res)
               else if ( iop == NOP+31 ) then 
                  call s_NINT    (a=pdata(st), res=res)
               else if ( iop == NOP+32 ) then 
                  call s_ANINT   (a=pdata(st), res=res)
               else if ( iop == NOP+33 ) then 
                  call s_AINT    (a=pdata(st), res=res)
               else if ( iop == NOP+34 ) then 
                  call s_FLOOR   (a=pdata(st), res=res)
               else if ( iop == NOP+35 ) then 
                  call s_CEIL    (a=pdata(st), res=res)
               else if ( iop == NOP+36 ) then 
                  call s_SIGN    (a=pdata(st), res=res)
               else if ( iop == NOP+37 ) then 
                  call s_HEAV    (a=pdata(st), res=res)
               else if ( iop == NOP+38 ) then 
                  call s_REAL    (a=pdata(st), res=res)
               else if ( iop == NOP+39 ) then 
                  call s_IMAG    (a=pdata(st), res=res)
               else if ( iop == NOP+40 ) then 
                  call s_CONJ    (a=pdata(st), res=res)
               else if ( iop == NOP+41 ) then 
                  call s_TRACE   (a=pdata(st), res=res)
               else if ( iop == NOP+42 ) then 
                  call s_TRANS   (a=pdata(st), res=res, conj=.true.)
               else if ( iop == NOP+43 ) then 
                  call s_TRANS   (a=pdata(st), res=res, conj=.false.)
               else if ( iop == NOP+44 ) then 
                  call s_DET     (a=pdata(st), res=res)
               else if ( iop == NOP+45 ) then 
                  call s_INV     (a=pdata(st), res=res)
               else if ( iop == NOP+47 ) then 
                  call s_NUMEL   (a=pdata(st), res=res)
               else if ( iop == NOP+48 ) then 
                  call s_FIND    (a=pdata(st), res=res)
               else if ( iop == NOP+49 ) then 
                  call s_NUM2STR (a=pdata(st), res=res)
               else if ( iop == NOP+50 ) then 
                  call s_STR2NUM (a=pdata(st), res=res)
               else if ( iop == NOP+51 ) then 
                  call s_MAGIC   (a=pdata(st), res=res)
               else if ( iop == NOP+52 ) then 
                  call s_IS_SYMM (a=pdata(st), res=res)
               else if ( iop == NOP+53 ) then 
                  call s_IS_SKEW (a=pdata(st), res=res)
               else if ( iop == NOP+80 ) then 
                  call s_TRIM    (a=pdata(st), res=res)
               else if ( iop == NOP+81 ) then 
                  call s_LENGTH  (a=pdata(st), res=res)
               else if ( iop == NOP+46 ) then
!
!-                caution: this function has a variable number of outputs:
!               
                  if ( nsdata == 0 ) then
                     call s_SIZE (a=pdata(st), res=res)
                  else 
                     call s_SIZE (a=pdata(st), res=res, res2=sdata(1))
                     nsused = max(nsused,1_Ikind)
                  end if                     
               else if ( iop == NOP+68 ) then
!
!-                caution: this function has a variable number of outputs:
!                              
                  if ( nsdata == 0 ) then
                     call s_EIG (a=pdata(st), eigval=res)
                  else
                     call s_EIG (a=pdata(st), eigval=res, eigvec=sdata(1))
                     nsused = max(nsused,1_Ikind)
                  end if
               else if ( iop == NOP+89 ) then
!
!-                caution: this function has a variable number of outputs:
!                                             
                  if ( nsdata == 0 ) then
                     call s_LU (a=pdata(st), res=res)
                  else if ( nsdata == 1 ) then
                     call s_LU (a=pdata(st), res=res, res2=sdata(1))
                     nsused = max(nsused,1_Ikind)
                  else
                     call s_LU (a=pdata(st), res=res, res2=sdata(1), res3=sdata(2))
                     nsused = max(nsused,2_Ikind)
                  end if  
                  
               else if ( iop == NOP+96 ) then 
                  call s_DEV (a=pdata(st), res=res)
               else if ( iop == NOP+98 ) then 
                  call s_SIZEOF (a=pdata(st), res=res)                  
               else if ( iop == NOP+99 ) then 
                  call s_TYPEOF (a=pdata(st), res=res)   
                  
               else if ( iop == NOP+100 ) then
!
!-                caution: this function has a variable number of outputs:
!                                             
                  if ( nsdata == 0 ) then
                     call s_SVD (a=pdata(st), s=res)
                  else if ( nsdata == 1 ) then
                     call s_SVD (a=pdata(st), u=res, s=sdata(1))
                     nsused = max(nsused,1_Ikind)
                  else
                     call s_SVD (a=pdata(st), u=res, s=sdata(1), v=sdata(2))
                     nsused = max(nsused,2_Ikind)
                  end if                 
                  
               else if ( iop == NOP+101 ) then
!
!-                caution: this function has a variable number of outputs:
!                                        
                  if ( nsdata == 0 ) then
                     call s_Poldec (F=pdata(st), R=res)
                  else 
                     call s_Poldec (F=pdata(st), R=res, U=sdata(1))
                     nsused = max(nsused,1_Ikind)
                  end if                 

               else if ( iop == NOP+102 ) then 
                  call s_SINC (a=pdata(st), res=res)   

               end if  

            end if  
!
!-       Function evaluations. Functions that have exactly 2 pk2_t input arguments:
!         
         case ( NOP+1:NOP+6, NOP+55, NOP+57, NOP+86, NOP+90, NOP+92, NOP+95, NOP+97 )
            assignto = st-1
            if ( narg(i) /= 2 ) then
               call flagerr%Set(UERROR, HERE, & 
                  '2 arguments expected for the function << '//FuncList(iop-NOP)%str//' >>' )
               return   
            else               
               if      ( iop == NOP+1 ) then
                  call pk2_subEq         (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+2 ) then 
                  call pk2_subNe         (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+3 ) then 
                  call pk2_subLt         (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+4 ) then 
                  call pk2_subGt         (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+5 ) then 
                  call pk2_subLe         (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+6 ) then 
                  call pk2_subGe         (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+55 ) then
                  call s_CROSS     (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+57 ) then 
                  call s_MLDIVIDE  (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+86 ) then 
                  call s_MOD       (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+90 ) then 
                  call s_LUSOLV    (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+92 ) then
!
!-                caution: this function has a variable number of outputs:
!                 
                  if ( nsdata == 0 ) then
                     call s_REGLIN (x=pdata(st-1), y=pdata(st), res=res)
                  else if ( nsdata == 1 ) then
                     call s_REGLIN (x=pdata(st-1), y=pdata(st), res=res, &
                                                                      b  =sdata(1   )  )
                     nsused = max(nsused,1_Ikind)
                  else
                     call s_REGLIN (x=pdata(st-1), y=pdata(st), res=res, &
                                                                      b  =sdata(1   ), &
                                                                      sig=sdata(2   )  )
                     nsused = max(nsused,2_Ikind)
                  end if   
               else if ( iop == NOP+95 ) then
                  call s_PART      (a=pdata(st-1), b=pdata(st), res=res)
               else if ( iop == NOP+97 ) then
                  call s_INTTRAP   (a=pdata(st-1), b=pdata(st), res=res)
                  
               end if
               
               st = st - 1                         
            end if
!
!-       Function evaluations. Functions with a number of pk2_t input arguments determined at 
!        run-time (optional arguments or variable number of arguments):  
!
         case ( NOP+54 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_NORM     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+56 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_RESHAPE  (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0
         
         case ( NOP+58 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_ZEROS    (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+59 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_FALSES   (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+60 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_ONES     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0
            
         case ( NOP+61 )   
            k0 = st-narg(i)+1 ; assignto = k0
            call s_EYE      (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+62 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_RAND     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+63 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_DIAG     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0
            
         case ( NOP+64 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_TRIL     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0
            
         case ( NOP+65 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_TRIU     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+66 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_COLON    (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0
            
         case ( NOP+67 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_DATE     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0
                
         case ( NOP+69 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_MERGEMATS   (matrs=[(pdata(k),k=k0,st)], res=res, &
                                      delim=handle%csep(i)%str)
            st = k0

         case ( NOP+70 )    
            k0 = st-narg(i)+1 ; assignto = k0
            call s_MINMAX   (matrs=[(pdata(k),k=k0,st)], res=res, &
                                      minmax='min')
            st = k0

         case ( NOP+71 )     
            k0 = st-narg(i)+1 ; assignto = k0
            call s_MINMAX   (matrs=[(pdata(k),k=k0,st)], res=res, &
                                      minmax='max')
            st = k0

         case ( NOP+72 )   
            k0 = st-narg(i)+1 ; assignto = k0
            call s_SUM      (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+73 )     
            k0 = st-narg(i)+1 ; assignto = k0
            call s_MEAN     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+74 )      
            k0 = st-narg(i)+1 ; assignto = k0
            call s_PROD     (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+77 )
            k0 = st-narg(i)+1 ; assignto = IZERO
            pdata(k0) = pk2f_HELP    (matrs=[(pdata(k),k=k0,st)])
            st = k0

         case ( NOP+78 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_READMAT  (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+79 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_WRITEMAT    (matrs=[(pdata(k),k=k0,st)], status=res)
            st = k0
            
         case ( NOP+82 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_MESHGRID (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+83 )
!
!-          caution: this function has a variable number of outputs (depending of nsdata):
!         
            k0 = st-narg(i)+1 ; assignto = k0
            if ( nsdata == 0 ) then
               call s_SORT  (matrs=[(pdata(k),k=k0,st)], res=res)
            else   
               call s_SORT  (matrs=[(pdata(k),k=k0,st)], res=res, res2=sdata(1))
               nsused = max(nsused,1_Ikind)
            end if   
            st = k0
                          
         case ( NOP+84 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_ALL      (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+85 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_ANY      (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+87 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_RANDI    (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+88 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_RANDPERM (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0
            
         case ( NOP+91 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_COV      (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+93 )
            k0 = st-narg(i)+1 ; assignto = k0
            call s_CONVSTR  (matrs=[(pdata(k),k=k0,st)], res=res)
            st = k0

         case ( NOP+94 )
!
!-          caution: this function has a variable number of outputs (depending of nsdata):
!         
            k0 = st-narg(i)+1
            assignto = k0
            if ( nsdata == 0 ) then
               call s_REPLACE  (matrs=[(pdata(k),k=k0,st)], res=res)
            else   
               call s_REPLACE  (matrs=[(pdata(k),k=k0,st)], res=res, &
                                         res2=sdata(1))
               nsused = max(nsused,1_Ikind)
            end if   
            st = k0
!
!-       Values of a variable (or a subset (sub-matrix) of them):
!                              
         case ( NOP+NFC+NFU+1: )         
            ivar = iop-(NFC+NOP+NFU)

            if ( ivar > nvars .or. ivar <= 0 ) then
               call flagerr%Set(UERROR, HERE, 'Wrong variable Id')
               return
            else if ( handle%involvedVar(i)%str /= vars(ivar)%name ) then
!
!-             For the case when the list of variables has changed since the analysis,
!              try to find again this variable in the list:
!
               call flagerr%Set(WARNING, HERE, 'The list of variables has changed')
               ivar = pk2Interpreter_FindVarId ( handle%involvedVar(i)%str, vars )
               if ( ivar == 0 ) then
                  call flagerr%Set(UERROR, HERE, 'Unknown variable "'//  &
                                   handle%involvedVar(i)%str//'"')
                  return
               else
                  handle%operations(i) = NOP+NFC+NFU+ivar
               end if
            end if   
            
            if ( narg(i) == 0 ) then ! e.g. a
               st = st + 1 ; assignto = IZERO
               call pk2_assign (pdata(st), vars(ivar))
            else if ( narg(i) == 1 ) then ! e.g. a(m), a(3:2:15), a(:)
               assignto = st
               call s_EXTRACMAT (a=vars(ivar), res=res, vindx=pdata(st))
            else if ( narg(i) == 2 ) then ! e.g. a(m,n), a(:,21), a(:,:)
               assignto = st-1
               call s_EXTRACMAT (a=vars(ivar), res=res, vindx=pdata(st-1), &
                                       vjndx=pdata(st))
               st = st - 1               
            end if
            
         case ( NOP+NFC+1 : NOP+NFC+NFU )
            ivar = iop-(NFC+NOP)
            k0 = st-narg(i)+1 ; assignto = k0
            call s_userProcDriver ( ifunc=ivar, inputs=[(pdata(k),k=k0,st)], &
                                    firstOut=res, optOuts=sdata, nOpt = nres2 )
            nsused = min(nres2,nans-1)
            st = k0                 
!
!-       Internal error:
!                              
         case default
            call flagerr%Set(UERROR, HERE, 'Unknown operation')
            return
            
         end select

         if ( opflag%code > 0 ) then
            call opflag%AddTrace(HERE)
            call err_moveAlloc (from = opflag, to = flagerr)            
            return
         end if   
         
         if ( assignto > 0 ) call pk2_assign(pdata(assignto),res)
      end do
      
      if ( nsused /= nsdata ) then
         if (allocated(answer)) deallocate(answer) !###    
         allocate( answer(0) )
         call flagerr%Set(UERROR, HERE, "The maximum number of outputs ("//i2a(nsused+1)// &
         ") of the invoqued function differs from the requested one ("//i2a(nsdata+1)//")" )
         return
      end if   

      if (allocated(answer)) then !###
         if (size(answer) /= nsused+1) deallocate(answer) !###
       end if
       if (.not. allocated(answer)) allocate( answer(nsused+1) ) !###
                     
      call pk2_moveAlloc ( from = pdata(1), to = answer(1), movename = .false. )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         call err_moveAlloc (from = opflag, to = flagerr)
         return
      end if
      
      do i = 1, nsused
         call pk2_moveAlloc ( from = sdata(i), to = answer(i+1), movename = .false. )
         if ( opflag%code > 0 ) then
            call opflag%AddTrace(HERE)
            call err_moveAlloc (from = opflag, to = flagerr)
            return
         end if
      end do
   
   END ASSOCIATE ! ( narg => handle%narg, pdata => handle%pdata )
!
!- If the expression was just like 'a', where "a" is one of the variables vars(:), then set 
!  the name of the answer (answer(1)%name) to the name of the corresponding variable:
!
   if ( nsdata == 0 .and. handle%noperat == 1 ) then
      iop = handle%operations(1) ; ivar = iop - (NFC+NOP+NFU)
      if ( ivar > 0 .and. handle%narg(1) == 0 ) answer(1)%name = vars(ivar)%name
   end if   
              
   END SUBROUTINE pk2Interpreter_EvalExpr_v2

!= Modified (riad) ===========================================================================  
   SUBROUTINE pk2Interpreter_Identifica ( expr, flagerr ) 
!=============================================================================================    
   character(len=*), intent(in    ) :: expr 
   type     (err_t), intent(in out) :: flagerr
!--------------------------------------------------------------------------------------------- 
!  Detects possible syntax errors in the user expression "expr". 
!
!  Caution: must be called after 
!  . replacement of simple quotation marks (') used for string by double quotation marks (")
!  . blank spaces removing in non-string part
!  (tasks that have been done by pk2Interpreter_convert)
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter   :: HERE  = "pk2Interpreter_Identifica"  ,   &
                                    ALPH  = '_%#@0123456789*-/\:.~'      //  &
                                            'abcdefghijklmnopqrstuvwxyz' //  &
                                            'ABCDEFGHIJKLMNOPQRSTUVWXYZ'    
   integer  (Ikind)              :: lexpr, i, opcl, Id, err
   character(len=1)              :: ch1, ch2
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return

   lexpr = len(trim(expr))
   
   if ( lexpr == 0 ) return
!
!- Check for possible unbalanced brackets, parentheses or braces in "expr"
!   
   if ( index(expr,'"') == 0 ) then   
      if ( .not. util_IsBalanced2 (expr, '()[]{}', flagerr) ) then
         call flagerr%Set(UERROR, HERE, 'Unbalanced parentheses, brackets or braces')
         return
      end if
   end if
               
!
!- If "expr" starts with an invalid operator --> error:
! 
   ch1 = expr(1:1)
   id = pk2Interpreter_FindId ( ch1, OperCode, IONE, size(OperCode), IONE )
   if ( id /= 0 ) then
      if ( .not. StartEnd(id,1) ) then
         call flagerr%Set( UERROR, HERE,                                                  &
                  'Expression starting with an illegal operator ('//OperList(id)%str//')' ) 
         return
      end if
   end if   
!
!- If "expr" ends with an invalid operator --> error:
!
   ch1 = expr(lexpr:lexpr)
   id = pk2Interpreter_FindId ( ch1, OperCode, IONE, size(OperCode), IONE )
   if ( id /= 0 ) then
      if ( .not. StartEnd(id,2) ) then
         call flagerr%Set(UERROR, HERE,                                            &
                          'Expression ending with an illegal operator ('//ch1//')' ) 
         return
      end if
   end if   
!
!- Test the validity of operator sequences (that are not part of text)
!
   err = 0 ; opcl = 0
   do i = 1, lexpr-1
      ch1 = expr(i:i)

      if ( ch1 == '"' ) then ; if (opcl == 1) then ; opcl=0 ; else ; opcl=1 ; end if ; end if   
            
      if ( opcl == 1 ) cycle ! ch1 is within a part of a text: move to the next character

      ch2 = expr(i+1:i+1)
      
      if ( ch1 == '"' .and. index(ALPH,ch2) /= 0 ) then
!
!-       error: a closing " followed by an alphanumerical symbol
!
         err = 1 ; exit
      end if
!
!-    If ch1 is an operator:
!      
      Id = pk2Interpreter_FindId ( ch1, OperCode, IONE, size(OperCode), IONE )
      
      if ( Id  ==  0 ) cycle ! ch1 is not an operator: move to the next character
!
!-    See if the sequence formed by ch1 and the next character is a legal one:
!      
      if ( index(ForbidSq(id)%str,ch2) /= 0 ) then
!
!-       error: ch1 is an operator followed by a forbiden symbol for this operator
!        (ForbidSq is defined in pk2Interpreter_FuncAndOperList)
!
         err = 2 ; exit
      end if
      
   end do
   
   if ( err == 1 ) then   
      call flagerr%Set(UERROR, HERE, 'Illegal operation (1): '//ch1//ch2            //NLT// &
      "May be a problem with quotation marks. Recall that single quotes can be part"//NLT// &
      "of a text delimited by double quotes, but the reverse is not possible, e.g."// NLT// &
      '"'//"'hello man', he said"//'" is valid but not '//"'"// '"hello man", he said'//"'" ) 
      return
   end if         

   if ( err == 2 ) then   
      i = pk2Interpreter_FindId ( ch2, OperCode, IONE, size(OperCode), IONE )
      if ( i == 0 ) then
         call flagerr%Set(UERROR, HERE, &
                  'Illegal operation (2): '//OperList(id)%str//ch2) 
      else
         call flagerr%Set(UERROR, HERE, &
                  'Illegal operation (2): '//OperList(id)%str//OperList(i)%str) 
      end if
      return
   end if         

!
!- Other particular cases to exclude:
!
   if ( expr == '()' .or. expr == '(:)' .or. expr == '[:]' ) then
      call flagerr%Set(UERROR, HERE, 'Illegal operation (3): '//expr) 
      return
   end if      
      
   END SUBROUTINE pk2Interpreter_Identifica


!= Modified (riad) ===========================================================================  
   SUBROUTINE pk2Interpreter_Convert ( expr, flagerr )
!=============================================================================================  
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Does some conversions (only on regular part, i.e. on non-text portion of expr) and check 
!  for syntax errors.
!
!  In particular,
!  . Replace, if appropriate, ' by " used as text delimiters
!  . Remove blank spaces
!  . Replace array constructor [] by merge() (i.e. [xxx] is replaced by merge(xxx))
!  . Replace the remaining ' by trans() and .' by transp() (i.e. x' is replaced by trans(x) 
!    and x.' by transp(x))
!  . Replace the symbol ":" by colon() (ie. x:y:z is replaced by colon(x,z,y)). Note that
!    XX(:) is replaced by XX(-1), XX(:,yy) by XX(-1,yy) and XX(yy,:) by XX(yy,-1), the index
!    '-1' will be used as a code.
!
!  Added (05/24):
!  . Replace all operators by their codes given in OperCode (defined in the subroutine 
!    pk2Interpreter_FuncAndOperList)
!-------------------------------------------------------------R.H. 01/18, 04/18, 06/18, 05/24

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2Interpreter_Convert'
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return
!
!- Replace, if appropriate, single quotation marks (') by double quotation ones (")
!  (in text parts only. The possible remaining single quotes are for transposition):
!
   call pk2Interpreter_ConvertQuotes ( expr, flagerr )
   error_TraceNreturn(flagerr, HERE)
!   
!- Remove (non-double-quoted) spaces or replace them by a ',' if between '[ ]':
!
   call pk2Interpreter_ConvertSpaces ( expr, flagerr )
   error_TraceNreturn(flagerr, HERE)
!
!- Replace all operators by their codes given in OperCode:
!
   call pk2Interpreter_ConvertOperators ( expr, flagerr )
   error_TraceNreturn(flagerr, HERE)   
!
!- Check for basic syntax errors in the expression:
!   
   call pk2Interpreter_Identifica ( expr, flagerr )
   error_TraceNreturn(flagerr, HERE)
!
!- Replace all occurences of '[...]' with 'merge(...)'  (if not part of text):
!
   call pk2Interpreter_ConvertMatrix ( expr, flagerr )
   error_TraceNreturn(flagerr, HERE)
!
!- Replace an expression like f(x=v, y=w, z=t) by f(3, "x", "y", "z", v, w, t):
!
   call pk2Interpreter_ConvertArgKeywd ( expr, flagerr )
   error_TraceNreturn(flagerr, HERE)
!
!- Replace all occurences of a sequence of unary operators (e.g. "++" or "++-" or "~~") by
!  what is appropriate (if not part of text)
!   
   call pk2Interpreter_ConvertMultiUnary ( expr, flagerr )  
   error_TraceNreturn(flagerr, HERE)
!
!- replace .' (transposition) by "transp(...)" (if not part of text)
!
!!   call pk2Interpreter_ConvertTrans ( expr, 'transp', flagerr )
!!   error_TraceNreturn(flagerr, HERE)
!
!- replace symbol ' (conjugate-transposition) by "trans(...)" (if not part of text)
!
!!   call pk2Interpreter_ConvertTrans ( expr, 'trans' , flagerr )
!!   error_TraceNreturn(flagerr, HERE)
!
!- For the couple operators " : : " (colon operator)
!         
   call pk2Interpreter_ConvertColon ( expr, flagerr )
   error_TraceNreturn(flagerr, HERE)

   END SUBROUTINE pk2Interpreter_Convert

   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Added Procedures (riad):
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_Driver ( expr, vars, valExpr, flagerr, varNames, nAnswer )
!=============================================================================================
   character(len=*),                        intent(in    ) :: expr
   class    (pk2_t),                        intent(in    ) :: vars(:)
   !### type     (pk2_t), allocatable,           intent(   out) :: valExpr(:)
   type     (pk2_t), allocatable,           intent(in out) :: valExpr(:) !###
   type     (err_t),                        intent(in out) :: flagErr
   type     (str_t),              optional, intent(in    ) :: varNames(:)
   integer  (Ikind),              optional, intent(in    ) :: nAnswer
!---------------------------------------------------------------------------------------------
!  Analysis and Evaluation
!
!  Calls the tokenizor to parse the string "expr" that can be a mathematical expression of the
!  2-rank variables "vars(:)" and then calls the evaluator to evaluate it.
!
!  Inputs:
!  ------
!
!  . expr    : the string to evaluate
!  . vars    : an array of pk2 variables
!  . varNames: (optional) the list of variable names. If not present, "vars(:)%name" are used
!  . nAnswer : (optional) expected number of answers (default: 1)
!
!  Outputs:
!  -------
!
!  . valExpr: a pk2 array corresponding to the result(s)
!  . flagErr: error/warning status with the following convention:
!             . flagErr%code = 0: no error and no warning
!             . flagErr%code > 0: an error has occured 
!             . flagErr%code < 0: a warning message is available
!             . and flagErr%mesg gives the corresponding error or warning message.
!
!  Note:
!  ----
!
!  . UERROR (and WARNING) is the code used for error (or warning) due to an error in the
!    user's expression "expr"
!  . The caller have to test the error status. The error/waning message can be printed by a 
!    call to the bound procedure "display": call flagErr%display()
!
!  Example:
!  -------
!
!  program sample0
!     use pk2f_m ; use pk2Interpreter_m ; use dispmodule
!
!     type(pk2_t)              :: vars(10) ! Example with 10 variables
!     type(pk2_t), allocatable :: res(:)   ! Wil contain the result(s)
!     type(err_t)              :: flag     ! Error flag
!
!     ndim = 3
!
!     vars(1) = ndim            ; vars(1)%name = 'n' !(or vars(1) = pk2_t(matrix=ndim, name='n')
!     vars(2) = rand(ndim,ndim) ; vars(2)%name = 'A' !(...)
!     vars(3) = rand(ndim,ndim) ; vars(3)%name = 'B'  
!     vars(4) = 1.234           ; vars(4)%name = 'lam'
!     ...
!
!     ! Then, to compute for example, the eigenvalues of (A*B - lam*I) we can call
!
!     call pk2Interpreter_Driver ('eig (A*B-lam*eye(n,n))', vars(1:4), res, flag)
!
!     ! and we have to check the error status:
!
!     if (flag%code /= 0) then
!        call flag%display()
!        if (flag%code > 0) stop
!     end if
!
!     call res(1)%PrintMe(form = 'values', msg = 'Result = ')
!
!     ! If we want to compute the eigenvalues and the eigenvectors of "A":
!
!     call pk2Interpreter_Driver ('eig(A)', vars(1:4), res, flag, nanswer = 2)
!
!     if (flag%code /= 0) then
!        call flag%display()
!        if (flag%code > 0) stop
!     end if
!
!     call res(1)%PrintMe(form = 'values', msg = 'Eigenvalues of A: ')
!     call res(2)%PrintMe(form = 'values', msg = 'Eigenvectors of A: ')
!
!
!  end program sample0
!----------------------------------------------------------------------------R.H. 12/16, 03/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2Interpreter_Driver'
   type     (hdl_t)            :: handle
!---------------------------------------------------------------------------------------------

   if ( flagerr%code > IZERO ) return
               
   if ( len_trim(expr) == 0 ) then
      call flagerr%Set(WARNING, HERE, 'Empty expression (nothing to evaluate)')
      return
   end if
!
!- Analyze the user expression "expr":
!
   call pk2Interpreter_Tokenizor ( expr   = expr  , varNames = varNames, vars = vars, &
                                   handle = handle, flagerr  = flagErr              )         
!
!- Exit if an error was detected in the expression:
!                      
   error_TraceNreturn(flagerr, HERE)
!
!- Otherwise evaluate the expression:
!
   call pk2Interpreter_Evaluator ( handle, vars, valExpr, flagErr, nAnswer )
!
!- Clean the handle:
!
   call handle%Destroy()
!
!- Report any error:
! 
   error_TraceNreturn(flagerr, HERE)   
   
   END SUBROUTINE pk2Interpreter_Driver


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_Tokenizor ( expr, varNames, vars, handle, flagerr )
!=============================================================================================
   character(len=*),           intent(in    ) :: expr
   type     (str_t), optional, intent(in    ) :: varNames(:)
   class    (pk2_t), optional, intent(in    ) :: vars(:)
   type     (hdl_t),           intent(in out) :: handle   
   type     (err_t),           intent(in out) :: flagErr
!---------------------------------------------------------------------------------------------
!  Analysis only: call the tokens analyzer
!
!  Routine to call if only the analysis of the expression is required. 
!
!  Inputs:
!  ------
!
!  . expr    : the string to evaluate
!  . varNames: (optional) the list of variable names. If not present, "vars" should be present
!  . vars    : (optional) the list of variable (only their members "name" are used). If not 
!              present, "VarNames" should be present
!
!  Outputs:
!  -------
!
!  . handle : contains the analysis (will be mandatory for subsequent call to the evaluator)
!  . flagErr: error/warning status with the following convention:
!             . flagErr%code = 0: no error and no warning
!             . flagErr%code > 0: an error has occured 
!             . flagErr%code < 0: a warning message is available
!             . and flagErr%mesg gives the corresponding error or warning message.
!---------------------------------------------------------------------R.H. 12/16, 03/18, 11/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2Interpreter_Tokenizor'
   type     (str_t), allocatable :: names(:)
   integer  (Ikind)              :: i, nvars, err
!---------------------------------------------------------------------------------------------

   if ( flagerr%code > IZERO ) return
      
   if ( len_trim(expr) == 0 ) then
      handle%initialized = .true. ; handle%expr = ''
      call flagerr%Set(WARNING, HERE, 'Empty expression (nothing to analyze)')
      return
   end if
   
   if ( present(VarNames) ) then
!
!-    The variable names are given (array "varNames")
!
      call pk2Interpreter_TokensAnalyzer ( expr, varNames, handle, flagErr ) 
             
   else if ( present(vars) ) then
!
!-    The variable names are not given. Extract them for the variables list (vars):
!
      nvars = size(vars)
      allocate(names(nvars),stat = err)
      if ( err /= 0 ) then
         call flagerr%Set(IERROR, HERE, 'Allocation failure for << names >>')
         return
      end if
      
      do i = 1, nvars
         if ( allocated(vars(i)%name) ) then
            names(i)%str = vars(i)%name
         else
            names(i)%str = ''      
         end if   
      end do   

      call pk2Interpreter_TokensAnalyzer ( expr, names, handle, flagErr ) 
      
   else
      call flagerr%Set(IERROR,HERE,'Missing the variables list or the variable names list')
      return
   end if
   
   error_TraceNreturn(flagErr, HERE)

   END SUBROUTINE pk2Interpreter_Tokenizor


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_Evaluator ( handle, vars, valExpr, flagerr, nAnswer )
!=============================================================================================
   type     (hdl_t),                        intent(in out) :: handle
   class    (pk2_t),                        intent(in    ) :: vars(:)
   !### type     (pk2_t), allocatable,           intent(   out) :: valExpr(:)
   type     (pk2_t), allocatable,           intent(in out) :: valExpr(:) !###
   type     (err_t),                        intent(in out) :: flagErr
   integer  (Ikind),              optional, intent(in    ) :: nAnswer
!---------------------------------------------------------------------------------------------
!  Evaluation only: call the evaluator (pk2Interpreter_EvalExpr)
!
!  Routine to call once the expression has been analyzed by the tokenizor.
!
!  Inputs:
!  ------
!
!  . handle : the analysis of the expression done by the prior call to the tokens analyzer
!  . vars   : the pk2 array containing the values of the variables
!  . nAnswer: (optional) expected number of answers (default: 1)
!
!  Outputs:
!  -------
!
!  . valExpr: a pk2 array corresponding to the result(s)
!  . flagErr: error/warning status with the following convention:
!             . flagErr%code = 0: no error and no warning
!             . flagErr%code > 0: an error has occured 
!             . flagErr%code < 0: a warning message is available
!             . and flagErr%mesg gives the corresponding error or warning message.
!---------------------------------------------------------------------R.H. 12/16, 03/18, 11/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2Interpreter_Evaluator'
!---------------------------------------------------------------------------------------------

   if ( flagerr%code > IZERO ) return
      
   if ( handle%initialized ) then
      if ( handle%flag%code > 0 ) then
         flagErr = handle%flag
      else
         if ( handle%noperat > 0 .and. handle%ntokens > 0 ) then  
            call pk2Interpreter_EvalExpr_v2 ( handle, vars, valExpr, flagErr, nAnswer ) 
         else
            call flagerr%Set(UERROR, HERE,'Nothing to evaluate')
            return
         end if
      end if
   else
      call flagerr%Set(UERROR, HERE,'The expression must be first analyzed')
      return
   end if
   
   error_TraceNreturn(flagErr, HERE)   
   
   END SUBROUTINE pk2Interpreter_Evaluator


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_SplitInTokens ( handle, ntokens, flagerr ) ! modified (riad)
!=============================================================================================
   type   (hdl_t), intent(in out) :: handle
   integer(Ikind), intent(   out) :: ntokens
   type   (err_t), intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Splits handle%expr in tokens and allocates members of handle
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2Interpreter_SplitInTokens'                                   
   integer  (Ikind)              :: i, n, lstr, opcl, noper
   character(len=1)              :: c, op
   logical                       :: is_modified
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return
!
!- At this step, in expressions like 1e-5, 1e+5, 1E-5, 1E+5, 1d-5, 1d+5, 1D-5, 1D+5, the two
!  symbols '-' and '+' are not considered as operators. 
!  We temporarily replace them by S01 and S02 characters, resp.:
!
   call pk2Interpreter_Expos ( handle%expr, S01, S02, is_modified, flagerr )  
   error_TraceNreturn(flagerr, HERE)
!
!- Find the positions in "handle%expr" of all operators given in the list "OperList".
!  The occurence of any operator "op" is replaced by SOP//op//SCL.

   noper = 0
   do i = 1, NOP
      op = OperCode(i)
!
!-    enclose op between SOP and SCL symbols if it's not already the case and if
!     op is not a part of text (i.e. not enclosed between " ")
!
      handle%expr = util_ReplaceSubstring2 ( str     = handle%expr,  &
                                             remove  = op,           &
                                             replace = SOP//op//SCL, &
                                             opcl    = '""',         &
                                             nrep    = n,            &
                                             stat    = flagerr       ) 
      error_TraceNreturn(flagerr, HERE)
      noper = noper + n
   end do   

   lstr = len_trim(handle%expr)
!
!- allocate the members of the handle (a priori ntokens cannot be greater than n = 2*noper+1):
!
   n = 2*noper+1
   
   call handle%Alloc ( n, flagerr ) ;  error_TraceNreturn(flagerr, HERE)
 
   do i = 1, n
      handle%stokens(i)%str = ''
      handle%involvedVar(i)%str = ''
   end do                                        
!
!- put back the sign of exponents if appropriate:
!
   if ( is_modified ) then
      do i = 1, lstr
         if ( handle%expr(i:i) == S01 ) handle%expr(i:i) = '-'
         if ( handle%expr(i:i) == S02 ) handle%expr(i:i) = '+'
      end do
   end if       
!
!- Split now "handle%expr" in tokens:
!
   ntokens = 1 ; opcl = 0
   do i = 1, lstr   
      c = handle%expr(i:i)
      if ( c == SOP ) then
         if ( opcl == 0 .and. i > 1    ) ntokens = ntokens + 1
         opcl = 1 ; cycle
      end if   
      if ( c == SCL ) then
         if ( opcl == 0 .and. i < lstr ) ntokens = ntokens + 1
         opcl =-1 ; cycle      
      end if      
      opcl = 0 ; handle%stokens(ntokens)%str = handle%stokens(ntokens)%str // c
   end do

   do i = 1, ntokens
      handle%stokens(i)%str = trim(adjustl(handle%stokens(i)%str))
   end do     
!
!- Initialization:
!
   call handle%Init ( ntokens, flagerr )
   error_TraceNreturn(flagerr, HERE)
   
   END SUBROUTINE pk2Interpreter_SplitInTokens
   
   
!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertSpaces ( expr, flagerr ) 
!============================================================================================= 
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Removes blank spaces from "expr" if they are not between double quotes (text) or are not
!  used as column delimiters in an array constructor (i.e. between [ ]). In this latter case,
!  blanks are first replaced by commas (',').
!-----------------------------------------------------------------------------------R.H. 06/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE  = 'pk2Interpreter_ConvertSpaces',      &
                                    UNARY = "+-~"                         ,      &
                                    DELIM1 = "[({,;"                      ,      &
                                    DELIM2 = "])},;'"                     ,      &
                                    OP1 = UNARY//DELIM1//"*/.^<>=&| "     ,      &
                                    OP2 = DELIM2//"*/.^<>=&|'"            
   integer  (Ikind)              :: i, i1, i2, opcl, lexpr
   character(len=1)              :: ch1, ch2, ch3
   logical                       :: is_enclosed
!--------------------------------------------------------------------------------------------- 

   if ( flagerr%code > IZERO ) return

   lexpr = len_trim(expr)
   
   if ( lexpr == 0 ) return
      
   if ( index(expr,'[') /= 0 ) then 
!
!-    Replace occurences of blank by a comma if used as column delimiter:
!    
      i1 = 1 ; opcl = 0
      do while ( i1 < lexpr )
         ch1 = expr(i1:i1)
         if ( ch1 == '"' ) then
            if (opcl == 0) then
               opcl = 1
            else
               opcl = 0
            end if
         end if         
         if ( index(OP1,ch1) /= 0 .and. expr(i1:i1+1) /= '. ' ) then
            i1 = i1 + 1
            cycle
         end if
      
         i2 = i1 ; ch3 = '?'

         do i = i1+1, lexpr
            ch2 = expr(i:i)
            if ( ch2 /= ' ' ) then
               i2 = i
               if ( i2 < lexpr ) ch3 = expr(i2+1:i2+1)
               exit
            end if
         end do
            
         if ( index(OP2,ch2) == 0 .and. opcl == 0 .and. i2 - i1 > 1 ) then
            if ( index(UNARY,ch2) == 0 .or. ch3 /= ' ' ) then
               is_enclosed = util_IsEnclosed ( expr, i1+1, '[', ']', flagerr)
               if ( flagerr%code > 0 ) return 
               if ( is_enclosed ) expr(i1+1:i1+1) = ','
            end if
         end if

         i1 = i2
      end do
   
   end if
!
!- Now removes all spaces that are not part of text (i.e. not between double quotes):
!   
   expr = util_RemoveSpaces2 ( expr, opt = 'ndqt', stat = flagerr )
   error_TraceNreturn(flagerr, HERE)

   END SUBROUTINE pk2Interpreter_ConvertSpaces
   
   
!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertMatrix ( expr, flagerr ) 
!============================================================================================= 
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Replaces the occurences in the string "expr" of '[...]' by 'merge(...)' 
!-----------------------------------------------------------------------------------R.H. 06/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE  = 'pk2Interpreter_ConvertMatrix'
!--------------------------------------------------------------------------------------------- 

   if ( flagerr%code > IZERO ) return
   
   if ( index(expr,'[') == 0 ) return    
   
   expr = util_ReplaceSubstring2 ( str = expr, remove = "[" , replace = "merge(", &
                                   opcl = '""', stat = flagerr)         ! replace [ by merge(   
   error_TraceNreturn(flagerr, HERE)
      
   expr = util_ReplaceSubstring2 ( str = expr, remove = "]" , replace = ")", &
                                   opcl = '""', stat = flagerr)         ! and replace ] by )   
   error_TraceNreturn(flagerr, HERE)

   END SUBROUTINE pk2Interpreter_ConvertMatrix

   
!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertQuotes ( expr, flagerr ) 
!============================================================================================= 
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Replaces the occurences in the string "expr" of single quote (') by double quote (")
!
!  Note: 
!     
!  the following rules are considered:
!
!       . A string must be enclosed in either " " or by ' '. 
!
!         Examples:  "hello man",   (is valid)
!                    'hello man'    (is valid and will be changed to "hello man")
!
!       . A single quotation mark (') not surrounded by double quotation marks (") is
!         considered to be the transposition symbol (except when it is the first or 
!         last character of the expression) and it is therefore not transformed.
!
!         Example:  a + b' (will be unchanged)
!
!  and (added 06/20):
!
!       . Single quotation marks (') may themselves be part of the string provided that the 
!         string is defined by double quotation marks ("). The reverse is not possible.
!
!         Examples:  "he said: 'hello man how, are you?'"   (is valid and will be unchanged)
!                    "le nombre d'iterations est : "        (is valid and will be unchanged)     
!                    'he said: "hello man how, are you?"'   (is not valid)
!
!----------------------------------------------------------------------------R.H. 06/18, 06/20

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'pk2Interpreter_ConvertQuotes', &
                                    operat = '+-*/^,;&|~=><([{"' 
   integer  (Ikind)              :: i, j, p, q, cl, nloc, lexpr
   integer  (Ikind), allocatable :: loc(:)
!--------------------------------------------------------------------------------------------- 

   if ( flagerr%code > IZERO ) return

   lexpr = len_trim(expr,kind=Ikind)   

   if ( lexpr == 0 ) return
!
!- find the positions of single quotation marks:
!
   call util_FindSubstring2 ( loc, nloc, expr, IONE, lexpr, "'", '""', flagerr )
   error_TraceNreturn(flagerr, HERE)
!
!- for each mark, identify whether it is a real quotation (used for the text) or whether it is
!  the transposition symbol
!   
   cl = 0
   do j = 1, nloc
!
!-    position of the j-th quotation mark:
!   
      p = loc(j)      
!
!-    position (q) of the previous non-blank character:
!
      q = p
      do i = p-1, 1, -1
         if ( expr(i:i) /= ' ' ) then
            q = i ; exit
         end if
      end do    
   
      if ( p == 1 ) then
!
!-       a ' at 1st position: replaced by a " (it can not be a transposition)
!         
         expr(p:p) = '"'
!
!-       tag it as an opening quote:
!
         cl = 1         
         
      else if ( index(operat,expr(q:q)) /= 0 ) then
!
!-       the preceding character is an operator or a delimiter or an opening bracket,
!        replace expr(p:p) by ":
!
         expr(p:p) = '"'
         
         if ( cl == 1 ) then
            cl = 0 ! it's a closing quote
         else
            cl = 1 ! it's an opening quote
         end if
         
      else if ( cl == 1 ) then      
         cl = 0   ! it's a closing quote
         expr(p:p) = '"'
      end if   
   end do          
   
   if ( cl /= 0 .or. .not. util_IsBalanced1 (expr,'"','"') ) then
      call flagerr%Set(UERROR,HERE,'Unbalanced quotation marks (<< " >> or << '' >>)')
      return
   end if      

   END SUBROUTINE pk2Interpreter_ConvertQuotes
   
   
!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertTrans ( expr, trs, flagerr ) 
!=============================================================================================   
   character(len=:), allocatable, intent(in out) :: expr
   character(len=*),              intent(in    ) :: trs
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Replaces the occurences in "expr" of  xxx' or xxx.' by "trs"(xxx) 
!  where "trs" is 'trans' or 'transp'
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=* ), parameter :: HERE = 'pk2Interpreter_ConvertTrans', &
                                   operat='+-*/\^,;&|~='//ELEMS//RELAT
   integer  (Ikind)             :: j, pq, p1, p2, pop, cl, n
   character(len=1)             :: ch
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return

   if ( trs == 'trans' ) then
      expr = util_ReplaceSubstring2 ( expr, "'" , S01,'""', flagerr, n ) ! replace ' by S01 
   else if ( trs == 'transp' ) then   
      expr = util_ReplaceSubstring2 ( expr, ".'", S01,'""', flagerr, n ) ! replace .' by S01
   else
      call flagerr%Set( UERROR, HERE, "trs = "//trs//" (must be 'trans' or 'transp')" )
      return
   end if
   error_TraceNreturn(flagerr, HERE)
   
   if ( n == 0 ) return
      
   pq = index(expr,S01)

   if ( pq == 1 ) then
      call flagerr%Set( UERROR, HERE, " Syntax error" )
      return
   end if
   
   do while ( pq /= 0 )  

      p2 = pq   
      
      if ( expr(pq-1:pq-1) == ')' ) then
!
!-       the symbol S01 is preceded by a closing parenthesis, find the position (p2) of the
!        corresponding opening parenthesis 
!
!        Example: (k+g)*((a+b)*c).' --> p2 = 7
!                 1234567
!
         p2 = 0
         cl = 0
         do j = pq-1, 1, -1
            ch = expr(j:j)
            if ( ch == ')' ) cl = cl + 1
            if ( ch == '(' ) cl = cl - 1
            if ( cl == 0 ) then
               p2 = j
               exit
            end if
         end do
      end if     
!      
!-    if presents, find the position p1 of the operator or '('  that preceeds the position p2

!     Examples : (k+g)*a' --> p2 = pq = 8, p1 = 6, 
!                123456 
!
!                (k+g)*(a'+b) --> p2 = pq = 9, p1 = 7
!                1234567
!
!                (k+g)*((a+b)*c)' --> p2 = 7, p1 = 6, 
!                123456
!
!                (k+g)*cos((a+b)*c)' --> p2 = 10, p1 = 6
!                123456
!
      p1 = 0
      do j = p2-1, 1, -1
         ch = expr(j:j)
         pop = index( operat//'(', ch )
         if ( pop /= 0 ) then         
            p1 = j
            exit
         end if
      end do
!
!-    put expr(p1+1:pq-1) as argument of the function trans:
!    
      expr = expr(:p1) // trim(trs) // '(' // expr(p1+1:pq-1) // ')' // expr(pq+1:) 

      pq = index( expr, S01 )
   end do   

   END SUBROUTINE pk2Interpreter_ConvertTrans


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertColon ( expr, flagerr )
!=============================================================================================
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------
!  Replaces the occurences in "expr" of  'a : c : b' by 'colon(a,b,c)'
!
!  Warning: must be called after:
!  . pk2Interpreter_ConvertMatrix: replacements of [ ] by merge()
!  . pk2Interpreter_ConvertTrans : replacements of ' or .' by trans() or transp()
!
!  Examples:  
!
!  . expr = '1:2:10'           is transformed into   expr = 'colon(1,10,2)'
!  . expr = 'a:b + x:y'        is transformed into   expr = 'colon(a,y,b+x)'
!  . expr = '(a:b) + (x:y:z)'  is transformed into   expr = '(colon(a,b,1)) + (colon(x,z,y))'
!  
!  and the following expressions are not valid:
!  . expr = 'a:b + x:y:z'
!  . expr = '(a:b) + x:y:z'
!
!  Note: expr = 'A(:)' or 'A(i,:)' are transformed into expr = 'A(-1)' or 'A(i,-1)'
!        '-1' will be used as a code to tell the function pk2f_EXTRACMAT to extract the whole
!        elements of the corresponding row or column.
!
!  Modified: 04/19 (new version)
!-----------------------------------------------------------------------------R.H.04/18, 04/19

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter   :: HERE = 'pk2Interpreter_ConvertColon', &
                                    ALPH = 'abcdefghijklmnopqrstuvwxyz'// &
                                           'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   integer  (Ikind)              :: i, j, p1, p2, q1, q2, unbal, lexpr, nt, nloc
   character(len=1)              :: ch
   integer  (Ikind), allocatable :: loc(:), par(:,:) 
   character(len=:), allocatable :: tmp, exp0, exp1, arg1, arg2, arg3
   logical                       :: is_enclosed
!---------------------------------------------------------------------------------------------

   if ( flagerr%code > IZERO ) return
      
   if ( index(expr,":") == 0 ) return
   
   exp0 = expr
!
!- Replace all occurences of '(:)' by '(-1)':
!
   expr = util_ReplaceSubstring2(str=expr, remove='(:)', replace='(-1)',opcl='', stat=flagerr)
   error_TraceNreturn(flagerr, HERE)
!
!- Replace all occurences of '(:,' by '(-1,':
!
   expr = util_ReplaceSubstring2(str=expr, remove='(:,', replace='(-1,',opcl='', stat=flagerr)
   error_TraceNreturn(flagerr, HERE)
!
!- Replace all occurences of ',:)' by ',-1)':
!
   expr = util_ReplaceSubstring2(str=expr, remove=',:)', replace=',-1)',opcl='', stat=flagerr)   
   error_TraceNreturn(flagerr, HERE)

   lexpr = len(expr)
!
!- 1) first, find the positions of all ':', store them into the array "loc":
!      
   call util_FindSubstring2 ( loc, nloc, expr, IONE, lexpr, ":", '""', flagerr )
   error_TraceNreturn(flagerr, HERE)
!      
!- if there are none, return
! 
   if ( nloc == 0 ) return  
         
   if ( nloc > 2 .and. index(expr,'(') == 0 ) then
      call flagerr%Set(UERROR, HERE,                                        &
                       'More than two << : >> in tbe expression:'  // NL // &
                       '--> << '//trim(expr)//' >>.'               // NL // &
                       '--> Please use parentheses'                         )
      return
   end if
!
!- 3) Find, if any, the couple of parentheses that enclose each ":". Store their positions
!     in col(1:2,:) and set col(3,:) = 1 if these parentheses exist:
!
   allocate(par(3,nloc),source = 0_Ikind)
      
   nt = 0
   do i = 1, nloc
      p1 = loc(i)
!
!-    left part:
!         
      unbal = 0
      do j = p1-1,1,-1
         ch = expr(j:j)
         if ( ch == ')'                               ) unbal = unbal - 1
         if ( ch == '(' .or. ch == ',' .or. ch == ';' ) unbal = unbal + 1
         if ( unbal == 1 ) then
            par(1,i) = j
            exit
         end if
      end do   
!
!-    right part:
!            
      unbal = 0
      do j = p1+1,lexpr
         ch = expr(j:j)      
         if ( ch == ')' .or. ch == ',' .or. ch == ';' ) unbal = unbal + 1
         if ( ch == '('                               ) unbal = unbal - 1
         if ( unbal == 1 ) then
            par(2,i) = j
            exit
         end if
      end do    
!
!-    set col(3,i) = 1 if parentheses were found:
!         
      if ( par(1,i) /= 0 .and. par(2,i) /= 0 ) then
         par(3,i) = 1  
         nt = nt + 1
      end if   
   end do     
!
!- Check for errors: see if groupes of ":" are well separated:
! 
   if ( nt /= nloc ) then
      if ( nloc > 2 ) then
         call flagerr%Set( UERROR, HERE,                                        &
                           'More than two << : >> in tbe expression:'  // NL // &
                           '--> << '//trim(expr)//' >>.'               // NL // &
                           '--> Please use parentheses'                         )
         return
      else if ( nloc == 2 .and. nt /= 0 ) then  
         call flagerr%Set( UERROR, HERE,                                                     &
         '--> groupes of << : >> must be separated by parentheses in the expression'// NL // &
         '--> << '//trim(expr)//' >>.'                                                       )
         return
      end if
   end if
!
!- Check for errors: check for crossing parentheses:
!        
   do i = 1, nloc-1
      p1 = par(1,i) ; p2 = par(2,i)
      do j = i+1, nloc    
         q1 = par(1,j) ; q2 = par(2,j)
         if ( q1 /= p1 .and. q2 /= p2 .and. q1 < p2 ) then
            call flagerr%Set( UERROR, HERE,                                                  &
                          'syntax error or misplaced parentheses in the expression' // NL // &
                          '--> << '//trim(expr)//' >>.'                                      )
            return
         end if
      end do
   end do        
!
!- 3) try to form pairs of ':' if possible. Temporarily enclose them between SOP and SCL
!  
!     Examples: 
!           (for clarity SOP and SCL are denoted here by '{' and '}', respectively):         
!
!           (1)  1 : n - 1                  --> { 1 : n - 1 }
!           (2)  a : b + c : d              --> { a : b + c : d }   
!           (3)  1 +  x : y : z             --> { 1 + x : y : z }
!           (4)  (a : b : c)  +  (d : e)    --> ( { a : b : c } )  +  ( { d : e } )
!           (5)  (a : b : c)  +   d : e     --> error
!
!     note that these expressions will be interpreted as
!           (1) "1:(n-1)" and not as "(1:n) - 1"
!           (2) "a:(b+c):d" and not as "(a:b) + (c:d)"
!           (3) "(1+x):y:z" and not as "1 + (x:y:z)"
!     and parentheses are required in an expression with multiple ':' (as in (4)).
!

   tmp = expr
      
   do i = 1, nloc
      is_enclosed = util_IsEnclosed ( tmp, loc(i), SOP, SCL, flagerr )
      error_TraceNreturn(flagerr, HERE)
!
!-    if already enclosed, cycle:
!
      if ( is_enclosed ) cycle
!
!-    include SOP and SCL:
!         
      p1 = par(1,i) ; p2 = par(2,i)
         
      if ( par(3,i) == 0 ) then  
!
!-       case without parentheses (enclose the whole string):
!         
         tmp = SOP // tmp // SCL
      else
!
!-       case with parentheses (include SOP and SCL inside them):
!
         tmp = tmp(:p1) // SOP // tmp(p1+1:p2-1) // SCL // tmp(p2:)
      end if
!
!-    update the locations of the ":" and "(", ")"
!            
      loc = loc + 2 
      par(1:2,1:nloc) = par(1:2,1:nloc) + 2
         
   end do
            
!
!- 4) Now, for each pair of "{ }" (SOP, SCL), see how many ":" are present:
!     - if two (e.g. "{arg1 : arg3 : arg2}") then the step (arg3) is between them
!     - if only one (e.g. "{arg1 : arg2}"), then by default the  step is set to 1.
!
!     Remove then the pairs "{ }" and replace the ": :" by the function name "colon", i.e.
!     - "{arg1 : arg3 : arg2}" is replaced by "colon(arg1,arg2,arg3)"
!     - "{arg1 : arg2}"        is replaced by "colon(arg1,arg2,1)"
!
   unbal = 0 ; expr = '' ; exp1 = '' ; arg1 = '' ; arg2 = '' ; arg3 = ''
   
   do i = 1, len(tmp)
      ch = tmp(i:i)
      if ( ch == SOP ) unbal = unbal + 1
      if ( ch == SCL ) unbal = unbal - 1

      if ( unbal == 0 .and. ch /= SOP .and. ch /= SCL ) then
!
!-       Part outside the braces. Store it in str
!      
         expr = expr // ch  
         
      else if ( ch == SOP ) then
!
!-       The openning brace. Initialize exp1 to the null string
!      
         exp1 = ''
         
      else if ( ch /= SOP .and. ch /= SCL ) then
!
!-       Part inside the braces. Store it in exp1:
!      
         exp1 = exp1 // ch  
         
      else if ( ch == SCL ) then
!
!-       The closing brace. Split exp1 in arg1 and arg2 (and arg3 if presents):
!       
         p1 = 0 ; p2 = 0
         do j = 1, len(exp1)
            if ( exp1(j:j) == ':' ) then
               if ( p1 == 0 ) then
                  p1 = j
                  arg1 = exp1(1:p1-1) ; arg2 = exp1(p1+1:) ; arg3 = '1'
               else 
                  p2 = j   
                  arg3 = exp1(p1+1:p2-1) ; arg2 = exp1(p2+1:)   
               end if 
            end if
         end do
!
!-       Copy in expr:
!         
         expr = expr //'colon('// trim(arg1) //','// trim(arg2) //','// trim(arg3) //')'
      end if   
   end do   
                  
   if ( index(expr,':') /= 0 ) then
      call flagerr%Set(UERROR, HERE, 'Syntax error in the expression: << '//trim(exp0)//' >>')
      return
   end if
                  
   END SUBROUTINE pk2Interpreter_ConvertColon


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertMultiUnary ( expr, flagerr ) 
!============================================================================================= 
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------
!  Replaces in "expr" all sequences of unary operators by what is appropriate, e.g. 
!  . ~~~x becomes ~x
!  . +-+x becomes -x
!  . -+-x becomes +x
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2Interpreter_ConvertMultiUnary'
   integer  (Ikind)            :: plus, minus, tilde, n1, n2, n3, n4
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return

   plus = index(expr,'+') ; minus = index(expr,'-') ; tilde = index(expr,'~')
   
   if ( tilde /= 0 ) then
      expr = util_ReplaceSubstring2 ( str = expr, remove = "~~", replace = "", &
                                      opcl = '""', stat = flagerr              ) 
      error_TraceNreturn(flagerr, HERE)
   end if   
   
   if ( minus == 0 .and. plus == 0 ) return
   
   if ( minus == 0 ) then
      do 
         expr = util_ReplaceSubstring2 ( str = expr, remove = "++", replace = "+", &
                                         opcl = '""', stat = flagerr, nrep = n1    ) 
         error_TraceNreturn(flagerr, HERE)
         if ( n1 == 0 ) exit
      end do
   else
      do 
         expr = util_ReplaceSubstring2 ( expr, "++", "+" ,'""', flagerr, n1 )
         error_TraceNreturn(flagerr, HERE)
         
         expr = util_ReplaceSubstring2 ( expr, "--", "+" ,'""', flagerr, n2 )
         error_TraceNreturn(flagerr, HERE)
         
         expr = util_ReplaceSubstring2 ( expr, "+-", "-" ,'""', flagerr, n3 )
         error_TraceNreturn(flagerr, HERE)
         
         expr = util_ReplaceSubstring2 ( expr, "-+", "-" ,'""', flagerr, n4 )
         error_TraceNreturn(flagerr, HERE)
         
         if ( n1 + n2 + n3 + n4 == 0 ) exit
      end do   
   end if
   
   END SUBROUTINE pk2Interpreter_ConvertMultiUnary

       
!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertRelOper ( expr, flagerr ) 
!=============================================================================================   
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Replaces the occurences in "expr" of 'a op b' by 'opname(a,b)'
!  where op is one of the relational operators (==, ~=, >, <, >=, <=) and opname is the
!  corresponding name function (eq, ne, lt, gt, ge, le)
!
!  Examples:  
!
!   .  a < b & c == d         is transformed into  lt(a,b) & eq(c,d)
!
!   . find(A <= 0 | A >= 2)   is transofrmed into  find(le(A,0) | ge(A,2))
!
!  Note: although the relational operators are defined (as overloaded for pk2 matrices) I
!        chose to use functions instead going through operators directly (since I didn't
!        take the time to think carefully (!) to insert them into the algorithm of 
!        Wilton & Soares)
!
!  05/24: Done. THIS ROUTINE IS NO MORE USED
!-----------------------------------------------------------------------------------R.H. 06/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'pk2Interpreter_ConvertRelOper'
   character(len=2), parameter   :: opsymb(6) = ['>=','<=','==','~=','> ','< '], &
                                    opname(6) = ['ge','le','eq','ne','gt','lt']
   character(len=1)              :: ch
   integer  (Ikind)              :: i, j, p1, p2, nloc, unbal, ioper, lsymb, ltmp
   logical                       :: exist, is_enclosed
   character(len=:), allocatable :: tmp, exp1, exp2, arg1, arg2, symb
   integer  (Ikind), allocatable :: loc(:)
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return
   
   exist = .false.
   
   tmp = expr

   do ioper = 1, 6
      symb  = trim(opsymb(ioper)) ; lsymb = len(symb)      
!
!-    1) first, find the positions of "symb", store them into the array "loc" of size "nloc":
!
      ltmp = len_trim(tmp)

      !call util_FindSubstring2 ( loc, nloc, tmp, IONE, ltmp, symb, '""'//"''", flagerr )
      call util_FindSubstring2 ( loc, nloc, tmp, IONE, ltmp, symb, '""', flagerr ) ! 05/24
      error_TraceNreturn(flagerr, HERE)
!
!-    2) Temporarily enclose them between SOP and SCL symbols if not already the case
!  
      do i = 1, nloc
   
         is_enclosed = util_IsEnclosed ( tmp, loc(i), SOP, SCL, flagerr )
         error_TraceNreturn(flagerr, HERE)
         
         if ( is_enclosed ) cycle

         exp1 = tmp(:loc(i)-1) ! left part of "symb"
         call pk2Interpreter_cuttopar ( exp1, '~&|,;', -IONE, p1, flagerr )      
         error_TraceNreturn(flagerr, HERE)
      
         exp2 = tmp(loc(i)+lsymb:) ! right part of "symb"        
         call pk2Interpreter_cuttopar ( exp2, '&|,;', +IONE, p2, flagerr )      
         error_TraceNreturn(flagerr, HERE)

         if ( p1 == 0 ) p1 = 1
         if ( p1 > 1 ) then
            tmp = exp1(:p1-1) // SOP // exp1(p1:) // symb // exp2(:p2) // SCL
         else
            tmp = SOP // exp1(p1:) // symb // exp2(:p2) // SCL
         end if   
         if ( p2 < len_trim(exp2) ) tmp = tmp // exp2(p2+1:)
                              
            
         loc = loc + 2 ! update the locations of "symb" in tmp (SOP and SCL were added to tmp)
      end do
      
      if ( nloc > 0 ) exist = .true.
   end do     
   
   if ( .not. exist ) return      
!
!- 3) Now, remove the pairs SOP-SCL and replace the "symb" by the corresponding function name
!     e.g. "arg1 > arg2" is replaced by "gt(arg1,arg2)"
!
   unbal = 0 ; expr = '' ; exp1 = ''
   
   do i = 1, len(tmp)
      ch = tmp(i:i)
      if ( ch == SOP ) unbal = unbal + 1
      if ( ch == SCL ) unbal = unbal - 1

      if ( unbal == 0 .and. ch /= SOP .and. ch /= SCL ) then
!
!-       Part outside the braces. Store it in expr
!      
         expr = expr // ch  
         
      else if ( ch == SOP ) then
!
!-       The openning brace. Initialize exp1 to the null string
!      
         exp1 = ''
         
      else if ( ch /= SOP .and. ch /= SCL ) then
!
!-       Part inside the braces. Store it in exp1:
!      
         exp1 = exp1 // ch  
         
      else if ( ch == SCL ) then
!
!-       The closing brace. Split exp1 in arg1 and arg2:
!  
         p1 = 0 ; j = 0
         do ioper = 1, 6
            p1 = index( exp1,trim(opsymb(ioper)) )
            if ( p1 /= 0 ) then
               j = ioper
               exit
            end if   
         end do
         
         if ( p1 == 0 ) then
            call flagerr%Set(UERROR, HERE,'A problem has occured!!')
            return
         end if
         
         arg1 = exp1(1:p1-1) ; arg2 = exp1(p1+len_trim(opsymb(j)):)
!
!-       Copy in expr:
!         
         expr = expr // opname(j) // '(' // trim(arg1) // ',' // trim(arg2) //  ')'
      end if   
   end do     

   END SUBROUTINE pk2Interpreter_ConvertRelOper
        

!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertArgKeywd ( expr, flagerr ) 
!=============================================================================================   
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------
!  Replaces an expression where keywords are used in a call statement of a function, like in
!
!                  func (kwd1 = val1, kwd2 = val2, ..., kwdn = valn)
!
!  by an expression where the values follow the keywords, like in
!
!              func (n, "kwd1", "kdw2",..., "kwdn", val1, val2, ..., valn)
!
!  Here, 'func' must be the name of a function listed in "FuncList".
!
!  Note: 
!      . This routine MUST BE CALLED AFTER ConvertQuotes (because if val? is a string, double
!        quotes must be used instead of single ones).
!      . A call mixing argument by position and argument by keyword is not allowed.
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter   :: HERE = 'pk2Interpreter_ConvertArgKeywd'
   type     (str_t ), allocatable :: args(:,:)
   integer  (Ikind )              :: narg, neq, p1, p2, iarg, Id, lstr
   character(len=: ), allocatable :: str
   character(len=99)              :: cnum
!---------------------------------------------------------------------------------------------

   if ( flagerr%code > IZERO ) return

   if ( index(expr,'=') == 0 ) return
   
   p1 = index(expr,'(') ; if ( p1 == 0 ) return
!
!- See if "expr" start with the name of a defined function in FuncList:
! 
   Id = pk2Interpreter_FindId ( expr(1:p1-1), FuncList, IONE, NFC+nuserf, IONE )
   if (Id == 0) return

   p2 = index(expr,')', back=.true.)

   if ( p2 < 2 .or. p1 > p2 ) then
      call flagerr%Set(UERROR, HERE, 'Unbalanced parentheses')
      return
   end if
!
!- Part between () (corresponding to the arguments):
!
   str = expr(p1+1:p2-1) ; if ( len_trim(str) == 0 ) return
!
!- Split it according to the comma delimiters (',') that are not between " " or (), [], {}:
!
   narg = util_CountTokens ( str, delims = ',', BlkToken = .false., opcl = '""()[]{}', &
                             tokens = args, stat = flagerr )
   error_TraceNreturn(flagerr, HERE)
   
   if ( narg == 0 ) return

   write(cnum,'(i0)')narg
!
!- Put the number of arguments in the first position of the new expression:
!
   str = expr(1:p1)//trim(cnum)  ! 'func(n'
!
!- For each argument, find the '=' (not enclosed by " ") and put the part corresponding to the
!  lhs (keyword name) in the new expression:
!
   neq = 0     
   do iarg = 1, narg
      p1 = index(args(iarg,1)%str,'=')
      
      if ( p1 == 0 ) cycle
      
      if ( util_IsEnclosed ( args(iarg,1)%str, p1, '"', '"', flagerr ) ) cycle
      
      lstr = len_trim(args(iarg,1)%str)
      
      if ( p1 > 1 ) then
         if ( args(iarg,1)%str(p1-1:p1-1)=='~' .or. args(iarg,1)%str(p1-1:p1-1)=='>' .or. &
              args(iarg,1)%str(p1-1:p1-1)=='<' .or. args(iarg,1)%str(p1-1:p1-1)=='='      ) &
            cycle
      else if ( p1 < lstr ) then
         if ( args(iarg,1)%str(p1+1:p1+1) == '=' ) cycle
      end if
      
      if ( p1 < lstr ) then
         if ( args(iarg,1)%str(p1+1:p1+1) /= '=' ) neq = neq + 1
         str = trim(str)//',"'//trim(adjustl(args(iarg,1)%str(1:p1-1)))//'"'  ! 'func(n,"kwd1","kwd2'
      else
         call flagerr%Set( UERROR, HERE, 'Invalid expression' )
         return
      end if
   end do

   if ( neq == 0 ) return
!
!- See if all arguments have a keyword: 
!
   if ( neq > 0 .and. neq /= narg ) then
      call flagerr%Set( UERROR, HERE, 'A keyword name is missing' )
      return
   end if
!
!- Put now the values (the rhs) at the end of the new expression:
!
   do iarg = 1, narg
      p1 = index(args(iarg,1)%str,'=')
      str = trim(str)//','//trim(adjustl(args(iarg,1)%str(p1+1:)))
   end do
   
   expr = trim(str)//')'

   END SUBROUTINE pk2Interpreter_ConvertArgKeywd
   

!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_ConvertOperators ( expr, flagerr )
!=============================================================================================
   character(len=:), allocatable, intent(in out) :: expr
   type     (err_t),              intent(in out) :: flagerr
!--------------------------------------------------------------------------------------------- 
!  Replace all operators by their corresponding code given in OperCode
!  (we do it only for operators with more than one character) 
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return

   expr = util_ReplaceSubstring2 ( expr, ".*", ELEMMUL  ,'""', flagerr )
   expr = util_ReplaceSubstring2 ( expr, "./", ELEMDIV  ,'""', flagerr )
   expr = util_ReplaceSubstring2 ( expr, ".^", ELEMPOW  ,'""', flagerr )
   expr = util_ReplaceSubstring2 ( expr, ".'", ELEMTRANS,'""', flagerr )

   expr = util_ReplaceSubstring2 ( expr, "==", EQUAL    ,'""', flagerr )
   expr = util_ReplaceSubstring2 ( expr, "~=", NOTEQ    ,'""', flagerr )
   expr = util_ReplaceSubstring2 ( expr, "<=", LESSOREQ ,'""', flagerr )
   expr = util_ReplaceSubstring2 ( expr, ">=", GREATOREQ,'""', flagerr )
   
   END SUBROUTINE pk2Interpreter_ConvertOperators


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_cuttopar ( str, oper, sens, icut, flagerr )
!=============================================================================================   
   character(len=*),              intent(in    ) :: str, oper
   integer  (Ikind),              intent(in    ) :: sens
   integer  (Ikind),              intent(   out) :: icut
   type     (err_t),              intent(in out) :: flagerr
!--------------------------------------------------------------------------------------------- 
!  NO MORE USED
!---------------------------------------------------------------------------------------------    
   
!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'pk2Interpreter_cuttopar'  
   integer  (Ikind), allocatable :: parentheses(:)
   character(len=:), allocatable :: new
   character(len=1)              :: ch
   logical                       :: inparentheses
   integer  (Ikind)              :: i, op, np, lstr, lnew, i1, i2, i3, icut1, err
!---------------------------------------------------------------------------------------------    

   if ( flagerr%code > IZERO ) return
      
   if ( sens /= 1 .and. sens /= -1 ) then
      call flagerr%Set( UERROR, HERE, '"sens" must be 1 or -1' )
      return
   end if
       
   lstr = len_trim(str)

   np = 0
   do i = 1, lstr
      if ( str(i:i) == ')' .or. str(i:i) == '(' ) np = np + 1
   end do
   
   allocate(parentheses(np), source = IZERO, stat = err)
   
   if ( err /= 0 ) then
      call flagerr%Set( IERROR, HERE, 'Allocation failure' )   
      return
   end if    
      
   if ( sens == -1 ) then
      i1 = 1 ; i2 = lstr ; i3 = 1
   else
      i1 = lstr ; i2 = 1 ; i3 = -1
   end if

   np = 0
   do i = i1, i2, i3
      if ( str(i:i) == '(' ) then
         if ( sens == -1 ) then
            np = np + 1
            parentheses(np) = i
         else
            np = np - 1
         end if
      end if
      if ( str(i:i) == ')' ) then
         if (sens == -1) then
            np = np - 1
         else
            np = np + 1
            parentheses(np) = i
         end if 
      end if
   end do

   if ( sens == -1 ) then
      if ( np > 0 ) then
         new = str(parentheses(np)+1:)
         icut1 = parentheses(np)
      else
         new = str
         icut1 = 0
      end if      
   else
      if ( np > 0 ) then
         new = str(1:parentheses(np)-1)
         icut1 = parentheses(np)
      else
         new = str
         icut1 = 0
      end if   
   end if

   lnew = len_trim(new)
      
   if ( lnew == 0 ) then
      call flagerr%Set( UERROR, HERE, 'Invalid index' )
      return
   end if   
   
   if ( sens == -1 ) then
      i1 = lnew ; i2 = 1 ; i3 =-1
   else
      i1 = 1 ; i2 = lnew ; i3 = 1
   end if

   inparentheses = .false.
   op = 0

   do i = i1, i2, i3
      ch = new(i:i)
      
      if ( index(oper,ch) /= 0 ) then
         if ( .not. inparentheses ) then
            if ( sens ==-1 ) icut = i+1
            if ( sens == 1 ) icut = i-1
            exit
         end if   
      end if
 
      if ( ch == '(' .and. sens == 1 ) then
         op = op + 1
         inparentheses = .true.
      end if
      if ( ch == ')' .and. sens == 1 ) then
         op = op - 1
         if ( op == 0 ) inparentheses = .false.
      end if
      if ( ch == ')' .and. sens ==-1 ) then
         op = op + 1
         inparentheses = .true.
      end if
      if ( ch == '(' .and. sens ==-1 ) then
         op = op - 1
         if ( op == 0 ) inparentheses = .false.
      end if
      icut = i   
   end do

   if ( sens ==-1 ) icut = min(icut1+icut,lstr)
   if ( sens == 1 ) icut = max(IONE,icut)

   END SUBROUTINE pk2Interpreter_cuttopar


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_FuncAndOperList ()
!============================================================================================= 
 
!---------------------------------------------------------------------------------------------    
!  Initializes the list of functions (FuncList) and operators (OperList).
!
!  Moreover
!
!  . A short description of each function or operator are given in the arrays FuncDesc and
!    OperDesc.
!
!  . For each operator #i in OperList, the logicals StartEnd(i,1) and StartEnd(i,2) indicate 
!    whether this operator is legal at the beginning and at the end of a regular expression,
!    respectively. For example, StartEnd(1,:) = [.true.,.false.] means that "+" can start a
!    regular expression (unary "+") and that "+" cannot be the last symbol of an expression.
!
!  . For each operator #i in OperList, a string is given in ForbidSq(i) and corresponds to a
!    list of symbols that cannot follow this operator in any regular expression, e.g. for the
!    operator "+" ForbidSq(1) = "~*/^\&|<>:=,;)]}" which precludes the sequences "+~", "+*", 
!    "+/", "+^", ... in a regular expression.
!
!  . To each operator #i corresponds a character (of length 1) givens in OperCode(i). 
!    For an operator with single-character name, Opercode is this name itself.  
!    For others, Opercode is selected from the ascii table beyond position 127.
!    Examples:
!       - for the operator OperList(17) = '>'  we have OperCode(17) = '>'
!       - for the operator OperList(19) = '>=' we have OperCode(19) = char(137)
!    All operators present in the expression to be analyzed will be substituted by their 
!    corresponding OperCode codes (done by pk2Interpreter_Convert)
!
!  Note: StartEnd and ForbidSq are used in the subroutine pk2Interpreter_identifica to detect 
!  basic syntax errors in the user expression.
!
!  Modified: 11/19 (StartEnd and ForbidSq added).
!  Modified: 05/24 (OperCode added)
!----------------------------------------------------------------------------R.H. 06/18, 11/19

!- local variables ---------------------------------------------------------------------------   
   logical         , parameter   :: f = .false., t = .true.
   character(len=*), parameter   :: OPERS = RELAT//ELEMS, &
                  ALPHB = "_%#@0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 
   type     (str_t), allocatable :: userFuncList(:)
   integer  (Ikind)              :: i
!---------------------------------------------------------------------------------------------    

!
!- 1) Operators and delimiters symbols  
!
      
!  algebraic and logical operators:

   OperList(1) = "+"        ; StartEnd(1,:) = [t,f] ; ForbidSq(1) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(1) = "+"        ; OperDesc(1) = "Addition"
   
   OperList(2) = "-"        ; StartEnd(2,:) = [t,f] ; ForbidSq(2) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(2) = "-"        ; OperDesc(2) = "Subtraction"
      
   OperList(3) = "|"        ; StartEnd(3,:) = [f,f] ; ForbidSq(3) = "*/^\&|)]}:=,;"//OPERS
   OperCode(3) = "|"        ; OperDesc(3) = "Logical OR"
   
   OperList(4) = "-"        ; StartEnd(4,:) = [t,f] ; ForbidSq(4) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(4) = "-"        ; OperDesc(4) = "Numeric negation"
   
   OperList(5) = "~"        ; StartEnd(5,:) = [t,f] ; ForbidSq(5) ="*/^\&|)]}:,;'"//OPERS
   OperCode(5) = "~"        ; OperDesc(5) = "Logical negation"
   
   OperList(6) = "*"        ; StartEnd(6,:) = [f,f] ; ForbidSq(6) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(6) = "*"        ; OperDesc(6) = "Multiplication"
   
   OperList(7) = "/"        ; StartEnd(7,:) = [f,f] ; ForbidSq(7) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(7) = "/"        ; OperDesc( 7) = "Divide"
   
   OperList(8) = ".*"       ; StartEnd(8,:) = [f,f] ; ForbidSq(8) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(8) = ELEMMUL    ; OperDesc(8) = "Element-wise multiplication"
   
   OperList(9) = "./"       ; StartEnd(9,:) = [f,f] ; ForbidSq(9) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(9) = ELEMDIV    ; OperDesc(9) = "Element-wise divide"
   
   OperList(10) = "&"       ; StartEnd(10,:) = [f,f] ; ForbidSq(10) = "*/^\&|)]}:=,;"//OPERS
   OperCode(10) = "&"       ; OperDesc(10) = "Logical AND" 
   
   OperList(11) = "\"       ; StartEnd(11,:) = [f,f] ; ForbidSq(11) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(11) = "\"       ; OperDesc(11) = "Linear system solve (or mldivide())"   
   
   OperList(12) = "^"       ; StartEnd(12,:) = [f,f] ; ForbidSq(12) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(12) = "^"       ; OperDesc(12) = "Exponentiation"    
   
   OperList(13) = ".^"      ; StartEnd(13,:) = [f,f] ; ForbidSq(13) = "~*/^\&|:=,;)]}"//OPERS
   OperCode(13) = ELEMPOW   ; OperDesc(13) = "Element-wise exponentiation"
   
!  relational operators:

   OperList(14) = "=="      ; StartEnd(14,:) = [f,f] ; ForbidSq(14) = "~*/^\&|)]}:,;"//OPERS
   OperCode(14) = EQUAL     ; OperDesc(14) = "Equal to (or eq())"     
   
   OperList(15) = "~="      ; StartEnd(15,:) = [f,f] ; ForbidSq(15) = "~*/^\&|)]}:,;"//OPERS
   OperCode(15) = NOTEQ     ; OperDesc(15) = "Not equal to (or ne())"
   
   OperList(16) = "<"       ; StartEnd(16,:) = [f,f] ; ForbidSq(16) = "~*/^\&|)]}:,;"//OPERS
   OperCode(16) = "<"       ; OperDesc(16) = "Less than (or lt())"
   
   OperList(17) = ">"       ; StartEnd(17,:) = [f,f] ; ForbidSq(17) = "~*/^\&|)]}:,;"//OPERS
   OperCode(17) = ">"       ; OperDesc(17) = "Greater than (or gt())"
   
   OperList(18) = "<="      ; StartEnd(18,:) = [f,f] ; ForbidSq(18) = "~*/^\&|)]}:,;"//OPERS
   OperCode(18) = LESSOREQ  ; OperDesc(18) = "Less than or equal (or le())"
   
   OperList(19) = ">="      ; StartEnd(19,:) = [f,f] ; ForbidSq(19) = "~*/^\&|)]}:,;"//OPERS
   OperCode(19) = GREATOREQ ; OperDesc(19) = "Greater than or equal (or ge())"
   
!  delimiters:

   OperList(20) = "("       ; StartEnd(20,:) = [t,f] ; ForbidSq(20) = "*/^\&|=,;"//OPERS
   OperCode(20) = "("       ; OperDesc(20) = "Left parenthesis" 
   
   OperList(21) = ")"       ; StartEnd(21,:) = [f,t] ; ForbidSq(21) = "([{"//ALPHB
   OperCode(21) = ")"       ; OperDesc(21) = "Right parenthesis"
    
   OperList(22) = ","       ; StartEnd(22,:) = [f,f] ; ForbidSq(22) = "*/^\&|;=,)]}"//OPERS
   OperCode(22) = ","       ; OperDesc(22) = "Comma delimiter" 
   
   OperList(23) = ";"       ; StartEnd(23,:) = [f,f] ; ForbidSq(23) = "*/^\&|)]}:=,;"//OPERS
   OperCode(23) = ";"       ; OperDesc(23) = "Semi-colon delimiter"
    
   OperList(24) = "["       ; StartEnd(24,:) = [t,f] ; ForbidSq(24) = "*/^\&|=,;:"//OPERS
   OperCode(24) = "["       ; OperDesc(24) = "Left bracket (array constructor)"
   
   OperList(25) = "]"       ; StartEnd(25,:) = [f,t] ; ForbidSq(25) = "([{"//ALPHB
   OperCode(25) = "]"       ; OperDesc(25) = "Right bracket (array constructor)"
   
   OperList(26) = "{"       ; StartEnd(26,:) = [t,f] ; ForbidSq(26) = "*/^\&|=,;:"//OPERS
   OperCode(26) = "{"       ; OperDesc(26) = "Left brace (not yet used)"
    
   OperList(27) = "}"       ; StartEnd(27,:) = [t,f] ; ForbidSq(27) = "([{"//ALPHB
   OperCode(27) = "}"       ; OperDesc(27) = "Right brace (not yet used))"
    
!  others:   

   OperList(28) = "'"       ; StartEnd(28,:) = [f,t] ; ForbidSq(28) = "([{~"  //ALPHB ! (04/20)
   OperCode(28) = "'"       ; OperDesc(28) = "Conjugate transpose (or trans())"
    
   OperList(29) = ".'"      ; StartEnd(29,:) = [f,t] ; ForbidSq(29) = "([{~"  //ALPHB ! (04/20)
   OperCode(29) = ELEMTRANS ; OperDesc(29) = "transpose (or transp())"
    
   OperList(30) = ":"       ; StartEnd(30,:) = [f,f] ; ForbidSq(30) = "~*/^\&|<>:=;]}"
   OperCode(30) = ":"       ; OperDesc(30) = "Colon operator (or colon())"
    
   OperList(31) = "="       ; StartEnd(31,:) = [f,f] ; ForbidSq(31) = "*/^\&|<>)]}:,;"
   OperCode(31) = "="       ; OperDesc(31) = "Assignment"
    
   OperList(32) = "!"       ; StartEnd(32,:) = [f,t] ; ForbidSq(32) = "([{~"  //ALPHB
   OperCode(32) = "!"       ; OperDesc(32) = "Experimental"
!
!- 2) Function names:
!

!  relational (can replace the corresponding operators):

   FuncList( 1) = "eq" ; FuncDesc( 1) = "Equal to (==)"
   FuncList( 2) = "ne" ; FuncDesc( 2) = "Not equal to (~=)"
   FuncList( 3) = "lt" ; FuncDesc( 3) = "Less than (<)"
   FuncList( 4) = "gt" ; FuncDesc( 4) = "Greater than (>)"
   FuncList( 5) = "le" ; FuncDesc( 5) = "Less than or equal (<=)"
   FuncList( 6) = "ge" ; FuncDesc( 6) = "Greater than or equal (>=)"
   
!  usual functions:
   
   FuncList( 7) = "sin"  ; FuncDesc( 7) = "Sine (arg. in radians)"
   FuncList( 8) = "cos"  ; FuncDesc( 8) = "Cosine (arg. in radians)"
   FuncList( 9) = "tan"  ; FuncDesc( 9) = "Tangent (arg. in radians)"
   FuncList(10) = "asin" ; FuncDesc(10) = "Inverse sine in radians"
   FuncList(11) = "acos" ; FuncDesc(11) = "Inverse cosine in radians"
   FuncList(12) = "atan" ; FuncDesc(12) = "Inverse tangent in radians"
   FuncList(13) = "sind" ; FuncDesc(13) = "Sine (arg. in degrees)"
   FuncList(14) = "cosd" ; FuncDesc(14) = "Cosine (arg. in degrees)"
   FuncList(15) = "tand" ; FuncDesc(15) = "Tangent (arg. in degrees)"
   FuncList(16) = "asind"; FuncDesc(16) = "Inverse sine in degrees"
   FuncList(17) = "acosd"; FuncDesc(17) = "Inverse cosine in degrees"
   FuncList(18) = "atand"; FuncDesc(18) = "Inverse tangent in degrees"
   FuncList(19) = "sinh" ; FuncDesc(19) = "Hyperbolic sine"
   FuncList(20) = "cosh" ; FuncDesc(20) = "Hyperbolic cosine"
   FuncList(21) = "tanh" ; FuncDesc(21) = "Hyperbolic tangent"

   FuncList(22) = "log"       ; FuncDesc(22) = "Natural logarithm"
   FuncList(23) = "log10"     ; FuncDesc(23) = "Common logarithm (base 10)"
   FuncList(24) = "exp"       ; FuncDesc(24) = "Exponential"
   FuncList(25) = "sqrt"      ; FuncDesc(25) = "Square root"
   FuncList(26) = "abs"       ; FuncDesc(26) = "Absolute value and complex magnitude"
   FuncList(27) = "erf"       ; FuncDesc(27) = "Error function"
   FuncList(28) = "erfc"      ; FuncDesc(28) = "Complementary error function"
   FuncList(29) = "gamma"     ; FuncDesc(29) = "Gamma function"
   FuncList(30) = "factorial" ; FuncDesc(30) = "Factorial function"

   FuncList(31) = "nint"  ; FuncDesc(31) = "Nearest whole number (integer result)"
   FuncList(32) = "anint" ; FuncDesc(32) = "Nearest whole number (real result)"
   FuncList(33) = "aint"  ; FuncDesc(33) = "Truncation to a whole number (real result)"
   FuncList(34) = "floor" ; FuncDesc(34) = "Greatest integer s.t. x >= floor(x)"
   FuncList(35) = "ceil"  ; FuncDesc(35) = "Least integer s.t. x <= ceil(x)"

   FuncList(36) = "sign"  ; FuncDesc(36) = "Signum function"
   FuncList(37) = "heav"  ; FuncDesc(37) = "Heaviside function"
   
   FuncList(38) = "real" ; FuncDesc(38) = "Real part"
   FuncList(39) = "imag" ; FuncDesc(39) = "Imaginary part"
   FuncList(40) = "conj" ; FuncDesc(40) = "Complex conjugate"

   FuncList(41) = "trace" ; FuncDesc(41) = "Sum of diagonal elements"
   FuncList(42) = "trans" ; FuncDesc(42) = "Conjugate transpose (or ')" 
   FuncList(43) = "transp"; FuncDesc(43) = "Transpose (or .')"
   FuncList(44) = "det"   ; FuncDesc(44) = "Determinant"
   FuncList(45) = "inv"   ; FuncDesc(45) = "Inverse matrix"
   
   FuncList(46) = "size"  ; FuncDesc(46) = "Array size (shape)"
   FuncList(47) = "numel" ; FuncDesc(47) = "Number of array elements"
   FuncList(48) = "find"  ; FuncDesc(48) = "Indices of non-zero (or non-false) elt."

   FuncList(49) = "num2str" ; FuncDesc(49) = "Numbers to strings conversion"
   FuncList(50) = "str2num" ; FuncDesc(50) = "Strings to numbers conversion"

   FuncList(51) = "magic"   ; FuncDesc(51) = "Magic square"
   
   FuncList(52) = "is_symm" ; FuncDesc(52) = "is_symm(A) = %t if A is symmetric" 
   FuncList(53) = "is_skew" ; FuncDesc(53) = "is_skew(A) = %t if A is skew-symmetric"
   
   FuncList(54) = "norm"    ; FuncDesc(54) = "Array norm(s)"
               
   FuncList(55) = "cross"   ; FuncDesc(55) = "Cross product"
   
   FuncList(56) = "reshape" ; FuncDesc(56) = "Reshape array"
   
   FuncList(57) = "mldivide"; FuncDesc(57) = "Linear system solve (or << \ >>)"

   FuncList(58) = "zeros"   ; FuncDesc(58) = "Array of all zeros"
   FuncList(59) = "falses"  ; FuncDesc(59) = "Array of all falses"
   FuncList(60) = "ones"    ; FuncDesc(60) = "Array of all ones" 
   FuncList(61) = "eye"     ; FuncDesc(61) = "Identity matrix"
   FuncList(62) = "rand"    ; FuncDesc(62) = "Pseudo-random array"
   FuncList(63) = "diag"    ; FuncDesc(63) = "Diagonal matrix or diagonal elements"
   FuncList(64) = "tril"    ; FuncDesc(64) = "Lower triangular part"
   FuncList(65) = "triu"    ; FuncDesc(65) = "Upper triangular part"
   
   FuncList(66) = "colon"   ; FuncDesc(66) = 'Colon function (same as ":" operator)'
   FuncList(67) = "date"    ; FuncDesc(67) = "Get date and time"
   FuncList(68) = "eig"     ; FuncDesc(68) = "Eigenvalues and eigenvectors"
   
   FuncList(69) = "merge"   ; FuncDesc(69) = "Arrays merging"
   
   FuncList(70) = "min"     ; FuncDesc(70) = "Minimum of elements"
   FuncList(71) = "max"     ; FuncDesc(71) = "Maximum of elements"
   FuncList(72) = "sum"     ; FuncDesc(72) = "Sum of elements"
   FuncList(73) = "mean"    ; FuncDesc(73) = "Mean of elements"
   FuncList(74) = "prod"    ; FuncDesc(74) = "Product of elements"

   FuncList(75) = "pwd"     ; FuncDesc(75) = "Current directory"
   FuncList(76) = "cputime" ; FuncDesc(76) = "Cpu time"

   FuncList(77) = "help"    ; FuncDesc(77) = "Helper"

   FuncList(78) = "readmat" ; FuncDesc(78) = "Read a matrix"
   FuncList(79) = "writemat"; FuncDesc(79) = "Write a matrix"
   
   FuncList(80) = "trim"    ; FuncDesc(80) = "Remove trailing blank characters"
   FuncList(81) = "length"  ; FuncDesc(81) = "Number of elements or matrix of lengths"
   
   FuncList(82) = "meshgrid"; FuncDesc(82) = "2-D and 3-D grids"
   
   FuncList(83) = "sort"    ; FuncDesc(83) = "Sort element array"

   FuncList(84) = "all"     ; FuncDesc(84) = "Function all"
   FuncList(85) = "any"     ; FuncDesc(85) = "Function any"
   
   FuncList(86) = "mod"     ; FuncDesc(86) = "Remainder after division"
   
   FuncList(87) = "randi"   ; FuncDesc(87) = "Pseudo-random integer array"
   
   FuncList(88) = "randperm"; FuncDesc(88) = "Random permutation"
   FuncList(89) = "lu"      ; FuncDesc(89) = "LU factorization"
   FuncList(90) = "lusolv"  ; FuncDesc(90) = "Solve a linear system after factorization"

   FuncList(91) = "cov"     ; FuncDesc(91) = "Covariance matrix"
   FuncList(92) = "reglin"  ; FuncDesc(92) = "Simple linear regression model"

   FuncList(93) = "convstr" ; FuncDesc(93) = "Convert strings to lowercase or uppercase"
   FuncList(94) = "replace" ; FuncDesc(94) = "Replace a substring in a string"
   FuncList(95) = "part"    ; FuncDesc(95) = "Extraction of characters from strings"

   FuncList(96) = "dev "    ; FuncDesc(96) = "Deviatoric part of a square matrix"
   
   FuncList(97) = "inttrap" ; FuncDesc(97) = "1d integration via trapezoidal rule"
   
   FuncList(98) = "sizeof"  ; FuncDesc(98) = "Memory size (number of bytes) of a variable"

   FuncList(99) = "typeof"  ; FuncDesc(99) = "Type of a variable"
   
   FuncList(100) = "svd"    ; FuncDesc(100) = "Singular values decomposition"
   FuncList(101) = "poldec" ; FuncDesc(101) = "Polar decomposition"

   FuncList(102) = "sinc"   ; FuncDesc(102) = "Cardinal sine function (= sin(x)/x) "

   call userProcList ( nfunc = nuserf, funcList = userFuncList )
      
   do i = 1, nuserf
      FuncList(NFC+i) = userFuncList(i)
      FuncDesc(NFC+i) = "user function"
   end do   
   
   END SUBROUTINE pk2Interpreter_FuncAndOperList 


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_Expos ( str, S01, S02, is_modified, flagerr )
!=============================================================================================
   character(len=*), intent(in out) :: str
   character(len=1), intent(in    ) :: S01, S02
   logical         , intent(   out) :: is_modified
   type     (err_t), intent(   out) :: flagerr
!---------------------------------------------------------------------------------------------        
!  Replaces the symbol '-' by S01 and '+' by S02 in any occurence of 'ne-m', 'nE-m', 'nd-m',
!  'nD-m', 'ne+m', 'nE+m', 'nd+m', 'nD+m' (where n and m are numbers) in the string "str".
!  Sets "is_modified" to .true. if any of these replacements occurs.
!---------------------------------------------------------------------------------------------        

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2Interpreter_Expos'
   character(len=2), parameter :: EXPO(8) = ['e-','E-','d-','D-','e+','E+','d+','D+']
   character(len=*), parameter :: NUMB = ".0123456789", OPER = "+-*/^([{<>=,;&|~:\"
   character(len=1)            :: c, cm, cp
   integer  (Ikind)            :: lstr, iexpo, j, pos, start
   logical                     :: is_number
!---------------------------------------------------------------------------------------------        

   if ( flagerr%code > IZERO ) return

   is_modified = .false.

   lstr = len_trim(str)
!
!- Look for the presence of the 8 patterns (e-, E-, d-, ...) in the string. For each pattern
!  found, check whether it is followed and preceded by "numbers" before any replacement:
!
   do iexpo = 1, size(EXPO)
      pos =  1 ; start = 1
      
      do
         pos = index(str(start:),EXPO(iexpo)) ; if ( pos == 0 ) exit
         
         pos = pos + start - 1
         
         if ( pos > 1 .and. pos < lstr-1 ) then
!
!-          If the previous and the following characters are not "numbers": exit
!         
            cm = str(pos-1:pos-1) ; cp = str(pos+2:pos+2)
            
            if ( index(NUMB,cm) == 0 .or. index(NUMB,cp) == 0 ) exit
!
!-          If any of the previous characters (up to an operator) is not a "number" set 
!           is_number = .false.
!            
            is_number = .true.
            do j = pos-1, 1, -1
               c = str(j:j)
               if ( index(OPER,c) /= 0 ) exit
               if ( index(NUMB,c) == 0 ) then
                  is_number = .false.
                  exit
               end if
            end do
!
!-          Replace the signs '-' and '+' by S01 and S02, resp., or exit if is_number=.false. 
!       
            if ( is_number ) then
               if ( iexpo <= 4 ) then
                  str(pos+1:pos+1) = S01 ! replace '-' by S01
               else
                  str(pos+1:pos+1) = S02 ! replace '+' by S02
               end if
               is_modified = .true.
               start = pos + 1
            else
               exit
            end if
         else if ( pos >= lstr-1 ) then
            call flagerr%Set( UERROR, HERE, 'Invalid expression' )
            return
         else 
            exit 
         end if
      end do
   end  do

   END SUBROUTINE pk2Interpreter_Expos

   
!= Added (riad) ==============================================================================  
   FUNCTION pk2Interpreter_IsAnOperator ( str ) result ( is_operator )
!=============================================================================================   
   character(len=*), intent(in) :: str
   logical                      :: is_operator
!---------------------------------------------------------------------------------------------                   
   
!- local variables: --------------------------------------------------------------------------   
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------                   
   
   is_operator = .false.
   do i = 1, NOP
      if ( trim(str) == trim(OperList(i)%str) ) then
         is_operator = .true.
         return
      end if
   end do
         
   END FUNCTION pk2Interpreter_IsAnOperator   


!= Added (riad) ==============================================================================  
   FUNCTION pk2Interpreter_FindIdStr ( str, ListNames, ind1, ind2, step ) result ( Id )
!=============================================================================================   
   character(len=*), intent(in) :: str
   type     (str_t), intent(in) :: ListNames(:)
   integer  (Ikind), intent(in) :: ind1, ind2, step
   integer  (Ikind)             :: Id
!---------------------------------------------------------------------------------------------                   
!  Returns the # in the array "ListNames" of the first occurence of "str" (0 otherwise)
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------   
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------                   
   
   Id = 0
 
   do i = ind1, ind2, step ! 1, size(listnames)
      if ( allocated(ListNames(i)%str) ) then
         if ( trim(adjustl(str)) == trim(ListNames(i)%str) ) then
            Id = i
            return
         end if   
      end if
   end do
         
   END FUNCTION pk2Interpreter_FindIdStr  


!= Added (riad) ==============================================================================  
   FUNCTION pk2Interpreter_FindIdChar1 ( str, ListNames, ind1, ind2, step ) result ( Id )
!=============================================================================================   
   character(len=*), intent(in) :: str
   character(len=1), intent(in) :: ListNames(:)
   integer  (Ikind), intent(in) :: ind1, ind2, step
   integer  (Ikind)             :: Id
!---------------------------------------------------------------------------------------------                   
!  Returns the # in the array "ListNames" of the first occurence of "str" (0 otherwise)
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------   
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------                   

   Id = 0
 
   do i = ind1, ind2, step ! 1, size(listnames)
      if ( str == ListNames(i) ) then
         Id = i
         return
      end if   
   end do
         
   END FUNCTION pk2Interpreter_FindIdChar1
   
   
!= Added (riad) ==============================================================================  
   FUNCTION pk2Interpreter_FindVarId ( str, vars ) result ( Id )
!=============================================================================================   
   character(len=*), intent(in) :: str
   type     (pk2_t), intent(in) :: vars(:)
   integer  (Ikind)             :: Id
!---------------------------------------------------------------------------------------------                   
!  Returns the # in the array "vars" of the first occurence of "str" (0 otherwise)
!---------------------------------------------------------------------------------------------                   

!- local variables: --------------------------------------------------------------------------   
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------                   
   
   Id = 0
 
   do i = 1, size(vars)
      if ( allocated(vars(i)%name) ) then
         if ( trim(adjustl(str)) == trim(vars(i)%name) ) then
            Id = i
            return
         end if   
      end if
   end do
         
   END FUNCTION pk2Interpreter_FindVarId 


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_AllocHdl ( handle, n, flagerr ) 
!=============================================================================================
   class  (hdl_t), intent(in out) :: handle
   integer(Ikind), intent(in    ) :: n
   type   (err_t), intent(in out) :: flagerr   
!---------------------------------------------------------------------------------------------
!  Allocate or reallocate the allocatable members of the handle with a size of n.
!-----------------------------------------------------------------------------------R.H. 04/20       

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2Interpreter_AllocHdl'
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   if ( flagerr%code > IZERO ) return

   if ( handle%allocated ) then
      if ( handle%size >= n ) then
!
!-       The handle is already allocated and has a sufficient size
!
         return
      else
!
!-       The handle is allocated but need to be resized
!      
         deallocate( handle%stokens, handle%csep , handle%operations, handle%narg, &
                     handle%scalars, handle%pdata, handle%sdata,                   &
                     handle%involvedVar, stat=err                                  )
                     
         if ( err /= 0 ) then
            call flagerr%Set( UERROR, HERE, 'Deallocation failure' )
            return
         end if
         
      end if   
   end if

   allocate(handle%stokens(n), stat=err)
   if ( err /= 0 ) then
      call flagerr%Set( UERROR, HERE, 'Allocation failure for << handle%stokens >>' )
      return
   end if   

   allocate(handle%csep(n), stat=err)
   if ( err /= 0 ) then
      call flagerr%Set( UERROR, HERE, 'Allocation failure for << handle%csep >>' )
      return
   end if   
         
   allocate(handle%operations(n), stat=err)
   if ( err /= 0 ) then
      call flagerr%Set( UERROR, HERE, 'Allocation failure for << handle%operations >>' )
      return
   end if      
   
   allocate(handle%narg(n), stat=err)
   if ( err /= 0 ) then
      call flagerr%Set( UERROR, HERE, 'Allocation failure for << handle%arg >>' )
      return
   end if      
   
   allocate(handle%scalars(n), stat=err)
   if ( err /= 0 ) then
      call flagerr%Set( UERROR, HERE, 'Allocation failure for << handle%scalars >>' )
      return
   end if   

   allocate(handle%pdata(n), stat=err)
   if ( err /= 0 ) then
      call flagerr%Set( UERROR, HERE, 'Allocation failure for << handle%pdata >>' )
      return
   end if   

   allocate(handle%sdata(handle%nsdata), stat=err)
   if ( err /= 0 ) then
      call flagerr%Set( UERROR, HERE, 'Allocation failure for << handle%sdata >>' )
      return
   end if   

   allocate(handle%involvedVar(n), stat=err)
   if ( err /= 0 ) then
      call flagerr%Set( UERROR, HERE, 'Allocation failure for << handle%involvedVar >>' )
      return
   end if   
         
   handle%allocated = .true.
   handle%size      = n
   
   END SUBROUTINE pk2Interpreter_AllocHdl


!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_InitHdl ( self, ntokens, flagerr ) 
!=============================================================================================    
   class  (hdl_t), intent(in out) :: self
   integer(Ikind), intent(in    ) :: ntokens
   type   (err_t), intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------
!  Initializes (or resets) the handle "self"                
!---------------------------------------------------------------------------------------------                   

!- local variables ---------------------------------------------------------------------------
   integer  (Ikind) :: i
   integer  (Ikind), allocatable :: nsresult(:)
!---------------------------------------------------------------------------------------------

   if ( flagerr%code > IZERO ) return

   i = 0
   if ( .not. self%allocated ) then
      i = 1
   else
      if ( self%size < ntokens ) i = 1
   end if
   
   if ( i == 1 ) then
      call self%Alloc ( ntokens, flagerr )
      error_TraceNreturn(flagerr, 'pk2Interpreter_InitHdl')
   end if
   
   self%ntokens = ntokens
   self%noperat = 1
            
   do i = 1, ntokens
      self%csep       (i)%str  = ''
      self%scalars    (i)%name = ''
      self%pdata      (i)%name = ''
      self%operations (i)      = 0
      self%involvedVar(i)%str  = ''
      self%narg       (i)      = 0
   end do   

   call userProcList ( nfunc = i, nbrOptOutputs = nsresult )
      
   self%nsdata = max(self%nsdata, maxval(nsresult))
   
   do i = 1, self%nsdata
      self%sdata(i)%name = ''
   end do   
   
   self%flag%code = 0
   self%flag%mesg = ''
   
   END SUBROUTINE pk2Interpreter_InitHdl
   
   
!= Added (riad) ==============================================================================  
   SUBROUTINE pk2Interpreter_DestroyHdl ( self ) 
!=============================================================================================    
   class(hdl_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------                   
!---------------------------------------------------------------------------------------------                   

   if ( allocated(self%expr       ) ) deallocate(self%expr)       
   if ( allocated(self%stokens    ) ) deallocate(self%stokens)       
   if ( allocated(self%csep       ) ) deallocate(self%csep)       
   if ( allocated(self%operations ) ) deallocate(self%operations)    
   if ( allocated(self%narg       ) ) deallocate(self%narg)       
   if ( allocated(self%scalars    ) ) deallocate(self%scalars) 
   if ( allocated(self%pdata      ) ) deallocate(self%pdata) 
   if ( allocated(self%sdata      ) ) deallocate(self%sdata) 
   if ( allocated(self%involvedVar) ) deallocate(self%involvedVar)    
   
   self%noperat = 0 ; self%ntokens = 0 
   
   self%initialized = .false. ; self%allocated = .false. ; self%size = 0

   END SUBROUTINE pk2Interpreter_DestroyHdl   


END MODULE pk2Interpreter_m
  
