!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------

MODULE signalHandler_m

   use kindParameters_m, only: Ikind
   use pk2Constants_m  , only: STDOUT, NLT
   use util_Strings_m  , only: util_ScrollingMessage
   use iso_c_binding   , only: c_int, c_funptr, c_funloc

   implicit none
   
   private
   public :: signalHandler_SignalCatch, signalHandler_CatchIll , signalHandler_CatchAbrt, &
             signalHandler_CatchFpe   , signalHandler_CatchSegv, signalHandler_CatchTerm, &
             signalHandler_CatchInt   
   
   integer(c_int), parameter :: sigint = 2, sigill  =  4, sigabrt =  6, &
                                sigfpe = 8, sigsegv = 11, sigterm = 15
                                
   character(len=:), allocatable :: m, farewellMessage
   integer  (Ikind)              :: uo
!
!- interface to linux API
!   
   interface
      function c_signal (signum, handler) bind(c, name = "signal") result(res)
         use iso_c_binding
         type   (c_funptr), value, intent(in) :: handler
         integer(c_int   ), value, intent(in) :: signum
         type   (c_funptr)                    :: res
      end function 
   end interface

CONTAINS

!=============================================================================================
   recursive SUBROUTINE signalHandler_SignalCatch ( unit, title, farewellMsg )
!=============================================================================================
   integer  (Ikind),           intent(in) :: unit
   character(len=*), optional, intent(in) :: title, farewellMsg
   
!- local variables: --------------------------------------------------------------------------
   type(c_funptr) :: h
!---------------------------------------------------------------------------------------------
   
   uo = unit
   
   if (present(title)) then
      m = title
   else
      m = ''
   end if      
   
   if ( present(farewellMsg) ) farewellMessage = farewellMsg
  
   !!h = c_signal ( sigill  , c_funloc(signalHandler_CatchIll)  )
   h = c_signal ( sigabrt , c_funloc(signalHandler_CatchAbrt) )
   !!h = c_signal ( sigfpe  , c_funloc(signalHandler_CatchFpe)  )
   !!h = c_signal ( sigsegv , c_funloc(signalHandler_CatchSegv) )
   !!h = c_signal ( sigterm , c_funloc(signalHandler_CatchTerm) )
   h = c_signal ( sigint  , c_funloc(signalHandler_CatchInt)  )

   END SUBROUTINE signalHandler_SignalCatch


!=============================================================================================
   recursive SUBROUTINE signalHandler_CatchInt (sig) bind(c)
!=============================================================================================   
   integer(c_int), intent(in) :: sig
!--------------------------------------------------------------------------------------------- 

   m = char(10) // char(10) // trim(m) // char(10) // '--> Caught SIGINT signal (CTRL+C)'
   
   if ( allocated(farewellMessage) ) then
      m = m // NLT // farewellMessage 
   else
      m = m // NLT // 'Exiting now. Bye!'
   end if 
   m = m // ' ..............'

   if (uo == STDOUT) then
      call util_ScrollingMessage (m, .1)
   else
      write(uo,'(a)')m
   end if
   
   write(uo,'(a)') ; stop
   
   END SUBROUTINE signalHandler_CatchInt 


!=============================================================================================
   recursive SUBROUTINE signalHandler_CatchIll (sig) bind(c)
!=============================================================================================
   integer(c_int), intent(in) :: sig
!--------------------------------------------------------------------------------------------- 
   
   m = char(10) // char(10) // trim(m) // char(10) //   &
       '--> Caught SIGILL signal (Illegal Instruction), exiting now. Bye! ...................'

   if (uo == STDOUT) then
      call util_ScrollingMessage (m, .1)
   else
      write(uo,'(a)')m
   end if
   
   write(uo,'(a)') ; stop
   
   END SUBROUTINE signalHandler_CatchIll


!=============================================================================================
   recursive SUBROUTINE signalHandler_CatchAbrt (sig) bind(c)
!=============================================================================================
   integer(c_int), intent(in) :: sig
!--------------------------------------------------------------------------------------------- 

   m = char(10) // char(10) // trim(m) // char(10) //   &
       '--> Caught SIGABRT signal (Abort), exiting now. Bye! ...................'
   
   if (uo == STDOUT) then
      call util_ScrollingMessage (m, .1)
   else
      write(uo,'(a)')m
   end if
   
   write(uo,'(a)') ; stop
      
   END SUBROUTINE signalHandler_CatchAbrt


!=============================================================================================
   recursive SUBROUTINE signalHandler_CatchFpe (sig) bind(c)
!=============================================================================================
   integer(c_int), intent(in) :: sig
!--------------------------------------------------------------------------------------------- 
   
   m = char(10) // char(10) // trim(m) // char(10) //   &
       '--> Caught SIGFPE signal (Floating Point Exception), exiting now. Bye! ..............'
   
   if (uo == STDOUT) then
      call util_ScrollingMessage (m, .1)
   else
      write(uo,'(a)')m
   end if
   
   write(uo,'(a)') ; stop
   
   END SUBROUTINE signalHandler_CatchFpe


!=============================================================================================
   recursive SUBROUTINE signalHandler_CatchSegv (sig) bind(c)
!=============================================================================================
   integer(c_int), intent(in) :: sig
!--------------------------------------------------------------------------------------------- 
   
   m = char(10) // char(10) // trim(m) // char(10) //   &
       '--> Caught SIGSEGV signal (Segmentation Violation), exiting now. Bye! ..............'
   
   if (uo == STDOUT) then
      call util_ScrollingMessage (m, .1)
   else
      write(uo,'(a)')m
   end if
   
   write(uo,'(a)') ; stop
   
   END SUBROUTINE signalHandler_CatchSegv


!=============================================================================================
   recursive SUBROUTINE signalHandler_CatchTerm(sig) bind(c)
!=============================================================================================
   integer(c_int), intent(in) :: sig
!--------------------------------------------------------------------------------------------- 

   m = char(10) // char(10) // trim(m) // char(10) //   &
       '--> Caught SIGTERM signal (Termination Request), exiting now. Bye! ..................'
   
   if (uo == STDOUT) then
      call util_ScrollingMessage (m, .1)
   else
      write(uo,'(a)')m
   end if
   
   write(uo,'(a)') ; stop
      
   END SUBROUTINE signalHandler_CatchTerm


END MODULE signalHandler_m
