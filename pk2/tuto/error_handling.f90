program error_handling

!---------------------------------------------------------------------------------------------
!
!- A small demo showing how to use the pk2 library
!
!  Example showing how to 
!  . manage error
!
!  For compiling this program:
!
!  gfortran -cpp -I $dmod error_handling.f90 -L$dlib -lpk2 -llapack -lblas -o error_handling
!  ifort    -fpp -I $dmod error_handling.f90 -L$dlib -lpk2 -llapack -lblas -o error_handling
!  nagfor   -fpp -I $dmod error_handling.f90 -L$dlib -lpk2 -llapack -lblas -kind=byte -o error_handling
!
!  where $dmod is ../mod/$comp  and $dlib is ../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   type(pk2_t) :: x, bool 
!---------------------------------------------------------------------------------------------

   bool = 2 < 1

!=============================================================================================
!                                     Error handling
!=============================================================================================

   write(*,'(/,a,/)')'******** Error handling *********'
!
!  a) Under the control of the caller
!     -------------------------------
!
   call err_SetHaltingMode ( halting = .false., &       ! disable automatic stop due to error
                             DisplayWarning = .false. ) ! disable automatic print of warnings

!  the following will cause a warning, but with DisplayWarning=.false. nothing is printed  

   write(*,'(a,/)') &
         '1) Intentional warning (on caller control): x = x + 1 (with x empty)'

   x = x + 1

!  and we can check the opflag variable code (opflag%code < 0 in case of warning):

   if (opflag%code < 0) then
      write(*,'(a)')'There is a warning message (opflag < 0)'
      write(*,'(a)',advance = 'no') &
            'Press return to continue (the warning message will be printed)'
      read*
   end if
 
!  or we can display the message it contains (and by default "opflag" is reset after the print):
!  (if no error/warning has occured, nothing would be printed)

   call opflag%display()

   write(*,'(a)',advance = 'no')'Continue (press return)?'
   read*
   
!  the following will cause an error, but with halting = .false., the execution is not stopped

   write(*,'(/,a,/)') &
         '2) Intentional error (on caller control): x = sin(bool) with "bool" a boolean'
 
   x = sin ( bool )

!  and we can check the opflag variable code (opflag%code > 0 in case of error):

   if (opflag%code > 0) then
      write(*,'(a)')'There is an error (opflag > 0)'
      write(*,'(a)',advance = 'no') &
            'Press return to continue (the error message will be printed)'
      read*
   end if
 
!  or we can display the message it contains (and by default "opflag" is reset after the print):
!  (if no error has occured, nothing would be printed)

   call opflag%display()

   write(*,'(a)',advance = 'no')'Continue (press return)?'
   read*

!   
!  b) Under the control of the callee
!     -------------------------------

!  Now we reset 'DisplayWarning' to .true., the same instruction will cause the print of the
!  message

   call err_SetHaltingMode ( DisplayWarning = .true. )  ! enable automatic print of warnings 

   write(*,'(/,a,/,a)') &
   '3) Same intentional warning (on callee control): x = x + 1 (with x empty)',  &
   '   (the program will print the warning and will continue)'

   x = pk2_t() ; x = x + 1

   write(*,'(a)',advance = 'no')'Continue (press return)?'
   read*

!  Now we also reset "halting" to .true., the following instruction will cause the stop of 
!  the program preceded by the print of the error message (on stdout by default)

   call err_SetHaltingMode ( halting = .true. )  ! enable automatic stop due to error

   write(*,'(/,a,/,a)') &
   '4) Same intentional error (on callee control): x = sin(bool) with "bool" a boolean', &
   '   (the program will stop and the remainder will not be reached)'

   x = sin ( bool )

   write(*,'(/,a)')'Terminated' ! Of course, not reached
   
end program error_handling
