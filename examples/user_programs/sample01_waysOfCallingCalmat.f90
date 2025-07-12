program sample1_waysOfCallingCalmat
!
! An example showing the different ways to call calmat
!
!-----------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
   type(err_t)        :: stat
   logical, parameter :: Yes = .true., No = .false.
!-----------------------------------------------------------------------------------------------

   ! 1) calling calmat to parse and evaluate instructions from a file:
   
   call calmat ( warning=Yes, welcome=No, fileIn='../scripts/newton2', dispRes=No )

   ! 2) calling calmat to parse and evaluate a given expression:
   
   call calmat ( exprIn='xs = %pi*(0:.2:1)' )

   ! 3) calling calmat to parse and evaluate several expressions
   !    (variables defined previously (e.g. "xs") can be reused):
   
   call calmat ( exprIn='w = 2/3; ys = xs+w ; zs = sin( %pi*xs ), list()' )

   ! 4) calling calmat in interactive mode 
   !    (prompt the user for expressions. Type exit to finish):
   
   call calmat ( dispRes=Yes )

   ! 5) As you may have noticed by printing out the list of variables ("list" command),    
   !    all the variables created during the various calmat calls (and also during setup)  
   !    have been stored in memory and can therefore be reused from one call to the next. 
   !
   !    If this is not the desired behavior, a reset is necessary
   !    (another way is to call calmat with the "clear" command: exprIn = "clear"):
   
   call calmat_resetAll ()

   ! 6) And the following call will result in an error as the variables xs and w are now 
   !    undefined due to the reset:
   
   print '(/,a,/)', ansiColor_colorTxt ( str = 'Intentional error:', unit = 6, &
                                         ansiColorCode = err_colorError        )
   
   ! calling calmat with errors on the caller control (presence of "stat"):
   
   call calmat ( stat = stat, exprIn = 'ys = exp(-xs+w)' )
   
   call stat%display() ! print the error message (and do not stop)  
   
   print*,'Terminated'
   
end program sample1_waysOfCallingCalmat
