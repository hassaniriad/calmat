! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor Check_pk2interpreter.f90 -L../../lib/nagfor -lpk2 -llapack -lblas -o check_pk2interpreter

! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran Check_pk2interpreter.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_pk2interpreter

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort Check_pk2interpreter.f90 -L../../lib/ifort -lpk2 -llapack -lblas -o check_pk2interpreter

program Check_pk2interpreter
   use pk2mod_m
   use, intrinsic :: ieee_arithmetic
   
   implicit none
!---------------------------------------------------------------------------------------------           
   type     (pk2_t)              :: vars(10) ! A list of 10 pk2 arrays
   type     (pk2_t), allocatable :: res(:)   ! The result of an evaluation (must be an alloc.)
                                             ! through a call to pk2Interpreter_driver
                                               
   type     (err_t)              :: flag     ! Flag error
   integer  (Ikind)              :: n = 4, m = 10, i, j, ntimes=100000, ktimes
   type     (hdl_t)              :: h(2)
   type     (str_t)              :: varList(10)
   character(len=90)             :: iter,   buf
   real                          :: t1, t2, tp1, tp2
!---------------------------------------------------------------------------------------------        
   
   call SignalHandler_SignalCatch (unit = STDOUT, title = '--> Check_pk2interpreter Info:')
   
   ! Enable automatic stop due to error (useless since this is the default):

   call err_SetHaltingMode ( halting = .true. )       
   
   ! We set our current variables (4 for example):
   vars(1) = n ; vars(1)%name = 'n'   ! an integer scalar
   vars(2) = magic(n)
   vars(2) = cos(vars(2)) * ( 1.0_Rkind - atan(vars(2)) )   ! a given 3x3 matrix
   vars(2) = vars(2) + trans(vars(2)) ; vars(2)%name = 'A'  ! a 3x3 sym. matrix
   vars(3) = zeros(n,n)      ; vars(3)%name = 'l'           ! a matrix of zeros
   vars(4) = ones(n,1_Ikind) ; vars(4)%name = 'v'           ! a vector of ones

   do i = 1, 10
      if (allocated(vars(i)%name)) then
         varList(i)%str = vars(i)%name
      else
         varList(i)%str = ''
      end if   
   end do
      
   call vars(2)% PrintMe (form = 'values', msg = 'Let A = ')
   call vars(4)% PrintMe (form = 'values', msg = 'and v = ')

   write(*,'(/,a,/,a)',advance = 'no') &
   'test with a long loop to check the memory usage? (no result displayed) (yes=return)',&
   '(alternate: on mac os x use "leaks -atExit -- ./check_pk2interpreter"): '
   
   read(*,'(a)') buf
   print*
   
   if (len_trim(buf) /= 0 .and. buf /= 'y') ntimes = 1
   
   if (len_trim(buf) /= 0) ntimes = 1
   
   call cpu_time(t1) ; tp1 = t1
   
   do ktimes = 1, ntimes
!
!-    I) An expression to be evaluated once. We can use the default driver:
!
      if (ntimes == 1) &
         print '(/,a)','I) An expression to be evaluated once (using the default driver):'
         
      call pk2Interpreter_driver (" eig ( A ) ", vars, res, flag, nanswer = 2_Ikind)
   
      if (ntimes==1) then
         call res(1)%PrintMe (form='values', msg = 'The eigenvalues of A are: ')
         call res(2)%PrintMe (form='values', msg = 'The eigenvectors of A are: ')
   
         call pk2_Print(max(abs(res(1))),form='values', &
                        msg = 'The largest absolute value of the eigenvalues of A is: ')    
      end if   
!
!-    I-bis) Or we can access the handle first (result of the token analyzer):
!
      if (ntimes == 1) &
         print '(/,a)','I-bis) Another one but in two steps: call the token analyzer first:'

      call pk2Interpreter_Tokenizor (expr="size(v)", varNames=varList, handle=h(1), flagerr=flag)
      
     if (ntimes == 1) &
        print '(/,a)','       Analysis: done'
!
!-    and then evaluate the analyzed expression:
! 
      if (ntimes == 1) &
         print '(/,a)','I-bis) and then evaluate the analyzed expression:'
       
      call pk2Interpreter_Evaluator ( handle=h(1), vars=vars, valExpr=res, flagerr=flag )
      
      call h(1)%Destroy()
      
      if (ntimes == 1) then
         do j = 1, size(res)
            call res(j)%PrintMe (form='values', msg = 'size(A) : ')
         end do 
      end if    

      if (ntimes == 1) &
         print '(/,a)','I-ter) Same thing but the list of variable names is not given:'
      
      call pk2Interpreter_Tokenizor (expr="size(v)", vars=vars, handle=h(1), flagerr=flag) 
      
      if (ntimes == 1) &
         print '(/,a)','       Analysis: done'

      if (ntimes == 1) &
         print '(/,a)','I-ter) and then evaluate the analyzed expression:'
       
      call pk2Interpreter_Evaluator ( handle=h(1), vars=vars, valExpr=res, flagerr=flag )
      
      if (ntimes == 1) then
         do j = 1, size(res)
            call res(j)%PrintMe (form='values', msg = 'size(A) : ')
         end do 
      end if    
      
!
!-    II) Two expressions to be evaluated several times provided that the list of variables
!         doesn't change:
!
      if (ntimes == 1) then
         print '(/,a)','II) Two expressions to evaluate several times'
         print '(a  )','    (but analyzed only at the first pass):'
      end if
       
      call pk2Interpreter_Tokenizor &
                  (expr="norm(A*v) / norm(v)", varNames=varList, handle=h(1), flagerr=flag) 
      
      if (ntimes == 1) &         
         print '(/,a)','    Analysis (expression 1): done'   

      call pk2Interpreter_Tokenizor &
                     (expr="A*v / norm(A*v)", varNames=varList, handle=h(2), flagerr=flag) 

      if (ntimes == 1) &
         print '(/,a)','    Analysis (expression 2): done'   
      
      if (ntimes == 1) &
         print '(/,a)',"Now, let's evaluate them m times:"  
         
      do i = 1, m
   
         ! Evaluate the 1st expression:
         if (ntimes == 1) &
            print '(/,a)','Evaluate the 1st expression:'

         call pk2Interpreter_Evaluator ( handle=h(1), vars=vars, valExpr=res, flagerr=flag )
               
         if (ntimes==1) then
            write(iter,'(a,i0)')'iteration #',i      
            call res(1)% PrintMe ( form = 'values', &
                                    msg = trim(iter) // ', norm(A*v) / norm(v) = ' )
            do j = 1, size(res)-1
               call res(j)% PrintMe ( form = 'values',msg = 'r(j) = ' )
            end do   
         end if

         ! Evaluate the 2d expression:
         if (ntimes == 1) &
             print '(/,a)','Evaluate the 2d expression:'

         call pk2Interpreter_Evaluator ( handle=h(2), vars=vars, valExpr=res, flagerr=flag )
               
         if (ntimes==1) &
            call res(1)% PrintMe ( form = 'values',msg = 'A*v / norm(A*v) = ' )

         ! We overwrite vars(4) (i.e. "v") by this result:
         vars(4) = res(1) ; vars(4)%name = 'v' 
      
      end do
      
      if (mod(ktimes,1000_Ikind)==0) then
         call cpu_time(tp2)
         write(*,'(a,i0,a,i0,a,g0,a)',advance='no') "iterations #",ktimes," / ",ntimes, &
                                                    ". Partial cpu time: ",tp2-tp1,  &
                                                    " (hit return to continue) "
         call cpu_time(tp1)
         read*
      end if
   
   end do

   call h(1)%destroy ; call h(2)%destroy ; deallocate(res)
      
   call cpu_time(t2)  
   print '(/,a,/,f12.5)','Ellapsed Time: ',t2-t1   
   print '(//,a,/,24("="),/)','End Check_pk2interpreter'

end program Check_pk2interpreter
