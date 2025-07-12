program sample6_evaluateAsum
!
! Parsing is time consuming
!
! Assume we want to calculate the sum (an approximation of pi):  
!
!            s = \sum_{i=0}^n X_i
!   where 
!            X_i = 4 (-1)^i / (2i+1)  
!  
! We have at least three choices:
!
! 1) calling calmat n+1-times in a do-loop for evaluating each term X_i
! 2) same as (1) but we parse first the expression then evaluate it n+1-times in a do-loop
! 3) calling calmat only once using a calmat for-loop
!
! The first solution takes more time because each time calmat is called, the expression of
! X_i is parsed 
!---------------------------------------------------------------------------------------------   
   use calmat_m
   implicit none
   logical, parameter            :: Yes = .true., No = .false.
   
   integer  (Ikind)              :: i, n = 50000, fId
   character(len=:), allocatable :: expr
   integer  (Ikind), pointer     :: i_ptr => NULL()
   real     (Rkind), pointer     :: x_ptr => NULL()
   type     (ind_t)              :: indi, indx
   real     (Rkind)              :: s     
   real                          :: t1, t2     
!---------------------------------------------------------------------------------------------

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 1) Calling calmat in a do-loop (not recommended for this example):
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call cpu_time(t1)
   ! define a variable named "i" and a variable named "s" initialized to 0 :
   call calmat ( exprIn = 'i=0; s=0.0;', welcome = No )
   
   ! get the index of the variable "i" in the list of calmat variables (G_vars): 
   call Calmat_inquire ( 'i', indi )
   if ( indi%Id == 0 ) stop 'variable "i" not found'
   ! and associate it with the (integer) pointer i_ptr:
   call Calmat_getPointer ( var = indi, ptr = i_ptr )
   
   ! the loop:
   do i = 0, n
      ! set the calmat variable 'i' (i.e. G_vars(indi%Id)) to the new value of i:
      i_ptr = i
      ! call calmat to evaluate X_i and sum the result into s:
      call calmat ( exprIn = 's = s + (4*(-1)^i)/(2*i+1);' )
   end do
   ! display the result:
   call calmat ( exprIn='disp(s-%pi,"s-pi = ")' )

   ! nullify the pointer:
   nullify(i_ptr)
   
   call cpu_time(t2)
   print*,'cpu time for the method 1: ',t2-t1
     
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 2) Calling calmat in a do-loop but with the expression parsed first
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call calmat_resetAll() ! first reset the previous setup and the list of variables

   call cpu_time(t1)
   ! define a variable named "i" and a variable named "s" initialized to 0 :
   call calmat ( exprIn = 'i=0; s=0.0;', welcome = No )
   
   ! get the index of the variable "i" in the list of calmat variables (G_vars): 
   call Calmat_inquire ( 'i', indi )
   if ( indi%Id == 0 ) stop 'variable "i" not found'
   ! and associate it with the (integer) pointer i_ptr:
   call Calmat_getPointer ( var = indi, ptr = i_ptr )
   
   ! Parse the expression to evaluate (the result will be stored in the stream #fId):
   call calmat ( exprIn = 's = s + (4*(-1)^i)/(2*i+1);', parseOnly = fId )

   ! the loop:
   do i = 0, n
      ! set the calmat variable 'i' (i.e. G_vars(indi%Id)) to the new value of i:
      i_ptr = i
      ! evaluate s (i.e. execute the stream #fId):
      call calmat ( evaluate = fId )
   end do
   ! display the result:
   call calmat ( exprIn='disp(s-%pi,"s-pi = ")' )
   
   ! delete the flow and nullify the pointer
   call calmat_delFlows ( fId )
   nullify(i_ptr)

   call cpu_time(t2)
   print*,'cpu time for the method 2: ',t2-t1

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 2-bis) Another variant of method 2 (evaluate Xi and not s)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call calmat_resetAll() ! first reset the previous setup and the list of variables

   call cpu_time(t1)
   ! define a variable named "i" and a variable named "Xi" initialized to 0 :
   call calmat ( exprIn = 'i=0; Xi=0.0;', welcome = No )
   
   ! get the index of the variable "i" in the list of calmat variables (G_vars): 
   call Calmat_inquire ( 'i', indi )
   if ( indi%Id == 0 ) stop 'variable "i" not found'
   ! and associate it with the (integer) pointer i_ptr:
   call Calmat_getPointer ( var = indi, ptr = i_ptr )

   ! get the index of the variable "Xi" in the list of calmat variables (G_vars): 
   call Calmat_inquire ( 'Xi', indx )
   if ( indx%Id == 0 ) stop 'variable "Xi" not found'

   ! Parse the expression to evaluate:
   call calmat ( exprIn = 'Xi = (4*(-1)^i)/(2*i+1);', parseOnly = fId )

   ! the loop:
   s = 0.0_Rkind
   do i = 0, n
      ! set the calmat variable 'i' (i.e. G_vars(indi%Id)) to the new value of i:
      i_ptr = i
      ! evaluate Xi:
      call calmat ( evaluate = fId )
      ! get a pointer to the value of Xi and add this value into s:
      call calmat_getPointer ( var = indx, ptr = x_ptr ) ! cannot be done outside the loop
                                                         ! since the variable is reallocated
      s = s + x_ptr      
   end do
   ! display the result:
   print '(/,a,g0)','s-pi = ',s-4.0_Rkind*atan(1.0_Rkind)
   
   ! delete the flow and nullify the pointers
   call calmat_delFlows ( fId )
   nullify(i_ptr,x_ptr)

   call cpu_time(t2)
   print*,'cpu time for the method 2-bis: ',t2-t1
           
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! 3) Calling calmat only once (using a for-loop in the expression):
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call calmat_resetAll() ! first reset the previous setup and the list of variables
   
   call cpu_time(t1)
   
   expr = 's = 0 ;'                                 // &
          'for i = 0 :' // util_intToChar(n) // ';' // &
          '    s = s + (4*(-1)^i)/(2*i+1) ;'        // &
          'end for ;'                               // &
          'disp(s-%pi,"s-pi = ")'
   ! (note 1: util_intToChar converts an integer to a string)
   ! (note 2: we can also put the expression in a file and use the fileIn argument of calmat)
  
   call calmat ( exprIn = expr, welcome = No )
   
   call cpu_time(t2)
   print*,'cpu time for the method 3: ',t2-t1
       
   print '(/,a)','Terminated'
   
end program sample6_evaluateAsum
