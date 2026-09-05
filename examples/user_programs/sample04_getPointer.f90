program sample4_getPointer
!
! Example showing how to associate a pointer to a calmat variable
!
!---------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
!---------------------------------------------------------------------------------------------

   call example1
   
   call example2
      
contains

   subroutine example1
   ! In this example we associate a pointer of rank 2 to a calmat variable
   ! This variable is modified through the pointer and is involved in a new calmat expression

   complex(Rkind), pointer :: zmat_ptr(:,:)! we know that our result will be a complex matrix

   ! A calmat expression (the result is in the variable "ans"):
   call calmat ( exprIn = 'exp(-%i*magic(3)) \ [1:3;4:6;7:9]' )
   
   ! pointer association (pointer of rank 2):
   call calmat_getPointer ( var = 'ans', ptr = zmat_ptr )

   ! Let's do what needs to be done with this result (but let's be careful not to resize it):
   call myproc_example1 ( zmat_ptr ) 
   
   ! A new expression involving the modified values:
   call calmat ( exprIn='ans+eye(3,3)*%i' )

   ! Don't forget to nullify the pointer when it's no longer needed:
   nullify(zmat_ptr)
   
   !...
   end subroutine example1
      
   subroutine myproc_example1 ( z )
   complex(Rkind), intent(in out) :: z(3,3)
   !...
   z = abs(z)
   end subroutine myproc_example1


   subroutine example2
   ! In this example we associate a pointer of rank 1 to a calmat variable
   integer(Ikind), pointer :: ivect_ptr(:)! we know that our result will be an integer matrix

   ! A calmat expression (the result is in the variable "ans"):
   call calmat ( exprIn = '[-1,2,3; 4,-5,6; 7,8,-9]' )

   ! pointer association (pointer of rank 2):
   call calmat_getPointer ( var = 'ans', ptr = ivect_ptr )

   ! Let's do what needs to be done with this result (but let's be careful not to resize it):
   call myproc_example2 ( ivect_ptr ) 

   ! A new expression involving the modified values:
   call calmat ( exprIn='ans+eye(3,3)' )

   ! Don't forget to nullify the pointer when it's no longer needed:
   nullify(ivect_ptr)
   
   !...
   end subroutine example2
   
   subroutine myproc_example2 ( i )
   integer(Ikind), intent(in out) :: i(9)
   !...
   i(1) = 10 ; i(5)=50 ; i(9)=90
   end subroutine myproc_example2
   
end program sample4_getPointer
