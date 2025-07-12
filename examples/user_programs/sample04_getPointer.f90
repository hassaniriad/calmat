program sample4_getPointer
!
! Example showing how to associate a pointer to a calmat variable
!
!-----------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
   complex(Rkind), pointer :: zmat_ptr(:,:) ! we know that our result is a complex matrix
!-----------------------------------------------------------------------------------------------

   call calmat ( exprIn = 'exp(-%i*magic(3)) \ [1:3;4:6;7:9]' )
   
   ! Pointer association:
   call calmat_getPointer ( var = 'ans', ptr = zmat_ptr )

   ! Do what you have to do with the result (but make sure you don't resize it):
   call myproc ( zmat_ptr ) 
   
   ! A new expression involving the modified values:
   call calmat ( exprIn='ans+eye(3,3)*%i' )

   ! Don't forget to nullify the pointer when it's no longer needed:
   nullify(zmat_ptr)
   
   !...
   
contains
   
   subroutine myproc ( z )
   complex(Rkind), intent(in out) :: z(3,3)
   !...
   z = abs(z)
   end subroutine myproc
   
end program sample4_getPointer
