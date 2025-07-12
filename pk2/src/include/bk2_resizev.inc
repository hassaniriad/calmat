!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_resizev.inc" for the module bk2. Common lines for subroutines:
!      . bk2_ResizevIk2
!      . bk2_ResizevRk2
!      . bk2_ResizevCk2
!      . bk2_ResizevLk2
!---------------------------------------------------------------------------------------------
   if (mode == 'e' .or. self%typ == EMPTY) then
      self%nrow = n ; self%ncol = m ; self%typ = typ
      if (allocated(self%v)) deallocate(self%v)
      allocate(self%v(n,m), source = zero, stat = err)
      if (err /= 0) then
         call opflag%Set ( IERROR, HERE, 'Allocation failure for array "self%v"' )
         return
      end if  
      return            
   end if
      
   if (mode == 's') then
   
      allocate(tmp(n,m), source = zero, stat = err)
      
      if (err /= 0) then
         call opflag%Set ( IERROR, HERE, 'Allocation failure for array "tmp"' )
         return
      end if    

      nn = min(n,self%nrow) ; mm = min(m,self%ncol)                                               
      tmp(1:nn,1:mm) = self%v(1:nn,1:mm)
      
      call move_alloc (from = tmp, to = self%v)
      
      self%nrow = n ; self%ncol = m
   
   else
      call opflag%Set ( UERROR, HERE, &
                        "Invalid mode << "//mode//" >> (must be << e >> or << s >>)" ) 
      return           
   end if          
!---------------------------------------------------------------------------------------------         
