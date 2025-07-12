!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "util_allock1.inc" for the module util_Other. Common lines for subroutines:
!      . util_allocIk1
!      . util_allocRk1
!      . util_allocCk1
!      . util_allocLk1
!---------------------------------------------------------------------------------------------
   
   err = 0
   
   if (.not. allocated(t)) then

      allocate(t(n), source=constant, stat=err)
      if (err /= 0) msg = "Allocation failure for array 't' [1]"        

   else if (mode == 'a') then

      if (size(t) /= n) then
         deallocate(t)
         allocate(t(n), source=constant, stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [2]"  
      else
         return
      end if      
      
   else if (mode == 'e') then

      if (size(t) /= n) then
         deallocate(t)
         allocate(t(n), source=constant, stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [3]"  
      else
         t(:) = constant ; return
      end if      
           
   else if (mode == 's') then                      
   
      allocate(tmp(n), source=constant, stat=err)
      if (err /= 0) then
         msg = "Allocation failure for array 'tmp'"       
      else   
         nn = min(n,size(t,kind=Ikind))
         tmp(1:nn) = t(1:nn)
         call move_alloc (from = tmp, to = t)
      end if
      
   else if (mode == 'i') then
   
      if (size(t) < n) then
         deallocate(t)
         allocate(t(n), source=constant, stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [4]"
      else
         return
      end if  

   else if (mode == 'z') then
      if (size(t) < n) then
         deallocate(t)
         allocate(t(n), source=constant, stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [5]"
      else 
         t(:) = constant ; return
      end if  
            
   else
      call stat%Set ( UERROR, HERE,                                                      &
                     "Invalid mode '" // mode // "' (must be 'a', 'e', 's', 'i' or 'z')" )
      return
   end if    
   
   if (err /= 0) then   
      call stat%Set ( IERROR, HERE, msg )
      return
   end if      
   
   if ( present(reallocated) ) reallocated = .true.