!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "util_allock2.inc" for the module util_Other. Common lines for subroutines:
!      . util_allocIk2
!      . util_allocRk2
!      . util_allocCk2
!      . util_allocLk2
!---------------------------------------------------------------------------------------------
    
   err = 0
        
   if (.not. allocated(t)) then

      allocate(t(n,m), source=constant, stat=err)
      if (err /= 0) msg = "Allocation failure for array 't' [1]"        

   else if (mode == 'a') then

      if (size(t,dim=1) /= n .or. size(t,dim=2) /= m) then 
         deallocate(t)
         allocate(t(n,m), source=constant, stat=err)      
         if (err /= 0) msg = "Allocation failure for array 't' [2]"   
      else
         return     
      end if       
      
   else if (mode == 'e') then

      if (size(t,dim=1) /= n .or. size(t,dim=2) /= m) then 
         deallocate(t)
         allocate(t(n,m), source=constant, stat=err)      
         if (err /= 0) msg = "Allocation failure for array 't' [3]"        
      else
         t(:,:) = constant ; return
      end if       
      
   else if (mode == 's') then
   
      allocate(tmp(n,m), source=constant, stat=err)
      if (err /= 0) then
         msg = "Allocation failure for array 'tmp'"       
      else
         nn = min(n,size(t,1,kind=Ikind))
         mm = min(m,size(t,2,kind=Ikind))                                               
         tmp(1:nn,1:mm) = t(1:nn,1:mm)
         call move_alloc (from = tmp, to = t)
      end if

   else if (mode == 'i') then
      if (size(t,1) < n .or. size(t,2) < m) then
         deallocate(t)
         allocate(t(n,m), source=constant, stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [4]"
      else
         return
      end if  

   else if (mode == 'z') then
      if (size(t,1) < n .or. size(t,2) < m) then
         deallocate(t)
         allocate(t(n,m), source=constant, stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [5]"
      else 
         t(:,:) = constant ; return
      end if     
   else
      call stat%Set ( UERROR, HERE,                                                       &
                      "Invalid mode '" // mode // "' (must be 'a', 'e', 's', 'i' or 'z')" )
      return
   end if 
   
   if (err /= 0) then      
      call stat%Set(IERROR, HERE, msg)
      return
   end if
   
   if ( present(reallocated) ) reallocated = .true.         