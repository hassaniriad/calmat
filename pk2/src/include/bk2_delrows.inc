!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
! 
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_delrows.inc" for the module bk2. Common lines for subroutines:
!      . bk2_DelRowsIk2
!      . bk2_DelRowsRk2
!      . bk2_DelRowsCk2
!      . bk2_DelRowsLk2
!---------------------------------------------------------------------------------------------
   nkeep = 0
   
   select case (opt)

      case ('r')  
!
!-       Delete the rows of self%v whose #s are given in indx 
!                     
         do i = 1, self%nrow
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1 ; lkeep(nkeep) = i
         end do   

         if (nkeep == 0) then
            call self%Destroy () ; return
         end if
            
         allocate(tmp(nkeep,self%ncol), stat = err)
         if (err /= 0) then
            call opflag%Set ( IERROR, HERE, 'Allocation failure for array "tmp"' )   
            return   
         end if   
         
         tmp(1:nkeep,:) = self%v(lkeep(1:nkeep),:)
         
         self%nrow = nkeep
           
      case ('c')
!
!-       Delete the columns of self%v whose #s are given in indx 
!          
         do i = 1, self%ncol
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1 ; lkeep(nkeep) = i   
         end do   
         if (nkeep == 0) then
            call self%Destroy ; return
         end if
               
         allocate(tmp(self%nrow,nkeep), stat = err)
         if (err /= 0) then
            call opflag%Set ( IERROR, HERE, 'Allocation failure for array "tmp"' )  
            return    
         end if  
         
         tmp(:,1:nkeep) = self%v(:,lkeep(1:nkeep))
         
         self%ncol = nkeep

      case ('e') 
!
!-       Delete the elements of self%v whose #s (in column-major ord.) are given in indx 
!      
         nm = self%nrow * self%ncol   
         self%v = reshape(self%v,[nm,IONE])
            
         do i = 1, nm
            if ( any(indx == i) ) cycle
            nkeep = nkeep + 1
            lkeep(nkeep) = i
         end do   
         
         if (nkeep == 0) then
            call self%Destroy ; return
         end if
               
         allocate(tmp(nkeep,1), stat = err)
         if (err /= 0) then
            call opflag%Set ( IERROR, HERE, 'Allocation failure for array "tmp"' )  
            return    
         end if  
         
         tmp(1:nkeep,1) = self%v(lkeep(1:nkeep),1)
         
         self%nrow = nkeep ; self%ncol = 1
               
      case default
         call opflag%Set ( UERROR, HERE, &      
                           "Unknown option opt = "//opt//" (must be 'r', 'c' or 'e')" )
         return         
   end select
     
   call move_alloc (from = tmp, to = self%v)
!---------------------------------------------------------------------------------------------         
   
