!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "pk2_extracsubmat.inc" for the module pk2. Common lines for subroutines:
!      . pk2_i32ExtracSubMat
!      . pk2_i64ExtracSubMat
!---------------------------------------------------------------------------------------------

   if (a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0) then
      call opflag%set ( stat=WARNING, where=HERE, msg='<< a >> is empty (--> self = [ ])' )
      if (present(stat)) stat = opflag
      return
   end if   
                    
   ni = size(indx) 
        
   if (ni == 0) then
      call opflag%set ( stat=WARNING, where=HERE, msg='<< Indx >> is empty (--> self = [ ])' ) 
      if (present(stat)) stat = opflag      
      return
   end if   
            
   if (present(jndx)) then  
!
!-    cas du type A(indx,jndx)
!
      nj = size(jndx)
      if (nj == 0) then
         call opflag%set(stat=WARNING, where=HERE, msg='<< Jndx >> is empty (--> self = [ ])')
         if (present(stat)) stat = opflag
         return
      end if   
      
      if (ni == 1 .and. indx(1) == -1) then 

         if (nj == 1 .and. jndx(1) == -1) then 
!
!-          cas du type A(:,:)         
!            
            call pk2_Assign (self,a)
            
            if (opflag%code > 0) then
               call opflag%addTrace(HERE)
               if (present(stat)) stat = opflag
               return
            end if
         
         else  
!
!-          cas du type A(:,jndx)
!
            if (minval(jndx) < 1) then
               write(emsg,'(a,i0,a)') &
                          "index '",minval(jndx),"' of dimension #2 below lower bound of 1"                                     
               call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
               if (present(stat)) stat = opflag
               return
            end if
                  
            if (maxval(jndx) > a%ncol) then
               write(emsg,'(a,i0,a,i0)') &
                     "index '",maxval(jndx),"' of dimension #2 above upper bound of ",a%ncol                                     
               call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
               if (present(stat)) stat = opflag
               return
            end if
            
            allocate(tmp(a%nrow), stat = err)
            if (err /= 0) then
               call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure' )
               if (present(stat)) stat = opflag
               return
            end if
            
            do i = 1, a%nrow  
               tmp(i) = i
            end do   

            call a%m%CopySubmat ( self%m, indi = tmp, indj = int(jndx,Ikind) )
            
            if (opflag%code > 0) then
               call opflag%addTrace(HERE)
               if (present(stat)) stat = opflag
               return
            end if
            
            self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol                                               
         end if         
         
      else
        
         if (nj == 1 .and. jndx(1) == -1) then 
!
!-          cas du type A(indx,:)
!                  
            if (minval(indx) < 1) then
               write(emsg,'(a,i0,a)') &
                          "index '",minval(indx),"' of dimension #1 below lower bound of 1"                                     
               call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
               if (present(stat)) stat = opflag
               return
            end if
            if (maxval(indx) > a%nrow) then
               write(emsg,'(a,i0,a,i0)') &
                      "index '",maxval(indx),"' of dimension #1 above upper bound of ",a%nrow                                     
               call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
               if (present(stat)) stat = opflag
               return
            end if
                              
            allocate(tmp(a%ncol), stat = err)
            if (err /= 0) then
               call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure' )
               if (present(stat)) stat = opflag
               return
            end if
            
            do i = 1, a%ncol
               tmp(i) = i
            end do   

            call a%m%CopySubmat ( self%m, indi = int(indx,Ikind), indj = tmp )
            
            if (opflag%code > 0) then
               call opflag%AddTrace(HERE)
               if (present(stat)) stat = opflag
               return
            end if
            
            self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
               
         else
!
!-          cas du type A(indx,jndx)  
!
            if (minval(jndx) < 1) then
               write(emsg,'(a,i0,a)') &
                          "index '",minval(jndx),"' of dimension #2 below lower bound of 1"                                     
               call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
               if (present(stat)) stat = opflag
               return
            end if
            if (maxval(jndx) > a%ncol) then
               write(emsg,'(a,i0,a,i0)') &
                      "index '",maxval(jndx),"' of dimension #2 above upper bound of ",a%ncol                                     
               call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
               if (present(stat)) stat = opflag
               return
            end if

            if (minval(indx) < 1) then
               write(emsg,'(a,i0,a)') &
                          "index '",minval(indx),"' of dimension #1 below lower bound of 1"                                     
               call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
               if (present(stat)) stat = opflag
               return
            end if
            if (maxval(indx) > a%nrow) then
               write(emsg,'(a,i0,a,i0)') &
                      "index '",maxval(indx),"' of dimension #1 above upper bound of ",a%nrow                                     
               call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
               if (present(stat)) stat = opflag
               return
            end if

            call a%m%CopySubmat ( self%m, indi = int(indx,Ikind), indj = int(jndx,Ikind) )
            
            if (opflag%code > 0) then
               call opflag%AddTrace(HERE)
               if (present(stat)) stat = opflag
               return
            end if

            self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
            !if (allocated(a%name)) self%name = 'copy of a subarray of "'//trim(a%name)//'"'              
               
         end if
      end if     
                                  
   else   
!
!-    cas du type A(indx)
!        
      na = a%nrow * a%ncol
                   
      if (ni == 1 .and. indx(1) == -1) then 
!
!-       cas du type A(:)
!
         allocate(tmp(na), stat = err)
         if (err /= 0) then
            call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure' )
            if (present(stat)) stat = opflag
            return
         end if

         do i = 1, na
            tmp(i) = i
         end do

         call a%m%CopySubmat ( self%m, indi = tmp )

         if (opflag%code > 0) then
            call opflag%AddTrace(HERE)
            if (present(stat)) stat = opflag
            return
         end if
            
         self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol

      else  
!         
!-       cas du type A(indx)
!
         if (minval(indx) < 1) then
            write(emsg,'(a,i0,a)') "index '",minval(indx),"' below lower bound of 1"                                     
            call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
            if (present(stat)) stat = opflag
            return
         end if

         if (maxval(indx) > na) then
            write(emsg,'(a,i0,a,i0)') &
                       "index '",maxval(indx),"' of dimension #1 above upper bound of ",na                                     
            call opflag%set ( stat = UERROR, where = HERE, msg = emsg )
            if (present(stat)) stat = opflag
            return
         end if

         call a%m%CopySubmat ( self%m, indi = int(indx,Ikind) )
         
         if (opflag%code > 0) then
            call opflag%AddTrace(HERE)
            if (present(stat)) stat = opflag
            return
         end if

         self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
            
      end if   
         
   end if
!---------------------------------------------------------------------------------------------         
