!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
! 
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_copysubmat.inc" for the module bk2. Common lines for subroutines:
!      . bk2_CopySubmatIk2
!      . bk2_CopySubmatRk2
!      . bk2_CopySubmatCk2
!      . bk2_CopySubmatLk2
!---------------------------------------------------------------------------------------------
         if (present(indi) .and. present(indj)) then
            minind = minval(indi) ; maxind = maxval(indi)
      
            if (minind < 1) then
               write(emsg,'(a,i0,a)') "index '",minind,"' below lower bound of 1"                                     
               call opflag%Set(UERROR, HERE, emsg)    
               return  
            end if

            if (maxind > self%nrow) then
               write(emsg,'(a,i0,a,i0)') &
                        "index '",maxind,"' of dimension #1 above upper bound of ",self%nrow                                    
               call opflag%Set(UERROR, HERE, emsg)     
               return 
            end if

            minind = minval(indj) ; maxind = maxval(indj)
      
            if (minind < 1) then
               write(emsg,'(a,i0,a)') "index '",minind,"' below lower bound of 1"                                     
               call opflag%Set(UERROR, HERE, emsg)    
               return  
            end if

            if (maxind > self%ncol) then
               write(emsg,'(a,i0,a,i0)') &
                        "index '",maxind,"' of dimension #2 above upper bound of ",self%ncol                              
               call opflag%Set(UERROR, HERE, emsg) 
               return     
            end if

            ni = size(indi) ; nj = size(indj)

            !allocate(res%v(ni,nj), source = self%v(indi,indj), stat = err) ! argh ifort (overhead)!!
            
            allocate(res%v(ni,nj), stat = err)
                        
            if (err == 0) then
               res%nrow = ni ; res%ncol = nj ; res%typ = typ
               do j = 1, nj
                  k = indj(j)
                  do i = 1, ni
                     res%v(i,j) = self%v(indi(i),k)
                  end do
               end do
            else
               call opflag%set ( stat = IERROR, msg = "Allocation failure", where = HERE )
            end if   
      
         else if (present(indi) .or. present(indj)) then

            if (present(indi)) then
               ni = size(indi)
               minind = minval(indi)
               maxind = maxval(indi)
            else   
               ni = size(indj)
               minind = minval(indj) 
               maxind = maxval(indj)
            end if
      
            if (ni == 0) return
      
            if (minind < 1) then
               write(emsg,'(a,i0,a)') "index '",minind,"' below lower bound of 1"                                     
               call opflag%Set ( UERROR, HERE, emsg )  
               return    
            end if

            if (maxind > self%nrow * self%ncol) then
               write(emsg,'(a,i0,a,i0)') &
                          "index '",maxind,"' above upper bound of ",self%nrow*self%ncol                                     
               call opflag%Set ( UERROR, HERE, emsg )
               return    
            end if

            if (self%nrow == 1) then
               allocate(res%v(1,ni), stat = err)
            else
               allocate(res%v(ni,1), stat = err)
            end if   
                                    
            if (err /= 0) then
               call opflag%Set ( stat = IERROR, msg = "Allocation failure", where = HERE )
            else
               if (present(indi)) then
                  if (self%nrow == IONE) then
                     do k = 1, ni
                        indk = indi(k)
                        j = (indk-1) / (self%nrow) ; i = indk - j*self%nrow ; j = j + 1
                        res%v(1,k) = self%v(i,j)
                     end do
                     res%nrow = 1 ; res%ncol = ni ; res%typ = typ
                  else
                     do k = 1, ni
                        indk = indi(k)
                        j = (indk-1) / (self%nrow) ; i = indk - j*self%nrow ; j = j + 1
                        res%v(k,1) = self%v(i,j)
                     end do
                     res%nrow = ni ; res%ncol = 1 ; res%typ = typ
                  end if
               else   
                  if (self%nrow == IONE) then               
                     do k = 1, ni
                        indk = indj(k)
                        j = (indk-1) / (self%nrow) ; i = indk - j*self%nrow ; j = j + 1
                        res%v(1,k) = self%v(i,j)
                     end do
                     res%nrow = 1 ; res%ncol = ni ; res%typ = typ
                  else
                     do k = 1, ni
                        indk = indj(k)
                        j = (indk-1) / (self%nrow) ; i = indk - j*self%nrow ; j = j + 1
                        res%v(k,1) = self%v(i,j)
                     end do
                     res%nrow = ni ; res%ncol = 1 ; res%typ = typ
                  end if               
               end if
            end if
         end if
         
!---------------------------------------------------------------------------------------------         

