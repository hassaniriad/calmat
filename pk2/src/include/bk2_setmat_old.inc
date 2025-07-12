!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_setmat.inc" for the module bk2. Common lines for subroutines:
!      . bk2_SetMatIk2
!      . bk2_SetMatRk2
!      . bk2_SetMatCk2
!
! Modified (03/19): nagfor complains when in statements like res%v(indi,indj) = x a subscript
!                   vector "indi" (or "indj") has duplicate value. I modified them with loops.
! 
!----------------------------------------------------------------------------R.H. 09/18, 03/19

   err = 0
!
!- case 5: (type A(indi,indj) = matrix)
!   
   if (present(matrix) .and. present(indi) .and. present(indj)) then
      nn = maxval(indi) ; mm = maxval(indj) ; ni = size(indi) ; mi = size(indj)
      na = size(matrix,dim=1) ; ma = size(matrix,dim=2) ; is_scalar = (na*ma == 1)
      
      if (.not. is_scalar .and. (ni /= na .or. mi /= ma)) then
         opflag = err_t ( stat = UERROR, abort = ErrAbort, &
                           msg = HERE//' Incompatible shapes (case #5)' )         
         return
      end if   
      
      if ( allocated(res%v) ) then
         if ( nn > res%nrow .or. mm > res%ncol ) then ! check if "res" needs to be resized
            nn = max(nn,res%nrow) ; mm = max(mm,res%ncol)
            call res%Resize ( 's', nn, mm)
            if (opflag%code > 0) return            
         else
            nn = res%nrow ; mm = res%ncol   
         end if   
      else 
         allocate(res%v(nn,mm), source = zero, stat = err)
      end if   
      
      if (err == 0) then            
         res%nrow = nn ; res%ncol = mm ; res%typ = typ
         if (is_scalar) then
            !res%v(indi,indj) = matrix(1,1) ! nagfor complains when subscript indi (or indj)
                                            ! has duplicate value. I use do loops:
            if (cumul) then
               do j = 1, mi
                  do i = 1, ni
                     res%v(indi(i),indj(j)) = res%v(indi(i),indj(j)) + matrix(1,1)
                  end do
               end do
            else         
               do j = 1, mi
                  do i = 1, ni
                     res%v(indi(i),indj(j)) = matrix(1,1)
                  end do
               end do      
            end if
         else
            !res%v(indi,indj) = matrix ! same
            if (cumul) then
               do j = 1, mi
                  do i = 1, ni
                     res%v(indi(i),indj(j)) = res%v(indi(i),indj(j)) + matrix(i,j)
                  end do
               end do 
            else
               do j = 1, mi
                  do i = 1, ni
                     res%v(indi(i),indj(j)) = matrix(i,j)
                  end do
               end do                                
            end if
         end if
      end if
!
!- case 4: (type A(indi) = matrix --> "matrix" must be an n-vector)
!          
   else if (present(matrix) .and. present(indi)) then
      na = size(matrix,dim=1) ; ma = size(matrix,dim=2)  
      
      if (na /= 1 .and. ma /= 1) then
         opflag = err_t ( stat = UERROR, abort = ErrAbort, msg = HERE // &
                         ' Incompatible shapes (case #4.0): the rhs must be a vector' )         
         return         
      end if
       
      sizea = na*ma ; is_scalar = (sizea == 1) ; nn = maxval(indi) ; ni = size(indi)
      
      if (.not. is_scalar .and. sizea /= ni) then
         opflag = err_t ( stat = UERROR, abort = ErrAbort, &
                           msg = HERE//' Incompatible shapes (case #4.1)' )         
         return         
      end if
      
      if (.not. allocated(res%v)) then 
!
!-       "res" is empty on input. The resulting matrix will be a nn x 1 or 1 x nn matrix:
!
         if (is_scalar) then
            allocate(res%v(nn,1), source = zero, stat = err)
            if (err == 0) then
               res%nrow = nn ; res%ncol = 1 ; res%typ = typ
               !res%v(indi,1) = matrix(1,1)
               do i = 1, ni ; res%v(indi(i),1) = matrix(1,1) ; end do
            end if   
         else if (na == ni .and. ma == 1) then
            allocate(res%v(nn,1), source = zero, stat = err)
            if (err == 0) then
               res%nrow = nn ; res%ncol = 1 ; res%typ = typ
               !res%v(indi,1) = matrix(:,1)
               do i = 1, ni ; res%v(indi(i),1) = matrix(i,1) ; end do
            end if   
         else if (ma == ni .and. na == 1) then
            allocate(res%v(1,nn), source = zero, stat = err)
            if (err == 0) then
               res%nrow = 1 ; res%ncol = nn ; res%typ = typ
               !res%v(1,indi) = matrix(1,:)
               do i = 1, ni ; res%v(1,indi(i)) = matrix(1,i) ; end do
            end if   
         end if
      
      else if (res%nrow == 1) then 
!
!-       "res" is a 1 x ? matrix. The result will be a 1 x nn matrix (resize it if necessary):
!
         if (res%ncol < nn) call res%Resize ('s', IONE, nn )
         if (opflag%code > 0) return         

         if (is_scalar) then
            !res%v(1,indi) = matrix(1,1)  
            if (cumul) then       
               do i = 1, ni ; res%v(1,indi(i)) = res%v(1,indi(i)) + matrix(1,1) ; end do
            else   
               do i = 1, ni ; res%v(1,indi(i)) = matrix(1,1) ; end do
            end if   
         else if (na == 1) then
            !res%v(1,indi) = matrix(1,:)
            if (cumul) then
               do i = 1, ni ; res%v(1,indi(i)) = res%v(1,indi(i)) + matrix(1,i) ; end do
            else   
               do i = 1, ni ; res%v(1,indi(i)) = matrix(1,i) ; end do
            end if   
         else 
            !res%v(1,indi) = matrix(:,1)
            if (cumul) then
               do i = 1, ni ; res%v(1,indi(i)) = res%v(1,indi(i)) + matrix(i,1) ; end do
            else   
               do i = 1, ni ; res%v(1,indi(i)) = matrix(i,1) ; end do
            end if   
         end if
      
      else if (res%ncol == 1) then 
!
!-       "res" is a ? x 1 matrix. The result will be a 1 x nn matrix (resize it if necessary):

         if (res%nrow < nn) call res%Resize ( 's', nn, IONE )
         if (opflag%code > 0) return         

         if (is_scalar) then
            if (cumul) then
               do i = 1, ni ; res%v(indi(i),1) = res%v(indi(i),1) + matrix(1,1) ; end do 
            else     
               do i = 1, ni ; res%v(indi(i),1) = matrix(1,1) ; end do   
            end if   
         else if (na == 1) then
            !res%v(indi,1) = matrix(1,:)
            if (cumul) then
               do i = 1, ni ; res%v(indi(i),1) = res%v(indi(i),1) + matrix(1,i); end do 
            else   
               do i = 1, ni ; res%v(indi(i),1) = matrix(1,i); end do 
            end if   
         else
            !res%v(indi,1) = matrix(:,1)
            if (cumul) then
               do i = 1, ni ; res%v(indi(i),1) = res%v(indi(i),1) + matrix(i,1) ; end do 
            else   
               do i = 1, ni ; res%v(indi(i),1) = matrix(i,1) ; end do
            end if    
         end if
         
      else  
!      
!-       "res" is n x m, copy the value of "matrix" into the elements of res%v
!        located at their natural positions (column-major ordering) given in "indi":
                          
         if (res%nrow * res%ncol < nn) then
            opflag = err_t ( stat = UERROR, abort = ErrAbort, &
                              msg = HERE//' Incompatible shapes (case #4.2)' )
            return
         end if                          

         if (cumul) then
            do k = 1, ni
               kg = indi(k)
               i = mod(kg,res%nrow) ; if (i == 0) i = res%nrow ; j = (kg - i)/res%nrow + 1
               if (is_scalar) then
                  res%v(i,j) = res%v(i,j) + matrix(1,1)
               else if (na == 1) then
                  res%v(i,j) = res%v(i,j) + matrix(1,k)
               else
                  res%v(i,j) = res%v(i,j) + matrix(k,1)
               end if
            end do
         else
            do k = 1, ni
               kg = indi(k)
               i = mod(kg,res%nrow) ; if (i == 0) i = res%nrow ; j = (kg - i)/res%nrow + 1
               if (is_scalar) then
                  res%v(i,j) = matrix(1,1)
               else if (na == 1) then
                  res%v(i,j) = matrix(1,k)
               else
                  res%v(i,j) = matrix(k,1)
               end if
            end do         
         end if
                     
      end if        
!
!- case 3: (type A(1:n,1:m) =  matrix(1,1))
!   
   else if (present(matrix) .and. present(n) .and. present(m)) then  
      if (size(matrix) /= 1) then
         res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
         opflag = err_t ( stat = UERROR, abort = ErrAbort, msg = HERE // &
                  ' Invalid data (if data = {matrix, n, m} --> size(matrix)=1) (case #3.1)' )
         return
      endif
      
      if (cumul .and. allocated(res%v)) then
         if (n == res%nrow .and. m == res%ncol) then
            res%v = res%v + matrix(1,1)
         else
            opflag = err_t ( stat = UERROR, abort = ErrAbort, msg = HERE // &
                             ' Incompatible shape (case #3.2)' )
            return
         end if   
      else
         if (allocated(res%v)) deallocate(res%v)
         allocate(res%v(n,m), source = matrix(1,1), stat = err)
         if (err == 0) then
            res%nrow = n ; res%ncol = m ; res%typ = typ
         end if
      end if   
!
!- case 2: (type A = matrix)
!   
   else if (present(matrix)) then   
      nn = size(matrix,dim=1) ; mm = size(matrix,dim=2)
      
      if (cumul .and. allocated(res%v)) then
         if (nn == res%nrow .and. mm == res%ncol) then
            res%v = res%v + matrix
         else
            opflag = err_t ( stat = UERROR, abort = ErrAbort, msg = HERE // &
                             ' Incompatible shape (case #2)' )
            return
         end if                    
      else
         if (allocated(res%v)) deallocate(res%v)
         allocate(res%v(nn,mm), source = matrix, stat = err)
         if (err == 0) then
            res%nrow = nn ; res%ncol = mm ; res%typ = typ
         end if   
      end if         
!
!- case 1: (type A(1:n,1:m) = 0)
!         
   else if (present(n) .and. present(m)) then
      if (allocated(res%v)) deallocate(res%v)
      allocate(res%v(n,m), source = zero, stat = err)
      if (err == 0) then
         res%nrow = n ; res%ncol = m ; res%typ = typ
      end if   
      
   else
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      opflag = err_t (stat = UERROR, abort = ErrAbort, msg = HERE//' Invalid data (case #1)')
      return      
   end if     
   
   if (err /= 0) then
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      opflag = err_t ( stat = IERROR, abort = ErrAbort, &
                        msg = HERE // ' Allocation failure for << res >>' )      
   end if
!---------------------------------------------------------------------------------------------         
