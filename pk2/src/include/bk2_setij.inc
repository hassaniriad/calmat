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
   if (present(indj)) then

      if ( minval(indi) <= 0 .or. minval(indj) <= 0 ) then
         call opflag%Set ( UERROR, HERE, 'Non-positive index not allowed' ) 
         return        
      end if
   
      nn = maxval(indi) ; mm = maxval(indj) ; ni = size(indi) ; mi = size(indj)
      na = size(matrix,dim=1) ; ma = size(matrix,dim=2) ; is_scalar = (na*ma == 1)
      
      if (.not. is_scalar .and. (ni /= na .or. mi /= ma)) then
         call opflag%Set ( UERROR, HERE, 'Incompatible shapes (case #5)' )
         return
      end if   
      
      if ( allocated(res%v) ) then
         if ( nn > res%nrow .or. mm > res%ncol ) then ! check if "res" needs to be resized
            nn = max(nn,res%nrow) ; mm = max(mm,res%ncol)
            call res%Resize ( 's', nn, mm)
            error_TraceNreturn(opflag, HERE)
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
            r = matrix(1,1)
            if (cumul) then
               do j = 1, mi ; do i = 1, ni 
                  res%v(indi(i),indj(j)) = res%v(indi(i),indj(j)) + r 
               end do ; end do
            else         
               do j = 1, mi ;  do i = 1, ni
                  res%v(indi(i),indj(j)) = r 
               end do ; end do
            end if
         else
            !res%v(indi,indj) = matrix ! same rem.
            if (cumul) then
               do j = 1, mi ;  do i = 1, ni
                  res%v(indi(i),indj(j)) = res%v(indi(i),indj(j)) + matrix(i,j)
               end do ; end do 
            else
               do j = 1, mi ; do i = 1, ni
                  res%v(indi(i),indj(j)) = matrix(i,j)
               end do ; end do                                
            end if
         end if
      end if
!
!- case 4: (type A(indi) = matrix --> "matrix" must be an n-vector)
!          
   else 
      
      if ( minval(indi) <= 0 ) then
         call opflag%Set ( UERROR, HERE, 'Non-positive index not allowed' ) 
         return        
      end if
      
      na = size(matrix,dim=1) ; ma = size(matrix,dim=2)  
      
      if (na /= 1 .and. ma /= 1) then
         call opflag%Set( UERROR, HERE, &
                          'Incompatible shapes (case #4.0): the rhs must be a vector' ) 
         return        
      end if
       
      sizea = na*ma ; is_scalar = (sizea == 1) ; nn = maxval(indi) ; ni = size(indi)
      
      if (.not. is_scalar .and. sizea /= ni) then
         call opflag%Set ( UERROR, HERE, 'Incompatible shapes (case #4.1)' )
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
               r = matrix(1,1)
               do i = 1, ni ; res%v(indi(i),1) = r ; end do
            end if   
         else if (na == ni .and. ma == 1) then
            allocate(res%v(nn,1), source = zero, stat = err)
            if (err == 0) then
               res%nrow = nn ; res%ncol = 1 ; res%typ = typ
               !res%v(indi,1) = matrix(:,1)
               do i = 1, ni ;  res%v(indi(i),1) = matrix(i,1) ; end do
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
         error_TraceNreturn(opflag, HERE)

         if (is_scalar) then
            !res%v(1,indi) = matrix(1,1) 
            r = matrix(1,1)
            if (cumul) then       
               do i = 1, ni ; res%v(1,indi(i)) = res%v(1,indi(i)) + r ; end do
            else   
               do i = 1, ni ; res%v(1,indi(i)) = r ; end do
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
         error_TraceNreturn(opflag, HERE)

         if (is_scalar) then
            r = matrix(1,1)
            if (cumul) then
               do i = 1, ni ; res%v(indi(i),1) = res%v(indi(i),1) + r ; end do 
            else     
               do i = 1, ni ; res%v(indi(i),1) = r ; end do   
            end if   
         else if (na == 1) then
            !res%v(indi,1) = matrix(1,:)
            if (cumul) then
               do i = 1, ni ; res%v(indi(i),1) = res%v(indi(i),1) + matrix(1,i) ; end do
            else   
               do i = 1, ni ; res%v(indi(i),1) = matrix(1,i) ; end do
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
            call opflag%Set ( UERROR, HERE, 'Incompatible shapes (case #4.2)' )
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
       
   end if     
   
   if (err /= 0) then
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set ( IERROR, HERE, 'Allocation failure' )
      return
   end if
!---------------------------------------------------------------------------------------------         
