!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_setmat.inc" for the module bk2. Common lines for subroutines:
!
!      . bk2_SetMatIk2I    . bk2_SetMatRk2I     . bk2_SetMatCk2I
!                          . bk2_SetMatRk2R     . bk2_SetMatCk2R
!                                               . bk2_SetMatCk2C
! 
!----------------------------------------------------------------------------R.H. 09/18, 03/19

   err = 0
!   
!- case 3: (type A(1:n,1:m) =  matrix(1,1))
!   
   if ( present(n) .and. present(m) ) then  
      if ( size(matrix) /= 1 ) then
         res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
         call opflag%Set ( UERROR, HERE, &      
                  'Invalid data (if data = {matrix, n, m} --> size(matrix)=1) (case #3.1)' )
         return
      endif
      
      if ( cumul .and. allocated(res%v) ) then
         if ( n == res%nrow .and. m == res%ncol ) then
            res%v = res%v + matrix(1,1)
         else
            call opflag%Set ( UERROR, HERE, 'Incompatible shape (case #3.2)' )  
            return    
         end if   
      else
         if ( allocated(res%v) ) then
            if ( res%nrow /= n .or. res%ncol /= m ) then
               deallocate(res%v)
               allocate(res%v(n,m), stat = err)
            end if
         else
            allocate(res%v(n,m), stat = err)
         end if
         
         !if ( allocated(res%v) ) deallocate(res%v) ; allocate(res%v(n,m), stat = err)
         
         if ( err == 0 ) then
            res%nrow = n ; res%ncol = m ; res%typ = typ
            res%v = matrix(1,1)
         end if
      end if   
!
!- case 2: (type A = matrix)
!   
   else   
      nn = size(matrix,dim=1) ; mm = size(matrix,dim=2)
      
      if ( cumul .and. allocated(res%v) ) then
         if ( nn == res%nrow .and. mm == res%ncol ) then
            res%v = res%v + matrix
         else
            call opflag%Set ( UERROR, HERE, 'Incompatible shape (case #2)' )   
            return   
         end if                    
      else
         if ( allocated(res%v) ) then
            if ( res%nrow /= nn .or. res%ncol /= mm ) then
               deallocate(res%v)
               allocate(res%v(nn,mm), stat = err)
            end if
         else
            allocate(res%v(nn,mm), stat = err)
         end if
               
         !if (allocated(res%v)) deallocate(res%v) ; allocate(res%v(nn,mm), stat = err)
         
         if (err == 0) then
            res%nrow = nn ; res%ncol = mm ; res%typ = typ
            res%v = matrix
         end if   
      end if 
   end if           
   
   if (err /= 0) then
      res%typ = EMPTY ; res%nrow = 0 ; res%ncol = 0
      call opflag%Set ( IERROR, HERE, 'Allocation failure' )   
      return   
   end if
!---------------------------------------------------------------------------------------------         
