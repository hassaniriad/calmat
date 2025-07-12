!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_insertmat.inc" for the module bk2. Common lines for subroutines:
!      . bk2_InsertIinIk2
!      . bk2_InsertIinRk2
!      . bk2_InsertIinCk2
!      . bk2_InsertRinRk2
!      . bk2_InsertRinCk2
!      . bk2_InsertCinCk2
!      . bk2_InsertLinLk2
!      . bk2_InsertSinSk2
!---------------------------------------------------------------------------------------------

   na = a%nrow ; ni = size(matrix,dim=1) 
   ma = a%ncol ; mi = size(matrix,dim=2) 

   if (a%typ == EMPTY .or. na*ma == 0) then
      if (allocated(a%v)) deallocate(a%v)
      allocate(a%v(ni,mi), stat = err)
      if (err /= 0) then
         call opflag%Set ( IERROR, HERE, 'Allocation failure for array "a%v"' )      
         return
      end if
      a%v = matrix
      a%nrow = ni ; a%ncol = mi ; a%typ = typ
      return
   end if      

   if (cr == 'r') then

      if (pnum < 1 .or. pnum > na+1) then
         call opflag%Set ( UERROR, HERE, "Row # outside of expected range" )
         return
      end if
!
!-    Insert the ni rows of "matrix" into self%v starting at the row # "pnum":
!     (note: if mi < ma, each inserted row is completed by 0, if mi > ma, elements beyond
!      ma are ignored)
!
      allocate(tmp(na+ni,ma), source = zero, stat = err)
      if (err /= 0) then
         call opflag%Set ( IERROR, HERE, 'Allocation failure for array "tmp"' )  
         return    
      end if
      
      minai = min(ma,mi)
      p1 =      1 ; p2 = pnum - 1  ; tmp(p1:p2, 1:ma   ) = a%v   (  p1:p2, 1:ma   )
      p1 = p2 + 1 ; p2 = p2 + ni   ; tmp(p1:p2, 1:minai) = matrix(   1:ni, 1:minai)
      p1 = p2 + 1 ; p2 = na + ni   ; tmp(p1:p2, 1:ma   ) = a%v   (pnum:na, 1:ma   )
      
      call move_alloc (from = tmp, to = a%v)
      
      a%nrow = na + ni
   
   else if (cr == 'c') then

      if (pnum < 1 .or. pnum > ma+1) then
         call opflag%Set ( UERROR, HERE, "Column # outside expected range" )
         return
      end if   
!
!-    Insert the mi columns of "matx" into self%v starting at the column # "pnum":
!     (note: if ni < na, each inserted column is completed by 0, if ni > na, elements beyond
!      na are ignored)
      
      allocate(tmp(na,ma+mi), source = zero, stat = err)
      if (err /= 0) then
         call opflag%Set ( IERROR, HERE, 'Allocation failure for array "tmp"' ) 
         return     
      end if
      
      minai = min(na,ni)
      p1 =      1 ; p2 = pnum - 1  ; tmp(1:na   , p1:p2) = a%v   (1:na   ,   p1:p2)
      p1 = p2 + 1 ; p2 = p2 + mi   ; tmp(1:minai, p1:p2) = matrix(1:minai,    1:mi)
      p1 = p2 + 1 ; p2 = ma + mi   ; tmp(1:na   , p1:p2) = a%v   (1:na   , pnum:ma)
      
      call move_alloc (from = tmp, to = a%v)
      
      a%ncol = ma + mi
   
   else
      call opflag%Set ( UERROR,HERE,"Invalid option cr ="//cr//" (must be 'c' or 'r')" )
      return
   end if
!---------------------------------------------------------------------------------------------         
