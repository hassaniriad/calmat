!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "pk2_setsubmat.inc" for the module pk2. Common lines for subroutines:
!      . pk2_i32setsubmat
!      . pk2_i64setsubmat
!---------------------------------------------------------------------------------------------
   if ( a%typ == EMPTY .or. .not. allocated(a%m) ) then
!
!-    case self(indi,indj) = [ ]   (if self is also empty, it remains empty): 
!   
      if ( self%typ /= EMPTY ) then
!
!-       self is non-empty: this case corresponds to rows or columns removal:  
!           
         if ( present(indj) ) then
!
!-          case self(indi,indj) = []
!         
            ni = count(indi <= self%nrow .and. indi >= 1) ! ni = nbr of rows to remove
            mi = count(indj <= self%ncol .and. indj >= 1) ! mi = nbr of column to remove
!
!-          at least ni or mi must be equal to self%nrow or to self%ncol:
!
            if ( ni == self%nrow ) then
!
!-             remove the columns whose #s are given in "indj":
!         
               call self%DelRows (indj,'c')
               
            else if ( mi == self%ncol ) then
!
!-             remove the rows whose #s are given in "indi":
!
               call self%DelRows (indi,'r')
               
            else
               
               call opflag%set ( stat = UERROR, where = HERE, &
                                  msg = 'the submatrix is not correctly defined' )
               if ( present(stat) ) call err_moveAlloc (from = opflag, to = stat)  !stat = opflag
               return
            
            end if           
         
         else   
!
!-          case self(indi) = [] (elements removal)
!         
            call self%DelRows (indi,'e')

         end if
   
         if ( opflag%code > 0 ) then
            call opflag%AddTrace(HERE)
            if ( present(stat) ) call err_moveAlloc (from = opflag, to = stat)  !stat = opflag
         end if

      end if   
      
      return   
   
   end if  

   
   if ( .not. present(indj) ) then
      call a%m%SetSubmat (self%m, indi = int(indi,Ikind), add = add)
   else
      call a%m%SetSubmat (self%m, indi = int(indi,Ikind), indj = int(indj,Ikind), add = add)
   end if      
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace(HERE)
      if ( present(stat) ) call err_moveAlloc (from = opflag, to = stat)  !stat = opflag
      return
   end if
   
   self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol    
!---------------------------------------------------------------------------------------------         
