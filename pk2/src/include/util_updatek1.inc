!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "util_updatek1.inc" for the module util_Other. Common lines for subroutines:
!      . util_UpdateIk1
!      . util_UpdateRk1
!      . util_UpdateCk1
!      . util_UpdateLk1
!---------------------------------------------------------------------------------------------
   
   if (.not. allocated(List)) allocate(List(0))
   
   if (present(EltToremove)) then
      
      nkeep = 0
      do i = 1, size(List)
         if ( any(EltToRemove == i) ) cycle
         nkeep = nkeep + 1
         lkeep(nkeep) = i
      end do   

      if (nkeep == 0) then
         deallocate(List) ; allocate(List(0))
      else                    
         allocate(tmp(nkeep), stat = err)
         if (err /= 0) then
            call stat%Set ( UERROR, HERE, 'Allocation failure for array "tmp"' )
            return
         end if   
         
         tmp(1:nkeep) = List(lkeep(1:nkeep))
         call move_alloc (from = tmp, to = List)
      end if   
   end if
   
   if (present(ListToInsert)) then
      nold = size(List)   
      nins = size(ListToInsert) ; if (nins == 0) return
      
      ipos = 1 ; if (present(Pos)) ipos = Pos
   
      if (ipos == -1) ipos = nold + 1
      
      nnew = nins + max(nold, ipos-1)
             
      allocate(tmp(nnew), stat = err)
      if (err /= 0) then
         call stat%Set ( UERROR, HERE, 'Allocation failure for array "tmp"' )
         return
      end if   

      p1 =      1 ; p2 = min(ipos-1,nold) ; tmp(p1:p2) = List(p1:p2)
      p1 = p2 + 1 ; p2 = ipos-1           ; tmp(p1:p2) = zero
      p1 = p2 + 1 ; p2 = ipos + nins - 1  ; tmp(p1:p2) = ListToInsert(1:nins)
      p1 = p2 + 1 ; p2 = nnew             ; tmp(p1:p2) = List(ipos:nold)      
      
      call move_alloc (from = tmp, to = List)      
   end if