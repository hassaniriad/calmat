!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
! 
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "util_reshape.inc" for the module util_Other. Common lines for subroutines:
!      . util_ReshapeI
!      . util_ReshapeR
!      . util_ReshapeC
!      . util_ReshapeL
!---------------------------------------------------------------------------------------------

   if (present(pad)) defpad(1,1) = pad

   nr = shape(1) ; mr = shape(2)
   
   if (nr == 0 .or. mr == 0) then
      allocate(res(0,0))
      return
   end if
      
   if (size(a) < ysize) then
!
!-    when matrix size is not too big, use intrinsic reshape:
!   
      res = reshape ( a, shape, pad=defpad )
   else
!
!-    otherwise use do loops:
!   
      call util_alloc ( mode='e', t=res, n=nr, m=mr, stat=stat, const=defpad(1,1) )
      error_TraceNreturn(stat, HERE)
      
      !allocate(res(nr,mr), source = defpad(1,1), stat = err)
      !if (err /= 0) then
      !   call stat%set (stat = IERROR, msg = HERE // " Allocation failure")         
      !   return
      !end if   
      
      na = size(a,1) ; ma = size(a,2) ; szr = nr*mr
      
      ir = 1 ; jr = 1 ; kr = 0
      
      do ja = 1, ma
         do ia = 1, na
            kr = kr + 1 ; if (kr > szr) return
            
            res(ir,jr) = a(ia,ja)
            
            if (ir < nr) then
!
!-             the next element will be on the same column # jr and at the row # ir:
!            
               ir = ir + 1
            else
!
!-             the next element will be at the 1st row of the next column # jr:
!
               ir = 1
               jr = jr + 1
            end if
            
         end do
      end do
      
   end if
