!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "util_sort.inc" for the module util_Other. Common lines for subroutines:
!      . util_SortIk1
!      . util_SortRk1
!      . util_SortCk1
!
!  Source: this is a copy of LAPACK routine DLASRT where "indx" was added.
!---------------------------------------------------------------------------------------------

!            
!- Test the input parameters.
!
   dir = -1
   if (util_stringlow (ord) == 'd') then
      dir = 0
   else if (util_stringlow (ord) == 'i') then
      dir = 1
   else
      call stat%Set ( UERROR, HERE, &
                     "Invalid value '"// ord // "' for << ord >> (must be 'i' or 'd' only)" )
      return
   end if

   allocate(indxd(n), stat = estat)   
   
   if (estat /= 0) then
      call stat%Set ( IERROR, HERE, " Allocation failure for array 'indxd'" )
      return
   end if

   indxd(1:n) = [(i,i=1,n)]
!
!- Quick return if possible
!
   if ( n <= 1 ) then
      if (present(indx)) call move_alloc (from = indxd, to = indx)
      return
   end if
   
   stkpnt = 1
   stack( 1, 1 ) = 1
   stack( 2, 1 ) = n

10 continue
      start = stack( 1, stkpnt )
      endd  = stack( 2, stkpnt )
      stkpnt = stkpnt - 1
      if ( endd-start.le.select .and. endd-start.gt.0 ) then
!
!        do insertion sort on d( start:endd )
!
         if ( dir.eq.0 ) then
!
!           sort into decreasing order
!
            do 30 i = start + 1, endd
               do 20 j = i, start + 1, -1
                  if( d( j ).gt.d( j-1 ) ) then
                     dmnmx  = d(j  ) ; imnmx      = indxd(j  )
                     d(j  ) = d(j-1) ; indxd(j  ) = indxd(j-1)
                     d(j-1) = dmnmx  ; indxd(j-1) = imnmx
                  else
                     go to 30
                  end if
20             continue
30          continue

         else
!
!           sort into increasing order
!
            do 50 i = start + 1, endd
               do 40 j = i, start + 1, -1
                  if( d( j ).lt.d( j-1 ) ) then
                     dmnmx  = d(j  ) ; imnmx      = indxd(j  )
                     d(j  ) = d(j-1) ; indxd(j  ) = indxd(j-1)
                     d(j-1) = dmnmx  ; indxd(j-1) = imnmx
                  else
                     go to 50
                  end if
40             continue
50          continue

         end if

      else if ( endd-start.gt.select ) then
!
!        partition d( start:endd ) and stack parts, largest one first
!
!        choose partition entry as median of 3
!
         d1 = d( start ) ; i1 = indxd(start)
         d2 = d( endd  ) ; i2 = indxd(endd )
         i = ( start+endd ) / 2
         d3 = d( i ) ; i3 = indxd(i)
         if ( d1.lt.d2 ) then
            if( d3.lt.d1 ) then
               dmnmx = d1 ; imnmx = i1
            else if ( d3.lt.d2 ) then
               dmnmx = d3 ; imnmx = i3
            else
               dmnmx = d2 ; imnmx = i2
            end if
         else
            if ( d3.lt.d2 ) then
               dmnmx = d2 ; imnmx = i2
            else if ( d3.lt.d1 ) then
               dmnmx = d3 ; imnmx = i3
            else
               dmnmx = d1 ; imnmx = i1
            end if
         end if

         if( dir.eq.0 ) then
!
!           sort into decreasing order
!
            i = start - 1
            j = endd + 1
60          continue
70          continue
            j = j - 1
            if ( d( j ).lt.dmnmx ) &
               go to 70
80          continue
            i = i + 1
            if ( d( i ).gt.dmnmx ) &
               go to 80
            if ( i.lt.j ) then
               tmp    = d( i ) ; itmp     = indxd(i)
               d( i ) = d( j ) ; indxd(i) = indxd(j)
               d( j ) = tmp    ; indxd(j) = itmp
               go to 60
            end if
            if ( j-start.gt.endd-j-1 ) then
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = start
               stack( 2, stkpnt ) = j
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = j + 1
               stack( 2, stkpnt ) = endd
            else
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = j + 1
               stack( 2, stkpnt ) = endd
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = start
               stack( 2, stkpnt ) = j
            end if
         else
!
!           sort into increasing order
!
            i = start - 1
            j = endd + 1
90          continue
100         continue
            j = j - 1
            if ( d( j ).gt.dmnmx ) &
               go to 100
110         continue
            i = i + 1
            if ( d( i ).lt.dmnmx ) &
               go to 110
            if ( i.lt.j ) then
               tmp    = d( i ) ; itmp     = indxd(i)
               d( i ) = d( j ) ; indxd(i) = indxd(j)
               d( j ) = tmp    ; indxd(j) = itmp
               go to 90
            end if
            if ( j-start.gt.endd-j-1 ) then
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = start
               stack( 2, stkpnt ) = j
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = j + 1
               stack( 2, stkpnt ) = endd
            else
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = j + 1
               stack( 2, stkpnt ) = endd
               stkpnt = stkpnt + 1
               stack( 1, stkpnt ) = start
               stack( 2, stkpnt ) = j
            end if
         end if
      end if
      if ( stkpnt.gt.0 ) &
         go to 10
!---------------------------------------------------------------------------------------------         
