!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "bk2_epow.inc" for the module bk2. Common lines for subroutine bk2_epow

!---------------------------------------------------------------------------------------------
         
   if ( nama == 1 ) then  ! scalar .^ matrix
      do j = 1, mb
         do i = 1, nb
            call util_pow ( base=a%v(1,1), expo=b%v(i,j), res=c%v(i,j), typres=tres )
            tc = max(tc,tres)
         end do
      end do

   else if ( nbmb == 1 ) then  ! matrix .^ scalar
      do j = 1, ma
         do i = 1, na
            call util_pow ( base=a%v(i,j), expo=b%v(1,1), res=c%v(i,j), typres=tres )
            tc = max(tc,tres)
         end do
      end do 

   else ! matrix .^ matrix (of the same shape)
      do j = 1, ma
         do i = 1, na
            call util_pow ( base=a%v(i,j), expo=b%v(i,j), res=c%v(i,j), typres=tres )
            tc = max(tc,tres)
         end do
      end do 
   end if   
!---------------------------------------------------------------------------------------------         
