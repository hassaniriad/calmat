program ex_delrows
!---------------------------------------------------------------------------------------------
!  Deleting some rows or columns
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   type(pk2_t) :: a, a0
!---------------------------------------------------------------------------------------------

   a0 = reshape([11, 21, 31, 12, 22, 32 ,13, 23, 33]*1_Ikind,[3,3])

   a = a0   
   ! 1) Deleting rows 1 and 3 of the 3x3 matrix:
   call a%printMe ( msg = 'Let a = ', form = 'values' ) 
   call a%DelRows ( opt = 'r', indx = [1_Ikind,3_Ikind] )
   call a%printMe ( msg = 'then a%DelRows([1,3],"r") gives : ', form = 'values') 
   
   a = a0
   ! 2) Deleting columns 1 and 3 of the 3x3 matrix:
   call a%printMe ( msg = 'Let a = ', form = 'values' ) 
   call a%DelRows ( opt = 'c', indx = [1_Ikind,3_Ikind] )
   call a%printMe ( msg = 'then a%DelRows([1,3],"c") gives : ', form = 'values') 

   a = a0
   ! 3) Deleting elements 1, 3 and 5 (i.e. (1,1), (3,1) and (2,2) of the 3x3 matrix:
   call a%printMe ( msg = 'Let a = ', form = 'values' )
   call a%DelRows ( opt = 'e', indx = [1,3,5]*1_Ikind )
   call a%printMe ( msg = 'then a%DelRows([1,3,5],"e") gives : ', form = 'values')

end program ex_delrows

