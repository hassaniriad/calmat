program ex_extracmat
   use pk2mod_m
   implicit none
   type(pk2_t) :: A, subA
  
   A = reshape([11,12,13,14,15,16,17,18,19],[3,3]) * 1_Ikind
   call A%printMe ( msg = 'Let A = ', form = 'values' )

   ex1: block
     ! Version 1: Passing indices as integers or booleans:
     integer(Ikind) :: i
 
     ! extract an element at given indices (i,j):  
     subA = extracmat ( A, [3_Ikind], [1_Ikind] )
     call subA%printMe ( msg = '    A(3,1) = ', form = 'values' )

     ! extract an element at a given major-column index:
     subA = extracmat ( A, [3_Ikind] )
     call subA%printMe ( msg = '    A(3) = ', form = 'values' )

     ! extract a set of elements given by their indices (i,j)      
     subA = extracmat ( A, [1,3]*1_Ikind, [2,3]*1_Ikind )
     call subA%printMe ( msg = '    A([1,3],[2,3]) = ', form = 'values' )

     ! another way is to give boolean arrays (mixing is possible). For example:
     subA = extracmat ( A, [.true., .false., .true.], [.false.,.true.,.true.])
     call subA%printMe ( msg = '    A([T,F,T,],[F,T,T]) = ', form = 'values' )

     ! extract a set of elements given by their major-column indices:      
     subA = extracmat ( A, [1,3,4]*1_Ikind )
     call subA%printMe ( msg = '    A([1,3,4]) = ', form = 'values' )

     ! extract contiguous elements:         
     subA = extracmat ( A, [(i,i=2,3)], [(i,i=1,2)] )
     call subA%printMe ( msg = '    A(2:3,1:2) = ', form = 'values' )

     ! extract a given column (i = -1 means the whole column, j = -1 means the whole row): 
     subA = extracmat ( A, [-1_Ikind], [2_Ikind] )
     call subA%printMe ( msg = '    A(:,2) = ', form = 'values' )

     ! all elements in column-major ordering (same as subA = reshape(A,[n,1] where n = numel(A)):   
     subA = extracmat ( A, [-1_Ikind] )
     call subA%printMe ( msg = '    A(:) = ', form = 'values' )
   end block ex1

   ex2: block
     ! Version 2: Same things but passing indices as pk2 variables:
     type(pk2_t) :: rowIndx, colIndx

     rowIndx = 3_Ikind ; colIndx = 1_Ikind
     subA = extracmat ( A, rowIndx, colIndx )

    ! etc.
   end block ex2
end program ex_extracmat
