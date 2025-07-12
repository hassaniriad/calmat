   program foo
      use pk2mod_m
      type(pk2_t) :: A, B, C

      A = 1 ! A is a scalar integer (actually a 1x1 matrix)
      A = 'hello' ! now A becomes a 1x1 string  
      A = [.true., .false.] ! now A is a logical vector
      A = reshape([1., 2., 3., 4., 5., 6.],[2,3]) ! now A is a real matrix
      A = reshape([(1,1),(2,0),(0,2),(4,3)],[2,2]) ! now A is a 2x2 matrix of complexes

      ! operators and usual functions are overloaded: 
      B = sin(2*A + 1) ; call B%printMe(form='values')
      C = abs(A) >= 2  ; call C%printMe(form='values')
      A = ['hello','world']
      B = [' ','!']
      C = trans(A + trim(B)) ; call C%printMe(form='values')
      !...
 
      A = 10 ; B = 3

      ! remember that user-defined binary operators have the lowest precedence, e.g.
      C = a .m. 2 + b !< gives 50 (10 * (2+3)) and not 23 (10*2 + 3)  
      call C%printMe(form='values')

      ! so use parentheses where appropriate:
      C = (A .m. 2) + B
      call C%printMe(form='values')

   end program foo
