program ex_setsubmat
   use pk2mod_m
   type(pk2_t) :: a, b

   a = reshape([1,2,3,4,5,6,7,8,9],[3,3],order = [2,1])
   b = reshape([10,100,1000,10000],[2,2],order = [2,1])

   call a%printMe(msg='Let a = ',form='values') ;
   call b%printMe(msg='and b = ',form='values') ;

   ! add the elements of b into a at i in {2,3} and j in {1,3}:
   call a%SetSubmat(b,[2,3],[1,3], add=.true.)

   call a%printMe(msg='Then a%SetSubmat(b,[2,3],[1,3], add=.true.)',form='values')
   
end program ex_setsubmat

