program ex_insert
      use pk2mod_m
      type   (pk2_t)              :: pToinsert, pHost
      integer(Ikind), allocatable :: iToinsert(:,:)

      ! pToinsert contains a real 2x4 matrix:
      pToinsert = reshape([11,21,12,22,13,23,14,24],[2,4])*1.0_Rkind
      ! pHost contains an integer 3x4 matrix:
      pHost = reshape([101,201,301,102,202,302,103,203,303,104,204,304], [3,4])

      call pToinsert%printMe(msg='Let pToinsert = ',form='values')
      call pHost%printMe(msg='Let pHost = ',form='values')

      print '(/,a)','1) Insert pToinsert into pHost at the 2-nd row of pHost'
      ! (the result contains a real 5x4 matrix):

      call pk2_Insert(insert = pToinsert, into = pHost, at_row = 2_Ikind)

      call pHost%printMe(msg='pHost is now: ',form='values')

      ! iToinsert is an intrinsic integer 2x1 matrix:
      iToinsert = reshape([1,2],[2,1])

      print*
      call disp ('Let iToinsert = ', iToinsert)

      print '(/,a)','2) Insert iToinsert into pHost at the 3-rd column of b'
      ! (the container is a real 5x5 matrix, ):

      call pk2_Insert(insert = iToinsert, into = pHost, at_col= 3_Ikind)

      call pHost%printMe(msg='pHost is now: ',form='values')

     ! iToinsert is an intrinsic integer 7x1 matrix:
      iToinsert =-reshape([1,2,3,4,5,6,7],[7,1])

      print*
      call disp ('Let iToinsert = ', iToinsert)

      print '(/,a)','3) Insert iToinsert into pHost at the 4-th column of pHost'
      ! (the container is a real 5x6 matrix, ):

      call pk2_Insert(insert = iToinsert, into = pHost, at_col= 4_Ikind)

      call pHost%printMe(msg='pHost is now: ',form='values')

      ! Same things with logicals:
      
      pHost = reshape([.false.,.false.,.false.,.false.,&
                       .false.,.false.,.false.,.false.,.false.],[3,3]) 
      pToinsert = reshape([.true.,.true.],[2,1])

      call pToinsert%printMe(msg='Let pToinsert = ',form='values')
      call pHost%printMe(msg='Let pHost = ',form='values')

      print '(/,a)','1) Insert pToinsert into pHost at the 2-nd column of pHost'

      call pk2_Insert(insert = pToinsert, into = pHost, at_col = 2_Ikind)

      call pHost%printMe(msg='pHost is now: ',form='values')

      ! etc

      ! Same things with strings:
      
      pHost = reshape(['H11','H21','H31','H12','H22','H32','H13','H23','H33'],[3,3]) 
      pToinsert = reshape(['I1','I2'],[2,1])

      call pToinsert%printMe(msg='Let pToinsert = ',form='values')
      call pHost%printMe(msg='Let pHost = ',form='values')

      print '(/,a)','1) Insert pToinsert into pHost at the 2-nd column of pHost'

      call pk2_Insert(insert = pToinsert, into = pHost, at_col = 2_Ikind)

      call pHost%printMe(msg='pHost is now: ',form='values')
      
      ! etc
end program ex_insert

