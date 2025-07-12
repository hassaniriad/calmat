   program ex_reshape

      use pk2mod_m

      integer(Ikind) :: ia(2,3) = reshape([1,2,3,4,5,6],[2,3]), idim(2)
      type(pk2_t) :: A, Res, Pad, Dim

      A = ia
      call A%printMe(msg = 'Let A = ', form = 'values')

      ! idim(1) * idim(2) must be equal to A%nrow * A%ncol 
      ! unless "pad" is present:

      idim = [3,2]
      call s_reshape ( a=A, dim=idim, res=Res  )  ! or Res = reshape ( A, idim )
      call Res%printMe(msg = 'Then reshape(A,[3,2]) is: ',form='values')

      ! when "pad" is present it must be of the same type as "a"
      !  and only its (1,1) element is used:

      idim = [4,3] ; Pad = 0
      call s_reshape ( a=A, dim=idim, res=Res, pad=Pad  )  ! or Res = reshape ( A, idim, Pad )
      call Res%printMe(msg = 'and reshape(A,[4,3],Pad) is: ',form='values')
      
      ! the argument "dim" can also be a pk2_t variable:

      Dim = [3,2]*1_Ikind
      call s_reshape ( a=A, dim=Dim, res=Res  )  ! or B = reshape ( A, Dim )
 
   end program ex_reshape
