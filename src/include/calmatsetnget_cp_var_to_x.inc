
   call err_resetHaltingMode ( reset = .false., stat = stat )

   if ( from%Id > 0 .and. from%Id <= size(G_vars) ) then
      select rank(to)
         rank(0)
            call G_vars(from%Id)%GetMatij ( IONE, IONE, to, G_flagerr )
         rank(1)
            call G_vars(from%Id)%GetMatPacked ( to, G_flagerr )
         rank(2)
            call G_vars(from%Id)%GetMat ( to, G_flagerr )
         rank default
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Unexpected rank of '//util_intToChar(rank(from)) )
            if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end select  

      if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

   else
      call G_flagerr%set ( msg = "Invalid variable Id", where = HERE, stat = G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end if   
   
   call err_resetHaltingMode ( reset = .true. )
   