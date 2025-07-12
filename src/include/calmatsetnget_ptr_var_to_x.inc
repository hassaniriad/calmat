   integer :: err

   call err_resetHaltingMode ( reset = .false., stat = stat )

   err = 0
   
   select rank(ptr)
      rank(0)
         if ( var%Id > 0 .and. var%Id <= size(G_vars) ) then 
            call G_vars(var%Id)%pointer ( ptr = ptr, stat = G_flagerr )
         else
            err = 1 ; ptr => NULL()
         end if
      rank(1)
         if ( var%Id > 0 .and. var%Id <= size(G_vars) ) then 
            call G_vars(var%Id)%pointer ( ptr = ptr, stat = G_flagerr )
         else
            err = 1 ; ptr => NULL()
         end if
      rank(2)
         if ( var%Id > 0 .and. var%Id <= size(G_vars) ) then 
            call G_vars(var%Id)%pointer ( ptr = ptr, stat = G_flagerr )
         else
            err = 1 ; ptr => NULL()
         end if
      rank default
         call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                               msg = 'Unexpected rank of '//util_intToChar(rank(ptr)) )
         if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end select  
   
   if ( err == 1 ) then
      call G_flagerr%set ( msg = "Invalid variable Id", where = HERE, stat = G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      if ( present(stat) .and. G_flagerr > IZERO ) then
         call G_flagerr%Addmsg ( msg = 'The target is the calmat variable #'//i2a(var%id), &
                                 newline = .true. )
         call err_movealloc ( from = G_flagerr, to = stat )
         call stat%AddTrace (  HERE )
         return
      end if
   end if

   call err_resetHaltingMode ( reset = .true. )
