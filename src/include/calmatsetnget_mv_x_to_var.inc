
   call err_resetHaltingMode ( reset = .false., stat = stat )

   if ( to%Id > 0 .and. to%Id <= size(G_vars) ) then

      if ( G_vars(to%Id)%status == G_PROTECTED .or. &
           G_vars(to%Id)%status == G_USERPROTECTED   ) then
         call G_flagerr%set ( stat=G_UERROR, where=HERE, msg = 'The variable #'// &
         i2a(to%Id)//' ('//G_vars(to%Id)%name//') is protected and thus can not be changed' )
         if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end if 
         
      select rank ( from )
         rank ( 0 )
            if ( allocated(from) ) then
               call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               if ( G_flagerr <= IZERO ) deallocate ( from )
            else
               call G_vars(to%Id)%pk2_t%Destroy()
            end if
         rank ( 1 )
            if ( allocated(from) ) then
               call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               if ( G_flagerr <= IZERO ) deallocate ( from )
            else
               call G_vars(to%Id)%pk2_t%Destroy()
            end if            
         rank ( 2 )
            ! Bug with nagfor (7.2 Build 7207), yet unresolved (05/24)
            ! Resolved (05/24) with the release 7.2, Build 7208
            call pk2_movealloc ( from=from, to=G_vars(to%Id), stat=G_flagerr )
         rank default
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                 msg = 'Unexpected rank of '//util_intToChar(rank(from)) )
            if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end select
      
      if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if
      
      call G_vars(to%Id)%resetDesc()    
      to%size = [G_vars(to%Id)%nrow,G_vars(to%Id)%ncol]
      to%type = varTypeShort(G_vars(to%Id)%typ)
      
   else
      call G_flagerr%set ( msg = "Invalid variable Id", where = HERE, stat = G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
