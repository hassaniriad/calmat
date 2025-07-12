
   ! if stat is not present, set the halting mode to .true. (any error will stop execution)
   
   call err_resetHaltingMode ( reset = .false., stat = stat )

   if ( to%Id > 0 .and. to%Id <= size(G_vars) ) then
         
      select rank ( from )
         rank ( 0 )
            call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
         rank ( 1 )
            call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
         rank ( 2 )
            call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
         rank default
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Unexpected rank of '//util_intToChar(rank(from)) )
            if ( present(stat) ) then
               ! if there is an error:
               ! (1) set stat=G_flagerr (2) reset the initial halting mode (3) return:
               setNreturn(G_flagerr,stat)
            end if
      end select
      
      if ( present(stat) ) then
         ! if there is an error:      
         ! (1) set stat=G_flagerr (2) add trace (3) reset the initial halting mode (4) return:
         setNtraceNreturn(G_flagerr,HERE,stat)
      end if
      
      call G_vars(to%Id)%resetDesc()            
      to%size = [ G_vars(to%Id)%nrow, G_vars(to%Id)%ncol ]
      to%type = varType(G_vars(to%Id)%typ)
      
   else
      call G_flagerr%set ( msg = "Invalid variable Id", where = HERE, stat = G_UERROR ) 
      if ( present(stat) ) then
         ! if there is an error:      
         ! (1) set stat=G_flagerr (2) reset the initial halting mode (3) return:
         setNreturn(G_flagerr,stat)
      end if
   end if

   ! reset the initial halting mode:
   
   call err_resetHaltingMode ( reset = .true. )
   
   