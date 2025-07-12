
   call err_resetHaltingMode ( reset = .false., stat = stat )

   if ( from%Id > 0 .and. from%Id <= size(G_vars) ) then
      name = G_vars(from%Id)%name

      if ( G_vars(from%Id)%status == G_PROTECTED .or. &
           G_vars(from%Id)%status == G_USERPROTECTED   ) then
         call G_flagerr%set ( stat = G_UERROR, where = HERE, msg = 'The variable #' //   &
                  i2a(from%Id)//' ('//name//') is protected and thus can not be changed' )
         if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end if 
      
      err = 0
      
      select case ( G_vars(from%Id)%typ )
         case ( ITYP )
            if ( toType == LTYP .or. toType == STYP ) err = 1
         case ( RTYP )
            if ( toType == ITYP .or. toType == LTYP .or. toType == STYP ) err = 1
         case ( CTYP ) 
            if ( toType == ITYP .or. toType == RTYP .or. toType == LTYP .or. &
                 toType == STYP ) err = 1
         case ( LTYP )
            if ( toType == RTYP .or. toType == CTYP .or. toType == STYP ) err = 1
         case ( STYP )
            if ( toType /= STYP ) err = 1
      end select
      
      if ( err /= 0 ) then
         call G_flagerr%set (stat=G_UERROR, where=HERE,                     &
            msg='Cannot move the calmat variable "'//name//'" (type = '  // &
            trim(varTypeLong(G_vars(from%Id)%typ))//') to a variable of '// &
            trim(varTypeLong(toTYpe)) // ' type'                            )
         if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end if
            
      select rank( to )
         rank ( 0 )
            call G_vars(from%Id)%GetMatij ( IONE, IONE, to, G_flagerr )
            if ( G_flagerr <= IZERO ) call G_vars(from%Id)%pk2_t%Destroy()
         rank ( 1 ) 
            call G_vars(from%Id)%GetMatPacked ( to, G_flagerr )      
            if ( G_flagerr <= IZERO ) call G_vars(from%Id)%pk2_t%Destroy()
         rank ( 2 )
            if ( toType == G_vars(from%Id)%typ ) then
               call pk2_movealloc ( from = G_vars(from%Id), to = to, stat = G_flagerr )
            else
               call G_vars(from%Id)%GetMat ( to, G_flagerr)
               if ( G_flagerr <= IZERO ) call G_vars(from%Id)%pk2_t%Destroy()
            end if
         rank default
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Unexpected rank of '//util_intToChar(rank(to)) )
            if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end select
      
      G_vars(from%Id)%name = name
      if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

      call G_vars(from%Id)%resetDesc()
      from%size = [IZERO,IZERO]
      from%type = 'e'
      
   else
      call G_flagerr%set ( msg = "Invalid variable Id", where = HERE, stat = G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end if  
   
   call err_resetHaltingMode ( reset = .true. )
