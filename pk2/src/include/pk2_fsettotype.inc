!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "pk2_fsettotype.inc" for the module pk2. Common lines for subroutines:
!      . pk2_fi4i4SetToType
!      . pk2_fi4i8SetToType
!      . pk2_fi8i4SetToType
!      . pk2_fi8i8SetToType
!---------------------------------------------------------------------------------------------
   n = 0 ; m = 0 
   
   if (size(shape) >= 2) then
      n = shape(1) ; m = shape(2)
   end if   
   
   if (typ < EMPTY .or. typ > STYP) then
      write(cnum,'(i0)')typ   
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'Invalid value ('//trim(cnum)//') for array type' )
      if (present(stat)) stat = opflag                 
      return
   end if   
   if (n < 0) then
      write(cnum,'(i0)')n
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'Negative value ('//trim(cnum)//') for dimension #1' )
      if (present(stat)) stat = opflag                 
      return
   end if   
   if (m < 0) then
      write(cnum,'(i0)')m
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'Negative value ('//trim(cnum)//') for dimension #2' )
      if (present(stat)) stat = opflag                 
      return
   end if
   
   if (present(name)) res%name = name
!
!- Note: if (n == 0 or m == 0), set res%nrow = 0 & res%ncol = 0
!   
   select case (typ)
      case (EMPTY)
         res%m = bk2_t()
      case (ITYP)
            !res%m = ik2_t( n = n, m = m )
            allocate(ik2_t::res%m)
            select type (p=>res%m)
               type is (ik2_t) ; call bk2_SetMatIk2nm (res=p, n=n, m=m)
            end select
      case (RTYP)
            !res%m = rk2_t( n = n, m = m )
            allocate(rk2_t::res%m)
            select type (p=>res%m)
               type is (rk2_t) ; call bk2_SetMatRk2nm (res=p, n=n, m=m)
            end select
      case (CTYP)
            !res%m = ck2_t( n = n, m = m )
            allocate(ck2_t::res%m)
            select type (p=>res%m)
               type is (ck2_t) ; call bk2_SetMatCk2nm (res=p, n=n, m=m)
            end select
      case (LTYP)
            !res%m = lk2_t( n = n, m = m )
            allocate(lk2_t::res%m)
            select type (p=>res%m)
               type is (lk2_t) ; call bk2_SetMatLk2nm (res=p, n=n, m=m)
            end select
      case (STYP)
            !res%m = sk2_t( n = n, m = m )
            allocate(sk2_t::res%m)
            select type (p=>res%m)
               type is (sk2_t) ; call bk2_SetMatSk2nm (res=p, n=n, m=m)
            end select
      case default
         call opflag%set ( stat = UERROR, where = HERE, msg = 'Unknown array type' )
         if (present(stat)) stat = opflag                 
         return
   end select  
      
   if (opflag%code > 0) then
      call opflag%AddTrace(HERE)
      if (present(stat)) stat = opflag                 
      return
   end if
   
   res%nrow = res%m%nrow ; res%ncol = res%m%ncol ; res%typ = res%m%typ
!---------------------------------------------------------------------------------------------         
