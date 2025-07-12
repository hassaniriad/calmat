!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! include file "pk2_insertmat.inc" for the module pk2. Common lines for subroutines:
!      . pk2_InsertImat32
!      . pk2_InsertImat64
!      . pk2_InsertRmat32
!      . pk2_InsertRmat64
!      . pk2_InsertCmat32
!      . pk2_InsertCmat64
!      . pk2_InsertLmat
!      . pk2_InsertSmat
!      . pk2_InsertChmat
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------      
   type     (pk2_t) :: ptmp
   character(len=1) :: cr
   integer  (Ikind) :: pnum
!--------------------------------------------------------------------------------------------- 

   opflag = err_t ()

   pnum = 0 ; cr = '?'
   
   if (present(at_col) .and. present(at_row)) then
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'Column # and row # should not both be given ' )
                       
   else if (present(at_col)) then
      cr = 'c' ; pnum = at_col
   else if (present(at_row)) then
      cr = 'r' ; pnum = at_row  
   else
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'Missing the column or row # where the array must be inserted' )
   end if
   
   if (opflag%code > 0) then
      if (present(stat)) stat = opflag
      return
   end if
   
   call pk2_assign ( lhs = ptmp, rhs = insert, stat  = opflag )

   if (opflag%code > 0) then
      call opflag%AddTrace(HERE)
      if (present(stat)) stat = opflag
      return
   end if
    
   call ptmp%m%InsertInto ( into%m, int(pnum,Ikind), cr )
   
   if (opflag%code > 0) then
      call opflag%AddTrace(HERE)
      if (present(stat)) stat = opflag
      return
   end if
   
   into%typ = into%m%typ ; into%nrow = into%m%nrow ; into%ncol = into%m%ncol    
!---------------------------------------------------------------------------------------------         
