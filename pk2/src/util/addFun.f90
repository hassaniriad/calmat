#include "../error.fpp"

MODULE addfun_m
   use pk2mod_m, i2a => util_intToChar
   implicit none
   
   private
   public :: readNwrite, nfunc, getCommandArg

   type :: var_t
      character(len=:), allocatable :: name
      integer  (Ikind)              :: typ =-1, rank =-1, size(2) = [-1,-1]
      logical                       :: alloc = .false., optional = .false.
   end type var_t
   
   character(len=:), allocatable :: funcName, subrHeader, funcHeader
   type     (var_t), allocatable :: varIn(:), varOut(:), declar(:)
   type     (str_t), allocatable :: body(:)
   integer  (Ikind)              :: nvarIn = 0, nvarOut = 0, nline = 0, ndecl = 0, numl, n
   
   integer  (Ikind), parameter   :: NFUNCINC = 10, NLINEINC = 50, NVARINC = 10, NDECLINC = 10

   integer  (Ikind)              :: nfuncMax   = NFUNCINC, &
                                    nlineMax   = NLINEINC, &
                                    ndeclMax   = NDECLINC

   integer  (Ikind)              :: ui = 0, uo = 0, uof = 0, nfunc = 0

   character(len=:), allocatable :: fileName, libName, moduledir
   type     (str_t), allocatable :: listfun(:,:)
   integer  (Ikind), allocatable :: nargfun(:,:)
   
   character(len=*), parameter   :: KWDSTART = 'fortranproc', KWDEND = 'endfortranproc'
   
CONTAINS

   
!=============================================================================================
   SUBROUTINE readNwrite ( file, lib, dynlib, delf90, onlysub, stat )
!=============================================================================================
   character(len=*), intent(in    ) :: file, lib
   logical         , intent(in    ) :: dynlib, delf90, onlysub
   type     (err_t), intent(   out) :: stat
!---------------------------------------------------------------------------------------------   
!  Inputs:
!    . file  : Name of file containing any user functions
!    . lib   : Name of the resulting library
!    . dynlib: .true. in case of a shared library
!
!  Output:
!    . stat: error handler
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'readNwrite'
   integer                       :: lenkwd, err
   character(LGSTR)              :: iomsg
   logical                       :: waitForEndFunc
   character(len=:), allocatable :: str, buf, rec, rec0
!---------------------------------------------------------------------------------------------   

   lenkwd = len(KWDEND)
   
   fileName = trim(adjustl(file)) ; libName = trim(adjustl(lib))
   
   open ( newunit = ui, file = fileName, status = 'old', action = 'read', &
          iostat = err, iomsg = iomsg                                     )
   
   if ( err /= 0 ) then
      stat = err_t ( where = HERE, stat = UERROR, msg =                              &
                     "Failed to open '"//fileName//"'"//NLT// "IOMSG: "//trim(iomsg) )
      return
   end if
   
   open ( newunit = uo, file = libName//'.tmp', status = 'replace', &
          iostat = err, iomsg = iomsg                               )

   if ( err /= 0 ) then
      stat = err_t ( where = HERE, stat = UERROR, msg =                                 &
                     "Failed to open '"//libName//".tmp'"//NLT// "IOMSG: "//trim(iomsg) )
      close(ui)
      return
   end if

   open ( newunit = uof, file = libName//'.f90', status = 'replace', action = 'write', &
          iostat = err, iomsg = iomsg                                                  )

   if ( err /= 0 ) then
      stat = err_t ( where = HERE, stat = UERROR, msg =                                 &
                     "Failed to open '"//libName//".f90'"//NLT// "IOMSG: "//trim(iomsg) )
      close(ui)
      close(uo, status = 'delete')
      return
   end if
   
   call util_alloc ( mode = 'e', t = listfun, n = nfuncMax, m = 3_Ikind, stat = stat )

   if ( stat > IZERO ) then
      close(ui) ; close(uo, status = 'delete') ; close(uof, status = 'delete')
      call stat%AddTrace(HERE)
      return
   end if
      
   call util_alloc ( mode = 'e', t = nargfun, n = nfuncMax, m = 2_Ikind, stat = stat )
   
   if ( stat > IZERO ) then
      close(ui) ; close(uo, status = 'delete') ; close(uof, status = 'delete')
      call stat%AddTrace(HERE)
      return
   end if
   
   waitForEndFunc = .false.
   
   numl = 0
   
   do
      rec = util_GetRecord ( ui, stat, rmblk=.false., comments=['//','/*','*/'], nlines = n, rec0=rec0 )
      if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if

      numl = numl + n
      
      if ( stat < IZERO ) then
!
!-       the end-of-file has been reached (close the script if the read was not on stdin)
!      
         stat = err_t()       

         exit
      end if   
      
      str = util_StringLow2 ( rec, stat, 'nqt' )
      
      if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if

      if ( waitForEndFunc ) then
      
         if ( index(str,KWDEND) == 1 ) then 
            buf = trim(adjustl(str(lenkwd+1:)))
            if ( len(buf) > 0 ) then
               if ( util_StringLow(funcName) /= buf ) then
                  stat = err_t ( where = HERE, stat = UERROR, msg = "'" // rec // &
                                 "' found when '"//KWDEND//" "//funcName//"' expected" )
                  exit
               end if
            end if
            
            waitForEndFunc = .false. 
            
            call writeSubroutine ( onlysub, stat )
            
            if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if
         
         else
         
            if ( index(str,KWDSTART) == 1 ) then 
               stat = err_t ( msg = rec // " found when '" // KWDEND//"' expected", &
                              where = HERE, stat = UERROR )
               exit
            end if
            
            call functionBody ( rec, rec0, stat )
            
            if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if
         end if
      
      else if ( index(str,KWDSTART) == 1 ) then  
         
         ! a new function:
         
         nline = IZERO ; nlineMax = NLINEINC 
         ndecl = IZERO ; ndeclMax = NDECLINC
         
         call util_alloc ( mode = 'i', t = body, n = nlineMax, stat = stat )
         
         if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if

         call allocVark1 ( mode = 'i', t = declar, n = ndeclMax, stat = stat )
         
         if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if
                  
         nfunc = nfunc + 1
         
         if ( nfunc > nfuncMax ) then
            nfuncMax = nfuncMax + NFUNCINC
            
            call util_alloc ( mode = 's', t = listfun, n = nfuncMax, m = 3_Ikind, stat = stat )
            
            if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if
            
            call util_alloc ( mode = 's', t = nargfun, n = nfuncMax, m = 2_Ikind, stat = stat )
            
            if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if

         end if
         
         call functionHeader ( rec, stat )
         
         if ( stat > IZERO ) then ; call stat%AddTrace(HERE) ; exit ; end if
         
         waitForEndFunc = .true. 
      end if      
      
   end do

   close(ui)
      
   if ( stat == IZERO ) then
      if ( waitForEndFunc ) then
         stat = err_t ( where = HERE, stat = UERROR, msg = "missing "//KWDEND)
      else
         call writeModuleHeaderAndModuleEnd ( onlysub, stat )
         if ( stat > IZERO ) call stat%AddTrace(HERE)
      end if            
   end if
   
   close(uof)
   close(uo, status = 'delete')
   
   if ( stat == IZERO .and. nfunc > IZERO ) then
      call compilLib ( dynlib, delf90, stat )
      if ( stat > IZERO ) then
         call stat%AddTrace(HERE)
      else
         call resume ()
      end if
   end if
   
   END SUBROUTINE readNwrite
   
   
!=============================================================================================
   SUBROUTINE functionBody ( rec, rec0, stat )
!=============================================================================================
   character(len=:), allocatable, intent(in out) :: rec, rec0
   type     (err_t),              intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'functionBody'
   character(len=:), allocatable :: lhs, rhs, str, indent
   integer  (Ikind)              :: i, m, n, lb, rb, typ, lenc, err
   integer  (Ikind), allocatable :: loc(:)
   type     (str_t), allocatable :: subRec(:,:)
!--------------------------------------------------------------------------------------------- 
   
   ! see if this is a fortran comment (occurence of "!" not between '' or ""):
   call util_FindSubstring2 ( loc, m, rec, 1, len(rec), '!', '""'//"''", stat )
   error_TraceNreturn(stat, HERE)

   if ( m > 0 ) then
      ! the record contains one or more "!"
      if ( loc(1) == 1 ) return ! it starts with a "!" => fortran comment => return
      rec = rec(1:loc(1)-1) ! => remove the comment part
   end if

   m = util_CountTokens ( str = adjustl(rec), delims = '::', opcl = '()[]""'//"''",  &
                          BlkToken =.false., tokens = subRec, stat = stat            )
   error_TraceNreturn(stat, HERE)

   nline = nline + 1
   if ( nline > nlineMax ) nlineMax = nlineMax + NLINEINC
    
   call util_alloc ( mode = 's', t = body, n = nlineMax, stat = stat )
   error_TraceNreturn(stat, HERE)

   i = index(rec0,NL)
   rec0 = rec0(:i-1)//rec0(i+1:) 
   i = len_trim(rec0) - len_trim(adjustl(rec0)) - 3
   if ( i < 0 ) then
      indent = ''
   else
      indent = repeat(' ',i)
   end if
   rec0 = adjustl(rec0)
   
   if ( m == 2 ) then
   
      ! Declaration part:
   
      lhs = util_stringlow ( subRec(1,1)%str ) ; rhs = subRec(2,1)%str
         
      n = util_CountTokens ( str = lhs, delims = ',', opcl = '()',  &
                             BlkToken =.false., tokens = subRec,    &
                             stat = stat                            )
      error_TraceNreturn(stat, HERE)
      
      typ = -1
      if      ( index(subRec(1,1)%str,'integer'  ) == 1 ) then            
         subRec(1,1) = 'integer(Ikind)' 
         typ = ITYP
      else if ( index(subRec(1,1)%str,'real'     ) == 1 ) then
         subRec(1,1) = 'real(Rkind)' 
         typ = RTYP
      else if ( index(subRec(1,1)%str,'complex'  ) == 1 ) then
         subRec(1,1) = 'complex(Rkind)' 
         typ = CTYP
      else if ( index(subRec(1,1)%str,'logical'  ) == 1 ) then
         typ = LTYP
      else if ( index(subRec(1,1)%str,'character') == 1 ) then
         lb = index(subRec(1,1)%str,'(') ; rb = index(subRec(1,1)%str,')')
         lenc = 1
         if ( lb * rb /= 0 ) then
            str = trim(adjustl(subRec(1,1)%str(lb+1:rb-1)))
            if ( str /= ':' .and. str /= '*' ) then
               read(str,*,iostat =err) lenc
               if ( err /= 0 ) then
                  stat = err_t( stat = UERROR, where = HERE,  &
                                 msg ='Bad character length in statement: "'//rec//'"' )
                  return
               end if
            end if
         else if ( lb /= 0 .or. rb /= 0 ) then
            stat = err_t( stat = UERROR, where = HERE,  &
                           msg ='Unbalanced parentheses in expression "'//rec//'"' )
            return
         end if            
         typ = STYP+100+lenc
      else if ( index(subRec(1,1)%str,'type'     ) == 1 ) then
         lb = index(subRec(1,1)%str,'(') ; rb = index(subRec(1,1)%str,')')
         str = trim(adjustl(subRec(1,1)%str(lb+1:rb-1)))
         if ( lb * rb /= 0 ) then
            if ( str == 'str_t') then
               subRec(1,1) = 'type(str_t)'
               typ = STYP
            end if
         else if ( lb /= 0 .or. rb /= 0 ) then
            stat = err_t( stat = UERROR, where = HERE,  &
                           msg ='Unbalanced parentheses in expression "'//rec//'"' )
            return
         else
            stat = err_t( stat = UERROR, where = HERE,  &
                           msg ='Missing parentheses in expression "'//rec//'"' )
            return
         end if            
      end if       
      if ( typ == -1 ) then
         stat = err_t( stat = UERROR, where = HERE, &
                        msg = 'Unkown type in statement "'//rec//'"' )
         return
      end if  
      body(nline) = subRec(1,1)
      do i = 2, n
         body(nline) = body(nline) + ', ' + subRec(i,1) 
      end do             
      
      call functionDeclar ( typ, rhs, rec, stat ) ; error_TraceNreturn(stat, HERE)
      
      body(nline) = indent + body(nline) + (' :: ' // rhs)

   else
      
      ! Executable part:
      
      body(nline) = indent // rec0   
   
   end if      
   
   END SUBROUTINE functionBody


!=============================================================================================
   SUBROUTINE functionDeclar ( typ, vars, rec, stat )
!=============================================================================================
   integer  (Ikind), intent(in    ) :: typ
   character(len=*), intent(in    ) :: vars, rec
   type     (err_t), intent(in out) :: stat  
!--------------------------------------------------------------------------------------------- 
!  ex.: typ = 2 and vars = "x(:,:), y(:), z, w(2) = [1.,2.]"
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'functionDeclar'
   integer  (Ikind)              :: lb, rb, i, n, rank, eq, num1, num2, err
   type     (str_t), allocatable :: subRec(:,:), size(:,:)
   character(len=:), allocatable :: str, varName, msg
!--------------------------------------------------------------------------------------------- 

   n = util_CountTokens ( str = vars, delims = ',', opcl = '()[]""'//"''", &
                          BlkToken =.false., tokens = subRec, stat = stat  )
   error_TraceNreturn(stat, HERE)

   if ( ndecl + n > ndeclMax ) ndeclMax = ndeclMax + NLINEINC
    
   call allocVark1 ( mode = 's', t = declar, n = ndeclMax, stat = stat )
   error_TraceNreturn(stat, HERE)
   
   do i = 1, n
      eq = index(subRec(i,1)%str,'=')
      if ( eq  > 0 ) then
         str = subRec(i,1)%str(:eq-1)
      else
         str = subRec(i,1)%str
      end if

      lb = index(str,'(') ; rb = index(str,')',back=.true.)
      if ( lb*rb /= 0 ) then
         varName = trim(adjustl(str(:lb-1))) // &
                   trim(adjustl(str(rb+1:)))
         rank = util_CountTokens ( str = str(lb+1:rb-1), delims = ',',  &
                                   BlkToken =.false., tokens = size, stat = stat, opcl='()' )
         error_TraceNreturn(stat, HERE)
      else if ( lb == 0 .and. rb == 0 ) then
         varName = str
         rank = 0
      else
         stat = err_t( stat = UERROR, where = HERE,  &
                        msg ='Unbalanced parentheses in expression "'//vars//'"' )
         return
      end if
      
      ndecl = ndecl + 1 
      declar(ndecl)%name = varName         ! ex.: "x"
      declar(ndecl)%rank = rank            ! ex.: 2
      declar(ndecl)%typ  = typ             ! ex.: 1
      
      if ( index(body(nline)%str,'allocatable') > 0 ) then
         declar(ndecl)%alloc = .true.
      else
         declar(ndecl)%alloc = .false.
      end if

      if ( index(body(nline)%str,'optional') > 0 ) then
         declar(ndecl)%optional = .true.
      else
         declar(ndecl)%optional = .false.
      end if

      declar(ndecl)%size =[-IONE,-IONE]
      
      if ( rank == 0 ) cycle

      err = 0 ; msg = ''
      
      if ( rank == 1 ) then
       
         if ( size(1,1)%str /= ':' ) then
            if ( declar(ndecl)%alloc ) then
               msg = 'Allocatable array "'// varName//'" must have a deferred shape.'
            else
               read(size(1,1)%str, *, iostat = err) num1
               if ( err == 0) declar(ndecl)%size(1) = num1
            end if
         end if 
                  
      else if ( rank == 2 ) then

         if ( size(1,1)%str /= ':' .and. size(2,1)%str /= ':' ) then
            if ( declar(ndecl)%alloc ) then
               msg = 'Allocatable array "'// varName//'" must have a deferred shape.'
            else
                               read(size(1,1)%str, *, iostat = err) num1
               if ( err == 0 ) read(size(2,1)%str, *, iostat = err) num2
               if ( err == 0 ) declar(ndecl)%size = [ num1, num2 ]
            end if
         else if ( size(1,1)%str == ':' .and. size(2,1)%str == ':' ) then
            err = 0
         else
            msg = 'Bad declaration for "'// varName//'" (1).'
         end if        

      else
         stat = err_t ( stat = UERROR, where = HERE, &
                       msg ='Rank > 2 (not yet implemented) in statement: "'//rec//'"' )
         return
      end if

      if ( err /= 0 ) msg = 'Bad declaration for "'// varName//'" (2).'
      
      if ( len_trim(msg) > 0 ) then
         msg = msg//' At line '//i2a(numl)//' of file "'//fileName//'": '
         stat = err_t( stat = UERROR, where = HERE, msg = msg // NLT // '"'//trim(rec)//'"' )
         return
      end if
               
   end do
       
   END SUBROUTINE functionDeclar 
   
   
!=============================================================================================
   SUBROUTINE functionHeader ( rec, stat )
!=============================================================================================
   character(len=*), intent(in    ) :: rec
   type     (err_t), intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Given the function interface (e.g. rec = '[x,y] = myfun(a,b,c)'), it extracts the function
!  name (funcName), the number (nvarIn, nvarOut) of the input and output variables and their 
!  names (varIn(:)%name, varOut(:)%name)
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'functionHeader'
   integer  (Ikind)              :: pos, lb, rb, i
   character(len=:), allocatable :: lhs, rhs, callingseq, signature
   type     (str_t), allocatable :: subRec(:,:)
!--------------------------------------------------------------------------------------------- 

   signature = adjustl(rec(len(KWDSTART)+1:))
   call util_getLhsNRhs ( signature, '=', lhs, rhs, pos )

   if ( pos /= 0 ) then
      lb = index(lhs,'[') ; rb = index(lhs,']',back=.true.) 
      
      if ( lb * rb /= 0 ) then
      
         nvarOut = util_CountTokens ( str = lhs(lb+1:rb-1), delims = ',', &
                                      BlkToken =.false., tokens = subRec, &
                                      stat = stat                         )
         error_TraceNreturn(stat, HERE)
                  
         call allocVark1 ( mode = 'i', t = varOut, n = nvarOut, stat = stat )
         error_TraceNreturn(stat, HERE)
                  
         do i = 1, nvarOut
            varOut(i)%name = subRec(i,1)%str 
         end do
      
      else if ( lb == 0 .and. rb == 0 ) then
      
         call allocVark1 ( mode = 'i', t = varOut, n = 1_Ikind, stat = stat )
         error_TraceNreturn(stat, HERE)
               
         nvarOut = 1
         varOut(1)%name = lhs
         
      else
         
         stat = err_t(stat = UERROR, where = HERE, msg = &
                      'Unbalanced brackets in expression "'//rec//'"' )
         return
      
      end if 
      
   else

      nvarOut = 0
      rhs = lhs        

   end if
   
   lb = index (rhs,'(') ; rb = index(rhs,')',back=.true.)
   
   if ( lb * rb /= 0 ) then
   
      funcName = trim(rhs(1:lb-1))
      nvarIn = util_CountTokens ( str = rhs(lb+1:rb-1), delims = ',', &
                                  BlkToken =.false., tokens = subRec, &
                                  stat = stat                         )
                                    
      call allocVark1 ( mode = 'i', t = varIn, n = nvarIn, stat = stat )
      error_TraceNreturn(stat, HERE)

      do i = 1, nvarIn
         varIn(i)%name = subRec(i,1)%str
      end do
                                
   else if ( lb == 0 .and. rb == 0 ) then
   
      funcName = rhs
      nvarIn = 0
      
   else
   
      stat = err_t(stat = UERROR, where = HERE, msg = &
                  'Unbalanced (1) parentheses in expression "'//rec//'"' )
      return
      
   end if   

   subrHeader = 's_'//funcName
   funcHeader = funcName
   callingseq = ''
   
   if ( nvarOut /= 0 .or. nvarIn /= 0 ) then
      subrHeader = subrHeader // ' ( '   
      funcHeader = funcHeader // ' ( '   
      callingseq = '( '
      do i = 1, nvarIn
         subrHeader = subrHeader // varIn(i)%name
         funcHeader = funcHeader // varIn(i)%name
         callingseq = callingseq // varIn(i)%name // '=inputs('//i2a(i)//')'
         if ( i < nvarIn ) then
            subrHeader = subrHeader // ', '
            funcHeader = funcHeader // ', '
            callingseq = callingseq // ', '
         end if
      end do
      if ( nvarOut > 0 .and. nvarIn > 0 ) then
         subrHeader = subrHeader // ', '
         callingseq = callingseq // ', '
      end if
      if ( nvarOut > 1 .and. nvarIn > 0 ) funcHeader = funcHeader // ', '
      
      do i = 1, nvarOut
         subrHeader = subrHeader // varOut(i)%name
         if ( i > 1 ) funcHeader = funcHeader // varOut(i)%name
         if ( i == 1 ) then
            callingseq = callingseq // varOut(i)%name // '=firstOut'
         else
            callingseq = callingseq // varOut(i)%name // '=r2('//i2a(i-1)//')'
         end if
         if ( i < nvarOut ) then
            subrHeader = subrHeader // ', '
            if ( i > 1 ) funcHeader = funcHeader // ', '
            callingseq = callingseq // ', '
         end if
      end do
      subrHeader = subrHeader // ' )'
      funcHeader = funcHeader // ' )' // ' result( '//varOut(1)%name//' )'
      callingseq = callingseq // ' )'
   end if
   
   listfun(nfunc,1) = funcName
   listfun(nfunc,2) = callingseq
   listfun(nfunc,3) = signature
   nargfun(nfunc,1) = nvarOut
   nargfun(nfunc,2) = nvarIn
   
   END SUBROUTINE functionHeader
   

!=============================================================================================
   SUBROUTINE writeSubroutine ( onlysub, stat )
!=============================================================================================
   logical    , intent(in    ) :: onlysub
   type(err_t), intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'writeSubroutine'
   character(len=:), allocatable :: str, name, ctype, charcode, shortname, s, s1, s2, &
                                    pname, tmpname
   integer  (Ikind)              :: i, j, typ
!--------------------------------------------------------------------------------------------- 

   do i = 1, nvarIn   
      varIn(i)%rank = -1   
      do j = 1, ndecl
         if ( varIn(i)%name == declar(j)%name ) then
            varIn(i)%typ   = declar(j)%typ 
            varIn(i)%rank  = declar(j)%rank
            varIn(i)%alloc = declar(j)%alloc
            varIn(i)%size  = declar(j)%size
            exit
         end if
      end do
      if ( varIn(i)%rank == -1 ) then
         stat = err_t( stat=UERROR, where=HERE,  msg='Declaration of input arg. "'//&
                       varIn(i)%name//'" of function "'//funcName//'" not found'  )
         return
      end if   
   end do
   
   do i = 1, nvarOut
      varOut(i)%rank = -1   
      do j = 1, ndecl
         if ( varOut(i)%name == declar(j)%name ) then
            varOut(i)%typ   = declar(j)%typ 
            varOut(i)%rank  = declar(j)%rank
            varOut(i)%alloc = declar(j)%alloc
            varOut(i)%size  = declar(j)%size
            exit
         end if
      end do
      if ( varOut(i)%rank == -1 ) then
         stat = err_t( stat=UERROR, where=HERE,  msg='Declaration of output arg. "'//&
                       varOut(i)%name//'" of function "'//funcName//'" not found'  )
         return
      end if   
   end do   

   write(uo,'("!",93("="))')  
   write(uo,'(3x,a)')'SUBROUTINE user_'//subrHeader
   write(uo,'("!",93("="))')  
   do i = 1, nline
      write(uo,'(3x,a)')body(i)%str
   end do
   write(uo,*)
   write(uo,'(3x,a)')'END SUBROUTINE user_s_'//funcName  
   write(uo,*)
   write(uo,*)
   
   !if ( nvarOut > 0 ) then
      write(uo,'("!",93("="))')
      write(uo,'(3x,a)')'SUBROUTINE '// subrHeader 
      write(uo,'("!",93("="))')     
   !else
     ! stat = err_t( stat = UERROR, where = HERE,  &
     !                msg = 'case nvarOut = 0 not yet implemented' )
     ! return
   !end if
      
   if ( nvarIn > 0 ) then
      str = 'type(pk2_t), target, intent(in    ) :: '
      do i = 1, nvarIn-1
         str = str // varIn(i)%name //', '
      end do
      str = str // varIn(nvarIn)%name
      write(uo,'(3x,a)') str
   end if
      
   if ( nvarOut > 0 ) then
      str = 'type(pk2_t),         intent(in out) :: '
      do i = 1, nvarOut-1
         str = str // varOut(i)%name //', '
      end do
      str = str // varOut(nvarOut)%name
      write(uo,'(3x,a)') str
   end if
   if ( nvarIn > 0 .or. nvarOut > 0 ) write(uo,'("!",93("-"))')
   
   write(uo,*)
   write(uo,'(a,1x,75("-"))')'!- local variables'          
   write(uo,'(3x,a)')'character(len=*), parameter :: HERE = "'//funcName//'"'
   
   do i = 1, nvarIn

      if ( varIn(i)%rank == 0 ) then
         str = ''
      else if ( varIn(i)%rank == 1 ) then
         str = '(:)'
      else if ( varIn(i)%rank == 2 ) then
         str = '(:,:)'    
      else 
         stat = err_t( stat=UERROR, where=HERE,  msg='arg. rank > 2 not yet implemented' )
         return
      end if
      
      typ = varIn(i)%typ
      
      if ( typ <= STYP ) then
         call typeCodesNnames ( typ, charcode, shortname, ctype )
      else if ( typ > STYP+100 ) then
         ctype = 'character(len='//i2a(typ-STYP-100)//')'
      end if
         
      write(uo,'(3x,a)')ctype // ', pointer :: p_'//varIn(i)%name//str//' => NULL()'
      write(uo,'(3x,a)')'type(pk2_t) :: '//varIn(i)%name//'tmp'
   end do 
   
   do i = 1, nvarOut

      if ( varOut(i)%rank == 0 ) then
         str = ''
      else if ( varOut(i)%rank == 1 ) then
         if ( varOut(i)%size(1) == -1 ) then
            str = '(:)'
         else
            str = '('//i2a(varOut(i)%size(1))//')'
         end if
      else if ( varOut(i)%rank == 2 ) then
         if ( varOut(i)%size(1) == -1 ) then
            str = '(:,:)'
         else
            str = '('//i2a(varOut(i)%size(1))//','//i2a(varOut(i)%size(2))//')'
         end if
      else 
         stat = err_t( stat=UERROR, where=HERE,  msg='arg. rank > 2 not yet implemented' )
         return
      end if   

      typ = varOut(i)%typ
      
      if ( typ <= STYP ) then
         call typeCodesNnames ( typ, charcode, shortname, ctype )
      else if ( typ > STYP+100 ) then
         ctype = 'character(len='//i2a(typ-STYP-100)//')'
      end if
      
      if ( varOut(i)%alloc ) ctype = ctype // ', allocatable, save'
      
      write(uo,'(3x,a)')ctype // ' :: r_'//varOut(i)%name//str
   end do  

   write(uo,'("!",93("-"))')  

   write(uo,*)
   
   do i = 1, nvarIn
      name = varIn(i)%name
      
      ! Check the shape of the input variable #i:
      if ( varIn(i)%rank == 1 .and. varIn(i)%size(1) > 0 ) then
         s1 = i2a(varIn(i)%size(1))
         str = util_OrdinalAbbrev(i) // " argument of '" // funcName // "'" // &
               " ("//name//") must be a " // s1 // "x1 or a 1x"// s1 // " array"
         write(uo,'(3x,a)')'if ( '//name//'%nrow * '//name//'%ncol /= '//s1//' ) then'
         write(uo,'(3x,a)')'   call opflag%set ( UERROR,HERE,"'//str//'")'
         write(uo,'(3x,a)')'   return'
         write(uo,'(3x,a)')'end if'   
         write(uo,*)
      else if ( varIn(i)%rank == 2 .and. varIn(i)%size(1) >= 0 .and. varIn(i)%size(1) >= 0 ) then
         s1 = i2a(varIn(i)%size(1)) ; s2 = i2a(varIn(i)%size(2))
         str = util_OrdinalAbbrev(i) // " argument of '" // funcName // "'" // &
               " ("//name//") must be a " // s1 // "x" // s2 // " array"
         write(uo,'(3x,a)')'if ( '//name//'%nrow /= '// s1 // &
                          ' .or. '//name//'%ncol /=' // s2 // ' ) then'
         write(uo,'(3x,a)')'   call opflag%set ( UERROR,HERE,"'//str//'")'
         write(uo,'(3x,a)')'   return'
         write(uo,'(3x,a)')'end if'   
         write(uo,*)
      end if

      ! Check the type of the i-th input variable:
      
      ! the expected type (as specified by the user):
      typ = varIn(i)%typ      
      if ( typ <= STYP ) then
         call typeCodesNnames ( typ, charcode, shortname, ctype )
      else if ( typ > STYP+100 ) then
         charcode = 'STYP'
         shortname = 'string'
      end if
      
      ! use a converted temporary copy (name//'tmp') when the expected type is:
      ! - complex but the received variable is real or integer
      ! - real but the the received variable is integer
      ! otherwise if the types do not match return with an error
   
      str = util_OrdinalAbbrev(i) // " argument of '" // funcName // "'" // &
            " ("//name//") must be of "
            
      pname = "p_"//name
      if ( typ == CTYP ) then
         tmpname = name//"tmp"
         write(uo,'(3x,a)')'select case ( '//name//'%typ )'
         write(uo,'(3x,a)')'   case ( CTYP ) '
         write(uo,'(3x,a)')'      call '//name//'%pointer ( '//pname//', stat = opflag )'
         write(uo,'(3x,a)')'   case ( ITYP, RTYP ) '
         write(uo,'(3x,a)')'      call pk2_assign ( '//tmpname//' ,'//name//' )'
         write(uo,'(3x,a)')'      call bk2_convertToCk2 ( '//tmpname//'%m ) ; ' // &
                                                           tmpname//'%typ = CTYP'
         write(uo,'(3x,a)')'      call '//tmpname//'%pointer ( '//pname//', stat=opflag )'
         write(uo,'(3x,a)')'   case default'
         write(uo,'(3x,a)')'      call opflag%set ( UERROR,HERE,"'//str // &
                                                   'complex, real or integer type" )'
         write(uo,'(3x,a)')"end select"
      else if ( typ == RTYP ) then
         tmpname = name//"tmp"
         write(uo,'(3x,a)')'select case ( '//name//'%typ )'
         write(uo,'(3x,a)')'   case ( RTYP ) '
         write(uo,'(3x,a)')'      call '//name//'%pointer ( '//pname//', stat = opflag )'
         write(uo,'(3x,a)')'   case ( ITYP ) '
         write(uo,'(3x,a)')'      call pk2_assign ( '//tmpname//' ,'//name//' )'
         write(uo,'(3x,a)')'      call bk2_convertToRk2 ( '//tmpname//'%m ) ; ' // &
                                                      tmpname//'%typ = RTYP'
         write(uo,'(3x,a)')'      call '//tmpname//'%pointer ( '//pname//', stat=opflag )'
         write(uo,'(3x,a)')'   case default'
         write(uo,'(3x,a)')'      call opflag%set ( UERROR,HERE, "'//str// &
                                                   'real or integer type" )'    
         write(uo,'(3x,a)')'end select'   
      else
         write(uo,'(3x,a)')'if ( '//name//'%typ /= '//charcode//' ) then'
         write(uo,'(3x,a)')'   call opflag%set ( UERROR,HERE,"'//str//shortname//' type" )'  
         write(uo,'(3x,a)')'else'
         write(uo,'(3x,a)')'   call '//name//'%pointer ( '//pname//', stat = opflag )'
         write(uo,'(3x,a)')'end if'
      end if
      
      write(uo,*)
      write(uo,'(3x,a)')'if ( opflag /= 0 ) then ; '//pname//' => NULL() ; return ; end if'
      write(uo,*)
   end do
   
   
   s = ""
   do i = 1, nvarIn
      name = varIn(i)%name
      if ( i == 1 ) then
         s = "p_"//name
      else
         s = s // ", p_"//name
      end if
   end do
      
   str = s
   do i = 1, nvarOut
      name = varOut(i)%name
      if ( len_trim(str) == 0 ) then
         str = "r_"//name
      else
         str = str // ", r_"//name
      end if
   end do
   write(uo,'(3x,a)')"call user_s_"//funcName//" ( "//str//" )"
   
   write(uo,*)
   
   do i = 1, nvarOut
      
      name = varOut(i)%name
      
      if ( varOut(i)%alloc  .and. varOut(i)%rank == 2 ) then
         str = "call pk2_movealloc ( from = r_" // name // ", to = "
      else
         str = "call pk2_assign ( rhs = r_" // name // ", lhs = "
      end if

      write(uo,'(3x,a)')str // name // " )"
               
   end do
         
   !do i = 1, nvarIn
   !   write(uo,'(3x,a)')"p_"//varIn(i)%name//" => NULL()"
   !end do
   if ( nvarIn > 0 ) write(uo,'(/,3x,a,/)') "nullify ( "// s //" )"

   write(uo,'(3x,a)')'END SUBROUTINE s_'//funcName
   write(uo,*)
   write(uo,*)
   
   if ( .not. onlysub ) then
      if ( nvarOut > 0 ) then
      write(uo,'("!",93("="))')  
      write(uo,'(3x,a)')'FUNCTION '// funcHeader
      write(uo,'("!",93("="))')  
      if ( nvarIn > 0 ) then
         str = 'type(pk2_t), target, intent(in    ) :: '
         do i = 1, nvarIn-1
            str = str // varIn(i)%name //', '
         end do
         str = str // varIn(nvarIn)%name
         write(uo,'(3x,a)') str
      end if
      
      if ( nvarOut > 1 ) then
         str = 'type(pk2_t),         intent(in out) :: '
         do i = 2, nvarOut-1
            str = str // varOut(i)%name //', '
         end do
         str = str // varOut(nvarOut)%name
         write(uo,'(3x,a)') str
         write(uo,'(3x,a)') 'type(pk2_t) :: '//varOut(1)%name
      else if ( nvarOut == 1 ) then
         write(uo,'(3x,a)') 'type(pk2_t) :: '//varOut(1)%name
      end if
      if ( nvarIn > 0 .or. nvarOut > 0 ) write(uo,'("!",93("-"))')  
      write(uo,*)        
      write(uo,'(3x,a)')'call '//subrHeader
      write(uo,*)        
      write(uo,'(3x,a)')'END FUNCTION '// funcName
      write(uo,*)          
      write(uo,*)          
      end if
   end if
   
   END SUBROUTINE writeSubroutine 

         
!=============================================================================================
   SUBROUTINE writeModuleHeaderAndModuleEnd ( onlysub, stat )
!=============================================================================================
   logical    , intent(in    ) :: onlysub
   type(err_t), intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 
 
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'writeModuleHeaderAndModuleEnd'
   character(LGSTR)              :: line
   character(len=:), allocatable :: str
   integer  (Ikind)              :: i, n
!--------------------------------------------------------------------------------------------- 

   call get_command ( line )
   
   write(uof,'(a)')'! '   
   write(uof,'(a)')'! This file was auto created by addfun from the user file '//fileName
   write(uof,'(a)')'! with the command:'
   write(uof,'(a)')'! '   
   write(uof,'(a)')'!      '//trim(line)
   write(uof,'(a)')'! '   
   
   write(uof,'(a)')'MODULE userfunc_m'
   write(uof,'(3x,a)')'use pk2_m'
   write(uof, *   )
   write(uof,'(3x,a)')'implicit none'
   write(uof, *   )
   write(uof,'(3x,a)')'!private'
   write(uof,'(3x,a)')'!public :: userProcList, userProcDriver, s_userProcDriver'
   write(uof, *   )
   write(uof, *   )
   write(uof,'(3x,a)')'interface s_userProcDriver'
   write(uof,'(3x,a)')'   module procedure usersubr_driver1, usersubr_driver2'
   write(uof,'(3x,a)')'end interface'
   write(uof, *   )
   write(uof,'(3x,a)')'interface userProcDriver'
   write(uof,'(3x,a)')'   module procedure userfunc_driver1, userfunc_driver2'
   write(uof,'(3x,a)')'end interface'
   write(uof, *   )

   i = maxval(nargfun(:,1))
   write(uof,'(3x,a)')'integer(Ikind), parameter :: maxOptionalOutputs = '//i2a(i)
     
   write(uof, *   )
   write(uof, *   )
     
   write(uof,'(a)')'CONTAINS'
   write(uof,*)
   write(uof,'("!",93("="))')  
   write(uof,'(3x,a)')'SUBROUTINE userProcList ( nfunc, funcList, nbrOptOutputs )'
   write(uof,'("!",93("="))')  
   write(uof,'(3x,a)')'integer(Ikind),                        intent(out) :: nfunc'
   write(uof,'(3x,a)')'type   (str_t), optional, allocatable, intent(out) :: funcList(:)'
   write(uof,'(3x,a)')'integer(Ikind), optional, allocatable, intent(out) :: nbrOptOutputs(:)'
   write(uof,'("!",93("-"))')  
   write(uof,*)
   write(uof,'(3x,a)')'nfunc = '//i2a(nfunc)
   write(uof,'(3x,a)')'if ( present(funcList) ) then'
   write(uof,'(3x,a)')'   allocate(funcList(nfunc))'
   do i = 1, nfunc
      write(uof,'(3x,a)')'   funcList('//i2a(i)//')%str = "'//listfun(i,1)%str//'"'
   end do
   write(uof,'(3x,a)')'end if'
   write(uof,*)
   write(uof,'(3x,a)')'if ( present(nbrOptOutputs) ) then'
   write(uof,'(3x,a)')'   allocate(nbrOptOutputs(nfunc))'
   do i = 1, nfunc
      write(uof,'(3x,a)')'   nbrOptOutputs('//i2a(i)//') = '//i2a(nargfun(i,1)-1)
   end do
   write(uof,'(3x,a)')'end if'
   write(uof,*)
   write(uof,'(3x,a)')'END SUBROUTINE userProcList'  
   write(uof,*)
   write(uof,*)

   write(uof,'("!",93("="))')  
   write(uof,'(3x,a)')'SUBROUTINE usersubr_driver1 ( fname, inputs, firstOut, optOuts, nOpt )'
   write(uof,'("!",93("="))')  
   write(uof,'(3x,a)')'character(len=*),           intent(in    ) :: fname'
   write(uof,'(3x,a)')'type     (pk2_t), target,   intent(in    ) :: inputs(:)'
   write(uof,'(3x,a)')'type     (pk2_t)          , intent(in out) :: firstOut'
   write(uof,'(3x,a)')'type     (pk2_t), optional, intent(in out) :: optOuts(:)'
   write(uof,'(3x,a)')'integer  (Ikind), optional, intent(   out) :: nOpt'   
   write(uof,'("!",93("-"))')  
   write(uof,*)
   write(uof,'(a,1x,75("-"))')'!- local variables'          
   write(uof,'(3x,a)')'character(len=*), parameter :: HERE = "usersubr_driver1"'
   write(uof,'(3x,a)')'type     (pk2_t)            :: r2(maxOptionalOutputs)'
   write(uof,'(3x,a)')'integer  (Ikind)            :: i, n'
   write(uof,'("!",93("-"))')  
   write(uof,*)
   write(uof,'(3x,a)')'select case ( fname )'
      
   do i = 1, nfunc
      str = i2a(nargfun(i,2))
      write(uof,'(3x,a)')'   case ( "'//listfun(i,1)%str//'" )'
      write(uof,'(3x,a)')'      if ( size(inputs) < '//str//' ) then'
      write(uof,'(3x,a)')'         call opflag%set ( UERROR, HERE, &'
      write(uof,'(3x,a)')'              "The function ' // listfun(i,1)%str // ' needs '// &
                                         str // ' input arguments" // NLT // &'
      write(uof,'(3x,a)')'              "Signature of this function: '//listfun(i,3)%str//'")'
      write(uof,'(3x,a)')'         return'
      write(uof,'(3x,a)')'      end if'

      write(uof,'(3x,a)')'      call s_'//listfun(i,1)%str//listfun(i,2)%str
      n = nargfun(i,1)-1
      str = i2a(n)
   
      write(uof,*)
      if ( n > 0 ) then
         write(uof,'(3x,a)')'      if ( present(optOuts) ) then'
         write(uof,*)
         write(uof,'(3x,a)')'         if ( size(optOuts) < 1 ) then'
         write(uof,'(3x,a)')'            call opflag%set ( UERROR, HERE, msg = & ' 
         write(uof,'(20x,a)')'"Size of the optional outputs (optOuts array) must be at least 1" )'
         write(uof,'(3x,a)')'            return'
         write(uof,'(3x,a)')'         end if'
         write(uof,*)
         if ( n == 1 ) then
            write(uof,'(3x,a)')'         call pk2_movealloc(from=r2(1), to=optOuts(1))'
            write(uof,'(3x,a)')'         if ( present(nOpt) ) nOpt = 1'
         else 
            write(uof,'(3x,a)')'         n = min('//str//',size(optOuts))'
            write(uof,'(3x,a)')'         do i = 1, n'
            write(uof,'(3x,a)')'             call pk2_movealloc(from=r2(i), to=optOuts(i))'
            write(uof,'(3x,a)')'         end do'
            write(uof,'(3x,a)')'         if ( present(nOpt) ) nOpt = n'
         end if
         write(uof,*)
         write(uof,'(3x,a)')'      else'
         write(uof,'(3x,a)')'         if ( present(nOpt) ) nOpt = 0'
         write(uof,'(3x,a)')'      end if'
      else
         write(uof,'(3x,a)')'      if ( present(nOpt) ) nOpt = 0'
      end if
      write(uof,*)
   end do
   write(uof,'(3x,a)')'   case default'
   str = '"Unknown user function << " // fname // " >>"'
   write(uof,'(3x,a)')'      call opflag%Set(UERROR,HERE,'//str//')'
   write(uof,'(3x,a)')'end select'
   write(uof,*)
   write(uof,'(3x,a)')'END SUBROUTINE usersubr_driver1'  
   write(uof,*)
   write(uof,*)

   write(uof,'("!",93("="))')  
   write(uof,'(3x,a)')'SUBROUTINE usersubr_driver2 ( ifunc, inputs, firstOut, optOuts, nOpt )'
   write(uof,'("!",93("="))')  
   write(uof,'(3x,a)')'integer(Ikind),           intent(in    ) :: ifunc'
   write(uof,'(3x,a)')'type   (pk2_t), target,   intent(in    ) :: inputs(:)'
   write(uof,'(3x,a)')'type   (pk2_t),           intent(in out) :: firstOut'
   write(uof,'(3x,a)')'type   (pk2_t), optional, intent(in out) :: optOuts(:)'
   write(uof,'(3x,a)')'integer(Ikind), optional, intent(   out) :: nOpt'   
   write(uof,'("!",93("-"))')  
   write(uof,*)
   write(uof,'("!- local variables ",75("-"))')      
   write(uof,'(3x,a)')'character(len=*), parameter :: HERE = "usersubr_driver2"'
   write(uof,'(3x,a)')'type     (pk2_t)            :: r2(maxOptionalOutputs)'
   write(uof,'(3x,a)')'integer  (Ikind)            :: i, n'
   write(uof,'("!",93("-"))')  
   write(uof,*)
   write(uof,'(3x,a)')'select case ( ifunc )'
   
   do i = 1, nfunc
   
      str = i2a(nargfun(i,2))
      write(uof,'(3x,a)')'   case ( '//i2a(i)//' )'
      write(uof,'(3x,a)')'      if ( size(inputs) < '//str//' ) then'
      write(uof,'(3x,a)')'         call opflag%set ( UERROR, HERE, &'
      write(uof,'(3x,a)')'              "The function ' // listfun(i,1)%str // ' needs '// &
                                         str // ' input arguments" // NLT // &'
      write(uof,'(3x,a)')'              "Signature of this function: '//listfun(i,3)%str//'")'
      write(uof,'(3x,a)')'         return'
      write(uof,'(3x,a)')'      end if'

      write(uof,'(3x,a)')'      call s_'//listfun(i,1)%str//listfun(i,2)%str
      n = nargfun(i,1)-1
      str = i2a(n)

      write(uof,*)
      if ( n > 0 ) then
         write(uof,'(3x,a)')'      if ( present(optOuts) ) then'
         write(uof,*)
         write(uof,'(3x,a)')'         if ( size(optOuts) < 1 ) then'
         write(uof,'(3x,a)')'            call opflag%set ( UERROR, HERE, msg = & ' 
         write(uof,'(20x,a)')'"Size of the optional outputs (optOuts array) must be at least 1" )'
         write(uof,'(3x,a)')'            return'
         write(uof,'(3x,a)')'         end if'
         write(uof,*)
         if ( n == 1 ) then
            write(uof,'(3x,a)')'         call pk2_movealloc(from=r2(1), to=optOuts(1))'
            write(uof,'(3x,a)')'         if ( present(nOpt) ) nOpt = 1'
         else 
            write(uof,'(3x,a)')'         n = min('//str//',size(optOuts))'
            write(uof,'(3x,a)')'         do i = 1, n'
            write(uof,'(3x,a)')'             call pk2_movealloc(from=r2(i), to=optOuts(i))'
            write(uof,'(3x,a)')'         end do'
            write(uof,'(3x,a)')'         if ( present(nOpt) ) nOpt = n'
         end if
         write(uof,*)
         write(uof,'(3x,a)')'      else'
         write(uof,'(3x,a)')'         if ( present(nOpt) ) nOpt = 0'
         write(uof,'(3x,a)')'      end if'
      else
         write(uof,'(3x,a)')'      if ( present(nOpt) ) nOpt = 0'
      end if
      write(uof,*)      
   end do
   write(uof,'(3x,a)')'   case default'
   str = '"Unknown user function ID ("//util_intToChar(ifunc)//")"'
   write(uof,'(3x,a)')'      call opflag%Set(UERROR,HERE,'//str//')'
   write(uof,'(3x,a)')'end select'
   write(uof,*)
   write(uof,'(3x,a)')'END SUBROUTINE usersubr_driver2'  
   write(uof,*)
   write(uof,*)      

   
   if ( .not. onlysub ) then
      write(uof,'("!",93("="))')  
      write(uof,'(3x,a)')'FUNCTION userfunc_driver1 ( fname, inputs, optOuts, nOpt )' // &
                      ' result( firstOut )'
      write(uof,'("!",93("="))')  
      write(uof,'(3x,a)')'character(len=*),           intent(in    ) :: fname'
      write(uof,'(3x,a)')'type     (pk2_t), target,   intent(in    ) :: inputs(:)'
      write(uof,'(3x,a)')'type     (pk2_t), optional, intent(in out) :: optOuts(:)'
      write(uof,'(3x,a)')'integer  (Ikind), optional, intent(   out) :: nOpt'   
      write(uof,'(3x,a)')'type     (pk2_t)                           :: firstOut'
      write(uof,'("!",93("-"))')  
      write(uof,*)
      write(uof,'(3x,a)')'call usersubr_driver1 ( fname, inputs, firstOut, optOuts, nOpt )'
      write(uof,*)
      write(uof,'(3x,a)')'END FUNCTION userfunc_driver1'
      write(uof,*)
      write(uof,*)
      write(uof,'("!",93("="))')  
      write(uof,'(3x,a)')'FUNCTION userfunc_driver2 ( ifunc, inputs, optOuts, nOpt )' // &
                      ' result( firstOut )'
      write(uof,'("!",93("="))')  
      write(uof,'(3x,a)')'integer  (Ikind),           intent(in    ) :: ifunc'
      write(uof,'(3x,a)')'type     (pk2_t), target,   intent(in    ) :: inputs(:)'
      write(uof,'(3x,a)')'type     (pk2_t), optional, intent(in out) :: optOuts(:)'
      write(uof,'(3x,a)')'integer  (Ikind), optional, intent(   out) :: nOpt'   
      write(uof,'(3x,a)')'type     (pk2_t)                           :: firstOut'
      write(uof,'("!",93("-"))')  
      write(uof,*)
      write(uof,'(3x,a)')'call usersubr_driver2 ( ifunc, inputs, firstOut, optOuts, nOpt )'
      write(uof,*)
      write(uof,'(3x,a)')'END FUNCTION userfunc_driver2'
      write(uof,*)
      write(uof,*)
   end if

   rewind(uo)
   
   do 
      read(uo,'(a)',end=1) line
      write(uof,'(a)')trim(line)
   end do
   
1   write(uof,'(a)')'END MODULE userfunc_m'

   END SUBROUTINE writeModuleHeaderAndModuleEnd


!=============================================================================================
   SUBROUTINE allocVark1 ( mode, t, n, stat )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode
   integer  (Ikind),              intent(in    ) :: n
   type     (var_t), allocatable, intent(in out) :: t(:)
   type     (err_t),              intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 
 
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'allocVark1'
   character(len=:), allocatable :: msg
   integer  (Ikind)              :: nn, err
   type     (var_t), allocatable :: tmp(:)
!--------------------------------------------------------------------------------------------- 

   err = 0
   
   if (.not. allocated(t)) then

      allocate(t(n), stat=err)
      if (err /= 0) msg = "Allocation failure for array 't' [1]"        

   else if (mode == 'a') then

      if (size(t) /= n) then
         deallocate(t)
         allocate(t(n), stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [2]"  
      else
         return
      end if      
      
   else if (mode == 'e') then

      if (size(t) /= n) then
         deallocate(t)
         allocate(t(n), stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [3]"  
      else
         return
      end if      
           
   else if (mode == 's') then                      
   
      allocate(tmp(n), stat=err)
      if (err /= 0) then
         msg = "Allocation failure for array 'tmp'"       
      else   
         nn = min(n,size(t,kind=Ikind))
         tmp(1:nn) = t(1:nn)
         call move_alloc (from = tmp, to = t)
      end if
      
   else if (mode == 'i') then
   
      if (size(t) < n) then
         deallocate(t)
         allocate(t(n), stat=err)
         if (err /= 0) msg = "Allocation failure for array 't' [4]"
      else
         return
      end if  

   else
      call stat%Set ( UERROR, HERE,                                                      &
                     "Invalid mode '" // mode // "' (must be 'a', 'e', 's' or 'i')" )
      return
   end if    
   
   if (err /= 0) then   
      call stat%Set ( UERROR, HERE, msg )
      return
   end if      
   
   END SUBROUTINE allocVark1  
   
!=============================================================================================   
   SUBROUTINE typeCodesNnames ( code, charcode, shortname, longname )
!=============================================================================================   
   integer  (Ikind),              intent(in)  :: code
   character(len=:), allocatable, intent(out) :: charcode, shortname, longname
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

   select case ( code )
      case ( ITYP ) 
         charcode  = 'ITYP'
         shortname = 'integer'
         longname  = 'integer(Ikind)'
      case ( RTYP ) 
         charcode  = 'RTYP'
         shortname = 'real'
         longname  = 'real(Rkind)'
      case ( CTYP ) 
         charcode  = 'CTYP'
         shortname = 'complex'
         longname  = 'complex(Rkind)'
      case ( LTYP ) 
         charcode  = 'LTYP'
         shortname = 'logical'
         longname  = 'logical'
      case ( STYP ) 
         charcode = 'STYP'
         shortname = 'str_t'
         longname  = 'type(str_t)'
      case ( STYP+100:)
         charcode = ''
         shortname = 'character'
         longname  = 'character'
      case default
         charcode  = ''
         shortname = ''
         longname  = ''
   end select
   
   END SUBROUTINE typeCodesNnames
   

!=============================================================================================
   SUBROUTINE resume
!=============================================================================================

!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind) :: i
!--------------------------------------------------------------------------------------------- 
   
   if ( nfunc == 1 ) then
      write(STDOUT,'(a)') &
         'The function '//listfun(1,1)%str//' was added to the library '//libName
   else
      write(STDOUT,'(a)') &
         'The following '//i2a(nfunc)//' functions were added to the library '//libName//': '
      do i = 1, nfunc
         write(STDOUT,'(a)')'. '//listfun(i,1)%str
      end do
   end if

   END SUBROUTINE resume
   
   
!=============================================================================================
   SUBROUTINE compilLib ( dynlib, delf90, stat )
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only : compiler_version
   logical    , intent(in    ) :: dynlib, delf90
   type(err_t), intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!
!--------------------------------------------------------------------------------------------- 

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'compilLib'
   character(len=:), allocatable :: msg, usedCompiler, flags, flagdyn, buf, &
                                    pk2dir, dirmod, dirlib, dirobj, f90file, objfile
   character(LGSTR)              :: iomsg
   integer                       :: uc, err, estat, endf
!--------------------------------------------------------------------------------------------- 

   f90file = libName //'.f90'
   objfile = libName //'.o'
!
!- Use the same compiler that was used to compile this program:
!
#ifdef __COMP
   usedCompiler = __COMP
#else
   usedCompiler = util_stringLow(compiler_version())
   if ( index(usedCompiler,'nag') > 0 ) then
      usedCompiler = 'nagfor'
   else if ( index(usedCompiler,'gcc') > 0 .or. index(usedCompiler,'gfortran') > 0) then
      usedCompiler = 'gfortran'
   else if ( index(usedCompiler,'intel') > 0 ) then
      usedCompiler = 'ifort'
   end if
#endif

   if ( usedCompiler == 'nagfor' ) then
      flags = ' -fpp -kind=byte -ieee=full '
      if ( len_trim(moduledir) > 0 ) flags = flags // '-mdir '//moduledir//' '
      flagdyn = ' -PIC -Wl,-shared '
   else if ( usedCompiler == 'ifort' ) then
      flags = ' -fpp '
      if ( len_trim(moduledir) > 0 ) flags = flags // '-module '//moduledir//' '
      flagdyn = ' -fPIC -shared '
   else if ( usedCompiler == 'gfortran' ) then
      flags = ' -cpp '
      if ( len_trim(moduledir) > 0 ) flags = flags // '-J '//moduledir//' '
      flagdyn = ' -fPIC -shared '
   else
      stat = err_t(stat=UERROR, msg="Unknown compiler '"//usedCompiler// &
                  "'. Please update addfun.f90 with your compiler.", where=here)
      return
   end if
!
!- Absolute path of the pk2 installation directory (the pk2 library must be compiled with
!  the -DDIR option):
!
#ifdef __DIR
   pk2dir = __DIR
#else
   stat = err_t(stat=UERROR, msg="pk2 should be compiled with -DDIR flag", where=here)
   return
#endif

   dirmod = pk2dir // '/mod/' // usedCompiler
   dirlib = pk2dir // '/lib/' // usedCompiler
   dirobj = pk2dir // '/obj/' // usedCompiler
   flags = flags // ' -c -O3 -I ' // dirmod // ' -L' // dirlib //' -lpk2 -llapack -lblas '
!
!- if exists delete the obj libName.o:
!
   open(newunit=uc, iostat=err, file=objfile, status='old')
   if (err == 0) close(uc, status='delete')
!           
!- compile libName.f90 with the compiler given in default(1)
!            
   buf = usedCompiler//' '//flags//' '//f90file//" -o "//objfile//" 1> .tmp_comp_report 2>&1"

   estat = 0 ; err = 0
   
   call execute_command_line (trim(buf), exitstat = estat, cmdstat = err, cmdmsg = iomsg)
   
   if (err > 0) then
      msg = "system command ('"//trim(buf)//"') failed with error: "//NLT//trim(iomsg)
      stat = err_t(stat=IERROR, msg=msg, where=here)
      return
   else if (err < 0) then
      msg = "command execution not supported (needed for compilation of "//f90file
      stat = err_t(stat=IERROR, msg=msg, where=here)
      return
   else
      !!write(*,'(/,a,/)')trim(buf)//" completed with status: "//i2a(estat)
      if (estat /= 0) then
         open(newunit = uc,file='.tmp_comp_report')         
         msg = 'Error during compilation of '//f90file//NLT//'Compilation report:'
         do       
            read(uc,'(a)',iostat=endf) buf ; if (endf /= 0) exit 
            msg = msg // NLT // trim(buf)
         end do    
         close(uc,status='delete')
         msg = msg // NLT // 'End compilation report.'
         stat = err_t(stat=UERROR, msg=msg, where=here)
         return
      end if  
   end if 
      
   open(newunit=uc, iostat=estat, file='.tmp_comp_report')
   if (estat == 0) close(uc, status='delete')
   
   if ( dynlib ) then
      libName = libName // '.so'
!
!-    if exists delete the library libName//'.so':
!
      open(newunit=uc, iostat=estat, file=libName, status='replace')
      if (estat == 0) close(uc, status='delete')
!
!-    make now the dynamic shared library:
!         
      buf = usedCompiler // flagdyn // objfile // " "//dirobj//"/*.o -lblas -llapack "// &
            "-o "//libName//" 1> .tmp_comp_report 2>&1"

      estat = 0 ; err = 0

      call execute_command_line (trim(buf), exitstat = estat, cmdstat = err, cmdmsg = iomsg)
      
      if (err > 0) then
         msg = "system command ('"//trim(buf)//"') failed with error: "//NLT//trim(iomsg)
         stat = err_t(stat=IERROR, msg=msg, where=here)
         return
      else if (err < 0) then
         msg = "command execution not supported (compilation of "//libName
         stat = err_t(stat=IERROR, msg=msg, where=here)
         return
      else
         if (estat /= 0) then
            open(newunit=uc,file='.tmp_comp_report')  
            msg = 'Error while making the '// libName // NLT // 'Compilation report:'
            do       
               read(uc,'(a)',iostat=endf) buf ; if (endf /= 0) exit         
               msg = msg // NLT // trim(buf)
            end do    
            close(uc,status='delete')
            msg = msg // NLT // 'End compilation report.'
            stat = err_t(stat=UERROR, msg=msg, where=here)
            return
         end if              
      end if
      
      open(newunit=uc, iostat=estat, file='.tmp_comp_report')
      if (estat == 0) close(uc, status='delete')   
         
   else
      libName = libName // '.a'
!
!-    if exists delete the library libName//'.a':
!
      open(newunit=uc, iostat=estat, file=libName)
      if (estat == 0) close(uc, status='delete')   
!
!-    make now the static library:
!         
      buf = "/usr/bin/ar cr "//libName//"  " // dirobj // "/*.o " // objfile // &
            " 1> .tmp_comp_report 2>&1"// " ; "// &
            "/usr/bin/ranlib "//libName //" 1> .tmp_comp_report 2>&1"

      estat = 0 ; err = 0

      call execute_command_line (trim(buf), exitstat = estat, cmdstat = err, cmdmsg = iomsg)

      if (err > 0) then
         msg = "system command ('"//trim(buf)//"') failed with error: "//NLT//trim(iomsg)
         stat = err_t(stat=IERROR, msg=msg, where=here)
         return
      else if (err < 0) then
         msg = "command execution not supported (compilation of "//libName
         stat = err_t(stat=IERROR, msg=msg, where=here)
         return
      end if

   end if
   
   open(newunit = uc, file = '.tmp_comp_report') ; close(uc, status='delete')
   open(newunit = uc, file = objfile)            ; close(uc, status='delete')
   !open(newunit = uc, file= 'userfunc_m.mod')    ; close(uc, status='delete')
   
   if ( delf90 ) then
      open(newunit = uc, file = f90file)         ; close(uc, status='delete')
   end if
   
   END SUBROUTINE compilLib
   
   
!=============================================================================================   
   SUBROUTINE getCommandArg ( stat, fileIn, fileOut, dynamic, delf90, onlysub )
!=============================================================================================   
   type     (err_t),              intent(in out) :: stat
   character(len=:), allocatable, intent(   out) :: fileIn, fileOut
   logical         ,              intent(   out) :: dynamic, delf90, onlysub
!---------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------
!  Gets the command line arguments (retrieve user-entered options)
!---------------------------------------------------------------------------------- RH 06/22 -
   
!- local variables: --------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'getCommandArg'
   integer                       :: i, narg
   character(len=:), allocatable :: opt, typelib
   character(LGSTR)              :: msg
   logical                       :: help = .false.
!---------------------------------------------------------------------------------------------
   
   fileIn = '' ; fileOut = '' ; moduledir = ''
   dynamic = .false. ; delf90 = .false. ; onlysub = .false.
!
!- get the command line arguments:
!      
   narg = command_argument_count()

   if ( narg == 0 ) help = .true.
   
   i = 1
   do while ( i <= narg )   
      call get_command_argument( i, msg )
      opt = trim(adjustl(msg))
      select case ( opt )
         case ( '-i' )
            i = i + 1      
            call get_command_argument( i, msg )
            fileIn = trim(adjustl(msg))
         case( '-o' )
            i = i + 1      
            call get_command_argument( i, msg )
            fileOut = trim(adjustl(msg))
         case( '-l' )
            i = i + 1
            call get_command_argument( i, msg )
            typelib = trim(adjustl(msg))
            if ( typelib == 'dynamic' ) then
               dynamic = .true.
            else if ( typelib /= 'static' ) then 
               stat = err_t( stat=UERROR, where=HERE, &
                     msg="Valid values with -l option are static (default) and dynamic")
               return
            end if
         case ( '-mdir' )
            i = i + 1
            call get_command_argument( i, msg )
            moduledir = trim(adjustl(msg))
         case( '-d' )
            delf90 = .true.
         case( '-h' )
            help = .true.
         case( '-s' )
            onlysub = .true.
         case default
            stat = err_t( stat=UERROR, where=HERE, msg="Unknown option '"//opt//"'")
            return
      end select
      i = i + 1
   end do
   
   if ( help ) then
      write(STDOUT,*)
      write(STDOUT,'(a)')'Adds user functions to the pk2 library'
      write(STDOUT,*)
      write(STDOUT,'(a)')'Usage:'
      write(STDOUT,*)
      write(STDOUT,'(7x,a)')'addfun -i inFile [-o outFile] [-l libtype] [-mdir dirmod] [-d] [-h]'
      write(STDOUT,*)
      write(STDOUT,'(2x,a)')'where'
      write(STDOUT,*)
      
      write(STDOUT,'(2x,a,/)') &
         '. -i (required): is used to give the input file name'
      write(STDOUT,'(2x,a,/)') &
         '. -o (optional): is used for the name of the library (default: outFile = $inFile)'
      write(STDOUT,'(2x,a)')  &
         '. -l (optional): is used to set the library type. Valid values are static and '
      write(STDOUT,'(19x,a)')  &
         'dynamic. (default: static). The resulting library will be'
      write(STDOUT,'(19x,a,/)') &
         '$outFile.a or $outFile.so'
      write(STDOUT,'(2x,a,/)') &
         '. -mdir (optional): for choosing the destination directory of the '
      write(STDOUT,'(19x,a,/)') &
         'module file userfunc_m.mod'
      write(STDOUT,'(2x,a,/)') &
         '. -d (optional): after compilation, delete outFile.f90'
      write(STDOUT,'(2x,a,/)') &
         '. -h (optional): prints this message'
      
      write(STDOUT,'(a,/)')'Desciption: '
      write(STDOUT,'(7x,a)')'addfun adds the user fortran procedures (keyword "fortranproc")'
      write(STDOUT,'(7x,a)')'contained in the $inFile file to the pk2 library procedures.'
      write(STDOUT,'(7x,a)')'It writes the $outFile.f90 file containing the “userfunc_m”'
      write(STDOUT,'(7x,a)')'module, compiles it and creates the $outFile.a (or $outFile.so)'
      write(STDOUT,'(7x,a)')'library. This can then be used in place of the original libpk2.a'
      write(STDOUT,'(7x,a)')'(or libpk2_d.so) library.'
      write(STDOUT,*)
      write(STDOUT,'(a)')'Example:'
      write(STDOUT,*)
      write(STDOUT,'(7x,a)')'addfun -i foo/myfun.txt -o mylib/libpk2_d -l dynamic'
      write(STDOUT,*)
      write(STDOUT,'(7x,a)') &
                  'this will create libpk2_d.so library on mylib directory. The used'
      write(STDOUT,'(7x,a)') &
                  'compiler is the one used to build the pk2 package (including addfun).'
      write(STDOUT,*)
      write(STDOUT,'(7x,a)') &
                  'Note that when an error occurs during the compilation, the file'
      write(STDOUT,'(7x,a)') &
                 '$outFile.f90 (mylib/libpk2_d.f90 in this example) is not deleted'
      write(STDOUT,'(7x,a)') &
                 '(even if the -d option is used) letting you identify the issue.'
      write(STDOUT,*)
   end if
   
   if ( narg == 0 .or. ( narg == 1 .and. help ) ) stop
      
   if ( len_trim(fileIn) == 0 ) then
      stat = err_t( stat=UERROR, where=HERE, msg='Wrong input file name')
      return
   else if ( len_trim(fileOut) == 0 ) then
      fileOut = fileIn
   end if
      
   END SUBROUTINE getCommandArg
      
END MODULE addfun_m


PROGRAM addfun
!--------------------------------------------------------------------------------------------- 
!  Usage:
!
!        addfun -i inputFile [-o outputFile] [-l libtype] [-h] [-s]
!
!  where
!
!  . -i (required): is used to give the input file name.
!  . -o (optional): is used for the name of the library (default: outputFile = $inputFile).
!  . -l (optional): is used to set the library type. It can be static or dynamic. 
!                   The resulting library will be $outputFile.a or $outputFile.so
!  . -s (optional): write only subroutine versions of user procedures
!  . -h (optional): prints this message.
!--------------------------------------------------------------------------------------------- 
   use pk2mod_m, only: err_t, err_SetHaltingMode, STDERR
   use addfun_m, only: readNwrite, nfunc, getCommandArg
   implicit none
!--------------------------------------------------------------------------------------------- 
   type     (err_t)              :: stat
   character(len=:), allocatable :: fileIn, fileOut
   logical                       :: dynamic = .true., delf90 = .false., onlysub = .false.
   integer                       :: unit
!--------------------------------------------------------------------------------------------- 

   call err_SetHaltingMode ( halting = .false., unit = STDERR, DisplayWarning = .false. )  

   call getCommandArg ( stat, fileIn, fileOut, dynamic, delf90, onlysub )

   call readNwrite (file=fileIn, lib=fileOut, dynlib=dynamic, delf90=delf90, &
                    onlysub=onlysub, stat=stat)

   if ( stat > 0 ) nfunc = -1

   open(newunit = unit, file = 'addfun'//'.out', status = 'replace')
   write(unit,*) nfunc
   close (unit)

   call stat%display()

END PROGRAM addfun
