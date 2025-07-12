! For compiling (or use the makefile):

! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran Check_str.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_str

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort Check_str.f90 -L../../lib/ifort -lpk2 -llapack -lblas -o check_str

! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor Check_str.f90  -L../../lib/nagfor -lpk2 -llapack -lblas -o check_str

program Check_str

   use pk2mod_m, c => str_color
   
   implicit none
   
!---------------------------------------------------------------------------      
   type(err_t) :: stat ! for error/warning report   
   type(str_t) :: x,y,z, m1(2,3), m2(2,3)
   type(pk2_t) :: p
!---------------------------------------------------------------------------   

   call SignalHandler_SignalCatch (unit = STDOUT, title = '--> Check_str:')

   x = 'Check_str'
   y = 'Welcome in ' + c(x,'rhB') + '.' + c(' We','yhb') + c(' test','bhb') + &
       c(' here','chb') + c(' text','ghb') + c(' color','mhb') + c(' printing','bhb')
   
   call x%printMe(6,fmt='("x = ",a)')
   call y%printMe(6,fmt='("y = ",a)')
   
   p = x
   call display(p,msg='p')
   p = [x,y]
   call display(p,msg='p')
   
   m1(1,1) = 'hello'          ; m1(1,2) = c('world','r')    ; m1(1,3) = c('!','bb')
   m1(2,1) = c('bonjour','m') ; m1(2,2) = c('le monde','g') ; m1(2,3) = c('!','c')
   
   print*
   m2 = m1
   call str_print(s=m2,unit=STDOUT)
   
contains
!=============================================================================================   
   subroutine display ( a , msg, symb )
!=============================================================================================   
   type     (pk2_t),           intent(in) :: a
   character(len=*), optional, intent(in) :: msg
   character(len=1), optional, intent(in) :: symb
!---------------------------------------------------------------------------------------------    
!  Displays the variable "a".
!
!  If "msg" is present, prints "msg" and "a" in the form << msg  = {value(s) of a} >>.
!  If "symb" is present, replaces the "=" by symb.
!
!  Note: dispmodule is used.
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------   
   character(len=:), allocatable :: str, csymb
   character(len=:), allocatable :: res(:,:)
   character(len=9)              :: fmt
   integer                       :: nfmt   
!---------------------------------------------------------------------------------------------    
   
   
   if (present(msg)) then
      str = msg
   else
      if (allocated(a%name)) then
         str = a%name
      else
         str = ''
      end if   
   end if
   
   if (present(symb)) then
      csymb = ' '//symb//' '
   else
      csymb = ' = '
   end if      
         
   print*

   if (a%typ == EMPTY .or. a%nrow * a%ncol == 0) then
      call disp(str // csymb // '[ ]')
      return
   end if
            
   select type (p=>a%m)
      type is (ik2_t)
         call disp(str // csymb,p%v)
      type is (rk2_t)
         call disp(str // csymb,p%v)
      type is (ck2_t)
         call disp(str // csymb,p%v)
      type is (lk2_t)
         call disp(str // csymb,p%v)
      type is (sk2_t)     
         call str_print ( s=p%v, unit=STDOUT, lhsMsg = msg // csymb )
        
!          call p%GetMat(Ch=res)
!          fmt = 'a0'
!          if (all(len_trim(res(1,:)) == 0)) then
!             nfmt = max(1,maxval(len_trim(res)))
!             print*,'nfmt=',nfmt
!             write(fmt,'(a,i0)')'a',nfmt 
!          end if     
!          call disp(str // csymb, res, fmt = fmt)
   end select
      
   end subroutine display   
   end
