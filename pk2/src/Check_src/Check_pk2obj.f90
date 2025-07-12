! nagfor -C=all -O0 -fpp -kind=byte -I ../../mod/nagfor Check_pk2obj.f90 -L../../lib/nagfor -lpk2 -llapack -lblas -o check_pk2obj

! gfortran -fcheck=all -fbacktrace -Wall -fimplicit-none -Og -I ../../mod/gfortran Check_pk2obj.f90 -L../../lib/gfortran -lpk2 -llapack -lblas -o check_pk2obj

! ifort -check all -traceback -gen-interfaces -warn interfaces -O0 -fpp -I ../../mod/ifort Check_pk2obj.f90 -L../../lib/ifort -lpk2 -llapack -lblas -o check_pk2obj

program CheckObj
   use pk2mod_m
   use, intrinsic :: ieee_arithmetic 
   
   implicit none
   
   type   (pk2obj_t)              :: mobj, mobj2
   type   (pk2_t   )              :: a(5), b, c(2)
   type   (pk2_t   ), allocatable :: d(:)
   type   (str_t   )              :: name(5)
   integer(Ikind   )              :: i
   
   call ieee_set_halting_mode (ieee_all , halting = .false.) 
   
   call SignalHandler_SignalCatch (unit = STDOUT, title = '--> CheckObj Info:')
   
   a(1) = pk2_t(matrix = reshape([1,2,3],[1,3]), name = 'A') 
   a(2) = pk2_t(matrix = reshape([.true.,.false.],[1,2]), name = 'B') 
   a(3) = pk2_t(scalar = 'hello world!', name = 'C')
   a(4) = pk2_t(scalar = 3.14159265, name = 'pi') 
   a(5) = pk2_t(scalar = 1966, name = 'date') 
   
   do i = 1, 5 ; name(i)%str = a(i)%name ; end do
   
   b = cmplx(1_Rkind,2_Rkind) ; b%name = 'b'
   c(1) = reshape([11,12,13,14],[2,2]) ; c(1)%name = 'c1'
   c(2) = reshape(['abc','def','ghi','jkl'],[2,2]) ; c(2)%name = 'c2'
!
!- Create an empty structure:
!   
   print '(/,a,/,40("="))','Create an empty structure (named "MOBJ"):'

   mobj = pk2obj_t('MOBJ')
   call mobj%PrintMe(opt=2_Ikind)   
!
!- Create a structure from a set of pk2 variable:
!   
   print '(/,a,/,54("="))','Create a structure (named "MOBJ") made of 4 components:'

   mobj = pk2obj_t(a(1:4),name(1:4),'MOBJ')
   call mobj%PrintMe(opt=2_Ikind)
!
!- Add a component to an existing structure:
!
   print '(/,a,/,43("+"))','Insert the component "b" into the structure:' 

   call mobj%InsertComp(b) 
   call mobj%PrintMe(opt=2_Ikind)

   print '(/,a,/,42("+"))','Insert the component "'//a(3)%name//'" (already present):' 
   
   call mobj%InsertComp(a(3)) 
   call mobj%PrintMe(opt=2_Ikind)

   print '(/,a,/,43("+"))','Insert the component "'//a(4)%name//'" (already present):' 
   
   call mobj%InsertComp(a(4)) 
   call mobj%PrintMe(opt=2_Ikind)  
!
!- Remove a component from a structure:
!
   print '(/,a,/,48("-"))','Remove the component "MOBJ.C" from the structure:'
   
   call mobj%RemoveComp('C')
   call mobj%PrintMe(opt=2_Ikind)

   print '(/,a,/,48("-"))','Remove the component "MOBJ.A" from the structure:'
   
   call mobj%RemoveComp('A')
   call mobj%PrintMe(opt=2_Ikind)
!
!- Add a component after a remove:
!   
   print '(/,a,/,46("+"))','Insert the component "'//a(5)%name//'" into the structure:' 
   
   call mobj%InsertComp(a(5)) 
   call mobj%PrintMe(opt=2_Ikind)  
   
   print '(/,a,/,46("+"))','Insert the component "truc" into the structure:' 
   
   call mobj%InsertComp(pk2_t(vector=[111,222,333]),'truc') 
   call mobj%PrintMe(opt=2_Ikind)  
!
!- Add a set of components:
!
   print '(/,a,/,54("+"))','Insert two components "c1" and "c2" into the structure:'

   call mobj%InsertComp(c)
   call mobj%PrintMe(opt=1_Ikind)
!
!- Delete a structure:
!
   print '(/,a,/,20("-"))','Delete the structure:'

   call mobj%destroy()
   call mobj%PrintMe(opt=1_Ikind)
!
!- Add a set of components into an empty structure (alternative to the constructor):
!
   print '(/,a,/,15("="))','A new structure:'
   
   mobj2 = pk2Obj_t('New_Obj')    
   call mobj2%PrintMe(opt=2_Ikind)
   
   print '(/,a,/,54("+"))','Insert two components "c1" and "c2" into the structure:'
   
   call mobj2%InsertComp(c)
   call mobj2%PrintMe(opt=1_Ikind)
!
!- transfer the data of a structure into a set of pk2 variables:
!
   print '(/,a,/,63("="))','transfer the data of the structure into a set of pk2 variables:'

   call mobj2%TransferToPk2 (d)
   
   do i = 1, size(d)
      call d(i)%PrintMe(form='values')
   end do   

   call mobj2%PrintMe(opt=1_Ikind)
!
!- A structure with only one pk2 variable and no structure-name:
!
   print '(/,a,/,60("="))','A structure with only one pk2 variable and no structure-name:'

   call mobj2%InsertComp(c(1))
   call mobj2%PrintMe(opt=1_Ikind)
!
!- Assignments:
!  ===========

!
!- Copy a structure into another one (assignment pk2obj = pk2obj):
!
   mobj = pk2obj_t(a(1:4),name(1:4),'MOBJ')

   print '(/,a,/,60("="))','Copy one structure into another (assignment pk2Obj = pk2Obj):'

   mobj2 = mobj ; call mobj2%SetName('MOBJ2') ; call mobj2%PrintMe(opt=2_Ikind) 
!
!- Copy a pk2 into a structure (assignment pk2obj = pk2):
!
   call mobj2%Destroy()
   print '(/,a,/,62("="))','Copy a pk2 variable into a structure (assignment pk2obj = pk2):'

   mobj2 = a(1)
   call mobj2%PrintMe(opt=2_Ikind) 
      
   call mobj2%Destroy()
   print '(/,a,/,60("="))','Copy an empty pk2 into a structure (assignment pk2obj = pk2):'   
   
   call a(1)%Destroy() ; a(1)%name = 'toto'
   mobj2 = a(1)
   call mobj2%PrintMe(opt=2_Ikind)
!
!- Get the name of a structure:
!
   print '(/,a,/,28("="))','Get the name of a structure:'
   print*,mobj%GetName()
!
!- Get a given component: 
!
   print '(/,a,/,36("="))','Get a given component of a structure:'
   b = mobj%GetAcmp('B') ; call b%printMe(form='values') 
   
   print '(/,a,/,51("="))','Get a given component of a structure (non-existent):'
   b = mobj%GetAcmp('Notfound') ; call b%printMe(form='values')
!
!- Set a given component:
!
   print '(/,a,/,54("="))','Set a given component of a structure (here modify "B"):'
   b = 1010101 ; call mobj%SetAcmp(b,'B') ; call mobj%PrintMe(opt=1_Ikind)  
   
   print '(/,a,/,58("="))','Set a given component of a structure (here insert "added"):'   
   b = 2020202 ; call mobj%SetAcmp(b,'added') ; call mobj%PrintMe(opt=1_Ikind)  
   
   read*
   print '(//,a,/,16("="),/)','End Check_pk2Obj'

end program CheckObj