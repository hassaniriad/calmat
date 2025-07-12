program sample11_userFunctions
!---------------------------------------------------------------------------------------------
!  In this example the user functions added in the pk2 library are invoked.
!  This program can be compiled and linked against the shared version of calmat and pk2
!  (libcalmat_d.so and libpk2_d.so)
!
!  For adding or modifying a (Fortran) function the user has to:
!
!  1) (re)write it in a text file (say myfunc.txt)
!  2) run the executable addfun (in the bin/ directory of pk2) choosing the option -l to 
!     create a shared pk2 library (to avoid recompilation of calmat and of this program)
!         addfun -i myfun.txt -o libpk2_d -l dynamic
!  3) Add in the environnemnt variable DYLD_LIBRARY_PATH the path of this new libpk2_d 
!     (e.g. export DYLD_LIBRARY_PATH=.:$DYLD_LIBRARY_PATH)
!
!  Example:
!  I wrote in myfun.txt the following 3 functions:
!
!   fortranproc r = f(x,y)
!      real :: x, y, r
!      r = x**2 + y**2
!   endfortranproc
!
!   fortranproc y = g(x)
!      real :: x, y
!      y = cos(x) - 1
!   endfortranproc
!
!   fortranproc y = h(x)
!      real :: x, y
!      y = x**2 + 2*x + 1
!   endfortranproc
!
!   I ran addfun: addfun -i myfun.txt -o mylib/libpk2_d -l dynamic
!   
!   addfun creates the file libpk2_d.so in the subdirectory mylib/
!
!   I add into the DYLD_LIBRARY_PATH the path of this library:
!
!          export DYLD_LIBRARY_PATH=./mylib:$DYLD_LIBRARY_PATH
!
!   I can now run my program sample11_userFunctions to compute the value of 
!
!               f(g(x),y)^2 + h(x)
!
!   when e.g. x = 1 and y = 2:
!
!        ./sample11_userFunctions
!
!   I obtain:
! 
!        ans = 21.735233
!
!   I change now one of the functions of myfun.txt, for example, g(x) = sin(x) - 1, 
!   I run again addfun to recreate the pk2_d library. 
!
!   The execution of sample11_userFunctions gives now:
!
!        ans = 20.201683
!
!---------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
!---------------------------------------------------------------------------------------------
 
   print '(/,a)','Evaluate "f(g(x),y)^2 + h(x)" at x = 1 and y = 2'
   print '(a,/)','where f, g, h have been added by the user (using addfun)'

   call calmat ( exprIn="x=1; y=2; f(g(x),y)^2 + h(x)", dispRes=.true., welcome=.false. )
   
end program sample11_userFunctions
