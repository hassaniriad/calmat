program poisson2d

!---------------------------------------------------------------------------------------------
!- Example using the pk2 module to solve the 2d Poisson problem:
!
!     -laplacian(u) = f   in  Omega = (0,1)x(0,1)
!                u  = g   on  dOmega
!
!  by the centered Finite Difference Method.
!
!  We take here:  f(x,y) = (w1^2 + w2^2) * sin(w1*x) * cos(w2*y)
!                 g(x,y) = sin(w1*x) * cos(w2*y)
!
!  for which the exact solution is u(x,y) = sin(w1*x) * cos(w2*y).
!
!
!  For compiling this program:
!
!  gfortran -cpp -I $dirmod poisson2d.f90 -L$dirlib -lpk2 -llapack -lblas
!  ifort    -fpp -I $dirmod poisson2d.f90 -L$dirlib -lpk2 -llapack -lblas
!  nagfor   -fpp -I $dirmod poisson2d.f90 -L$dirlib -lpk2 -llapack -lblas -kind=byte
!
!  where $dirmod is ../../mod/$comp  and $dirlib is ./../../lib/$comp and $comp is your compiler
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   ! Used pk2 variables:
   type   (pk2_t)            :: K, F, U                  ! the matrix, the rhs and the solution
   type   (pk2_t)            :: XY                       ! coordinates of the grid mesh
   type   (pk2_t)            :: Zero, X, Y, Xbnod, Ybnod ! auxiliary pk2 var.
   ! Auxiliary variables of intrinsic type:
   integer(Ikind), parameter :: nx = 71, i1 = 1, i2 = 2, i4 = 4
   integer(Ikind)            :: N, i, bnod(4*nx)
   real   (Rkind)            :: a = 0.0, b = 1.0, w1 = 6.28, w2 = 6.28, h, t1, t2
!---------------------------------------------------------------------------------------------

   call err_SetHaltingMode ( halting = .true. )  ! enable automatic stop due to error 
                                                 ! (not useful as it is the default)
   
   write(*,'(/,a)')"Example: solve by the FDM the 2D Poisson's problem"
   write(*,'(/,a)')'      -laplacian(u) = f in Omega = (0,1)^2'
   write(*,'(a,/)')'                  u = g on dOmega' 
   write(*,'(a)'  )'with f = (2*pi^2)*sin(2*pi*x)*cos(2*pi*y)'
   write(*,'(a)'  )'and  g = sin(2*pi*x)*cos(2*pi*y)'
   write(*,'(a,/)')'(the solution is then u = g)'
   
   Zero = 0 ! the pk2 0

   h = (b-a) / (nx-1) ! mesh size (uniform grid with hx = hy = h)
   N = nx**2          ! total number of unknowns         
!
!- Grid generation (use the function meshgrid of the pk2f module):
!   
   write(*,'(a)')'1) Grid generation ('//util_intToChar(N)//' nodes)'
   X = [(a+(i-1)*h, i=1,nx)] ; XY = meshgrid ([X,X]) 
!
!- Set the Matrix (penta-diagonal) of the 2d-laplacian (use the functions diag and ones of
!  the pk2f module):
!
   write(*,'(a)')'2) Set the matrix of the 2D-Laplacian (full matrix storage format)'
   call cpu_time(t1) 
   K = i4 * diag(ones(n,i1)) - diag(ones(n-i1,i1),-i1) - diag(ones(n-i1,i1),i1) &
                             - diag(ones(n-nx,i1),-nx) - diag(ones(n-nx,i1),nx)
   call cpu_time(t2) 
   write(*,'(a,f0.3,a)')'   Done (Ellapsed time: ',t2-t1,')'
!
!- Set the rhs: f = (w1^2 + w2^2) * sin(w1*x) * cos(w2*y) (use the functions extracmat, sin
!  and cos of pk2f):
!  
   write(*,'(a)')'3) Set the RHS'
   X = extracmat ( XY, [-i1], [i1] ) ! x coord. of the grid nodes (-1 means the whole col. 1)
   Y = extracmat ( XY, [-i1], [i2] ) ! y coord. of the grid nodes (-1 means the whole col. 2)  
              
   F = (h**2) * (w1**2 + w2**2) * sin(w1*X) .m. cos(w2*Y) ! note: ".m.": element-wise mult.
!
!- Insert the BC (adapt K and F):
!
   write(*,'(a)')'4) Insert the boundary conditions'
   ! boundary node #s:
   !        bottom, top   , left        , right
   bnod = [ (     i, N-nx+i, (i-1)*nx + 1, i*nx, i=1,nx ) ]

   ! boundary node coordinates:  
   Xbnod = extracmat ( XY, bnod, [i1] ) ! elements of the 1st column of XY
   Ybnod = extracmat ( XY, bnod, [i2] ) ! elements of the 2nd column of XY
 
   ! prescribed values on the boundary (u = g = sin(w1*x) * cos(w2*y) on dOmega):
   U =  sin(w1*Xbnod) .m. cos(w2*Ybnod)  ! (note the element-wise mult)

   ! Modify K and F:    
   call F%SetSubmat ( U, bnod, [i1] )              ! put U(Xbnod,Ybnod) on elt. #bnod(:) of F 
   call K%SetSubmat ( Zero, bnod, [(i,i=1,N)] )    ! put 0 on the rows #bnod(:) of K
   call K%SetSubmat ( eye(4*nx,4*nx), bnod, bnod ) ! put 1 on the diagonal elt. #bnod(:) of K
!
!- Solve the system (use the operator .bslah. of the function mldivide of pk2f):
!
   write(*,'(a)')'5) Solve the linear system'
   call cpu_time(t1) 
   U = K .bslash. F  ! or  U = mldivide (K, F)
   call cpu_time(t2) 
   
   write(*,'(a,f0.3,a)')'   Done (Ellapsed time: ',t2-t1,')'
!
!- Print the result (in the form: X,Y,U,Uexact), using for example the pk2_writemat routine:
!   
   write(*,'(a)')'6) Write the result'
   call pk2_writemat &
        (                                                     &
        mergemats([XY,U,sin(w1*X) .m. cos(w2*Y)],delim=',,'), & ! the pk2 object to print
        fname = 'out_poisson2d',                              & ! name of the file
        size = .false.,                                       & ! do not print the dimensions (optional)
        format = '(4e13.5)',                                  & ! selected format (optional)
        title =                                               & ! set a header (optional)
        'Solution of -lap(u) = f, u = g on boundary \n'  //   & ! note: '\n' for new line
        'x            y            u            u_exact'      &
        )

   write(*,'(a)')'   The result was written to the file: "out_poisson2d"'
   write(*,'(a)')'   You can use gnuplot for example to visualize the result:'
   write(*,'(a)')'     gnuplot> set dgrid3d ' // util_intToChar(nx-1) // ',' // &
                       util_intToChar(nx-1) // '; set contour'
   write(*,'(a)')'     gnuplot> sp "out_poisson2d" u 1:2:3, "out_poisson2d" u 1:2:4 w l'
!
!- Clean up (not useful here as we've finished)
!
   write(*,'(a)')'7) Clean-up (superfluous)'
   call K%destroy() ; call F%destroy() ; call U%destroy()
   call XY%destroy() ; call Zero%destroy() ; call X%destroy() ; call Y%destroy()
   call Xbnod%destroy() ; call Ybnod%destroy()
   call opflag%destroy()

   write(*,'(a)')
   call util_ScrollingMessage ('Program terminated normaly. Bye!',0.5)
end program poisson2d   
   
   

