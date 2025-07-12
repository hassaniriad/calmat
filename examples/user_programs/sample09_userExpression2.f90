program sample09_userExpression2
!-----------------------------------------------------------------------------------------------
!  In this example the user give:
!
!   - a literal element-wise expression involving the coordinates of mesh nodes (given in
!     a 2 x n array coord), for example: 
!                     f = sin( a * coord(1,:) ) .* cos ( b * coord(2,:) ) 
!     the result is then a 1 x n array
!
!   - any constants used in this expression (e.g. a and b)
!
!   - the node coordinates (coord) where to evaluate the expression
!
!  We use here a vectorial form (see sample08_userExpression1 for a slightly less efficient 
!  way using a do-loop)
!-----------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
   logical, parameter              :: Yes = .true., No = .false.
   character(len=100)              :: userConstants="", userExpression=""
   real     ( Rkind ), allocatable :: coord(:,:), result(:,:)
   integer  ( Ikind )              :: inode, nNode, u, err
   real     ( Rkind )              :: t1, t2
!-----------------------------------------------------------------------------------------------

!
!- 1) Read the user data:
!  ======================
   
   print '(/,a)','Reading the user data in sample09.input'
   
   open(newunit=u, file='data/sample09.input', status='old', action='read', iostat=err) 
   if ( err /= 0 ) stop 'Unable to open the input file "data/sample09.input"'
   
   ! read the user expression: we assume this is a real element-wise expression of the two
   ! real variables coord(1,:) and coord(2,:) and that the result is noted f (a valid 
   ! expression is for example: f = 3*coord(1,:) + 2*coord(2,:))
   read(u,'(a)',iostat=err) userExpression
   
   ! if the user expression involves some constants define them (otherwise set an empty line), 
   ! for example, if f = a*coord(1,:) + b*coord(2,:), define a and b on the same line, e.g.: 
   ! a = 3 ; b = 2 
   if ( err == 0 ) read(u,'(a)',iostat=err) userConstants

   ! read the number of nodes:
   if ( err == 0 ) read(u,*,iostat=err) nNode
   if ( err == 0 ) allocate(coord(2,nNode))
   
   ! read the node coordinates:
   if ( err == 0 ) then
      do inode = 1, nNode
         read(u,*,iostat=err) coord(1,inode), coord(2,inode)
         if ( err > 0 ) exit
      end do
   end if
   
   if ( err /= 0 ) stop 'Error occured during the read'
   close(u)
   
   open(newunit=u, file='data/sample09.output', status='replace', action='write', iostat=err) 
   if ( err /= 0 ) stop 'Unable to open the output file "data/sample09.output"'

   call cpu_time(t1)
! 
!- 2) Initialize the user constants (if any):
!  ==========================================

   call calmat ( exprIn = trim(userConstants), warning = Yes, welcome = No, dispRes = No )   
!
!- 3) Create and set a new variable called 'coord' and evaluate the user expression:
!  =================================================================================

   call calmat_moveAlloc ( from = coord, to = 'coord' ) !< whenever possible, prefer to use
                                                        !< movealloc rather than a copy

   print '(/,a,i0,a)','Evaluate "'//trim(userExpression)//'" at ',nNode,' points'
   
   call calmat ( exprIn = trim(userExpression) )
!
!- 4) Get the result:
!  ==================

   call calmat_moveAlloc ( from = 'coord', to = coord  )
   call calmat_moveAlloc ( from = 'f'    , to = result )

   call cpu_time(t2)
   print*,'cpu time spent for evaluation: ',t2-t1

   ! use the result:
   ! ...
   ! write the result:
   do inode = 1, nNode
      write(u,*) coord(1,inode), coord(2,inode), result(1,inode)
   end do
   close(u)   

   print '(/,a)','Done. The result is printed in sample09.output'
   print '(a,/)','(You can draw the result with gnuplot for example)'
   
end program sample09_userExpression2
