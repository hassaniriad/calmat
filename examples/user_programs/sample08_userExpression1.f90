program sample08_userExpression1
!-----------------------------------------------------------------------------------------------
!  In this example the user give:
!
!   - a literal expression involving two real variables x, y (coordinates of a mesh node), for
!     example
!                     f = sin( a * x ) .* cos ( b * y ) 
!
!   - any constants used in this expression (e.g. a and b)
!
!   - the node coordinates (coord) where to evaluate this expression
!
!  This example show how to do this with a do-loop.
!
!  For a slightly more efficient way, see sample09_userExpression2.f90 (where a vectorial form
!  is used)
!-----------------------------------------------------------------------------------------------
   use calmat_m
   implicit none
   logical, parameter              :: Yes = .true., No = .false.
   character(len=100)              :: userConstants="", userExpression=""
   real     ( Rkind ), allocatable :: coord(:,:), F, result(:,:)
   integer  ( Ikind )              :: inode, nNode, flowId, u, err
   type     ( ind_t )              :: indx, indy, indf
   real     ( Rkind )              :: t1, t2
!-----------------------------------------------------------------------------------------------

!
!- 1) Read the user data:
!  ======================
   
   print '(/,a)','Reading the user data in sample08.input'
   
   open(newunit=u, file='data/sample08.input', status='old', action='read', iostat=err) 
   if ( err /= 0 ) stop 'Unable to open the input file "data/sample08.input"'
   
   ! read the user expression: we assume this is a real expression of the two real variables
   ! x and y and that the result is noted f (a valid expression is for example: f = 3*x + 2*y)
   read(u,'(a)',iostat=err) userExpression
   
   ! if the user expression involves some constants define them (otherwise set an empty line), 
   ! for example, if f = a*x + b*y, define a and b on the same line, e.g.: a = 3 ; b = 2 
   if ( err == 0 ) read(u,'(a)',iostat=err) userConstants

   ! read the number of nodes:
   if ( err == 0 ) read(u,*,iostat=err) nNode
   if ( err == 0 ) allocate(coord(2,nNode),result(1,nNode))
   
   ! read the node coordinates:
   if ( err == 0 ) then
      do inode = 1, nNode
         read(u,*,iostat=err) coord(1,inode), coord(2,inode)
         if ( err > 0 ) exit
      end do
   end if
   
   if ( err /= 0 ) stop 'Error occured during the read'
   close(u)
   
   open(newunit=u, file='data/sample08.output', status='replace', action='write', iostat=err) 
   if ( err /= 0 ) stop 'Unable to open the output file "data/sample08.output"'

   call cpu_time(t1)
! 
!- 2) Initialize the calmat variables x, y, f and the user constants (if any):
!  ===========================================================================

   call calmat ( exprIn = trim(userConstants) // ' ; x = 0.0 ; y = 0.0 ; f = 0.0', &
                 warning = Yes, welcome = No, dispRes = No )   
                 
   ! and get the indexes of the calmat variables x, y and f
   call calmat_inquire ( 'x', indx )
   call calmat_inquire ( 'y', indy )
   call calmat_inquire ( 'f', indf )
!
!- 3) Parse the user function:
!  ===========================

   print '(/,a,i0,a)','Evaluate "'//trim(userExpression)//'" at ',nNode,' points'
      
   call calmat ( exprIn = trim(userExpression), parseOnly = flowId )
!
!- 4) Evaluate the user expression at x = coord(1,i) and y = coord(2,i), i = 1,..., nNode:
!  ======================================================================================

   do inode = 1, nNode
      ! set 'x' to coord(1,i) and 'y' to coord(2,i):
      call calmat_copy ( from = coord(1,inode), to = indx )
      call calmat_copy ( from = coord(2,inode), to = indy )
      ! evaluate the user expression:
      call calmat ( evaluate = flowId )
      ! get the value of 'f':
      call calmat_copy ( from = indf, to = F )
      result(1,inode) = F
   end do

   call cpu_time(t2)
   print*,'cpu time spent for evaluation: ',t2-t1

   ! use the result:
   ! ...
   ! write the result:
   do inode = 1, nNode
      write(u,*) coord(1,inode), coord(2,inode), result(1,inode)
   end do
   
   close(u)

   print '(/,a)','Done. The result is printed in data/sample08.output'
   print '(a,/)','(You can draw the result with gnuplot for example)'
      
end program sample08_userExpression1
