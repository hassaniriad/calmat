program sample5_parseThenEvaluate
! 
! This example illustrates another way of using calmat: parse first then evaluate
!
! - In the parsing phase, calmat returns a "flow" Id (this flow contains the result of the 
!   parsing). 
! - This flow Id can later be passed back to calmat in the evaluation phase.
!-----------------------------------------------------------------------------------------------
   use calmat_m
   implicit none

   type(err_t)        :: stat
   logical, parameter :: Yes = .true., No = .false.
   integer(Ikind)     :: flow1, flow2
!-----------------------------------------------------------------------------------------------

   
   print*
   print*,'********************************************************'
   print*,'*****       Example of parsing a given script      *****'
   print*,'********************************************************'
   print*
   
   call calmat ( fileIn = '../scripts/newton1', parseOnly = flow1, &
                 welcome = No, stat = stat )
   
   if ( stat > 0 ) then
      call stat%Display()
   else
      print*,'Done'
   end if       

   print*
   print*,'********************************************************'
   print*,'**** Example of parsing a given set of expressions *****'
   print*,'********************************************************'
   print*
   call calmat ( exprIn = 'a=%i*%pi, b=exp(a), norm(b+1) < %eps', parseOnly = flow2, &
                 stat = stat )
   
   if ( stat > 0 ) then
      call stat%Display()
   else
      print*,'Done'
   end if   

   print*
   print*,'********************************************************'
   print*,'*****         Evaluate now the given script        *****'
   print*,'********************************************************'
   print*   
   print '(a,i0,a)','(--> Execute the flow #flow1 = ',flow1,')'
   print*
   
   call Calmat ( evaluate = flow1, stat = stat )
   
   if ( stat > 0 ) then
      call stat%Display()
   else
      print*,'Done'
   end if   
   
   print*
   print*,'********************************************************'
   print*,'*****   Evaluate now the given set of expressions  *****'
   print*,'********************************************************'
   print*   
   print '(a,i0,a)','(--> Execute the flow #flow2 = ',flow2,')'  
   print* 
   
   call Calmat ( evaluate = flow2, stat = stat )
   
   if ( stat > 0 ) then
      call stat%Display()
   else
      print*,'Done'
   end if   
         
   print*
   print*,'********************************************************'
   print*,'*****               Delete all flows               *****'
   print*,'********************************************************'
   print*      
   
   ! Once a flow is no longer needed, it can be deleted (to free up memory)
   ! by calling Calmat_delFlows(flowId). If no flow Id is given, it will
   ! delete the set of active flows:
   
   call Calmat_delFlows ( )   
 
   !...
   
   print*,'Terminated'
   
end program sample5_parseThenEvaluate
