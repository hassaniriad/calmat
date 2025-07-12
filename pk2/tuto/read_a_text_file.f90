program read_a_text_file
!---------------------------------------------------------------------------------------------
!- Reads the lines of a text file. Stores them in an array of string (str_t).
!---------------------------------------------------------------------------------------------
   use pk2mod_m
   implicit none
   integer  (Ikind), parameter   :: incremSize = 10
   character(len=*), parameter   :: myFile = 'datafiles/myTextFile.txt', comment = '#'
   
   integer  (Ikind)              :: currentSize = incremSize, usedSize = 0, u = 0, ios = 0, &
                                    n = 0, nlinesRead = 0
   type     (str_t), allocatable :: myText(:)
   character(len=:), allocatable :: rec
   type     (err_t)              :: flagErr
!---------------------------------------------------------------------------------------------

   call err_SetHaltingMode ( halting = .true.,  DisplayWarning = .true. )

   !================================================
   ! Method 1: (see Method 2 for a more concise one)
   !================================================

   print '(/,a)','============================================================='
   print '( a )','METHOD 1 (calling the function "util_GetRecord" in a do-loop)'
   print '( a )','============================================================='
   
   ! Open the file:
   
   open ( newunit = u, file = myFile, action = 'read', iostat = ios )

   if ( ios /= 0 ) then
      print '(a)', 'Unable to open the file "'//myFile//'"'
      stop
   end if
   
   ! Allocate the array "myText" with an initial size of "currentSize":
   
   call util_alloc ( mode = 'a', t = myText, n = currentSize, stat = flagErr )
   
   ! Start reading the file:
   
   do 
      ! Read a new record from the file
      ! - comment = '#'   => ignore part of the line to the right of the symbol '#'
      ! - rmblk = .false. => do not delete spaces (but ignore empty lines)
      
      rec = util_GetRecord ( unit = u, stat = flagErr, comments = [comment,' ',' '], &
                             rmblk = .false., nlines = n, contSymb = ['...'] )
                             
      ! Terminate if end-of-file is reached:     
                      
      if ( flagErr == EOF ) exit 

      ! Number of lines actually read (can be useful to signal an error during reading):
      
      nlinesRead = nlinesRead + n 
      
      ! Store "rec" in the array "myText" (first check that it is large enough): 
      
      usedSize = usedSize + 1
      if ( usedSize > currentSize ) then
         ! increase the size of "myText" by "incremSize":
         currentSize = currentSize + incremSize
         call util_alloc ( mode = 's', t = myText, n = currentSize, stat = flagErr )
      end if
      myText(usedSize) = rec
      
   end do
  
   close(u)
    
   print '(/,a,i0)'    ,'Number of lines read in "'//myFile//'": ',nlinesRead
   print '(a)'         ,'Considered symbol for comments: "'//comment//'"'
   print '(/,a,i0,a,/)','The array "myText" contains the following ',usedSize,' records:'
   
   do n = 1, usedSize
      call myText(n)%printMe ( unit = STDOUT )
   end do

   ! If your application requires it: reduce the size of the array to its useful size:
   call util_alloc ( mode = 's', t = myText, n = usedSize, stat = flagErr )   
   print '(/,a,i0,/)','Size of "myText": ',size(myText)
   ! ...

   deallocate(myText)

   !=============================================================================
   ! Method 2: same thing but more quickly using the subroutine util_ReadTextFile
   !=============================================================================

   print '(/,a)','=================================================='
   print '( a )','METHOD 2 (using the procedure "util_ReadTextFile")'
   print '( a )','=================================================='

   call util_ReadTextFile ( fileName = myFile, stat = flagErr, comment = comment, &
                            rmblk = .false., nlines = nlinesRead, recs = myText )
                                
   print '(/,a,i0)'    ,'Number of lines read in "'//myFile//'": ',nlinesRead
   print '(a)'         ,'Considered symbol for comments: "'//comment//'"'
   print '(/,a,i0,a,/)','The array "myText" contains the following ',size(myText),' records:'
   
   do n = 1, size(myText)
      call myText(n)%printMe ( unit = STDOUT )
   end do
   
   print*
                                   
end program read_a_text_file
