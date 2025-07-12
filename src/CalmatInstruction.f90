!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Module CalmatInstruction
!
! This module contains the definition of DT for an instruction and a flow of instructions
!
! Author: R. Hassani, Universite Cote d'Azur
!
! Date: 01/20, 04/20, 05/20
!---------------------------------------------------------------------------------------------

#include "error.fpp"

MODULE CalmatInstruction_m
   
   use kindParameters_m, only: Ikind, Rkind
   use str_m, only: str_t
   use err_m, only: err_t
   use pk2Interpreter_m, only: hdl_t
   use pk2Constants_m, only: LGSTR, STDIN, STDOUT, STDERR, IZERO
   use util_Other_m, only: util_GetUnit
   use util_Strings_m, only: util_RemoveSpaces1
                       
   
   use CalmatConstants_m, only: G_IERROR, G_UERROR, G_FORBLOCK, G_IFBLOCK, G_EXECBLOCK
      
   implicit none
   
   integer(Ikind) :: incnstack = 1000, yieldnstack = 10
!
!- A DT for an instruction:
!
   type :: ins_t
         
      logical                       :: parsed = .false., assignment = .false.
      
      character(len=:), allocatable :: rec              
      integer  (Ikind)              :: flowId = 0, & ! The script # where it was read
                                       numl   = 0, & ! The line # in this script
                                       verb   = 1    ! Level of verbosity
                                       
      integer  (Ikind)              :: blk = 0 !  =  0  : regular instruction
                                               ! >=  k>0: start of a block of instructions
                                               ! <= -k  : end of a block of instructions
                                               
      integer  (Ikind)              :: globalInsNum = 0, nextFlow = 0, breakpt = 0                                          
!
!-    for the parsing of the rhs:
!
      character(len=:), allocatable :: rhs        ! Expression of the rhs 
      type     (hdl_t)              :: rhsHdl     ! Tokenization of the rhs
      type     (str_t), allocatable :: rhsCmps(:) ! Component names (if the result is an object) 
      integer  (Ikind)              :: rhsNCmp    ! Number of components
      
      integer  (Ikind)              :: idCmd = 0  ! >0 if a calmat command is invoked
      
!
!-    for the parsing of the lhs:
!
      character(len=:), allocatable :: lhs         ! Expression of the lhs
      integer  (Ikind)              :: nlhs        ! Number of lhs 
      type     (str_t), allocatable :: lhsNames(:) ! Variable name of each lhs
      integer  (Ikind), allocatable :: lhsVarId(:) ! Corresponding variables #      
      type     (str_t), allocatable :: lhsIndex(:) ! Indices expression (submatrix assignment)
!
!-    for an opening block instruction (blk /= 0):
!     
      integer  (Ikind) :: nlink = 0 ! if blk > 0: number of instructions that follow up to 
                                    ! the closing keyword of the corresponding block (e.g.
                                    ! "endif" or "endfor")
!
!-    for a FOR instruction:
!
      character(len=:), allocatable :: cName        ! Name of the counter
      integer  (Ikind)              :: cValue       ! Current value of the counter
      integer  (Ikind)              :: cIvar        ! Corresponding variable #      
      type     (str_t)              :: cExpr(3)     ! Expression of each bounds
      integer  (Ikind)              :: cVal (3) = 0 ! Value of each bounds
      type     (hdl_t)              :: cHdl (3)     ! Tokenization of the bounds
!
!-    for an IF instruction:
!
      integer  (Ikind)              :: nBr = 0    ! Number of branches
      integer  (Ikind), allocatable :: startBr(:) ! Starting Id of each branch
      integer  (Ikind), allocatable :: endBr  (:) ! Ending Id of each branch
      type     (str_t), allocatable :: tExpr  (:) ! Expression of each branch condition
      logical         , allocatable :: tVal   (:) ! Value of each condition
      type     (hdl_t), allocatable :: tHdl   (:) ! Tokenization of the branch conditions

   contains
      procedure, pass :: destroy => CalmatInstruction_Destroy
      !final :: CalmatInstruction_Finalizer
   end type ins_t
!
!- A DT for a flow of instructions (i.e. a set of instructions) read from a given file or
!  supplied from an expression:
!
   type :: flw_t      
      logical                       :: used = .false.

      character(len=:), allocatable :: expression     ! the supplied expression or the      
      character(len=:), allocatable :: fileName       ! file name from which the set was read
      integer  (Ikind)              :: curLine   = 0  ! Current line #
      integer  (Ikind)              :: unit      =-1  ! Corresponding unit number (0 in case
                                                      ! of expression)
                                                      ! -1 means "closed" !04/24 
      
      type     (ins_t), allocatable :: stack(:)       ! The set (a stack) of instructions 
      integer  (Ikind)              :: maxnStack = 0  ! Current max. size of the stack 
      integer  (Ikind)              :: nStack    = 0  ! Current number of instructions      
      
      integer  (Ikind)              :: start     = 1  ! (added 04/24)
   contains
      procedure, pass :: destroyFlow            => CalmatInstruction_destroyFlow
      procedure, pass :: initFlow               => CalmatInstruction_initFlow
      procedure, pass :: allocateStack          => CalmatInstruction_allocateStack
      procedure, pass :: destroyStack           => CalmatInstruction_destroyStack
   
      procedure, pass :: insertRecordsIntoStack => CalmatInstruction_insertRecordsIntoStack
      procedure, pass :: completeStack          => CalmatInstruction_completeStack
      
      procedure, pass :: debug                  => CalmatInstruction_printFlow
      
      !Ajouter finalizer (close file)
   end type flw_t
   

CONTAINS

!=============================================================================================
   SUBROUTINE CalmatInstruction_initFlow ( self, flagerr, fileName, expression  )
!=============================================================================================
   class    (flw_t),           intent(in out) :: self
   type     (err_t),           intent(in out) :: flagerr
   character(len=*), optional, intent(in    ) :: fileName, expression
!---------------------------------------------------------------------------------------------   
!  Connects the flow "self" to the file of name "fileName". If "fileName" is empty the flow is
!  connected to the stdin.
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   character(len=*    ), parameter :: HERE = 'CalmatInstruction_initFlow'
   character(len=LGSTR)            :: iomsg
   integer                         :: u, iostat   
!---------------------------------------------------------------------------------------------   
   
   call self%destroyFlow ( flagerr )
   if_error_trace_and_RETURN ( flagerr, HERE )

   if ( present(fileName) ) then
      if ( len_trim(fileName) /= 0 ) then
         u = util_GetUnit ()
         if ( u == 0 ) then
            call flagerr%set ( stat=G_IERROR, where=HERE,  msg="No free unit available" )
            return
         end if   

         open(unit=u, file=fileName, status='old', action='read', iostat=iostat, iomsg=iomsg)
      
         if ( iostat /= 0 ) then
            call flagerr%set ( msg = 'Attempt to open the file "'//               &
                                     trim(fileName) // '"'//new_line('a') //      &
                                     '--> Reported IO message: ' // trim(iomsg),  &
                               stat = G_UERROR, where = HERE                      )
            return
         end if
      
         self%unit = u
         
      else
         self%unit = STDIN
      end if

      self%used     = .true.   
      self%fileName = fileName
      self%expression = '' 
   else if ( present(expression) ) then
      self%unit = IZERO  ! for case where it is an expression rather than a file name 
      self%used = .true. ! that is provided to calmat
      self%expression = expression 
      self%fileName = ''        
   else
      call flagerr%set (stat= G_IERROR, where=HERE, msg="Missing fileName or expression" )
      return
   end if
   
   END SUBROUTINE CalmatInstruction_initFlow


!=============================================================================================
   SUBROUTINE CalmatInstruction_destroyFlow ( self, flagerr )
!=============================================================================================
   class(flw_t), intent(in out) :: self
   type (err_t), intent(   out) :: flagerr   
!---------------------------------------------------------------------------------------------   
!  Resets a flow
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   logical :: is_open 
!---------------------------------------------------------------------------------------------   

   if ( self%unit /= STDIN .and. self%unit /= STDOUT .and. self%unit /= STDERR & !!) then
        .and.  self%unit >= IZERO ) then !!04/24
      inquire ( unit = self%unit, opened = is_open )      
      if ( is_open ) close ( self%unit )
   end if

   self%used       = .false.
   self%expression = ''
   self%fileName   = ''
   self%curLine    = 0
   self%unit       = -1 !!! 0 !04/24 (-1: means "closed")
   self%start      = 1 !04/24

   call self%destroyStack ( flagerr )
      
   END SUBROUTINE CalmatInstruction_destroyFlow


!=============================================================================================
   SUBROUTINE CalmatInstruction_destroyStack ( self, flagerr )
!=============================================================================================
   class(flw_t), intent(in out) :: self
   type (err_t), intent(   out) :: flagerr
!---------------------------------------------------------------------------------------------   
!  Sets self%nstack = 0 and allocates or reallocates self%stack as follows:
!
!  . If not allocated, self%stack is allocated with the minimal size incnstack. 
!  . If allocated, self%stack is reallocated with this minimal size only if its original size
!    is greater than yieldnstack*incnstack (in order to avoid too frequent deallocations /  
!    allocations as well as an excessive memory increase)
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------  
!---------------------------------------------------------------------------------------------   
      
   self%nstack = 0 
   
   if ( .not. allocated(self%stack) .or. self%maxnStack > yieldnstack*incnstack ) &
      call self%allocateStack ( IZERO, flagerr )
      
   END SUBROUTINE CalmatInstruction_destroyStack


!=============================================================================================
   SUBROUTINE CalmatInstruction_allocateStack ( self, n, flagerr )
!=============================================================================================  
   class  (flw_t), intent(in out) :: self 
   integer(Ikind), intent(in    ) :: n
   type   (err_t), intent(   out) :: flagerr   
!---------------------------------------------------------------------------------------------   
!  Allocates or resizes self%stack.
!
!  . If n == 0 (initialization / re-initialization), self%stack is first deallocated (if 
!    allocated) and allocated with a minimal size given by incnstack.
!
!  . If size(self%stack) > n, self%stack is unchanged. 
!
!  . If size(self%stack) <= n, self%stack is resized with a size n + incnstack. 
!    Its first self%nStack elements are preserved.
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'CalmatInstruction_allocateStack'
   type     (ins_t), allocatable :: tmp(:)   
   integer  (Ikind)              :: i, err 
!---------------------------------------------------------------------------------------------   
   
   err = 0

   if ( n == 0 .and. allocated(self%stack) ) then
      self%nStack = 0 ; self%maxnStack = 0 

      do i = 1, size(self%stack)
         call self%stack(i)%destroy ( flagerr )
         if_error_trace_and_RETURN ( flagerr, HERE )
      end do

      deallocate(self%stack, stat = err)
      if ( err /= 0 ) then
         call flagerr%set ( stat = G_IERROR, where = HERE, &
                             msg = 'Failure to de-allocate self%stack' )
         return
      end if
   end if

   if ( .not. allocated(self%stack) ) then
   
      self%maxnStack = incnstack
      if ( n > self%maxnStack ) self%maxnStack = self%maxnStack + n
      
      allocate(self%stack(self%maxnStack), stat = err)
      if ( err /= 0 ) then
         call flagerr%set ( stat = G_IERROR, where = HERE, &
                             msg = 'Allocation failure for self%stack' )
         return
      end if
            
   else if ( n >= self%maxnStack ) then
   
      self%maxnStack = incnstack + n
      
      allocate(tmp(self%maxnStack), stat = err) 
      if ( err /= 0 ) then
         call flagerr%set ( stat=G_IERROR, where=HERE, msg='Allocation failure for tmp' )
         return
      end if

      do i = 1, self%nStack
         tmp(i) = self%stack(i)
      end do
      !!tmp(1:self%nStack) = self%stack(1:self%nStack)
       
      call move_alloc ( from = tmp, to = self%stack )
      
   end if
   
   END SUBROUTINE CalmatInstruction_allocateStack


!=============================================================================================
   SUBROUTINE CalmatInstruction_insertRecordsIntoStack ( self, subRec, flowId, flagerr )
!=============================================================================================
   class  (flw_t), intent(in out) :: self
   type   (str_t), intent(in    ) :: subRec(:,:)
   integer(Ikind), intent(in    ) :: flowId
   type   (err_t), intent(   out) :: flagerr
!---------------------------------------------------------------------------------------------  
!  Add the set of records (subRec) at the end of the flow "self"   
!---------------------------------------------------------------------------------- R.H. 04/20

!- local variables: -------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'CalmatInstruction_insertRecordsIntoStack'
   integer  (Ikind)            :: i, j, nStackOld, nStackNew, nSubRec
!---------------------------------------------------------------------------------------------  
   
   nSubRec = size(Subrec,dim=1) ! number of instructions to insert
   
   nStackOld = self%nStack
   nStackNew = nStackOld + nsubRec
      
   if ( nStackNew > self%maxnStack ) then
      call self%allocateStack(nstackNew, flagerr)
      if_error_trace_and_RETURN ( flagerr, HERE )
   end if
   
   self%nStack = nStackNew

   do i = 1, nSubRec
      j = nStackOld + i
      self%stack(j)%rec = trim(adjustl(SubRec(i,1)%str))
      if ( SubRec(i,2)%str == ';' ) then
         self%stack(j)%verb = 0
      else
         self%stack(j)%verb = 1
      end if      
      self%stack(j)%numl     = self%curLine
      self%stack(j)%blk      = 0
      self%stack(j)%nlink    = 0
      self%stack(j)%parsed   = .false.
      self%stack(j)%nextFlow = 0
      self%stack(j)%breakpt  = 0
      self%stack(j)%flowId   = flowId
      
      if ( index(self%stack(j)%rec,'exec') == 1 ) self%stack(j)%blk = G_EXECBLOCK
   end do

   END SUBROUTINE CalmatInstruction_insertRecordsIntoStack


!=============================================================================================
   SUBROUTINE CalmatInstruction_completeStack ( self, nfor, nif, flagerr )
!=============================================================================================
   class  (flw_t), intent(in out) :: self
   integer(Ikind), intent(in    ) :: nfor, nif
   type   (err_t), intent(   out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Adds informations in the stack for FOR or IF constructs 
!
!  Note: the alternates "end if" and "end for" were transformed to "endif" and "endfor" during
!        the read (Calmat_read subroutine). This is not the case for "else if".
!---------------------------------------------------------------------------------- R.H. 01/20

!- local variables: --------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'CalmatInstruction_completeStack'
   integer  (Ikind)              :: m, i, j, nbr, opif, opfor, forId, ifId
   integer  (Ikind)              :: lfor(nfor), lif(nif), sbr(self%nStack,nif), tbr(nif)
!---------------------------------------------------------------------------------------------    
               
   opfor = 0 ; opif = 0

   ASSOCIATE ( stack => self%stack )
   
   do i = 1, self%nStack
         
      if ( index(stack(i)%rec,"for ") == 1 ) then  
        
         stack(i)%blk = G_FORBLOCK
            
         opfor = opfor + 1 ; lfor(opfor) = i

      else if ( stack(i)%rec == "endfor" ) then

         if ( opfor <= 0 ) then
            call flagerr%set ( msg = 'Unexpected "endfor"', where = HERE, stat = G_UERROR )
            return
         end if  
         
         stack(i)%blk = -G_FORBLOCK
            
         forId = lfor(opfor)
         m = i - forId ! number of instructions between the "for" and its "endfor"
         stack(i)%nlink =-m ; stack(forId)%nlink = m 
            
         opfor = opfor - 1      
            
      else if ( index(stack(i)%rec,"if ") == 1 ) then
      
         stack(i)%blk = G_IFBLOCK
            
         opif = opif + 1 ; lif(opif) = i
            
         stack(i)%nBr = 1 ; sbr(1,opif) = i ; tbr(opif) = 1
            
      else if ( stack(i)%rec == "endif" ) then
            
         if ( opif <= 0 ) then
            call flagerr%set ( msg = 'Unexpected "endif"', where = HERE, stat = G_UERROR )
            return
         end if  
            
         stack(i)%blk = -G_IFBLOCK
            
         ifId = lif(opif)
         m = i - ifId ! number of instructions between the "if" and its "endif"
         stack(i)%nlink =-m ; stack(ifId)%nlink = m 
            
         nbr = stack(ifId)%nBr
         
         if ( allocated(stack(ifId)%startBr) ) then
            if ( size(stack(ifId)%startBr) < nbr ) &
               deallocate(stack(ifId)%startBr, stack(ifId)%endBr)
         end if
         
         if ( .not. allocated(stack(ifId)%startBr) ) &
            allocate( stack(ifId)%startBr(nbr), stack(ifId)%endBr(nbr) )
                        
         do j = 1, nBr-1
            stack(ifId)%startBr(j) = sbr(j  ,opif)+1
            stack(ifId)%  endBr(j) = sbr(j+1,opif)-1
         end do 
         stack(ifId)%startBr(nBr) = sbr(nBr,opif)+1
         stack(ifId)%  endBr(nBr) = i-1
               
         opif = opif - 1

      else if ( stack(i)%rec == "else" ) then

         if ( opif <= 0 ) then
            call flagerr%set ( msg = 'Unexpected "else"', where = HERE, stat = G_UERROR )
            return
         end if
         
         stack(i)%blk = -G_IFBLOCK

         ifId = lif(opif)
         stack(ifId)%nBr = stack(ifId)%nBr + 1
         sbr(stack(ifId)%nBr,opif) = i ; tbr(opif) = 2
            
      !else if (index(stack(i)%rec, "elseif ") == 1) then 
              
      else if ( index(stack(i)%rec, "else") == 1 ) then
!
!-       It starts with "else". See if it is an "elseif":
!      
         if ( stack(i)%rec(5:7) /= "if " ) then
!
!-          This is not an "elseif". See if it is an "else if". 
!           If so, transform it to "elseif". If not, cycle.
!         
            stack(i)%rec = adjustl(stack(i)%rec(5:))
            
            if ( index(stack(i)%rec,"if ") == 1 ) then
               stack(i)%rec = "else"//trim(stack(i)%rec) 
            else
               cycle 
            end if
         end if
               
         if ( opif <= 0 ) then
            call flagerr%set ( msg = 'Unexpected "elseif"', where = HERE, stat = G_UERROR )
            return
         end if
         
         stack(i)%blk = -G_IFBLOCK

         if ( tbr(opif) == 2 ) then
            call flagerr%set ( msg = '"elseif" after "else"', where = HERE, stat = G_UERROR )
            return
         end if
                     
         ifId = lif(opif)
         stack(ifId)%nBr = stack(ifId)%nBr + 1
         sbr(stack(ifId)%nBr,opif) = i
      end if
         
   end do
!
!- Check for overlapping blocks:
!   
   do i = 1, self%nStack
      if ( stack(i)%blk == G_FORBLOCK ) then
         m = i + stack(i)%nlink
         do j = i+1, m-1
            if ( stack(j)%blk == G_IFBLOCK ) then
               if ( j + stack(j)%nlink > m ) then
                  call flagerr%set ( msg='Overlapping FOR/IF', where=HERE, stat=G_UERROR )
                  return
               end if  
            end if
         end do
      else if ( stack(i)%blk == G_IFBLOCK ) then
         m = i + stack(i)%nlink
         do j = i+1, m-1
            if ( stack(j)%blk == G_FORBLOCK ) then
               if ( j + stack(j)%nlink > m ) then
                  call flagerr%set ( msg='Overlapping IF/FOR', where=HERE, stat=G_UERROR )
                  return
               end if  
            end if
         end do
      end if     
   end do    
   
   END ASSOCIATE !end of association stack => stack%self
   
   END SUBROUTINE CalmatInstruction_completeStack   
      
   
!=============================================================================================
   SUBROUTINE CalmatInstruction_destroy ( self, flagerr )
!=============================================================================================
   class(ins_t), intent(in out) :: self
   type (err_t), intent(   out) :: flagerr
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatInstruction_destroy'
   integer  (Ikind)            :: i, err
!---------------------------------------------------------------------------------------------

   err = 0
   
   self%parsed = .false.

   if ( err == 0 .and. allocated(self%rec) ) deallocate(self%rec,stat = err)
   
   if ( err == 0 ) then
      self%flowId       = 0
      self%numl         = 0
      self%verb         = 1
      self%globalInsNum = 0
      self%nextFlow     = 0
      self%breakpt      = 0
      self%blk          = 0
      self%nlink        = 0
      self%idCmd        = 0
      if ( allocated(self%rhs) ) deallocate(self%rhs,stat = err)
      call self%rhsHdl%destroy()
   end if
   
   if ( err == 0 ) then
      if ( allocated(self%rhsCmps) ) deallocate(self%rhsCmps,stat = err)
      self%rhsNCmp = 0
   end if

   if ( err == 0 ) then
      if ( allocated(self%lhs) ) deallocate(self%lhs,stat = err)
      self%nlhs = 0 
   end if
   
   if ( err == 0 .and. allocated(self%lhsNames) ) deallocate(self%lhsNames,stat = err)
   if ( err == 0 .and. allocated(self%lhsVarId) ) deallocate(self%lhsVarId,stat = err)
   if ( err == 0 .and. allocated(self%lhsIndex) ) deallocate(self%lhsIndex,stat = err)   
   if ( err == 0 .and. allocated(self%cName)    ) deallocate(self%cName,stat = err)
   
   if ( err == 0 ) then
      self%cValue = 0
      self%cIvar = 0
   
      do i = 1, 3
         if ( allocated(self%cExpr(i)%str) ) deallocate(self%cExpr(i)%str,stat = err)
         self%cVal(i) = 0
         call self%cHdl(i)%destroy()
      end do
   end if
   
   if ( err == 0 ) then
      self%nBr = 0
      if ( err == 0 .and. allocated(self%startBr) ) deallocate(self%startBr,stat = err)
      if ( err == 0 .and. allocated(self%endBr  ) ) deallocate(self%endBr,stat = err)
      if ( err == 0 .and. allocated(self%tExpr  ) ) deallocate(self%tExpr,stat = err)
      if ( err == 0 .and. allocated(self%tVal   ) ) deallocate(self%tVal,stat = err)
   
      if ( err == 0 .and. allocated(self%tHdl) ) then
         do i = 1, size(self%tHdl)
            call self%tHdl(i)%destroy()
         end do
         deallocate(self%tHdl,stat = err)
      end if
   end if
   
   if ( err /= 0 ) call flagerr%set ( msg = 'A de-allocation problem has occured', &
                                      where = HERE, stat = G_IERROR )   
   
   END SUBROUTINE CalmatInstruction_destroy
   

!=============================================================================================
   SUBROUTINE CalmatInstruction_Finalizer ( self )
!=============================================================================================
   type(ins_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------      
   type(err_t) :: flagerr
!---------------------------------------------------------------------------------------------

   call self%destroy(flagerr) 

   END SUBROUTINE CalmatInstruction_Finalizer
   

!=============================================================================================
   SUBROUTINE CalmatInstruction_printFlow ( self )
!=============================================================================================
   class(flw_t), intent(in ) :: self
!---------------------------------------------------------------------------------------------
!   ... just for debugging purpose ...
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------      
   integer :: i
!---------------------------------------------------------------------------------------------

   if (allocated(self%expression)) then
      print*,'expression:', self%expression
   else
      print*,'expression: (not allocated)'
   end if
   if (allocated(self%filename)) then
      print*,'filename:', self%filename
   else
      print*,'filename: (not allocated)'
   end if   
   print*,'curLine, unit:',self%curLine, self%unit
   print*,'maxnStack, nStack:',self%maxnStack, self%nStack
   if (allocated(self%stack)) then
      print*,'stack:'
      do i = 1, self%nStack
         print*,'. parsed, assignment:',self%stack(i)%parsed,self%stack(i)%assignment
         if (allocated(self%stack(i)%rec)) then
            print*,'. rec:',self%stack(i)%rec
         else
            print*,'. rec: (not allocated)'
         end if
         print*,'. flowId, numl, verb, blk:',self%stack(i)%flowId,self%stack(i)%numl,&
                                           self%stack(i)%verb,self%stack(i)%blk
         print*,'. globalInsNum, nextFlow, breakpt:', self%stack(i)%globalInsNum, &
                                     self%stack(i)%nextFlow, self%stack(i)%breakpt
         if (allocated(self%stack(i)%rhs)) then
            print*,'. rhs:',self%stack(i)%rhs
         else
            print*,'. rhs: (not allocated)'
         end if
         if (allocated(self%stack(i)%lhs)) then
            print*,'. lhs:',self%stack(i)%lhs
         else
            print*,'. lhs: (not allocated)'
         end if         
      end do
   else
      print*,'stack (not allocated)'
   end if
   
   END SUBROUTINE CalmatInstruction_printFlow 
        
END MODULE CalmatInstruction_m