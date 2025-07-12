!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Module CalmatUtil
!
! This module contains some usefull procedures:
!
! . CalmatUtil_Error              : print error or warning messages
! . CalmatUtil_getAFreeNumFlow    : get a free flow #
! . CalmatUtil_AllocFlow          : allocate or re-allocate the array of flows
! . CalmatUtil_AssignAndPrint     : move  a variable into another one and print it
! . CalmatUtil_IeeeSignalException: signal an ieee exception
! . CalmatUtil_FindVar            : find the # of a variable in the global list
! . CalmatUtil_FindObj            : find the # of an object in the global list
! . CalmatUtil_AddVar             : add a variable in the global list of variables
! . CalmatUtil_AddObj             : add an object in the global list of objects
! . CalmatUtil_RemoveVar          : remove a variable from the global list
! . CalmatUtil_RemoveObj          : remove an object from the global list
! . CalmatUtil_GetAFreeNumVar     : get a free variable # in the global list
! . CalmatUtil_GetAFreeNumObj     : get a free object # in the global list
! . CalmatUtil_CheckName          : see if a given name is suited for a variable name
! . CalmatUtil_GetIndx            : evaluates a set of indices of a variable
! . CalmatUtil_ExecuteCommandLine : call to execute_command_line
! . CalmatUtil_display            : print the value(s) of a variables or an object
! . CalmatUtil_welcome            : print the welcome mesage
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Date: 11/18
! Modified: 03/19
!---------------------------------------------------------------------------------------------

#include "error.fpp"

MODULE CalmatUtil_m

   use ansiColor_m
   use pk2interpreter_m
   use dispmodule ; use disp_i8mod
   
   use CalmatGlobal_m
   
   implicit none   
   
   interface CalmatUtil_Display
      module procedure CalmatUtil_ObjDisplay, CalmatUtil_Pk2Display
   end interface   
      
CONTAINS


!============================================================================================= 
   SUBROUTINE CalmatUtil_Error ( empty, abort, rec, file, line )  
!=============================================================================================   
   logical         ,           intent(in) :: empty
   logical         , optional, intent(in) :: abort   
   character(len=*), optional, intent(in) :: rec, file
   integer  (Ikind), optional, intent(in) :: line
!---------------------------------------------------------------------------------------------    
!  Prints error or warning message on the output unit and resets G_flagerr. Moreover,
!
!  returns G_task = G_STOP when
!
!                       G_flagerr%code > 0  &  abort == .true.
!
!              or when  G_flagerr%code = G_IERROR (internal error)
!
!  otherwise empties the stack, closes the script(s) and sets G_task = G_ERR when
!
!                       G_flagerr%code > 0  &  empty == .true.
!---------------------------------------------------------------------------------- R.H. 11/18

!- local variables ---------------------------------------------------------------------------   
   character(len=99) :: cnum
   integer  (Ikind ) :: i, ivar
   type     (err_t ) :: flag
   logical           :: quit
!---------------------------------------------------------------------------------------------

   if ( G_flagerr == IZERO ) return ! no error

   call err_moveAlloc (from = G_flagerr, to = flag)

   if ( present(rec) ) flag%mesg =  &
      trim(flag%mesg) // NL // "--> Where: in expression << " // trim(rec) // " >>"
   
   if ( present(file) ) then
      if ( len_trim(file) > 0 ) then
         if ( present(line) ) then
            write(cnum,'(i0)') line
            flag%mesg = trim(flag%mesg) // NL //  &
                         "--> (at line #"//trim(cnum)// " of the file '"//trim(file)//"')"
         else
            flag%mesg = trim(flag%mesg) // NL //  "--> (file: '"//trim(file)//"')"
         end if
       end if      
    end if
    
    quit = .false. ; if ( present(abort) ) quit = abort
!
!- If requested when an error has occured: delete the stack, delete all variables with a 
!  temporary status and close the script(s):
!   
   if ( flag > IZERO ) then
   
      if ( quit .or. flag == G_IERROR .or. .not. G_dispPrompt ) then
      
         G_task = G_STOP
         flag%mesg = flag%mesg // NL // '--> ABORT (calmat)'
        
      else if ( empty ) then
      
         do i = 1, G_maxnFlow
            call G_flows(i)%destroyFlow ( G_flagerr )
            
            if ( G_flagerr > IZERO ) flag%mesg = flag%mesg // NL // G_flagerr%mesg
         end do     
          
         do i = 1, G_nvar
            !!if ( G_vars(i)%Status == G_TEMPORARY ) then
            if ( abs(G_vars(i)%Status) == G_TEMPORARY ) then
               ivar = i ; call CalmatUtil_RemoveVar ( ivar, G_flagerr ) 
            end if
         end do

         call CalmatUtil_AllocFlow ( IZERO, G_flagerr )
                  
         if ( G_flagerr > IZERO ) flag%mesg = flag%mesg // NL // G_flagerr%mesg

         G_curFlow = CalmatUtil_getAFreeNumFlow ( G_flagerr, fileName = '' )
         
         if ( G_flagerr > IZERO ) flag%mesg = flag%mesg // NL // G_flagerr%mesg
         
         G_task = G_ERR

      end if      
      
   end if
!
!- Display the error/warning message:
!      
   call flag%display ( unit=G_uo, verb=G_verb, title='--> Calmat Info:', trace=G_traceback )           
!
!- If flag has not been reset by %display (G_verb <= 0) return with G_flagerr = flag:
!   
   if ( flag > IZERO ) call err_moveAlloc ( from = flag, to = G_flagerr ) 
   opflag = err_t()
!
!- Abort if requested:
!   
   !!if ( G_task == G_STOP ) stop ! do not stop here, leave this job to the caller 

   END SUBROUTINE CalmatUtil_Error 


!=============================================================================================   
   FUNCTION CalmatUtil_getAFreeNumFlow ( flagerr, fileName, expression ) result ( iflow )
!=============================================================================================   
   type     (err_t),           intent(in out) :: flagerr
   character(len=*), optional, intent(in    ) :: fileName, expression
   integer  (Ikind)                           :: iflow
!---------------------------------------------------------------------------------------------   
!  Finds a free flow # in the list "G_flows" and connects it with the file "fileName" 
!  (if "fileName" is the empty string the flow will be connected to the stdin) or to the
!  expression "expression".
!
!  If there is no free # available, resizes this list "G_flows".
!
!  Note: iflow is a free flow # if G_flows(iflow)%used = .false.
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatUtil_getAFreeNumFlow'
   integer  (Ikind)            :: i
!---------------------------------------------------------------------------------------------   
   
   iflow = 0
   do i = 1, G_maxnFlow
      if ( .not. G_flows(i)%used ) then
         iflow = i
         exit
      end if
   end do
   
   if ( iflow == 0 ) then
!
!-    No free # available. Set iflow = G_maxnFlow + 1 and increase the number of flows.
!
      iflow = G_maxnFlow + 1
   
      call CalmatUtil_AllocFlow ( iflow, flagerr )
      if_error_trace_and_RETURN ( flagerr, HERE )
   end if
   
   if ( present(fileName) ) then
      call G_flows(iflow)%initFlow ( flagerr, fileName = fileName )
   else if ( present(expression) ) then
      call G_flows(iflow)%initFlow ( flagerr, expression = expression )
   else
      call flagerr%set ( stat=G_UERROR, where=HERE, msg="Missing file name or expression" )
      return
   end if
   
   if_error_trace_and_RETURN ( flagerr, HERE )
   
   G_task = G_CONTINUE
   
   END FUNCTION CalmatUtil_getAFreeNumFlow


!============================================================================================= 
   SUBROUTINE CalmatUtil_AllocFlow ( n, flagerr )
!============================================================================================= 
   integer  (Ikind), intent(in    ) :: n
   type     (err_t), intent(in out) :: flagerr   
!---------------------------------------------------------------------------------------------
!  Allocate or reallocate G_flows with a size larger than n
!
!  . If n = 0 (initialization or re-initilization): 
!       G_flows is deallocated (if allocated) and allocated with a maximum size of 
!       G_maxnFlow = G_incnFlow.
!
!  . If n < G_maxnFlow: 
!       G_flow is unchanged.
!
!  . If n >= G_maxnFlow: 
!       G_flow is resized to n + G_incnFlow and the G_maxnFlow first elements are preserved.
!---------------------------------------------------------------------------------- R.H. 04/20

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'CalmatUtil_AllocFlow'
   type     (flw_t), allocatable :: tmp(:)
   integer  (Ikind)              :: maxOld, i, err
!---------------------------------------------------------------------------------------------

   if ( n == 0 .and. allocated(G_flows) ) then
      err = 0
      do i = 1, size(G_flows); call G_flows(i)%destroyFlow(flagerr); enddo
      !!deallocate(G_flows, stat = err)
      if ( err > 0 ) then
         call flagerr%set ( stat = G_IERROR, where = HERE, &
                             msg = "Deallocation failure of 'G_flows'" )
         return
      end if
   end if

   if ( .not. allocated(G_flows) ) then
   
      G_maxnFlow = G_incnFlow
      if ( n > G_maxnFlow ) G_maxnFlow = G_maxnFlow + n
      
      allocate(G_flows(G_maxnFlow), stat = err)
      
      if ( err > 0 ) then
         call flagerr%set ( stat = G_IERROR, where = HERE, &
                             msg = "Allocation failure for 'G_flows'" )
         return
      end if

      do i = 1, G_maxnFlow
         call G_flows(i)%allocateStack ( IZERO, flagerr )
         if_error_trace_and_RETURN ( flagerr, HERE )
      end do

      if ( G_dispPrompt ) G_flows(1)%unit = STDIN   !!! a voir s'il faut le laisser ici     
      
   else if ( n >= G_maxnFlow ) then

      maxOld = G_maxnFlow
      
      G_maxnFlow = G_incnFlow + n
      allocate(tmp(G_maxnFlow), stat = err)
      
      if ( err >  0) then
         call flagerr%set ( stat = G_IERROR, where = HERE, &
                            msg = " Allocation failure for 'tmp'" )
         return
      end if
      
      do i = 1, maxOld
         tmp(i)%used      = G_flows(i)%used
         tmp(i)%unit      = G_flows(i)%unit
         tmp(i)%curLine   = G_flows(i)%curLine
         tmp(i)%maxnStack = G_flows(i)%maxnStack
         tmp(i)%nStack    = G_flows(i)%nStack
         
         if ( allocated(G_flows(i)%fileName) ) &
            call move_alloc (from = G_flows(i)%fileName, to = tmp(i)%fileName)
         if ( allocated(G_flows(i)%stack) ) &
            call move_alloc (from = G_flows(i)%stack, to = tmp(i)%stack)
      end do   
      
      !tmp(1:maxOld) = G_flows(1:maxOld)
            
      call move_alloc ( from = tmp, to = G_flows )
      
      do i = maxOld+1, G_maxnFlow
         call G_flows(i)%allocateStack ( IZERO, flagerr )
         if_error_trace_and_RETURN ( flagerr, HERE )
      end do
      
   end if

   END SUBROUTINE CalmatUtil_AllocFlow


!============================================================================================= 
   SUBROUTINE CalmatUtil_AssignAndPrint ( instr, nanswer, rhsval, flagerr )
!============================================================================================= 
   type     (ins_t),              intent(in out) :: instr
   integer  (Ikind),              intent(in    ) :: nanswer
   type     (pk2_t),              intent(in out) :: rhsval(:)
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Moves the result(s) of the evaluation (rhsval) to the corresponding lhs variable(s)
!  Prints the result(s) if requested.
!---------------------------------------------------------------------------------------------    

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'CalmatUtil_AssignAndPrint'
   logical                       :: whole
   integer  (Ikind)              :: i, ivar
   integer  (Ikind), allocatable :: indi(:), indj(:)
!---------------------------------------------------------------------------------------------    

   if ( instr%nlhs > 0 .and. instr%assignment ) then
!
!-    Case with lhs:
!      
      do i = 1, nanswer
         ivar = instr%lhsVarId(i) ! the variable to assign
                
         if ( ivar > 0 ) then
         
            if ( len(instr%lhsIndex(i)%str) == 0 ) then  
!
!-             Assignment of the kind "b = a":
!            
               call pk2_moveAlloc (from = rhsval(i), to = G_vars(ivar), movename = .false.)
               
            else
!
!-             Submatrix assignment. First, evaluate the string corresponding to the indexes
!              (instr%lhsHdl(i)%stokens(2)%str). The result(s) is stored into "indi" 
!              and "indj":
!   
               call CalmatUtil_GetIndx ( instr%lhsIndex(i)%str, ivar,    &
                                         rhsval(i)%nrow, rhsval(i)%ncol, &
                                         indi, indj, flagerr           )

               call CalmatUtil_IeeeSignalException ( flagerr )
               if_error_trace_and_RETURN ( flagerr, HERE )

               if ( size(indi) == 0 .and. size(indj) == 0 ) then
!
!-                Assignment of the kind "b(:) = a" with b initially empty (--> same as b = a)
!
                  call pk2_moveAlloc (from = rhsval(i), to = G_vars(ivar), movename = .false.)
                              
               else if ( size(indj) == 0 ) then
!
!-                Assignment of the kind "b(indi) = a"
!
                  call G_vars(ivar)%SetSubmat ( rhsval(i), indi, stat = flagerr )
                  
               else
!
!-                Assignment of the kind "b(indi,indj) = a"
!
                  call G_vars(ivar)%SetSubmat ( rhsval(i), indi, indj, stat = flagerr )
                     
               end if      

               if_error_trace_and_RETURN ( flagerr, HERE )
               
            end if                      
!
!-          Display the result if requested:
!            
            if ( instr%verb > 0 .and. G_disp ) then
               whole = .false. ; if (instr%verb >= 2) whole = .true.
               call CalmatUtil_display ( a = G_vars(ivar), whole = whole )
            end if    
            
            G_vars(ivar)%status = G_USED !essai (05/24)
                        
         end if ! end if (ivar > 0)
      end do
      
   else
!
!-    Display the result if requested:
!         
      if ( instr%verb > 0 .and. G_disp ) then
         whole = .false. ; if (instr%verb >= 2) whole = .true.
         call CalmatUtil_display ( a = rhsval(1), whole = whole )
      end if   
   end if
!
!- If appropriate, delete the variables whose names are now used as object names
!  (i.e. those marked in Calmat_lhsAnalysis by the status "G_DELETE"):
! 
   do i = 1, G_nvar
      if ( G_vars(i)%status /= G_FREE .and. G_vars(i)%deleteAt == instr%globalInsNum ) then
         call CalmatUtil_RemoveVar ( i, flagerr )
         if_error_trace_and_RETURN ( flagerr, HERE )
      end if
   end do
!
!- If appropriate, delete the objects whose names are now used as variable names:
!  (i.e. those marked in Calmat_lhsAnalysis by the status "G_DELETE"):
!
   do i = 1, G_nobj
      if ( G_objs(i)%status /= G_FREE .and. G_objs(i)%deleteAt == instr%globalInsNum ) then
         call CalmatUtil_RemoveObj ( i, flagerr ) 
         if_error_trace_and_RETURN ( flagerr, HERE )
      end if
   end do   

   END SUBROUTINE CalmatUtil_AssignAndPrint
   

!=============================================================================================
   SUBROUTINE CalmatUtil_IeeeSignalException ( flagerr )
!=============================================================================================
   type(err_t), intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Signals any ieee exception by adding a warning message into flagerr
!---------------------------------------------------------------------------------- R.H. 09/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'CalmatUtil_IeeeSignalException' 
   logical                       :: ieee_flags(5)
   character(len=:), allocatable :: msg
!---------------------------------------------------------------------------------------------    

!
!- Get the ieee flag values:
!   
   call ieee_get_flag (ieee_all, ieee_flags)

   if ( any(ieee_flags(1:4)) ) then
!
!-    add a warning message to G_flagerr:
!
      if ( ieee_flags(4) ) msg = 'An underflow occurred'
      if ( ieee_flags(3) ) msg = 'A floating invalid operation occurred'
      if ( ieee_flags(2) ) msg = 'A divide by zero occurred' 
      if ( ieee_flags(1) ) msg = 'An overflow occurred' 
      
      if ( flagerr == IZERO ) then
         call flagerr%set ( stat = G_WARNING, where = HERE, msg = msg )
      else
         flagerr%mesg = flagerr%mesg // NL // '--> Warning: ' // msg  
      end if
!
!-    Reset all flags to .false.:         
!
      call ieee_set_flag (ieee_all, flag_value = G_QUIET)
      
   end if   
   
   END SUBROUTINE CalmatUtil_IeeeSignalException


!=============================================================================================
   SUBROUTINE CalmatUtil_FindVar ( varName, varId, size, type )
!============================================================================================= 
   character(len=*),           intent(in    ) :: varName
   integer  (Ikind), optional, intent(   out) :: varId, size(2)
   character(len=1), optional, intent(   out) :: type
!---------------------------------------------------------------------------------------------    
!  See if "varName" is a variable name (in the list "G_vars"). 
!  If so, returns 
!  - if varId is present: its index in G_vars (varId = 0 if varName is not in G_vars)
!  - if size  is present: the dimensions (nrow and ncol) of its container
!  - if type  is present: the type ('I', 'R', 'C', 'L', 'S') of its container
!---------------------------------------------------------------------------------- R.H. 09/18
   
!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------
      
   do i = 1, G_nVar
      if ( trim(adjustl(varName)) == trim(adjustl(G_vars(i)%GetName())) ) then
         if ( present(varId) ) varId = i
         if ( present(size) ) then
            size(1) = G_vars(i)%nrow ; size(2) = G_vars(i)%ncol
         end if
         if ( present(type ) ) then
            select case (G_vars(i)%typ)
               case ( EMPTY ) ; type = 'E'
               case ( ITYP  ) ; type = 'I'
               case ( RTYP  ) ; type = 'R'
               case ( CTYP  ) ; type = 'C'
               case ( LTYP  ) ; type = 'L'
               case ( STYP  ) ; type = 'S'  
            end select
         end if
         return
      end if
   end do

   if ( present(varId) ) varId = IZERO
   if ( present(size)  ) size  = IZERO
   if ( present(type)  ) type  = '-' ! means not in the G_vars list
    
   END SUBROUTINE CalmatUtil_FindVar

   
!=============================================================================================
   SUBROUTINE CalmatUtil_FindObj ( objName, objId )
!============================================================================================= 
   character(len=*), intent(in    ) :: objName
   integer  (Ikind), intent(   out) :: objId
!---------------------------------------------------------------------------------------------    
!  See if "objName" is an object name (in the list "G_objs"). If so, returns its #
!  Otherwise returns objId = 0.
!---------------------------------------------------------------------------------- R.H. 09/18
   
!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i
!---------------------------------------------------------------------------------------------
      
   objId = 0
   do i = 1, G_nobj
      if ( allocated(G_objs(i)%name) ) then
         if ( trim(adjustl(objName)) == trim(adjustl(G_objs(i)%name)) ) then
            objId = i
            return
         end if
      end if   
   end do
    
   END SUBROUTINE CalmatUtil_FindObj


!=============================================================================================
   SUBROUTINE CalmatUtil_AddVar ( varName, varStat, varId, is_new, flagerr )
!=============================================================================================
   character(len=*), intent(in    ) :: varName
   integer  (Ikind), intent(in    ) :: varStat
   integer  (Ikind), intent(   out) :: varId
   logical         , intent(   out) :: is_new
   type     (err_t), intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------
!  Adds a variable of name "varName" into the list of variables G_vars if this name is not 
!  already present and sets its status to "varStat".
!---------------------------------------------------------------------------------------------    
   
!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'CalmatUtil_AddVar'
!---------------------------------------------------------------------------------------------

   call CalmatUtil_FindVar ( varName = varName, varId = varId )
   
   if ( varId == 0 ) then   
!
!-    It is not an existing variable: create it
!
      is_new = .true.
      
      call CalmatUtil_CreateNewVar ( varName, varStat, G_ins, varId, flagerr )
      if_error_trace_and_RETURN ( flagerr, HERE )

   else
!
!-    It is an existing variable: check if protected
!   
      is_new = .false.
      
      if ( G_vars(varId)%status == G_PROTECTED .or. &
           G_vars(varId)%status == G_USERPROTECTED   ) then
!
!-       but if it has the protected status: error
!
         call flagerr%set ( stat = G_UERROR, where = HERE, msg =                        &
                            'The value of a protected variable can not be changed ' //  &
                            '(<< ' //  trim(varName) // ' >>)' // NLT               //  &
                            'To modify a user-protected variable use the "set" command' )
      end if 

      G_vars(varId)%lastIns = G_ins ! last occurence of this variable as lhs

      G_varNames(varId)%str = (varName)

   end if
   
   END SUBROUTINE CalmatUtil_AddVar
   

!=============================================================================================
   SUBROUTINE CalmatUtil_CreateNewVar ( varName, varStat, instrId, varId, flagerr )
!=============================================================================================
   character(len=*), intent(in    ) :: varName
   integer  (Ikind), intent(in    ) :: varStat, instrId
   integer  (Ikind), intent(   out) :: varId
   type     (err_t), intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------
!  Creates a new variable
!  Caution: should be called only when the variable (of name "varName") is not already present
!  in the G_vars list
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'CalmatUtil_CreateNewVar'
!---------------------------------------------------------------------------------------------

   call CalmatUtil_CheckName ( varName, flagerr ) 
   if_error_trace_and_RETURN ( flagerr, HERE )
      
   varId = CalmatUtil_GetAFreeNumVar ( flagerr )
   if_error_trace_and_RETURN ( flagerr, HERE )
      
   call G_vars(varId)%Reset ( name     = varName, &
                              status   = varStat, & 
                              firstIns = instrId, & ! 1st occurence of this variable as lhs
                              lastIns  = instrId, & ! last occurence
                              objId    = IZERO  , &
                              cmpObjId = IZERO  )
                                 
   G_varNames(varId)%str = (varName) ! the current list of variable names
      
   if ( varId > G_nVar ) G_nVar = varId ! new used variables number
   
   END SUBROUTINE CalmatUtil_CreateNewVar
   
   
!=============================================================================================
   SUBROUTINE CalmatUtil_AddObj ( objName, objStat, objId, is_new, flagerr )
!=============================================================================================
   character(len=*), intent(in    ) :: objName
   integer  (Ikind), intent(in    ) :: objStat
   integer  (Ikind), intent(   out) :: objId
   logical         , intent(   out) :: is_new
   type     (err_t), intent(in out) :: flagerr   
!---------------------------------------------------------------------------------------------
!  Adds an object of name "objName" into the list of objects G_objs if this name is not 
!  already present and sets its status to "objStat".
!---------------------------------------------------------------------------------------------    
   
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'CalmatUtil_AddObj'  
!---------------------------------------------------------------------------------------------

   call CalmatUtil_FindObj (objName, objId)
            
   if ( objId == 0 ) then 
!
!-    It's not an existing object
!
      is_new = .true.
      
      call CalmatUtil_CheckName ( objName, flagerr )
      if_error_trace_and_RETURN ( flagerr, HERE )
      
      objId = CalmatUtil_GetAFreeNumObj ( flagerr )
      if_error_trace_and_RETURN ( flagerr, HERE )
                        
      G_objs(objId) = obj_t ( objName, objStat, G_ins, G_ins, -IONE, flagerr ) ! create the object 
      if_error_trace_and_RETURN ( flagerr, HERE )

      G_objNames(objId)%str = (objName)
      
      if ( objId > G_nObj ) G_nObj = objId ! new used objects number 
   else
!
!-    It's an existing object
!      
      is_new = .false.     

      G_objs(objId)%lastIns = G_ins

      G_objNames(objId)%str = (objName)
            
   end if

   END SUBROUTINE CalmatUtil_AddObj


!=============================================================================================
   SUBROUTINE CalmatUtil_RemoveVar ( iVar, flagerr )
!============================================================================================= 
   integer(Ikind), intent(in    ) :: iVar
   type   (err_t), intent(in out) :: flagerr   
!---------------------------------------------------------------------------------------------    
!  Deletes the content of the variable #iVar and sets its location in G_vars as free.
!---------------------------------------------------------------------------------- R.H. 09/18
   
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'CalmatUtil_RemoveVar'
   integer  (Ikind)            :: iObj
!---------------------------------------------------------------------------------------------
      
   if ( iVar < 1 .or. iVar > G_maxnVar )     return
   if ( G_vars(iVar)%status == G_PROTECTED ) return
   
   iObj = G_vars(iVar)%objId
   
   if ( iObj /= 0 ) then
      call G_objs(iObj)%RemoveAComponent ( iVar, flagerr )
      if_error_trace_and_RETURN ( flagerr, HERE )
      if ( G_objs(iObj)%nCmp == 0 ) call G_objs(iObj)%Frees()
   end if
   
   call G_vars(iVar)%Destroy ()
   
   G_varNames(iVar)%str = ''
   
   END SUBROUTINE CalmatUtil_RemoveVar
   
   
!=============================================================================================
   SUBROUTINE CalmatUtil_RemoveObj ( iObj, flagerr )
!============================================================================================= 
   integer(Ikind), intent(in    ) :: iObj
   type   (err_t), intent(in out) :: flagerr   
!---------------------------------------------------------------------------------------------    
!  Deletes the object # iobj and removes it from the list G_objs.
!---------------------------------------------------------------------------------- R.H. 09/18
   
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'CalmatUtil_RemoveObj'  
   integer  (Ikind)            :: i, iVar
!---------------------------------------------------------------------------------------------
   
   if ( iObj < 1 .or. iObj > G_maxnObj ) return
!
!- First delete the variables attached to this object:
!   
   do i = 1, G_objs(iObj)%nCmp
      iVar = G_objs(iObj)%varId(i)
      call CalmatUtil_RemoveVar ( iVar, flagerr )
      if_error_trace_and_RETURN ( flagerr, HERE )
   end do      
!
!- Delete the object:
!   
   call G_objs(iObj)%Destroy ()
   
   END SUBROUTINE CalmatUtil_RemoveObj
   

!=============================================================================================
   FUNCTION CalmatUtil_GetAFreeNumVar ( flagerr ) result ( ivar )
!=============================================================================================   
   type   (err_t), intent(in out) :: flagerr
   integer(Ikind)                 :: ivar
!---------------------------------------------------------------------------------------------   
!  Finds a free variable # in the list "G_vars". 
!  If there is no free # available, resizes this list. 
!
!  Note: a free variable is such that G_vars(i)%status = G_FREE
!---------------------------------------------------------------------------------- R.H. 09/18
    
!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatUtil_GetAFreeNumVar'  
   integer  (Ikind)            :: i
!---------------------------------------------------------------------------------------------    

!
!- Find the first free variable in the list:
!
   ivar = 0
   do i = 1, G_maxnVar
      if ( G_vars(i)%status == G_FREE ) then
         ivar = i
         return
      end if
   end do   
!
!- No free # available. 
!  Set ivar = G_maxnVar + 1, increase G_maxnVar and resize G_vars and G_varNames:
!   
   ivar = G_maxnVar + 1 ; G_maxnVar = G_maxnVar + G_incnVar
   
   call CalmatVar_alloc1 (mode = 's', t = G_vars, n = G_maxnVar, stat = flagerr)
   if_error_trace_and_RETURN ( flagerr, HERE )
     
   call util_alloc (mode = 's', t = G_varNames, n = G_maxnVar, stat = flagerr)
   if_error_trace_and_RETURN ( flagerr, HERE )
   
   END FUNCTION CalmatUtil_GetAFreeNumVar


!=============================================================================================
   FUNCTION CalmatUtil_GetAFreeNumObj ( flagerr ) result ( iobj )
!============================================================================================= 
   type   (err_t), intent(in out) :: flagerr 
   integer(Ikind)                 :: iobj
!---------------------------------------------------------------------------------------------   
!  Finds a free object # in the list "G_objs". 
!  If there is no free # available, resizes this list. 
!
!  Note: a free object is such that G_objStatus(1,i) = G_FREE
!---------------------------------------------------------------------------------- R.H. 09/18
    
!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatUtil_GetAFreeNumObj'        
   integer  (Ikind)            :: i
!---------------------------------------------------------------------------------------------    

!
!- Find the first free object in the list:
!
   iobj = 0
   do i = 1, G_maxnObj
      if ( G_objs(i)%status == G_FREE ) then
         iobj = i
         return
      end if
   end do   
!
!- No free # available. 
!  Set iobj = G_maxnObj + 1, increase G_maxnObj and resize "G_objs" and "G_objNames":
!   
   iobj = G_maxnObj + 1 ; G_maxnObj = G_maxnObj + G_incnObj
   
   call CalmatObject_alloc1 (mode = 's', t = G_objs, n = G_maxnObj, stat = flagerr) 
   if_error_trace_and_RETURN ( flagerr, HERE )
     
   call util_alloc (mode = 's', t = G_objNames, n = G_maxnObj, stat = flagerr)
   if_error_trace_and_RETURN ( flagerr, HERE )
   
   END FUNCTION CalmatUtil_GetAFreeNumObj


!=============================================================================================
   SUBROUTINE CalmatUtil_CheckName ( name, flagerr, exclude_non_printable )
!=============================================================================================   
   character(len=*),           intent(in    ) :: name
   type     (err_t),           intent(in out) :: flagerr
   logical         , optional, intent(in    ) :: exclude_non_printable
!---------------------------------------------------------------------------------------------    
!  Checks if "name" is well suited for a variable name
!
!  Modified (05/24): if "exclude_non_printable" is not present or .true. exclude non-printable
!                    characters (ascii codes < 33 or > 127) if 
!                    
!---------------------------------------------------------------------------------- R.H. 09/18
 
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'CalmatUtil_CheckName'
   integer  (Ikind)            :: i, ich, ich1, ich2
   character(len=*), parameter :: forbiden = '0123456789+-*/:$,;"^|&~ '//"'"
   character(len=1)            :: ch
!---------------------------------------------------------------------------------------------   
      
   if ( index(forbiden(1:10),name(1:1)) /= 0 ) then
      call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                        'The name of a variable cannot start with a number (<< ' // &
                         trim(name)//' >>)' )
      return
   end if         

   ich1 = 33 ; ich2 = 127
   if ( present(exclude_non_printable) ) then
      if ( .not. exclude_non_printable ) then
         ich1 = 0 ; ich2 = 256
      end if
   end if
   
   do i = 1, len_trim(name)
      ch = name(i:i) ; ich = iachar(ch)
      if ( ich < ich1 .or. ich > ich2 .or. index(forbiden(11:),ch) /= 0 ) then
      !if ( index(forbiden(11:),name(i:i)) /= 0 ) then
         call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
         'Illegal character found in the the name of a variable (<< ' //trim(name)//' >>)' )
         return
      end if
   end do

   if ( .not. allocated(G_FuncNames) .or. .not. allocated(G_CmdNames) ) then
      call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
               'Lists of functions or commands are not initialised' )
      return
   end if

! Exclude or not? Is there really a danger? 
!
! If, e.g., the user set : sin=123, the sine function is still available, he can set e.g.
! x=sin(3)+sin (i.e. = sin(3)+123)
!
!    do i = 1, size(G_FuncNames)
!       if ( allocated(G_FuncNames(i)%str) ) then
!          if ( trim(G_FuncNames(i)%str) == trim(name) ) then
!             call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
!             'The name of a variable should not be the name of a function (<< ' // &
!             trim(name)//' >>)' )
!             return
!          end if   
!       end if   
!    end do
!
! However, if he set ls = 123, and then type ls, this will execute the linux command ls and
! not the print of the variable ls (i.e. 123) but he can use it: ls + 1 will give 124. The
! only way he has to print the variable ls is to use disp(ls).
! 
!    do i = 1, size(G_CmdNames)
!       if ( allocated(G_CmdNames(i)%str) ) then
!          if ( trim(G_CmdNames(i)%str) == trim(name) ) then
!             call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
!             'The name of a variable should not be the name of a command (<< ' // &
!             trim(name)//' >>)' )
!             return
!          end if   
!       end if   
!    end do
   
   END SUBROUTINE CalmatUtil_CheckName  
   
   
!=============================================================================================
   SUBROUTINE CalmatUtil_GetIndx ( str, ivar, nrhs, mrhs, indi, indj, flagerr  )
!=============================================================================================
   character(len=*),              intent(in    ) :: str
   integer  (Ikind),              intent(in    ) :: ivar, nrhs, mrhs
   integer  (Ikind), allocatable, intent(   out) :: indi(:), indj(:)
   type     (err_t),              intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------     
!  Evaluates the string "str" to find a set of indices corresponding to the variables #ivar
!---------------------------------------------------------------------------------- R.H. 11/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'CalmatUtil_GetIndx'    
   integer  (Ikind)              :: i, n, nv, mv, typ, err
   character(len=:), allocatable :: cindi, cindj
   type     (pk2_t), allocatable :: valexpr(:)
   type     (str_t), allocatable :: SubRec(:)
   logical                       :: is_bal
!---------------------------------------------------------------------------------------------                   
      
   call ieee_set_flag (ieee_all, flag_value = G_QUIET)
!
!- Get the dimensions and the type of the variables:
!  
   nv = G_vars(ivar)%nrow; mv = G_vars(ivar)%ncol ; typ = G_vars(ivar)%typ

   is_bal = util_IsBalanced2 ( str = str, opcl = "[]" , stat=flagerr )
   if_error_trace_and_RETURN ( flagerr, HERE )
   
   if ( .not. is_bal ) then
      call flagerr%set ( stat = G_UERROR, where = HERE, &
                          msg = 'Unbalanced brackets in << '//trim(str)//' >>' )
      return
   end if   
   
   call util_SplitString1(str, ',', '[](){}""'//"''", SubRec, flagerr, n)
   if_error_trace_and_RETURN ( flagerr, HERE )

   cindi = ''; cindj=''
!
!- Is "str" is like foo(cindi) or foo(cindi,cindj)? (cindi, cindj are 2 expressions)
!
   !n = 0
   if ( allocated(SubRec) ) then
      !n = size(SubRec)
      if ( n == 1 ) then
         cindi = SubRec(1)%str 
      else if ( n == 2 ) then
         cindi = SubRec(1)%str ; cindj = SubRec(2)%str
      else    
         call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                            'Invalid index in << '//trim(str)//' >> [msg #1]' )
         return
      end if
   else
      n = 0
   end if

   if ( len_trim(cindi) == 0 .and. len_trim(cindj) == 0 ) then
      call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                         'Invalid index in << '//trim(str)//' >> [msg #2]' )
      return
   end if            
!
!- (n = 1): Case b(cindi) = a   ("indi" will contain the indices in column-major ordering)
!
   if ( n == 1 ) then
!
!-    set size(indj) = 0
!   
      allocate(indj(0))
      
      if ( trim(cindi) == ':' ) then
!
!-       case b(:) = a
!    
         if ( typ == EMPTY ) then
!
!-          b is empty. This case is the same as b = a (set size(indi) = 0)
!         
            allocate(indi(0)); return
         else
!
!-          if b is an existing variable, set indi = 1:size(b)
!         
            allocate(indi(nv*mv), source = [(i,i=1,nv*mv)], stat = err)
            
            if ( err /= 0 ) call flagerr%set ( stat = G_IERROR, where = HERE, msg = &
                                              'Allocation failure for array "indi"' )
            
            return
         end if
      else
!
!-       "cindi" has to be evaluated: call the interpreter:
!      
         call pk2Interpreter_driver ( expr     = trim(cindi)         , &
                                      vars     = G_vars(1:G_nvar)    , &
                                      valexpr  = valexpr             , &
                                      flagerr  = flagerr             )
         if_error_trace_and_RETURN ( flagerr, HERE )
         
         if ( valexpr(1)%nrow /= 1 .and. valexpr(1)%ncol /= 1 ) then
            call flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Syntax error [msg #3]' )
            return
         else
            call valexpr(1)%GetMatPacked ( indi, flagerr )
            if_error_trace_and_RETURN ( flagerr, HERE )
         end if
         
      end if    
      
      return        
   
   end if         
!
!- (n = 2): Case b(cindi,cindj) = a
!
         
   if ( typ == EMPTY .and. trim(cindi) == ':' .and. trim(cindj) == ':' ) then
!
!-    case b(:,:) = a --> not allowed if "b" is empty:
!   
      call flagerr%set ( stat = G_UERROR, where = HERE, msg = &
                         'The submatrix is not correctly defined [msg #4]' )
      return
   end if
!
!- Evaluate "cindi":
!   
   if ( trim(cindi) == ':' ) then
!
!-    case b(:,?)
!   
      if ( typ == EMPTY ) then
!
!-       b is empty, set indi = 1:nrhs
!      
         allocate(indi(nrhs), source = [(i,i=1,nrhs)], stat = err)
      else
!
!-       b exists, set indi = 1:size(b,1)
!      
         allocate(indi(nv), source = [(i,i=1,nv)], stat = err)
      end if
      
      if ( err /= 0 ) then
         call flagerr%set (stat = G_IERROR, where = HERE, &
                            msg = 'Allocation failure for array "indi"' )
         return
      end if   
   else
!
!-    case b(cindi,?), call the interpreter to evaluate "cindi":
!   
      call pk2Interpreter_driver ( expr     = trim(cindi)         , &
                                   vars     = G_vars(1:G_nvar)    , &
                                   valexpr  = valexpr             , &
                                   flagerr  = flagerr             )
      if_error_trace_and_RETURN ( flagerr, HERE )

      if ( valexpr(1)%nrow /= 1 .and. valexpr(1)%ncol /= 1 ) then
         call flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Syntax error [msg #5]' )
         return
      else
         call valexpr(1)%GetMatPacked ( indi, flagerr )
         if_error_trace_and_RETURN ( flagerr, HERE )
      end if
      
   end if
!
!- Evaluate "cindj":
!   
   if ( trim(cindj) == ':' ) then
!
!-    case b(?,:)
!   
      if ( typ == EMPTY ) then
!
!-       b is empty, set indj = 1:mrhs
!      
         allocate(indj(mrhs), source = [(i,i=1,mrhs)], stat = err)
      else
!
!-       b exists, set indj = 1:size(b,2)
!      
         allocate(indj(mv), source = [(i,i=1,mv)], stat = err)
      end if

      if ( err /= 0 ) then
         call flagerr%set ( stat = G_IERROR, where = HERE, &
                             msg = 'Allocation failure for array "indj"' )
         return
      end if   
     
   else
!
!-    case b(?,cindj), call the interpreter to evaluate "cindj":
!
      call pk2Interpreter_driver ( expr     = trim(cindj)         , &
                                   vars     = G_vars(1:G_nvar)    , &
                                   valexpr  = valexpr             , &
                                   flagerr  = flagerr             )
      if_error_trace_and_RETURN ( flagerr, HERE )
      
      if ( valexpr(1)%nrow /= 1 .and. valexpr(1)%ncol /= 1 ) then
         call flagerr%set ( stat = G_UERROR, where = HERE, msg = 'Syntax error [msg #6]' )
         return
      else
         call valexpr(1)%GetMatPacked ( indj, flagerr )
         if_error_trace_and_RETURN ( flagerr, HERE )
      end if
   
   end if
            
   END SUBROUTINE CalmatUtil_GetIndx


!=============================================================================================   
   SUBROUTINE CalmatUtil_ExecuteCommandLine ( expr, flagerr )
!=============================================================================================   
   character(len=*), intent(in    ) :: expr
   type     (err_t), intent(in out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  Calls execute_command_line
!---------------------------------------------------------------------------------------------

!- local variables --------------------------------------------------------------------------- 
   character(len=*    ), parameter :: HERE = 'CalmatUtil_ExecuteCommandLine'  
   character(len=LGSTR)            :: msg
   integer                         :: exitstat, cmdstat   
!---------------------------------------------------------------------------------------------

   msg = ''  ; exitstat = 0    
   call execute_command_line (expr, exitstat = exitstat, cmdstat = cmdstat, cmdmsg = msg)
   
   !if (cmdstat > 0) then
   !   flagerr = err_t ( stat = G_UERROR, &
   !                      msg = 'Command execution failed with the error "'//trim(msg)//'"' )
   !else if (cmdstat < 0) then
   !   flagerr = err_t ( stat = G_UERROR, &
   !                      msg = 'Command line execution not supported' )
   !end if   
   
   !print*,'exitstat, cmdstat, msg:',exitstat, cmdstat,' <<'//trim(msg)//'>>'
   
   if ( cmdstat > 0 ) then
      if ( len_trim(msg) == 0 ) then
         msg = 'Command execution failed '
      else
         msg = 'Command execution failed with the error "'//trim(msg)//'"'
      end if
      call flagerr%set ( stat = G_UERROR, where = HERE, msg = trim(msg) )
   else if ( cmdstat < 0 ) then
      call flagerr%set ( stat = G_UERROR, where = HERE, &
                          msg = 'Command line execution not supported' )
   else if ( exitstat /= 0 ) then
      if ( len_trim(msg) == 0 ) then
         msg = 'Command execution failed '
      else
         msg = 'Command execution failed with the error "'//trim(msg)//'"'
      end if
      call flagerr%set ( stat = G_UERROR, where = HERE, msg = trim(msg) )
   end if

   END SUBROUTINE CalmatUtil_ExecuteCommandLine
   

!=============================================================================================   
   SUBROUTINE CalmatUtil_Pk2Display ( a, msg, symb, style, whole )
!=============================================================================================   
   class    (pk2_t),           intent(in) :: a
   character(len=*), optional, intent(in) :: msg, style
   character(len=*), optional, intent(in) :: symb
   logical         , optional, intent(in) :: whole
!---------------------------------------------------------------------------------------------    
!  Displays the pk2 array "a" on G_uo unit file
!
!  If "whole" is present and .true., display the entire array. Else only a portion will be 
!  displayed defined by the default sizes t * G_maxdisp(1:2) (t = 2 for integer or logical
!  arrays)
!
!  Note: it uses the module dispmodule (included in pk2 library) developped by Kristj JÛnasson
!
!---------------------------------------------------------------------------------- R.H. 09/18

!- local variables ---------------------------------------------------------------------------   
   integer  (Ikind )              :: n, m !, lenmax
   character(len=: ), allocatable :: str, csymb, styl
   !character(len=: ), allocatable :: res(:,:)
   logical                        :: portion, check
   character(len=99)              :: cnum
   !character(len=9 )              :: fmt
!---------------------------------------------------------------------------------------------    
               
   if ( present(msg) ) then
      str = msg
   else
      if ( allocated(a%name) ) then
         str = a%name
      else
         str = ''
      end if   
   end if
   
   if ( present(symb) ) then
      csymb = symb
   else
      csymb = ' = '
   end if      
   
   if ( present(style) ) then
      styl = style
   else
      styl = 'left'
   end if      
         
   write(G_uo,*)
   
   if ( a%typ == EMPTY .or. a%nrow * a%ncol == 0 ) then
      call disp(str // csymb // '[ ]')
      return
   end if
      
   n = a%nrow ; m = a%ncol ; portion = .false.
   
   check = .true.
   if ( present(whole) ) then
      if ( whole ) check = .false.
   end if
         
   if ( check ) then
      if ( a%typ == ITYP .or. a%typ == LTYP ) then
         if ( a%nrow > 2*G_maxdisp(1) ) then
            n = 2*G_maxdisp(1) ; portion = .true.
         end if   
      else
         if ( a%nrow > G_maxdisp(1) ) then
            n = G_maxdisp(1) ; portion = .true.
         end if   
      end if      

      if ( a%typ == ITYP .or. a%typ == LTYP ) then
         if ( a%ncol > 2*G_maxdisp(2) ) then
            m = 2*G_maxdisp(2) ; portion = .true.
         end if   
      else
         if ( a%ncol > G_maxdisp(2) ) then
            m = G_maxdisp(2) ; portion = .true.
         end if   
      end if       
   end if            

   if ( portion ) then 
      write(cnum,'(a,i0,a,i0,a)')'[1:',n,',1:',m,']'
      write(G_uo,'(a,/a,/)') " (large array: only the portion "//trim(cnum)// &
                             "  will be printed. ",  &
                             "  To display the whole array (at your own risk!) use 'disp')"
   end if
            
   select type ( p=>a%m )
      type is ( ik2_t )
         call disp (str//csymb, p%v(1:n,1:m), unit = int(G_uo), style = styl, zeroas = '0')
                     
      type is ( rk2_t )
         call disp (str//csymb, p%v(1:n,1:m), unit = int(G_uo), style = styl, zeroas = '0', &
                    digmax = int(G_digmax))
                     
      type is ( ck2_t )
         call disp (str//csymb, p%v(1:n,1:m), unit = int(G_uo), style = styl, &
                    digmax = int(G_digmax) )
                     
      type is ( lk2_t )
         call disp (str//csymb, p%v(1:n,1:m), unit = int(G_uo), style = styl )
         
      type is ( sk2_t )       

         call str_print ( s = p%v(1:n,1:m), unit = int(G_uo), lhsMsg = str // csymb )

!          call p%GetMatChar(res)
!          if (n * m == 1 .and. len_trim(str) == 0) then
!             str = util_ReplaceSubstring1 ( res(1,1), '\n', char(10) )
!             write(G_uo,'(a)') trim(str)
!          else            
!             fmt = 'a0'                    
!             if (all(len_trim(res(1,:)) == 0) .or. &   ! dispmodule crashes if the elements
!                 all(len_trim(res(:,m)) == 0)   ) then ! of the first row or the last column
!                lenmax = max(1,maxval(len_trim(res)))  ! are all of 0 lenght. Then I call it 
!                write(fmt,'(a,i0)')'a',lenmax          ! with a format 'Aw' with w based on the
!             end if                                    ! max. element lengths.
!             call disp (str//csymb, res(1:n,1:m), unit = int(G_uo), style = styl, fmt = fmt)
!          end if   
   end select
   
   END SUBROUTINE CalmatUtil_Pk2Display


!=============================================================================================   
   SUBROUTINE CalmatUtil_VarDisplay ( a, msg, symb, style, whole )
!=============================================================================================   
   type     (var_t), target  , intent(in) :: a
   character(len=*), optional, intent(in) :: msg, style
   character(len=*), optional, intent(in) :: symb
   logical         , optional, intent(in) :: whole
!---------------------------------------------------------------------------------------------    
!
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------    
   
   call CalmatUtil_Pk2Display ( a, msg, symb, style, whole )
   
   END SUBROUTINE CalmatUtil_VarDisplay


!=============================================================================================   
   SUBROUTINE CalmatUtil_ObjDisplay ( a )
!=============================================================================================   
   type (obj_t), intent(in    ) :: a
!---------------------------------------------------------------------------------------------    
!  Displays the obj "a" on G_uo unit file
!
!---------------------------------------------------------------------------------- R.H. 05/19

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------    
         
   call a%PrintMe()
         
   write(G_uo,*)
         
   END SUBROUTINE CalmatUtil_ObjDisplay


!=============================================================================================
   SUBROUTINE CalmatUtil_defaultSettings ( flagerr )
!=============================================================================================
   type(err_t), intent(out) :: flagerr
!---------------------------------------------------------------------------------------------    
!  See if a default settings file exists in the current directory or in the home
!  directory. Otherwise, create such a file (in the home directory) with default values
!---------------------------------------------------------------------------------------------  

!- local variables ---------------------------------------------------------------------------
   character(len=*    ), parameter   :: HERE = 'CalmatUtil_defaultSettings', &
                                        ColorPrompt = 'b_hb', ColorError  = 'r_b', &
                                        ColorWarn   = 'g_b' , ColorHelp   = 'c_b', &
                                        ColorTitle  = 'c_b' , ColorNormal = 'b'
   character(len=:    ), allocatable :: file
   character(len=MDSTR)              :: str
   integer                           :: ud, lfile, err
   logical                           :: exists
!--------------------------------------------------------------------------------------------- 

   G_filedef = G_FDEF
   
   G_prompt = "(calmat) ~> "

   G_colorHelp   = ansiColor_getAnsiCode ( ColorHelp  )   
   G_colorTitle  = ansiColor_getAnsiCode ( ColorTitle )
      
   G_gnuplot(1) = "gnuplot"     ! gnuplot path
   G_gnuplot(2) = "qt"          ! gnuplot term
   G_gnuplot(3) = "Verdana, 12" ! gnuplot font
   
   G_welcome  = .true.
   G_farewell = "Thank you for using calmat. See you soon. Bye!"

   G_traceback = .false.  
   G_digmax    = 8
   G_maxdisp   = [10,10]
   
   G_prompt   = str_color ( G_prompt  , ColorPrompt )
   
   err_colorError   = ansiColor_getAnsiCode ( ColorError  )
   err_colorWarning = ansiColor_getAnsiCode ( ColorWarn   )
   err_colorNormal  = ansiColor_getAnsiCode ( ColorNormal )
    
   inquire ( file = G_filedef, exist = exists )   

   if ( exists ) then
      return
   else   
      call get_environment_variable ( name = 'HOME', length = lfile )
      allocate(character(len=lfile) :: file)
      call get_environment_variable ( name = 'HOME', value = file )
      G_filedef = trim(adjustl(file)) // '/' // G_filedef
      inquire ( file = G_filedef, exist = exists )
      if ( exists ) return 
   end if
   
   ud = util_GetUnit ()

   if ( ud == 0 ) then
      call flagerr%set ( stat = G_IERROR, where = HERE,  msg = "No free unit available" )
      return
   end if   

   open(unit=ud, file=G_filedef, status='replace', action='write', iostat=err, iomsg=str)
      
   if ( err /= 0 ) then    
      call flagerr%set ( stat = G_IERROR, where = HERE, msg = &
                         'Unable to write into the G_filedef '// file //NLT// &
                         'IOMSG: '//trim(str) )
      return                      
   end if   
           
   write(ud,'(/,a)')"// calmat user's settings"
   
   write(ud,'(/,a,/)')'// prompt symbol: symbol used for the prompt:'
   
   write(ud,'(a)')    '   set(prompt,"' // G_prompt%str // '");'
   
   write(ud,'(/,a,/)')'// user colors: (choose among k, r, g, b, y, m, c, w '// &
                    'and append b for bold and h for high):'
   write(ud,'(a)') '   set(color.prompt , "'// ColorPrompt // '") ;'
   write(ud,'(a)') '   set(color.help   , "'// ColorHelp   // '") ;'
   write(ud,'(a)') '   set(color.error  , "'// ColorError  // '") ;'
   write(ud,'(a)') '   set(color.warning, "'// ColorWarn   // '") ;'
   write(ud,'(a)') '   set(color.title  , "'// ColorTitle  // '") ;'
   write(ud,'(a)') '   set(color.normal  ,"'// ColorNormal // '") ;'
   
   write(ud,'(/,a,/)')'// messages: display welcome message (yes/no) and ' // &
                    'content of the farewell message:'

   write(ud,'(a)') '   set(message.welcome , "no") ; '
   write(ud,'(a)') '   set(message.farewell, "'// G_farewell // '") ;'

   write(ud,'(/,a,/)')'// gnuplot: path of gnuplot (if not in your $PATH), '// &
                    'valid terminal and font:'
   write(ud,'(a)') '   set(gnuplot.path    , "'// G_gnuplot(1)%str // '") ;'
   write(ud,'(a)') '   set(gnuplot.terminal, "'// G_gnuplot(2)%str // '") ;'
   write(ud,'(a)') '   set(gnuplot.font    , "'// G_gnuplot(3)%str // '") ;'

   write(ud,'(/,a,/)')'// max_display: number of digits and portion size ' // &
                    'to be displayed for large arrays:'
   write(ud,'(a)') '   set(display.maxDigits , 8) ;'   
   write(ud,'(a)') '   set(display.portion , [10,10]) ;'   

   write(ud,'(/,a,/)')'// error_traceback: yes/no:'
   write(ud,'(a)') '   set(error.traceback , "no") ;'
              
   close(ud)
            
   END SUBROUTINE CalmatUtil_defaultSettings

      
!=============================================================================================
   SUBROUTINE CalmatUtil_welcome ( printmsg )
!=============================================================================================
   use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
   logical, optional, intent(in) :: printmsg
!---------------------------------------------------------------------------------------------
!  Prints a banner and a welcome message
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=:), allocatable :: m, mc, m1, m2, m3, s0, b, r, w, g
   character(len=9)              :: sz
   integer  (Ikind)              :: lenmax, lm, ls0, dl, p, q, n, i, printme
   integer                       :: uo
!---------------------------------------------------------------------------------------------

   printme = IZERO
   if ( present(printmsg) ) then
      if ( printmsg ) then 
         printme = IONE
      else
         printme =-IONE
      end if
   end if
   
   if ( G_task /= G_CONTINUE .or. printme == -IONE ) return
   
   !if ( G_uo /= STDOUT .or. trim(adjustl(G_default(2)%str)) == 'no' ) return
   !if ( G_uo /= STDOUT .or. .not. G_welcome ) return
   
   uo = int(G_uo)
   
   r = ansiColor_getAnsiCode ( 'r_bh' )
   b = ansiColor_getAnsiCode ( 'b_bh' )
   w = ansiColor_getAnsiCode ( 'w_bh' )
   g = ansiColor_getAnsiCode ( 'g_bh' )
  
   write(uo,'(/)')
   m = '<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>'
   !m = ansiColor_colorTxt (unit=uo, ansiColorCode=b, str= m)
   call util_ScrollingMessage (m, .01, uo)      
   m = '<>                                                                            <>'
   !m = ansiColor_colorTxt (unit=uo, ansiColorCode=r, str= m)
   call util_ScrollingMessage (m, .01, uo)  
   m = '<>      ____        __       __        _        _        __      .........    <>'
   !m = ansiColor_colorTxt (unit=uo, ansiColorCode=b, str= m)
   call util_ScrollingMessage (m, .01, uo)    
   m = '<>    **           //\\      ++        ||\    /||       //\\    ||   ^   ||   <>'
   !m = ansiColor_colorTxt (unit=uo, ansiColorCode=r, str= m)
   call util_ScrollingMessage (m, .01, uo)   
   m = '<>   **           //  \\     ++        || \  / ||      //  \\        &        <>'
   !ansiColor_colorTxt (unit=uo, ansiColorCode=b, str= m)
   call util_ScrollingMessage (m, .01, uo)   
   m = '<>   **          //====\\    ++        ||  \/  ||     //====\\       ^        <>'
   !m = ansiColor_colorTxt (unit=uo, ansiColorCode=r, str= m)
   call util_ScrollingMessage (m, .01, uo)   
   m = '<>    **____   _//      \\_  =======| _||      ||_  _//      \\_    _&_       <>'
   !m = ansiColor_colorTxt (unit=uo, ansiColorCode=b, str= m)
   call util_ScrollingMessage (m, .01, uo)  
   m = '<>                                                                            <>'
   !m = ansiColor_colorTxt (unit=uo, ansiColorCode=r, str= m)
   call util_ScrollingMessage (m, .01, uo)  
   m = '<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>'
   !m = ansiColor_colorTxt (unit=uo, ansiColorCode=r, str= m)    
   call util_ScrollingMessage (m, .01, uo)          

   write(uo,*)
   lenmax = len_trim(m)

   m1 = ansiColor_colorTxt (unit=uo, ansiColorCode=b, str='=')
   m2 = ansiColor_colorTxt (unit=uo, ansiColorCode=w, str='=')
   m3 = ansiColor_colorTxt (unit=uo, ansiColorCode=r, str='=')
   n = mod(lenmax,3_Ikind) 
   mc = repeat(m1//m2//m3,lenmax/3_Ikind) 
   if ( n > 0 ) then
      mc = mc // m1
      if ( n > 1 ) mc = mc // m2
   end if   
   call util_ScrollingMessage (mc, .01, uo)   
   
   m = '     W e l c o m e  t o  t h e  C o m m a n d  L i n e  C a l c u l a t o r    '
   call util_ScrollingMessage (m, .02, uo)
   m = ansiColor_colorTxt (unit=uo, ansiColorCode=b, str='C A L M A T')
   m = '                                << ' // m // ' >>                   '
   call util_ScrollingMessage (m, .02, uo)
   m = ' -- a  F o r t r a n  P a r s e r  f o r  M a t r i x  O p e r a t i o n s --  '
   call util_ScrollingMessage (m, .02, uo)

   m1 = ansiColor_colorTxt (unit=uo, ansiColorCode=g, str='=')
   m2 = ansiColor_colorTxt (unit=uo, ansiColorCode=r, str='=')
   m3 = ansiColor_colorTxt (unit=uo, ansiColorCode=w, str='=')   
   n = mod(lenmax,3_Ikind) 
   mc = repeat(m1//m2//m3,lenmax/3_Ikind) 
   if ( n > 0 ) then
      mc = mc // m1
      if ( n > 1 ) mc = mc // m2
   end if   
   call util_ScrollingMessage (mc, .01, uo)   
   
   m = "              (R. Hassani, Univ. Côte d'Azur - version: "//G_numversion//")    "
   call util_ScrollingMessage (m, .0, uo)

   write(uo,'(/)')
   m = 'Version info:'
   call util_ScrollingMessage (m, .0, uo)

   if ( len_trim(G_compilDate) /= 0 ) then
      m  = ' . compilation date: '//trim(G_compilDate)
      call util_ScrollingMessage (m, .0, uo)   
   end if   
   
   s0 = ' . compiler version: ' ; ls0 = len(s0)   
   m = trim(adjustl(compiler_version())) ; lm  = len(m ) 
   dl = lenmax - ls0   
   if ( lm <= dl ) then
      call util_ScrollingMessage (s0 // m, .0, uo)   
   else   
      dl = lenmax - ls0
      n = lm / dl
      p = 1 ; q = dl
      call util_ScrollingMessage (s0 // m(p:q), .0, uo)
      s0 = repeat(' ',ls0)     
      do i = 2, n
         p = q + 1 ; q = p + dl - 1
         call util_ScrollingMessage (s0 // m(p:q), .0, uo)
      end do      
      if ( q < lm ) call util_ScrollingMessage (s0 // m(q+1:lm), .0, uo)
   end if
   
   s0 = ' . compiler options: ' ; ls0 = len(s0)   
   if ( len_trim(G_compilOPts) /= 0 ) then
      m = trim(G_compilOPts) ; lm  = len(m )       
   else
      m = trim(adjustl(compiler_options())) ; lm  = len(m ) 
   end if   
   dl = lenmax - ls0
   if ( lm <= dl ) then
      call util_ScrollingMessage (s0 // m, .0, uo)   
   else   
      n = lm / dl
      p = 1 ; q = dl
      call util_ScrollingMessage (s0 // m(p:q), .0, uo)
      s0 = repeat(' ',ls0)     
      do i = 2, n
         p = q + 1 ; q = p + dl - 1
         call util_ScrollingMessage (s0 // m(p:q), .0, uo)
      end do      
      if ( q < lm ) call util_ScrollingMessage (s0 // m(q+1:lm), .0, uo)
   end if

   write(sz,'(i0)')Ikind   
   m  = ' . storage size: '// trim(sz) // '-byte integers and '   
   write(sz,'(i0)')Rkind
   m = m // trim(sz) // '-byte reals and complexes'
   call util_ScrollingMessage (m, .0, uo)

   write(uo,'(/)')
   m = 'Type: '
   call util_ScrollingMessage (m, .0, uo)
   m = ' . '  // ansiColor_colorTxt (unit=uo, ansiColorCode=G_colorHelp, str='quit') // &
       ' or ' // ansiColor_colorTxt (unit=uo, ansiColorCode=G_colorHelp, str='exit') // &
       ' ............ to exit the program'
   call util_ScrollingMessage (m, .0, uo)
   m = ' . '  // ansiColor_colorTxt (unit=uo, ansiColorCode=G_colorHelp, str='help') // &
       ' .................... to get general help or help about a given command '
   call util_ScrollingMessage (m, .0, uo)
   m = ' . '  // ansiColor_colorTxt (unit=uo, ansiColorCode=G_colorHelp, str='list') // &
       ' vars ............... to display the list of your variables '
   call util_ScrollingMessage (m, .0, uo)
   m = ' . '  // ansiColor_colorTxt (unit=uo, ansiColorCode=G_colorHelp, str='list') // &   
       ' op ................. to display the list of available operators '
   call util_ScrollingMessage (m, .0, uo)
   m = ' . '  // ansiColor_colorTxt (unit=uo, ansiColorCode=G_colorHelp, str='list') // &   
       ' fun ................ to display the list of available functions '
   call util_ScrollingMessage (m, .0, uo)
   m = ' . '  // ansiColor_colorTxt (unit=uo, ansiColorCode=G_colorHelp, str='list') // &   
       ' cmd ................ to display the list of available commands '
   call util_ScrollingMessage (m, .0, uo)
   
   write(uo,'(/)')
   
   if ( printme == IZERO ) return

   m ='  (to no longer see this message set "message.welcome" to "no" in the file: '// NL // &
      '   "'// G_filedef //'")'
   call util_ScrollingMessage (m, .0, uo)

   write(uo,'(/)')

   END SUBROUTINE CalmatUtil_welcome


!=============================================================================================
   SUBROUTINE CalmatUtil_printFlows (flowId)
!=============================================================================================
   integer(Ikind), optional, intent(in) :: flowId
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------

   if ( present(flowId) ) then
      i = flowId
      write(G_uo,*)'Debug: flow #',flowId,' and its stack'
      if ( i > 0 .and. i <= G_maxnFlow ) then
         if ( G_flows(i)%used ) then
            write(G_uo,'(a,i0,a)')'The flow #',i,' contains the instructions:' 
            do j = 1, G_flows(i)%nStack
               if ( allocated(G_flows(i)%stack(j)%rec) ) then
                  write(G_uo,'(3x,i0,".",1x,a)')j,G_flows(i)%stack(j)%rec
               else
                  write(G_uo,'(3x,i0,"-",1x,a)')j,'(humm, a non-allocated record)'
               endif
            enddo
         else
            write(G_uo,'(a,i0,a)')'The flow #',i,' is not used' 
         end if
      else
         write(G_uo,'(a)')'Not a valid # of flow'
      end if
   
   else
      write(G_uo,*)'Debug: all flows and their stack'
   
      do i = 1, G_maxnFlow
         if ( G_flows(i)%used ) then
            if ( G_flows(i)%nStack /= 0 ) then
               write(G_uo,'(a,i0,a)')'The flow #',i,' contains the instructions:' 
               do j = 1, G_flows(i)%nStack
                  if ( allocated(G_flows(i)%stack(j)%rec) ) then
                     write(G_uo,'(3x,i0,".",1x,a,", bkpt: ",i0,", flow: ",i0, ", blk: ",i0)') &
                                                             j,                            &
                                                             G_flows(i)%stack(j)%rec,      &
                                                             G_flows(i)%stack(j)%breakpt,  &
                                                             G_flows(i)%stack(j)%flowId,   &
                                                             G_flows(i)%stack(j)%blk
                  else
                     write(G_uo,'(3x,i0,"-",1x,a)')j,'(hum... a non-allocated record)'
                  endif
               enddo
            else
               write(G_uo,'(a,i0,a)')'The flow #',i,' is empty' 
            endif
         end if
      end do
   end if

   END SUBROUTINE CalmatUtil_printFlows


END MODULE CalmatUtil_m