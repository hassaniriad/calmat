!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Module CalmatSetNGet
!
! This module contains interfaces that are only really useful for users programs to set or
! to get the container of a given variable of the G_vars list
!
! Author: R. Hassani, Universite Cote d'Azur
!
! Date: 05/24
!---------------------------------------------------------------------------------------------

#include "error.fpp"


MODULE CalmatSetNGet_m

   use CalmatGlobal_m
   use CalmatUtil_m, only: calmatUtil_findVar, calmatUtil_AddVar, i2a=>util_intToChar
   
   implicit none
   
   private
   public :: ind_t, Calmat_Inquire, Calmat_AddVar, Calmat_copy, Calmat_getPointer, &
             Calmat_movealloc, Calmat_display
!
!- A DT to store the index (in the G_vars list) of a variable, its shape and its type
!
   type :: ind_t
      integer  (Ikind) :: Id = IZERO, size(2) = [IZERO,IZERO]
      character(len=1) :: type = '-'
   end type ind_t
!
!- Copy from an intrinsic (of rank 0, 1, or 2) to a variable container and vice-versa:
!   
   interface Calmat_copy
      ! from an intrinsic to a variable:
      ! (e.g. call Calmat_copy ( from = 1.5, to = 's', stat = err )
      module procedure                                                        &
                ! var identified by its name,    var identified by its indexe
                  CalmatSetNGet_cpXToVar_byName, CalmatSetNGet_cpXToVar_byId
      ! from a variable to an intrinsic:
      ! (e.g. call Calmat_copy ( from = 'M', to = Mat, stat = err )
      module procedure                                                        &
                ! var identified by its name,    var identified by its indexe
                  CalmatSetNGet_cpVarToI_byName, CalmatSetNGet_cpVarToI_byId, &
                  CalmatSetNGet_cpVarToR_byName, CalmatSetNGet_cpVarToR_byId, &
                  CalmatSetNGet_cpVarToC_byName, CalmatSetNGet_cpVarToC_byId, &
                  CalmatSetNGet_cpVarToL_byName, CalmatSetNGet_cpVarToL_byId, &
                  CalmatSetNGet_cpVarToS_byName, CalmatSetNGet_cpVarToS_byId
                                   
   end interface
!
!- Associate a pointer of intrinsic type with a variable container:
!
   interface Calmat_getPointer
      ! (e.g. call Calmat_getPointer ( var = 'M', ptr = pMat, stat = err )
      module procedure                                                    &
                ! var identified by its name,  var identified by its indexe
                  CalmatSetNGet_varNameToIPtr, CalmatSetNGet_varIdToIPtr, &
                  CalmatSetNGet_varNameToRPtr, CalmatSetNGet_varIdToRPtr, &
                  CalmatSetNGet_varNameToCPtr, CalmatSetNGet_varIdToCPtr, &
                  CalmatSetNGet_varNameToLPtr, CalmatSetNGet_varIdToLPtr, &
                  CalmatSetNGet_varNameToSPtr, CalmatSetNGet_varIdToSPtr
   end interface
!
!- Move allocation from a variable container to an intrinsic and vice-versa:
!
   interface Calmat_movealloc
      ! from a variable to an intrinsic:
      ! (e.g. call calmat_movealloc ( from = 'M', to = Mat, stat = err )
      module procedure                                                  &
                ! var identified by its name,    var identified by its indexe
                  CalmatSetNGet_mvVarToI_byName, CalmatSetNGet_mvVarToI_byId, &
                  CalmatSetNGet_mvVarToR_byName, CalmatSetNGet_mvVarToR_byId, &
                  CalmatSetNGet_mvVarToC_byName, CalmatSetNGet_mvVarToC_byId, &
                  CalmatSetNGet_mvVarToL_byName, CalmatSetNGet_mvVarToL_byId, &
                  CalmatSetNGet_mvVarToS_byName, CalmatSetNGet_mvVarToS_byId
      ! from an intrinsic to variable container
      ! (e.g. call calmat_movealloc ( from = Mat, to = 'M', stat = err )
      module procedure                                                  &
                ! var identified by its name,    var identified by its indexe
                  CalmatSetNGet_mvIToVar_byName, CalmatSetNGet_mvIToVar_byId, &
                  CalmatSetNGet_mvRToVar_byName, CalmatSetNGet_mvRToVar_byId, &
                  CalmatSetNGet_mvCToVar_byName, CalmatSetNGet_mvCToVar_byId, &
                  CalmatSetNGet_mvLToVar_byName, CalmatSetNGet_mvLToVar_byId, &
                  CalmatSetNGet_mvSToVar_byName, CalmatSetNGet_mvSToVar_byId

   end interface
!
!- Printing a variable:
!
   interface Calmat_display
      module procedure &
                ! var identified by its name,     var identified by its indexe
                  CalmatSetNGet_displayByVarname, CalmatSetNGet_displayByVarId
   end interface
   
   character(len=1) :: varTypeShort(0:5) = ['e','i','r','c','l','s']
   character(len=7) :: varTypeLong (0:5) = ['empty  ','integer','real   ',&
                                            'complex','logical','string ']
   
CONTAINS

!=============================================================================================   
   SUBROUTINE Calmat_Inquire ( varName, varId )
!=============================================================================================   
   character(len=*), intent(in    ) :: varName
   type     (ind_t), intent(   out) :: varId
!---------------------------------------------------------------------------------------------
!  Checks if a variable of name "varName" is present in the list G_vars. Returns its indexe 
!  in varId%Id (= 0 if not present), its type in varId%type) and its size varId%size.
!---------------------------------------------------------------------------------------------

   call CalmatUtil_FindVar ( varName = varName, varId = varId%Id )
   
   if ( varId%Id /= 0 ) then
      varId%size = [G_vars(varId%Id)%nrow, G_vars(varId%Id)%ncol]
      varId%type = varTypeShort(G_vars(varId%Id)%typ)
   end if
   
   END SUBROUTINE Calmat_Inquire
   
!=============================================================================================   
   SUBROUTINE Calmat_AddVar ( ofName, ofType, stat, var )
!=============================================================================================   
   character(len=*),           intent(in    ) :: ofName, ofType
   type     (err_t), optional, intent(in out) :: stat   
   type     (ind_t), optional, intent(   out) :: var
!---------------------------------------------------------------------------------------------
!  Adds in the list of variables G_vars a variable of name "ofName" and a type of "ofName".
!  Optionally returns its indexe "var".
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'Calmat_AddVar'
   integer  (Ikind), parameter :: shap0(2) = [IZERO,IZERO], shap1(2) = [IONE,IONE]
   logical                     :: is_new
   integer  (Ikind)            :: ivar
   character(len=1)            :: type
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )
   
   call CalmatUtil_AddVar ( ofName, G_TEMPORARY, ivar, is_new, G_flagerr )
   
   if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if
   
   select case ( util_StringLow(ofType) )
      case ( 'e', 'empty' )
         G_vars(ivar) = pk2_t( typ = EMPTY, name = ofName, shape = shap0, stat = G_flagerr )
         type = 'e'
      case ( 'i','int','integer' ) 
         G_vars(ivar) = pk2_t( typ = ITYP , name = ofName, shape = shap1, stat = G_flagerr )
         type = 'i'
      case ( 'r','real','float' ) 
         G_vars(ivar) = pk2_t( typ = RTYP , name = ofName, shape = shap1, stat = G_flagerr )
         type = 'r'
      case ( 'c','cmplx','complex' ) 
         G_vars(ivar) = pk2_t( typ = CTYP , name = ofName, shape = shap1, stat = G_flagerr )
         type = 'c'
      case ( 'l','logical','boolean' ) 
         G_vars(ivar) = pk2_t( typ = LTYP , name = ofName, shape = shap1, stat = G_flagerr )
         type = 'l'
      case ( 'string','char' ) 
         G_vars(ivar) = pk2_t( typ = STYP , name = ofName, shape = shap1, stat = G_flagerr )
         type = 's'
      case default
         call G_flagerr%set ( msg = 'Invalid type "'//trim(oftype)//'"',  &
                              where = HERE, stat = G_UERROR ) 
         if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end select
   
   if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if
   
   if ( present(var) ) then
      var%Id   = ivar
      var%size = [G_vars(ivar)%nrow, G_vars(ivar)%ncol]
      var%type = type
   end if 

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE Calmat_AddVar

   
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_displayByVarId ( var, unit, msg, dispstyle, form, stat )
!=============================================================================================
   type     (ind_t),           intent(in    ) :: var
   integer  (Ikind), optional, intent(in    ) :: unit
   character(len=*), optional, intent(in    ) :: msg, dispstyle, form
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Prints the container of a variable given by its indexe ("var")
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_displayByVarId'
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   if ( var%Id > 0 .and. var%Id <= size(G_vars) ) then
      call G_vars(var%Id) % printMe ( unit, dispstyle, msg, form )
   else
      call G_flagerr%set ( msg = "Invalid variable Id", where = HERE, stat = G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end if
   
   call err_resetHaltingMode ( reset = .true. )

   END SUBROUTINE CalmatSetNGet_displayByVarId


!=============================================================================================   
   SUBROUTINE CalmatSetNGet_displayByVarname ( var, unit, msg, dispstyle, form, stat )
!=============================================================================================
   character(len=*),           intent(in    ) :: var
   integer  (Ikind), optional, intent(in    ) :: unit
   character(len=*), optional, intent(in    ) :: msg, dispstyle, form
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Prints the container of a variable given by its name ("var")
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_displayByVarname'
   type     (ind_t)            :: v
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = var, varId = v%Id )
   if ( v%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//var//'"', where=HERE, stat=G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_displayByVarId ( v, unit, msg, dispstyle, form, stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if
   
   call err_resetHaltingMode ( reset = .true. )
      
   END SUBROUTINE CalmatSetNGet_displayByVarname
          
   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Setter: copying from an intrinsic to a variable container +++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!- 1) The index (to%Id) of the variable in G_vars is known:
!   
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpXTovar_byId ( from, to, stat )
!=============================================================================================   
   class( *  ), optional, intent(in    ) :: from(..) 
   type(ind_t),           intent(in out) :: to
   type(err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copies a rank 0, 1 or 2 to a variable (of G_vars list) known by its index
!---------------------------------------------------------------------------------------------   

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpXTovar_byId'
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   if ( .not. util_isAllocated(from) ) then
      call G_flagerr%set ( stat = G_UERROR , where = HERE, msg = &
            "Unable to set the calmat variable #"//i2a(to%Id)//" from an unallocated object" ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end if

   if ( to%Id > 0 .and. to%Id <= size(G_vars) ) then

      if ( G_vars(to%Id)%status == G_PROTECTED .or. &
           G_vars(to%Id)%status == G_USERPROTECTED   ) then
         call G_flagerr%set (stat=G_UERROR, where=HERE, msg='The variable #'//i2a(to%Id)//  &
                     ' ('//G_vars(to%Id)%name//') is protected and thus can not be changed' )
         if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end if 
            
      select rank ( from )
         rank ( 0 )
            select type ( from )
               type is ( integer(i32) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( integer(i64) )               
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( real(rSp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( real(rDp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( complex(rSp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( complex(rDp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( logical )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( str_t )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               class default
                  call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='Unexpected type' )
                  if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; endif
           end select
         rank ( 1 )
            select type ( from )
               type is ( integer(i32) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( integer(i64) )               
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( real(rSp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( real(rDp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( complex(rSp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( complex(rDp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( logical )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( str_t )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               class default
                  call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='Unexpected type' )
                  if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; endif
            end select

         rank ( 2 )
            select type ( from )
               type is ( integer(i32) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( integer(i64) )               
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( real(rSp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( real(rDp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( complex(rSp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( complex(rDp) )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( logical )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               type is ( str_t )
                  call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               class default
                  call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='Unexpected type' )
                  if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; endif
            end select
            
         rank default
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                  msg = 'Unexpected rank of '//util_intToChar(rank(from)) )
            if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end select

      if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if
      
      call G_vars(to%Id)%resetDesc()            
      to%size = [ G_vars(to%Id)%nrow, G_vars(to%Id)%ncol ]
      to%type = varTypeShort(G_vars(to%Id)%typ)
      
   else
      call G_flagerr%set ( msg = "Invalid variable Id", where = HERE, stat = G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end if
      
   call err_resetHaltingMode ( reset = .true. )   
   
   END SUBROUTINE CalmatSetNGet_cpXTovar_byId   
   
      
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpXTovar_byName ( from, to, stat, varId )
!=============================================================================================   
   class    (  *  ), optional, intent(in    ) :: from(..) 
   character(len=*),           intent(in    ) :: to
   type     (err_t), optional, intent(in out) :: stat
   type     (ind_t), optional, intent(   out) :: varId
!---------------------------------------------------------------------------------------------
!  Copies an integer of rank 0, 1 or 2 to a variable (of G_vars list) known by its name.
!  If this variable doesn't exist, creates it and optionally returns its Id
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpXTovar_byName'
   type     (ind_t)            :: var
   logical                     :: is_new
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   if ( .not. util_isAllocated(from) ) then
      call G_flagerr%set ( stat = G_UERROR , where = HERE, msg = &
             "Unable to set the calmat variable '"//trim(to)//"' from an unallocated object" ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end if
   
   call CalmatUtil_AddVar ( to, G_TEMPORARY, var%Id, is_new, G_flagerr )
   if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

   call CalmatSetNGet_cpXTovar_byId ( from, var, stat )
   if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if

   if ( present(varId) ) varId = var

   call err_resetHaltingMode ( reset = .true. )   

   END SUBROUTINE CalmatSetNGet_cpXTovar_byName


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Setter: move allocation from an intrinsic to a variable container +++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!- 1) The index (to%Id) of the variable (in G_vars) is known:
!
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvITovar_byId ( from, to, stat )
!=============================================================================================   
   integer(Ikind), allocatable, intent(in out) :: from(..)
   type   (ind_t),              intent(in out) :: to
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Move allocation from an integer to variable (of G_vars list) known by its index
!  Note: actually, move allocation only applies for "from" of rank 2. For rank < 2 a copy 
!        is made and "from" is deallocated
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvITovar_byId'
!---------------------------------------------------------------------------------------------

#include "include/calmatsetnget_mv_x_to_var.inc"

   END SUBROUTINE CalmatSetNGet_mvITovar_byId   

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvRTovar_byId ( from, to, stat )
!=============================================================================================   
   real(Rkind), allocatable, intent(in out) :: from(..)
   type(ind_t),              intent(in out) :: to
   type(err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
! (see CalmatSetNGet_mvITovar_byId)
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvRTovar_byId'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_mv_x_to_var.inc"

   END SUBROUTINE CalmatSetNGet_mvRTovar_byId 

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvCTovar_byId ( from, to, stat )
!=============================================================================================   
   complex(Rkind), allocatable, intent(in out) :: from(..)
   type   (ind_t),              intent(in out) :: to
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
! (see CalmatSetNGet_mvITovar_byId)
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvCTovar_byId'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_mv_x_to_var.inc"

   END SUBROUTINE CalmatSetNGet_mvCTovar_byId    

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvLTovar_byId ( from, to, stat )
!=============================================================================================   
   logical       , allocatable, intent(in out) :: from(..)
   type   (ind_t),              intent(in out) :: to
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
! (see CalmatSetNGet_mvITovar_byId)
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvLTovar_byId'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_mv_x_to_var.inc"

   END SUBROUTINE CalmatSetNGet_mvLTovar_byId   

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvSTovar_byId ( from, to, stat )
!=============================================================================================   
   type(str_t), allocatable, intent(in out) :: from(..)
   type(ind_t),              intent(in out) :: to
   type(err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
! (see CalmatSetNGet_mvITovar_byId)
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvSTovar_byId'
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )
   
   if ( to%Id > 0 .and. to%Id <= size(G_vars) ) then
   
      if ( G_vars(to%Id)%status == G_PROTECTED .or. &
           G_vars(to%Id)%status == G_USERPROTECTED   ) then
         call G_flagerr%set ( stat=G_UERROR, where=HERE, msg='The variable #'//i2a(to%Id)//  &
                     ' ('//G_vars(to%Id)%name//') is protected and thus can not be changed' )
         if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end if 
      
      select rank ( from )
         rank ( 0 )
            if ( allocated(from) ) then
               call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               if ( G_flagerr <= IZERO ) call deallocateS0 ( from )
            else
               call G_vars(to%Id)%pk2_t%Destroy()
            end if
         rank ( 1 )
            if ( allocated(from) ) then
               call pk2_assign ( lhs=G_vars(to%Id), rhs=from, stat=G_flagerr )
               if ( G_flagerr <= IZERO ) deallocate ( from )
            else
               call G_vars(to%Id)%pk2_t%Destroy()
            end if            
         rank ( 2 )
            call pk2_movealloc ( to=G_vars(to%Id), from=from, stat=G_flagerr )
         rank default
            call G_flagerr%set ( stat = G_UERROR, where = HERE, &
                                 msg = 'Unexpected rank of '//util_intToChar(rank(from)) )
            if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
      end select

      if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

      to%size = [G_vars(to%Id)%nrow,G_vars(to%Id)%ncol]
      to%type = varTypeShort(G_vars(to%Id)%typ)
   else
      call G_flagerr%set ( msg = "Invalid variable Id", where = HERE, stat = G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
   
   contains
   
      subroutine deallocateS0 ( s )  ! workaround for a nagfor bug
      type(str_t), allocatable, intent(in out) :: s
      deallocate(s)
      end subroutine deallocateS0
   
   END SUBROUTINE CalmatSetNGet_mvSTovar_byId 
!
!- 2) The name of the variable (in G_vars) is known:
!
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvITovar_byName ( from, to, stat, varId )
!=============================================================================================   
   character(len=*),              intent(in    ) :: to
   integer  (Ikind), allocatable, intent(in out) :: from(..)
   type     (err_t), optional,    intent(in out) :: stat
   type     (ind_t), optional,    intent(in out) :: varId
!---------------------------------------------------------------------------------------------
!  Move allocation from an integer to variable (of G_vars list) known by its name.
!  If this variable doesn't exist, creates it and optionally returns its Id
!  Note: actually, move allocation only applies for "from" of rank 2. For rank < 2 a copy 
!        is made and "from" is deallocated
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvITovar_byName'
   type     (ind_t)            :: var
   logical                     :: is_new
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call CalmatUtil_AddVar ( to, G_TEMPORARY, var%Id, is_new, G_flagerr )
   if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

   call CalmatSetNGet_mvITovar_byId ( from, var, stat )
   if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if

   if ( present(varId) ) varId = var

   call err_resetHaltingMode ( reset = .true. )
      
   END SUBROUTINE CalmatSetNGet_mvITovar_byName

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvRTovar_byName ( from, to, stat, varId )
!=============================================================================================   
   character(len=*),              intent(in    ) :: to
   real     (Rkind), allocatable, intent(in out) :: from(..)
   type     (err_t), optional,    intent(in out) :: stat
   type     (ind_t), optional,    intent(in out) :: varId
!---------------------------------------------------------------------------------------------
! (see CalmatSetNGet_mvITovar_byName)
!--------------------------------------------------------------------------------------------- 
  
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvRTovar_byName'
   type     (ind_t)            :: var
   logical                     :: is_new
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call CalmatUtil_AddVar ( to, G_TEMPORARY, var%Id, is_new, G_flagerr )
   if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

   call CalmatSetNGet_mvRTovar_byId ( from, var, stat )
   if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if

   if ( present(varId) ) varId = var

   call err_resetHaltingMode ( reset = .true. )
      
   END SUBROUTINE CalmatSetNGet_mvRTovar_byName
     
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvCTovar_byName ( from, to, stat, varId )
!=============================================================================================   
   character(len=*),              intent(in    ) :: to
   complex  (Rkind), allocatable, intent(in out) :: from(..)
   type     (err_t), optional,    intent(in out) :: stat
   type     (ind_t), optional,    intent(in out) :: varId
!---------------------------------------------------------------------------------------------
! (see CalmatSetNGet_mvITovar_byName)
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvCTovar_byName'
   type     (ind_t)            :: var
   logical                     :: is_new
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call CalmatUtil_AddVar ( to, G_TEMPORARY, var%Id, is_new, G_flagerr )
   if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

   call CalmatSetNGet_mvCTovar_byId ( from, var, stat )
   if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if

   if ( present(varId) ) varId = var

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_mvCTovar_byName
   
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvLTovar_byName ( from, to, stat, varId )
!=============================================================================================   
   character(len=*),              intent(in    ) :: to
   logical         , allocatable, intent(in out) :: from(..)
   type     (err_t), optional,    intent(in out) :: stat
   type     (ind_t), optional,    intent(in out) :: varId
!---------------------------------------------------------------------------------------------
! (see CalmatSetNGet_mvITovar_byName)
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvLTovar_byName'
   type     (ind_t)            :: var
   logical                     :: is_new
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call CalmatUtil_AddVar ( to, G_TEMPORARY, var%Id, is_new, G_flagerr )
   if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

   call CalmatSetNGet_mvLTovar_byId ( from, var, stat )
   if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if

   if ( present(varId) ) varId = var

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_mvLTovar_byName
       
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvSTovar_byName ( from, to, stat, varId )
!=============================================================================================   
   character(len=*),              intent(in    ) :: to
   type     (str_t), allocatable, intent(in out) :: from(..)
   type     (err_t), optional,    intent(in out) :: stat
   type     (ind_t), optional,    intent(in out) :: varId
!---------------------------------------------------------------------------------------------
! (see CalmatSetNGet_mvITovar_byName)
!--------------------------------------------------------------------------------------------- 

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvSTovar_byName'
   type     (ind_t)            :: var
   logical                     :: is_new
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call CalmatUtil_AddVar ( to, G_TEMPORARY, var%Id, is_new, G_flagerr )
   if ( present(stat) ) then ; setNtraceNreturn(G_flagerr,HERE,stat) ; end if

   call CalmatSetNGet_mvSTovar_byId ( from, var, stat )
   if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if

   if ( present(varId) ) varId = var

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_mvSTovar_byName     


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Getter: copy a variable container into an intrinsic +++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!- 1) The index (from%Id) of the variable (in G_vars) is known:
!
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToI_byId ( from, to, stat )
!=============================================================================================   
   type   (ind_t),              intent(in    ) :: from
   integer(Ikind), allocatable, intent(in out) :: to(..)
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its index to a real of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatSetNGet_cpVarToI_byId'
!--------------------------------------------------------------------------------------------- 
   
#include "include/calmatsetnget_cp_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_cpVarToI_byId
   
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToR_byId ( from, to, stat )
!=============================================================================================   
   type(ind_t),              intent(in    ) :: from
   real(Rkind), allocatable, intent(in out) :: to(..)
   type(err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its index to a real of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatSetNGet_cpVarToR_byId'
!--------------------------------------------------------------------------------------------- 
   
#include "include/calmatsetnget_cp_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_cpVarToR_byId

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToC_byId ( from, to, stat )
!=============================================================================================   
   type   (ind_t),              intent(in    ) :: from
   complex(Rkind), allocatable, intent(in out) :: to(..)
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its index to a complex of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatSetNGet_cpVarToC_byId'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_cp_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_cpVarToC_byId

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToL_byId ( from, to, stat )
!=============================================================================================   
   type   (ind_t),              intent(in    ) :: from
   logical       , allocatable, intent(in out) :: to(..)
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its index to a logical of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpVarToL_byId'
!---------------------------------------------------------------------------------------------

#include "include/calmatsetnget_cp_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_cpVarToL_byId

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToS_byId ( from, to, stat )
!=============================================================================================   
   type(ind_t),              intent(in    ) :: from
   type(str_t), allocatable, intent(in out) :: to(..)
   type(err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its index to a str_t of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpVarToS_byId'
!---------------------------------------------------------------------------------------------

   select rank(to)
      rank(0)
      call allocateS ( to ) ! workaround for a nagfor bug
   end select
   
#include "include/calmatsetnget_cp_var_to_x.inc"

   contains
      subroutine allocateS ( s )
      type(str_t), allocatable, intent(in out) :: s
      if ( .not. allocated(s) ) allocate(s)
      end subroutine allocateS

   END SUBROUTINE CalmatSetNGet_cpVarToS_byId   

!
!- 2) The name of the variable (in G_vars) is known:
!      
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToI_byName ( from, to, stat )
!=============================================================================================
   character(len=*),              intent(in    ) :: from
   integer  (Ikind), allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its name to an integer of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpVarToI_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_cpVarToI_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_cpVarToI_byName

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToR_byName ( from, to, stat )
!=============================================================================================
   character(len=*),              intent(in    ) :: from
   real     (Rkind), allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its name to a real of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpVarToR_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_cpVarToR_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )   
   
   END SUBROUTINE CalmatSetNGet_cpVarToR_byName

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToC_byName ( from, to, stat )
!=============================================================================================
   character(len=*),              intent(in    ) :: from
   complex  (Rkind), allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its name to a complex of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpVarToC_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_cpVarToC_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_cpVarToC_byName

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToL_byName ( from, to, stat )
!=============================================================================================
   character(len=*),              intent(in    ) :: from
   logical         , allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its name to a logical of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpVarToL_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_cpVarToL_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )   
   
   END SUBROUTINE CalmatSetNGet_cpVarToL_byName
   
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_cpVarToS_byName ( from, to, stat )
!=============================================================================================
   character(len=*),              intent(in    ) :: from
   type     (str_t), allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Copy a variable (of G_vars list) known by its name to a str_t of rank 0, 1 or 2
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_cpVarToS_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_cpVarToS_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
      
   END SUBROUTINE CalmatSetNGet_cpVarToS_byName


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Getter: pointer to a variable container +++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!- 1) The index (from%Id) of the variable (in G_vars) is known:
!
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varIdToIptr ( var, ptr, stat )
!=============================================================================================   
   type   (ind_t),           intent(in    ) :: var
   integer(Ikind), pointer , intent(   out) :: ptr(..)
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate an integer pointer of rank 0, 1 or 2 to a variable known by its index in G_vars
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varIdToIptr'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_ptr_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_varIdToIptr 
   
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varIdToRptr ( var, ptr, stat )
!=============================================================================================   
   type(ind_t),           intent(in    ) :: var
   real(Rkind), pointer , intent(in out) :: ptr(..)
   type(err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate a real pointer of rank 0, 1 or 2 to a variable known by its index
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varIdToRptr'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_ptr_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_varIdToRptr 

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varIdToCptr ( var, ptr, stat )
!=============================================================================================   
   type   (ind_t),           intent(in    ) :: var
   complex(Rkind), pointer , intent(   out) :: ptr(..)
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate a complex pointer of rank 0, 1 or 2 to a variable known by its index
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varIdToCptr'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_ptr_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_varIdToCptr  

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varIdToLptr ( var, ptr, stat )
!=============================================================================================   
   type   (ind_t),           intent(in    ) :: var
   logical       , pointer , intent(   out) :: ptr(..)
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate a logical pointer of rank 0, 1 or 2 to a variable known by its index
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varIdToLptr'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_ptr_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_varIdToLptr     

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varIdToSptr ( var, ptr, stat )
!=============================================================================================   
   type(ind_t),           intent(in    ) :: var
   type(str_t), pointer , intent(   out) :: ptr(..)
   type(err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate a str_t pointer of rank 0, 1 or 2 to a variable known by its index
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varIdToSptr'
!---------------------------------------------------------------------------------------------
   
#include "include/calmatsetnget_ptr_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_varIdToSptr    
!
!- 2) The name of the variable (in G_vars) is known:
!
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varNameToIptr ( var, ptr, stat )
!=============================================================================================   
   character(len=*),           intent(in    ) :: var
   integer  (Ikind), pointer , intent(   out) :: ptr(..)
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate an integer pointer of rank 0, 1 or 2 to a variable known by its name
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varNameToptr'
   type     (ind_t)            :: v
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = var, varId = v%Id )
   if ( v%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//var//'"', where=HERE, stat=G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_varIdToIptr ( var = v, ptr = ptr, stat = stat )
      if ( present(stat) ) then
         if ( stat > IZERO ) then 
            call stat%AddMsg ( ' (of name "'//trim(var)//'")' )
            call stat%AddTrace ( HERE )
            return
         end if
      end if
      !if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_varNameToIptr

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varNameToRptr ( var, ptr, stat )
!=============================================================================================   
   character(len=*),           intent(in    ) :: var
   real     (Rkind), pointer , intent(   out) :: ptr(..)
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate a real pointer of rank 0, 1 or 2 to a variable known by its name
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varNameToRptr'
   type     (ind_t)            :: v
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = var, varId = v%Id )
   if ( v%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//var//'"', where=HERE, stat=G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_varIdToRptr ( var = v, ptr = ptr, stat = stat )
      if ( present(stat) ) then
         if ( stat > IZERO ) then 
            call stat%AddMsg ( ' (of name "'//trim(var)//'")' )
            call stat%AddTrace ( HERE )
            return
         end if
      end if
      !if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_varNameToRptr 

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varNameToCptr ( var, ptr, stat )
!=============================================================================================   
   character(len=*),           intent(in    ) :: var
   complex  (Rkind), pointer , intent(   out) :: ptr(..)
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate a complex pointer of rank 0, 1 or 2 to a variable known by its name
!---------------------------------------------------------------------------------------------
   
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varNameToCptr'
   type     (ind_t)            :: v
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = var, varId = v%Id )
   if ( v%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//var//'"', where=HERE, stat=G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_varIdToCptr ( var = v, ptr = ptr, stat = stat )
      if ( present(stat) ) then
         if ( stat > IZERO ) then 
            call stat%AddMsg ( ' (of name "'//trim(var)//'")' )
            call stat%AddTrace ( HERE )
            return
         end if
      end if
      !if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_varNameToCptr

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varNameToLptr ( var, ptr, stat )
!=============================================================================================   
   character(len=*),           intent(in    ) :: var
   logical         , pointer , intent(   out) :: ptr(..)
   type     (err_t), optional, intent(in out) :: stat
 !---------------------------------------------------------------------------------------------
!  Associate a logical pointer of rank 0, 1 or 2 to a variable known by its name
!---------------------------------------------------------------------------------------------
  
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varNameToLptr'
   type     (ind_t)            :: v
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = var, varId = v%Id )
   if ( v%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//var//'"', where=HERE, stat=G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_varIdToLptr ( var = v, ptr = ptr, stat = stat )
      if ( present(stat) ) then
         if ( stat > IZERO ) then 
            call stat%AddMsg ( ' (of name "'//trim(var)//'")' )
            call stat%AddTrace ( HERE )
            return
         end if
      end if
      !if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
      
   END SUBROUTINE CalmatSetNGet_varNameToLptr

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_varNameToSptr ( var, ptr, stat )
!=============================================================================================   
   character(len=*),           intent(in    ) :: var
   type     (str_t), pointer , intent(   out) :: ptr(..)
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Associate a str_t pointer of rank 0, 1 or 2 to a variable known by its name
!---------------------------------------------------------------------------------------------
   
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_varNameToSptr'
   type     (ind_t)            :: v
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = var, varId = v%Id )
   if ( v%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//var//'"', where=HERE, stat=G_UERROR ) 
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_varIdToSptr ( var = v, ptr = ptr, stat = stat )
      if ( present(stat) ) then
         if ( stat > IZERO ) then 
            call stat%AddMsg ( ' (of name "'//trim(var)//'")' )
            call stat%AddTrace ( HERE )
            return
         end if
      end if
      !if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_varNameToSptr


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Getter: move allocation from a variable container to an intrinsic +++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!- 1) The index of the variable (in G_vars) is known:
!
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToI_byId ( from, to, stat )
!=============================================================================================   
   type   (ind_t),              intent(in out) :: from
   integer(Ikind), allocatable, intent(in out) :: to(..)
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Move allocation from a variable of index "from"  to an integer of rank 0,1, or 2 
!  Note: actually, move allocation only applies for "to" of rank 2. For rank < 2 a copy 
!        is made and the variable G_vars(from) is deallocated
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatSetNGet_mvVarToI_byId'
   character(len=:), allocatable :: name
   integer  (Ikind)              :: toType, err
!---------------------------------------------------------------------------------------------
   
   toType = ITYP
   
#include "include/calmatsetnget_mv_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_mvVarToI_byId    

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToR_byId ( from, to, stat )
!=============================================================================================   
   type(ind_t),              intent(in out) :: from
   real(Rkind), allocatable, intent(in out) :: to(..)
   type(err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatSetNGet_mvVarToR_byId'
   character(len=:), allocatable :: name
   integer  (Ikind)              :: toType,  err
!---------------------------------------------------------------------------------------------

   toType = RTYP

#include "include/calmatsetnget_mv_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_mvVarToR_byId

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToC_byId ( from, to, stat )
!=============================================================================================   
   type   (ind_t),              intent(in out) :: from
   complex(Rkind), allocatable, intent(in out) :: to(..)
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatSetNGet_mvVarToC_byId'
   character(len=:), allocatable :: name
   integer  (Ikind)              :: toType, err
!---------------------------------------------------------------------------------------------

   toType = CTYP

#include "include/calmatsetnget_mv_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_mvVarToC_byId

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToL_byId ( from, to, stat )
!=============================================================================================   
   type   (ind_t),              intent(in out) :: from
   logical       , allocatable, intent(in out) :: to(..)
   type   (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatSetNGet_mvVarToL_byId'
   character(len=:), allocatable :: name
   integer  (Ikind)              :: toType, err
!---------------------------------------------------------------------------------------------

   toType = LTYP

#include "include/calmatsetnget_mv_var_to_x.inc"

   END SUBROUTINE CalmatSetNGet_mvVarToL_byId

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToS_byId ( from, to, stat )
!=============================================================================================   
   type(ind_t),              intent(in out) :: from
   type(str_t), allocatable, intent(in out) :: to(..)
   type(err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------

!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'CalmatSetNGet_mvVarToS_byId'
   character(len=:), allocatable :: name
   integer  (Ikind)              :: toType, err
!---------------------------------------------------------------------------------------------

   toType = STYP
   
   select rank(to)
      rank(0)
      call allocateS0 ( to ) ! workaround for a nagfor bug
   end select
   
#include "include/calmatsetnget_mv_var_to_x.inc"

   contains
   
      subroutine allocateS0 ( s )
      type(str_t), allocatable, intent(in out) :: s
      if ( .not. allocated(s) ) allocate(s)
      end subroutine allocateS0
   
   END SUBROUTINE CalmatSetNGet_mvVarToS_byId 
!
!- 2) The index of the variable (in G_vars) is known:
!
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToI_byName ( from, to, stat )
!=============================================================================================   
   character(len=*),              intent(in    ) :: from
   integer  (Ikind), allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------
   
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvVarToI_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_mvVarToI_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_mvVarToI_byName

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToR_byName ( from, to, stat )
!=============================================================================================   
   character(len=*),              intent(in    ) :: from
   real     (Rkind), allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------
   
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvVarToR_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_mvVarToR_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if   

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_mvVarToR_byName
   
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToC_byName ( from, to, stat )
!=============================================================================================   
   character(len=*),              intent(in    ) :: from
   complex  (Rkind), allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------
   
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvVarToC_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_mvVarToC_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if   

   call err_resetHaltingMode ( reset = .true. )
   
   END SUBROUTINE CalmatSetNGet_mvVarToC_byName

!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToL_byName ( from, to, stat )
!=============================================================================================   
   character(len=*),              intent(in    ) :: from
   logical         , allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------
   
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvVarToL_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_mvVarToL_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if   

   call err_resetHaltingMode ( reset = .true. )

   END SUBROUTINE CalmatSetNGet_mvVarToL_byName
   
!=============================================================================================   
   SUBROUTINE CalmatSetNGet_mvVarToS_byName ( from, to, stat )
!=============================================================================================   
   character(len=*),              intent(in    ) :: from
   type     (str_t), allocatable, intent(in out) :: to(..)
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see CalmatSetNGet_mvVarToI_byId) 
!---------------------------------------------------------------------------------------------
  
!- local variables: --------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'CalmatSetNGet_mvVarToS_byName'
   type     (ind_t)            :: var
!---------------------------------------------------------------------------------------------

   call err_resetHaltingMode ( reset = .false., stat = stat )

   call calmatUtil_findVar ( varName = from, varId = var%Id )
   if ( var%Id == IZERO ) then
      call G_flagerr%set ( msg='Unknown variable "'//from//'"', where=HERE, stat=G_UERROR )
      if ( present(stat) ) then ; setNreturn(G_flagerr,stat) ; end if
   else
      call CalmatSetNGet_mvVarToS_byId ( from = var, to = to, stat = stat )
      if ( present(stat) ) then ; if_error_trace_and_RETURN ( stat, HERE ) ; end if
   end if

   call err_resetHaltingMode ( reset = .true. )
      
   END SUBROUTINE CalmatSetNGet_mvVarToS_byName


   
END MODULE CalmatSetNGet_m
