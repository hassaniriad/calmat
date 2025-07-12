!---------------------------------------------------------------------------------------------
! CALMAT-2019, A Command Line Calculator for Matrix Operations
!---------------------------------------------------------------------------------------------
!
! Module CalmatGlobal
!
! This module contains the definition of shared parameters and DT
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Date: 11/18
! Modified: 03/19, 07/20
!---------------------------------------------------------------------------------------------

MODULE CalmatGlobal_m
   
   use, intrinsic :: ieee_arithmetic

   use pk2_m            !<-- defines the DT pk2_t
   
   use CalmatVar_m         !<-- defines the DT var_t
   use CalmatObject_m      !<-- defines the DT obj_t
   use CalmatInstruction_m !<-- defines the DT ins_t and the DT flw_t
   use CalmatConstants_m
      
   implicit none
         
!--------------------------------------------------------------------------------------------- 
!  Declaration of shared variables (their names start with a 'G_')
!--------------------------------------------------------------------------------------------- 
!
!- Error handler:
!
   type   (err_t)              :: G_flagerr 
!
!- Flows of instructions:
!   
   type   (flw_t), allocatable :: G_flows(:)     ! Flows of instructions to be parsed
   integer(Ikind)              :: G_curFlow  = 0 ! Current flow (= current number of flows)        
   integer(Ikind)              :: G_maxnFlow     ! Max. number of flows (will be able to  
   integer(Ikind), parameter   :: G_incnFlow = 5 ! grow incrementally by G_incnFlow >=2)
!
!- The set of user's variables:
!
   type   (var_t), allocatable, target :: G_vars(:)      ! Register of variables
   integer(Ikind)                      :: G_nVar    = 0  ! Number of current variables    
   integer(Ikind)                      :: G_maxnVar      ! Max. number of variables (can grows
   integer(Ikind), parameter           :: G_incnVar = 20 ! incrementally by G_incnVar)
   type   (str_t), allocatable         :: G_varNames(:)  ! Current list of variable names
   
!- The set of user's objects:

   type   (obj_t), allocatable :: G_objs(:)      ! Register of objects
   integer(Ikind)              :: G_nObj    = 0  ! Number of current objects          
   integer(Ikind)              :: G_maxnObj      ! Max. number of objects (will be able to
   integer(Ikind ), parameter  :: G_incnObj = 20 ! to grow incrementally by G_incnObj)
   type   (str_t), allocatable :: G_objNames(:)  ! Current list of object names
!
!- Some user's default parameters:
!
   character(len=:), allocatable :: G_filedef    ! User's default settings file
   character(len=:), allocatable :: G_colorHelp, G_colorTitle, G_farewell
   
   type     (str_t)              :: G_prompt     ! Prompt symbol
                                    
   type     (str_t)              :: G_gnuplot(3) ! 1: path, 2: term, 3: font & size
   
   logical                       :: G_welcome   = .true., G_traceback = .true.
   
   integer  (Ikind)              :: G_digmax = 8,       &  ! Number of digits to show
                                    G_maxdisp(2) = [10,10] ! By default, display only this 
                                                           ! portion of arrays (for statement
                                                           ! not followed by a ";")
!
!- Auxiliary variables:
!    
   logical                     :: G_dispPrompt ! Will be set to .false. if batch mode

   logical                     :: G_init = .false.
   
   integer(Ikind)              :: G_ins ! current instruction #
   
   integer(Ikind)              :: G_verb = 1      ! Print error/warning messages when > 0
   logical                     :: G_disp = .true. ! Display or not display results (that's...)

   integer(Ikind)              :: G_uo = STDOUT ! Output unit (default: stdout)
   
   integer(Ikind)              :: G_task  = G_CONTINUE, &   ! Task manager
                                  G_start = 1

   integer(Ikind)              :: G_nLine = 1
                     
   type   (str_t), allocatable :: G_FuncNames(:), & ! List of avalaible fct in the interpreter
                                  G_FuncDescr(:), & ! Their short descriptions
                                  G_CmdNames (:), & ! List of available commands
                                  G_CmdDescr (:)    ! Their short descriptions
!
!- All concerning the version
!   
   character(len=:), allocatable :: G_numVersion ! version ID
   character(len=:), allocatable :: G_compilDate ! compilation date
   character(len=:), allocatable :: G_compilOpts ! compilation options
                    
END MODULE CalmatGlobal_m
