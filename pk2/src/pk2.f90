! Copyright 2018-2024  Riad Hassani, Universite Cote d'Azur
!
! This file is part of the pk2 library.
!
! The  pk2 library  is free software: you can redistribute it and/or modify it under the terms
! of  the  GNU  General  Public  License  as published by the Free Software Foundation, either 
! version 3 of the License, or (at your option) any later version.
!
! The pk2 library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
! without  even the implied warranty of  MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE. 
! See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with the pk2 library.
! If not, see <https://www.gnu.org/licenses/>. 


!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: pk2
!
! Description: 
! This module defines the DT pk2_t (for "Polymorphic ranK-2" array) for which the main member 
! ("m") is a polymorphic DT of rank 2 array (bk2_t).  At this time, available types are ik2_t 
! (for integer matrix), rk2_t (real), ck2_t (complex), lk2_t (logical), sk2_t (string). 
! Each of them are defined in the low-level module bk2 (for "Base ranK-2" matrix).
!---------------------------------------------------------------------------------------------
#include "error.fpp"

MODULE pk2_m

   use bk2_m
      
   implicit none
   
   type :: pk2_t
      class    (bk2_t), allocatable :: m                ! matrix value(s)   
      character(len=:), allocatable :: name             ! name
      integer  (Ikind)              :: typ = EMPTY, &   ! a copy of m%typ
                                       nrow = 0   , &   ! a copy of m%nrow
                                       ncol = 0         ! a copy of m%ncol
      logical                       :: constr = .false. ! .true. when defined by the constr.
   contains                                               
!
!-    Some useful procedures:
!
      ! Deleting row or column:
      procedure, pass :: pk2_i32DelRows, pk2_i64DelRows
      generic, public :: DelRows => pk2_i32DelRows, pk2_i64DelRows  
      
      ! Copying or adding a set of elements:
      procedure, pass :: pk2_i32SetSubMat, pk2_i64SetSubMat
      generic, public :: SetSubMat => pk2_i32SetSubMat, pk2_i64SetSubMat 

      ! Extracting a sub-matrix:
      procedure, pass :: pk2_i32ExtracSubMat, pk2_i64ExtracSubMat
      generic, public :: ExtracSubMat => pk2_i32ExtracSubMat, pk2_i64ExtracSubMat          

      ! Inserting an array into another one
      procedure, pass :: pk2_i32InsertInto, pk2_i64InsertInto
      generic, public :: InsertInto => pk2_i32InsertInto, pk2_i64InsertInto

      ! Making a copy into a rank 2 array of an intrinsic type:   
      procedure, pass :: pk2_GetMatI   !  into an integer array
      !!essai 05/24: procedure, pass :: pk2_GetI32stat, pk2_GetI64stat   !  into an integer array
      procedure, pass :: pk2_GetMatR   !        a real    array
      procedure, pass :: pk2_GetMatC   !        a complex array
      procedure, pass :: pk2_GetMatL   !        a logical array
      procedure, pass :: pk2_GetMatS   !        a string  array
      procedure, pass :: pk2_GetMatBk2 !        a bk2 variable
      generic, public :: GetMat => pk2_GetMatI, pk2_GetMatR, pk2_GetMatC, &
                                   pk2_GetMatL, pk2_GetMatS, pk2_GetMatBk2

      !!essai 05/24: generic, public :: GetMat => pk2_GetI32stat, pk2_GetI64stat, pk2_GetMatR, pk2_GetMatC, &

      ! Making a packed copy into a rank 1 array of an intrinsic type:   
      procedure, pass :: pk2_GetMatPackedI   ! into an integer rank 1 array
      procedure, pass :: pk2_GetMatPackedR   !       a real    rank 1 array
      procedure, pass :: pk2_GetMatPackedC   !       a complex rank 1 array
      procedure, pass :: pk2_GetMatPackedL   !       a logical rank 1 array
      procedure, pass :: pk2_GetMatPackedS   !       a string  rank 1 array
      generic, public :: GetMatPacked => pk2_GetMatPackedI, pk2_GetMatPackedR, &
                                         pk2_GetMatPackedC, pk2_GetMatPackedL, &
                                         pk2_GetMatPackedS

      ! Making a copy of an element (i,j) into a scalar intrinsic type:   
      procedure, pass :: pk2_GetMatijI   ! into an integer 
      procedure, pass :: pk2_GetMatijR   !       a real    
      procedure, pass :: pk2_GetMatijC   !       a complex 
      procedure, pass :: pk2_GetMatijL   !       a logical 
      procedure, pass :: pk2_GetMatijS   !       a string  
      procedure, pass :: pk2_GetMatijCh  !       a character  
      generic, public :: GetMatij => pk2_GetMatijI, pk2_GetMatijR, &
                                     pk2_GetMatijC, pk2_GetMatijL, &
                                     pk2_GetMatijS, pk2_GetMatijCh
               
      generic, public :: get => pk2_GetMatijI, pk2_GetMatijR, &
                                pk2_GetMatijC, pk2_GetMatijL, &
                                pk2_GetMatijS, pk2_GetMatijCh,&
                                pk2_GetMatPackedI, pk2_GetMatPackedR, &
                                pk2_GetMatPackedC, pk2_GetMatPackedL, &
                                pk2_GetMatPackedS, &
                                pk2_GetMatI, pk2_GetMatR, pk2_GetMatC, &
                                pk2_GetMatL, pk2_GetMatS, pk2_GetMatBk2                                        
      ! pointer association:
      procedure, pass :: pk2_IPointer,  pk2_IvecPointer,  pk2_ImatPointer
      procedure, pass :: pk2_RPointer,  pk2_RvecPointer,  pk2_RmatPointer
      procedure, pass :: pk2_CPointer,  pk2_CvecPointer,  pk2_CmatPointer
      procedure, pass :: pk2_LPointer,  pk2_LvecPointer,  pk2_LmatPointer
      procedure, pass :: pk2_SPointer,  pk2_SvecPointer,  pk2_SmatPointer
      !procedure, pass :: pk2_pointer
      generic, public :: Pointer => pk2_IPointer,  pk2_IvecPointer,  pk2_ImatPointer, &
                                    pk2_Rpointer,  pk2_RvecPointer,  pk2_RmatPointer, &
                                    pk2_CPointer,  pk2_CvecPointer,  pk2_CmatPointer, &
                                    pk2_LPointer,  pk2_LvecPointer,  pk2_LmatPointer, &
                                    pk2_SPointer,  pk2_SvecPointer,  pk2_SmatPointer
                                    !pk2_pointer

      ! Other:
      procedure, public, pass :: ReadMe  => pk2_ReadMat  ! read an array from a file
      procedure, public, pass :: WriteMe => pk2_WriteMat ! write an array in column-organized
      procedure, public, pass :: PrintMe => pk2_Print    ! print a variable
      procedure, public, pass :: Destroy => pk2_Destroy
      procedure, public, pass :: Is_Symm => pk2_IsSymm
      procedure, public, pass :: LU      => pk2_LU2
      procedure, public, pass :: LUsolv  => pk2_LUsolv
      procedure, public, pass :: inv     => pk2_inv
                                          
      procedure, pass :: resetDesc => pk2_resetDesc
      
      !final :: pk2_Finalize
                                            
   end type pk2_t   

!
!- Defined constructor:
!   
   interface pk2_t
      module procedure pk2_fSetToI32, pk2_fSetToI64  ! set to a given integer scalar
      module procedure pk2_fSetToRsp, pk2_fSetToRdp  ! set to a given real scalar
      module procedure pk2_fSetToCsp, pk2_fSetToCdp  ! set to a given complex scalar
      module procedure pk2_fSetToL                   ! set to a given boolean scalar
      module procedure pk2_fSetToS                   ! set to a given str_t scalar
      module procedure pk2_fSetToCh                  ! set to a given character string scalar

      module procedure pk2_fSetToI32vec, pk2_fSetToI64vec ! set to a given vector of integers
      module procedure pk2_fSetToRspVec, pk2_fSetToRdpVec ! set to a given vector of reals
      module procedure pk2_fSetToCspVec, pk2_fSetToCdpVec ! set to a given vector of complexes
      module procedure pk2_fSetToLvec                     ! set to a given vector of booleans
      module procedure pk2_fSetToSvec                     ! set to a given vector of str_t
      module procedure pk2_fSetToChvec                    ! set to a given vector of strings
      
      module procedure pk2_fSetToI32mat, pk2_fSetToI64mat ! set to a given matrix of integers
      module procedure pk2_fSetToRspMat, pk2_fSetToRdpMat ! set to a given matrix of reals
      module procedure pk2_fSetToCspMat, pk2_fSetToCdpMat ! set to a given matrix of complexes
      module procedure pk2_fSetToLmat                     ! set to a given matrix of booleans
      module procedure pk2_fSetToSmat                     ! set to a given matric of str_t 
      module procedure pk2_fSetToChmat                    ! set to a given matrix of strings
      
      module procedure pk2_fi32i32SetToType ! initialize to a prescribed shape of a given type 
      module procedure pk2_fi32i64SetToType ! initialize to a prescribed shape of a given type 
      module procedure pk2_fi64i32SetToType ! initialize to a prescribed shape of a given type 
      module procedure pk2_fi64i64SetToType ! initialize to a prescribed shape of a given type
      
      module procedure pk2_fSetToEmpty
      
   end interface
!
!- Operators overload:
!
!  Note(01/2024): bear in mind when using these operators in a complicated expression that 
!   - user-defined UNARY  operators have the HIGHEST precedence
!   - user-defined BINARY operators have the LOWEST  precedence
!  (ref.: stevelionel.com/drfortran/2021/04/03/doctor-fortran-in-order-order)
!
   interface assignment(=)  
      module procedure pk2_SetToPk2, pk2_SetToBk2   ,                  &
                       pk2_SetToI32, pk2_SetToI32vec, pk2_SetToI32mat, &
                       pk2_SetToI64, pk2_SetToI64vec, pk2_SetToI64mat, &
                       pk2_SetToRsp, pk2_SetToRspVec, pk2_SetToRspMat, &
                       pk2_SetToRdp, pk2_SetToRdpVec, pk2_SetToRdpMat, &
                       pk2_SetToCsp, pk2_SetToCspVec, pk2_SetToCspMat, &
                       pk2_SetToCdp, pk2_SetToCdpVec, pk2_SetToCdpMat, &
                       pk2_SetToL  , pk2_SetToLvec  , pk2_SetToLmat  , &
                       pk2_SetToS  , pk2_SetToSvec  , pk2_SetToSmat  , &
                       pk2_SetToCh , pk2_SetToChvec , pk2_SetToChmat 
      !! experim
      !module procedure pk2_fromPk2toI32, pk2_fromPk2toI64
                       
                       
      ! essai 05/24:
      !!module procedure pk2_GetI32, pk2_GetI64              
   end interface
   
   interface operator(+)     
      module procedure pk2_Add,                                            &
                       pk2_AddIntg, pk2_IntgAdd, pk2_AddReal, pk2_RealAdd, &
                       pk2_AddCplx, pk2_CplxAdd, pk2_AddStr , pk2_StrAdd , &
                       pk2_AddChar, pk2_CharAdd, pk2_AddBool, pk2_BoolAdd
   end interface
   
   interface operator(-)     
      module procedure pk2_minus  , pk2_Sub    ,                           &
                       pk2_SubIntg, pk2_IntgSub, pk2_SubReal, pk2_RealSub, &      
                       pk2_SubCplx, pk2_CplxSub, pk2_SubBool, pk2_BoolSub, &
                       pk2_SubStr , pk2_StrSub , pk2_SubChar, pk2_CharSub
   end interface

   interface operator(*)     
      module procedure pk2_mult,                                               &
                       pk2_MultIntg, pk2_IntgMult, pk2_MultReal, pk2_RealMult, &
                       pk2_MultCplx, pk2_CplxMult, pk2_MultBool, pk2_BoolMult, &
                       pk2_MultStr , pk2_StrMult , pk2_MultChar, pk2_CharMult
   end interface

   interface operator(/)     
      module procedure pk2_Div,                                            &
                       pk2_DivIntg, pk2_IntgDiv, pk2_DivReal, pk2_RealDiv, &
                       pk2_DivCplx, pk2_CplxDiv, pk2_DivBool, pk2_BoolDiv, &
                       pk2_DivStr , pk2_StrDiv , pk2_DivChar, pk2_CharDiv
   end interface

   interface operator(**)        
      module procedure pk2_Pow,                                            &
                       pk2_PowIntg, pk2_IntgPow, pk2_PowReal, pk2_RealPow, &
                       pk2_PowCplx, pk2_CplxPow, pk2_PowBool, pk2_BoolPow, &
                       pk2_PowStr , pk2_StrPow , pk2_PowChar, pk2_CharPow
   end interface

   interface operator(.m.)        
      module procedure pk2_eMult,                                                  &
                       pk2_eMultIntg, pk2_IntgEmult, pk2_eMultReal, pk2_RealEmult, &
                       pk2_eMultCplx, pk2_CplxEmult, pk2_eMultBool, pk2_BoolEmult, &
                       pk2_eMultStr , pk2_StrEmult , pk2_eMultChar, pk2_CharEmult
   end interface

   interface operator(.d.)        
      module procedure pk2_eDiv,                                               &
                       pk2_eDivIntg, pk2_IntgEdiv, pk2_eDivReal, pk2_RealEdiv, &
                       pk2_eDivCplx, pk2_CplxEdiv, pk2_eDivBool, pk2_BoolEdiv, &
                       pk2_eDivStr , pk2_StrEdiv , pk2_eDivChar, pk2_CharEdiv
   end interface

   interface operator(.p.)           
      module procedure pk2_ePow,                                               &
                       pk2_ePowIntg, pk2_IntgEpow, pk2_ePowReal, pk2_RealEpow, &
                       pk2_ePowCplx, pk2_CplxEpow, pk2_ePowBool, pk2_BoolEpow, &
                       pk2_ePowStr , pk2_StrEpow , pk2_ePowChar, pk2_CharEpow    
   end interface

   interface operator(.and.)           
      module procedure pk2_And,                                            &
                       pk2_AndBool, pk2_BoolAnd, pk2_AndIntg, pk2_IntgAnd, &
                       pk2_AndReal, pk2_RealAnd, pk2_AndCplx, pk2_CplxAnd, &
                       pk2_AndStr , pk2_StrAnd , pk2_AndChar, pk2_CharAnd
   end interface

   interface operator(.or.)    
      module procedure pk2_Or,                                         &
                       pk2_OrBool, pk2_BoolOr, pk2_OrIntg, pk2_IntgOr, &
                       pk2_OrReal, pk2_RealOr, pk2_OrCplx, pk2_CplxOr, &
                       pk2_OrStr , pk2_StrOr , pk2_OrChar, pk2_CharOr
   end interface

   interface operator(.not.)    
      module procedure pk2_not
   end interface


   interface operator(==)    
      module procedure pk2_Eq,                                         &
                       pk2_EqBool, pk2_BoolEq, pk2_EqIntg, pk2_IntgEq, &
                       pk2_EqReal, pk2_RealEq, pk2_EqCplx, pk2_CplxEq, &
                       pk2_EqStr , pk2_StrEq , pk2_EqChar, pk2_CharEq 
   end interface

   interface operator(/=)    
      module procedure pk2_Ne,                                         &
                       pk2_NeBool, pk2_BoolNe, pk2_NeIntg, pk2_IntgNe, &
                       pk2_NeReal, pk2_RealNe, pk2_NeCplx, pk2_CplxNe, &
                       pk2_NeStr , pk2_StrNe , pk2_NeChar, pk2_CharNe
   end interface

   interface operator(>)    
      module procedure pk2_Gt,                                         &
                       pk2_GtBool, pk2_BoolGt, pk2_GtIntg, pk2_IntgGt, &
                       pk2_GtReal, pk2_RealGt, pk2_GtCplx, pk2_CplxGt, &
                       pk2_GtStr , pk2_StrGt , pk2_GtChar, pk2_CharGt
   end interface

   interface operator(<)    
      module procedure pk2_Lt,                                         &
                       pk2_LtBool, pk2_BoolLt, pk2_LtIntg, pk2_IntgLt, &
                       pk2_LtReal, pk2_RealLt, pk2_LtCplx, pk2_CplxLt, &
                       pk2_LtStr , pk2_StrLt , pk2_LtChar, pk2_CharLt
   end interface

   interface operator(>=)    
      module procedure pk2_Ge,                                         &
                       pk2_GeBool, pk2_BoolGe, pk2_GeIntg, pk2_IntgGe, &
                       pk2_GeReal, pk2_RealGe, pk2_GeCplx, pk2_CplxGe, &
                       pk2_GeStr , pk2_StrGe , pk2_GeChar, pk2_CharGe
   end interface

   interface operator(<=)    
      module procedure pk2_Le,                                         &
                       pk2_LeBool, pk2_BoolLe, pk2_LeIntg, pk2_IntgLe, &
                       pk2_LeReal, pk2_RealLe, pk2_LeCplx, pk2_CplxLe, &
                       pk2_LeStr , pk2_StrLe , pk2_LeChar, pk2_CharLe    

   end interface

   interface operator(.bslash.)    
      module procedure pk2_mldivide !--> operator "\"
   end interface


   interface pk2_movealloc
      module procedure pk2_moveallocPk2ToPk2 ,                         &
                       pk2_moveallocImatToPk2, pk2_moveallocPk2ToImat, & 
                       pk2_moveallocRmatToPk2, pk2_moveallocPk2ToRmat, &
                       pk2_moveallocCmatToPk2, pk2_moveallocPk2ToCmat, &
                       pk2_moveallocLmatToPk2, pk2_moveallocPk2ToLmat, &
                       pk2_moveallocSmatToPk2, pk2_moveallocPk2ToSmat, &
                       pk2_moveallocPk2ToCH
   end interface
   
   interface pk2_Assign  
   !
   ! to avoid the overhead of the defined assignment and whenever possible, it is better to 
   ! call these procedures (because a = b is not equivalent to call pk2_Assign( a,b ) but to 
   ! pk2_Assign( a,(b) ) that is, a copy to create a temporary is made which can cause an 
   ! overflow of the stack especially with ifort if -heap_arrays option is not used)
   !   
      module procedure pk2_SetToPk2Stat, pk2_SetToBk2Stat   , pk2_SetToType      , &
                       pk2_SetToI32Stat, pk2_SetToI32vecStat, pk2_SetToI32matStat, &
                       pk2_SetToI64Stat, pk2_SetToI64vecStat, pk2_SetToI64matStat, &
                       pk2_SetToRspStat, pk2_SetToRspVecStat, pk2_SetToRspMatStat, &
                       pk2_SetToRdpStat, pk2_SetToRdpVecStat, pk2_SetToRdpMatStat, &
                       pk2_SetToCspStat, pk2_SetToCspVecStat, pk2_SetToCspMatStat, &
                       pk2_SetToCdpStat, pk2_SetToCdpVecStat, pk2_SetToCdpMatStat, &
                       pk2_SetToLStat  , pk2_SetToLvecStat  , pk2_SetToLmatStat  , &
                       pk2_SetToSStat  , pk2_SetToSvecStat  , pk2_SetToSmatStat  , &
                       pk2_SetToChStat , pk2_SetToChvecStat , pk2_SetToChmatStat
   ! essai 05/24
    !!  module procedure pk2_GetI32stat, pk2_GetI64stat
   
   end interface
   
   interface pk2_Insert  
      module procedure pk2_InsertImat32, pk2_InsertImat64, &
                       pk2_InsertRmat32, pk2_InsertRmat64, &
                       pk2_InsertCmat32, pk2_InsertCmat64, &
                       pk2_InsertLmat  , pk2_InsertSmat  , &
                       pk2_InsertChmat , pk2_InsertPk2
   end interface

   character(len=7) :: pk2Type (0:5) = ['empty  ','integer','real   ',&
                                        'complex','logical','string ']

CONTAINS


!=============================================================================================
   SUBROUTINE pk2_resetDesc ( self )
!=============================================================================================
   class(pk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Resets:
!  . self%typ to the type of self%m (if self%m is not allocated, sets self%typ = EMPTY)
!  . self%nrow, self%ncol to the shape of self%m%v (if self%v is not allocated, sets them to 0)
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind) :: n, m, typ
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()  

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (ik2_t)
            typ = ITYP
            if ( allocated(p%v) ) then
               n = size(p%v,1) ; m = size(p%v,2)
            else
               n = 0 ; m = 0
            end if
         type is (rk2_t)
            typ = RTYP
            if ( allocated(p%v) ) then
               n = size(p%v,1) ; m = size(p%v,2)
            else
               n = 0 ; m = 0
            end if
         type is (ck2_t)
            typ = CTYP
            if ( allocated(p%v) ) then
               n = size(p%v,1) ; m = size(p%v,2)
            else
               n = 0 ; m = 0
            end if
         type is (lk2_t)
            typ = LTYP
            if ( allocated(p%v) ) then
               n = size(p%v,1) ; m = size(p%v,2)
            else
               n = 0 ; m = 0
            end if
         type is (sk2_t)
            typ = STYP 
            if ( allocated(p%v) ) then
               n = size(p%v,1) ; m = size(p%v,2)
            else
               n = 0 ; m = 0
            end if
         class default
            typ = EMPTY
            n = 0 ; m = 0
      end select
      self%m%nrow = n ; self%m%ncol = m ; self%m%typ = typ
      self%nrow   = n ; self%ncol   = m ; self%typ   = typ
   else
      self%nrow   = 0 ; self%ncol   = 0 ; self%typ   = EMPTY
   end if
   
   END SUBROUTINE pk2_resetDesc
   

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Move allocation +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!   From allocatable of intrinsic types to pk2:
!  . pk2_moveallocPk2ToPk2   (pk2 --> pk2)
!  . pk2_moveallocImatToPk2  (integer rank 2 array --> pk2)
!  . pk2_moveallocRmatToPk2  (real rank 2 array    --> pk2)
!  . pk2_moveallocCmatToPk2  (complex rank 2 array --> pk2)
!  . pk2_moveallocLmatToPk2  (logical rank 2 array --> pk2)
!  . pk2_moveallocSmatToPk2  (str_t rank 2 array   --> pk2)
!
!   From pk2 container to allocatable of intrinsic types:
!  . pk2_moveallocPk2ToImat  (pk2 --> integer rank 2 array)
!  . pk2_moveallocPk2ToRmat  (pk2 --> real rank 2 array)
!  . pk2_moveallocPk2ToCmat  (pk2 --> complex rank 2 array)
!  . pk2_moveallocPk2ToLmat  (pk2 --> logical rank 2 array)
!  . pk2_moveallocPk2ToSmat  (pk2 --> str_t rank 2 array)
!  . pk2_moveallocPk2ToCH    (first element of pk2 --> character)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

!=============================================================================================
   SUBROUTINE pk2_moveallocPk2ToPk2 ( from, to, movename, stat )
!=============================================================================================
   class  (pk2_t),           intent(in out) :: from
   class  (pk2_t),           intent(in out) :: to
   logical       , optional, intent(in    ) :: movename
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from" to "to" where "from" and "to" are two pk2_t variables.
!  If "movename" is .true., moves also %name (it is the default when movename is not present).
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables --------------------------------------------------------------------------- 
   logical :: mvname
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()  
   
   if ( present(movename) ) then
      mvname = movename
   else
      mvname = .true.
   end if
   
   if ( allocated(from%name) ) then
      if ( mvname ) then      
         call move_alloc (from = from%name, to = to%name)
      else
         deallocate(from%name)
      end if
   end if
   
   if ( allocated(from%m) ) then
      to%typ = from%typ ; to%nrow = from%nrow ; to%ncol = from%ncol
      call from%m%movealloc (to%m) 
      deallocate(from%m)
   else
      to%typ = EMPTY ; to%nrow = IZERO ; to%ncol = IZERO   
      if ( allocated(to%m) ) deallocate(to%m)   
   end if

   from%typ = EMPTY ; from%nrow = IZERO ; from%ncol = IZERO
   
   END SUBROUTINE pk2_moveallocPk2ToPk2


!=============================================================================================
   SUBROUTINE pk2_moveallocImatToPk2 ( from, to, stat )
!=============================================================================================
   integer(Ikind), allocatable, intent(in out) :: from(:,:)
   class  (pk2_t),              intent(in out) :: to
   type   (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from" to "to%m%v", where "from" is an allocatable integer array
!-----------------------------------------------------------------------------------R.H. 11/19       

   if ( opflag%code > IZERO ) return !!call opflag%set ()  

   if ( allocated(from) ) then
      call bk2_movealloc ( from = from, to = to%m )
      to%typ = to%m%typ ; to%nrow = to%m%nrow ; to%ncol = to%m%ncol
   else
      to%typ = EMPTY ; to%nrow = IZERO ; to%ncol = IZERO   
      if ( allocated(to%m) ) deallocate(to%m)
   end if
         
   END SUBROUTINE pk2_moveallocImatToPk2


!=============================================================================================
   SUBROUTINE pk2_moveallocRmatToPk2 ( from, to, stat )
!=============================================================================================
   real (Rkind), allocatable, intent(in out) :: from(:,:)
   class(pk2_t),              intent(in out) :: to
   type (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from" to "to%m%v", where "from" is an allocatable real array
!-----------------------------------------------------------------------------------R.H. 11/19       

   if ( opflag%code > IZERO ) return !!call opflag%set ()  

   if ( allocated(from) ) then
      call bk2_movealloc ( from = from, to = to%m )
      to%typ = to%m%typ ; to%nrow = to%m%nrow ; to%ncol = to%m%ncol
   else
      to%typ = EMPTY ; to%nrow = IZERO ; to%ncol = IZERO   
      if ( allocated(to%m) ) deallocate(to%m)
   end if
            
   END SUBROUTINE pk2_moveallocRmatToPk2


!=============================================================================================
   SUBROUTINE pk2_moveallocCmatToPk2 ( from, to, stat )
!=============================================================================================
   complex(Rkind), allocatable, intent(in out) :: from(:,:)
   class  (pk2_t),              intent(in out) :: to
   type   (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from" to "to%m%v", where "from" is an allocatable complex array
!-----------------------------------------------------------------------------------R.H. 11/19       

   if ( opflag%code > IZERO ) return !!call opflag%set ()  

   if ( allocated(from) ) then
      call bk2_movealloc ( from = from, to = to%m )
      to%typ = to%m%typ ; to%nrow = to%m%nrow ; to%ncol = to%m%ncol
   else
      to%typ = EMPTY ; to%nrow = IZERO ; to%ncol = IZERO   
      if ( allocated(to%m) ) deallocate(to%m)
   end if
            
   END SUBROUTINE pk2_moveallocCmatToPk2


!=============================================================================================
   SUBROUTINE pk2_moveallocLmatToPk2 ( from, to, stat )
!=============================================================================================
   logical       , allocatable, intent(in out) :: from(:,:)
   class  (pk2_t),              intent(in out) :: to
   type   (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from" to "to%m%v", where "from" is an allocatable logical array
!-----------------------------------------------------------------------------------R.H. 11/19       
      
   if ( opflag%code > IZERO ) return !!call opflag%set ()  

   if ( allocated(from) ) then
      call bk2_movealloc ( from = from, to = to%m )
      to%typ = to%m%typ ; to%nrow = to%m%nrow ; to%ncol = to%m%ncol
   else
      to%typ = EMPTY ; to%nrow = IZERO ; to%ncol = IZERO   
      if ( allocated(to%m) ) deallocate(to%m)
   end if
               
   END SUBROUTINE pk2_moveallocLmatToPk2


!=============================================================================================
   SUBROUTINE pk2_moveallocSmatToPk2 ( from, to, stat )
!=============================================================================================
   type (str_t), allocatable, intent(in out) :: from(:,:)
   class(pk2_t),              intent(in out) :: to
   type (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from" to "to%m%v", where "from" is an allocatable string array
!-----------------------------------------------------------------------------------R.H. 11/19       

   if ( opflag%code > IZERO ) return !!call opflag%set ()  

   if ( allocated(from) ) then
      call bk2_movealloc ( from = from, to = to%m )
      to%typ = to%m%typ ; to%nrow = to%m%nrow ; to%ncol = to%m%ncol
   else
      to%typ = EMPTY ; to%nrow = IZERO ; to%ncol = IZERO   
      if ( allocated(to%m) ) deallocate(to%m)
   end if
               
   END SUBROUTINE pk2_moveallocSmatToPk2


!=============================================================================================
   SUBROUTINE pk2_moveallocPk2ToImat ( from, to, stat )
!=============================================================================================
   class  (pk2_t),              intent(in out) :: from
   integer(Ikind), allocatable, intent(   out) :: to(:,:)
   type   (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from%m%v" to "to", where "to" is an allocatable integer array
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_moveallocPk2ToImat'
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()  
          
   if ( allocated(from%m) ) then
      select type (p=>from%m)
         type is (ik2_t)
            if ( allocated(p%v) ) call move_alloc (from = p%v, to = to)
         class default
            call opflag%set ( stat = UERROR, where = HERE, msg = &
                             'Cannot move allocation from non-integer pk2 to integer array' )
            if ( present(stat) ) stat = opflag
            allocate(to(0,0)) ; return
      end select
   else
      allocate(to(0,0))
   end if

   call from%destroy()
      
   END SUBROUTINE pk2_moveallocPk2ToImat
   
   
!=============================================================================================
   SUBROUTINE pk2_moveallocPk2ToRmat ( from, to, stat )
!=============================================================================================
   class(pk2_t),              intent(in out) :: from
   real (Rkind), allocatable, intent(   out) :: to(:,:)
   type (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from%m%v" to "to", where "to" is an allocatable real array
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_moveallocPk2ToRmat'
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()  
          
   if ( allocated(from%m) ) then
      select type (p=>from%m)
         type is (rk2_t)
            if ( allocated(p%v) ) call move_alloc (from = p%v, to = to)
         class default
            call opflag%set ( stat = UERROR, where = HERE, msg = &
                             'Cannot move allocation from non-real pk2 to real array' )
            if ( present(stat) ) stat = opflag
            allocate(to(0,0)) ; return
      end select
   else
      allocate(to(0,0))
   end if

   call from%destroy()
      
   END SUBROUTINE pk2_moveallocPk2ToRmat


!=============================================================================================
   SUBROUTINE pk2_moveallocPk2ToCmat ( from, to, stat )
!=============================================================================================
   class  (pk2_t),              intent(in out) :: from
   complex(Rkind), allocatable, intent(   out) :: to(:,:)
   type   (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from%m%v" to "to", where "to" is an allocatable complex array
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_moveallocPk2ToCmat'
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()  
          
   if ( allocated(from%m) ) then
      select type (p=>from%m)
         type is (ck2_t)
            if ( allocated(p%v) ) call move_alloc (from = p%v, to = to)
         class default
            call opflag%set ( stat = UERROR, where = HERE, msg = &
                            'Cannot move allocation from non-complex pk2 to complex array' )
            if ( present(stat) ) stat = opflag
            allocate(to(0,0)) ; return
      end select
   else
      allocate(to(0,0))
   end if

   call from%destroy()
      
   END SUBROUTINE pk2_moveallocPk2ToCmat
   

!=============================================================================================
   SUBROUTINE pk2_moveallocPk2ToLmat ( from, to, stat )
!=============================================================================================
   class  (pk2_t),              intent(in out) :: from
   logical       , allocatable, intent(   out) :: to(:,:)
   type   (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from%m%v" to "to", where "to" is an allocatable logical array
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_moveallocPk2ToLmat'
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()  
          
   if ( allocated(from%m) ) then
      select type (p=>from%m)
         type is (lk2_t)
            if ( allocated(p%v) ) call move_alloc (from = p%v, to = to)
         class default
            call opflag%set ( stat = UERROR, where = HERE, msg = &
                            'Cannot move allocation from non-logical pk2 to logical array' )
            if ( present(stat) ) stat = opflag
            allocate(to(0,0)) ; return
      end select
   else
      allocate(to(0,0))
   end if

   call from%destroy()
      
   END SUBROUTINE pk2_moveallocPk2ToLmat
   

!=============================================================================================
   SUBROUTINE pk2_moveallocPk2ToSmat ( from, to, stat )
!=============================================================================================
   class(pk2_t),              intent(in out) :: from
   type (str_t), allocatable, intent(   out) :: to(:,:)
   type (err_t), optional   , intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from%m%v" to "to", where "to" is an allocatable string array
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_moveallocPk2ToSmat'
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()  
   
   if ( allocated(from%m) ) then
      select type (p=>from%m)
         type is (sk2_t)
            if ( allocated(p%v) ) call move_alloc (from = p%v, to = to)
         class default
            call opflag%set ( stat = UERROR, where = HERE, msg = &
                            'Cannot move allocation from non-string pk2 to string array' )
            if ( present(stat) ) stat = opflag
            allocate(to(0,0)) ; return
      end select
   else
      allocate(to(0,0))
   end if

   call from%destroy()
      
   END SUBROUTINE pk2_moveallocPk2ToSmat


!=============================================================================================
   SUBROUTINE pk2_moveallocPk2ToCH ( from, to, stat )
!=============================================================================================
   class    (pk2_t),              intent(in out) :: from
   character(len=:), allocatable, intent(   out) :: to
   type     (err_t), optional   , intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Moves the allocation from "from%m%v(1,1)%str" to "to", where "to" is an allocatable string
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_moveallocPk2ToCH'
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO) return !!call opflag%set ()  
   
   if ( allocated(from%m) ) then
      select type (p=>from%m)
         type is (sk2_t)
            if ( p%nrow >= 1 .and. p%ncol >= 1 ) then
               call str_moveAlloc ( from = p%v(1,1), to = to ) ! RH 09/22
            end if
         class default
            call opflag%set ( stat = UERROR, where = HERE, msg = &
                            'Cannot move allocation from non-string pk2 to string' )
            if ( present(stat) ) stat = opflag
            return
      end select
   end if

   call from%destroy()
      
   END SUBROUTINE pk2_moveallocPk2ToCH


!=============================================================================================
   SUBROUTINE pk2_SetToPk2 ( lhs, rhs ) ! a verifier (leak w/ nagfor)
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   class(pk2_t), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Replaces all members of "lhs" (except %name) by those of "rhs".
!
!  Note (10/19): when lhs%m is of the same type as rhs%m and when lhs%m%v is already allocated 
!  and has the same shape as rhs%m%v avoid overhead due to deallocation/allocation.
!-----------------------------------------------------------------------------R.H. 05/18,10/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_SetToPk2'
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > 0 ) return !call opflag%set ()

   if ( allocated(rhs%name) .and. rhs%constr ) lhs%name = (rhs%name) ! 03/2019
   
   if ( allocated(rhs%m) ) then
      if ( allocated(lhs%m) .and. lhs%typ == rhs%typ .and. &
           lhs%nrow == rhs%nrow .and. lhs%ncol == rhs%ncol )  then
!
!-       don't reallocate lhs%m (lhs%m and rhs%m are of the same type and have the same shape):
!          
         select type(p=>rhs%m)
            type is (ik2_t)
               select type (q=>lhs%m) ; type is (ik2_t) ; q%v = p%v ; end select
            type is (rk2_t)
               select type (q=>lhs%m) ; type is (rk2_t) ; q%v = p%v ; end select
            type is (ck2_t)
               select type (q=>lhs%m) ; type is (ck2_t) ; q%v = p%v ; end select
            type is (lk2_t)
               select type (q=>lhs%m) ; type is (lk2_t) ; q%v = p%v ; end select
            type is (sk2_t)
               select type (q=>lhs%m) ; type is (sk2_t) ; q%v = p%v ; end select
            class default
               call opflag%Set(IERROR, HERE, &      
                                "Hum... we shouldn't be there with lhs%typ = rhs%typ!")
               return
         end select
      else   
!
!-       allocate (or reallocate) lhs%m:
!
         if ( allocated(lhs%m) ) deallocate(lhs%m)            
         allocate(lhs%m, source = rhs%m, stat = err) 
         
         if ( err == 0 ) then
            lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol
         else
            call opflag%Set(IERROR, HERE, 'Allocation failure')      
            return
         end if
      end if   
   else
      if ( allocated(lhs%m) ) deallocate(lhs%m)
      lhs%typ = rhs%typ ; lhs%nrow = 0 ; lhs%ncol = 0
   end if
   
   END SUBROUTINE pk2_SetToPk2


!=============================================================================================
   SUBROUTINE pk2_SetToPk2Stat ( lhs, rhs, stat ) 
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   class(pk2_t),           intent(in    ) :: rhs
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Same as pk2_Assign with "stat" (used in generic pk2_Assign)
!---------------------------------------------------------------------------------------------

   call pk2_SetToPk2 ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToPk2Stat')
      if ( present(stat) ) stat = opflag
   end if
   
   !if (opflag%code > 0 .and. present(stat)) stat = opflag
   
   END SUBROUTINE pk2_SetToPk2Stat


!=============================================================================================
   SUBROUTINE pk2_SetToBk2 ( lhs, rhs ) 
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   class(bk2_t), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 10/19       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_SetToBk2'
   integer  (Ikind)            :: err
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > 0 ) return !call opflag%set ()
   
   if ( allocated(lhs%m) .and. lhs%typ == rhs%typ .and. &
        lhs%nrow == rhs%nrow .and. lhs%ncol == rhs%ncol ) then
!
!-    don't reallocate lhs%m (lhs%m and rhs are of the same type and have the same shape):
!          
      select type(rhs)
         type is (ik2_t)
            select type (q=>lhs%m) ; type is (ik2_t) ; q%v = rhs%v ; end select
         type is (rk2_t)
            select type (q=>lhs%m) ; type is (rk2_t) ; q%v = rhs%v ; end select
         type is (ck2_t)
            select type (q=>lhs%m) ; type is (ck2_t) ; q%v = rhs%v ; end select
         type is (lk2_t)
            select type (q=>lhs%m) ; type is (lk2_t) ; q%v = rhs%v ; end select
         type is (sk2_t)
            select type (q=>lhs%m) ; type is (sk2_t) ; q%v = rhs%v ; end select
         class default
            call opflag%Set(IERROR, HERE, &      
                             "Hum... we shouldn't be there with lhs%typ = rhs%typ!")
            return
      end select
   else   
!
!-    allocate (or reallocate) lhs%m:
!
      if ( allocated(lhs%m) ) deallocate(lhs%m) ; allocate(lhs%m, source = rhs, stat = err) 
         
      if ( err == 0 ) then
         lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol
      else
         call opflag%Set(IERROR, HERE, 'Allocation failure')      
         return
      end if
   end if   
   
   END SUBROUTINE pk2_SetToBk2


!=============================================================================================
   SUBROUTINE pk2_SetToBk2Stat ( lhs, rhs, stat ) 
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   class(bk2_t),           intent(in    ) :: rhs
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToBk2 with "stat" (used in generic pk2_Assign)
!---------------------------------------------------------------------------------------------

   call pk2_SetToBk2 ( lhs, rhs )

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToBk2Stat')
      if ( present(stat) ) stat = opflag
   end if
      
   !if (opflag%code > 0 .and. present(stat)) stat = opflag
   
   END SUBROUTINE pk2_SetToBk2Stat


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!§ Pointer associations ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!

!=============================================================================================
   SUBROUTINE pk2_IPointer ( self, ptr, i, j, stat ) ! I => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   integer(Ikind), pointer , intent(   out) :: ptr   
   integer(Ikind), optional, intent(in    ) :: i, j      
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Returns a pointer associated to the element (1,1) (default) or (i,j) of self when the
!  container of self is of integer type
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2_IPointer'
   integer  (Ikind)              :: i_, j_
   integer                       :: err
!---------------------------------------------------------------------------------------------

   ptr => NULL() ; err = 0

   if ( present(i) .and. present(j) ) then
      i_ = i  ; j_ = j   
   else if ( .not. present(i) .and. .not. present(j) ) then
      i_ = 1  ; j_ = 1
   else 
      call stat%set ( stat=UERROR, where=HERE, msg='Two indices expected' )
      return
   end if
   
   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (ik2_t)
            if ( i_ >= 1 .and. i_ <= self%nrow .and. j_ >= 1 .and. j_ <= self%ncol ) then
               ptr => p%v(i_,j_)
            else
               err = 1
            end if
         class default
            err = 2
      end select  
   else
      err = 3
   end if
   
   if ( err /= 0 .and. present(stat) ) then
      if ( err == 1 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Element ('//util_intToChar(i_)//','//&
                        util_intToChar(j_)//') is out of bounds'   )
      else if ( err == 2 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Type mismatch: the target type is '//&
                        trim(pk2Type(self%typ))//' while the pointer type is integer' )
      else if ( err == 3 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
      end if
   end if
   
   END SUBROUTINE pk2_IPointer
   
   
!=============================================================================================
   SUBROUTINE pk2_IvecPointer ( self, ptr, stat ) ! Ivec => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   integer(Ikind), pointer , intent(   out) :: ptr(:)   
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Return a pointer associated to the matrix values of self with rank remapping when these
!  values are integers
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_IvecPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (ik2_t)
            ptr(1:size(p%v)) => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is ' //  &
                               trim(pk2Type(self%typ))//' while the pointer type is integer' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if

   END SUBROUTINE pk2_IvecPointer


!=============================================================================================
   SUBROUTINE pk2_ImatPointer ( self, ptr, stat ) ! Imat => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   integer(Ikind), pointer , intent(   out) :: ptr(:,:)   
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Return a pointer associated to the matrix values of self when these values are integers
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_ImatPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (ik2_t)
            ptr => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is '//  &
                               trim(pk2Type(self%typ))//' while the pointer type is integer' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if

   END SUBROUTINE pk2_ImatPointer


!=============================================================================================
   SUBROUTINE pk2_RPointer ( self, ptr, i, j, stat ) ! R => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   real   (Rkind), pointer , intent(   out) :: ptr   
   integer(Ikind), optional, intent(in    ) :: i, j      
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Returns a pointer associated to the element (1,1) (default) or (i,j) of self when the
!  container of self is of real type
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2_RPointer'
   integer  (Ikind)              :: i_, j_
   integer                       :: err
!---------------------------------------------------------------------------------------------

   ptr => NULL() ; err = 0

   if ( present(i) .and. present(j) ) then
      i_ = i  ; j_ = j   
   else if ( .not. present(i) .and. .not. present(j) ) then
      i_ = 1  ; j_ = 1
   else 
      call stat%set ( stat=UERROR, where=HERE, msg='Two indices expected' )
      return
   end if
   
   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (rk2_t)
            if ( i_ >= 1 .and. i_ <= self%nrow .and. j_ >= 1 .and. j_ <= self%ncol ) then
               ptr => p%v(i_,j_)
            else
               err = 1
            end if
         class default
            err = 2
      end select  
   else
      err = 3
   end if
   
   if ( err /= 0 .and. present(stat) ) then
      if ( err == 1 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Element ('//util_intToChar(i_)//','//&
                        util_intToChar(j_)//') is out of bounds'   )
      else if ( err == 2 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Type mismatch: the target type is '//&
                        trim(pk2Type(self%typ))//' while the pointer type is real' )
      else if ( err == 3 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
      end if
   end if
   
   END SUBROUTINE pk2_RPointer
      
   
!=============================================================================================
   SUBROUTINE pk2_RvecPointer ( self, ptr, stat ) ! Rvec => pk2
!=============================================================================================
   class(pk2_t), target  , intent(in    ) :: self
   real (Rkind), pointer , intent(   out) :: ptr(:)   
   type (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  (see pk2_IvecPointer)
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_RvecPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (rk2_t)
            ptr(1:size(p%v)) => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is '//  &
                               trim(pk2Type(self%typ))//' while the pointer type is real' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if   
   
   END SUBROUTINE pk2_RvecPointer


!=============================================================================================
   SUBROUTINE pk2_RmatPointer ( self, ptr, stat ) ! Rmat => pk2
!=============================================================================================
   class(pk2_t), target  , intent(in    ) :: self
   real (Rkind), pointer , intent(   out) :: ptr(:,:)   
   type (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Return a pointer associated to the matrix values of self when these values are reals
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_RmatPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (rk2_t)
            ptr => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is ' //  &
                               trim(pk2Type(self%typ))//' while the pointer type is real' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if   
   
   END SUBROUTINE pk2_RmatPointer


!=============================================================================================
   SUBROUTINE pk2_CPointer ( self, ptr, i, j, stat ) ! C => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   complex(Rkind), pointer , intent(   out) :: ptr   
   integer(Ikind), optional, intent(in    ) :: i, j      
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Returns a pointer associated to the element (1,1) (default) or (i,j) of self when the
!  container of self is of complex type
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2_CPointer'
   integer  (Ikind)              :: i_, j_
   integer                       :: err
!---------------------------------------------------------------------------------------------

   ptr => NULL() ; err = 0

   if ( present(i) .and. present(j) ) then
      i_ = i  ; j_ = j   
   else if ( .not. present(i) .and. .not. present(j) ) then
      i_ = 1  ; j_ = 1
   else 
      call stat%set ( stat=UERROR, where=HERE, msg='Two indices expected' )
      return
   end if
   
   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (ck2_t)
            if ( i_ >= 1 .and. i_ <= self%nrow .and. j_ >= 1 .and. j_ <= self%ncol ) then
               ptr => p%v(i_,j_)
            else
               err = 1
            end if
         class default
            err = 2
      end select  
   else
      err = 3
   end if
   
   if ( err /= 0 .and. present(stat) ) then
      if ( err == 1 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Element ('//util_intToChar(i_)//','//&
                        util_intToChar(j_)//') is out of bounds'   )
      else if ( err == 2 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Type mismatch: the target type is '//&
                        trim(pk2Type(self%typ))//' while the pointer type is complex' )
      else if ( err == 3 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
      end if
   end if
   
   END SUBROUTINE pk2_CPointer
   
   
!=============================================================================================
   SUBROUTINE pk2_CvecPointer ( self, ptr, stat )  ! Cvec => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   complex(Rkind), pointer , intent(   out) :: ptr(:)   
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see pk2_IvecPointer)
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_CvecPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (ck2_t)
            ptr(1:size(p%v)) => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is '//  &
                               trim(pk2Type(self%typ))//' while the pointer type is complex' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if   
   
   END SUBROUTINE pk2_CvecPointer  


!=============================================================================================
   SUBROUTINE pk2_CmatPointer ( self, ptr, stat )  ! Cmat => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   complex(Rkind), pointer , intent(   out) :: ptr(:,:)   
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Return a pointer associated to the matrix values of self when these values are complexes
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_CmatPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (ck2_t)
            ptr => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is '//  &
                               trim(pk2Type(self%typ))//' while the pointer type is complex' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if   
   
   END SUBROUTINE pk2_CmatPointer  


!=============================================================================================
   SUBROUTINE pk2_LPointer ( self, ptr, i, j, stat ) ! L => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   logical       , pointer , intent(   out) :: ptr   
   integer(Ikind), optional, intent(in    ) :: i, j      
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Returns a pointer associated to the element (1,1) (default) or (i,j) of self when the
!  container of self is of logical type
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2_LPointer'
   integer  (Ikind)              :: i_, j_
   integer                       :: err
!---------------------------------------------------------------------------------------------

   ptr => NULL() ; err = 0

   if ( present(i) .and. present(j) ) then
      i_ = i  ; j_ = j   
   else if ( .not. present(i) .and. .not. present(j) ) then
      i_ = 1  ; j_ = 1
   else 
      call stat%set ( stat=UERROR, where=HERE, msg='Two indices expected' )
      return
   end if
   
   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (lk2_t)
            if ( i_ >= 1 .and. i_ <= self%nrow .and. j_ >= 1 .and. j_ <= self%ncol ) then
               ptr => p%v(i_,j_)
            else
               err = 1
            end if
         class default
            err = 2
      end select  
   else
      err = 3
   end if
   
   if ( err /= 0 .and. present(stat) ) then
      if ( err == 1 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Element ('//util_intToChar(i_)//','//&
                        util_intToChar(j_)//') is out of bounds'   )
      else if ( err == 2 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Type mismatch: the target type is '//&
                        trim(pk2Type(self%typ))//' while the pointer type is logical' )
      else if ( err == 3 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
      end if
   end if
   
   END SUBROUTINE pk2_LPointer  


!=============================================================================================
   SUBROUTINE pk2_LvecPointer ( self, ptr, stat )  ! Lvec => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   logical       , pointer , intent(   out) :: ptr(:)   
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  (see pk2_IvecPointer)
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_LvecPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (lk2_t)
            ptr(1:size(p%v)) => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is '//  &
                               trim(pk2Type(self%typ))//' while the pointer type is logical' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if   
   
   END SUBROUTINE pk2_LvecPointer   


!=============================================================================================
   SUBROUTINE pk2_LmatPointer ( self, ptr, stat )  ! Lmat => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   logical       , pointer , intent(   out) :: ptr(:,:)   
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Return a pointer associated to the matrix values of self when these values are logicals
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_LmatPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (lk2_t)
            ptr => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is '//  &
                               trim(pk2Type(self%typ))//' while the pointer type is logical' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if   
   
   END SUBROUTINE pk2_LmatPointer   
   

!=============================================================================================
   SUBROUTINE pk2_SPointer ( self, ptr, i, j, stat ) ! S => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   type   (str_t), pointer , intent(   out) :: ptr   
   integer(Ikind), optional, intent(in    ) :: i, j      
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Returns a pointer associated to the element (1,1) (default) or (i,j) of self when the
!  container of self is of str_t type
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2_SPointer'
   integer  (Ikind)              :: i_, j_
   integer                       :: err
!---------------------------------------------------------------------------------------------

   ptr => NULL() ; err = 0

   if ( present(i) .and. present(j) ) then
      i_ = i  ; j_ = j   
   else if ( .not. present(i) .and. .not. present(j) ) then
      i_ = 1  ; j_ = 1
   else 
      call stat%set ( stat=UERROR, where=HERE, msg='Two indices expected' )
      return
   end if
   
   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (sk2_t)
            if ( i_ >= 1 .and. i_ <= self%nrow .and. j_ >= 1 .and. j_ <= self%ncol ) then
               ptr => p%v(i_,j_)
            else
               err = 1
            end if
         class default
            err = 2
      end select  
   else
      err = 3
   end if
   
   if ( err /= 0 .and. present(stat) ) then
      if ( err == 1 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Element ('//util_intToChar(i_)//','//&
                        util_intToChar(j_)//') is out of bounds'   )
      else if ( err == 2 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Type mismatch: the target type is '//&
                        trim(pk2Type(self%typ))//' while the pointer type is of str_t' )
      else if ( err == 3 ) then
         call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
      end if
   end if
   
   END SUBROUTINE pk2_SPointer 


!=============================================================================================
   SUBROUTINE pk2_SvecPointer ( self, ptr, stat )  ! Svec => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   type   (str_t), pointer , intent(   out) :: ptr(:)   
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  (see pk2_IvecPointer)
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_SvecPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (sk2_t)
            ptr(1:size(p%v)) => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is '//  &
                               trim(pk2Type(self%typ))//' while the pointer type is string' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if   
      
   END SUBROUTINE pk2_SvecPointer   
         

!=============================================================================================
   SUBROUTINE pk2_SmatPointer ( self, ptr, stat )  ! Smat => pk2
!=============================================================================================
   class  (pk2_t), target  , intent(in    ) :: self
   type   (str_t), pointer , intent(   out) :: ptr(:,:)   
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Return a pointer associated to the matrix values of self when these values are strings
!-----------------------------------------------------------------------------------R.H. 03/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_SmatPointer'
!---------------------------------------------------------------------------------------------

   ptr => NULL()

   if ( allocated(self%m) ) then
      select type ( p => self%m )
         type is (sk2_t)
            ptr => p%v
         class default
            if ( present(stat) ) &
               call stat%set ( stat = UERROR, where = HERE, &
                               msg = 'Type mismatch: the target type is '//  &
                               trim(pk2Type(self%typ))//' while the pointer type is string' )
      end select  
   else
      if ( present(stat) ) call stat%set ( stat=UERROR, where=HERE, msg='Unallocated target' )
   end if   
      
   END SUBROUTINE pk2_SmatPointer 
   
      
!=============================================================================================
   SUBROUTINE pk2_finalize ( self )
!=============================================================================================
   type(pk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------          
!
!---------------------------------------------------------------------------------------------          
        
   if ( allocated(self%m   ) ) deallocate(self%m   )
   if ( allocated(self%name) ) deallocate(self%name)
     
   end SUBROUTINE pk2_finalize


!=============================================================================================   
   FUNCTION pk2_IsSymm ( a ) result ( is_symm )
!=============================================================================================   
   class  (pk2_t), intent(in) :: a
   logical                    :: is_symm
!---------------------------------------------------------------------------------------------          
!  Returns .true. if a is symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()
        
   call a%m%Is_symm ( is_symm )
                                 
   END FUNCTION pk2_IsSymm

   
!=============================================================================================
   SUBROUTINE pk2_smldivide ( a, b, res, rcond, stat ) 
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   real (Rkind), optional, intent(in    ) :: rcond
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Computes the solution of the linear system a * c = b
!-----------------------------------------------------------------------------------R.H. 07/18       

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'pk2_smldivide'    
   integer  (Ikind)              :: err
   real     (Rkind), allocatable :: ar(:,:), br(:,:), xr(:,:)
   complex  (Rkind), allocatable :: ac(:,:), bc(:,:), xc(:,:)
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a \ b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag
      return
   end if

   err = 0
      
   select type (p=>a%m)
      type is (ik2_t)
         select type (q=>b%m)
            type is (ik2_t)
               ! a and b are integers => convert them to reals
               ar = real(p%v,kind=Rkind)      
               br = real(q%v,kind=Rkind)
               ! ar and br are overwritten by solvLin and b contains the solution:
               call util_SolvLin ( A=ar, B=br, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = br, to = res%m )

            type is (rk2_t)
               ! a is integer and b is real => convert a to real
               ar = real(p%v,kind=Rkind)
               call util_SolvLin ( A=ar, B=q%v, X=xr, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = xr, to = res%m )
               
            type is (ck2_t)
               ! a is integer and b complex => convert a to complex
               ac = cmplx(p%v,kind=Rkind)
               call util_SolvLin ( A=ac, B=q%v, X=xc, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = xc, to = res%m )

            class default 
               err = 1
         end select      

      type is (rk2_t)
         select type (q=>b%m)
            type is (ik2_t)
               ! a is real and b integer => convert b to real
               br = real(q%v,kind=Rkind)
               call util_SolvLin ( A=p%v, B=br, X=xr, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = xr, to = res%m )
               
            type is (rk2_t)
               ! a and b are reals 
               call util_SolvLin ( A=p%v, B=q%v, X=xr, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = xr, to = res%m )
               
            type is (ck2_t)
               ! a is real and b is complex => convert a to complex
               ac = cmplx(p%v,kind=Rkind)
               call util_SolvLin ( A=ac, B=q%v, X=xc, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = xc, to = res%m )
                   
            class default 
               err = 1
         end select                     

      type is (ck2_t)
         select type (q=>b%m)
            type is (ik2_t)
               ! a is complex and b integer => convert b to complex
               bc = cmplx(q%v,kind=Rkind)
               call util_SolvLin ( A=p%v, B=bc, X=xc, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = xc, to = res%m )
               
            type is (rk2_t)
               ! a is complex and b real => convert b to complex
               bc = cmplx(q%v,kind=Rkind)
               call util_SolvLin ( A=p%v, B=bc, X=xc, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = xc, to = res%m )
               
            type is (ck2_t)
               ! a and b are complexes
               call util_SolvLin ( A=p%v, B=q%v, X=xc, rcond=rcond, stat=opflag )
               call bk2_movealloc ( from = xc, to = res%m )

            class default 
               err = 1  
         end select          
                                                           
      class default
         err = 1
   end select
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace(HERE)
      if ( present(stat) ) stat = opflag
      return  
   end if
      
   if ( err == 1 ) then
      opflag =  err_t ( stat = UERROR, where = HERE, msg = &  
                        '<< a \ b >> : a and b must be integer, real or complex matrices' )
      if ( present(stat) ) stat = opflag                          
      return
   end if
      
   res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
         
   END SUBROUTINE pk2_smldivide


!=============================================================================================
   FUNCTION pk2_mldivide ( a, b ) result( c ) 
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Computes the solution of the linear system a * c = b
!-----------------------------------------------------------------------------------R.H. 07/18       

!- local variables ---------------------------------------------------------------------------           
!---------------------------------------------------------------------------------------------

   call pk2_smldivide ( a, b, c ) ; error_TraceNreturn(opflag, 'pk2_mldivide')
       
   END FUNCTION pk2_mldivide


!=============================================================================================
   SUBROUTINE pk2_i32DelRows ( self, indx, opt, stat )
!=============================================================================================
   class    (pk2_t),           intent(in out) :: self
   integer  (i32  ),           intent(in    ) :: indx(:)
   character(len=1),           intent(in    ) :: opt
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt=='r'), some columns (if opt=='c') or some elements (if opt=='e')
!  The rows or columns #s are given in indx(:)
!  If all indx are out of range "self" is inchanged
!-----------------------------------------------------------------------------------R.H. 06/18

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   call self%m%DelRows ( int(indx,Ikind), opt )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace ('pk2_i32DelRows')
      if ( present(stat) ) stat = opflag
      return
   end if
   
   self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
      
   END SUBROUTINE pk2_i32DelRows


!=============================================================================================
   SUBROUTINE pk2_i64DelRows ( self, indx, opt, stat )
!=============================================================================================
   class    (pk2_t),           intent(in out) :: self
   integer  (i64  ),           intent(in    ) :: indx(:)
   character(len=1),           intent(in    ) :: opt
   type     (err_t), optional, intent(in out) :: stat   
!--------------------------------------------------------------------------------------------- 
!  Deletes some rows (if opt=='r'), some columns (if opt=='c') or some elements (if opt=='e')
!  The rows or columns #s are given in indx(:)
!  If all indx are out of range "self" is inchanged
!-----------------------------------------------------------------------------------R.H. 06/18

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   call self%m%DelRows ( int(indx,Ikind), opt )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace ('pk2_i64DelRows')
      if ( present(stat) ) stat = opflag
      return
   end if   
   
   self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
      
   END SUBROUTINE pk2_i64DelRows
   
        
!=============================================================================================
   SUBROUTINE pk2_Destroy ( self )
!=============================================================================================
   class(pk2_t), intent(in out) :: self
!---------------------------------------------------------------------------------------------
!  Deallocates self
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   self%typ = EMPTY ; self%nrow = 0 ; self%ncol = 0
   if ( allocated(self%name) ) deallocate(self%name)
   if ( allocated(self%m   ) ) deallocate(self%m   )
   
   END SUBROUTINE pk2_Destroy
   
         
!=============================================================================================
   SUBROUTINE pk2_subEq ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Boolean condition res = a == b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'pk2_subEq'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a == b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag  
      res = pk2_t()
   else
      call bk2_Eq ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag     
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subEq


!=============================================================================================
   FUNCTION pk2_Eq ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'pk2_Eq'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a == b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_Eq ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_Eq
   

!=============================================================================================
   FUNCTION pk2_EqIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_EqIntg'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                       '<< c = a == b  >> with << a >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Eq ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)     
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_EqIntg


!=============================================================================================
   FUNCTION pk2_IntgEq ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_EqIntg ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_IntgEq')
      
   END FUNCTION pk2_IntgEq


!=============================================================================================
   FUNCTION pk2_EqReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_EqReal'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a == b  >> with << a >>  non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Eq ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)            
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_EqReal


!=============================================================================================
   FUNCTION pk2_RealEq ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_EqReal ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_RealEq')
      
   END FUNCTION pk2_RealEq


!=============================================================================================
   FUNCTION pk2_EqCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_EqCplx'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a == b  >> with << a >>  non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Eq ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)                  
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_EqCplx


!=============================================================================================
   FUNCTION pk2_CplxEq ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_EqCplx ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_CplxEq')
      
   END FUNCTION pk2_CplxEq


!=============================================================================================
   FUNCTION pk2_EqBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_EqBool'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a == b  >> with << a >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Eq ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)             
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_EqBool


!=============================================================================================
   FUNCTION pk2_BoolEq ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_EqBool ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_BoolEq')
      
   END FUNCTION pk2_BoolEq


!=============================================================================================
   FUNCTION pk2_EqStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_EqStr'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a == b  >> with << a >>  non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Eq ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)    
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_EqStr


!=============================================================================================
   FUNCTION pk2_StrEq ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_EqStr ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_StrEq')
      
   END FUNCTION pk2_StrEq


!=============================================================================================
   FUNCTION pk2_EqChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_EqChar'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a == b  >> with << a >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Eq ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_EqChar


!=============================================================================================
   FUNCTION pk2_CharEq ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a == scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_EqChar ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_CharEq')
      
   END FUNCTION pk2_CharEq


!=============================================================================================
   SUBROUTINE pk2_subNe ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Boolean condition res = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subNe'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a ~= b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag  
      res = pk2_t()
   else
      call bk2_NE ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag  
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subNe


!=============================================================================================
   FUNCTION pk2_Ne ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_Ne'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a ~= b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_NE ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_Ne
   

!=============================================================================================
   FUNCTION pk2_NeIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_NeIntg'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  ='<< c = a ~= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_NE ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_NeIntg


!=============================================================================================
   FUNCTION pk2_IntgNe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_NeIntg ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_IntgNe')
     
   END FUNCTION pk2_IntgNe


!=============================================================================================
   FUNCTION pk2_NeReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_NeReal'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a ~= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_NE ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_NeReal


!=============================================================================================
   FUNCTION pk2_RealNe ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_NeReal ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_RealNe')
     
   END FUNCTION pk2_RealNe
   

!=============================================================================================
   FUNCTION pk2_NeCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_NeCplx'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a ~= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_NE ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_NeCplx


!=============================================================================================
   FUNCTION pk2_CplxNe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_NeCplx ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_CplxNe')
     
   END FUNCTION pk2_CplxNe


!=============================================================================================
   FUNCTION pk2_NeBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_NeBool'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg = '<< c = a ~= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_NE ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_NeBool


!=============================================================================================
   FUNCTION pk2_BoolNe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_NeBool ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_BoolNe')
     
   END FUNCTION pk2_BoolNe


!=============================================================================================
   FUNCTION pk2_NeStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_NeStr'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a ~= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_NE ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_NeStr


!=============================================================================================
   FUNCTION pk2_StrNe ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_NeStr ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_StrNe')
     
   END FUNCTION pk2_StrNe


!=============================================================================================
   FUNCTION pk2_NeChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_NeChar'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg = '<< c = a ~= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_NE ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_NeChar


!=============================================================================================
   FUNCTION pk2_CharNe ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a /= b
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_NeChar ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_CharNe')
     
   END FUNCTION pk2_CharNe
      

!=============================================================================================
   SUBROUTINE pk2_subGt ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Boolean condition res = a > b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subGt'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a > b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag  
      res = pk2_t()
   else
      call bk2_GT ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag  
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subGt

      
!=============================================================================================
   FUNCTION pk2_Gt ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a > b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_Gt'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a > b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_GT ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_Gt


!=============================================================================================
   FUNCTION pk2_GtIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a > scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_GtIntg'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a > b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_GT ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_GtIntg
   
   
!=============================================================================================
   FUNCTION pk2_IntgGt ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal > a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_IntgGt'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a > b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_GT ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_IntgGt


!=============================================================================================
   FUNCTION pk2_GtReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a > scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_GtReal'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a > b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_GT ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_GtReal
   
   
!=============================================================================================
   FUNCTION pk2_RealGt ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal > a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_RealGt'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a > b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_GT ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if

   
   END FUNCTION pk2_RealGt
   
   
!=============================================================================================
   FUNCTION pk2_GtCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_GtCplx', &
                    msg = '<< c = a > b >> not defined for complexes')
   
   END FUNCTION pk2_GtCplx
   

!=============================================================================================
   FUNCTION pk2_CplxGt ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_CplxGt', &
                    msg = '<< c = a > b >> not defined for complexes')
   
   END FUNCTION pk2_CplxGt


!=============================================================================================
   FUNCTION pk2_GtBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR,  where = 'pk2_GtBool', &
                    msg = '<< c = a > b >> not defined for booleans')
   
   END FUNCTION pk2_GtBool
   

!=============================================================================================
   FUNCTION pk2_BoolGt ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_BoolGt', &
                    msg = '<< c = a > b >> not defined for booleans')
   
   END FUNCTION pk2_BoolGt


!=============================================================================================
   FUNCTION pk2_GtStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_GtStr', &
                    msg = '<< c = a > b >> not defined for strings')
   
   END FUNCTION pk2_GtStr


!=============================================================================================
   FUNCTION pk2_StrGt ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_StrGt', &
                    msg = '<< c = a > b >> not defined for strings')
   
   END FUNCTION pk2_StrGt
   
   
!=============================================================================================
   FUNCTION pk2_GtChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_GtChar', &
                    msg = '<< c = a > b >> not defined for character')
   
   END FUNCTION pk2_GtChar
   

!=============================================================================================
   FUNCTION pk2_CharGt ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_CharGt', &
                    msg = '<< c = a > b >> not defined for character')
   
   END FUNCTION pk2_CharGt


!=============================================================================================
   SUBROUTINE pk2_subGe ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Boolean condition res = a >= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subGe'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a >= b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag  
      res = pk2_t()
   else
      call bk2_GE ( a%m, b%m, res%m )      
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag  
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subGe


!=============================================================================================
   FUNCTION pk2_Ge ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a >= b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_Ge'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a >= b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_GE ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_Ge


!=============================================================================================
   FUNCTION pk2_GeIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a >= scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_GeIntg'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                        msg  = '<< c = a >= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_GE ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_GeIntg
   

!=============================================================================================
   FUNCTION pk2_IntgGe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal >= a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_IntgGe'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                        msg  = '<< c = a >= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_GE ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_IntgGe


!=============================================================================================
   FUNCTION pk2_GeReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a >= scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_GeReal'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                        msg  = '<< c = a >= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_GE ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_GeReal
   

!=============================================================================================
   FUNCTION pk2_RealGe ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal >= a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_RealGe'    
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                        msg  = '<< c = a >= b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_GE ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_RealGe


!=============================================================================================
   FUNCTION pk2_GeCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, msg = '<< c = a >= b >> not defined for complexes')
   
   END FUNCTION pk2_GeCplx
   

!=============================================================================================
   FUNCTION pk2_CplxGe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CplxGe', &
                     msg = '<< c = a >= b >> not defined for complexes')
   
   END FUNCTION pk2_CplxGe


!=============================================================================================
   FUNCTION pk2_GeBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_GeBool', &
                     msg = '<< c = a >= b >> not defined for booleans')
   
   END FUNCTION pk2_GeBool
   

!=============================================================================================
   FUNCTION pk2_BoolGe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolGe', &
                     msg = '<< c = a >= b >> not defined for booleans')
   
   END FUNCTION pk2_BoolGe


!=============================================================================================
   FUNCTION pk2_GeStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_GeStr', &
                     msg = '<< c = a >= b >> not defined for strings')
   
   END FUNCTION pk2_GeStr


!=============================================================================================
   FUNCTION pk2_StrGe ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrGe', &
                     msg = '<< c = a >= b >> not defined for strings')
   
   END FUNCTION pk2_StrGe
   
   
!=============================================================================================
   FUNCTION pk2_GeChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_GeChar', &
                    msg = '<< c = a >= b >> not defined for character')
   
   END FUNCTION pk2_GeChar
   

!=============================================================================================
   FUNCTION pk2_CharGe ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharGe', &
                    msg = '<< c = a >= b >> not defined for character')
   
   END FUNCTION pk2_CharGe
   
      
!=============================================================================================
   SUBROUTINE pk2_subLt ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Boolean condition res = a < b (<=> res = b > a)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subLt'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING , where = HERE, msg = &
                       '<< c = a < b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag  
      res = pk2_t()
   else
      call bk2_GT ( b%m, a%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag  
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subLt


!=============================================================================================
   FUNCTION pk2_Lt ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a < b (<=> c = b > a)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_Lt'    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING , where = HERE, msg = &
                       '<< c = a < b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_GT ( b%m, a%m, c%m ) ; error_TraceNreturn(opflag,HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_Lt


!=============================================================================================
   FUNCTION pk2_LtIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a < scal (<=> c = scal > a)
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_IntgGt ( scal, a ) ; error_TraceNreturn(opflag,'pk2_LtIntg')
   
   END FUNCTION pk2_LtIntg


!=============================================================================================
   FUNCTION pk2_IntgLt ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal < a (<=> c = a > scal)
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_GtIntg ( a, scal ) ; error_TraceNreturn(opflag,'pk2_IntgLt')
   
   END FUNCTION pk2_IntgLt


!=============================================================================================
   FUNCTION pk2_LtReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a < scal (<=> c = scal > a)
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_RealGt ( scal, a ) ; error_TraceNreturn(opflag,'pk2_LtReal')
   
   END FUNCTION pk2_LtReal


!=============================================================================================
   FUNCTION pk2_RealLt ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal < a (<=> c = a > scal)
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_GtReal ( a, scal ) ; error_TraceNreturn(opflag,'pk2_RealLt')
   
   END FUNCTION pk2_RealLt


!=============================================================================================
   FUNCTION pk2_LtCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_LtCplx', &
                    msg = '<< c = a < b >> not defined for complexes')
   
   END FUNCTION pk2_LtCplx
   

!=============================================================================================
   FUNCTION pk2_CplxLt ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CplxLt', &
                    msg = '<< c = a < b >> not defined for complexes')
   
   END FUNCTION pk2_CplxLt


!=============================================================================================
   FUNCTION pk2_LtBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_LtBool', &
                    msg = '<< c = a < b >> not defined for booleans')
   
   END FUNCTION pk2_LtBool
   

!=============================================================================================
   FUNCTION pk2_BoolLt ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolLt', &
                    msg = '<< c = a < b >> not defined for booleans')
   
   END FUNCTION pk2_BoolLt


!=============================================================================================
   FUNCTION pk2_LtStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_LtStr', &   
                    msg = '<< c = a < b >> not defined for strings')
   
   END FUNCTION pk2_LtStr


!=============================================================================================
   FUNCTION pk2_StrLt ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrLt', &   
                    msg = '<< c = a < b >> not defined for strings')
   
   END FUNCTION pk2_StrLt
   
   
!=============================================================================================
   FUNCTION pk2_LtChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_LtChar', &   
                    msg = '<< c = a < b >> not defined for character')
   
   END FUNCTION pk2_LtChar
   

!=============================================================================================
   FUNCTION pk2_CharLt ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharLt', &      
                    msg = '<< c = a < b >> not defined for character')
   
   END FUNCTION pk2_CharLt
   
   
!=============================================================================================
   SUBROUTINE pk2_subLe ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Boolean condition res = a <= b (<=> res = b >= a)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subLe'
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a <= b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag  
      res = pk2_t()
   else
      call bk2_GE ( b%m, a%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag  
         return 
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subLe


!=============================================================================================
   FUNCTION pk2_Le ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a <= b (<=> c = b >= a)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_Le'
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a <= b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_GE ( b%m, a%m, c%m ) ; error_TraceNreturn(opflag,HERE) 
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_Le


!=============================================================================================
   FUNCTION pk2_LeIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a <= scal (<=> c = scal >= a)
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_IntgGe ( scal, a ) ; error_TraceNreturn(opflag,'pk2_LeIntg') 
   
   END FUNCTION pk2_LeIntg


!=============================================================================================
   FUNCTION pk2_IntgLe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal <= a (<=> c = a >= scal)
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_GeIntg ( a, scal ) ; error_TraceNreturn(opflag,'pk2_IntgLe') 
   
   END FUNCTION pk2_IntgLe


!=============================================================================================
   FUNCTION pk2_LeReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a <= scal (<=> c = scal >= a)
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_RealGe ( scal, a ) ; error_TraceNreturn(opflag,'pk2_LeReal')
   
   END FUNCTION pk2_LeReal


!=============================================================================================
   FUNCTION pk2_RealLe ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal <= a (<=> c = a >= scal)
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_GeReal ( a, scal ) ; error_TraceNreturn(opflag,'pk2_RealLe')
   
   END FUNCTION pk2_RealLe


!=============================================================================================
   FUNCTION pk2_LeCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_LeCplx', &
                    msg = '<< c = a <= b >> not defined for complexes')
   
   END FUNCTION pk2_LeCplx
   

!=============================================================================================
   FUNCTION pk2_CplxLe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CplxLe', &   
                    msg = '<< c = a <= b >> not defined for complexes')
   
   END FUNCTION pk2_CplxLe


!=============================================================================================
   FUNCTION pk2_LeBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_LeBool', &      
                    msg = '<< c = a <= b >> not defined for booleans')
   
   END FUNCTION pk2_LeBool
   

!=============================================================================================
   FUNCTION pk2_BoolLe ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolLe', &         
                    msg = '<< c = a <= b >> not defined for booleans')
   
   END FUNCTION pk2_BoolLe


!=============================================================================================
   FUNCTION pk2_LeStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_LeStr', &         
                    msg = '<< c = a <= b >> not defined for strings')
   
   END FUNCTION pk2_LeStr


!=============================================================================================
   FUNCTION pk2_StrLe ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrLe', &            
                    msg = '<< c = a <= b >> not defined for strings')
   
   END FUNCTION pk2_StrLe
   
   
!=============================================================================================
   FUNCTION pk2_LeChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_LeChar', &               
                    msg = '<< c = a <= b >> not defined for characters')
   
   END FUNCTION pk2_LeChar
   

!=============================================================================================
   FUNCTION pk2_CharLe ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharLe', &               
                    msg = '<< c = a <= b >> not defined for characters')
   
   END FUNCTION pk2_CharLe


!=============================================================================================
   SUBROUTINE pk2_subAnd ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Boolean condition res = a .and. b 
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_subAnd'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .and. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a & b  >> with << a >> and << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag  
      res = pk2_t() ! res = []
     
   else if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a & b  >> with << a >> non-allocated (--> c = b)' )
      if ( present(stat) ) stat = opflag  
      call pk2_Assign (res, b) !res = b
      if ( opflag%code > 0 .and. present(stat) ) stat = opflag 
      
   else if ( b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a & b  >> with << b >> non-allocated (--> c = a)' )
      if ( present(stat) ) stat = opflag  
      call pk2_Assign (res, a) !res = a
      if ( opflag%code > 0 .and. present(stat) ) stat = opflag 
   else
      call bk2_AND ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag 
         return
      end if 
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subAnd


!=============================================================================================
   FUNCTION pk2_And ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a .and. b 
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_And'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .and. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a & b  >> with << a >> and << b >> non-allocated (--> c = [ ])')
     
   else if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a & b  >> with << a >> non-allocated (--> c = b)' )
      !c = b
      call pk2_Assign (c,b)
   else if ( b%typ == EMPTY ) then

      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a & b  >> with << b >> non-allocated (--> c = a)' )
      !c = a      
      call pk2_Assign (c,a)
   else
      call bk2_AND ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol    
   end if
   
   END FUNCTION pk2_And
   

!=============================================================================================
   FUNCTION pk2_AndBool ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   logical     , intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a .and. scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_AndBool'         
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a & b  >> with << a >> non-allocated (--> c = b)' )
      c = scal
   else
      tmp = scal
      call bk2_AND ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_AndBool


!=============================================================================================
   FUNCTION pk2_BoolAnd ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   logical     , intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal .and. b 
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_AndBool ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_BoolAnd')
   
   END FUNCTION pk2_BoolAnd
   

!=============================================================================================
   FUNCTION pk2_AndIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
 
   call opflag%set (stat = UERROR, where = 'pk2_AndIntg', &
                    msg = '<< & >> not defined for integers')
      
   END FUNCTION pk2_AndIntg


!=============================================================================================
   FUNCTION pk2_IntgAnd ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
 
   call opflag%set (stat = UERROR, where = 'pk2_IntgAnd', &
                    msg = '<< & >> not defined for integers')

   END FUNCTION pk2_IntgAnd
   

!=============================================================================================
   FUNCTION pk2_AndReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_AndReal', &
                    msg = '<< & >> not defined for reals')

   END FUNCTION pk2_AndReal


!=============================================================================================
   FUNCTION pk2_RealAnd ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_RealAnd', &
                    msg = '<< & >> not defined for reals')
   
   END FUNCTION pk2_RealAnd


!=============================================================================================
   FUNCTION pk2_AndCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_AndCplx', &
                    msg = '<< & >> not defined for complexes')
   
   END FUNCTION pk2_AndCplx


!=============================================================================================
   FUNCTION pk2_CplxAnd ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CplxAnd', &
                    msg = '<< & >> not defined for complexes')

   END FUNCTION pk2_CplxAnd


!=============================================================================================
   FUNCTION pk2_AndStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_AndStr', &
                    msg = '<< & >> not defined for strings')
   
   END FUNCTION pk2_AndStr


!=============================================================================================
   FUNCTION pk2_StrAnd ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrAnd', &
                    msg = '<< & >> not defined for strings')
   
   END FUNCTION pk2_StrAnd


!=============================================================================================
   FUNCTION pk2_AndChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_AndChar', &
                    msg = '<< & >> not defined for characters')

   END FUNCTION pk2_AndChar


!=============================================================================================
   FUNCTION pk2_CharAnd ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharAnd', &
                    msg = '<< & >> not defined for characters')

   END FUNCTION pk2_CharAnd


!=============================================================================================
   SUBROUTINE pk2_subOR ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Boolean condition res = a .or. b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2_subOR'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .and. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a | b  >> with << a >> and << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag  
      res = pk2_t() ! res = []
      
   else if ( a%typ == EMPTY ) then
      if ( b%typ == LTYP ) then
         call opflag%set (stat = WARNING, where = HERE, msg = &
                         '<< c = a | b  >> with << a >> non-allocated (--> c = b)')
         if ( present(stat) ) stat = opflag  
         call pk2_Assign (res, b) ! res = b
         if ( opflag%code > 0 ) call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
      else
         call opflag%set (stat = WARNING, where = HERE, msg = &
         '<< c = a | b  >> with << a >> non-allocated and << b >> not a boolean (--> c = [])')
         if ( present(stat) ) stat = opflag  
         res = pk2_t() ! res = []
      end if   
 
   else if ( b%typ == EMPTY ) then
      if ( a%typ == LTYP ) then
         call opflag%set (stat = WARNING, where = HERE, msg = &
                         '<< c = a | b  >> with << b >> non-allocated (--> c = a)')
         if ( present(stat) ) stat = opflag  
         call pk2_Assign (res, a) ! res = a
         if ( opflag%code > 0 ) call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
      else
         call opflag%set (stat = WARNING, where = HERE, msg = &
         '<< c = a | b  >> with << b >> non-allocated and << a >> not a boolean (--> c = [])')
         if ( present(stat) ) stat = opflag  
         res = pk2_t() ! res = []
      end if   
      
   else
      call bk2_OR ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag  
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subOR


!=============================================================================================
   FUNCTION pk2_OR ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a .or. b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2_OR'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .and. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a | b  >> with << a >> and << b >> non-allocated (--> c = [ ])')
   else if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a | b  >> with << a >> non-allocated (--> c = b)')
      !c = b
      call pk2_Assign(c,b)
   else if ( b%typ == EMPTY ) then
    
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a | b  >> with << b >> non-allocated (--> c = a)')
      !c = a    
      call pk2_Assign(c,a) ; error_TraceNreturn(opflag, HERE)
   else
      call bk2_OR ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)             
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol 
   end if
   
   END FUNCTION pk2_OR


!=============================================================================================
   FUNCTION pk2_OrBool ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   logical     , intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = a .or. scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_OrBool'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a | b  >> with << a >> non-allocated (--> c = b)')
      c = scal
   else
      tmp = scal
      call bk2_OR ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)                                   
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_OrBool


!=============================================================================================
   FUNCTION pk2_BoolOr ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   logical     , intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Boolean condition c = scal .or. b 
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_OrBool ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_BoolOr')
   
   END FUNCTION pk2_BoolOr


!=============================================================================================
   FUNCTION pk2_OrIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_OrIntg', &
                    msg = '<< | >> not defined for integers')
      
   END FUNCTION pk2_OrIntg


!=============================================================================================
   FUNCTION pk2_IntgOr ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_IntgOr', &
                    msg = '<< | >> not defined for integers')

   END FUNCTION pk2_IntgOr
   

!=============================================================================================
   FUNCTION pk2_OrReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_OrReal', &
                    msg = '<< | >> not defined for reals')
   
   END FUNCTION pk2_OrReal


!=============================================================================================
   FUNCTION pk2_RealOr ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_RealOr', &
                    msg = '<< | >> not defined for reals')

   END FUNCTION pk2_RealOr


!=============================================================================================
   FUNCTION pk2_OrCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_OrCplx', &
                    msg = '<< | >> not defined for complexes')

   END FUNCTION pk2_OrCplx


!=============================================================================================
   FUNCTION pk2_CplxOr ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CplxOr', &
                    msg = '<< | >> not defined for complexes')
   
   END FUNCTION pk2_CplxOr


!=============================================================================================
   FUNCTION pk2_OrStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_OrStr', &
                    msg = '<< | >> not defined for strings')

   END FUNCTION pk2_OrStr


!=============================================================================================
   FUNCTION pk2_StrOr ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrOr', &
                    msg = '<< | >> not defined for strings')
   
   END FUNCTION pk2_StrOr


!=============================================================================================
   FUNCTION pk2_OrChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_OrChar', &
                    msg = '<< | >> not defined for characters')
   
   END FUNCTION pk2_OrChar


!=============================================================================================
   FUNCTION pk2_CharOr ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharOr', &
                    msg = '<< | >> not defined for characters')
   
   END FUNCTION pk2_CharOr


!=============================================================================================
   SUBROUTINE pk2_subNot ( a, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Logical negation res = .not. a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_subNot'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< b = ~a  >> with << a >> non-allocated (--> b = [ ])' )
      if ( present(stat) ) stat = opflag
   else
      call bk2_NOT ( a%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if

   END SUBROUTINE pk2_subNot


!=============================================================================================
   FUNCTION pk2_Not ( a ) result( b )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (pk2_t)             :: b
!---------------------------------------------------------------------------------------------
!  Logical negation b = .not. a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_Not'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< b = ~a  >> with << a >> non-allocated (--> b = [ ])' )
   else
      call bk2_NOT ( a%m, b%m ) ; error_TraceNreturn(opflag, HERE)
      b%typ = b%m%typ ; b%nrow = b%m%nrow ; b%ncol = b%m%ncol      
   end if

   END FUNCTION pk2_Not


!=============================================================================================
   SUBROUTINE pk2_subMinus ( a, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Numeric negation: res = -a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_subMinus'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< b =-a  >> with << a >> non-allocated (--> b = [ ])' )
      if ( present(stat) ) stat = opflag
   else
      call bk2_Minus ( a%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if

   END SUBROUTINE pk2_subMinus

   
!=============================================================================================
   FUNCTION pk2_minus ( a ) result( b )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (pk2_t)             :: b
!---------------------------------------------------------------------------------------------
!  Numeric negation
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_minus'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< b =-a  >> with << a >> non-allocated (--> b = [ ])' )
   else
      call bk2_Minus ( a%m, b%m ) ; error_TraceNreturn(opflag, HERE)
      b%typ = b%m%typ ; b%nrow = b%m%nrow ; b%ncol = b%m%ncol  
   end if

   END FUNCTION pk2_minus
   

!=============================================================================================
   SUBROUTINE pk2_subSub ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Subtraction of two pk2 matrices:  res = a - b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_subSub'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .and. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a - b  >> with << a >> and << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag
      res = pk2_t()
   else if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a - b  >> with << a >> non-allocated (--> c =-b)')
      if ( present(stat) ) stat = opflag
      call bk2_Minus ( b%m, res%m ) !res = pk2_minus (b)
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      

   else if ( b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a - b  >> with << b >> non-allocated (--> c = a)')   
      if ( present(stat) ) stat = opflag
      call pk2_Assign (res, a) !res = a
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      
   else
      call bk2_Sub ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subSub


!=============================================================================================
   FUNCTION pk2_Sub ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Subtraction of two pk2 matrices
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_Sub'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .and. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a - b  >> with << a >> and << b >> non-allocated (--> c = [ ])')
   else if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a - b  >> with << a >> non-allocated (--> c =-b)')
      !c = pk2_minus (b)
      call bk2_Minus ( b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  

   else if ( b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a - b  >> with << b >> non-allocated (--> c = a)')   
      !c = a
      call pk2_Assign(c,a) ; error_TraceNreturn(opflag, HERE)
   else
      call bk2_Sub ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)      
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_Sub


!=============================================================================================
   FUNCTION pk2_SubIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Subtracts the integer "scal" from the pk2 matrix "a":  c = a - scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_SubIntg'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a - b  >> with << a >> non-allocated (--> c =-b)' )   
      c =-scal
   else
      tmp = scal
      call bk2_Sub ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE) 
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_SubIntg


!=============================================================================================
   FUNCTION pk2_IntgSub ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Subtracts the pk2 matrix "a" from the integer "scal":  c = scal - a
!-----------------------------------------------------------------------------------R.H. 05/18       

   c =-pk2_SubIntg ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_IntgSub') 
   
   END FUNCTION pk2_IntgSub


!=============================================================================================
   FUNCTION pk2_SubReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Subtracts the real "scal" from the pk2 matrix "a":  c = a - scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_SubReal'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a - b  >> with << a >> non-allocated (--> c =-b)' )
      c =-scal
   else
      tmp = scal
      call bk2_Sub ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE) 
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_SubReal


!=============================================================================================
   FUNCTION pk2_RealSub ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Subtracts the pk2 matrix "a" from the real "scal":  c = scal - a
!-----------------------------------------------------------------------------------R.H. 05/18       

   c =-pk2_SubReal ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_RealSub')
   
   END FUNCTION pk2_RealSub
   

!=============================================================================================
   FUNCTION pk2_SubCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Subtracts the complex "scal" from the pk2 matrix "a":  c = a - scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_SubCplx'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a - b  >> with << a >> non-allocated (--> c =-b)' )      
      c =-scal
   else
      tmp = scal
      call bk2_Sub ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol      
   end if
   
   END FUNCTION pk2_SubCplx


!=============================================================================================
   FUNCTION pk2_CplxSub ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Subtracts the pk2 matrix "a" from the complex "scal":  c = scal - a
!-----------------------------------------------------------------------------------R.H. 05/18       

   c =-pk2_SubCplx ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_CplxSub')
   
   END FUNCTION pk2_CplxSub
   

!=============================================================================================
   FUNCTION pk2_SubBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_SubBool', &
                    msg = '<< - >> not defined for booleans')  
   
   END FUNCTION pk2_SubBool


!=============================================================================================
   FUNCTION pk2_BoolSub ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolSub', &   
                    msg = '<< - >> not defined for booleans')  
   
   END FUNCTION pk2_BoolSub
   

!=============================================================================================
   FUNCTION pk2_SubStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_SubStr', &
                    msg = '<< - >> not defined for strings')  
   
   END FUNCTION pk2_SubStr


!=============================================================================================
   FUNCTION pk2_StrSub ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrSub', &
                    msg = '<< - >> not defined for strings')  
   
   END FUNCTION pk2_StrSub


!=============================================================================================
   FUNCTION pk2_SubChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_SubChar', &
                    msg = '<< - >> not defined for characters')  
   
   END FUNCTION pk2_SubChar


!=============================================================================================
   FUNCTION pk2_CharSub ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharSub', &
                    msg = '<< - >> not defined for characters')  
   
   END FUNCTION pk2_CharSub
   
      
!=============================================================================================
   FUNCTION pk2_Pow ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of two pk2 matrices:  c = a ** b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_Pow'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a ^ b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_Pow ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol    
   end if
   
   END FUNCTION pk2_Pow


!=============================================================================================
   SUBROUTINE pk2_subPow ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Exponentiation of two pk2 matrices:  res = a ** b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_subPow'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a ^ b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag
      res = pk2_t()
   else
      call bk2_Pow ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subPow


!=============================================================================================
   FUNCTION pk2_PowIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by an integer:  c = a ** scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_PowIntg'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                      '<< c = a ^ b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Pow ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_PowIntg


!=============================================================================================
   FUNCTION pk2_IntgPow ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of an integer by a pk2 matrix:  c = scal ** a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_IntgPow'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a ^ b  >> with << b >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Pow ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)     
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_IntgPow


!=============================================================================================
   FUNCTION pk2_PowReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a real (if possible):  c = a ** scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_PowReal'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a ^ b  >> with << a >> non-allocated (--> c = [ ])')     
   else
      tmp = scal
      call bk2_Pow ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)      
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_PowReal


!=============================================================================================
   FUNCTION pk2_RealPow ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a real by a pk2 matrix:  c = scal ** a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_RealPow'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a ^ b  >> with << b >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Pow ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)     
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol     
   end if
   
   END FUNCTION pk2_RealPow


!=============================================================================================
   FUNCTION pk2_PowCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a complex (if possible):  c = a ** scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_PowCplx'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a ^ b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Pow ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)         
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_PowCplx


!=============================================================================================
   FUNCTION pk2_CplxPow ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a complex by a pk2 matrix:  c = scal ** a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_CplxPow'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a ^ b  >> with << b >> non-allocated (--> c = [ ])')   
   else
      tmp = scal
      call bk2_Pow ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)     
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_CplxPow


!=============================================================================================
   FUNCTION pk2_PowBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a boolean --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_PowBool', &
                    msg = '<< ** >> (<< ^ >>) not defined for booleans')  
  
   END FUNCTION pk2_PowBool


!=============================================================================================
   FUNCTION pk2_BoolPow ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a boolean by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolPow', &
                    msg = '<< ** >> (<< ^ >>) not defined for booleans')  
   
   END FUNCTION pk2_BoolPow
   

!=============================================================================================
   FUNCTION pk2_PowStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_PowStr', &
                    msg = '<< ** >> (<< ^ >>) not defined for strings')  
   
   END FUNCTION pk2_PowStr


!=============================================================================================
   FUNCTION pk2_StrPow ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a string by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrPow', &
                    msg = '<< ** >> (<< ^ >>) not defined for strings')  
   
   END FUNCTION pk2_StrPow
   

!=============================================================================================
   FUNCTION pk2_PowChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a character string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_PowChar', &
                    msg = '<< ** >> (<< ^ >>) not defined for characters')  
   
   END FUNCTION pk2_PowChar


!=============================================================================================
   FUNCTION pk2_CharPow ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a character string by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharPow', &
                    msg = '<< ** >> (<< ^ >>) not defined for characters')  
   
   END FUNCTION pk2_CharPow
   

!=============================================================================================
   SUBROUTINE pk2_subEPow ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Exponentiation (element-wise) of two pk2 matrices:  res = a .^ b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_subEPow'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< c = a .^ b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
      if ( present(stat) ) stat = opflag
      res = pk2_t()
   else
      call bk2_ePow ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return  
      end if     
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subEPow

      
!=============================================================================================
   FUNCTION pk2_ePow ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation (element-wise) of two pk2 matrices:  c = a .^ b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_ePow'                              
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                     '<< c = a .^ b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_ePow ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)       
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol     
   end if
   
   END FUNCTION pk2_ePow
   

!=============================================================================================
   FUNCTION pk2_ePowIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by an integer:  c = a .^ scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_ePowIntg'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a .^ b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_ePow ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)   
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_ePowIntg


!=============================================================================================
   FUNCTION pk2_IntgEPow ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of an integer by a pk2 matrix:  c = scal .^ a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_IntgEPow'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a .^ b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_ePow ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol   
   end if
   
   END FUNCTION pk2_IntgEPow


!=============================================================================================
   FUNCTION pk2_ePowReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a real (if possible):  c = a .^ scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_ePowReal'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a .^ b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_ePow ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE) 
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol 
   end if
   
   END FUNCTION pk2_ePowReal


!=============================================================================================
   FUNCTION pk2_RealEPow ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a real by a pk2 matrix:  c = scal .^ a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_RealEPow'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a .^ b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_ePow ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol   
   end if
   
   END FUNCTION pk2_RealEPow


!=============================================================================================
   FUNCTION pk2_ePowCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a complex (if possible):  c = a .^ scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_ePowCplx'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a .^ b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_ePow ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol   
   end if
   
   END FUNCTION pk2_ePowCplx


!=============================================================================================
   FUNCTION pk2_CplxEPow ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a complex by a pk2 matrix:  c = scal .^ a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_CplxEPow'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg =  '<< c = a .^ b  >> with << b >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_ePow ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_CplxEPow


!=============================================================================================
   FUNCTION pk2_ePowBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a boolean --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_ePowBool', &
                    msg = "<< .^ >> not defined for booleans")
   
   END FUNCTION pk2_ePowBool


!=============================================================================================
   FUNCTION pk2_BoolEPow ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a boolean by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolEPow', &
                    msg = "<< .^ >> not defined for booleans")
   
   END FUNCTION pk2_BoolEPow
   

!=============================================================================================
   FUNCTION pk2_ePowStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_ePowStr', &
                     msg = "<< .^ >> not defined for strings")
   
   END FUNCTION pk2_ePowStr


!=============================================================================================
   FUNCTION pk2_StrEPow ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a string by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrEPow', &
                    msg = "<< .^ >> not defined for strings")
   
   END FUNCTION pk2_StrEPow
   

!=============================================================================================
   FUNCTION pk2_ePowChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a pk2 matrix by a character string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_ePowChar', &
                    msg = "<< .^ >> not defined for characters")
   
   END FUNCTION pk2_ePowChar


!=============================================================================================
   FUNCTION pk2_CharEPow ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Exponentiation of a character string by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharEPow', &
                    msg = "<< .^ >> not defined for characters")
   
   END FUNCTION pk2_CharEPow
   

!=============================================================================================
   SUBROUTINE pk2_subAdd ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Addition of two pk2 matrices:  res = a + b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_subAdd'                              
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%typ == EMPTY .and. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = & 
                      '<< c = a + b  >> with << a >> and << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag
      res = pk2_t()
   else if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = & 
                      '<< c = a + b  >> with << a >> non-allocated (--> c = b)')
      if ( present(stat) ) stat = opflag
      call pk2_Assign (res, b)
      if ( opflag%code > 0 .and. present(stat) ) stat = opflag
   else if ( b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = & 
                      '<< c = a + b  >> with << b >> non-allocated (--> c = a)')
      if ( present(stat) ) stat = opflag
      call pk2_Assign (res, a)
      if ( opflag%code > 0 ) call opflag%AddTrace(HERE)
      if ( present(stat) ) stat = opflag
   else
      call bk2_Add ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
      
   END SUBROUTINE pk2_subAdd

   
!=============================================================================================
   FUNCTION pk2_Add ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of two pk2 matrices:  c = a + b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter :: HERE = 'pk2_Add'                              
!---------------------------------------------------------------------------------------------   

   !call pk2_subAdd ( a, b, c )  
   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%typ == EMPTY .and. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = & 
                      '<< c = a + b  >> with << a >> and << b >> non-allocated (--> c = [ ])')
      c = pk2_t()
   else if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = & 
                      '<< c = a + b  >> with << a >> non-allocated (--> c = b)')
      call pk2_Assign(c,b)
   else if ( b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = & 
                      '<< c = a + b  >> with << b >> non-allocated (--> c = a)')
      call pk2_Assign(c,a) ; error_TraceNreturn(opflag, HERE)
   else
      call bk2_Add ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
    
      
   END FUNCTION pk2_Add


!=============================================================================================
   FUNCTION pk2_AddIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and an integer:  c = a + scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_AddIntg'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      c = scal   
      call opflag%set (stat = WARNING, where = HERE, & 
                       msg = '<< c = a + b  >> with << a >> non-allocated (--> c = b)')
   else
      tmp = scal
      call bk2_Add ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol 
   end if
   
   END FUNCTION pk2_AddIntg


!=============================================================================================
   FUNCTION pk2_IntgAdd ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and an integer:  c = scal + a
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   c = pk2_AddIntg (a, scal) ; error_TraceNreturn(opflag, 'pk2_IntgAdd')
   
   END FUNCTION pk2_IntgAdd


!=============================================================================================
   FUNCTION pk2_AddReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and a real:  c = a + scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_AddReal'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, & 
                       msg = '<< c = a + b  >> with << a >> non-allocated (--> c = b)')
      c = scal
   else
      tmp = scal
      call bk2_Add ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_AddReal

!=============================================================================================
   FUNCTION pk2_RealAdd ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and a real:  c = scal + a
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   c = pk2_AddReal (a, scal) ; error_TraceNreturn(opflag, 'pk2_RealAdd')
   
   END FUNCTION pk2_RealAdd


!=============================================================================================
   FUNCTION pk2_AddCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and a complex:  c = a + scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_AddCplx'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, & 
                       msg = '<< c = a + b  >> with << a >> non-allocated (--> c = b)')
      c = scal
   else
      tmp = scal
      call bk2_Add ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol    
   end if
   
   END FUNCTION pk2_AddCplx


!=============================================================================================
   FUNCTION pk2_CplxAdd ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and a complex:  c = scal + a
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   c = pk2_AddCplx (a, scal) ; error_TraceNreturn(opflag, 'pk2_CplxAdd')
   
   END FUNCTION pk2_CplxAdd
   

!=============================================================================================
   FUNCTION pk2_AddBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_AddBool', &
                    msg = '<< + >> not defined for booleans (use .or.)')
   
   END FUNCTION pk2_AddBool


!=============================================================================================
   FUNCTION pk2_BoolAdd ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolAdd', &
                    msg =  '<< + >> not defined for booleans (use .or.)')
   
   END FUNCTION pk2_BoolAdd
   
   
!=============================================================================================
   FUNCTION pk2_AddStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and a variable of str_t type:  c = a + scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_AddStr'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                      msg  = '<< c = a + b  >> with << a >> non-allocated (--> c = b)')
      c = scal
   else      
      tmp = scal
      call bk2_Add ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)      
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol   
   end if
   
   END FUNCTION pk2_AddStr


!=============================================================================================
   FUNCTION pk2_StrAdd ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and a variable of str_t type:  c = scal + a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_StrAdd'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                      msg  = '<< c = a + b  >> with << b >> non-allocated (--> c = a)')
      c = scal
   else   
      tmp = scal
      call bk2_Add ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_StrAdd

   
!=============================================================================================
   FUNCTION pk2_AddChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and a scalar string:  c = a + scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_AddChar'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                      msg  = '<< c = a + b  >> with << a >> non-allocated (--> c = b)')
      c = scal
   else
      tmp = scal
      call bk2_Add ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_AddChar


!=============================================================================================
   FUNCTION pk2_CharAdd ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Addition of a pk2 matrix and a scalar string:  c = scal + a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_CharAdd'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                     msg  = '<< c = a + b  >> with << b >> non-allocated (--> c = a)')
      c = scal
   else
      tmp = scal
      call bk2_Add ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_CharAdd
      

!=============================================================================================
   SUBROUTINE pk2_subMult ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Multiplication of two pk2 matrices:  res = a * b
!-----------------------------------------------------------------------------------R.H. 12/19       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subMult'                              
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                     '<< c = a * b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
      if ( present(stat) ) stat = opflag
      res = pk2_t()
   else
      call bk2_Mult ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subMult


!=============================================================================================
   FUNCTION pk2_Mult ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of two pk2 matrices:  c = a * b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_Mult'                              
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, msg = &
                     '<< c = a * b  >> with << a >> or << b >> non-allocated (--> c = [ ])')
   else
      call bk2_Mult ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_Mult


!=============================================================================================
   FUNCTION pk2_MultIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with an integer:  c = a * scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_MultIntg'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a * b  >> with << a >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Mult ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol   
   end if
   
   END FUNCTION pk2_MultIntg


!=============================================================================================
   FUNCTION pk2_IntgMult ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with an integer:  c = scal * a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_IntgMult'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg = '<< c = a * b  >> with << b >> non-allocated (--> c = [ ])')
   else
      tmp = scal
      call bk2_Mult ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol   
   end if
   
   END FUNCTION pk2_IntgMult


!=============================================================================================
   FUNCTION pk2_MultReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a real:  c = a * scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_MultReal'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a * b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Mult ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol 
   end if
   
   END FUNCTION pk2_MultReal


!=============================================================================================
   FUNCTION pk2_RealMult ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a real:  c = scal * a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_RealMult'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a * b  >> with << b >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Mult ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol 
   end if
   
   END FUNCTION pk2_RealMult


!=============================================================================================
   FUNCTION pk2_MultCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a complex:  c = a * scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_MultCplx'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  = '<< c = a * b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Mult ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol 
   end if
   
   END FUNCTION pk2_MultCplx
   

!=============================================================================================
   FUNCTION pk2_CplxMult ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a complex:  c = scal * a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_CplxMult'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set (stat = WARNING, where = HERE, &
                       msg  =  '<< c = a * b  >> with << a >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Mult ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_CplxMult


!=============================================================================================
   FUNCTION pk2_MultBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a boolean --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_MultBool', &
                    msg = '<<  *  >> not defined for booleans (use .and.)')
   
   END FUNCTION pk2_MultBool


!=============================================================================================
   FUNCTION pk2_BoolMult ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a boolean --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolMult', &
                    msg = '<<  *  >> not defined for booleans (use .and.)')
   
   END FUNCTION pk2_BoolMult


!=============================================================================================
   FUNCTION pk2_MultStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a str_t type variable --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_MultStr', &
                    msg = '<<  *  >> not defined for strings')
   
   END FUNCTION pk2_MultStr


!=============================================================================================
   FUNCTION pk2_StrMult ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a str_t type variable --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrMult', &
                    msg = '<<  *  >> not defined for strings')
   
   END FUNCTION pk2_StrMult


!=============================================================================================
   FUNCTION pk2_MultChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a character string --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_MultChar', &
                    msg = '<<  *  >> not defined for characters')
   
   END FUNCTION pk2_MultChar


!=============================================================================================
   FUNCTION pk2_CharMult ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a character string --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharMult', &
                    msg = '<<  *  >> not defined for characters')
   
   END FUNCTION pk2_CharMult


!=============================================================================================
   SUBROUTINE pk2_subDiv ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Divide of two pk2 matrices:  res = a / b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subDiv'                              
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< c = a / b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
      if ( present(stat) ) stat = opflag
      res = pk2_t()
   else
      call bk2_Div ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subDiv

   
!=============================================================================================
   FUNCTION pk2_Div ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of two pk2 matrices:  c = a / b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_Div'                              
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< c = a / b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
   else
      call bk2_Div ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_Div


!=============================================================================================
   FUNCTION pk2_DivIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by an integer:  c = a / scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_DivIntg'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a / b  >> with << a >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Div ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol    
   end if
   
   END FUNCTION pk2_DivIntg


!=============================================================================================
   FUNCTION pk2_IntgDiv ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of an integer by a pk2 matrix:  c = scal / a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_IntgDiv'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a / b  >> with << b >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Div ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol 
   end if
   
   END FUNCTION pk2_IntgDiv


!=============================================================================================
   FUNCTION pk2_DivReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a real:  c = a / scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_DivReal'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a / b  >> with << a >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Div ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_DivReal


!=============================================================================================
   FUNCTION pk2_RealDiv ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a real by a pk2 matrix:  c = scal / a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_RealDiv'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a / b  >> with << b >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Div ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol 
   end if
   
   END FUNCTION pk2_RealDiv


!=============================================================================================
   FUNCTION pk2_DivCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a complex:  c = a / scal
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_DivCplx'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a / b  >> with << a >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Div ( a%m, tmp%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_DivCplx


!=============================================================================================
   FUNCTION pk2_CplxDiv ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a complex:  c = scal / a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_CplxDiv'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                       msg  = '<< c = a / b  >> with << a >>  non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_Div ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol  
   end if
   
   END FUNCTION pk2_CplxDiv
   

!=============================================================================================
   FUNCTION pk2_DivBool ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   logical     , intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a boolean --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_DivBool', &
                    msg = '<<  /  >> not defined for booleans')
   
   END FUNCTION pk2_DivBool


!=============================================================================================
   FUNCTION pk2_BoolDiv ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   logical     , intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a boolean by a pk2 matrix --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_BoolDiv', &   
                    msg = '<<  /  >> not defined for booleans')
   
   END FUNCTION pk2_BoolDiv


!=============================================================================================
   FUNCTION pk2_DivStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a character string --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = WARNING, where = 'pk2_DivStr', &   
                    msg = '<<  /  >> not defined for strings')
   
   END FUNCTION pk2_DivStr


!=============================================================================================
   FUNCTION pk2_StrDiv ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a character string --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = WARNING, where = 'pk2_StrDiv', &   
                    msg = '<<  /  >> not defined for strings')
   
   END FUNCTION pk2_StrDiv


!=============================================================================================
   FUNCTION pk2_DivChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a character string --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_DivChar', &   
                    msg = '<<  /  >> not defined for characters')
   
   END FUNCTION pk2_DivChar


!=============================================================================================
   FUNCTION pk2_CharDiv ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a character string --> error report
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_CharDiv', &   
                    msg = '<<  /  >> not defined for characters')
   
   END FUNCTION pk2_CharDiv


!=============================================================================================
   SUBROUTINE pk2_subEDiv ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Element-wise divide of two pk2 matrices:  res = a ./ b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subEDiv'                              
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< c = a ./ b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
      if ( present(stat) ) stat = opflag
      res = pk2_t()
   else
      call bk2_eDiv ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subEDiv


!=============================================================================================
   FUNCTION pk2_eDiv ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Element-wise divide of two pk2 matrices:  c = a ./ b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_eEDiv'                              
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< c = a ./ b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
   else
      call bk2_eDiv ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_eDiv
   

!=============================================================================================
   FUNCTION pk2_eDivIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by an integer:  c = a ./ scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_DivIntg ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_eDivIntg')
   
   END FUNCTION pk2_eDivIntg


!=============================================================================================
   FUNCTION pk2_IntgEdiv ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of an integer by a pk2 matrix (if possible):  c = scal ./ a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subEDiv'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< c = a ./ b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_eDiv ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_IntgEdiv


!=============================================================================================
   FUNCTION pk2_eDivReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a real:  c = a ./ scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_DivReal ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_eDivReal')
   
   END FUNCTION pk2_eDivReal


!=============================================================================================
   FUNCTION pk2_RealEdiv ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a real by a pk2 matrix (if possible):  c = scal ./ a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_RealEdiv'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                       '<< c = a ./ b  >> with << b >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_eDiv ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_RealEdiv
   

!=============================================================================================
   FUNCTION pk2_eDivCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a complex:  c = a ./ scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_DivCplx ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_eDivCplx')
   
   END FUNCTION pk2_eDivCplx


!=============================================================================================
   FUNCTION pk2_CplxEdiv ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a complex by a pk2 matrix (if possible):  c = scal ./ a
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_CplxEdiv'                              
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                       '<< c = a ./ b  >> with << b >> non-allocated (--> c = [ ])' )
   else
      tmp = scal
      call bk2_eDiv ( tmp%m, a%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_CplxEdiv
  

!=============================================================================================
   FUNCTION pk2_eDivBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a boolean --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_eDivBool', &
                    msg = '<< ./ >> not defined for booleans')

   END FUNCTION pk2_eDivBool


!=============================================================================================
   FUNCTION pk2_BoolEdiv ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a boolean by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_BoolEdiv', &
                    msg = '<< ./ >> not defined for booleans')
      
   END FUNCTION pk2_BoolEdiv


!=============================================================================================
   FUNCTION pk2_eDivStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_eDivStr', &
                    msg = '<< ./ >> not defined for strings')
      
   END FUNCTION pk2_eDivStr


!=============================================================================================
   FUNCTION pk2_StrEdiv ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a string by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_StrEdiv', &
                    msg = '<< ./ >> not defined for strings')
      
   END FUNCTION pk2_StrEdiv
   

!=============================================================================================
   FUNCTION pk2_eDivChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a pk2 matrix by a character string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_eDivChar', &
                    msg = '<< ./ >> not defined for characters')
      
   END FUNCTION pk2_eDivChar


!=============================================================================================
   FUNCTION pk2_CharEdiv ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Divide of a character string by a pk2 matrix --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = WARNING, where = 'pk2_CharEdiv', &
                    msg = '<< ./ >> not defined for characters')
      
   END FUNCTION pk2_CharEdiv
   
      
!=============================================================================================
   FUNCTION pk2_eMult ( a, b ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Element-wise multiplication of two pk2 matrices:  c = a .* b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_eMult'                              
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< c = a .* b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
   else
      call bk2_eMult ( a%m, b%m, c%m ) ; error_TraceNreturn(opflag, HERE)
      c%typ = c%m%typ ; c%nrow = c%m%nrow ; c%ncol = c%m%ncol
   end if
   
   END FUNCTION pk2_eMult


!=============================================================================================
   SUBROUTINE pk2_subEMult ( a, b, res, stat )
!=============================================================================================
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Element-wise multiplication of two pk2 matrices:  res = a .* b
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = 'pk2_subEMult'                              
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. b%typ == EMPTY ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< c = a .* b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
      if ( present(stat) ) stat = opflag
      res = pk2_t()
   else
      call bk2_eMult ( a%m, b%m, res%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
         return
      end if
      res%typ = res%m%typ ; res%nrow = res%m%nrow ; res%ncol = res%m%ncol      
   end if
   
   END SUBROUTINE pk2_subEMult


!=============================================================================================
   FUNCTION pk2_eMultIntg ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with an integer:  c = a .* scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_MultIntg ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_eMultIntg')
   
   END FUNCTION pk2_eMultIntg


!=============================================================================================
   FUNCTION pk2_IntgEmult ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with an integer:  c = scal .* a
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_MultIntg ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_IntgEmult')
   
   END FUNCTION pk2_IntgEmult
   
 
!=============================================================================================
   FUNCTION pk2_eMultReal ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a real:  c = a .* scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_MultReal ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_eMultReal')
      
   END FUNCTION pk2_eMultReal


!=============================================================================================
   FUNCTION pk2_RealEmult ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   Real (Rkind), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a real:  c = scal .* a
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_MultReal ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_RealEmult')
      
   END FUNCTION pk2_RealEmult


!=============================================================================================
   FUNCTION pk2_eMultCplx ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a complex:  c = a .* scal
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_MultCplx ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_eMultCplx')
      
   END FUNCTION pk2_eMultCplx


!=============================================================================================
   FUNCTION pk2_CplxEmult ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   complex(Rkind), intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a complex:  c = scal .* a
!-----------------------------------------------------------------------------------R.H. 05/18       

   c = pk2_MultCplx ( a, scal ) ; error_TraceNreturn(opflag, 'pk2_CplxEmult')
      
   END FUNCTION pk2_CplxEmult


!=============================================================================================
   FUNCTION pk2_eMultBool ( a, scal ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a boolean --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       
   
   call opflag%set (stat = UERROR, where = 'pk2_eMultBool', &
                    msg = '<< .* >> not defined for booleans')
      
   END FUNCTION pk2_eMultBool


!=============================================================================================
   FUNCTION pk2_BoolEmult ( scal, a ) result( c )
!=============================================================================================
   class  (pk2_t), intent(in) :: a
   logical       , intent(in) :: scal
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a boolean --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_BoolEmult', &
                    msg = '<< .* >> not defined for booleans')
         
   END FUNCTION pk2_BoolEmult


!=============================================================================================
   FUNCTION pk2_eMultStr ( a, scal ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_eMultStr', &
                    msg = '<< .* >> not defined for strings')
  
   END FUNCTION pk2_eMultStr


!=============================================================================================
   FUNCTION pk2_StrEmult ( scal, a ) result( c )
!=============================================================================================
   class(pk2_t), intent(in) :: a
   type (str_t), intent(in) :: scal
   type (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_StrEmult', &
                    msg = '<< .* >> not defined for strings')
      
   END FUNCTION pk2_StrEmult


!=============================================================================================
   FUNCTION pk2_eMultChar ( a, scal ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a character string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_eMultChar', &
                    msg = '<< .* >> not defined for characters')
      
   END FUNCTION pk2_eMultChar


!=============================================================================================
   FUNCTION pk2_CharEmult ( scal, a ) result( c )
!=============================================================================================
   class    (pk2_t), intent(in) :: a
   character(len=*), intent(in) :: scal
   type     (pk2_t)             :: c
!---------------------------------------------------------------------------------------------
!  Multiplication of a pk2 matrix with a character string --> report error
!-----------------------------------------------------------------------------------R.H. 05/18       

   call opflag%set (stat = UERROR, where = 'pk2_CharEmult', &
                    msg = '<< .* >> not defined for characters')
      
   END FUNCTION pk2_CharEmult

   
!=============================================================================================   
   FUNCTION pk2_GetMat ( self, shape ) result ( mat )
!=============================================================================================   
   class  (pk2_t),             intent(in) :: self
   integer(Ikind), optional,   intent(in) :: shape(2) 
   class  (bk2_t), allocatable            :: mat
!---------------------------------------------------------------------------------------------  
!  Returns the bk2 matrix member of "self". Optionaly reshape it acording to "shape" 
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m))  then
      mat = self%m

      if ( present(shape) .and. mat%typ /= EMPTY ) then
         select type(mat)
            type is (ik2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)
            type is (rk2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)            
            type is (ck2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)            
            type is (lk2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)            
            type is (sk2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)            
         end select
      end if
   else
      mat = bk2_t()   
   end if   
            
   END FUNCTION pk2_GetMat


!=============================================================================================   
   SUBROUTINE pk2_GetMatBk2 ( self, mat, shape, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   class  (bk2_t), allocatable, intent(   out) :: mat
   integer(Ikind), optional,    intent(in    ) :: shape(2)    
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Returns the bk2 matrix member of "self". Optionaly reshape it according to "shape" 
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      mat = self%m

      if ( present(shape) .and. mat%typ /= EMPTY ) then
         select type(mat)
            type is (ik2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)
            type is (rk2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)            
            type is (ck2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)            
            type is (lk2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)            
            type is (sk2_t)   
               mat%v    = reshape(mat%v,shape)
               mat%nrow = size(mat%v,dim=1)
               mat%ncol = size(mat%v,dim=2)            
         end select
      end if
   else
      mat = bk2_t()   
   end if   
            
   END subroutine pk2_GetMatbk2


!=============================================================================================   
   SUBROUTINE pk2_GetMatI ( self, mat, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   integer(Ikind), allocatable, intent(in out) :: mat(:,:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies "self%m%v" into the integer array "mat", if appropriate (i.e. self%m%v can be 
!  integer or logical)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatI'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (ik2_t) ; mat = p%v
         type is (lk2_t) ; mat = merge(tsource = IONE, fsource = IZERO, mask = p%v)
         class default
            call opflag%set (stat = UERROR, where = HERE, &
                             msg = 'An integer or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(mat) ) deallocate(mat)   
   end if
         
   END SUBROUTINE pk2_GetMatI


!=============================================================================================   
   SUBROUTINE pk2_fromPk2ToI32 ( lhs, rhs )
!=============================================================================================   
   integer( i32 ), intent(in out) :: lhs
   class  (pk2_t), intent(in    ) :: rhs
!--------------------------------------------------------------------------------------------- 
! 
!-----------------------------------------------------------------------------------R.H. 10/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_fromPk2ToI32'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(rhs%m) .and. rhs%nrow >=1 .and. rhs%ncol >= 1 ) then
      select type ( p=>rhs%m )
         type is (ik2_t)
            lhs = int(p%v(1,1),kind=i32)
         type is (rk2_t)
            lhs = int(p%v(1,1),kind=i32)
         type is (ck2_t)
            lhs = int(p%v(1,1),kind=i32)
         type is (lk2_t)
            if ( p%v(1,1) ) then
               lhs = 1_i32
            else
               lhs = 0_i32
            end if
         type is (sk2_t)
            call opflag%set ( stat = UERROR, where = HERE, &
                              msg = 'Unable to convert a string to an integer')
      end select
   else
      call opflag%set ( stat = UERROR, where = HERE, &
                              msg = 'Unable to convert an empty array to an integer')
   end if
         
   END SUBROUTINE pk2_fromPk2ToI32   


!=============================================================================================   
   SUBROUTINE pk2_fromPk2ToI64 ( lhs, rhs )
!=============================================================================================   
   integer( i64 ), intent(in out) :: lhs
   class  (pk2_t), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------  
!-----------------------------------------------------------------------------------R.H. 10/23       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_fromPk2ToI64'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(rhs%m) .and. rhs%nrow >=1 .and. rhs%ncol >= 1 ) then
      select type ( p=>rhs%m )
         type is (ik2_t)
            lhs = int(p%v(1,1),kind=i64)
         type is (rk2_t)
            lhs = int(p%v(1,1),kind=i64)
         type is (ck2_t)
            lhs = int(p%v(1,1),kind=i64)
         type is (lk2_t)
            if ( p%v(1,1) ) then
               lhs = 1_i64
            else
               lhs = 0_i64
            end if
         type is (sk2_t)
            call opflag%set ( stat = UERROR, where = HERE, &
                              msg = 'Unable to convert a string to an integer')
      end select
   else
      call opflag%set ( stat = UERROR, where = HERE, &
                              msg = 'Unable to convert an empty array to an integer')
   end if
         
   END SUBROUTINE pk2_fromPk2ToI64   
   

!=============================================================================================   
   SUBROUTINE pk2_GetMatR ( self, mat, stat )
!=============================================================================================   
   class(pk2_t),              intent(in    ) :: self
   real (Rkind), allocatable, intent(in out) :: mat(:,:)
   type (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies "self%m%v" into the real array "mat, if appropriate (i.e. self%m%v can be real,
!  integer or logical) 
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatR'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (ik2_t) ; mat = real(p%v,kind=Rkind)
         type is (rk2_t) ; mat = p%v
         type is (lk2_t) ; mat = merge(tsource = RONE, fsource = RZERO, mask = p%v)
         class default
            call opflag%set (stat = UERROR, where = HERE, &
                             msg = 'A real, an integer or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(mat) ) deallocate(mat)   
   end if
         
   END SUBROUTINE pk2_GetMatR


!=============================================================================================   
   SUBROUTINE pk2_GetMatC ( self, mat, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   complex(Rkind), allocatable, intent(in out) :: mat(:,:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies "self%m%v" into the complex array "mat, if appropriate (i.e. self%m%v can be complex
!  real, integer or logical) 
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatC'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (ik2_t) ; mat = cmplx(p%v,kind=Rkind)
         type is (rk2_t) ; mat = cmplx(p%v,kind=Rkind)
         type is (ck2_t) ; mat = p%v
         type is (lk2_t) ; mat = merge(tsource = CONE, fsource = CZERO, mask = p%v)
         class default
            call opflag%set (stat = UERROR, where = HERE, &
                     msg = 'A complex, a real, an integer  or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(mat) ) deallocate(mat)   
   end if
         
   END SUBROUTINE pk2_GetMatC


!=============================================================================================   
   SUBROUTINE pk2_GetMatL ( self, mat, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   logical       , allocatable, intent(in out) :: mat(:,:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies, if appropriate, "self%m%v" into the logical array "mat". 
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatL'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (lk2_t) ; mat = p%v
         class default
            call opflag%set (stat = UERROR, where = HERE, msg = 'A logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(mat) ) deallocate(mat)   
   end if
         
   END SUBROUTINE pk2_GetMatL


!=============================================================================================   
   SUBROUTINE pk2_GetMatS ( self, mat, stat )
!=============================================================================================   
   class(pk2_t),              intent(in    ) :: self
   type (str_t), allocatable, intent(in out) :: mat(:,:)
   type (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies, if appropriate, "self%m%v" into the string array "mat". 
!  Note: "mat" is not reallocated if already allocated with the same shape as "self%m%v"
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatS'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (sk2_t) ; ! mat = p%v ! pb with gfortran
            if ( allocated(mat) ) then
               if ( size(mat,1) /= p%nrow .or. size(mat,2) /= p%ncol ) deallocate(mat)
            end if
            if ( .not. allocated(mat) ) allocate(mat(p%nrow,p%ncol))
            mat = p%v
         class default
            call opflag%set (stat = UERROR, where = HERE, msg = 'A string pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(mat) ) deallocate(mat)   
   end if
         
   END SUBROUTINE pk2_GetMatS


!=============================================================================================   
   SUBROUTINE pk2_GetMatPackedI ( self, vec, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   integer(Ikind), allocatable, intent(in out) :: vec(:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies the elements of "self%m%v" into the rank-1 integer array "vec", if appropriate (i.e.
!  self%m%v can be integer or logical) 
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatPackedI'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (ik2_t) ; vec = pack(p%v,mask=.true.)
         type is (lk2_t) ; vec = pack( merge(tsource = IONE, fsource = IZERO, mask = p%v), &
                                       mask=.true. )
         class default
            call opflag%set(stat = UERROR, where = HERE, &
                            msg = 'An integer or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(vec) ) deallocate(vec)   
   end if
         
   END SUBROUTINE pk2_GetMatPackedI


!=============================================================================================   
   SUBROUTINE pk2_GetMatPackedR ( self, vec, stat )
!=============================================================================================   
   class(pk2_t),              intent(in    ) :: self
   real (Rkind), allocatable, intent(in out) :: vec(:)
   type (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies the elements of "self%m%v" into the rank-1 real array "vec", if appropriate (i.e.
!  self%m%v can be real, integer or logical)
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatPackedR'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (ik2_t) ; vec = real(pack(p%v,mask=.true.),kind=Rkind)
         type is (rk2_t) ; vec = pack(p%v,mask=.true.)
         type is (lk2_t) ; vec = pack( merge(tsource = RONE, fsource = RZERO, mask = p%v), &
                                       mask=.true. )
         class default
            call opflag%set (stat = UERROR, where = HERE, &
                             msg = 'A real, an integer or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(vec) ) deallocate(vec)   
   end if
         
   END SUBROUTINE pk2_GetMatPackedR


!=============================================================================================   
   SUBROUTINE pk2_GetMatPackedC ( self, vec, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   complex(Rkind), allocatable, intent(in out) :: vec(:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies the elements of "self%m%v" into the rank-1 complex array "vec", if appropriate (i.e.
!  self%m%v can be complex, real, integer or logical)
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatPackedC'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (ik2_t) ; vec = cmplx(pack(p%v,mask=.true.),kind=Rkind)
         type is (rk2_t) ; vec = cmplx(pack(p%v,mask=.true.),kind=Rkind)
         type is (ck2_t) ; vec = pack(p%v,mask=.true.)
         type is (lk2_t) ; vec = pack( merge(tsource = CONE, fsource = CZERO, mask = p%v), &
                                       mask=.true. )
         class default 
            call opflag%set (stat = UERROR, where = HERE, &
                     msg = 'A complex, a real, an integer or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(vec) ) deallocate(vec)   
   end if
         
   END SUBROUTINE pk2_GetMatPackedC


!=============================================================================================   
   SUBROUTINE pk2_GetMatPackedL ( self, vec, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   logical       , allocatable, intent(in out) :: vec(:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies, if appropriate, the elements  of"self%m%v" into the rank-1 logical array "vec". 
!  Note: "vec" is not reallocated if already allocated with the good size.
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatPackedL'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (lk2_t) ; vec = pack(p%v,mask=.true.)
         class default
            call opflag%set (stat = UERROR, where = HERE, msg = 'A logical pk2 array expected')
            if (present(stat)) stat = opflag
      end select
   else
      if ( allocated(vec) ) deallocate(vec)   
   end if
         
   END SUBROUTINE pk2_GetMatPackedL


!=============================================================================================   
   SUBROUTINE pk2_GetMatPackedS ( self, vec, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   type   (str_t), allocatable, intent(in out) :: vec(:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies, if appropriate, the elements  of"self%m%v" into the rank-1 string array "vec". 
!  Note: "vec" is not reallocated if already allocated with the good size.
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatPackedS'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
      select type (p=>self%m)
         type is (sk2_t) ; !vec = pack(p%v,mask=.true.) ! pb with gfortran
            if ( allocated(vec) ) then
               if ( size(vec) /= p%nrow * p%ncol ) deallocate(vec)
            end if
            if ( .not. allocated(vec) ) allocate(vec(p%nrow*p%ncol))
            vec = pack(p%v,mask=.true.)
         class default
            call opflag%set (stat = UERROR, where = HERE, msg = 'A string pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select
   else
      if ( allocated(vec) ) deallocate(vec)   
   end if
         
   END SUBROUTINE pk2_GetMatPackedS


!=============================================================================================   
   SUBROUTINE pk2_GetMatijI ( self, i, j, mij, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   integer(Ikind),              intent(in    ) :: i, j
   integer(Ikind), allocatable, intent(in out) :: mij
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies, the element self%m%v(i,j) into the integer scalar "mij", if appropriate (i.e.
!  self%m%v must be integer)
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatijI'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
   
      if ( i < 0 .or. i > self%nrow .or. j < 0 .or. j > self%ncol ) then
         call opflag%set(stat = UERROR, where = HERE, msg = 'Index out of bounds (i='  //    &
                      util_intToChar(i)//' and j='//util_intToChar(j)//' while nrow='  //    &
                      util_intToChar(self%nrow)//' and ncol='//util_intToChar(self%ncol)//')')
         if ( present(stat) ) stat = opflag
         return
      end if
         
      select type (p=>self%m)
         type is (ik2_t) ; mij = p%v(i,j)
         type is (lk2_t) ; mij = merge(tsource = IONE, fsource = IZERO, mask = p%v(i,j))
         class default
            call opflag%set(stat = UERROR, where = HERE, &
                            msg = 'An integer or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select  
   else
      if ( allocated(mij) ) deallocate(mij)
   end if
         
   END SUBROUTINE pk2_GetMatijI


!=============================================================================================   
   SUBROUTINE pk2_GetMatijR ( self, i, j, mij, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   integer(Ikind),              intent(in    ) :: i, j
   real   (Rkind), allocatable, intent(in out) :: mij
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies the element self%m%v(i,j) into the real scalar "mij", if appropriate (i.e. self%m%v
!  can be real, integer or logical)
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatijR'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then

      if ( i < 0 .or. i > self%nrow .or. j < 0 .or. j > self%ncol ) then
         call opflag%set(stat = UERROR, where = HERE, msg = 'Index out of bounds (i='  //    &
                      util_intToChar(i)//' and j='//util_intToChar(j)//' while nrow='  //    &
                      util_intToChar(self%nrow)//' and ncol='//util_intToChar(self%ncol)//')')
         if ( present(stat) ) stat = opflag
         return
      end if
      
      select type (p=>self%m)
         type is (ik2_t) ; mij = real(p%v(i,j),kind=Rkind)
         type is (rk2_t) ; mij = p%v(i,j)
         type is (lk2_t) ; mij = merge(tsource = RONE, fsource = RZERO, mask = p%v(i,j))
         class default
            call opflag%set(stat = UERROR, where = HERE, &
                            msg = 'A real, an integer or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select  
   else
      if ( allocated(mij) ) deallocate(mij)
   end if
         
   END SUBROUTINE pk2_GetMatijR


!=============================================================================================   
   SUBROUTINE pk2_GetMatijC ( self, i, j, mij, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   integer(Ikind),              intent(in    ) :: i, j
   complex(Rkind), allocatable, intent(in out) :: mij
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies the element self%m%v(i,j) into the complex scalar "mij", if appropriate (i.e. 
!  self%m%v can be complex, real, integer or logical)
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatijC'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then
   
      if ( i < 0 .or. i > self%nrow .or. j < 0 .or. j > self%ncol ) then
         call opflag%set(stat = UERROR, where = HERE, msg = 'Index out of bounds (i='  //    &
                      util_intToChar(i)//' and j='//util_intToChar(j)//' while nrow='  //    &
                      util_intToChar(self%nrow)//' and ncol='//util_intToChar(self%ncol)//')')
         if ( present(stat) ) stat = opflag
         return
      end if
      
      select type (p=>self%m)
         type is (ik2_t) ; mij = cmplx(p%v(i,j),kind=Rkind)
         type is (rk2_t) ; mij = cmplx(p%v(i,j),kind=Rkind)
         type is (ck2_t) ; mij = p%v(i,j)
         type is (lk2_t) ; mij = merge(tsource = CONE, fsource = CZERO, mask = p%v(i,j))
         class default
            call opflag%set(stat = UERROR, where = HERE, &
                     msg = 'A complex, a real, an integer or a logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select  
   else
      if ( allocated(mij) ) deallocate(mij)
   end if
         
   END SUBROUTINE pk2_GetMatijC


!=============================================================================================   
   SUBROUTINE pk2_GetMatijL ( self, i, j, mij, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   integer(Ikind),              intent(in    ) :: i, j
   logical       , allocatable, intent(in out) :: mij
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies, the element self%m%v(i,j) into the logical scalar "mij", if appropriate (i.e.
!  self%m%v must be logical)
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatijL'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then

      if ( i < 0 .or. i > self%nrow .or. j < 0 .or. j > self%ncol ) then
         call opflag%set(stat = UERROR, where = HERE, msg = 'Index out of bounds (i='  //    &
                      util_intToChar(i)//' and j='//util_intToChar(j)//' while nrow='  //    &
                      util_intToChar(self%nrow)//' and ncol='//util_intToChar(self%ncol)//')')
         if ( present(stat) ) stat = opflag
         return
      end if
      
      select type (p=>self%m)
         type is (lk2_t) ; mij = p%v(i,j)
         class default
            call opflag%set(stat = UERROR, where = HERE, msg = 'A logical pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select  
   else
      if ( allocated(mij) ) deallocate(mij)
   end if
         
   END SUBROUTINE pk2_GetMatijL


!=============================================================================================   
   SUBROUTINE pk2_GetMatijS ( self, i, j, mij, stat )
!=============================================================================================   
   class  (pk2_t),              intent(in    ) :: self
   integer(Ikind),              intent(in    ) :: i, j
   type   (str_t), allocatable, intent(in out) :: mij
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies, the element self%m%v(i,j) into the str_t scalar "mij", if appropriate (i.e.
!  self%m%v must be str_t)
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatijS'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then

      if ( i < 0 .or. i > self%nrow .or. j < 0 .or. j > self%ncol ) then
         call opflag%set(stat = UERROR, where = HERE, msg = 'Index out of bounds (i='  //    &
                      util_intToChar(i)//' and j='//util_intToChar(j)//' while nrow='  //    &
                      util_intToChar(self%nrow)//' and ncol='//util_intToChar(self%ncol)//')')
         if ( present(stat) ) stat = opflag
         return
      end if
      
      select type (p=>self%m)
         type is (sk2_t)
            if ( .not. allocated(mij) ) allocate(mij)
            mij%str = p%v(i,j)%str
         class default
            call opflag%set(stat = UERROR, where = HERE, msg = 'A str_t pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select  
   else
      if ( allocated(mij) ) deallocate(mij)
   end if
         
   END SUBROUTINE pk2_GetMatijS   


!=============================================================================================   
   SUBROUTINE pk2_GetMatijCh ( self, i, j, mij, stat )
!=============================================================================================   
   class    (pk2_t),              intent(in    ) :: self
   integer  (Ikind),              intent(in    ) :: i, j
   character(len=:), allocatable, intent(in out) :: mij
   type     (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------  
!  Copies, the element self%m%v(i,j) into the character scalar "mij", if appropriate (i.e.
!  self%m%v must be str_t)
!-----------------------------------------------------------------------------------R.H. 07/20       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetMatijCh'         
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( allocated(self%m) ) then

      if ( i < 0 .or. i > self%nrow .or. j < 0 .or. j > self%ncol ) then
         call opflag%set(stat = UERROR, where = HERE, msg = 'Index out of bounds (i='  //    &
                      util_intToChar(i)//' and j='//util_intToChar(j)//' while nrow='  //    &
                      util_intToChar(self%nrow)//' and ncol='//util_intToChar(self%ncol)//')')
         if ( present(stat) ) stat = opflag
         return
      end if
      
      select type (p=>self%m)
         type is (sk2_t)
            mij = p%v(i,j)%str
         class default
            call opflag%set(stat = UERROR, where = HERE, msg = 'A str_t pk2 array expected')
            if ( present(stat) ) stat = opflag
      end select  
   else
      if ( allocated(mij) ) deallocate(mij)
   end if
         
   END SUBROUTINE pk2_GetMatijCh 
   
!essai 05/24
!=============================================================================================
   SUBROUTINE pk2_GetI32 ( lhs, rhs )  ! <-- i32(..) = pk2 (for generic "=")
!=============================================================================================
   integer( i32 ), allocatable, intent(in out) :: lhs(..)
   class  (pk2_t),              intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  (experimental: my first use of assumed rank arrays... not sure that all compilers are
!   ready or free of bugs in particularly for DT with allocatable members)
!-----------------------------------------------------------------------------------R.H. 05/24       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetI32'         
!---------------------------------------------------------------------------------------------

!!#include "include/pk2_getmatgeneric.inc"
   
   END SUBROUTINE pk2_GetI32     

!=============================================================================================
   SUBROUTINE pk2_GetI64 ( lhs, rhs )  ! <-- i64(..) = pk2 (for generic "=")
!=============================================================================================
   integer( i64 ), allocatable, intent(in out) :: lhs(..)
   class  (pk2_t),              intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  (experimental: my first use of assumed rank arrays... not sure that all compilers are
!   ready or free of bugs in particularly for DT with allocatable members)
!-----------------------------------------------------------------------------------R.H. 05/24       

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2_GetI64'         
!---------------------------------------------------------------------------------------------

!!#include "include/pk2_getmatgeneric.inc"
   
   END SUBROUTINE pk2_GetI64

!=============================================================================================
   SUBROUTINE pk2_GetI32stat ( self, mat, stat )  ! <-- i32(..) = pk2 (bound proc & pk2_assign )
!=============================================================================================
   class  (pk2_t),              intent(in    ) :: self
   integer( i32 ), allocatable, intent(in out) :: mat(:,:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 05/24       

   call pk2_GetI32 ( lhs = mat, rhs = self )

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_GetI32stat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_GetI32stat        

!=============================================================================================
   SUBROUTINE pk2_GetI64stat ( self, mat, stat )  ! <-- i64(..) = pk2 (bound proc & pk2_assign )
!=============================================================================================
   class  (pk2_t),              intent(in    ) :: self
   integer( i64 ), allocatable, intent(in out) :: mat(:,:)
   type   (err_t), optional,    intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  
!-----------------------------------------------------------------------------------R.H. 05/24       

   call pk2_GetI64 ( lhs = mat, rhs = self )

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_GetI64stat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_GetI64stat    
!fin  essai 05/24
                      
!=============================================================================================
   SUBROUTINE pk2_SetToI32 ( lhs, rhs )  ! <-- pk2 = i32
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   integer(i32  ), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a scalar integer (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromI32 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToI32') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToI32


!=============================================================================================
   SUBROUTINE pk2_SetToI32stat ( lhs, rhs, stat )  ! <-- pk2 = i32
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   integer(i32  ),           intent(in    ) :: rhs
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToI32 with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToI32 ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToI32stat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToI32stat
   

!=============================================================================================
   SUBROUTINE pk2_SetToI64 ( lhs, rhs )  ! <-- pk2 = i64
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   integer(i64  ), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a scalar integer (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromI64 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToI64') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
         
   END SUBROUTINE pk2_SetToI64

   
!=============================================================================================
   SUBROUTINE pk2_SetToI64stat ( lhs, rhs, stat )  ! <-- pk2 = i32
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   integer(i64  ),           intent(in    ) :: rhs
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToI64 with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToI64 ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToI64stat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToI64stat
   

!=============================================================================================
   SUBROUTINE pk2_SetToRsp ( lhs, rhs )  ! <-- pk2 = rSP
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   real (rSP  ), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a real scalar (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromRsp (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToRsp') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToRsp


!=============================================================================================
   SUBROUTINE pk2_SetToRspstat ( lhs, rhs, stat )  ! <-- pk2 = i32
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   real (rSP  ),           intent(in    ) :: rhs
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToRsp with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToRsp ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToRspstat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToRspstat


!=============================================================================================
   SUBROUTINE pk2_SetToRdp ( lhs, rhs )  ! <-- pk2 = rDP
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   real (rDP  ), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a real scalar (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromRdp (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToRdp') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToRdp


!=============================================================================================
   SUBROUTINE pk2_SetToRdpstat ( lhs, rhs, stat )  ! <-- pk2 = rDP
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   real (rDP  ),           intent(in    ) :: rhs
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToRdp with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToRdp ( lhs, rhs )

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToRdpstat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToRdpstat


!=============================================================================================
   SUBROUTINE pk2_SetToCsp ( lhs, rhs )  ! <-- pk2 = cDP
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   complex(rSP  ), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a complex scalar (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromCsp (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToCsp') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 

   END SUBROUTINE pk2_SetToCsp


!=============================================================================================
   SUBROUTINE pk2_SetToCspstat ( lhs, rhs, stat )  ! <-- pk2 = cSP
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   complex(rSP  ),           intent(in    ) :: rhs
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToCsp with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToCsp ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToCspstat')
      if ( present(stat) ) stat = opflag 
   end if

   END SUBROUTINE pk2_SetToCspstat


!=============================================================================================
   SUBROUTINE pk2_SetToCdp ( lhs, rhs )  ! <-- pk2 = cDP
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   complex(rDP  ), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a complex scalar (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromCdp (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToCdp') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToCdp


!=============================================================================================
   SUBROUTINE pk2_SetToCdpstat ( lhs, rhs, stat )  ! <-- pk2 = cDP
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   complex(rDP  ),           intent(in    ) :: rhs
   type   (err_t), optional, intent(in out) :: stat      
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToCdp with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToCdp ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToCdpstat')
      if ( present(stat) ) stat = opflag 
   end if

   END SUBROUTINE pk2_SetToCdpstat
   
   
!=============================================================================================
   SUBROUTINE pk2_SetToL ( lhs, rhs )  ! <-- pk2 = l
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   logical       , intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a boolean scalar (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromL (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToL')  

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToL


!=============================================================================================
   SUBROUTINE pk2_SetToLstat ( lhs, rhs, stat )  ! <-- pk2 = l
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   logical       ,           intent(in    ) :: rhs
   type   (err_t), optional, intent(in out) :: stat         
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToL with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToL ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToLstat')
      if ( present(stat) ) stat = opflag 
   end if

   END SUBROUTINE pk2_SetToLstat


!=============================================================================================
   SUBROUTINE pk2_SetToS ( lhs, rhs )  ! <-- pk2 = s
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   type (str_t), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a scalar str_t (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromS (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToS') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 

   END SUBROUTINE pk2_SetToS


!=============================================================================================
   SUBROUTINE pk2_SetToSstat ( lhs, rhs, stat )  ! <-- pk2 = s
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   type (str_t),           intent(in    ) :: rhs
   type (err_t), optional, intent(in out) :: stat            
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToS with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToS ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToSstat')
      if ( present(stat) ) stat = opflag 
   end if

   END SUBROUTINE pk2_SetToSstat


!=============================================================================================
   SUBROUTINE pk2_SetToCh ( lhs, rhs )  ! <-- pk2 = ch
!=============================================================================================
   class    (pk2_t), intent(in out) :: lhs
   character(len=*), intent(in    ) :: rhs
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a scalar character string (1x1 array)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   call bk2_AssignFromCH (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToCh') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToCh


!=============================================================================================
   SUBROUTINE pk2_SetToChStat ( lhs, rhs, stat )  ! <-- pk2 = ch
!=============================================================================================
   class    (pk2_t),           intent(in out) :: lhs
   character(len=*),           intent(in    ) :: rhs
   type     (err_t), optional, intent(in out) :: stat               
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToCh with "stat" (used in generic pk2_Assign)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToCh ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToChStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToChStat


!=============================================================================================
   SUBROUTINE pk2_SetToI32vec ( lhs, rhs )  ! <-- pk2 = I32vec
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   integer(i32  ), intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an integer vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromIvec32 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToI32vec') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 

   END SUBROUTINE pk2_SetToI32vec


!=============================================================================================
   SUBROUTINE pk2_SetToI32vecStat ( lhs, rhs, stat )  ! <-- pk2 = I32vec
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   integer(i32  ),           intent(in    ) :: rhs(:)
   type   (err_t), optional, intent(in out) :: stat                  
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToI32vec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToI32vec ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToI32vecStat')
      if ( present(stat) ) stat = opflag 
   end if

   END SUBROUTINE pk2_SetToI32vecStat


!=============================================================================================
   SUBROUTINE pk2_SetToI64vec ( lhs, rhs )  ! <-- pk2 = I64vec
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   integer(i64  ), intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an integer vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromIvec64 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToI64vec') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToI64vec


!=============================================================================================
   SUBROUTINE pk2_SetToI64vecStat ( lhs, rhs, stat )  ! <-- pk2 = I64vec
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   integer(i64  ),           intent(in    ) :: rhs(:)
   type   (err_t), optional, intent(in out) :: stat                     
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToI64vec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToI64vec ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToI64vecStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToI64vecStat


!=============================================================================================
   SUBROUTINE pk2_SetToRspVec ( lhs, rhs )  ! <-- pk2 = RspVec
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   real (rSP  ), intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an real vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromRvec32 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToI64vec') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToRspVec


!=============================================================================================
   SUBROUTINE pk2_SetToRspVecStat ( lhs, rhs, stat )  ! <-- pk2 = RspVec
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   real (rSP  ),           intent(in    ) :: rhs(:)
   type (err_t), optional, intent(in out) :: stat                        
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToRspVec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToRspVec ( lhs, rhs )

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToRspVecStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToRspVecStat
   
   
!=============================================================================================
   SUBROUTINE pk2_SetToRdpVec ( lhs, rhs )  ! <-- pk2 = RdpVec
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   real (rDP  ), intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to a real vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromRvec64 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToRdpVec') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToRdpVec


!=============================================================================================
   SUBROUTINE pk2_SetToRdpVecStat ( lhs, rhs, stat )  ! <-- pk2 = RdpVec
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   real (rDP  ),           intent(in    ) :: rhs(:)
   type (err_t), optional, intent(in out) :: stat                        
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToRdpVec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToRdpVec ( lhs, rhs )

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToRdpVecStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToRdpVecStat


!=============================================================================================
   SUBROUTINE pk2_SetToCspVec ( lhs, rhs )  ! <-- pk2 = CspVec
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   complex(rSP  ), intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to a complex vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromCvec32 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToCspVec') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToCspVec


!=============================================================================================
   SUBROUTINE pk2_SetToCspVecStat ( lhs, rhs, stat )  ! <-- pk2 = CspVec
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   complex(rSP  ),           intent(in    ) :: rhs(:)
   type   (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToCspVec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToCspVec ( lhs, rhs ) 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToCspVecStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToCspVecStat


!=============================================================================================
   SUBROUTINE pk2_SetToCdpVec ( lhs, rhs )  ! <-- pk2 = CdpVec
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   complex(rDP  ), intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to a complex vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromCvec64 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToCdpVec') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToCdpVec


!=============================================================================================
   SUBROUTINE pk2_SetToCdpVecStat ( lhs, rhs, stat )  ! <-- pk2 = CdpVec
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   complex(rDP  ),           intent(in    ) :: rhs(:)
   type   (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToCdpVec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToCdpVec ( lhs, rhs )

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToCdpVecStat')
      if ( present(stat) ) stat = opflag 
   end if
      
   END SUBROUTINE pk2_SetToCdpVecStat


!=============================================================================================
   SUBROUTINE pk2_SetToLvec ( lhs, rhs )  ! <-- pk2 = Lvec
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   logical       , intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to a boolean vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromLvec (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToLvec') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToLvec


!=============================================================================================
   SUBROUTINE pk2_SetToLvecStat ( lhs, rhs, stat )  ! <-- pk2 = Lvec
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   logical       ,           intent(in    ) :: rhs(:)
   type   (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToLvec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToLvec ( lhs, rhs )

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToLvecStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToLvecStat


!=============================================================================================
   SUBROUTINE pk2_SetToSvec ( lhs, rhs )  ! <-- pk2 = Svec
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   type (str_t), intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to a str_t vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromSvec (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToSvec') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToSvec


!=============================================================================================
   SUBROUTINE pk2_SetToSvecStat ( lhs, rhs, stat )  ! <-- pk2 = Svec
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   type (str_t),           intent(in    ) :: rhs(:)
   type (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToSvec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToSvec ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToSvecStat')
      if ( present(stat) ) stat = opflag 
   end if
      
   END SUBROUTINE pk2_SetToSvecStat
   

!=============================================================================================
   SUBROUTINE pk2_SetToChvec ( lhs, rhs )  ! <-- pk2 = CHvec
!=============================================================================================
   class    (pk2_t), intent(in out) :: lhs
   character(len=*), intent(in    ) :: rhs(:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 matrix (1 row) to a character string vector
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromCHvec (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToChvec') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
      
   END SUBROUTINE pk2_SetToChvec


!=============================================================================================
   SUBROUTINE pk2_SetToChvecStat ( lhs, rhs, stat )  ! <-- pk2 = CHvec
!=============================================================================================
   class    (pk2_t),           intent(in out) :: lhs
   character(len=*),           intent(in    ) :: rhs(:)
   type     (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToChvec with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToChvec ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToChvecStat')
      if ( present(stat) ) stat = opflag 
   end if
      
   END SUBROUTINE pk2_SetToChvecStat


!=============================================================================================
   SUBROUTINE pk2_SetToI32mat ( lhs, rhs )  ! <-- pk2 = I32mat
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   integer(i32  ), intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to an integer matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromImat32 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToI32mat') 
      
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 

   END SUBROUTINE pk2_SetToI32mat


!=============================================================================================
   SUBROUTINE pk2_SetToI32matStat ( lhs, rhs, stat )  ! <-- pk2 = I32mat
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   integer(i32  ),           intent(in    ) :: rhs(:,:)
   type   (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToI32mat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToI32mat ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToI32matStat')
      if ( present(stat) ) stat = opflag 
   end if

   END SUBROUTINE pk2_SetToI32matStat


!=============================================================================================
   SUBROUTINE pk2_SetToI64mat ( lhs, rhs )  ! <-- pk2 = I64mat
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   integer(i64  ), intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to an integer matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   call bk2_AssignFromImat64 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToI64mat') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToI64mat


!=============================================================================================
   SUBROUTINE pk2_SetToI64matStat ( lhs, rhs, stat )  ! <-- pk2 = I64mat
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   integer(i64  ),           intent(in    ) :: rhs(:,:)
   type   (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToI64mat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToI64mat ( lhs, rhs )
   
   if ( opflag%code > 0)  then
      call opflag%AddTrace('pk2_SetToI64matStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToI64matStat
   
   
!=============================================================================================
   SUBROUTINE pk2_SetToRspMat ( lhs, rhs )  ! <-- pk2 = RspMat
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   real (rSP  ), intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a real matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   call bk2_AssignFromRmat32 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToRspMat')
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
         
   END SUBROUTINE pk2_SetToRspMat


!=============================================================================================
   SUBROUTINE pk2_SetToRspMatStat ( lhs, rhs, stat )  ! <-- pk2 = RspMat
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   real (rSP  ),           intent(in    ) :: rhs(:,:)
   type (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToRspMat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToRspMat ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToRspMatStat')
      if ( present(stat) ) stat = opflag 
   end if
         
   END SUBROUTINE pk2_SetToRspMatStat


!=============================================================================================
   SUBROUTINE pk2_SetToRdpMat ( lhs, rhs )  ! <-- pk2 = RdpMat
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   real (rDP  ), intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a real matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromRmat64 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToRdpMat') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToRdpMat


!=============================================================================================
   SUBROUTINE pk2_SetToRdpMatStat ( lhs, rhs, stat )  ! <-- pk2 = RdpMat
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   real (rDP  ),           intent(in    ) :: rhs(:,:)
   type (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToRdpMat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToRdpMat ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToRdpMatStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToRdpMatStat


!=============================================================================================
   SUBROUTINE pk2_SetToCspMat ( lhs, rhs )  ! <-- pk2 = CspMat
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   complex(rSP  ), intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a complex matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------
 
   if ( opflag%code > IZERO ) return !!call opflag%set ()
  
   call bk2_AssignFromCmat32 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToCspMat') 
      
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToCspMat


!=============================================================================================
   SUBROUTINE pk2_SetToCspMatStat ( lhs, rhs, stat )  ! <-- pk2 = CspMat
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   complex(rSP  ),           intent(in    ) :: rhs(:,:)
   type   (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToCspMat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToCspMat ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToCspMatStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToCspMatStat


!=============================================================================================
   SUBROUTINE pk2_SetToCdpMat ( lhs, rhs )  ! <-- pk2 = CdpMat
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   complex(rDP  ), intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a complex matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------
 
   if ( opflag%code > IZERO ) return !!call opflag%set ()
  
   call bk2_AssignFromCmat64 (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToCdpMat') 

   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToCdpMat


!=============================================================================================
   SUBROUTINE pk2_SetToCdpMatStat ( lhs, rhs, stat )  ! <-- pk2 = CdpMat
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   complex(rDP  ),           intent(in    ) :: rhs(:,:)
   type   (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToCdpMat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToCdpMat ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToCdpMatStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToCdpMatStat
   

!=============================================================================================
   SUBROUTINE pk2_SetToLmat ( lhs, rhs )  ! <-- pk2 = Lmat
!=============================================================================================
   class  (pk2_t), intent(in out) :: lhs
   logical       , intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a boolean matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromLmat (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToLmat') 
      
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToLmat


!=============================================================================================
   SUBROUTINE pk2_SetToLmatStat ( lhs, rhs, stat )  ! <-- pk2 = Lmat
!=============================================================================================
   class  (pk2_t),           intent(in out) :: lhs
   logical       ,           intent(in    ) :: rhs(:,:)
   type   (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToLmat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToLmat ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToLmatStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToLmatStat


!=============================================================================================
   SUBROUTINE pk2_SetToSmat ( lhs, rhs )  ! <-- pk2 = Smat
!=============================================================================================
   class(pk2_t), intent(in out) :: lhs
   type (str_t), intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a str_t matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------   
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call bk2_AssignFromSmat (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToSmat') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 

   END SUBROUTINE pk2_SetToSmat


!=============================================================================================
   SUBROUTINE pk2_SetToSmatStat ( lhs, rhs, stat )  ! <-- pk2 = Smat
!=============================================================================================
   class(pk2_t),           intent(in out) :: lhs
   type (str_t),           intent(in    ) :: rhs(:,:)
   type (err_t), optional, intent(in out) :: stat                           
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToSmat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToSmat ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToSmatStat')
      if ( present(stat) ) stat = opflag 
   end if

   END SUBROUTINE pk2_SetToSmatStat


!=============================================================================================
   SUBROUTINE pk2_SetToChmat ( lhs, rhs )  ! <-- pk2 = CHmat
!=============================================================================================
   class    (pk2_t), intent(in out) :: lhs
   character(len=*), intent(in    ) :: rhs(:,:)
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a character string matrix
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------           
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   call bk2_AssignFromCHmat (lhs%m, rhs) ; error_TraceNreturn(opflag, 'pk2_SetToChmat') 
   
   lhs%typ = lhs%m%typ ; lhs%nrow = lhs%m%nrow ; lhs%ncol = lhs%m%ncol 
   
   END SUBROUTINE pk2_SetToChmat


!=============================================================================================
   SUBROUTINE pk2_SetToChmatStat ( lhs, rhs, stat )  ! <-- pk2 = CHmat
!=============================================================================================
   class    (pk2_t),           intent(in out) :: lhs
   character(len=*),           intent(in    ) :: rhs(:,:)
   type     (err_t), optional, intent(in out) :: stat                              
!---------------------------------------------------------------------------------------------
!  Same as pk2_SetToChmat with "stat" (used for the generic pk2_Assign subroutine)
!-----------------------------------------------------------------------------------R.H. 05/18       

   call pk2_SetToChmat ( lhs, rhs )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_SetToChmatStat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END SUBROUTINE pk2_SetToChmatStat


!=============================================================================================
   FUNCTION pk2_fSetToI32 ( scalar, name, stat ) result ( res )
!=============================================================================================
   integer  (i32  ),           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a scalar integer (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToI32 (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true.
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToI32')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToI32


!=============================================================================================
   FUNCTION pk2_fSetToI64 ( scalar, name, stat ) result ( res )
!=============================================================================================
   integer  (i64  ),           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a scalar integer (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToI64 (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToI64')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToI64


!=============================================================================================
   FUNCTION pk2_fSetToRsp ( scalar, name, stat ) result ( res )
!=============================================================================================
   real     (rSP  ),           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a real scalar (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToRsp (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToRsp')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToRsp


!=============================================================================================
   FUNCTION pk2_fSetToRdp ( scalar, name, stat ) result ( res )
!=============================================================================================
   real     (rDP  ),           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a real scalar (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToRdp (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToRdp')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToRdp


!=============================================================================================
   FUNCTION pk2_fSetToCsp ( scalar, name, stat ) result ( res )
!=============================================================================================
   complex  (rSP  ),           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a complex scalar (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToCsp (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToCsp')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToCsp
   

!=============================================================================================
   FUNCTION pk2_fSetToCdp ( scalar, name, stat ) result ( res )
!=============================================================================================
   complex  (rDP  ),           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a complex scalar (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToCdp (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToCdp')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToCdp
   

!=============================================================================================
   FUNCTION pk2_fSetToL ( scalar, name, stat ) result ( res )
!=============================================================================================
   logical         ,           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a boolean scalar (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToL (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToL')
      if ( present(stat) ) stat = opflag 
   end if

   END FUNCTION pk2_fSetToL


!=============================================================================================
   FUNCTION pk2_fSetToS ( scalar, name, stat ) result ( res )
!=============================================================================================
   type     (str_t),           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a scalar str_t string (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToS (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToS')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToS


!=============================================================================================
   FUNCTION pk2_fSetToCh ( scalar, name, stat ) result ( res )
!=============================================================================================
   character(len=*),           intent(in    ) :: scalar
   character(len=*), optional, intent(in    ) :: name
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a scalar character string (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToCh (res,scalar) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToCh')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToCh
   

!=============================================================================================
   FUNCTION pk2_fSetToI32vec ( vector, name, stat ) result ( res )
!=============================================================================================
   integer  (i32  ),           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an integer vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToI32vec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToI32vec')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToI32vec


!=============================================================================================
   FUNCTION pk2_fSetToI64vec ( vector, name, stat ) result ( res )
!=============================================================================================
   integer  (i64  ),           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an integer vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToI64vec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToI64vec')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToI64vec
   

!=============================================================================================
   FUNCTION pk2_fSetToRspVec ( vector, name, stat ) result ( res )
!=============================================================================================
   real     (rSP  ),           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an real vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToRspVec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToRspVec')
      if ( present(stat) ) stat = opflag 
   end if

   END FUNCTION pk2_fSetToRspVec


!=============================================================================================
   FUNCTION pk2_fSetToRdpVec ( vector, name, stat ) result ( res )
!=============================================================================================
   real     (rDP  ),           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an real vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToRdpVec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToRdpVec')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToRdpVec


!=============================================================================================
   FUNCTION pk2_fSetToCspVec ( vector, name, stat ) result ( res )
!=============================================================================================
   complex  (rSP  ),           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an complex vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToCspVec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToCspVec')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToCspVec


!=============================================================================================
   FUNCTION pk2_fSetToCdpVec ( vector, name, stat ) result ( res )
!=============================================================================================
   complex  (rDP  ),           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to an complex vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToCdpVec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToCdpVec')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToCdpVec
   

!=============================================================================================
   FUNCTION pk2_fSetToLvec ( vector, name, stat ) result ( res )
!=============================================================================================
   logical         ,           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name      
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to a boolean vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToLvec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToLvec')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToLvec


!=============================================================================================
   FUNCTION pk2_fSetToSvec ( vector, name, stat ) result ( res )
!=============================================================================================
   type     (str_t),           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to a str_t vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToSvec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToSvec')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToSvec


!=============================================================================================
   FUNCTION pk2_fSetToChvec ( vector, name, stat ) result ( res )
!=============================================================================================
   character(len=*),           intent(in    ) :: vector(:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array (1 row) to a character string vector (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToChvec (res,vector) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToChvec')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToChvec

         
!=============================================================================================
   FUNCTION pk2_fSetToI32mat ( matrix, name, stat ) result ( res )
!=============================================================================================
   integer  (i32  ),           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to an integer matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToI32mat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToI32mat')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToI32mat


!=============================================================================================
   FUNCTION pk2_fSetToI64mat ( matrix, name, stat ) result ( res )
!=============================================================================================
   integer  (i64  ),           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to an integer matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToI64mat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToI64mat')
      if ( present(stat) ) stat = opflag 
   end if
      
   END FUNCTION pk2_fSetToI64mat


!=============================================================================================
   FUNCTION pk2_fSetToRspMat ( matrix, name, stat ) result ( res )
!=============================================================================================
   real     (rSP  ),           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a real matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToRspMat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToRspMat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToRspMat


!=============================================================================================
   FUNCTION pk2_fSetToRdpMat ( matrix, name, stat ) result ( res )
!=============================================================================================
   real     (rDP  ),           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name   
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a real matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToRdpMat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToRdpMat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToRdpMat


!=============================================================================================
   FUNCTION pk2_fSetToCspMat ( matrix, name, stat ) result ( res )
!=============================================================================================
   complex  (rSP  ),           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a complex matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToCspMat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToCspMat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToCspMat
   

!=============================================================================================
   FUNCTION pk2_fSetToCdpMat ( matrix, name, stat ) result ( res )
!=============================================================================================
   complex  (rDP  ),           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a complex matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToCdpMat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToCdpMat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToCdpMat


!=============================================================================================
   FUNCTION pk2_fSetToLmat ( matrix, name, stat ) result ( res )
!=============================================================================================
   logical         ,           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name      
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a boolean matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToLmat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToLmat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToLmat


!=============================================================================================
   FUNCTION pk2_fSetToSmat ( matrix, name, stat ) result ( res )
!=============================================================================================
   type     (str_t),           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a str_t matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToSmat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToSmat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToSmat


!=============================================================================================
   FUNCTION pk2_fSetToChmat ( matrix, name, stat ) result ( res )
!=============================================================================================
   character(len=*),           intent(in    ) :: matrix(:,:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Set a pk2 array to a character string matrix (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call pk2_SetToChmat (res,matrix) ; if (present(name)) res%name = name ; res%constr = .true. 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_fSetToChmat')
      if ( present(stat) ) stat = opflag 
   end if
   
   END FUNCTION pk2_fSetToChmat


!=============================================================================================
   FUNCTION pk2_fi32i32SetToType ( typ, shape, name, stat ) result ( res )
!=============================================================================================
   integer  (i32  ),           intent(in    ) :: typ
   integer  (i32  ),           intent(in    ) :: shape(:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Initializes a pk2 matrix to a given shape of a given type (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter :: HERE = 'pk2_fi32i32SetToType'
   integer  (Ikind )            :: n, m
   character(len=99)            :: cnum
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
     
#include "include/pk2_fsettotype.inc"

   res%constr = .true.

   END FUNCTION pk2_fi32i32SetToType


!=============================================================================================
   FUNCTION pk2_fi32i64SetToType ( typ, shape, name, stat ) result ( res )
!=============================================================================================
   integer  (i32  ),           intent(in    ) :: typ
   integer  (i64  ),           intent(in    ) :: shape(:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Initializes a pk2 matrix to a given shape of a given type (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter :: HERE = 'pk2_fi32i64SetToType'
   integer  (Ikind )            :: n, m
   character(len=99)            :: cnum
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
     
#include "include/pk2_fsettotype.inc"

   res%constr = .true.

   END FUNCTION pk2_fi32i64SetToType


!=============================================================================================
   FUNCTION pk2_fi64i32SetToType ( typ, shape, name, stat ) result ( res )
!=============================================================================================
   integer  (i64  ),           intent(in    ) :: typ
   integer  (i32  ),           intent(in    ) :: shape(:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Initializes a pk2 matrix to a given shape of a given type (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter :: HERE = 'pk2_fi64i32SetToType'
   integer  (Ikind )            :: n, m
   character(len=99)            :: cnum
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
     
#include "include/pk2_fsettotype.inc"

   res%constr = .true.

   END FUNCTION pk2_fi64i32SetToType
      

!=============================================================================================
   FUNCTION pk2_fi64i64SetToType ( typ, shape, name, stat ) result ( res )
!=============================================================================================
   integer  (i64  ),           intent(in    ) :: typ
   integer  (i64  ),           intent(in    ) :: shape(:)
   character(len=*), optional, intent(in    ) :: name         
   type     (err_t), optional, intent(in out) :: stat
   type     (pk2_t)                           :: res   
!---------------------------------------------------------------------------------------------
!  Initializes a pk2 matrix to a given shape of a given type (constructor)
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter :: HERE = 'pk2_fi64i64SetToType'
   integer  (Ikind )            :: n, m
   character(len=99)            :: cnum
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
     
#include "include/pk2_fsettotype.inc"

   res%constr = .true.

   END FUNCTION pk2_fi64i64SetToType


!=============================================================================================
   FUNCTION pk2_fSetToEmpty ( ) result ( res )
!=============================================================================================
   type(pk2_t) :: res   
!---------------------------------------------------------------------------------------------
!  Added because instructions that use the default constructor, like
!               A = pk2_t( )   or    A = pk2_t(name='xxx', typ=CTYP),
!  strangely, cause memory leaks with ifort.
!
!  Note: A = pk2_t() is equivalent to:
!        . A = pk2_t(shape = [0,0],typ = EMPTY)
!        . call A%Destroy()
!-----------------------------------------------------------------------------------R.H. 12/18       
        
   END FUNCTION pk2_fSetToEmpty


!=============================================================================================
   SUBROUTINE pk2_SetToType ( typ, shape, res, name, stat )
!=============================================================================================
   integer  (Ikind),           intent(in    ) :: typ
   integer  (Ikind),           intent(in    ) :: shape(:)
   class    (pk2_t),           intent(   out) :: res   
   character(len=*), optional, intent(in    ) :: name  
   type     (err_t), optional, intent(in out) :: stat                
!---------------------------------------------------------------------------------------------
!  Initializes a pk2 matrix to a given shape of a given type
!-----------------------------------------------------------------------------------R.H. 05/18       

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter :: HERE = 'pk2_SetToType'
   integer  (Ikind )            :: n, m
   character(len=99)            :: cnum
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
     
#include "include/pk2_fsettotype.inc"

   res%constr = .true.

   END SUBROUTINE pk2_SetToType


!=============================================================================================
   SUBROUTINE pk2_i32SetSubmat ( self, a, indi, indj, add, stat )
!=============================================================================================
   class    (pk2_t),           intent(in out) :: self   
   class    (pk2_t),           intent(in    ) :: a   
   integer  (i32  ),           intent(in    ) :: indi(:)
   integer  (i32  ), optional, intent(in    ) :: indj(:)
   logical         , optional, intent(in    ) :: add
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  The elements of the array "a" are copied into or added to the elements of array "self" at
!  the entries (i,j) where i and j belongs to the sets of index "indi" and "indj", resp.:
!
!  . if add is present and true: 
!
!               self%m%v (indi(:),indj(:)) = self%m%v (indi(:),indj(:)) + a%m%v (:,:)
!
!  . else: 
!               self%m%v (indi(:),indj(:)) = a%m%v (:,:)
!
!
!  Notes: 
!
!  1) Concerning the shape of the resulting matrix (self):
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     let n = maxval(indi) and m = maxval(indj) and [nb,mb] = shape(self)
!
!     . If shape(a) /= [size(indi),size(indj)] and
!          shape(a) /= [1,1] and
!          shape(a) /= [0,0] 
!
!        --> Error
! 
!     . If "self" is empty on input: 
!
!       - if "a" is empty: "self" remains empty
!
!       - if "a" is non-empty: "self" will be n x m with self(indi,indj) = a
!
!     . If "self" is non-empty: 
!
!       - if "a" is empty: corresponding row(s) or column(s) are deleted from "self"
!
!       - if "a" is non-empty and if n > nb or m > mb: "self" is first resized to 
!                                                       max(n,nb) x max(m,mb)
!
!
!  2) Concerning the type of the resulting matrix (self):
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     . If a%typ and self%typ are in {ITYP,RTYP,CTYP} then self%typ = max(a%typ, b%typ)
!
!     . Otherwise, if "a" and "self" are not of the same type --> Error    
!
!  Examples:
!  ^^^^^^^^
!  1)    If a = | 11 12 |
!               | 13 14 |
!
!        and indi = [1,3], indj = [2,4], self = [ ]
!
!                        | 0  11  0  12 | 
!        then     self = | 0   0  0   0 |
!                        | 0  13  0  14 |
!
!  2)    If a = [ ]  and  indi = [1,2], indj = [1,2,3,4] and self is the previous matrix
!
!        then      self = | 0  13  0  14 |
!
!  3)    If              | 11  12  13  14 | 
!                 self = | 21  22  23  24 |
!                        | 31  32  33  34 |
!
!        and 
!                 a = | 2100  2400 |   indi = [2,3], indj = [1,4]
!                     | 3100  3400 |
!
!        then
!
!        . if add = .false. (or absent):
!
!                        |   11  12  13    14 |    (elements (2,1),(2,4),(3,1),(3,4) are
!                 self = | 2100  22  23  2400 |     replaced by elements of a)
!                        | 3100  32  33  3400 |
!
!        . if add = .true.:
!
!                        |   11  12  13    14 |    (elements (2,1),(2,4),(3,1),(3,4) are
!                 self = | 2121  22  23  2424 |     replaced by the sum with those of a)
!                        | 3131  32  33  3434 |
!----------------------------------------------------------------------------R.H. 06/18, 04/19       

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2_i32SetSubmat'
   integer  (Ikind)            :: ni, mi
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
#include "include/pk2_setsubmat.inc"    
            
   END SUBROUTINE pk2_i32SetSubmat


!=============================================================================================
   SUBROUTINE pk2_i64SetSubmat ( self, a, indi, indj, add, stat )
!=============================================================================================
   class    (pk2_t),           intent(in out) :: self
   class    (pk2_t),           intent(in    ) :: a   
   integer  (i64  ),           intent(in    ) :: indi(:)
   integer  (i64  ), optional, intent(in    ) :: indj(:)
   logical         , optional, intent(in    ) :: add
   type     (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------
!  The elements of the array "a" are copied into or added to the elements of array "self" at
!  the entries (i,j) where i and j belongs to the sets of index "indi" and "indj", resp.:
!
!  . if add is present and true: 
!
!               self%m%v (indi(:),indj(:)) = self%m%v (indi(:),indj(:)) + a%m%v (:,:)
!
!  . else: 
!               self%m%v (indi(:),indj(:)) = a%m%v (:,:)
!
!  Description: see pk2_i32SetSubmat
!----------------------------------------------------------------------------R.H. 06/18, 04/19       

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2_i64SetSubmat'
   integer  (Ikind)            :: ni, mi
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
#include "include/pk2_setsubmat.inc"    
            
   END SUBROUTINE pk2_i64SetSubmat
   

!=============================================================================================
   SUBROUTINE pk2_SetSubmat0 ( self, a, indi, indj )
!=============================================================================================
   class    (pk2_t),           intent(in out) :: self
   class    (pk2_t),           intent(in    ) :: a   
   integer  (Ikind), optional, intent(in    ) :: indi(:)
   integer  (Ikind), optional, intent(in    ) :: indj(:)
!---------------------------------------------------------------------------------------------
!  Copies the elements of the array "a" into the array "self" at the entries (i,j) where i 
!  and j belongs to the sets of index "indi" and "indj", respectively:
!
!                          self%m%v (indi(:),indj(:)) = a%m%v (:,:)
!  Notes: 
!
!  1) Concerning the shape of the resulting matrix (self):
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     let n = maxval(indi) and m = maxval(indj) and [nb,mb] = shape(self)
!
!     . If shape(a) /= [size(indi),size(indj)] and
!          shape(a) /= [1,1] and
!          shape(a) /= [0,0] 
!
!        --> Error
! 
!     . If "self" is empty on input: 
!
!       - if "a" is empty: "self" remains empty
!
!       - if "a" is non-empty: "self" will be n x m with self(indi,indj) = a
!
!     . If "self" is non-empty: 
!
!       - if "a" is empty: corresponding row(s) or column(s) are deleted from "self"
!
!       - if "a" is non-empty and if n > nb or m > mb: "self" is first resized to 
!                                                       max(n,nb) x max(m,mb)
!
!
!  2) Concerning the type of the resulting matrix (b):
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!     . If a%typ and self%typ are in {ITYP,RTYP,CTYP} then self%typ = max(a%typ, b%typ)
!
!     . Otherwise, if "a" and "self" are not of the same type --> Error    
!
!  Examples:
!  ^^^^^^^^
!  1)    if a = | 11 12 |
!               | 13 14 |
!
!        and indi = [1,3], indj = [2,4], self = [ ]
!
!                        | 0  11  0  12 | 
!        then     self = | 0   0  0   0 |
!                        | 0  13  0  14 |
!
!  2)    if a = [ ]  and  indi = [1,2], indj = [1,2,3,4] and self is the previous matrix
!
!        then      self = | 0  13  0  14 |
!                        
!-----------------------------------------------------------------------------------R.H. 06/18       

!- local variables ---------------------------------------------------------------------------
   integer(Ikind) :: ni, mi
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%typ == EMPTY .or. .not. allocated(a%m) ) then
!
!-    case self(indi,indj) = [ ]   (if self is also empty, it remains empty): 
!   
      if ( self%typ /= EMPTY ) then
!
!-       self is non-empty: this case corresponds to rows or columns removal:  
!  
         if ( .not. present(indi) .and. .not. present(indj) ) then
!
!-          case self = [] (reset self)
!
            self%typ = EMPTY ; self%nrow = 0 ; self%ncol = 0 ; deallocate(self%m)
            return
         
         else if ( present(indi) .and. present(indj) ) then
!
!-          case self(indi,indj) = []
!         
            ni = count(indi <= self%nrow .and. indi >= 1) ! ni = nbr of rows to remove
            mi = count(indj <= self%ncol .and. indj >= 1) ! mi = nbr of column to remove
!
!-          at least ni or mi must be equal to nb or mb:
!
            if ( ni /= self%nrow .and. mi /= self%ncol ) then
               call opflag%set ( stat = UERROR, &
                                 msg = 'the submatrix is not correctly defined' )
               return
            end if
   
            if ( ni == self%nrow ) then
!
!-             remove the columns whose #s are given in "indj":
!         
               call self%DelRows (indj,'c')
            else
!
!-             remove the rows whos #s are given in "indi":
!
               call self%DelRows (indi,'r')
            end if      
         
         else if ( present(indi) ) then   
!
!-          case self(indi) = [] (elements removal)
!         
            call self%DelRows (indi,'e')

         end if
      end if   
      return   
   end if  
   
   if ( .not. present(indj) ) then
      call a%m%SetSubmat (self%m, indi)
   else
      call a%m%SetSubmat (self%m, indi, indj)
   end if      

   self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol      
            
   END SUBROUTINE pk2_SetSubmat0


!=============================================================================================
   SUBROUTINE pk2_i32InsertInto ( self, a, pnum, cr, stat )
!=============================================================================================
   class    (pk2_t),           intent(in    ) :: self
   class    (pk2_t),           intent(in out) :: a   
   integer  (i32  ),           intent(in    ) :: pnum
   character(len=1),           intent(in    ) :: cr
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Insert the matrix "self" into the matrix "a". The insertion is done
!  . at the row # pnum if cr == 'r' (with  1 <= pnum <= a%nrow+1)
!  . else if cr == 'c', at the column # pnum (with 1 <= pnum <= a%ncol+1)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call self%m%InsertInto ( a%m, int(pnum,Ikind), cr )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_i32InsertInto')
      if ( present(stat) ) stat = opflag
      return
   end if
   
   a%typ = a%m%typ ; a%nrow = a%m%nrow ; a%ncol = a%m%ncol      
   
   END SUBROUTINE pk2_i32InsertInto


!=============================================================================================
   SUBROUTINE pk2_i64InsertInto ( self, a, pnum, cr, stat )
!=============================================================================================
   class    (pk2_t),           intent(in    ) :: self
   class    (pk2_t),           intent(in out) :: a   
   integer  (i64  ),           intent(in    ) :: pnum
   character(len=1),           intent(in    ) :: cr
   type     (err_t), optional, intent(in out) :: stat   
!--------------------------------------------------------------------------------------------- 
!  Insert the matrix "self" into the matrix "a". The insertion is done
!  . at the row #  pnum if cr == 'r' (with  1<= pnum <= a%nrow+1)
!  . else if cr == 'c', at the column # pnum (with 1 <= pnum <= a%ncol+1)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   call self%m%InsertInto ( a%m, int(pnum,Ikind), cr ) 

   if ( opflag%code > 0 ) then
      call opflag%AddTrace('pk2_i64InsertInto')
      if ( present(stat) ) stat = opflag
      return
   end if   
   
   a%typ = a%m%typ ; a%nrow = a%m%nrow ; a%ncol = a%m%ncol      
   
   END SUBROUTINE pk2_i64InsertInto
   

!=============================================================================================
   SUBROUTINE pk2_InsertPk2 ( insert, into, at_col, at_row, stat )
!=============================================================================================
   class    (pk2_t),           intent(in    ) :: insert
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts a pk2 into a pk2
!  Alternate to the bound procedure %insertinto
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertPk2'  
   character(len=1)            :: cr
   integer  (Ikind)            :: pnum
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( present(at_col) .and. present(at_row) ) then
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'Column # and row # should not both be given ' )
                       
   else if ( present(at_col) ) then
      cr = 'c' ; pnum = at_col
   else if ( present(at_row) ) then
      cr = 'r' ; pnum = at_row  
   else
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       'Missing the column or row # where the array must be inserted' )
   end if
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace(HERE)
      if ( present(stat) ) stat = opflag
      return
   end if
             
   call insert%m%InsertInto ( into%m, pnum, cr )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace(HERE)
      if ( present(stat) ) stat = opflag
      return
   end if
   
   into%typ = into%m%typ ; into%nrow = into%m%nrow ; into%ncol = into%m%ncol      
   
   END SUBROUTINE pk2_InsertPk2


!=============================================================================================
   SUBROUTINE pk2_InsertImat32 ( insert, into, at_col, at_row, stat )
!=============================================================================================
   integer  (i32  ),           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts an integer array (int32) into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertImat32'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertImat32


!=============================================================================================
   SUBROUTINE pk2_InsertImat64 ( insert, into, at_col, at_row, stat )
!=============================================================================================
   integer  (i64  ),           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts an integer array (int64) into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertImat64'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertImat64


!=============================================================================================
   SUBROUTINE pk2_InsertRmat32 ( insert, into, at_col, at_row, stat )
!=============================================================================================
   real     (rSP  ),           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts a real array (real32) into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertRmat32'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertRmat32


!=============================================================================================
   SUBROUTINE pk2_InsertRmat64 ( insert, into, at_col, at_row, stat )
!=============================================================================================
   real     (rDP  ),           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts a real array (real64) into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertRmat64'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertRmat64


!=============================================================================================
   SUBROUTINE pk2_InsertCmat32 ( insert, into, at_col, at_row, stat )
!=============================================================================================
   complex  (rSP  ),           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts a complex array (cmplx) into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertCmat32'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertCmat32


!=============================================================================================
   SUBROUTINE pk2_InsertCmat64 ( insert, into, at_col, at_row, stat )
!=============================================================================================
   complex  (rDP  ),           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts a complex array (cmplx64) into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertCmat64'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertCmat64


!=============================================================================================
   SUBROUTINE pk2_InsertLmat ( insert, into, at_col, at_row, stat )
!=============================================================================================
   logical         ,           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts an logical array into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertLmat'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertLmat


!=============================================================================================
   SUBROUTINE pk2_InsertSmat ( insert, into, at_col, at_row, stat )
!=============================================================================================
   type     (str_t),           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts a string (str_t) array into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertSmat'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertSmat


!=============================================================================================
   SUBROUTINE pk2_InsertChmat ( insert, into, at_col, at_row, stat )
!=============================================================================================
   character(len=*),           intent(in    ) :: insert(:,:)
   class    (pk2_t),           intent(in out) :: into 
   integer  (Ikind), optional, intent(in    ) :: at_col, at_row
   type     (err_t), optional, intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  Inserts a character array into a pk2
!---------------------------------------------------------------------------------- R.H. 07/20

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter :: HERE = 'pk2_InsertChmat'  
!--------------------------------------------------------------------------------------------- 

#include "include/pk2_insertmat.inc"
      
   END SUBROUTINE pk2_InsertChmat

   
!=============================================================================================   
   SUBROUTINE pk2_ExtracSubMat0 ( self, a, indx, jndx )
!=============================================================================================     
   class  (pk2_t),           intent(   out) :: self
   class  (pk2_t),           intent(in    ) :: a
   integer(Ikind), optional, intent(in    ) :: indx (:)
   integer(Ikind), optional, intent(in    ) :: jndx (:)
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  indx and jndx. 
!
!  If indx is not present: self = a
!
!  If indx is present:
!
!     - If jndx is present: self%m%v(i,j) = a%m%v(indx(i),jndx(j))
!
!     - If jndx is not present: "self" is an 1 x ni or ni x 1 array (ni=size(vindx))
!       reshaped from the elements of "a" ordered in column-major order:
!
!            if size(a,1) == 1: size(self) = shape = [1,ni]
!            else             : size(self) = shape = [ni,1]
!
!            self%m%v(i,j) = reshape(tmp(vindx),shape)  with  tmp = pack(a%m%v)
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   character(len=* ), parameter   :: HERE = 'pk2_ExtracSubMat0'  
   integer  (Ikind ), allocatable :: tmp(:)
   integer  (Ikind )              :: ni, nj, na, i
   character(len=99)              :: emsg
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set (stat = WARNING, msg = HERE//': << a >> is empty (--> self = [ ])')
      return
   end if   
               
   if ( .not. present(indx) ) then
      self = a
      if ( allocated(a%name) ) self%name = 'copy of "'//trim(a%name)//'"' 
      return
   end if
      
   ni = size(indx) 
        
   if ( ni == 0 ) then
      call opflag%set (stat = WARNING, msg = HERE//': << Indx >> is empty (--> self = [ ])') 
      return
   end if   
            
   if ( present(jndx) ) then  
!
!-    cas du type A(indx,jndx)
!
      nj = size(jndx)
      if ( nj == 0 ) then
         call opflag%set (stat = WARNING, msg = HERE//': << Jndx >> is empty (--> self = [ ])')
         return
      end if   
      
      if ( ni == 1 .and. indx(1) == huge(1_Ikind) ) then 

         if ( nj == 1 .and. jndx(1) == huge(1_Ikind) ) then 
!
!-          cas du type A(:,:)         
!            
            self = a
            if ( allocated(a%name) ) self%name = 'copy of "'//trim(a%name)//'"' 
         else  
!
!-          cas du type A(:,jndx)
!
            if ( minval(jndx) < 1 ) then
               write(emsg,'(a,i0,a,a)') &
               "index '",minval(jndx),"' of dimension #2 below lower bound of 1",&
               " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
               return
            end if
                  
            if ( maxval(jndx) > a%ncol ) then
               write(emsg,'(a,i0,a,i0,a)') &
               "index '",maxval(jndx),"' of dimension #2 above upper bound of ",a%ncol,&
               " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
               return
            end if
            
            allocate(tmp(a%nrow))
            do i = 1, a%nrow  
               tmp(i) = i
            end do   

            call a%m%CopySubmat ( self%m, indi = tmp, indj = jndx )
            
            self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
            if ( allocated(a%name) ) self%name = 'copy of a subarray of "'//trim(a%name)//'"'
                                               
         end if         
      else
        
         if ( nj == 1 .and. jndx(1) == huge(1_Ikind) ) then 
!
!-          cas du type A(indx,:)
!                  
            if ( minval(indx) < 1 ) then
               write(emsg,'(a,i0,a,a)') &
               "index '",minval(indx),"' of dimension #1 below lower bound of 1",&
               " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
               return
            end if
            if ( maxval(indx) > a%nrow ) then
               write(emsg,'(a,i0,a,i0,a)') &
               "index '",maxval(indx),"' of dimension #1 above upper bound of ",a%nrow,&
               " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
               return
            end if
                              
            allocate(tmp(a%ncol))
            do i = 1, a%ncol
               tmp(i) = i
            end do   

            call a%m%CopySubmat ( self%m, indi = indx, indj = tmp )
            
            self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
            if ( allocated(a%name) ) self%name = 'copy of a subarray of "'//trim(a%name)//'"'              
               
         else
!
!-          cas du type A(indx,jndx)  
!
            if ( minval(jndx) < 1 ) then
               write(emsg,'(a,i0,a,a)') &
               "index '",minval(jndx),"' of dimension #2 below lower bound of 1",&
               " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
               return
            end if
            if ( maxval(jndx) > a%ncol ) then
               write(emsg,'(a,i0,a,i0,a)') &
               "index '",maxval(jndx),"' of dimension #2 above upper bound of ",a%ncol,&
               " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
               return
            end if

            if ( minval(indx) < 1 ) then
               write(emsg,'(a,i0,a,a)') &
               "index '",minval(indx),"' of dimension #1 below lower bound of 1 ",&
               " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
               return
            end if
            if ( maxval(indx) > a%nrow ) then
               write(emsg,'(a,i0,a,i0,a)') &
               "index '",maxval(indx),"' of dimension #1 above upper bound of ",a%nrow,&
               " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
               return
            end if

            call a%m%CopySubmat ( self%m, indi = indx, indj = jndx )
            
            self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
            if ( allocated(a%name) ) self%name = 'copy of a subarray of "'//trim(a%name)//'"'              
               
         end if
      end if     
                                  
   else   
!
!-    cas du type A(indx)
!        
      na = a%nrow * a%ncol
             
      if ( ni == 1 .and. indx(1) == huge(1_Ikind) ) then 
!
!-       cas du type A(:)
!
         allocate(tmp(na))
         do i = 1, na
            tmp(i) = i
         end do

         call a%m%CopySubmat ( self%m, indi = tmp )
            
         self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
         if ( allocated(a%name) ) &
            self%name = 'copy of "'//trim(a%name)//'" in major-column ordering'              
                
      else  
!         
!-       cas du type A(indx)
!
         if ( minval(indx) < 1 ) then
            write(emsg,'(a,i0,a,a)') &
            "index '",minval(indx),"' below lower bound of 1"," (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
            return
         end if

         if ( maxval(indx) > na ) then
            write(emsg,'(a,i0,a,i0,a)') &
            "index '",maxval(indx),"' of dimension #1 above upper bound of ",na,&
            " (message from "//HERE//")"                                     
               call opflag%set (stat = UERROR, msg = emsg)
            return
         end if

         call a%m%CopySubmat ( self%m, indi = indx )

         self%typ = self%m%typ ; self%nrow = self%m%nrow ; self%ncol = self%m%ncol
         if ( allocated(a%name) ) &
         self%name = 'copy of of a subarray of"'//trim(a%name)//'" in major-column ordering'              
            
      end if   
         
   end if      

   END SUBROUTINE pk2_ExtracSubMat0


!=============================================================================================   
   SUBROUTINE pk2_i32ExtracSubMat ( self, a, indx, jndx, stat )
!=============================================================================================     
   class  (pk2_t),           intent(   out) :: self
   class  (pk2_t),           intent(in    ) :: a
   integer(i32  ),           intent(in    ) :: indx (:)
   integer(i32  ), optional, intent(in    ) :: jndx (:)
   type   (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  indx and jndx. 
!
!  . If jndx is present: self%m%v(i,j) = a%m%v(indx(i),jndx(j)).
!
!  . If jndx is not present: "self" is an 1 x ni or ni x 1 array (ni=size(indx)) reshaped from
!    the elements of "a" ordered in column-major order:
!
!            if size(a,1) == 1: size(self) = shape = [1,ni]
!            else             : size(self) = shape = [ni,1]
!
!            self%m%v(i,j) = reshape(tmp(indx),shape)  with  tmp = pack(a%m%v).
!
!  . size(indx)=1 with indx(1)=-1 means that the whole columns given by jndx will be extracted
!    (<=> a(:,jndx)).
!
!  . size(jndx)=1 with jndx(1)=-1 means that the whole rows given by indx will be extracted
!    (<=> a(indx,:)).
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   character(len=* ), parameter   :: HERE = 'pk2_i32ExtracSubMat'  
   integer  (Ikind ), allocatable :: tmp(:)
   integer  (Ikind )              :: ni, nj, na, i, err
   character(len=99)              :: emsg
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

#include "include/pk2_extracsubmat.inc"

   END SUBROUTINE pk2_i32ExtracSubMat

   
!=============================================================================================   
   SUBROUTINE pk2_i64ExtracSubMat ( self, a, indx, jndx, stat )
!=============================================================================================     
   class  (pk2_t),           intent(   out) :: self
   class  (pk2_t),           intent(in    ) :: a
   integer(i64  ),           intent(in    ) :: indx (:)
   integer(i64  ), optional, intent(in    ) :: jndx (:)
   type   (err_t), optional, intent(in out) :: stat   
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  indx and jndx. 
!
!  Description: see pk2_i32ExtracSubMat
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   character(len=* ), parameter   :: HERE = 'pk2_i64ExtracSubMat'  
   integer  (Ikind ), allocatable :: tmp(:)
   integer  (Ikind )              :: ni, nj, na, i, err
   character(len=99)              :: emsg
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
#include "include/pk2_extracsubmat.inc" 

   END SUBROUTINE pk2_i64ExtracSubMat
   
      
!=============================================================================================
   SUBROUTINE pk2_alloc1 ( mode, t, n, stat )
!=============================================================================================
   character(len=1),              intent(in    ) :: mode
   integer  (Ikind),              intent(in    ) :: n
   type     (pk2_t), allocatable, intent(in out) :: t(:)
   type     (err_t), optional   , intent(in out) :: stat
!--------------------------------------------------------------------------------------------- 
!  This procedure allocates or re-allocates the 1d pk2_t array t. It is specially useful when  
!  t is already allocated but needs to be extended.
! 
! * If mode = 's' (s for 'save'), t is saved in a temporary array, de-allocated and re-allocated 
!   with the new size (n). The saved entries are then copied back to t.
!
!   Warning: if the new size n is less than the old one, the part of t that is located beyond n
!            will be definitively lost. 
!
! * If mode = 'e' (e for 'erase'), t is simply de-allocated and re-allocated without a copy of 
!   its possible previous values.
! 
!
!  Inputs
!
!   mode: 's' or 'e'
!      n: the (new) size of t
!      t: array of pk2_t to be allocated or re-allocated with the new size n
!
!---------------------------------------------------------R.H. 12/16, 12/17 (use of move_alloc)
 
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'pk2_alloc1'
   integer  (Ikind)              :: nn, err
   type     (pk2_t), allocatable :: tmp(:)
   type     (err_t)              :: flag
!--------------------------------------------------------------------------------------------- 

   flag = err_t ()
   
   if ( .not. allocated(t) ) then

      allocate(t(n), stat=err)
      if ( err /= 0 ) &
         flag = err_t (msg = "Allocation failure for array t", stat = IERROR, where = HERE) 

   else if ( mode == 'e' ) then

      if ( allocated(t) ) deallocate(t)
      allocate(t(n), stat=err)
      if ( err /= 0 ) &
         flag = err_t (msg = "Allocation failure for array t", stat = IERROR, where = HERE) 
           
   else if ( mode == 's' ) then                      
   
      allocate(tmp(n), stat=err)
      if ( err /=  0) then
         flag = err_t (msg = "Allocation failure for array tmp", stat = IERROR, where = HERE) 
      else   
                                         
         nn = min(n,size(t,kind=Ikind))
         tmp(1:nn) = t(1:nn)
      
         call move_alloc (from = tmp, to = t)
      end if   
         
   else
      flag = err_t (stat = UERROR, where = HERE, msg =  &
                    "Invalid mode << "//mode//" >> (must be << e >> or << s >>)" ) 
   end if 
   
   if ( present(stat) ) then
      stat = flag
      return
   else
      if ( flag%code > 0 ) call flag%display(abort=.true.)
   end if      
         
   END SUBROUTINE pk2_alloc1


!=============================================================================================   
   SUBROUTINE pk2_ReadMat (self, unit, fname, nrow, ncol, colindx, typ, convert, comment, stat)
!=============================================================================================
   use util_m, only: util_GetUnit
   class    (pk2_t),           intent(   out) :: self
   integer  (Ikind), optional, intent(in    ) :: unit, nrow, ncol, colindx(:)
   character(len=*), optional, intent(in    ) :: fname, comment, typ, convert
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------           
   character(len=*    ), parameter   :: HERE = "pk2_ReadMat"
   character(len=:    ), allocatable :: com
   integer  (Ikind    )              :: ui, err
   character(len=LGSTR)              :: iomsg      
   logical                           :: exists, is_open
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !call opflag%set()
      
   if ( present(unit) .and. present(fname) ) then     
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       "Give either the unit number or the file name (or a string)" )
      if ( present(stat) ) stat = opflag
      return
   end if   
!
!- Set the unit number (ui), open the file if required:
!    
   exists = .true. ; is_open = .false.

   if ( present(unit) ) then
      ui = unit
   else if ( present(fname) ) then   
      inquire (file = fname, exist = exists)
      if ( exists ) then
         ui = util_GetUnit ()
         open (unit=ui, file=fname, status='old', action='read', iostat=err, iomsg=iomsg)  
         if ( err /= 0 ) then
            call opflag%set( stat = UERROR, where = HERE, msg = &
                            "Problem during file openning. I/O-msg: "//trim(iomsg))
            if ( present(stat) ) stat = opflag
            return
         end if
         is_open = .true.
      else
         if ( index(adjustl(fname),'[') /= 1 ) then
            call opflag%set( stat = UERROR, where = HERE, msg = &
            "Give an existing file name or a string begining by '[' and ending by ']'"//NL//&
            "--> File "//trim(fname)//" not found" )
            if ( present(stat) ) stat = opflag
            return
         end if    
      end if   
   else
      ui = STDIN   
   end if       
!
!- Symbol for comments (default: '#')
!
   com = '#'
   if ( present(comment) ) then
      if ( len_trim(comment) /= 0 ) com = comment
   end if  
   
   if ( .not. present(nrow) .and. .not. present(ncol) .and. .not. present(colindx) .and. &
        .not. present(typ ) ) then
       
      if ( exists ) then
         call pk2_ReadMat1 ( self, unit=ui, comment=com )
      else   
         call pk2_ReadMat1 ( self, string=fname, comment=com )
      end if      
      
   else if ( present(nrow) .and. present(ncol) .and. .not. present(colindx) .and. &
       .not. present(typ ) ) then
       
      call pk2_ReadMat2 (self, ui, nrow, ncol=ncol, comment=com, convert=convert)
      
   else if ( present(nrow) .and. present(colindx) .and. .not. present(ncol) .and. &
        .not. present(typ ) ) then
       
      call pk2_ReadMat2 (self, ui, nrow, colindx=colindx, comment=com, convert=convert)
      
   else if ( present(nrow) .and. present(ncol) .and. present(typ) .and. &
        .not. present(colindx) ) then
       
      call pk2_ReadMat3 (self, ui, typ, nrow, ncol=ncol, comment=com)
      
   else if ( present(nrow) .and. present(colindx) .and. present(typ) .and. &
        .not. present(ncol) ) then
       
      call pk2_ReadMat3 (self, ui, typ, nrow, colindx=colindx, comment=com)
      
   else
      call opflag%set(stat=UERROR, where=HERE, msg="Incompatible option in << readmat >>")
      if ( present(stat) ) stat = opflag
      return
   end if   

   if ( opflag%code > 0 ) then
      call opflag%AddTrace(HERE)
      if ( present(stat) ) stat = opflag
   end if

   if ( is_open ) close(ui)
      
   END SUBROUTINE pk2_ReadMat


!=============================================================================================   
   SUBROUTINE pk2_ReadMat1 ( self, unit, string, comment )
!=============================================================================================
   use util_m, only: util_GetRecord, util_SplitString1
   class    (pk2_t),           intent(   out) :: self
   integer  (Ikind), optional, intent(in    ) :: unit
   character(len=*), optional, intent(in    ) :: comment, string
!---------------------------------------------------------------------------------------------
!  Reads an array from a file or from a string in the format: [ .. , ..  ; .. , ..  ]
!
!  (usefull for small arrays entered from stdin for example).
!
!  Input(s):
! 
!     . unit   : (optional) must be a valid unit number of an already opened file
!     . string : (optional) a string containing the array
!     . comment: (optional) the symbol(s) used in the file for comments.
!
!  Ouputs:
!
!     . self  : the corresponding pk2 array.
!     . opflag: if an error is detected, opflag%code > 0 and opflag%msg contains the error
!               message.
!
!  Notes: 
!  . The data should be enclosed between square brackets, separated by comma (,) (column 
!    delimiter) and/or semi-colon (;) (row delimiter). 
!    - String elements must be enclosed by quotes (') or double quotes. 
!    - Complex elements must be enclosed by parentheses with real and imaginary parts separated
!      by comma (,). 
!    - Logical can be writen as .true., .false. or more succinctly .t., .f.
!  . Shape conformity between rows and type conformity between elements are first checked.
!
!  Examples:
!  1) 
!       [ "a string", 'another one' ; 'hello', "world" ]   a 2x2 string array
!       [ .t., .f. ; .false., .t.]  a 2x2 logical array
!       [ 1, 2  , 3  ; 4,   5    , 6 ]  a 2x3 integer array
!       [ 1, 2. , 3  ; 4,   5    , 6 ]  a 2x3 real array (at least one element is real)
!       [ 1, 2. , 3  ; 4, (5,1)  , 6 ]  a 2x3 complex array (at least one element is complex)
!
!     These data can be read from an openned file or read from the stdin or passed directly in
!     a string:
!
!     call pk2_ReadMat1 (A, ui)               --> the array is read from an opened file 
!     call pk2_ReadMat1 (A, ’[1,2,3;4,5,6]')  --> the array is read from a string 
!
!     The symbols '...' may be used as a break line, and comments must be added by using some
!     user symbols (the default is '#'). Blank line must also be inserted.
!
!  2) Example of a file containing the following lines ('//' are used for comments):
!
!       // this is an example file where an array is defined as follows:
!          [ 
!              1,  2, 3, 4.1 ; ... // this is the 1st row, followed by a blank line
!
!              5,  6, 7, 8   ; ... // this is the 2nd row
!              9, -1, 0, 0     ... // this is the 3rd row
!          ]
!       // end of the file
!
!     If the logical unit of the file is "ui", the corresponding call is
! 
!         call pk2_ReadMat1 (A, unit = ui, comment = '//')
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables ---------------------------------------------------------------------------           
   character(len=*    ), parameter   :: HERE = "pk2_ReadMat1"
   character(len=:    ), allocatable :: buf, str, com(:)
   type     (str_t    ), allocatable :: rows(:), elts(:)
   character(len=LGSTR)              :: iomsg   
   real     (Rkind    )              :: rtmp  
   integer  (Ikind    )              :: icol, irow, nrow, ncol, nelt, rowtyp, elttyp,   &
                                        typ, lstr, ioerr, err
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return ! call opflag%set()
!
!- Read from the file the string containing the array:
!   
   if ( present(unit) ) then
      if ( present(comment) ) then
         allocate(character(len_trim(comment))::com(3))
         com(1) = comment ; com(2) = '' ; com(3) = ''
         buf = util_GetRecord ( unit, opflag, contSymb = ['...'], comments = com )
      else
         buf = util_GetRecord ( unit, opflag, contSymb = ['...'] )
      end if
      error_TraceNreturn(opflag, HERE)
   else if ( present(string) ) then
      buf = string   
   else
      call opflag%Set(UERROR, HERE, "Unit or string must be present")
      return
   end if   

   if ( index(buf,'[') /= 1 .or. index(buf,']') /= len(buf) ) then
      str = ''
      if ( index(buf,'#'  ) == 1 .and. comment /= '#' ) str = '#'
      if ( index(buf,'$'  ) == 1 .and. comment /= '$' ) str = '$'
      if ( index(buf,'!'  ) == 1 .and. comment /= '!' ) str = '!'
      if ( index(buf,'//' ) == 1 .and. comment /= '//') str = '//'
      buf = "Array definition requires square brackets"
      if ( len_trim(str) > 0 ) &
         buf = trim(buf) // NL // "    (symbol for comment seems to be '"//trim(str)// &
                                  "' and not '"//trim(comment)//"' (see the help for readmat))"
      call opflag%Set(UERROR, HERE, trim(buf))
      return
   end if   
   
   buf = buf(2:len(buf)-1)  ! remove the '[' and ']'
   
   if ( len_trim(buf) == 0 ) return
!
!- Form the rows of the array (split the string "buf" if formed of multiple parts separated by
!  the row delimiter ';'):
!   
   call util_SplitString1 ( buf, ";", "''"//'""', rows, opflag, nrow )

   error_TraceNreturn(opflag, HERE)
   
   typ = 0  ; ncol = 0
!
!- Loop over the rows:
!
   do irow = 1, nrow   
!
!-    Form the elements of the row #irow (split the string "rows(irow)%str" if formed of
!     multiple parts separated by the column delimiter ','):
!
      call util_SplitString1 ( rows(irow)%str, ",", "''"//'""()', elts, opflag, nelt )

      if ( opflag%code > 0 ) then
         self = pk2_t()
         call opflag%AddTrace(HERE)
         return                  
      end if         
!
!-    Determine the type of the current row:
!
      rowtyp = pk2_DataType ( rows(irow)%str )
         
      if ( irow == 1 ) then
!
!-       Allocate the array of type "rowtyp" and shape [nrow, nelt]:
!      
         typ = rowtyp ;  ncol = size(elts)
         call pk2_SetToType ( typ = rowtyp, shape = [ nrow, ncol ], res = self )
         error_TraceNreturn(opflag, HERE)
      else 
!
!-       Check shape conformity between rows:
!      
         if ( ncol /= nelt ) then
            self = pk2_t()  
            call opflag%Set(UERROR, HERE, "Inconsistent shapes")
            return
         end if
!
!-       Check type conformity between the previous row:
!              
         if ( typ /= rowtyp ) then
!
!-          a conversion of the previous row(s) is possibly needed for numeric types:
!         
            if ( typ <= CTYP .and. rowtyp <= CTYP ) then
               if ( typ < rowtyp ) then
                  typ = rowtyp
                  if ( rowtyp == CTYP ) then
                     call bk2_convertToCk2 (self%m)
                  else if ( rowtyp == RTYP ) then
                     call bk2_convertToRk2 (self%m)
                  end if
                  self%typ = rowtyp                                    
                  error_TraceNreturn(opflag, HERE)
               end if   
            else
               self = pk2_t()  
               call opflag%Set(UERROR, HERE, "Inconsistent types")
               return
            end if 
         end if 
      end if        
!
!-    Read the "nelt" elements of the current row:
!
      err = 0            
      select type (p => self%m)
         type is (ik2_t) 
            do icol = 1, nelt
               elttyp = pk2_DataType ( elts(icol)%str )
               if ( elttyp == ITYP ) then
                  read(elts(icol)%str,*,iostat=ioerr,iomsg=iomsg) p%v(irow,icol)
                  if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if
               else     
                  err = 2 ; exit
               end if   
            end do 
               
         type is (rk2_t) 
            do icol = 1, nelt
               elttyp = pk2_DataType ( elts(icol)%str )
               if ( elttyp <= RTYP ) then
                  read(elts(icol)%str,*,iostat=ioerr,iomsg=iomsg) p%v(irow,icol)
                  if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if
               else
                  err = 2 ; exit
               end if   
            end do 
               
         type is (ck2_t) 
            do icol = 1, nelt
               elttyp = pk2_DataType ( elts(icol)%str )
               if ( elttyp <= CTYP ) then
                  if ( index(adjustl(elts(icol)%str),'(') /= 0 ) then
                     read(elts(icol)%str,*,iostat=ioerr,iomsg=iomsg) p%v(irow,icol)
                     if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if
                  else   
                     read(elts(icol)%str,*,iostat=ioerr,iomsg=iomsg) rtmp
                     if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if
                     p%v(irow,icol) = rtmp
                  end if
               else   
                  err = 2 ; exit
               end if   
            end do 
               
         type is (lk2_t) 
            do icol = 1, nelt
               elttyp = pk2_DataType ( elts(icol)%str )
               if ( elttyp == LTYP ) then
                  read(elts(icol)%str,*,iostat=ioerr,iomsg=iomsg) p%v(irow,icol)
                  if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if
               else     
                  err = 2 ; exit
               end if   
            end do 
               
         type is (sk2_t) 
            do icol = 1, nelt
               elttyp = pk2_DataType ( elts(icol)%str )
               if ( elttyp == STYP ) then               
                  str = trim(adjustl(elts(icol)%str)) ; lstr = len(str)
                  if ( (str(1:1) == '"' .and. str(lstr:lstr) == '"') .or. &
                       (str(1:1) == "'" .and. str(lstr:lstr) == "'")      ) then
                     p%v(irow,icol)%str = str(2:lstr-1) ! remove the quotes
                  else   
                     err = 3 ; exit
                  end if   
               else   
                  err = 2 ; exit
               end if   
            end do 
      end select 
      
      if ( err /= 0 ) then
         self = pk2_t()                  
         if ( err == 1 ) then
            call opflag%Set(UERROR, HERE, "Problem during reading. I/O-msg: "//trim(iomsg))
            return
         else if ( err == 2 ) then
            call opflag%Set(UERROR, HERE, "Inconsitent types.") 
            return
         else if ( err == 3 ) then
            call opflag%Set(UERROR, HERE, "Missing quotes.") 
            return
         end if
      end if
                     
   end do 
      
   END SUBROUTINE pk2_ReadMat1     


!=============================================================================================   
   SUBROUTINE pk2_ReadMat2 ( self, unit, nrow, ncol, colindx, convert, comment )
!=============================================================================================
   use util_m, only: util_GetRecord, util_countTokens
   class    (pk2_t),           intent(   out) :: self
   integer  (Ikind),           intent(in    ) :: unit, nrow
   integer  (Ikind), optional, intent(in    ) :: ncol, colindx(:)
   character(len=*), optional, intent(in    ) :: comment, convert
!---------------------------------------------------------------------------------------------
!  Reads an array from a file where data are organized by columns.
!
!  Input(s):
!
!      . nrow   : The number of rows to read (rows above "nrow" are ignored).
!                 If nrow = -1 the read is done until the end of the file.
!
!      . ncol   : The number of columns (columns above "ncol" are ignored).
!                 If ncol = -1 all the elements of the line are taken into account
!
!      . unit   : must be a valid unit number of an already opened file.
!!
!      . convert: If present, forces the data type to be of a given type:
!                 - if convert == 'i', convert if needed data in integers
!                 - if convert == 'r', convert if needed data in reals
!                 - if convert == 'c', convert if needed data in complexes
!                 - if convert == 'l', convert if needed data in logicals
!                 - if convert == 's', considere data as strings
!
!      . comment: the symbols used in the file for comments
!
!  Ouputs:
!
!      . self  : the corresponding pk2 array.
!      . opflag: if an error is detected, opflag%code > 0 and opflag%msg contains the error
!                message.
!
!  Notes: 
!
!  . The procedures pk2_?ReadMat2 (as pk2_?ReadMat3) are most useful when the columns of the
!    file are of different type (mixed data). On the contrary, if the data are type-homogeneous 
!    with a known type at run-time, it is more advisable to use the procedures pk2_?ReadMat4 
!    which are faster)
!
!  . The read is done from the standard input if "unit" and "fname" are not present.
!
!  . "unit" and "fname" can not both be present.
!
!  . The data should not be enclosed between square brackets. They must be organized row after
!    row, with elements separated by space(s). 
!    - String elements may or may not be enclosed by quotation marks (' or ") (obviously, 
!      quotes are needed if some elements contain spaces).
!    - Complex elements must be enclosed by parentheses with real and imaginary parts separated
!      by comma (,). 
!    - Logical can be writen as .true., .false. or more succinctly .t., .f.
!
!  . The file may contains blank lines, comment-lines, comment-end-lines.
!
!  Examples:
!
!  . assume the file 'foo.txt' contains the following 6 lines ("(l?)|" added for clarity) 
!
!   (l1)|  # A file containing column data of various type                      
!   (l2)|  # integers  reals  integers integers  strings  logicals  complexes   
!   (l3)|    1         2e1     3       4         'a'      .true.    (1,1)      # the 1st row   
!   (l4)|    5         6e-1    7       8         'b'      .true.    (2.0,3)    # the 2nd row   
!   (l5)|                                                                       
!   (l6)|    9        -1e3     0       0         'c'      .false.   (3.0,-4.0) # the 3rd row 
!
!  . call pk2_ReadMat2 (a, nrow = 2, ncol = 3, fname = 'foo.txt')
!
!    will store in "a" the first three elements of the first two lines of the file 'foo.txt' 
!    that are not a comment-line or a blank line (other elements will be ignored). 
!    The shape of the resulting array will be 2 x 3 and its type will be determined automati-
!    cally according to the read data (real in this example: a = [1.0,20.0,3.0 ; 5.0,0.6,7.0]).
!
!  . call pk2_ReadMat2 (a, nrow = 1, ncol =-1, fname = 'foo.txt', convert = 's')
!
!    will store in "a" all elements of the first line (that are not a comment-line or a blank
!    line) of the file 'foo.txt', all considered as strings.
!    The resulting array is then a 1 x 7 array (a = ['1','2e1','3','4','a','.true.','(1,1)']).
!
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables ---------------------------------------------------------------------------           
   character(len=*    ), parameter   :: HERE = "pk2_ReadMat2"
   character(len=:    ), allocatable :: CurrentRow, str, com(:)
   type     (str_t    ), allocatable :: elts(:,:)
   integer  (Ikind    ), allocatable :: col(:)
   character(len=LGSTR)              :: iomsg 
   character(len=99   )              :: c1, c2, c3  
   integer  (Ikind    )              :: itmp  
   real     (Rkind    )              :: rtmp  
   complex  (Rkind    )              :: ctmp  
   logical                           :: ltmp
   integer  (Ikind    )              :: i, n, m, icol, irow, nelt0, nelt, &
                                        maxcol, typ, rowtyp, elttyp, exptyp, lstr, ioerr, err    
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !call opflag%set()  
   
   if ( present(ncol) ) then
      m = ncol
      if (m == 0) return      
      maxcol = m
      if ( m > 0 ) then
         allocate(col(m)) ; do i=1,m ; col(i) = i ; end do
      end if    
   else if ( present(colindx) ) then
      m = size(colindx)
      if ( m == 0 ) return
      if ( any(colindx <= 0) ) then
         call opflag%Set(UERROR, HERE, "Negative column number ") 
         return
      end if
      maxcol = maxval(colindx) 
      allocate(col(m), source = colindx)
   else
      call opflag%Set(UERROR, HERE, "Missing ncol or colindx") 
      return
   end if 
!
!- If the type is imposed (present(convert) & len(convert) > 0): 
!  set "exptyp" to this type, else set exptyp = 0
!   
   exptyp = 0
   if ( present(convert) ) then
      if ( len_trim(convert ) /= 0) then
         if ( convert == 'i' ) exptyp = ITYP
         if ( convert == 'r' ) exptyp = RTYP
         if ( convert == 'c' ) exptyp = CTYP
         if ( convert == 'l' ) exptyp = LTYP
         if ( convert == 's' ) exptyp = STYP
         if ( exptyp == 0 ) then
            call opflag%Set(UERROR, HERE, "invalid value ('"//convert//"') for convert")
            return
         end if
      end if   
   end if    
!
!- Symbol for commented lines (default = '#')
!
   if ( present(comment) ) then
      allocate(character(len=len_trim(comment))::com(3))
      com(1) = comment
   else
      allocate(character(len=1)::com(3))
      com(1) = '#'
   end if         
   com(2) = '' ; com(3) = ''
!
!- Find the number of lines in the file if "nrow" is not given:
!   
   if ( nrow ==-1 ) then
      if ( unit == STDIN ) then
         call opflag%Set(UERROR, HERE, &
                        "The number of rows (nrow) is required for reading on standard input")
         return
      end if   
      n = 0
      do 
         read(unit,*,iostat=ioerr,iomsg=iomsg) c1
         if ( ioerr < 0 ) exit
         if ( ioerr > 0 ) then
            call opflag%Set(UERROR,HERE,"Problem during reading. "//"I/O-msg: "//trim(iomsg))
            return
         end if                                
         c2 = adjustl(c1)
!
!-       do not count commented lines or blank lines:
!         
         if ( index(c2,com(1)) == 1 .or. len_trim(c2) == 0 ) cycle 
         n = n + 1
      end do   
      rewind(unit)
   else
      n = nrow   
   end if   
!
!- Read the file row after row (note: as blank line or commented line may be inserted in the
!  file, an infinite do-loop is used. Exit from the loop occurs when the eof is reached or when
!  the n rows have been read):
!    
   irow = 0
   
   do 
!
!-    read the current line:
!   
      CurrentRow = adjustl ( util_GetRecord (unit, opflag, comments = com, rmblk = .false.) )
      if ( len_trim(CurrentRow) == 0 ) cycle
                              
      if ( opflag%code > 0 ) then
         self = pk2_t()
         call opflag%addTrace(HERE)                 
         return                  
      end if
!
!-    if the end-of-file is reached, exit from the loop:
!          
      if ( opflag%code < 0 ) then
         call opflag%set() ; exit
      end if       
      
      irow = irow + 1
!
!-    Counts the number of the different elements present in the current row (tokens separated 
!     by blank space(s)):

      nelt = util_CountTokens ( CurrentRow, delims = ' ', BlkToken = .false., &
                                opcl = '""()'//"''", tokens = elts, stat=opflag )
      if ( opflag%code > 0 ) then
         self = pk2_t()
         call opflag%addTrace(HERE)                 
         return                  
      end if      

      if ( irow == 1 ) then
         if ( maxcol == -1 ) then
            m = nelt ; allocate(col(m)) ; do i = 1, m ; col(i) = i ; end do ; maxcol = m
         end if
         !allocate(elts(nelt)) ; 
         nelt0 = nelt
      else
!
!-       Check shape confirmity with the previous row:
!              
         if ( nelt /= nelt0 ) then  
            self = pk2_t()  
            write(c1,'(i0)')irow ; write(c2,'(i0)')nelt ; write(c3,'(i0)')nelt0       
            call opflag%Set(UERROR, HERE,                                         &
                  "Inconsistent shape:" // NL //                                           &
                  ". the row #" // trim(c1) // " has " // trim(c2) // " elements" // NL // &
                  ". the first row has "//trim(c3)//" elements"                            ) 
            return
         end if  
      end if  
      
      if ( nelt < maxcol ) then    
         self = pk2_t()                  
         write(c1,'(i0)') nelt ; write(c2,'(i0)') maxcol
         call opflag%Set(UERROR, HERE, 'Only ' // trim(c1) //       &
                          ' columns found in the file while the column #' // &
                          trim(c2) // ' is requested'                        )
         return
      end if       
!
!-    Splits then "CurrentRow" in "nelt" substrings:
!        
      !elts(1)%str = trim(adjustl(CurrentRow(:ptok(1)-1)))   
      !do i = 2, nelt-1
      !   elts(i)%str = trim(adjustl(CurrentRow(ptok(i-1)+1:ptok(i)-1)))
      !end do
      !if (nelt > 0) elts(nelt)%str = trim(adjustl(CurrentRow(ptok(nelt-1)+1:)))
!
!-    Determine the type of the current row:
!
      rowtyp = 0
      do i = 1, m
         icol = col(i)
         rowtyp = max(rowtyp, pk2_DataType ( elts(icol,1)%str ))
      end do   
                     
      if ( irow == 1 ) then
!
!-       Allocate the pk2 array (type "typ" and shape [nrow=n, ncol=m]):
!      
         typ = rowtyp
         if ( exptyp /= 0 ) typ = exptyp
         
         !self = pk2_t ( typ = typ, shape = [ n, m ] )
         call pk2_SetToType ( typ = typ, shape = [ n, m ], res = self )
         error_TraceNreturn(opflag, HERE)
      else 
!
!-       Check type conformity with the previous row (excepted for the case where the type
!        is imposed (exptyp /= 0)) and if needed convert the previous data. 
!              
         if ( typ /= rowtyp .and. exptyp == 0 ) then
!
!-          a conversion of previous row(s) is possibly needed for numeric types:
!         
            if ( typ <= CTYP .and. rowtyp <= CTYP ) then
               if ( typ < rowtyp ) then
                  typ = rowtyp

                  if ( rowtyp == CTYP ) then
                     call bk2_convertToCk2 (self%m)
                  else if ( rowtyp == RTYP ) then
                     call bk2_convertToRk2 (self%m)
                  end if
                  error_TraceNreturn(opflag, HERE)
                  self%typ = rowtyp                                 
               end if   
            else
               self = pk2_t()
               call opflag%Set(UERROR, HERE, "Inconsistent types")
               return
            end if 
         end if                            
      end if
!
!-    Read the requested "m" elements of the current row:
!
      if ( exptyp == 0 ) then
!
!-       case without imposed type:
!      
         err = 0            
         select type (p => self%m)
            type is (ik2_t) 
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  if ( elttyp == ITYP ) then
                     read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) p%v(irow,i)
                     if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                  else     
                     err = 2 ; exit
                  end if   
               end do 
               
            type is (rk2_t) 
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  if ( elttyp <= RTYP ) then
                     read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) p%v(irow,i)
                     if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                  else
                     err = 2 ; exit
                  end if   
               end do 
               
            type is (ck2_t) 
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  if ( elttyp <= CTYP ) then
                     if ( index(adjustl(elts(icol,1)%str),'(') /= 0 ) then
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) p%v(irow,i)
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                     else   
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) rtmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        p%v(irow,icol) = cmplx(rtmp,kind=Rkind)
                     end if
                  else   
                     err = 2 ; exit
                  end if   
               end do 
               
            type is (lk2_t) 
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  if ( elttyp == LTYP ) then
                     read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) p%v(irow,i)
                     if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                  else     
                     err = 2 ; exit
                  end if   
               end do 
               
            type is (sk2_t) 
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  if ( elttyp == STYP ) then               
                     str = trim(adjustl(elts(icol,1)%str)) ; lstr = len(str)
                     if ( (str(1:1) == '"' .and. str(lstr:lstr) == '"') .or. &
                          (str(1:1) == "'" .and. str(lstr:lstr) == "'")      ) then
                        p%v(irow,i)%str = str(2:lstr-1) ! remove the quotes
                     else   
                        err = 3 ; exit
                     end if   
                  else   
                     err = 2 ; exit
                  end if   
               end do 
         end select 
      
      else
!
!-       case with an imposed type:
!            
         err = 0   
         select type (p => self%m)   
            type is (ik2_t)
!
!-             note: convert any numeric value to an integer number. For a logical value, 
!              convert to 1 if this value is .true. or to 0 if this value is .false.
!                                       
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  select case (elttyp)
                     case (ITYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) itmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                     case (RTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) rtmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        itmp = int(rtmp,Ikind)
                     case (CTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) ctmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        itmp = int(ctmp,Ikind)
                     case (LTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) ltmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        itmp = IZERO ; if ( ltmp ) itmp = IONE
                     case (STYP)
                        err = 2                     
                  end select  
                  p%v(irow,i) = itmp 
               end do   
               
            type is (rk2_t)
!
!-             note: convert any numeric value to a real number. For a logical value, 
!              convert to 1.0 if this value is .true. or to 0.0 if this value is .false.
!                                       
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  select case (elttyp)
                     case (ITYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) itmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        rtmp = real(itmp,Rkind)
                     case (RTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) rtmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                     case (CTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) ctmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        rtmp = real(ctmp,Rkind)
                     case (LTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) ltmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        rtmp = RZERO ; if ( ltmp ) rtmp = RONE
                     case (STYP)
                        err = 2                     
                  end select  
                  p%v(irow,i) = rtmp 
               end do   
               
            type is (ck2_t)
!
!-             note: convert any numeric value to a complex number. For a logical value, 
!              convert to (1,0) if this value is .true. or to (0,0) if this value is .false.
!                           
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  select case (elttyp)
                     case (ITYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) itmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        ctmp = cmplx(itmp,kind=Rkind)
                     case (RTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) rtmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        ctmp = cmplx(rtmp,kind=Rkind)
                     case (CTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) ctmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                     case (LTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) ltmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        ctmp = CZERO ; if ( ltmp ) ctmp = CONE
                     case (STYP)
                        err = 2                        
                  end select  
                  p%v(irow,i) = ctmp 
               end do   
               
            type is (lk2_t)
!
!-             note: for a numeric value, set to .true. if this value is /= 0
!            
               do i = 1, m
                  icol = col(i)
                  elttyp = pk2_DataType ( elts(icol,1)%str )
                  select case (elttyp)
                     case (ITYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) itmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        ltmp = LZERO ; if ( itmp /= IZERO ) ltmp = .true. 
                     case (RTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) rtmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        ltmp = LZERO ; if ( rtmp /= RZERO ) ltmp = .true. 
                     case (CTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) ctmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                        ltmp = LZERO ; if ( ctmp /= CZERO ) ltmp = .true. 
                     case (LTYP)
                        read(elts(icol,1)%str,*,iostat=ioerr,iomsg=iomsg) ltmp
                        if ( ioerr /= 0 ) then ; err = 1 ; exit ; end if 
                     case (STYP)
                        err = 2                        
                  end select  
                  p%v(irow,i) = ltmp 
               end do   
                  
            type is (sk2_t)
               do i = 1, m
                  icol = col(i)
                  str = trim(adjustl(elts(icol,1)%str)) ; lstr = len(str)
                  if ( (str(1:1) == '"' .and. str(lstr:lstr) == '"') .or. &
                       (str(1:1) == "'" .and. str(lstr:lstr) == "'")      ) then 
                     p%v(irow,i)%str = str(2:lstr-1)
                  else   
                     p%v(irow,i)%str = str(1:lstr)
                  end if
               end do
                           
         end select
      end if
      
      if ( err /= 0 ) then
         self = pk2_t()      
         if ( err == 1 ) then
            call opflag%Set(UERROR, HERE, "Problem during reading. I/O-msg: "//trim(iomsg))
            return
         else if ( err == 2 ) then
            call opflag%Set(UERROR, HERE, "Inconsitent types.") 
            return
         else if ( err == 3 ) then
            call opflag%Set(UERROR, HERE, "Missing quotes.")
            return
         end if
      end if       

      if ( irow == n ) exit
      
   end do 
!
!- Report an error if the eof has been reached while the number of rows read is less than the
!  number requested (nrow):
!
   if ( nrow > 0 .and. irow < nrow ) then
      self = pk2_t()
      write(c1,'(i0)')irow ; write(c2,'(i0)')nrow
      call opflag%Set(UERROR, HERE, NL //                                    &
                       '  . number of rows found in the file: '// trim(c1)   // NL // &
                       '  . number of rows requested        : '// trim(c2)            )
      return
   end if      
         
   END SUBROUTINE pk2_ReadMat2    


!=============================================================================================
   SUBROUTINE pk2_ReadMat3 ( self, unit, typ, nrow, ncol, colindx, comment )
!=============================================================================================
   use util_m, only: util_GetLine, util_CountTokens, util_StringLow
   class    (pk2_t),           intent(   out) :: self
   integer  (Ikind),           intent(in    ) :: unit, nrow
   character(len=*),           intent(in    ) :: typ
   integer  (Ikind), optional, intent(in    ) :: ncol, colindx(:)
   character(len=*), optional, intent(in    ) :: comment
!---------------------------------------------------------------------------------------------
!  Reads an array from a file where data are organized in columns with a known type.
!  
!   Inputs:
!
!     . nrow   : The number of rows to read (rows above "nrow" are ignored).
!                If nrow = -1 the read is done until the end of the file.
!
!     . ncol   : The number of columns (columns above "ncol" are ignored). 
!                If ncol = -1 the number of column is first determined based on the number of
!                tokens present in the first line of the file. 
!
!     . typ    : The type of data (typ = 'i': integers, typ = 'r': reals, typ = 'c': complexes,
!                typ = 'l': logicals, typ = 's': strings).
!
!     . unit   : (optional) must be a valid unit number of an already opened file ("fname" 
!                 must not be present).
!
!     . fname  : (optional) the name of the file ("unit" must not be present).
!
!     . comment: (optional) the symbol(s) used as a marker of comment in the first lines of the
!                file (header).
!
!  Ouputs:
!
!     . self  : the corresponding pk2 array.
!     . opflag: if an error is detected, opflag%code > 0 and opflag%msg contains the error
!               message.
!
!  Notes:
!
!  . The procedures pk2_?ReadMat4 are used when the columns of the file are all of the same 
!    type (e.g. all of integer type, or all of character type, ...) and this type is known at 
!    run-time. It is recommended to always use these procedures for these cases.
!
!  . If the read is not from stdin, the file may contain a header, i.e. any set of first lines 
!    that begin with a given symbol (the default is '#', otherwise, the symbol(s) given by 
!    "comment"). 
!
!  . No other commented-lines or blank lines may be inserted between lines of data.
!
!  Examples:
!
!  Assume the file 'foo.txt' contains only real data displayed in 4 columns and 10 rows with a
!  header of 3 lines:
!
!    // file foo.txt generated by xxxx
!    // date: xxxx
!    // t      X      Y      Z
!       0.1e0  0.0e0  0.0e0  0.0e0
!       0.2e0  1.0e0  0.0e0  0.0e0
!       0.3e0  2.0e0  0.0e0  0.0e0
!       ...
!       0.1e1  5.1e0  0.0e0  0.0e0
!
!  we can read these data with the following options:
!
!  . call pk2_ReadMat4 (A, nrow=10, ncol=4, typ='r', fname = 'foo.txt', comment='//')
!
!  . call pk2_ReadMat4 (A, nrow=10, ncol=2, typ='r', fname = 'foo.txt', comment='//')
!    (only the first 2 columns will be stored in A)
!
!  . call pk2_ReadMat4 (A, nrow=-1, ncol=4, typ='r', fname = 'foo.txt', comment='//')
!    (the number of rows will be determined during the read)

!  . call pk2_ReadMat4 (A, nrow=-1, ncol=-1, typ='r', fname = 'foo.txt', comment='//')
!    (the number of rows and the number of columns will be determined during the read)
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables ---------------------------------------------------------------------------           
   character(len=*), parameter :: HERE = "pk2_ReadMat3"
   character(len=:    ), allocatable :: line, com
   character(len=LGSTR)              :: iomsg, c1, c2   
   integer  (Ikind    )              :: i, j, n, m, m0, mtok, nheader, typm, maxcol, err
   integer  (Ikind    ), allocatable :: itmp(:), col(:)
   real     (Rkind    ), allocatable :: rtmp(:)
   complex  (Rkind    ), allocatable :: ctmp(:)
   logical             , allocatable :: ltmp(:)
   type     (str_t    ), allocatable :: stmp(:,:)
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !call opflag%set()  

   if ( present(ncol) ) then
      m = ncol
      if ( m == 0 ) return      
      maxcol = m
      if ( m > 0 ) then
         allocate(col(m)) ; do i=1,m ; col(i) = i ; end do
      end if    
   else if ( present(colindx) ) then
      m = size(colindx)
      if ( m == 0 ) return
      maxcol = maxval(colindx) 
      allocate(col(m), source = colindx)
   else
      call opflag%Set(UERROR, HERE, "Missing ncol or colindx")
      return
   end if   

   select case (util_StringLow(typ))
      case('i') ; typm = ITYP
      case('r') ; typm = RTYP
      case('c') ; typm = CTYP
      case('l') ; typm = LTYP
      case('s') ; typm = STYP
      case default
         call opflag%Set(UERROR, HERE, "Invalid type: "//typ)
         return
   end select                  
!
!- First, read the header if present (first lines of the file beginning with symbol '#' or
!  optionally with symbol(s) given in "comment" (if present)):
!
   com = '#'
   if ( present(comment) ) then
      if ( len_trim(comment) /= 0 ) com = comment
   end if
         
   nheader = 0
   
   if ( unit /= STDIN ) then
      do 
         read(unit,*,iostat=err,iomsg=iomsg) c1
         if ( err < 0 ) exit
         if ( err > 0 ) then
            call opflag%Set(UERROR, HERE, "Problem during reading. I/O-msg: "//trim(iomsg)) 
            return
         end if                                
         c2 = adjustl(c1)
         if ( index(c2,com) == 1 ) then
            nheader = nheader + 1
            cycle 
         else
            backspace(unit)
            exit
         end if      
      end do 
   end if
!
!- If the number of rows (n) of the array is not known (nrow=-1), find the remaining number of 
!  lines in the file (else n = nrow):
!
   if ( nrow ==-1 ) then
      n = 0
      do 
         read(unit,*,iostat=err,iomsg=iomsg) c1(1:1)
         if ( err < 0 ) then
            err = 0
            exit
         end if   
         if ( err > 0 ) then
            call opflag%Set(UERROR, HERE, "Problem during reading. I/O-msg: "//trim(iomsg))
            return
         end if                 
         n = n + 1
      end do   
      rewind(unit)
      do i = 1, nheader
         read(unit,*) c1(1:1)
      end do   
   else if ( nrow > 0 ) then
      n = nrow   
   else
      call opflag%Set(UERROR, HERE, "nrow must be > 0 or =-1")
      return
   end if
!
!- If the number of columns (m) of the array is not known (ncol=-1), read the first line then
!  count the number of tokens (assuming that the remaining lines contain the same number of
!  columns):
!
   line = util_GetLine ( unit, opflag ) ; error_TraceNreturn(opflag, HERE)
   
   m0 = util_CountTokens ( line, delims = ' ', opcl = '""()'//"''", BlkToken = .false., &
                            stat = opflag )
   error_TraceNreturn(opflag, HERE)
                            
   
   backspace(unit)

   if ( maxcol > 0 .and. maxcol > m0 ) then
      call opflag%Set(UERROR,HERE,"Insufficient number of columns found in the file") 
      return
   end if
      
   if ( maxcol ==-1 ) then
      m = m0 ; allocate(col(m)) ; do i = 1, m ; col(i) = i ; end do ; maxcol = m
   end if
             
   !self = pk2_t ( typ = typm, shape = [ n, m ] )
   call pk2_SetToType ( typ = typm, shape = [ n, m ], res = self )
   error_TraceNreturn(opflag, HERE)
   
   err = 0      
   select case (typm)
      case(ITYP) ; allocate(itmp(maxcol),stat=err)
      case(RTYP) ; allocate(rtmp(maxcol),stat=err)
      case(CTYP) ; allocate(ctmp(maxcol),stat=err)
      case(LTYP) ; allocate(ltmp(maxcol),stat=err)
   end select
   
   if ( err /= 0 ) then
      call opflag%Set(UERROR, HERE, "Allocation failure for ?tmp")
      return
   end if               

   select type(p=>self%m)
   
      type is (ik2_t)
         do i = 1, n
            read(unit,*,iostat=err,iomsg=iomsg)(itmp(j),j=1,maxcol)
            if ( err /= 0 ) exit
            do j = 1, m ; p%v(i,j) = itmp(col(j)) ; end do               
         end do

      type is (rk2_t)
         do i = 1, n
            read(unit,*,iostat=err,iomsg=iomsg)(rtmp(j),j=1,maxcol)
            if ( err /= 0 ) exit
            do j = 1, m ; p%v(i,j) = rtmp(col(j)) ; end do     
         end do

      type is (ck2_t)
         do i = 1, n
            read(unit,*,iostat=err,iomsg=iomsg)(ctmp(j),j=1,maxcol)
            if ( err /= 0 ) exit
            do j = 1, m ; p%v(i,j) = ctmp(col(j)) ; end do     
         end do

      type is (lk2_t)
         do i = 1, n
            read(unit,*,iostat=err,iomsg=iomsg)(ltmp(j),j=1,maxcol)
            if ( err /= 0 ) exit
            do j = 1, m ; p%v(i,j) = ltmp(col(j)) ; end do     
         end do

      type is (sk2_t)
         do i = 1, n
            !can not do that (because commas (as blanks) are also interpreted as delimiters)
            !read(unit,*,iostat=err,iomsg=iomsg)(buf(j),j=1,maxcol)
            !if (err /= 0) exit            
            !do j = 1, m ; p%v(i,j)%str = trim(adjustl(buf(col(j)))) ; end do
         
            line = util_GetLine ( unit, stat = opflag )
            
            if ( opflag%code > 0 ) then
               self = pk2_t()
               call opflag%addTrace(HERE)                
               return
            end if   
!
!-          split the current row in "mtok" sub-strings "stmp"
!       
            mtok = util_CountTokens ( line, delims = ' ', opcl = '""()'//"''",         &
                                      BlkToken = .false., tokens = stmp, stat = opflag )
            if ( opflag%code > 0 ) then
               self = pk2_t()
               call opflag%addTrace(HERE)  
               return              
            end if   
            
            if ( mtok /= m0 ) then
               self = pk2_t()
               call opflag%Set(UERROR, HERE, "Non-constant number of columns" )
               return
            end if
            if ( mtok < m ) then
               self = pk2_t()
               call opflag%Set(UERROR, HERE, "Insufficient number of columns" )
               return
            end if
            
            m0 = mtok

            do j = 1, m
               p%v(i,j)%str =  trim( adjustl( stmp(col(j),1)%str ) )
            end do
                     
         end do

   end select         
      
   if ( err /= 0 ) then
      self = pk2_t()
      call opflag%Set(UERROR, HERE, "Problem during reading. I/O-msg: "//trim(iomsg))
      return
   end if   
   
   if ( opflag%code < 0 ) call opflag%set()          
      
   END SUBROUTINE pk2_ReadMat3


!=============================================================================================
   FUNCTION pk2_DataType ( string ) result ( typ )
!=============================================================================================
   use util_m, only: util_stringlow
   character(len=*), intent(in) :: string
   integer  (Ikind)             :: typ
!---------------------------------------------------------------------------------------------
!  "Determines" the type (ITYP, RTYP, CTYP, LTYP or STYP) of the data stored in a string.
!
!  Notes:
!  . if string = " 1 1.2 1d3 .true. .f. 'hello' "world" (1,3) (1.0,3)" -->  typ = STYP
!  . if string = " 1 1.2 1d3 .true. .f. 'hello'         (1,3) (1.0,3)" -->  typ = STYP
!
!  . if string = " 1 1.2 1d3 .true. .f.                 (1,3) (1.0,3)" -->  typ = LTYP
!  . if string = " 1 1.2 1d3 .true.                     (1,3) (1.0,3)" -->  typ = LTYP
!
!  . if string = " 1 1.2 1d3                            (1,3) (1.0,3)" -->  typ = CTYP
!  . if string = " 1 1.2 1d3                            (1,3)        " -->  typ = CTYP
!
!  . if string = " 1 1.2 1d3                                         " -->  typ = RTYP
!  . if string = " 1 1.2                                             " -->  typ = RTYP
!
!  . if string = " 1 1 2                                             " -->  typ = ITYP
!
!  . if string = "                                                   " -->  typ = EMPTY
!
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables ---------------------------------------------------------------------------          
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------
   
   str = util_stringlow(string)
   
   if ( len_trim(str) == 0 ) then
      typ = EMPTY
   else if ( index(str,"'") /= 0 .or. index(str,'"') /= 0 ) then
      typ = STYP
   else if ( index(str,'.f') /= 0 .or. index(str,'.t') /= 0 ) then
      typ = LTYP
   else if ( index(str,'(') /= 0) then
      typ = CTYP
   else if ( index(str,'.') /= 0 .or. index(str,'e') /= 0 .or. index(str,'d' ) /= 0) then
      typ = RTYP
   else
      typ = ITYP
   end if               
   
   END FUNCTION pk2_DataType      


!=============================================================================================   
   SUBROUTINE pk2_WriteMat ( self, unit, fname, comment, title, format, quote, size, stat )
!=============================================================================================
   use util_m, only: util_GetUnit, util_ReplaceSubstring1, util_CountTokens
   class    (pk2_t),           intent(in    ) :: self
   integer  (Ikind), optional, intent(in    ) :: unit
   character(len=*), optional, intent(in    ) :: fname, title, format, comment, quote
   logical         , optional, intent(in    ) :: size
   type     (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Writes an array to a file
!
!      . unit    : If present ("fname" should not be present), must be a valid unit number
!                  of an already opened file. The caller has to close this file.
!
!      . fname   : If present ("unit" should not be present), the name of the file.
!                  The file is closed by this subroutine.
!
!      . comment : If present, the symbols to use for comments (default: '#').
!
!      . title   : If present, the title to be placed at the beginning of the file.
!
!      . format  : If present, a fortran format (otherwise default format will be used).
!
!      . quote   : If present, any symbol or pair of symbols used as quotation marks to 
!                  enclose the elements if the array is of string type (left mark = quote(1:1)
!                  rigth mark = left mark if len(quote)=1 and right mark = quote(2;2) if 
!                  len(quote) > 1). If len(quote) = 0, no quotation marks.
!                  If not present (default): no quotation marks.
!
!      . size    : If present and .false., do not print the size of the array at the beginning
!                  of the file. 
!                  If not present (default): print the size (after the title if "title" is
!                  present).
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables ---------------------------------------------------------------------------   
   character(len=*    ), parameter   :: HERE = 'pk2_WriteMat'
   integer  (Ikind    )              :: i, j, uo, nline, err
   character(len=:    ), allocatable :: com, fmt, buf
   character(len=1    )              :: lquote, rquote
   character(len=LGSTR)              :: iomsg   
   type     (str_t    ), allocatable :: lines(:,:)
   logical                           :: print_size, add_quote 
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !call opflag%set()    
!
!- Set the unit number (uo), open the file if required:
!   
   if ( present(unit) ) then
      uo = unit
   else if ( present(fname) ) then
      uo = util_GetUnit ()
      open (unit=uo, file=fname, status='replace', action='write', iostat=err, iomsg=iomsg)  
      if ( err /= 0 ) then
         call opflag%set( stat = UERROR, where = HERE, msg = &
                         "Problem during file openning. I/O-msg: "//trim(iomsg) )
         if ( present(stat) ) stat = opflag
         return
      end if
   else
      call opflag%set(stat=UERROR, where=HERE, msg="Give the unit number or the file name")
      if ( present(stat) ) stat = opflag
      return         
   end if    
!
!- Symbol for comments:
!   
   com = '#'
   if ( present(comment) ) then
      if ( len_trim(comment) /= 0 ) com = comment
   end if
!
!- Print the title:
!
   if ( present(title) ) then
      if ( len_trim(title) /= 0 ) then
         buf = util_ReplaceSubstring1 (title, '\n', char(10))
         nline = util_CountTokens ( buf, delims=char(10), BlkToken=.false., tokens=lines, &
                                    stat=opflag )
         error_TraceNreturn(opflag, HERE)
         do i = 1, nline
            if ( allocated(lines(i,1)%str) ) write(uo,'(a,1x,a)')com,(lines(i,1)%str)
         end do
      end if      
   end if   
!
!- Print the size of the array:
!   
   print_size = .true. ; if ( present(size) ) print_size = size       
   if ( print_size ) write(uo,'(a,i0,1x,i0)')com//" size (nrow,ncol): ",self%nrow,self%ncol
         
   if ( .not. allocated(self%m) ) then
      if ( present(fname) ) close(uo)
      return
   end if   
!
!- Define the format:
!
   fmt = ''     
   if ( present(format) ) fmt = util_ReplaceSubstring1 ( format, '""', '"') 
   
   if ( len_trim(fmt) == 0 ) then
      if ( self%typ == CTYP ) then
         fmt = '(*(:"(",g0,",",1x,g0,")",1x))'   
      else
         fmt = '(*(g0,1x))'   
      end if
   end if
!
!- Write the array row after row (test fmt on the 1st row):
!          
   err = 0 
   select type (p=>self%m)
      type is (ik2_t)
         if ( self%nrow >=1 ) write(uo,fmt,iostat=err,iomsg=iomsg)(p%v(1,j),j=1,self%ncol)
         if ( err == 0 ) then
            do i = 2, self%nrow
               write(uo,fmt)(p%v(i,j),j=1,self%ncol)
            end do   
         end if   
      type is (rk2_t)
         if ( self%nrow >=1 ) write(uo,fmt,iostat=err,iomsg=iomsg)(p%v(1,j),j=1,self%ncol) 
         if ( err == 0 ) then
            do i = 2, self%nrow
               write(uo,fmt)(p%v(i,j),j=1,self%ncol)
            end do   
         end if   
      type is (ck2_t)
         if ( self%nrow >=1 ) write(uo,fmt,iostat=err,iomsg=iomsg)(p%v(1,j),j=1,self%ncol) 
         if ( err == 0 ) then
            do i = 2, self%nrow
               write(uo,fmt)(p%v(i,j),j=1,self%ncol)
            end do   
         end if   
      type is (lk2_t)
         if ( self%nrow >=1 ) write(uo,fmt,iostat=err,iomsg=iomsg)(p%v(1,j),j=1,self%ncol) 
         if ( err == 0 ) then
            do i = 2, self%nrow
               write(uo,fmt)(p%v(i,j),j=1,self%ncol)
            end do   
         end if   
      type is (sk2_t)
!
!-       for string array, add quotation marks if "quote" is present
!      
         add_quote = .false.
         if ( present(quote) ) then
            if ( len_trim(quote) >= 1 ) then
               lquote = quote(1:1) ; rquote = lquote
               if ( len_trim(quote) > 1 ) rquote = quote(2:2) 
               add_quote = .true.
            end if
         end if      
         
         if ( add_quote ) then
            !humm... with ifort the following cause memory growth (to be re-tested with 19.1):  
                 
            !if (self%nrow >= 1) write(uo,fmt,iostat=err,iomsg=iomsg) &
            !                         (lquote//p%v(1,j)%str//rquote,j=1,self%ncol)
            !if (err == 0) then
            !   do i = 2, self%nrow
            !      write(uo,fmt)(lquote//p%v(i,j)%str//rquote,j=1,self%ncol)
            !   end do   
            !end if
            
            ! Workaround (but cannot use fmt):
            do i = 1, self%nrow
               buf = ''
               do j = 1, self%ncol
                  buf = buf // lquote // (p%v(1,j)%str) // rquote // ' '
               end do
               write(uo,'(a)',iostat=err,iomsg=iomsg) buf
            end do                

         else
            if ( self%nrow >= 1 ) write(uo,fmt,iostat=err,iomsg=iomsg) &
                                          (p%v(1,j)%str,j=1,self%ncol)
            if ( err == 0 ) then 
               do i = 2, self%nrow
                  write(uo,fmt)(p%v(i,j)%str,j=1,self%ncol)
               end do   
            end if
         end if    
   end select         

   if ( present(fname) ) close(uo)

   if ( err /= 0 ) then
      call opflag%set( stat = UERROR, where = HERE, &
                       msg = "Problem while writing. I/O-msg: "//trim(iomsg) )
      if ( present(stat) ) stat = opflag
   end if                 
                                       
   END SUBROUTINE pk2_WriteMat
   

!=============================================================================================   
   SUBROUTINE pk2_Print ( self, unit, dispstyle, msg, form )
!=============================================================================================   
   class    (pk2_t),           intent(in) :: self
   integer  (Ikind), optional, intent(in) :: unit
   character(len=*), optional, intent(in) :: msg, dispstyle, form
!---------------------------------------------------------------------------------------------
!  Prints a pk2 array on file of unit "unit"
!
!  When the default format is used (i.e. when "form" is not present or when form /= 'values'), 
!  it prints:
!       msg (the string "msg" if present)
!      'name  : ' (self%name if allocated)
!      'type  : ' self%typ (code of the type)
!      'size  : ' self%nrow 'x' self%ncol (the dimensions)
!      'values: ' the_values (call to self%m%printme)
!
!  When "form" is present and form == 'values', it prints only the values in the form:
!
!  . if "msg" is present:
!       msg the_values
!
!  . if "msg" is not present and self%name is allocated
!      self%name = the_values
!
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------           
   integer  (Ikind)              :: uo, opt
   character(len=:), allocatable :: str
!---------------------------------------------------------------------------------------------

   if ( present(unit) ) then
      uo = unit
   else
      uo = STDOUT
   end if    
   
   opt = 0
   if ( present(form))  then
      if ( form == 'values'   ) opt = 1
      if ( form == 'novalues' ) opt =-1
   end if
   
   write(uo,*)
      
   if ( opt <= 0 ) then
      if ( present(msg) ) write(uo,'(a)') msg
      if ( allocated(self%name) ) then
         write(uo,'(a)')'name  : '//trim(self%name)
      else
         write(uo,'(a)')'name  : (no name)'
      end if
      str = '(unknown)'
      if      ( self%typ == EMPTY) then
         str = 'empty'  
      else if ( self%typ == ITYP ) then
         str = 'integer'  
      else if ( self%typ == RTYP ) then
         str = 'real'  
      else if ( self%typ == CTYP ) then
         str = 'complex'  
      else if ( self%typ == LTYP ) then
         str = 'logical'  
      else if ( self%typ == STYP ) then
         str = 'string'
      end if 
      write(uo,'(a)')'type  : '//trim(str) 
      write(uo,'(a,i0,a,i0)')'size  : ',self%nrow,' x ',self%ncol
      if ( allocated(self%m) .and. opt == 0 ) &
         call self%m%PrintMe (msg = 'values: ',unit = uo, dispstyle = dispstyle)
   else
      if ( present(msg) ) then
         str = msg
      else
         if ( allocated(self%name) ) then
            str = self%name // ' = '
         else
            str = ''
         end if
      end if
      if ( len_trim(str) /= 0 ) then
         if ( allocated(self%m) ) then
            call self%m%PrintMe (msg = str ,unit = uo, dispstyle = dispstyle)
         else
            write(uo,'(a)')str // ' (empty)'
         end if      
      else   
         if ( allocated(self%m) ) then
            call self%m%PrintMe (unit = uo, dispstyle = dispstyle)
         else
            write(uo,'(a)')'(empty)'
         end if                  
      end if
   end if      
      
   END SUBROUTINE pk2_Print
      
               
!=============================================================================================   
   SUBROUTINE pk2_Affiche ( self, num )
!=============================================================================================   
   class  (pk2_t),           intent(in) :: self
   integer(Ikind), optional, intent(in) :: num
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------           
   logical :: noname
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > 0 ) then
      if ( opflag%code < 0 ) then
         print*,'--> Warning: '//trim( opflag%mesg )    
      else
         print*,'--> Error: '//trim( opflag%mesg )
      end if   
   end if      

   if ( present(num) ) then
      print '(a,i0)','variable #',num
   end if   
   
   noname = .true.
   if ( allocated(self%name) ) then
      if ( len_trim(self%name) /= 0 ) then
         noname = .false.
      end if
   end if

   if ( noname ) then
      print '(a)','name: (none)'
   else
      print '(a)','name: '// (self%name)
   end if 
   
   if ( self%typ /= EMPTY ) then              
      call self%m%affiche()
   else if ( noname ) then
      print '(a)','(unused)'
   else
      print '(a)','(empty)'
   end if
   
   END SUBROUTINE pk2_Affiche


!=============================================================================================   
   SUBROUTINE pk2_LU ( self, L, U, P, stat )
!=============================================================================================   
   use LapackInterface_m, only: LapackInterface_getrf
   class(pk2_t),           intent(in    ) :: self
   class(pk2_t),           intent(   out) :: L, U
   class(pk2_t), optional, intent(   out) :: P
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Computes an LU factorization of "self". 
!
!  The factorization has the form 
!                              self = P' * L * U   ( <=>  P * self = L * U )
!  where P' is a permutation matrix, L is lower triangular with unit diagonal elements (lower
!  trapezoidal if size(self,1) > size(self,2)), and U is upper triangular (upper trapezoidal
!  if size(self,1) < size(self,2)).
!
!  external routines:
!     . ?getrf (lapack)
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'pk2_LU'
   character(len=99)              :: cnum
   integer  (Ikind )              :: nrow, ncol, mindim, i, j, itmp
   integer  (Ikind ), allocatable :: row(:)   
!- needed by LAPACK routines ?getrf ----------------------------------------------------------
   integer                        :: n, m, info, typ, err
   real     (Rkind ), allocatable :: Rmat(:,:)
   complex  (Rkind ), allocatable :: Cmat(:,:)
   integer          , allocatable :: piv(:)
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( self%typ == EMPTY .or. self%nrow == 0 .or. self%ncol == 0 .or. &
        .not. allocated(self%m) ) return
   
   err = 0 
!
!- Make a copy (in Rmat or Cmat) of the matrix (lapack overwrite it by the result):
!
   select type (q=>self%m)
      type is (ik2_t)
         typ = 1
         nrow = q%nrow ; ncol = q%ncol ; mindim = min(nrow,ncol)
         allocate(Rmat(nrow,ncol), piv(mindim), stat = err)
         if ( err == 0 ) Rmat = real(q%v,kind=Rkind)
      type is (rk2_t)
         typ = 1
         nrow = q%nrow ; ncol = q%ncol ; mindim = min(nrow,ncol)
         allocate(Rmat(nrow,ncol), piv(mindim), stat = err)
         if ( err == 0 ) Rmat = q%v
      type is (ck2_t)
         typ = 2
         nrow = q%nrow ; ncol = q%ncol ; mindim = min(nrow,ncol)
         allocate(Cmat(nrow,ncol), piv(mindim), stat = err)
         if ( err == 0 ) Cmat = q%v
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg = &
                          "Bad type for << lu >> (integer, real or complex matrix expected)" )
         if ( present(stat) ) stat = opflag
         return
   end select
   
   if ( err /= 0 ) then
      call opflag%set(stat=UERROR, where=HERE, msg="Allocation failure for Rmat, Cmat or piv")
      if ( present(stat) ) stat = opflag
      return
   end if   
   
   n = nrow ; m = ncol
            
   if ( typ == 1 ) then
!
!-    Real case:
!   
      call LapackInterface_getrf ( n, m, Rmat, n, piv, info )
         
      if ( info >= 0 ) then
         call pk2_SetToType ( typ = RTYP, shape = [mindim,ncol], res = U )
         if ( opflag%code > 0 ) then
            call opflag%AddTrace(HERE)
            if ( present(stat) ) stat = opflag
            return
         end if
         call pk2_SetToType (typ = RTYP, shape = [nrow,mindim], res = L)
         if ( opflag%code > 0 ) then
            call opflag%AddTrace(HERE)
            if ( present(stat) ) stat = opflag
            return
         end if
      end if
      
   else
!
!-    Complex case:
!   
      call LapackInterface_getrf ( n, m, Cmat, n, piv, info )

      if ( info >= 0 ) then
         call pk2_SetToType (typ = CTYP, shape = [mindim,ncol], res = U)
         if ( opflag%code > 0 ) then
            call opflag%AddTrace(HERE)
            if ( present(stat) ) stat = opflag
            return
         end if
         call pk2_SetToType (typ = CTYP, shape = [nrow,mindim], res = L)
         if ( opflag%code > 0 ) then
            call opflag%AddTrace(HERE)
            if ( present(stat) ) stat = opflag
            return
         end if
      end if
      
   end if

   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call opflag%set ( stat = UERROR, where = HERE,                             &
                        msg = 'Message returned by "?getrf": the ' //            &
                               trim(cnum) // '-th argument had an illegal value' )
      if ( present(stat) ) stat = opflag
      return
   end if
!
!- Extract the lower triangular (or trapezoidal) matrix L and set its main diagonale elements
!  to unity:
!
   select type (q=>L%m)
      type is (rk2_t)
         do j = 1, mindim
            q%v(j,j) = RONE
            do i = j+1, nrow
               q%v(i,j) = Rmat(i,j)
            end do
         end do   
      type is (ck2_t)
         do j = 1, mindim
            q%v(j,j) = CONE
            do i = j+1, nrow
               q%v(i,j) = Cmat(i,j)
            end do
         end do   
   end select
!
!- Extract the upper triangular (or trapezoidal) matrix U:

   select type (q=>U%m)
      type is (rk2_t)   
         do j = 1, ncol
            do i = 1, min(j,mindim)
               q%v(i,j) = Rmat(i,j)
            end do
         end do  
      type is (ck2_t)
         do j = 1, ncol
            do i = 1, min(j,mindim)
               q%v(i,j) = Cmat(i,j)
            end do
         end do           
   end select

   if ( .not. present(P) ) return
!         
!- Form the permutation matrix P' if requested:
!      
   allocate(row(nrow), stat = err) 
   if ( err /= 0 ) then
      call opflag%set (stat = UERROR, where = HERE, msg = 'Allocation failure for "row"')
      if ( present(stat) ) stat = opflag
      return
   end if      
   
   do i = 1, nrow
      row(i) = i
   end do
   
   do i = 1, mindim
      j = piv(i)
      itmp = row(j) ; row(j) = row(i) ; row(i) = itmp
   end do   

   call pk2_SetToType (typ = ITYP, shape = [nrow,nrow], res = P)
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace(HERE)
      if ( present(stat) ) stat = opflag
      return
   end if
         
   select type (q=>P%m)     
      type is (ik2_t)
         do i = 1, nrow ; q%v(i,row(i)) = IONE ; end do
   end select
   
   END SUBROUTINE pk2_LU


!=============================================================================================   
   SUBROUTINE pk2_LU2 ( self, L, U, P, h, stat )
!=============================================================================================  
   use LapackInterface_m, only: LapackInterface_getrf 
   class(pk2_t),           intent(in    ) :: self
   class(pk2_t), optional, intent(   out) :: L, U, P, h
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Computes an LU factorization of "self". 
!
!  The factorization has the form 
!                              self = P' * L * U   ( <=>  P * self = L * U )
!  where P' is a permutation matrix, L is lower triangular with unit diagonal elements (lower
!  trapezoidal if size(self,1) > size(self,2)), and U is upper triangular (upper trapezoidal
!  if size(self,1) < size(self,2)).
!
!  external routines:
!     . ?getrf (lapack)
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'pk2_LU2'
   character(len=99)              :: cnum
   integer  (Ikind )              :: nrow, ncol, mindim, i, j, itmp
   integer  (Ikind ), allocatable :: row(:)   
!- needed by LAPACK routines ?getrf ----------------------------------------------------------
   integer                        :: n, m, info, typ, opt, err
   real     (Rkind ), allocatable :: Rmat(:,:)
   complex  (Rkind ), allocatable :: Cmat(:,:)
   integer          , allocatable :: piv(:,:)
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( self%typ == EMPTY .or. self%nrow == 0 .or. self%ncol == 0 .or. &
        .not. allocated(self%m) ) return

   if ( present(L) .and. present(U) .and. .not.present(h) ) then
      opt = 0
   else if ( .not.present(L) .and. .not.present(U) .and. &
             .not.present(P) .and. present(h)            ) then
      opt = 1
   else 
      call opflag%set (stat = UERROR, where = HERE, msg = "Bad combination of arguments")
      if ( present(stat) ) stat = opflag
      return
   end if  
         
   err = 0 
      
   select type (q=>self%m)
      type is (ik2_t)
         typ = 1
         nrow = q%nrow ; ncol = q%ncol ; mindim = min(nrow,ncol)
         allocate(Rmat(nrow,ncol), piv(mindim,1), stat = err)
         if ( err == 0 ) Rmat(:,:) = real(q%v(:,:),kind=Rkind) 
      type is (rk2_t)
         typ = 1
         nrow = q%nrow ; ncol = q%ncol ; mindim = min(nrow,ncol)
         allocate(Rmat(nrow,ncol), source = q%v, stat = err)
         if ( err == 0 ) allocate(piv(mindim,1), stat = err)
      type is (ck2_t)
         typ = 2
         nrow = q%nrow ; ncol = q%ncol ; mindim = min(nrow,ncol)
         allocate(Cmat(nrow,ncol), source = q%v, stat = err)
         if ( err == 0 ) allocate(piv(mindim,1), stat = err)
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg = &
                          "Bad type for << lu >> (integer, real or complex matrix expected)" )
         if ( present(stat) ) stat = opflag
         return
   end select
   
   if ( err /= 0 ) then
      call opflag%set(stat=UERROR, where=HERE, msg=" Allocation failure for Rmat, Cmat or piv")
      if ( present(stat) ) stat = opflag
      return
   end if   
   
   n = nrow ; m = ncol
            
   if ( typ == 1 ) then
!
!-    Real case:
!   
      call LapackInterface_getrf ( n, m, Rmat, n, piv(:,1), info ) !works w/ Rkind=4 or =8
      
      if ( opt == 1 ) then
         call pk2_MoveAlloc ( from = Rmat, to = h    ) ! possible w/ Rkind=4 or =8
         call pk2_Insert ( insert = piv, into = h, at_col = ncol+1, stat = opflag ) 
         if ( opflag%code > 0 ) then
            call opflag%addTrace(HERE)
            if ( present(stat) ) stat = opflag
         end if
         return
      end if   
                  
      if ( info >= 0 ) then
         call pk2_SetToType (typ = RTYP, shape = [mindim,ncol], res = U)
         if ( opflag%code > 0 ) then
            call opflag%addTrace(HERE)
            if ( present(stat) ) stat = opflag
            return
         end if
         call pk2_SetToType (typ = RTYP, shape = [nrow,mindim], res = L)
         if ( opflag%code > 0 ) then
            call opflag%addTrace(HERE)
            if ( present(stat) ) stat = opflag
            return
         end if
      end if
      
   else
!
!-    Complex case:
!   
      call LapackInterface_getrf ( n, m, Cmat, n, piv(:,1), info )

      if ( opt == 1 ) then
         call pk2_MoveAlloc ( from = Cmat, to = h   )
         call pk2_Insert ( insert = piv, into = h, at_col = ncol+1, stat = opflag ) 
         if ( opflag%code > 0 ) then
            call opflag%addTrace(HERE)
            if ( present(stat) ) stat = opflag
         end if
         return
      end if   

      if ( info >= 0 ) then
         call pk2_SetToType (typ = CTYP, shape = [mindim,ncol], res = U)
         if ( opflag%code > 0 ) then
            call opflag%addTrace(HERE)
            if ( present(stat) ) stat = opflag
            return
         end if
         call pk2_SetToType (typ = CTYP, shape = [nrow,mindim], res = L)
         if ( opflag%code > 0 ) then
            call opflag%addTrace(HERE)
            if ( present(stat) ) stat = opflag
            return
         end if
      end if
      
   end if

   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call opflag%set ( stat = UERROR, where = HERE,                           &
                        msg = 'Message returned by "?getrf": the ' //          &
                              trim(cnum)// '-th argument had an illegal value' )
      if ( present(stat) ) stat = opflag
      return
   end if
!
!- Extract the lower triangular (or trapezoidal) matrix L and set its main diagonale elements
!  to unity:
!
   select type (q=>L%m)
      type is (rk2_t)
         do j = 1, mindim
            q%v(j,j) = RONE
            do i = j+1, nrow
               q%v(i,j) = Rmat(i,j)
            end do
         end do   
      type is (ck2_t)
         do j = 1, mindim
            q%v(j,j) = CONE
            do i = j+1, nrow
               q%v(i,j) = Cmat(i,j)
            end do
         end do   
   end select
!
!- Extract the upper triangular (or trapezoidal) matrix U:

   select type (q=>U%m)
      type is (rk2_t)   
         do j = 1, ncol
            do i = 1, min(j,mindim)
               q%v(i,j) = Rmat(i,j)
            end do
         end do  
      type is (ck2_t)
         do j = 1, ncol
            do i = 1, min(j,mindim)
               q%v(i,j) = Cmat(i,j)
            end do
         end do           
   end select

   if ( .not. present(P) ) return
!         
!- Form the permutation matrix P' if requested:
!      
   allocate(row(nrow), stat = err) 
   if ( err /= 0 ) then
      call opflag%set (stat = UERROR, where = HERE, msg = 'Allocation failure for "row"')
      if ( present(stat) ) stat = opflag
      return
   end if      
   
   do i = 1, nrow
      row(i) = i
   end do
   
   do i = 1, mindim
      j = piv(i,1)
      itmp = row(j) ; row(j) = row(i) ; row(i) = itmp
   end do   

   call pk2_SetToType (typ = ITYP, shape = [nrow,nrow], res = P)
   if ( opflag%code > 0 ) return
   
   select type (q=>P%m)     
      type is (ik2_t)
         do i = 1, nrow ; q%v(i,row(i)) = IONE ; end do
   end select
   
   END SUBROUTINE pk2_LU2


!=============================================================================================
   SUBROUTINE pk2_LUsolv ( self, handl, rhs, stat )
!=============================================================================================
   use LapackInterface_m, only: LapackInterface_getrs 
   class(pk2_t),           intent(in out) :: self
   class(pk2_t),           intent(in    ) :: handl, rhs
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Computes the solution "self" of a linear system. 
!  The matrix must have been preivously LU-factorized by a call to the pk2_LU2. "handl" must
!  contain the result of this factorization and each column of "rhs" corresponds to a rhs of 
!  the linear system.    
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------
   character(len=* ), parameter   :: HERE = 'pk2_LUsolv'
   character(len=99)              :: cnum
!- needed by LAPACK routines ?getrs ----------------------------------------------------------
   integer                        :: nrow, ncol, nrhs, info, typ, i, j, err
   real     (Rkind ), allocatable :: Rmat(:,:), Rrhs(:,:)
   complex  (Rkind ), allocatable :: Cmat(:,:), Crhs(:,:)
   integer          , allocatable :: piv(:)
!---------------------------------------------------------------------------------------------
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nrow = handl%nrow ; ncol = handl%ncol
   if ( ncol /= nrow + 1 ) then
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       "The matrix must be square and first factorized by the function lu" )
      if ( present(stat) ) stat = opflag
      return
   end if
   
   if ( rhs%nrow /= nrow ) then
      call opflag%set (stat = UERROR, where = HERE, msg = "Incompatible shapes")
      if ( present(stat) ) stat = opflag
      return
   end if
   
   nrhs = rhs%ncol
   
   typ = max(handl%typ, rhs%typ)
   
   if ( typ <= ITYP .or. typ > CTYP ) then
      call opflag%set (stat = UERROR, where = HERE, msg = "Bad matrix type")
      if ( present(stat) ) stat = opflag
      return
   end if
   
   err = 0
   
   select type (p=>rhs%m)
      type is (ik2_t)
         if ( typ == RTYP ) then
            allocate(Rrhs(nrow,nrhs), stat = err)
            if ( err == 0 ) Rrhs(:,:) = real(p%v(:,:),kind=Rkind)
         else
            allocate(Crhs(nrow,nrhs), stat = err)
            if ( err == 0 ) Crhs(:,:) = cmplx(p%v(:,:),kind=Rkind)
         end if
      type is (rk2_t)   
         if ( typ == RTYP ) then
            allocate(Rrhs(nrow,nrhs), source = p%v, stat = err)
         else
            allocate(Crhs(nrow,nrhs), stat = err)
            if ( err == 0 ) Crhs(:,:) = cmplx(p%v(:,:),kind=Rkind)
         end if
      type is (ck2_t)   
         allocate(Crhs(nrow,nrhs), source = p%v, stat = err)
   end select
   
   if ( err /= 0 ) then
      call opflag%set(stat = UERROR, where = HERE, msg = "Allocation failure for Rhs or Crhs")
      if ( present(stat) ) stat = opflag
      return
   end if
   
   select type (p=>handl%m)
      type is (rk2_t)
         if ( typ == RTYP ) then
            allocate(Rmat(nrow,nrow), stat = err)
            if ( err == 0 ) allocate(piv(nrow), stat = err)
            if ( err == 0 ) then
               do j = 1, nrow
                  do i = 1, nrow
                     Rmat(i,j) = p%v(i,j)
                  end do
                  piv(j) = int(p%v(j,ncol))
               end do
            end if
         else   
            allocate(Cmat(nrow,nrow), stat = err)
            if ( err == 0 ) allocate(piv(nrow), stat = err)
            if ( err == 0 ) then
               do j = 1, nrow
                  do i = 1, nrow
                     Cmat(i,j) = p%v(i,j)
                  end do
                  piv(j) = int(p%v(j,ncol))
               end do
            end if
         end if
      type is (ck2_t)
         allocate(Cmat(nrow,nrow), stat = err)
         if ( err == 0 ) allocate(piv(nrow), stat = err)
         if ( err == 0 ) then
            do j = 1, nrow
               do i = 1, nrow
                  Cmat(i,j) = p%v(i,j)
               end do
               piv(j) = int(p%v(j,ncol))
            end do 
         end if        
   end select      
   
   if ( err /= 0 ) then
      call opflag%set(stat=UERROR, where=HERE, msg="Allocation failure for Rmat, Cmat or piv")
      if ( present(stat) ) stat = opflag
      return
   end if
   
   if ( typ == RTYP ) then
      call LapackInterface_getrs('No transpose', nrow, nrhs, Rmat, nrow, piv, Rrhs, nrow, info)      
      call pk2_movealloc (from = Rrhs, to = self)
   else            
      call LapackInterface_getrs('No transpose', nrow, nrhs, Cmat, nrow, piv, Crhs, nrow, info)
      call pk2_movealloc (from = Crhs, to = self)
   end if
   
   if ( info < 0 ) then
      write(cnum,'(i0)') -info
      call opflag%set ( stat = UERROR, where = HERE, msg = &
            'Returned by "?getrs": The '// trim(cnum)// '-th argument had an illegal value' )
      if ( present(stat) ) stat = opflag
      return
   end if
           
   END SUBROUTINE pk2_LUsolv
   
   
!=============================================================================================   
   SUBROUTINE pk2_INV ( self, a, stat )
!=============================================================================================  
   class(pk2_t),           intent(in out) :: self
   class(pk2_t),           intent(in    ) :: a
   type (err_t), optional, intent(in out) :: stat
!---------------------------------------------------------------------------------------------
!  Computes in "self" the inverse of "a". 
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'pk2_INV'
   character(len=99)              :: cnum
   integer                        :: typ
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. a%nrow == 0 .or. a%ncol == 0 .or. .not. allocated(a%m) ) return
   
   if ( a%typ /= ITYP .and. a%typ /= RTYP .and. a%typ /= CTYP ) then
      call opflag%set ( stat = UERROR, where = HERE, msg = &
                       "An integer, a real or a complex matrix expected for << inv >>" )
      if ( present(stat) ) stat = opflag
      return
   end if  

   if ( a%nrow /= a%ncol ) then
      call opflag%set(stat=UERROR, where=HERE, msg="A square matrix is expected for << inv >>")
      if ( present(stat) ) stat = opflag
      return
   end if          
!
!- first copy "a" into "self":
!
   call pk2_assign ( lhs = self, rhs = a )
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace(HERE)
      if ( present(stat) ) stat = opflag
   end if   
!
!- if "self" is an integer matrix convert it to a real one:
!  
   if ( self%typ == ITYP ) then 
      call bk2_ConvertToRk2 ( self%m )
      if ( opflag%code > 0 ) then
         call opflag%AddTrace(HERE)
         if ( present(stat) ) stat = opflag
      end if  
      self%typ = RTYP
   end if
!
!- inverse "self":
!   
   select type ( p=>self%m )         
      type is (rk2_t)
         call util_InvMats ( p%v, stat=opflag )
      type is (ck2_t)
         call util_InvMats ( p%v, stat=opflag )
   end select

   if ( opflag%code > 0 ) then
      call opflag%AddTrace(HERE)
      if ( present(stat) ) stat = opflag
   end if
   
   END SUBROUTINE pk2_INV
    
END MODULE pk2_m
