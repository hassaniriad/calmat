!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: pk2f
!
! Description: 
! This module contains a set of functions and subroutines acting on one or more pk2 variables.
!
! Naming convention: "pk2f_Name" (function version), "pk2f_subName" (subroutine version).
!                     and for generics: "name" and "s_name"
!---------------------------------------------------------------------------------------------
#include "error.fpp"

MODULE pk2f_m

!- module of functions acting on pk2 variables

   use pk2_m

   implicit none

   real(Rkind), parameter, private :: DEG2RAD = 1.745329251994330e-02_Rkind, & != pi / 180
                                      RAD2DEG = 5.729577951308232e+01_Rkind, & != 180 / pi
                                      ILOGTEN = 4.342944819032518e-01_Rkind    != 1 / log(10)                                      
!
!- Generic interface for some procedures:
!

!- Identity matrix:
   interface eye
      module procedure pk2f_eye, pk2f_eyenm, pk2f_eyewrp
   end interface 
   interface s_eye
      module procedure pk2f_subeye, pk2f_subeyenm, pk2f_subeyewrp
   end interface 

!- array of zeros:
   interface zeros
      module procedure pk2f_zeros, pk2f_zerosnm, pk2f_zeroswrp
   end interface 
   interface s_zeros
      module procedure pk2f_subzeros, pk2f_subzerosnm, pk2f_subzeroswrp
   end interface 

!- array of ones:
   interface ones
      module procedure pk2f_ones, pk2f_onesnm, pk2f_oneswrp
   end interface 
   interface s_ones
      module procedure pk2f_subones, pk2f_subonesnm, pk2f_suboneswrp
   end interface 

!- array of "falses":
   interface falses
      module procedure pk2f_falses, pk2f_falsesnm, pk2f_falseswrp
   end interface 
   interface s_falses
      module procedure pk2f_subfalses, pk2f_subfalsesnm, pk2f_subfalseswrp
   end interface    

!- array of real random numbers:
   interface rand
      module procedure pk2f_rand, pk2f_randnm, pk2f_randwrp
   end interface 
   interface s_rand
      module procedure pk2f_subrand, pk2f_subrandnm, pk2f_subrandwrp
   end interface 

!- array of integer random numbers:
   interface randi   ; module procedure pk2f_randinm   , pk2f_randiwrp    ; end interface  
   interface s_randi ; module procedure pk2f_subrandinm, pk2f_subrandiwrp ; end interface  

!- random permutation:
   interface randperm  ; module procedure pk2f_randpermn   , pk2f_randpermwrp   ; end interface  
   interface s_randperm; module procedure pk2f_subrandpermn, pk2f_subrandpermwrp; end interface  

!- magic square:
   interface magic   ; module procedure pk2f_magic   , pk2f_magicn    ; end interface  
   interface s_magic ; module procedure pk2f_submagic, pk2f_submagicn ; end interface 

!- norms:
   interface norm   ; module procedure pk2f_norm   , pk2f_normwrp    ; end interface 
   interface s_norm ; module procedure pk2f_subnorm, pk2f_subnormwrp ; end interface

!- min., max.:
   interface min   ; module procedure pk2f_min   , pk2f_minwrp    ; end interface 
   interface s_min ; module procedure pk2f_submin, pk2f_subminwrp ; end interface 
   interface max   ; module procedure pk2f_max   , pk2f_maxwrp    ; end interface 
   interface s_max ; module procedure pk2f_submax, pk2f_submaxwrp ; end interface 

   interface s_minmax ; module procedure pk2f_subminmax, pk2f_subminmaxwrp ; end interface 

!- mean:
   interface mean   ; module procedure pk2f_mean   , pk2f_meanwrp    ; end interface 
   interface s_mean ; module procedure pk2f_submean, pk2f_submeanwrp ; end interface 

!- sum of the elements of an array:
   interface sum   ; module procedure pk2f_sum   , pk2f_sumwrp    ; end interface 
   interface s_sum ; module procedure pk2f_subsum, pk2f_subsumwrp ; end interface 

!- trace and dev:
   interface trace   ; module procedure pk2f_trace    ; end interface
   interface s_trace ; module procedure pk2f_subtrace ; end interface
   interface dev     ; module procedure pk2f_dev      ; end interface  
   interface s_dev   ; module procedure pk2f_subdev   ; end interface
   
!- product of the elements of an array:
   interface prod   ; module procedure pk2f_prod   , pk2f_prodwrp    ; end interface 
   interface s_prod ; module procedure pk2f_subprod, pk2f_subprodwrp ; end interface 

!- extraction of a diagonal or creation of a diagonal matrix:
   interface diag
      module procedure pk2f_diag    , pk2f_diagn   , pk2f_diagwrp ,  &
                       pk2f_setidiag, pk2f_setrdiag, pk2f_setcdiag,  &
                       pk2f_setldiag, pk2f_setsdiag,                 &
                       pk2f_getidiag, pk2f_getrdiag, pk2f_getcdiag,  &
                       pk2f_getldiag, pk2f_getsdiag                         
   end interface 
   interface s_diag
      module procedure pk2f_subdiag    , pk2f_subdiagn   , pk2f_subdiagwrp ,  &
                       pk2f_subsetidiag, pk2f_subsetrdiag, pk2f_subsetcdiag,  &
                       pk2f_subsetldiag, pk2f_subsetsdiag,                    &
                       pk2f_subgetidiag, pk2f_subgetrdiag, pk2f_subgetcdiag,  &
                       pk2f_subgetldiag, pk2f_subgetsdiag                         
   end interface

!- extraction of lower triangle part of a matrix:
   interface tril
      module procedure pk2f_tril    , pk2f_triln   , pk2f_trilwrp , &
                       pk2f_getitril, pk2f_getrtril, pk2f_getctril, &
                       pk2f_getltril, pk2f_getstril      
   end interface 
   interface s_tril
      module procedure pk2f_subtril    , pk2f_subtriln   , pk2f_subtrilwrp , &
                       pk2f_subgetitril, pk2f_subgetrtril, pk2f_subgetctril, &
                       pk2f_subgetltril, pk2f_subgetstril      
   end interface 

!- extraction of upper triangle part of a matrix:
   interface triu
      module procedure pk2f_triu    , pk2f_triun   , pk2f_triuwrp , &
                       pk2f_getitriu, pk2f_getrtriu, pk2f_getctriu, &
                       pk2f_getltriu, pk2f_getstriu
   end interface  
   interface s_triu
      module procedure pk2f_subtriu    , pk2f_subtriun   , pk2f_subtriuwrp , &
                       pk2f_subgetitriu, pk2f_subgetrtriu, pk2f_subgetctriu, &
                       pk2f_subgetltriu, pk2f_subgetstriu
   end interface

!- generation of linearly spaced numbers:
   interface colon
      module procedure pk2f_colon, pk2f_coloni, pk2f_colonr, pk2f_colonwrp
   end interface  
   interface s_colon
      module procedure pk2f_subcolon, pk2f_subcoloni, pk2f_subcolonr, pk2f_subcolonwrp
   end interface 
   
   interface linspace
      module procedure pk2f_linspace, pk2f_linspacewrp
   end interface  
   interface s_linspace
      module procedure pk2f_sublinspace, pk2f_sublinspacewrp
   end interface    

!- array reshaping:
   interface reshape
      module procedure pk2f_reshape, pk2f_reshapenm, pk2f_reshapewrp
   end interface   
   interface s_reshape
      module procedure pk2f_subreshape, pk2f_subreshapenm, pk2f_subreshapewrp
   end interface
   
!- finding non-zero (non-false) element indices:
   interface find   ; module procedure pk2f_find    ; end interface   
   interface s_find ; module procedure pk2f_subfind ; end interface   
   
!  Transposition:
   interface trans   ; module procedure pk2f_trans    ; end interface
   interface s_trans ; module procedure pk2f_subtrans ; end interface
   interface transp  ; module procedure pk2f_transp   ; end interface

!- Eigenvalues / Eigenvectors:
   interface eig   ; module procedure pk2f_eig    ; end interface
   interface s_eig ; module procedure pk2f_subeig ; end interface

!- Singular values decomposition:
   interface svd   ; module procedure pk2f_svd    ; end interface
   interface s_svd ; module procedure pk2f_subsvd ; end interface

!- Polar decomposition:
   interface poldec   ; module procedure pk2f_poldec    ; end interface
   interface s_poldec ; module procedure pk2f_subpoldec ; end interface

!- LU factorization:
   interface lu   ; module procedure pk2f_lu    ; end interface
   interface s_lu ; module procedure pk2f_sublu ; end interface

!- Linear solve:
   interface mldivide   ; module procedure pk2f_mldivide    ; end interface
   interface s_mldivide ; module procedure pk2f_submldivide ; end interface
   interface lusolv     ; module procedure pk2f_lusolv      ; end interface
   interface s_lusolv   ; module procedure pk2f_sublusolv   ; end interface

!- Determinant:
   interface det   ; module procedure pk2f_det    ; end interface
   interface s_det ; module procedure pk2f_subdet ; end interface
   
!- Inverse:
   interface inv   ; module procedure pk2f_inv    ; end interface
   interface s_inv ; module procedure pk2f_subinv ; end interface
!- Cross product:
   interface cross   ; module procedure pk2f_cross    ; end interface
   interface s_cross ; module procedure pk2f_subcross ; end interface

!- Assembling matrices:
   interface mergemats   ; module procedure pk2f_mergemats    ; end interface
   interface s_mergemats ; module procedure pk2f_submergemats ; end interface

!- Sub-array extraction:
   interface extracmat
      module procedure pk2f_extracmat  , pk2f_extracmati, pk2f_extracmatij, &
                       pk2f_extracmatil, pk2f_extracmatl, pk2f_extracmatli, &
                       pk2f_extracmatll
   end interface
   interface s_extracmat
      module procedure pk2f_subextracmat  , pk2f_subextracmati, pk2f_subextracmatij, &
                       pk2f_subextracmatil, pk2f_subextracmatl, pk2f_subextracmatli, &
                       pk2f_subextracmatll
   end interface

!- Sorting:
   interface sort   ; module procedure pk2f_sortn   , pk2f_sortwrp    ; end interface
   interface s_sort ; module procedure pk2f_subsortn, pk2f_subsortwrp ; end interface

!- All and Any functions:
   interface all   ; module procedure pk2f_all   , pk2f_allwrp    ; end interface
   interface s_all ; module procedure pk2f_suball, pk2f_suballwrp ; end interface
   interface any   ; module procedure pk2f_any   , pk2f_anywrp    ; end interface  
   interface s_any ; module procedure pk2f_subany, pk2f_subanywrp ; end interface      

!- covariance matrix:
   interface cov   ; module procedure pk2f_covn   , pk2f_covwrp    ; end interface  
   interface s_cov ; module procedure pk2f_subcovn, pk2f_subcovwrp ; end interface  

!- lowercase/uppercase conversions:
   interface convstr ; 
      module procedure pk2f_convstr, pk2f_convstrn, pk2f_convstrwrp
   end interface  
   interface s_convstr
      module procedure pk2f_subconvstr, pk2f_subconvstrn, pk2f_subconvstrwrp
   end interface  
   
!- Sub-string replacement:
   interface replace   ; module procedure pk2f_replace   , pk2f_replacewrp    ; end interface  
   interface s_replace ; module procedure pk2f_subreplace, pk2f_subreplacewrp ; end interface  
   
!- extraction of characters in a string:
   interface part   ; module procedure pk2f_part   , pk2f_partn    ; end interface  
   interface s_part ; module procedure pk2f_subpart, pk2f_subpartn ; end interface  

!- date:
   interface date
      module procedure pk2f_date, pk2f_datestr, pk2f_datewrp
   end interface 
   interface s_date
      module procedure pk2f_subdate, pk2f_subdatestr, pk2f_subdatewrp
   end interface 

!- reading / writting array:
   interface readmat  ; module procedure pk2f_readmatn   , pk2f_readmatwrp   ; end interface    
   interface s_readmat; module procedure pk2f_subreadmatn, pk2f_subreadmatwrp; end interface        
   
   interface writemat   ; module procedure pk2f_writemat    ; end interface  
   interface s_writemat ; module procedure pk2f_subwritemat ;  end interface    

!- mesh grid generation:
   interface meshgrid   ; module procedure pk2f_meshgridwrp    ;  end interface       
   interface s_meshgrid ; module procedure pk2f_submeshgridwrp ;  end interface       

!- trapezoidal rule:
   interface inttrap   ; module procedure pk2f_inttrap    ; end interface   
   interface s_inttrap ; module procedure pk2f_subinttrap ; end interface   

!- linear regression:
   interface reglin   ; module procedure pk2f_reglin    ; end interface   
   interface s_reglin ; module procedure pk2f_subreglin ; end interface   

!- working directory:
   interface pwd   ; module procedure pk2f_pwd    ; end interface   
   interface s_pwd ; module procedure pk2f_subpwd ; end interface
   
!- cpu time:
   interface cputime   ; module procedure pk2f_cputime    ; end interface   
   interface s_cputime ; module procedure pk2f_subcputime ; end interface
   interface tictoc    ; module procedure pk2f_tictoc     ; end interface   

!- usual functions:
   interface abs      ; module procedure pk2f_abs       ; end interface   
   interface s_abs    ; module procedure pk2f_subabs    ; end interface   
   
   interface sin      ; module procedure pk2f_sin       ; end interface   
   interface s_sin    ; module procedure pk2f_subsin    ; end interface   
   interface cos      ; module procedure pk2f_cos       ; end interface   
   interface s_cos    ; module procedure pk2f_subcos    ; end interface   
   interface tan      ; module procedure pk2f_tan       ; end interface   
   interface s_tan    ; module procedure pk2f_subtan    ; end interface   
   
   interface sind     ; module procedure pk2f_sind      ; end interface   
   interface s_sind   ; module procedure pk2f_subsind   ; end interface   
   interface cosd     ; module procedure pk2f_cosd      ; end interface   
   interface s_cosd   ; module procedure pk2f_subcosd   ; end interface   
   interface tand     ; module procedure pk2f_tand      ; end interface   
   interface s_tand   ; module procedure pk2f_subtand   ; end interface   

   interface asin     ; module procedure pk2f_asin      ; end interface   
   interface s_asin   ; module procedure pk2f_subasin   ; end interface   
   interface acos     ; module procedure pk2f_acos      ; end interface   
   interface s_acos   ; module procedure pk2f_subacos   ; end interface   
   interface atan     ; module procedure pk2f_atan      ; end interface   
   interface s_atan   ; module procedure pk2f_subatan   ; end interface   

   interface asind    ; module procedure pk2f_asind     ; end interface   
   interface s_asind  ; module procedure pk2f_subasind  ; end interface   
   interface acosd    ; module procedure pk2f_acosd     ; end interface   
   interface s_acosd  ; module procedure pk2f_subacosd  ; end interface   
   interface atand    ; module procedure pk2f_atand     ; end interface   
   interface s_atand  ; module procedure pk2f_subatand  ; end interface   
   
   interface sinh     ; module procedure pk2f_sinh      ; end interface   
   interface s_sinh   ; module procedure pk2f_subsinh   ; end interface   
   interface cosh     ; module procedure pk2f_cosh      ; end interface   
   interface s_cosh   ; module procedure pk2f_subcosh   ; end interface   
   interface tanh     ; module procedure pk2f_tanh      ; end interface   
   interface s_tanh   ; module procedure pk2f_subtanh   ; end interface  

   interface sinc     ; module procedure pk2f_sinc      ; end interface   
   interface s_sinc   ; module procedure pk2f_subsinc   ; end interface   
 
   interface log      ; module procedure pk2f_log       ; end interface   
   interface s_log    ; module procedure pk2f_sublog    ; end interface   
   interface log10    ; module procedure pk2f_log10     ; end interface   
   interface s_log10  ; module procedure pk2f_sublog10  ; end interface   

   interface exp      ; module procedure pk2f_exp       ; end interface   
   interface s_exp    ; module procedure pk2f_subexp    ; end interface  
    
   interface erf      ; module procedure pk2f_erf       ; end interface   
   interface s_erf    ; module procedure pk2f_suberf    ; end interface   
   interface erfc     ; module procedure pk2f_erfc      ; end interface   
   interface s_erfc   ; module procedure pk2f_suberfc   ; end interface   

   interface gamma    ; module procedure pk2f_gamma     ; end interface   
   interface s_gamma  ; module procedure pk2f_subgamma  ; end interface   
   interface factor   ; module procedure pk2f_factor    ; end interface   
   interface s_factor ; module procedure pk2f_subfactor ; end interface   

   interface sqrt     ; module procedure pk2f_sqrt      ; end interface   
   interface s_sqrt   ; module procedure pk2f_subsqrt   ; end interface   

   interface heav     ; module procedure pk2f_heav      ; end interface   
   interface s_heav   ; module procedure pk2f_subheav   ; end interface   

   interface real     ; module procedure pk2f_real      ; end interface   
   interface s_real   ; module procedure pk2f_subreal   ; end interface   

   interface floor    ; module procedure pk2f_floor     ; end interface   
   interface s_floor  ; module procedure pk2f_subfloor  ; end interface   

   interface ceil     ; module procedure pk2f_ceil      ; end interface   
   interface s_ceil   ; module procedure pk2f_subceil   ; end interface   

   interface nint     ; module procedure pk2f_nint      ; end interface   
   interface s_nint   ; module procedure pk2f_subnint   ; end interface   

   interface anint    ; module procedure pk2f_anint     ; end interface   
   interface s_anint  ; module procedure pk2f_subanint  ; end interface   

   interface aint     ; module procedure pk2f_aint      ; end interface   
   interface s_aint   ; module procedure pk2f_subnint   ; end interface   

   interface imag     ; module procedure pk2f_imag      ; end interface   
   interface s_imag   ; module procedure pk2f_subimag   ; end interface   

   interface conj     ; module procedure pk2f_conj      ; end interface   
   interface s_conj   ; module procedure pk2f_subconj   ; end interface   

   interface mod      ; module procedure pk2f_mod       ; end interface   
   interface s_mod    ; module procedure pk2f_submod    ; end interface   
   
   interface sign     ; module procedure pk2f_sign      ; end interface   
   interface s_sign   ; module procedure pk2f_subsign   ; end interface   

   interface is_symm  ; module procedure pk2f_issymm    ; end interface   
   interface s_is_symm; module procedure pk2f_subissymm ; end interface   

   interface is_skew  ; module procedure pk2f_isskew    ; end interface   
   interface s_is_skew; module procedure pk2f_subisskew ; end interface   

   interface size     ; module procedure pk2f_size      ; end interface   
   interface s_size   ; module procedure pk2f_subsize   ; end interface   

   interface numel    ; module procedure pk2f_numel     ; end interface   
   interface s_numel  ; module procedure pk2f_subnumel  ; end interface   

   interface length   ; module procedure pk2f_length    ; end interface   
   interface s_length ; module procedure pk2f_sublength ; end interface   

   interface trim     ; module procedure pk2f_trim      ; end interface   
   interface s_trim   ; module procedure pk2f_subtrim   ; end interface   

   interface num2str  ; module procedure pk2f_num2str   ; end interface   
   interface s_num2str; module procedure pk2f_subnum2str; end interface   

   interface str2num  ; module procedure pk2f_str2num   ; end interface   
   interface s_str2num; module procedure pk2f_substr2num; end interface   

   interface typeof   ; module procedure pk2f_typeof    ; end interface   
   interface s_typeof ; module procedure pk2f_subtypeof ; end interface   

   interface sizeof   ; module procedure pk2f_sizeof    ; end interface   
   interface s_sizeof ; module procedure pk2f_subsizeof ; end interface   
      
   character(len=len(HELPCOLOR)+10) :: pk2f_helpcol = ''
                 
CONTAINS


!=============================================================================================   
   SUBROUTINE pk2f_subTRANS ( a, res, conj )
!=============================================================================================   
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out) :: res   
   logical       , optional, intent(in    ) :: conj
!---------------------------------------------------------------------------------------------    
!  Computes the transpose of "a" or its conjugate transpose if conj = .true. (for complexes)
!----------------------------------------------------------------------------R.H. 04/18, 11/19      

!- local variables ---------------------------------------------------------------------------
   integer(Ikind), allocatable :: Itmp(:,:)
   real   (Rkind), allocatable :: Rtmp(:,:)
   complex(Rkind), allocatable :: Ctmp(:,:)
   logical       , allocatable :: Ltmp(:,:)
   type   (str_t), allocatable :: Stmp(:,:)
   integer(Ikind)              :: err
!---------------------------------------------------------------------------------------------    
      
   if ( opflag%code > IZERO ) return !!call opflag%set ()
         
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set( stat = WARNING, where = "pk2f_subTRANS", &
                        msg = "<< b = a' >> with << a >> non-allocated (--> b = [ ])" )
      if ( allocated(res%m) ) then
          deallocate(res%m) ; call res%resetDesc()
      end if
      return
   end if
            
   err = 0
   
   ! (note: cannot avoid using ?tmp and then realloc res since this routine can be called as 
   ! trans(a,a) i.e. actual input is also the actual output)
   select type (p=>a%m)
      type is (ik2_t)
         allocate(Itmp, source = transpose(p%v), stat = err)
         if ( err == 0 ) call pk2_movealloc ( from = Itmp, to = res )
         
      type is (rk2_t)
         allocate(Rtmp, source = transpose(p%v), stat = err)
         if ( err == 0 ) call pk2_movealloc ( from = Rtmp, to = res )
      
      type is (ck2_t)
         allocate(Ctmp, source = transpose(p%v), stat = err)
         if ( err == 0 ) then
            if ( present(conj) ) then
               if ( conj ) Ctmp = conjg(Ctmp)
            end if
            call pk2_movealloc ( from = Ctmp, to = res )
         end if

      type is (lk2_t)
         allocate(Ltmp, source = transpose(p%v), stat = err)
         if ( err == 0 ) call pk2_movealloc ( from = Ltmp, to = res )
         
      type is (sk2_t)
         allocate(Stmp(a%ncol,a%nrow), stat = err)
         if ( err == 0 ) then   
            Stmp = transpose(p%v)
            call pk2_movealloc ( from = Stmp, to = res )
         end if
   end select

   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = "pk2f_subTRANS", msg = "Allocation failure" ) 
      return
   end if   
                       
   END SUBROUTINE pk2f_subTRANS


!=============================================================================================   
   FUNCTION pk2f_TRANS ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------    
!  Computes the conjugate transpose of "a"
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------    
   type(str_t) :: h(10), f1, f2, f1p, f2p
!---------------------------------------------------------------------------------------------    
      
   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( .not. present(a) ) then
!
!-    return in "res" some helps (for calmat) and exit:
!
      f1 = str_color("trans",pk2f_helpcol)
      f2 = str_color("'"    ,pk2f_helpcol)
      h(1) = "<< " + f1 + " >> or << " + f2 + &
             " >> returns the conjugate transpose (Hermitian transpose)"
      h(2) = " "
      h(3) = "Syntax for calmat:"
      h(4) = "    B = " + f1 + "(A)  or  B = A" + f2
      h(5) = " "
      f1p = str_color("transp",pk2f_helpcol)
      f2p = str_color(".'"    ,pk2f_helpcol)
      h(6) = "(Note: use << "+ f1p + " >> or << " + f2p +">> for the non-conjugate transpose)"
      h(7) = " "
      f1p = str_color("s_trans",pk2f_helpcol)
      h(8) = "Syntax for user program:"
      h(9) = "    B = " + f1 + "(A)  or  call " + f1p + "(A, B, conj=.true.)"
      h(10) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subTRANS ( a = a, res = res, conj = .true. )
   
   error_TraceNreturn(opflag, "pk2f_TRANS")
                    
   END FUNCTION pk2f_TRANS


!=============================================================================================   
   FUNCTION pk2f_TRANSP ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------    
!  Computes the transpose of "a"
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------
   type(str_t) :: h(8), f1, f2
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
!
!-    return in "res" some helps (for calmat) and exit:
!
      f1 = str_color("transp",pk2f_helpcol)
      f2 = str_color(".'"    ,pk2f_helpcol)
      h(1) = "<< " + f1 + " >> or << " + f2 + " >> returns the non-conjugate transpose"
      h(2) = " "
      h(3) = "Syntax for calmat:"
      h(4) = "    B = " + f1 + "(A)  or  B = A" + f2
      h(5) = " "
      f2 = str_color("s_trans",pk2f_helpcol)
      h(6) = "Syntax for user program:"
      h(7) = "    B = " + f1 + "(A)  or  call " + f2 + "(A, B, conj=.false.)"
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subTRANS ( a = a, res = res )  
   
   error_TraceNreturn(opflag, "pk2f_TRANSP")
            
   END FUNCTION pk2f_TRANSP
   

!=============================================================================================   
   SUBROUTINE pk2f_subEYE ( res, a, b )
!=============================================================================================   
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: a, b
!---------------------------------------------------------------------------------------------       
!  Returns a matrix whose elements on its main diagonal are equal to 1
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if a and b are not present
!     . the same of a if only a is present,  
!     . n x m where n and m are the first entries of a and b, respectively.
!----------------------------------------------------------------------------R.H. 04/18, 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: n, m, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   err = 0

   if ( present(a) ) then
      n = 0 ; m = 0
      if ( present(b) ) then
      
         if ( a%typ /= EMPTY .and. allocated(a%m) ) then
            select type (p=>a%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  n = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  n = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( b%typ /= EMPTY .and. allocated(b%m) ) then
            select type (p=>b%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  m = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  m = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( err == 1 ) then                     
            call opflag%Set ( UERROR, "pk2f_subEYE", &
                             "arguments (a,b) in << eye(a,b) >> must be integers (or reals)" )
            return
         end if   
               
      else 
         n = a%nrow ; m = a%ncol ! (if b is not present)
      end if
   else
      n = 1 ; m = 1 ! (if a and b are not present)
   end if      
      
   call pk2f_subEYEnm ( n, m, res ) ; error_TraceNreturn(opflag, "pk2f_subEYE")
      
   END SUBROUTINE pk2f_subEYE


!=============================================================================================   
   FUNCTION pk2f_EYE ( a, b, help ) result ( res )
!=============================================================================================   
   class  (pk2_t), optional, intent(in) :: a, b
   logical       , optional, intent(in) :: help
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       
!  Returns a matrix whose elements on its main diagonal are equal to 1
!
!  If help is present (and whatever its value) only the help is returned in "res".
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if a and b are not present
!     . the same of a if only a is present,  
!     . n x m where n and m are the first entries of a and b, respectively.
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(8), f   
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( present(help) ) then
!
!-    return in "res" some helps (for calmat) and exit:
!      
      f = str_color("eye",pk2f_helpcol)
      h(1) = "<< " + f + &
              " >> returns a matrix whose elements on its main diagonal are equal to 1"
      h(2) = " "
      h(3) = "Syntax: I = "+f+"(), I = "+f+"(n,m), I = "+f+"(A)"
      h(4) = " "
      h(5) = " . "+f+"()    returns the scalar 1"
      h(6) = " . "+f+"(n,m) returns an n x m matrix "
      h(7) = " . "+f+"( A ) returns a matrix of the same size of A"
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_SubEYE ( a = a, b = b, res = res ) ; error_TraceNreturn(opflag, "pk2f_EYE")
      
   END FUNCTION pk2f_EYE
   
   
!=============================================================================================   
   SUBROUTINE pk2f_subEYEwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------       
!  Returns a matrix whose elements on its main diagonal are equal to 1
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if size(matrs) == 0,
!     . same as a=matrs(1) if size(matrs)==1,  
!     . n x m where n and m are the first entries of a=matrs(1) and b=matrs(2), respectively.
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!---------------------------------------------------------------------R.H. 04/18, 12/18, 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: nargs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   select case (nargs)
      case (0)
         call pk2f_SubEYE ( res = res )
      case (1)
         call pk2f_SubEYE ( a = matrs(1), res = res )
      case (2)
         call pk2f_SubEYE ( a = matrs(1), b = matrs(2), res = res )
      case default
         call opflag%set ( stat = UERROR, where = "pk2f_subEYEwrp", &
                           msg  = '0, 1 or 2 argument(s) expected for the function << eye >>')
   end select
                    
   END SUBROUTINE pk2f_subEYEwrp


!=============================================================================================   
   FUNCTION pk2f_EYEwrp ( matrs ) result ( res ) 
!=============================================================================================   
   class(pk2_t), intent(in) :: matrs(:)
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------       
     
   call pk2f_subEYEwrp (matrs, res) ; error_TraceNreturn(opflag, "pk2f_EYEwrp")

   END FUNCTION pk2f_EYEwrp

   
!=============================================================================================   
   SUBROUTINE pk2f_subEYEnm ( n, m, res )
!=============================================================================================   
   integer (Ikind), intent(in    ) :: n, m
   class   (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------       
!  Returns a n x m matrix whose elements on its main diagonal are equal to 1
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: i, err
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   err = 0
   
   if ( n /= 0 .and. m /= 0 ) then
      allocate(ik2_t::res%m)
      select type (p=>res%m)
         type is (ik2_t)
            allocate(p%v(n,m), source = IZERO, stat = err)
            if ( err == 0 ) then
               do i = 1, min(n,m)
                  p%v(i,i) = IONE              
               end do
            end if   
      end select
   else         
      call opflag%set ( stat = WARNING, where = "pk2f_subEYEnm",                &
                         msg = "<< c = eye(n,m) >> with n=0 or m=0 (--> c=[ ])" )
      return
   end if   

   if ( err == 0 ) then
      call res%resetDesc()
   else
      res%m = bk2_t()
      call opflag%set ( stat = IERROR, where = "pk2f_subEYEnm",     &
                         msg = "(in pk2f_EYEnm) Allocation failure" )
   end if
      
   END SUBROUTINE pk2f_subEYEnm


!=============================================================================================   
   FUNCTION pk2f_EYEnm ( n, m ) result ( c )
!=============================================================================================   
   integer(Ikind), intent(in) :: n, m
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------       

   call pk2f_subEYEnm ( n, m, c ) ; error_TraceNreturn(opflag, "pk2f_EYEnm")
    
   END FUNCTION pk2f_EYEnm
   
        
!=============================================================================================   
   SUBROUTINE pk2f_SubZEROS ( res, a, b )
!=============================================================================================   
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: a, b   
!---------------------------------------------------------------------------------------------  
!  Returns a matrix whose elements are 0
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if a and b are not present
!     . equal to the shape of a if only a is present,  
!     . n x m where n and m are the first entries of a and b, respectively.
!----------------------------------------------------------------------------R.H. 04/18, 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: n, m, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   err = 0
   
   if ( present(a) ) then
      n = 0 ; m = 0
      if ( present(b) ) then
      
         if ( a%typ /= EMPTY .and. allocated(a%m) ) then
            select type (p=>a%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) n = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  n = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( b%typ /= EMPTY .and. allocated(b%m) ) then
            select type (p=>b%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) m = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  m = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( err == 1 ) then                     
            call opflag%set ( stat = UERROR, where = "pk2f_SubZEROS", msg = &
                           "arguments (a,b) in << zeros(a,b) >> must be integers (or reals)" )
            return
         end if   
               
      else
         n = a%nrow ; m = a%ncol
      end if
   else
      n = 1 ; m = 1
   end if      
      
   call pk2f_subZEROSnm ( n, m, res ) ; error_TraceNreturn(opflag, "pk2f_SubZEROS")
                  
   END SUBROUTINE pk2f_SubZEROS


!=============================================================================================   
   FUNCTION pk2f_ZEROS ( a, b, help ) result ( res )
!=============================================================================================   
   class  (pk2_t), optional, intent(in) :: a, b
   logical       , optional, intent(in) :: help
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------  
!  Returns a matrix whose elements are 0
!
!  If help is present (and whatever its value) only the help is returned in "res".
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if a and b are not present
!     . equal to the shape of a if only a is present,  
!     . n x m where n and m are the first entries of a and b, respectively.
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(8), f
!---------------------------------------------------------------------------------------------      

   if ( present(help) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color("zeros",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns an array whose elements are all 0"
      h(2) = " "
      h(3) = "Syntax: Z = "+f+"(), Z = "+f+"(n,m), Z = "+f+"(A)"
      h(4) = " "
      h(5) = " . "+f+"()    returns the scalar 0"
      h(6) = " . "+f+"(n,m) returns the null n x m matrix"
      h(7) = " . "+f+"( A ) returns the null matrix of the same size of A" 
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_SubZEROS ( a = a, b = b, res = res ) ; error_TraceNreturn(opflag, "pk2f_ZEROS")
         
   END FUNCTION pk2f_ZEROS
   

!=============================================================================================   
   SUBROUTINE pk2f_subZEROSwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------  
!  Returns a matrix whose elements are 0
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if a and b are not present
!     . equal to the shape of a=matrs(1) if only a is present,  
!     . n x m where n and m are the first entries of a=matrs(1) and b=matrs(2), respectively.
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!----------------------------------------------------------------------------R.H. 04/18, 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: nargs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)

   select case (nargs)
      case (0)
         call pk2f_SubZEROS ( res = res )
      case (1)
         call pk2f_SubZEROS ( a = matrs(1), res = res )
      case (2)
         call pk2f_SubZEROS ( a = matrs(1), b = matrs(2), res = res )
      case default
         call opflag%set( stat = UERROR, where = "pk2f_subZEROSwrp", &
                           msg = '0, 1 or 2 argument(s) expected for the function << zeros >>')
   end select
   
   error_TraceNreturn(opflag, "pk2f_subZEROSwrp")
   
   END SUBROUTINE pk2f_subZEROSwrp


!=============================================================================================   
   FUNCTION pk2f_ZEROSwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), intent(in) :: matrs(:)
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------  

   call pk2f_subZEROSwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_ZEROSwrp")
             
   END FUNCTION pk2f_ZEROSwrp


!=============================================================================================   
   SUBROUTINE pk2f_subZEROSnm ( n, m, res )
!=============================================================================================   
   integer(Ikind), intent(in    ) :: n, m
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------       
!  Returns a n x m matrix null matrix
!-----------------------------------------------------------------------------------R.H. 04/18 

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( n /= 0 .and. m /= 0 ) then
      allocate(ik2_t::res%m)
      err = 0
      select type (p=>res%m)
         type is (ik2_t)
            allocate(p%v(n,m), source = IZERO, stat = err)
      end select
      if ( err == 0 ) then
         call res%resetDesc()
      else
         res%m = bk2_t()
         call opflag%set ( stat=IERROR, where="pk2f_subZEROSnm", msg="Allocation failure" )
      end if
   else         
      call opflag%set ( stat = WARNING, where="pk2f_subZEROSnm", &
                        msg  = "<< c = zeros(n,m) >> with n = 0 or m = 0  (--> c=[ ])" )
   end if   
      
   END SUBROUTINE pk2f_subZEROSnm


!=============================================================================================   
   FUNCTION pk2f_ZEROSnm ( n, m ) result ( c )
!=============================================================================================   
   integer(Ikind), intent(in) :: n, m
   type   (pk2_t)             :: c
!---------------------------------------------------------------------------------------------       

   call pk2f_subZEROSnm ( n, m, c ) ; error_TraceNreturn(opflag, "pk2f_ZEROSnm")
   
   END FUNCTION pk2f_ZEROSnm


!=============================================================================================   
   SUBROUTINE pk2f_subFALSES ( res, a, b )
!=============================================================================================   
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: a, b
!---------------------------------------------------------------------------------------------  
!  Returns a boolean matrix whose elements are .false.
!
!  The shape of this matrix is
!     . 1 x 1 if a and b are not present
!     . equal to the shape of a=matrs(1) if a is only present,  
!     . n x m where n and m are the first entries of a=matrs(1) and b=matrs(2), respectively.
!----------------------------------------------------------------------------R.H. 04/18, 11/19     

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: n, m, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   err = 0
   
   if ( present(a) ) then
      n = 0 ; m = 0
      if ( present(b) ) then
      
         if ( a%typ /= EMPTY .and. allocated(a%m) ) then
            select type (p=>a%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) n = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  n = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( b%typ /= EMPTY .and. allocated(b%m) ) then
            select type (p=>b%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) m = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  m = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( err == 1 ) then                     
            call opflag%set ( stat = UERROR, where = "pk2f_subFALSES", msg = &
                        "arguments (a,b) in << falses(a,b) >> must be integers (or reals)" )
            return
         end if   
               
      else
         n = a%nrow ; m = a%ncol
      end if
   else
      n = 1 ; m = 1
   end if      
      
   call pk2f_subFALSESnm ( n, m , res ) ; error_TraceNreturn(opflag, "pk2f_subFALSES")
        
   END SUBROUTINE pk2f_subFALSES


!=============================================================================================   
   FUNCTION pk2f_FALSES ( a, b, help ) result ( res )
!=============================================================================================   
   class  (pk2_t), optional, intent(in) :: a, b
   logical       , optional, intent(in) :: help
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------  
!  Returns a boolean matrix whose elements are .false.
!
!  If help is present (and whatever its value) only the help is returned in "res".
!
!  The shape of this matrix is
!     . 1 x 1 if a and b are not present
!     . equal to the shape of a=matrs(1) if a is only present,  
!     . n x m where n and m are the first entries of a=matrs(1) and b=matrs(2), respectively.
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(8), f
!---------------------------------------------------------------------------------------------      

   if ( present(help) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color("falses",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns a boolean array whose elements are all F"
      h(2) = " "
      h(3) = "Syntax: F = "+f+"(), F = "+f+"(n,m), F = "+f+"(A)"
      h(4) = " "
      h(5) = " . "+f+"()    returns the scalar F"
      h(6) = " . "+f+"(n,m) returns an n x m matrix"
      h(7) = " . "+f+"( A ) returns a matrix of the same size of A" 
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subFALSES ( a = a, b = b, res = res ) ; error_TraceNreturn(opflag, "pk2f_FALSES")
            
   END FUNCTION pk2f_FALSES


!=============================================================================================   
   SUBROUTINE pk2f_subFALSESwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------  
!  Returns a boolean matrix whose elements are .false.
!
!  The shape of this matrix is
!     . 1 x 1 if size(matrs) == 0
!     . equal to the shape of a=matrs(1) if size(matrs) == 1,  
!     . n x m where n and m are the first entries of a=matrs(1) and b=matrs(2), respectively.
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!----------------------------------------------------------------------------R.H. 04/18, 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: nargs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   select case (nargs)
      case (0)
         call pk2f_subFALSES ( res = res )
      case (1)
         call pk2f_subFALSES ( a = matrs(1), res = res )
      case (2)
         call pk2f_subFALSES ( a = matrs(1), b = matrs(2), res = res )
      case default
         call opflag%set ( stat = UERROR, where = "pk2f_subFALSESwrp", msg = &
                               '0, 1 or 2 argument(s) expected for the function << falses >>')
         return
   end select
   
   error_TraceNreturn(opflag, "pk2f_subFALSESwrp")   
                  
   END SUBROUTINE pk2f_subFALSESwrp


!=============================================================================================   
   FUNCTION pk2f_FALSESwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), intent(in) :: matrs(:)
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------    

   call pk2f_subFALSESwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_FALSESwrp") 
             
   END FUNCTION pk2f_FALSESwrp


!=============================================================================================   
   SUBROUTINE pk2f_subFALSESnm ( n, m, res )
!=============================================================================================   
   integer(Ikind), intent(in    ) :: n, m
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------      
!  Returns a n x m boolean matrix whose elements are .false.
!-----------------------------------------------------------------------------------R.H. 04/18   

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( n /= 0 .and. m /= 0 ) then
      allocate(lk2_t::res%m)
      err = 0
      select type (p=>res%m)
         type is (lk2_t)
            allocate(p%v(n,m), source = .false., stat = err)
      end select
      if ( err == 0 ) then
         call res%resetDesc()
      else
         call opflag%set (stat=IERROR, where="pk2f_subFALSESnm)", msg="Allocation failure")
         deallocate(res%m)
      end if
   else         
      call opflag%set ( stat = WARNING, where = "pk2f_subFALSESnm)",                &
                        msg  = "<< c = falses(n,m) >> with n=0 or m=0  (--> c=[ ])" )
      return
   end if   
         
   END SUBROUTINE pk2f_subFALSESnm


!=============================================================================================   
   FUNCTION pk2f_FALSESnm ( n, m ) result ( res )
!=============================================================================================   
   integer(Ikind), intent(in) :: n, m
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------      

   call pk2f_subFALSESnm ( n, m, res ) ; error_TraceNreturn(opflag, "pk2f_FALSESnm")
          
   END FUNCTION pk2f_FALSESnm


!=============================================================================================   
   SUBROUTINE pk2f_subONES ( res, a, b )
!=============================================================================================   
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: a, b
!---------------------------------------------------------------------------------------------  
!  Returns a matrix whose elements are 1
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if a and b are not present
!     . equal to the shape of a if only a is present,  
!     . n x m where n and m are the first entries of a and b, respectively.
!----------------------------------------------------------------------------R.H. 04/18, 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: n, m, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   err = 0
   
   if ( present(a) ) then
      n = 0 ; m = 0
      if ( present(b) ) then
      
         if ( a%typ /= EMPTY .and. allocated(a%m) ) then
            select type (p=>a%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) n = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  n = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( b%typ /= EMPTY .and. allocated(b%m) ) then
            select type (p=>b%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) m = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  m = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( err == 1 ) then                     
            call opflag%set ( stat = UERROR, where = "pk2f_subONES", msg = &
                           "arguments (a,b) in << ones(a,b) >> must be integers (or reals)" )            
            return
         end if   
               
      else
         n = a%nrow ; m = a%ncol
      end if
   else
      n = 1 ; m = 1
   end if      
      
   call pk2f_subONESnm ( n, m, res ) ; error_TraceNreturn(opflag, "pk2f_subONES")
          
   END SUBROUTINE pk2f_subONES


!=============================================================================================   
   FUNCTION pk2f_ONES ( a, b, help ) result ( res )
!=============================================================================================   
   class  (pk2_t), optional, intent(in) :: a, b
   logical       , optional, intent(in) :: help
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------  
!  Returns a matrix whose elements are 1
!
!  If help is present (and whatever its value) only the help is returned in "res".
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if a and b are not present
!     . equal to the shape of a if only a is present,  
!     . n x m where n and m are the first entries of a and b, respectively.
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(8), f
!---------------------------------------------------------------------------------------------      

   if ( present(help) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color("ones",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns a matrix whose elements are all 1"
      h(2) = " "
      h(3) = "Syntax: X = "+f+"(), X = "+f+"(n,m), X = "+f+"(A)"
      h(4) = " "
      h(5) = " . "+f+"()    returns the scalar 1"
      h(6) = " . "+f+"(n,m) returns an n x m matrix"
      h(7) = " . "+f+"( A ) returns a matrix of the same size of A" 
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_SubONES ( a = a, b = b, res = res ) ; error_TraceNreturn(opflag, "pk2f_ONES")
       
   END FUNCTION pk2f_ONES


!=============================================================================================   
   SUBROUTINE pk2f_subONESwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------  
!  Returns a matrix whose elements are 1
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if size(matrs) == 0
!     . equal to the shape of a=matrs(1) if size(matrs) == 1,  
!     . n x m where n and m are the first entries of a=matrs(1) and b=matrs(2), respectively.
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!----------------------------------------------------------------------------R.H. 04/18, 11/19      

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: nargs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)

   select case (nargs)
      case (0)
         call pk2f_SubONES ( res = res )
      case (1)
         call pk2f_SubONES ( a = matrs(1), res = res )
      case (2)
         call pk2f_SubONES ( a = matrs(1), b = matrs(2), res = res )
      case default
         call opflag%set( stat = UERROR, where = "pk2f_subONESwrp", &
                          msg  = '0, 1 or 2 argument(s) expected for the function << ones >>')
         return
   end select

   error_TraceNreturn(opflag, "pk2f_subONESwrp")
                        
   END SUBROUTINE pk2f_subONESwrp


!=============================================================================================   
   FUNCTION pk2f_ONESwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), intent(in) :: matrs(:)
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------  

   call pk2f_subONESwrp ( matrs, res) ; error_TraceNreturn(opflag, "pk2f_ONESwrp")
                      
   END FUNCTION pk2f_ONESwrp
   

!=============================================================================================   
   SUBROUTINE pk2f_subONESnm ( n, m, res )
!=============================================================================================   
   integer(Ikind), intent(in    ) :: n, m
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------      
!  Returns a n x m matrix whose elements are 1.
!-----------------------------------------------------------------------------------R.H. 04/18   

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if (n /= 0 .and. m /= 0) then
      allocate(ik2_t::res%m)
      err = 0
      select type (p=>res%m)
         type is (ik2_t)
            allocate(p%v(n,m), source = IONE, stat = err)
      end select
      if ( err == 0 ) then
         call res%resetDesc()
      else
         call opflag%set(stat = IERROR, where = "pk2f_subONESnm", msg = "Allocation failure")
         deallocate(res%m)
      end if
   else         
      call opflag%set ( stat = WARNING, where = "pk2f_subONESnm", &
                         msg = "<< c = ones(n,m) >> with n=0 or m=0 (--> c=[ ])" )
   end if      

   END SUBROUTINE pk2f_subONESnm


!=============================================================================================   
   FUNCTION pk2f_ONESnm ( n, m ) result ( res )
!=============================================================================================   
   integer(Ikind), intent(in) :: n, m
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------      

   call pk2f_subONESnm ( n, m, res) ; error_TraceNreturn(opflag, "pk2f_ONESnm")
   
   END FUNCTION pk2f_ONESnm
   
    
!=============================================================================================   
   SUBROUTINE pk2f_subRAND ( res, a, b )
!=============================================================================================   
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: a, b   
!---------------------------------------------------------------------------------------------      
!  Returns a matrix of uniformly distributed random numbers in the interval (0,1).
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if "a" and "b" are not present
!     . equal to the shape of a if only a is present,  
!     . n x m where n and m are the first entries of a and b, respectively.
!----------------------------------------------------------------------------R.H. 04/18, 11/19      

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: n, m, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   err = 0
   
   if ( present(a) ) then
      n = 0 ; m = 0
      if ( present(b) ) then

         if ( a%typ /= EMPTY .and. allocated(a%m) ) then
            select type (p=>a%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) n = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  n = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( b%typ /= EMPTY .and. allocated(b%m) ) then
            select type (p=>b%m)
               type is (ik2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) m = p%v(1,1)
               type is (rk2_t)
                  if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  m = int(p%v(1,1),kind=Ikind)
               class default
                  err = 1
            end select
         end if
         
         if ( err == 1 ) then                     
            call opflag%set( stat = UERROR, where = "pk2f_subRAND", msg = &
                            "arguments (a,b) in << rand(a,b) >> must be integers (or reals)" )
            return
         end if   
               
      else
         n = a%nrow ; m = a%ncol
      end if
   else
      n = 1 ; m = 1
   end if      
             
   call pk2f_subRANDnm ( n, m, res ) ; error_TraceNreturn(opflag, "pk2f_subRAND")
                  
   END SUBROUTINE pk2f_subRAND


!=============================================================================================   
   FUNCTION pk2f_RAND ( a, b, help ) result ( res )
!=============================================================================================   
   class  (pk2_t), optional, intent(in) :: a, b
   logical       , optional, intent(in) :: help
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------      
!  Returns a matrix of uniformly distributed random numbers in the interval (0,1).
!
!  If help is present (and whatever its value) only the help is returned in "res".
!
!  The type of this matrix is integer and its shape is
!     . 1 x 1 if "a" and "b" are not present
!     . equal to the shape of a if only a is present,  
!     . n x m where n and m are the first entries of a and b, respectively.
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(9), f
!---------------------------------------------------------------------------------------------      

   if ( present(help) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('rand',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns a matrix of uniformly distributed pseudo-random" 
      h(2) = "           numbers in the interval (0,1)"
      h(3) = " "
      h(4) = "Syntax: x = "+f+"(), X = "+f+"(n,m), X = "+f+"(A)"
      h(5) = " "
      h(6) = " . "+f+"()    returns a single number"
      h(7) = " . "+f+"(n,m) returns an n x m matrix"
      h(8) = " . "+f+"( A ) returns a matrix of the same size of A" 
      h(9) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_SubRAND ( a = a, b = b, res = res ) ; error_TraceNreturn(opflag, "pk2f_RAND")
        
   END FUNCTION pk2f_RAND
   

!=============================================================================================   
   SUBROUTINE pk2f_subRANDwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------      
!  Returns a matrix of uniformly distributed pseudo-random numbers in the interval (0,1).
!
!  The type of this matrix is integer and its shape is
!     . 1 x x if size(matrs) == 0
!     . equal to the shape of a=matrs(1) if size(matrs)==1,  
!     . n x m where n and m are the first entries of a=matrs(1) and b=matrs(2), respectively.
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!----------------------------------------------------------------------------R.H. 04/18, 11/19      

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind ) :: nargs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)

   select case (nargs)
      case (0)
         call pk2f_SubRAND ( res = res )
      case (1)
         call pk2f_SubRAND ( a = matrs(1), res = res )
      case (2)
         call pk2f_SubRAND ( a = matrs(1), b = matrs(2), res = res )
      case default
         call opflag%set( stat = UERROR, where = "pk2f_subRANDwrp", &
                          msg  = '0, 1 or 2 argument(s) expected for the function << rand >>')
         return
   end select
              
   error_TraceNreturn(opflag, "pk2f_subRANDwrp")   
   
   END SUBROUTINE pk2f_subRANDwrp


!=============================================================================================   
   FUNCTION pk2f_RANDwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), intent(in) :: matrs(:)
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------      

   call pk2f_subRANDwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_RANDwrp")     
                        
   END FUNCTION pk2f_RANDwrp


!=============================================================================================   
   SUBROUTINE pk2f_subRANDnm ( n, m, res )
!=============================================================================================   
   integer(Ikind), intent(in    ) :: n, m
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------      
!  Returns a n x m matrix of uniformly distributed random numbers in the interval (0,1).
!-----------------------------------------------------------------------------------R.H. 04/18       

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( n /= 0 .and. m /= 0 ) then
      allocate(rk2_t::res%m)
      select type (p=>res%m)
         type is (rk2_t)
            call util_SubRandom (n, m, p%v, opflag)
      end select
      if ( opflag%code <= 0 ) then
         call res%resetDesc()
      else   
         deallocate(res%m)
         call opflag%addTrace("pk2f_subRANDnm")
      end if   
   else
      call opflag%set ( stat = WARNING, where = "pk2f_subRANDnm", &
                         msg = "<< c = rand(n,m) >> with n=0 or m=0 (--> c=[ ])" )          
   end if  

   END SUBROUTINE pk2f_subRANDnm


!=============================================================================================   
   FUNCTION pk2f_RANDnm ( n, m ) result ( res )
!=============================================================================================   
   integer(Ikind), intent(in) :: n, m
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------      

   call pk2f_subRANDnm ( n, m, res ) ; error_TraceNreturn(opflag, "pk2f_RANDnm")
           
   END FUNCTION pk2f_RANDnm


!=============================================================================================   
   SUBROUTINE pk2f_subRANDInm ( imin, imax, n, m, res )
!=============================================================================================   
   integer(Ikind), intent(in    ) :: imin, imax, n, m
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------      
!  Returns a n x m matrix of uniformly distributed pseudo-random integer numbers between imin
!  and imax.
!-----------------------------------------------------------------------------------R.H. 04/19       

!- local variables ---------------------------------------------------------------------------     
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( n /= 0 .and. m /= 0 ) then
      allocate(ik2_t::res%m)
      select type (p=>res%m)
         type is (ik2_t)
            call util_SubRandomI ( imin, imax, n, m, p%v, opflag )
      end select   
      if ( opflag%code <= 0 ) then
         call res%resetDesc()
      else   
         deallocate(res%m)
         call opflag%addTrace("pk2f_subRANDInm")
      end if         
   else
      call opflag%set ( stat = WARNING, where = "pk2f_subRANDInm", &
                         msg = "<< randi(i1,i2,n,m) >> with n=0 or m=0 (--> c=[ ])" )
   end if   
               
   END SUBROUTINE pk2f_subRANDInm


!=============================================================================================   
   FUNCTION pk2f_RANDInm ( imin, imax, n, m ) result ( res )
!=============================================================================================   
   integer(Ikind), intent(in) :: imin, imax, n, m
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------      

   call pk2f_subRANDInm ( imin, imax, n, m, res ) ; error_TraceNreturn(opflag, "pk2f_RANDInm")
             
   END FUNCTION pk2f_RANDInm


!=============================================================================================   
   SUBROUTINE pk2f_subRANDIwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------      
!  Returns a n x m matrix of uniformly distributed pseudo-random integer numbers between imin
!  and imax.
!----------------------------------------------------------------------------R.H. 04/19, 11/19       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind)  :: nargs, imin, imax, n, m, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nargs = size(matrs)
   if ( nargs /= 1 .and. nargs /= 2 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subRANDIwrp", msg =            &
                        "bad number of arguments for << randi >> (1 or 2 expected)" )
      return
   end if   
   
   err = 0
!
!- determine imin and imax:
!   
   select type (p=>matrs(1)%m)
      type is (ik2_t)
         if ( p%nrow == 1 .and. p%ncol == 1 ) then
            imin = 1 ; imax = p%v(1,1)
            if (imax <= 0) err = 2
         else if ( p%nrow == 1 .and. p%ncol == 2 ) then
            imin = p%v(1,1) ; imax = p%v(1,2)
            if (imax < imin) err = 3
         else if ( p%nrow == 2 .and. p%ncol == 1 ) then
            imin = p%v(1,1) ; imax = p%v(2,1)
            if ( imax < imin ) err = 3
         else
            err = 1
         end if   
      type is (rk2_t)
         if ( p%nrow == 1 .and. p%ncol == 1 ) then
            imin = 1 ; imax = int(p%v(1,1))
            if ( imax <= 0 ) err = 2
         else if ( p%nrow == 1 .and. p%ncol == 2 ) then
            imin = int(p%v(1,1)) ; imax = int(p%v(1,2))
            if ( imax < imin ) err = 3
         else if ( p%nrow == 2 .and. p%ncol == 1 ) then
            imin = int(p%v(1,1)) ; imax = int(p%v(2,1))
            if ( imax < imin ) err = 3
         else
            err = 1
         end if   
      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subRANDIwrp",          &
                            msg = 'integer arguments expected for << randi >>' )
         return
   end select    
   
   if ( err /= 0 ) then
      if ( err == 1 ) then
         call opflag%set( stat = UERROR, where = "pk2f_subRANDIwrp", msg =                   &
                  'in << randi(a) >> or << randi(a,sz), size of "a" must be equal to 1 or 2' )
      else if ( err == 2 ) then
         call opflag%set( stat = UERROR, where = "pk2f_subRANDIwrp", msg = &
                          'in << randi(imax) >> imax must be positive'     )
      else if ( err == 3 ) then
         call opflag%set( stat = UERROR, where = "pk2f_subRANDIwrp", msg =           &
                          'in << randi(imin,imax) >> imax must be greater than imin' )
      end if
      return
   end if   
!
!- determine n and m:
!
   if ( nargs == 1 ) then
      n = 1 ; m = 1
   else
      select type (p=>matrs(2)%m)
         type is (ik2_t)
            if ( p%nrow == 1 .and. p%ncol == 1 ) then
               n = p%v(1,1) ; m = n
            else if ( p%nrow == 1 .and. p%ncol == 2 ) then
               n = p%v(1,1) ; m = p%v(1,2)
            else if ( p%nrow == 2 .and. p%ncol == 1 ) then
               n = p%v(1,1) ; m = p%v(2,1)
            else
               err = 1
            end if   
         type is (rk2_t)
            if ( p%nrow == 1 .and. p%ncol == 1 ) then
               n = int(p%v(1,1)) ; m = n
            else if ( p%nrow == 1 .and. p%ncol == 2 ) then
               n = int(p%v(1,1)) ; m = int(p%v(1,2))
            else if ( p%nrow == 2 .and. p%ncol == 1 ) then
               n = int(p%v(1,1)) ; m = int(p%v(2,1))
            else
               err = 1
            end if   
         class default
            call opflag%set( stat = UERROR, where = "pk2f_subRANDIwrp",         &
                             msg  = 'integer arguments expected for << randi >>')
            return
      end select    
   
      if ( err /= 0 ) then
         call opflag%set( stat = UERROR, where = "pk2f_subRANDIwrp", msg =     &
                     'in << randi(a,sz), size of "sz" must be equal to 1 or 2' )
         return
      end if   
   end if
                        
   call pk2f_subRANDInm ( imin, imax, n, m, res )
   
   error_TraceNreturn(opflag, "pk2f_subRANDIwrp")

   END SUBROUTINE pk2f_subRANDIwrp


!=============================================================================================   
   FUNCTION pk2f_RANDIwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------      

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(14), f
!---------------------------------------------------------------------------------------------      

   if ( .not. present(matrs) ) then
      f = str_color('randi',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns a matrix of uniformly distributed pseudo-random" 
      h( 2) = "            integer numbers in a given interval"
      h( 3) = " "
      h( 4) = "Syntax: i = "+f+"(imax)         i = "+f+"([imin,imax]) "
      h( 5) = "        I = "+f+"(imax,n)       I = "+f+"([imin,imax],n)"
      h( 6) = "        I = "+f+"(imax,[n,m])   I = "+f+"([imin,imax],[n,m])"
      h( 7) = " "
      h( 8) = ". "+f+"(imax)              returns a single number in [1,imax]"
      h( 9) = ". "+f+"([imin,imax])       returns a single number in [imin,imax]"
      h(10) = ". "+f+"(imax,n)            returns a n x n matrix of numbers in [1,imax]" 
      h(11) = ". "+f+"([imin,imax],n)     returns a n x n matrix of numbers in [imin,imax]" 
      h(12) = ". "+f+"(imax,[n,m])        returns a n x m matrix in [1,imax]" 
      h(13) = ". "+f+"([imin,imax],[n,m]) returns a n x m matrix of numbers in [imin,imax]" 
      h(14) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subRANDIwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_RANDIwrp")

   END FUNCTION pk2f_RANDIwrp
   

!=============================================================================================   
   SUBROUTINE pk2f_subRANDPERMn ( n, k, res )
!=============================================================================================   
   integer(Ikind),           intent(in    ) :: n
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k      
!---------------------------------------------------------------------------------------------      
!  Returns a row vector containing
!  . a random permutation of {1,...,n} if k is not present
!  . a sample of k unique integers of {1,...,n} if k is present
!  
!  Source: Rosetta Code (the  Knuth shuffle algorithm)
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables --------------------------------------------------------------------------- 
   integer(Ikind), allocatable :: tmp(:)  
   integer(Ikind)              :: err
!---------------------------------------------------------------------------------------------      

   call util_RandPerm ( n, k, tmp, opflag ) ; error_TraceNreturn(opflag, "pk2f_subRANDPERMn")
   
   allocate(ik2_t::res%m)

   err = 0
   
   select type (p=>res%m)
      type is (ik2_t)
         allocate(p%v(1,size(tmp)),stat = err)
         if ( err == 0 ) p%v(1,:) = tmp(:)
   end select

   if ( err == 0 ) then      
      call res%resetDesc()
   else
      call opflag%set(stat = IERROR, where = "pk2f_subRANDPERMn)", msg = "Allocation failure")
   end if
               
   END SUBROUTINE pk2f_subRANDPERMn


!=============================================================================================   
   FUNCTION pk2f_RANDPERMn ( n, k ) result ( res )
!=============================================================================================   
   integer(Ikind),           intent(in) :: n
   integer(Ikind), optional, intent(in) :: k   
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------      

   call pk2f_subRANDPERMn ( n = n, res = res, k = k )
   
   error_TraceNreturn(opflag, "pk2f_RANDPERMn")
           
   END FUNCTION pk2f_RANDPERMn
    

!=============================================================================================   
   SUBROUTINE pk2f_subRANDPERMwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------      
!  Returns a row vector containing
!  . a random permutation of {1,...,n} if size(matrs) = 1
!  . a sample of k unique integers of {1,...,n} if size(matrs) = 2
!----------------------------------------------------------------------------R.H. 04/19, 11/19  

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: nargs, n, k
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nargs = size(matrs)
   if ( nargs /= 1 .and. nargs /= 2 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subRANDPERMwrp", msg =           &
                        "bad number of arguments for << randperm >> (1 or 2 expected)" )
      return
   end if   
!
!- Determine n:
!   
   n = 0
   select type (p=>matrs(1)%m)
      type is (ik2_t)
         if ( p%nrow /= 0 .and. p%ncol /= 0 ) n = p%v(1,1)
      type is (rk2_t)
         if ( p%nrow /= 0 .and. p%ncol /= 0 ) n = int(p%v(1,1))
      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subRANDPERMwrp",          &
                            msg = 'integer arguments expected for << randperm >>' )
         return
   end select    

   if ( nargs == 1 ) then
!
!-    Random permutation:
!
      call pk2f_subRANDPERMn ( n=n, res=res )
     
   else   
!
!-    Determine k:
!
      select type (p=>matrs(2)%m)
         type is (ik2_t)
            if ( p%nrow /= 0 .and. p%ncol /= 0 ) k = p%v(1,1)
         type is (rk2_t)
            if ( p%nrow /= 0 .and. p%ncol /= 0 ) k = int(p%v(1,1))
         class default
            call opflag%set ( stat = UERROR, where = "pk2f_subRANDPERMwrp",          &
                               msg = 'integer arguments expected for << randperm >>' )
            return
      end select   
!
!-    Random selection of k out of n:
!
      call pk2f_subRANDPERMn ( n=n, res=res, k=k )

   end if   
   
   error_TraceNreturn(opflag, "pk2f_subRANDPERMwrp")
   
   END SUBROUTINE pk2f_subRANDPERMwrp


!=============================================================================================   
   FUNCTION pk2f_RANDPERMwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------         

!- local variables ---------------------------------------------------------------------------   
   type(str_t) :: h(8), f 
!---------------------------------------------------------------------------------------------      

   if ( .not. present(matrs) ) then
      f = str_color('randperm',pk2f_helpcol)
      h(1) = "<< "+f+" >> random permutation" 
      h(2) = " "
      h(3) = "Syntax:   A = "+f+"(n) "
      h(4) = "          A = "+f+"(n,k) "
      h(5) = " "
      h(6) = ". "+f+"(n)   returns a random permutation of {1,...,n}"
      h(7) = ". "+f+"(n,k) returns a sample of unique k integers of {1,...,n}"
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subRANDPERMwrp ( matrs, res ) 

   error_TraceNreturn(opflag, "pk2f_RANDPERMwrp")

   END FUNCTION pk2f_RANDPERMwrp
   

!=============================================================================================   
   SUBROUTINE pk2f_subDIAG ( a, b, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in    ) :: b
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------       
!  Extracts the diagonal of a matrix or forms a diagonal matrix from an n-vector
!
!  1) If "a" is an n-vector (i.e. a%nrow or a%ncol = 1 and n = a%nrow * a%ncol):
!
!     forms the square matrix "res" of size n+abs(k) with the entries of "a" on its k-th
!     diagonal where k is the first entry of b (if b is empty, consider k = 0).
!
!  2) If "a" is a matrix (i.e. a%nrow /= 1 and a%ncol /= 1):
!
!     forms the n-vector whose elements are on the k-th diagonal of "a" where k is the first 
!     entry of b (if b is empty, consider k = 0).
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: k
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subDIAG",                          &
                         msg = " << c = diag(a[,b]) >> with << a >> empty (--> c = [ ])" )
      return
   end if
      
   k = 0
   if ( b%typ /= EMPTY .and. allocated(b%m) ) then ! si vide ou non-alloue, prendre k = 0  
      select type (p=>b%m)
         type is (ik2_t)
            if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) k = p%v(1,1)  
         type is (rk2_t)   
            if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  k = int(p%v(1,1),kind=Ikind) 
         class default
            call opflag%set( stat = UERROR, where = "pk2f_subDIAG", msg =                   &
                            "arguments b in << diag(a,b) >> must be an integer (or a real)" )
            return
      end select
   end if            
    
   call pk2f_SubDIAGn ( a = a, res = res, k = k )

   error_TraceNreturn(opflag, "pk2f_subDIAG")
   
   END SUBROUTINE pk2f_subDIAG


!=============================================================================================   
   FUNCTION pk2f_DIAG ( a, b ) result ( res )
!=============================================================================================   
   class(pk2_t), intent(in) :: a
   class(pk2_t), intent(in) :: b
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------       
    
   call pk2f_SubDIAG ( a, b, res ) ; error_TraceNreturn(opflag, "pk2f_DIAG")
                  
   END FUNCTION pk2f_DIAG


!=============================================================================================   
   SUBROUTINE pk2f_subDIAGn ( a, res, k )
!=============================================================================================   
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out):: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------       
!  Extracts the diagonal of a matrix or forms a diagonal matrix from an n-vector
!
!  1) If "a" is an n-vector (i.e. a%nrow or a%ncol = 1 and n = a%nrow * a%ncol):
!     diagi(a,k) forms the square matrix of size n+abs(k) with the entries of "a" on its k-th
!     diagonal (if k is not present, consider k = 0, i.e. main diagonal).
!
!  2) If "a" is a matrix (i.e. a%nrow /= 1 and a%ncol /= 1):
!     diagi(a,k) forms the n-vector whose elements are on the k-th diagonal of a (if k is not 
!     present, consider k = 0, i.e. main diagonal).
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: kdiag
   type   (pk2_t) :: tmp
!---------------------------------------------------------------------------------------------       
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subDIAGn",                         &
                         msg = " << c = diag(a[,k]) >> with << a >> empty (--> c = [ ])" )
      res = pk2_t()
      return
   end if
   
   if ( present(k) ) then
      kdiag = k
   else 
      kdiag = 0
   end if      
      
   if ( a%nrow == 1 .or. a%ncol == 1 ) then
!
!-    Cas ou a est une colonne ou une ligne a na elements, former la matrice carree 
!     de dimensions na+abs(k) dont la k ieme diagonale est formee des elements de a :
!     
      select type (p=>a%m)
         type is (ik2_t)
            call pk2f_subSetIdiag ( pack(p%v, mask = .true.) , tmp, kdiag )
         type is (rk2_t)
            call pk2f_subSetRdiag ( pack(p%v, mask = .true.) , tmp, kdiag )
         type is (ck2_t)
            call pk2f_subSetCdiag ( pack(p%v, mask = .true.) , tmp, kdiag )
         type is (lk2_t)
            call pk2f_subSetLdiag ( pack(p%v, mask = .true.) , tmp, kdiag )
         type is (sk2_t)
            call pk2f_subSetSdiag ( pack(p%v, mask = .true.) , tmp, kdiag )
      end select
   else   
!
!-    Cas ou a est une matrice de dimensions na x ma, former la matrice colonne dont les
!     elements sont ceux de la k ieme diagonale de a :
!
      select type (p=>a%m)
         type is (ik2_t)
            call pk2f_subGetIdiag ( p%v, tmp, kdiag )
         type is (rk2_t)
            call pk2f_subGetRdiag ( p%v, tmp, kdiag )
         type is (ck2_t)
            call pk2f_subGetCdiag ( p%v, tmp, kdiag )
         type is (lk2_t)
            call pk2f_subGetLdiag ( p%v, tmp, kdiag )
         type is (sk2_t)
            call pk2f_subGetSdiag ( p%v, tmp, kdiag )
      end select
   end if   
   
   error_TraceNreturn(opflag, "pk2f_subDIAGn")  
    
   call pk2_movealloc (from = tmp, to = res)
                  
   END SUBROUTINE pk2f_subDIAGn


!=============================================================================================   
   FUNCTION pk2f_DIAGn ( a, k ) result ( res )
!=============================================================================================   
   class  (pk2_t),           intent(in) :: a
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       
  
   call pk2f_SubDIAGn ( a, res, k ) ; error_TraceNreturn(opflag, "pk2f_DIAGn")

   END FUNCTION pk2f_DIAGn


!=============================================================================================   
   SUBROUTINE pk2f_subDIAGwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------       
!  Extracts the diagonal of a matrix or forms a diagonal matrix from an n-vector
!
!  1) If a=matrs(1) is an n-vector (i.e. a%nrow or a%ncol = 1 and n = a%nrow * a%ncol):
!
!   . If size(matrs) == 1: forms the diagonal matrix "res" with the entries of "a" on its main
!                          diagonal.
!
!   . If size(matrs) == 2: forms the square matrix "res" of size n+abs(k) with the entries of 
!                          "a" on its k-thdiagonal where k is the first entry of matrs(2) 
!                          (if matrs(2) is empty, consider k=0).
!
!  2) If a=matrs(1) is a matrix (i.e. a%nrow /= 1 and a%ncol /= 1):
!
!   . If size(matrs) == 1: forms the n-vector whose elements are on the main diagonal of "a".
!
!   . If size(matrs) == 2: forms the n-vector whose elements are on the k-th diagonal of "a" 
!                          where k is the first entry of matrs(2) (if matrs(2) is empty, 
!                          consider k = 0).
!
!  3) If matrs is not present, returns the help in res.
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: k, nargs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   if ( nargs /= 1 .and. nargs /=2 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subDIAGwrp",                        &
                         msg = '1 or 2 argument(s) expected for the function << diag >>' )
      return
   end if

   if ( matrs(1)%typ  == EMPTY .or. .not. allocated(matrs(1)%m) .or. &
        matrs(1)%nrow == 0     .or. matrs(1)%ncol == 0               ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subDIAGwrp",                       &
                         msg = " << c = diag(a[,b]) >> with << a >> empty (--> c = [ ])" )
      return
   end if
         
   k = 0
   if ( nargs == 2 ) then
      if ( matrs(2)%typ /= EMPTY .and. allocated(matrs(2)%m) ) then ! si vide, prendre k = 0  
         select type (p=>matrs(2)%m)
            type is (ik2_t)
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  k = p%v(1,1)  
            type is (rk2_t)   
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  k = int(p%v(1,1),kind=Ikind) 
            class default
               call opflag%set( stat = UERROR, where = "pk2f_subDIAGwrp", msg =              &
                             "arguments b in << diag(a,b) >> must be an integer (or a real)" )
               return
         end select
      end if            
   end if      
   
   call pk2f_subDIAGn ( a = matrs(1), res = res, k = k )
   
   error_TraceNreturn(opflag, "pk2f_subDIAGwrp")
                  
   END SUBROUTINE pk2f_subDIAGwrp


!=============================================================================================   
   FUNCTION pk2f_DIAGwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------            

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(16), f
!---------------------------------------------------------------------------------------------      

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('diag',pk2f_helpcol)
      h( 1) = "<< "+f+" >> Extracts the diagonal of a matrix or"
      h( 2) = "           forms a diagonal matrix from an n-vector"
      h( 3) = " "
      h( 4) = "Syntax: u = "+f+"(A), u = "+f+"(A,k), D = "+f+"(v), D = "+f+"(v,k)"
      h( 5) = " "
      h( 6) = " . "+f+"(A)   returns a column vector matrix whose elements are"
      h( 7) = "             those of the main diagonal of A"
      h( 8) = " . "+f+"(A,k) returns a column vector matrix whose elements are"
      h( 9) = "             those of the k-th diagonal of A (k > 0 for a upper"
      h(10) = "             diagonal, k < 0 for lower diagonal)"
      h(11) = " "
      h(12) = " . "+f+"(v)   returns the square matrix whose main diagonal is"
      h(13) = "             formed by the elements of v"
      h(14) = " . "+f+"(v,k) returns the square matrix whose k-th diagonal is"
      h(15) = "             formed by the elements of v" 
      h(16) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subDIAGwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_DIAGwrp")
           
   END FUNCTION pk2f_DIAGwrp


!=============================================================================================
   SUBROUTINE pk2f_subSetIdiag ( vector, res, k )
!=============================================================================================
   integer(Ikind),           intent(in    ) :: vector(:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------
!  Puts the values of the vector "vector" on the k th diagonal of a square matrix (of size
!  n+abs(k))
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, na, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   na = size(vector) 
   
   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
   
   if ( kk > 0 ) then
      nc = na + kk ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = na - kk ; i0 =-kk ; j0 = 0
   else
      nc = na      ; i0 = 0  ; j0 = 0         
   end if

   if ( nc <= 0 ) return
      
   allocate(ik2_t::res%m)

   err = 0
      
   select type (p=>res%m)
      type is (ik2_t)
         allocate(p%v(nc,nc), source = IZERO, stat = err)
         if ( err == 0 ) then
            do i = 1, na ; p%v(i+i0,i+j0) = vector(i) ; end do   
         end if   
         !forall (i=1:na) p%v(i+i0,i+j0) = vector(i) ! run-time error with nagfor
   end select
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set ( stat = IERROR, where = "pk2f_subSetIdiag", msg ="Allocation failure" )
   end if
            
   END SUBROUTINE pk2f_subSetIdiag


!=============================================================================================
   SUBROUTINE pk2f_subSetRdiag ( vector, res, k )
!=============================================================================================
   real   (Rkind),           intent(in    ) :: vector(:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k   
!---------------------------------------------------------------------------------------------
!  Puts the values of the vector "vector" on the k th diagonal of a square matrix (of size
!  n+abs(k))
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, na, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   na = size(vector) 
   
   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
   
   if ( kk > 0 ) then
      nc = na + kk ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = na - kk ; i0 =-kk ; j0 = 0
   else
      nc = na      ; i0 = 0  ; j0 = 0         
   end if
         
   if ( nc <= 0 ) return
   
   allocate(rk2_t::res%m)

   err = 0
      
   select type (p=>res%m)
      type is (rk2_t)
         allocate(p%v(nc,nc), source = RZERO, stat = err)
         if ( err == 0 ) then
            do i = 1, na ; p%v(i+i0,i+j0) = vector(i) ; end do   
         end if   
   end select
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subSetRdiag", msg = "Allocation failure" )
   end if
   
   END SUBROUTINE pk2f_subSetRdiag


!=============================================================================================
   SUBROUTINE pk2f_subSetCdiag ( vector, res, k )
!=============================================================================================
   complex(Rkind),           intent(in    ) :: vector(:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k   
!---------------------------------------------------------------------------------------------
!  Puts the values of the vector "vector" on the k th diagonal of a square matrix (of size
!  n+abs(k))
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, na, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   na = size(vector) 

   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
   
   if ( kk > 0 ) then
      nc = na + kk ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = na - kk ; i0 =-kk ; j0 = 0
   else
      nc = na      ; i0 = 0  ; j0 = 0         
   end if

   if ( nc <= 0 ) return
   
   allocate(ck2_t::res%m)

   err = 0
      
   select type (p=>res%m)
      type is (ck2_t)
         allocate(p%v(nc,nc), source = CZERO, stat = err)
         if ( err == 0 ) then
            do i = 1, na ; p%v(i+i0,i+j0) = vector(i) ; end do   
         end if   
   end select
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subSetCdiag", msg = "Allocation failure" )
   end if
   
   END SUBROUTINE pk2f_subSetCdiag


!=============================================================================================
   SUBROUTINE pk2f_subSetLdiag ( vector, res, k )
!=============================================================================================
   logical       ,           intent(in    ) :: vector(:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------
!  Puts the values of the vector "vector" on the k th diagonal of a square matrix (of size
!  n+abs(k))
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, na, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   na = size(vector) 

   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
   
   if ( kk > 0 ) then
      nc = na + kk ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = na - kk ; i0 =-kk ; j0 = 0
   else
      nc = na      ; i0 = 0  ; j0 = 0         
   end if
         
   if ( nc <= 0 ) return
      
   allocate(lk2_t::res%m)

   err = 0
      
   select type (p=>res%m)
      type is (lk2_t)
         allocate(p%v(nc,nc), source = LZERO, stat = err)
         if ( err == 0 ) then
            do i = 1, na ; p%v(i+i0,i+j0) = vector(i) ; end do   
         end if   
   end select
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subSetLdiag", msg = "Allocation failure" )
   end if
   
   END SUBROUTINE pk2f_subSetLdiag


!=============================================================================================
   SUBROUTINE pk2f_subSetSdiag ( vector, res, k )
!=============================================================================================
   type   (str_t),           intent(in    ) :: vector(:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k   
!---------------------------------------------------------------------------------------------
!  Puts the values of the vector "vector" on the k th diagonal of a square matrix (of size
!  n+abs(k))
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, na
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   na = size(vector) 

   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
   
   if ( kk > 0 ) then
      nc = na + kk ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = na - kk ; i0 =-kk ; j0 = 0
   else
      nc = na      ; i0 = 0  ; j0 = 0         
   end if
         
   if ( nc <= 0 ) return

   res = pk2_t(typ=STYP, shape=[nc,nc])
   
   if ( opflag%code > 0 ) then
      deallocate(res%m)
      call opflag%addTrace("pk2f_subSetSdiag")
      return
   end if   
      
   select type (p=>res%m)
      type is (sk2_t)
         do i = 1, na
            if ( allocated(vector(i)%str) ) then
               p%v(i+i0,i+j0)%str = (vector(i)%str)
            else
               p%v(i+i0,i+j0)%str = ''
            end if  
         end do 
   end select
   
   call res%resetDesc()
   
   END SUBROUTINE pk2f_subSetSdiag


!=============================================================================================
   FUNCTION pk2f_SetIdiag ( vector, k ) result( res )
!=============================================================================================
   integer(Ikind),           intent(in) :: vector(:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subSetIdiag ( vector, res, k ) ; error_TraceNreturn(opflag, "pk2f_SetIdiag")
   
   END FUNCTION pk2f_SetIdiag


!=============================================================================================
   FUNCTION pk2f_SetRdiag ( vector, k ) result( res )
!=============================================================================================
   real   (Rkind),           intent(in) :: vector(:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subSetRdiag ( vector, res, k ) ; error_TraceNreturn(opflag, "pk2f_SetRdiag")
   
   END FUNCTION pk2f_SetRdiag


!=============================================================================================
   FUNCTION pk2f_SetCdiag ( vector, k ) result( res )
!=============================================================================================
   complex(Rkind),           intent(in) :: vector(:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subSetCdiag ( vector, res, k ) ; error_TraceNreturn(opflag, "pk2f_SetICiag")
   
   END FUNCTION pk2f_SetCdiag

   
!=============================================================================================
   FUNCTION pk2f_SetLdiag ( vector, k ) result( res )
!=============================================================================================
   logical       ,           intent(in) :: vector(:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subSetLdiag ( vector, res, k ) ; error_TraceNreturn(opflag, "pk2f_SetLdiag")
   
   END FUNCTION pk2f_SetLdiag
   

!=============================================================================================
   FUNCTION pk2f_SetSdiag ( vector, k ) result( res )
!=============================================================================================
   type   (str_t),           intent(in) :: vector(:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subSetSdiag ( vector, res, k ) ; error_TraceNreturn(opflag, "pk2f_SetSdiag")
   
   END FUNCTION pk2f_SetSdiag
      
   
!=============================================================================================
   SUBROUTINE pk2f_subGetIdiag ( matrix, res, k )
!=============================================================================================
   integer(Ikind),           intent(in    ) :: matrix(:,:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------
!  Gets the k th diagonal of "matrix" and puts the result in res
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
   
   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
            
   if ( kk > 0 ) then
      nc = min(nrow, ncol - kk) ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = min(ncol, nrow + kk) ; i0 =-kk ; j0 = 0
   else
      nc = min(nrow,ncol)       ; i0 = 0  ; j0 = 0
   end if         

   if ( nc <= 0 ) return
      
   allocate(ik2_t::res%m)

   err = 0
   
   select type (p=>res%m)
      type is (ik2_t)
         allocate(p%v(nc,IONE), stat = err)
         if ( err == 0 ) then
            do i = 1, nc ; p%v(i,1) = matrix(i+i0,i+j0) ; end do  
         end if    
   end select      
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subGetIdiag", msg = "Allocation failure" )
   end if

   END SUBROUTINE pk2f_subGetIdiag


!=============================================================================================
   SUBROUTINE pk2f_subGetRdiag ( matrix, res, k )
!=============================================================================================
   real   (Rkind),           intent(in    ) :: matrix(:,:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------
!  Gets the k th diagonal of "matrix" and puts the result in res
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)

   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
            
   if ( kk > 0 ) then
      nc = min(nrow, ncol - kk) ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = min(ncol, nrow + kk) ; i0 =-kk ; j0 = 0
   else
      nc = min(nrow,ncol)       ; i0 = 0  ; j0 = 0
   end if         

   if ( nc <= 0 ) return
   
   allocate(rk2_t::res%m)

   err = 0
   
   select type (p=>res%m)
      type is (rk2_t)
         allocate(p%v(nc,IONE), stat = err)
         if ( err == 0 ) then
            do i = 1, nc ; p%v(i,1) = matrix(i+i0,i+j0) ; end do  
         end if    
   end select      
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subGetRdiag", msg = "Allocation failure" )
   end if

   END SUBROUTINE pk2f_subGetRdiag


!=============================================================================================
   SUBROUTINE pk2f_subGetCdiag ( matrix, res, k )
!=============================================================================================
   complex(Rkind),           intent(in    ) :: matrix(:,:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------
!  Gets the k th diagonal of "matrix" and puts the result in res
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)

   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
            
   if ( kk > 0 ) then
      nc = min(nrow, ncol - kk) ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = min(ncol, nrow + kk) ; i0 =-kk ; j0 = 0
   else
      nc = min(nrow,ncol)       ; i0 = 0  ; j0 = 0
   end if         

   if ( nc <= 0 ) return
   
   allocate(ck2_t::res%m)

   err = 0
   
   select type (p=>res%m)
      type is (ck2_t)
         allocate(p%v(nc,IONE), stat = err)
         if ( err == 0 ) then
            do i = 1, nc ; p%v(i,1) = matrix(i+i0,i+j0) ; end do  
         end if    
   end select      
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subGetCdiag", msg = "Allocation failure" )
   end if

   END SUBROUTINE pk2f_subGetCdiag


!=============================================================================================
   SUBROUTINE pk2f_subGetLdiag ( matrix, res, k )
!=============================================================================================
   logical       ,           intent(in    ) :: matrix(:,:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------
!  Gets the k th diagonal of "matrix" and puts the result in res
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)

   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if
            
   if ( kk > 0 ) then
      nc = min(nrow, ncol - kk) ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = min(ncol, nrow + kk) ; i0 =-kk ; j0 = 0
   else
      nc = min(nrow,ncol)       ; i0 = 0  ; j0 = 0
   end if         

   if ( nc <= 0 ) return
   
   allocate(lk2_t::res%m)

   err = 0
   
   select type (p=>res%m)
      type is (lk2_t)
         allocate(p%v(nc,IONE), stat = err)
         if ( err == 0 ) then
            do i = 1, nc ; p%v(i,1) = matrix(i+i0,i+j0) ; end do  
         end if    
   end select      
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subGetLdiag", msg = "Allocation failure" )
   end if

   END SUBROUTINE pk2f_subGetLdiag


!=============================================================================================
   SUBROUTINE pk2f_subGetSdiag ( matrix, res, k )
!=============================================================================================
   type   (str_t),           intent(in    ) :: matrix(:,:)
   class  (pk2_t),           intent(   out) :: res
   integer(Ikind), optional, intent(in    ) :: k   
!---------------------------------------------------------------------------------------------
!  Gets the k-th diagonal of "matrix" and puts the result in res
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: kk, i, i0, j0, nc, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
   
   if ( present(k) ) then
      kk = k
   else
      kk = 0
   end if

   if ( kk > 0 ) then
      nc = min(nrow, ncol - kk) ; i0 = 0  ; j0 = kk
   else if ( kk < 0 ) then
      nc = min(ncol, nrow + kk) ; i0 =-kk ; j0 = 0
   else
      nc = min(nrow,ncol)       ; i0 = 0  ; j0 = 0
   end if         
               
   if ( nc <= 0 ) return
      
   allocate(sk2_t::res%m)
   
   err = 0
   
   select type (p=>res%m)
      type is (sk2_t)
         allocate(p%v(nc,IONE), stat = err)
         if ( err == 0 ) then
            do i = 1, nc
               if ( allocated(matrix(i+i0,i+j0)%str) ) then
                  p%v(i,1)%str = trim( matrix(i+i0,i+j0)%str )
               else   
                  p%v(i,1)%str = ''
               end if  
            end do 
         end if    
   end select      
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subGetSdiag", msg = "Allocation failure" )
   end if

   END SUBROUTINE pk2f_subGetSdiag


!=============================================================================================
   FUNCTION pk2f_GetIdiag ( matrix, k ) result( res )
!=============================================================================================
   integer(Ikind),           intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetIdiag ( matrix, res, k ) ; error_TraceNreturn(opflag, "pk2f_GetIdiag")
   
   END FUNCTION pk2f_GetIdiag


!=============================================================================================
   FUNCTION pk2f_GetRdiag ( matrix, k ) result( res )
!=============================================================================================
   real   (Rkind),           intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetRdiag ( matrix, res, k ) ; error_TraceNreturn(opflag, "pk2f_GetRdiag")
   
   END FUNCTION pk2f_GetRdiag


!=============================================================================================
   FUNCTION pk2f_GetCdiag ( matrix, k ) result( res )
!=============================================================================================
   complex(Rkind),           intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetCdiag ( matrix, res, k ) ; error_TraceNreturn(opflag, "pk2f_GetCdiag")
   
   END FUNCTION pk2f_GetCdiag


!=============================================================================================
   FUNCTION pk2f_GetLdiag ( matrix, k ) result( res )
!=============================================================================================
   logical       ,           intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetLdiag ( matrix, res, k ) ; error_TraceNreturn(opflag, "pk2f_GetLdiag")
   
   END FUNCTION pk2f_GetLdiag


!=============================================================================================
   FUNCTION pk2f_GetSdiag ( matrix, k ) result( res )
!=============================================================================================
   type   (str_t),           intent(in) :: matrix(:,:)
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetSdiag ( matrix, res, k ) ; error_TraceNreturn(opflag, "pk2f_GetSdiag")
   
   END FUNCTION pk2f_GetSdiag
   

!=============================================================================================   
   SUBROUTINE pk2f_subTRIL ( a, b, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a, b
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------       
!  Forms the lower triangular part of "a" on and above its k th diagonal where k is the 1st
!  entry of b (k = 0 corresponds to the main diagonal)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------  
   integer(Ikind) :: k  
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subTRIL",                        &
                         msg = " << c = tril(a,k) >> with << a >> empty (--> c = [ ])" )
      return
   end if
   
   k = 0
   if ( b%typ /= EMPTY .and. allocated(b%m) ) then ! si vide ou non-alloue, prendre k = 0  
      select type (p=>b%m)
         type is (ik2_t)
            if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) k = p%v(1,1)  
         type is (rk2_t)   
            if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  k = int(p%v(1,1),kind=Ikind) 
         class default
            call opflag%set ( stat = UERROR, where = "pk2f_subTRIL", msg =             &
                  "argument << b >> in << tril(a,b) >> must be an integer (or a real)" )
            return
      end select
   end if            
   
   call pk2f_SubTRILn ( a, res, k ) ; error_TraceNreturn(opflag, "pk2f_subTRIL")
               
   END SUBROUTINE pk2f_subTRIL


!=============================================================================================   
   FUNCTION pk2f_TRIL ( a, b ) result ( res )
!=============================================================================================   
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------       
   
   call pk2f_SubTRIL ( a, b, res ) ; error_TraceNreturn(opflag, "pk2f_TRIL")
               
   END FUNCTION pk2f_TRIL


!=============================================================================================   
   SUBROUTINE pk2f_subTRILn ( a, res, k )
!=============================================================================================   
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out) :: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------       
!  Forms the upper triangular part of "a" on and above its k th diagonal 
!  (k = 0 corresponds to the main diagonal)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------  
   integer(Ikind) :: kdiag   
   type   (pk2_t) :: tmp
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subTRILn",                       &
                         msg = " << c = tril(a,k) >> with << a >> empty (--> c = [ ])" )
      return
   end if
   
   if ( present(k) ) then
      kdiag = k
   else
      kdiag = 0
   end if
            
   select type (p=>a%m)
      type is (ik2_t)
         call pk2f_subGetItril ( p%v, kdiag, tmp )
      type is (rk2_t)
         call pk2f_subGetRtril ( p%v, kdiag, tmp )
      type is (ck2_t)
         call pk2f_subGetCtril ( p%v, kdiag, tmp )
      type is (lk2_t)
         call pk2f_subGetLtril ( p%v, kdiag, tmp )
      type is (sk2_t)
         call pk2f_subGetStril ( p%v, kdiag, tmp ) 
   end select
   
   error_TraceNreturn(opflag, "pk2f_subTRILn")   
   
   call pk2_movealloc ( from = tmp, to = res )
                     
   END SUBROUTINE pk2f_subTRILn
   

!=============================================================================================   
   FUNCTION pk2f_TRILn ( a, k ) result ( res )
!=============================================================================================   
   class  (pk2_t),           intent(in) :: a
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       
!  Forms the upper triangular part of "a" on and above its k th diagonal 
!  (k = 0 corresponds to the main diagonal)
!-----------------------------------------------------------------------------------R.H. 04/18       

   call pk2f_subTRILn ( a, res, k ) ; error_TraceNreturn(opflag, "pk2f_TRILn") 
                   
   END FUNCTION pk2f_TRILn
   

!=============================================================================================   
   SUBROUTINE pk2f_subTRILwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------       
!  Forms the lower triangular part of a=matrs(1),
!
!  . If size(matrs) == 1: on and below its main diagonal.
!  . If size(matrs) == 2: on and below its k-th diagonal, with k is the first entry of matrs(2).
!                         (if matrs(2) is empty, consider k = 0, i.e. main diagonal).
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: k, nargs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nargs = size(matrs)
   
   if ( nargs /= 1 .and. nargs /= 2 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subTRILwrp",                        &
                         msg = "1 or 2 argument(s) expected for the function << tril >>" )
      return
   end if

   if ( matrs(1)%typ  == EMPTY .or. .not. allocated(matrs(1)%m) .or. &
        matrs(1)%nrow == 0     .or. matrs(1)%ncol == 0                ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subTRILwrp",                       &
                         msg = " << c = tril(a[,b]) >> with << a >> empty (--> c = [ ])" )
      return
   end if
         
   k = 0
   if ( nargs == 2 ) then
      if ( matrs(2)%typ /= EMPTY .and. allocated(matrs(2)%m) ) then ! si vide, prendre k=0  
         select type (p=>matrs(2)%m)
            type is (ik2_t)
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) k = p%v(1,1)  
            type is (rk2_t)   
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  k = int(p%v(1,1),kind=Ikind) 
            class default
               call opflag%set ( stat = UERROR, where = "pk2f_subTRILwrp", msg =          &
                     "argument << b >> in << tril(a,b) >> must be an integer (or a real)" )
               return
         end select
      end if            
   end if      
   
   call pk2f_SubTRILn ( matrs(1), res, k ) ; error_TraceNreturn(opflag, "pk2f_subTRILwrp")
                           
   END SUBROUTINE pk2f_subTRILwrp


!=============================================================================================   
   FUNCTION pk2f_TRILwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       
!  Forms the lower triangular part of a=matrs(1),
!
!  . If size(matrs) == 1: on and below its main diagonal.
!  . If size(matrs) == 2: on and below its k-th diagonal, with k is the first entry of matrs(2).
!                         (if matrs(2) is empty, consider k = 0, i.e. main diagonal).
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(9), f
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!     
      f = str_color('tril',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the lower triangular part of a matrix"
      h(2) = " "
      h(3) = "Syntax: L = "+f+"(A), U= "+f+"(A,k)"
      h(4) = " "
      h(5) = "  . "+f+"( A ) returns the lower triangular part of A"
      h(6) = "  . "+f+"(A,k) returns the lower triangular part of A which is on and below"
      h(7) = "              its k-th diagonal (k > 0 for a upper diagonal, k < 0 for a"
      h(8) = "              lower diagonal)"
      h(9) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subTRILwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_TRILwrp")
                           
   END FUNCTION pk2f_TRILwrp
      

!=============================================================================================   
   SUBROUTINE pk2f_subTRIU ( a, b, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a, b
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------       
!  Forms the upper triangular part of "a" on and above its k th diagonal where k is the 1st
!  entry of b (k = 0 corresponds to the main diagonal)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------  
   integer(Ikind) :: k  
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subTRIU",                        &
                         msg = " << c = triu(a,k) >> with << a >> empty (--> c = [ ])" )
      return
   end if
   
   k = 0
   if ( b%typ /= EMPTY .and. allocated(b%m) ) then ! si vide ou non-alloue, prendre k = 0  
      select type (p=>b%m)
         type is (ik2_t)
            if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) k = p%v(1,1)  
         type is (rk2_t)   
            if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  k = int(p%v(1,1),kind=Ikind) 
         class default
            call opflag%set ( stat = UERROR, where = "pk2f_subTRIU", msg =             &
                  "argument << b >> in << triu(a,b) >> must be an integer (or a real)" )
            return
      end select
   end if            
   
   call pk2f_subTRIUn ( a, res, k ) ; error_TraceNreturn(opflag, "pk2f_subTRIU")
                     
   END SUBROUTINE pk2f_subTRIU


!=============================================================================================   
   FUNCTION pk2f_TRIU ( a, b ) result ( res )
!=============================================================================================   
   class(pk2_t), intent(in) :: a, b
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------       

   call pk2f_subTRIU ( a, b, res ) ; error_TraceNreturn(opflag, "pk2f_TRIU")
                     
   END FUNCTION pk2f_TRIU


!=============================================================================================   
   SUBROUTINE pk2f_subTRIUn ( a, res, k )
!=============================================================================================   
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out) :: res
   integer(Ikind), optional, intent(in    ) :: k
!---------------------------------------------------------------------------------------------       
!  Forms the upper triangular part of "a" on and above its k th diagonal 
!  (k = 0 corresponds to the main diagonal)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------  
   integer(Ikind) :: kdiag  
   type   (pk2_t) :: tmp 
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subTRIUn",                       &
                         msg = " << c = triu(a,k) >> with << a >> empty (--> c = [ ])" )
      return
   end if
   
   if ( present(k) ) then
      kdiag = k
   else
      kdiag = 0
   end if
            
   select type (p=>a%m)
      type is (ik2_t)
         call pk2f_subGetItriu ( p%v, kdiag, tmp )
      type is (rk2_t)
         call pk2f_subGetRtriu ( p%v, kdiag, tmp )
      type is (ck2_t)
         call pk2f_subGetCtriu ( p%v, kdiag, tmp )
      type is (lk2_t)
         call pk2f_subGetLtriu ( p%v, kdiag, tmp )
      type is (sk2_t)
         call pk2f_subGetStriu ( p%v, kdiag, tmp )            
   end select
   
   error_TraceNreturn(opflag, "pk2f_subTRIUn")   
   
   call pk2_movealloc ( from = tmp, to = res )
                     
   END SUBROUTINE pk2f_subTRIUn


!=============================================================================================   
   FUNCTION pk2f_TRIUn ( a, k ) result ( res )
!=============================================================================================   
   class  (pk2_t),           intent(in) :: a
   integer(Ikind), optional, intent(in) :: k
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       
!  Forms the upper triangular part of "a" on and above its k th diagonal 
!  (k = 0 corresponds to the main diagonal)
!-----------------------------------------------------------------------------------R.H. 04/18       

   call pk2f_SubTRIUn ( a, res, k ) ; error_TraceNreturn(opflag, "pk2f_TRIUn")
                     
   END FUNCTION pk2f_TRIUn


!=============================================================================================   
   SUBROUTINE pk2f_subTRIUwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------       
!  Forms the upper triangular part of a=matrs(1),
!
!  . If size(matrs) == 1: on and above its main diagonal.
!  . If size(matrs) == 2: on above its k-th diagonal, with k is the first entry of matrs(2).
!                         (if matrs(2) is empty, consider k = 0, i.e. main diagonal).
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: k, nargs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   if ( nargs /= 1 .and. nargs /= 2 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subTRIUwrp",                        &
                         msg = "1 or 2 argument(s) expected for the function << triu >>" )
      return
   end if

   if ( matrs(1)%typ  == EMPTY .or. .not. allocated(matrs(1)%m) .or. &
        matrs(1)%nrow == 0     .or. matrs(1)%ncol == 0               ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subTRIUwrp",                       &
                         msg = " << c = triu(a[,b]) >> with << a >> empty (--> c = [ ])" )
      return
   end if
         
   k = 0
   if ( nargs == 2 ) then
      if ( matrs(2)%typ /= EMPTY .and. allocated(matrs(2)%m) ) then ! si vide, prendre k = 0  
         select type (p=>matrs(2)%m)
            type is (ik2_t)
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) k = p%v(1,1)  
            type is (rk2_t)   
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) &
                                                                  k = int(p%v(1,1),kind=Ikind) 
            class default
               call opflag%set ( stat = UERROR, where = "pk2f_subTRIUwrp", msg =          &
                     "argument << b >> in << triu(a,b) >> must be an integer (or a real)" )
               return
         end select
      end if            
   end if      
   
   call pk2f_subTRIUn ( matrs(1), res, k ) ; error_TraceNreturn(opflag, "pk2f_subTRIUwrp")
                           
   END SUBROUTINE pk2f_subTRIUwrp


!=============================================================================================   
   FUNCTION pk2f_TRIUwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       
!  Forms the upper triangular part of a=matrs(1),
!
!  . If size(matrs) == 1: on and above its main diagonal.
!  . If size(matrs) == 2: on above its k-th diagonal, with k is the first entry of matrs(2).
!                         (if matrs(2) is empty, consider k = 0, i.e. main diagonal).
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(9), f
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!     
      f = str_color('triu',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the upper triangular part of a matrix"
      h(2) = " "
      h(3) = "Syntax: U = "+f+"(A), U= "+f+"(A,k)"
      h(4) = " "
      h(5) = "  . "+f+"( A ) returns the upper triangular part of A"
      h(6) = "  . "+f+"(A,k) returns the upper triangular part of A which is on and above"
      h(7) = "              its k-th diagonal (k > 0 for a upper diagonal, k < 0 for a"
      h(8) = "              lower diagonal)"
      h(9) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subTRIUwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_TRIUwrp")
                      
   END FUNCTION pk2f_TRIUwrp


!=============================================================================================
   SUBROUTINE pk2f_subGetItril ( matrix, k, res )
!=============================================================================================
   integer(Ikind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the lower triangular part of "matrix" on and below its k th diagonal
!
!  Example :  res = pk2f_GetItril (matrix, 3)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   if ( nrow == 0 .or. ncol == 0 ) return
          
   allocate(ik2_t::res%m)
   
   err = 0
   
   select type (p=>res%m)   
      type is (ik2_t)
         allocate(p%v(nrow,ncol), source = IZERO, stat = err)
         if ( err == 0 ) then
            do j = 1, min(nrow+k,ncol)
               do i = max(j-k,IONE), nrow
                  p%v(i,j) = matrix(i,j)
               end do
            end do 
         end if     
   end select

   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set( stat = IERROR, where = "pk2f_subGetItril)", msg = "Allocation failure" )
   end if
   
   END SUBROUTINE pk2f_subGetItril


!=============================================================================================
   SUBROUTINE pk2f_subGetRtril ( matrix, k, res )
!=============================================================================================
   real   (Rkind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the lower triangular part of "matrix" on and below its k th diagonal
!
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   if ( nrow == 0 .or. ncol == 0 ) return
          
   allocate(rk2_t::res%m)
   
   err = 0
   
   select type (p=>res%m)   
      type is (rk2_t)
         allocate(p%v(nrow,ncol), source = RZERO, stat = err)
         if ( err == 0 ) then
            do j = 1, min(nrow+k,ncol)
               do i = max(j-k,IONE), nrow
                  p%v(i,j) = matrix(i,j)
               end do
            end do 
         end if     
   end select

   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set(stat = IERROR, where = "pk2f_subGetRtril)", msg = "Allocation failure")
   end if

   END SUBROUTINE pk2f_subGetRtril


!=============================================================================================
   SUBROUTINE pk2f_subGetCtril ( matrix, k, res )
!=============================================================================================
   complex(Rkind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the lower triangular part of "matrix" on and below its k th diagonal
!
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   if ( nrow == 0 .or. ncol == 0 ) return
          
   allocate(ck2_t::res%m)

   err = 0
   
   select type (p=>res%m)   
      type is (ck2_t)
         allocate(p%v(nrow,ncol), source = CZERO, stat = err)
         if ( err == 0 ) then
            do j = 1, min(nrow+k,ncol)
               do i = max(j-k,IONE), nrow
                  p%v(i,j) = matrix(i,j)
               end do
            end do 
         end if     
   end select
   
   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set(stat = IERROR, where = "pk2f_subGetCtril)", msg = "Allocation failure")
   end if
   
   END SUBROUTINE pk2f_subGetCtril


!=============================================================================================
   SUBROUTINE pk2f_subGetLtril ( matrix, k, res )
!=============================================================================================
   logical       , intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the lower triangular part of "matrix" on and below its k th diagonal
!
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   if ( nrow == 0 .or. ncol == 0 ) return
          
   allocate(lk2_t::res%m)

   err = 0
   
   select type (p=>res%m)   
      type is (lk2_t)
         allocate(p%v(nrow,ncol), source = LZERO, stat = err)
         if ( err == 0 ) then
            do j = 1, min(nrow+k,ncol)
               do i = max(j-k,IONE), nrow
                  p%v(i,j) = matrix(i,j)
               end do
            end do 
         end if     
   end select

   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set(stat = IERROR, where = "pk2f_subGetLtril)", msg = "Allocation failure")
   end if

   END SUBROUTINE pk2f_subGetLtril


!=============================================================================================
   SUBROUTINE pk2f_subGetStril ( matrix, k, res )
!=============================================================================================
   type   (str_t), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the lower triangular part of "matrix" on and below its k th diagonal
!
!  Example :  res = pk2f_GetStril (matrix, 3)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   if ( nrow == 0 .or. ncol == 0 ) return
          
   res = pk2_t(typ = STYP, shape = [nrow, ncol])
   
   if ( opflag%code > 0 ) then
      deallocate(res%m) ; call opflag%AddTrace("pk2f_subGetStril") ; return
   end if
   
   select type (p=>res%m)   
      type is (sk2_t)
         do j = 1, min(nrow+k,ncol)
            do i = max(j-k,IONE), nrow
               if ( allocated( matrix(i,j)%str) ) then
                  p%v(i,j)%str = (matrix(i,j)%str)
               else
                  p%v(i,j)%str = ''
               end if      
            end do
         end do
   end select

   call res%resetDesc()
   
   END SUBROUTINE pk2f_subGetStril


!=============================================================================================
   FUNCTION pk2f_GetItril ( matrix, k ) result ( res )
!=============================================================================================
   integer(Ikind), intent(in) :: matrix(:,:)
   integer(Ikind), intent(in) :: k
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetItril ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetItril")
   
   END FUNCTION pk2f_GetItril
   
   
!=============================================================================================
   FUNCTION pk2f_GetRtril ( matrix, k ) result ( res )
!=============================================================================================
   real   (Rkind), intent(in) :: matrix(:,:)
   integer(Ikind), intent(in) :: k
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetRtril ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetRtril")
   
   END FUNCTION pk2f_GetRtril


!=============================================================================================
   FUNCTION pk2f_GetCtril ( matrix, k ) result ( res )
!=============================================================================================
   complex(Rkind), intent(in) :: matrix(:,:)
   integer(Ikind), intent(in) :: k
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetCtril ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetCtril")
   
   END FUNCTION pk2f_GetCtril


!=============================================================================================
   FUNCTION pk2f_GetLtril ( matrix, k ) result ( res )
!=============================================================================================
   logical       , intent(in) :: matrix(:,:)
   integer(Ikind), intent(in) :: k
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetLtril ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetLtril")
   
   END FUNCTION pk2f_GetLtril

   
!=============================================================================================
   FUNCTION pk2f_GetStril ( matrix, k ) result ( res )
!=============================================================================================
   type   (str_t), intent(in) :: matrix(:,:)
   integer(Ikind), intent(in) :: k
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetStril ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetStril")
   
   END FUNCTION pk2f_GetStril
   
   
!=============================================================================================
   SUBROUTINE pk2f_subGetItriu ( matrix, k, res )
!=============================================================================================
   integer(Ikind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the upper triangular part of "matrix" on and above its k th diagonal
!
!  Example :  call pk2f_GetItriu (matrix, 3, res)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
   
   if ( nrow == 0 .or. ncol == 0 ) return
          
   allocate(ik2_t::res%m)

   err = 0
   
   select type (p=>res%m)   
      type is (ik2_t)
         allocate(p%v(nrow,ncol), source = IZERO, stat = err)
         if ( err == 0 ) then
            do j = max(IONE,k+1), ncol
               do i = 1, min(j-k,nrow)
                  p%v(i,j) = matrix(i,j)
               end do
            end do 
         end if  
   end select

   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set(stat = IERROR, where = "pk2f_subGetItriu", msg = "Allocation failure")
   end if   

   END SUBROUTINE pk2f_subGetItriu
   

!=============================================================================================
   SUBROUTINE pk2f_subGetRtriu ( matrix, k, res )
!=============================================================================================
   real   (Rkind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the upper triangular part of "matrix" on and above its k th diagonal
!
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   if ( nrow == 0 .or. ncol == 0 ) return
          
   allocate(rk2_t::res%m)

   err = 0
   
   select type (p=>res%m)   
      type is (rk2_t)
         allocate(p%v(nrow,ncol), source = RZERO, stat = err)
         if ( err == 0 ) then
            do j = max(IONE,k+1), ncol
               do i = 1, min(j-k,nrow)
                  p%v(i,j) = matrix(i,j)
               end do
            end do 
         end if  
   end select

   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set(stat = IERROR, where = "pk2f_subGetRtriu", msg = "Allocation failure")
   end if   

   END SUBROUTINE pk2f_subGetRtriu


!=============================================================================================
   SUBROUTINE pk2f_subGetCtriu ( matrix, k, res )
!=============================================================================================
   complex(Rkind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the upper triangular part of "matrix" on and above its k th diagonal
!
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   allocate(ck2_t::res%m)

   err = 0
   
   select type (p=>res%m)   
      type is (ck2_t)
         allocate(p%v(nrow,ncol), source = CZERO, stat = err)
         if ( err == 0 ) then
            do j = max(IONE,k+1), ncol
               do i = 1, min(j-k,nrow)
                  p%v(i,j) = matrix(i,j)
               end do
            end do 
         end if  
   end select

   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set(stat = IERROR, where = "pk2f_subGetCtriu", msg = "Allocation failure")
   end if   
   
   END SUBROUTINE pk2f_subGetCtriu


!=============================================================================================
   SUBROUTINE pk2f_subGetLtriu ( matrix, k, res )
!=============================================================================================
   logical       , intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the upper triangular part of "matrix" on and above its k th diagonal
!
!  Example :  call pk2f_GetLtriu (matrix, 3, res)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol, err
!---------------------------------------------------------------------------------------------   

   if (opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   allocate(lk2_t::res%m)

   err = 0
   select type (p=>res%m)   
      type is (lk2_t)
         allocate(p%v(nrow,ncol), source = LZERO, stat = err)
         if ( err == 0 ) then
            do j = max(IONE,k+1), ncol
               do i = 1, min(j-k,nrow)
                  p%v(i,j) = matrix(i,j)
               end do
            end do 
         end if  
   end select

   if ( err == 0 ) then
      call res%resetDesc()
   else
      deallocate(res%m)
      call opflag%set(stat = IERROR, where = "pk2f_subGetLtriu", msg = "Allocation failure")
   end if   
   
   END SUBROUTINE pk2f_subGetLtriu


!=============================================================================================
   SUBROUTINE pk2f_subGetStriu ( matrix, k, res )
!=============================================================================================
   type   (str_t), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   class  (pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------
!  Gets the upper triangular part of "matrix" on and above its k-th diagonal
!
!  Example :  call pk2f_GetStriu (matrix, 3, res)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j, nrow, ncol
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nrow = size(matrix,dim=1) ; ncol = size(matrix,dim=2)
    
   if ( nrow == 0 .or. ncol == 0 ) return
          
   res = pk2_t(typ = STYP, shape = [nrow,ncol])
   
   if ( opflag%code > 0 ) then
      deallocate(res%m) ; call opflag%AddTrace("pk2f_subGetStriu") ; return
   end if

   select type (p=>res%m)   
      type is (sk2_t)
         do j = max(IONE,k+1), ncol
            do i = 1, min(j-k,nrow)
               if ( allocated(matrix(i,j)%str) ) then
                  p%v(i,j)%str = (matrix(i,j)%str)
               else
                  p%v(i,j)%str = ''
               end if      
            end do
         end do  
   end select

   call res%resetDesc()
   
   END SUBROUTINE pk2f_subGetStriu
   

!=============================================================================================
   FUNCTION pk2f_GetItriu ( matrix, k ) result ( res )
!=============================================================================================
   integer(Ikind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetItriu ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetItriu")
   
   END FUNCTION pk2f_GetItriu


!=============================================================================================
   FUNCTION pk2f_GetRtriu ( matrix, k ) result ( res )
!=============================================================================================
   real   (Rkind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetRtriu ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetRtriu")
   
   END FUNCTION pk2f_GetRtriu


!=============================================================================================
   FUNCTION pk2f_GetCtriu ( matrix, k ) result ( res )
!=============================================================================================
   complex(Rkind), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetCtriu ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetCtriu")
   
   END FUNCTION pk2f_GetCtriu
   
   
!=============================================================================================
   FUNCTION pk2f_GetLtriu ( matrix, k ) result ( res )
!=============================================================================================
   logical       , intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetLtriu ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetLtriu")
   
   END FUNCTION pk2f_GetLtriu


!=============================================================================================
   FUNCTION pk2f_GetStriu ( matrix, k ) result ( res )
!=============================================================================================
   type   (str_t), intent(in    ) :: matrix(:,:)
   integer(Ikind), intent(in    ) :: k
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subGetStriu ( matrix, k, res ) ; error_TraceNreturn(opflag, "pk2f_GetStriu")
   
   END FUNCTION pk2f_GetStriu
   
   
!=============================================================================================   
   SUBROUTINE pk2f_subTRACE ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the trace of "a".
!----------------------------------------------------------------------------R.H. 04/18, 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: tmpi, i
   real   (Rkind) :: tmpr
   complex(Rkind) :: tmpc
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%nrow /= a%ncol ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subTRACE",                  &
                         msg = "<< trace >> is only defined for square matrices" )
      return
   end if
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 ) then
      res = IZERO      
      call opflag%set ( stat = WARNING, where = "pk2f_subTRACE",                   &
                         msg = "<< b = trace(a) >> with << a >> empty (--> b = 0)" )
      return
   end if
         
   select type (p=>a%m)
      type is (ik2_t)
         tmpi = IZERO 
         if ( allocated(p%v) ) then
            do i = 1, a%nrow ; tmpi = tmpi + p%v(i,i) ; end do
         end if   
         res = tmpi
      type is (rk2_t)
         tmpr = RZERO
         if ( allocated(p%v) ) then
            do i = 1, a%nrow ; tmpr = tmpr + p%v(i,i) ; end do
         end if
         res = tmpr
      type is (ck2_t)
         tmpc = CZERO
         if ( allocated(p%v) ) then         
            do i = 1, a%nrow ; tmpc = tmpc + p%v(i,i) ; end do
         end if 
         res = tmpc
      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subTRACE", msg =                    &
                  'the function << trace >> needs a matrix of integers, reals or complexes' )
         return
   end select
         
   END SUBROUTINE pk2f_subTRACE


!=============================================================================================   
   FUNCTION pk2f_TRACE ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Returns the trace of a
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(5), f
!---------------------------------------------------------------------------------------------      

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!
      f = str_color('trace',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the sum of the diagonal elements of a matrix."
      h(2) = "            The matrix must be square, integer, real or complex."
      h(3) = " "
      h(4) = "Syntax: t = "+f+"(A)"
      h(5) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subTRACE ( a, res ) ; error_TraceNreturn(opflag, "pk2f_TRACE")
   
   END FUNCTION pk2f_TRACE


!=============================================================================================   
   SUBROUTINE pk2f_subDEV ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the deviatoric part of "a".
!----------------------------------------------------------------------------R.H. 04/18, 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind)              :: i, j, err
   integer(Ikind)              :: itr
   real   (Rkind)              :: rtr
   complex(Rkind)              :: ctr
   integer(Ikind), allocatable :: idev(:,:)
   real   (Rkind), allocatable :: rdev(:,:)
   complex(Rkind), allocatable :: cdev(:,:)
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%nrow /= a%ncol ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subDEV",                  &
                         msg = "<< dev >> is only defined for square matrices" )
      return
   end if
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 ) then
      res = pk2_t()      
      call opflag%set ( stat = WARNING, where = "pk2f_subDEV",                    &
                         msg = "<< b = dev(a) >> with << a >> empty (--> b = [Ê])" )
      return
   end if
         
   select type (p=>a%m)   
      type is (ik2_t)
         if ( allocated(p%v) ) then
            itr = IZERO          
            do i = 1, a%nrow ; itr = itr + p%v(i,i) ; end do
            if ( mod(itr,a%nrow) == 0 ) then
               itr = itr / a%nrow
               allocate(idev(a%nrow,a%ncol), stat = err)
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, "pk2f_subDEV", 'Allocation failure for "idev"')
                  return
               end if
               do j = 1, a%ncol
                  do i = 1, a%nrow ; idev(i,j) = p%v(i,j) ; end do
                  idev(j,j) = idev(j,j) - itr
               end do
               call pk2_movealloc ( from = idev, to = res )
            else
               rtr = real(itr,Rkind) / a%nrow
               allocate(rdev(a%nrow,a%ncol), stat = err)
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, "pk2f_subDEV", 'Allocation failure for "rdev"')
                  return
               end if
               do j = 1, a%ncol
                  do i = 1, a%nrow ; rdev(i,j) = p%v(i,j) ; end do
                  rdev(j,j) = rdev(j,j) - rtr
               end do
               call pk2_movealloc ( from = rdev, to = res )
            end if            
         else
            res = pk2_t()   
         end if   
        
      type is (rk2_t)
         if ( allocated(p%v) ) then
            rtr = RZERO          
            do i = 1, a%nrow ; rtr = rtr + p%v(i,i) ; end do
            rtr = rtr / a%nrow
            allocate(rdev(a%nrow,a%ncol), stat = err)
            if ( err /= 0 ) then
               call opflag%Set(IERROR, "pk2f_subDEV", 'Allocation failure for "rdev"')
               return
            end if
            do j = 1, a%ncol
               do i = 1, a%nrow ; rdev(i,j) = p%v(i,j) ; end do
               rdev(j,j) = rdev(j,j) - rtr
            end do
            call pk2_movealloc ( from = rdev, to = res )   
         else
            res = pk2_t()   
         end if   

      type is (ck2_t)
         if ( allocated(p%v) ) then
            ctr = CZERO 
            do i = 1, a%nrow ; ctr = ctr + p%v(i,i) ; end do
            ctr = ctr / a%nrow
            allocate(cdev(a%nrow,a%ncol), stat = err)
            if ( err /= 0 ) then
               call opflag%Set(IERROR, "pk2f_subDEV", 'Allocation failure for "cdev"')
               return
            end if
            do j = 1, a%ncol
               do i = 1, a%nrow ; cdev(i,j) = p%v(i,j) ; end do
               cdev(j,j) = cdev(j,j) - ctr
            end do
            call pk2_movealloc ( from = cdev, to = res )   
         else
            res = pk2_t()   
         end if   

      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subDEV", msg =                     &
                   'the function << dev >> needs a matrix of integers, reals or complexes' )
         return
   end select
         
   END SUBROUTINE pk2f_subDEV


!=============================================================================================   
   FUNCTION pk2f_DEV ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Returns the deviatoric part of a
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(9), f1, f2, f3
!---------------------------------------------------------------------------------------------      

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!
      f1 = str_color('dev',pk2f_helpcol)
      f2 = str_color('eye',pk2f_helpcol)
      f3 = str_color('trace',pk2f_helpcol)
      h(1) = "<< "+f1+" >> returns the deviatoric part of a matrix."
      h(2) = "          The matrix must be square, integer, real or complex."
      h(3) = " "
      h(4) = "Syntax: D = "+f1+"(A)"
      h(5) = " "
      h(6) = "Note: for a n x n matrix"
      h(7) = "      . "+f1+"(A) = A - "+f2+"(n,n) * "+f3+"(A)/n "
      h(8) = "      . "+f3+"("+f1+"(A)) = 0"
      h(9) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subDEV ( a, res ) ; error_TraceNreturn(opflag, "pk2f_DEV")
   
   END FUNCTION pk2f_DEV


!=============================================================================================    
   SUBROUTINE pk2f_subPROD ( a, res, mask, dim ) 
!=============================================================================================       
   class  (pk2_t),           intent(in     ) :: a
   class  (pk2_t),           intent(in  out) :: res
   class  (pk2_t), optional, intent(in     ) :: mask
   integer(Ikind), optional, intent(in     ) :: dim
!---------------------------------------------------------------------------------------------   
!  Computes the product of the elements of a.
!
!  Four cases:
!
!  . If neither dim or mask are present: computes the product of all of them.
!
!  . If mask is present (must be boolean): computes the product of those that obey the  
!                                          relation given in this array.
!
!  . If dim is present (must be integer): computes the product of those that are along the 
!                                         dimension given in dim.
!
!  . If mask and dim are present: computes the product of those that are along the dimension 
!                                 given in dim and that obey the relation given in mask.
!
!  Examples: 
!            . call pk2f_subPROD ( a, res )
!            . call pk2f_subPROD ( a, res, mask = a > 0 )
!            . call pk2f_subPROD ( a, res, dim = 2 )
!            . call pk2f_subPROD ( a, res, mask = a < 0, dim = 1 )
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------
   integer(Ikind) :: ldim, n, m, err
   type   (pk2_t) :: tmp
   logical        :: overflow
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   n = a%nrow ; m = a%ncol
     
   if ( a%typ == EMPTY .or. n == 0 .or. m == 0 .or. .not. allocated(a%m) ) then
      res = IONE
      call opflag%set ( stat = WARNING, where = 'pk2f_subPROD', msg =                        &
                       '<< b = prod(a,[mask,dim]) >> with << a >> non-allocated (--> b = 1)' )
      return
   end if

   if ( present(mask) ) then
      err = 0
      if ( mask%typ == LTYP .and. allocated(mask%m) ) then
         select type (p=>mask%m)
            type is (lk2_t)
               if ( mask%nrow /= n .or. mask%ncol /= m ) err = 2
            class default
               err = 1
         end select
      else
         err = 1
      end if      
      
      if ( err /= 0 ) then
          if ( err == 1 ) call opflag%set ( stat = UERROR, where = 'pk2f_subPROD', msg =   &
                                            'in << prod(a,mask) >>,  mask must be boolean' )
          if  (err == 2 ) call opflag%set ( stat = UERROR, where = 'pk2f_subPROD', msg =   &
                             'in << prod(a,mask) >>,  a and mask must have the same shape' )                                
         return
      end if              
   end if

   ldim = 0               
   if ( present(dim) ) then
      if ( dim < 1 .or. dim > 2 ) then
         call opflag%set ( stat = UERROR, where = 'pk2f_subPROD',             &
                            msg = 'in << prod(a,dim) >>,  dim must be 1 or 2' )
         return
      else
         ldim = dim
      end if
   end if      
                    
   select type (p=>a%m)
      type is (ik2_t)
         if ( .not. allocated(p%v) ) then
            tmp = IONE
         else
            if ( present(mask) ) then
               select type (q=>mask%m)
                  type is (lk2_t)
                     if ( ldim == 0 ) then
                        tmp = product((p%v)*RONE,mask=q%v)
                     else
                        tmp = product((p%v)*RONE,dim=ldim,mask=q%v)
                     end if
               end select
            else
               if ( ldim == 0 ) then
                  tmp = product((p%v)*RONE)
               else       
                  tmp = product(p%v*RONE,dim=ldim)
               end if                 
            end if       
            
            overflow = .false.
            select type (q=>tmp%m)
               type is (rk2_t)
                  if ( any(abs(q%v) > huge(1_Ikind)) ) overflow = .true.
            end select
         
            if ( .not. overflow ) then
               call bk2_ConvertToIk2 (tmp%m)
               tmp%typ = ITYP
            end if
         end if
                  
      type is (rk2_t)
         if ( .not. allocated(p%v) ) then
            tmp = IONE
         else
            if ( present(mask) ) then
               select type (q=>mask%m)
                  type is (lk2_t)
                     if ( ldim == 0 ) then
                        tmp = product(p%v,mask=q%v)
                     else
                        tmp = product(p%v,dim=ldim,mask=q%v)
                     end if
               end select
            else
               if ( ldim == 0 ) then
                  tmp = product(p%v)
               else       
                  tmp = product(p%v,dim=ldim)
               end if  
            end if               
         end if

      type is (ck2_t)
         if ( .not. allocated(p%v) ) then
            tmp = IONE
         else
            if ( present(mask) ) then
               select type (q=>mask%m)
                  type is (lk2_t)
                     if ( ldim == 0 ) then
                        tmp = product(p%v,mask=q%v)
                     else
                        tmp = product(p%v,dim=ldim,mask=q%v)
                     end if
               end select
            else
               if ( ldim == 0 ) then
                  tmp = product(p%v)
               else       
                  tmp = product(p%v,dim=ldim)
               end if                 
            end if                 
         end if
               
      class default
         call opflag%set ( stat = UERROR, where = 'pk2f_subPROD', msg =                     &
                   'the function << prod >> needs a matrix of integers, reals or complexes' )
         return
   end select

   if ( ldim == 1 ) then
      call tmp%m%transpose
      call tmp%resetDesc()
   end if   
   
   call pk2_movealloc ( from = tmp, to = res )   
               
   END SUBROUTINE pk2f_subPROD


!=============================================================================================    
   FUNCTION pk2f_PROD ( a, mask, dim ) result ( res ) 
!=============================================================================================       
   class  (pk2_t)          , intent(in) :: a
   class  (pk2_t), optional, intent(in) :: mask
   integer(Ikind), optional, intent(in) :: dim
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
      
   call pk2f_subPROD ( a, res, mask = mask, dim = dim )
   
   error_TraceNreturn(opflag, "pk2f_PROD")
         
   END FUNCTION pk2f_PROD
   

!=============================================================================================    
   SUBROUTINE pk2f_subPRODwrp ( matrs, res ) 
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)  
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Computes the product of the elements of matrs(1).
!
!  Three cases:
!
!  . If size(matrs) == 1: computes the product of all of them.
!
!  . If size(matrs) == 2: If matrs(2) is boolean, computes the product of those that obey the 
!                         relation given in this array.
!
!                         If matrs(2) is integer, computes the product of those that are along 
!                         the dimension given in this array.
!
!  . If size(matrs) == 3: Computes the product of those that are along the dimension given in 
!                         matrs(2) and that obey the relation given in matrs(3)
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------
   integer  (Ikind) :: i, nargs, lmask, ldim, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   if ( nargs < 1 .or. nargs >  3 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subPRODwrp", msg =              &
                        '1, 2 or 3 argument(s) expected for the function << prod >>' )   
      return       
   end if
   
   ldim = 0 ; lmask = 0 ; err = 0
   
   do i = 2, nargs
      if ( matrs(i)%typ == EMPTY .or. .not. allocated(matrs(i)%m) ) cycle
      select type (p=>matrs(i)%m)
         type is (ik2_t)
            if ( ldim /= 0 ) then
               err = 1 ; exit
            else   
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) ldim = p%v(1,1) 
            end if
         type is (lk2_t)
            if ( lmask /= 0 ) then
               err = 1 ; exit
            else
               lmask = i
            end if
         class default
            err = 1 ; exit   
      end select
   end do                 
   
   if ( err /= 0 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subPRODwrp", msg =                     &
                'in << prod(a,[dim,mask]) >>, dim must be equal to 1 or 2 and mask boolean' )
      return 
   end if                 
   
   select case (nargs)
      case (1)
         call pk2f_subPROD ( matrs(1), res )
      case (2)      
         if ( ldim /= 0 ) then
            call pk2f_subPROD ( matrs(1), res, dim = ldim )
         else if ( lmask /= 0 ) then
            call pk2f_subPROD ( matrs(1), res, mask = matrs(lmask) )
         else
            err = 1
         end if
      case (3)
         if ( ldim /= 0 .and. lmask /= 0 ) then
            call pk2f_subPROD ( matrs(1), res, dim = ldim, mask = matrs(lmask) )
         else if ( ldim /= 0 ) then
            call pk2f_subPROD ( matrs(1), res, dim = ldim )
         else if ( lmask /= 0 ) then
            call pk2f_subPROD ( matrs(1), res, mask = matrs(lmask) )
         else          
            err = 1
         end if
   end select         
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace("pk2f_subPRODwrp")    
   else if ( err /= 0 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subPRODwrp", msg =                     &
                'in << prod(a,[dim,mask]) >>, dim must be equal to 1 or 2 and mask boolean' )
   end if                 

   END SUBROUTINE pk2f_subPRODwrp


!=============================================================================================    
   FUNCTION pk2f_PRODwrp ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       

!- local variables ---------------------------------------------------------------------------
   type (str_t) :: h(13), f
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!
      f = str_color('prod',pk2f_helpcol)   
      h( 1) = "<< "+f+" >> returns the product of the elements of a matrix."
      h( 2) = "           The matrix must be integer, real or complex."
      h( 3) = " "
      h( 4) = "Syntax: p = "+f+"(A), p = "+f+"(A,mask), p = "+f+"(A,dim), p = "+f+"(A,mask,dim)"
      h( 5) = " "
      h( 6) = "  . "+f+"(A)      returns the product of the elements of A."
      h( 7) = "  . "+f+"(A,mask) returns the product of the elements of A that obey the"
      h( 8) = "                 relation given in mask. Example: "+f+"(A,A >= 2)"
      h( 9) = "  . "+f+"(A,dim)  returns the products along dimension dim. The result is a"
      h(10) = "                 row or a column vector. Example: "+f+"(A,1) gives a row"
      h(11) = "                 vector containing the products of each column of A."
      h(12) = "  . "+f+"(A,mask,dim) = "+f+"(A,dim,mask)"
      h(13) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subPRODwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_PRODwrp")
   
   END FUNCTION pk2f_PRODwrp


!=============================================================================================    
   SUBROUTINE pk2f_subSUM ( a, res, mask, dim ) 
!=============================================================================================       
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out) :: res
   class  (pk2_t), optional, intent(in    ) :: mask
   integer(Ikind), optional, intent(in    ) :: dim
!---------------------------------------------------------------------------------------------   
!  Computes the sum of the elements of a.
!
!  Four cases:
!
!  . If neither dim or mask are present: computes the sum of all of them.
!
!  . If mask is present (must be boolean): computes the sum of those that obey the  
!                                          relation given in this array.
!
!  . If dim is present (must be integer): computes the sum of those that are along the 
!                                         dimension given in dim.
!
!  . If mask and dim are present: computes the sum of those that are along the dimension given
!                                 in dim and that obey the relation given in mask.
!
!  Examples: 
!            . call pk2f_subSUM ( a, res )
!            . call pk2f_subSUM ( a, res, mask = a > 0 )
!            . call pk2f_subSUM ( a, res, dim = 2 )
!            . call pk2f_subSUM ( a, res, mask = a < 0, dim = 1)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------
   integer(Ikind) :: ldim, n, m, err
   type   (pk2_t) :: tmp
   logical        :: overflow
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   n = a%nrow ; m = a%ncol
     
   if ( a%typ == EMPTY .or. n == 0 .or. m == 0 .or. .not. allocated(a%m) ) then
      res = IZERO
      call opflag%set ( stat = WARNING, where = "pk2f_subSUM", msg =                         &
                        '<< b = sum(a,[mask,dim]) >> with << a >> non-allocated (--> b = 0)' )
      return
   end if

   if ( present(mask) ) then
      err = 0
      if ( mask%typ == LTYP .and. allocated(mask%m) ) then
         select type (p=>mask%m)
            type is (lk2_t)
               if ( mask%nrow /= n .or. mask%ncol /= m ) err = 2
            class default
               err = 1
         end select
      else
         err = 1
      end if      

      if ( err /= 0 ) then
         if ( err == 1 ) then
            call opflag%set ( stat = UERROR, where = "pk2f_subSUM",                &
                               msg = 'in << sum(a,mask) >>,  mask must be boolean' )
         else if ( err == 2 ) then
            call opflag%set ( stat = UERROR, where = "pk2f_subSUM", msg =               &
                           'in << sum(a,mask) >>,  a and mask must have the same shape' )  
         end if                              
         return
      end if           
   end if

   ldim = 0               
   if ( present(dim) ) then
      if (dim < 1 .or. dim > 2) then
         call opflag%set ( stat = UERROR, where = "pk2f_subSUM", &
                            msg = 'in << sum(a,dim) >>,  dim must be 1 or 2' )
         return
      else
         ldim = dim
      end if
   end if      
                                
   select type (p=>a%m)
      type is (ik2_t)
         if ( .not. allocated(p%v) ) then
            tmp = IZERO
         else
            if ( present(mask) ) then
               select type (q=>mask%m)
                  type is (lk2_t)
                     if ( ldim == 0 ) then
                        tmp = sum((p%v)*RONE,mask=q%v)
                     else
                        tmp = sum((p%v)*RONE,dim=ldim,mask=q%v)
                     end if
               end select
            else
               if ( ldim == 0 ) then
                  tmp = sum((p%v)*RONE)
               else       
                  tmp = sum((p%v)*RONE,dim=ldim)
               end if                 
            end if    
            
            overflow = .false.
            select type (q=>tmp%m)
               type is (rk2_t)
                  if ( any(abs(q%v) > huge(1_Ikind)) ) overflow = .true.
            end select
         
            if ( .not. overflow ) then
               call bk2_ConvertToIk2 (tmp%m)
               tmp%typ = ITYP
            end if

         end if
                   
      type is (rk2_t)
         if ( .not. allocated(p%v) ) then
            tmp = IZERO
         else
            if ( present(mask) ) then
               select type (q=>mask%m)
                  type is (lk2_t)
                     if ( ldim == 0 ) then
                        tmp = sum(p%v,mask=q%v)
                     else
                        tmp = sum(p%v,dim=ldim,mask=q%v)
                     end if
               end select
            else
               if ( ldim == 0 ) then
                  tmp = sum(p%v)
               else       
                  tmp = sum(p%v,dim=ldim)
               end if
            end if
         end if

      type is (ck2_t)
         if ( .not. allocated(p%v) ) then
            tmp = IZERO
         else
            if ( present(mask) ) then
               select type (q=>mask%m)
                  type is (lk2_t)
                     if ( ldim == 0 ) then
                        tmp = sum(p%v,mask=q%v)
                     else
                        tmp = sum(p%v,dim=ldim,mask=q%v)
                     end if
               end select
            else
               if ( ldim == 0 ) then
                  tmp = sum(p%v)
               else       
                  tmp = sum(p%v,dim=ldim)
               end if                  
            end if                  
         end if
               
      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subSUM", msg =                    &
                  'the function << sum >> needs a matrix of integers, reals or complexes' )
   end select
   
   if ( ldim == 1 ) then
      call tmp%m%transpose
      call tmp%resetDesc()
   end if   
   
   call pk2_movealloc ( from = tmp, to = res )
               
   END SUBROUTINE pk2f_subSUM


!=============================================================================================    
   FUNCTION pk2f_SUM ( a, mask, dim ) result ( res ) 
!=============================================================================================       
   class  (pk2_t)          , intent(in) :: a
   class  (pk2_t), optional, intent(in) :: mask
   integer(Ikind), optional, intent(in) :: dim
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subSUM ( a, res, mask = mask, dim = dim )
   
   error_TraceNreturn(opflag, "pk2f_SUM")
     
   END FUNCTION pk2f_SUM


!=============================================================================================    
   SUBROUTINE pk2f_subSUMwrp ( matrs, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Computes the sum of the elements of matrs(1).
!
!  Three cases:
!
!  . If size(matrs) == 1: computes the sum of all of them.
!
!  . If size(matrs) == 2: If matrs(2) is boolean, computes the sum of those that obey the 
!                         relation given in this array.
!
!                         If matrs(2) is integer, computes the sum of those that are along the 
!                         dimensiongiven in this array.
!
!  . If size(matrs) == 3: Computes the sum of those that are along the dimension given in 
!                         matrs(2) and that obey the relation given in matrs(3)
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------
   integer  (Ikind) :: i, nargs, lmask, ldim, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   nargs = size(matrs)
   
   if ( nargs < 1 .or. nargs >  3 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subSUMwrp", msg =              &
                        '1, 2 or 3 argument(s) expected for the function << sum >>' )   
      return       
   end if
   
   ldim = 0 ; lmask = 0 ; err = 0
   
   do i = 2, nargs
      if ( matrs(i)%typ == EMPTY .or. .not. allocated(matrs(i)%m) ) cycle
      select type (p=>matrs(i)%m)
         type is (ik2_t)
            if ( ldim /= 0 ) then
               err = 1 ; exit
            else   
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) ldim = p%v(1,1) 
            end if
         type is (lk2_t)
            if ( lmask /= 0 ) then
               err = 1 ; exit
            else
               lmask = i
            end if
         class default
            err = 1 ; exit   
      end select
   end do                 
   
   if ( err /= 0 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subSUMwrp", msg =                       &
                  'in << sum(a,[dim,mask]) >>, dim must be equal to 1 or 2 and mask boolean' )
      return 
   end if                 
   
   select case (nargs)
      case (1)
         call pk2f_subSUM ( matrs(1), res )
      case (2)      
         if ( ldim /= 0 ) then
            call pk2f_subSUM ( matrs(1), res, dim = ldim )
         else if ( lmask /= 0 ) then
            call pk2f_subSUM ( matrs(1), res, mask = matrs(lmask) )
         else
            err = 1
         end if
      case (3)
         if ( ldim /= 0 .and. lmask /= 0 ) then
            call pk2f_subSUM ( matrs(1), res, dim = ldim, mask = matrs(lmask) )
         else if ( ldim /= 0 ) then
            call pk2f_subSUM ( matrs(1), res, dim = ldim )
         else if ( lmask /= 0 ) then
            call pk2f_subSUM ( matrs(1), res, mask = matrs(lmask) )
         else          
            err = 1
         end if
   end select         
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace("pk2f_subSUMwrp")        
   else if ( err /= 0 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subSUMwrp", msg =                       &
                  'in << sum(a,[dim,mask]) >>, dim must be equal to 1 or 2 and mask boolean' )
   end if                 

   END SUBROUTINE pk2f_subSUMwrp


!=============================================================================================    
   FUNCTION pk2f_SUMwrp ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------      

!- local variables ---------------------------------------------------------------------------
   type(str_t) :: h(13), f
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!
      f = str_color('sum',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the sum of the elements of a matrix."
      h( 2) = "          The matrix must be integer, real or complex."
      h( 3) = " "
      h( 4) = "Syntax: p = "+f+"(A), p = "+f+"(A,mask), p = "+f+"(A,dim), p = "+f+"(A,mask,dim)"
      h( 5) = " "
      h( 6) = "  . "+f+"(A)      returns the sum of the elements of A."
      h( 7) = "  . "+f+"(A,mask) returns the sum of the elements of A that obey the"
      h( 8) = "                relation given in mask. Example: "+f+"(A,A >= 2)."
      h( 9) = "  . "+f+"(A,dim)  returns the sums along dimension dim. The result is a"
      h(10) = "                row or a column vector. Example: "+f+"(A,1) gives a row"
      h(11) = "                vector containing the sums of each column of A."
      h(12) = "  . "+f+"(A,mask,dim) = "+f+"(A,dim,mask)"
      h(13) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subSUMwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_SUMwrp")

   END FUNCTION pk2f_SUMwrp
      

!=============================================================================================    
   SUBROUTINE pk2f_subMEAN ( a, res, mask, dim )
!=============================================================================================
   use, intrinsic :: ieee_arithmetic       
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out) :: res   
   class  (pk2_t), optional, intent(in    ) :: mask
   integer(Ikind), optional, intent(in    ) :: dim
!---------------------------------------------------------------------------------------------   
!  Computes the mean of the elements of a.
!
!  Four cases:
!
!  . If neither dim or mask are present: computes the mean of all of them.
!
!  . If mask is present (must be boolean): computes the mean of those that obey the  
!                                          relation given in this array.
!
!  . If dim is present (must be integer): computes the mean of those that are along the 
!                                         dimension given in dim.
!
!  . If mask and dim are present: computes the mean of those that are along the dimension given
!                                 in dim and that obey the relation given in mask.
!
!  Examples: 
!            . call pk2f_subMEAN ( a, res )
!            . call pk2f_subMEAN ( a, res, mask = a > 0 )
!            . call pk2f_subMEAN ( a, res, dim = 2 )
!            . call pk2f_subMEAN ( a, res, mask = a < 0, dim = 1)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------
   integer(Ikind) :: ldim, n, m, nv, err
   type   (pk2_t) :: tmp
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   n = a%nrow ; m = a%ncol
     
   if ( a%typ == EMPTY .or. n == 0 .or. m == 0 .or. .not. allocated(a%m) ) then
      res = ieee_value(RONE, IEEE_QUIET_NAN)
      call opflag%set ( stat = WARNING, where = "pk2f_subMEAN", msg =                        &
                     '<< b = mean(a,[mask,dim]) >> with << a >> non-allocated (--> b = NaN)' )
      return
   end if

   if ( present(mask) ) then
      err = 0
      if ( mask%typ == LTYP .and. allocated(mask%m) ) then
         select type (p=>mask%m)
            type is (lk2_t)
               if ( mask%nrow /= n .or. mask%ncol /= m ) err = 2
            class default
               err = 1
         end select
      else
         err = 1
      end if      
      
      if ( err /= 0 ) then
         if ( err == 1 ) then
            call opflag%set ( stat = UERROR, where = "pk2f_subMEAN",                &
                               msg = 'in << mean(a,mask) >>,  mask must be boolean' )
         else if ( err == 2 ) then
            call opflag%set ( stat = UERROR, where = "pk2f_subMEAN", msg =               &
                           'in << mean(a,mask) >>,  a and mask must have the same shape' )
         end if                                
         return
      end if            
   end if

   ldim = 0               
   if ( present(dim) ) then
      if ( dim < 1 .or. dim > 2 ) then
         call opflag%set ( stat = UERROR, where = "pk2f_subMEAN",             &
                            msg = 'in << mean(a,dim) >>,  dim must be 1 or 2' )
         return
      else
         ldim = dim
      end if
   end if      
                        
   select type (p=>a%m)
 
      type is (ik2_t)
         !if (.not. allocated(p%v)) return
         if ( present(mask) ) then
            select type (q=>mask%m)
               type is (lk2_t)
                  if ( ldim == 0 ) then
                     nv = count(q%v)
                     if ( nv > 0 ) tmp = sum(RONE*(p%v),mask=q%v) / real(nv,Rkind)
                  else
                     if ( any(q%v) ) then
                        ! possibly Nan:
                        tmp = sum(RONE*(p%v),dim=ldim,mask=q%v) / real(count(q%v,ldim),Rkind) 
                     end if                
                  end if
            end select
         else
            if ( ldim == 0 ) then
               tmp = sum(RONE*(p%v)) / real(n*m,Rkind)
            else       
               tmp = sum(RONE*(p%v),dim=ldim) / real(size(p%v,ldim),Rkind)
            end if                 
         end if

      type is (rk2_t)
         !if (.not. allocated(p%v)) return
         if ( present(mask) ) then
            select type (q=>mask%m)
               type is (lk2_t)
                  if ( ldim == 0 ) then
                     nv = count(q%v)
                     if ( nv > 0 ) tmp = sum(p%v,mask=q%v) / real(nv,kind=Rkind)
                  else
                     if ( any(q%v) ) then
                        ! possibly Nan:
                        tmp = sum(p%v,dim=ldim,mask=q%v) / real(count(q%v,ldim),kind=Rkind)
                     end if                
                  end if
            end select
         else
            if ( ldim == 0 ) then
               tmp = sum(p%v) / real(n*m,kind=Rkind)
            else       
               tmp = sum(p%v,dim=ldim) / real(size(p%v,ldim),kind=Rkind)
            end if                 
         end if

      type is (ck2_t)
         if ( .not. allocated(p%v) ) return
         if ( present(mask) ) then
            select type (q=>mask%m)
               type is (lk2_t)
                  if ( ldim == 0 ) then
                     nv = count(q%v)
                     if ( nv > 0 ) tmp = sum(p%v,mask=q%v) / real(nv,kind=Rkind)
                  else
                     if ( any(q%v) ) then
                        ! possibly Nan:
                        tmp = sum(p%v,dim=ldim,mask=q%v) / real(count(q%v,ldim),kind=Rkind) 
                     end if                
                  end if
            end select
         else
            if ( ldim == 0 ) then
               tmp = sum(p%v) / real(n*m,kind=Rkind)
            else       
               tmp = sum(p%v,dim=ldim) / real(size(p%v,ldim),kind=Rkind)
            end if                 
         end if
               
      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subMEAN", msg =                    &
                  'the function << mean >> needs a matrix of integers, reals or complexes' )
         return
   end select
   
   if ( ldim == 1 ) then
      call tmp%m%transpose
      call tmp%resetDesc()
   end if   
   
   call pk2_movealloc ( from = tmp, to = res )   
               
   END SUBROUTINE pk2f_subMEAN


!=============================================================================================    
   FUNCTION pk2f_MEAN ( a, mask, dim ) result ( res ) 
!=============================================================================================       
   class  (pk2_t),           intent(in) :: a
   class  (pk2_t), optional, intent(in) :: mask
   integer(Ikind), optional, intent(in) :: dim
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subMEAN ( a, res, mask, dim ) ; error_TraceNreturn(opflag, "pk2f_MEAN")
           
   END FUNCTION pk2f_MEAN
   

!=============================================================================================    
   SUBROUTINE pk2f_subMEANwrp ( matrs, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: matrs(:)   
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Computes the mean of the elements of matrs(1). Four cases:
!
!  . If size(matrs) == 1: computes the mean of all of them.
!
!  . If size(matrs) == 2: If matrs(2) is boolean, computes the mean of those that obey 
!                         the relation given in this array.
!
!                         If matrs(2) is integer, computes the mean of those that are 
!                         along the dimension given in matrs(2).
!
!  . If size(matrs) == 3: computes the mean of those that are along the dimension given in
!                         matrs(2) and that obey the relation given in matrs(3).
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: i, nargs, lmask, ldim, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   if ( nargs < 1 .or. nargs >  3 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subMEANwrp", msg =              &
                        '1, 2 or 3 argument(s) expected for the function << mean >>' )   
      return       
   end if
   
   ldim = 0 ; lmask = 0 ; err = 0
      
   do i = 2, nargs
      if ( matrs(i)%typ == EMPTY .or. .not. allocated(matrs(i)%m) ) cycle   
      select type (p=>matrs(i)%m)
         type is (ik2_t)
            if ( ldim /= 0 ) then
               err = 1 ; exit
            else   
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) ldim = p%v(1,1) 
            end if
         type is (lk2_t)
            if ( lmask /= 0 ) then
               err = 1 ; exit
            else
               lmask = i
            end if
         class default
            err = 1 ; exit   
      end select
   end do                 
   
   if ( err /= 0 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subMEANwrp", msg =                    &
               'in << mean(a,[dim,mask]) >>, dim must be equal to 1 or 2 and mask boolean' )
      return 
   end if                 
   
   select case (nargs)
      case (1)
         call pk2f_subMEAN ( matrs(1), res )
      case (2)      
         if ( ldim /= 0 ) then
            call pk2f_subMEAN ( matrs(1), res, dim = ldim )
         else if ( lmask /= 0 ) then
            call pk2f_subMEAN ( matrs(1), res, mask = matrs(lmask) )
         else
            err = 1
         end if
      case (3)
         if ( ldim /= 0 .and. lmask /= 0 ) then
            call pk2f_subMEAN ( matrs(1), res, dim = ldim, mask = matrs(lmask) )
         else if ( ldim /= 0 ) then
            call pk2f_subMEAN ( matrs(1), res, dim = ldim )
         else if ( lmask /= 0 ) then
            call pk2f_subMEAN ( matrs(1), res, mask = matrs(lmask) )  
         else          
            err = 1
         end if
   end select         
   
   if ( opflag%code > 0 ) then
      call opflag%AddTrace("pk2f_subMEANwrp")
   else if ( err /= 0 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subMEANwrp", msg =                    &
               'in << mean(a,[dim,mask]) >>, dim must be equal to 1 or 2 and mask boolean' )
   end if                 

   END SUBROUTINE pk2f_subMEANwrp 


!=============================================================================================    
   FUNCTION pk2f_MEANwrp ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: matrs(:)   
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------         

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(13), f   
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('mean',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the mean of the elements of a matrix."
      h( 2) = "          The matrix must be integer, real or complex."
      h( 3) = " "
      h( 4) = "Syntax: p = "+f+"(A), p = "+f+"(A,mask), p = "+f+"(A,dim), p = "+f+"(A,mask,dim)"
      h( 5) = " "
      h( 6) = "  . "+f+"(A)      returns the mean of the elements of A."
      h( 7) = "  . "+f+"(A,mask) returns the mean of the elements of A that obey the"
      h( 8) = "                 relation given in mask. Example: "+f+"(A,A >= 2)."
      h( 9) = "  . "+f+"(A,dim)  returns the means along dimension dim. The result is a"
      h(10) = "                 row or a column vector. Example: "+f+"(A,1) gives a row"
      h(11) = "                 vector containing the means of each column of A."
      h(12) = "  . "+f+"(A,mask,dim) = "+f+"(A,dim,mask)"
      h(13) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subMEANwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_MEANwrp")
   
   END FUNCTION pk2f_MEANwrp


!=============================================================================================    
   SUBROUTINE pk2f_subCOVn ( a, res, b, w ) 
!=============================================================================================  
   use, intrinsic :: ieee_arithmetic        
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out) :: res
   class  (pk2_t), optional, intent(in    ) :: b
   integer(Ikind), optional, intent(in    ) :: w
!---------------------------------------------------------------------------------------------
!  Computes the covariance. Several cases:
!
!  . "w" is present: the covariance is normalized by nobs-1 if w = 0, by nobs if w = 1, where
!                    nobs is the number of observations.
!
!  . "w" is not present: same as w = 0.
!
!  . "b" is not present:
!    - if "a" is a vector: returns the variance of "a".
!    - if "a" is a matrix: returns the nvar x nvar covariance matrix of the nvar columns of  
!                          "a". Each column of "a" (of length nobs) is interpreted as a  
!                          variable and each row of "a" as an observation."
!  
!  . "b" is present: returns the 2 x 2 covariance matrix of "a" and "b". 
!                    "a" and "b" must have the same size (but not necessarily the same shape:
!                    they are treated as vectors).
!
!  Notes: cov(x) = 0 for x scalar, cov(x) = NaN for x empty.
!-----------------------------------------------------------------------------------R.H. 11/19   

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: nobs, nvar, typ, inorm
   real   (Rkind) :: wnorm, NaN
   type   (pk2_t) :: deva, devb, tmp
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nobs = a%nrow ; nvar = a%ncol ; typ = a%typ
      
   if ( present(b) ) then
      if ( b%nrow * b%ncol /= nobs*nvar ) then
         call opflag%set ( stat = UERROR, where = 'pk2f_subCOVn',                         &
                            msg = 'in << cov(a,b[,w]) >> a and b must have the same size' )
         return 
      end if   
      nobs = nobs*nvar
      typ = max(typ,b%typ)
   else
      if ( nobs == 1 ) nobs = nvar   
   end if
   
   if ( typ .gt. CTYP ) then
      call opflag%set ( stat = UERROR, where = 'pk2f_subCOVn',                          &
                         msg = 'in << cov(a[,b,w]) >> a and b must be of numerics type' )
      return 
   end if   
   
   if ( nobs == 0 ) then   
      res = ieee_value(NaN, IEEE_QUIET_NAN)
      return
   end if   
   
   inorm = 0
   if ( present(w) ) then
      if ( w /= 0 .and. w /= 1 ) then
         call opflag%set ( stat = UERROR, where = 'pk2f_subCOVn',          &
                            msg = 'in << cov(a[,b],w) >> w must be 0 or 1' )
         return 
      end if   
      inorm = w
   end if
   
   wnorm = 1.0_Rkind
   if ( inorm == 0 ) then
      if ( nobs /= 1 ) wnorm = 1.0_Rkind / real(nobs-1,Rkind)
   else
      if ( nobs /= 0 ) wnorm = 1.0_Rkind / real(nobs,Rkind)
   end if

   if ( present(b) ) then                   
      deva = reshape(a,[nobs,IONE]) - mean(a)
      devb = reshape(b,[nobs,IONE]) - mean(b)
      res = (trans(deva) * deva) * wnorm 
      tmp = (trans(deva) * devb) * wnorm 
      call res%SetSubmat ( tmp, [2], [1] )
      call res%SetSubmat ( tmp, [1], [2] )
      tmp = (trans(devb) * devb) * wnorm 
      call res%SetSubmat ( tmp, [2], [2] )
   else   
      if ( a%nrow == 1 ) then
         deva = a - mean(a)
         res = real( deva * trans(deva) ) * wnorm 
      else if ( a%ncol == 1 ) then   
         deva = a - pk2f_mean(a)
         res = real( trans(deva) * deva ) * wnorm 
      else
         deva = a - ones(nobs,IONE)*mean(a,dim=IONE) 
         res = (trans(deva) * deva) * wnorm 
      end if   
   end if   
        
   END SUBROUTINE pk2f_subCOVn
   

!=============================================================================================    
   FUNCTION pk2f_COVn ( a, b, w ) result ( res ) 
!=============================================================================================  
   use, intrinsic :: ieee_arithmetic        
   class  (pk2_t),           intent(in) :: a
   class  (pk2_t), optional, intent(in) :: b
   integer(Ikind), optional, intent(in) :: w
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

   call pk2f_subCOVn ( a, res, b, w ) ; error_TraceNreturn(opflag, "pk2f_COVn")
   
   END FUNCTION pk2f_COVn
   

!=============================================================================================    
   SUBROUTINE pk2f_subCOVwrp ( matrs, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: matrs(:)   
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Computes the covariance.
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: nargs, nrow, ncol, typ, w
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   if ( nargs < 1 .or. nargs >  3 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subCOVwrp", msg =              &
                        '1, 2 or 3 argument(s) expected for the function << cov >>' )   
      return       
   end if
               
   if ( nargs == 1 ) then
      call pk2f_subCOVn ( matrs(1), res )
   else
      nrow = matrs(nargs)%nrow ; ncol = matrs(nargs)%ncol ; typ = matrs(nargs)%typ
      if ( typ /= ITYP .or. nrow*ncol /= 1 ) then
         if ( nargs == 2 ) then
            call pk2f_subCOVn ( matrs(1), res, b = matrs(2) )
         else
            call opflag%set ( stat = UERROR, where = "pk2f_subCOVwrp", msg = &
                             'in << cov(a,b,w) >> w must be an integer (0 or 1)' )
            return
         end if
      else
         select type (p=>matrs(nargs)%m)
            type is (ik2_t)
               w = p%v(1,1)
         end select
         if ( nargs == 2 ) then
            call pk2f_subCOVn ( matrs(1), res, w = w )
         else
            call pk2f_subCOVn ( matrs(1), res, b = matrs(2), w = w )
         end if
      end if   
   end if
   
   error_TraceNreturn(opflag, "pk2f_subCOVwrp")

   END SUBROUTINE pk2f_subCOVwrp


!=============================================================================================    
   FUNCTION pk2f_COVwrp ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: matrs(:)   
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(36), f   
!---------------------------------------------------------------------------------------------      

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('cov',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the covariance matrix."
      h( 2) = " "
      h( 3) = "Syntax: c = "+f+"(v),      c = "+f+"(v,0),      c = "+f+"(v,1)"
      h( 4) = "        C = "+f+"(M),      C = "+f+"(M,0),      C = "+f+"(M,1)"
      h( 5) = "        C = "+f+"(v,u),    C = "+f+"(v,u,0),    C = "+f+"(v,u,1)"
      h( 6) = "        C = "+f+"(A,B),    C = "+f+"(A,B,0),    C = "+f+"(A,B,1)"
      h( 7) = " "
      h( 8) = "where  "
      h( 9) = "  . v and u are vectors of the same length (nobs x 1 or 1 x nobs matrices)"
      h(10) = "  . M is an nobs x nvar matrix"
      h(11) = "  . A and B are matrices of the same size (treated as vectors)"
      h(12) = " "
      h(13) = "Description:"
      h(14) = "  . "+f+"(v)    : Returns the variance of the vector v, normalized by nobs-1."
      h(15) = "  . "+f+"(v,1)  : Same as "+f+"(v) but normalized by nobs."
      h(16) = "  . "+f+"(v,0)  : Same as "+f+"(v)."     
      h(17) = " "
      h(18) = "  . "+f+"(M)    : Returns the nvar x nvar covariance matrix of the columns of M,"
      h(19) = "                normalized by nobs-1." 
      h(20) = "                Each column of M is interpreted as a variable and each row of M"
      h(21) = "                as an observation."
      h(22) = "  . "+f+"(M,1)  : Same as "+f+"(M) but normalized by nobs."
      h(23) = "  . "+f+"(M,0)  : Same as "+f+"(M)."
      h(24) = " "
      h(25) = "  . "+f+"(v,u)  : Returns the 2 x 2 covariance matrix of the two vectors u and v,"
      h(26) = "                normalized by nobs-1."
      h(27) = "  . "+f+"(v,u,1): Same as "+f+"(v,u) but normalized by nobs."
      h(28) = "  . "+f+"(v,u,0): Same as "+f+"(v,u)."
      h(29) = " "
      h(30) = "  . "+f+"(A,B)  : Returns the 2 x 2 covariance matrix of the two matrices A and B"
      h(31) = "                treated as vectors. The covariance is normalized by length(A)-1." 
      h(32) = "  . "+f+"(A,B,1): Same as "+f+"(A,B) but normalized by length(A)."
      h(33) = "  . "+f+"(A,B,0): Same as "+f+"(A,B)."
      h(34) = " "
      h(35) = " Notes: If X is a scalar, "+f+"(X) = 0. If X is empty, "+f+"(X) = NaN."
      h(36) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subCOVwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_COVwrp")
   
   END FUNCTION pk2f_COVwrp


!=============================================================================================    
   SUBROUTINE pk2f_subMINMAX ( a, minmax, res, mask, dim )
!=============================================================================================       
   class    (pk2_t),           intent(in    ) :: a
   character(len=3),           intent(in    ) :: minmax
   class    (pk2_t),           intent(in out) :: res   
   class    (pk2_t), optional, intent(in    ) :: mask
   integer  (Ikind), optional, intent(in    ) :: dim 
!---------------------------------------------------------------------------------------------   
!  Computes the min or the max of the elements of a. 
!
!  Four cases:
!
!  . If neither dim or mask are present: computes the min/max of all of them.
!
!  . If mask is present (must be boolean): computes the min/max of those that obey the  
!                                          relation given in this array.
!
!  . If dim is present (must be integer): computes the min/max of those that are along the 
!                                         dimension given in dim.
!
!  . If mask and dim are present: computes the min+/max of those that are along the dimension 
!                                 given in dim and that obey the relation given in mask.
!
!  Examples: 
!            . call pk2f_subMINMAX ( a, 'min', res )
!            . call pk2f_subMINMAX ( a, 'max', res, mask = a > 0 )
!            . call pk2f_subMINMAX ( a, 'min', res, dim = 2 )
!            . call pk2f_subMINMAX ( a, 'max', res, mask = a < 0, dim = 1)
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------    
   integer(Ikind) :: ldim, n, m, err
   type   (pk2_t) :: tmp
   logical        :: is_min
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   n = a%nrow ; m = a%ncol
     
   if ( a%typ == EMPTY .or. n == 0 .or. m == 0 .or. .not. allocated(a%m) ) then
      res = IZERO
      call opflag%set ( stat = WARNING, where = "pk2f_subMINMAX", msg =                     &
                  '<< b = min[max](a,[mask,dim]) >> with << a >> non-allocated (--> b = 0)' )
      return
   end if
   
   if ( minmax == 'min' ) then
      is_min = .true.
   else if ( minmax == 'max' ) then
      is_min = .false.
   else
      call opflag%set ( stat = UERROR, where = "pk2f_subMINMAX", msg =                &
               "(in pk2f_subMINMAX) invalid value for minmax: must be 'min' or 'max'" )
      return
   end if
         

   if ( present(mask) ) then
      err = 0
      if ( mask%typ == LTYP .and. allocated(mask%m) ) then
         select type (p=>mask%m)
            type is (lk2_t)
               if ( mask%nrow /= n .or. mask%ncol /= m ) err = 2
            class default
               err = 1
         end select
      else
         err = 1
      end if
      
      if ( err /= 0 ) then
         if ( err == 1 ) then
            call opflag%set ( stat = UERROR, where = "pk2f_subMINMAX", msg =                &
                                         'in << min[max](a,mask) >>,  mask must be boolean' )
         else if ( err == 2 ) then
            call opflag%set ( stat = UERROR, where = "pk2f_subMINMAX", msg =                &
                          'in << min[max](a,mask) >>,  a and mask must have the same shape' )
         end if                                
         return
      end if            
   end if

   ldim = 0               
   if ( present(dim) ) then
      if ( dim < 1 .or. dim > 2 ) then
         call opflag%set ( stat = UERROR, where = "pk2f_subMINMAX",             &
                           msg = 'in << min[max](a,dim) >>, dim must be 1 or 2' )
         return
      else
         ldim = dim
      end if
   end if      

   select type (p=>a%m)
      type is (ik2_t)
         if ( .not. allocated(p%v) ) return
         if ( present(mask) ) then
            select type (q=>mask%m)
               type is (lk2_t)
                  if ( ldim == 0 ) then
                     if ( is_min ) then
                        tmp = minval(p%v,mask=q%v)
                     else
                        tmp = maxval(p%v,mask=q%v)
                     end if
                  else
                     if ( is_min ) then
                        tmp = minval(p%v,dim=ldim,mask=q%v)
                     else
                        tmp = maxval(p%v,dim=ldim,mask=q%v)
                     end if
                  end if
            end select
         else
            if ( ldim == 0 ) then
               if ( is_min ) then
                  tmp = minval(p%v)
               else
                  tmp = maxval(p%v)
               end if
            else       
               if ( is_min ) then
                  tmp = minval(p%v,dim=ldim)
               else
                  tmp = maxval(p%v,dim=ldim)
               end if
            end if                 
         end if
      type is (rk2_t)
         if ( .not. allocated(p%v) ) return
         if ( present(mask) ) then
            select type (q=>mask%m)
               type is (lk2_t)
                  if ( ldim == 0 ) then
                     if ( is_min ) then
                        tmp = minval(p%v,mask=q%v)
                     else
                        tmp = maxval(p%v,mask=q%v)
                     end if
                  else
                     if ( is_min ) then
                        tmp = minval(p%v,dim=ldim,mask=q%v)
                     else
                        tmp = maxval(p%v,dim=ldim,mask=q%v)
                     end if
                  end if
            end select
         else
            if ( ldim == 0 ) then
               if ( is_min ) then
                  tmp = minval(p%v)
               else
                  tmp = maxval(p%v)
               end if
            else       
               if ( is_min ) then
                  tmp = minval(p%v,dim=ldim)
               else
                  tmp = maxval(p%v,dim=ldim)
               end if
            end if                 
         end if

      type is (ck2_t)
         call opflag%set ( stat = UERROR, where = "pk2f_subMINMAX", msg =            &
                  'argument of the function << max >> must be integer or real'//NL// &
                  '    (here, the argument is a complex)' ) ; return
      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = "pk2f_subMINMAX", msg =            &
                  'argument of the function << max >> must be integer or real'//NL// &
                  '    (here, the argument is a logical)' ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = "pk2f_subMINMAX", msg =            &
                  'argument of the function << max >> must be integer or real'//NL// &
                  '    (here, the argument is a string)' ) ; return
      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subMINMAX", msg =            &
                  'argument of the function << max >> must be integer or real'//NL// &
                  '    (here, the argument is of unknown type)' )  ; return               
   end select
   
   if ( ldim == 1 ) then
      call tmp%m%transpose
      call tmp%resetDesc()
   end if   
   
   call pk2_movealloc ( from = tmp, to = res )   

   END SUBROUTINE pk2f_subMINMAX


!=============================================================================================    
   SUBROUTINE pk2f_subMAX ( a, res, mask, dim ) 
!=============================================================================================       
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out) :: res      
   class  (pk2_t), optional, intent(in    ) :: mask
   integer(Ikind), optional, intent(in    ) :: dim 
!---------------------------------------------------------------------------------------------   

   call pk2f_subMINMAX ( a, 'max', res, mask, dim )
   
   error_TraceNreturn(opflag, "pk2f_subMAX")
   
   END SUBROUTINE pk2f_subMAX
   

!=============================================================================================    
   FUNCTION pk2f_MAX ( a, mask, dim ) result ( res ) 
!=============================================================================================       
   class  (pk2_t),           intent(in) :: a
   class  (pk2_t), optional, intent(in) :: mask
   integer(Ikind), optional, intent(in) :: dim 
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subMINMAX ( a, 'max', res, mask, dim ) ; error_TraceNreturn(opflag, "pk2f_MAX")
   
   END FUNCTION pk2f_MAX


!=============================================================================================    
   SUBROUTINE pk2f_subMIN ( a, res, mask, dim ) 
!=============================================================================================       
   class  (pk2_t),           intent(in    ) :: a
   class  (pk2_t),           intent(in out) :: res      
   class  (pk2_t), optional, intent(in    ) :: mask
   integer(Ikind), optional, intent(in    ) :: dim 
!---------------------------------------------------------------------------------------------   

   call pk2f_subMINMAX ( a, 'min', res, mask, dim )

   error_TraceNreturn(opflag, "pk2f_subMIN")
   
   END SUBROUTINE pk2f_subMIN


!=============================================================================================    
   FUNCTION pk2f_MIN ( a, mask, dim ) result ( res ) 
!=============================================================================================       
   class  (pk2_t),           intent(in) :: a
   class  (pk2_t), optional, intent(in) :: mask
   integer(Ikind), optional, intent(in) :: dim 
   type   (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subMINMAX ( a, 'min', res, mask, dim ) ; error_TraceNreturn(opflag, "pk2f_MIN")
   
   END FUNCTION pk2f_MIN


!=============================================================================================    
   SUBROUTINE pk2f_subMINMAXwrp ( matrs, minmax, res ) 
!=============================================================================================       
   class    (pk2_t), intent(in    ) :: matrs(:)   
   character(len=3), intent(in    ) :: minmax
   class    (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Computes the min or the max of the elements of matrs(1). Four cases:
!
!  . If size(matrs) == 1: computes the min/max of all of them.
!
!  . If size(matrs) == 2: If matrs(2) is boolean, computes the min/max of those that obey 
!                         the relation given in this array.
!
!                         If matrs(2) is integer, computes the min/max of those that are 
!                         along the dimension given in matrs(2).
!
!  . If size(matrs) == 3: computes the min/max of those that are along the dimension given in
!                         matrs(2) and that obey the relation given in matrs(3).
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: i, nargs, ldim, lmask, err
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   if ( nargs < 1 .or. nargs >  3 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subMINMAXwrp", msg =                &
                        '1, 2 or 3 argument(s) expected for the functions << min/max >>' )   
      return       
   end if
   
   ldim = 0 ; lmask = 0 ; err = 0
   
   do i = 2, nargs
      if ( matrs(i)%typ == EMPTY .or. .not. allocated(matrs(i)%m) ) cycle
      select type (p=>matrs(i)%m)
         type is (ik2_t)
            if ( ldim /= 0 ) then
               err = 1 ; exit
            else   
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) ldim = p%v(1,1) 
            end if
         type is (lk2_t)
            if ( lmask /= 0 ) then
               err = 1 ; exit
            else
               lmask = i
            end if
         class default
            err = 1 ; exit   
      end select
   end do                 
   
   if ( err /= 0 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subMINMAXwrp", msg =                  &
           'in << min[max](a,[dim,mask]) >>, dim must be equal to 1 or 2 and mask boolean' )
      return 
   end if                 
   
   select case (nargs)
      case (1)
         call pk2f_subMINMAX ( matrs(1), minmax, res )
      case (2)      
         if ( ldim /= 0 ) then
            call pk2f_subMINMAX ( matrs(1), minmax, res, dim = ldim )
         else if ( lmask /= 0 ) then
            call pk2f_subMINMAX ( matrs(1), minmax, res, mask = matrs(lmask) )
         else
            err = 1
         end if
      case (3)
         if ( ldim /= 0 .and. lmask /= 0 ) then
            call pk2f_subMINMAX ( matrs(1), minmax, res, mask = matrs(lmask), dim = ldim )
         else if ( ldim /= 0 ) then
            call pk2f_subMINMAX ( matrs(1), minmax, res, dim = ldim )
         else if ( lmask /= 0 ) then
            call pk2f_subMINMAX ( matrs(1), minmax, res, mask = matrs(lmask) )
         else          
            err = 1
         end if
   end select         
            
   if ( opflag%code > 0 ) then
      call opflag%AddTrace("pk2f_subMINMAXwrp")
   else if ( err /= 0 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subMINMAXwrp", msg =                    &
             'in << min[max](a,[dim,mask]) >>, dim must be equal to 1 or 2 and mask boolean' )
   end if                 

   END SUBROUTINE pk2f_subMINMAXwrp


!=============================================================================================    
   SUBROUTINE pk2f_subMAXwrp ( matrs, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: matrs(:)   
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------    

   call pk2f_subMINMAXwrp ( matrs, 'max', res ) ; error_TraceNreturn(opflag, "pk2f_subMAXwrp")
   
   END SUBROUTINE pk2f_subMAXwrp


!=============================================================================================    
   FUNCTION pk2f_MAXwrp ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: matrs(:)   
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------    

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(13), f
!---------------------------------------------------------------------------------------------      

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!   
      f = str_color('max',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the maximum elements of a matrix."
      h( 2) = "          The matrix must be integer or real."
      h( 3) = " "
      h( 4) = "Syntax: p = "+f+"(A), p = "+f+"(A,mask), p = "+f+"(A,dim), p = "+f+"(A,mask,dim)"
      h( 5) = " "
      h( 6) = "  . "+f+"(A)      returns the maximum of the elements of A."
      h( 7) = "  . "+f+"(A,mask) returns the maximum of the elements of A that obey the"
      h( 8) = "                relation given in mask. Example: "+f+"(A,A >= 2)."
      h( 9) = "  . "+f+"(A,dim)  returns the maximums along dimension dim. The result is a"
      h(10) = "                row or a column vector. Example: "+f+"(A,1) gives a row"
      h(11) = "                vector containing the maximums of each column of A."
      h(12) = "  . "+f+"(A,mask,dim) = "+f+"(A,dim,mask)"
      h(13) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subMINMAXwrp ( matrs, 'max', res ) ; error_TraceNreturn(opflag, "pk2f_MAXwrp")
   
   END FUNCTION pk2f_MAXwrp


!=============================================================================================    
   SUBROUTINE pk2f_subMINwrp ( matrs, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: matrs(:)   
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------    

   call pk2f_subMINMAXwrp ( matrs, 'min', res ) ; error_TraceNreturn(opflag, "pk2f_subMINwrp")
   
   END SUBROUTINE pk2f_subMINwrp


!=============================================================================================    
   FUNCTION pk2f_MINwrp ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: matrs(:)   
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(13), f
!---------------------------------------------------------------------------------------------      

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!   
      f = str_color('min',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the minimum elements of a matrix."
      h( 2) = "          The matrix must be integer or real."
      h( 3) = " "
      h( 4) = "Syntax: p = "+f+"(A), p = "+f+"(A,mask), p = "+f+"(A,dim), p = "+f+"(A,mask,dim)"
      h( 5) = " "
      h( 6) = "  . "+f+"(A)      returns the minimum of the elements of A."
      h( 7) = "  . "+f+"(A,mask) returns the minimum of the elements of A that obey the"
      h( 8) = "                relation given in mask. Example: "+f+"(A,A >= 2)."
      h( 9) = "  . "+f+"(A,dim)  returns the minimums along dimension dim. The result is a"
      h(10) = "                row or a column vector. Example: "+f+"(A,1) gives a row"
      h(11) = "                vector containing the minimums of each column of A."
      h(12) = "  . "+f+"(A,mask,dim) = "+f+"(A,dim,mask)"
      h(13) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subMINMAXwrp ( matrs, 'min', res ) ; error_TraceNreturn(opflag, "pk2f_MINwrp")

   END FUNCTION pk2f_MINwrp

      
!=============================================================================================   
   SUBROUTINE pk2f_subREAL ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the real parts of a
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables --------------------------------------------------------------------------- 
   real(Rkind), allocatable :: tmp(:,:)    
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subREAL",                     &
                         msg = "<< b = real(a) >> with << a >> empty (--> b = [ ])" )
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         call pk2_assign ( res, a ) ; return
      type is (rk2_t)
         call pk2_assign ( res, a ) ; return
      type is (ck2_t)
         tmp = real(p%v,kind=Rkind)    

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_subREAL", msg =                      &
               'argument of the function << real >> must be integer, real or complex'//NL// &
               '    (here, the argument is a logical)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_subREAL", msg =                      &
               'argument of the function << real >> must be integer, real or complex'//NL// &
               '    (here, the argument is a string)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = "pk2f_subREAL", msg =                      &
               'argument of the function << real >> must be integer, real or complex'//NL// &
               '    (here, the argument is of unknown type)' ) ; return          
   end select
   
   !res = tmp
   call pk2_movealloc ( from = tmp, to = res )
      
   END SUBROUTINE pk2f_subREAL


!=============================================================================================   
   FUNCTION pk2f_REAL ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!
      f = str_color('real',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the real part of the elements of a complex array."
      h(2) = " "
      h(3) = "Syntax: Ra = "+f+"(A)"
      h(4) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subREAL ( a, res ) ; error_TraceNreturn(opflag, "pk2f_REAL")
      
   END FUNCTION pk2f_REAL


!=============================================================================================   
   SUBROUTINE pk2f_subIMAG ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the imaginary parts of a
!-----------------------------------------------------------------------------------R.H. 04/18       
   
!- local variables ---------------------------------------------------------------------------
   real(Rkind), allocatable :: tmp(:,:)  
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subIMAG",                     &
                         msg = "<< b = imag(a) >> with << a >> empty (--> b = [ ])" )
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         res = pk2_t(shape=[a%nrow,a%ncol], typ=ITYP) !(<-- res = 0)
         return
      type is (rk2_t)
         res = pk2_t(shape=[a%nrow,a%ncol], typ=RTYP) !(<-- res = 0.0)
         return
      type is (ck2_t)
         tmp = aimag(p%v)  

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_subIMAG", msg =                      &
               'argument of the function << imag >> must be integer, real or complex'//NL// &
               '    (here, the argument is a logical)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_subIMAG", msg =                      &
               'argument of the function << imag >> must be integer, real or complex'//NL// &
               '    (here, the argument is a string)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = "pk2f_subIMAG", msg =                      &
               'argument of the function << imag >> must be integer, real or complex'//NL// &
               '    (here, the argument is of unknown type)' ) ; return        
   end select
   
   !res = tmp
   call pk2_movealloc ( from = tmp, to = res )
   
   END SUBROUTINE pk2f_subIMAG


!=============================================================================================   
   FUNCTION pk2f_IMAG ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------      

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('imag',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the imaginary part of the elements of a complex array."
      h(2) = " "
      h(3) = "Syntax: IA = "+f+"(A)"
      h(4) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subIMAG ( a, res ) ; error_TraceNreturn(opflag, "pk2f_IMAG")
   
   END FUNCTION pk2f_IMAG


!=============================================================================================   
   SUBROUTINE pk2f_subCONJ ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Conjugates a
!-----------------------------------------------------------------------------------R.H. 04/18       
   
!- local variables ---------------------------------------------------------------------------
   complex(Rkind), allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subCONJ",                     &
                         msg = "<< b = conj(a) >> with << a >> empty (--> b = [ ])" )
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         call pk2_assign ( res, a ) ; return
      type is (rk2_t)
         call pk2_assign ( res, a ) ; return
      type is (ck2_t)
         tmp = conjg(p%v)   

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_subCONJ", msg =                      &
               'argument of the function << conj >> must be integer, real or complex'//NL// &
               '    (here, the argument is a logical)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_subCONJ", msg =                      &
               'argument of the function << conj >> must be integer, real or complex'//NL// &
               '    (here, the argument is a string)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = "pk2f_subCONJ", msg =                      &
               'argument of the function << conj >> must be integer, real or complex'//NL// &
               '    (here, the argument is of unknown type)' ) ; return         
   end select
   
   !res = tmp
   call pk2_movealloc ( from = tmp, to = res )   
   
   END SUBROUTINE pk2f_subCONJ


!=============================================================================================   
   FUNCTION pk2f_CONJ ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Conjugates a
!-----------------------------------------------------------------------------------R.H. 04/18       
   
!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('conj',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the complex conjugate of the elements of a complex array."
      h(2) = " "
      h(3) = "Syntax: Ca = "+f+"(A)"
      h(4) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subCONJ ( a, res ) ; error_TraceNreturn(opflag, "pk2f_CONJ")
   
   END FUNCTION pk2f_CONJ


!=============================================================================================   
   SUBROUTINE pk2f_subNUM2STR ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Converts a pk2 array of any type into a string one
!-----------------------------------------------------------------------------------R.H. 06/18       

!- local variables ---------------------------------------------------------------------------     
   integer  (Ikind )              :: i, j, err
   character(len=99)              :: cnum  
   type     (str_t ), allocatable :: tmp(:,:) 
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subNUM2STR",                     &
                         msg = "<< b = num2str(a) >> with << a >> empty (--> b = [ ])" )
      return
   end if

   if ( a%typ == STYP ) then
      res = a
      return
   end if
         
   allocate(tmp(a%nrow,a%ncol), stat = err)
   
   if ( err == 0 ) then
      select type (p=>a%m)
         type is (ik2_t)
            do j = 1, a%ncol
               do i = 1, a%nrow
                  write(cnum,'(g0)') p%v(i,j)
                  tmp(i,j)%str = trim(cnum)
               end do
            end do
         type is (rk2_t)
            do j = 1, a%ncol
               do i = 1, a%nrow
                  write(cnum,'(g0)') p%v(i,j)
                  tmp(i,j)%str = trim(cnum)
               end do
            end do
         type is (ck2_t)
            do j = 1, a%ncol
               do i = 1, a%nrow
                  write(cnum,'("(",g0," , ",g0,")")') p%v(i,j)
                  tmp(i,j)%str = trim(cnum)
               end do
            end do
         type is (lk2_t)
            do j = 1, a%ncol
               do i = 1, a%nrow
                  write(cnum,'(g0)') p%v(i,j)
                  tmp(i,j)%str = trim(cnum)
               end do
            end do
               
         class default
            call opflag%set ( stat = UERROR, where = "pk2f_subNUM2STR",        &
                               msg = '<< num2str >>: argument of unknown type' ) 
            return               
      end select
   else
      call opflag%set ( stat = IERROR, where = "pk2f_subNUM2STR",        &
                         msg = '(in pk2f_subNUM2STR) Allocation failure' ) 
      return               
   end if
         
   !res = tmp   
   call pk2_movealloc ( from = tmp, to = res )   
      
   END SUBROUTINE pk2f_subNUM2STR


!=============================================================================================   
   FUNCTION pk2f_NUM2STR ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------    

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('num2str',pk2f_helpcol)
      h(1)%str = "<< "+f+" >> converts numbers to strings."
      h(2)%str = " "
      h(3)%str = "Syntax: S = "+f+"(A)"
      h(4)%str = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subNUM2STR ( a, res ) ; error_TraceNreturn(opflag, "pk2f_NUM2STR")
   
   END FUNCTION pk2f_NUM2STR
 

!=============================================================================================   
   SUBROUTINE pk2f_subSTR2NUM ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Converts a pk2 array of strings type to a numeric one (only integer or real at the moment)
!-----------------------------------------------------------------------------------R.H. 06/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind)              :: i, j, err
   real   (Rkind)              :: x
   logical                     :: is_intg
   real   (Rkind), allocatable :: tmpr(:,:)
   integer(Ikind), allocatable :: tmpi(:,:)
!---------------------------------------------------------------------------------------------    
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = "pk2f_subSTR2NUM",                     &
                         msg = "<< b = str2num(a) >> with << a >> empty (--> b = [ ])" )
      return
   end if

   select type (p=>a%m)   
      type is (sk2_t)   
         allocate(tmpr(a%nrow,a%ncol), stat = err)
         if ( err /= 0 ) then
            call opflag%set (stat=IERROR, where="pk2f_subSTR2NUM", msg='Allocation failure')
            return
         end if
         is_intg = .true.
         do j = 1, a%ncol
            do i = 1, a%nrow
               if ( allocated(p%v(i,j)%str) ) then
                  read(p%v(i,j)%str,*,iostat=err) x
               else
                  err = 1
               end if      
               if ( err /= 0 ) then
                  call opflag%set ( stat = UERROR, where = "pk2f_subSTR2NUM", msg =        &
                                   "<< b = str2num(a) >> with << a >> not a string-number" )
                  return
               end if   
               tmpr(i,j) = x
               is_intg = is_intg .and. util_IsIntg ( x )   
            end do
         end do                  
         
      class default
         res = a    
         return        
   end select
      
   if ( is_intg ) then
      tmpi = int(tmpr,kind=Ikind)
      !res = tmpi
      call pk2_movealloc ( from = tmpi, to = res )
   else   
      !res = tmpr
      call pk2_movealloc ( from = tmpr, to = res )
   end if   
      
   END SUBROUTINE pk2f_subSTR2NUM


!=============================================================================================   
   FUNCTION pk2f_STR2NUM ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------    
      
   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('str2num',pk2f_helpcol)
      h(1) = "<< "+f+" >> converts string-numbers to numbers."
      h(2) = " "
      h(3) = "Syntax: A = "+f+"(S)"
      h(4) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subSTR2NUM ( a, res ) ; error_TraceNreturn(opflag, "pk2f_STR2NUM")
   
   END FUNCTION pk2f_STR2NUM
   

!=============================================================================================   
   SUBROUTINE pk2f_subSIZE ( a, res, res2 )
!=============================================================================================   
   class(pk2_t),           intent(in    ) :: a
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in out) :: res2
!---------------------------------------------------------------------------------------------          
!  Returns the shape of a
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------
   integer :: sz(1,2)
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   sz(1,1) = a%nrow ; sz(1,2) = a%ncol

   if ( present(res2) ) then
      res = sz(1,1) ; res2 = sz(1,2)
   else
      res = sz
   end if      
   
   END SUBROUTINE pk2f_subSIZE


!=============================================================================================   
   FUNCTION pk2f_SIZE ( a, res2 ) result ( res )
!============================================================================================= 
   class(pk2_t), optional, intent(in    ) :: a
   class(pk2_t), optional, intent(in out) :: res2
   type (pk2_t)                           :: res
!---------------------------------------------------------------------------------------------          
!  Returns the shape of a
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(7), f
!---------------------------------------------------------------------------------------------          

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('size',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the shape of an array"
      h(2) = " "
      h(3) = "Syntax: sz = "+f+"(A)  or  [n,m] = "+f+"(A)"
      h(4) = " "
      h(5) = " . sz(1) or n is the number of rows,"
      h(6) = " . sz(2) or m is the number of columns." 
      h(7) = " "
      call pk2_assign ( res, h )
      return
   end if   

   if ( present(res2) ) then
      call pk2f_subSIZE ( a, res, res2 )
   else
      call pk2f_subSIZE ( a, res )
   end if
   
   error_TraceNreturn(opflag, "pk2f_SIZE")
   
   END FUNCTION pk2f_SIZE


!=============================================================================================   
   SUBROUTINE pk2f_subNUMEL ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the number of elements of a
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   res = a%nrow * a%ncol
   
   END SUBROUTINE pk2f_subNUMEL


!=============================================================================================   
   FUNCTION pk2f_NUMEL ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Returns the number of elements of a
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('numel',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the number of elements in an array"
      h(2) = " "
      h(3) = "Syntax: n = "+f+"(A)"
      h(4) = " "
      call pk2_assign ( res, h )
      return
   end if   
      
   res = a%nrow * a%ncol
   
   END FUNCTION pk2f_NUMEL


!=============================================================================================   
   SUBROUTINE pk2f_subLENGTH ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the number of elements of "a" or the length of each elements of "a" if these 
!  elements are made of character string
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind)              :: i, j, err
   integer(Ikind), allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ /= STYP ) then
      res = a%nrow * a%ncol
   else
      select type (p=>a%m)
         type is (sk2_t)
            allocate(tmp(a%nrow,a%ncol), stat = err)
            if ( err /= 0 ) then
               call opflag%set(stat=IERROR, where='pk2f_subLENGTH)', msg='Allocation failure')
               return
            endif
            do j = 1, a%ncol
               do i = 1, a%nrow
                  tmp(i,j) = len(p%v(i,j)%str)
               end do
            end do
      end select
      !res = tmp
      call pk2_movealloc ( from = tmp, to = res )
   end if                         
   
   END SUBROUTINE pk2f_subLENGTH


!=============================================================================================   
   FUNCTION pk2f_LENGTH ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------              

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(8), f
!---------------------------------------------------------------------------------------------          

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('length',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the number of elements in an array (same as numel)"
      h(2) = "             or the length of its elements (for array of strings)"
      h(3) = " "
      h(4) = "Syntax: n = "+f+"(A), L = "+f+"(S)"
      h(5) = " "
      h(6) = "Examples: A = [1,2 ; 3,4],   S = ['John', 'Steed' ; 'Emma', 'Peel  ']"
      h(7) = "          then n = 4   and   L = [ 4    ,  5      ;  4    ,  6      ]"
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subLENGTH ( a, res ) ; ; error_TraceNreturn(opflag, "pk2f_LENGTH")
   
   END FUNCTION pk2f_LENGTH


!=============================================================================================   
   SUBROUTINE pk2f_subISSYMM ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns b = .true. if a is symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------     
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
         
   res = a%Is_Symm ()
                                    
   END SUBROUTINE pk2f_subISSYMM


!=============================================================================================   
   FUNCTION pk2f_ISSYMM ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Returns b = .true. if a is symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('is_symm',pk2f_helpcol)
      h(1) = "<< "+f+" >> determines if a matrix is (strictely) symmetric"
      h(2) = " "
      h(3) = "Syntax: bool = "+f+"(A)"
      h(4) = " "
      call pk2_assign ( res, h )
      return
   end if   
         
   res = a%Is_Symm ()
                                    
   END FUNCTION pk2f_ISSYMM


!=============================================================================================   
   SUBROUTINE pk2f_subISSKEW ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns b = .true. if a is symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------   
   integer(Ikind) :: i, j
   logical        :: is_skew  
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%nrow /= a%ncol .or. .not. allocated(a%m) ) then
      res = .false.
      return
   end if   
   
   is_skew = .true.
   
   select type (p=>a%m)
      type is (ik2_t)
        
         do j = 1, a%ncol
            if ( p%v(j,j) /= IZERO ) then
               is_skew = .false. ; exit
            end if
            do i = 1, j-1
               if ( p%v(i,j) /= -p%v(j,i) ) then
                  is_skew = .false. ; exit
               end if
            end do
            if ( .not. is_skew ) exit
         end do
        
      type is (rk2_t)      
         do j = 1, a%ncol
            if ( p%v(j,j) /= RZERO ) then
               is_skew = .false. ; exit
            end if
            do i = 1, j-1
               if ( abs( p%v(i,j) + p%v(j,i) ) /= RZERO ) then
                  is_skew = .false. ; exit
               end if
            end do
            if ( .not. is_skew ) exit
         end do
         
      type is (ck2_t)      
         do j = 1, a%ncol
            if ( real(p%v(j,j)) /= RZERO ) then
               is_skew = .false. ; exit
            end if
            do i = 1, j-1
               if ( abs( p%v(i,j) + conjg(p%v(j,i)) ) /= RZERO ) then
                  is_skew = .false. ; exit
               end if
            end do
            if ( .not. is_skew ) exit
         end do
      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subISSKEW",         &
                            msg = "invalid matrix type in << is_skew(a) >>" )
   end select
   
   res = is_skew      
                                    
   END SUBROUTINE pk2f_subISSKEW


!=============================================================================================   
   FUNCTION pk2f_ISSKEW ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Returns b = .true. if a is symmetric
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables ---------------------------------------------------------------------------   
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('is_skew',pk2f_helpcol)
      h(1) = "<< "+f+" >> determines if a matrix is (strictely) skew-symmetric"
      h(2) = " "
      h(3) = "Syntax: bool = "+f+"(A)"
      h(4) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subISSKEW ( a, res ) ; error_TraceNreturn(opflag, "pk2f_ISSKEW")
                                    
   END FUNCTION pk2f_ISSKEW
   

!=============================================================================================   
   SUBROUTINE pk2f_subRESHAPE ( a, dim, res, pad )
!=============================================================================================   
   class(pk2_t),           intent(in    ) :: a, dim
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: pad
!---------------------------------------------------------------------------------------------          
!  Reshapes the matrix "a" using the size vector "b".
!  If "pad" is present, it must be of the same type as "a" and only its (1,1) element is used.
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: sz(2)
!---------------------------------------------------------------------------------------------  

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( dim%typ /= ITYP .or. .not. allocated(dim%m) ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subRESHAPE", msg =                    &
                   "<< reshape(a,dim[,pad]) >>: dim must be an integer matrix (of size 2)" )
      return
   end if
      
   if ( dim%nrow * dim%ncol /= 2 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subRESHAPE", msg =                    &
                     '<< reshape(a,dim[,pad]) >>: "dim" must be 1x2 or 2x1 integer matrix' )
      return
   end if
    
   sz = 0  
   select type (p=>dim%m)
      type is (ik2_t)
         if ( dim%nrow == 1 ) then
            sz(1) = p%v(1,1) ; sz(2) = p%v(1,2)
         else if ( dim%ncol == 1 ) then
            sz(1) = p%v(1,1) ; sz(2) = p%v(2,1)   
         end if
      class default
         call opflag%set ( stat = UERROR, where = "pk2f_subRESHAPE", msg =                  &
                  '<< reshape(a,dim[,pad]) >>: "dim" must be an integer matrix (of size 2)' )
         return
   end select
            
   call pk2f_subRESHAPEnm ( a, sz, res, pad ) ; error_TraceNreturn(opflag, "pk2f_subRESHAPE")
   
   END SUBROUTINE pk2f_subRESHAPE


!=============================================================================================   
   FUNCTION pk2f_RESHAPE ( a, dim, pad ) result ( res )
!=============================================================================================   
   class(pk2_t),           intent(in) :: a, dim
   class(pk2_t), optional, intent(in) :: pad
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

   call pk2f_subRESHAPE ( a, dim, res, pad ) ; error_TraceNreturn(opflag, "pk2f_RESHAPE")
      
   END FUNCTION pk2f_RESHAPE
      

!=============================================================================================   
   SUBROUTINE pk2f_subRESHAPEnm ( a, dim, res, pad )
!=============================================================================================   
   class  (pk2_t),           intent(in    ) :: a
   integer(Ikind),           intent(in    ) :: dim(:)
   class  (pk2_t),           intent(in out) :: res
   class  (pk2_t), optional, intent(in    ) :: pad
!---------------------------------------------------------------------------------------------          
!  Reshapes the matrix "a" using the size vector "dim" and optionaly the pad "pad". If "pad"
!  is not present, dim(1)*dim(2) must be equal to size(a).
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables ---------------------------------------------------------------------------
   integer(Ikind)              :: padi 
   real   (Rkind)              :: padr 
   complex(Rkind)              :: padc
   logical                     :: padl
   type   (str_t)              :: pads
   integer(Ikind), allocatable :: tmpi(:,:)
   real   (Rkind), allocatable :: tmpr(:,:)
   complex(Rkind), allocatable :: tmpc(:,:)
   logical       , allocatable :: tmpl(:,:)
   type   (str_t), allocatable :: tmps(:,:)
!---------------------------------------------------------------------------------------------  

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( size(dim) < 2 ) then      
      call opflag%set( stat = UERROR, where = "pk2f_subRESHAPEnm", msg =                    &
      '<< reshape(a,dim[,pad]) >>: the integer vector "dim" must contain at least 2 numbers')
      return
   end if

   if ( present(pad) ) then
      if ( pad%typ /= a%typ ) then
         call opflag%set( stat = UERROR, where = "pk2f_subRESHAPEnm", msg =                &
                         '<< reshape(a,dim,pad) >>: "pad" must be of the same type as "a"' )
         return
      end if
      if ( pad%nrow * pad%ncol < 1 ) then
         call opflag%set( stat = UERROR, where = "pk2f_subRESHAPEnm", msg =   &
                         '<< reshape(a,dim,pad) >>: "pad" must at least 1 x 1' )
         return
      end if
      select type (p=>pad%m)
         type is (ik2_t) ; padi = p%v(1,1)
         type is (rk2_t) ; padr = p%v(1,1)
         type is (ck2_t) ; padc = p%v(1,1)
         type is (lk2_t) ; padl = p%v(1,1)
         type is (sk2_t) ; pads = p%v(1,1)
      end select
   else
      if ( a%nrow * a%ncol /= dim(1) * dim(2) .or. .not. allocated(a%m) ) then
         call opflag%set( stat = UERROR, where = "pk2f_subRESHAPEnm", msg =                 &
                  "<< reshape(a,dim) >>: the number of elements of a must be dim(1)*dim(2)" )
        return
      end if
   end if            
         
   select type (p=>a%m)
      type is (ik2_t)
         if ( present(pad) ) then
            call util_ReshapeI (p%v, dim, tmpi, opflag, padi)
         else
            call util_ReshapeI (p%v, dim, tmpi, opflag)
         end if
         error_TraceNreturn(opflag, "pk2f_subRESHAPEnm")
         call pk2_movealloc ( from = tmpi, to = res )
      type is (rk2_t)
         if ( present(pad) ) then
            call util_ReshapeR (p%v, dim, tmpr, opflag, padr)
         else
            call util_ReshapeR (p%v, dim, tmpr, opflag)
         end if
         error_TraceNreturn(opflag, "pk2f_subRESHAPEnm")
         call pk2_movealloc ( from = tmpr, to = res )
      type is (ck2_t)
         if ( present(pad) ) then
            call util_ReshapeC (p%v, dim, tmpc, opflag, padc)
         else
            call util_ReshapeC (p%v, dim, tmpc, opflag)
         end if
         error_TraceNreturn(opflag, "pk2f_subRESHAPEnm")
         call pk2_movealloc ( from = tmpc, to = res )
      type is (lk2_t)
         if ( present(pad) ) then
            call util_ReshapeL (p%v, dim, tmpl, opflag, padl)
         else
            call util_ReshapeL (p%v, dim, tmpl, opflag)
         end if
         error_TraceNreturn(opflag, "pk2f_subRESHAPEnm")
         call pk2_movealloc ( from = tmpl, to = res )
      type is (sk2_t)
         if ( present(pad) ) then
            call util_ReshapeS (p%v, dim, tmps, opflag, pads)
         else
            call util_ReshapeS (p%v, dim, tmps, opflag)
         end if
         error_TraceNreturn(opflag, "pk2f_subRESHAPEnm")
         call pk2_movealloc ( from = tmps, to = res )
   end select                
               
   END SUBROUTINE pk2f_subRESHAPEnm


!=============================================================================================   
   FUNCTION pk2f_RESHAPEnm ( a, dim ) result ( res )
!=============================================================================================   
   class  (pk2_t), intent(in) :: a
   integer(Ikind), intent(in) :: dim(:)
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------          

   call pk2f_subRESHAPEnm ( a, dim, res ) ; error_TraceNreturn(opflag, "pk2f_RESHAPEnm")

        
   END FUNCTION pk2f_RESHAPEnm


!=============================================================================================   
   SUBROUTINE pk2f_subRESHAPEwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Reshapes the matrix matrs(1) using the size vector matrs(2) and the pad matrs(3) 
!-----------------------------------------------------------------------------------R.H. 11/19       

!- local variables ---------------------------------------------------------------------------     
   integer(Ikind) :: nargs
!---------------------------------------------------------------------------------------------  

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nargs = size(matrs)
   
   if ( nargs == 2 ) then
      call pk2f_subRESHAPE ( a=matrs(1), dim=matrs(2), res=res )
   else if ( nargs == 3 ) then
      call pk2f_subRESHAPE ( a=matrs(1), dim=matrs(2), res=res, pad=matrs(3) )
   else
      call opflag%set ( stat = UERROR, where = "pk2f_subRESHAPEwrp", msg =         &
                        '2 or 3 argument(s) expected for the function << shape >>' )   
      return       
   end if
   
   error_TraceNreturn(opflag, "pk2f_subRESHAPEwrp")
   
   END SUBROUTINE pk2f_subRESHAPEwrp


!=============================================================================================   
   FUNCTION pk2f_RESHAPEwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Reshapes the matrix matrs(1) using the size vector matrs(2) and the pad matrs(3) 
!-----------------------------------------------------------------------------------R.H. 12/19       

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(12), f1, f2, f3  
!---------------------------------------------------------------------------------------------  

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f1 = str_color('reshape',pk2f_helpcol)
      f2 = str_color('rand',pk2f_helpcol)
      f3 = str_color('numel',pk2f_helpcol)
      h( 1) = "<< "+f1+" >> reshapes an array"
      h( 2) = " "
      h( 3) = "Syntax: B = "+f1+"(A,v)   B = "+f1+"(A,v,pad)"
      h( 4) = " "
      h( 5) = " . The shape vector v must contains 2 integers: v = [i1,i2]"
      h( 6) = " . If pad is included, it must have the same type as A and it will be used"
      h( 7) = "   to complete B if necessary (i.e. when i1*i2 > "+f3+"(A))"
      h( 8) = " . If pad is not included, the size of A has to be at least i1*i2"
      h( 9) = " "
      h(10) = "Examples: . A = "+f2+"(4,4), B = "+f1+"(A,[2,8])"
      h(11) = "          . A = "+f2+"(4,4), B = "+f1+"(A,[2,9],0.0)"
      call pk2_assign ( res, h )
      return      
   end if   
      
   call pk2f_subRESHAPEwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_RESHAPEwrp")
   
   END FUNCTION pk2f_RESHAPEwrp


!=============================================================================================   
   SUBROUTINE pk2f_subNORM ( a, res, option )
!=============================================================================================   
   class    (pk2_t),           intent(in    ) :: a
   class    (pk2_t),           intent(in out) :: res
   character(len=1), optional, intent(in    ) :: option
!---------------------------------------------------------------------------------------------    
!  Returns the norm of a. Two cases: 
!
!  . If option is present and
!
!          . option = 'm': computes the largest absolute value (max(abs(a(i,j)))   
!          . option = '1': computes the 1 norme (maximum column sum)
!          . option = 'i': computes the infinity norm (maximum row sum)
!          . option = 'f': computes the Frobenius's norm (square root of sum of squares)  
!
!  . If option is not present, computes the Frobenius's norm
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables --------------------------------------------------------------------------- 
   character(len=1) :: opt
!---------------------------------------------------------------------------------------------  

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      res = IZERO
      call opflag%set ( stat = WARNING, where = "pk2f_subNORM",                   &
                         msg = "<< b = norm(a) >> with << a >> empty (--> b = 0)" )
      return
   end if
      
   if ( .not. present(option) ) then
      opt = 'f'
   else
      if ( option /= 'm' .and. option /= '1' .and. option /= 'i' .and. option /= 'f' ) then
         call opflag%set ( stat = UERROR, where = "pk2f_subNORM", msg =                   &
                          'in << norm(a,option) >> "option" must be "m", "1", "i" or "f"' )
         return
      end if   
      opt = option
   end if   
      
   select type (p=>a%m)
      type is (ik2_t)
          res =  util_MatrixNorm ( opt, Imat=p%v, stat=opflag )
      type is (rk2_t)
          res =  util_MatrixNorm ( opt, Rmat=p%v, stat=opflag )
      type is (ck2_t)
          res =  util_MatrixNorm ( opt, Cmat=p%v, stat=opflag )
          
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_subNORM", msg =                      &
               'in << norm(a,[option]) >> "a" must be integer, real or complex array'//NL// &
               '    (here, "a" is an array of logicals)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_subNORM", msg =                      &
               'in << norm(a,[option]) >> "a" must be integer, real or complex array'//NL// &
               '    (here, "a" is an array of strings)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = "pk2f_subNORM", msg =                      &
               'in << norm(a,[option]) >> "a" must be integer, real or complex array'//NL// &
               '    (here, "a" is an array of unknown type)' ) ; return
   end select
   
   error_TraceNreturn(opflag, "pk2f_subNORM")
                  
   END SUBROUTINE pk2f_subNORM


!=============================================================================================   
   FUNCTION pk2f_NORM ( a, option ) result ( res )
!=============================================================================================   
   class    (pk2_t),           intent(in) :: a
   character(len=1), optional, intent(in) :: option
   type     (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------    
!  Returns the norm of a. Two cases: 
!
!  . If option is present and
!
!          . option = 'm': computes the largest absolute value (max(abs(a(i,j)))   
!          . option = '1': computes the 1 norme (maximum column sum)
!          . option = 'i': computes the infinity norm (maximum row sum)
!          . option = 'f': computes the Frobenius's norm (square root of sum of squares)  
!
!  . If option is not present, computes the Frobenius's norm
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables --------------------------------------------------------------------------- 
   character(len=1) :: opt
!---------------------------------------------------------------------------------------------  

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      res = IZERO
      call opflag%set ( stat = WARNING, where = "pk2f_NORM",                      &
                         msg = "<< b = norm(a) >> with << a >> empty (--> b = 0)" )
      return
   end if
      
   if ( .not. present(option) ) then
      opt = 'f'
   else
      if (option /= 'm' .and. option /= '1' .and. option /= 'i' .and. option /= 'f') then
         call opflag%set ( stat = UERROR, where = "pk2f_NORM", msg =                      &
                          'in << norm(a,option) >> "option" must be "m", "1", "i" or "f"' )
         return
      end if   
      opt = option
   end if   
      
   select type (p=>a%m)
      type is (ik2_t)
          res =  util_MatrixNorm ( opt, Imat=p%v, stat=opflag )
      type is (rk2_t)
          res =  util_MatrixNorm ( opt, Rmat=p%v, stat=opflag )
      type is (ck2_t)
          res =  util_MatrixNorm ( opt, Cmat=p%v, stat=opflag )
          
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_NORM", msg =                         &
               'in << norm(a,[option]) >> "a" must be integer, real or complex array'//NL// &
               '    (here, "a" is an array of logicals)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = "pk2f_NORM", msg =                         &
               'in << norm(a,[option]) >> "a" must be integer, real or complex array'//NL// &
               '    (here, "a" is an array of strings)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = "pk2f_NORM", msg =                         &
               'in << norm(a,[option]) >> "a" must be integer, real or complex array'//NL// &
               '    (here, "a" is an array of unknown type)' ) ; return
   end select

   error_TraceNreturn(opflag, "pk2f_NORM")
               
   END FUNCTION pk2f_NORM


!=============================================================================================   
   SUBROUTINE pk2f_subNORMwrp ( matrs,res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------    
!  Returns the norm of a=matrs(1). Three cases:
!
!  . If size(matrs) == 2 and 
!
!              . matrs(2) = 'm': computes the largest absolute value (max(abs(a(i,j)))   
!              . matrs(2) = '1': computes the 1 norme (maximum column sum)
!              . matrs(2) = 'i': computes the infinity norm (maximum row sum)
!              . matrs(2) = 'f': computes the Frobenius's norm (square root of sum of squares)   
!
!  . If size(matrs) == 1: computes the Frobenius's norm.
!
!  . If matrs is not present, returns help in res.
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!-----------------------------------------------------------------------------------R.H. 10/18       

!- local variables --------------------------------------------------------------------------- 
   integer  (Ikind) :: nargs, err
   character(len=1) :: option 
!---------------------------------------------------------------------------------------------  

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   if ( nargs < 1 .or. nargs >  2 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subNORMwrp", msg =           &
                        '1 or 2 argument(s) expected for the function << norm >>' )   
      return       
   end if
   
   err = 0
   
   if ( nargs == 2 ) then
      if ( matrs(2)%typ == STYP .and. allocated(matrs(2)%m) ) then 
         select type (p=>matrs(2)%m)
            type is (sk2_t)
               if ( allocated(p%v) .and. p%nrow >= 1 .and. p%ncol >= 1 ) then
                  if ( allocated(p%v(1,1)%str) ) then
                     option = (p%v(1,1)%str(1:1))
                  else
                     err = 1
                  end if      
               else
                  err = 1
               end if
            class default
               err = 1
         end select
      else
         err = 1
      end if      
   end if
   
   if ( err == 1 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subNORMwrp",       &
                         msg = 'invalid option in << norm(a,option) >>' )
      return
   end if   
                         
   select case (nargs)
      case (1)
         call pk2f_subNORM ( a = matrs(1), res = res, option = 'f' )
      case (2)
         call pk2f_subNORM ( a = matrs(1), res = res, option = option )
   end select

   error_TraceNreturn(opflag, "pk2f_subNORMwrp")
   
   END SUBROUTINE pk2f_subNORMwrp


!=============================================================================================   
   FUNCTION pk2f_NORMwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------         

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(17), f  
!---------------------------------------------------------------------------------------------  

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('norm',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the norm of a matrix"
      h( 2) = " "
      h( 3) = "Syntax: x = "+f+"(v), x = "+f+"(A),  x = "+f+"(v,s), x = "+f+"(A,s)"
      h( 4) = " "
      h( 5) = " . "+f+"(v)   computes the Euclidian norm of the vector v"
      h( 6) = " . "+f+"(A)   computes the Frobenius's norm of A" 
      h( 7) = "             (square root of sum of squares)."
      h( 8) = " . "+f+"(v,s) computes, for"
      h( 9) = "      . s = 'i' (or 'm'), the largest absolute value (max(abs(v(i))))"
      h(10) = "      . s = '1' the 1 norm of v"
      h(11) = "      . s = 'f' the euclidian norm of v (same as norm(v))"
      h(12) = " . "+f+"(A,s) computes, for"
      h(13) = "      . s = 'm', the largest absolute value (max(abs(A(i,j))))"
      h(14) = "      . s = '1', the 1 norm (maximum column sum)"
      h(15) = "      . s = 'i', the infinity norm (maximum row sum)"
      h(16) = "      . s = 'f', the Frobenius's norm "
      h(17) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subNORMwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_NORMwrp")

                     
   END FUNCTION pk2f_NORMwrp


!=============================================================================================    
   SUBROUTINE pk2f_subLU ( a, res, res2, res3 ) 
!=============================================================================================       
   class(pk2_t),           intent(in    ) :: a
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in out) :: res2, res3
!---------------------------------------------------------------------------------------------   
!  Computes an LU factorization of "a":  a =  P'*L*U
!
!  . If "res2" and "res3" are not present, returns in "res" the factors L and U (the unit 
!    diagonal elements of L are not stored).
!  . If "res2" (or "res3") is present, returns in "res" the matrix L and in "res2" (or "res3")
!    the matrix U
!  . If "res2" and "res3" are present, returns in "res" the matrix L, in "res2" the matrix U
!    and in "res3" the matrix P'.
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables ---------------------------------------------------------------------------
   type(pk2_t) :: tmp1, tmp2, tmp3
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( present(res2) .and. present(res3) ) then
      call a%lu ( L=tmp1, U=tmp2, P=tmp3 ) ; error_TraceNreturn(opflag, "pk2f_subLU")
      call pk2_movealloc ( from = tmp1, to = res  )
      call pk2_movealloc ( from = tmp2, to = res2 )
      call pk2_movealloc ( from = tmp3, to = res3 )
   else if ( present(res2) ) then
      call a%lu ( L=tmp1, U=tmp2 ) ;         error_TraceNreturn(opflag, "pk2f_subLU")
      call pk2_movealloc ( from = tmp1, to = res  )
      call pk2_movealloc ( from = tmp2, to = res2 )
   else if ( present(res3) ) then
      call a%lu ( L=tmp1, U=tmp2 ) ;         error_TraceNreturn(opflag, "pk2f_subLU")
      call pk2_movealloc ( from = tmp1, to = res  )
      call pk2_movealloc ( from = tmp2, to = res3 )
   else
      call a%lu ( h=tmp1 ) ;                 error_TraceNreturn(opflag, "pk2f_subLU")
      call pk2_movealloc ( from = tmp1, to = res  )
   end if
               
   END SUBROUTINE pk2f_subLU


!=============================================================================================    
   FUNCTION pk2f_LU ( a, res2, res3 ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in    ) :: a
   class(pk2_t), optional, intent(   out) :: res2, res3
   type (pk2_t)                           :: res
!---------------------------------------------------------------------------------------------   
!  Computes an LU factorization of "a". 
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables ---------------------------------------------------------------------------   
   type(str_t) :: h(17), f1, f2
!---------------------------------------------------------------------------------------------          
   
   if ( .not. present(a) ) then
      f1 = str_color('lu',pk2f_helpcol)
      f2 = str_color('size',pk2f_helpcol)
      h( 1) = "<< "+f1+" >> computes an LU factorization"
      h( 2) = " "
      h( 3) = "Syntax: . [L,U,P] = "+f1+"(A)"                                
      h( 4) = "        . [L,U]   = "+f1+"(A)"                                
      h( 5) = "        . handl   = "+f1+"(A)"                                
      h( 6) = " " 
      h( 7) = "where "
      h( 8) = ". L is lower triangular with unit diagonal elements (lower trapezoidal"
      h( 9) = "  if "+f2+"(A,1) > "+f2+"(A,2)) " 
      h(10) = ". U is upper triangular (upper trapezoidal if size(A,1) < size(A,2))" 
      h(11) = ". P is a permutation matrix such that P*A = L*U (<=> A = P'*L*U)"
      h(12) = ". handl is a working array containing the factorized matrix and permutation"
      h(13) = "  indices. It can be used in lusolv to solve linear systems A*x = b with"
      h(14) = "  different right-hand-sides b without having to refactorize A." 
      h(15) = " "
      h(16) = "(Note: Lapack routines dgetrf/zgetrf are used)"
      h(17) = " "
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subLU ( a, res, res2, res3 ) ; error_TraceNreturn(opflag, "pk2f_LU")
   
   END FUNCTION pk2f_LU
   

!=============================================================================================    
   SUBROUTINE pk2f_subLUsolv ( a, b, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a, b
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Solves a linear system after having factorized the matrix by an LU decomposition.
!  The array "a" corresponds to the output of lu. 
!  The array "b" is the right-hand-side
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2f_subLUsolv'
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%ncol /= a%nrow + 1 ) then
      call opflag%set ( stat = UERROR, where = HERE, msg = &
               "The matrix must be square and previously factorized by the function lu" )
      return
   end if
   
   call res%LUsolv ( handl = a, rhs = b ) ; error_TraceNreturn(opflag, HERE)
      
   END SUBROUTINE pk2f_subLUsolv 


!=============================================================================================    
   FUNCTION pk2f_LUsolv ( a, b ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a, b
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Solves a linear system after having factorized the matrix by an LU decomposition.
!  The array "a" corresponds to the output of lu. 
!  The array "b" is the right-hand-side
!-----------------------------------------------------------------------------------R.H. 04/19       
   
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2f_LUsolv'  
   type     (str_t)            :: h(14), f
!---------------------------------------------------------------------------------------------          
   
   if ( .not. present(a) .and. .not. present(b) ) then
      f = str_color('lusolv',pk2f_helpcol)
      h( 1) = "<< "+f+" >> solves a linear system after an LU factorization"
      h( 2) = " "
      h( 3) = "Syntax: x = "+f+" (handl,b)  or  X = "+f+" (handl, B)"  
      h( 4) = " "
      h( 5) = "where "
      h( 6) = ". handl is the output of the lu function (handl = lu(A))"    
      h( 7) = ". b (or B) is the right-hand-side vector (or matrix)."
      h( 8) = ". x (or X) is the solution vector (or matrix)" 
      h( 9) = " "
      h(10) = "Example: "
      h(11) = "   A = rand(5,5); b = rand(5,1) ; h = lu(A); "
      h(12) = "   x = lusolv(h,b)"
      h(13) = "   disp(norm(A*x-b),'residual = ')"
      h(14) = " "
      call pk2_assign ( res, h )
      return
   else if ( .not. present(a) .or. .not. present(b) ) then
      call opflag%set ( stat = UERROR, where = HERE, &
                         msg = 'Missing an input argument in << lusolv >>' )
      return
   end if
      
   call pk2f_subLUsolv ( a, b, res ) ; error_TraceNreturn(opflag, HERE)
   
   END FUNCTION pk2f_LUsolv

         
!=============================================================================================    
   SUBROUTINE pk2f_subINV ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Computes the inverse of "a".
!-----------------------------------------------------------------------------------R.H. 04/18       
   
!- local variables ---------------------------------------------------------------------------     
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
         
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subINV', msg =                 &
                        '<< b = inv(a)  >> with << a >> non-allocated (--> b = [ ])' )
      return
   end if

   call res%inv(a) ; error_TraceNreturn(opflag, "pk2f_subINV")
      
   END SUBROUTINE pk2f_subINV
   

!=============================================================================================    
   FUNCTION pk2f_INV ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Computes the inverse of "a".
!-----------------------------------------------------------------------------------R.H. 04/18       
   
!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(6), f
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('inv',pk2f_helpcol)
      h(1) = "<< "+f+" >> computes the inverse of a square matrix"
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"                                
      h(4) = " " 
      h(5) = "(Note: Lapack routines dgetrf/zgetrf and dgetri/zgetri are used)"
      h(6) = " " 
      call pk2_assign ( res, h )
      return
   end if   
         
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_INV', msg =                    &
                        '<< b = inv(a)  >> with << a >> non-allocated (--> b = [ ])' )
      return
   end if
   
   call res%inv(a) ; error_TraceNreturn(opflag, "pk2f_subINV")
      
   END FUNCTION pk2f_INV
   

!=============================================================================================    
   FUNCTION pk2f_INV_old ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Computes the inverse of "a".
!-----------------------------------------------------------------------------------R.H. 04/18       
   
!- local variables ---------------------------------------------------------------------------     
   type(str_t), allocatable :: h(:)
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      allocate(h(5))
      h(1)%str = "<< inv >> computes the inverse of a square matrix"
      h(2)%str = " "
      h(3)%str = "Syntax: B = inv(A)"                                
      h(4)%str = " " 
      h(5)%str = "(Note: Lapack routines dgetrf/zgetrf and dgetri/zgetri are used)"
      call pk2_assign ( res, h )
      return
   end if   
         
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, msg = &
                       '<< b = inv(a)  >> with << a >> non-allocated (--> b = [ ])' )
      return
   end if
   
   select type (p=>a%m)
      type is (ik2_t)
         call bk2_Invmats ( I = p%v, res = res%m )
      type is (rk2_t)
         call bk2_Invmats ( R = p%v, res = res%m )
      type is (ck2_t)
         call bk2_Invmats ( C = p%v, res = res%m )

      type is (lk2_t)
         call opflag%set ( stat = UERROR, msg = &
                 'in << inv(a) >> "a" must be integer, real or complex'//NL// &
                 '    (here, "a" is an array of booleans)' )
      type is (sk2_t)
         call opflag%set ( stat = UERROR, msg = &
                 'in << inv(a) >> "a" must be integer, real or complex'//NL// &
                 '    (here, "a" is an array of strings)' )
      class default
         call opflag%set ( stat = UERROR, msg = &
                 'in << inv(a) >> "a" must be integer, real or complex'//NL// &
                 '    (here, "a" is an array of unknown type)' )
   end select
      
   if ( opflag%code /= 0 ) then
      res%m = bk2_t()
      res%typ = EMPTY
   else
      res%nrow = res%m%nrow ; res%ncol = res%m%ncol ; res%typ = res%m%typ         
   end if
      
   END FUNCTION pk2f_INV_old
   

!=============================================================================================    
   SUBROUTINE pk2f_subMLDIVIDE ( a, b, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a, b
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Solves a general linear system: a * res = b.
!-----------------------------------------------------------------------------------R.H. 07/18       
    
!- local variables ---------------------------------------------------------------------------     
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. &
        b%typ == EMPTY .or. .not. allocated(b%m)       ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subMLDIVIDE', msg =              &
                '<< c = a \ b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
      return
   end if

   call pk2_smldivide ( a, b, res ) ; error_TraceNreturn(opflag, "pk2f_subMLDIVIDE")
      
   END SUBROUTINE pk2f_subMLDIVIDE


!=============================================================================================    
   FUNCTION pk2f_MLDIVIDE ( a, b ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a, b
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Solves a general linear system: a * res = b.
!
!  If help is present (and whatever its value) only the help is returned in "res".
!-----------------------------------------------------------------------------------R.H. 07/18       
    
!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(7), f1, f2
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) .and. .not. present(b) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f1 = str_color('mldivide',pk2f_helpcol)
      f2 = str_color('\',pk2f_helpcol)
      h(1) = "<< "+f2+" >> or << "+f1+" >> solves a linear system"
      h(2) = "(least square solution for non-square systems)"
      h(3) = " "
      h(4) = "Syntax: x = A"+f2+"b,  x = "+f1+"(A,b)"              
      h(5) = " "
      h(6) = "(Note: Lapack routines ?gesv and ?gelsy are used)"
      h(7) = " "
      call pk2_assign ( res, h )
      return
   end if   

   if ( .not. present(a) .or. .not. present(b) ) then
      call opflag%set ( stat = UERROR, where = 'pk2f_MLDIVIDE',     &
                         msg = "<< mldivide >> needs two arguments" )
      return
   end if   
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. &
        b%typ == EMPTY .or. .not. allocated(b%m)      ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_MLDIVIDE', msg =                &
               '<< c = a \ b  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
      return
   end if

   call pk2_smldivide ( a, b, res ) ; error_TraceNreturn(opflag, "pk2f_MLDIVIDE")
      
   END FUNCTION pk2f_MLDIVIDE


!=============================================================================================    
   SUBROUTINE pk2f_subMOD ( a, b, res ) 
!=============================================================================================    
   use, intrinsic :: ieee_arithmetic   
   class(pk2_t), intent(in    ) :: a, b
   class(pk2_t), intent(in out):: res
!---------------------------------------------------------------------------------------------   
!  Computes the remainder after division
!-----------------------------------------------------------------------------------R.H. 04/19     
    
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'pk2f_subMOD'             
   integer  (Ikind)              :: i,j, itmp, n, m, restyp, err
   real     (Rkind)              :: rtmp, NaN
   logical                       :: a_is_scalar, b_is_scalar
   integer  (Ikind), allocatable :: tmpi(:,:)
   real     (Rkind), allocatable :: tmpr(:,:)
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. &
        b%typ == EMPTY .or. .not. allocated(b%m)      ) then
      call opflag%set ( stat = WARNING, where = HERE, msg =                               &
                '<< c = mod(a,b)  >> with << a >> or << b >> non-allocated (--> c = [ ])' )
      return
   end if
   
   if ( a%nrow == 1 .and. a%ncol == 1 ) then
      a_is_scalar = .true.
   else
      a_is_scalar = .false.
   end if

   if ( b%nrow == 1 .and. b%ncol == 1 ) then
      b_is_scalar = .true.
   else
      b_is_scalar = .false.
   end if
      
   if ( .not. a_is_scalar .and. .not. b_is_scalar ) then
      if ( a%nrow /= b%nrow .or. a%ncol /= b%ncol ) then
         call opflag%set ( stat = UERROR, where = HERE, msg =                  &
                          'in << mod(a,b) >> a and b must have the same shape' )
         return
      end if
   end if      
   
   err = 0
   NaN = ieee_value(nan, IEEE_QUIET_NAN)
   
   n = max(a%nrow,b%nrow) ; m = max(a%ncol,b%ncol)
   
   select type (p=>a%m)
      type is (ik2_t)
         select type (q=>b%m)
            type is (ik2_t)

               if ( any(q%v == IZERO) ) then
                  allocate(tmpr(n,m),source = RZERO, stat = err) ; restyp = RTYP
               else    
                  allocate(tmpi(n,m),source = IZERO, stat = err) ; restyp = ITYP
               end if     
               
               if ( err /= 0 ) then
                  call opflag%set (stat = IERROR, where = HERE, msg = 'Allocation failure')
                  return          
               end if
                                             
               if ( (      a_is_scalar .and.       b_is_scalar) .or.  &
                    (.not. a_is_scalar .and. .not. b_is_scalar)       ) then
                    
                  if ( restyp == ITYP ) then  
                     do j = 1, b%ncol
                        do i = 1, b%nrow
                           tmpi(i,j) = mod(p%v(i,j),q%v(i,j))
                        end do
                     end do
                     call pk2_movealloc ( from = tmpi, to = res ) ; return
                  else
                     do j = 1, b%ncol
                        do i = 1, b%nrow
                           itmp = q%v(i,j)
                           if ( itmp /= IZERO ) then
                              tmpr(i,j) = mod(p%v(i,j),itmp)
                           else
                              tmpr(i,j) = NaN
                           end if      
                        end do
                     end do
                     call pk2_movealloc ( from = tmpr, to = res ) ; return
                  end if
                  
               else if ( a_is_scalar ) then

                  itmp = p%v(1,1)

                  if ( restyp == ITYP ) then  
                     do j = 1, b%ncol
                        do i = 1, b%nrow
                           tmpi(i,j) = mod(itmp,q%v(i,j))
                        end do
                     end do
                     call pk2_movealloc ( from = tmpi, to = res ) ; return
                  else
                     do j = 1, b%ncol
                        do i = 1, b%nrow
                           if ( q%v(i,j) /= IZERO ) then
                              tmpr(i,j) = mod(itmp,q%v(i,j))
                           else
                              tmpr(i,j) = NaN
                           end if      
                        end do
                     end do
                     call pk2_movealloc ( from = tmpr, to = res ) ; return
                  end if 
               
               else
                  
                  if ( restyp == ITYP ) then  
                     itmp = q%v(1,1)
                     do j = 1, a%ncol
                        do i = 1, a%nrow
                           tmpi(i,j) = mod(p%v(i,j),itmp)
                        end do
                     end do
                     call pk2_movealloc ( from = tmpi, to = res ) ; return
                  else
                     tmpr = NaN
                     call pk2_movealloc ( from = tmpr, to = res ) ; return
                  end if
               
               end if
                        
            type is (rk2_t)
            
               allocate(tmpr(n,m),source = RZERO, stat = err)
                             
               if ( err /= 0 ) then
                  call opflag%set (stat = IERROR, where = HERE, msg = 'Allocation failure')
                  return          
               end if
               
               if ( (      a_is_scalar .and.       b_is_scalar) .or.  &
                    (.not. a_is_scalar .and. .not. b_is_scalar)       ) then

                  do j = 1, m
                     do i = 1, n
                        rtmp = q%v(i,j)
                        if ( rtmp /= RZERO ) then
                           tmpr(i,j) = mod(real(p%v(i,j),kind=Rkind),rtmp)
                        else
                           tmpr(i,j) = NaN
                        end if
                     end do
                  end do
                           
               else if ( a_is_scalar ) then
                                    
                  rtmp = real(p%v(1,1),kind=Rkind)
                  do j = 1, b%ncol
                     do i = 1, b%nrow
                        if ( q%v(i,j) /= RZERO ) then
                           tmpr(i,j) = mod(rtmp,q%v(i,j))
                        else
                           tmpr(i,j) = NaN
                        end if
                      end do
                  end do
               else

                  rtmp = q%v(1,1)                  
                  if ( rtmp /= RZERO ) then
                     do j = 1, a%ncol
                        do i = 1, a%nrow
                           tmpr(i,j) = mod(real(p%v(i,j),kind=Rkind),rtmp)
                        end do
                     end do
                  else
                     tmpr = NaN
                  end if

               end if
               call pk2_movealloc ( from = tmpr, to = res ) ; return

            class default 
               err = 1
         end select      
         
      type is (rk2_t)
      
         allocate(tmpr(n,m),source = RZERO, stat = err)
                             
         if ( err /= 0 ) then
            call opflag%set (stat = IERROR, where = HERE, msg = 'Allocation failure')
            return          
         end if

         select type (q=>b%m)
            type is (ik2_t)
               if ( (      a_is_scalar .and.       b_is_scalar) .or.  &
                    (.not. a_is_scalar .and. .not. b_is_scalar)       ) then
                 
                  do j = 1, m
                     do i = 1, n
                        itmp = q%v(i,j)
                        if ( itmp /= IZERO ) then
                           tmpr(i,j) = mod(p%v(i,j),real(itmp,kind=Rkind))
                        else
                           tmpr(i,j) = NaN
                        end if
                    end do
                  end do
                           
               else if ( a_is_scalar ) then
                                    
                  rtmp = p%v(1,1)
                  do j = 1, b%ncol
                     do i = 1, b%nrow
                        itmp = q%v(i,j)
                        if ( itmp /= IZERO ) then
                           tmpr(i,j) = mod(rtmp,real(itmp,kind=Rkind))
                        else
                           tmpr(i,j) = NaN
                        end if
                     end do
                  end do

               else
                                 
                  itmp = q%v(1,1)
                  if ( itmp /= IZERO ) then
                     rtmp = real(q%v(1,1),kind=Rkind)
                     do j = 1, a%ncol
                        do i = 1, a%nrow
                           tmpr(i,j) = mod(p%v(i,j),rtmp)
                        end do
                     end do
                  else
                     tmpr = NaN
                  end if
                  
               end if
               
            type is (rk2_t)

               if ( (      a_is_scalar .and.       b_is_scalar) .or.  &
                    (.not. a_is_scalar .and. .not. b_is_scalar)       ) then
                    
                  do j = 1, m
                     do i = 1, n
                        rtmp = q%v(i,j)
                        if ( rtmp /= RZERO ) then
                           tmpr(i,j) = mod(p%v(i,j),rtmp)
                        else
                           tmpr(i,j) = NaN
                        end if
                     end do
                  end do
                                    
               else if ( a_is_scalar ) then
                  
                  rtmp = p%v(1,1)
                  do j = 1, b%ncol
                     do i = 1, b%nrow
                        if ( q%v(i,j) /= RZERO ) then
                           tmpr(i,j) = mod(rtmp,q%v(i,j))
                        else
                           tmpr(i,j) = NaN
                        end if
                     end do
                  end do

               else
               
                  rtmp = q%v(1,1)
                  if ( rtmp /= RZERO ) then
                     do j = 1, a%ncol
                        do i = 1, a%nrow
                           tmpr(i,j) = mod(p%v(i,j),rtmp)
                        end do
                     end do
                  else
                     tmpr = NaN   
                  end if
                  
               end if
               
               call pk2_movealloc ( from = tmpr, to = res ) ; return

            class default 
               err = 1
         end select                     

      class default
         err = 1
   end select
   
   if ( err == 1 ) then
      call opflag%set ( stat = UERROR, where = HERE,                                     &
                         msg = '<< mod(a,b) >> : a and b must be integer or real arrays' )          
   end if
            
   END SUBROUTINE pk2f_subMOD


!=============================================================================================    
   FUNCTION pk2f_MOD ( a, b ) result ( res ) 
!=============================================================================================    
   class(pk2_t), optional, intent(in) :: a, b
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
    
!- local variables ---------------------------------------------------------------------------     
   type   (str_t) :: h(8), f
!---------------------------------------------------------------------------------------------          

   if ( .not. present(a) .and. .not. present(b) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('mod',pk2f_helpcol)
      h(1) = "<< "+f+" >> computes the remainder after division"
      h(2) = " "
      h(3) = "Syntax: r = "+f+"(a,p),  R = "+f+"(A,p),  R = "+f+"(a,P),  R = "+f+"(A,P)"              
      h(4) = " "
      h(5) = "where"
      h(6) = ". a, p, r are integer or real scalars"
      h(7) = ". A, P, R are integer or real arrays of same shape"
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   

   if ( .not. present(a) .or. .not. present(b) ) then
      call opflag%set(stat=UERROR, where="pk2f_MOD", msg="<< mod >> needs two arguments")
      return
   end if   

   call pk2f_subMOD ( a, b, res ) ; error_TraceNreturn(opflag, "pk2f_MOD")
   
   END FUNCTION pk2f_MOD


!=============================================================================================    
   SUBROUTINE pk2f_subSORTn ( a, ord, res, dim, caseinsens, ind )
!=============================================================================================       
   class    (pk2_t),           intent(in    ) :: a
   character(len=*),           intent(in    ) :: ord
   class    (pk2_t),           intent(in out) :: res   
   integer  (Ikind), optional, intent(in    ) :: dim
   logical         , optional, intent(in    ) :: caseinsens
   class    (pk2_t), optional, intent(   out) :: ind   
!--------------------------------------------------------------------------------------------- 
!  Sorts the elements of the array "a" according to "ord", "dim" and "caseinsens" (if 
!  appropriate). Returns also the original indices in "ind" if present.
!
!  Inputs:
!  . a         : the array to sort.
!  . ord       : 1-char with value 'i' for increasing order or 'd' for decreasing order.
!  . dim       : (optional) the dimension along with the sorting must be done.
!                If dim = 1 (default), sort each column. If dim = 2, sort each row.
!  . caseinsens: (optional) for string array, if present, it indicates if the sorting must be
!                done in case insensitive mode (caseinsens = .true.) or in case sensitive 
!                (caseinsens = .false.). The default is caseinsens = .false. (case sensitive).
!
!  Ouputs:
!  . res: the sorted array.
!  . ind: (optional) the original indices.
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subSORTn'
   integer  (Ikind)              :: i, j, k, n, m, err, vdim
   logical                       :: ins, vec
   integer  (Ikind), allocatable :: i1(:), tmpi(:,:), indx(:)
   real     (Rkind), allocatable :: r1(:), tmpr(:,:)
   complex  (Rkind), allocatable :: c1(:), tmpc(:,:)
   type     (str_t), allocatable :: s1(:), tmps(:,:)
!---------------------------------------------------------------------------------------------            

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   err = 0
   
   vdim = 1 ; if ( present(dim) ) vdim = dim
   
   if ( vdim /= 1  .and. vdim /= 2  ) err = 1
   if ( ord /= 'i' .and. ord /= 'd' ) err = 1
         
   if ( err /= 0 ) then
      call opflag%Set(UERROR, HERE, &
               "in << sort(a, ord, res [, dim, caseinsens, ind])>>:"         // NL // &
               "--> if the optional arguments are present they should be: "  // NL // &
               "--> << dim >> an integer (1 or 2)"                           // NL // &
               "--> << caseinsens >> the string 'CaseInsensitive'"           // NL // &
               "--> << ind >> (output) an integer vector'" )
      return
   end if
   
   ins = .false. ; if ( present(caseinsens) ) ins = caseinsens

   n = a%nrow ; m = a%ncol 
      
   if ( n == 1 .or. m == 1 ) then
      vec = .true.
   else
      vec = .false.
   end if      

   if ( present(ind) ) then
      call pk2_SetToType ( typ = ITYP, shape = [n, m], res = ind )
      error_TraceNreturn(opflag, HERE)
   end if
   
   select type (p=>a%m)
      type is (ik2_t)
         
         allocate(tmpi(n,m), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure (tmpi(n,m))')
            return
         end if

         if ( vec ) then
!
!-          Case 1: "a" is a 1 x m or n x 1 matrix:
!         
            allocate(i1(n*m), stat = err)
            if ( err /= 0 ) then
               call opflag%Set(IERROR, HERE, 'Allocation failure (i1(n*m))')
               return
            end if
            
            k = 0
            do j = 1, m
               do i = 1, n
                  k = k + 1
                  i1(k) = p%v(i,j)
               end do
            end do
            if ( present(ind) ) then
               call util_SortIk1 (i1, ord, opflag, indx=indx)
            else   
               call util_SortIk1 (i1, ord, opflag)
            end if
            error_TraceNreturn(opflag, HERE)
            
            if ( n == 1 ) then
               tmpi(1,:) = i1
               if ( present(ind) ) then
                  select type (q=>ind%m) ; type is (ik2_t) ; q%v(1,:) = indx ; end select
               end if
            else
               tmpi(:,1) = i1
               if ( present(ind) ) then
                  select type (q=>ind%m) ; type is (ik2_t) ; q%v(:,1) = indx ; end select
               end if
            end if
         
         else
!
!-          Case 2: "a" is a n x m matrix:
!         
            if ( vdim == 1 ) then
!           
!-             Case 2.1: sort along 1st dimension:
! 
               allocate(i1(n), stat = err)
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure (i1(n))')
                  return
               end if
               
               do j = 1, m
                  do i = 1, n
                     i1(i) = p%v(i,j)
                  end do
                  if ( present(ind) ) then
                     call util_SortIk1 (i1, ord, opflag, indx)
                  else
                     call util_SortIk1 (i1, ord, opflag)
                  end if
                  error_TraceNreturn(opflag, HERE)
                  
                  tmpi(:,j) = i1
                  
                  if ( present(ind) ) then
                     select type (q=>ind%m)
                        type is (ik2_t)
                           q%v(:,j) = indx
                     end select
                  end if         
               end do    
            else
!
!-             Case 2.2: sort along 2nd dimension:
!            
               allocate(i1(m), stat = err)
               
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure (i1(m))')
                  return
               end if
               
               do i = 1, n
                  do j = 1, m
                     i1(j) = p%v(i,j)
                  end do
                  if ( present(ind) ) then
                     call util_SortIk1 (i1, ord, opflag, indx)
                  else    
                     call util_SortIk1 (i1, ord, opflag)
                  end if
                  error_TraceNreturn(opflag, HERE)
                  
                  tmpi(i,:) = i1

                  if ( present(ind) ) then
                     select type (q=>ind%m)
                        type is (ik2_t)
                           q%v(i,:) = indx
                     end select
                  end if         
               end do       
            end if
            
         end if    
                          
         call pk2_movealloc ( from = tmpi, to = res )

      type is (rk2_t)
      
         allocate(tmpr(n,m), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure (tmpr(n,m))')
            return
         end if

         if ( vec ) then
!
!-          Case 1: "a" is a 1 x m or n x 1 matrix:
!                  
            allocate(r1(n*m), stat = err)
            if ( err /= 0 ) then
               call opflag%Set(IERROR, HERE, 'Allocation failure (r1(n*m))')
               return
            end if
            
            k = 0
            do j = 1, m
               do i = 1, n
                  k = k + 1
                  r1(k) = p%v(i,j)
               end do
            end do
            if ( present(ind) ) then
               call util_SortRk1 (r1, ord, opflag, indx)
            else    
               call util_SortRk1 (r1, ord, opflag)
            end if
            error_TraceNreturn(opflag, HERE)
            
            if ( n == 1 ) then
               tmpr(1,:) = r1
               if ( present(ind) ) then
                  select type (q=>ind%m) ; type is (ik2_t) ; q%v(1,:) = indx ; end select
               end if
            else
               tmpr(:,1) = r1
               if ( present(ind) ) then
                  select type (q=>ind%m) ; type is (ik2_t) ; q%v(:,1) = indx ; end select
               end if
            end if

         else
!
!-          Case 2: "a" is a n x m matrix:
!                  
            if ( vdim == 1 ) then
!           
!-             Case 2.1: sort along 1st dimension:
!             
               allocate(r1(n))
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure (r1(n))')
                  return
               end if

               do j = 1, m
                  do i = 1, n
                     r1(i) = p%v(i,j)
                  end do
                  if ( present(ind) ) then
                     call util_SortRk1 (r1, ord, opflag, indx)
                  else    
                     call util_SortRk1 (r1, ord, opflag)
                  end if
                  error_TraceNreturn(opflag, HERE)
                  
                  tmpr(:,j) = r1

                  if ( present(ind) ) then
                     select type (q=>ind%m)
                        type is (ik2_t)
                           q%v(:,j) = indx
                     end select
                  end if         
               end do       
            else
!           
!-             Case 2.1: sort along 2nd dimension:
!                         
               allocate(r1(m))
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure (r1(m))')
                  return
               end if

               do i = 1, n
                  do j = 1, m
                     r1(j) = p%v(i,j)
                  end do
                  if ( present(ind) ) then
                     call util_SortRk1 (r1, ord, opflag, indx)
                  else  
                     call util_SortRk1 (r1, ord, opflag)
                  end if
                  error_TraceNreturn(opflag, HERE)
                  
                  tmpr(i,:) = r1

                  if ( present(ind) ) then
                     select type (q=>ind%m)
                        type is (ik2_t)
                           q%v(i,:) = indx
                     end select
                  end if         
               end do                     
            end if
         end if   
         
         call pk2_movealloc ( from = tmpr, to = res )
               
      type is (ck2_t)
      
         allocate(tmpc(n,m), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure (tmpc(n,m))')
            return
         end if

         if ( vec ) then
!
!-          Case 1: "a" is a 1 x m or n x 1 matrix:
!                           
            allocate(c1(n*m))
            if ( err /= 0 ) then
               call opflag%Set(IERROR, HERE, 'Allocation failure (c1(n*m))')
               return
            end if
            
            k = 0
            do j = 1, m
               do i = 1, n
                  k = k + 1
                  c1(k) = p%v(i,j)
               end do
            end do
            if ( present(ind) ) then
               call util_SortCk1 (c1, ord, opflag, indx)
            else    
               call util_SortCk1 (c1, ord, opflag)
            end if
            error_TraceNreturn(opflag, HERE)
            
            if ( n == 1 ) then
               tmpc(1,:) = c1
               if ( present(ind) ) then
                  select type (q=>ind%m) ; type is (ik2_t) ; q%v(1,:) = indx ; end select
               end if
            else
               tmpc(:,1) = c1
               if ( present(ind) ) then
                  select type (q=>ind%m) ; type is (ik2_t) ; q%v(:,1) = indx ; end select
               end if
            end if
         
         else
!
!-          Case 2: "a" is a n x m matrix:
!                           
            if ( vdim == 1 ) then
!           
!-             Case 2.1: sort along 1st dimension:
!                         
               allocate(c1(n))
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure (c1(n))')
                  return
               end if
               do j = 1, m
                  do i = 1, n
                     c1(i) = p%v(i,j)
                  end do
                  if ( present(ind) ) then
                     call util_SortCk1 (c1, ord, opflag, indx)
                  else    
                     call util_SortCk1 (c1, ord, opflag)
                  end if
                  error_TraceNreturn(opflag, HERE)

                  tmpc(:,j) = c1
                  
                  if ( present(ind) ) then
                     select type (q=>ind%m)
                        type is (ik2_t)
                           q%v(:,j) = indx
                     end select
                  end if         
               end do       
            else
!           
!-             Case 2.2: sort along 2nd dimension:
!                         
               allocate(c1(m))
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure (c1(m))')
                  return
               end if
               
               do i = 1, n
                  do j = 1, m
                     c1(j) = p%v(i,j)
                  end do
                  if ( present(ind) ) then
                     call util_SortCk1 (c1, ord, opflag, indx)
                  else    
                     call util_SortCk1 (c1, ord, opflag)
                  end if
                  error_TraceNreturn(opflag, HERE)
                  
                  tmpc(i,:) = c1

                  if ( present(ind) ) then
                     select type (q=>ind%m)
                        type is (ik2_t)
                           q%v(i,:) = indx
                     end select
                  end if         
               end do                     
            end if
         end if   
         
         call pk2_movealloc ( from = tmpc, to = res )
               
      type is (sk2_t)
      
         allocate(tmps(n,m), stat = err)
         if ( err /= 0 ) then
            call opflag%Set(IERROR, HERE, 'Allocation failure (tmps(n,m))')
            return
         end if

         if ( vec ) then
!
!-          Case 1: "a" is a 1 x m or n x 1 matrix:
!                                    
            allocate(s1(n*m))
            if ( err /= 0 ) then
               call opflag%Set(IERROR, HERE, 'Allocation failure (s1(n*m))')
               return
            end if
            k = 0
            do j = 1, m
               do i = 1, n
                  k = k + 1
                  if ( allocated(p%v(i,j)%str) ) then
                     s1(k)%str = (p%v(i,j)%str)
                  else
                     s1(k)%str = ''
                  end if      
               end do
            end do
            if ( present(ind) ) then
               call util_SortSk1 (s1, ord, is_CaseInsensitive=ins, indx=indx, stat=opflag)
               error_TraceNreturn(opflag, HERE)
               tmps = reshape(s1,[n,m]) ; ind = reshape(indx,[n,m])
            else    
               call util_SortSk1 (s1, ord, is_CaseInsensitive=ins, stat=opflag)
               error_TraceNreturn(opflag, HERE)
               tmps = reshape(s1,[n,m]) 
            end if
            
            if ( n == 1 ) then
               do j = 1, m
                  tmps(1,j)%str = (s1(j)%str)
               end do   
               if ( present(ind) ) then
                  select type (q=>ind%m) ; type is (ik2_t) ; q%v(1,:) = indx ; end select
               end if
            else
               do i = 1, n
                  tmps(i,1)%str = (s1(i)%str)
               end do   
               if ( present(ind) ) then
                  select type (q=>ind%m) ; type is (ik2_t) ; q%v(:,1) = indx ; end select
               end if
            end if

         else
!
!-          Case 2: "a" is a n x m matrix:
!                                    
            if ( vdim == 1 ) then
!           
!-             Case 2.1: sort along 1st dimension:
!                                     
               allocate(s1(n))
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure (s1(n))')
                  return
               end if

               do j = 1, m
                  do i = 1, n
                     if ( allocated(p%v(i,j)%str) ) then
                        s1(i)%str = (p%v(i,j)%str)
                     else   
                        s1(i)%str = ''
                     end if   
                  end do
                  if ( present(ind) ) then
                     call util_SortSk1 (s1, ord, is_CaseInsensitive=ins, indx=indx, stat=opflag)
                  else    
                     call util_SortSk1 (s1, ord, is_CaseInsensitive=ins, stat=opflag)
                  end if
                  error_TraceNreturn(opflag, HERE)
                  
                  do i = 1, n
                     tmps(i,j)%str = (s1(i)%str)   
                  end do               

                  if ( present(ind) ) then
                     select type (q=>ind%m)
                        type is (ik2_t)
                           q%v(:,j) = indx
                     end select
                  end if         
               end do       
            else
!           
!-             Case 2.2: sort along 2nd dimension:
!                                     
               allocate(s1(m))
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure (s1(m))')
                  return
               end if

               do i = 1, n
                  do j = 1, m
                     if ( allocated(p%v(i,j)%str) ) then
                        s1(j)%str = (p%v(i,j)%str)
                     else
                        s1(j)%str = ''
                     end if      
                  end do
                  if ( present(ind) ) then
                     call util_SortSk1 (s1, ord, is_CaseInsensitive=ins, indx=indx, stat=opflag)
                  else    
                     call util_SortSk1 (s1, ord, is_CaseInsensitive=ins, stat=opflag)
                  end if
                  error_TraceNreturn(opflag, HERE)
                  
                  do j = 1, m
                     tmps(i,j)%str = (s1(j)%str)
                  end do
                     
                  if ( present(ind) ) then
                     select type (q=>ind%m)
                        type is (ik2_t)
                           q%v(i,:) = indx
                     end select
                  end if         
               end do                     
            end if
         end if 
           
         call pk2_movealloc ( from = tmps, to = res )
         
      class default
         res = pk2_t()
         
   end select
   
   END SUBROUTINE pk2f_subSORTn
   
   
!=============================================================================================    
   FUNCTION pk2f_SORTn ( a, ord, dim, caseinsens, ind ) result ( res ) 
!=============================================================================================       
   class    (pk2_t),           intent(in    ) :: a
   character(len=*),           intent(in    ) :: ord
   integer  (Ikind), optional, intent(in    ) :: dim
   logical         , optional, intent(in    ) :: caseinsens
   class    (pk2_t), optional, intent(   out) :: ind   
   type     (pk2_t)                           :: res
!--------------------------------------------------------------------------------------------- 

   call pk2f_subSORTn ( a, ord, res, dim, caseinsens, ind )
   
   error_TraceNreturn(opflag, "pk2f_SORTn") 
   
   END FUNCTION pk2f_SORTn
   
         
!=============================================================================================    
   SUBROUTINE pk2f_subSORTwrp ( matrs, res, res2 )
!=============================================================================================       
   class(pk2_t),           intent(in    ) :: matrs(:)
   class(pk2_t),           intent(in out) :: res   
   class(pk2_t), optional, intent(in out) :: res2  
!---------------------------------------------------------------------------------------------   
!  Sorts the elements of the array matrs(1). 
!
!  If size(matrs) = 1:
!  . sorts each column in increasing order
!
!  If size(matrs) = 2: matrs(2) may give
!  . the direction ('i' for increasing or 'd' for descending),
!  . the dimension (1 for column or 2 for row) along which matrs(1) will be sorted.
!                      
!  If size(matrs) = 3: matrs(2) and matrs(3) give the direction and the dimension.
!
!  The sorted array is returned in res and the initial indices, if requested, in res2
!
!  Note: the main interest of this "wrapped" version is especially useful for pk2Interpreter
!----------------------------------------------------------------------------R.H. 10/18, 04/19       

!- local variables ---------------------------------------------------------------------------
   integer  (Ikind)              :: nargs, dim, i, err
   character(len=:), allocatable :: ord
   logical                       :: insens
   type     (pk2_t)              :: tmp1, tmp2
!---------------------------------------------------------------------------------------------            

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   nargs = size(matrs)
   
   if ( nargs < 1 ) return
   
   if ( .not. allocated(matrs(1)%m) .or. matrs(1)%typ == EMPTY .or. &
       matrs(1)%nrow == 0 .or. matrs(1)%ncol == 0                   ) return
   
   if ( matrs(1)%typ == LTYP ) then
      call opflag%Set(UERROR,"pk2f_subSORTwrp", &
               "in << sort(a [,b[, c]]) >>: a must be of numerics type or of string type")
      return
   end if   
   
!
!- Determine the direction ("ord") and the dimension ("vdim"):
!         
   ord = '?' ; dim = 0 ; insens =.false. ; err = 0
       
   do i = 2, nargs
      if ( allocated(matrs(i)%m) ) then
         select type (p=>matrs(i)%m)
            type is (sk2_t)
               if ( allocated(p%v) .and. p%nrow /= 0 .and. p%ncol /= 0 ) then
                  if ( p%v(1,1)%str == 'i' .or. p%v(1,1)%str == 'd' ) then
                     if ( ord == '?' ) then
                        ord = (p%v(1,1)%str)
                     else 
                        err = 1
                     end if
                  else if ( p%v(1,1)%str == 'CaseInsensitive' ) then
                     if ( .not. insens ) then
                        insens = .true.
                     else
                        err = 1
                     end if
                  else
                     err = 1   
                  end if    
               end if                                       
            type is (ik2_t)     
               if ( allocated(p%v) .and. p%nrow /= 0 .and. p%ncol /= 0 ) then
                  if ( dim == 0 ) then
                     dim = p%v(1,1)
                  else
                     err = 1
                  end if   
               end if       
            class default
               err = 1
         end select
      end if
   end do
   
   if ( err /= 0 ) then
      call opflag%Set(UERROR, "pk2f_subSORTwrp",                             &
               "in << sort(a [,b[,c,[d]]]) >>:"                              // NL // &
               "--> if the optional arguments are present they should be: "  // NL // &
               "--> << b >> an integer (1 or 2) or a character ('i' or 'd')" // NL // &
               "--> << c >> a character ('i' or 'd') or an integer (1 or 2)" // NL // &
               "--> << d >> the string 'CaseInsensitive'" )
      return
   end if
   
   if ( dim == 0 ) dim = 1 ; if ( ord == '?' ) ord = 'i'

   if ( present(res2) ) then    
      call pk2f_subSORTn ( matrs(1), ord, res=tmp1, dim=dim, caseinsens=insens, ind=tmp2 )
      error_TraceNreturn(opflag, "pk2f_subSORTwrp") 
      call pk2_moveAlloc ( from = tmp1, to = res  )
      call pk2_moveAlloc ( from = tmp2, to = res2 )
   else   
      call pk2f_subSORTn ( matrs(1), ord, res=tmp1, dim=dim, caseinsens=insens )
      error_TraceNreturn(opflag, "pk2f_subSORTwrp") 
      call pk2_moveAlloc ( from = tmp1, to = res  )
   end if      
      
   END SUBROUTINE pk2f_subSORTwrp


!=============================================================================================    
   FUNCTION pk2f_SORTwrp ( matrs, res2 ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in    ) :: matrs(:)
   class(pk2_t), optional, intent(in out) :: res2
   type (pk2_t)                           :: res
!---------------------------------------------------------------------------------------------     

!- local variables ---------------------------------------------------------------------------
   type(str_t) :: h(23), f
!---------------------------------------------------------------------------------------------            
   
   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps (for calmat) and exit:
!     
      f = str_color('sort',pk2f_helpcol)
      h( 1) = "<< "+f+" >> sort array elements"
      h( 2) = " "
      h( 3) = "Syntax: . B     = "+f+"(A,[,ord,dim,'CaseInsensitive'])"
      h( 4) = "        . [B,I] = "+f+"(A,[,ord,dim,'CaseInsensitive'])"
      h( 5) = " "
      h( 6) = "where the optional arguments may appear in any order with"
      h( 7) = " . ord: 'i' for increasing or 'd' for decreasing"
      h( 8) = " . dim: 1 (sort each column) or 2 (sort each row)"
      h( 9) = " "
      h(10) = "and if present, 'CaseInsensitive' is used for array of strings"
      h(11) = "(the default is case sensitive)."
      h(12) = " "
      h(13) = "Examples:"
      h(14) = " . "+f+"(A)      : sort each column of A in increasing order"
      h(15) = "                  (same as "+f+"(A,1)" 
      h(16) = " . "+f+"(A,2)    : sort each row of A in increasing order"
      h(17) = " . "+f+"(A,'d')  : sort each column of A in decreasing order"
      h(18) = " . "+f+"(A,2,'d'): sort each row of A in decreasing order"
      h(19) = " "
      h(20) = " . [B,I] = "+f+"(A): save the sorted array A in B and save the "
      h(21) = "                    orginal indices in I"
      h(22) = "                    (if A is an n-vector, B = A(I))"
      h(23) = " "
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subSORTwrp ( matrs, res, res2 ) ;  error_TraceNreturn(opflag, "pk2f_SORTwrp") 
   
   END FUNCTION pk2f_SORTwrp
    
      
!=============================================================================================    
   SUBROUTINE pk2f_subEIG ( a, eigval, eigvec ) 
!=============================================================================================       
   class(pk2_t),           intent(in    ) :: a
   class(pk2_t),           intent(in out) :: eigval
   class(pk2_t), optional, intent(in out) :: eigvec
!---------------------------------------------------------------------------------------------   
!  Computes the eigenvalues of the general matrix "a".
!  The eigenvectors are also computed if eigvec is present.
!
!  Version 2: the ouputs are returned in eigval and eigvec.
!---------------------------------------------------------------------R.H. 10/18, 04/19, 12/19       

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2f_subEIG'
!---------------------------------------------------------------------------------------------            

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%Set(WARNING, HERE,                                      &
                      '<< c = eig(a)  >> with << a >> non-allocated (--> c = [ ])' )
      return
   end if
   
   if ( a%typ /= ITYP .and. a%typ /= RTYP .and. a%typ /= CTYP ) then
      call opflag%Set(UERROR, HERE,                                             &
               "in << eig(a) >>  << a >> must be an integer, a real or a complex matrix" )
      return
   end if
      
   if ( a%nrow /= a%ncol ) then
      call opflag%Set(UERROR, HERE, "in << eig(a) >>  << a >> must be square")
      return
   end if   
      
   select type (p=>a%m)
   
      type is (ik2_t)
         if ( a%Is_Symm() ) then
!
!-          case of a symetric integer matrix:
!       
            call pk2f_subEigRsym ( eigval = eigval, Imat = p%v, eigvec = eigvec )         
         else
!
!-          case of a general integer matrix:
!    
            call pk2f_subEigRgen ( eigval = eigval, Imat = p%v, eigvec = eigvec )
         end if   
         
      type is (rk2_t)
         if ( a%Is_Symm() ) then
!
!-          case of a symetric real matrix:
!          
            call pk2f_subEigRsym ( eigval = eigval, Rmat = p%v, eigvec = eigvec )         
         else
!
!-          case of a general real matrix:
!          
            call pk2f_subEigRgen ( eigval = eigval, Rmat = p%v, eigvec = eigvec )
         end if 
         
      type is (ck2_t)
         if ( a%Is_Symm() ) then
!
!-          case of an hermitian matrix:
!          
            call pk2f_subEigCherm ( Cmat = p%v, eigval = eigval, eigvec = eigvec )   
         else      
!
!-          case of a general complex matrix:
!          
            call pk2f_subEigCgen  ( Cmat = p%v, eigval = eigval, eigvec = eigvec )
         end if   
   end select       
   
   error_TraceNreturn(opflag, HERE) 
   
   END SUBROUTINE pk2f_subEIG
   

!=============================================================================================    
   FUNCTION pk2f_EIG ( a, res2 ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in    ) :: a
   class(pk2_t), optional, intent(   out) :: res2
   type (pk2_t)                           :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------
   type(str_t) :: h(10), f
!---------------------------------------------------------------------------------------------            
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( .not. present(a) ) then
!
!-    return in "res" some helps (for calmat) and exit:
!     
      f = str_color('eig',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the eigenvalues and eigenvectors of an n x n matrix"
      h( 2) = " "
      h( 3) = "Syntax: d = "+f+"(A)  or  [d,P] = "+f+"(A)"
      h( 4) = " "            
      h( 5) = " . d: an n-vector formed by the eigenvalues of A"            
      h( 6) = " . P: an n x n matrix whose columns are the corresponding right eigenvectors" 
      h( 7) = "      i.e. A*P(:,i) = d(i)*P(:,i),  i = 1,2,..,n" 
      h( 8) = " "           
      h( 9) = "(Note: lapack routines [s/d]geev-[c/z]geev and [s/d]syev-[c/z]heev are used)"
      h(10) = " "           
      call pk2_assign ( res, h )
      return
   end if      
   
   call pk2f_subEIG ( a=a, eigval=res, eigvec=res2 ) 

   error_TraceNreturn(opflag, "pk2f_EIG") 
      
   END FUNCTION pk2f_EIG


!=============================================================================================
   SUBROUTINE pk2f_subEigRsym ( eigval, Imat, Rmat, eigvec )
!=============================================================================================
   use util_m, only: util_EigRsym
   class  (pk2_t),           intent(in out) :: eigval
   integer(Ikind), optional, intent(in    ) :: Imat(:,:)
   real   (Rkind), optional, intent(in    ) :: Rmat(:,:)
   class  (pk2_t), optional, intent(in out) :: eigvec
!---------------------------------------------------------------------------------------------
!  Computes the eigenvalues (eigval) and the eigenvectors (eigvec, if present) of the 
!  symmetric integer matrix Imat or the symmetric real matrix Rmat.
!
!  Examples :  call pk2f_subEigRsym (Imat=a, eigval=eigval),   
!              call pk2f_subEigRsym (Rmat=a, eigval=eigval, eigvec=eigvec)
!
!  external routines:
!     . ?SYEV (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'pk2f_subEigRsym'          
   integer  (Ikind )              :: n, err
   real     (Rkind ), allocatable :: mat(:,:), w(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
!
!- Make a copy of the input matrix (Lapack will overwrite it):
!
   if ( present(Imat) ) then
      n = size(Imat,1) 
      allocate(mat(n,n), stat = err)
      if ( err == 0 ) mat(:,:) = real(Imat(:,:),kind=Rkind)
   else if ( present(Rmat) ) then 
      n = size(Rmat,1) 
      allocate(mat(n,n), stat = err)
      if ( err == 0 ) mat(:,:) = Rmat(:,:)
   else
      call opflag%Set(UERROR, HERE,'The input matrix is missing!')
      return
   end if
   
   if ( err == 0 ) then
      allocate(w(n,1), stat = err)  
   else
      call opflag%Set(IERROR, HERE,"Allocation failure for 'mat' or 'w'")
      return
   end if   
!
!- Compute the eigenvalues (and eigenvectors):
!       
   call util_EigRsym &
              ( Rmat = mat, compute_eigvec = present(eigvec), eigval = w(:,1), stat = opflag )
              
   error_TraceNreturn(opflag, HERE)
!
!- Move to eigval and eigvec:
!   
                          call pk2_moveAlloc ( from = w  , to = eigval )
   if ( present(eigvec) ) call pk2_moveAlloc ( from = mat, to = eigvec )
      
   END SUBROUTINE pk2f_subEigRsym


!=============================================================================================
   SUBROUTINE pk2f_subEigCherm ( Cmat, eigval, eigvec )
!=============================================================================================
   use util_m, only: util_EigCherm
   complex(Rkind),           intent(in    ) :: Cmat(:,:)
   class  (pk2_t),           intent(in out) :: eigval
   class  (pk2_t), optional, intent(in out) :: eigvec
!---------------------------------------------------------------------------------------------
!  Computes the eigenvalues (eigval) and the eigenvectors (eigvec if present) of a complex 
!  hermitian matrix
!
!  Examples :  call pk2f_EigCherm (a, eigval)
!              call pk2f_EigCherm (a, eigval, eigvec)
!
!  external routines:
!     . ?HEEV (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'pk2f_subEigCherm'          
   integer  (Ikind )              :: n, err
   logical                        :: is_real
!- needed by LAPACK routine ?heev: -----------------------------------------------------------
   real     (Rkind ), allocatable :: w(:,:)
   complex  (Rkind ), allocatable :: zmat(:,:)
   real     (Rkind ), allocatable :: rmat(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   n = size(Cmat,2)
!
!- Make a copy of the input matrix (Lapack will overwrite it):
!
   is_real = util_IsReal(Cmat)
   
   if ( is_real ) then
      allocate(rmat(n,n), stat = err)
      if ( err == 0 ) rmat(:,:) = real(Cmat(:,:),kind=Rkind)
   else
      allocate(zmat, source = Cmat, stat = err)
   end if
   
   if ( err == 0 ) then
      allocate(w(n,1), stat = err)  
   else
      call opflag%Set(IERROR, HERE, "Allocation failure")
      return
   end if   
!
!- Compute the eigenvalues (and eigenvectors):
!       
   if ( is_real ) then
      call util_EigRsym  &
             ( Rmat = rmat, compute_eigvec = present(eigvec), eigval = w(:,1), stat = opflag )
      error_TraceNreturn(opflag, HERE)
      if ( present(eigvec) ) call pk2_moveAlloc ( from = rmat, to = eigvec )       
   else
      call util_EigCherm &
             ( Cmat = zmat, compute_eigvec = present(eigvec), eigval = w(:,1), stat = opflag )
      error_TraceNreturn(opflag, HERE)
      if ( present(eigvec) ) call pk2_moveAlloc ( from = zmat, to = eigvec ) 
   end if
   
   call pk2_moveAlloc ( from = w, to = eigval )
        
   END SUBROUTINE pk2f_subEigCherm


!=============================================================================================
   SUBROUTINE pk2f_subEigRgen ( eigval, Imat, Rmat, Cmat, eigvec )
!=============================================================================================
   use, intrinsic :: ieee_arithmetic
   use util_m, only: util_EigRgen
   class  (pk2_t),           intent(in out) :: eigval
   integer(Ikind), optional, intent(in    ) :: Imat(:,:)
   real   (Rkind), optional, intent(in    ) :: Rmat(:,:)
   complex(Rkind), optional, intent(in    ) :: Cmat(:,:)
   class  (pk2_t), optional, intent(in out) :: eigvec
!---------------------------------------------------------------------------------------------
!  Computes the eigenvalues (eigval) and the right eigenvectors (eigvec, if present) of a
!  NON-SYMMETRIC REAL SQUARE MATRIX.
!  
!  On entry this matrix may be given as:
!  . a real matrix (Rmat),
!  . an integer matrix (Imat),
!  . a complex matrix (Cmat). It is assumed that its imaginary part is 0. No check is done.
!
!  Examples :  call pk2f_subEigRgen (Imat=a, eigval=eigval),   
!              call pk2f_subEigRgen (Rmat=a, eigval=eigval, eigvec=eigvec)
!
!  external routines:
!     . ?GEEV (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'pk2f_subEigRgen'          
   integer  (Ikind )              :: n, j, err
   real     (Rkind ), allocatable :: mat(:,:), wreal(:,:), wimag(:,:), vright(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
!
!- Make a copy of the input matrix (Lapack will overwrite it):
!
   if ( present(Imat) ) then
      n = size(Imat,1) 
      allocate(mat(n,n), stat = err)
      if ( err == 0 ) mat(:,:) = real(Imat(:,:),kind=Rkind)
   else if ( present(Rmat) ) then 
      n = size(Rmat,1) 
      allocate(mat(n,n), stat = err)
      if ( err == 0 ) mat(:,:) = Rmat(:,:)
   else if ( present(Cmat) ) then
      n = size(Cmat,1)
      allocate(mat(n,n), stat = err)
      if ( err == 0 ) mat(:,:) = real(Cmat(:,:),kind=Rkind)
   else
      call opflag%Set(IERROR, HERE, 'The input matrix is missing!')
      return
   end if
   
   if ( err == 0 ) allocate(wreal(n,1), wimag(n,1), stat = err)  
   if ( err == 0 .and. present(eigvec) ) allocate (vright(n,n), stat = err)

   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, &
                      "Allocation failure for 'mat', 'wreal', 'wimag' or 'vright'" )
      return
   end if   
!
!- Compute the eigenvalues (and eigenvectors):
!       
   if ( present(eigvec) ) then
      call util_EigRgen ( mat, wreal(:,1), wimag(:,1), opflag, vright ) 
   else
      call util_EigRgen ( mat, wreal(:,1), wimag(:,1), opflag )
   end if
   
   error_TraceNreturn(opflag, HERE)
!
!- Store the eigenvalues and eigenvectors:
!
   if ( any(wimag(:,1) /= 0.0_Rkind) ) then 
!
!-    Case 1 - Complex eigenvalues and eigenvectors:
!   
      call pk2_SetToType ( typ = CTYP, shape = [n, IONE], res = eigval )
      error_TraceNreturn(opflag, HERE)
      
      select type (p=>eigval%m)
         type is (ck2_t)
            do j = 1, n
               p%v(j,1) = cmplx(wreal(j,1),wimag(j,1),kind=Rkind)
            end do
      end select

      if ( present(eigvec) ) then
         call pk2_SetToType ( typ = CTYP, shape = [n, n], res = eigvec )
         error_TraceNreturn(opflag, HERE)
         
         select type (q=>eigvec%m)
            type is (ck2_t)
               j = 1
               do while ( j <= n )
                  if (wimag(j,1) == 0.0_Rkind) then
                     ! q%v(:,j) = vright(:,j) ! ICE (nagfor 7211): assigning to 'Complex' from incompatible type 'float'
                     q%v(:,j) = cmplx(vright(:,j),kind=Rkind) 
                     j = j + 1         
                  else
                     q%v(:,j  ) = cmplx(vright(:  ,j),+vright(: ,j+1),kind=Rkind)                            
                     q%v(:,j+1) = cmplx(vright(:  ,j),-vright(: ,j+1),kind=Rkind)
                     j = j + 2
                  end if
               end do
         end select
      end if
      
   else
!
!-    Case 2 - All eigenvalues and eigenvectors are real:
!      
      call pk2_moveAlloc ( from = wreal, to = eigval )
      
      if ( present(eigvec) ) call pk2_moveAlloc ( from = vright, to = eigvec )
   
   end if

   call ieee_set_flag (ieee_overflow, flag_value=.false.) ! Hmm yes I cheat...I don't know the 
                                                          ! reason, but when eigenvectors are 
                                                          ! requested (and only in this case)
                                                          ! an overflow is observed. But the 
                                                          ! result is good. So I reset here
                                                          ! the ieee_overflow to .false.!
                                                          ! Have to look in detail at ?geev
                                                          ! but I suspect it comes from the
                                                          ! normalization of the eigenvectors.
   END SUBROUTINE pk2f_subEigRgen


!=============================================================================================
   SUBROUTINE pk2f_subEigCgen ( Cmat, eigval, eigvec )
!=============================================================================================
   use util_m, only: util_EigCgen
   complex(Rkind),           intent(in    ) :: Cmat(:,:)
   class  (pk2_t),           intent(in out) :: eigval
   class  (pk2_t), optional, intent(in out) :: eigvec
!---------------------------------------------------------------------------------------------
!  Computes the eigenvalues (eigval) and the eigenvectors (eigvec if present) of a complex 
!  square matrix.
!
!  Example :  call pk2f_subEigCgen (a, eigval)
!             call pk2f_subEigCgen (a, eigval, eigvec)
!
!  external routines:
!     . ?GEEV (lapack)
!-----------------------------------------------------------------------------------R.H. 05/18

!- local variables ---------------------------------------------------------------------------   
   character(len=* ), parameter   :: HERE = 'pk2f_subEigCgen'          
   integer  (Ikind )              :: n, err
   complex  (Rkind ), allocatable :: mat(:,:), w(:,:), vright(:,:)
   logical                        :: is_real
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   n = size(Cmat,2)

   is_real = util_IsReal(Cmat)
!
!- If all elements of Cmat are reals call pk2f_subEigRgen and exit:   
!
   if ( is_real ) then
      call pk2f_subEigRgen ( eigval = eigval, Cmat = Cmat, eigvec = eigvec )
      error_TraceNreturn(opflag, HERE)
      return
   end if
!
!- Make a copy of the input matrix (Lapack will overwrite it):
!
   allocate(mat, source = Cmat, stat = err)
   if ( err == 0)  allocate(w(n,1), stat = err)
   if ( err == 0 .and. present(eigvec) ) allocate(vright(n,n), stat = err)
    
   if ( err /= 0 ) then
      call opflag%Set(IERROR, HERE, 'Allocation failure')
      return
   end if     
!
!- Compute the eigenvalues (and eigenvectors):
!
   if ( present(eigvec) ) then
      call util_EigCgen ( mat, w(:,1), opflag, vright )
   else
      call util_EigCgen ( mat, w(:,1), opflag )
   end if
      
   error_TraceNreturn(opflag, HERE)
!
!- Store the eignvalues and eigenvectors:
!      
                          call pk2_moveAlloc ( from = w     , to = eigval)
   if ( present(eigvec) ) call pk2_moveAlloc ( from = vright, to = eigvec)
            
   END SUBROUTINE pk2f_subEigCgen
   

!=============================================================================================    
   SUBROUTINE pk2f_subSVD ( A, S, U, V, saveA ) 
!=============================================================================================       
   class(pk2_t),           intent(in out) :: A
   class(pk2_t),           intent(in out) :: S
   class(pk2_t), optional, intent(in out) :: U, V
   logical     , optional, intent(in    ) :: saveA
!---------------------------------------------------------------------------------------------   
!  Computes singular values of A (call to lapack routines)
!
!  if saveA is present and .true., A is unchanged (work with a copy of A)
!-----------------------------------------------------------------------------------R.H. 05/24       

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subSVD'
   real     (Rkind), allocatable :: utmpR(:,:), vtmpR(:,:), atmp(:,:), stmp(:,:)
   complex  (Rkind), allocatable :: utmpC(:,:), vtmpC(:,:)
   integer                       :: i, n, m, typ = 0
!---------------------------------------------------------------------------------------------            

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   n = a%nrow ; m = a%ncol
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. n == 0 .or. m == 0 ) then
      call opflag%Set(WARNING, HERE,                                      &
                      '<< c = svd(a)  >> with << a >> non-allocated (--> c = [Ê])' )
      return
   end if
   
   if ( a%typ /= ITYP .and. a%typ /= RTYP .and. a%typ /= CTYP ) then
      call opflag%Set(UERROR, HERE,                                             &
               "in << svd(a) >>  << a >> must be an integer, a real or a complex matrix" )
      return
   end if
   
   allocate(stmp(min(n,m),1))
            
   select type (p=>a%m)
   
      type is (ik2_t)
         typ = RTYP
         atmp = real(p%v,kind=Rkind)
         if ( present(u) .and. present(v) ) then
            allocate(utmpR(n,n),vtmpR(m,m))
            call util_Svd ( a=atmp, s=stmp(:,1), stat=opflag, u=utmpR, vt=vtmpR )
         else if ( present(u) ) then
            allocate(utmpR(n,n))
            call util_Svd ( a=atmp, s=stmp(:,1), stat=opflag, u=utmpR )
         else if ( present(v) ) then
            allocate(vtmpR(m,m))
            call util_Svd ( a=atmp, s=stmp(:,1), stat=opflag, vt=vtmpR )
         else
            call util_Svd ( a=atmp, s=stmp(:,1), stat=opflag )
         end if
         atmp = RZERO
         
      type is (rk2_t)
         typ = RTYP         
         if ( present(u) .and. present(v) ) then
            allocate(utmpR(n,n),vtmpR(m,m))
            call util_Svd ( a=p%v, s=stmp(:,1), stat=opflag, u=utmpR, vt=vtmpR, saveA=saveA )
         else if ( present(u) ) then
            allocate(utmpR(n,n))
            call util_Svd ( a=p%v, s=stmp(:,1), stat=opflag, u=utmpR, saveA=saveA )
         else if ( present(v) ) then
            allocate(vtmpR(m,m))
            call util_Svd ( a=p%v, s=stmp(:,1), stat=opflag, vt=vtmpR, saveA=saveA )
         else
            call util_Svd ( a=p%v, s=stmp(:,1), stat=opflag, saveA=saveA )
         end if     
             
      type is (ck2_t)
         typ = CTYP
         if ( present(u) .and. present(v) ) then
            allocate(utmpC(n,n),vtmpC(m,m))
            call util_Svd ( a=p%v, s=stmp(:,1), stat=opflag, u=utmpC, vt=vtmpC, saveA=saveA )
         else if ( present(u) ) then
            allocate(utmpC(n,n))
            call util_Svd ( a=p%v, s=stmp(:,1), stat=opflag, u=utmpC, saveA=saveA )
         else if ( present(v) ) then
            allocate(vtmpC(m,m))
            call util_Svd ( a=p%v, s=stmp(:,1), stat=opflag, vt=vtmpC, saveA=saveA )
         else
            call util_Svd ( a=p%v, s=stmp(:,1), stat=opflag, saveA=saveA )
         end if     

   end select       
   
   error_TraceNreturn(opflag, HERE) 

   if ( present(u) .or. present(v) ) then
!
!-    Store S (same shape as A) with singular values on its diagonal:
!
      if ( .not. allocated(atmp) ) allocate(atmp(n,m), source = RZERO)
      do i = 1, size(stmp)
         atmp(i,i) = stmp(i,1)
      end do
      call pk2_movealloc ( from = atmp, to = s )

      if ( present(u) ) then
!
!-       Store U:
!   
         if ( typ == RTYP ) then
            call pk2_movealloc ( from = utmpR, to = u )
         else
            call pk2_movealloc ( from = utmpC, to = u )
         end if
      end if
   
      if ( present(v) ) then
!
!-       Store V:
!   
         if ( typ == RTYP ) then
            vtmpR = transpose(vtmpR)
            call pk2_movealloc ( from = vtmpR, to = v )
         else
            vtmpC = conjg(transpose(vtmpC))
            call pk2_movealloc ( from = vtmpC, to = v )
         end if
      end if
      
   else
!
!-    Store the singular values in an n-vector:
!
      call pk2_movealloc ( from = stmp, to = s )
   end if
   
   END SUBROUTINE pk2f_subSVD
   
   
!=============================================================================================    
   FUNCTION pk2f_SVD ( A, U, V ) result ( S ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in out) :: A
   class(pk2_t), optional, intent(in out) :: U, V
   type (pk2_t)                           :: S
!---------------------------------------------------------------------------------------------   
!  Computes the SVD of A (A preserved)
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------
   type(str_t) :: h(14), f
!---------------------------------------------------------------------------------------------            
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( .not. present(A) ) then
!
!-    return in "S" some helps (for calmat) and exit:
!     
      f = str_color('svd',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the singular values of an n x m matrix"
      h( 2) = " "
      h( 3) = "Syntax: s = "+f+"(A)  or  [U,S] = "+f+"(A) or  [U,S,V] = "+f+"(A)"
      h( 4) = " "            
      h( 5) = " . A: an integer, a real or a complex n x m matrix"            
      h( 6) = " . s: a real n-vector formed by the singular values of A"            
      h( 7) = " . S: a real n x m matrix with the singular values of A on its diagonal"
      h( 8) = " . U: Left singular vectors (n x n matrix)"
      h( 9) = " . V: Right singular vectors (m x m matrix)"
      h(10) = " "           
      h(11) = "   with A = U * S * V'"      
      h(12) = " "           
      h(13) = "(Note: lapack routines [s/d]gesvd or [c/z]gesvd are used)"
      h(14) = " "           
      S = h
      return
   end if      
   
   if ( present(U) .and. present(V) ) then
      call pk2f_subSVD ( A=A, U=U, S=S, V=V, saveA = .true. )
   else if ( present(U) ) then
      call pk2f_subSVD ( A=A, U=U, S=S     , saveA = .true. )
   else if ( present(V) ) then
      call pk2f_subSVD ( A=A, S=S, V=V     , saveA = .true. )
   else 
      call pk2f_subSVD ( A=A, S=S          , saveA = .true. )
   end if
   
   error_TraceNreturn(opflag, "pk2f_SVD") 
      
   END FUNCTION pk2f_SVD

         
!=============================================================================================    
   SUBROUTINE pk2f_subPolDec ( F, R, U ) 
!=============================================================================================       
   class(pk2_t),           intent(in    ) :: F
   class(pk2_t), optional, intent(in out) :: R, U
!---------------------------------------------------------------------------------------------   
!  Computes the Polar decomposition of F
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'pk2f_subPolDec'
   real     (Rkind), allocatable :: RmatR(:,:), UmatR(:,:)       
   complex  (Rkind), allocatable :: RmatC(:,:), UmatC(:,:)
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   select type (p=>F%m)
      type is ( rk2_t )
         if ( present(R) .and. present(U) ) then
            allocate(RmatR(F%nrow,F%ncol),UmatR(F%nrow,F%ncol))
            call util_PoldecR ( F = p%v, R = RmatR, U = UmatR, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = RmatR, to = R )
            call pk2_movealloc ( from = UmatR, to = U )
         else if ( present(R) ) then
            allocate(RmatR(F%nrow,F%ncol))         
            call util_PoldecR ( F = p%v, R = RmatR, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = RmatR, to = R )
         else if ( present(U) ) then
            allocate(UmatR(F%nrow,F%ncol))
            call util_PoldecR ( F = p%v, U = UmatR, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = UmatR, to = U )
         end if
      type is ( ck2_t )
         if ( present(R) .and. present(U) ) then
            allocate(RmatC(F%nrow,F%ncol),UmatC(F%nrow,F%ncol))
            call util_PoldecC ( F = p%v, R = RmatC, U = UmatC, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = RmatC, to = R )
            call pk2_movealloc ( from = UmatC, to = U )
         else if ( present(R) ) then
            allocate(RmatC(F%nrow,F%ncol))         
            call util_PoldecC ( F = p%v, R = RmatC, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = RmatC, to = R )
         else if ( present(U) ) then
            allocate(UmatC(F%nrow,F%ncol))         
            call util_PoldecC ( F = p%v, U = UmatC, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = UmatC, to = U )
         end if
      type is ( ik2_t )
         if ( present(R) .and. present(U) ) then
            allocate(RmatR(F%nrow,F%ncol),UmatR(F%nrow,F%ncol))
            call util_PoldecI ( F = p%v, R = RmatR, U = UmatR, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = RmatR, to = R )
            call pk2_movealloc ( from = UmatR, to = U )
         else if ( present(R) ) then
            allocate(RmatR(F%nrow,F%ncol))         
            call util_PoldecI ( F = p%v, R = RmatR, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = RmatR, to = R )
         else if ( present(U) ) then
            allocate(UmatR(F%nrow,F%ncol))
            call util_PoldecI ( F = p%v, U = UmatR, stat = opflag )
            error_TraceNreturn(opflag, HERE)
            call pk2_movealloc ( from = UmatR, to = U )
         end if
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =           &
                          'The matrix must be integer, real or complex' )    
         return
   end select
     
   END SUBROUTINE pk2f_subPolDec
   

!=============================================================================================    
   FUNCTION pk2f_PolDec ( F, U ) result ( R ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in    ) :: F
   class(pk2_t), optional, intent(in out) :: U
   type (pk2_t)                           :: R
!---------------------------------------------------------------------------------------------   
!  Computes the Polar decomposition of F
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------
   type(str_t) :: h(11), s
!---------------------------------------------------------------------------------------------            
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( .not. present(F) ) then
!
!-    return in "S" some helps (for calmat) and exit:
!     
      s = str_color('poldec',pk2f_helpcol)
      h( 1) = "<< "+s+" >> returns the polar decomposition of a real or a complex square matrix"
      h( 2) = " "
      h( 3) = "Syntax: R = "+s+"(F)  or  [R,U] = "+s+"(F)"
      h( 4) = " "            
      h( 5) = " . R: a unitary (resp. orthogonal if F is real) matrix"
      h( 6) = " . U: a positive semi-definite Hermitian (resp. symmetric) matrix"
      h( 7) = " "           
      h( 8) = "   with F = R * U"      
      h( 9) = " "           
      h(10) = "(Note: lapack routines [s/d]gesvd or [c/z]gesvd are used)"
      h(11) = " "           
      R = h
      return
   end if      
   
   if ( present(U) ) then
      call pk2f_subPolDec ( F=F, R=R, U=U ) 
   else 
      call pk2f_subPolDec ( F=F, R=R ) 
   end if
   
   error_TraceNreturn(opflag, "pk2f_PolDec") 
      
   END FUNCTION pk2f_PolDec


!=============================================================================================    
   SUBROUTINE pk2f_subDET ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Computes the determinant of a
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'pk2f_subDET'          
   type     (pk2_t)            :: tmp   
!---------------------------------------------------------------------------------------------          

   if ( opflag%code > IZERO ) return !!call opflag%set ()
            
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      res = IONE
      call opflag%set ( stat = WARNING, where = HERE, msg =                       &
                        '<< b = det(a) >> with << a >> non-allocated (--> b = 1)' )
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         call bk2_Detmats ( I = p%v, res = tmp%m )
      type is (rk2_t)
         call bk2_Detmats ( R = p%v, res = tmp%m )
      type is (ck2_t)
         call bk2_Detmats ( C = p%v, res = tmp%m )
         
      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                  &
                  'in << det(a) >> "a" must be integer, real or complex'//NL// &
                  '    (here, "a" is an array of booleans)' ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                  &
                  'in << det(a) >> "a" must be integer, real or complex'//NL// &
                  '    (here, "a" is an array of strings)' ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                  &
                  'in << det(a) >> "a" must be integer, real or complex'//NL// &
                  '    (here, "a" is an array of unknown type)' ) ; return
   end select

   error_TraceNreturn(opflag, HERE)

   tmp%typ = tmp%m%typ ; tmp%nrow = tmp%m%nrow ; tmp%ncol = tmp%m%ncol ; 
   
   call pk2_moveAlloc ( from = tmp, to = res )
               
   END SUBROUTINE pk2f_subDET
   
!=============================================================================================    
   FUNCTION pk2f_DET ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(6), f
!---------------------------------------------------------------------------------------------          

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('det',pk2f_helpcol)
      h(1) = "<< "+f+" >> computes the determinant of a square matrix"
      h(2) = " "
      h(3) = "Syntax: d = "+f+"(A)"
      h(4) = " "
      h(5) = "(Note: lapack routines dgetrf/zgetrf are used)"
      h(6) = " "
      call pk2_assign ( res, h )
      return
   end if   
            
   call pk2f_subDET ( a, res ) ;  error_TraceNreturn(opflag, "pk2f_DET")
              
   END FUNCTION pk2f_DET
   

!=============================================================================================   
   SUBROUTINE pk2f_subCROSS ( a, b, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a, b
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------       
!  Computes the cross product of a and b
!  a and b must have the same shape and must be
!  . 3-vectors (i.e. 3 x 1 or 1 x 3 matrices)
!  . or 3 x m matrices
!-----------------------------------------------------------------------------------R.H. 04/18       

!- local variables ---------------------------------------------------------------------------     
   character(len=*), parameter :: HERE = 'pk2f_subCROSS'          
   integer  (Ikind)            :: n, m
   type     (pk2_t)            :: tmp
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ > CTYP .or. b%typ > CTYP .or. a%typ == EMPTY .or. b%typ == EMPTY .or. &
        .not. allocated(a%m) .or. .not. allocated(b%m) ) then
      call opflag%set ( stat = UERROR, where = HERE, msg =                        &
       "<< a >> and << b >> in << cross(a,b) >> must be integer, real or complex" )
      return
   end if   
      
   n = a%nrow ; m = a%ncol
   if ( n /= b%nrow .or. m /= b%ncol ) then
      call opflag%set ( stat = UERROR, where = HERE, msg =                &
       "<< a >> and << b >> in << cross(a,b) >> must have the same shape" )
      return
   end if

   if ( n * m /= 3 .and. n /= 3 ) then
      call opflag%set ( stat = UERROR, where = HERE, msg =                              &   
       "<< a >> and << b >> in << cross(a,b) >> must be 1 x 3, 3 x 1 or 3 x n matrices" )
      return
   end if
   
   select type (p=>a%m)
   
      type is (ik2_t)
         select type (q=>b%m)
            type is (ik2_t)
               call bk2_crossIJRC (I = p%v, J = q%v, n = n, m = m, res = tmp%m)
            type is (rk2_t)
               call bk2_crossIJRC (I = p%v, R = q%v, n = n, m = m, res = tmp%m)
            type is (ck2_t)
               call bk2_crossIJRC (I = p%v, C = q%v, n = n, m = m, res = tmp%m)
         end select      
               
      type is (rk2_t)
         select type (q=>b%m)
            type is (ik2_t)
               call bk2_crossRISC (R = p%v, I = q%v, n = n, m = m, res = tmp%m)
            type is (rk2_t)
               call bk2_crossRISC (R = p%v, S = q%v, n = n, m = m, res = tmp%m)
            type is (ck2_t)
               call bk2_crossRISC (R = p%v, C = q%v, n = n, m = m, res = tmp%m)
         end select      
  
      type is (ck2_t)
         select type (q=>b%m)
            type is (ik2_t)
               call bk2_crossCIRD (C = p%v, I = q%v, n = n, m = m, res = tmp%m)
            type is (rk2_t)
               call bk2_crossCIRD (C = p%v, R = q%v, n = n, m = m, res = tmp%m)
            type is (ck2_t)
               call bk2_crossCIRD (C = p%v, D = q%v, n = n, m = m, res = tmp%m)
         end select      

   end select
   
   error_TraceNreturn(opflag, HERE)   
   
   tmp%nrow = tmp%m%nrow ; tmp%ncol = tmp%m%ncol ; tmp%typ = tmp%m%typ
   
   call pk2_moveAlloc ( from = tmp, to = res )

   END SUBROUTINE pk2f_subCROSS
   

!=============================================================================================   
   FUNCTION pk2f_CROSS ( a, b ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a, b
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------            

!- local variables ---------------------------------------------------------------------------     
   type(str_t) :: h(9), f
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) .and. .not. present(b) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('cross',pk2f_helpcol)
      h(1) = "<< "+f+" >> computes the cross product"
      h(2) = " "
      h(3) = "Syntax: w = "+f+"(u,v), C = "+f+"(A,B)"
      h(4) = " "
      h(5) = " . w = "+f+"(u,v): u and v are two vectors of the same shape and of"
      h(6) = "                   length 3. The result is a vector of the same shape."
      h(7) = " . C = "+f+"(A,B): A and B are two matrices of the same shape with 3"
      h(8) = "                   rows. The result is a matrix of the same shape."
      h(9) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   if ( .not. present(a) .or. .not. present(b) ) then
      call opflag%set ( stat = UERROR, where = "pk2f_CROSS",     &
                         msg = "<< cross >> needs two arguments" )
      return
   end if   
      
   call pk2f_subCROSS ( a, b, res ) ; error_TraceNreturn(opflag, "pk2f_CROSS")   

   END FUNCTION pk2f_CROSS


!=============================================================================================   
   SUBROUTINE pk2f_subCOLONr ( x1, x2, res, step )
!=============================================================================================     
   real (Rkind),           intent(in    ) :: x1, x2
   class(pk2_t),           intent(   out) :: res
   real (Rkind), optional, intent(in    ) :: step
!--------------------------------------------------------------------------------------------- 
!  Returns the linearly spaced n-vector (1 x n) "x1 : step : x2"   
!  If the step is not present, consider a step of 1.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------           
   integer(Ikind) :: n, i
   real   (Rkind) :: s
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( present(step) ) then
      s = step
   else
      s = RONE
   end if         
                     
   if ( s == 0 ) then
      n = 0
   else         
      n = floor((x2 - x1)/s) + 1
   end if
   
   if ( n > 0 ) then      
      res = reshape([(i,i=0,n-1)] * s + x1,[IONE,n])
   else      
      call opflag%set ( stat = WARNING, where = "pk2f_subCOLONr",           &
                         msg = "operation << d = a:c:b >> is empty (d=[Ê])" ) 
   end if   

   END SUBROUTINE pk2f_subCOLONr


!=============================================================================================   
   FUNCTION pk2f_COLONr ( x1, x2, step ) result ( res )
!=============================================================================================     
   real(Rkind),           intent(in) :: x1, x2
   real(Rkind), optional, intent(in) :: step
   type(pk2_t)                       :: res
!--------------------------------------------------------------------------------------------- 

   call pk2f_subCOLONr (x1, x2, res, step) ; error_TraceNreturn(opflag, "pk2f_COLONr")
   
   END FUNCTION pk2f_COLONr


!=============================================================================================   
   SUBROUTINE pk2f_subCOLONi ( x1, x2, res, step )
!=============================================================================================     
   integer(Ikind),           intent(in    ) :: x1, x2
   class  (pk2_t),           intent(   out) :: res   
   integer(Ikind), optional, intent(in    ) :: step
!--------------------------------------------------------------------------------------------- 
!  Returns the linearly spaced n-vector (1 x n) "x1 : step : x2"   
!  If the step is not present, consider a step of 1.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------           
   integer(Ikind) :: n, i, s
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( present(step) ) then
      s = step
   else
      s = IONE
   end if         
                     
   if ( s == 0 ) then
      n = 0
   else        
      n = (x2 - x1)/s + 1
   end if
   
   if ( n > 0 ) then      
      res = reshape(int([(i,i=0,n-1)] * s + x1 , kind=Ikind ),[IONE,n])
   else      
      call opflag%set ( stat = WARNING, where = "pk2f_subCOLONi",           &
                         msg = "operation << d = a:c:b >> is empty (d=[Ê])" ) 
   end if   

   END SUBROUTINE pk2f_subCOLONi


!=============================================================================================   
   FUNCTION pk2f_COLONi ( x1, x2, step ) result ( res )
!=============================================================================================     
   integer(Ikind),           intent(in) :: x1, x2
   integer(Ikind), optional, intent(in) :: step
   type   (pk2_t)                       :: res
!--------------------------------------------------------------------------------------------- 

   call pk2f_subCOLONi (x1, x2, res, step) ; error_TraceNreturn(opflag, "pk2f_COLONi")
   
   END FUNCTION  pk2f_COLONi


!=============================================================================================   
   SUBROUTINE pk2f_subCOLON ( a, b, res, c ) 
!=============================================================================================     
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: c
!--------------------------------------------------------------------------------------------- 
!  Returns the linearly spaced n-vector (1 x n) "d = a:c:b"   
!  The step is given by c (if not present, consider a step of 1)
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2f_subCOLON'      
   integer  (Ikind)            :: cnrow, cncol, ctype, restype, n, i, err
   real     (Rkind)            :: x1, step, x2
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( .not. allocated(a%m) .or. .not. allocated(b%m) ) then
      call opflag%set (stat = UERROR, where = HERE, msg = "Invalid data for << : >>")
      return
   end if   
       
   err = 0   
!
!- Get the step ("step"):
!   
   if ( present(c) ) then
      cnrow = c%nrow ; cncol = c%ncol ; ctype = c%typ
      if ( allocated(c%m) ) then
         select type (p=>c%m)
            type is (ik2_t)
               if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >=1 ) then
                  step = real(p%v(1,1),kind=Rkind)
               else
                  step = RONE
               end if      
            type is (rk2_t)
               if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >=1 ) then
                  step = p%v(1,1)
               else
                  step = RONE
               end if      
            class default
               err = 2
         end select
      else
         cnrow = 1 ; cncol = 1; ctype = ITYP  ; step = RONE  ! if empty set step = 1
      end if
   else
      cnrow = 1 ; cncol = 1; ctype = ITYP  ; step = RONE   ! if not present set step = 1
   end if          

   if ( cnrow /= 1 .or. cncol /= 1 ) then
      call opflag%set (stat=UERROR, where=HERE, msg="Arguments of << : >> must be scalars")
      return
   end if
            
   if ( err == 2 ) then
      call opflag%set(stat=UERROR,where=HERE,msg="Arguments of << : >> must be integer or real") 
      return
   end if   
!
!- Get the first bound ("x1"):
!      
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            x1 = real(p%v(1,1), kind=Rkind)
         else
            err = 1
         end if
      type is (rk2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            x1 = p%v(1,1)
         else
            err = 1
         end if
      class default
         err = 2   
   end select      

   if ( err == 1 .or. a%nrow /= 1 .or. a%ncol /= 1 ) then
      call opflag%set (stat=UERROR, where=HERE, msg="Arguments of << : >> must be scalars")
      return
   end if
            
   if ( err == 2 ) then
      call opflag%set(stat=UERROR,where=HERE,msg="Arguments of << : >> must be integer or real") 
      return      
   end if   
!
!- Get the second bound ("x2"):
!
   select type (p=>b%m)
      type is (ik2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            x2 = real(p%v(1,1), kind=Rkind)
         else
            err = 1
         end if
      type is (rk2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            x2 = p%v(1,1)
         else
            err = 1
         end if
      class default
         err = 2     
   end select      
   
   if ( err == 1 .or. b%nrow /= 1 .or. b%ncol /= 1 ) then
      call opflag%set (stat=UERROR, where=HERE, msg="Arguments of << : >> must be scalars")
      return
   end if
            
   if ( err == 2 ) then
      call opflag%set(stat=UERROR,where=HERE,msg="Arguments of << : >> must be integer or real") 
      return      
   end if   
         
!
!- Compute res:
!   
   restype = max(a%typ, b%typ, ctype)
   
   if ( step == 0 ) then
      n = 0
   else         
      n = floor((x2 - x1)/step) + 1
   end if
   
   if ( n > 0 ) then      
      if ( restype == ITYP ) then
         res = reshape(int([(i,i=0,n-1)] * step + x1 , kind=Ikind ),[IONE,n])
      else
         res = reshape([(i,i=0,n-1)] * step + x1,[IONE,n])
      end if
   else      
      call opflag%set ( stat = WARNING, where = HERE,                       &
                         msg = "Operation << d = a:c:b >> is empty (d=[Ê])" ) 
      res = pk2_t()
   end if   

   END SUBROUTINE pk2f_subCOLON


!=============================================================================================   
   FUNCTION pk2f_COLON ( a, b, c ) result ( res )
!=============================================================================================     
   class(pk2_t),           intent(in) :: a, b
   class(pk2_t), optional, intent(in) :: c
   type (pk2_t)                       :: res
!--------------------------------------------------------------------------------------------- 

   call pk2f_subCOLON ( a, b, res, c ) ; error_TraceNreturn(opflag, "pk2f_COLON")
   
   END FUNCTION pk2f_COLON


!=============================================================================================   
   SUBROUTINE pk2f_subCOLONwrp ( matrs, res )
!=============================================================================================     
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!--------------------------------------------------------------------------------------------- 
!  Returns the linearly spaced n-vector (1 x n) "d = a:c:b"   
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------           
   integer(Ikind) :: nmatrs
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nmatrs = size(matrs)

   if ( nmatrs == 2 ) then
      call pk2f_subCOLON ( a = matrs(1), b = matrs(2), res = res )
   else if ( nmatrs == 3 ) then
      call pk2f_subCOLON ( a = matrs(1), b = matrs(2), res = res, c = matrs(3) )
   else
      call opflag%set ( stat = UERROR, where = "pk2f_subCOLONwrp",     &
                         msg = "2 or 3 arguments expected for << : >>" )
      return
   end if

   error_TraceNreturn(opflag, "pk2f_subCOLONwrp")   
   
   END SUBROUTINE pk2f_subCOLONwrp


!=============================================================================================   
   FUNCTION pk2f_COLONwrp ( matrs ) result ( res )
!=============================================================================================     
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------           
   type   (str_t) :: h(23), f1, f2
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps (for calmat) and exit:
!      
      f1 = str_color('colon',pk2f_helpcol)
      f2 = str_color(':'    ,pk2f_helpcol)
      h( 1) = "<< "+f1+" >> or << "+f2+" >> "
      h( 2) = " "
      h( 3) = "Syntax:      x = "+f1+"(a,b), x = "+f1+"(a,b,step)  "
      h( 4) = "         or  x = a "+f2+" b,      x = a "+f2+" step "+f2+" b     "
      h( 5) = " "
      h( 6) = "Caution: The << "+f2+" >> has the lowest precedence, e.g. the expression"
      h( 7) = "                    a + b "+f2+" c + d "
      h( 8) = "         has to be understood as"
      h( 9) = "                  (a + b) "+f2+" (c + d)   and not as   a + (b"+f2+"c) + d"
      h(10) = " "
      h(11) = "         Moreover, for an expression involving several << "+f2+" >>, the use of"
      h(12) = "         parentheses is"
      h(13) = " "
      h(14) = "         . recommended, for example the expression"
      h(15) = "                a "+f2+" b + c "+f2+" d"
      h(16) = "           will be interpreted as"
      h(17) = "                a "+f2+" (b + c) "+f2+" d   and not as (a"+f2+"b) + (c"+f2+"d)"
      h(18) = " "
      h(19) = "         . mandatory, for example the expression"
      h(20) = "                (a "+f2+" b) + x "+f2+" y "+f2+" z   is not valid"
      h(21) = "           while"
      h(22) = "                (a "+f2+" b) + (x "+f2+" y "+f2+" z) is."
      h(23) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subCOLONwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_COLONwrp")   

   END FUNCTION pk2f_COLONwrp
      

!=============================================================================================   
   SUBROUTINE pk2f_subLINSPACEi ( x1, x2, res, n )
!=============================================================================================     
   integer(Ikind),           intent(in    ) :: x1, x2
   class  (pk2_t),           intent(   out) :: res   
   integer(Ikind), optional, intent(in    ) :: n
!--------------------------------------------------------------------------------------------- 
!  Returns the linearly spaced n-vector (1 x n)   
!  If the step is not present, consider n = 100
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------           
   integer(Ikind) :: np, i, istep
   real   (Rkind) :: rstep
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( present(n) ) then
      np = n
   else
      np = 100
   end if         

   if ( np > 0 ) then      
      if ( mod(x2-x1,np-1) == 0 ) then
         istep = (x2-x1)/(np-1)
         res = reshape( [(i,i=0,n-1)] * istep + x1, [IONE,n] )
      else
         rstep = real(x2-x1,Rkind) / real(np-1,Rkind)
         res = reshape( [(i,i=0,n-1)] * rstep + real(x1,Rkind), [IONE,n] )
      end if   
   else      
      call opflag%set ( stat = WARNING, where = "pk2f_subLINSPACEi",           &
                         msg = "operation << c = linspace(a,b,0) >> is empty (c=[Ê])" ) 
   end if   

   END SUBROUTINE pk2f_subLINSPACEi
   
   
!=============================================================================================   
   FUNCTION pk2f_LINSPACEi ( x1, x2, n ) result ( res )
!=============================================================================================     
   integer(Ikind),           intent(in) :: x1, x2
   integer(Ikind), optional, intent(in) :: n
   type   (pk2_t)                       :: res
!--------------------------------------------------------------------------------------------- 

   call pk2f_subLINSPACEi (x1, x2, res, n) ; error_TraceNreturn(opflag, "pk2f_LINSPACEi")
   
   END FUNCTION  pk2f_LINSPACEi
   
   
!=============================================================================================   
   SUBROUTINE pk2f_subLINSPACE ( a, b, res, c ) 
!=============================================================================================     
   class(pk2_t),           intent(in    ) :: a, b
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: c
!--------------------------------------------------------------------------------------------- 
!  Returns the linearly spaced n-vector (1 x n)
!  If c is not present, set n = 100
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2f_subLINSPACE'      
   integer  (Ikind)            :: cnrow, cncol, abtype, n, i, err
   integer  (Ikind)            :: i1, i2, istep
   real     (Rkind)            :: r1, r2, rstep
   complex  (Rkind)            :: c1, c2, cstep
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( .not. allocated(a%m) .or. .not. allocated(b%m) .or. .not. allocated(c%m)) then
      call opflag%set (stat = UERROR, where = HERE, msg = "Invalid data for << : >>")
      return
   end if   
       
   err = 0   
!
!- Get the number of points ("n"):
!   
   if ( present(c) ) then
      cnrow = c%nrow ; cncol = c%ncol 
      if ( allocated(c%m) ) then
         select type (p=>c%m)
            type is (ik2_t)
               if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >=1 ) then
                  n = p%v(1,1)
               else
                  n = 100
               end if      
            class default
               err = 2
         end select
      else
         cnrow = 1 ; cncol = 1 ; n = 100  ! if empty set npoin = 100
      end if
   else
      cnrow = 1 ; cncol = 1 ; n = 100   ! if not present set npoin = 100
   end if          

   if ( cnrow /= 1 .or. cncol /= 1 ) then
      call opflag%set ( stat = UERROR, where = HERE, &
                         msg = "The number of points must be scalar" )
      return
   end if
            
   if ( err == 2 ) then
      call opflag%set ( stat = UERROR, where = HERE, &
                          msg= "The number of points must be integer" ) 
      return
   end if  

   abtype = max(a%typ, b%typ)
!
!- Get the first bound:
!      
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            i1 = p%v(1,1)
            if ( abtype == RTYP ) then
               r1 = i1
            else
               c1 = i1
            end if
         else
            err = 1
         end if
      type is (rk2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            r1 = p%v(1,1)
            if ( abtype == CTYP ) c1 = r1
         else
            err = 1
         end if
      type is (ck2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            c1 = p%v(1,1)
         else
            err = 1
         end if         
      class default
         err = 2   
   end select      

   if ( err == 1 .or. a%nrow /= 1 .or. a%ncol /= 1 ) then
      call opflag%set ( stat = UERROR, where = HERE, &
                         msg = "Bounds in << linspace >> must be scalars" )
      return
   end if
            
   if ( err == 2 ) then
      call opflag%set ( stat = UERROR, where = HERE, &
                     msg = "Bounds in << linspace >> must be integers, reals or complexes" ) 
      return      
   end if   
!
!- Get the second bound:
!
   select type (p=>b%m)
      type is (ik2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            i2 = p%v(1,1)
            if ( abtype == RTYP ) then
               r2 = i2
            else if ( abtype == CTYP ) then
               c2 = i2
            end if
         else
            err = 1
         end if
      type is (rk2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            r2 = p%v(1,1)
            if ( abtype == CTYP ) c2 = r2
         else
            err = 1
         end if
      type is (ck2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            c2 = p%v(1,1)
         else
            err = 1
         end if
      class default
         err = 2     
   end select      
   
   if ( err == 1 .or. b%nrow /= 1 .or. b%ncol /= 1 ) then
      call opflag%set ( stat = UERROR, where = HERE, &
                         msg = "Bounds in << linspace >> must be scalars" )
      return
   end if
            
   if ( err == 2 ) then
      call opflag%set ( stat = UERROR, where = HERE, &
                     msg = "Bounds in << linspace >> must be integers, reals or complexes" ) 
      return      
   end if   

!
!- Compute res:
!      
   if ( n > 0 ) then    
      if ( abtype == ITYP ) then
         if ( mod(i2-i1,n-1) == 0 ) then
            istep = (i2-i1) / (n-1)
            res = reshape( [(i,i=0,n-1)] * istep + i1, [IONE,n] )
         else
            rstep = real(i2-i1,Rkind) / real(n-1,Rkind)
            res = reshape( [(i,i=0,n-1)] * rstep + real(i1,Rkind), [IONE,n] )
         end if
      else if ( abtype == RTYP ) then
         rstep = (r2-r1) / real(n-1,Rkind)
         res = reshape( [(i,i=0,n-1)] * rstep + r1, [IONE,n] )
      else if ( abtype == CTYP ) then
         cstep = (c2-c1) / real(n-1,Rkind)
         res = reshape( [(i,i=0,n-1)] * cstep + c1, [IONE,n] )
      end if
   else      
      call opflag%set ( stat = WARNING, where = HERE,                       &
                         msg = "Operation << c = linspace(a,b,0) >> is empty (c=[Ê])" ) 
      res = pk2_t()
   end if   

   END SUBROUTINE pk2f_subLINSPACE
   
   
!=============================================================================================   
   FUNCTION pk2f_LINSPACE ( a, b, c ) result ( res )
!=============================================================================================     
   class(pk2_t),           intent(in) :: a, b
   class(pk2_t), optional, intent(in) :: c
   type (pk2_t)                       :: res
!--------------------------------------------------------------------------------------------- 

   call pk2f_subLINSPACE ( a, b, res, c ) ; error_TraceNreturn(opflag, "pk2f_LINSPACE")
   
   END FUNCTION pk2f_LINSPACE
   
   
!=============================================================================================   
   SUBROUTINE pk2f_subLINSPACEwrp ( matrs, res )
!=============================================================================================     
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!--------------------------------------------------------------------------------------------- 
!  Returns the linearly spaced n-vector (1 x n)  
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind) :: nmatrs          
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nmatrs = size(matrs)
   
   if ( nmatrs == 3 ) then
      call pk2f_subLINSPACE ( a = matrs(1), b = matrs(2), c = matrs(3), res = res )
   else if ( nmatrs == 2 ) then
      call pk2f_subLINSPACE ( a = matrs(1), b = matrs(2), res = res )
   else
      call opflag%set ( stat = UERROR, where = "pk2f_subLINSPACEwrp",     &
                         msg = "2 or 3 arguments expected for << linspace >>" )
      return
   end if
   
   END SUBROUTINE pk2f_subLINSPACEwrp
   
   
!=============================================================================================   
   FUNCTION pk2f_LINSPACEwrp ( matrs ) result ( res )
!=============================================================================================     
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------           
   type   (str_t) :: h(8), f1
!---------------------------------------------------------------------------------------------      

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps (for calmat) and exit:
!      
      f1 = str_color('linspace',pk2f_helpcol)
      h( 1) = "<< "+f1+" >> returns a row vector of evenly spaced points"
      h( 2) = " "
      h( 3) = "Syntax:  x = "+f1+"(a,b,n)  "
      h( 4) = " "
      h( 5) = " . a, b: bounds of the interval (integers, reals or complexes)"
      h( 6) = " . n   : number of points (integer)"
      h( 7) = " . x   : set of the n points evenly spaced"
      h( 8) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subLINSPACEwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_LINSPACEwrp")   

   END FUNCTION pk2f_LINSPACEwrp
   
   
!=============================================================================================   
   FUNCTION pk2f_EXTRACMAT0 ( a, vindx, vjndx ) result ( res )
!=============================================================================================     
   class(pk2_t),           intent(in) :: a
   class(pk2_t), optional, intent(in) :: vindx, vjndx
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Extracts from the matrix "a" the sub-matrix corresponding to the set of indices given in 
!  vindx and vjndx. These indices can be integers or booleans.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   character(len=* ), parameter   :: HERE = 'pk2f_EXTRACMAT'  
   integer  (Ikind ), allocatable :: tmpi(:), indx(:), jndx(:)
   real     (Rkind ), allocatable :: tmpr(:)
   complex  (Rkind ), allocatable :: tmpc(:)
   logical          , allocatable :: tmpl(:)
   type     (str_t ), allocatable :: tmps(:)
   integer  (Ikind )              :: ni, nj, na, shape(2), i, j, k, err
   character(len=99)              :: emsg
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
               '<< b = extrac(a,Indx,Jndx) >> with << a >> empty (--> b = [Ê])' )
      return
   end if   
               
   if ( present(vindx) ) then
   
      if ( vindx%typ  == EMPTY .or. .not. allocated(vindx%m) .or. &
           vindx%nrow == 0     .or. vindx%ncol == 0               ) then
         call opflag%set ( stat = WARNING, where = HERE, msg = &
                  '<< b = extrac(a,Indx,Jndx) >> with << Indx >> empty (--> b = [Ê])') 
         return
      end if   
      
      ni = 0
      select type (p=>vindx%m)
      
         type is (ik2_t)
            if ( allocated(p%v) ) then
               ni = size(p%v)
               allocate( indx(ni), source = pack(p%v, mask = .true.), stat = err )
               if ( err /= 0 ) then
                  call opflag%set(stat=IERROR, where=HERE, msg='Allocation failure for indx')   
                  return
               end if                              
            end if   
            
         type is (lk2_t)
            if ( allocated(p%v) ) then         
               ni = count(p%v) ; allocate( indx(ni) )
               k = 0 ; ni = 0
               do j = 1, vindx%ncol
                  do i = 1, vindx%nrow
                     k = k + 1
                     if ( p%v(i,j) ) then
                        ni = ni + 1 ; indx(ni) = k
                     end if   
                  end do
               end do  
            end if          
               
         class default
            call opflag%set ( stat = UERROR, where = HERE, msg = &
                          'indices must be integer or boolean (message from pk2f_extracmat)' )
            return
      end select      
            
      if ( present(vjndx) ) then  
!
!-       cas du type A(indx,jndx)
!
         if ( vjndx%typ  == EMPTY .or. .not. allocated(vjndx%m) .or. &
              vjndx%nrow == 0     .or. vjndx%ncol == 0               ) then
            call opflag%set ( stat = WARNING, where = HERE, msg = &
                     '<< b = extrac(a,Indx,Jndx) >> with << Jndx >> empty (--> b = [ ])' )
            return
         end if   
      
         nj = 0
         select type(p=>vjndx%m)
         
            type is (ik2_t)
               if ( allocated(p%v) ) then
                  nj = size(p%v)
                  allocate( jndx(nj), source = pack(p%v, mask = .true.), stat = err )
                  if ( err /= 0 ) then
                     call opflag%set(stat=IERROR, where=HERE,msg='Allocation failure for jndx')   
                     return
                  end if                                       
               end if   

            type is (lk2_t)
               if ( allocated(p%v) ) then         
                  nj = count(p%v) ; allocate( jndx(nj) )
                  k = 0 ; nj = 0
                  do j = 1, vjndx%ncol
                     do i = 1, vjndx%nrow
                        k = k + 1
                        if ( p%v(i,j) ) then
                           nj = nj + 1 ; jndx(nj) = k
                        end if   
                     end do
                  end do  
               end if      
                           
            class default
               call opflag%set ( stat = UERROR, where = HERE,               &
                                  msg ='indices must be integer or boolean' )
               return
         end select      

         if ( ni == 1 .and. indx(1) == huge(1_Ikind) ) then 

            if ( nj == 1 .and. jndx(1) == huge(1_Ikind) ) then 
!
!-             cas du type A(:,:)         
!            
               select type (p=>a%m)
                  type is (ik2_t) ; res = p%v(1:a%nrow,1:a%ncol)
                  type is (rk2_t) ; res = p%v(1:a%nrow,1:a%ncol)
                  type is (ck2_t) ; res = p%v(1:a%nrow,1:a%ncol)
                  type is (lk2_t) ; res = p%v(1:a%nrow,1:a%ncol)
                  type is (sk2_t) ; res = p%v(1:a%nrow,1:a%ncol)
               end select      
            else  
!
!-             cas du type A(:,jndx)
!
               if ( minval(jndx) < 1 ) then
                  write(emsg,'(a,i0,a)') &
                  "index '",minval(jndx),"' of dimension #2 below lower bound of 1"                                     
                  call opflag%set (stat = UERROR, where = HERE, msg = emsg)
                  return
               end if
                  
               if ( maxval(jndx) > a%ncol ) then
                  write(emsg,'(a,i0,a,i0)') &
                  "index '",maxval(jndx),"' of dimension #2 above upper bound of ",a%ncol
                  call opflag%set (stat = UERROR, where = HERE, msg = emsg)
                  return
               end if
                              
               select type (p=>a%m)
                  type is (ik2_t) ; res = p%v(1:a%nrow,jndx)
                  type is (rk2_t) ; res = p%v(1:a%nrow,jndx)
                  type is (ck2_t) ; res = p%v(1:a%nrow,jndx)
                  type is (lk2_t) ; res = p%v(1:a%nrow,jndx)
                  type is (sk2_t) ; res = p%v(1:a%nrow,jndx)
               end select                     
            end if         
         else
        
            if ( nj == 1 .and. jndx(1) == huge(1_Ikind) ) then 
!
!-             cas du type A(indx,:)
!                  
               if ( minval(indx) < 1 ) then
                  write(emsg,'(a,i0,a)') &
                  "index '",minval(indx),"' of dimension #1 below lower bound of 1"
                  call opflag%set (stat = UERROR, where = HERE, msg = emsg)
                  return
               end if
               if ( maxval(indx) > a%nrow ) then
                  write(emsg,'(a,i0,a,i0)') &
                  "index '",maxval(indx),"' of dimension #1 above upper bound of ",a%nrow
                  call opflag%set (stat = UERROR, where = HERE, msg = emsg)
                  return
               end if
                              
               select type (p=>a%m)
                  type is (ik2_t) ; res = p%v(indx,1:a%ncol)
                  type is (rk2_t) ; res = p%v(indx,1:a%ncol)
                  type is (ck2_t) ; res = p%v(indx,1:a%ncol)
                  type is (lk2_t) ; res = p%v(indx,1:a%ncol)
                  type is (sk2_t) ; res = p%v(indx,1:a%ncol)
               end select                     
               
            else
!
!-             cas du type A(indx,jndx)  
!
               if ( minval(jndx) < 1 ) then
                  write(emsg,'(a,i0,a)') &
                  "index '",minval(jndx),"' of dimension #2 below lower bound of 1"
                  call opflag%set (stat = UERROR, where = HERE, msg = emsg)
                  return
               end if
               if ( maxval(jndx) > a%ncol ) then
                  write(emsg,'(a,i0,a,i0)') &
                  "index '",maxval(jndx),"' of dimension #2 above upper bound of ",a%ncol
                  call opflag%set (stat = UERROR, where = HERE, msg = emsg)
                  return
               end if

               if ( minval(indx) < 1 ) then
                  write(emsg,'(a,i0,a)') &
                  "index '",minval(indx),"' of dimension #1 below lower bound of 1 "
                  call opflag%set (stat = UERROR, where = HERE, msg = emsg)
                  return
               end if
               if ( maxval(indx) > a%nrow ) then
                  write(emsg,'(a,i0,a,i0)') &
                  "index '",maxval(indx),"' of dimension #1 above upper bound of ",a%nrow
                  call opflag%set (stat = UERROR, where = HERE, msg = emsg)
                  return
               end if
               
               select type (p=>a%m)
                  type is (ik2_t) ; res = p%v(indx,jndx)
                  type is (rk2_t) ; res = p%v(indx,jndx)
                  type is (ck2_t) ; res = p%v(indx,jndx)
                  type is (lk2_t) ; res = p%v(indx,jndx)
                  type is (sk2_t) ; res = p%v(indx,jndx)
               end select  
                                  
            end if
         end if     
                         
         deallocate(jndx) ! inutile
         
      else   
!
!-       cas du type A(indx)
!         
         na = a%nrow * a%ncol
             
         if ( ni == 1 .and. indx(1) == huge(1_Ikind) ) then 
!
!-          cas du type A(:)
!
            shape = [na,IONE]

            select type (p=>a%m)
               type is (ik2_t) 
                  res = reshape(p%v,shape) 
               type is (rk2_t) 
                  res = reshape(p%v,shape) 
               type is (ck2_t) 
                  res = reshape(p%v,shape) 
               type is (lk2_t) 
                  res = reshape(p%v,shape) 
               type is (sk2_t)
                  res = reshape(p%v,shape)
            end select       
                
         else  
!         
!-          cas du type A(indx)
!
            if ( minval(indx) < 1 ) then
               write(emsg,'(a,i0,a)') &
               "index '",minval(indx),"' below lower bound of 1"
               call opflag%set (stat = UERROR, where = HERE, msg = emsg)
               return
            end if

            if ( maxval(indx) > na ) then
               write(emsg,'(a,i0,a,i0)') &
               "index '",maxval(indx),"' of dimension #1 above upper bound of ",na
               call opflag%set (stat = UERROR, where = HERE, msg = emsg)
               return
            end if
            
            if ( a%nrow == 1 ) then
               shape = [IONE,ni]
            else
               shape = [ni,IONE]
            end if
               
            select type (p=>a%m)
               type is (ik2_t)
                  tmpi = pack(p%v, mask = .true.) ; res = reshape(tmpi(indx),shape)
               type is (rk2_t)
                  tmpr = pack(p%v, mask = .true.) ; res = reshape(tmpr(indx),shape)                   
               type is (ck2_t)
                  tmpc = pack(p%v, mask = .true.) ; res = reshape(tmpc(indx),shape)
               type is (lk2_t)
                  tmpl = pack(p%v, mask = .true.) ; res = reshape(tmpl(indx),shape)
               type is (sk2_t)
                  tmps = pack(p%v, mask = .true.) ; res = reshape(tmps(indx),shape)                    
            end select           
               
         end if   
         
      end if      
      
      deallocate(indx) ! inutile
   
   else
!
!-    vindx (and vjndx) is not present <=> res = a:
!   
      res = a    
   end if               

   END FUNCTION pk2f_EXTRACMAT0
 

!=============================================================================================   
   SUBROUTINE pk2f_subEXTRACMAT ( a, res, vindx, vjndx )
!=============================================================================================     
   class(pk2_t),           intent(in    ) :: a
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: vindx, vjndx
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  vindx and vjndx. These indices can be integers or booleans.
!
!  If "vindx" and "vjndx" are not present: 
!
!                  res = a
!
!  If "vindx" and "vjndx" are present: 
!
!                  res%m%v(i,j) = a%m%v(vindx(i),vjndx(j))
!
!  If "vindx" is present and not "vjndx": "res" is an 1 x ni or ni x 1 array (ni=size(vindx))
!  reshaped from the column-major ordering of a:
!
!            if size(a,1) == 1: size(res) = shape = [1,ni]
!            else             : size(res) = shape = [ni,1]
!
!            res%m%v(i,j) = reshape(tmp(vindx),shape)  with  tmp = pack(a%m%v)
!
!----------------------------------------------------------------------------R.H. 04/18, 12/18

!- local variables ---------------------------------------------------------------------------      
   character(len=*), parameter   :: HERE = 'pk2f_subEXTRACMAT'  
   integer  (Ikind), allocatable :: indx(:), jndx(:)
   integer  (Ikind)              :: ni, nj, i, j, k, err
   type     (pk2_t)              :: tmp
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      !opflag = err_t ( stat = WARNING, msg = &
      !         '<< b = extrac(a,Indx,Jndx) >> with << a >> empty (--> b = [Ê])' )
      res = pk2_t()  ! a = [Ê] => res = [Ê]
      return
   end if   
               
   if ( present(vindx) ) then
   
      if ( vindx%typ  == EMPTY .or. .not. allocated(vindx%m) .or. &
           vindx%nrow == 0     .or. vindx%ncol == 0               ) then
         !opflag = err_t ( stat = WARNING, msg = &
         !         '<< b = extrac(a,Indx,Jndx) >> with << Indx >> empty (--> b = [Ê])') 
         res = pk2_t() ! a([Ê]) => res = [Ê]
         return
      end if   
      
      ni = 0
      select type (p=>vindx%m)
      
         type is (ik2_t)
            if ( allocated(p%v) ) then
               ni = size(p%v)
               allocate( indx(ni), source = pack(p%v, mask = .true.), stat = err )
               if ( err /= 0 ) then
                  call opflag%Set(IERROR, HERE, 'Allocation failure for indx')  
                  return 
               end if                              
            end if   
            
         type is (lk2_t)
            if ( allocated(p%v) ) then         
               ni = count(p%v) ; allocate( indx(ni) )
               k = 0 ; ni = 0
               do j = 1, vindx%ncol
                  do i = 1, vindx%nrow
                     k = k + 1
                     if ( p%v(i,j) ) then
                        ni = ni + 1 ; indx(ni) = k
                     end if   
                  end do
               end do  
            end if          

         type is (rk2_t)
            if ( allocated(p%v) ) then         
               ni = size(p%v) ; allocate( indx(ni) )
               k = 0
               do j = 1, vindx%ncol
                  do i = 1, vindx%nrow
                     k = k + 1
                     indx(k) = int(p%v(i,j),kind=Ikind)
                  end do
               end do  
            end if   
               
         class default
            call opflag%Set(UERROR, HERE, 'Indices must be integer or boolean')
            return
      end select      
            
      if ( present(vjndx) ) then        
!
!-       cas du type A(indx,jndx)
!
         if ( vjndx%typ  == EMPTY .or. .not. allocated(vjndx%m) .or. &
              vjndx%nrow == 0     .or. vjndx%ncol == 0               ) then
            !opflag = err_t ( stat = WARNING, msg = &
            !         '<< b = extrac(a,Indx,Jndx) >> with << Jndx >> empty (--> b = [ ])' )
            res = pk2_t() ! a(i,[Ê]) => res = [Ê]
            return
         end if   
      
         nj = 0
         select type(p=>vjndx%m)
         
            type is (ik2_t)
               if ( allocated(p%v) ) then
                  nj = size(p%v)
                  allocate( jndx(nj), source = pack(p%v, mask = .true.), stat = err )
                  if ( err /= 0 ) then
                     call opflag%Set(IERROR, HERE, 'Allocation failure for jndx') 
                     return 
                  end if                                       
               end if   

            type is (lk2_t)
               if ( allocated(p%v) ) then         
                  nj = count(p%v) ; allocate( jndx(nj) )
                  k = 0 ; nj = 0
                  do j = 1, vjndx%ncol
                     do i = 1, vjndx%nrow
                        k = k + 1
                        if ( p%v(i,j) ) then
                           nj = nj + 1 ; jndx(nj) = k
                        end if   
                     end do
                  end do  
               end if      

            type is (rk2_t)
               if ( allocated(p%v) ) then         
                  nj = size(p%v) ; allocate( jndx(nj) )
                  k = 0
                  do j = 1, vjndx%ncol
                     do i = 1, vjndx%nrow
                        k = k + 1
                        jndx(k) = int(p%v(i,j),kind=Ikind)
                     end do
                  end do  
               end if     
                                          
            class default
               call opflag%Set(UERROR, HERE, 'Indices must be integer or boolean')
               return
         end select      

         call tmp%ExtracSubMat ( a, indx, jndx ) 
         
      else   
      
         call tmp%ExtracSubMat ( a, indx ) 
      
      end if      
         
   else
!
!-    vindx (and vjndx) is not present <=> res = a:
!   
      tmp = a    
      if ( allocated(a%name) ) tmp%name = 'copy of "'//trim(a%name)//'"'
   end if            
   
   error_TraceNreturn(opflag, HERE)  
    
   call pk2_moveAlloc ( from = tmp, to = res )

   END SUBROUTINE pk2f_subEXTRACMAT
 

!=============================================================================================   
   FUNCTION pk2f_EXTRACMAT ( a, vindx, vjndx ) result ( res )
!=============================================================================================     
   class(pk2_t),           intent(in) :: a
   class(pk2_t), optional, intent(in) :: vindx, vjndx
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subEXTRACMAT ( a, res, vindx, vjndx )
   
   error_TraceNreturn(opflag, 'pk2f_EXTRACMAT')
   
   END FUNCTION pk2f_EXTRACMAT
 

!=============================================================================================   
   SUBROUTINE pk2f_subEXTRACMATi ( a, indx, res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   integer(Ikind), intent(in    ) :: indx (:)
   class  (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in indx
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   type(pk2_t) :: tmp
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATi', msg = &
                '<< b = extrac(a,Indx) >> with << a >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if   

   if ( size(indx) == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATi', msg =    &
                '<< b = extrac(a,Indx) >> with << Indx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if      
    
   call tmp%ExtracSubMat ( a, indx ) 
   
   error_TraceNreturn(opflag, 'pk2f_subEXTRACMATi')
   
   call pk2_moveAlloc ( from = tmp, to = res )
   
   END SUBROUTINE pk2f_subEXTRACMATi


!=============================================================================================   
   FUNCTION pk2f_EXTRACMATi ( a, indx ) result ( res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   integer(Ikind), intent(in    ) :: indx (:)
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subEXTRACMATi ( a, indx, res ) ; error_TraceNreturn(opflag, 'pk2f_EXTRACMATi')
   
   END FUNCTION pk2f_EXTRACMATi
   
   
!=============================================================================================   
   SUBROUTINE pk2f_subEXTRACMATij ( a, indx, jndx, res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   integer(Ikind), intent(in    ) :: indx (:)
   integer(Ikind), intent(in    ) :: jndx (:)
   class  (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  indx (integers) and jndx (integers).
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   type(pk2_t) :: tmp
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATij', msg =     &
                '<< b = extrac(a,Indx,Jndx) >> with << a >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if   

   if ( size(indx) == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATij', msg =        &
                '<< b = extrac(a,Indx,Jndx) >> with << Indx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if      

   if ( size(jndx) == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATij', msg =        &
                '<< b = extrac(a,Indx,Jndx) >> with << Jndx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if             
   
   call tmp%ExtracSubMat ( a, indx, jndx ) 
   
   error_TraceNreturn(opflag, 'pk2f_subEXTRACMATij')
   
   call pk2_moveAlloc ( from = tmp, to = res )
   
   END SUBROUTINE pk2f_subEXTRACMATij


!=============================================================================================   
   FUNCTION pk2f_EXTRACMATij ( a, indx, jndx ) result ( res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   integer(Ikind), intent(in    ) :: indx (:)
   integer(Ikind), intent(in    ) :: jndx (:)
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subEXTRACMATij ( a, indx, jndx, res )
   
   error_TraceNreturn(opflag, 'pk2f_EXTRACMATij')
   
   END FUNCTION pk2f_EXTRACMATij


!=============================================================================================   
   SUBROUTINE pk2f_subEXTRACMATil ( a, indx, ljndx, res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   integer(Ikind), intent(in    ) :: indx (:)
   logical       , intent(in    ) :: ljndx (:)
   class  (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  indx (integers) and ljndx (booleans).
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   integer(Ikind)              :: nj, j, k, err
   integer(Ikind), allocatable :: jndx(:)
   type   (pk2_t)              :: tmp
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATil', msg =     &
                '<< b = extrac(a,Indx,Jndx) >> with << a >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if   

   if ( size(indx) == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATil', msg =        &
                '<< b = extrac(a,Indx,Jndx) >> with << Indx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if             
      
   nj = count(ljndx)
   
   if ( nj == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATil', msg =        &
                '<< b = extrac(a,Indx,Jndx) >> with << Jndx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if    
   
   allocate( jndx(nj), stat = err )
   
   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = 'pk2f_subEXTRACMATil', &
                         msg = 'Allocation failure'                   )
      res = pk2_t()
      return
   end if    
   
   
   k = 0 ; nj = 0
   do j = 1, size(ljndx)
      k = k + 1
      if ( ljndx(j) ) then
         nj = nj + 1 ; jndx(nj) = k
      end if   
   end do
    
   call tmp%ExtracSubMat ( a, indx, jndx ) 
   
   error_TraceNreturn(opflag, 'pk2f_subEXTRACMATil')
   
   call pk2_moveAlloc ( from = tmp, to = res )
   
   END SUBROUTINE pk2f_subEXTRACMATil


!=============================================================================================   
   FUNCTION pk2f_EXTRACMATil ( a, indx, ljndx ) result ( res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   integer(Ikind), intent(in    ) :: indx (:)
   logical       , intent(in    ) :: ljndx (:)
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subEXTRACMATil ( a, indx, ljndx, res )

   error_TraceNreturn(opflag, 'pk2f_EXTRACMATil')
   
   END FUNCTION pk2f_EXTRACMATil
   

!=============================================================================================   
   SUBROUTINE pk2f_subEXTRACMATl ( a, lindx, res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   logical       , intent(in    ) :: lindx (:)
   class  (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  lindx (booleans).
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   integer(Ikind)              :: ni, j, k, err
   integer(Ikind), allocatable :: indx(:)
   type   (pk2_t)              :: tmp
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATl', msg = &
                '<< b = extrac(a,Indx) >> with << a >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if         
      
   ni = count(lindx)
   
   if ( ni == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATl', msg =    &
                '<< b = extrac(a,Indx) >> with << Indx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if
   
   allocate( indx(ni), stat = err )
   
   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = 'pk2f_subEXTRACMATl', &
                         msg = 'Allocation failure'                  )
      res = pk2_t()
      return
   end if   
   
   k = 0 ; ni = 0
   do j = 1, size(lindx)
      k = k + 1
      if ( lindx(j) ) then
         ni = ni + 1 ; indx(ni) = k
      end if   
   end do

   call tmp%ExtracSubMat ( a, indx ) 
   
   error_TraceNreturn(opflag, 'pk2f_subEXTRACMATl')
   
   call pk2_moveAlloc ( from = tmp, to = res )   
   
   END SUBROUTINE pk2f_subEXTRACMATl


!=============================================================================================   
   FUNCTION pk2f_EXTRACMATl ( a, lindx ) result ( res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   logical       , intent(in    ) :: lindx (:)
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subEXTRACMATl ( a, lindx, res )
   
   error_TraceNreturn(opflag, 'pk2f_EXTRACMATl')
   
   END FUNCTION pk2f_EXTRACMATl


!=============================================================================================   
   SUBROUTINE pk2f_subEXTRACMATli ( a, lindx, jndx, res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   logical       , intent(in    ) :: lindx (:)
   integer(Ikind), intent(in    ) :: jndx (:)   
   class  (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  lindx (booleans) and jndx (integers).
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   integer(Ikind)              :: ni, j, k, err
   integer(Ikind), allocatable :: indx(:)
   type   (pk2_t)              :: tmp   
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATli', msg =     &
                '<< b = extrac(a,Indx,Jndx) >> with << a >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if         

   if ( size(jndx) == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATli', msg =        &
                '<< b = extrac(a,Indx,Jndx) >> with << Jndx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if         
      
   ni = count(lindx)
   
   if ( ni == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATli', msg =        &
                '<< b = extrac(a,Indx,Jndx) >> with << Indx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if
   
   allocate( indx(ni), stat = err )

   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = 'pk2f_subEXTRACMATli', &
                         msg = 'Allocation failure'                   )
      res = pk2_t()
      return
   end if   
   
   k = 0 ; ni = 0
   do j = 1, size(lindx)
      k = k + 1
      if ( lindx(j) ) then
         ni = ni + 1 ; indx(ni) = k
      end if   
   end do

   call tmp%ExtracSubMat ( a, indx, jndx ) 

   error_TraceNreturn(opflag, 'pk2f_subEXTRACMATli')
   
   call pk2_moveAlloc ( from = tmp, to = res )   
   
   END SUBROUTINE pk2f_subEXTRACMATli


!=============================================================================================   
   FUNCTION pk2f_EXTRACMATli ( a, lindx, jndx ) result ( res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   logical       , intent(in    ) :: lindx (:)
   integer(Ikind), intent(in    ) :: jndx (:)   
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subEXTRACMATli ( a, lindx, jndx, res )

   error_TraceNreturn(opflag, 'pk2f_EXTRACMATli')
   
   END FUNCTION pk2f_EXTRACMATli
   

!=============================================================================================   
   SUBROUTINE pk2f_subEXTRACMATll ( a, lindx, ljndx, res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   logical       , intent(in    ) :: lindx (:)
   logical       , intent(in    ) :: ljndx (:)
   class  (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Extracts from the array "a" the sub-array corresponding to the set of indices given in 
!  lindx (booleans) and ljndx (booleans).
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------      
   integer(Ikind)              :: ni, nj, j, k, err
   integer(Ikind), allocatable :: indx(:), jndx(:)
   type   (pk2_t)              :: tmp      
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATll', msg =     &
                '<< b = extrac(a,Indx,Jndx) >> with << a >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if         
      
   ni = count(lindx)
   
   if ( ni == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATll', msg =        &
                '<< b = extrac(a,Indx,Jndx) >> with << Indx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if
   
   allocate( indx(ni), stat = err )

   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = 'pk2f_subEXTRACMATll', &
                         msg = 'Allocation failure'                   )
      res = pk2_t()
      return
   end if   
   
   k = 0 ; ni = 0
   do j = 1, size(lindx)
      k = k + 1
      if ( lindx(j) ) then
         ni = ni + 1 ; indx(ni) = k
      end if   
   end do

   nj = count(lindx)
   
   if ( nj == 0 ) then
      call opflag%set ( stat = WARNING, where = 'pk2f_subEXTRACMATll', msg =        &
                '<< b = extrac(a,Indx,Jndx) >> with << Jndx >> empty (--> b = [Ê])' )
      res = pk2_t()
      return
   end if
   
   allocate( jndx(nj), stat = err )

   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = 'pk2f_subEXTRACMATll', &
                         msg = 'Allocation failure'                   )
      res = pk2_t()
      return
   end if   
      
   k = 0 ; nj = 0
   do j = 1, size(ljndx)
      k = k + 1
      if ( ljndx(j) ) then
         nj = nj + 1 ; jndx(nj) = k
      end if   
   end do

   call tmp%ExtracSubMat ( a, indx, jndx ) 

   error_TraceNreturn(opflag, 'pk2f_subEXTRACMATll')
   
   call pk2_moveAlloc ( from = tmp, to = res )
   
   END SUBROUTINE pk2f_subEXTRACMATll   


!=============================================================================================   
   FUNCTION pk2f_EXTRACMATll ( a, lindx, ljndx ) result ( res )
!=============================================================================================     
   class  (pk2_t), intent(in    ) :: a
   logical       , intent(in    ) :: lindx (:)
   logical       , intent(in    ) :: ljndx (:)
   type   (pk2_t)                 :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subEXTRACMATll ( a, lindx, ljndx, res )

   error_TraceNreturn(opflag, 'pk2f_EXTRACMATll')
   
   END FUNCTION pk2f_EXTRACMATll   
   

!=============================================================================================
   SUBROUTINE pk2f_subMERGEMATS ( matrs, delim, res )
!=============================================================================================   
   class    (pk2_t), intent(in    ) :: matrs(:)    
   character(len=*), intent(in    ) :: delim
   class    (pk2_t), intent(in out) :: res    
!---------------------------------------------------------------------------------------------  
!  This routine assembles a given set of matrices matrs(1:n) in a larger one according to the
!  delimiters delim ("," corresponds to a column delimiter, ";" to a row delimiter)
!  A check for shape and type compatibilities is first done
!
!  Example: call pk2f_subMergeMats (pdata(3:6), ",,;",res)
!
!  Note: suppose given the 13 sub-matrices matrs(i) = di, we want to build the following matrix
!
!               | d1 d2  d3  d4      |
!        res =  | d5 d6  d7          |
!               | d8                 |
!               | d9 d10 d11 d12 d13 |
!
!  The corresponfing calling instruction is 
!
!              call pk2f_MergeMats (matrs, delim, res)
!
!  with matrs(1:13) contains the di's and 
!
!                 delim = ', , , ; , , ; ; , , , ,'  (no blanks, added here for clarity)
!
!  that is 
!              res = [ d1, d2, d3, d4 ; d5, d6, d7 ; d8 ; d9, d10, d11, d12, d13 ]
!
!  Shape and type consistency of the sub-matrices are first checked. The resulting type and
!  size of "res" are determined and "res" is allocated.
!
!  The matrix "res" is then assembled by copying the submatrices in their corresponding
!  locations.
!
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=* ), parameter   :: HERE = 'pk2f_MERGEMATS'          
   character(len=1 )              :: ch
   character(len=99)              :: cnum   
   logical                        :: is_TheFirstNonEmpty
   integer  (Ikind )              :: nmatrs, nRowRes, nColRes, TypRes, nMacroRow, iMacroRow, &
                                     ldelim, nj     , mj     , typj  , nrow     , ncol     , &
                                     jmat1 , jmat2  , i1     , i2    , j1       , j2       , &
                                     i     , j      , err 
   integer  (Ikind ), allocatable :: EndRow(:)
   type     (pk2_t )              :: tmp
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return !!call opflag%set ()
            
   nmatrs = size(matrs)
   
   if ( nmatrs == 0 ) then
      !opflag = err_t ( stat = WARNING, msg = "merge([Ê]) --> [Ê]" )
      res = pk2_t()
      return
   end if

   ldelim = len_trim(delim)
      
   if ( ldelim /= nmatrs-1 ) then
      write(cnum, '(i0)') nmatrs-1
      call opflag%set ( stat = UERROR, where = HERE, msg =                     &
                        "A number of " // trim(cnum) //" delimiters is needed" )
      return
   end if   
!
!- save in "EndRow" the positions of end-row delimiters (';'):
!
   allocate(EndRow(ldelim+1),stat=err)
   
   nMacroRow = 0
   do i = 1, ldelim   
      ch = delim(i:i)
      if ( ch == ';' ) then
         nMacroRow = nMacroRow + 1 ; EndRow(nMacroRow) = i
      else if ( ch /= ',' ) then
         call opflag%set (stat = UERROR, where = HERE,                      &
                           msg = "<< "//ch//" >> is not a valid delimiters" )
         return      
      end if
   end do
   nMacroRow = nMacroRow + 1 ; EndRow(nMacroRow) = ldelim + 1     
!
!- Check the shape and type consistencies, compute the size (nRowRes,nColRes) of the global
!  matrix "Res" and determine its type (TypRes):
!    
   nColRes = 0 ; nRowRes = 0 ; TypRes = EMPTY

   jmat2 = 0
    
   iMacroRow = 0
!
!- Loop on the macro-rows:
!        
   do i = 1, nMacroRow
!
!-    Loop on the sub-matrices of the macro-row #i:
!            
      ncol = 0 ! will contain the number of columns on this macro-row  
      nrow = 0 ! will contain the number of rows on this macro-row  
            
      jmat1 = jmat2 + 1 ! index of the first sub-matrix
      jmat2 = EndRow(i) ! index of the last sub-matrix     

      is_TheFirstNonEmpty = .true.
      
      do j = jmat1, jmat2
         nj = matrs(j)%nrow ; mj = matrs(j)%ncol ; typj = matrs(j)%typ
!
!-       cylce, if the j-th matrix is empty:
!
         if ( typj == EMPTY .or. nj == 0 .or. mj == 0 .or. .not. allocated(matrs(j)%m) ) cycle
         
         if ( TypRes == EMPTY ) TypRes = typj
!
!-       for non-empty sub-matrices, the number of rows has to be the same for all of them:
!
         if ( is_TheFirstNonEmpty ) then
            nrow = nj
            is_TheFirstNonEmpty = .false.
         else if ( nj /= nrow ) then   
            call opflag%set (stat = UERROR, where = HERE, msg = 'Inconsistent shapes (a)')
            return
         end if
!
!-       the type has to be compatible:
!                           
         if ( typj /= TypRes ) then
            select case (TypRes)
               case (ITYP)
                  if (typj == LTYP .or. typj == STYP) err = 1
               case (RTYP)
                  if (typj == LTYP .or. typj == STYP) err = 1
               case (CTYP)
                  if (typj == LTYP .or. typj == STYP) err = 1
               case default
                  err = 1
            end select
            if ( err == 1 ) then
               call opflag%set (stat = UERROR, where = HERE, msg = 'Incompatible types')
               return
            end if
            TypRes = max(TypRes, typj)
         end if
!
!-       update the number of columns of this macro-row:
!
         ncol = ncol + mj
         
      end do
!
!-    cycle if the i-th macro-row is empty:
!
      if ( ncol == 0 ) cycle
!
!-    the number of columns has to be the same for all macro-rows:
!
      iMacroRow = iMacroRow + 1
      if ( iMacroRow > 1 .and. ncol /= nColRes ) then
         call opflag%set (stat = UERROR, where = HERE, msg = 'Inconsistent shapes (b)')
         return  
      else if ( iMacroRow == 1 ) then
         nColRes = ncol   
      end if
!
!-    update the number of rows of the resulting matrix:
!         
      nRowRes = nRowRes + nrow

   end do         
!
!- Allocate the resulting global matrix (of size nRowRes x nColRes and type = TypRes):
!       
   call pk2_SetToType ( typ = TypRes, shape = [nRowRes,nColRes], res = tmp )
   
   error_TraceNreturn(opflag, HERE)
!
!- Assemble the global matrix:
!        
   jmat2 = 0 ; i2 = 0
   do i = 1, nMacroRow
   
      i1 = i2 + 1 ; j2 = 0
!
!-    loop on the sub-matrices of macro-row #i:
!                  
      jmat1 = jmat2 + 1
      jmat2 = EndRow(i)
      
      do j = jmat1, jmat2
         nj = matrs(j)%nrow ; mj = matrs(j)%ncol ; typj = matrs(j)%typ

         if ( typj == EMPTY .or. nj == 0 .or. mj == 0 .or. .not. allocated(matrs(j)%m) ) cycle

         j1 = j2 + 1
         
         i2 = i1 + nj - 1
         j2 = j1 + mj - 1         
!
!-       copy the elements of sub-matrix #i into the global one:
!         
         select type (p=>tmp%m)
            type is (ik2_t)         
               select type (q=>matrs(j)%m)
                  type is (ik2_t)            
                     p%v(i1:i2, j1:j2) = q%v
               end select      
            type is (rk2_t)
               select type (q=>matrs(j)%m)
                  type is (ik2_t)            
                     p%v(i1:i2, j1:j2) = real(q%v,kind=Rkind)
                  type is (rk2_t)
                     p%v(i1:i2, j1:j2) = q%v                  
               end select      
            type is (ck2_t)
               select type (q=>matrs(j)%m)
                  type is (ik2_t)            
                     p%v(i1:i2, j1:j2) = cmplx(q%v,kind=Rkind)
                  type is (rk2_t)
                     p%v(i1:i2, j1:j2) = cmplx(q%v,kind=Rkind)
                  type is (ck2_t)
                     p%v(i1:i2, j1:j2) = q%v                                    
               end select      
            type is (lk2_t)
               select type (q=>matrs(j)%m)
                  type is (lk2_t)            
                     p%v(i1:i2, j1:j2) = q%v
               end select      
            type is (sk2_t)
               select type (q=>matrs(j)%m)
                  type is (sk2_t)            
                     p%v(i1:i2, j1:j2) = q%v
               end select      
         end select                     
      end do
   end do      
   
   call pk2_moveAlloc ( from = tmp, to = res )
   
   END SUBROUTINE pk2f_subMERGEMATS


!=============================================================================================    
   FUNCTION pk2f_MERGEMATS ( matrs, delim ) result ( res )
!=============================================================================================   
   class    (pk2_t), optional, intent(in) :: matrs(:)    
   character(len=*), optional, intent(in) :: delim
   type     (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------  


!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter :: HERE = 'pk2f_MERGEMATS'          
   type     (str_t)            :: h(18), f1, f2
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) .and. .not. present(delim) ) then
!
!-    return in "res" some helps (for calmat) and exit:
!      
      f1 = str_color('merge',pk2f_helpcol)
      f2 = str_color('[ ]'  ,pk2f_helpcol)
      h( 1) = "<< "+f1+" >> or << "+f2+" >> array constructor"
      h( 2) = " "
      h( 3) = "Syntax: M = [A11,A12,... ; A21, ... ], M = "+f1+"(A11,A12,... ; A21, ...)"
      h( 4) = " "
      h( 5) = "Note: . The type and the shape of sub-arrays Aij must be consistent."
      h( 6) = "      . A semi-colon (;) is used between two consecutive rows, while a"
      h( 7) = "        comma (,) is used between two consecutive elements of the same row."
      h( 8) = "      . Space can be used in place of comma."
      h( 9) = " "
      h(10) = "Examples: "
      h(11) = " . A = [1,2 ; 1,2], B = [3,4 ; 3,4], C = [5 ; 5], L = [1,2,3,4,5]"
      h(12) = " "      
      h(13) = "   then M = [ A,B,C ; L ] gives the 3x5 array [1,2,3,4,5 ; 1,2,3,4,5 ; 1,2,3,4,5]"
      h(14) = " "
      h(15) = " . c = [5 ; 5 ; 5],  l = [1 2 3 4]"
      h(16) = " "      
      h(17) = "   then N = [ [A,B ; l], c ] gives the same 3x5 array"
      h(18) = " "
      call pk2_assign ( res, h )
      return
   end if   

   if ( .not. present(matrs) .or. .not. present(delim) ) then
      call opflag%set ( stat = UERROR, where = HERE, msg = 'An argument is missing' )
      return
   end if   
            
   call pk2f_subMERGEMATS ( matrs, delim, res )

   error_TraceNreturn(opflag, HERE)
   
   END FUNCTION pk2f_MERGEMATS
         

!=============================================================================================    
   SUBROUTINE pk2f_subMERGEMATS1 ( matrs, delim, res )
!=============================================================================================   
   class    (pk2_t), intent(in    ) :: matrs(:)    
   character(len=*), intent(in    ) :: delim
   class    (pk2_t), intent(in out) :: res    
!---------------------------------------------------------------------------------------------  
!  (another version of mergemats)
!  This routine assembles a given set of matrices matrs(1:n) in a larger one according to the
!  delimiters delim ("," corresponds to a column delimiter, ";" to a row delimiter)
!  A check for shape and type compatibilities is first done
!
!  Example: suppose given the 13 sub matrices "di", we want to build the following matrix   
!
!               | d1 d2  d3  d4      |
!        res =  | d5 d6  d7          |
!               | d8                 |
!               | d9 d10 d11 d12 d13 |
!
!  The corresponfing calling instruction is 
!
!              call pk2f_subMergeMats1 (matrs, delim)
!
!  with matrs(1:13) contains the di's and 
!
!                 delim = ', , , ; , , ; ; , , , ,'  (no blanks, added here for clarity)
!
!  that is 
!              res = [ d1, d2, d3, d4 ; d5, d6, d7 ; d8 ; d9, d10, d11, d12, d13 ]
!
!  Each macro-row of "res" (i.e., [d1,d2,d3,d4], [d5,d6,d7], ...) is analyzed (shape and type
!  consistency checks), formed and added at the next rows of "res".
!
!  Version: this version use the bound procedure %InsertInto (which inserts the columns or the
!           rows of a matrix into another one). This version is more compact than the previous 
!           one (pk2f_subMERGEMATS) but may be less efficient as an allocation-reallocation is 
!           needed for each macro-row.
!-----------------------------------------------------------------------------------R.H. 10/18

!- local variables ---------------------------------------------------------------------------        
   character(len=* ), parameter   :: HERE = 'pk2f_subMERGEMATS1'  
   character(len=1 )              :: ch
   character(len=99)              :: cnum
   integer  (Ikind ), allocatable :: EndRow(:)
   integer  (Ikind )              :: i, j, nmatrs, ldelim, jmat1, jmat2, err
   integer  (Ikind )              :: nMacroRow, iMacroRow
   type     (pk2_t )              :: MacroRow, tmp
   logical                        :: is_TheFirstNonEmpty
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nmatrs = size(matrs) 

   if ( nmatrs == 0 ) then
   !   opflag = err_t ( stat = WARNING, msg = "b = merge() --> b=[Ê]" )
      res = pk2_t()
      return
   end if
   
   ldelim = len_trim(delim)         
  
   if ( ldelim /= nmatrs-1 ) then
      write(cnum, '(i0)') nmatrs-1
      call opflag%set ( stat = UERROR, where = HERE,                                       &
                         msg = "A number of "//trim(cnum)//" delimiters is needed" )
      return
   end if   
!
!- save in EndRow the positions of end-row delimiters (';'):
!
   allocate(EndRow(ldelim+1),stat=err)
   if ( err /= 0 ) then
      call opflag%set(stat=IERROR, where=HERE, msg='Allocation failure for array EndRow')
      return
   end if   
   
   nMacroRow = 0
   do i = 1, ldelim   
      ch = delim(i:i)
      if ( ch == ';' ) then
         nMacroRow = nMacroRow + 1 ; EndRow(nMacroRow) = i
      else if ( ch /= ',' ) then
         call opflag%set ( stat = UERROR, where = HERE, &
                            msg = "<< "//ch//" >> is not a valid delimiters" )
         return      
      end if
   end do
   nMacroRow = nMacroRow + 1 ; EndRow(nMacroRow) = ldelim + 1  
!
!- Loop on the macro-rows:
!        
   jmat2 = 0 ; iMacroRow = 0
 
   do i = 1, nMacroRow
!
!-    initialize the current macro-row to the empty matrix:
!
      MacroRow = pk2_t (shape = [IZERO,IZERO], typ = EMPTY)
!
!-    loop on the sub-matrices of this macro-row:
!
      jmat1 = jmat2 + 1 ! index of the first sub-matrix on the macro-row #i
      jmat2 = EndRow(i) ! index of the last sub-matrix on the macro-row #i    
      
      is_TheFirstNonEmpty = .true.
      
      do j = jmat1, jmat2
!
!-       cycle if the j-th sub-matrix is empty:
!      
         if ( matrs(j)%typ == EMPTY .or. matrs(j)%nrow == 0 .or. matrs(j)%ncol == 0 ) cycle
!
!-       check that the j-th sub-matrix and the previous one have the same number of rows:
!
         if ( is_TheFirstNonEmpty ) then
            is_TheFirstNonEmpty = .false.
         else if ( matrs(j)%nrow /= MacroRow%nrow ) then   
            call opflag%set (stat = UERROR, where = HERE, msg = 'Inconsistent shapes')
            return
         end if            
!
!-       put the columns of this matrix after those of the current macro-row:
!
         call matrs(j)%InsertInto (a = MacroRow, pnum = MacroRow%ncol+1, cr = 'c')
         error_TraceNreturn(opflag, HERE)
      end do
!
!-    cycle if the macro-row is empty:
!
      if ( MacroRow%typ == EMPTY ) cycle      
!
!-    check that this macro-row and the previous one have the same number of column:
!
      iMacroRow = iMacroRow + 1      

      if ( iMacroRow > 1 .and. MacroRow%ncol /= tmp%ncol ) then
         call opflag%set (stat = UERROR, where = HERE, msg = 'Inconsistent shapes')
         return
      end if
!
!-    put the rows of this macro-row after those of the current global matrix "Res":
!    
      call MacroRow % InsertInto (a = tmp, pnum = tmp%nrow+1, cr = 'r')
      error_TraceNreturn(opflag, HERE)

   end do      
   
   call pk2_moveAlloc ( from = tmp, to = res )
   
   END SUBROUTINE pk2f_subMERGEMATS1


!=============================================================================================    
   FUNCTION pk2f_MERGEMATS1 ( matrs, delim ) result ( res )
!=============================================================================================   
   class    (pk2_t), optional, intent(in) :: matrs(:)    
   character(len=*), optional, intent(in) :: delim
   type     (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------  


!- local variables ---------------------------------------------------------------------------        
   character(len=* ), parameter   :: HERE = 'pk2f_MERGEMATS1'  
   type     (str_t )              :: h(18), f1, f2   
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) .and. .not. present(delim) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f1 = str_color('merge'  ,pk2f_helpcol)
      f2 = str_color('[ ]'     ,pk2f_helpcol)
      h( 1) = "<< "+f1+" >> or << "+f2+" >> array constructor"
      h( 2) = " "
      h( 3) = "Syntax: M = [A11,A12,... ; A21, ... ], M = "+f1+"(A11,A12,... ; A21, ...)"
      h( 4) = " "
      h( 5) = "Note: . The type and the shape of sub-arrays Aij must be consistent."
      h( 6) = "      . A semi-colon (;) is used between two consecutive rows, while a"
      h( 7) = "        comma (,) is used between two consecutive elements of the same row."
      h( 8) = "      . Space can be used in place of comma."
      h( 9) = " "
      h(10) = "Examples: "
      h(11) = " . A = [1,2 ; 1,2], B = [3,4 ; 3,4], C = [5 ; 5], L = [1,2,3,4,5]"
      h(12) = " "      
      h(13) = "   then M = [ A,B,C ; L ] gives the 3x5 array [1,2,3,4,5 ; 1,2,3,4,5 ; 1,2,3,4,5]"
      h(14) = " "
      h(15) = " . c = [5 ; 5 ; 5],  l = [1 2 3 4]"
      h(16) = " "      
      h(17) = "   then N = [ [A,B ; l], c ] gives the same 3x5 array"
      h(18) = " "
      call pk2_assign ( res, h )
      return
   end if   

   if ( .not. present(matrs) .or. .not. present(delim) ) then
      call opflag%set (stat = UERROR,  where = HERE, msg = 'An argument is missing')
      return
   end if   
   
   call pk2f_subMERGEMATS1 ( matrs, delim, res ) ; error_TraceNreturn(opflag, HERE)
   
   END FUNCTION pk2f_MERGEMATS1
   

!=============================================================================================
   SUBROUTINE pk2f_subALL ( a, dim, res )
!=============================================================================================   
   class(pk2_t),           intent(in    ) :: a
   class(pk2_t),           intent(in out) :: res   
   class(pk2_t), optional, intent(in    ) :: dim
!---------------------------------------------------------------------------------------------          
!  Determines if all array elements are nonzero or true or non-empty (for string)
!-----------------------------------------------------------------------------------R.H. 04/19

!- local variables ---------------------------------------------------------------------------  
   integer(Ikind)       :: i, j, ldim, err     
   logical, allocatable :: bool(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. a%nrow == 0 .or. a%ncol == 0 ) then
      res = .true.
      return
   end if
   
   ldim = 0
   if ( present(dim) ) then
      if ( allocated(dim%m) .and. dim%typ /= EMPTY .and. dim%nrow > 0 .and. dim%ncol >0 ) then
         select type (p=>dim%m)
            type is (ik2_t)
               ldim = p%v(1,1)
            type is (rk2_t)
               ldim = int(p%v(1,1))
            class default
               call opflag%set ( stat = UERROR, where = 'pk2f_subALL', msg =           &
                                '2nd argument of << all >> ("dim") must be an integer' )
               return
         end select
      end if
   end if
   
   if ( ldim < 0 .or. ldim > 2 ) then
       call opflag%set ( stat = UERROR, where = 'pk2f_subALL', msg =                    &
                        'bad value for argument of "dim" of << all >> (must be 1 or 2)' )
       return
   end if   
   
   if ( ldim == 0 ) then
      allocate(bool(1,1), source = .true., stat = err)
   else if ( ldim == 1 ) then
      allocate(bool(1,a%ncol), source = .true., stat = err)
   else   
      allocate(bool(a%nrow,1), source = .true., stat = err)
   end if
   
   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = 'pk2f_subALL', msg = 'Allocation failure')
      return
   end if   
   
   select type (p=>a%m)
      type is (ik2_t)
         if ( ldim == 0 ) then
            if ( any(p%v == IZERO) ) bool(1,1) = .false.
         else if ( ldim == 1 ) then
            do i = 1, a%ncol
               if ( any(p%v(:,i) == IZERO) ) bool(1,i) = .false.
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               if ( any(p%v(i,:) == IZERO) ) bool(i,1) = .false.
            end do
         end if      

      type is (rk2_t)
         if ( ldim == 0 ) then
            if ( any(p%v == RZERO) ) bool(1,1) = .false.
         else if (ldim == 1) then
            do i = 1, a%ncol
               if ( any(p%v(:,i) == RZERO) ) bool(1,i) = .false.
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               if ( any(p%v(i,:) == RZERO) ) bool(i,1) = .false.
            end do
         end if      

      type is (ck2_t)
         if ( ldim == 0 ) then
            if ( any(p%v == CZERO) ) bool(1,1) = .false.
         else if (ldim == 1 ) then
            do i = 1, a%ncol
               if ( any(p%v(:,i) == CZERO) ) bool(1,i) = .false.
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               if ( any(p%v(i,:) == CZERO) ) bool(i,1) = .false.
            end do
         end if      

      type is (lk2_t)
         if ( ldim == 0 ) then
            if ( any(.not. p%v)) bool(1,1) = .false.
         else if ( ldim == 1 ) then
            do i = 1, a%ncol
               if ( any(.not. p%v(:,i)) ) bool(1,i) = .false.
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               if ( any(.not. p%v(i,:)) ) bool(i,1) = .false.
            end do
         end if                

      type is (sk2_t)
         if ( ldim == 0 ) then
            do j = 1, a%ncol
               do i = 1, a%nrow
                  if ( allocated(p%v(i,j)%str) ) then
                     if ( len(p%v(i,j)%str) == 0 ) then
                        bool(1,1) = .false. 
                        exit
                     end if
                  else
                     bool(1,1) = .false.
                     exit
                  end if
               end do
               if ( .not. bool(1,1) ) exit
            end do                          
         else if ( ldim == 1 ) then
            do j = 1, a%ncol
               do i = 1, a%nrow
                  if ( allocated(p%v(i,j)%str) ) then
                     if ( len(p%v(i,j)%str) == 0 ) then
                        bool(1,j) = .false. 
                        exit
                     end if
                  else
                     bool(1,j) = .false.
                     exit
                  end if
               end do
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               do j = 1, a%ncol
                  if ( allocated(p%v(i,j)%str) ) then
                     if ( len(p%v(i,j)%str) == 0 ) then
                        bool(i,1) = .false. 
                        exit
                     end if
                  else
                     bool(i,1) = .false.
                     exit
                  end if
               end do
            end do                  
         end if             
 
   end select
   
   call pk2_moveAlloc ( from = bool, to = res )
   
   END SUBROUTINE pk2f_subALL


!=============================================================================================
   FUNCTION pk2f_ALL ( a, dim ) result ( res )
!=============================================================================================   
   class(pk2_t),           intent(in) :: a
   class(pk2_t), optional, intent(in) :: dim
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

   call pk2f_subALL ( a=a, res=res, dim=dim ) ; error_TraceNreturn(opflag, "pk2f_ALL")
   
   END FUNCTION pk2f_ALL


!=============================================================================================
   SUBROUTINE pk2f_subALLwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Determines if all array elements are nonzero or true or non-empty (for string)
!-----------------------------------------------------------------------------------R.H. 04/19

!- local variables ---------------------------------------------------------------------------        
   integer(Ikind) :: nargs
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nargs = size(matrs)
   
   if ( nargs < 1 .or. nargs >  2 ) then
      call opflag%set ( stat = UERROR, where = "pk2f_subALLwrp",                        &
                         msg = '1 or 2 argument(s) expected for the function << all >>' )   
      return       
   end if

   if ( nargs == 1 ) then   
      call pk2f_subALL ( a = matrs(1), res = res )
   else
      call pk2f_subALL ( a = matrs(1), res = res, dim = matrs(2) )
   end if    
   
   error_TraceNreturn(opflag, "pk2f_subALLwrp")
   
   END SUBROUTINE pk2f_subALLwrp 
   
   
!=============================================================================================
   FUNCTION pk2f_ALLwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Determines if all array elements are nonzero or true or non-empty (for string)
!-----------------------------------------------------------------------------------R.H. 04/19

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(10), f   
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color('all',pk2f_helpcol)
      h( 1) = "<< "+f+" >> determines if all array elements are nonzero or true or non-empty"
      h( 2) = " "
      h( 3) = "Syntax:  l = "+f+"(A),  l = "+f+"(A,dim)"
      h( 4) = " "
      h( 5) = "  . "+f+"(A)      returns T (true) if all elements of A are nonzero or true"
      h( 6) = "                or non-empty (for string)"
      h( 7) = "  . "+f+"(A,dim)  tests elements along dimension dim"
      h( 8) = " "
      h( 9) = "Note: if A is empty, "+f+"(A) is true."
      h(10) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subALLwrp ( matrs, res ) ; error_TraceNreturn(opflag, "pk2f_ALLwrp")
   
   END FUNCTION pk2f_ALLwrp
   

!=============================================================================================
   SUBROUTINE pk2f_subANY ( a, res, dim ) 
!=============================================================================================   
   class(pk2_t),           intent(in    ) :: a
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in    ) :: dim
!---------------------------------------------------------------------------------------------          
!  Determine if any array element is nonzero or true or non-empty (for string)
!-----------------------------------------------------------------------------------R.H. 04/19

!- local variables ---------------------------------------------------------------------------  
   integer(Ikind)       :: i, j, ldim, err    
   logical, allocatable :: bool(:,:)
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. a%nrow == 0 .or. a%ncol == 0 ) then
      res = .false.
      return
   end if
   
   ldim = 0
   if ( present(dim) ) then
      if ( allocated(dim%m) .and. dim%typ /= EMPTY .and. dim%nrow > 0 .and. dim%ncol >0 ) then
         select type (p=>dim%m)
            type is (ik2_t)
               ldim = p%v(1,1)
            type is (rk2_t)
               ldim = int(p%v(1,1))
            class default
               call opflag%set ( stat = UERROR, where = 'pk2f_subANY', msg =           &
                                '2nd argument of << all >> ("dim") must be an integer' )
               return
         end select
      end if
   end if
   
   if ( ldim < 0 .or. ldim > 2 ) then
       call opflag%set ( stat = UERROR, where = 'pk2f_subANY', msg =                    &
                        'bad value for argument of "dim" of << all >> (must be 1 or 2)' )
       return
   end if   
   
   if ( ldim == 0 ) then
      allocate(bool(1,1), source = .false., stat = err)
   else if ( ldim == 1 ) then
      allocate(bool(1,a%ncol), source = .false., stat = err)
   else   
      allocate(bool(a%nrow,1), source = .false., stat = err)
   end if

   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = 'pk2f_subANY', msg = 'Allocation failure' )
      return
   end if   
      
   select type (p=>a%m)
      type is (ik2_t)
         if ( ldim == 0 ) then
            if ( any(p%v /= IZERO) ) bool(1,1) = .true.
         else if ( ldim == 1 ) then
            do i = 1, a%ncol
               if ( any(p%v(:,i) /= IZERO) ) bool(1,i) = .true.
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               if ( any(p%v(i,:) /= IZERO) ) bool(i,1) = .true.
            end do
         end if      

      type is (rk2_t)
         if ( ldim == 0 ) then
            if ( any(p%v /= RZERO) ) bool(1,1) = .true.
         else if ( ldim == 1 ) then
            do i = 1, a%ncol
               if ( any(p%v(:,i) /= RZERO) ) bool(1,i) = .true.
            end do
         else if ( ldim == 2) then
            do i = 1, a%nrow
               if ( any(p%v(i,:) /= RZERO) ) bool(i,1) = .true.
            end do
         end if      

      type is (ck2_t)
         if ( ldim == 0 ) then
            if ( any(p%v /= CZERO) ) bool(1,1) = .true.
         else if ( ldim == 1 ) then
            do i = 1, a%ncol
               if ( any(p%v(:,i) /= CZERO) ) bool(1,i) = .true.
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               if ( any(p%v(i,:) /= CZERO) ) bool(i,1) = .true.
            end do
         end if      

      type is (lk2_t)
         if ( ldim == 0 ) then
            if ( any(p%v)) bool(1,1) = .true.
         else if (ldim == 1 ) then
            do i = 1, a%ncol
               if ( any(p%v(:,i)) ) bool(1,i) = .true.
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               if ( any(p%v(i,:)) ) bool(i,1) = .true.
            end do
         end if                

      type is (sk2_t)
         if ( ldim ==  0) then
            do j = 1, a%ncol
               do i = 1, a%nrow
                  if ( allocated(p%v(i,j)%str) ) then
                     if ( len(p%v(i,j)%str) /= 0 ) then
                        bool(1,1) = .true. 
                        exit
                     end if
                  end if   
               end do
               if ( bool(1,1) ) exit
            end do                          
         else if ( ldim == 1 ) then
            do j = 1, a%ncol
               do i = 1, a%nrow
                  if ( allocated(p%v(i,j)%str) ) then
                     if ( len(p%v(i,j)%str) /= 0 ) then
                        bool(1,j) = .true. 
                        exit
                     end if
                  end if
               end do
            end do
         else if ( ldim == 2 ) then
            do i = 1, a%nrow
               do j = 1, a%ncol
                  if ( allocated(p%v(i,j)%str) ) then
                     if ( len(p%v(i,j)%str) /= 0 ) then
                        bool(i,1) = .true. 
                        exit
                     end if
                  end if
               end do
            end do                  
         end if             
 
   end select
   
   call pk2_moveAlloc ( from = bool, to = res )
   
   END SUBROUTINE pk2f_subANY


!=============================================================================================
   FUNCTION pk2f_ANY ( a, dim ) result ( res )
!=============================================================================================   
   class(pk2_t),           intent(in) :: a
   class(pk2_t), optional, intent(in) :: dim
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

   call pk2f_subANY ( a = a, res = res, dim = dim)  ; error_TraceNreturn(opflag, "pk2f_ANY")
    
   END FUNCTION pk2f_ANY
      
   
!=============================================================================================
   SUBROUTINE pk2f_subANYwrp ( matrs, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Determine if any array element is nonzero or true or non-empty (for string)
!-----------------------------------------------------------------------------------R.H. 04/19

!- local variables ---------------------------------------------------------------------------        
   integer(Ikind) :: nargs
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nargs = size(matrs)
   
   if ( nargs < 1 .or. nargs >  2 ) then
      call opflag%set ( stat = UERROR, where = 'pk2f_subANYwrp',                       &
                        msg = '1 or 2 argument(s) expected for the function << any >>' )   
      return       
   end if

   if ( nargs == 1 ) then   
      call pk2f_subANY ( a = matrs(1), res = res )
   else
      call pk2f_subANY ( a = matrs(1), res = res, dim = matrs(2) )
   end if    

   error_TraceNreturn(opflag, "pk2f_subANYwrp")
   
   END SUBROUTINE pk2f_subANYwrp


!=============================================================================================
   FUNCTION pk2f_ANYwrp ( matrs ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Determine if any array element is nonzero or true or non-empty (for string)
!-----------------------------------------------------------------------------------R.H. 04/19

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(10), f   
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(matrs) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color("any",pk2f_helpcol)
      h( 1) = "<< "+f+" >> determines if any array element is nonzero or true or non-empty"
      h( 2) = " "
      h( 3) = "Syntax:  l = "+f+"(A),  l = "+f+"(A,dim)"
      h( 4) = " "
      h( 5) = "  . "+f+"(A)      returns T (true) if any element of A is nonzero or true"
      h( 6) = "                or non-empty (for string)"
      h( 7) = "  . "+f+"(A,dim)  tests elements along dimension dim"
      h( 8) = " "
      h( 9) = "Note: if A is empty, "+f+"(A) is false."
      h(10) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subANYwrp ( matrs, res ) ;  error_TraceNreturn(opflag, "pk2f_ANYwrp")
   
   END FUNCTION pk2f_ANYwrp
      
                     
!=============================================================================================
   SUBROUTINE pk2f_subFIND ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!   Description:
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subFIND'  
   logical         , allocatable :: bool(:,:)
   integer                       :: err
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%nrow == 0 .or. a%ncol == 0 .or. a%typ == EMPTY .or. .not. allocated(a%m) ) then
      call opflag%set ( stat = WARNING, where = HERE,                             &
                         msg = 'find(a)  >> with << a >> non-allocated (--> [Ê])' )
      res = pk2_t()
      return
   end if
      
   err = 0   
   select type (p=>a%m)
      type is (lk2_t)
         call pk2f_subFindx (p%v, res)
      type is (ik2_t)
         allocate(bool(p%nrow,p%ncol), stat = err)
         if ( err == 0 ) then
            bool(:,:) = (p%v(:,:) /= IZERO)
            call pk2f_subFindx (bool, res)
         end if
      type is (rk2_t)
         allocate(bool(p%nrow,p%ncol), stat = err)
         if ( err == 0 ) then
            bool(:,:) = (p%v(:,:) /= RZERO)
            call pk2f_subFindx (bool, res)
         end if
      type is (ck2_t)
         allocate(bool(p%nrow,p%ncol), stat = err)
         if ( err == 0 ) then
             bool(:,:) = (p%v(:,:) /= CZERO)
             call pk2f_subFindx (bool, res)
         end if
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg = 'Bad argument for << find >>' )
         return
   end select      
   
   if ( opflag%code > 0 ) then
      error_TraceNreturn(opflag, HERE)
   else if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure' )
   end if
      
   END SUBROUTINE pk2f_subFIND


!=============================================================================================
   FUNCTION pk2f_FIND ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!   Description:
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(4), f   
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
!
!-    return in "res" some helps for this function and exit:
!      
      f = str_color("find",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the indices of non-zero (or non-false) elements of an array"
      h(2) = " "
      h(3) = "Syntax: k = "+f+"(A)"
      h(4) = " "
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subFIND ( a , res ) ; error_TraceNreturn(opflag, 'pk2f_FIND')

   END FUNCTION pk2f_FIND
   

!=============================================================================================
   SUBROUTINE pk2f_subFindx ( bool, res )
!=============================================================================================   
   logical       , intent(in    ) :: bool(:,:)
   class  (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'pk2f_subFindx'
   integer  (Ikind)              :: i, j, k, n, na, ma, err
   integer  (Ikind), allocatable :: tmp(:,:)
!---------------------------------------------------------------------------------------------    

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   na = size(bool,dim=1) ; ma = size(bool,dim=2)
   
   n = count(bool)
   
   if ( n == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                           &
                         msg = "<< find(a) >> with << a >> = .false. (--> [Ê])" )
      res = pk2_t()
      return
   end if
   
   if ( na == 1 ) then
      allocate(tmp(1,n), stat = err)
   else
      allocate(tmp(n,1), stat = err)
   end if
   
   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = HERE, msg = "Allocation failure" )
      return
   end if
   
   n = 0 ; k = 0
   if ( na == 1 ) then
      do j = 1, ma
         k = k + 1
         if ( bool(1,j) ) then
            n = n + 1 ; tmp(1,n) = k
         end if
      end do         
   else
      do j = 1, ma
         do i = 1, na
            k = k + 1
            if ( bool(i,j) ) then
               n = n + 1 ; tmp(n,1) = k
            end if
         end do
      end do         
   end if   
   
   call pk2_moveAlloc ( from = tmp, to = res )
         
   END SUBROUTINE pk2f_subFindx
   
   
!=============================================================================================
   SUBROUTINE pk2f_subSIN ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the sine of the array "a" (integer, real or complex). The result is an array of the
!  same shape, real if "a" is integer or real, complex otherwise.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subSIN'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                             &
                        msg ="<< sin(a) >> with << a >> empty (--> sin(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   ! (Note: avoid reallocating res (or using tmpr/tmpc) when it has already the right type)
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = sin(real(p%v,kind=Rkind))
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select 
         end if
         tmpr = sin(real(p%v,kind=Rkind))
         call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = sin(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = sin(p%v)
         call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = sin(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = sin(p%v)
         call pk2_moveAlloc ( from = tmpc, to = res )
         
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << sin >> must be integer, real or complex'//NLT// &
               '(here, the argument is a logical)' )
         return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << sin >> must be integer, real or complex'//NLT// &
               '(here, the argument is a string)' )
         return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << sin >> must be integer, real or complex'//NLT// &
               '(here, the argument is of unknown type)' )
         return
   end select      
   
   END SUBROUTINE pk2f_subSIN


!=============================================================================================
   FUNCTION pk2f_SIN ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Returns the sine of the array "a" (integer, real or complex). The result is an array of the
!  same shape, real if "a" is integer or real, complex otherwise.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("sin",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the sine with arguments in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is an array of the same shape of A, complex if A is"
      h(7) = "      complex, real otherwise"
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subSIN ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_SIN')
   
   END FUNCTION pk2f_SIN


!=============================================================================================
   SUBROUTINE pk2f_subCOS ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the cosine of the array "a" (integer, real or complex). The result is an array of
!  the same shape, real if "a" is integer or real, complex otherwise.
!-----------------------------------------------------------------------------------R.H. 04/18  

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subCOS'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                               & 
                         msg = "<< cos(a) >> with << a >> empty (--> cos(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   ! (Note: avoid reallocating res when it has already the right type)
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = cos(real(p%v,kind=Rkind))
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select 
         end if
         tmpr = cos(real(p%v,kind=Rkind))
         call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = cos(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = cos(p%v)
         call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = cos(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = cos(p%v)
         call pk2_moveAlloc ( from = tmpc, to = res )
         
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << cos >> must be integer, real or complex'//NLT// &
               '(here, the argument is a logical)' )
         return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << cos >> must be integer, real or complex'//NLT// &
               '(here, the argument is a string)' )
         return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << cos >> must be integer, real or complex'//NLT// &
               '(here, the argument is of unknown type)' )
         return
   end select  
   
   END SUBROUTINE pk2f_subCOS


!=============================================================================================
   FUNCTION pk2f_COS ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
!  Returns the cosine of the array "a" (integer, real or complex). The result is an array of
!  the same shape, real if "a" is integer or real, complex otherwise.
!-----------------------------------------------------------------------------------R.H. 04/18  

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("cos",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the cosine with arguments in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is an array of the same shape of A, complex if A is"
      h(7) = "      complex, real otherwise"
      h(8) = " "
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subCos ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_COS')
   
   END FUNCTION pk2f_COS


!=============================================================================================
   SUBROUTINE pk2f_subTAN ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the tangent of the array "a" (integer, real or complex). The result is an array of
!  the same shape, real if "a" is integer or real, complex otherwise.
!-----------------------------------------------------------------------------------R.H. 04/18
  
!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subTAN'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)     
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                               &
                         msg = "<< tan(a) >> with << a >> empty (--> tan(a) = [Ê])" )
      res = pk2_t()
      return
   end if   
   
   ! (Note: avoid reallocating res when it has already the right type)
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = tan(real(p%v,kind=Rkind))
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select 
         end if
         tmpr = tan(real(p%v,kind=Rkind))
         call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = tan(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = tan(p%v)
         call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = tan(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = tan(p%v)
         call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << tan >> must be integer, real or complex'//NLT// &
               '(here, the argument is a logical)' )
         return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << tan >> must be integer, real or complex'//NLT// &
               '(here, the argument is a string)' )
         return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << tan >> must be integer, real or complex'//NLT// &
               '(here, the argument is of unknown type)' )
         return
   end select   
      
   END SUBROUTINE pk2f_subTAN


!=============================================================================================
   FUNCTION pk2f_TAN ( a ) result ( res )
!=============================================================================================   
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          
  
!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("tan",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the tangent with arguments in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is an array of the same shape of A, complex if A is"
      h(7) = "      complex, real otherwise"
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if

   call pk2f_subTAN ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_TAN')
      
   END FUNCTION pk2f_TAN
      

!=============================================================================================    
   FUNCTION pk2f_ASIN_old ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc sine of the array "a" (integer, real or complex). The result is an array of
!  the same shape.
!
!  Notes:
!    . For a real (or an integer) array, the result may be complex (if at least one entry of
!      "a" is not in the domain [-1, 1])
!    . asin(of a complex) is not yet supported by ifort 15.0. We then use the formula
!                    asin(z) = -i * log ( i * z + sqrt(1 - z^2) )
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("asin",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse sine in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is an array of the same shape of A, complex if A is complex"
      h(7) = "      or if at least one of its elements is not in [-1,1]"
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, &
                         msg = "<< b = asin(a) >> with << a >> empty (--> b = [Ê])" )
      return
   end if
   
   select type (p=>a%m)
      type is (ik2_t)
         if ( any (abs(p%v) > IONE) ) then
            res = -CIMAG * log( CIMAG * p%v + sqrt(CONE-p%v**2) )
         else
            res = asin(real(p%v,kind=Rkind))
         end if      
      type is (rk2_t)
         if ( any (abs(p%v) > RONE) ) then
            res = -CIMAG * log( CIMAG * p%v + sqrt(CONE-p%v**2) )
         else
            res = asin(p%v)
         end if            
      type is (ck2_t)
          res = -CIMAG * log( CIMAG * p%v + sqrt(CONE-p%v**2) )
      type is (lk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << asin >> must be real or complex'//NLT//&
               '(here, the argument is a logical)' )
      type is (sk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << asin >> must be real or complex'//NLT//&
               '(here, the argument is a string)' )
      class default
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << asin >> must be real or complex'//NLT//&
               '(here, the argument is of unknown type)' )

   end select      
             
   END FUNCTION pk2f_ASIN_old


!=============================================================================================    
   SUBROUTINE pk2f_subASIN ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc sine of the array "a" (integer, real or complex). The result is an array of
!  the same shape.
!
!  Notes:
!    . For a real (or an integer) array, the result may be complex (if at least one entry of
!      "a" is not in the domain [-1, 1])
!    . asin(of a complex) is now supported by ifort
!----------------------------------------------------------------------------R.H. 04/18, 11/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subASIN'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                 &
                         msg = "<< asin(a) >> with << a >> empty (--> asin(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   ! (Note: avoid reallocating res when it has already the right type)   
   select type (p=>a%m)
      type is (ik2_t)
         if ( any (abs(p%v) > IONE) ) then
            call bk2_reallocIfNeeded ( res%m, CTYP, a%nrow, a%ncol )
            call pk2f_resetSize ( a%nrow, a%ncol, res )

            !!if ( allocated(res%m) ) then
               select type (q=>res%m)
                  type is (ck2_t)
                     q%v = asin(cmplx(p%v,kind=Rkind))
              !!       call pk2f_resetSize ( a%nrow, a%ncol, res )
                     return
               end select
            !!end if
            !!tmpc = asin(cmplx(p%v,kind=Rkind))
            !!call pk2_moveAlloc ( from = tmpc, to = res )
         else
            if ( allocated(res%m) ) then
               select type (q=>res%m)
                  type is (rk2_t)
                     q%v = asin(real(p%v,kind=Rkind))
                     call pk2f_resetSize ( a%nrow, a%ncol, res )
                     return
               end select
            end if
            tmpr = asin(real(p%v,kind=Rkind))
            call pk2_moveAlloc ( from = tmpr, to = res )
         end if  
             
      type is (rk2_t)
         if ( any (abs(p%v) > RONE) ) then
            if ( allocated(res%m) ) then
               select type (q=>res%m)
                  type is (ck2_t)
                     q%v = asin(cmplx(p%v,kind=Rkind))
                     call pk2f_resetSize ( a%nrow, a%ncol, res )
                     return
               end select
            end if
            tmpc  = asin(cmplx(p%v,kind=Rkind))
            call pk2_moveAlloc ( from = tmpc, to = res )
         else
            if ( allocated(res%m) ) then
               select type (q=>res%m)
                  type is (rk2_t)
                     q%v = asin(p%v)
                     call pk2f_resetSize ( a%nrow, a%ncol, res )
                     return
               end select
            end if
            tmpr = asin(p%v) 
            call pk2_moveAlloc ( from = tmpr, to = res )
         end if
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = asin(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = asin(p%v) 
         call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << asin >> must be integer, real or complex'//NLT//&
               '(here, the argument is a logical)' )
         return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << asin >> must be integer, real or complex'//NLT//&
               '(here, the argument is a string)' )
         return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << asin >> must be integer, real or complex'//NLT//&
               '(here, the argument is of unknown type)' )
         return

   end select  

   END SUBROUTINE pk2f_subASIN


!=============================================================================================    
   FUNCTION pk2f_ASIN ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("asin",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse sine in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is an array of the same shape of A, complex if A is complex"
      h(7) = "      or if at least one of its elements is not in [-1,1]"
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if

   call pk2f_subASIN ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ASIN')
   
   END FUNCTION pk2f_ASIN


!=============================================================================================    
   FUNCTION pk2f_ACOS_old ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc cosine of the array "a" (integer, real or complex). The result is an array
!  of the same shape.
!
!  Notes:
!    . For a real (or an integer) array, the result may be complex (if at least one entry of
!      "a" is not in the domain [-1, 1])
!    . acos(of a complex) is not yet supported by ifort 15.0. We then use the formula
!                    acos(z) = -i * log ( z + i * sqrt(1 - z^2) )
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("acos",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse cosine in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is an array of the same shape of A, complex if A is complex"
      h(7) = "      or if at least one of its elements is not in [-1,1]"
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, &
                         msg = "<< b = acos(a) >> with << a >> empty (--> b = [Ê])" )
      return
   end if
   
   select type (p=>a%m)
      type is (ik2_t)
         if ( any (abs(p%v) > IONE) ) then
            res = -CIMAG * &
                   log( real(p%v,kind=Rkind) + CIMAG * sqrt( CONE - real(p%v,kind=Rkind)**2 ) )         
         else
            res = acos(real(p%v,kind=Rkind))
         end if      
      type is (rk2_t)
         if ( any (abs(p%v) > RONE) ) then
            res = -CIMAG * log( p%v + CIMAG * sqrt(CONE - p%v**2) )         
         else
            res = acos(p%v)
         end if            
      type is (ck2_t)
            res = -CIMAG * log( p%v + CIMAG * sqrt(CONE - p%v**2) )         
      type is (lk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << acos >> must be real or complex'//NLT//&
               '(here, the argument is a logical)' )
      type is (sk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << acos >> must be real or complex'//NLT//&
               '(here, the argument is a string)' )
      class default
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << acos >> must be real or complex'//NLT//&
               '(here, the argument is of unknown type)' )

   end select      
            
   END FUNCTION pk2f_ACOS_old


!=============================================================================================    
   SUBROUTINE pk2f_subACOS ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc cosine of the array "a" (integer, real or complex). The result is an array
!  of the same shape.
!
!  Notes:
!    . For a real (or an integer) array, the result may be complex (if at least one entry of
!      "a" is not in the domain [-1, 1])
!    . acos(of a complex) is now supported by ifort
!----------------------------------------------------------------------------R.H. 04/18, 11/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subACOS'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                 &
                         msg = "<< acos(a) >> with << a >> empty (--> acos(a) = [Ê])" )
      res = pk2_t()
      return
   end if
   
   select type (p=>a%m)
      type is (ik2_t)
         if ( any (abs(p%v) > IONE) ) then
            if ( allocated(res%m) ) then
               select type (q=>res%m)
                  type is (ck2_t)
                     q%v = acos(cmplx(p%v,kind=Rkind))
                     call pk2f_resetSize ( a%nrow, a%ncol, res )
                     return
               end select 
            end if
            tmpc = acos(cmplx(p%v,kind=Rkind))
            call pk2_moveAlloc ( from = tmpc, to = res )
         else
            if ( allocated(res%m) ) then
               select type (q=>res%m)
                  type is (rk2_t)
                     q%v = acos(real(p%v,kind=Rkind))
                     call pk2f_resetSize ( a%nrow, a%ncol, res )
                     return
               end select
            end if
            tmpr = acos(real(p%v,kind=Rkind))
            call pk2_moveAlloc ( from = tmpr, to = res )
         end if   
                 
      type is (rk2_t)
         if ( any (abs(p%v) > RONE) ) then
            if ( allocated(res%m) ) then
               select type (q=>res%m)
                  type is (ck2_t)
                     q%v = acos(cmplx(p%v,kind=Rkind))
                     call pk2f_resetSize ( a%nrow, a%ncol, res )
                     return
               end select
            end if
            tmpc  = acos(cmplx(p%v,kind=Rkind))
            call pk2_moveAlloc ( from = tmpc, to = res )
         else
            if ( allocated(res%m) ) then
               select type (q=>res%m)
                  type is (rk2_t)
                     q%v = acos(p%v)
                     call pk2f_resetSize ( a%nrow, a%ncol, res )
                     return
               end select
            end if
            tmpr = acos(p%v) 
            call pk2_moveAlloc ( from = tmpr, to = res )
         end if
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = acos(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = acos(p%v) 
         call pk2_moveAlloc ( from = tmpc, to = res )
         
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of  << acos >> must be integer, real or complex'//NLT//&
               '(here, the argument is a logical)' )
         return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of  << acos >> must be integer, real or complex'//NLT//&
               '(here, the argument is a string)' )
         return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of  << acos >> must be integer, real or complex'//NLT//&
               '(here, the argument is of unknown type)' )
         return

   end select      
            
   END SUBROUTINE pk2f_subACOS


!=============================================================================================    
   FUNCTION pk2f_ACOS ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("acos",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse cosine in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is an array of the same shape of A, complex if A is complex"
      h(7) = "      or if at least one of its elements is not in [-1,1]"
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if

   call pk2f_subACOS ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ACOS')
   
   END FUNCTION pk2f_ACOS

   
!=============================================================================================    
   FUNCTION pk2f_ATAN_old ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc tangent of the array "a" (integer, real or complex).  The result is an
!  array of the same shape, real (if "a" is integer or real) or complex.
!
!  Note: atan(of a complex) is not yet supported by ifort 15.0. We then use the formula
!                    atan(z) = (1/2) * i * log ( (i + z) / (i - z) )
!-----------------------------------------------------------------------------------R.H. 04/18   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("atan",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse tangent in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is a real array (if A is real or integer) or a complex"
      h(7) = "      array of the same shape of A."   
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, &
                          msg = "<< b = atan(a) >> with << a >> empty (--> b = [Ê])" )
      return
   end if
   
   select type (p=>a%m)
      type is (ik2_t)
         res = atan(real(p%v,kind=Rkind))         
      type is (rk2_t)
         res = atan(p%v)         
      type is (ck2_t)
         res = half * CIMAG * log( (CIMAG + p%v) / (CIMAG - p%v) )    
      type is (lk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << atan >> must be real or complex'//NLT//&
               '(here, the argument is a logical)' )
      type is (sk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << atan >> must be real or complex'//NLT//&
               '(here, the argument is a string)' )
      class default
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << atan >> must be real or complex'//NLT//&
               '(here, the argument is of unknown type)' )
   end select      
      
   END FUNCTION pk2f_ATAN_old


!=============================================================================================    
   SUBROUTINE pk2f_subATAN ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc tangent of the array "a" (integer, real or complex).  The result is an
!  array of the same shape, real (if "a" is integer or real) or complex.
!
!  Note: atan(of a complex) is now supported by ifort
!----------------------------------------------------------------------------R.H. 04/18, 11/19  

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subATAN'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                 &
                         msg = "<< atan(a) >> with << a >> empty (--> atan(a) = [Ê])" )
      res = pk2_t()
      return
   end if
   
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = atan(real(p%v,kind=Rkind))
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = atan(real(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = atan(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = atan(p%v) ; call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = atan(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if      
         tmpc = atan(p%v) ; call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << atan >> must be integer, real or complex'//NLT//&
               '(here, the argument is a logical)' )
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << atan >> must be integer, real or complex'//NLT//&
               '(here, the argument is a string)' )
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << atan >> must be integer, real or complex'//NLT//&
               '(here, the argument is of unknown type)' )
   end select      
      
   END SUBROUTINE pk2f_subATAN


!=============================================================================================    
   FUNCTION pk2f_ATAN ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("atan",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse tangent in radians" 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is a real array (if A is real or integer) or a complex"
      h(7) = "      array of the same shape of A."  
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subATAN ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ATAN')
   
   END FUNCTION pk2f_ATAN


!=============================================================================================    
   FUNCTION pk2f_SINH_old ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Returns the hyperbolic sine of the array "a" (integer, real or complex). The result is an
!  array of the same shape, real (if "a" is integer or real) or complex.
!
!  Note: sinh(of a complex) is not yet supported by ifort 15.0. We then use the formula
!                    sinh(z) = (1/2) * ( exp(z) - exp(-z) )
!-----------------------------------------------------------------------------------R.H. 04/18   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("sinh",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the hyperbolic sine." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is a real array (if A is real or integer) or a complex"
      h(7) = "      array of the same shape of A."  
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, &
                         msg = "<< b = sinh(a) >> with << a >> empty (--> b = [Ê])" )
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         res = sinh(real(p%v,kind=Rkind))         
      type is (rk2_t)
         res = sinh(p%v)         
      type is (ck2_t)
         res = half * (exp(p%v) - exp(-p%v))   
      type is (lk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << sinh >> must be real or complex'//NLT//&
               '(here, the argument is a logical)' )
      type is (sk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << sinh >> must be real or complex'//NLT//&
               '(here, the argument is a string)' )
      class default
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << sinh >> must be real or complex'//NLT//&
               '(here, the argument is of unknown type)' )
   end select      
      
   END FUNCTION pk2f_SINH_old


!=============================================================================================    
   SUBROUTINE pk2f_subSINH ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the hyperbolic sine of the array "a" (integer, real or complex). The result is an
!  array of the same shape, real (if "a" is integer or real) or complex.
!
!  Note: sinh(of a complex) is now supported by ifort
!----------------------------------------------------------------------------R.H. 04/18, 11/19  

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subSINH'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                 &
                         msg = "<< sinh(a) >> with << a >> empty (--> sinh(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = sinh(real(p%v,kind=Rkind))
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = sinh(real(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = sinh(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = sinh(p%v) ; call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = sinh(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = sinh(p%v) ; call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of  << sinh >> must be integer, real or complex'//NLT//&
               '(here, the argument is a logical)' )
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of  << sinh >> must be integer, real or complex'//NLT//&
               '(here, the argument is a string)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of  << sinh >> must be integer, real or complex'//NLT//&
               '(here, the argument is of unknown type)' ) ; return
   end select      

   END SUBROUTINE pk2f_subSINH
   

!=============================================================================================    
   FUNCTION pk2f_SINH ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("sinh",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the hyperbolic sine." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is a real array (if A is real or integer) or a complex"
      h(7) = "      array of the same shape of A." 
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subSINH ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_SINH')
   
   END FUNCTION pk2f_SINH
   

!=============================================================================================    
   FUNCTION pk2f_COSH_old ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Returns the hyperbolic cosine of the array "a" (integer, real or complex). The result is an
!  array of the same shape, real (if "a" is integer or real) or complex.
!
!  Note: cosh(of a complex) is not yet supported by ifort 15.0. We then use the formula
!                    cosh(z) = (1/2) * ( exp(z) + exp(-z) )
!-----------------------------------------------------------------------------------R.H. 04/18 

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("cosh",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the hyperbolic cosine." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is a real array (if A is real or integer) or a complex"
      h(7) = "      array of the same shape of A."     
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   if (a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0) then
      call opflag%set ( stat = WARNING, &
                         msg = "<< b = cosh(a) >> with << a >> empty (--> b = [Ê])" )
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         res = cosh(real(p%v,kind=Rkind))         
      type is (rk2_t)
         res = cosh(p%v)         
      type is (ck2_t)
         res = half * (exp(p%v) + exp(-p%v))   
      type is (lk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << cosh >> must be real or complex'//NLT//&
               '(here, the argument is a logical)' )
      type is (sk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << cosh >> must be real or complex'//NLT//&
               '(here, the argument is a string)' )
      class default
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << cosh >> must be real or complex'//NLT//&
               '(here, the argument is of unknown type)' )
   end select      
                      
   END FUNCTION pk2f_COSH_old


!=============================================================================================    
   SUBROUTINE pk2f_subCOSH ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the hyperbolic cosine of the array "a" (integer, real or complex). The result is an
!  array of the same shape, real (if "a" is integer or real) or complex.
!
!  Note: cosh(of a complex) is now  supported by
!----------------------------------------------------------------------------R.H. 04/18, 11/19   
  
!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'pk2f_subCOSH'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                 &
                         msg = "<< cosh(a) >> with << a >> empty (--> cosh(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = cosh(real(p%v,kind=Rkind))
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = cosh(real(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = cosh(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = cosh(p%v) ; call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = cosh(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = cosh(p%v)                  ; call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << cosh >> must be integer, real or complex'//NLT//&
               '(here, the argument is a logical)' ) 
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << cosh >> must be integer, real or complex'//NLT//&
               '(here, the argument is a string)' ) 
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << cosh >> must be integer, real or complex'//NLT//&
               '(here, the argument is of unknown type)' ) 
   end select          
                      
   END SUBROUTINE pk2f_subCOSH


!=============================================================================================    
   FUNCTION pk2f_COSH ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------     

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("cosh",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the hyperbolic cosine." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is a real array (if A is real or integer) or a complex"
      h(7) = "      array of the same shape of A."     
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subCOSH ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_COSH')
                      
   END FUNCTION pk2f_COSH


!=============================================================================================    
   FUNCTION pk2f_TANH_old ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Returns the hyperbolic tangent of the array "a" (integer, real or complex). The result is
!  an array of the same shape, real (if "a" is integer or real) or complex.
!
!  Note: tanh(of a complex) is not yet supported by ifort 15.0. We then use the formula
!                  cosh(z) = ( exp(z) - exp(-z) ) / ( exp(z) + exp(-z) )
!-----------------------------------------------------------------------------------R.H. 04/18   

!- local variables ---------------------------------------------------------------------------        
   complex(Rkind), allocatable :: ep(:,:), em(:,:)
   type   (str_t)              :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("tanh",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the hyperbolic tangent." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is a real array (if A is real or integer) or a complex"
      h(7) = "      array of the same shape of A."  
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, &
                         msg = "<< b = tanh(a) >> with << a >> empty (--> b = [Ê])" )
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         res = tanh(real(p%v,kind=Rkind))         
      type is (rk2_t)
         res = tanh(p%v)         
      type is (ck2_t)
         allocate(ep(p%nrow,p%ncol), source = exp( p%v))
         allocate(em(p%nrow,p%ncol), source = exp(-p%v))
         res = (ep - em) / (ep + em)      
      type is (lk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << tanh >> must be real or complex'//NLT//&
               '(here, the argument is a logical)' )
      type is (sk2_t)
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << tanh >> must be real or complex'//NLT//&
               '(here, the argument is a string)' )
      class default
         call opflag%set( stat = UERROR, msg = &
               'argument of the intrinsic function << tanh >> must be real or complex'//NLT//&
               '(here, the argument is of unknown type)' )
   end select      
   
   END FUNCTION pk2f_TANH_old


!=============================================================================================    
   SUBROUTINE pk2f_subTANH ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the hyperbolic tangent of the array "a" (integer, real or complex). The result is
!  an array of the same shape, real (if "a" is integer or real) or complex.
!
!  Note: tanh(of a complex) is now supported by ifort
!----------------------------------------------------------------------------R.H. 04/18, 11/19  

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subTANH'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< tanh(a) >> with << a >> empty (--> tanh(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = tanh(real(p%v,kind=Rkind))
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = tanh(real(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = tanh(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = tanh(p%v) ; call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = tanh(p%v)
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = tanh(p%v) ; call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << tanh >> must be integer, real or complex'//NLT//&
               '(here, the argument is a logical)' ) 
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << tanh >> must be integer, real or complex'//NLT//&
               '(here, the argument is a string)' ) 
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                   &
               'Argument of << tanh >> must be integer, real or complex'//NLT//&
               '(here, the argument is of unknown type)' ) 
   end select      

   END SUBROUTINE pk2f_subTANH


!=============================================================================================    
   FUNCTION pk2f_TANH ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(8), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("tanh",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the hyperbolic tangent." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A is an integer, real or complex array."
      h(6) = "      The result is a real array (if A is real or integer) or a complex"
      h(7) = "      array of the same shape of A."     
      h(8) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subTANH ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_TANH')
   
   END FUNCTION pk2f_TANH


!=============================================================================================    
   SUBROUTINE pk2f_subSIND ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the sine of the array "a" with "a" given in degrees. 
!
!  Note: the array "a" must only be integer or real (not complex). The result is a real array
!        of the same shape.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subSIND' 
   real     (Rkind), allocatable :: tmpr(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< sind(a) >> with << a >> empty (--> sind(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = sin( real(p%v,kind=Rkind) * deg2rad )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = sin( real(p%v,kind=Rkind) * deg2rad )
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = sin( p%v * deg2rad )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = sin( p%v * deg2rad )
      type is (ck2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< sind >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a complex array)' ) ; return
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< sind >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a logical array)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< sind >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a string array)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                         &
                          '<< sind >>: a real or an integer array is expected'//NL// &
                          '(here the argument is of an unknown type)' ) ; return
   end select   
   
   call pk2_moveAlloc ( from = tmpr, to = res )   

   END SUBROUTINE pk2f_subSIND


!=============================================================================================    
   FUNCTION pk2f_SIND ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("sind",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the sine with argument in degrees." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A must only be integer or real."
      h(6) = "      The result is a real array of the same shape of A."  
      h(7) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subSIND ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_SIND')
         
   END FUNCTION pk2f_SIND


!=============================================================================================    
   SUBROUTINE pk2f_subCOSD ( a, res ) 
!=============================================================================================    
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the cosine of the matrix "a" with "a" given in degrees.
!
!  Note: the matrix "a" must only be integer or real (not complex). The result is a real array
!        of the same shape
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subCOSD'
   real     (Rkind), allocatable :: tmpr(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                 &
                         msg = "<< cosd(a) >> with << a >> empty (--> cosd(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = cos( real(p%v,kind=Rkind) * deg2rad )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = cos( real(p%v,kind=Rkind) * deg2rad )
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = cos( p%v * deg2rad )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = cos( p%v * deg2rad )
      type is (ck2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< cosd >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a complex array)' ) ; return
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< cosd >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a logical array)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< cosd >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a string array)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< cosd >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is of an unknown type)' ) ; return
   end select      

   call pk2_moveAlloc ( from = tmpr, to = res )
	
   END SUBROUTINE pk2f_subCOSD


!=============================================================================================    
   FUNCTION pk2f_COSD ( a ) result ( res ) 
!=============================================================================================    
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("cosd",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the cosine with argument in degrees." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A must only be integer or real."
      h(6) = "      The result is a real array of the same shape of A."  
      h(7) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subCOSD ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_COSD')
           
   END FUNCTION pk2f_COSD


!=============================================================================================    
   SUBROUTINE pk2f_subTAND ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the tangent of the array "a" with "a" given in degrees.
!
!  Note: the array "a" must only be integer or real (not complex). The result is a real array
!        of the same shape.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subTAND'
   real     (Rkind), allocatable :: tmpr(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                 &
                         msg = "<< tand(a) >> with << a >> empty (--> tand(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = tan( real(p%v,kind=Rkind) * deg2rad )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = tan( real(p%v,kind=Rkind) * deg2rad )
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = tan( p%v * deg2rad )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = tan( p%v * deg2rad )
      type is (ck2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< tand >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a complex array)' ) ; return
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< tand >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a logical array)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< tand >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a string array)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                          &
                          '<< tand >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is of an unknown type)' ) ; return
   end select      

   call pk2_moveAlloc ( from = tmpr, to = res )

   END SUBROUTINE pk2f_subTAND


!=============================================================================================    
   FUNCTION pk2f_TAND ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------          

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("tand",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the tangent with argument in degrees." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A must only be integer or real."
      h(6) = "      The result is a real array of the same shape of A." 
      h(7) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subTAND ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_TAND')
     
   END FUNCTION pk2f_TAND


!=============================================================================================    
   SUBROUTINE pk2f_subASIND ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc sine of the array "a" given in degrees
!
!  Note: the array must only be integer or real with all of its entries in the domain [-1,1]
!        The result is a real array of the same shape.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subASIND'
   real     (Rkind), allocatable :: tmpr(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                   &
                         msg = "<< asind(a) >> with << a >> empty (--> asind(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         if ( any(abs(p%v) > IONE) ) then
            call opflag%set ( stat = UERROR, where = HERE, msg =                    &
                          '<< asind >>: the argument must be in the domain [-1, 1]' ) ; return
         end if
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = asin(real(p%v,kind=Rkind)) * rad2deg
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = asin(real(p%v,kind=Rkind)) * rad2deg
      type is (rk2_t)
         if ( any(abs(p%v) > RONE) ) then
            call opflag%set ( stat = UERROR, where = HERE, msg =                    &
                          '<< asind >>: the argument must be in the domain [-1, 1]' ) ; return
         endif
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = asin(p%v) * rad2deg
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = asin(p%v) * rad2deg
         
      type is (ck2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< asind >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a complex array)' ) ; return
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< asind >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a logical array)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< asind >>: A real or an integer array is expected'//NLT// &
                          '(here the argument is a string array)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< asind >>: A real or an integer array is expected'//NLT// &
                          '(here the argument of an unknown type)' ) ; return     
   end select      

   call pk2_moveAlloc ( from = tmpr, to = res )
      
   END SUBROUTINE pk2f_subASIND


!=============================================================================================    
   FUNCTION pk2f_ASIND ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("asind",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse sine in degrees." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A must only be integer or real with all of its elements in [-1,1]"
      h(6) = "      The result is a real array of the same shape of A."   
      h(7) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subASIND ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ASIND')
         
   END FUNCTION pk2f_ASIND
   

!=============================================================================================    
   SUBROUTINE pk2f_subACOSD ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc cosine of the array "a" given in degrees
!
!  Note: the array must only be integer or real with all of its entries in the domain [-1,1]
!        The result is a real array of the same shape.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subACOSD'
   real     (Rkind), allocatable :: tmpr(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                &
                         msg = "<< b = acosd(a) >> with << a >> empty (--> b = [Ê])" )
      res = pk2_t()
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         if ( any(abs(p%v) > IONE) ) then
            call opflag%set ( stat = UERROR, where = HERE, msg =                    &
                          '<< acosd >>: the argument must be in the domain [-1, 1]' ) ; return
         end if         
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = acos(real(p%v,kind=Rkind)) * rad2deg
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = acos(real(p%v,kind=Rkind)) * rad2deg
         
      type is (rk2_t)
         if ( any(abs(p%v) > RONE) ) then
            call opflag%set ( stat = UERROR, where = HERE, msg =                    &
                          '<< acosd >>: the argument must be in the domain [-1, 1]' ) ; return
         endif
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = acos(p%v) * rad2deg
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = acos(p%v) * rad2deg        
          
      type is (ck2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< acosd >>: a real or an integer array is expected'//NLT// &
                          '(here the argument is a complex array)' ) ; return
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< acosd >>: a real or an integer array is expected'//NLT// &
                          '(here the argument is a logical array)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< acosd >>: a real or an integer array is expected'//NLT// &
                          '(here the argument is a string array)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< acosd >>: a real or an integer array is expected'//NLT// &
                          '(here the argument of an unknown type)' ) ; return     
   end select      
   
   call pk2_moveAlloc ( from = tmpr, to = res )
   
   END SUBROUTINE pk2f_subACOSD


!=============================================================================================    
   FUNCTION pk2f_ACOSD ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc cosine of the array "a" given in degrees
!
!  Note: the array must only be integer or real with all of its entries in the domain [-1,1]
!        The result is a real array of the same shape.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color("acosd",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse cosine in degrees." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A must only be integer or real with all of its elements in [-1,1]"
      h(6) = "      The result is a real array of the same shape of A."  
      h(7) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subACOSD ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ACOSD')
                 
   END FUNCTION pk2f_ACOSD


!=============================================================================================    
   SUBROUTINE pk2f_subATAND ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the arc tangent of the array "a" given in degrees
!
!  Note: the array must only be integer or real. The result is a real array of same shape.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subATAND'
   real     (Rkind), allocatable :: tmpr(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                   &
                         msg = "<< atand(a) >> with << a >> empty (--> atand(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = atan(real(p%v,kind=Rkind)) * rad2deg 
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = atan(real(p%v,kind=Rkind)) * rad2deg
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = atan(p%v) * rad2deg
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = atan(p%v) * rad2deg

      type is (ck2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< atand >>: a real or an integer array is expected'//NLT// &
                          '(here the argument is a complex array)' ) ; return
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< atand >>: a real or an integer array is expected'//NLT// &
                          '(here the argument is a logical array)' ) ; return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< atand >>: a real or an integer array is expected'//NLT// &
                          '(here the argument is a string array)' ) ; return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                           &
                          '<< atand >>: a real or an integer array is expected'//NLT// &
                          '(here the argument of an unknown type)' ) ; return     
   end select      
   
   call pk2_moveAlloc ( from = tmpr, to = res )

   END SUBROUTINE pk2f_subATAND


!=============================================================================================    
   FUNCTION pk2f_ATAND ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("atand",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the inverse tangent in degrees." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A must only be integer or real. "
      h(6) = "      The result is a real array of the same shape of A." 
      h(7) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subATAND ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ATAND')
          
   END FUNCTION pk2f_ATAND


!=============================================================================================
   SUBROUTINE pk2f_subSINC ( a, res )
!=============================================================================================   
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------          
!  Returns the sine of the array "a" (integer, real or complex). The result is an array of the
!  same shape, real if "a" is integer or real, complex otherwise.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subSINC'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                             &
                        msg ="<< sinc(a) >> with << a >> empty (--> sin(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   ! (Note: avoid reallocating res (or using tmpr/tmpc) when it has already the right type)
   select type (p=>a%m)
      type is (ik2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = util_sinc ( p%v )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select 
         end if
         tmpr = util_sinc ( p%v )
         call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (rk2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (rk2_t)
                  q%v = util_sinc ( p%v )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpr = util_sinc ( p%v )
         call pk2_moveAlloc ( from = tmpr, to = res )
         
      type is (ck2_t)
         if ( allocated(res%m) ) then
            select type (q=>res%m)
               type is (ck2_t)
                  q%v = util_sinc ( p%v )
                  call pk2f_resetSize ( a%nrow, a%ncol, res )
                  return
            end select
         end if
         tmpc = util_sinc ( p%v )
         call pk2_moveAlloc ( from = tmpc, to = res )
         
      type is (lk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of << sinc >> must be integer, real or complex'//NLT// &
               '(here, the argument is a logical)' )
         return
      type is (sk2_t)
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of << sinc >> must be integer, real or complex'//NLT// &
               '(here, the argument is a string)' )
         return
      class default
         call opflag%set( stat = UERROR, where = HERE, msg =                    &
               'Argument of << sinc >> must be integer, real or complex'//NLT// &
               '(here, the argument is of unknown type)' )
         return
   end select      
   
   END SUBROUTINE pk2f_subSINC


!=============================================================================================    
   FUNCTION pk2f_SINC ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color("sinc",pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the sinc function (= " + str_color("sin",pk2f_helpcol) + &
             "(x)/x)." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: A may be integer, real or complex. "
      h(6) = "      The result is a real or a complex array of the same shape of A." 
      h(7) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subSINC ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_SINC')
          
   END FUNCTION pk2f_SINC
   
   
!=============================================================================================    
   SUBROUTINE pk2f_subLOG ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the natural logarithm (element-wise) of the array "a". The result is an array of
!  the same shape, real or complex.
!
!  Note: For a real or an integer array with at least one negative entry, the result is a 
!        complex array.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subLOG'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)   
   integer  (Ikind)              :: err       
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
  
   err = 0
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                               &
                         msg = "<< log(a) >> with << a >> empty (--> log(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         if ( any( p%v == IZERO) ) then
            err = 2
         else if ( any( p%v < IZERO ) ) then
            tmpc = log(cmplx(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpc, to = res )
         else   
            tmpr = log(real(p%v,kind=Rkind))  ; call pk2_moveAlloc ( from = tmpr, to = res )
         end if   
      type is (rk2_t)
         if ( any( p%v == RZERO ) ) then
            err = 2
         else if ( any( p%v < RZERO ) ) then
            tmpc = log(cmplx(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpc, to = res )
         else   
            tmpr = log(p%v)                   ; call pk2_moveAlloc ( from = tmpr, to = res )
         end if  
      type is (ck2_t)
         if ( any( abs(p%v) == RZERO ) ) then
            err = 2
         else   
            tmpc = log(p%v)                   ; call pk2_moveAlloc ( from = tmpc, to = res )
         end if         
      class default
         err = 1
   end select      

   if ( err /= 0 ) then 
      if ( err == 1 ) then
         call opflag%set ( stat = UERROR, where = HERE, msg =                         &
                           '<< log >>: the argument must be integer, real or complex' )
      else if ( err == 2 ) then 
         call opflag%set ( stat = UERROR, where = HERE, msg = "log(0): singular value" )
      end if      
   end if
         
   END SUBROUTINE pk2f_subLOG


!=============================================================================================    
   FUNCTION pk2f_LOG ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color('log',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the natural logarithm of an array (element-wise)." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: the result B is a complex array if A is a complex array, or if A is "
      h(6) = "      a real or an integer array with at least one negative element. "    
      h(7) = " "                    
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subLOG ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_LOG')
   
   END FUNCTION pk2f_LOG


!=============================================================================================    
   SUBROUTINE pk2f_subLOG10 ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the decimal logarithm (element-wise) of the array "a". The result is an array of
!  the same shape, real or complex.
!
!  Notes: 
!       . If "a" is a real or an integer array and if any of its entries is negative, the 
!         result is a complex array.
!       . log10(of complex) not yet supported by gfortran 6.3. We then compute it by
!                          log10(z) = log(z) / log(10)
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subLOG10'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:) 
   integer  (Ikind)              :: err         
!--------------------------------------------------------------------------------------------- 

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   err = 0
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                                   &
                         msg = "<< log10(a) >> with << a >> empty (--> log10(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         if ( any( p%v == IZERO ) ) then
            err = 2
         else if ( any( p%v < IZERO ) ) then
            tmpc = log(cmplx(p%v,kind=Rkind))*ilogten ; call pk2_moveAlloc(from=tmpc, to=res)
         else   
            tmpr = log10(real(p%v,kind=Rkind))        ; call pk2_moveAlloc(from=tmpr, to=res)
         end if   
      type is (rk2_t)
         if ( any( p%v == RZERO ) ) then
            err = 2
         else if ( any( p%v < RZERO ) ) then
            tmpc = log(cmplx(p%v,kind=Rkind))*ilogten ; call pk2_moveAlloc(from=tmpc, to=res)
         else   
            tmpr = log10(p%v)                         ; call pk2_moveAlloc(from=tmpr, to=res) 
         end if  
      type is (ck2_t)
         if ( any( abs(p%v) == RZERO ) ) then
            err = 2
         else   
            tmpc = log(p%v) * ilogten                 ; call pk2_moveAlloc(from=tmpc, to=res)
         end if         
      class default
         err = 1
   end select      

   if ( err /= 0 ) then
      if ( err == 1 ) then
         call opflag%set ( stat = UERROR, where = HERE, msg =                        &
                          "<< log >>: the argument must be integer, real or complex" ) 
      else if ( err == 2)  then   
         call opflag%set ( stat = UERROR, where = HERE, msg = "log(0): singular value" )
      end if      
   end if

   END SUBROUTINE pk2f_subLOG10


!=============================================================================================    
   FUNCTION pk2f_LOG10 ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!--------------------------------------------------------------------------------------------- 

   if ( .not. present(a) ) then
      f = str_color('log10',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the decimal logarithm of an array (element-wise)." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "
      h(5) = "Note: the result B is a complex array if A is a complex array, or if A is"
      h(6) = "      a real or an integer array with at least one negative element." 
      h(7) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subLOG10 ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_LOG10')
   
   END FUNCTION pk2f_LOG10


!=============================================================================================    
   SUBROUTINE pk2f_subEXP ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the exponential (element-wise) of the array "a". The result is an array of
!  the same shape, real (if "a" is integer or real) or complex.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subEXP'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< exp(a) >> with << a >> empty (--> exp(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         tmpr = exp(real(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpr, to = res )
      type is (rk2_t)
         tmpr = exp(p%v)                  ; call pk2_moveAlloc ( from = tmpr, to = res )
      type is (ck2_t)
         tmpc = exp(p%v)                  ; call pk2_moveAlloc ( from = tmpc, to = res )
 
      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                           "<< exp >>: integer, real or complex matrix is expected"//NLT// &
                           "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                           "<< exp >>: integer, real or complex matrix is expected"//NLT// &
                           "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                           "<< exp >>: integer, real or complex matrix is expected"//NLT// &
                           "(here the argument of an unknown type)" ) ; return
   end select      
      
   END SUBROUTINE pk2f_subEXP
   

!=============================================================================================    
   FUNCTION pk2f_EXP ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('exp',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the exponential of an array (element-wise)." 
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"   
      h(4) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subEXP ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_EXP')
   
   END FUNCTION pk2f_EXP


!=============================================================================================    
   SUBROUTINE pk2f_subERF ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the values of the error function evaluated for each element of the real or integer
!  matrix "a". 
!  The result is a real matrix of the same shape. 
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subERF'
   real     (Rkind), allocatable :: tmpr(:,:)       
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE,                               &
                         msg = "<< erf(a) >> with << a >> empty (--> erf(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         tmpr = erf(real(p%v,kind=Rkind)) 
      type is (rk2_t)
         tmpr = erf(p%v) 

      type is (ck2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                             &
                           "<< erf >>: an integer or a real matrix is expected" // NLT // & 
                           "(here the argument is a complex array)" ) ; return
      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                             &
                           "<< erf >>: an integer or a real matrix is expected" // NLT // &
                           "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                             &
                           "<< erf >>: an integer or a real matrix is expected" // NLT // &
                           "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                             &
                           "<< erf >>: an integer or a real matrix is expected" // NLT // &
                           "(here the argument of an unknown type)" ) ; return         
   end select      

   call pk2_moveAlloc ( from = tmpr, to = res )
            
   END SUBROUTINE pk2f_subERF   
      

!=============================================================================================    
   FUNCTION pk2f_ERF ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(5), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('erf',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the values of the error function evaluated for each"
      h(2) = "           element of an integer or a real array" 
      h(3) = " "
      h(4) = "Syntax: B = "+f+"(A)"   
      h(5) = " "                       
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subERF ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ERF')
               
   END FUNCTION pk2f_ERF   


!=============================================================================================    
   SUBROUTINE pk2f_subERFC ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the values of the complementary error function evaluated for each element of the 
!  real or integer matrix "a"
!  The result is a real matrix of the same shape.
!-----------------------------------------------------------------------------------R.H. 04/18
      
!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subERFC'
   real     (Rkind), allocatable :: tmpr(:,:)       
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< erfc(a) >> with << a >> empty (--> erfc(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         tmpr = erfc(real(p%v,kind=Rkind)) 
      type is (rk2_t)
         tmpr = erfc(p%v) 

      type is (ck2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                             &
                          "<< erfc >>: an integer or a real matrix is expected" // NLT // &
                          "(here the argument is a complex array)" ) ; return
      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                             &
                          "<< erfc >>: an integer or a real matrix is expected" // NLT // &
                          "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                             &
                          "<< erfc >>: an integer or a real matrix is expected" // NLT // &
                          "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                             &
                          "<< erfc >>: an integer or a real matrix is expected" // NLT // &
                          "(here the argument of an unknown type)" ) ; return            
   end select         

   call pk2_moveAlloc ( from = tmpr, to = res )
   
   END SUBROUTINE pk2f_subERFC   
      

!=============================================================================================    
   FUNCTION pk2f_ERFC ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
      
!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(5), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('erfc',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the values of the complementary error function evaluated"
      h(2) = "           for each element of an integer or a real array" 
      h(3) = " "
      h(4) = "Syntax: B = "+f+"(A)" 
      h(5) = " "                       
      call pk2_assign ( res, h )
      return
   end if
        
   call pk2f_subERFC ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ERFC')
   
   END FUNCTION pk2f_ERFC   


!=============================================================================================    
   SUBROUTINE pk2f_subSQRT ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------  
!  Returns the square root of "a" (element-wise). The result is a matrix of the same shape,
!  real or complex. 
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subSQRT'
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)       
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< sqrt(a) >> with << a >> empty (--> sqrt(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         if ( any (p%v < IZERO) ) then
            tmpc = sqrt(cmplx(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpc, to = res )
         else   
            tmpr = sqrt(real(p%v,kind=Rkind))  ; call pk2_moveAlloc ( from = tmpr, to = res )
         end if            
      type is (rk2_t)
         if ( any (p%v < RZERO) ) then
            tmpc = sqrt(cmplx(p%v,kind=Rkind)) ; call pk2_moveAlloc ( from = tmpc, to = res )
         else   
            tmpr = sqrt(p%v)                   ; call pk2_moveAlloc ( from = tmpr, to = res )
         end if            
      type is (ck2_t)
         tmpc = sqrt(p%v)                      ; call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< sqrt >>: integer, real or complex matrix is expected" // NLT // &
                        "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< sqrt >>: integer, real or complex matrix is expected" // NLT // &
                        "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< sqrt >>: integer, real or complex matrix is expected" // NLT // &
                        "(here the argument of an unknown type)" ) ; return             
   end select      

   END SUBROUTINE pk2f_subSQRT


!=============================================================================================    
   FUNCTION pk2f_SQRT ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------  

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(5), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('sqrt',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the square root of an array (element-wise)." 
      h(2) = "           The result is a real or complex array of the same shape."
      h(3) = " "
      h(4) = "Syntax: B = "+f+"(A)"      
      h(5) = " "                         
      call pk2_assign ( res, h )
      return
   end if      
                     
   call pk2f_subSQRT ( a, res )  ; error_TraceNreturn(opflag, 'pk2f_SQRT')
   
   END FUNCTION pk2f_SQRT

 
!=============================================================================================    
   SUBROUTINE pk2f_subABS ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------  
!  Returns the absolute value or complex magnitude of each element of "a". The result is a 
!  matrix of the same shape and of integer or real type.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subABS'
   real     (Rkind), allocatable :: tmpr(:,:)       
   integer  (Ikind), allocatable :: tmpi(:,:)                                 
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< abs(a) >> with << a >> empty (--> abs(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         tmpi = abs(p%v) ; call pk2_moveAlloc ( from = tmpi, to = res )
      type is (rk2_t)
         tmpr = abs(p%v) ; call pk2_moveAlloc ( from = tmpr, to = res )
      type is (ck2_t)
         tmpr = abs(p%v) ; call pk2_moveAlloc ( from = tmpr, to = res )

      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                          "<< abs >>: integer, real or complex array is expected" // NLT // &
                          "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                          "<< abs >>: integer, real or complex array is expected" // NLT // &
                          "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                          "<< abs >>: integer, real or complex array is expected" // NLT // &
                          "(here the argument of an unknown type)" ) ; return          
   end select      
   
   END SUBROUTINE pk2f_subABS
   

!=============================================================================================    
   FUNCTION pk2f_ABS ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------  
!  Returns the absolute value or complex magnitude of each element of "a". The result is a 
!  matrix of the same shape and of integer or real type.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(5), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('abs',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the absolute value or complex magnitude of each "
      h(2) = "          element of an array"                    
      h(3) = " "
      h(4) = "Syntax: B = "+f+"(A)" 
      h(5) = " "                         
      call pk2_assign ( res, h )
      return
   end if
   
   call pk2f_subABS ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ABS')
   
   END FUNCTION pk2f_ABS


!=============================================================================================    
   SUBROUTINE pk2f_subFLOOR ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Rounds each element "a" to the greatest integer less than or equal to that element. The
!  result is a matrix of the same shape, of integer type if "a" is integer or real, of complex 
!  otherwise.
!     . If a is integer, res = a
!     . If a is complex, res = (floor(real(a)), floor(aimag(a)))
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter   :: HERE = 'pk2f_subFLOOR' 
   integer  (Ikind), parameter   :: IMAX = huge(1_Ikind)        
   logical                       :: overflow
   integer  (Ikind)              :: ireal, iimag, i, j
   real     (Rkind)              :: rreal, rimag   
   integer  (Ikind), allocatable :: tmpi(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)    
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< floor(a) >> with << a >> empty (--> floor(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   overflow = .false.
   
   select type (p=>a%m)
      type is (ik2_t)
         tmpi = p%v
         call pk2_moveAlloc ( from = tmpi, to = res )
         
      type is (rk2_t)
         allocate(tmpi(a%nrow,a%ncol))
         do j = 1, a%ncol
            do i = 1, a%nrow
               if ( p%v(i,j) < -IMAX ) then
                  tmpi(i,j) =-IMAX ; overflow = .true.
               else if ( p%v(i,j) > IMAX ) then
                  tmpi(i,j) = IMAX ; overflow = .true.
               else      
                  tmpi(i,j) = floor(p%v(i,j), kind=Ikind)
               end if  
            end do
         end do        
         call pk2_moveAlloc ( from = tmpi, to = res )
         
      type is (ck2_t)
         allocate(tmpc(a%nrow,a%ncol))
         do j = 1, a%ncol
            do i = 1, a%nrow
               rreal = real(p%v(i,j))
               if ( rreal >= IMAX ) then
                  ireal = IMAX ; overflow = .true.
               else if ( rreal <= -IMAX ) then
                  ireal =-IMAX ; overflow = .true.
               else
                  ireal = floor(rreal)
               end if
                     
               rimag = aimag(p%v(i,j))
               if ( rimag >= IMAX ) then
                  iimag = IMAX ; overflow = .true.
               else if ( rimag <= -IMAX ) then
                  iimag =-IMAX ; overflow = .true.
               else
                  iimag = floor(rimag)
               end if
                           
               tmpc(i,j) = cmplx(ireal,iimag, kind=Rkind)                  
            end do
         end do         
         call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< floor >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< floor >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< floor >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument of an unknown type)" ) ; return         
   end select      

   if ( overflow ) call opflag%set ( stat = WARNING, where = HERE,              &
                                      msg =  "Overflow occurred in << floor >>" )

   END SUBROUTINE pk2f_subFLOOR   
   

!=============================================================================================    
   FUNCTION pk2f_FLOOR ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------   
   type(str_t) :: h(12), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('floor',pk2f_helpcol)
      h( 1) = "<< "+f+" >> rounds each element of an array to the greatest integer less"
      h( 2) = "            than or equal to that element ("+f+"(x) = max{n, n <= x})."
      h( 3) = " "
      h( 4) = "Syntax: N = "+f+"(X)"                             
      h( 5) = " "
      h( 6) = "N is an array of the same shape of X"             
      h( 7) = " "
      h( 8) = " . if X is integer: N = X"
      h( 9) = " . if X is complex: N = "+f+"(real(X)) + i * "+f+"(imag(X))"
      h(10) = " "
      h(11) = "Note: overflow occurs if |X(i)| > Imax = "//util_intToChar(IMAX)
      h(12) = " "                         
      call pk2_assign ( res, h )
      return
   end if
          
   call pk2f_subFLOOR ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_FLOOR')
   
   END FUNCTION pk2f_FLOOR   
   

!=============================================================================================    
   SUBROUTINE pk2f_subCEIL ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Rounds each element of "a" to the smallest integer greater than or equal to that element.
!  The result is a matrix of the same shape, of integer type if "a" is integer or real, of 
!  complex type otherwise.
!     . If a is integer, res = a
!     . If a is complex, res = (ceil(real(a)), ceil(aimag(a)))
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------  
   character(len=*), parameter   :: HERE = 'pk2f_subCEIL'      
   integer  (Ikind), parameter   :: IMAX = huge(1_Ikind)   
   logical                       :: overflow
   integer  (Ikind)              :: ireal, iimag, i, j
   real     (Rkind)              :: rreal, rimag
   integer  (Ikind), allocatable :: tmpi(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)                                    
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< ceil(a) >> with << a >> empty (--> ceil(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   overflow = .false.
   
   select type (p=>a%m)
      type is (ik2_t)
         allocate(tmpi(a%nrow,a%ncol)) 
         tmpi(:,:) = p%v(:,:)
         call pk2_moveAlloc ( from = tmpi, to = res )
      type is (rk2_t)
         allocate(tmpi(a%nrow,a%ncol)) 
         do j = 1, a%ncol
            do i = 1, a%nrow
               if ( p%v(i,j) < -IMAX ) then
                  tmpi(i,j) =-IMAX ; overflow = .true.
               else if ( p%v(i,j) > IMAX ) then
                  tmpi(i,j) = IMAX ; overflow = .true.
               else      
                  tmpi(i,j) = ceiling(p%v(i,j), kind=Ikind)
               end if  
            end do
         end do        
         call pk2_moveAlloc ( from = tmpi, to = res )
         
      type is (ck2_t)
         allocate(tmpc(a%nrow,a%ncol))
         do j = 1, a%ncol
            do i = 1, a%nrow
               rreal = real(p%v(i,j))
               if ( rreal >= IMAX ) then
                  ireal = IMAX ; overflow = .true.
               else if ( rreal <= -IMAX ) then
                  ireal =-IMAX ; overflow = .true.
               else
                  ireal = ceiling(rreal)
               end if
                     
               rimag = aimag(p%v(i,j))
               if ( rimag >= IMAX ) then
                  iimag = IMAX ; overflow = .true.
               else if ( rimag <= -IMAX ) then
                  iimag =-IMAX ; overflow = .true.
               else
                  iimag = ceiling(rimag)
               end if
                           
               tmpc(i,j) = cmplx(ireal,iimag, kind=Rkind)                  
            end do
         end do         
         call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< ceil >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< ceil >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< ceil >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument of an unknown type)" ) ; return          
   end select      

   if ( overflow ) call opflag%set ( stat = WARNING, where = HERE,            &
                                      msg = "Overflow occurred in << ceil >>" )   
   
   END SUBROUTINE pk2f_subCEIL   
   

!=============================================================================================    
   FUNCTION pk2f_CEIL ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(12), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('ceil',pk2f_helpcol)
      h( 1) = "<< "+f+" >> rounds each element of an array to the smallest integer greater"
      h( 2) = "           than or equal to that element ("+f+"(x) = min{n, n >= x})" 
      h( 3) = " "
      h( 4) = "Syntax: N = "+f+"(X)"                              
      h( 5) = " "
      h( 6) = "N is an array of the same shape of X"             
      h( 7) = " "
      h( 8) = " . if X is integer: N = X"
      h( 9) = " . if X is complex: N = "+f+"(real(X)) + i * "+f+"(imag(X))"
      h(10) = " "
      h(11) = "Note: overflow occurs if |X(i)| > Imax = "//util_intTochar(IMAX)
      h(12) = " "                         
      call pk2_assign ( res, h )
      return
   end if
         
   call pk2f_subCEIL ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_CEIL')
   
   END FUNCTION pk2f_CEIL   


!=============================================================================================    
   SUBROUTINE pk2f_subNINT ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Rounds each element of "a" to its nearest integer
!  The result is a matrix of the same shape. Its type is integer if "a" is integer or real,
!  is complex if "a" is complex.
!     . If a is integer, res = a
!     . If a is complex, res = (nint(real(a)), nint(aimag(a)))
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subNINT'  
   integer  (Ikind), parameter   :: IMAX = huge(1_Ikind)   
   logical                       :: overflow
   integer  (Ikind)              :: ireal, iimag, i, j
   real     (Rkind)              :: rreal, rimag
   integer  (Ikind), allocatable :: tmpi(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)                                 
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< nint(a) >> with << a >> empty (--> nint(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   overflow = .false.
      
   select type (p=>a%m)
      type is (ik2_t)
         allocate(tmpi(a%nrow,a%ncol))
         tmpi(:,:) = p%v(:,:)
         call pk2_moveAlloc ( from = tmpi, to = res )
                  
      type is (rk2_t)
         allocate(tmpi(a%nrow,a%ncol))
         do j = 1, a%ncol
            do i = 1, a%nrow
               if ( p%v(i,j) < -IMAX ) then
                  tmpi(i,j) =-IMAX ; overflow = .true.
                else if ( p%v(i,j) > IMAX ) then
                   tmpi(i,j) = IMAX ; overflow = .true.
                else      
                   tmpi(i,j) = nint(p%v(i,j), kind=Ikind)
                end if  
            end do
         end do        
         call pk2_moveAlloc ( from = tmpi, to = res )

      type is (ck2_t)
         allocate(tmpc(a%nrow,a%ncol))
         do j = 1, a%ncol
            do i = 1, a%nrow
               rreal = real(p%v(i,j))
               if ( rreal >= IMAX ) then
                  ireal = IMAX ; overflow = .true.
               else if ( rreal <= -IMAX ) then
                  ireal =-IMAX ; overflow = .true.
               else
                  ireal = nint(rreal)
               end if
               rimag = aimag(p%v(i,j))
               if ( rimag >= IMAX ) then
                  iimag = IMAX ; overflow = .true.
               else if ( rimag <= -IMAX ) then
                  iimag =-IMAX ; overflow = .true.
               else
                  iimag = nint(rimag)
               end if
               tmpc(i,j) = cmplx(ireal,iimag, kind=Rkind)
                             
            end do
         end do         
         call pk2_moveAlloc ( from = tmpc, to = res )

      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< nint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< nint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< nint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument of an unknown type)" ) ; return      
   end select   
   
   if ( overflow ) call opflag%set ( stat = WARNING, where = HERE,            &
                                      msg = "Overflow occurred in << nint >>" )

   END SUBROUTINE pk2f_subNINT


!=============================================================================================    
   FUNCTION pk2f_NINT ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------   
   type(str_t) :: h(12), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('nint',pk2f_helpcol)
      h( 1) = "<< "+f+" >> rounds the elements of an array to their nearest integers "
      h( 2) = "           ("+f+"(x) = argmin { | n - x | })."
      h( 3) = " "
      h( 4) = "Syntax: N = "+f+"(X)"                          
      h( 5) = " "
      h( 6) = "N is an array of the same shape of X"
      h( 7) = " "
      h( 8) = " . if X is an integer: N = X"
      h( 9) = " . if X is complex   : N = "+f+"(real(X)) + i * "+f+"(imag(X))"
      h(10) = " "
      h(11) = "Note: overflow occurs if |X(i)| > Imax = "//util_intToChar(IMAX)
      h(12) = " "                         
      call pk2_assign ( res, h )
      return
   end if
        
   call pk2f_subNINT ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_NINT')
   
   END FUNCTION pk2f_NINT


!=============================================================================================    
   SUBROUTINE pk2f_subANINT ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!--------------------------------------------------------------------------------------------- 
!  Rounds each element of "a" up or down to its nearest whole number
!  The result is a matrix of the same shape and of the same type
!     . If a is integer,  res = a
!     . If a is complex,  res = (anint(real(a)), anint(aimag(a)))
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subANINT'
   integer  (Ikind), allocatable :: tmpi(:,:)       
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)  
   integer  (Ikind)              :: i, j 
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = " << anint(a) >> with << a >> empty (--> anint(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         allocate(tmpi(a%nrow,a%ncol))
         tmpi(:,:) = p%v(:,:)
         call pk2_moveAlloc ( from = tmpi, to = res )
      type is (rk2_t)
         allocate(tmpr(a%nrow,a%ncol))
         tmpr(:,:) = anint(p%v(:,:))
         call pk2_moveAlloc ( from = tmpr, to = res )
      type is (ck2_t)
         allocate(tmpc(a%nrow,a%ncol))
         do j = 1, a%ncol
            do i = 1, a%nrow
               tmpc(i,j)= cmplx(anint(real(p%v(i,j))),anint(aimag(p%v(i,j))), kind=Rkind)
            end do
         end do
         call pk2_moveAlloc ( from = tmpc, to = res )
      
      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< anint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< anint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                               &
                        "<< anint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument of an unknown type)" ) ; return           
   end select   
   
   END SUBROUTINE pk2f_subANINT


!=============================================================================================    
   FUNCTION pk2f_ANINT ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!--------------------------------------------------------------------------------------------- 

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(9), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('anint',pk2f_helpcol)
      h(1) = "<< "+f+" >> rounds elements of an array up or down to their nearest numbers"
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"
      h(4) = " "
      h(5) = "B is an array of the same shape of A"
      h(6) = " "
      h(7) = " . if A is an integer array: B = A"
      h(8) = " . if A is complex: B = "+f+"(real(A)) + i * "+f+"(imag(A))"
      h(9) = " "                         
      call pk2_assign ( res, h )
      return
   end if
        
   call pk2f_subANINT ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_ANINT')
   
   END FUNCTION pk2f_ANINT


!=============================================================================================    
   SUBROUTINE pk2f_subAINT ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Truncates each element of "a" by removing its fractional part.
!  The result is a matrix of the same shape and of the same type.
!     . If a is integer,  res = a
!     . If a is complex,  res = (aint(real(a)), aint(aimag(a)))
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subAINT'
   integer  (Ikind), allocatable :: tmpi(:,:)       
   real     (Rkind), allocatable :: tmpr(:,:)       
   complex  (Rkind), allocatable :: tmpc(:,:)  
   integer  (Ikind)              :: i, j        
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
         
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< aint(a) >> with << a >> empty (--> aint(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         allocate(tmpi(a%nrow,a%ncol))
         tmpi(:,:) = p%v(:,:)
         call pk2_moveAlloc ( from = tmpi, to = res ) 
      type is (rk2_t)
         allocate(tmpr(a%nrow,a%ncol))
         tmpr(:,:) = aint(p%v(:,:))
         call pk2_moveAlloc ( from = tmpr, to = res ) 
      type is (ck2_t)
         allocate(tmpc(a%nrow,a%ncol))
         do j = 1, a%ncol
            do i = 1, a%nrow
               tmpc(i,j) = cmplx(aint(real(p%v(i,j))),aint(aimag(p%v(i,j))), kind=Rkind)
            end do
         end do
         call pk2_moveAlloc ( from = tmpc, to = res ) 

      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< aint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< aint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< aint >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument of an unknown type)" ) ; return
   end select   
   
   END SUBROUTINE pk2f_subAINT


!=============================================================================================    
   FUNCTION pk2f_AINT ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(9), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('aint',pk2f_helpcol)
      h(1) = "<< "+f+" >> truncates the elements of an array to real whole numbers"
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"
      h(4) = " "
      h(5) = "B is an array of the same shape of A"
      h(6) = " "
      h(7) = " . if A is an integer array: B = A"
      h(8) = " . if A is complex: B = "+f+"(real(A)) + i * "+f+"(imag(A))"
      h(9) = " "                         
      call pk2_assign ( res, h )
      return
   end if
         
   call pk2f_subAINT ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_AINT')
   
   END FUNCTION pk2f_AINT


!=============================================================================================    
   SUBROUTINE pk2f_subSIGN ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the signum function evaluated for each element of "a". The result is an integer
!  matrix of the same shape.
!  For an integer or a real matrix we use:
!
!                       | -1    if x < 0
!              sgn(x) = | 
!                       | +1    otherwise 
!
!  For a complex matrix we use csgn definition:
!
!                        | -1             if Real(z) < 0,
!              csgn(z) = | +1             if Real(z) > 0, 
!                        | sign(Imag(z))  if Real(z) = 0.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subSIGN'
   integer  (Ikind)              :: i, j, err   
   integer  (Ikind), allocatable :: tmpi(:,:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< sign(a) >> with << a >> empty (--> sign(a) = [Ê])" )
      res = pk2_t()
      return
   end if

   err = 0
    
   select type (p=>a%m)
      type is (ik2_t)
         allocate(tmpi(a%nrow,a%ncol),stat=err)
         if ( err == 0 ) tmpi(:,:) = sign(IONE,p%v(:,:))
      type is (rk2_t)
         allocate(tmpi(a%nrow,a%ncol),stat=err)
         if ( err == 0 ) then
            do j = 1, a%ncol
               do i = 1, a%nrow
                  tmpi(i,j) = int(sign(RONE,p%v(i,j)),kind=Ikind)
               end do
            end do
         end if      
      type is (ck2_t)      
         allocate(tmpi(a%nrow,a%ncol),stat=err)
         if ( err == 0 ) then
            do j = 1, p%ncol
               do i = 1, p%nrow
                  if ( real(p%v(i,j)) > 0 ) then
                     tmpi(i,j) = IONE
                  else if ( real(p%v(i,j)) < 0 ) then
                     tmpi(i,j) =-IONE
                  else
                     tmpi(i,j) = int(sign(RONE,aimag(p%v(i,j))),kind=Ikind)
                  end if         
               end do
            end do      
         end if      
         
      type is (lk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< sign >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a logical array)" ) ; return
      type is (sk2_t)
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< sign >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument is a string array)" ) ; return
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                              &
                        "<< sign >>: integer, real or complex array is expected" // NLT // &
                        "(here the argument of an unknown type)" ) ; return      
   end select      

   if ( err /= 0 ) then
      call opflag%set (stat = IERROR, where = HERE, msg = "Allocation failure") 
      return
   end if 
   
   call pk2_moveAlloc ( from = tmpi, to = res )
      
   END SUBROUTINE pk2f_subSIGN


!=============================================================================================    
   FUNCTION pk2f_SIGN ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(16), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('sign',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the signum function evaluated for each element of an array"
      h( 2) = " "
      h( 3) = "Syntax: B = "+f+"(A)"
      h( 4) = " "
      h( 5) = " . B is an array of the same shape of A"
      h( 6) = " "
      h( 7) = " . if A is an integer or a real array:" 
      h( 8) = "               | -1 ....... if A(i,j) <  0,"
      h( 9) = "      B(i,j) = |                           "
      h(10) = "               | +1 ....... if A(i,j) >= 0."
      h(11) = " "      
      h(12) = " . if A is a complex array:" 
      h(13) = "               | -1 ................. if Real(A(i,j)) < 0,"
      h(14) = "      B(i,j) = | +1 ................. if Real(A(i,j)) > 0,"
      h(15) = "               | "+f+"(Imag(A(i,j))... if Real(A(i,j)) = 0."
      h(16) = " "                         
      call pk2_assign ( res, h )
      return
   end if   
               
   call pk2f_subSIGN ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_SIGN')
   
   END FUNCTION pk2f_SIGN


!=============================================================================================    
   SUBROUTINE pk2f_subHEAV ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the Heaviside's function evaluated for each element of "a". The result is an 
!  integer matrix of the same shape.
!  Note: "a" must be an integer or a real matrix.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subHEAV'
   integer  (Ikind), allocatable :: tmpi(:,:)    
   integer  (Ikind)              :: err  
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< heav(a) >> with << a >> empty (--> heav(a) = [Ê])" )
      res = pk2_t()
      return
   end if
   
   err = 0
       
   select type (p=>a%m)
      type is (ik2_t)
         allocate(tmpi(a%nrow,a%ncol), source = IZERO, stat = err)
         if ( err == 0 ) then
            where ( p%v >= IZERO )
               tmpi = IONE
            end where
         end if
         
      type is (rk2_t)
         allocate(tmpi(a%nrow,a%ncol), source = IZERO, stat = err)
         if ( err == 0 ) then
            where ( p%v >= RZERO )
               tmpi = IONE
            end where
         end if
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                &
                        "<< heav >>: an integer or a real array is expected" )
   end select      

   if ( err /= 0 ) then
      call opflag%set (stat = IERROR, where = HERE, msg = "Allocation failure") 
      return
   end if 
   
   call pk2_moveAlloc ( from = tmpi, to = res )
   
   END SUBROUTINE pk2f_subHEAV


!=============================================================================================    
   FUNCTION pk2f_HEAV ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(10), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('heav',pk2f_helpcol)
      h( 1) = "<< "+f+" >> evaluates the heaviside function "
      h( 2) = " "
      h( 3) = "Syntax: B = "+f+"(A)"
      h( 4) = " "
      h( 5) = "  A must be an integer or a real array and B has the same shape with:"
      h( 6) = " "
      h( 7) = "              | 0  if A(i,j) <  0" 
      h( 8) = "     B(i,j) = |                  "    
      h( 9) = "              | 1  if A(i,j) >= 0"  
      h(10) = " "    
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subHEAV ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_HEAV')
   
   END FUNCTION pk2f_HEAV


!=============================================================================================    
   SUBROUTINE pk2f_subGAMMA ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the Gamma function evaluated for each element of "a". The result is a real matrix
!  of the same shape.
!  Note: "a" must be an integer or a real matrix.
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subGAMMA'
   integer  (Ikind)              :: i, j, err
   real     (Rkind), allocatable :: tmpr(:,:)             
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, &
                         msg = "<< gamma(a) >> with << a >> empty (--> gamma(a) = [Ê])" )
      res = pk2_t()
      return
   end if
 
   err = 0
   
   select type (p=>a%m)
      type is (ik2_t)
         allocate(tmpr(a%nrow,a%ncol), stat = err)
         if ( err == 0 ) then
            do j = 1, a%ncol
               do i = 1, a%nrow
                  tmpr(i,j) = gamma(real(p%v(i,j),kind=Rkind))
               end do
            end do
         end if
      type is (rk2_t)
         allocate(tmpr(a%nrow,a%ncol), stat = err)
         if ( err == 0 ) then
            do j = 1, a%ncol
               do i = 1, a%nrow
                  tmpr(i,j) = gamma(p%v(i,j))
               end do
            end do
         end if
      class default
         call opflag%set ( stat = UERROR, where = HERE, msg =                     &
                            "<< gamma >>: an integer or a real array is expected" )
         return
   end select      
   
   if ( err /= 0 ) then
      call opflag%set (stat = IERROR, where = HERE, msg = "Allocation failure") 
      return
   end if 
   
   call pk2_moveAlloc ( from = tmpr, to = res )

   END SUBROUTINE pk2f_subGAMMA


!=============================================================================================    
   FUNCTION pk2f_GAMMA ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(6), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('gamma',pk2f_helpcol)
      h(1) = "<< "+f+" >> evaluates the gamma function "
      h(2) = " "
      h(3) = "Syntax: B = "+f+"(A)"
      h(4) = " "
      h(5) = "(A must be an integer or a real array)"
      h(6) = " "    
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subGAMMA ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_GAMMA')
   
   END FUNCTION pk2f_GAMMA


!=============================================================================================    
   SUBROUTINE pk2f_subFACTOR ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Returns the factorial function evaluated for each element of "a". The result is a real 
!  matrix of the same shape.
!  Notes: 
!     . "a" must be a matrix of positive integers. 
!     . The result is real (since integer overflow quickly)
!     . The gamma function is used here (n! = gamma(n+1))
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subFACTOR'
   integer  (Ikind)              :: i, j, err
   real     (Rkind), allocatable :: tmpr(:,:)           
!---------------------------------------------------------------------------------------------

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                        "<< factorial(a) >> with << a >> empty (--> factorial(a) = [Ê])" )
      res = pk2_t()                       
      return
   end if
 
   select type (p=>a%m)
      type is (ik2_t)
         if ( any(p%v < IZERO) ) then
            call opflag%set ( stat = UERROR, where = HERE,                        &
                               msg = "<< factorial >>: argument must be positive" )
            return
         end if   
         allocate(tmpr(a%nrow,a%ncol), stat = err)
         if ( err == 0 ) then
            do j = 1, a%ncol
               do i = 1, a%nrow
                  tmpr(i,j) = gamma(real(p%v(i,j),kind=Rkind)+RONE)
               end do
            end do
            call pk2_moveAlloc ( from = tmpr, to = res )
         else
            call opflag%set ( stat = IERROR, where = HERE, msg = "Allocation failure" ) 
         end if
      class default
         call opflag%set( stat = UERROR, where = HERE,                           &
                           msg = "<< factorial >>: an integer array is expected" )
   end select      

   END SUBROUTINE pk2f_subFACTOR


!=============================================================================================    
   FUNCTION pk2f_FACTOR ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(9), f1, f2
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f1 = str_color('factorial',pk2f_helpcol)
      f2 = str_color('gamma'    ,pk2f_helpcol)
      h(1) = "<< "+f1+" >> returns the factorial of the elements of an integer array"
      h(2) = " "
      h(3) = "Syntax: M = "+f1+"(N)"
      h(4) = " "
      h(5) = ". the elements of N must be all positive"
      h(6) = ". the result is computed in real (since integers overflow quickly)"
      h(7) = " "
      h(8) = "(Note: the gamma function is used in this version (n! = "+f2+"(n+1))"
      h(9) = " "    
      call pk2_assign ( res, h )
      return
   end if   

   call pk2f_subFACTOR ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_FACTOR')
   
   END FUNCTION pk2f_FACTOR


!=============================================================================================    
   FUNCTION pk2f_TRIM ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(6), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(a) ) then
      f = str_color('trim',pk2f_helpcol)
      h(1) = "<< "+f+" >> removes trailing and leading blank characters from each "
      h(2) = "           element of string array"
      h(3) = " "
      h(4) = "Syntax: b = "+f+"(a)"
      h(5) = " "
      h(6) = " "    
      call pk2_assign ( res, h )
      return
   end if   
      
   call pk2f_subTRIM ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_TRIM')
    
   END FUNCTION pk2f_TRIM                  


!=============================================================================================    
   SUBROUTINE pk2f_subTRIM ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Removes trailing and leading blank characters from each element of a (if approriate)
!-----------------------------------------------------------------------------------R.H. 04/18

!- local variables ---------------------------------------------------------------------------        
   character(len=*), parameter   :: HERE = 'pk2f_subTRIM'
   integer  (Ikind)              :: i, j, err
   type     (str_t), allocatable :: tmps(:,:)                                 
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

    if ( .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = WARNING, where = HERE, msg = &
                        'b = << trim(a) >> with << a >> not allocated (trim(a) = [ ]' )
      res = pk2_t()
      return
    end if    
        
    select type (p=>a%m)
       type is (sk2_t)
          allocate(tmps(a%nrow,a%ncol), stat = err)
          if ( err /= 0 ) then
             call opflag%set ( stat = IERROR, where = HERE, msg = "Allocation failure" )
             return
          end if
          do j = 1, a%ncol
             do i = 1, a%nrow
                if ( allocated(p%v(i,j)%str) ) tmps(i,j)%str = trim(adjustl(p%v(i,j)%str))
             end do
          end do
          call pk2_moveAlloc ( from = tmps, to = res ) 
          
       class default
          call opflag%set ( stat = UERROR, where = HERE,                         &
                             msg = "<< trim >>: an array of strings is expected" )
    end select

   END SUBROUTINE pk2f_subTRIM           


!=============================================================================================    
   SUBROUTINE pk2f_subMAGIC ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Creates a magic square of order n = a%v(1,1)
!-----------------------------------------------------------------------------------R.H. 06/18

!- local variables ---------------------------------------------------------------------------    
   character(len=*), parameter :: HERE = 'pk2f_subMAGIC'    
   integer  (Ikind)            :: n   
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ /= ITYP .or. .not. allocated(a%m) .or. a%nrow == 0 .or. a%ncol == 0 ) then
      call opflag%set ( stat = UERROR, where = HERE,                    &
                         msg = " << b = magic(a) >>: a must be integer" )
      return
   end if
   
   n = 0
   select type (p=>a%m)
      type is (ik2_t)         
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) n = p%v(1,1)
   end select
   
   if ( n < 0 ) then
      call opflag%set ( stat = UERROR, where = HERE,                               &
                         msg = " << b = magic(a) >>: a must be a positive integer" )
      return
   end if
      
   call pk2_SetToType ( typ = ITYP, shape = [n, n], res = res )
   
   error_TraceNreturn(opflag, HERE)
   
   select type(p=>res%m)
      type is (ik2_t)
         call util_magic (n, p%v)
   end select      

   error_TraceNreturn(opflag, HERE)

   END SUBROUTINE pk2f_subMAGIC


!=============================================================================================    
   FUNCTION pk2f_MAGIC ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(4), f
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color('magic',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns an n x n magic square"
      h(2) = " "
      h(3) = "Syntax: M = "+f+"(n)"
      h(4) = " "    
      call pk2_assign ( res, h )
      return
   end if   
             
   call pk2f_subMAGIC ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_MAGIC')
   
   END FUNCTION pk2f_MAGIC


!=============================================================================================    
   SUBROUTINE pk2f_subMAGICn ( n, res ) 
!=============================================================================================       
   integer(Ikind), intent(in    ) :: n
   class  (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Creates a magic square of order n
!-----------------------------------------------------------------------------------R.H. 06/18

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2f_subMAGICn'           
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( n < 0 ) then
      call opflag%set ( stat=UERROR, where=HERE, msg=" << b = magic(n) >>: n must be >= 0" )
      return
   end if

   call pk2_SetToType ( typ = ITYP, shape = [n, n], res = res )
   
   error_TraceNreturn(opflag, HERE)

   select type (p=>res%m)
      type is (ik2_t)
         call util_magic ( n, p%v )
   end select

   error_TraceNreturn(opflag, HERE)
            
   END SUBROUTINE pk2f_subMAGICn


!=============================================================================================    
   FUNCTION pk2f_MAGICn ( n ) result ( res ) 
!=============================================================================================       
   integer(Ikind), intent(in) :: n
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subMAGICn ( n, res ) ; error_TraceNreturn(opflag, 'pk2f_MAGICn')
   
   END FUNCTION pk2f_MAGICn


!=============================================================================================    
   SUBROUTINE pk2f_subDATE ( a, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: a
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Gets date and time
!-----------------------------------------------------------------------------------R.H. 07/18

!- local variables --------------------------------------------------------------------------- 
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( a%typ == EMPTY .or. .not. allocated(a%m) ) then
      res = pk2f_DATEstr ( )
      return
   end if
   
   select type (p=>a%m)
      type is (sk2_t)
         if ( allocated(p%v) .and. p%ncol >= 1 .and. p%nrow >= 1 ) then
            if ( allocated(p%v(1,1)%str) ) then
               res = pk2f_DATEstr ( option = (p%v(1,1)%str) )
            else
               res = pk2f_DATEstr ( )
            end if
         else     
            res = pk2f_DATEstr ( )
         end if      
      class default
         call opflag%set ( stat = UERROR, where = 'pk2f_subDATE', msg =                  &
                           " << date >>: valid uses: << date() >> or << date('val') >> " )   
   end select
                  
   END SUBROUTINE pk2f_subDATE


!=============================================================================================    
   FUNCTION pk2f_DATE ( a ) result ( res ) 
!=============================================================================================       
   class(pk2_t), intent(in) :: a
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subDATE ( a, res ) ; error_TraceNreturn(opflag, 'pk2f_DATE')
                     
   END FUNCTION pk2f_DATE


!=============================================================================================    
   SUBROUTINE pk2f_subDATEstr ( res, option ) 
!=============================================================================================
   class    (pk2_t),           intent(   out) :: res     
   character(len=*), optional, intent(in    ) :: option
!---------------------------------------------------------------------------------------------   
!  Gets date and time (adapted from util_timestamp, subroutine by John Burkardt)
!-----------------------------------------------------------------------------------R.H. 07/18

!- local variables ---------------------------------------------------------------------------        
   integer  (Ikind )              :: y, m, d, hr, n, s, mm
   integer  (kind=4)              :: values(8)
   character(len=99)              :: cdate                                   
   character(len=9 ), parameter   :: month(12) =   (/     &
                                    'January  ', 'February ', 'March    ', 'April    ',     &
                                    'May      ', 'June     ', 'July     ', 'August   ',     &
                                    'September', 'October  ', 'November ', 'December ' /)     
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( present(option) ) then
      if ( trim(util_StringLow ( option )) == 'val' ) then
         call date_and_time ( values = values )
         res = int(reshape(values,[1_4,8_4]),kind=Ikind)
      else
         call opflag%set ( stat = UERROR, where = 'pk2f_subDATEstr', msg =               &
                           " << date >>: valid uses: << date() >> or << date('val') >> " )      
         return
      end if
   else
      call date_and_time ( values = values )
      y  = values(1); m  = values(2); d  = values(3); hr  = values(5)
      n  = values(6); s  = values(7); mm = values(8)                
      write ( cdate, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3)' ) &
                     d, trim ( month(m) ), y, hr, ':', n, ':', s, '.', mm
      res = trim(cdate)
   end if
               
   END SUBROUTINE pk2f_subDATEstr


!=============================================================================================    
   FUNCTION pk2f_DATEstr ( option, help ) result ( res ) 
!=============================================================================================       
   character(len=*), optional, intent(in) :: option
   logical         , optional, intent(in) :: help
   type     (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(6), f
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( present(help) ) then
      f = str_color('date',pk2f_helpcol)
      h(1) = "<< "+f+" >> returns the current date"
      h(2) = " "
      h(3) = "Syntax: s = "+f+"() or s = "+f+"('val')"
      h(4) = " "
      h(5) = "(Note: adapted from John Burkardt's subroutine ""timestamp"")"     
      h(6) = " "    
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subDATEstr ( res, option ) ; error_TraceNreturn(opflag, 'pk2f_DATEstr')
                  
   END FUNCTION pk2f_DATEstr


!=============================================================================================    
   SUBROUTINE pk2f_subDATEwrp ( matrs, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Gets date and time
!-----------------------------------------------------------------------------------R.H. 07/18

!- local variables ---------------------------------------------------------------------------        
   integer(Ikind ) :: nargs
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   nargs = size(matrs)
   
   if ( nargs /= 0 .and. nargs /= 1 ) then
      call opflag%set ( stat = UERROR, where = 'pk2f_subDATEwrp',         &
                         msg = "0 or 1 argument expected for << date >> " )   
      return
   end if
      
   if ( nargs == 0 ) then
      res = pk2f_DATEstr ( )
   else if ( nargs == 1 ) then
      res = pk2f_DATE ( matrs(1) )
   end if
   
   error_TraceNreturn(opflag, 'pk2f_subDATEwrp')   
            
   END SUBROUTINE pk2f_subDATEwrp


!=============================================================================================    
   FUNCTION pk2f_DATEwrp ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), intent(in) :: matrs(:)
   type (pk2_t)             :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subDATEwrp ( matrs, res ) ; error_TraceNreturn(opflag, 'pk2f_DATEwrp')
   
   END FUNCTION pk2f_DATEwrp
   

!=============================================================================================    
   FUNCTION pk2f_MESHGRIDwrp ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------  
   type(str_t) :: h(9), f
!---------------------------------------------------------------------------------------------   

   if ( .not. present(matrs) ) then
      f = str_color('meshgrid',pk2f_helpcol)
      h(1) = "<< "+f+" >> 2-D or 3-D grids"
      h(2) = ""
      h(3) = "Syntax: XY = "+f+"(x,y) ; XYZ = "+f+"(x,y,z)"
      h(4) = ""
      h(5) = "where"
      h(5) = ". x, y, z are 1 x n or n x 1 matrices"
      h(6) = ". XY and XYZ are n x 2 and n x 3 matrices containing the node coordinates"
      h(7) = ""
      h(8) = "Example:   XY = "+f+" (0:0.1:2*%pi, 0:0.05:3)"
      h(9) = " "    
      call pk2_assign ( res, h )
      return
   end if
      
   call pk2f_subMESHGRIDwrp ( matrs, res ) ; error_TraceNreturn(opflag, 'pk2f_MESHGRIDwrp')
   
   END FUNCTION pk2f_MESHGRIDwrp    


!=============================================================================================    
   SUBROUTINE pk2f_subMESHGRIDwrp ( matrs, res ) 
!=============================================================================================       
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subMESHGRIDwrp' 
   integer  (Ikind)              :: nargs, restyp, typ, n, m, iarg
   type     (pk2_t), allocatable :: xyz(:)
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   nargs = size(matrs)
   
   if ( nargs /= 2 .and. nargs /= 3 ) then
      call opflag%set ( stat = UERROR, where = HERE,                           &
                         msg = "2 or 3 arguments expected for << meshgrid >> " )   
      return
   end if
   
   restyp = 0
   
   allocate(xyz(nargs))
      
   do iarg = 1, nargs
      n = matrs(iarg)%nrow ; m = matrs(iarg)%ncol ; typ = matrs(iarg)%typ
      
      if ( typ /= ITYP .and. typ /= RTYP ) then
         call opflag%set ( stat = UERROR, where = HERE, msg =                            &
                           "Arguments of << meshgrid >> must be integer or real vectors" )   
         return
      end if   
      
      restyp = max(restyp,typ)
            
      if ( n /= 1 .and. m /= 1 ) then
         call opflag%set ( stat = UERROR, where = HERE, msg =            &
                           "Arguments of << meshgrid >> must be vectors" )   
         return
      end if   
      
      if ( n /= 1 ) then
         call pk2f_subRESHAPEnm ( a=matrs(iarg), dim=[1_Ikind,n], res=xyz(iarg) )
         error_TraceNreturn(opflag, HERE)
      else
         xyz(iarg) = matrs(iarg)
      end if   
   end do
!
!- if necessary convert all in the same type:
!   
   if ( restyp == RTYP ) then
      do iarg = 1, nargs
         if ( xyz(iarg)%typ == ITYP ) xyz(iarg) = xyz(iarg) + RZERO
      end do
   end if   
   
   if ( nargs == 2 ) then
      if ( restyp == ITYP ) then
         call pk2f_meshgrid2di (xyz(1), xyz(2), res) 
      else        
         call pk2f_meshgrid2dr (xyz(1), xyz(2), res) 
      end if
   else           
      if ( restyp == ITYP ) then
         call pk2f_meshgrid3di (xyz(1), xyz(2), xyz(3), res)    
      else     
         call pk2f_meshgrid3dr (xyz(1), xyz(2), xyz(3), res)      
      end if
   end if      
   
   error_TraceNreturn(opflag, HERE)
                  
   END SUBROUTINE pk2f_subMESHGRIDwrp    


!=============================================================================================  
   SUBROUTINE pk2f_meshgrid2di ( x, y, res )
!=============================================================================================    
   class(pk2_t), intent(in    ) :: x, y
   class(pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------   
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'pk2f_meshgrid2di'   
   integer  (Ikind)            :: n, m, nx, ny, i, j, err
   integer  (Ikind)            :: yy
!---------------------------------------------------------------------------------------------   

   nx = x%ncol ; ny = y%ncol ; n = nx*ny ; m = 2
   
   allocate(ik2_t :: res%m)

   select type (r=>res%m)
      type is (ik2_t)
         
         allocate(r%v(n,m),stat = err)
         if ( err /= 0 ) then
            call opflag%set (stat = IERROR, where = HERE, msg = 'Allocation failure')
            return
         end if
         
         r%nrow = n ; r%ncol = m ; r%typ = ITYP     

         select type (q=>y%m)
            type is (ik2_t)
               select type (p=>x%m)
                  type is (ik2_t)
                     n = 0
                     do j = 1, ny
                        yy = q%v(1,j)
                        do i = 1, nx
                           n = n + 1
                           r%v(n,1) = p%v(1,i) ; r%v(n,2) = yy
                        end do
                     end do
               end select
         end select      
   end select          

   res%typ = ITYP ; res%nrow = n ; res%ncol = m
   
   END SUBROUTINE pk2f_meshgrid2di


!=============================================================================================  
   SUBROUTINE pk2f_meshgrid2dr ( x, y, res )
!=============================================================================================    
   class(pk2_t), intent(in    ) :: x, y
   class(pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------   
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'pk2f_meshgrid2dr'   
   integer  (Ikind)            :: n, m, nx, ny, i, j, err
   real     (Rkind)            :: yy
!---------------------------------------------------------------------------------------------   

   nx = x%ncol ; ny = y%ncol ; n = nx*ny ; m = 2
   
   allocate(rk2_t :: res%m)

   select type (r=>res%m)
      type is (rk2_t)
         
         allocate(r%v(n,m),stat = err)
         if ( err /= 0 ) then
            call opflag%set (stat = IERROR, where = HERE, msg = 'Allocation failure')
            return
         end if
         
         r%nrow = n ; r%ncol = m ; r%typ = RTYP     

         select type (q=>y%m)
            type is (rk2_t)
               select type (p=>x%m)
                  type is (rk2_t)
                     n = 0
                     do j = 1, ny
                        yy = q%v(1,j)
                        do i = 1, nx
                           n = n + 1
                           r%v(n,1) = p%v(1,i) ; r%v(n,2) = yy
                        end do
                     end do
               end select
         end select      
   end select          

   res%typ = RTYP ; res%nrow = n ; res%ncol = m
   
   END SUBROUTINE pk2f_meshgrid2dr


!=============================================================================================  
   SUBROUTINE pk2f_meshgrid3di (x, y, z, res)
!=============================================================================================    
   class(pk2_t), intent(in    ) :: x, y, z
   class(pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------   
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'pk2f_meshgrid3dr'   
   integer  (Ikind)            :: n, m, nx, ny, nz, i, j, k, err
   integer  (Ikind)            :: yy, zz
!---------------------------------------------------------------------------------------------   

   nx = x%ncol ; ny = y%ncol ; nz = z%ncol ; n = nx*ny*nz ; m = 3
   
   allocate(ik2_t :: res%m)

   select type (r=>res%m)
      type is (ik2_t)
      
         allocate(r%v(n,m),stat = err)
         if ( err /= 0 ) then
            call opflag%set (stat = IERROR, where = HERE, msg = 'Allocation failure')
            return
         end if

         r%nrow = n ; r%ncol = m ; r%typ = ITYP     
         
         select type (t=>z%m)
            type is (ik2_t)
               select type (q=>y%m)
                  type is (ik2_t)
                     select type (p=>x%m)
                        type is (ik2_t)
                           n = 0
                           do k = 1, nz
                              zz = t%v(1,k)
                              do j = 1, ny
                                 yy = q%v(1,j)
                                 do i = 1, nx
                                    n = n + 1
                                    r%v(n,1) = p%v(1,i) ; r%v(n,2) = yy ; r%v(n,3) = zz
                                 end do
                              end do
                           end do
                     end select
               end select
         end select            
   end select                  

   res%typ = ITYP ; res%nrow = n ; res%ncol = m
   
   END SUBROUTINE pk2f_meshgrid3di


!=============================================================================================  
   SUBROUTINE pk2f_meshgrid3dr ( x, y, z, res )
!=============================================================================================    
   class(pk2_t), intent(in    ) :: x, y, z
   class(pk2_t), intent(   out) :: res
!---------------------------------------------------------------------------------------------   
!-----------------------------------------------------------------------------------R.H. 03/19

!- local variables ---------------------------------------------------------------------------   
   character(len=*), parameter :: HERE = 'pk2f_meshgrid3dr'   
   integer  (Ikind)            :: n, m, nx, ny, nz, i, j, k, err
   real     (Rkind)            :: yy, zz
!---------------------------------------------------------------------------------------------   

   nx = x%ncol ; ny = y%ncol ; nz = z%ncol ; n = nx*ny*nz ; m = 3
   
   allocate(rk2_t :: res%m)

   select type (r=>res%m)
      type is (rk2_t)
      
         allocate(r%v(n,m),stat = err)
         if ( err /= 0 ) then
            call opflag%set (stat = IERROR, where = HERE, msg = 'Allocation failure')
            return
         end if

         r%nrow = n ; r%ncol = m ; r%typ = RTYP     
         
         select type (t=>z%m)
            type is (rk2_t)
               select type (q=>y%m)
                  type is (rk2_t)
                     select type (p=>x%m)
                        type is (rk2_t)
                           n = 0
                           do k = 1, nz
                              zz = t%v(1,k)
                              do j = 1, ny
                                 yy = q%v(1,j)
                                 do i = 1, nx
                                    n = n + 1
                                    r%v(n,1) = p%v(1,i) ; r%v(n,2) = yy ; r%v(n,3) = zz
                                 end do
                              end do
                           end do
                     end select
               end select
         end select            
   end select                  

   res%typ = RTYP ; res%nrow = n ; res%ncol = m
   
   END SUBROUTINE pk2f_meshgrid3dr


!=============================================================================================    
   SUBROUTINE pk2f_subREGLIN ( x, y, res, b, sig ) 
!=============================================================================================  
   class(pk2_t),           intent(in    ) :: x, y
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in out) :: b, sig
!---------------------------------------------------------------------------------------------   
!  Linear regression. 
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2f_subREGLIN'
   integer  (Ikind)            :: err
   type     (pk2_t)            :: coefa, coefb, sigma
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   err = 0
   if ( (x%ncol /= 1 .and. x%nrow /= 1) .or. (y%ncol /= 1 .and. y%nrow /= 1) ) then
      err = 1
   else if ( x%ncol == 1 .and. y%ncol /= 1 ) then
      err = 1
   else if ( x%nrow == 1 .and. y%nrow /= 1 ) then
      err = 1
   else if ( x%nrow * x%ncol /= y%nrow * y%ncol ) then
      err = 1
   end if      

   if ( err == 1 ) then
      call opflag%set ( stat = UERROR, where = HERE, msg =                                   & 
                       'x and y must be two vectors of the same shape in << reglin(x,y) >> ' )
      return
   end if
     
   ! a revoir:   
   coefa = extracmat ( cov(x,y,w=IONE), [2_Ikind], [1_Ikind] ) / cov(x, w=IONE)
   
   error_TraceNreturn(opflag, HERE)

   if ( present(b) ) then
      coefb = mean(y) - mean(x)*coefa
      if ( present(sig) ) then
         sigma = sqrt( cov( coefa*x + coefb - y, w=IONE ) ) 
         error_TraceNreturn(opflag, HERE)
         call pk2_moveAlloc ( from = sigma, to = sig )
      end if
      call pk2_moveAlloc ( from = coefb, to = b )
   end if   

   call pk2_moveAlloc ( from = coefa, to = res )
   
   END SUBROUTINE pk2f_subREGLIN


!=============================================================================================    
   FUNCTION pk2f_REGLIN ( x, y, b, sig ) result ( res ) 
!=============================================================================================  
   class(pk2_t), optional, intent(in    ) :: x, y
   class(pk2_t), optional, intent(in out) :: b, sig
   type (pk2_t)                           :: res
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(7), f
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if (.not. present(x) .and. .not. present(y)) then
      f = str_color('reglin',pk2f_helpcol)
      h(1) = "<< "+f+" >> simple linear regression" 
      h(2) = ""
      h(3) = "Syntax:  a = "+f+"(x,y),  [a,b] = "+f+"(x,y),  [a,b,sig] = "+f+"(x,y)"
      h(4) = ""
      h(5) = ". x, y: two vectors of the same length"
      h(5) = ". a, b: the coefficients of the simple regression model ymodel(x) = a*x + b "
      h(6) = ". sig : the standard deviation of the residuals ymodel(x) - y"
      h(7) = " "    
      call pk2_assign ( res, h )
      return
   else if ( .not. present(x) .or. .not. present(y) ) then
      call opflag%set ( stat = UERROR, & 
                         msg = 'in << reglin >> one of the first two arguments is missing' )
      return
   end if
      
   call pk2f_subREGLIN ( x, y, res, b, sig ) ; error_TraceNreturn(opflag, 'pk2f_REGLIN')
         
   END FUNCTION pk2f_REGLIN


!=============================================================================================    
   SUBROUTINE pk2f_subPWD ( res ) 
!=============================================================================================  
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Gets Path of Current Working Directory
!-----------------------------------------------------------------------------------R.H. 07/18

   res = util_pwd ( opflag ) ; error_TraceNreturn(opflag, 'pk2f_subPWD')
   
   END SUBROUTINE pk2f_subPWD


!=============================================================================================    
   FUNCTION pk2f_PWD ( help ) result ( res ) 
!=============================================================================================  
   logical    , optional, intent(in) :: help
   type(pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Gets Path of Current Working Directory
!-----------------------------------------------------------------------------------R.H. 07/18

!- local variables ---------------------------------------------------------------------------        
   type(str_t) :: h(7), f
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( present(help) ) then
      f = str_color('pwd',pk2f_helpcol)
      h(1) = '<< ' + f + " >> returns the name of the current directory (linux command)"
      h(2) = " "
      h(3) = "Syntax: " + f + " or " + f + "()"
      h(4) = " "
      h(5) = 'Examples:'
      h(6) = '  . ' + f
      h(7) = '  . s = ' + f + '() + "/foo"'        
      call pk2_assign ( res, h )
   else
      res = util_pwd ( opflag ) ; error_TraceNreturn(opflag, 'pk2f_PWD')
   end if
   
   END FUNCTION pk2f_PWD


!=============================================================================================    
   SUBROUTINE pk2f_subCPUTIME ( res ) 
!=============================================================================================   
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  
!-----------------------------------------------------------------------------------R.H. 07/18

!- local variables ---------------------------------------------------------------------------        
   real(Rkind) :: t
!---------------------------------------------------------------------------------------------   
      
   call cpu_time ( t ) ; res = t
   
   END SUBROUTINE pk2f_subCPUTIME


!=============================================================================================    
   FUNCTION pk2f_CPUTIME ( help ) result ( res ) 
!=============================================================================================   
   logical    , optional, intent(in) :: help
   type(pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  
!-----------------------------------------------------------------------------------R.H. 07/18

!- local variables ---------------------------------------------------------------------------        
   real(Rkind) :: t
   type(str_t) :: h(12), f
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( present(help) ) then
      f = str_color('cputime',pk2f_helpcol)
      h( 1) = "<< "+f+" >> returns the elapsed CPU time in seconds" 
      h( 2) = " "
      h( 3) = "Syntax: t = "+f+"()"
      h( 4) = " "
      h( 5) = "Note: the absolute value is meaningless, only differences between"
      h( 6) = "      subsequent calls to this function should be used." 
      h( 7) = " "
      h( 8) = "Example:"
      h( 9) = "      t0 = "+f+"()"
      h(10) = "       ..."
      h(11) = "      dt = "+f+"() - t0"
      h(12) = " "    
      call pk2_assign ( res, h )
      return
   end if   
   
   call cpu_time ( t ) ; res = t
   
   END FUNCTION pk2f_CPUTIME
   
   
!=============================================================================================    
   FUNCTION pk2f_tictoc ( ) result ( b ) 
!=============================================================================================       
   type(pk2_t) :: b
!---------------------------------------------------------------------------------------------   
!  
!-----------------------------------------------------------------------------------R.H. 07/18

!- local variables ---------------------------------------------------------------------------        
   integer(Ikind), save :: init = 0
   real          , save :: tstart, tfinish
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( init == 0 ) then
      call cpu_time(tstart)
      init = 1
   else
      call cpu_time(tfinish)
      b = real(tfinish - tstart, kind=Rkind)
   end if
   
   END FUNCTION pk2f_tictoc
   

!=============================================================================================    
   SUBROUTINE pk2f_subREADMATn ( res, unit, file, nrow, ncol, col, typ, convert, comment ) 
!=============================================================================================
   class    (pk2_t),           intent(in out) :: res 
   integer  (Ikind), optional, intent(in    ) :: unit, nrow, ncol, col(:)
   character(len=*), optional, intent(in    ) :: file, typ, convert, comment
!---------------------------------------------------------------------------------------------   
!  Reads an array (see pk2_ReadMat?)
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
!---------------------------------------------------------------------------------------------   
   
   call res%ReadMe ( unit, file, nrow, ncol, col, typ, convert, comment )
   
   error_TraceNreturn(opflag, 'pk2f_subREADMATn')
      
   END SUBROUTINE pk2f_subREADMATn


!=============================================================================================    
   FUNCTION pk2f_READMATn ( unit, file, nrow, ncol, col, typ, convert, comment, help ) &
                            result ( res )
!=============================================================================================  
   integer  (Ikind), optional, intent(in) :: unit, nrow, ncol, col(:)
   character(len=*), optional, intent(in) :: file, comment, typ, convert
   logical         , optional, intent(in) :: help
   type     (pk2_t)                       :: res   
!---------------------------------------------------------------------------------------------   
!  Reads an array (see pk2_ReadMat?)
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(48), f
!---------------------------------------------------------------------------------------------   

   if ( present(help) ) then
      f = str_color('readmat',pk2f_helpcol)
      h( 1) = "<< "+f+" >> reads an array from a file"
      h( 2) = " "
      h( 3) = "Syntax: M = "+f+" ( file=name [,nrow=n, ncol=m, col=col, typ=t,"
      h( 4) = "                                 convert=s, comment=symb] )"
      h( 5) = " "      
      h( 6) = "where "      
      h( 7) = " . name: name of the file "      
      h( 8) = " . n   : number of rows (-1 if unknown) "      
      h( 9) = " . m   : number of columns (-1 if unknown) "      
      h(10) = " . col : array of column indices "      
      h(11) = " . t   : array type ('i', 'r', 'c', 'l' or 's') "      
      h(12) = " . s   : type conversion ('i', 'r', 'c', 'l' or 's') "      
      h(13) = " . symb: symbol(s) used in the file for commented lines (default '#')"      
      h(14) = " "      
      h(15) = "Notes: "
      h(16) = " . If the option 'typ = t' is not used, the data type will be determined"
      h(17) = "   during the read. Commented-lines or blank lines will be skipped."
      h(18) = " . If the data type is known, it is more efficient to use the option 'typ'."
      h(19) = "   Note however, that in such case, commented-line may be present only in"
      h(20) = "   the header of the file (i.e. a number of lines at begining of the file "      
      h(21) = "   starting with a '#' or with symbol(s) given by the 'comment' option."      
      h(22) = " "      
      h(23) = "Examples: "      
      h(24) = " "      
      h(25) = " . 1) "+f+"('foo.txt') "
      h(26) = "      and foo.txt is a file containing an array in the form [.. , .. ; ..,]"  
      h(27) = " "      
      h(28) = " . 2) "+f+"(file='foo.txt',nrow=10,ncol=3,comment='//') "
      h(29) = "      and foo.txt is a file containing an array organized in columns "      
      h(30) = "      with potentially commented lines beginning with '//'."      
      h(31) = "      Only the first 3 columns and first 10 rows will be read."      
      h(32) = " "      
      h(33) = " . 3) "+f+"(file='foo.txt',nrow=10,ncol=3,comment='//',convert='s') "
      h(34) = "      same as 2) but the returned array is converted in string-array."      
      h(35) = " "      
      h(36) = " . 4) "+f+"(file='foo.txt',nrow=-1,ncol=3,comment='//',convert='s') "
      h(37) = "      same as 3) but read until the end-of-file."      
      h(38) = " "      
      h(39) = " . 5) "+f+"(file='foo.txt',nrow=-1,ncol=-1,comment='//',convert='s') "
      h(40) = "      same as 4) but all columns present in the file will be read."      
      h(41) = " "      
      h(42) = " . 6) "+f+"(file='foo.txt',nrow=-1,col=[3,1,5]) "
      h(43) = "      reads only the columns #3, #1 and #5 (and stores them in this order)."      
      h(44) = " "      
      h(45) = " . 7) "+f+"(file='foo.txt',nrow=-1,col=[3,1,5],typ='r') "
      h(46) = "      if we know that the data of columns #3, #1 and #5 can be read with a"
      h(47) = "      type given by 'typ' (recommended because faster than 6)."      
      h(48) = " "    
      call pk2_assign ( res, h )
      return
   end if   
   
   call res%ReadMe ( unit, file, nrow, ncol, col, typ, convert, comment )

   error_TraceNreturn(opflag, 'pk2f_READMATn')
      
   END FUNCTION pk2f_READMATn


!=============================================================================================    
   SUBROUTINE pk2f_subREADMATwrp ( matrs, res )
!=============================================================================================    
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Reads an array
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subREADMATwrp'
   integer  (Ikind)              :: narg, n, iarg, ncol, nrow, colsize, ltyp, err
   integer  (Ikind), allocatable :: col(:)
   character(len=:), allocatable :: fname, com, argname
   character(len=1)              :: conv, typ
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   narg = size(matrs)
   if (narg < 1) return
      
   nrow = 0 ; ncol = 0 ; colsize = 0 ; ltyp = 0 
   fname = '' ; typ = '' ; conv = '' ; com = ''
    
   if ( narg == 1 ) then
      select type (p=>matrs(1)%m)
         type is (sk2_t)
            fname = p%v(1,1)%str
         class default
            call opflag%set ( stat = UERROR, where = HERE,                   &
                               msg = '<< readmat >> A file name is expected' )
            return
      end select
   else      
      select type (p=>matrs(1)%m)
         type is (ik2_t)
            n = p%v(1,1)
         class default
            call opflag%set ( stat = UERROR, where = HERE, msg = '<< readmat >> (error #1)' )
            return
      end select            
      if ( narg-1 /= 2*n ) then
         call opflag%set ( stat = UERROR, where = HERE, msg = '<< readmat >> (error #2)' )
         return
      end if
   
      do iarg = 2, n+1
         if ( matrs(iarg)%typ /= STYP .or. .not. allocated(matrs(iarg)%m) .or. &
              matrs(iarg)%nrow == 0 .or. matrs(iarg)%ncol == 0 ) then
            call opflag%set ( stat = UERROR, where = HERE, msg = '<< readmat >> (error #3)' )
            return
         end if
         if ( .not. allocated(matrs(iarg+n)%m) .or. matrs(iarg+n)%nrow == 0 .or. &
              matrs(iarg+n)%ncol == 0 ) then
            call opflag%set ( stat = UERROR, where = HERE, msg = '<< readmat >> (error #4)' )
            return
         end if
      end do
      
      err = 0
      do iarg = 2, n+1
         select type (p=>matrs(iarg)%m)
            type is (sk2_t)
               argname = trim(adjustl(p%v(1,1)%str))
               if ( argname == "file" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        fname = q%v(1,1)%str
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "nrow" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (ik2_t)
                        nrow = q%v(1,1)
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "ncol" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (ik2_t)
                        ncol = q%v(1,1)
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "col" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (ik2_t)
                        colsize = q%nrow*q%ncol
                        allocate(col(colsize), source = pack(q%v, mask = .true.))
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "typ" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        typ = (q%v(1,1)%str(1:1))
                        ltyp = len_trim(typ)
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "convert" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        conv = (q%v(1,1)%str(1:1))
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "comment" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        com = (q%v(1,1)%str)
                     class default
                        err = 1 ; exit
                  end select
               else
                  call opflag%set (stat = UERROR, where = HERE, msg = &
                                  '<< readmat >> Invalid option (error #5)')
                  return
               end if                                
            class default
               err = 1 ; exit
               return
         end select         
      end do  
      if ( err /= 0 ) then
         call opflag%set ( stat = UERROR, where = HERE, msg = '<< readmat >> (error #6)' )
         return
      end if   
      
   end if

   if ( len_trim(fname) == 0 ) then
       call opflag%set ( stat = UERROR, where = HERE,                   &
                          msg = '<< readmat >> A file name is expected' )
       return
   end if
   
   if      ( nrow == 0 .and. ncol == 0 .and. colsize == 0 .and. ltyp == 0 ) then !-> pk2_readmat1
      call res%ReadMe (fname=fname, comment=com)
   else if ( nrow /= 0 .and. ncol /= 0 .and. colsize == 0 .and. ltyp == 0 ) then !-> pk2_readmat2
      call res%ReadMe (fname=fname, nrow=nrow, ncol=ncol, comment=com, convert=conv)
   else if ( nrow /= 0 .and. ncol == 0 .and. colsize /= 0 .and. ltyp == 0 ) then !-> pk2_readmat2 
      call res%ReadMe (fname=fname, nrow=nrow, colindx=col, comment=com, convert=conv)
   else if ( nrow /= 0 .and. ncol /= 0 .and. colsize == 0 .and. ltyp /= 0 ) then !-> pk2_readmat3
      call res%ReadMe (fname=fname, nrow=nrow, ncol=ncol, typ=typ, comment=com)
   else if ( nrow /= 0 .and. ncol == 0 .and. colsize /= 0 .and. ltyp /= 0 ) then !-> pk2_readmat3 
      call res%ReadMe (fname=fname, nrow=nrow, colindx=col, typ=typ, comment=com)
   else
      call opflag%set ( stat = UERROR, where = HERE,                     &
                         msg = '<< readmat >> Invalid choice of options' )
      return
   end if
   
   error_TraceNreturn(opflag, HERE)
    
   END SUBROUTINE pk2f_subREADMATwrp


!=============================================================================================    
   FUNCTION pk2f_READMATwrp ( matrs ) result ( res )
!=============================================================================================    
   class(pk2_t), intent(in) :: matrs(:)
   type (pk2_t)             :: res   
!---------------------------------------------------------------------------------------------   
!  Reads an array
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
!---------------------------------------------------------------------------------------------   

   call pk2f_subREADMATwrp ( matrs, res ) ; error_TraceNreturn(opflag, 'pk2f_READMATwrp')
   
   END FUNCTION pk2f_READMATwrp


!=============================================================================================    
   SUBROUTINE pk2f_subWRITEMAT ( matrs, status )
!=============================================================================================    
   class(pk2_t),           intent(in    ) :: matrs(:)
   class(pk2_t), optional, intent(in out) :: status
!---------------------------------------------------------------------------------------------   
!  Writes an array
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subWRITEMAT'
   integer  (Ikind)              :: narg, n, iarg, imat, err   
   character(len=:), allocatable :: fname, com, title, fmt, qt, argname, msg
   logical                       :: siz
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   narg = size(matrs)
   
   if ( narg < 1 ) then
      if (present(status)) status = '<< writemat >>: Nothing to write'
      return
   end if
                  
   fname = '' ; com = '' ; title = '' ; fmt = '' ; qt = '' ; siz = .false. ; imat = 0
    
   if ( narg == 1 ) then
      select type (p=>matrs(1)%m)
         type is (sk2_t)
            fname = p%v(1,1)%str
         class default
            msg = '<< writemat >> A file name is expected (error #1)'
            call opflag%set ( stat = UERROR, where = HERE, msg = msg )
            return
      end select
   else      
      select type (p=>matrs(1)%m)
         type is (ik2_t)
            n = p%v(1,1)
         class default
            msg = 'in << writemat >> (error #2)'
            call opflag%set ( stat = UERROR, where = HERE, msg = msg )
            return
      end select            
      if ( narg-1 /= 2*n ) then
         msg = 'in << writemat >> (error #3)'
         call opflag%set ( stat = UERROR, where = HERE, msg = msg )
         return
      end if
   
      do iarg = 2, n+1
         if ( matrs(iarg)%typ /= STYP .or. .not. allocated(matrs(iarg)%m) .or. &
              matrs(iarg)%nrow == 0 .or. matrs(iarg)%ncol == 0) then
            msg = 'in << writemat >> (error #4)'
            call opflag%set ( stat = UERROR, where = HERE, msg = msg )
            return
         end if
         if ( .not. allocated(matrs(iarg+n)%m) .or. matrs(iarg+n)%nrow == 0 .or. &
              matrs(iarg+n)%ncol == 0) then
            msg = 'in << writemat >> (error #5)'
            call opflag%set ( stat = UERROR, where = HERE, msg = msg )
            return
         end if
      end do
      
      err = 0
      
      do iarg = 2, n+1
         select type (p=>matrs(iarg)%m)
            type is (sk2_t)
               argname = trim(adjustl(p%v(1,1)%str))
               if ( argname == "file" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        fname = q%v(1,1)%str
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "title" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        title = q%v(1,1)%str
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "format" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        fmt = q%v(1,1)%str
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "comment" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        com = q%v(1,1)%str
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "quote" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (sk2_t)
                        qt = q%v(1,1)%str
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "size" ) then
                  select type (q=>matrs(iarg+n)%m)
                     type is (lk2_t)
                        siz = q%v(1,1)
                     class default
                        err = 1 ; exit
                  end select
               else if ( argname == "mat" ) then
                  imat = iarg+n
               else
                  msg = '<< writemat >> Invalid option (error #6)'
                  call opflag%set ( stat = UERROR, where = HERE, msg = msg )
                  return
               end if                                
            class default
               err = 1 ; exit
               return
         end select         
      end do  
      if ( err /= 0 ) then
         msg = '<< writemat >> (error #7)'
         call opflag%set ( stat = UERROR, where = HERE, msg = msg )
         return
      end if   
      
   end if

   if ( imat == 0 ) then
      status = '<< writemat >>: Nothing to write'
      return
   end if

   if ( len_trim(fname) == 0 ) then
      msg = '<< writemat >> A file name is expected (error #8)'
      call opflag%set ( stat = UERROR, where = HERE, msg = msg )
      return
   end if
      
   call matrs(imat)%WriteMe (fname  = fname, comment = com, title = title, &
                             format = fmt  , quote   = qt , size  = siz  )

   error_TraceNreturn(opflag, HERE)
      
   if ( present(status) ) status = '<< writemat >>: Done'
   
   END SUBROUTINE pk2f_subWRITEMAT
   

!=============================================================================================    
   FUNCTION pk2f_WRITEMAT ( matrs ) result ( res )
!=============================================================================================    
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res   
!---------------------------------------------------------------------------------------------   
!  Writes an array
!-----------------------------------------------------------------------------------R.H. 01/19

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(21), f
!---------------------------------------------------------------------------------------------   
   
   if ( .not. present(matrs) ) then
      f = str_color('writemat',pk2f_helpcol)
      h( 1) = "<< "+f+" >> writes an array to a file"
      h( 2) = " "
      h( 3) = "Syntax: "+f+" ( mat=array, file=name, [,title=t, format=fmt, quote=qt,"
      h( 4) = "                                           comment=symb, size=sz] )"
      h( 5) = " "      
      h( 6) = "where "      
      h( 7) = " . array: the array to write"        
      h( 8) = " . name : (string) name of the file"  
      h( 9) = " . t    : (string) a title to be written on the top of the file (header)"  
      h(10) = " . fmt  : (string) a format (fortran based)"  
      h(11) = " . qt   : (string) for string array enclose data between this pair of symbols"  
      h(12) = " . symb : (string) symbol to use for comments (default: '#')"  
      h(13) = " . sz   : (boolean) print the array size (on the header)"  
      h(14) = " "      
      h(15) = "Examples: "      
      h(16) = ". "+f+"(mat=A, file='out') "      
      h(17) = ". "+f+"(mat=A, file='out', title='my title', format='(2e13.5)') "      
      h(18) = ". "+f+"(mat=A, file='out', comment='//', size = %t) "      
      h(19) = ". "+f+"(mat=A, file='out', quote ='""""') "      
      h(20) = ". "+f+"(mat=A, file='out', quote ='{}') "      
      h(21) = " "      
      call pk2_assign ( res, h )
      return
   end if

   call pk2f_subWRITEMAT ( matrs = matrs, status = res )
   
   error_TraceNreturn(opflag, 'pk2f_WRITEMAT')
   
   END FUNCTION pk2f_WRITEMAT


!=============================================================================================    
   SUBROUTINE pk2f_subPARTn ( a, indx, res )
!=============================================================================================  
   class  (pk2_t), intent(in    ) :: a  
   integer(Ikind), intent(in    ) :: indx(:) 
   class  (pk2_t), intent(in out) :: res  
!---------------------------------------------------------------------------------------------
!  Extraction of characters from strings 
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subPARTn'
   integer  (Ikind)              :: i, j, k, l, n, lstr, err
   character(len=:), allocatable :: str1, str2
   type     (str_t), allocatable :: tmps(:,:)
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ /= STYP ) then
      call opflag%set ( stat = UERROR, where = HERE,                                    &
                         msg = 'in << part(A,indx) >> a string array is expected for A' )
      return
   end if

   n = size(indx)
   
   if ( a%nrow == 0 .or. a%ncol == 0 .or. n == 0 ) return

   if  (any(indx <= 0) ) then
      call opflag%set ( stat = UERROR, where = HERE,                            &
                         msg = 'in << part(A,indx) >> all indx(i) must be >= 1' )
      return   
   end if
      
   allocate(tmps(a%nrow,a%ncol), stat = err)
   
   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure' )
      return
   end if
   
   select type (p=>a%m)
      type is (sk2_t)
         do j = 1, p%ncol
            do i = 1, p%nrow
               if ( allocated(p%v(i,j)%str) ) then
                  str1 = (p%v(i,j)%str) ; lstr = len(str1)
               else
                  str1 = '' ; lstr = 0
               end if
               if ( allocated(tmps(i,j)%str) ) then
                  str2 = (tmps(i,j)%str)
               else
                  str2 = ''
               end if
               do k = 1, n
                  l = indx(k)
                  if ( l <= lstr ) then
                     tmps(i,j)%str = (str2) // (str1(l:l))
                  else
                     tmps(i,j)%str = (str2) // ' '
                  end if
               end do
            end do
         end do            
   end select
   
   call pk2_moveAlloc ( from = tmps, to = res )
    
   END SUBROUTINE pk2f_subPARTn        


!=============================================================================================    
   FUNCTION pk2f_PARTn ( a, indx ) result ( res )
!=============================================================================================  
   class  (pk2_t), intent(in) :: a  
   integer(Ikind), intent(in) :: indx(:) 
   type   (pk2_t)             :: res
!---------------------------------------------------------------------------------------------   
      
   call pk2f_subPARTn ( a, indx, res ) ; error_TraceNreturn(opflag, 'pk2f_PARTn')
    
   END FUNCTION pk2f_PARTn


!=============================================================================================    
   SUBROUTINE pk2f_subPART ( a, b, res )
!=============================================================================================  
   class(pk2_t), intent(in    ) :: a, b 
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subPART'
   integer  (Ikind)              :: i, j, k, l, n, lstr, err
   logical                       :: is_row
   character(len=:), allocatable :: str1, str2
   type     (str_t), allocatable :: tmps(:,:)
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( a%typ /= STYP ) then
      call opflag%set ( stat = UERROR, where = HERE,                                    &
                         msg = 'in << part(A,Indx) >> a string array is expected for A' )
      return
   end if
   
   if ( b%typ /= ITYP .or. (b%nrow /= 1 .and. b%ncol /= 1) ) then
      call opflag%set (stat = UERROR, where = HERE,                                          &
                        msg = '(in << part(A,Indx) >> an integer vector is expected for Indx')
      return
   end if

   n = b%nrow * b%ncol
   
   if ( a%nrow == 0 .or. a%ncol == 0 .or. n == 0 ) return

   allocate(tmps(a%nrow,a%ncol), stat = err)
   
   if ( err /= 0 ) then
      call opflag%set ( stat = IERROR, where = HERE, msg = 'Allocation failure' )
      return
   end if
   
   is_row = ( b%nrow == 1 )
      
   select type (q=>b%m)
      type is (ik2_t)
         
         do k = 1, n
            if ( is_row ) then
               l = q%v(1,k)
            else 
               l = q%v(k,1)
            end if      
            
            if ( l <= 0 ) then
               call opflag%set (stat = UERROR, where = HERE, &
                                 msg = 'in << part(A,Indx) >> Indx(:) must be >= 1' )
               return
            end if
              
            select type (p=>a%m)
               type is (sk2_t) 
                  do j = 1, p%ncol
                     do i = 1, p%nrow
                        if ( allocated(p%v(i,j)%str) ) then
                           str1 = (p%v(i,j)%str) ; lstr = len(str1)
                        else
                           str1 = '' ; lstr = 0
                        end if
                        if ( allocated(tmps(i,j)%str) ) then
                           str2 = (tmps(i,j)%str)
                        else
                           str2 = ''
                        end if
                        if ( l <= lstr ) then
                           tmps(i,j)%str = (str2) // (str1(l:l))
                        else
                           tmps(i,j)%str = (str2) // ' '
                        end if
                     end do
                  end do
            end select
         end do
   end select
   
   call pk2_moveAlloc ( from = tmps, to = res )
    
   END SUBROUTINE pk2f_subPART    
   

!=============================================================================================    
   FUNCTION pk2f_PART ( a, b ) result ( res )
!=============================================================================================  
   class(pk2_t), intent(in), optional :: a, b 
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2f_PART'
   type     (str_t)            :: h(6), f
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) .and. .not. present(b) ) then
      f = str_color('part',pk2f_helpcol)
      h(1) = "<< "+f+" >> Extraction of characters from strings"
      h(2) = " "
      h(3) = "Syntax:  "+f+"(S,Indx)"
      h(4) = " "      
      h(5) = "where S is an array of strings and Indx is an array of integers"
      h(6) = " "    
      call pk2_assign ( res, h )
      return
   end if   
   
   if ( .not. present(a) ) then
      call opflag%set ( stat = UERROR, where = HERE,                         &
                         msg = 'Missing the 1st argument argument of "part"' )
      return
   end if

   if ( .not. present(b) ) then
      call opflag%set ( stat = UERROR, where = HERE,                         &
                         msg = 'Missing the 2nd argument argument of "part"' )
      return
   end if
      
   call pk2f_subPART ( a, b, res ) ; error_TraceNreturn(opflag, HERE)
   
   END FUNCTION pk2f_PART    


!=============================================================================================    
   SUBROUTINE pk2f_subCONVSTR ( a, b, res )
!=============================================================================================  
   class(pk2_t),           intent(in    ) :: a  
   class(pk2_t),           intent(in out) :: res 
   class(pk2_t), optional, intent(in    ) :: b  
!---------------------------------------------------------------------------------------------   
!  Converts the string array "a" to
!  . lowercase, if "b" is not present or present with the value 'l'
!  . uppercase, if "b" is present with the value 'u'.
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer  (Ikind) :: err
   character(len=1) :: dir
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( present(b) ) then    
      err = 0  
      if ( allocated(b%m) .and. b%nrow >= 1 .and. b%ncol >= 1 ) then
         select type (p=>b%m)
            type is (sk2_t)
               dir = (p%v(1,1)%str)
            class default
               err = 1
         end select
      else
         err = 1
      end if
      
      if ( err /= 0 ) then
         call opflag%set ( stat = UERROR, where = 'pk2f_subCONVSTR',               &
                            msg = 'in << convstr(A,dir) >> dir must be "l" or "u"' )
         return
      end if   
   else
      dir = 'l'   
   end if
         
   call pk2f_subCONVSTRn ( a, dir, res ) ; error_TraceNreturn(opflag, 'pk2f_subCONVSTR')
      
   END SUBROUTINE pk2f_subCONVSTR


!=============================================================================================    
   FUNCTION pk2f_CONVSTR ( a, b ) result ( res )
!=============================================================================================  
   class(pk2_t),           intent(in) :: a  
   class(pk2_t), optional, intent(in) :: b  
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subCONVSTR ( a, b, res ) ; error_TraceNreturn(opflag, 'pk2f_CONVSTR')
      
   END FUNCTION pk2f_CONVSTR


!=============================================================================================    
   SUBROUTINE pk2f_subCONVSTRn ( a, dir, res )
!=============================================================================================  
   class    (pk2_t), intent(in    ) :: a  
   character(len=1), intent(in    ) :: dir
   class    (pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Converts the string array "a" to
!  . lowercase, if dir = 'l'
!  . uppercase, if dir = 'u'
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   integer(Ikind) :: i, j
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ /= STYP ) then
      call opflag%set ( stat = UERROR, where = 'pk2f_subCONVSTRn',                          &
                         msg = 'in << convstr(A[,dir]) >> a string array is expected for A' )
      return
   end if
      
   call pk2_assign ( res, a ) ; error_TraceNreturn(opflag, 'pk2f_subCONVSTRn')
   
   select type (p=>res%m)
      type is (sk2_t)
         if ( dir == 'l' ) then
            do j = 1, p%ncol
               do i = 1, p%nrow
                  p%v(i,j)%str = util_StringLow(p%v(i,j)%str)
               end do
            end do
         else if ( dir == 'u' ) then
            do j = 1, p%ncol
               do i = 1, p%nrow
                  p%v(i,j)%str = util_StringCap(p%v(i,j)%str)
               end do
            end do
         else    
            call opflag%set ( stat = UERROR, where = 'pk2f_subCONVSTRn',              &
                               msg = 'in << convstr(A,dir) >> dir must be "l" or "u"' )
            return         
         end if   
   end select      
   
   END SUBROUTINE pk2f_subCONVSTRn


!=============================================================================================    
   FUNCTION pk2f_CONVSTRn ( a, dir ) result ( res )
!=============================================================================================  
   class    (pk2_t), intent(in) :: a  
   character(len=1), intent(in) :: dir
   type     (pk2_t)             :: res
!---------------------------------------------------------------------------------------------   

   call pk2f_subCONVSTRn ( a, dir, res ) ; error_TraceNreturn(opflag, 'pk2f_CONVSTRn')
   
   END FUNCTION pk2f_CONVSTRn


!=============================================================================================    
   SUBROUTINE pk2f_subCONVSTRwrp ( matrs, res )
!=============================================================================================  
   class(pk2_t), intent(in    ) :: matrs(:)
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------   
!  Converts the string array matrs(1) to lowercase or to uppercase
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   character(len=1) :: dir
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   dir = 'l'
   
   if ( size(matrs) >= 2 ) then
      dir = 'l'
      if ( allocated(matrs(2)%m) ) then
         select type (p=>matrs(2)%m)
            type is (sk2_t)
               if ( p%nrow * p%ncol >= 1 ) dir = p%v(1,1)%str
            class default
               call opflag%set ( stat = UERROR, where = 'pk2f_subCONVSTRwrp',            &
                                  msg = 'in << convstr(A,dir) >> dir must be "l" or "u"' )
               return
         end select
      end if
   else if ( size(matrs) < 1 ) then
      call opflag%set ( stat = UERROR, where = 'pk2f_subCONVSTRwrp',                      &
                         msg = '1 or 2 arguments expected for the function << convstr >>' )
      return
   end if
   
   call pk2f_subCONVSTRn ( matrs(1), dir, res )
   
   error_TraceNreturn(opflag, 'pk2f_subCONVSTRwrp')
   
   END SUBROUTINE pk2f_subCONVSTRwrp


!=============================================================================================    
   FUNCTION pk2f_CONVSTRwrp ( matrs ) result ( res )
!=============================================================================================  
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   type(str_t) :: h(8), f
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( .not. present(matrs) ) then
      f = str_color('convstr',pk2f_helpcol)
      h(1) = "<< "+f+" >> converts a string array to lowercase or uppercase"
      h(2) = " "
      h(3) = "Syntax: "+f+"(S),  "+f+"(S,'l'),  "+f+"(S,'u')"
      h(4) = " "      
      h(5) = ". "+f+"(S,'l'): lowercase conversion"
      h(6) = ". "+f+"(S,'u'): uppercase conversion"
      h(7) = ". "+f+"(S)    : same as "+f+"(S,'l')"
      h(8) = " "    
      call pk2_assign ( res, h )
      return
   end if   
   
   call pk2f_subCONVSTRwrp ( matrs, res ) ; error_TraceNreturn(opflag, 'pk2f_CONVSTRwrp')
   
   END FUNCTION pk2f_CONVSTRwrp


!=============================================================================================    
   SUBROUTINE pk2f_subREPLACE ( a, rmv, rpl, opcl, res, nrep )
!=============================================================================================  
   class    (pk2_t),           intent(in    ) :: a, rmv, rpl
   character(len=*),           intent(in    ) :: opcl 
   class    (pk2_t),           intent(in out) :: res   
   integer  (Ikind), optional, intent(   out) :: nrep
!---------------------------------------------------------------------------------------------  
!  Replaces all occurences of the substring(s) "rmv" with substrings "rpl" in the string "a".
!  Substrings between one of the opening/closing pairs given in "opcl" are ignored. 
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subREPLACE'
   character(len=:), allocatable :: remove, replace, str
   integer  (Ikind)              :: i, j, p, q, n, nrp
   type     (pk2_t)              :: tmp
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ /= STYP .or. rmv%typ /= STYP .or. rpl%typ /= STYP ) then
      call opflag%set ( stat = UERROR, where = HERE,                                     &
                         msg = 'in << replace(a,rm,rp) >> 3 string arrays are expected' )
      return
   end if
   
   if ( rmv%nrow /= rpl%nrow .or. rmv%ncol /= rpl%ncol ) then
      call opflag%set ( stat = UERROR, where = HERE, msg =                                &
                       'in << replace(a,rm,rp) >> "rm" and "rp" must have the same shape' )
      return
   end if
   
   call pk2_assign ( tmp, a ) ; error_TraceNreturn(opflag, HERE)
   
   nrp = 0
      
   select type (rem=>rmv%m) ; type is (sk2_t)
   select type (rep=>rpl%m) ; type is (sk2_t)
   select type (new=>tmp%m) ; type is (sk2_t)
   
      do j = 1, rem%ncol
         do i = 1, rem%nrow
            if ( allocated(rem%v(i,j)%str) ) then
               remove  = (rem%v(i,j)%str)
            else 
               remove = ''
            end if
            if ( allocated(rep%v(i,j)%str) ) then
               replace = (rep%v(i,j)%str)
            else
               replace = ''
            end if      
            do q = 1, new%ncol
               do p = 1, new%nrow
                  str = util_ReplaceSubstring2 ( str     = new%v(p,q)%str, &
                                                 remove  = remove        , &
                                                 replace = replace       , &
                                                 opcl    = opcl          , &
                                                 stat    = opflag        , &
                                                 nrep    = n               )
                  error_TraceNreturn(opflag, HERE)      
                                           
                  new%v(p,q)%str = (str)
                  nrp = nrp + n ! number of replacements
               end do
            end do
         end do
      end do
      
   end select
   end select
   end select
   
   call pk2_moveAlloc ( from = tmp, to = res )
   
   if ( present(nrep) ) nrep = nrp
         
   END SUBROUTINE pk2f_subREPLACE
   

!=============================================================================================    
   FUNCTION pk2f_REPLACE ( a, rmv, rpl, nrep, opcl ) result ( res )
!=============================================================================================  
   class    (pk2_t),           intent(in    ) :: a, rmv, rpl
   character(len=*),           intent(in    ) :: opcl 
   integer  (Ikind), optional, intent(   out) :: nrep
   type     (pk2_t)                           :: res
!---------------------------------------------------------------------------------------------  

   call pk2f_subREPLACE ( a, rmv, rpl, opcl, res, nrep )
   
   error_TraceNreturn(opflag, 'pk2f_REPLACE')
   
   END FUNCTION pk2f_REPLACE


!=============================================================================================    
   SUBROUTINE pk2f_subREPLACEwrp ( matrs, res, res2 )
!=============================================================================================  
   class(pk2_t),           intent(in    ) :: matrs(:)
   class(pk2_t),           intent(in out) :: res
   class(pk2_t), optional, intent(in out) :: res2
!---------------------------------------------------------------------------------------------   
!  Replaces in the string array matrs(1) any occurence of the substring matrs(2) by matrs(3)
!  if not enclosed by one of the pairs of symbols given in matrs(4) (if present). Optionaly, 
!  returns into res2 the number of replacements.
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter   :: HERE = 'pk2f_subREPLACEwrp'    
   integer  (Ikind)              :: nargs, nrep
   character(len=:), allocatable :: opcl
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()
            
   nargs = size(matrs)
   
   if ( nargs < 3 ) then
      call opflag%set ( stat = UERROR, where = HERE,                                      &
                         msg = '3 or 4 arguments expected for the function << replace >>' )
      return
   end if
   
   if ( nargs == 4 ) then
      if ( matrs(4)%typ /= STYP .or. matrs(4)%nrow * matrs(4)%ncol < 1 ) then
         call opflag%set ( stat = UERROR, where = HERE,                                  &
                           msg = ' << replace(a,rm,rp,opcl) >> string expected for opcl' )
         return   
      end if
      select type (p=>matrs(4)%m)
         type is (sk2_t)
            opcl = p%v(1,1)%str
      end select      
      call pk2f_subREPLACE ( a=matrs(1), rmv=matrs(2), rpl=matrs(3), opcl=opcl, &
                             res=res   , nrep=nrep )
   else
      call pk2f_subREPLACE ( a=matrs(1), rmv=matrs(2), rpl=matrs(3), opcl=''  , &
                             res=res   , nrep=nrep )
   end if   
   
   error_TraceNreturn(opflag, HERE)
   
   if ( present(res2) ) res2 = nrep

   END SUBROUTINE pk2f_subREPLACEwrp
   

!=============================================================================================    
   FUNCTION pk2f_REPLACEwrp ( matrs, res2 ) result ( res )
!=============================================================================================  
   class(pk2_t), optional, intent(in    ) :: matrs(:)
   class(pk2_t), optional, intent(   out) :: res2
   type (pk2_t)                           :: res 
!---------------------------------------------------------------------------------------------   

!- local variables ---------------------------------------------------------------------------
   character(len=*), parameter :: HERE = 'pk2f_REPLACEwrp'    
   type     (str_t)            :: h(17), f
!---------------------------------------------------------------------------------------------   
   
   if ( .not. present(matrs) ) then
      f = str_color('replace',pk2f_helpcol)
      h( 1) = "<< "+f+" >> replace one or more substrings"
      h( 2) = " "
      h( 3) = "Syntax:  [newstr,n] = "+f+"(str,rm,rp[,opcl])"      
      h( 4) = ""
      h( 5) = "where"
      h( 6) = ". str   : An array of strings"
      h( 7) = ". rm, rp: Two arrays of strings of the same shape. All occurrences of the"
      h( 8) = "          substrings of rm are replaced with those of rp."
      h( 9) = ". opcl  : A string giving pairs of openning-closing symbols. The substrings"
      h(10) = "          of rm lying between one of these pairs are not modified."
      h(11) = ". n     : number of replacements."
      h(12) = ""
      h(13) = "Examples:"
      h(14) = ""
      h(15) = ". "+f+"('sin(x)*log(sin(x)*z)','sin','sind')  gives: sind(x)*log(sind(x)*z)"
      h(16) = ". "+f+"('x*(2*x - x^2)+f(x)/x','x','X','()')  gives: X*(2*x - x^2)+f(x)/X"
      h(17) = " "    
      call pk2_assign ( res, h )
      return
   end if   
      
   call pk2f_subREPLACEwrp ( matrs, res, res2 ) ; error_TraceNreturn(opflag, HERE)

   END FUNCTION pk2f_REPLACEwrp


!=============================================================================================    
   SUBROUTINE pk2f_subINTTRAP ( a, b, res )
!=============================================================================================  
   class(pk2_t), intent(in    ) :: a, b 
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Trapezoidal rule   
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subINTTRAP'
   integer  (Ikind)              :: na, nb
   real     (Rkind)              :: v
   logical                       :: aIs_col, bIs_col
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
      
   if ( (a%typ /= ITYP .and. a%typ /= RTYP) .or. &
        (b%typ /= ITYP .and. b%typ /= RTYP)    ) then
      call opflag%set ( stat = UERROR, where = HERE,                                   &
                         msg = 'in << inttrap(a,b) >> a and b must be integer or real' )
      return
   end if
   
   if ( a%nrow /= 1 ) then
      if ( a%ncol /= 1 ) then
         call opflag%set ( stat = UERROR, where = HERE,                     &
                           msg = 'in << inttrap(a,b) >> a must be a vector' )
         return
      else
         aIs_col = .true.
         na = a%nrow
      end if
   else
      aIs_col = .false.
      na = a%ncol
   end if

   if ( b%nrow /= 1 ) then
      if ( b%ncol /= 1 ) then
         call opflag%set ( stat = UERROR, where = HERE,                    &
                           msg = 'in << inttrap(a,b) >> b must be a vector')
         return
      else
         bIs_col = .true.
         nb = b%nrow 
      end if
   else
      bIs_col = .false.
      nb = b%ncol
   end if   

   if ( na /= nb ) then
      call opflag%set ( stat = UERROR, where = HERE,                                    &
                         msg = 'in << inttrap(a,b) >> a and b must be of the same size' )
      return
   end if
            
   select type (x=>a%m)
      type is (ik2_t)     
         select type (y=>b%m)
            type is (ik2_t) 
               if ( aIs_col .and. bIs_col ) then
                  call util_trapeze ( ix = x%v(:,1), iy = y%v(:,1), res = v, stat = opflag )
               else if ( aIs_col ) then
                  call util_trapeze ( ix = x%v(:,1), iy = y%v(1,:), res = v, stat = opflag )
               else if ( bIs_col ) then
                  call util_trapeze ( ix = x%v(1,:), iy = y%v(:,1), res = v, stat = opflag )
               else
                  call util_trapeze ( ix = x%v(1,:), iy = y%v(1,:), res = v, stat = opflag )
               end if          
            type is (rk2_t) 
               if ( aIs_col .and. bIs_col ) then
                  call util_trapeze ( ix = x%v(:,1), ry = y%v(:,1), res = v, stat = opflag )
               else if ( aIs_col ) then
                  call util_trapeze ( ix = x%v(:,1), ry = y%v(1,:), res = v, stat = opflag )
               else if ( bIs_col ) then
                  call util_trapeze ( ix = x%v(1,:), ry = y%v(:,1), res = v, stat = opflag )
               else
                  call util_trapeze ( ix = x%v(1,:), ry = y%v(1,:), res = v, stat = opflag )
               end if          
         end select
      type is (rk2_t)     
         select type (y=>b%m)
            type is (ik2_t) 
               if ( aIs_col .and. bIs_col ) then
                  call util_trapeze ( rx = x%v(:,1), iy = y%v(:,1), res = v, stat = opflag )
               else if ( aIs_col ) then
                  call util_trapeze ( rx = x%v(:,1), iy = y%v(1,:), res = v, stat = opflag )
               else if ( bIs_col ) then
                  call util_trapeze ( rx = x%v(1,:), iy = y%v(:,1), res = v, stat = opflag )
               else
                  call util_trapeze ( rx = x%v(1,:), iy = y%v(1,:), res = v, stat = opflag )
               end if          
            type is (rk2_t) 
               if ( aIs_col .and. bIs_col ) then
                  call util_trapeze ( rx = x%v(:,1), ry = y%v(:,1), res = v, stat = opflag )
               else if ( aIs_col ) then
                  call util_trapeze ( rx = x%v(:,1), ry = y%v(1,:), res = v, stat = opflag )
               else if ( bIs_col ) then
                  call util_trapeze ( rx = x%v(1,:), ry = y%v(:,1), res = v, stat = opflag )
               else
                  call util_trapeze ( rx = x%v(1,:), ry = y%v(1,:), res = v, stat = opflag )
               end if          
         end select
   end select

   res = v
          
   END SUBROUTINE pk2f_subINTTRAP  
   
   
!=============================================================================================    
   FUNCTION pk2f_INTTRAP ( a, b ) result ( res )
!=============================================================================================  
   class(pk2_t), intent(in), optional :: a, b 
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2f_INTTRAP'
   type     (str_t)            :: h(8), f
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) .and. .not. present(b) ) then
      f = str_color('inttrap',pk2f_helpcol)
      h(1) = "<< "+f+" >> Trapezoidal rule approximation of a 1d integral"
      h(2) = " "
      h(3) = "Syntax:  v = "+f+"(x,y)"
      h(4) = " "      
      h(5) = "with x: the array of the sampling points (in increasing order)"
      h(6) = "     y: the array of the function values at the sampling points"
      h(7) = "     v: the approximation of the integral"
      h(8) = " "    
      call pk2_assign ( res, h )
      return
   end if   
   
   if ( .not. present(a) ) then
      call opflag%set ( stat = UERROR, where = HERE,                            &
                         msg = 'Missing the 1st argument argument of "inttrap"' )
      return
   end if

   if ( .not. present(b) ) then
      call opflag%set ( stat = UERROR, where = HERE,                            &
                         msg = 'Missing the 2nd argument argument of "inttrap"' )
      return
   end if
      
   call pk2f_subINTTRAP ( a, b, res ) ; error_TraceNreturn(opflag, HERE)
   
   END FUNCTION pk2f_INTTRAP   


!=============================================================================================    
   SUBROUTINE pk2f_subSIZEOF ( a, res )
!=============================================================================================  
   class(pk2_t), intent(in    ) :: a 
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Trapezoidal rule   
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2f_subSIZEOF'
   integer  (Ikind)            :: i, j, n
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      res = IZERO
   else if ( a%typ == ITYP ) then
      res = a%nrow * a%ncol * kind(IONE)
   else if ( a%typ == RTYP ) then
      res = a%nrow * a%ncol * kind(RONE)
   else if ( a%typ == CTYP ) then
      res = a%nrow * a%ncol * kind(CONE) * 2
   else if ( a%typ == LTYP ) then
      res = a%nrow * a%ncol * kind(LONE)
   else if ( a%typ == STYP ) then
      select type ( p => a%m )
         type is (sk2_t)
            n = 0
            if ( allocated(p%v) ) then
               do j = 1, a%ncol
                  do i = 1, a%nrow
                     n = n + len(p%v(i,j)%str) 
                  end do
               enddo 
               res = n * kind("char")
            end if
      end select
      !res = a%nrow * a%ncol * kind("char")
   end if 
         
   END SUBROUTINE pk2f_subSIZEOF
   
   
!=============================================================================================    
   FUNCTION pk2f_SIZEOF ( a ) result ( res )
!=============================================================================================  
   class(pk2_t), intent(in), optional :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2f_SIZEOF'
   type     (str_t)            :: h(4), f
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color('sizeof',pk2f_helpcol)
      h(1) = "<< "+f+" >> Returns the number of bytes occupied by a variable"
      h(2) = " "
      h(3) = "Syntax:  n = "+f+"(a)"
      h(4) = " "      
      call pk2_assign ( res, h )
      return
   end if   
      
   call pk2f_subSIZEOF ( a, res )
   
   END FUNCTION pk2f_SIZEOF       


!=============================================================================================    
   SUBROUTINE pk2f_subTYPEOF ( a, res )
!=============================================================================================  
   class(pk2_t), intent(in    ) :: a 
   class(pk2_t), intent(in out) :: res
!---------------------------------------------------------------------------------------------
!  Trapezoidal rule   
!-----------------------------------------------------------------------------------R.H. 11/19

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter   :: HERE = 'pk2f_subTYPEOF'
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()
   
   if ( a%typ == EMPTY ) then
      res = "empty"
   else if ( a%typ == ITYP ) then
      res = "integer"
   else if ( a%typ == RTYP ) then
      res = "real"
   else if ( a%typ == CTYP ) then
      res = "complex"
   else if ( a%typ == LTYP ) then
      res = "logical"
   else if ( a%typ == STYP ) then
      res = "string"
   end if 
         
   END SUBROUTINE pk2f_subTYPEOF
   
   
!=============================================================================================    
   FUNCTION pk2f_TYPEOF ( a ) result ( res )
!=============================================================================================  
   class(pk2_t), intent(in), optional :: a
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   character(len=*), parameter :: HERE = 'pk2f_TYPEOF'
   type     (str_t)            :: h(4), f
!---------------------------------------------------------------------------------------------   
   
   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(a) ) then
      f = str_color('typeof',pk2f_helpcol)
      h(1) = "<< "+f+" >> Returns the type of a variable"
      h(2) = " "
      h(3) = "Syntax:  n = "+f+"(a)"
      h(4) = " "      
      call pk2_assign ( res, h )
      return
   end if   
      
   call pk2f_subTYPEOF ( a, res )
   
   END FUNCTION pk2f_TYPEOF      
    
   
!=============================================================================================    
   FUNCTION pk2f_oprel (opname) result ( res )
!=============================================================================================  
   character(len=*), intent(in) :: opname  
   type     (pk2_t)             :: res
!---------------------------------------------------------------------------------------------   
!  Returns help for relational operators (defined in pk2 module)
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   type(str_t), allocatable :: h(:)
   type(str_t)              :: f1, f2
!---------------------------------------------------------------------------------------------   
      
   select case (opname)
   case('lt','<')
      allocate(h(9))
      f1 = str_color('lt',pk2f_helpcol)
      f2 = str_color('<', pk2f_helpcol)
      h(1) = '"'+f1+'" or "'+f2+'<" relational operator "less than"'
      h(2) = " "
      h(3) = "Syntax: L = "+f1+"(A,B)  or  L = A "+f2+" B "
      h(4) = " "
      h(5) = "where "
      h(6) = ". A and B are of the same shape (or at least one of them is 1x1)"       
      h(7) = ". The result L is a boolean array"       
      h(8) = ". A and B are integer/real or both are array of strings (lexical comparison)"       
      h(9) = " "
   case('le','<=')
      allocate(h(9))
      f1 = str_color('le',pk2f_helpcol)
      f2 = str_color('<=',pk2f_helpcol)      
      h(1) = '"'+f1+'" or "'+f2+'" relational operator "less than or equal"'
      h(2) = " "
      h(3) = "Syntax: L = "+f1+"(A,B)  or  L = A "+f2+" B "
      h(4) = " "
      h(5) = "where "
      h(6) = ". A and B are of the same shape (or at least one of them is 1x1)"       
      h(7) = ". The result L is a boolean array"       
      h(8) = ". A and B are integer/real or both are array of strings (lexical comparison)"    
      h(9) = " "
   case('gt','>')
      allocate(h(9))
      f1 = str_color('gt',pk2f_helpcol)
      f2 = str_color('>', pk2f_helpcol)
      h(1) = '"' + f1 + '" or "' + f2 + '" relational operator "greater than"'
      h(2) = " "
      h(3) = "Syntax: L = "+f1+"(A,B)  or  L = A "+f2+" B "
      h(4) = " "
      h(5) = "where "
      h(6) = ". A and B are of the same shape (or at least one of them is 1x1)"       
      h(7) = ". The result L is a boolean array"       
      h(8) = ". A and B are integer/real or both are array of strings (lexical comparison)"       
      h(9) = " "
   case('ge','>=')
      allocate(h(9))
      f1 = str_color('ge',pk2f_helpcol)
      f2 = str_color('>=',pk2f_helpcol)      
      h(1) = '"' + f1 + '" or "' + f2 + '" relational operator "greater than or equal"'
      h(2) = " "
      h(3) = "Syntax: L = "+f1+"(A,B)  or  L = A "+f2+" B "
      h(4) = " "
      h(5) = "where "
      h(6) = ". A and B are of the same shape (or at least one of them is 1x1)"       
      h(7) = ". The result L is a boolean array"       
      h(8) = ". A and B are integer/real or both are array of strings (lexical comparison)"
      h(9) = " "
   case('eq','==')
      allocate(h(9))
      f1 = str_color('eq',pk2f_helpcol)
      f2 = str_color('==',pk2f_helpcol)      
      h(1) = '"'+f1+'" or "'+f2+'" relational operator "equal to"'
      h(2) = " "
      h(3) = "Syntax: L = "+f1+"(A,B)  or  L = A "+f2+" B "
      h(4) = " "
      h(5) = "where "
      h(6) = ". A and B are of the same shape (or at least one of them is 1x1)"       
      h(7) = ". The result L is a boolean array"       
      h(8) = ". A and B are of any type"  
      h(9) = " "
   case('ne','~=')
      allocate(h(9))
      f1 = str_color('ne',pk2f_helpcol)
      f2 = str_color('~=',pk2f_helpcol)      
      h(1) = '"' + f1 + '" or "' + f2 + '" relational operator "not equal"'
      h(2) = " "
      h(3) = "Syntax: L = "+f1+"(A,B)  or  L = A "+f2+" B "
      h(4) = " "
      h(5) = "where "
      h(6) = ". A and B are of the same shape (or at least one of them is 1x1)"       
      h(7) = ". The result L is a boolean array"       
      h(8) = ". A and B are of any type"    
      h(9) = " "
   case default
      allocate(h(2))
      h(1) = "no help found for << " // trim(opname) // " >>"     
      h(2) = " " 
   end select
   call pk2_assign ( res, h )

   END FUNCTION pk2f_oprel


!=============================================================================================    
   FUNCTION pk2f_opalg (opname) result ( res )
!=============================================================================================  
   character(len=*), intent(in) :: opname  
   type     (pk2_t)             :: res
!---------------------------------------------------------------------------------------------   
!  Returns help for arithmetics operators (defined in pk2 module)
!---------------------------------------------------------------------------------------------   

!- local variables --------------------------------------------------------------------------- 
   type(str_t), allocatable :: h(:)
   type(str_t)              :: f, f1
!---------------------------------------------------------------------------------------------   
      
   select case (opname)
   case('+')
      allocate(h(12))
      f = str_color('+',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for numerical addition or strings concatenation'
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" B"
      h( 4) = " "
      h( 5) = "where A and B are two arrays of"
      h( 6) = ". compatible types (both of numerics type or both string type),"
      h( 7) = ". the same shape (or at least one of them is scalar)."    
      h( 8) = " "
      h( 9) = "The result is C(i,j) = A(i,j) "+f+" B(i,j) if A and B are of the same shape"
      h(10) = "           or C(i,j) = A(i,j) "+f+" B(1,1) if B is a 1x1 array (a scalar)"   
      h(11) = "           or C(i,j) = A(1,1) "+f+" B(i,j) if A is a 1x1 array (a scalar)"  
      h(12) = " " 
   case('-')
      allocate(h(12))
      f = str_color('-',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for subtraction'
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" B"
      h( 4) = " "
      h( 5) = "where A and B are two arrays of"
      h( 6) = ". numerics types,"
      h( 7) = ". the same shape (or at least one of them is scalar)." 
      h( 8) = " "
      h( 9) = "The result is C(i,j) = A(i,j) "+f+" B(i,j) if A and B are of the same shape"
      h(10) = "           or C(i,j) = A(i,j) "+f+" B(1,1) if B is a 1x1 array (a scalar)"   
      h(11) = "           or C(i,j) = A(1,1) "+f+" B(i,j) if A is a 1x1 array (a scalar)"  
      h(12) = " " 
   case('*')
      allocate(h(12))
      f = str_color('*',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for matrix multiplication'
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" B"
      h( 4) = " "
      h( 5) = "where A and B are two arrays of"
      h( 6) = ". numerics types,"
      h( 7) = ". compatible shapes (or at least one of them is scalar)."  
      h( 8) = " "
      h( 9) = "The result is C(i,j) = A(i,k) "+f+" B(k,j) (sum over the index k)"
      h(10) = "           or C(i,j) = A(i,j) "+f+" B(1,1) if B is a 1x1 array (a scalar)"   
      h(11) = "           or C(i,j) = A(1,1) "+f+" B(i,j) if A is a 1x1 array (a scalar)" 
      h(12) = " " 
   case('/')
      allocate(h(12))
      f = str_color('/',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for division'
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" x  or  C =  x "+f+" A"
      h( 4) = " "
      h( 5) = "where A is an array (or a scalar) and x is a scalar, both of numerics type."
      h( 6) = " "
      h( 7) = "In the above first expression, C = A "+f+" x, elements of A are simply divided"
      h( 8) = "by x: C(i,j) = A(i,j) "+f+" x(1,1)."
      h( 9) = " "
      h(10) = "In the second one, C = x "+f+" A, A must be a square matrix and the result is"
      h(11) = "equivalent to C = x(1,1) * inv(A)."       
      h(12) = " " 
   case('^')
      allocate(h(23))
      f = str_color('^',pk2f_helpcol) ; f1 = str_color('very limited','rbh')
      h( 1) = 'operator "'+f+'" for ('+f1+') exponentiation'
      h( 2) = " " 
      h( 3) = "Syntax: C = A "+f+" x   or   C = x "+f+" A"
      h( 4) = " "
      h( 5) = "where A is an array (or a scalar) and x is a scalar, both of numerics type."
      h( 6) = " "      
      h( 7) = ". In C = A"+f+"x, the exponent (x) must be an integer and the matrix (A) must" 
      h( 8) = "  be square. For x > 0 the result corresponds to C = A * A * ... * A (x times),"
      h( 9) = "  for x < 0 it corresponds to C = inv(A) * inv(A) *... * inv(A) (x times) and "
      h(10) = "  for x = 0 to C = eye(A)"
      h(11) = " "
      f1 = str_color('.^',pk2f_helpcol)
      h(12) = ". In C = x"+f+"A, the matrix (A) is of any shape and the result is an array of"
      h(13) = "  the same shape:  C(i,j) = x(1,1) "+f+" A(i,j) (i.e. C = x "+f1+" A)."    
      h(14) = " " 
      h(15) = "Examples:"
      h(16) = ". [ 1, 2 ; 3, 4 ]^2 "
      h(17) = "  ans =  7  10"
      h(18) = "        15  22"
      h(19) = " "
      h(20) = ". 2^[ 1, 2 ; 3, 4 ] "
      h(21) = "  ans = 2   4"
      h(22) = "        8  16"
      h(23) = " "
   case('.*')
      allocate(h(20))
      f = str_color('.*',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for element-wise multiplication'
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" B "
      h( 4) = " "
      h( 5) = "where A and B are two arrays of"
      h( 6) = ". numerics types,"
      h( 7) = ". the same shapes (or at least one of them is scalar)."  
      h( 8) = " "
      h( 9) = "The result is C(i,j) = A(i,j) * B(i,j) if A and B are of the same shape"
      h(10) = "           or C(i,j) = A(i,j) * B(1,1) if B is a 1x1 array (a scalar)"   
      h(11) = "           or C(i,j) = A(1,1) * B(i,j) if A is a 1x1 array (a scalar)"  
      h(12) = " " 
      h(13) = "Examples:"
      h(14) = ". [1, 2].*[3, 4]"
      h(15) = "  ans = 3  8" 
      h(16) = " "
      h(17) = ". (2).*[1, 2 ; 3, 4]"
      h(18) = "  ans = 2  4"
      h(19) = "        6  8"
      h(20) = " "      
   case('./')
      allocate(h(20))
      f = str_color('./',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for element-wise divide'
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" B "
      h( 4) = " "
      h( 5) = "where A and B are two arrays of"
      h( 6) = ". numerics types,"
      h( 7) = ". the same shapes (or at least one of them is scalar)."  
      h( 8) = " "
      h( 9) = "The result is C(i,j) = A(i,j) / B(i,j) if A and B are of the same shape"
      h(10) = "           or C(i,j) = A(i,j) / B(1,1) if B is a 1x1 array (a scalar)"  
      h(11) = "           or C(i,j) = A(1,1) / B(i,j) if A is a 1x1 array (a scalar)"
      h(12) = " " 
      h(13) = "Examples:"
      h(14) = ". [3, 4]./[1, 2] "
      h(15) = "  ans = 3.0000000  2.0000000"
      h(16) = " "
      h(17) = ". (20)./[1, 2 ; 4, 5]"
      h(18) = "  ans = 20.000000  10.000000"
      h(19) = "         5.000000   4.000000"
      h(20) = " "            
   case('.^')
      allocate(h(21))
      f = str_color('.^',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for element-wise exponentiation'
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" B "
      h( 4) = " "
      h( 5) = "where A and B are two arrays of"
      h( 6) = ". numerics types,"
      h( 7) = ". the same shapes (or at least one of them is scalar)."  
      h( 8) = " "
      h( 9) = "The result is C(i,j) = A(i,j) ^ B(i,j) if A and B are of the same shape"
      h(10) = "           or C(i,j) = A(i,j) ^ B(1,1) if B is a 1x1 array (a scalar)"  
      h(11) = "           or C(i,j) = A(1,1) ^ B(i,j) if A is a 1x1 array (a scalar)"  
      h(12) = " " 
      h(13) = "Examples:"
      h(14) = ". [ 1, 2 ; 3, 4 ].^2 "
      h(15) = "  ans = 1   4"
      h(16) = "        9  16"
      h(17) = " "
      h(18) = ". (2).^[ 1, 2 ; 3, 4 ] "
      h(19) = "  ans = 2   4"
      h(20) = "        8  16"
      h(21) = " "      
   case('&')
      allocate(h(12))
      f = str_color('&',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for logical conjunction ("and" operator) '
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" B "
      h( 4) = " "
      h( 5) = "where A and B are two arrays of"
      h( 6) = ". logical type,"
      h( 7) = ". the same shapes (or at least one of them is scalar)."  
      h( 8) = " "
      h( 8) = " "
      h( 9) = "The result is C(i,j) = A(i,j) "+f+" B(i,j) if A and B are of the same shape"
      h(10) = "           or C(i,j) = A(i,j) "+f+" B(1,1) if B is a 1x1 array (a scalar)"  
      h(11) = "           or C(i,j) = A(1,1) "+f+" B(i,j) if A is a 1x1 array (a scalar)" 
      h(12) = " " 
   case('|')
      allocate(h(12))
      f = str_color('|',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for logical (inclusive) disjunction ("or" operator) '
      h( 2) = " "
      h( 3) = "Syntax: C = A "+f+" B "
      h( 4) = " "
      h( 5) = "where A and B are two arrays of"
      h( 6) = ". logical type,"
      h( 7) = ". the same shapes (or at least one of them is scalar)."  
      h( 8) = " "
      h( 8) = " "
      h( 9) = "The result is C(i,j) = A(i,j) "+f+" B(i,j) if A and B are of the same shape"
      h(10) = "           or C(i,j) = A(i,j) "+f+" B(1,1) if B is a 1x1 array (a scalar)"  
      h(11) = "           or C(i,j) = A(1,1) "+f+" B(i,j) if A is a 1x1 array (a scalar)"  
      h(12) = " " 
   case('~')
      allocate(h(7))
      f = str_color('~',pk2f_helpcol)
      h( 1) = 'operator "'+f+'" for logical negation ("not" operator) '
      h( 2) = " "
      h( 3) = "Syntax: B = "+f+"A "
      h( 4) = " "
      h( 5) = "where A is an array of logical type and the result is of the same shape:"
      h( 6) = "B(i,j) = "+f+"A(i,j)"             
      h( 7) = " " 
   case default
      allocate(h(2))
      h(1) = "no help found for << " // trim(opname) // " >>"
      h(2) = " "
   end select
   call pk2_assign ( res, h )

   END FUNCTION pk2f_opalg

                          
!=============================================================================================    
   recursive FUNCTION pk2f_HELP ( matrs ) result ( res ) 
!=============================================================================================       
   class(pk2_t), optional, intent(in) :: matrs(:)
   type (pk2_t)                       :: res
!---------------------------------------------------------------------------------------------   
!  Returns help
!-----------------------------------------------------------------------------------R.H. 07/18

!- local variables ---------------------------------------------------------------------------        
   type     (str_t)              :: h(6), f
   integer  (Ikind)              :: nargs
   logical                       :: help
   character(len=:), allocatable :: fname
!---------------------------------------------------------------------------------------------   

   if ( opflag%code > IZERO ) return !!call opflag%set ()

   if ( .not. present(matrs) ) then
      nargs = 0
   else
      nargs = size(matrs)
   end if
   
   if ( nargs == 0 ) then      
!
!-    return in "res" some helps for this function and exit:
!     
      f = str_color('help',pk2f_helpcol)
      h(1) = "<< "+f+" >> displays help of a given function"
      h(2) = " "
      h(3) = "Syntax: "+f+"('fname')"
      h(4) = " "
      h(5) = "where fname is the name of the function"  
      h(6) = " "
      call pk2_assign ( res, h )
      return
   end if   

   if ( matrs(1)%typ  /= STYP .or. .not. allocated(matrs(1)%m) .or. &
        matrs(1)%nrow == 0    .or. matrs(1)%ncol == 0               ) then
      call opflag%set ( stat = UERROR, where = 'pk2f_HELP',          &
                         msg = 'a string is expected for << help >>' )
      return
   end if
   
   help = .true.
      
   select type (p=>matrs(1)%m)
      type is (sk2_t)
      
         if ( allocated(p%v(1,1)%str) ) then
            fname = trim ( (p%v(1,1)%str) )
         else
            fname = ' '
         end if
               
         select case ( fname )
!
!-          for these functions, no arguments means help:
!
            case('all')
               res = all()
            case('any')
               res = any()
            case('sin')
               res = sin()
            case('cos')
               res = cos()
            case('tan')
               res = tan()
            case('asin')
               res = asin()
            case('acos')
               res = acos()
            case('atan')
               res = atan()
            case('sind')
               res = sind()
            case('cosd')
               res = cosd()
            case('tand')
               res = tand()
            case('asind')
               res = asind()
            case('acosd')
               res = acosd()
            case('atand')
               res = atand()
            case('sinh')
               res = sinh()
            case('cosh')
               res = cosh()
            case('tanh')
               res = tanh()
            case('sinc')
               res = sinc()
            case('log')
               res = log()
            case('log10')
               res = log10()
            case('exp')
               res = exp()    
            case('sqrt')
               res = sqrt()
            case('abs')
               res = abs()
            case('erf')
               res = erf()
            case('erfc')
               res = erfc()
            case('gamma')
               res = gamma()
            case('factorial')
               res = factor()   
            case('nint')
               res = nint()
            case('anint')
               res = anint()
            case('aint')
               res = aint()    
            case('floor')
               res = floor()    
            case('ceil')
               res = ceil()    
            case('sign')
               res = sign()    
            case('heav')
               res = heav()                                                                
            case('real')
               res = real()
            case('imag')
               res = imag()
            case('conj')
               res = conj()
            case('trace')
               res = trace()
            case('dev')
               res = dev()
            case('trans')
               res = trans()
            case('transp')
               res = transp()
            case('det')
               res = det()
            case('inv')
               res = inv()
            case('size')
               res = size()
            case('numel')
               res = numel()
            case('length')
               res = length()
            case('mod')
               res = mod()
            case('find')
               res = find()
            case('num2str')
               res = num2str()
            case('str2num')
               res = str2num()
            case('magic')
               res = magic()
            case('is_symm')
               res = is_symm()
            case('is_skew')
               res = is_skew()
            case('norm')
               res = norm()
             case('cross')  
               res = cross()
             case('reshape')  
               res = reshape()
             case("mldivide","\")  
               res = mldivide()
             case('diag')  
               res = diag()
             case('tril')  
               res = tril()
             case('triu')  
               res = triu()
             case('colon',':')  
               res = colon()
             case('eig')  
               res = eig()
             case('merge','[]')  
               res = mergemats()
             case('min')  
               res = min()
             case('max')  
               res = max()
             case('sum')  
               res = sum()
             case('mean')  
               res = mean()
             case('cov')  
               res = cov()               
             case('prod')  
               res = prod()  
             case('randi')
                res = randi()
             case('randperm')
                res = randperm()
             case('trim')
                res = trim()
             case('writemat')
                res = writemat()
             case('meshgrid')
                res = meshgrid()
             case('lu')
                res = lu()
             case('lusolv')
                res = lusolv()
             case('sort')
                res = sort()
            case('reglin')
               res = reglin()
            case('convstr')
               res = convstr()       
            case('replace')
               res = replace()
            case('part')
               res = part()       
            case('inttrap')
               res = inttrap()       
            case('sizeof')
               res = sizeof()   
            case('typeof')
               res = typeof()       
            case('svd')
               res = svd()  
            case('poldec')
               res = poldec()
            case('linspace')
               res = linspace()
            case('help',' ')
               res = pk2f_HELP()
               
             case('lt','<','gt','>','eq','==','le','<=','ge','>=','ne','~=')
               res = pk2f_oprel(fname)                           
             case('+','-','*','/','^','.*','./','.^','&','|','~')
               res = pk2f_opalg(fname)    
!
!-           but the following functions can return a result without argument or
!            all arguments are declared optional.
!            Workaround: use the optional argument "help"
!
             case('zeros')  
               res = zeros(help=help)
             case('falses')  
               res = falses(help=help)
             case('ones')  
               res = ones(help=help)
             case('eye')  
               res = eye(help=help)
             case('rand')  
               res = rand(help=help)
             case('pwd')  
               res = pwd(help=help)  
             case('date')  
               res = date(help=help)
             case('cputime')
               res = pk2f_CPUTIME(help=help)             
             case('readmat') 
               res = readmat(help=help)
                
             case default
               res = 'sorry, no help found for << '//trim(fname)//' >>'
                
         end select
   end select               

   END FUNCTION pk2f_HELP
   

!=============================================================================================    
   SUBROUTINE pk2f_resetSize ( n, m, a )
!=============================================================================================    
   integer(Ikind), intent(in    ) :: n, m
   type   (pk2_t), intent(in out) :: a
!---------------------------------------------------------------------------------------------   
!  Caution: a%m must be allocated
!---------------------------------------------------------------------------------------------   

   a%nrow = n ; a%m%nrow = n ; 
   a%ncol = m ; a%m%ncol = m
   
   END SUBROUTINE pk2f_resetSize
   
   
!!! Test

   
   
subroutine pk2f_drivertruc1(matrs,res)
type(pk2_t), target, intent(in    ) :: matrs(:)
type(pk2_t),         intent(in out) :: res

real   (Rkind), pointer     :: px(:,:)
integer(Ikind), pointer     :: py(:,:)
real   (Rkind), allocatable :: r(:,:)
type   (err_t)              :: stat

call matrs(1)%pointer(px,stat) 
if ( stat /= IZERO ) then 
   call stat%display()
   px => NULL() ; py => NULL()
   return
endif 
call matrs(2)%pointer(py,stat) 
if ( stat /= IZERO ) then 
   call stat%display()
   px => NULL() ; py => NULL()
   return   
endif 

call truc1(px,py,r) ; call pk2_movealloc (from=r, to=res) ; res%name = 'x+y'

px => NULL() ; py => NULL()

end subroutine pk2f_drivertruc1

subroutine truc1(x,y,z)
real   (Rkind),              intent(in    ) :: x(:,:)
integer(Ikind),              intent(in    ) :: y(:,:)
real   (Rkind), allocatable, intent(   out) :: z(:,:)
z = x+y
end subroutine truc1    

!!! end Test
  
END MODULE pk2f_m
