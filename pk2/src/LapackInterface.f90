!---------------------------------------------------------------------------------------------
! The pk2 library, version 2019.1
!---------------------------------------------------------------------------------------------
!
! Author: R. Hassani, Universite de Nice - Sophia Antipolis
!
! Module: LapackInterface
!
! Description: interfaces and generic interfaces of some BLAS/LAPACK routines
!---------------------------------------------------------------------------------------------

MODULE LapackInterface_m

   
!= LU factorization of a general matrix ======================================================
   INTERFACE LapackInterface_getrf
      SUBROUTINE sgetrf ( m, n, A, lda, ipiv, info)
      integer            info, lda, m, n
      integer            ipiv( * )
      real               A( lda, * )
      END SUBROUTINE sgetrf
   
      SUBROUTINE dgetrf ( m, n, A, lda, ipiv, info)
      integer            info, lda, m, n
      integer            ipiv( * )
      double precision   A( lda, * )
      END SUBROUTINE dgetrf

      SUBROUTINE cgetrf ( m, n, A, lda, ipiv, info)
      integer            info, lda, m, n
      integer            ipiv( * )
      complex            A( lda, * )
      END SUBROUTINE cgetrf

      SUBROUTINE zgetrf ( m, n, A, lda, ipiv, info)
      integer            info, lda, m, n
      integer            ipiv( * )
      complex*16         A( lda, * )
      END SUBROUTINE zgetrf
   END INTERFACE

!= Solution to system of linear equations after LU factorization =============================
   INTERFACE LapackInterface_getrs
      SUBROUTINE sgetrs( trans, n, nrhs, A, lda, ipiv, B, ldb, info )
      character          trans
      integer            info, lda, ldb, n, nrhs
      integer            ipiv( * )
      real               A( lda, * ), B( ldb, * )  
      END SUBROUTINE sgetrs
      
      SUBROUTINE dgetrs( trans, n, nrhs, A, lda, ipiv, B, ldb, info )
      character          trans
      integer            info, lda, ldb, n, nrhs
      integer            ipiv( * )
      double precision   A( lda, * ), B( ldb, * )
      END SUBROUTINE dgetrs
      
      SUBROUTINE cgetrs( trans, n, nrhs, A, lda, ipiv, B, ldb, info )
      character          trans
      integer            info, lda, ldb, n, nrhs
      integer            ipiv( * )
      complex            A( lda, * ), B( ldb, * )
      END SUBROUTINE cgetrs
      
      SUBROUTINE zgetrs( trans, n, nrhs, A, lda, ipiv, B, ldb, info )
      character          trans
      integer            info, lda, ldb, n, nrhs
      integer            ipiv( * )
      complex*16         A( lda, * ), B( ldb, * )
      END SUBROUTINE zgetrs
   END INTERFACE    
   
!= Inverse of a matrix using the LU factorization ============================================
   INTERFACE LapackInterface_getri  
      SUBROUTINE sgetri( n, A, lda, ipiv, work, lwork, info )
      integer            info, lda, lwork, n
      integer            ipiv( * )
      real               A( lda, * ), work( * )
      END SUBROUTINE sgetri

      SUBROUTINE dgetri( n, A, lda, ipiv, work, lwork, info )
      integer            info, lda, lwork, n
      integer            ipiv( * )
      double precision   A( lda, * ), work( * )
      END SUBROUTINE dgetri

      SUBROUTINE cgetri( n, A, lda, ipiv, work, lwork, info )
      integer            info, lda, lwork, n
      integer            ipiv( * )
      complex            A( lda, * ), work( * )
      END SUBROUTINE cgetri

      SUBROUTINE zgetri( n, A, lda, ipiv, work, lwork, info )
      integer            info, lda, lwork, n
      integer            ipiv( * )
      complex*16         A( lda, * ), work( * )
      END SUBROUTINE zgetri
   END INTERFACE      
   
!= Solution to system of linear equations for general squares matrices =======================
   INTERFACE LapackInterface_gesv
      SUBROUTINE sgesv( n, nrhs, A, lda, ipiv, B, ldb, info )
      integer            info, lda, ldb, n, nrhs
      integer            ipiv( * )
      real               A( lda, * ), B( ldb, * )
      END SUBROUTINE sgesv

      SUBROUTINE dgesv( n, nrhs, A, lda, ipiv, B, ldb, info )
      integer            info, lda, ldb, n, nrhs
      integer            ipiv( * )
      double precision   A( lda, * ), B( ldb, * ) 
      END SUBROUTINE dgesv    

      SUBROUTINE cgesv( n, nrhs, A, lda, ipiv, B, ldb, info )
      integer            info, lda, ldb, n, nrhs
      integer            ipiv( * )
      complex            A( lda, * ), B( ldb, * )
      END SUBROUTINE cgesv   

      SUBROUTINE zgesv( n, nrhs, A, lda, ipiv, B, ldb, info )
      integer            info, lda, ldb, n, nrhs
      integer            ipiv( * )
      complex*16         A( lda, * ), B( ldb, * )
      END SUBROUTINE zgesv
   END INTERFACE    

!= Solution to overdetermined or underdetermined systems for general matrices ================
   INTERFACE LapackInterface_gelsy
      SUBROUTINE sgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, info )
      integer            info, lda, ldb, lwork, m, n, nrhs, rank
      real               rcond
      integer            jpvt( * )
      real               a( lda, * ), b( ldb, * ), work( * )
      END SUBROUTINE sgelsy
            
      SUBROUTINE dgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, info )
      integer            info, lda, ldb, lwork, m, n, nrhs, rank
      double precision   rcond
      integer            jpvt( * )
      double precision   a( lda, * ), b( ldb, * ), work( * )
      END SUBROUTINE dgelsy
      
      SUBROUTINE cgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, rwork, info )
      integer            info, lda, ldb, lwork, m, n, nrhs, rank
      real               rcond
      integer            jpvt( * )
      real               rwork( * )
      complex            a( lda, * ), b( ldb, * ), work( * )
      END SUBROUTINE cgelsy
      
      SUBROUTINE zgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, rwork, info )
      integer            info, lda, ldb, lwork, m, n, nrhs, rank
      double precision   rcond
      integer            jpvt( * )
      double precision   rwork( * )
      complex*16         a( lda, * ), b( ldb, * ), work( * )
      END SUBROUTINE zgelsy
   END INTERFACE    

!= Eigenvalues and eigenvectors for general matrices =========================================
   INTERFACE LapackInterface_geev
      SUBROUTINE sgeev(jobvl, jobvr, n, A, lda, wr, wi, Vl, ldvl, Vr, ldvr, work, lwork, info)
      implicit none
      character          jobvl, jobvr
      integer            info, lda, ldvl, ldvr, lwork, n
      real               A( lda, * ), Vl( ldvl, * ), Vr( ldvr, * ),wi( * ), work( * ), wr( *)
      END SUBROUTINE sgeev
      
      SUBROUTINE dgeev(jobvl, jobvr, n, A, lda, wr, wi, Vl, ldvl, Vr, ldvr, work, lwork, info)
      implicit none
      character          jobvl, jobvr
      integer            info, lda, ldvl, ldvr, lwork, n
      double precision   A( lda, * ), Vl( ldvl, * ), Vr( ldvr, * ), wi( * ), work( * ), wr( * )
      END SUBROUTINE dgeev
      
      SUBROUTINE cgeev(jobvl, jobvr, n, A, lda, w, Vl, ldvl, Vr, ldvr, work, lwork, rwork, info)
      implicit none
      character          jobvl, jobvr
      integer            info, lda, ldvl, ldvr, lwork, n
      real               rwork( * )
      complex            A( lda, * ), Vl( ldvl, * ), Vr( ldvr, * ), w( * ), work( * )    
      END SUBROUTINE cgeev

      SUBROUTINE zgeev(jobvl, jobvr, n, A, lda, w, Vl, ldvl, Vr, ldvr, work, lwork, rwork, info)
      implicit none
      character          jobvl, jobvr
      integer            info, lda, ldvl, ldvr, lwork, n
      double precision   rwork( * )
      complex*16         A( lda, * ), Vl( ldvl, * ), Vr( ldvr, * ), w( * ), work( * )
      END SUBROUTINE zgeev
   END INTERFACE     
    
!= Eigenvalues and eigenvectors for symetric matrices and for hermitians matrices ============
   INTERFACE LapackInterface_syheev
      SUBROUTINE ssyev( jobz, uplo, n, A, lda, w, work, lwork, info )
      character          jobz, uplo
      integer            info, lda, lwork, n
      real               A( lda, * ), w( * ), work( * )      
      END SUBROUTINE ssyev
      
      SUBROUTINE dsyev( jobz, uplo, n, A, lda, w, work, lwork, info )
      character          jobz, uplo
      integer            info, lda, lwork, n
      double precision   A( lda, * ), w( * ), work( * )  
      END SUBROUTINE dsyev

      SUBROUTINE cheev( jobz, uplo, n, A, lda, w, work, lwork, rwork, info )
      character          jobz, uplo
      integer            info, lda, lwork, n
      real               rwork( * ), w( * )
      complex            A( lda, * ), work( * )
      END SUBROUTINE cheev

      SUBROUTINE zheev( jobz, uplo, n, A, lda, w, work, lwork, rwork, info )
      character          jobz, uplo
      integer            info, lda, lwork, n
      double precision   rwork( * ), w( * )
      complex*16         A( lda, * ), work( * )
      END SUBROUTINE zheev
   END INTERFACE
   
!= General matrix multiplication (alpha*op(A)*op(B) + beta*C) ================================
   INTERFACE LapackInterface_gemm 
      SUBROUTINE sgemm( transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc )
      real               alpha, beta
      integer            k, lda, ldb, ldc, m, n
      character          transa, transb
      real               A(lda,*), B(ldb,*), C(ldc,*)
      END SUBROUTINE sgemm
      
      SUBROUTINE dgemm( transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc )
      double precision   alpha, beta
      integer            k, lda, ldb, ldc, m, n
      character          transa, transb
      double precision   A(lda,*), B(ldb,*), C(ldc,*)
      END SUBROUTINE dgemm
      
      SUBROUTINE cgemm( transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc )
      complex            alpha, beta
      integer            k, lda, ldb, ldc, m, n
      character          transa, transb
      complex            A(lda,*), B(ldb,*), C(ldc,*)
      END SUBROUTINE cgemm
      
      SUBROUTINE zgemm( transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc )
      complex*16         alpha, beta
      integer            k, lda, ldb, ldc, m, n
      character          transa, transb
      complex*16         A(lda,*), B(ldb,*), C(ldc,*)
      END SUBROUTINE zgemm
   END INTERFACE

!= Eigenvalues and eigenvectors for general matrices =========================================
   INTERFACE LapackInterface_gesvd
      SUBROUTINE sgesvd( jobu, jobvt, m, n, A, lda, S, U, ldu, VT, ldvt, WORK, lwork, info )
      implicit none
      character          jobu, jobvt
      integer            info, lda, ldu, ldvt, lwork, m, n
      real               A( lda, * ), S( * ), U( ldu, * ), VT( ldvt, * ), WORK( * )
      END SUBROUTINE sgesvd
      
      SUBROUTINE dgesvd( jobu, jobvt, m, n, A, lda, S, U, ldu, VT, ldvt, WORK, lwork, info )
      implicit none
      character          jobu, jobvt
      integer            info, lda, ldu, ldvt, lwork, m, n
      double precision   A( lda, * ), S( * ), U( ldu, * ), VT( ldvt, * ), WORK( * )
      END SUBROUTINE dgesvd
      
      SUBROUTINE cgesvd( jobu, jobvt, m, n, A, lda, S, U, ldu, VT, ldvt, WORK, lwork, RWORK, info )
      implicit none
      character          jobu, jobvt
      integer            info, lda, ldu, ldvt, lwork, m, n
      real               RWORK( * ), S( * )
      complex            A( lda, * ), U( ldu, * ), VT( ldvt, * ), WORK( * )    
      END SUBROUTINE cgesvd

      SUBROUTINE zgesvd( jobu, jobvt, m, n, A, lda, S, U, ldu, VT, ldvt, WORK, lwork, RWORK, info )
      implicit none
      character          jobu, jobvt
      integer            info, lda, ldu, ldvt, lwork, m, n
      double precision   RWORK( * ), S( * )
      complex*16         A( lda, * ), U( ldu, * ), VT( ldvt, * ), WORK( * )
      END SUBROUTINE zgesvd
   END INTERFACE  
         
END MODULE LapackInterface_m
