MODULE USR_SMlib_Band_Gauss_Solver
  !
  !!  This module is part of SMLIB v. 1.1.  It contains procedures needed
  !!  to interface smlib with Lapack"s band matrix Gaussian elimination routines
  !!  gbfa and gbsl.  If Lapack is not installed on your computer, these routines
  !!  are available from NETLIB.
  !
  !! ------------------------------------------------------------------------
  !
  !!  Copyright (C) 1998 Ernst A. Meese
  !!    Refer to the file copyright.doc for details and important disclaimer.
  !
  !!  Created: May 1998
  !
  !! ------------------------------------------------------------------------
  !!  Module contents
  !
  !!   Interfaces:
  !!      LU_Factor - Generic interface for LU factorisation of a matrix.
  !!      SOLVE     - Generic interface for the solution of a linear equation system.
  !
  !!   Procedures:
  !!      LU_Factor_MSR_to_Band_LU - Factor an MSR matrix, assuming it to have
  !!           some band structure, into a band LU format USEd by Lapack.
  !!      SOLVE_Band_LU - Solve a linear equation system, given a band LU
  !!           factorisation of the matrix.
  !
  !! -----------------------------------------------------------------------
  !
  !!  We use the modules defining abstract datatypes for the matrices
  !
  use USR_SMlib_Precision         !!((16-B-USR_SMlib_Precision.f90))
  use USR_SMlib_Matrix_Arithmetic !!((19-B-USR_SMlib_Matrix_Arithmetic.f90))
  use LIB_LINPACK_Rsp             !!((04-B-LIB_LINPACK_Rsp.f))
  use LIB_LINPACK_Rdp             !!((04-B-LIB_LINPACK_Rdp.f))
  IMPLICIT NONE

  PRIVATE

  INTERFACE LU_Factor
     MODULE PROCEDURE LU_Factor_MSR_to_Band_LU
  END INTERFACE

  INTERFACE Solve
     MODULE PROCEDURE SOLVE_Band_LU
  END INTERFACE

  PUBLIC :: LU_Factor, Solve

CONTAINS

  SUBROUTINE LU_Factor_MSR_to_Band_LU (A, LU, inform )
    !
    type(MSR),            intent(IN)    :: A
    type(Band_LU),        intent(INOUT) :: LU
    integer,    optional, intent(OUT)   :: inform
    !
    integer :: m, ml, mu, i, j, k, jl, ju, info

    mu = 0 ; ml = 0
    DO i = 1, A % N
       ju = MAX( i, MAXVAL( A % JA( A % JA(i):A % JA(i+1)-1 ) ) )
       jl = MIN( i, MINVAL( A % JA( A % JA(i):A % JA(i+1)-1 ) ) )
       ml = MAX( mu, ju - i )
       mu = MAX( ml, i - jl )
    END DO

    IF ( Allocated_Matrix(LU) ) THEN
       IF ( LU % Upper_band_width /= mu .or. LU % Lower_band_width /= ml .or. size(LU % abd,2) /= A % N ) &
            Call Deallocate_Matrix ( LU )
    END IF
    IF (.not.Allocated_Matrix(LU)) Call Allocate_matrix ( LU, ml, mu, A % N )

    LU % abd = 0.0_prec
    m = ml + mu + 1
    DO i = 1, A % N
       DO j = A % JA (i), A % JA (i+1) - 1
          k = A % JA ( j ) - i + m
          LU % abd(k,i) = A % A ( j )
       END DO
    END DO
    LU % abd(m,1:A%N) = A % A (1:A%N)

    !provide overloaded GBFA instead [waw]
        CALL GBFA( LU % ABD, SIZE(LU%ABD,1), LU%N, ML, MU, LU%IPVT, INFO)

        !commented out by [waw]
        !SELECT CASE ( KIND(LU % ABD(1,1)) )
    !CASE ( single_prec )
    !!   CALL SGBFA( REAL(LU % ABD,single_prec), SIZE(LU%ABD,1), &
        !!     LU%N, ML, MU, LU%IPVT, INFO)
    !CASE ( double_prec )
    !!   CALL DGBFA( REAL(LU % ABD,double_prec), SIZE(LU%ABD,1), &
        !!     LU%N, ML, MU, LU%IPVT, INFO)
    !CASE DEFAULT
    !!   STOP "Real type has to be either single or double precision for band Gaussian solver."
    !END SELECT
    IF( PRESENT( inform ) ) inform = INFO

  END SUBROUTINE LU_Factor_MSR_to_Band_LU


  SUBROUTINE SOLVE_Band_LU( LU, b, x )
    TYPE(Band_LU),                 intent(in) :: LU
    real(prec), dimension(LU % N), intent(in) :: b
    real(prec), dimension(LU % N), intent(out):: x

    x = b

        !provide overloaded GBSL instead [waw]
        CALL GBSL( LU%ABD, SIZE(LU%ABD,1), &
           LU%N, LU%Lower_band_width, LU%Upper_band_width, LU%IPVT, &
           x , 1 )

        !commented out by [waw]
        !SELECT CASE ( KIND(LU % ABD(1,1)) )
    !CASE ( single_prec )
    !!   CALL SGBSL( REAL(LU%ABD,single_prec), SIZE(LU%ABD,1), &
        !!   LU%N, LU%Lower_band_width, LU%Upper_band_width, LU%IPVT, &
        !!   REAL(x,single_prec), 1 )
    !CASE ( double_prec )
    !!   CALL DGBSL( REAL(LU%ABD,double_prec), SIZE(LU%ABD,1), &
        !!   LU%N, LU%Lower_band_width, LU%Upper_band_width, LU%IPVT, &
        !!   REAL(x,double_prec), 1 )
    !CASE DEFAULT
    !!   STOP "Real type has to be either single or double precision for band Gaussian solver."
    !END SELECT

  END SUBROUTINE SOLVE_Band_LU

END MODULE USR_SMlib_Band_Gauss_Solver
