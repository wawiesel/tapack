MODULE USR_SMlib_GMRES
  !
  !!  This module is part of SMLIB v. 1.1.  It contains procedures needed
  !!  to perform GMRES iterations to solve equations S x = b.  Currently
  !!  implemented is the CSR and the MSR sparse matrix storage.  Thanks to
  !!  prof. Y. Saad for granting access to his SPARSKIT FORTRAN77 code, making
  !!  me able to study details of his GMRES method.
  !
  !! ------------------------------------------------------------------------
  !
  !!  Copyright (C) 1996 Ernst A. Meese
  !!    Refer to the file copyright.doc for details and important disclaimer.
  !
  !!  Created: January 1996
  !!  Development version
  !
  !! ------------------------------------------------------------------------
  !!  Module contents
  !
  !!   Interfaces:
  !!      GMRES - Generic procedure for the GMRES algorithm.
  !
  !!   Procedures:
  !!      GMRES_P_MSR - Preconditioned GMRES for the MSR datastructure.
  !
  !! -----------------------------------------------------------------------
  !
  !!  We use the modules defining abstract datatypes for the matrices and
  !!  the module for features common for all iterative methods.
  !
  use USR_SMlib_Matrix_Arithmetic  !!((19-B-USR_SMlib_Matrix_Arithmetic.f90))
  use USR_SMlib_Iteration_Defaults !!((17-B-USR_SMlib_Iteration_Defaults.f90))

  IMPLICIT NONE

  INTERFACE GMRES
     MODULE PROCEDURE GMRES_MSR
  END INTERFACE

CONTAINS

  SUBROUTINE GMRES_MSR &
       (m, x, A, B, PC, init, tol, reduction_factor, min_it, max_it, INFO, no_of_its, &
       residual_norm, history)
    TYPE(MSR),        INTENT (IN) :: A
    TYPE(LU_CSR_MSR), INTENT (IN) :: PC
    INCLUDE "20-B-USR_SMlib_GMRES.f90.bdy"
  END SUBROUTINE GMRES_MSR

END MODULE USR_SMlib_GMRES


