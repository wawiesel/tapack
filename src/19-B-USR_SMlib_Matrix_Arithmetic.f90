MODULE USR_SMlib_Matrix_Arithmetic
  !
  !!  This module is part of SMLIB v. 1.1.  It collects all the matrices
  !!  modules.  It also contains a routine for conversion of an MSR matrix
  !!  to a CSR matrix by an overloaded assignment(=) operator.
  !
  !! ------------------------------------------------------------------------
  !
  !!  Copyright (C) 1996, 1997, 1998,  Ernst A. Meese
  !!    Refer to the file copyright.doc for details and important disclaimer.
  !
  use USR_SMlib_CSR        !!((17-B-USR_SMlib_CSR.f90))
  use USR_SMlib_MSR        !!((17-B-USR_SMlib_MSR.f90))
  use USR_SMlib_LU_CSR_MSR !!((18-B-USR_SMlib_LU_CSR_MSR.f90))
  use USR_SMlib_Band_LU    !!((17-B-USR_SMlib_Band_LU.f90))

  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE CSR_from_MSR
  END INTERFACE

CONTAINS

  SUBROUTINE CSR_from_MSR( C, M )
    TYPE(CSR), INTENT(INOUT) :: C
    TYPE(MSR), INTENT(IN)    :: M
    INTEGER :: I

    IF ( ALLOCATED_MATRIX(C) ) CALL DEALLOCATE_MATRIX(C)
    CALL ALLOCATE_MATRIX( C, M % N, M % JA ( M % N + 1 ) )

    DO I = 1, M % N
       Call setRow ( C, I, (/ I,         M % JA (M % JA (I) : M % JA(I+1)-1)  /), &
                           (/ M % A (I), M % A  (M % JA (I) : M % JA(I+1)-1)  /) )
    END DO

  END SUBROUTINE CSR_from_MSR

END MODULE USR_SMlib_Matrix_Arithmetic
