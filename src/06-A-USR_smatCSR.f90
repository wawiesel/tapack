!!# USER MODULE: <USR_smatCSR>
MODULE USR_smatCSR

!!## PURPOSE
!! This module is part of SMLIB v. 1.1.  It contains the data-structure
!! and procedures for manipulating the CSR (compressed sparse row)
!! sparse matrix format.

!!## SOURCE
!!  Copyright (C) 1996 Ernst A. Meese
!!    Refer to the file copyright.doc for details and important disclaimer.
!
!! Created: 30.1.96 by Ernst A. Meese
!! Version 1.0b, validated by TESTS/CSR/tester 1.2.96
!! Version 1.0.1b, validated by TESTS/CSR/tester 3.9.97

!!## MODIFICATIONS
!! William Wieselquist - added overloading for two real types (single and double
!!                       precision as defined in module KND and reformatted.

!!## EXTERNAL KINDS
!! @ get the two kinds to use in this module
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,KIND_Csp,KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL PARAMETERS
!! @ lengths of type-kinds
USE PAR_IntrinsicLengths,ONLY: LEN_Rsp,LEN_Rdp,LEN_Csp,LEN_Cdp   !!((02-A-PAR_IntrinsicLengths.f90))

!!## EXTERNAL PROCEDURES
USE FUN_NewUnit                                                  !!((04-B-FUN_NewUnit.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PARAMETERS
INTEGER,PARAMETER :: Block_Size = 50

!!## TYPE DEFINITIONS
!! @ single precision real
TYPE TYPE_smatCSR_Rsp
 INTEGER                :: N     =  0
 REAL(KIND_Rsp),POINTER :: A(:)  => NULL()
 INTEGER       ,POINTER :: IA(:) => NULL()
 INTEGER       ,POINTER :: JA(:) => NULL()
END TYPE
!! @ double precision real
TYPE TYPE_smatCSR_Rdp
 INTEGER                :: N     =  0
 REAL(KIND_Rdp),POINTER :: A(:)  => NULL()
 INTEGER       ,POINTER :: IA(:) => NULL()
 INTEGER       ,POINTER :: JA(:) => NULL()
END TYPE
!! @ single precision complex
TYPE TYPE_smatCSR_Csp
 INTEGER                  :: N     =  0
 COMPLEX(KIND_Csp),POINTER :: A(:)  => NULL()
 INTEGER         ,POINTER :: IA(:) => NULL()
 INTEGER         ,POINTER :: JA(:) => NULL()
END TYPE
!! @ double precision complex
TYPE TYPE_smatCSR_Cdp
 INTEGER                  :: N     =  0
 COMPLEX(KIND_Cdp),POINTER :: A(:)  => NULL()
 INTEGER         ,POINTER :: IA(:) => NULL()
 INTEGER         ,POINTER :: JA(:) => NULL()
END TYPE

!!## PROCEDURE OVERLOADING
!! @ allocation
INTERFACE ALLOCATE
 MODULE PROCEDURE ALLOCATE_smatCSR_Rsp
 MODULE PROCEDURE ALLOCATE_smatCSR_Rdp
 MODULE PROCEDURE ALLOCATE_smatCSR_Csp
 MODULE PROCEDURE ALLOCATE_smatCSR_Cdp
END INTERFACE
!! @ reallocation (involves memory copy)
INTERFACE REALLOCATE
 MODULE PROCEDURE REALLOCATE_smatCSR_Rsp
 MODULE PROCEDURE REALLOCATE_smatCSR_Rdp
 MODULE PROCEDURE REALLOCATE_smatCSR_Csp
 MODULE PROCEDURE REALLOCATE_smatCSR_Cdp
END INTERFACE
!! @ deallocation
INTERFACE DEALLOCATE
 MODULE PROCEDURE DEALLOCATE_smatCSR_Rsp
 MODULE PROCEDURE DEALLOCATE_smatCSR_Rdp
 MODULE PROCEDURE DEALLOCATE_smatCSR_Csp
 MODULE PROCEDURE DEALLOCATE_smatCSR_Cdp
END INTERFACE
!! @ nullify
INTERFACE NULLIFY
 MODULE PROCEDURE NULLIFY_smatCSR_Rsp
 MODULE PROCEDURE NULLIFY_smatCSR_Rdp
 MODULE PROCEDURE NULLIFY_smatCSR_Csp
 MODULE PROCEDURE NULLIFY_smatCSR_Cdp
END INTERFACE
!! @ allocated inquiry function
INTERFACE ALLOCATED
 MODULE PROCEDURE ALLOCATED_smatCSR_Rsp
 MODULE PROCEDURE ALLOCATED_smatCSR_Rdp
 MODULE PROCEDURE ALLOCATED_smatCSR_Csp
 MODULE PROCEDURE ALLOCATED_smatCSR_Cdp
END INTERFACE
!! @ entry retrieval
INTERFACE ENTRY
 MODULE PROCEDURE Entry_smatCSR_Rsp
 MODULE PROCEDURE Entry_smatCSR_Rdp
 MODULE PROCEDURE Entry_smatCSR_Csp
 MODULE PROCEDURE Entry_smatCSR_Cdp
END INTERFACE
!! @ set row
INTERFACE SetRow
 MODULE PROCEDURE SetRow_smatCSR_Rsp
 MODULE PROCEDURE SetRow_smatCSR_Rdp
 MODULE PROCEDURE SetRow_smatCSR_Csp
 MODULE PROCEDURE SetRow_smatCSR_Cdp
END INTERFACE
!! @ is lower triangular inquiry
INTERFACE IsLower
 MODULE PROCEDURE IsLower_smatCSR_Rsp
 MODULE PROCEDURE IsLower_smatCSR_Rdp
 MODULE PROCEDURE IsLower_smatCSR_Csp
 MODULE PROCEDURE IsLower_smatCSR_Cdp
END INTERFACE
!! @ is upper triangular inquiry
INTERFACE IsUpper
 MODULE PROCEDURE IsUpper_smatCSR_Rsp
 MODULE PROCEDURE IsUpper_smatCSR_Rdp
 MODULE PROCEDURE IsUpper_smatCSR_Csp
 MODULE PROCEDURE IsUpper_smatCSR_Cdp
END INTERFACE
!! @ check for structure correctness
INTERFACE IsOK
 MODULE PROCEDURE IsOK_smatCSR_Rsp
 MODULE PROCEDURE IsOK_smatCSR_Rdp
 MODULE PROCEDURE IsOK_smatCSR_Csp
 MODULE PROCEDURE IsOK_smatCSR_Cdp
END INTERFACE
!! @ set a value
INTERFACE SetVal
 MODULE PROCEDURE SetVal_smatCSR_Rsp
 MODULE PROCEDURE SetVal_smatCSR_Rdp
 MODULE PROCEDURE SetVal_smatCSR_Csp
 MODULE PROCEDURE SetVal_smatCSR_Cdp
END INTERFACE
!! @ add a value
INTERFACE AddVal
 MODULE PROCEDURE AddVal_smatCSR_Rsp
 MODULE PROCEDURE AddVal_smatCSR_Rdp
 MODULE PROCEDURE AddVal_smatCSR_Csp
 MODULE PROCEDURE AddVal_smatCSR_Cdp
END INTERFACE
!! @ sum of a row
INTERFACE RowSum
 MODULE PROCEDURE RowSum_smatCSR_Rsp
 MODULE PROCEDURE RowSum_smatCSR_Rdp
 MODULE PROCEDURE RowSum_smatCSR_Csp
 MODULE PROCEDURE RowSum_smatCSR_Cdp
END INTERFACE
!! @ save
INTERFACE SAVE
 MODULE PROCEDURE SAVE_smatCSR_Rsp
 MODULE PROCEDURE SAVE_smatCSR_Rdp
 MODULE PROCEDURE SAVE_smatCSR_Csp
 MODULE PROCEDURE SAVE_smatCSR_Cdp
END INTERFACE
!! @ number of nonzeros on a row
INTERFACE RowNNZ
 MODULE PROCEDURE RowNNZ_smatCSR_Rsp
 MODULE PROCEDURE RowNNZ_smatCSR_Rdp
 MODULE PROCEDURE RowNNZ_smatCSR_Csp
 MODULE PROCEDURE RowNNZ_smatCSR_Cdp
END INTERFACE
!! @ kind inquiry
INTERFACE KIND
 MODULE PROCEDURE KIND_smatCSR_Rsp
 MODULE PROCEDURE KIND_smatCSR_Rdp
 MODULE PROCEDURE KIND_smatCSR_Csp
 MODULE PROCEDURE KIND_smatCSR_Cdp
END INTERFACE
!! @ absolute value
INTERFACE ABS
 MODULE PROCEDURE ABS_smatCSR_Rsp
 MODULE PROCEDURE ABS_smatCSR_Rdp
 MODULE PROCEDURE ABS_smatCSR_Csp
 MODULE PROCEDURE ABS_smatCSR_Cdp
END INTERFACE
!! @ matrix multiplication function
INTERFACE MATMUL
 MODULE PROCEDURE MATMUL_smatCSR_dvec_Rsp
 MODULE PROCEDURE MATMUL_smatCSR_dvec_Rdp
 MODULE PROCEDURE MATMUL_smatCSR_dvec_Csp
 MODULE PROCEDURE MATMUL_smatCSR_dvec_Cdp
 MODULE PROCEDURE MATMUL_smatCSR_smatCSR_Rsp
 MODULE PROCEDURE MATMUL_smatCSR_smatCSR_Rdp
 MODULE PROCEDURE MATMUL_smatCSR_smatCSR_Csp
 MODULE PROCEDURE MATMUL_smatCSR_smatCSR_Cdp
END INTERFACE
!! @ matrix addition
INTERFACE ADD
 MODULE PROCEDURE ADD_smatCSR_smatCSR_Rsp
 MODULE PROCEDURE ADD_smatCSR_smatCSR_Rdp
 MODULE PROCEDURE ADD_smatCSR_smatCSR_Csp
 MODULE PROCEDURE ADD_smatCSR_smatCSR_Cdp
END INTERFACE
!! @ copy one matrix into another (first=second)
INTERFACE COPY
 MODULE PROCEDURE COPY_smatCSR_Rsp
 MODULE PROCEDURE COPY_smatCSR_Rdp
 MODULE PROCEDURE COPY_smatCSR_Csp
 MODULE PROCEDURE COPY_smatCSR_Cdp
END INTERFACE
!! @ identity matrix
INTERFACE Eye
 MODULE PROCEDURE Eye_smatCSR_Rsp
 MODULE PROCEDURE Eye_smatCSR_Rdp
 MODULE PROCEDURE Eye_smatCSR_Csp
 MODULE PROCEDURE Eye_smatCSR_Cdp
END INTERFACE


!!## OPERATOR DEFINITION
!! @ matrix multiplication operator
INTERFACE OPERATOR(*)
 MODULE PROCEDURE MUL_smatCSR_scalar_Rsp
 MODULE PROCEDURE MUL_smatCSR_scalar_Rdp
 MODULE PROCEDURE MUL_smatCSR_scalar_Csp
 MODULE PROCEDURE MUL_smatCSR_scalar_Cdp
 MODULE PROCEDURE MATMUL_smatCSR_dvec_Rsp
 MODULE PROCEDURE MATMUL_smatCSR_dvec_Rdp
 MODULE PROCEDURE MATMUL_smatCSR_dvec_Csp
 MODULE PROCEDURE MATMUL_smatCSR_dvec_Cdp
 MODULE PROCEDURE MATMUL_smatCSR_smatCSR_Rsp
 MODULE PROCEDURE MATMUL_smatCSR_smatCSR_Rdp
 MODULE PROCEDURE MATMUL_smatCSR_smatCSR_Csp
 MODULE PROCEDURE MATMUL_smatCSR_smatCSR_Cdp
END INTERFACE
!! @ dense matrix = CSR matrix
INTERFACE ASSIGNMENT(=)
 MODULE PROCEDURE EQUALS_dmat_smatCSR_Rsp
 MODULE PROCEDURE EQUALS_dmat_smatCSR_Rdp
 MODULE PROCEDURE EQUALS_dmat_smatCSR_Csp
 MODULE PROCEDURE EQUALS_dmat_smatCSR_Cdp
END INTERFACE
!! @ CSR matrix = dense matrix
INTERFACE ASSIGNMENT(=)
 MODULE PROCEDURE EQUALS_smatCSR_dmat_Rsp
 MODULE PROCEDURE EQUALS_smatCSR_dmat_Rdp
 MODULE PROCEDURE EQUALS_smatCSR_dmat_Csp
 MODULE PROCEDURE EQUALS_smatCSR_dmat_Cdp
END INTERFACE
!! @ CSR matrix = CSR matrix
INTERFACE ASSIGNMENT(=)
 MODULE PROCEDURE COPY_smatCSR_Rsp
 MODULE PROCEDURE COPY_smatCSR_Rdp
 MODULE PROCEDURE COPY_smatCSR_Csp
 MODULE PROCEDURE COPY_smatCSR_Cdp
END INTERFACE
!! @ matrix addition
INTERFACE OPERATOR(+)
 MODULE PROCEDURE ADD_smatCSR_smatCSR_Rsp
 MODULE PROCEDURE ADD_smatCSR_smatCSR_Rdp
 MODULE PROCEDURE ADD_smatCSR_smatCSR_Csp
 MODULE PROCEDURE ADD_smatCSR_smatCSR_Cdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: TYPE_smatCSR_Rsp
PUBLIC :: TYPE_smatCSR_Rdp
PUBLIC :: TYPE_smatCSR_Csp
PUBLIC :: TYPE_smatCSR_Cdp
PUBLIC :: OPERATOR(*),OPERATOR(+)
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: ALLOCATE
PUBLIC :: REALLOCATE
PUBLIC :: DEALLOCATE
PUBLIC :: NULLIFY
PUBLIC :: ALLOCATED
PUBLIC :: entry
PUBLIC :: SetRow
PUBLIC :: IsLower
PUBLIC :: IsUpper
PUBLIC :: IsOK
PUBLIC :: SetVal
PUBLIC :: AddVal
PUBLIC :: RowSum
PUBLIC :: SAVE
PUBLIC :: RowNNZ
PUBLIC :: KIND
PUBLIC :: ABS
PUBLIC :: MATMUL
PUBLIC :: COPY
PUBLIC :: ADD
PUBLIC :: Eye


!!## MODULE PROCEDURES
CONTAINS


SUBROUTINE NULLIFY_smatCSR_Rsp (A)
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(INOUT) :: A
!!--begin--
INCLUDE "06-A-USR_smatCSR__NULLIFY.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE NULLIFY_smatCSR_Rdp (A)
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(INOUT) :: A
!!--begin--
INCLUDE "06-A-USR_smatCSR__NULLIFY.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE NULLIFY_smatCSR_Csp (A)
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Csp),INTENT(INOUT) :: A
!!--begin--
INCLUDE "06-A-USR_smatCSR__NULLIFY.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE NULLIFY_smatCSR_Cdp (A)
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(INOUT) :: A
!!--begin--
INCLUDE "06-A-USR_smatCSR__NULLIFY.f90.bdy"
!!--end--
END SUBROUTINE



PURE FUNCTION KIND_smatCSR_Rsp( A ) RESULT(KIND)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
INTEGER :: KIND
!!--begin--
KIND = KIND_Rsp
!!--end--
END FUNCTION

PURE FUNCTION KIND_smatCSR_Rdp( A ) RESULT(KIND)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
INTEGER :: KIND
!!--begin--
KIND = KIND_Rdp
!!--end--
END FUNCTION

PURE FUNCTION KIND_smatCSR_Csp( A ) RESULT(KIND)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
INTEGER :: KIND
!!--begin--
KIND = KIND_Csp
!!--end--
END FUNCTION

PURE FUNCTION KIND_smatCSR_Cdp( A ) RESULT(KIND)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
INTEGER :: KIND
!!--begin--
KIND = KIND_Cdp
!!--end--
END FUNCTION


PURE FUNCTION ABS_smatCSR_Rsp( A ) RESULT(B)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rsp) :: B
!!--begin--
INCLUDE "06-A-USR_smatCSR__ABS.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION ABS_smatCSR_Rdp( A ) RESULT(B)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rdp) :: B
!!--begin--
INCLUDE "06-A-USR_smatCSR__ABS.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION ABS_smatCSR_Csp( A ) RESULT(B)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Csp) :: B
!!--begin--
INCLUDE "06-A-USR_smatCSR__ABS.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION ABS_smatCSR_Cdp( A ) RESULT(B)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Cdp) :: B
!!--begin--
INCLUDE "06-A-USR_smatCSR__ABS.f90.bdy"
!!--end--
END FUNCTION


PURE SUBROUTINE ALLOCATE_smatCSR_Rsp (A, N, NZMAX, InitDiag_scalar , InitDiag_dvec )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Rsp),INTENT(INOUT) :: A
INCLUDE "06-A-USR_smatCSR__ALLOCATE_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE ALLOCATE_smatCSR_Rdp (A, N, NZMAX, InitDiag_scalar , InitDiag_dvec )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Rdp),INTENT(INOUT) :: A
INCLUDE "06-A-USR_smatCSR__ALLOCATE_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE ALLOCATE_smatCSR_Csp (A, N, NZMAX, InitDiag_scalar , InitDiag_dvec )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Csp),INTENT(INOUT) :: A
INCLUDE "06-A-USR_smatCSR__ALLOCATE_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE ALLOCATE_smatCSR_Cdp (A, N, NZMAX , InitDiag_scalar , InitDiag_dvec )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Cdp),INTENT(INOUT) :: A
INCLUDE "06-A-USR_smatCSR__ALLOCATE_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE


PURE SUBROUTINE REALLOCATE_smatCSR_Rsp (S, DELTA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__REALLOCATE_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__REALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE REALLOCATE_smatCSR_Rdp (S, DELTA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__REALLOCATE_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__REALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE REALLOCATE_smatCSR_Csp (S, DELTA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Csp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__REALLOCATE_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__REALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE REALLOCATE_smatCSR_Cdp (S, DELTA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__REALLOCATE_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__REALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE



PURE SUBROUTINE DEALLOCATE_smatCSR_Rsp (A)
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Rsp),INTENT(INOUT) :: A
!!--begin--
INCLUDE "06-A-USR_smatCSR__DEALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE DEALLOCATE_smatCSR_Rdp (A)
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Rdp),INTENT(INOUT) :: A
!!--begin--
INCLUDE "06-A-USR_smatCSR__DEALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE DEALLOCATE_smatCSR_Csp (A)
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Csp),INTENT(INOUT) :: A
!!--begin--
INCLUDE "06-A-USR_smatCSR__DEALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE DEALLOCATE_smatCSR_Cdp (A)
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Cdp),INTENT(INOUT) :: A
!!--begin--
INCLUDE "06-A-USR_smatCSR__DEALLOCATE.f90.bdy"
!!--end--
END SUBROUTINE



PURE FUNCTION ALLOCATED_smatCSR_Rsp( A ) RESULT(ALLOCATED)
!!#### REQUIRED INPUT
TYPE (TYPE_smatCSR_Rsp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__ALLOCATED.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ALLOCATED.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION ALLOCATED_smatCSR_Rdp( A ) RESULT(ALLOCATED)
!!#### REQUIRED INPUT
TYPE (TYPE_smatCSR_Rdp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__ALLOCATED.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ALLOCATED.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION ALLOCATED_smatCSR_Csp( A ) RESULT(ALLOCATED)
!!#### REQUIRED INPUT
TYPE (TYPE_smatCSR_Csp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__ALLOCATED.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ALLOCATED.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION ALLOCATED_smatCSR_Cdp( A ) RESULT(ALLOCATED)
!!#### REQUIRED INPUT
TYPE (TYPE_smatCSR_Cdp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__ALLOCATED.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ALLOCATED.f90.bdy"
!!--end--
END FUNCTION



FUNCTION ENTRY_smatCSR_Rsp ( A , i , j ) RESULT(Entry)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__ENTRY_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ENTRY.f90.bdy"
!!--end--
END FUNCTION

FUNCTION ENTRY_smatCSR_Rdp ( A , i , j ) RESULT(Entry)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__ENTRY_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ENTRY.f90.bdy"
!!--end--
END FUNCTION

FUNCTION ENTRY_smatCSR_Csp ( A , i , j ) RESULT(Entry)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__ENTRY_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ENTRY.f90.bdy"
!!--end--
END FUNCTION

FUNCTION ENTRY_smatCSR_Cdp ( A , i , j ) RESULT(Entry)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__ENTRY_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ENTRY.f90.bdy"
!!--end--
END FUNCTION



PURE SUBROUTINE SetRow_smatCSR_Rsp( A , i , cols , vals )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Rsp),INTENT(INOUT) :: A
INCLUDE "06-A-USR_smatCSR__SetRow_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SetRow.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE SetRow_smatCSR_Rdp( A , i , cols , vals )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Rdp),INTENT(INOUT) :: A
INCLUDE "06-A-USR_smatCSR__SetRow_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SetRow.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE SetRow_smatCSR_Csp( A , i , cols , vals )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Csp),INTENT(INOUT) :: A
INCLUDE "06-A-USR_smatCSR__SetRow_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SetRow.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE SetRow_smatCSR_Cdp( A , i , cols , vals )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE (TYPE_smatCSR_Cdp),INTENT(INOUT) :: A
INCLUDE "06-A-USR_smatCSR__SetRow_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SetRow.f90.bdy"
!!--end--
END SUBROUTINE



PURE FUNCTION MATMUL_smatCSR_dvec_Rsp( A , x ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__MATMUL_dvec_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MATMUL_dvec.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MATMUL_smatCSR_dvec_Rdp( A , x ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__MATMUL_dvec_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MATMUL_dvec.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MATMUL_smatCSR_dvec_Csp( A , x ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__MATMUL_dvec_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MATMUL_dvec.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MATMUL_smatCSR_dvec_Cdp( A , x ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__MATMUL_dvec_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MATMUL_dvec.f90.bdy"
!!--end--
END FUNCTION



PURE FUNCTION MUL_smatCSR_scalar_Rsp( A , x ) RESULT(B)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rsp) :: B
INCLUDE "06-A-USR_smatCSR__MUL_scalar_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MUL_scalar.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MUL_smatCSR_scalar_Rdp( A , x ) RESULT(B)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rdp) :: B
INCLUDE "06-A-USR_smatCSR__MUL_scalar_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MUL_scalar.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MUL_smatCSR_scalar_Csp( A , x ) RESULT(B)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Csp) :: B
INCLUDE "06-A-USR_smatCSR__MUL_scalar_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MUL_scalar.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MUL_smatCSR_scalar_Cdp( A , x ) RESULT(B)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: A
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Cdp) :: B
INCLUDE "06-A-USR_smatCSR__MUL_scalar_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MUL_scalar.f90.bdy"
!!--end--
END FUNCTION



PURE FUNCTION MATMUL_smatCSR_smatCSR_Rsp( A , B ) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: A
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: B
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rsp) :: C
INCLUDE "06-A-USR_smatCSR__MATMUL_smatCSR_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MATMUL_smatCSR.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MATMUL_smatCSR_smatCSR_Rdp( A , B ) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: B
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rdp) :: C
INCLUDE "06-A-USR_smatCSR__MATMUL_smatCSR_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MATMUL_smatCSR.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MATMUL_smatCSR_smatCSR_Csp( A , B ) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: A
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: B
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Csp) :: C
INCLUDE "06-A-USR_smatCSR__MATMUL_smatCSR_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MATMUL_smatCSR.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION MATMUL_smatCSR_smatCSR_Cdp( A , B ) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: A
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: B
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Cdp) :: C
INCLUDE "06-A-USR_smatCSR__MATMUL_smatCSR_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__MATMUL_smatCSR.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION ADD_smatCSR_smatCSR_Rsp( A , B ) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: A
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: B
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rsp) :: C
INCLUDE "06-A-USR_smatCSR__ADD_smatCSR_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ADD_smatCSR.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION ADD_smatCSR_smatCSR_Rdp( A , B ) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: B
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rdp) :: C
INCLUDE "06-A-USR_smatCSR__ADD_smatCSR_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ADD_smatCSR.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION ADD_smatCSR_smatCSR_Csp( A , B ) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: A
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: B
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Csp) :: C
INCLUDE "06-A-USR_smatCSR__ADD_smatCSR_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ADD_smatCSR.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION ADD_smatCSR_smatCSR_Cdp( A , B ) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: A
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: B
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Cdp) :: C
INCLUDE "06-A-USR_smatCSR__ADD_smatCSR_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__ADD_smatCSR.f90.bdy"
!!--end--
END FUNCTION



SUBROUTINE EQUALS_smatCSR_dmat_Rsp( S , A )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(OUT) :: S
INCLUDE "06-A-USR_smatCSR__EQUALS_dmat_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__EQUALS_dmat.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE EQUALS_smatCSR_dmat_Rdp( S , A )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(OUT) :: S
INCLUDE "06-A-USR_smatCSR__EQUALS_dmat_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__EQUALS_dmat.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE EQUALS_smatCSR_dmat_Csp( S , A )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Csp),INTENT(OUT) :: S
INCLUDE "06-A-USR_smatCSR__EQUALS_dmat_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__EQUALS_dmat.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE EQUALS_smatCSR_dmat_Cdp( S , A )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(OUT) :: S
INCLUDE "06-A-USR_smatCSR__EQUALS_dmat_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__EQUALS_dmat.f90.bdy"
!!--end--
END SUBROUTINE



SUBROUTINE EQUALS_dmat_smatCSR_Rsp( A , S )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__dmat_EQUALS_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__dmat_EQUALS.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE EQUALS_dmat_smatCSR_Rdp( A , S )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__dmat_EQUALS_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__dmat_EQUALS.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE EQUALS_dmat_smatCSR_Csp( A , S )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__dmat_EQUALS_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__dmat_EQUALS.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE EQUALS_dmat_smatCSR_Cdp( A , S )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__dmat_EQUALS_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__dmat_EQUALS.f90.bdy"
!!--end--
END SUBROUTINE



PURE SUBROUTINE COPY_smatCSR_Rsp( S , A )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(OUT) :: S
TYPE(TYPE_smatCSR_Rsp),INTENT(IN)  :: A
INCLUDE "06-A-USR_smatCSR__COPY.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__COPY.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE COPY_smatCSR_Rdp( S , A )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(OUT) :: S
TYPE(TYPE_smatCSR_Rdp),INTENT(IN)  :: A
INCLUDE "06-A-USR_smatCSR__COPY.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__COPY.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE COPY_smatCSR_Csp( S , A )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(OUT) :: S
TYPE(TYPE_smatCSR_Csp),INTENT(IN)  :: A
INCLUDE "06-A-USR_smatCSR__COPY.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__COPY.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE COPY_smatCSR_Cdp( S , A )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(OUT) :: S
TYPE(TYPE_smatCSR_Cdp),INTENT(IN)  :: A
INCLUDE "06-A-USR_smatCSR__COPY.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__COPY.f90.bdy"
!!--end--
END SUBROUTINE



FUNCTION IsLower_smatCSR_Rsp( S ) RESULT(IsLower)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsLower.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsLower.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsLower_smatCSR_Rdp( S ) RESULT(IsLower)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsLower.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsLower.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsLower_smatCSR_Csp( S ) RESULT(IsLower)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsLower.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsLower.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsLower_smatCSR_Cdp( S ) RESULT(IsLower)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsLower.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsLower.f90.bdy"
!!--end--
END FUNCTION


FUNCTION IsUpper_smatCSR_Rsp( S ) RESULT(IsUpper)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsUpper.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsUpper.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsUpper_smatCSR_Rdp( S ) RESULT(IsUpper)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsUpper.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsUpper.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsUpper_smatCSR_Csp( S ) RESULT(IsUpper)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsUpper.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsUpper.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsUpper_smatCSR_Cdp( S ) RESULT(IsUpper)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsUpper.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsUpper.f90.bdy"
!!--end--
END FUNCTION



FUNCTION IsStrictlyLower_smatCSR_Rsp( S ) RESULT(IsStrictlyLower)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsStrictlyLower.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsStrictlyLower.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsStrictlyLower_smatCSR_Rdp( S ) RESULT(IsStrictlyLower)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsStrictlyLower.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsStrictlyLower.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsStrictlyLower_smatCSR_Csp( S ) RESULT(IsStrictlyLower)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsStrictlyLower.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsStrictlyLower.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsStrictlyLower_smatCSR_Cdp( S ) RESULT(IsStrictlyLower)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsStrictlyLower.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsStrictlyLower.f90.bdy"
!!--end--
END FUNCTION



FUNCTION IsStrictlyUpper_smatCSR_Rsp( S ) RESULT(IsStrictlyUpper)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsStrictlyUpper.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsStrictlyUpper.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsStrictlyUpper_smatCSR_Rdp( S ) RESULT(IsStrictlyUpper)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsStrictlyUpper.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsStrictlyUpper.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsStrictlyUpper_smatCSR_Csp( S ) RESULT(IsStrictlyUpper)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsStrictlyUpper.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsStrictlyUpper.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsStrictlyUpper_smatCSR_Cdp( S ) RESULT(IsStrictlyUpper)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsStrictlyUpper.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsStrictlyUpper.f90.bdy"
!!--end--
END FUNCTION



FUNCTION IsOK_smatCSR_Rsp ( S ) RESULT(IsOK)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsOK.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsOK.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsOK_smatCSR_Rdp ( S ) RESULT(IsOK)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsOK.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsOK.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsOK_smatCSR_Csp ( S ) RESULT(IsOK)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsOK.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsOK.f90.bdy"
!!--end--
END FUNCTION

FUNCTION IsOK_smatCSR_Cdp ( S ) RESULT(IsOK)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__IsOK.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__IsOK.f90.bdy"
!!--end--
END FUNCTION



SUBROUTINE SetVal_smatCSR_Rsp( S , i , j , val )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__SetVal_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SetVal.f90.bdy"
END SUBROUTINE

SUBROUTINE SetVal_smatCSR_Rdp( S , i , j , val )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__SetVal_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SetVal.f90.bdy"
END SUBROUTINE

SUBROUTINE SetVal_smatCSR_Csp( S , i , j , val )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Csp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__SetVal_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SetVal.f90.bdy"
END SUBROUTINE

SUBROUTINE SetVal_smatCSR_Cdp( S , i , j , val )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__SetVal_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SetVal.f90.bdy"
END SUBROUTINE



SUBROUTINE AddVal_smatCSR_Rsp( S , i , j , val )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__AddVal_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__AddVal.f90.bdy"
END SUBROUTINE

SUBROUTINE AddVal_smatCSR_Rdp( S , i , j , val )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__AddVal_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__AddVal.f90.bdy"
END SUBROUTINE

SUBROUTINE AddVal_smatCSR_Csp( S , i , j , val )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Csp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__AddVal_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__AddVal.f90.bdy"
END SUBROUTINE

SUBROUTINE AddVal_smatCSR_Cdp( S , i , j , val )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(INOUT) :: S
INCLUDE "06-A-USR_smatCSR__AddVal_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__AddVal.f90.bdy"
END SUBROUTINE



FUNCTION RowSum_smatCSR_Rsp( S, i ) RESULT( RowSum )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__RowSum_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__RowSum.f90.bdy"
!!--end--
END FUNCTION

FUNCTION RowSum_smatCSR_Rdp( S, i ) RESULT( RowSum )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__RowSum_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__RowSum.f90.bdy"
!!--end--
END FUNCTION

FUNCTION RowSum_smatCSR_Csp( S, i ) RESULT( RowSum )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__RowSum_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__RowSum.f90.bdy"
!!--end--
END FUNCTION

FUNCTION RowSum_smatCSR_Cdp( S, i ) RESULT( RowSum )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__RowSum_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__RowSum.f90.bdy"
!!--end--
END FUNCTION



FUNCTION RowNNZ_smatCSR_Rsp( S, i ) RESULT(RowNNZ)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__RowNNZ.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__RowNNZ.f90.bdy"
!!--end--
END FUNCTION

FUNCTION RowNNZ_smatCSR_Rdp( S, i ) RESULT(RowNNZ)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__RowNNZ.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__RowNNZ.f90.bdy"
!!--end--
END FUNCTION

FUNCTION RowNNZ_smatCSR_Csp( S, i ) RESULT(RowNNZ)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__RowNNZ.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__RowNNZ.f90.bdy"
!!--end--
END FUNCTION

FUNCTION RowNNZ_smatCSR_Cdp( S, i ) RESULT(RowNNZ)
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: S
INCLUDE "06-A-USR_smatCSR__RowNNZ.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__RowNNZ.f90.bdy"
!!--end--
END FUNCTION



SUBROUTINE SAVE_smatCSR_Rsp ( SA , A , form )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: LEN_R = LEN_Rsp
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__SAVE_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SAVE.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE SAVE_smatCSR_Rdp ( SA , A , form )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: LEN_R = LEN_Rdp
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__SAVE_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SAVE.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE SAVE_smatCSR_Csp ( SA , A , form )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: LEN_C = LEN_Csp
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__SAVE_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SAVE.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE SAVE_smatCSR_Cdp ( SA , A , form )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: LEN_C = LEN_Cdp
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN) :: A
INCLUDE "06-A-USR_smatCSR__SAVE_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__SAVE.f90.bdy"
!!--end--
END SUBROUTINE



FUNCTION Eye_smatCSR_Rsp( S ) RESULT(Eye)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rsp),INTENT(IN) :: S
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rsp) :: Eye
INCLUDE "06-A-USR_smatCSR__Eye_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__Eye.f90.bdy"
!!--end--
END FUNCTION

FUNCTION Eye_smatCSR_Rdp( S ) RESULT(Eye)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: S
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Rdp) :: Eye
INCLUDE "06-A-USR_smatCSR__Eye_R.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__Eye.f90.bdy"
!!--end--
END FUNCTION

FUNCTION Eye_smatCSR_Csp( S ) RESULT(Eye)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Csp),INTENT(IN)  :: S
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Csp) :: Eye
INCLUDE "06-A-USR_smatCSR__Eye_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__Eye.f90.bdy"
!!--end--
END FUNCTION

FUNCTION Eye_smatCSR_Cdp( S ) RESULT(Eye)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                    !!((01-A-KND_IntrinsicTypes.f90))
!!#### REQUIRED INPUT
TYPE(TYPE_smatCSR_Cdp),INTENT(IN)  :: S
!!#### REQUIRED OUTPUT
TYPE(TYPE_smatCSR_Cdp) :: Eye
INCLUDE "06-A-USR_smatCSR__Eye_C.f90.hdr"
!!--begin--
INCLUDE "06-A-USR_smatCSR__Eye.f90.bdy"
!!--end--
END FUNCTION


END MODULE
