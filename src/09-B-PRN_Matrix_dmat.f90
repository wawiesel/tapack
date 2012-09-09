!!### MODULE: PRINTING subroutine Matrix_dmat
MODULE PRN_Matrix_dmat
!!#### PURPOSE
!! This subroutine prints real or complex dense matrices (dmat).

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
                          KIND_Csp,KIND_Cdp
!!#### EXTERNAL PARAMETERS
USE PAR_Units,ONLY: window_unit                  !!((02-A-PAR_Units.f90))

!!#### EXTERNAL PROCEDURES
USE SUB_CLEAR                                    !!((04-A-SUB_CLEAR.f90))
USE FUN_STR                                      !!((05-B-FUN_STR.f90))
USE FUN_VSTR                                     !!((05-B-FUN_VSTR.f90))

!!#### GLOBAL ASSIGNMENT DEFINITIONS
USE ASN_IntrinsicS                               !!((05-B-ASN_IntrinsicS.f90))

!!#### GLOBAL PRINTING ROUTINES
USE PRN_Array2                                   !!((08-B-PRN_Array2.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: DEFAULT_unit            = window_unit
LOGICAL,PARAMETER :: DEFAULT_IncludeIndices  = .TRUE.

!!#### PROCEDURE OVERLOADING
INTERFACE PRINT_Matrix_dmat
 MODULE PROCEDURE PRINT_Matrix_dmat_Rsp
 MODULE PROCEDURE PRINT_Matrix_dmat_Rdp
 MODULE PROCEDURE PRINT_Matrix_dmat_Csp
 MODULE PROCEDURE PRINT_Matrix_dmat_Cdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: PRINT_Matrix_dmat


CONTAINS


SUBROUTINE PRINT_Matrix_dmat_Rsp(A,unit,IncludeIndices,FMT)
!!#### PARAMETERS
INTEGER,PARAMETER :: KIND_A = KIND_Rsp
INCLUDE "09-B-PRN_Matrix_dmat_R.f90.hdr"
!!--begin--
INCLUDE "09-B-PRN_Matrix_dmat.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE PRINT_Matrix_dmat_Rdp(A,unit,IncludeIndices,FMT)
!!#### PARAMETERS
INTEGER,PARAMETER :: KIND_A = KIND_Rdp
INCLUDE "09-B-PRN_Matrix_dmat_R.f90.hdr"
!!--begin--
INCLUDE "09-B-PRN_Matrix_dmat.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE PRINT_Matrix_dmat_Csp(A,unit,IncludeIndices,FMT)
!!#### PARAMETERS
INTEGER,PARAMETER :: KIND_A = KIND_Csp
INCLUDE "09-B-PRN_Matrix_dmat_C.f90.hdr"
!!--begin--
INCLUDE "09-B-PRN_Matrix_dmat.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE PRINT_Matrix_dmat_Cdp(A,unit,IncludeIndices,FMT)
!!#### PARAMETERS
INTEGER,PARAMETER :: KIND_A = KIND_Cdp
INCLUDE "09-B-PRN_Matrix_dmat_C.f90.hdr"
!!--begin--
INCLUDE "09-B-PRN_Matrix_dmat.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
