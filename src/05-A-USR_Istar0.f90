!!### MODULE: USER routines for USR_Istar0
MODULE USR_Istar0
!!#### PURPOSE
!! The scalar variable-kind (*) logical.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I,KIND_I1,KIND_I2,KIND_I4,KIND_I8 !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_Error                                                       !!((04-A-FUN_Error.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### TYPE DEFINITION: TYPE_Istar0
TYPE TYPE_Istar0
 INTEGER          :: KIND = -1
 INTEGER(KIND_I1) :: I1   = Error_I1
 INTEGER(KIND_I2) :: I2   = Error_I2
 INTEGER(KIND_I4) :: I4   = Error_I4
 INTEGER(KIND_I8) :: I8   = Error_I8
END TYPE

!!#### ASSIGNMENT OPERATOR
INTERFACE ASSIGNMENT(=)
 MODULE PROCEDURE ASSIGN_I1_Istar0
 MODULE PROCEDURE ASSIGN_I2_Istar0
 MODULE PROCEDURE ASSIGN_I4_Istar0
 MODULE PROCEDURE ASSIGN_I8_Istar0
 MODULE PROCEDURE ASSIGN_Istar0_I1
 MODULE PROCEDURE ASSIGN_Istar0_I2
 MODULE PROCEDURE ASSIGN_Istar0_I4
 MODULE PROCEDURE ASSIGN_Istar0_I8
END INTERFACE

INTERFACE IsOk
 MODULE PROCEDURE IsOk_Istar0
END INTERFACE

!!#### PUBLIC ACCESS
PUBLIC :: TYPE_Istar0
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: IsOk

CONTAINS


PURE ELEMENTAL FUNCTION IsOk_Istar0( Istar0 ) RESULT(IsOk)
!!#### REQUIRED INPUT
TYPE(TYPE_Istar0),INTENT(IN) :: Istar0
!!#### REQUIRED OUTPUT
LOGICAL :: IsOk
!!--begin--
IsOk = Istar0 % KIND >= 0
!!--end--
END FUNCTION


PURE ELEMENTAL SUBROUTINE ASSIGN_I1_Istar0(I,Istar0)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                        !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-USR_Istar0__ASSIGN_I_Istar0.f90.hdr"
!!--begin--
INCLUDE "05-A-USR_Istar0__ASSIGN_I_Istar0.f90.bdy"
!!--end--
END SUBROUTINE

PURE ELEMENTAL SUBROUTINE ASSIGN_I2_Istar0(I,Istar0)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                        !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-USR_Istar0__ASSIGN_I_Istar0.f90.hdr"
!!--begin--
INCLUDE "05-A-USR_Istar0__ASSIGN_I_Istar0.f90.bdy"
!!--end--
END SUBROUTINE

PURE ELEMENTAL SUBROUTINE ASSIGN_I4_Istar0(I,Istar0)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                        !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-USR_Istar0__ASSIGN_I_Istar0.f90.hdr"
!!--begin--
INCLUDE "05-A-USR_Istar0__ASSIGN_I_Istar0.f90.bdy"
!!--end--
END SUBROUTINE

PURE ELEMENTAL SUBROUTINE ASSIGN_I8_Istar0(I,Istar0)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                        !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-USR_Istar0__ASSIGN_I_Istar0.f90.hdr"
!!--begin--
INCLUDE "05-A-USR_Istar0__ASSIGN_I_Istar0.f90.bdy"
!!--end--
END SUBROUTINE



PURE ELEMENTAL SUBROUTINE ASSIGN_Istar0_I1(Istar0,I)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                        !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-USR_Istar0__ASSIGN_Istar0_I.f90.hdr"
!!--begin--
INCLUDE "05-A-USR_Istar0__ASSIGN_Istar0_I.f90.bdy"
!!--end--
END SUBROUTINE

PURE ELEMENTAL SUBROUTINE ASSIGN_Istar0_I2(Istar0,I)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                        !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-USR_Istar0__ASSIGN_Istar0_I.f90.hdr"
!!--begin--
INCLUDE "05-A-USR_Istar0__ASSIGN_Istar0_I.f90.bdy"
!!--end--
END SUBROUTINE

PURE ELEMENTAL SUBROUTINE ASSIGN_Istar0_I4(Istar0,I)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                        !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-USR_Istar0__ASSIGN_Istar0_I.f90.hdr"
!!--begin--
INCLUDE "05-A-USR_Istar0__ASSIGN_Istar0_I.f90.bdy"
!!--end--
END SUBROUTINE

PURE ELEMENTAL SUBROUTINE ASSIGN_Istar0_I8(Istar0,I)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                        !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-USR_Istar0__ASSIGN_Istar0_I.f90.hdr"
!!--begin--
INCLUDE "05-A-USR_Istar0__ASSIGN_Istar0_I.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
