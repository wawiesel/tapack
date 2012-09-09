MODULE FUN_xyNORM
!!#### PURPOSE
!! Compute the normalization factor for a vector in R^2.

!!#### METHOD
!! Square root of the sum of the squares of x and y components.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyNORM_V
 MODULE PROCEDURE xyNORM_V_Rsp
 MODULE PROCEDURE xyNORM_V_Rdp
ENDINTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyNORM_V

CONTAINS

PURE FUNCTION xyNORM_V_Rsp( V ) RESULT(NORM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyNORM_V.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyNORM_V.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyNORM_V_Rdp( V ) RESULT(NORM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyNORM_V.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyNORM_V.f90.bdy"
!!--end--
END FUNCTION

END MODULE
