MODULE FUN_xyzNORM
!!#### PURPOSE
!! Compute the normalization factor squared for a vector in R^3.

!!#### METHOD
!! Square root of the sum of the squares of x, y, and z components.

!!#### DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzNORM_V
 MODULE PROCEDURE xyzNORM_V_Rsp
 MODULE PROCEDURE xyzNORM_V_Rdp
ENDINTERFACE
PUBLIC :: xyzNORM_V

CONTAINS

PURE FUNCTION xyzNORM_V_Rsp( V ) RESULT(NORM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyzNORM_V.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyzNORM_V.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzNORM_V_Rdp( V ) RESULT(NORM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyzNORM_V.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyzNORM_V.f90.bdy"
!!--end--
END FUNCTION

END MODULE
