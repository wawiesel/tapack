MODULE FUN_xyzNORMSQRD
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
INTERFACE xyzNORMSQRD_V
 MODULE PROCEDURE xyzNORMSQRD_V_Rsp
 MODULE PROCEDURE xyzNORMSQRD_V_Rdp
ENDINTERFACE
PUBLIC :: xyzNORMSQRD_V

CONTAINS

PURE FUNCTION xyzNORMSQRD_V_Rsp( V ) RESULT(NORMSQRD)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyzNORMSQRD_V.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyzNORMSQRD_V.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzNORMSQRD_V_Rdp( V ) RESULT(NORMSQRD)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyzNORMSQRD_V.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyzNORMSQRD_V.f90.bdy"
!!--end--
END FUNCTION

END MODULE
