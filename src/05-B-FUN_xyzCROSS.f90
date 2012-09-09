MODULE FUN_xyzCROSS
!!#### PURPOSE
!! Cross product for R^3 Cartesian geometry,
!
!!         C = xyzCROSS_VV( A , B )
!
!! where A, B, and C are vectors.

!!#### DETAILS
!! The cross product is only defined for two vectors A,B of the same
!! type-kinds.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzCROSS_UU
 MODULE PROCEDURE xyzCROSS_UU_Rsp
 MODULE PROCEDURE xyzCROSS_UU_Rdp
END INTERFACE

INTERFACE xyzCROSS_VV
 MODULE PROCEDURE xyzCROSS_VV_Rsp
 MODULE PROCEDURE xyzCROSS_VV_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzCROSS_UU
PUBLIC :: xyzCROSS_VV


CONTAINS


PURE FUNCTION xyzCROSS_VV_Rsp( V_a , V_b ) RESULT(xyzV)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzCROSS_VV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzCROSS_VV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzCROSS_VV_Rdp( V_a , V_b ) RESULT(xyzV)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzCROSS_VV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzCROSS_VV.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyzCROSS_UU_Rsp( U_a , U_b ) RESULT(xyzU)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzCROSS_UU.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzCROSS_UU.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzCROSS_UU_Rdp( U_a , U_b ) RESULT(xyzU)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzCROSS_UU.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzCROSS_UU.f90.bdy"
!!--end--
END FUNCTION


END MODULE
