MODULE FUN_xyCROSS
!!#### PURPOSE
!! Cross product for R^2 Cartesian geometry,
!
!!         V_c = xyCROSS_VV( V_a , V_b )
!
!! where V_a, V_b, and V_c are vectors.

!!#### DETAILS
!! The cross product is only defined for two vectors V_a,V_b of the same
!! type-kinds.

!!#### EXTERNAL PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyCROSS_VV
 MODULE PROCEDURE xyCROSS_VV_Rsp
 MODULE PROCEDURE xyCROSS_VV_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyCROSS_VV


CONTAINS


PURE FUNCTION xyCROSS_VV_Rsp( V_a , V_b ) RESULT(V_c)
!!#### LOCAL PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyCROSS_VV.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyCROSS_VV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyCROSS_VV_Rdp( V_a , V_b ) RESULT(V_c)
!!#### LOCAL PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyCROSS_VV.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyCROSS_VV.f90.bdy"
!!--end--
END FUNCTION


END MODULE
