MODULE FUN_xyDOT
!!#### PURPOSE
!! Define DOT product for R^2 Cartesian geometry.

!!#### USAGE
!!         s = xyDOT_VV( V_a , V_b )
!
!! where V_a, V_b are vectors.

!!#### AUTHOR
!! William Wieselquist | william.wieselquist@gmail.com

!!#### DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyDOT_VV
 MODULE PROCEDURE xyDOT_VV_Rsp
 MODULE PROCEDURE xyDOT_VV_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyDOT_VV


CONTAINS

PURE FUNCTION xyDOT_VV_Rsp( V_a , V_b ) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyDOT_VV.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyDOT_VV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyDOT_VV_Rdp( V_a , V_b ) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyDOT_VV.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyDOT_VV.f90.bdy"
!!--end--
END FUNCTION

END MODULE
