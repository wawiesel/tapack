MODULE FUN_rpDOT
!!#### PURPOSE
!! Define DOT product for R^2 polar geometry,
!
!!         s = rpDOT_VV( V_a , V_b )
!
!! where V_a, V_b are vectors and s is a scalar.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### rpDOT PROCEDURE OVERLOADING
INTERFACE rpDOT
 MODULE PROCEDURE rpDOT_VV_Rsp
 MODULE PROCEDURE rpDOT_VV_Rdp
END INTERFACE

!!#### ACCESS
PUBLIC :: rpDOT


CONTAINS

PURE FUNCTION rpDOT_VV_Rsp( V_a , V_b ) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_rpDOT_VV.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_rpDOT_VV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION rpDOT_VV_Rdp( V_a , V_b ) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_rpDOT_VV.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_rpDOT_VV.f90.bdy"
!!--end--
END FUNCTION

END MODULE
