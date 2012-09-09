MODULE FUN_xyzDOT
!!#### PURPOSE
!! Define DOT product for R^2 Cartesian geometry,
!
!!         s = xyzDOT_VV( A , B )
!
!! where A, B are vectors and s is a scalar.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzDOT_VV
 MODULE PROCEDURE xyzDOT_VV_Rsp
 MODULE PROCEDURE xyzDOT_VV_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzDOT_VV


CONTAINS


PURE FUNCTION xyzDOT_VV_Rsp( V_a , V_b ) RESULT(s)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzDOT_VV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzDOT_VV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzDOT_VV_Rdp( V_a , V_b ) RESULT(s)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzDOT_VV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzDOT_VV.f90.bdy"
!!--end--
END FUNCTION


END MODULE
