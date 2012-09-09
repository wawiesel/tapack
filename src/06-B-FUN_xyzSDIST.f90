MODULE FUN_xyzSDIST
!!#### PURPOSE
!! Compute the signed distance from a plane to a point.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyzDOT,ONLY: xyzDOT_VV                 !!((05-B-FUN_xyzDOT.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzSDIST_PnP
 MODULE PROCEDURE xyzSDIST_PnP_Rsp
 MODULE PROCEDURE xyzSDIST_PnP_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzSDIST_PnP


CONTAINS


PURE FUNCTION xyzSDIST_PnP_Rsp( Pn , P ) RESULT( SDIST )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_xyzSDIST_PnP.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzSDIST_PnP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzSDIST_PnP_Rdp( Pn , P ) RESULT( SDIST )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_xyzSDIST_PnP.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzSDIST_PnP.f90.bdy"
!!--end--
END FUNCTION


END MODULE
