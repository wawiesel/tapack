!!# FUNCTION MODULE: <FUN_xyPOINT>
MODULE FUN_xyPOINT

!!## PURPOSE
!! Determine a point from:
!!  @ a plane.
!!  @ a line segment and a parametric location.

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_xyDIST                                 !!((04-B-FUN_xyDIST.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE xyPOINT_Pn
 MODULE PROCEDURE xyPOINT_Pn_Rsp
 MODULE PROCEDURE xyPOINT_Pn_Rdp
END INTERFACE
INTERFACE xyPOINT_LsL
 MODULE PROCEDURE xyPOINT_LsL_Rsp
 MODULE PROCEDURE xyPOINT_LsL_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyPOINT_Pn
PUBLIC :: xyPOINT_LsL


CONTAINS


PURE FUNCTION xyPOINT_Pn_Rsp( Pn ) RESULT( xyP )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyPOINT_Pn.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPOINT_Pn.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyPOINT_Pn_Rdp( Pn ) RESULT( xyP )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyPOINT_Pn.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPOINT_Pn.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyPOINT_LsL_Rsp( Ls,L ) RESULT( xyP )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp                          !!((02-A-PAR_Constants_Rsp.f90))
INCLUDE "05-B-FUN_xyPOINT_LsL.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPOINT_LsL.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyPOINT_LsL_Rdp( Ls,L ) RESULT( xyP )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp                          !!((02-A-PAR_Constants_Rdp.f90))
INCLUDE "05-B-FUN_xyPOINT_LsL.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPOINT_LsL.f90.bdy"
!!--end--
END FUNCTION



END MODULE
