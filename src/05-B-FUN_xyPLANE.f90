!!## MODULE: FUNCTION  xyPLANE
MODULE FUN_xyPLANE
!!### PURPOSE
!! Create a plane with three different interfaces.


!!### USAGE
!! Three different versions are provided:
!
!       Pn = xyPLANE_C( C )
!
!!  where <C> is a rank 1 array of coefficients, <C = [a,b,c,d]> which define
!!  the plane by equation $ ax + by + c = 0 $.
!
!       Pn = xyPLANE_P2( P2 )
!
!!  where <P2> is a rank 2 array of 2 points <P2(1,:) = P_a>, <P2(2,:) = P_b>.
!
!       Pn = xyPLANE_PV( P , V )
!
!
!!  where <P> is a point and <V> is a vector in the direction of the
!!  plane normal.


!!### METHOD
!! Uses the Hessian normal form for a plane.


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_0_Rsp => c_0     !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_0_Rdp => c_0     !!((02-A-PAR_Constants_Rdp.f90))


!!### EXTERNAL PROCEDURES
USE FUN_xyCROSS  ,ONLY: xyCROSS_VV             !!((03-A-FUN_xyCROSS.f90))
USE FUN_xyPERPCCW,ONLY: xyPERPCCW_V            !!((03-A-FUN_xyPERPCCW.f90))
USE FUN_xyDOT    ,ONLY: xyDOT_VV               !!((03-A-FUN_xyDOT.f90))
USE FUN_xyNORM   ,ONLY: xyNORM_V               !!((03-A-FUN_xyNORM.f90))
USE FUN_xyVECTOR ,ONLY: xyVECTOR_PP            !!((03-A-FUN_xyVECTOR.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE


!!### PROCEDURE OVERLOADING
INTERFACE xyPLANE_P2
 MODULE PROCEDURE xyPLANE_P2_Rsp
 MODULE PROCEDURE xyPLANE_P2_Rdp
END INTERFACE

INTERFACE xyPLANE_PV
 MODULE PROCEDURE xyPLANE_PV_Rsp
 MODULE PROCEDURE xyPLANE_PV_Rdp
END INTERFACE

INTERFACE xyPLANE_C
 MODULE PROCEDURE xyPLANE_PAR_Rsp
 MODULE PROCEDURE xyPLANE_PAR_Rdp
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: xyPLANE_P2
PUBLIC :: xyPLANE_PV
PUBLIC :: xyPLANE_C


!!## MODULE PROcEDURES
CONTAINS


PURE FUNCTION xyPLANE_PV_Rsp( P , V ) RESULT(Pn)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "05-B-FUN_xyPLANE_PV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPLANE_PV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyPLANE_PV_Rdp( P , V ) RESULT(Pn)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "05-B-FUN_xyPLANE_PV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPLANE_PV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyPLANE_PAR_Rsp( C ) RESULT(Pn)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyPLANE_C.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPLANE_C.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyPLANE_PAR_Rdp( C ) RESULT(Pn)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyPLANE_C.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPLANE_C.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyPLANE_P2_Rsp( P2 ) RESULT(Pn)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyPLANE_P2.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPLANE_P2.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyPLANE_P2_Rdp( P2 ) RESULT(Pn)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyPLANE_P2.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyPLANE_P2.f90.bdy"
!!--end--
END FUNCTION





END MODULE
