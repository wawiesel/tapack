MODULE FUN_xyzINTERSECT
!!#### PURPOSE
!! Determine if two objects intersect and optionally return some information
!! about the intersection.
!
!! Routine Suffix   Object 1    Object 2   Optional Information
!!     _PnRy         Plane       Ray       distance to intersection, point of intersection
!!     _PnLn         Plane       Line      signed distance to plane, point of intersection
!!     _SpRy         Spere       Ray       distance to intersection, point of intersection
!!     _RyRy         Ray         Ray       distance to intersection, point of intersection

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp                               !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_0_Rsp => c_0 , c_1_Rsp => c_1 , c_2_Rsp => c_2 !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_0_Rdp => c_0 , c_1_Rdp => c_1 , c_2_Rdp => c_2 !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyzVECTOR     ,ONLY: xyzVECTOR_PP                                    !!((03-A-FUN_xyzVECTOR.f90))
USE FUN_xyzDETERMINANT,ONLY: xyzDETERMINANT                                  !!((05-B-FUN_xyzDETERMINANT.f90))
USE FUN_xyzCROSS      ,ONLY: xyzCROSS_VV                                     !!((05-B-FUN_xyzCROSS.f90))
USE FUN_xyzNORMSQRD   ,ONLY: xyzNORMSQRD_V                                   !!((03-A-FUN_xyzNORMSQRD.f90))
USE FUN_xyzDOT        ,ONLY: xyzDOT_VV                                       !!((05-B-FUN_xyzDOT.f90))
USE FUN_xyzSDIST      ,ONLY: xyzSDIST_PnP                                    !!((06-B-FUN_xyzSDIST.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzINTERSECT_PnRy
 MODULE PROCEDURE xyzINTERSECT_PnRy_Rsp
 MODULE PROCEDURE xyzINTERSECT_PnRy_Rdp
END INTERFACE

INTERFACE xyzINTERSECT_PnLn
 MODULE PROCEDURE xyzINTERSECT_PnLn_Rsp
 MODULE PROCEDURE xyzINTERSECT_PnLn_Rdp
END INTERFACE

INTERFACE xyzINTERSECT_SpRy
 MODULE PROCEDURE xyzINTERSECT_SpRy_Rsp
 MODULE PROCEDURE xyzINTERSECT_SpRy_Rdp
END INTERFACE

INTERFACE xyzINTERSECT_RyRy
 MODULE PROCEDURE xyzINTERSECT_RyRy_Rsp
 MODULE PROCEDURE xyzINTERSECT_RyRy_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzINTERSECT_PnRy
PUBLIC :: xyzINTERSECT_PnLn
PUBLIC :: xyzINTERSECT_SpRy
PUBLIC :: xyzINTERSECT_RyRy


CONTAINS


FUNCTION xyzINTERSECT_PnRy_Rsp( Pn , Ry , DIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                                !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "07-B-FUN_xyzINTERSECT_PnRy.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzINTERSECT_PnRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyzINTERSECT_PnRy_Rdp( Pn , Ry , DIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                                !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "07-B-FUN_xyzINTERSECT_PnRy.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzINTERSECT_PnRy.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyzINTERSECT_RyRy_Rsp( Ry_a , Ry_b , DIST_a , DIST_b , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                                !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "07-B-FUN_xyzINTERSECT_RyRy.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzINTERSECT_RyRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyzINTERSECT_RyRy_Rdp( Ry_a , Ry_b , DIST_a , DIST_b , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                                !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "07-B-FUN_xyzINTERSECT_RyRy.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzINTERSECT_RyRy.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyzINTERSECT_PnLn_Rsp( Pn , Ln , SDIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                                !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "07-B-FUN_xyzINTERSECT_PnLn.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzINTERSECT_PnLn.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyzINTERSECT_PnLn_Rdp( Pn , Ln , SDIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                                !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "07-B-FUN_xyzINTERSECT_PnLn.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzINTERSECT_PnLn.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyzINTERSECT_SpRy_Rsp( Sp , Ry , DIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                                !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rsp
INCLUDE "07-B-FUN_xyzINTERSECT_SpRy.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzINTERSECT_SpRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyzINTERSECT_SpRy_Rdp( Sp , Ry , DIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                                !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rdp
INCLUDE "07-B-FUN_xyzINTERSECT_SpRy.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzINTERSECT_SpRy.f90.bdy"
!!--end--
END FUNCTION


END MODULE
