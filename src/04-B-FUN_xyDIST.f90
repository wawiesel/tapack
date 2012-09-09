MODULE FUN_xyDIST
!!#### PURPOSE
!! Compute the distance objects in R^2:
!!  @ two points <PP>,
!!  @ a line and a point <LnP>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyNORM,ONLY: xyNORM_V                  !!((03-A-FUN_xyNORM.f90))
USE FUN_xyDOT ,ONLY: xyDOT_VV                  !!((03-A-FUN_xyDOT.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyDIST_PP
 MODULE PROCEDURE xyDIST_PP_Rsp
 MODULE PROCEDURE xyDIST_PP_Rdp
ENDINTERFACE

INTERFACE xyDIST_LnP
 MODULE PROCEDURE xyDIST_LnP_Rsp
 MODULE PROCEDURE xyDIST_LnP_Rdp
ENDINTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyDIST_PP
PUBLIC :: xyDIST_LnP


CONTAINS


PURE FUNCTION xyDIST_PP_Rsp( P_a , P_b ) RESULT( DIST )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_xyDIST_PP.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyDIST_PP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyDIST_PP_Rdp( P_a , P_b ) RESULT( DIST )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_xyDIST_PP.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyDIST_PP.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyDIST_LnP_Rsp( Ln , P ) RESULT( DIST )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_xyDIST_LnP.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyDIST_LnP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyDIST_LnP_Rdp( Ln , P ) RESULT( DIST )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_xyDIST_LnP.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyDIST_LnP.f90.bdy"
!!--end--
END FUNCTION


END MODULE
