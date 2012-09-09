MODULE FUN_xyUNITVECTOR
!!#### PURPOSE
!! Determine a unit vector <U>, from:
!!  @ a vector <V>,
!!  @ two points <PP>, or
!!  @ two points in a list <P2>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_0_Rsp => c_0     !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_0_Rdp => c_0     !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyNORM,ONLY: xyNORM_V                  !!((03-A-FUN_xyNORM.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyUNITVECTOR_V
 MODULE PROCEDURE xyUNITVECTOR_V_Rsp
 MODULE PROCEDURE xyUNITVECTOR_V_Rdp
END INTERFACE

INTERFACE xyUNITVECTOR_PP
 MODULE PROCEDURE xyUNITVECTOR_PP_Rsp
 MODULE PROCEDURE xyUNITVECTOR_PP_Rdp
END INTERFACE

INTERFACE xyUNITVECTOR_P2
 MODULE PROCEDURE xyUNITVECTOR_P2_Rsp
 MODULE PROCEDURE xyUNITVECTOR_P2_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyUNITVECTOR_P2
PUBLIC :: xyUNITVECTOR_PP
PUBLIC :: xyUNITVECTOR_V

CONTAINS


PURE FUNCTION xyUNITVECTOR_V_Rsp( V ) RESULT( xyU )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "04-A-FUN_xyUNITVECTOR_V.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_xyUNITVECTOR_V.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyUNITVECTOR_V_Rdp( V ) RESULT( xyU )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "04-A-FUN_xyUNITVECTOR_V.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_xyUNITVECTOR_V.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyUNITVECTOR_PP_Rsp( P_a , P_b ) RESULT( xyU )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_xyUNITVECTOR_PP.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_xyUNITVECTOR_PP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyUNITVECTOR_PP_Rdp( P_a , P_b ) RESULT( xyU )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_xyUNITVECTOR_PP.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_xyUNITVECTOR_PP.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyUNITVECTOR_P2_Rsp( P2 ) RESULT( xyU )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_xyUNITVECTOR_P2.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_xyUNITVECTOR_P2.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyUNITVECTOR_P2_Rdp( P2 ) RESULT( xyU )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_xyUNITVECTOR_P2.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_xyUNITVECTOR_P2.f90.bdy"
!!--end--
END FUNCTION

END MODULE
