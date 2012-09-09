MODULE FUN_xyROTATE
!!#### PURPOSE
!! Rotate a point in polar coordinates about the origin in R^2.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_0_Rsp => c_0 , c_1_Rsp => c_1 !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_0_Rdp => c_0 , c_1_Rdp => c_1 !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyNORM                                              !!((03-A-FUN_xyNORM.f90))
USE FUN_xyDOT                                               !!((03-A-FUN_xyDOT.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyROTATE_V
 MODULE PROCEDURE xyROTATE_V_Rsp
 MODULE PROCEDURE xyROTATE_V_Rdp
END INTERFACE

INTERFACE xyROTATE_U
 MODULE PROCEDURE xyROTATE_U_Rsp
 MODULE PROCEDURE xyROTATE_U_Rdp
END INTERFACE

INTERFACE xyROTATE_Px
 MODULE PROCEDURE xyROTATE_Px_Rsp
 MODULE PROCEDURE xyROTATE_Px_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyROTATE_V
PUBLIC :: xyROTATE_U
PUBLIC :: xyROTATE_Px


CONTAINS


PURE FUNCTION xyROTATE_V_Rsp( V , dt ) RESULT( xyV )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp               !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rsp
INCLUDE "05-B-FUN_xyROTATE_V.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyROTATE_V.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyROTATE_V_Rdp( V , dt ) RESULT( xyV )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp               !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rdp
INCLUDE "05-B-FUN_xyROTATE_V.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyROTATE_V.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyROTATE_U_Rsp( U , dt ) RESULT( xyU )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp               !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rsp
INCLUDE "05-B-FUN_xyROTATE_U.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyROTATE_U.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyROTATE_U_Rdp( U , dt ) RESULT( xyU )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp               !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rdp
INCLUDE "05-B-FUN_xyROTATE_U.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyROTATE_U.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyROTATE_Px_Rsp( N , Px , dt ) RESULT( xyPx )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp               !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rsp
INCLUDE "05-B-FUN_xyROTATE_Px.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyROTATE_Px.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyROTATE_Px_Rdp( N , Px , dt ) RESULT( xyPx )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp               !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rdp
INCLUDE "05-B-FUN_xyROTATE_Px.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyROTATE_Px.f90.bdy"
!!--end--
END FUNCTION


END MODULE
