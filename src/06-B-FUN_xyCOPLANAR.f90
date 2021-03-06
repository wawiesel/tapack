MODULE FUN_xyCOPLANAR
!!#### PURPOSE
!! Test for coplanarity between:
!! @ three points in R^2 <P3>
!! @ a plane and a point in R^2 <PnP>

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_0_Rsp => c_0     !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_0_Rdp => c_0     !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyPLANE,ONLY: xyPLANE_P2               !!((05-B-FUN_xyPLANE.f90))
USE FUN_xySDIST,ONLY: xySDIST_PnP              !!((03-A-FUN_xySDIST.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyCOPLANAR_P3
 MODULE PROCEDURE xyCOPLANAR_P3_Rsp
 MODULE PROCEDURE xyCOPLANAR_P3_Rdp
END INTERFACE

INTERFACE xyCOPLANAR_PnP
 MODULE PROCEDURE xyCOPLANAR_PnP_Rsp
 MODULE PROCEDURE xyCOPLANAR_PnP_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyCOPLANAR_P3
PUBLIC :: xyCOPLANAR_PnP


CONTAINS


PURE FUNCTION xyCOPLANAR_P3_Rsp( P3 ) RESULT(COPLANAR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_xyCOPLANAR_P3.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyCOPLANAR_P3.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyCOPLANAR_P3_Rdp( P3 ) RESULT(COPLANAR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_xyCOPLANAR_P3.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyCOPLANAR_P3.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyCOPLANAR_PnP_Rsp( Pn , P ) RESULT(COPLANAR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "06-B-FUN_xyCOPLANAR_PnP.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyCOPLANAR_PnP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyCOPLANAR_PnP_Rdp( Pn , P ) RESULT(COPLANAR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "06-B-FUN_xyCOPLANAR_PnP.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyCOPLANAR_PnP.f90.bdy"
!!--end--
END FUNCTION


END MODULE
