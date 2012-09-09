MODULE FUN_xyLINE
!!#### PURPOSE
!! Determine a line in R^2 <xyLINE_> using:
!!  @ two points <PP>,
!!  @ point and a vector <PV>, or a
!!  @ point and a direction <PU>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyDIRECTION,ONLY: xyDIRECTION_V        !!((04-B-FUN_xyDIRECTION.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyLINE_PP
 MODULE PROCEDURE xyLINE_PP_Rsp
 MODULE PROCEDURE xyLINE_PP_Rdp
END INTERFACE

INTERFACE xyLINE_PV
 MODULE PROCEDURE xyLINE_PV_Rsp
 MODULE PROCEDURE xyLINE_PV_Rdp
END INTERFACE

INTERFACE xyLINE_PU
 MODULE PROCEDURE xyLINE_PU_Rsp
 MODULE PROCEDURE xyLINE_PU_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyLINE_PP
PUBLIC :: xyLINE_PV
PUBLIC :: xyLINE_PU


CONTAINS

PURE FUNCTION xyLINE_PP_Rsp( P_a , P_b ) RESULT( xyLn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINE_PP.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINE_PP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyLINE_PP_Rdp( P_a , P_b ) RESULT( xyLn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINE_PP.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINE_PP.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyLINE_PV_Rsp( P , V ) RESULT( xyLn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINE_PV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINE_PV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyLINE_PV_Rdp( P , V ) RESULT( xyLn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINE_PV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINE_PV.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyLINE_PU_Rsp( P , U ) RESULT( xyLn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINE_PU.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINE_PU.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyLINE_PU_Rdp( P , U ) RESULT( xyLn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINE_PU.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINE_PU.f90.bdy"
!!--end--
END FUNCTION


END MODULE
