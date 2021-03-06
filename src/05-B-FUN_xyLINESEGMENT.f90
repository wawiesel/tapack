MODULE FUN_xyLINESEGMENT
!!#### PURPOSE
!! Determine a line segment in R^2 <xyLINESEGMENT_> using:
!!  @ two points <PP>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp           !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_ComputationalGeometry,ONLY: xySHAPE_Ls,xySHAPE_P !!((02-A-PAR_ComputationalGeometry.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyDIRECTION,ONLY: xyDIRECTION_V                  !!((04-B-FUN_xyDIRECTION.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyLINESEGMENT_PP
 MODULE PROCEDURE xyLINESEGMENT_PP_Rsp
 MODULE PROCEDURE xyLINESEGMENT_PP_Rdp
END INTERFACE

INTERFACE xyLINESEGMENT_
 MODULE PROCEDURE xyLINESEGMENT__Rsp
 MODULE PROCEDURE xyLINESEGMENT__Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyLINESEGMENT_PP
PUBLIC :: xyLINESEGMENT_


CONTAINS


PURE FUNCTION xyLINESEGMENT_PP_Rsp( P_a , P_b ) RESULT( xyLs )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINESEGMENT_PP.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINESEGMENT_PP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyLINESEGMENT_PP_Rdp( P_a , P_b ) RESULT( xyLs )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINESEGMENT_PP.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINESEGMENT_PP.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyLINESEGMENT__Rsp( ARRAY ) RESULT( xyLs )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINESEGMENT_.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINESEGMENT_.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyLINESEGMENT__Rdp( ARRAY ) RESULT( xyLs )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyLINESEGMENT_.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyLINESEGMENT_.f90.bdy"
!!--end--
END FUNCTION

END MODULE
