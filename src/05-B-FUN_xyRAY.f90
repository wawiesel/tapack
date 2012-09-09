MODULE FUN_xyRAY
!!#### PURPOSE
!! Determine a ray using:
!!   @ two points <PP>,
!!   @ a point and a vector <PV>, or
!!   @ a point and a unit vector <PU>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp         !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyDIRECTION,ONLY: xyDIRECTION_V,xyDIRECTION_PP !!((04-B-FUN_xyDIRECTION.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyRAY_PP
 MODULE PROCEDURE xyRAY_PP_Rsp
 MODULE PROCEDURE xyRAY_PP_Rdp
END INTERFACE

INTERFACE xyRAY_PV
 MODULE PROCEDURE xyRAY_PV_Rsp
 MODULE PROCEDURE xyRAY_PV_Rdp
END INTERFACE

INTERFACE xyRAY_PU
 MODULE PROCEDURE xyRAY_PU_Rsp
 MODULE PROCEDURE xyRAY_PU_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyRAY_PP
PUBLIC :: xyRAY_PV
PUBLIC :: xyRAY_PU


CONTAINS


PURE FUNCTION xyRAY_PP_Rsp( P_a , P_b ) RESULT( xyRy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyRAY_PP.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyRAY_PP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyRAY_PP_Rdp( P_a , P_b ) RESULT( xyRy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyRAY_PP.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyRAY_PP.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyRAY_PV_Rsp( P , V ) RESULT( xyRy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyRAY_PV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyRAY_PV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyRAY_PV_Rdp( P , V ) RESULT( xyRy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyRAY_PV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyRAY_PV.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyRAY_PU_Rsp( P , U ) RESULT( xyRy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyRAY_PU.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyRAY_PU.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyRAY_PU_Rdp( P , U ) RESULT( xyRy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyRAY_PU.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyRAY_PU.f90.bdy"
!!--end--
END FUNCTION


END MODULE
