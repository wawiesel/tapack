MODULE FUN_xyzVECTOR
!!#### PURPOSE
!! Determine a vector <V>, from:
!!  @ two points <PP> or
!!  @ two points in a list <P2>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

INTERFACE xyzVECTOR_PP
 MODULE PROCEDURE xyzVECTOR_PP_Rsp
 MODULE PROCEDURE xyzVECTOR_PP_Rdp
END INTERFACE

INTERFACE xyzVECTOR_P2
 MODULE PROCEDURE xyzVECTOR_P2_Rsp
 MODULE PROCEDURE xyzVECTOR_P2_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzVECTOR_P2
PUBLIC :: xyzVECTOR_PP


CONTAINS


PURE FUNCTION xyzVECTOR_PP_Rsp( P_a , P_b ) RESULT( xyzV )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyzVECTOR_PP.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyzVECTOR_PP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzVECTOR_PP_Rdp( P_a , P_b ) RESULT( xyzV )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyzVECTOR_PP.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyzVECTOR_PP.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyzVECTOR_P2_Rsp( P2 ) RESULT( xyzV )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyzVECTOR_P2.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyzVECTOR_P2.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzVECTOR_P2_Rdp( P2 ) RESULT( xyzV )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xyzVECTOR_P2.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xyzVECTOR_P2.f90.bdy"
!!--end--
END FUNCTION

END MODULE
