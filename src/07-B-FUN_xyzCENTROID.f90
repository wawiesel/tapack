MODULE FUN_xyzCENTROID
!!#### PURPOSE
!! Calculate the centroid (center-of-mass) of a polygon.

!!#### METHOD
!! First collapse R^3 variables into R^2 using transformation matrix,
!! then calculate centroid via R^2 procedure xyCENTROID_Pg.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp       !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_1_by_6_Rsp => c_1_by_6 !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_1_by_6_Rdp => c_1_by_6 !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyzSAREA   ,ONLY:                            !!((06-B-FUN_xyzSAREA.f90))
USE FUN_xyzCOLLAPSE,ONLY: xyzCOLLAPSE_Pg             !!((05-B-FUN_xyzCOLLAPSE.f90))
USE FUN_xyCENTROID ,ONLY: xyCENTROID_Pg              !!((05-B-FUN_xyCENTROID.f90))
USE FUN_xyEXPAND   ,ONLY: xyEXPAND_P                 !!((05-B-FUN_xyEXPAND.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzCENTROID_Pg
 MODULE PROCEDURE xyzCENTROID_Pg_Rsp
 MODULE PROCEDURE xyzCENTROID_Pg_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzCENTROID_Pg


CONTAINS


FUNCTION xyzCENTROID_Pg_Rsp( N , Pg , M32_transform , SAREA ) RESULT(xyzP)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp        !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_6 = c_1_by_6_Rsp
INCLUDE "07-B-FUN_xyzCENTROID_Pg.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzCENTROID_Pg.f90.bdy"
!!--end--
ENDFUNCTION

FUNCTION xyzCENTROID_Pg_Rdp( N , Pg , M32_transform , SAREA ) RESULT(xyzP)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp        !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_6 = c_1_by_6_Rdp
INCLUDE "07-B-FUN_xyzCENTROID_Pg.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyzCENTROID_Pg.f90.bdy"
!!--end--
ENDFUNCTION

ENDMODULE
