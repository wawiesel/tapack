MODULE FUN_xyCENTROID
!!#### PURPOSE
!! Calculate the centroid (center-of-mass) of various shapes:
!!  1. polygon <Pg>

!!#### EXTERNAL PARAMETERS
USE PAR_ComputationalGeometry,ONLY: xySHAPE_Ls,xySHAPE_Pg,xySHAPE_P !!((02-A-PAR_ComputationalGeometry.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyCENTROID_Pg
 MODULE PROCEDURE xyCENTROID_Pg_Rsp
 MODULE PROCEDURE xyCENTROID_Pg_Rdp
END INTERFACE

INTERFACE xyCENTROID_Ls
 MODULE PROCEDURE xyCENTROID_Ls_Rsp
 MODULE PROCEDURE xyCENTROID_Ls_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyCENTROID_Pg
PUBLIC :: xyCENTROID_Ls

CONTAINS


PURE FUNCTION xyCENTROID_Ls_Rsp( Ls ) RESULT(xyP)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                       !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp,ONLY: c_1_by_2                                !!((02-A-PAR_Constants_Rsp.f90))
INCLUDE "05-B-FUN_xyCENTROID_Ls.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyCENTROID_Ls.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyCENTROID_Ls_Rdp( Ls ) RESULT(xyP)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                       !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp,ONLY: c_1_by_2                                !!((02-A-PAR_Constants_Rdp.f90))
INCLUDE "05-B-FUN_xyCENTROID_Ls.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyCENTROID_Ls.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyCENTROID_Pg_Rsp( N , Pg , AREA ) RESULT(xyP)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                       !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp,ONLY: c_1_by_6                                !!((02-A-PAR_Constants_Rsp.f90))
INCLUDE "05-B-FUN_xyCENTROID_Pg.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyCENTROID_Pg.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyCENTROID_Pg_Rdp( N , Pg , AREA ) RESULT(xyP)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                       !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp,ONLY: c_1_by_6                                !!((02-A-PAR_Constants_Rdp.f90))
INCLUDE "05-B-FUN_xyCENTROID_Pg.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyCENTROID_Pg.f90.bdy"
!!--end--
END FUNCTION


END MODULE
