MODULE FUN_xyzCOLLAPSE
!!#### PURPOSE
!! Calculate the collapse of something in R^3 to R^2:
!!  @ polygon <Pg>
!!  @ point <P>

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzCOLLAPSE_P
 MODULE PROCEDURE xyzCOLLAPSE_P_Rsp
 MODULE PROCEDURE xyzCOLLAPSE_P_Rdp
END INTERFACE

INTERFACE xyzCOLLAPSE_Pg
 MODULE PROCEDURE xyzCOLLAPSE_Pg_Rsp
 MODULE PROCEDURE xyzCOLLAPSE_Pg_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzCOLLAPSE_P
PUBLIC :: xyzCOLLAPSE_Pg


CONTAINS


PURE FUNCTION xyzCOLLAPSE_P_Rsp(  P , M32_transform ) RESULT(xyP)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzCOLLAPSE_P.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzCOLLAPSE_P.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzCOLLAPSE_P_Rdp(  P , M32_transform ) RESULT(xyP)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzCOLLAPSE_P.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzCOLLAPSE_P.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyzCOLLAPSE_Pg_Rsp(  N , Pg , M32_transform ) RESULT(xyPg)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzCOLLAPSE_Pg.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzCOLLAPSE_Pg.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzCOLLAPSE_Pg_Rdp(  N , Pg , M32_transform ) RESULT(xyPg)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyzCOLLAPSE_Pg.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyzCOLLAPSE_Pg.f90.bdy"
!!--end--
END FUNCTION


END MODULE

