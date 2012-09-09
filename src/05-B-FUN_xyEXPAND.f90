MODULE FUN_xyEXPAND
!!#### PURPOSE
!! Calculate the expansion of R^2 polygon into R^3 polygon or
!! dalculate the expansion of R^2 point to R^3 point.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE


!!#### PROCEDURE OVERLOADING
INTERFACE xyEXPAND_Pg
 MODULE PROCEDURE xyEXPAND_Pg_Rsp
 MODULE PROCEDURE xyEXPAND_Pg_Rdp
END INTERFACE

INTERFACE xyEXPAND_P
 MODULE PROCEDURE xyEXPAND_P_Rsp
 MODULE PROCEDURE xyEXPAND_P_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyEXPAND_Pg
PUBLIC :: xyEXPAND_P

CONTAINS

PURE FUNCTION xyEXPAND_Pg_Rsp( N , Pg , M32_transform ) RESULT(xyzPg)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyEXPAND_Pg.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyEXPAND_Pg.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyEXPAND_Pg_Rdp( N , Pg , M32_transform ) RESULT(xyzPg)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyEXPAND_Pg.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyEXPAND_Pg.f90.bdy"
!!--end--
END FUNCTION



PURE FUNCTION xyEXPAND_P_Rsp( P , M32_transform ) RESULT(xyzP)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyEXPAND_P.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyEXPAND_P.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyEXPAND_P_Rdp( P , M32_transform ) RESULT(xyzP)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyEXPAND_P.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyEXPAND_P.f90.bdy"
!!--end--
END FUNCTION

END MODULE
