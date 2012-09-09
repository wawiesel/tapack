!!## MODULE: FUNCTION  xyORDER
MODULE FUN_xyORDER
!!### PURPOSE
!! Order the points in various shapes in a consistent
!! way.
!! @ polygon verts are ordered with the lower
!!   left corner first (it is assumed that the polygon is
!!   already in counterclockwise ordering)

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyORDER_Pg
 MODULE PROCEDURE xyORDER_Pg_Rsp
 MODULE PROCEDURE xyORDER_Pg_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyORDER_Pg

CONTAINS


PURE FUNCTION xyORDER_Pg_Rsp( N , Pg ) RESULT( order_Pg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_xyORDER_Pg.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyORDER_Pg.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyORDER_Pg_Rdp( N , Pg ) RESULT( order_Pg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_xyORDER_Pg.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyORDER_Pg.f90.bdy"
!!--end--
END FUNCTION


END MODULE
