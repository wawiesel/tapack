!!# FUNCTION MODULE: <FUN_xyPOLYGON>
MODULE FUN_xyPOLYGON

!!## PURPOSE
!! Determine a polygon as a (properly) ordered list of points,
!! from the minimum x/y valued point, counter-clockwise.

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL PARAMETERS
USE PAR_ComputationalGeometry                  !!((02-A-PAR_ComputationalGeometry.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Error                                  !!((04-A-FUN_Error.f90))
USE FUN_xyCONTIGUOUS                           !!((06-B-FUN_xyCONTIGUOUS.f90))
USE FUN_xyCONVEXHULL                           !!((07-B-FUN_xyCONVEXHULL.f90))
USE FUN_xyORDER                                !!((04-B-FUN_xyORDER.f90))
USE FUN_Reorder                                !!((05-A-FUN_Reorder.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE xyPOLYGON_Px
 MODULE PROCEDURE xyPOLYGON_Px_Rsp
 MODULE PROCEDURE xyPOLYGON_Px_Rdp
 MODULE PROCEDURE xyPOLYGON_Px_order_Rsp
 MODULE PROCEDURE xyPOLYGON_Px_order_Rdp
END INTERFACE

INTERFACE xyPOLYGON_Lsx
 MODULE PROCEDURE xyPOLYGON_Lsx_Rsp
 MODULE PROCEDURE xyPOLYGON_Lsx_Rdp
END INTERFACE

INTERFACE xyPOLYGON_
 MODULE PROCEDURE xyPOLYGON__Rsp
 MODULE PROCEDURE xyPOLYGON__Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyPOLYGON_Px
PUBLIC :: xyPOLYGON_Lsx
PUBLIC :: xyPOLYGON_

!!## MODULE PROCEDURES
CONTAINS


PURE FUNCTION xyPOLYGON_Px_Rsp( N , Px ) RESULT( xyPg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-A-FUN_xyPOLYGON_Px.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyPOLYGON_Px.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyPOLYGON_Px_Rdp( N , Px ) RESULT( xyPg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-A-FUN_xyPOLYGON_Px.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyPOLYGON_Px.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyPOLYGON_Px_order_Rsp( N , Px , order ) RESULT( xyPg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-A-FUN_xyPOLYGON_Px_order.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyPOLYGON_Px_order.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyPOLYGON_Px_order_Rdp( N , Px , order ) RESULT( xyPg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-A-FUN_xyPOLYGON_Px_order.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyPOLYGON_Px_order.f90.bdy"
!!--end--
END FUNCTION



PURE FUNCTION xyPOLYGON_Lsx_Rsp( N , Lsx ) RESULT( xyPg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-A-FUN_xyPOLYGON_Lsx.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyPOLYGON_Lsx.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyPOLYGON_Lsx_Rdp( N , Lsx ) RESULT( xyPg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-A-FUN_xyPOLYGON_Lsx.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyPOLYGON_Lsx.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyPOLYGON__Rsp( ARRAY ) RESULT( xyPg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-A-FUN_xyPOLYGON_.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyPOLYGON_.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyPOLYGON__Rdp( ARRAY ) RESULT( xyPg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-A-FUN_xyPOLYGON_.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyPOLYGON_.f90.bdy"
!!--end--
END FUNCTION

END MODULE
