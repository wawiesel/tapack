MODULE FUN_xySDIST
!!#### PURPOSE
!! Compute the signed distance from <shape_a> to <shape_b>.
!!    shape_a       shape_b
!!  -----------   -----------
!!   plane <Pn>    point <P>


!!#### USAGE
!!           d = xySDIST_PnP( Pn , P )
!
!! where Pn is a plane and P is a point.


!!#### AUTHORS|CONTACT INFO
!! W. A. Wieselquist [waw] | william.wieselquist@gmail.com


!!#### DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xySDIST_PnP
 MODULE PROCEDURE xySDIST_PnP_Rsp
 MODULE PROCEDURE xySDIST_PnP_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xySDIST_PnP


CONTAINS


PURE FUNCTION xySDIST_PnP_Rsp( Pn , P ) RESULT( SDIST )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xySDIST_PnP.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xySDIST_PnP.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xySDIST_PnP_Rdp( Pn , P ) RESULT( SDIST )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_xySDIST_PnP.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xySDIST_PnP.f90.bdy"
!!--end--
END FUNCTION

END MODULE
