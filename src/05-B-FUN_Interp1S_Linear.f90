!!# MODULE: FUNCTION  Interp1S_Linear
MODULE FUN_Interp1S_Linear

!!## PURPOSE
!! Interpolate 1D scattered data using linear interpolation.

!!## USAGE
!
!      y0 = Interp1S_Linear( x0 , y , x , dy0 )
!
!! where <x> is list of x-values, <y> is list of y-values,
!! and <(x0,y0)> are the interpolated <(x,y)> pair.


!!## METHOD
!! Simple linear interpolation of two values.  If the lists
!! y and x have more than 2 points, then a search is performed
!! to determine the correct interval.  IMPORTANT:  Extrapolation
!! is not performed if the value <x0> lies outside the
!! domain.  Instead, the value <y0> is assigned the value at the
!! nearest endpoint.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))


!!## DEPENDENCIES
USE FUN_IntervalBisection,ONLY: Interval=>IntervalBisection !!((03-A-FUN_IntervalBisection.f90))
USE FUN_Error            ,ONLY: Error                       !!((04-A-FUN_Error.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## FUNCTION OVERLOAD
INTERFACE Interp1S_Linear
 MODULE PROCEDURE Interp1S_Linear_yRsp_xRsp
 MODULE PROCEDURE Interp1S_Linear_yRdp_xRsp
 MODULE PROCEDURE Interp1S_Linear_yRsp_xRdp
 MODULE PROCEDURE Interp1S_Linear_yRdp_xRdp
 MODULE PROCEDURE Interp1S_Linear_yRsp_xRsp_dy0
 MODULE PROCEDURE Interp1S_Linear_yRdp_xRsp_dy0
 MODULE PROCEDURE Interp1S_Linear_yRsp_xRdp_dy0
 MODULE PROCEDURE Interp1S_Linear_yRdp_xRdp_dy0
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: Interp1S_Linear


!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: <Interp1S_Linear_yRsp_xRsp>
PURE FUNCTION Interp1S_Linear_yRsp_xRsp( x0 , y , x  ) RESULT( y0 )

INTEGER,PARAMETER :: KIND_x=KIND_Rsp , KIND_y=KIND_Rsp
INCLUDE "05-B-FUN_Interp1S_Linear.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Interp1S_Linear.f90.sup"
INCLUDE "05-B-FUN_Interp1S_Linear.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: <Interp1S_Linear_yRdp_xRsp>
PURE FUNCTION Interp1S_Linear_yRdp_xRsp( x0 , y , x  ) RESULT( y0 )
INTEGER,PARAMETER :: KIND_x=KIND_Rsp , KIND_y=KIND_Rdp
INCLUDE "05-B-FUN_Interp1S_Linear.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Interp1S_Linear.f90.sup"
INCLUDE "05-B-FUN_Interp1S_Linear.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: <Interp1S_Linear_yRsp_xRdp>
PURE FUNCTION Interp1S_Linear_yRsp_xRdp( x0 , y , x  ) RESULT( y0 )
INTEGER,PARAMETER :: KIND_x=KIND_Rdp , KIND_y=KIND_Rsp
INCLUDE "05-B-FUN_Interp1S_Linear.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Interp1S_Linear.f90.sup"
INCLUDE "05-B-FUN_Interp1S_Linear.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: <Interp1S_Linear_yRdp_xRdp>
PURE FUNCTION Interp1S_Linear_yRdp_xRdp( x0 , y , x  ) RESULT( y0 )
INTEGER,PARAMETER :: KIND_x=KIND_Rdp , KIND_y=KIND_Rdp
INCLUDE "05-B-FUN_Interp1S_Linear.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Interp1S_Linear.f90.sup"
INCLUDE "05-B-FUN_Interp1S_Linear.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: <Interp1S_Linear_yRsp_xRsp_dy0>
FUNCTION Interp1S_Linear_yRsp_xRsp_dy0( x0 , y , x , dy0 ) RESULT( y0 )
INTEGER,PARAMETER :: KIND_x=KIND_Rsp , KIND_y=KIND_Rsp
INCLUDE "05-B-FUN_Interp1S_Linear_dy0.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Interp1S_Linear_dy0.f90.sup"
INCLUDE "05-B-FUN_Interp1S_Linear.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: <Interp1S_Linear_yRdp_xRsp_dy0>
FUNCTION Interp1S_Linear_yRdp_xRsp_dy0( x0 , y , x , dy0 ) RESULT( y0 )
INTEGER,PARAMETER :: KIND_x=KIND_Rsp , KIND_y=KIND_Rdp
INCLUDE "05-B-FUN_Interp1S_Linear_dy0.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Interp1S_Linear_dy0.f90.sup"
INCLUDE "05-B-FUN_Interp1S_Linear.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: <Interp1S_Linear_yRsp_xRdp_dy0>
FUNCTION Interp1S_Linear_yRsp_xRdp_dy0( x0 , y , x , dy0 ) RESULT( y0 )
INTEGER,PARAMETER :: KIND_x=KIND_Rdp , KIND_y=KIND_Rsp
INCLUDE "05-B-FUN_Interp1S_Linear_dy0.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Interp1S_Linear_dy0.f90.sup"
INCLUDE "05-B-FUN_Interp1S_Linear.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: <Interp1S_Linear_yRdp_xRdp_dy0>
FUNCTION Interp1S_Linear_yRdp_xRdp_dy0( x0 , y , x , dy0 ) RESULT( y0 )
INTEGER,PARAMETER :: KIND_x=KIND_Rdp , KIND_y=KIND_Rdp
INCLUDE "05-B-FUN_Interp1S_Linear_dy0.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_Interp1S_Linear_dy0.f90.sup"
INCLUDE "05-B-FUN_Interp1S_Linear.f90.bdy"
!!--end--
END FUNCTION


END MODULE
