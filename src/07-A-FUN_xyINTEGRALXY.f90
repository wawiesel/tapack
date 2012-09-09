!!# FUNCTION MODULE: <FUN_xyINTEGRALXY>
MODULE FUN_xyINTEGRALXY

!!## PURPOSE
!! Integrates the function $XY$ over a polygon in 2D.


!*******************************************************************************
!
!! xyINTEGRALXY_Pg integrates the function X*Y over a polygon in 2D.
!
!  Discussion:
!
!    The polygon is bounded by the points (X(1:N), Y(1:N)).
!
!    INTEGRAL = (1/24) * sum ( 1 <= I <= N )
!      ( Y(I)   * ( 3 * X(I)**2 + 2 * X(I) * X(I-1) +     X(I-1)**2 )
!      + Y(I-1) * (     X(I)**2 + 2 * X(I) * X(I-1) + 3 * X(I-1)**2 ) )
!      * ( Y(I) - Y(I-1) )
!
!    where X(0) and Y(0) should be replaced by X(N) and Y(N).
!
!  Modified:
!
!    10 July 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    SF Bockman,
!    Generalizing the Formula for Areas of Polygons to Moments,
!    American Mathematical Society Monthly,
!    1989, pages 131-132.
!
!  Parameters:
!
!    Input, integer N, the number of vertices of the polygon.
!    N should be at least 3 for a nonzero INTEGRALXY.
!
!    Input, real ( kind = 8 ) Pg(2,N), the coordinates of the vertices
!    of the polygon.  These vertices should be given in
!    counter clockwise order.
!
!    Output, real ( kind = 8 ) INTEGRALXY, the value of the integral.
!

!!## EXTERNAL MODULES
USE FUN_Error                                   !!((04-A-FUN_Error.f90))
USE FUN_xyAVERAGEX                              !!((05-A-FUN_xyAVERAGEX.f90))
USE FUN_xyINTEGRAL1                             !!((06-A-FUN_xyINTEGRAL1.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRALXY_Pg
 MODULE PROCEDURE xyINTEGRALXY_Pg_Rsp
 MODULE PROCEDURE xyINTEGRALXY_Pg_Rdp
END INTERFACE

!!## PROCEDURE OVERLOADING
!INTERFACE xyINTEGRALXY_Ls
! MODULE PROCEDURE xyINTEGRALXY_Ls_Rsp
! MODULE PROCEDURE xyINTEGRALXY_Ls_Rdp
!END INTERFACE
!
!!!## PUBLIC ACCESS LIST
!PUBLIC :: xyINTEGRALXY_Ls
!PUBLIC :: xyINTEGRALXY_Ls_Rsp
!PUBLIC :: xyINTEGRALXY_Ls_Rdp

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRALXY_Pg
PUBLIC :: xyINTEGRALXY_Pg_Rsp
PUBLIC :: xyINTEGRALXY_Pg_Rdp


!!## CONTAINED PROCEDURES
CONTAINS


!
!!!### FUNCTION <xyINTEGRALXY_Ls_Rsp>
!FUNCTION xyINTEGRALXY_Ls_Rsp( Ls ) RESULT(INTEGRALX)
!
!!!#### LOCAL MAPPINGS
!USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
!USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
!INCLUDE "06-A-FUN_xyINTEGRALXY_Ls.f90.hdr"
!!!--begin--
!INCLUDE "06-A-FUN_xyINTEGRALXY_Ls.f90.bdy"
!!!--end--
!END FUNCTION
!
!
!
!!!### FUNCTION <xyINTEGRALXY_Ls_Rdp>
!FUNCTION xyINTEGRALXY_Ls_Rdp( Ls ) RESULT(INTEGRALX)
!
!!!#### LOCAL MAPPINGS
!USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rsp.f90))
!USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
!INCLUDE "06-A-FUN_xyINTEGRALXY_Ls.f90.hdr"
!!!--begin--
!INCLUDE "06-A-FUN_xyINTEGRALXY_Ls.f90.bdy"
!!!--end--
!END FUNCTION




!!### FUNCTION <xyINTEGRALXY_Pg_Rsp>
FUNCTION xyINTEGRALXY_Pg_Rsp( N , Pg ) RESULT(INTEGRALXY)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "07-A-FUN_xyINTEGRALXY_Pg.f90.hdr"

!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALXY_Pg.f90.bdy"
!!--end--

END FUNCTION



!!### FUNCTION <xyINTEGRALXY_Pg_Rdp>
FUNCTION xyINTEGRALXY_Pg_Rdp( N , Pg ) RESULT(INTEGRALXY)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "07-A-FUN_xyINTEGRALXY_Pg.f90.hdr"
!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALXY_Pg.f90.bdy"

!!--end--
END FUNCTION




END MODULE
