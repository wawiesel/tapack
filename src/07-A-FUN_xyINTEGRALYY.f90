!!# FUNCTION MODULE: <FUN_xyINTEGRALYY>
MODULE FUN_xyINTEGRALYY

!!## PURPOSE
!! Integrates the function $YY$ over a polygon in 2D.


!*******************************************************************************
!
!! xyINTEGRALYY_Pg integrates the function Y*Y over a polygon in 2D.
!
!  Discussion:
!
!    The polygon is bounded by the points (X(1:N), Y(1:N)).
!
!    INTEGRAL = (1/12) * sum ( 1 <= I <= N )
!      - ( Y(I)**3 + Y(I)**2 * Y(I-1) + Y(I) * Y(I-1)**2 + Y(I-1)**3 )
!      * ( X(I) - X(I-1) )
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
!    Input, integer N, the number of Pgertices of the polygon.
!    N should be at least 3 for a nonzero INTEGRALYY.
!
!    Input, real ( kind = 8 ) Pg(2,N), the coordinates of the Pgertices
!    of the polygon.  These Pgertices should be giPgen in
!    counter clockwise order.
!
!    Output, real ( kind = 8 ) INTEGRALYY, the Pgalue of the integral.
!


!!## EXTERNAL MODULES
USE FUN_Error                                   !!((04-A-FUN_Error.f90))
USE FUN_xyAVERAGEX                              !!((05-A-FUN_xyAVERAGEX.f90))
USE FUN_xyINTEGRAL1                             !!((06-A-FUN_xyINTEGRAL1.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRALYY_Pg
 MODULE PROCEDURE xyINTEGRALYY_Pg_Rsp
 MODULE PROCEDURE xyINTEGRALYY_Pg_Rdp
END INTERFACE

!!## PROCEDURE OVERLOADING
!INTERFACE xyINTEGRALYY_Ls
! MODULE PROCEDURE xyINTEGRALYY_Ls_Rsp
! MODULE PROCEDURE xyINTEGRALYY_Ls_Rdp
!END INTERFACE
!
!!!## PUBLIC ACCESS LIST
!PUBLIC :: xyINTEGRALYY_Ls
!PUBLIC :: xyINTEGRALYY_Ls_Rsp
!PUBLIC :: xyINTEGRALYY_Ls_Rdp

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRALYY_Pg
PUBLIC :: xyINTEGRALYY_Pg_Rsp
PUBLIC :: xyINTEGRALYY_Pg_Rdp


!!## CONTAINED PROCEDURES
CONTAINS


!
!!!### FUNCTION <xyINTEGRALYY_Ls_Rsp>
!FUNCTION xyINTEGRALYY_Ls_Rsp( Ls ) RESULT(INTEGRALX)
!
!!!#### LOCAL MAPPINGS
!USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
!USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
!INCLUDE "06-A-FUN_xyINTEGRALYY_Ls.f90.hdr"
!!!--begin--
!INCLUDE "06-A-FUN_xyINTEGRALYY_Ls.f90.bdy"
!!!--end--
!END FUNCTION
!
!
!
!!!### FUNCTION <xyINTEGRALYY_Ls_Rdp>
!FUNCTION xyINTEGRALYY_Ls_Rdp( Ls ) RESULT(INTEGRALX)
!
!!!#### LOCAL MAPPINGS
!USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rsp.f90))
!USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
!INCLUDE "06-A-FUN_xyINTEGRALYY_Ls.f90.hdr"
!!!--begin--
!INCLUDE "06-A-FUN_xyINTEGRALYY_Ls.f90.bdy"
!!!--end--
!END FUNCTION




!!### PURE FUNCTION <xyINTEGRALYY_Pg_Rsp>
PURE FUNCTION xyINTEGRALYY_Pg_Rsp( N , Pg ) RESULT(INTEGRALYY)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "07-A-FUN_xyINTEGRALYY_Pg.f90.hdr"

!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALYY_Pg.f90.bdy"
!!--end--

END FUNCTION



!!### PURE FUNCTION <xyINTEGRALYY_Pg_Rdp>
PURE FUNCTION xyINTEGRALYY_Pg_Rdp( N , Pg ) RESULT(INTEGRALYY)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "07-A-FUN_xyINTEGRALYY_Pg.f90.hdr"
!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALYY_Pg.f90.bdy"

!!--end--
END FUNCTION




END MODULE
