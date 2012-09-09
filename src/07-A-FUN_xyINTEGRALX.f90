!!# FUNCTION MODULE: <FUN_xyINTEGRALX>
MODULE FUN_xyINTEGRALX

!!## PURPOSE
!! Integrates the function $X$ over a polygon in 2D.


!!## METHOD
!! According to [1], the formula for the integral of $x$ over a polygon
!! is given by:
!
!! $$ S = \frac{1}{6} \sum_{i=1}^N ( x_i^2 + x_i x_{i-1} + x_{i-1}^2 )
!!                                 ( y_i - y_{i-1} ) $$
!
!! where the vertices of the polygon are given by $(x_i,y_i)$
!! for $i=1,...,N$ and $(x_0,y_0)=(x_N,y_N)$.


!!## AUTHORS
!! [jb] John Burkardt
!! [waw] W. A. Wieselquist


!!## MODIFIED
!! 10 July 2001 [jb]
!! 14 April 2007 [waw]


!!## REFERENCES
!! [1] SF Bockman,
!!     Generalizing the Formula for Areas of Polygons to Moments,
!!     American Mathematical Society Monthly,
!!     1989, pages 131-132.

!!## EXTERNAL MODULES
USE FUN_Error                                   !!((04-A-FUN_Error.f90))
USE FUN_xyAVERAGEX                              !!((05-A-FUN_xyAVERAGEX.f90))
USE FUN_xyINTEGRAL1                             !!((06-A-FUN_xyINTEGRAL1.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRALX_Pg
 MODULE PROCEDURE xyINTEGRALX_Pg_Rsp
 MODULE PROCEDURE xyINTEGRALX_Pg_Rdp
END INTERFACE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRALX_Ls
 MODULE PROCEDURE xyINTEGRALX_Ls_Rsp
 MODULE PROCEDURE xyINTEGRALX_Ls_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRALX_Ls
PUBLIC :: xyINTEGRALX_Ls_Rsp
PUBLIC :: xyINTEGRALX_Ls_Rdp

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRALX_Pg
PUBLIC :: xyINTEGRALX_Pg_Rsp
PUBLIC :: xyINTEGRALX_Pg_Rdp


!!## CONTAINED PROCEDURES
CONTAINS



!!### FUNCTION <xyINTEGRALX_Ls_Rsp>
FUNCTION xyINTEGRALX_Ls_Rsp( Ls ) RESULT(INTEGRALX)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-A-FUN_xyINTEGRALX_Ls.f90.hdr"
!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALX_Ls.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <xyINTEGRALX_Ls_Rdp>
FUNCTION xyINTEGRALX_Ls_Rdp( Ls ) RESULT(INTEGRALX)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-A-FUN_xyINTEGRALX_Ls.f90.hdr"
!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALX_Ls.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION <xyINTEGRALX_Pg_Rsp>
PURE FUNCTION xyINTEGRALX_Pg_Rsp( N , Pg ) RESULT(INTEGRALX)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "07-A-FUN_xyINTEGRALX_Pg.f90.hdr"

!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALX_Pg.f90.bdy"
!!--end--

END FUNCTION



!!### PURE FUNCTION <xyINTEGRALX_Pg_Rdp>
PURE FUNCTION xyINTEGRALX_Pg_Rdp( N , Pg ) RESULT(INTEGRALX)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "07-A-FUN_xyINTEGRALX_Pg.f90.hdr"
!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALX_Pg.f90.bdy"

!!--end--
END FUNCTION


END MODULE
