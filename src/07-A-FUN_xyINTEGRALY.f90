!!# FUNCTION MODULE: <FUN_xyINTEGRALY>
MODULE FUN_xyINTEGRALY

!!## PURPOSE
!! Integrates the function $Y$ over a polygon in 2D.


!!## METHOD
!! According to [1], the formula for the integral of $x$ over a polygon
!! is given by:
!
!! $$ S = \frac{1}{6} \sum_{i=1}^N ( y_i^2 + y_i x_{i-1} + y_{i-1}^2 )
!!                                 ( x_i - x_{i-1} ) $$
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
USE FUN_xyAVERAGEY                              !!((05-A-FUN_xyAVERAGEY.f90))
USE FUN_xyINTEGRAL1                             !!((06-A-FUN_xyINTEGRAL1.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRALY_Pg
 MODULE PROCEDURE xyINTEGRALY_Pg_Rsp
 MODULE PROCEDURE xyINTEGRALY_Pg_Rdp
END INTERFACE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRALY_Ls
 MODULE PROCEDURE xyINTEGRALY_Ls_Rsp
 MODULE PROCEDURE xyINTEGRALY_Ls_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRALY_Ls
PUBLIC :: xyINTEGRALY_Ls_Rsp
PUBLIC :: xyINTEGRALY_Ls_Rdp

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRALY_Pg
PUBLIC :: xyINTEGRALY_Pg_Rsp
PUBLIC :: xyINTEGRALY_Pg_Rdp


!!## CONTAINED PROCEDURES
CONTAINS


!!### FUNCTION <xyINTEGRALY_Ls_Rsp>
FUNCTION xyINTEGRALY_Ls_Rsp( Ls ) RESULT(INTEGRALY)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-A-FUN_xyINTEGRALY_Ls.f90.hdr"
!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALY_Ls.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <xyINTEGRALY_Ls_Rdp>
FUNCTION xyINTEGRALY_Ls_Rdp( Ls ) RESULT(INTEGRALY)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-A-FUN_xyINTEGRALY_Ls.f90.hdr"
!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALY_Ls.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <xyINTEGRALY_Pg_Rsp>
FUNCTION xyINTEGRALY_Pg_Rsp( N , Pg ) RESULT(INTEGRALY)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "07-A-FUN_xyINTEGRALY_Pg.f90.hdr"

!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALY_Pg.f90.bdy"
!!--end--

END FUNCTION



!!### FUNCTION <xyINTEGRALY_Pg_Rdp>
FUNCTION xyINTEGRALY_Pg_Rdp( N , Pg ) RESULT(INTEGRALY)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "07-A-FUN_xyINTEGRALY_Pg.f90.hdr"
!!--begin--
INCLUDE "07-A-FUN_xyINTEGRALY_Pg.f90.bdy"

!!--end--
END FUNCTION


END MODULE
