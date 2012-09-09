!!# FUNCTION MODULE: <FUN_xyINTEGRAL1>
MODULE FUN_xyINTEGRAL1

!!## PURPOSE
!! Integrates the function $1$ over a polygon in 2D.


!!## METHOD
!! According to [1], the formula for the integral of $1$ over a polygon
!! is given by:
!
!! $$ S = \frac{1}{6} \sum_{i=1}^N ( x_{i-1} y_{i} - x_{i} y_{i-1} ) $$
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
USE FUN_xySAREA                                 !!((03-A-FUN_xySAREA.f90))
USE FUN_xyDIST                                  !!((04-B-FUN_xyDIST.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRAL1_Pg
 MODULE PROCEDURE xyINTEGRAL1_Pg_Rsp
 MODULE PROCEDURE xyINTEGRAL1_Pg_Rdp
ENDINTERFACE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTEGRAL1_Ls
 MODULE PROCEDURE xyINTEGRAL1_Ls_Rsp
 MODULE PROCEDURE xyINTEGRAL1_Ls_Rdp
ENDINTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRAL1_Ls
PUBLIC :: xyINTEGRAL1_Ls_Rsp
PUBLIC :: xyINTEGRAL1_Ls_Rdp

!!## PUBLIC ACCESS LIST
PUBLIC :: xyINTEGRAL1_Pg
PUBLIC :: xyINTEGRAL1_Pg_Rsp
PUBLIC :: xyINTEGRAL1_Pg_Rdp


!!## CONTAINED PROCEDURES
CONTAINS



!!### FUNCTION <xyINTEGRAL1_Ls_Rsp>
FUNCTION xyINTEGRAL1_Ls_Rsp( Ls ) RESULT(INTEGRAL1)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTEGRAL1_Ls.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTEGRAL1_Ls.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <xyINTEGRAL1_Ls_Rdp>
FUNCTION xyINTEGRAL1_Ls_Rdp( Ls ) RESULT(INTEGRAL1)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTEGRAL1_Ls.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTEGRAL1_Ls.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <xyINTEGRAL1_Pg_Rsp>
FUNCTION xyINTEGRAL1_Pg_Rsp( N , Pg ) RESULT(INTEGRAL1)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "06-A-FUN_xyINTEGRAL1_Pg.f90.hdr"

!!--begin--
INCLUDE "06-A-FUN_xyINTEGRAL1_Pg.f90.bdy"
!!--end--

END FUNCTION



!!### FUNCTION <xyINTEGRAL1_Pg_Rdp>
FUNCTION xyINTEGRAL1_Pg_Rdp( N , Pg ) RESULT(INTEGRAL1)

!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "06-A-FUN_xyINTEGRAL1_Pg.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTEGRAL1_Pg.f90.bdy"

!!--end--
END FUNCTION


END MODULE
