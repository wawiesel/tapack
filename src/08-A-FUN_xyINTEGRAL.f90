!!# FUNCTION MODULE: <FUN_xyINTEGRAL>
MODULE FUN_xyINTEGRAL

!!## PURPOSE
!! Integrate stuff in 2D.


!!## AUTHORS
!! [waw] W. A. Wieselquist


!!## MODIFIED
!! 14 April 2007 [waw]


!!## EXTERNAL MODULES
USE FUN_xyINTEGRAL1                             !!((06-A-FUN_xyINTEGRAL1.f90))
USE FUN_xyINTEGRALX                             !!((07-A-FUN_xyINTEGRALX.f90))
USE FUN_xyINTEGRALY                             !!((07-A-FUN_xyINTEGRALY.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE
PRIVATE

!!## PROCEDURE OVERLOADING
![nocompile]!doesn't compile under ifort 9.1. why?
!INTERFACE xyINTEGRAL
! MODULE PROCEDURE xyINTEGRAL_Lin2Ls_Rsp
! MODULE PROCEDURE xyINTEGRAL_Lin2Ls_Rdp
! MODULE PROCEDURE xyINTEGRAL_Lin1Ls_Rsp
! MODULE PROCEDURE xyINTEGRAL_Lin1Ls_Rdp
!END INTERFACE

INTERFACE xyINTEGRAL_Lin2Ls
 MODULE PROCEDURE xyINTEGRAL_Lin2Ls_Rsp
 MODULE PROCEDURE xyINTEGRAL_Lin2Ls_Rdp
END INTERFACE

INTERFACE xyINTEGRAL_Lin1Ls
 MODULE PROCEDURE xyINTEGRAL_Lin1Ls_Rsp
 MODULE PROCEDURE xyINTEGRAL_Lin1Ls_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
![nocompile]!PUBLIC :: xyINTEGRAL
PUBLIC :: xyINTEGRAL_Lin2Ls
PUBLIC :: xyINTEGRAL_Lin1Ls


!!## CONTAINED PROCEDURES
CONTAINS


FUNCTION xyINTEGRAL_Lin1Ls_Rsp(Lin1,Ls) RESULT(INTEGRAL)
!!#### LOCAL MAPPINGS
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
INCLUDE "08-A-FUN_xyINTEGRAL_Lin1Ls.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyINTEGRAL_Lin1Ls.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTEGRAL_Lin1Ls_Rdp(Lin1,Ls) RESULT(INTEGRAL)
!!#### LOCAL MAPPINGS
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
INCLUDE "08-A-FUN_xyINTEGRAL_Lin1Ls.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyINTEGRAL_Lin1Ls.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTEGRAL_Lin2Ls_Rsp(Lin2,Ls) RESULT(INTEGRAL)
!!#### LOCAL MAPPINGS
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
INCLUDE "08-A-FUN_xyINTEGRAL_Lin2Ls.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyINTEGRAL_Lin2Ls.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTEGRAL_Lin2Ls_Rdp(Lin2,Ls) RESULT(INTEGRAL)
!!#### LOCAL MAPPINGS
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
INCLUDE "08-A-FUN_xyINTEGRAL_Lin2Ls.f90.hdr"
!!--begin--
INCLUDE "08-A-FUN_xyINTEGRAL_Lin2Ls.f90.bdy"
!!--end--
END FUNCTION


END MODULE



