!!# FUNCTION MODULE: <FUN_xyAVERAGEY>
MODULE FUN_xyAVERAGEY

!!## PURPOSE
!! Averages the function $Y$ in 2D.


!!## METHOD


!!## AUTHORS
!! [waw] W. A. Wieselquist


!!## MODIFIED
!! 14 April 2007 [waw]



!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## PROCEDURE OVERLOADING
INTERFACE xyAVERAGEY_Ls
 MODULE PROCEDURE xyAVERAGEY_Ls_Rsp
 MODULE PROCEDURE xyAVERAGEY_Ls_Rdp
ENDINTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyAVERAGEY_Ls
PUBLIC :: xyAVERAGEY_Ls_Rsp
PUBLIC :: xyAVERAGEY_Ls_Rdp


!!## CONTAINED PROCEDURES
CONTAINS


!!### FUNCTION <xyAVERAGEY_Ls_Rsp>
FUNCTION xyAVERAGEY_Ls_Rsp( Ls ) RESULT(AVERAGEY)
!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_xyAVERAGEY_Ls.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_xyAVERAGEY_Ls.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <xyAVERAGEY_Ls_Rdp>
FUNCTION xyAVERAGEY_Ls_Rdp( Ls ) RESULT(AVERAGEY)
!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_xyAVERAGEY_Ls.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_xyAVERAGEY_Ls.f90.bdy"
!!--end--
END FUNCTION


END MODULE
