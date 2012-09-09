!!# FUNCTION MODULE: <FUN_xyAVERAGEX>
MODULE FUN_xyAVERAGEX

!!## PURPOSE
!! Averages the function $X$ in 2D.


!!## METHOD


!!## AUTHORS
!! [waw] W. A. Wieselquist


!!## MODIFIED
!! 14 April 2007 [waw]



!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## PROCEDURE OVERLOADING
INTERFACE xyAVERAGEX_Ls
 MODULE PROCEDURE xyAVERAGEX_Ls_Rsp
 MODULE PROCEDURE xyAVERAGEX_Ls_Rdp
ENDINTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyAVERAGEX_Ls
PUBLIC :: xyAVERAGEX_Ls_Rsp
PUBLIC :: xyAVERAGEX_Ls_Rdp


!!## CONTAINED PROCEDURES
CONTAINS


!!### FUNCTION <xyAVERAGEX_Ls_Rsp>
FUNCTION xyAVERAGEX_Ls_Rsp( Ls ) RESULT(AVERAGEX)
!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_xyAVERAGEX_Ls.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_xyAVERAGEX_Ls.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <xyAVERAGEX_Ls_Rdp>
FUNCTION xyAVERAGEX_Ls_Rdp( Ls ) RESULT(AVERAGEX)
!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_xyAVERAGEX_Ls.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_xyAVERAGEX_Ls.f90.bdy"
!!--end--
END FUNCTION



END MODULE
