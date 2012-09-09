!!# USER MODULE <<USR_NEGEXP>>
MODULE USR_NEGEXP

!!## PURPOSE
!! Retrieve the value of $exp(-x)$ from a lookup table.


!!## DEPENDENCIES
USE VAR_NEGEXP                                !!((09-C-VAR_NEGEXP.f90))
USE FUN_STR                                   !!((05-B-FUN_STR.f90))
USE FUN_Error                                 !!((04-A-FUN_Error.f90))
USE FUN_Interp1S_Linear                       !!((05-B-FUN_Interp1S_Linear.f90))
USE FUN_Default                               !!((04-A-FUN_Default.f90))
USE FUN_SIZEa                                 !!((06-B-FUN_SIZEa.f90))
USE FUN_NewUnit                               !!((04-B-FUN_NewUnit.f90))
USE FUN_IsApprox                              !!((03-A-FUN_IsApprox.f90))
USE SUB_Reallocate                            !!((04-B-SUB_Reallocate.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## INTERFACES
INTERFACE NEGEXP
 MODULE PROCEDURE NEGEXP_Rsp
 MODULE PROCEDURE NEGEXP_Rdp
END INTERFACE
INTERFACE INIT_NEGEXP
 MODULE PROCEDURE INIT_NEGEXP_Rsp
 MODULE PROCEDURE INIT_NEGEXP_Rdp
END INTERFACE
INTERFACE KILL_NEGEXP
 MODULE PROCEDURE KILL_NEGEXP_Rsp
 MODULE PROCEDURE KILL_NEGEXP_Rdp
END INTERFACE
INTERFACE RECALL_NEGEXP
 MODULE PROCEDURE RECALL_NEGEXP_Rsp
 MODULE PROCEDURE RECALL_NEGEXP_Rdp
END INTERFACE
INTERFACE DUMP_NEGEXP
 MODULE PROCEDURE DUMP_NEGEXP_Rsp
 MODULE PROCEDURE DUMP_NEGEXP_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: NEGEXP


!!## MODULE PROCEDURES
CONTAINS



SUBROUTINE INIT_NEGEXP_Rsp( MAX_s , tol )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rsp,y=>y_Rsp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP__INIT.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP__INIT.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE KILL_NEGEXP_Rsp(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rsp,y=>y_Rsp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP__KILL.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP__KILL.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE DUMP_NEGEXP_Rsp(s, Unit , file )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rsp,y=>y_Rsp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP__DUMP.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP__DUMP.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE RECALL_NEGEXP_Rsp(s, Unit , file )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rsp,y=>y_Rsp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP__RECALL.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP__RECALL.f90.bdy"
!!--end--
END SUBROUTINE


PURE FUNCTION NEGEXP_Rsp( s ) RESULT(val)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rsp,y=>y_Rsp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP.f90.bdy"
!!--end--
END FUNCTION




SUBROUTINE INIT_NEGEXP_Rdp( MAX_s , tol )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rdp,y=>y_Rdp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP__INIT.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP__INIT.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE KILL_NEGEXP_Rdp(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rdp,y=>y_Rdp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP__KILL.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP__KILL.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE DUMP_NEGEXP_Rdp(s, Unit , file )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rdp,y=>y_Rdp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP__DUMP.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP__DUMP.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE RECALL_NEGEXP_Rdp(s, Unit , file )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rdp,y=>y_Rdp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP__RECALL.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP__RECALL.f90.bdy"
!!--end--
END SUBROUTINE


PURE FUNCTION NEGEXP_Rdp( s ) RESULT(val)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_NEGEXP,ONLY: x=>x_Rdp,y=>y_Rdp        !!((09-C-VAR_NEGEXP.f90))
INCLUDE "10-C-USR_NEGEXP.f90.hdr"
!!--begin--
INCLUDE "10-C-USR_NEGEXP.f90.bdy"
!!--end--
END FUNCTION


END MODULE
