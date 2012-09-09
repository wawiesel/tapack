!!# FUNCTION MODULE <<Integrate1_aq>>
MODULE FUN_Integrate1_aq

!!## PURPOSE
!! Integrate a function using Adaptive Quadrature.


!!## METHOD
!! Uses Adaptive Quadrature to approximate the definite
!! integral of f(x) from p(1) to p(2) within a given tolerance, tol.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PROCEDURES
USE FUN_Error                                  !!((04-A-FUN_Error.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE OVERLOADING
INTERFACE Integrate1_aq
 MODULE PROCEDURE Integrate1_aq_Rsp
 MODULE PROCEDURE Integrate1_aq_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: Integrate1_aq


CONTAINS

!!### RECURSIVE FUNCTION <<Integrate1_aq_Rsp>>
RECURSIVE FUNCTION Integrate1_aq_Rsp(f,p,tol,N) RESULT(result)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp                          !!((02-A-PAR_Constants_Rsp.f90))
INCLUDE "03-A-INT_Function1_Rsp.f90.hdr"
INCLUDE "06-B-FUN_Integrate1_aq.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_Integrate1_aq.f90.bdy"
!!--end--
END FUNCTION


!!### RECURSIVE FUNCTION <<Integrate1_aq_Rdp>>
RECURSIVE FUNCTION Integrate1_aq_Rdp(f,p,tol,N) RESULT(result)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp                          !!((02-A-PAR_Constants_Rdp.f90))
INCLUDE "03-A-INT_Function1_Rdp.f90.hdr"
INCLUDE "06-B-FUN_Integrate1_aq.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_Integrate1_aq.f90.bdy"
!!--end--
END FUNCTION

END MODULE
