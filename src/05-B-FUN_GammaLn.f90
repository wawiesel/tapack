MODULE FUN_GammaLn
!!#### PURPOSE
!! Returns the natural log of the gamma function,
!!       Ln( Gamma( x ) ), x>0

!!#### MODULES
USE KND_IntrinsicTypes,ONLY: SP=>KIND_Rsp,DP=>KIND_Rdp,I4B=>KIND_I4 !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp,ONLY: c_0                                     !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_Error                                                       !!((04-A-FUN_Error.f90))
USE FUN_Sequence                                                    !!((03-A-FUN_Sequence.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### PARAMETERS
REAL(DP),PARAMETER :: stp       =  2.5066282746310005_dp
REAL(DP),PARAMETER :: coef(1:6) = (/76.18009172947146_dp,&
                                   -86.50532032941677_dp,&
                                    24.01409824083091_dp,&
                                   -1.231739572450155_dp,&
                                0.1208650973866179e-2_dp,&
                                  -0.5395239384953e-5_dp/)

!!#### DEFAULT ACCESS
!debug with PUBLIC default access, run with PRIVATE default access
PRIVATE


!!#### OPERATOR OVERLOADING
INTERFACE GammaLn
 MODULE PROCEDURE GammaLn_A0Rsp
 MODULE PROCEDURE GammaLn_A1Rsp
 MODULE PROCEDURE GammaLn_A0Rdp
 MODULE PROCEDURE GammaLn_A1Rdp
ENDINTERFACE
PUBLIC :: GammaLn

CONTAINS

PURE FUNCTION GammaLn_A0Rsp(x) RESULT(LOG_gamma)
INTEGER,PARAMETER :: KIND_R = SP
INCLUDE "05-B-FUN_GammaLn_A0.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_GammaLn_A0.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION GammaLn_A1Rsp(x) RESULT(LOG_gamma)
INTEGER,PARAMETER :: KIND_R = SP
INCLUDE "05-B-FUN_GammaLn_A1.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_GammaLn_A1.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION GammaLn_A0Rdp(x) RESULT(LOG_gamma)
INTEGER,PARAMETER :: KIND_R = DP
INCLUDE "05-B-FUN_GammaLn_A0.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_GammaLn_A0.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION GammaLn_A1Rdp(x) RESULT(LOG_gamma)
INTEGER,PARAMETER :: KIND_R = DP
INCLUDE "05-B-FUN_GammaLn_A1.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_GammaLn_A1.f90.bdy"
!!--end--
END FUNCTION


ENDMODULE
