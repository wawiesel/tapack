MODULE FUN_xyCOLINEAR
!!#### PURPOSE
!! Test for colinearity between:
!! @ three points in R^2 <P3>
!! @ a line and a point in R^2 <LnP>

!!#### DETAILS
!! Collinear is the most common spelling but I don"t know where the
!! extra "l" comes from so I"m not using it.


!!#### METHOD
!! For three points to be colinear, the following ratios must be equal:
!!       x2-x1:y2-y1 = x3-x1:y3-y1
!! where the three points being tested are (x1,y1), (x2,y2), and (x3,y3).


!!#### SOURCE
!! Eric W. Weisstein. "Collinear." From MathWorld--A Wolfram Web Resource. http://mathworld.wolfram.com/Collinear.html


!We will check to see that the following fraction is equal.
!
!! x2-x1   x3-x1
!! ----- = -----
!! y2-y1   y3-y1


!!#### DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp                   !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_Default                                                  !!((04-A-FUN_Default.f90))


!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyCOLINEAR_P3
 MODULE PROCEDURE xyCOLINEAR_P3_Rsp
 MODULE PROCEDURE xyCOLINEAR_P3_Rdp
END INTERFACE

INTERFACE xyCOLINEAR_LnP
 MODULE PROCEDURE xyCOLINEAR_LnP_Rsp
 MODULE PROCEDURE xyCOLINEAR_LnP_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyCOLINEAR_P3
PUBLIC :: xyCOLINEAR_LnP

CONTAINS


FUNCTION xyCOLINEAR_P3_Rsp( P3 , tol , err ) RESULT(COLINEAR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes       ,ONLY: KIND_R=>KIND_Rsp             !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp        ,ONLY: c_1                          !!((02-A-PAR_Constants_Rsp.f90))
USE VAR_ComputationalGeometry,ONLY: DEFAULT_tol=>DEFAULT_tol_Rsp !!((03-A-VAR_ComputationalGeometry.f90))
INCLUDE "05-B-FUN_xyCOLINEAR_P3.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyCOLINEAR_P3.f90.alg"
!!--end--
END FUNCTION

FUNCTION xyCOLINEAR_P3_Rdp( P3 , tol , err ) RESULT(COLINEAR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes       ,ONLY: KIND_R=>KIND_Rdp             !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp        ,ONLY: c_1                          !!((02-A-PAR_Constants_Rdp.f90))
USE VAR_ComputationalGeometry,ONLY: DEFAULT_tol=>DEFAULT_tol_Rdp !!((03-A-VAR_ComputationalGeometry.f90))
INCLUDE "05-B-FUN_xyCOLINEAR_P3.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyCOLINEAR_P3.f90.alg"
!!--end--
END FUNCTION


FUNCTION xyCOLINEAR_LnP_Rsp( Ln , P , tol , err ) RESULT(COLINEAR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes       ,ONLY: KIND_R=>KIND_Rsp             !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp        ,ONLY: c_1                          !!((02-A-PAR_Constants_Rsp.f90))
USE VAR_ComputationalGeometry,ONLY: DEFAULT_tol=>DEFAULT_tol_Rsp !!((03-A-VAR_ComputationalGeometry.f90))
INCLUDE "05-B-FUN_xyCOLINEAR_LnP.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyCOLINEAR_LnP.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyCOLINEAR_LnP_Rdp( Ln , P , tol , err ) RESULT(COLINEAR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes       ,ONLY: KIND_R=>KIND_Rdp             !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp        ,ONLY: c_1                          !!((02-A-PAR_Constants_Rdp.f90))
USE VAR_ComputationalGeometry,ONLY: DEFAULT_tol=>DEFAULT_tol_Rdp !!((03-A-VAR_ComputationalGeometry.f90))
INCLUDE "05-B-FUN_xyCOLINEAR_LnP.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyCOLINEAR_LnP.f90.bdy"
!!--end--
END FUNCTION


FUNCTION TEST_xyCOLINEAR_LnP() RESULT(Pass)
LOGICAL :: Pass
Pass = .TRUE.

END FUNCTION

END MODULE
