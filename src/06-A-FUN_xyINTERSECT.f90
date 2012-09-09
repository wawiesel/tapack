!!# FUNCTION MODULE <<FUN_xyINTERSECT>>
MODULE FUN_xyINTERSECT

!!## PURPOSE
!! Determine if two objects intersect and optionally return some information
!! about the intersection.  The procedure xyINTERSECT_AB is read:
!
!!       The 2-dimensional routine for INTERSECTion of A and B.
!

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_0_Rsp => c_0     !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_0_Rdp => c_0     !!((02-A-PAR_Constants_Rdp.f90))

!!## EXTERNAL PROCEDURES
USE FUN_xyPERPCCW    ,ONLY: xyPERPCCW_V        !!((03-A-FUN_xyPERPCCW.f90))
USE FUN_xyDOT        ,ONLY: xyDOT_VV           !!((03-A-FUN_xyDOT.f90))
USE FUN_xyDIRECTION  ,ONLY: xyDIRECTION_V      !!((04-B-FUN_xyDIRECTION.f90))
USE FUN_xyLINE       ,ONLY: xyLINE_PP          !!((05-B-FUN_xyLINE.f90))
USE FUN_xySDIST      ,ONLY: xySDIST_PnP        !!((03-A-FUN_xySDIST.f90))
USE FUN_xyDIST       ,ONLY: xyDIST_PP          !!((04-B-FUN_xyDIST.f90))
USE FUN_IsApprox     ,ONLY: IsApprox           !!((03-A-FUN_IsApprox.f90))
USE FUN_xyLINESEGMENT,ONLY: xyLINESEGMENT_PP   !!((05-B-FUN_xyLINESEGMENT.f90))
USE FUN_Error                                  !!((04-A-FUN_Error.f90))
USE FUN_xyRAY                                  !!((05-B-FUN_xyRAY.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE xyINTERSECT_LnLn
 MODULE PROCEDURE xyINTERSECT_LnLn_Rsp
 MODULE PROCEDURE xyINTERSECT_LnLn_Rdp
END INTERFACE

INTERFACE xyINTERSECT_RyRy
 MODULE PROCEDURE xyINTERSECT_RyRy_Rsp
 MODULE PROCEDURE xyINTERSECT_RyRy_Rdp
END INTERFACE

INTERFACE xyINTERSECT_LnRy
 MODULE PROCEDURE xyINTERSECT_LnRy_Rsp
 MODULE PROCEDURE xyINTERSECT_LnRy_Rdp
END INTERFACE

INTERFACE xyINTERSECT_LsRy
 MODULE PROCEDURE xyINTERSECT_LsRy_Rsp
 MODULE PROCEDURE xyINTERSECT_LsRy_Rdp
END INTERFACE

INTERFACE xyINTERSECT_RyLn
 MODULE PROCEDURE xyINTERSECT_RyLn_Rsp
 MODULE PROCEDURE xyINTERSECT_RyLn_Rdp
END INTERFACE

INTERFACE xyINTERSECT_RyLs
 MODULE PROCEDURE xyINTERSECT_RyLs_Rsp
 MODULE PROCEDURE xyINTERSECT_RyLs_Rdp
END INTERFACE

INTERFACE xyINTERSECT_LnLs
 MODULE PROCEDURE xyINTERSECT_LnLs_Rsp
 MODULE PROCEDURE xyINTERSECT_LnLs_Rdp
END INTERFACE

INTERFACE xyINTERSECT_PnRy
 MODULE PROCEDURE xyINTERSECT_PnRy_Rsp
 MODULE PROCEDURE xyINTERSECT_PnRy_Rdp
END INTERFACE

INTERFACE xyINTERSECT_PnLn
 MODULE PROCEDURE xyINTERSECT_PnLn_Rsp
 MODULE PROCEDURE xyINTERSECT_PnLn_Rdp
END INTERFACE

INTERFACE xyINTERSECT_LsLs
 MODULE PROCEDURE xyINTERSECT_LsLs_Rsp
 MODULE PROCEDURE xyINTERSECT_LsLs_Rdp
END INTERFACE

INTERFACE xyINTERSECT_PnLs
 MODULE PROCEDURE xyINTERSECT_PnLs_Rsp
 MODULE PROCEDURE xyINTERSECT_PnLs_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
!! * ray intersections
PUBLIC :: xyINTERSECT_RyRy
PUBLIC :: xyINTERSECT_RyLn
PUBLIC :: xyINTERSECT_RyLs
PUBLIC :: xyINTERSECT_LsRy
PUBLIC :: xyINTERSECT_LnRy
!! * line intersections
PUBLIC :: xyINTERSECT_LnLn
PUBLIC :: xyINTERSECT_LnLs
PUBLIC :: xyINTERSECT_LsLs
!! * plane intersections
PUBLIC :: xyINTERSECT_PnLn
PUBLIC :: xyINTERSECT_PnRy
PUBLIC :: xyINTERSECT_PnLs

!!## LOCAL DEFAULTS
LOGICAL,PARAMETER :: DEFAULT_IncludeEnds = .FALSE.


!!## MODULE PROCEDURES
CONTAINS


!!###
FUNCTION xyINTERSECT_LnLn_Rsp( Ln_a , Ln_b , SDIST_a , P_intersect , SDIST_b ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LnLn.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LnLn.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_LnLn_Rdp( Ln_a , Ln_b , SDIST_a , P_intersect , SDIST_b ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LnLn.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LnLn.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTERSECT_RyRy_Rsp( Ry_a , Ry_b , DIST_a , P_intersect , DIST_b ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_RyRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_RyRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_RyRy_Rdp( Ry_a , Ry_b , DIST_a , P_intersect , DIST_b ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_RyRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_RyRy.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTERSECT_LnRy_Rsp( Ln , Ry , SDIST_Ln , P_intersect , DIST_Ry ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LnRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LnRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_LnRy_Rdp( Ln , Ry , SDIST_Ln , P_intersect , DIST_Ry ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LnRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LnRy.f90.bdy"
!!--end--
END FUNCTION



FUNCTION xyINTERSECT_LsRy_Rsp( Ls , Ry , FRAC_Ls , P_intersect , DIST_Ry , tol ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LsRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LsRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_LsRy_Rdp( Ls , Ry , FRAC_Ls , P_intersect , DIST_Ry , tol ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LsRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LsRy.f90.bdy"
!!--end--
END FUNCTION



FUNCTION xyINTERSECT_RyLn_Rsp( Ry , Ln , DIST_Ry , P_intersect , SDIST_Ln ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LnRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LnRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_RyLn_Rdp( Ry , Ln , DIST_Ry , P_intersect , SDIST_Ln ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LnRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LnRy.f90.bdy"
!!--end--
END FUNCTION



FUNCTION xyINTERSECT_RyLs_Rsp( Ry , Ls , DIST_Ry , P_intersect , FRAC_Ls , tol ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LsRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LsRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_RyLs_Rdp( Ry , Ls , DIST_Ry , P_intersect , FRAC_Ls , tol ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LsRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LsRy.f90.bdy"
!!--end--
END FUNCTION



FUNCTION xyINTERSECT_LnLs_Rsp( Ln , Ls , SDIST_Ln , P_intersect , FRAC_Ls , &
  IncludeEnds , key , tol ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LnLs.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LnLs.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_LnLs_Rdp( Ln , Ls , SDIST_Ln , P_intersect , FRAC_Ls , &
  IncludeEnds , key , tol ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LnLs.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LnLs.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTERSECT_LsLs_Rsp( Ls_a , Ls_b , FRAC_a , P_intersect , FRAC_b , &
  IncludeEnds_a , IncludeEnds_b , key , tol_a , tol_b ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LsLs.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LsLs.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_LsLs_Rdp( Ls_a , Ls_b , FRAC_a , P_intersect , FRAC_b , &
  IncludeEnds_a , IncludeEnds_b , key , tol_a , tol_b ) RESULT(INTERSECT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-FUN_xyINTERSECT_LsLs.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_LsLs.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTERSECT_PnRy_Rsp( Pn , Ry , DIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "06-A-FUN_xyINTERSECT_PnRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_PnRy.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_PnRy_Rdp( Pn , Ry , DIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "06-A-FUN_xyINTERSECT_PnRy.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_PnRy.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTERSECT_PnLn_Rsp( Pn , Ln , SDIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "06-A-FUN_xyINTERSECT_PnLn.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_PnLn.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_PnLn_Rdp( Pn , Ln , SDIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "06-A-FUN_xyINTERSECT_PnLn.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_PnLn.f90.bdy"
!!--end--
END FUNCTION



FUNCTION xyINTERSECT_PnLs_Rsp( Pn , Ls , DIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "06-A-FUN_xyINTERSECT_PnLs.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_PnLs.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERSECT_PnLs_Rdp( Pn , Ls , DIST , P_intersect ) RESULT(INTERSECT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "06-A-FUN_xyINTERSECT_PnLs.f90.hdr"
!!--begin--
INCLUDE "06-A-FUN_xyINTERSECT_PnLs.f90.bdy"
!!--end--
END FUNCTION




END MODULE
