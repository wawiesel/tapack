MODULE FUN_xyINTERIOR
!!#### PURPOSE
!! Check if <shape> is interior to polygon, where shape is:
!!  @ point <P>,
!!  @ plane segment <Ps>.

!!#### METHOD
!! @ point <P>
!!   Check the dot product of the edge perpendicular (pointed inward)
!!   and vector from edge point to point of interest (P2) for each
!!   edge.  If the dot product is ever negative, then this indicates
!!   that the point is outside the edge.  This is an effective procedure
!!   because points are rejected with the first outside edge they
!!   find themselves on the wrong side of.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_0_Rsp => c_0     !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_0_Rdp => c_0     !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyDOT    ,ONLY: xyDOT_VV               !!((03-A-FUN_xyDOT.f90))
USE FUN_xyPERPCCW,ONLY: xyPERPCCW_V            !!((03-A-FUN_xyPERPCCW.f90))
USE FUN_xyPLANE  ,ONLY: xyPLANE_PV             !!((05-B-FUN_xyPLANE.f90))
USE FUN_xySDIST  ,ONLY: xySDIST_PnP            !!((03-A-FUN_xySDIST.f90))
USE FUN_xyINTERSECT,ONLY: xyINTERSECT_LsLs     !!((06-A-FUN_xyINTERSECT.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyINTERIOR_PgP
 MODULE PROCEDURE xyINTERIOR_PgP_Rsp
 MODULE PROCEDURE xyINTERIOR_PgP_Rdp
END INTERFACE

INTERFACE xyINTERIOR_PgLs
 MODULE PROCEDURE xyINTERIOR_PgLs_Rsp
 MODULE PROCEDURE xyINTERIOR_PgLs_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyINTERIOR_PgP
PUBLIC :: xyINTERIOR_PgLs

INTEGER :: Unit

CONTAINS


FUNCTION xyINTERIOR_PgP_Rsp( N , Pg , P , P_centroid , &
 KEY , tol , IncludeEdges ) RESULT(INTERIOR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "07-B-FUN_xyINTERIOR_PgP.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyINTERIOR_PgP.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERIOR_PgP_Rdp( N , Pg , P , P_centroid , &
  KEY , tol , IncludeEdges ) RESULT(INTERIOR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "07-B-FUN_xyINTERIOR_PgP.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyINTERIOR_PgP.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyINTERIOR_PgLs_Rsp( N , Pg , Ls , P_centroid , &
  KEY , tol , IncludeEnds ) RESULT(INTERIOR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_xyINTERIOR_PgLs.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyINTERIOR_PgLs.f90.bdy"
!!--end--
END FUNCTION

FUNCTION xyINTERIOR_PgLs_Rdp( N , Pg , Ls , P_centroid , &
  KEY , tol , IncludeEnds ) RESULT(INTERIOR)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_xyINTERIOR_PgLs.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyINTERIOR_PgLs.f90.bdy"
!!--end--
END FUNCTION


END MODULE
