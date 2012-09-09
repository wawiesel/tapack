MODULE FUN_xyzPLANE
!!#### PURPOSE
!! Create a plane with three different interfaces.
!
!!  1.  Pn = xyzPLANE( C )
!!            where C = (/a,b,c,d/) of plane equation a*x+b*y+c*z+d=0.
!
!!  2.  Pn = xyzPLANE( P3 )
!!            where P3 = (/1:3,i/) is the ith point which defines the plane,
!!            where the number of points, i=1:3.
!!
!!  3.  Pn = xyzPLANE( P , V )
!!            where P is a point and V is a vector.

!!#### METHOD
!! Use the Hessian normal form for a plane.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_0_Rsp => c_0     !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_0_Rdp => c_0     !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyzCROSS ,ONLY: xyzCROSS_VV            !!((05-B-FUN_xyzCROSS.f90))
USE FUN_xyzNORM  ,ONLY: xyzNORM_V              !!((03-A-FUN_xyzNORM.f90))
USE FUN_xyzDOT   ,ONLY: xyzDOT_VV              !!((05-B-FUN_xyzDOT.f90))
USE FUN_xyzVECTOR,ONLY: xyzVECTOR_PP           !!((03-A-FUN_xyzVECTOR.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzPLANE_P3
 MODULE PROCEDURE xyzPLANE_P3_Rsp
 MODULE PROCEDURE xyzPLANE_P3_Rdp
END INTERFACE

INTERFACE xyzPLANE_PV
 MODULE PROCEDURE xyzPLANE_PV_Rsp
 MODULE PROCEDURE xyzPLANE_PV_Rdp
END INTERFACE

INTERFACE xyzPLANE_C
 MODULE PROCEDURE xyzPLANE_PAR_Rsp
 MODULE PROCEDURE xyzPLANE_PAR_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzPLANE_P3
PUBLIC :: xyzPLANE_PV
PUBLIC :: xyzPLANE_C


CONTAINS


PURE FUNCTION xyzPLANE_PV_Rsp( P , V ) RESULT( xyzPn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "06-B-FUN_xyzPLANE_PV.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzPLANE_PV.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzPLANE_PV_Rdp( P , V ) RESULT( xyzPn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "06-B-FUN_xyzPLANE_PV.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzPLANE_PV.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyzPLANE_PAR_Rsp( C ) RESULT( xyzPn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "06-B-FUN_xyzPLANE_C.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzPLANE_C.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzPLANE_PAR_Rdp( C ) RESULT( xyzPn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "06-B-FUN_xyzPLANE_C.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzPLANE_C.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyzPLANE_P3_Rsp( P3 ) RESULT( xyzPn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "06-B-FUN_xyzPLANE_P3.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzPLANE_P3.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyzPLANE_P3_Rdp( P3 ) RESULT( xyzPn )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "06-B-FUN_xyzPLANE_P3.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzPLANE_P3.f90.bdy"
!!--end--
END FUNCTION


END MODULE
