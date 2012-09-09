!!### MODULE: FUNCTION  FUN_NDIM
MODULE FUN_NDIM
!!#### PURPOSE
!! Determine the number of dimensions for a shape in
!! 1D, 2D, or 3D rectangular, cylindrical, or spherical geometry.

!!#### USAGE
!
!         NDIM = NDIM_Shape( Coords , ARRAY )
!
!! where <Shape> is the shape-name of the object, <Coords> is either
!! <CGE_REC>, <CGE_CYL>, or <Spherical> (defined in <c_Geometry>,
!! and the <ARRAY> contains the values that define the shape.

!!#### AUTHOR
!! William Wieselquist | william.wieselquist@gmail.com

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_ComputationalGeometry,ONLY: CGE_Rec,CGE_Cyl,CGE_Sph !!((02-A-PAR_ComputationalGeometry.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_Error  ,ONLY: Error                                 !!((04-A-FUN_Error.f90))
USE FUN_Warning,ONLY: Warning                               !!((04-B-FUN_Warning.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE NDIM_P
 MODULE PROCEDURE NDIM_P_Rsp
 MODULE PROCEDURE NDIM_P_Rdp
END INTERFACE

INTERFACE NDIM_Pn
 MODULE PROCEDURE NDIM_Pn_Rsp
 MODULE PROCEDURE NDIM_Pn_Rdp
END INTERFACE

INTERFACE NDIM_Ry
 MODULE PROCEDURE NDIM_Ry_Rsp
 MODULE PROCEDURE NDIM_Ry_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: NDIM_Pn,NDIM_Ry,NDIM_P


CONTAINS


PURE FUNCTION NDIM_P_Rsp( Coords , P ) RESULT(NDIM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp               !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_NDIM_P.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_NDIM_P.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION NDIM_P_Rdp( Coords , P ) RESULT(NDIM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp               !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_NDIM_P.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_NDIM_P.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION NDIM_Pn_Rsp( Coords , Pn ) RESULT(NDIM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp               !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_NDIM_Pn.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_NDIM_Pn.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION NDIM_Pn_Rdp( Coords , Pn ) RESULT(NDIM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp               !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_NDIM_Pn.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_NDIM_Pn.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION NDIM_Ry_Rsp( Coords , Ry ) RESULT(NDIM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp               !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_NDIM_Ry.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_NDIM_Ry.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION NDIM_Ry_Rdp( Coords , Ry ) RESULT(NDIM)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp               !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_NDIM_Ry.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_NDIM_Ry.f90.bdy"
!!--end--
END FUNCTION


END MODULE
