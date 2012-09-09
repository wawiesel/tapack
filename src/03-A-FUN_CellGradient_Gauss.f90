!!# FUNCTION MODULE: <<CellGradient_Gauss>>
MODULE FUN_CellGradient_Gauss

!!## PURPOSE
!! Compute a gradient in a cell according to Gauss' Theorem:
!!
!! $$ \vec{\nabla} f = \lim_{V \rightarrow 0} \frac{1}{V}
!!                     \int_{A} f d\vec{A}  $$
!!
!! where $V$ is the volume and the integral over $A$ implies an
!! integral over the surface of the cell.


!!## DETAILS
!! In discrete form, for a cell composed of planar faces we have:
!!
!! $$ \vec{\nabla} f_c = \frac{1}{V_c} \sum_\alpha \vec{A}_\alpha f_\alpha $$,
!!
!! where $\vec{\nabla}f_c$ is the gradient at the centroid of the cell,
!! $V_c$ is the volume of the cell, $\vec{A}_\alpha$ are the area vectors
!! of each face $\alpha$, and $f_\alpha$ are the function values at the centroids
!! of the faces.


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE CellGradient_Gauss
 MODULE PROCEDURE CellGradient_Gauss_Rsp
 MODULE PROCEDURE CellGradient_Gauss_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: CellGradient_Gauss_Rsp
PUBLIC :: CellGradient_Gauss_Rdp
PUBLIC :: CellGradient_Gauss


!!## MODULE PROCEDURES
CONTAINS

!!### PURE FUNCTION <<CellGradient_Gauss_Rsp>>
PURE FUNCTION CellGradient_Gauss_Rsp( Nd , Na , Vc , A , Fa ) RESULT(CellGradient)

!!#### LOCAL PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-FUN_CellGradient_Gauss.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_CellGradient_Gauss.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION <<CellGradient_Gauss_Rdp>>
PURE FUNCTION CellGradient_Gauss_Rdp( Nd , Na , Vc , A , Fa ) RESULT(CellGradient)

!!#### LOCAL PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-FUN_CellGradient_Gauss.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_CellGradient_Gauss.f90.bdy"
!!--end--
END FUNCTION


END MODULE
