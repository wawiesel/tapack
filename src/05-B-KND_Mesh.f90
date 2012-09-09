!!# KIND MODULE >>KND_Mesh<<
MODULE KND_Mesh

!!## PURPOSE
!! Provides the kinds for all the mesh-oriented types.

!!## EXTERNAL KINDS I
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## EXTERNAL KINDS II
!! * define the mesh real kind
INTEGER,PARAMETER :: KIND_MSH = KIND_Rdp

!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_MSH

END MODULE
