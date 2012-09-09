!!# PARAMETERS MODULE >>PAR_Mesh<<
MODULE PAR_Mesh

!!## PURPOSE
!! Provides the parameters for all the mesh-oriented types.

!!## MODULES
USE KND_Mesh !!((05-B-KND_Mesh.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PARAMETERS
!! * label length
INTEGER,PARAMETER :: LEN_MESH_LABEL = 32
!! * face structure
CHARACTER(*),PARAMETER :: KEY_FaceStructure(1:2) =(/"Essential",&
                                                    "CellBased" /)
INTEGER,PARAMETER :: MSH_Essential = 1
INTEGER,PARAMETER :: MSH_CellBased = 2
!! * Mesh type
CHARACTER(*),PARAMETER :: KEY_MeshType(1:7) =(/"Uniform     ",&
                                               "Regular     ",&
                                               "Structured  ",&
                                               "Unstructured",&
                                               "Randomized  ",&
                                               "Saw         ",&
                                               "Z           " /)
INTEGER,PARAMETER :: MSH_Uniform      = 1
INTEGER,PARAMETER :: MSH_Regular      = 2
INTEGER,PARAMETER :: MSH_Structured   = 3
INTEGER,PARAMETER :: MSH_Unstructured = 4
INTEGER,PARAMETER :: MSH_Randomized   = 5
INTEGER,PARAMETER :: MSH_Saw          = 6
INTEGER,PARAMETER :: MSH_Z            = 7
!! * default tolerance
REAL(KIND_MSH),PARAMETER :: DEFAULT_tol = 100._KIND_MSH*EPSILON(1._KIND_MSH)


!!## PUBLIC ACCESS LIST
!! * variables in this <PAR_Mesh> module
PUBLIC :: LEN_MESH_LABEL
PUBLIC :: KEY_FaceStructure
PUBLIC :: MSH_Essential
PUBLIC :: MSH_CellBased
PUBLIC :: KEY_MeshType
PUBLIC :: MSH_Uniform
PUBLIC :: MSH_Regular
PUBLIC :: MSH_Structured
PUBLIC :: MSH_Unstructured
PUBLIC :: MSH_Randomized
PUBLIC :: MSH_Saw
PUBLIC :: MSH_Z
PUBLIC :: DEFAULT_tol


END MODULE
