!!# TOOLBOX MODULE  <<TBX_Mesh>>
MODULE TBX_Mesh

!!## PURPOSE
!! Provides routines for building Meshes for 1, 2, and
!! 3-dimensional uniform, regular, structured, and unstructured
!! meshes.


!!## MODULES
USE PAR_Constants_Rdp                                                !!((02-A-PAR_Constants_Rdp.f90))
USE BOP_sEQ                                                          !!((03-A-BOP_sEQ.f90))
USE FUN_CellGradient_Gauss                                           !!((03-A-FUN_CellGradient_Gauss.f90))
USE FUN_IsApprox                                                     !!((03-A-FUN_IsApprox.f90))
USE FUN_PACK2                                                        !!((03-A-FUN_PACK2.f90))
USE FUN_Random                                                       !!((03-A-FUN_Random.f90))
USE FUN_Sequence                                                     !!((03-A-FUN_Sequence.f90))
USE FUN_Upcase                                                       !!((03-A-FUN_Upcase.f90))
USE ISO_varying_string                                               !!((03-A-ISO_varying_string.f90))
USE VAR_Units,ONLY: DEFAULT_OUTPUT_UNIT                              !!((03-A-VAR_Units.f90))
USE USR_Iptr                                                         !!((03-C-USR_Iptr.f90))
USE VAR_FindMe                                                       !!((03-C-VAR_FindMe.f90))
USE FUN_Default                                                      !!((04-A-FUN_Default.f90))
USE FUN_Error                                                        !!((04-A-FUN_Error.f90))
USE FUN_ptr_Sequence                                                 !!((04-A-FUN_ptr_Sequence.f90))
USE FUN_Reverse                                                      !!((04-A-FUN_Reverse.f90))
USE SUB_CLEAR                                                        !!((04-A-SUB_CLEAR.f90))
USE SUB_CLEARn                                                       !!((04-A-SUB_CLEARn.f90))
USE SUB_Swap                                                         !!((04-A-SUB_Swap.f90))
USE FUN_NewUnit                                                      !!((04-B-FUN_NewUnit.f90))
USE FUN_RGBA_Hot                                                     !!((04-B-FUN_RGBA_Hot.f90))
USE FUN_Sentence                                                     !!((04-B-FUN_Sentence.f90))
USE LIB_Norm                                                         !!((04-B-LIB_Norm.f90))
USE SUB_Pause                                                        !!((04-B-SUB_Pause.f90))
USE SUB_Stop                                                         !!((04-B-SUB_Stop.f90))
USE FUN_IsError                                                      !!((05-A-FUN_IsError.f90))
USE FUN_Reorder                                                      !!((05-A-FUN_Reorder.f90))
USE SUB_FlagDuplicateVectors0                                        !!((05-A-SUB_FlagDuplicateVectors0.f90))
USE FUN_Counter                                                      !!((05-B-FUN_Counter.f90))
USE FUN_NewFile                                                      !!((05-B-FUN_NewFile.f90))
USE FUN_Spectrum                                                     !!((05-B-FUN_Spectrum.f90))
USE FUN_STR                                                          !!((05-B-FUN_STR.f90))
USE FUN_TimeStamp                                                    !!((05-B-FUN_TimeStamp.f90))
USE FUN_NEARLOC                                                      !!((06-A-FUN_NEARLOC.f90))
USE SUB_Sort2_quick,Sort2=>Sort2_quick                               !!((06-A-SUB_Sort2_quick.f90))
USE FUN_InterpGrid2S_BiVar                                           !!((06-B-FUN_InterpGrid2S_BiVar.f90))
USE FUN_InterpGrid2S_CShep                                           !!((06-B-FUN_InterpGrid2S_CShep.f90))
USE FUN_SIZEa                                                        !!((06-B-FUN_SIZEa.f90))
USE LIB_Prompts                                                      !!((06-B-LIB_Prompts.f90))
USE FUN_STRTIME                                                      !!((06-C-FUN_STRTIME.f90))
USE FUN_InterpGrid2S_Nearest                                         !!((07-B-FUN_InterpGrid2S_Nearest.f90))
USE FUN_STRn                                                         !!((07-B-FUN_STRn.f90))
USE LIB_GenericPhrases                                               !!((07-B-LIB_GenericPhrases.f90))
USE PRN_Text                                                         !!((07-B-PRN_Text.f90))
USE FUN_INDEXa                                                       !!((08-B-FUN_INDEXa.f90))
USE PRN_Array2                                                       !!((08-B-PRN_Array2.f90))
USE SUB_FlagDuplicateVectors                                         !!((08-B-SUB_FlagDuplicateVectors.f90))
USE USR_fdbk                                                         !!((08-C-USR_fdbk.f90))
USE TBX_ComputationalGeometry                                        !!((09-A-TBX_ComputationalGeometry.f90))
USE USR_SimpleList                                                   !!((09-B-USR_SimpleList.f90))
USE LIB_xy                                                           !!((10-B-LIB_xy.f90))
USE USR_Face                                                         !!((11-B-USR_Face.f90))
USE USR_Cell                                                         !!((12-B-USR_Cell.f90))
USE USR_ExplicitFace                                                 !!((12-B-USR_ExplicitFace.f90))
USE USR_SimpleSet                                                    !!((12-B-USR_SimpleSet.f90))
USE SUB_FindUniqueVectors                                            !!((13-B-SUB_FindUniqueVectors.f90))
USE USR_ExplicitCell                                                 !!((13-B-USR_ExplicitCell.f90))
USE USR_Domain                                                       !!((13-B-USR_Domain.f90))
USE FUN_Default

USE KND_Mesh                                                         !!((05-B-KND_Mesh.f90))
USE PAR_Mesh                                                         !!((06-B-PAR_Mesh.f90))
USE USR_Mesh                                                         !!((14-B-USR_Mesh.f90))
USE USR_IntegralRegion                                               !!((14-B-USR_IntegralRegion.f90))
USE SUB_Reallocate                                                   !!((04-B-SUB_Reallocate.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
!PUBLIC !PRIVATE
PRIVATE

!!## TYPE DEFINITION
TYPE TYPE_Interfaces
 INTEGER         :: master=0
 INTEGER,POINTER :: subs(:) => NULL()
END TYPE
!
![idea]
!
!Interfaces in a Single 1D array
!
! number of master faces is Njm
!
! A(1:Njm), starting subface locations, js0
! A(Njm+1:2*Njm) number of subfaces, Njs
!
! for master face jm
! js0 = A(jm)
! Njs = A(jm+Njm)
!
! each subface js belonging to master face jm is given as
!  js = A(n), n=js0,Njs-js0+1
!


!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_ ="TBX_Mesh"
CHARACTER(*),PARAMETER :: file_="15-B-TBX_Mesh.f90"
PRIVATE :: mod_,file_


INTERFACE CREATE
 MODULE PROCEDURE CREATE_Mesh
ENDINTERFACE

INTERFACE DESTROY
 MODULE PROCEDURE DESTROY_Mesh
ENDINTERFACE

INTERFACE IsQg
 MODULE PROCEDURE IsQg_Mesh
 MODULE PROCEDURE IsQg_
END INTERFACE

INTERFACE IsPg
 MODULE PROCEDURE IsPg_Mesh
 MODULE PROCEDURE IsPg_
END INTERFACE

INTERFACE IsTr
 MODULE PROCEDURE IsTr_Mesh
 MODULE PROCEDURE IsTr_
END INTERFACE

INTERFACE IsQr
 MODULE PROCEDURE IsQr_Mesh
 MODULE PROCEDURE IsQr_
END INTERFACE

INTERFACE IsQs
 MODULE PROCEDURE IsQs_Mesh
 MODULE PROCEDURE IsQs_
END INTERFACE

INTERFACE IsPs
 MODULE PROCEDURE IsPs_
END INTERFACE

INTERFACE IsJx
 MODULE PROCEDURE IsJx_
END INTERFACE

INTERFACE SETUP_FaceNormal
 MODULE PROCEDURE SETUP_FaceNormal_Domain
 MODULE PROCEDURE SETUP_FaceNormal_Mesh
END INTERFACE

INTERFACE IsBoundaryVert
 MODULE PROCEDURE IsBoundaryVert0
 MODULE PROCEDURE IsBoundaryVert1
END INTERFACE

INTERFACE WRAPUP_FaceNormal
 MODULE PROCEDURE WRAPUP_FaceNormal_Domain
 MODULE PROCEDURE WRAPUP_FaceNormal_Mesh
END INTERFACE

INTERFACE EVAL_FaceNormal
 MODULE PROCEDURE EVAL_FaceNormal_Domain
 MODULE PROCEDURE EVAL_FaceNormal_Mesh
END INTERFACE

INTERFACE FaceNormal
 MODULE PROCEDURE FaceNormal_Domain
 MODULE PROCEDURE FaceNormal_Mesh
END INTERFACE

INTERFACE DomainFaceNormal
 MODULE PROCEDURE DomainFaceNormal_Mesh
END INTERFACE

INTERFACE DomainFaceCentroid
 MODULE PROCEDURE DomainFaceCentroid_Mesh
END INTERFACE

INTERFACE UPDATE_FaceNormal
 MODULE PROCEDURE UPDATE_FaceNormal_Domain
 MODULE PROCEDURE UPDATE_FaceNormal_Mesh
END INTERFACE

INTERFACE UniZ
 MODULE PROCEDURE UniZ_Mesh
END INTERFACE

INTERFACE UniY
 MODULE PROCEDURE UniY_Mesh
END INTERFACE

INTERFACE UniX
 MODULE PROCEDURE UniX_Mesh
END INTERFACE

INTERFACE SETUP_Mesh_Adap3
 MODULE PROCEDURE SETUP_Mesh_Adap3_
END INTERFACE

INTERFACE PRINT_Mesh_Adap3
 MODULE PROCEDURE PRINT_Mesh_Adap3_
END INTERFACE

INTERFACE PRINT_Mesh
 MODULE PROCEDURE PRINT_Mesh_
END INTERFACE

!!## INTERFACES
INTERFACE ADD_DomainVert
 MODULE PROCEDURE ADD_DomainVert_Mesh
END INTERFACE

INTERFACE ADD_DomainFace
 MODULE PROCEDURE ADD_DomainFace_Mesh
END INTERFACE

INTERFACE ADD_DomainCell
 MODULE PROCEDURE ADD_DomainCell_Mesh
END INTERFACE

INTERFACE ADD_Vert
 MODULE PROCEDURE ADD_Vert_Mesh
 MODULE PROCEDURE ADD_Vert_Domain
END INTERFACE

INTERFACE ADD_Face
 MODULE PROCEDURE ADD_Face_Mesh
 MODULE PROCEDURE ADD_Face_Domain
END INTERFACE

INTERFACE ADD_Cell
 MODULE PROCEDURE ADD_Cell_Mesh
 MODULE PROCEDURE ADD_Cell_Domain
END INTERFACE

INTERFACE REMOVE_Vert
 MODULE PROCEDURE REMOVE_Vert_Mesh
! MODULE PROCEDURE REMOVE_Vert_Domain
END INTERFACE

INTERFACE REMOVE_Face
 MODULE PROCEDURE REMOVE_Face_Mesh
! MODULE PROCEDURE REMOVE_Face_Domain
END INTERFACE

INTERFACE REMOVE_Cell
 MODULE PROCEDURE REMOVE_Cell_Mesh
! MODULE PROCEDURE REMOVE_Cell_Domain
END INTERFACE

INTERFACE INSERT_Cell
 MODULE PROCEDURE INSERT_Cell_Mesh
! MODULE PROCEDURE INSERT_Cell_Domain
END INTERFACE

INTERFACE EdgeShapes
 MODULE PROCEDURE EdgeShapes_Mesh
END INTERFACE

INTERFACE ptr_FaceList
 MODULE PROCEDURE ptr_FaceList_Mesh
END INTERFACE

INTERFACE FINALIZE
 MODULE PROCEDURE FINALIZE_Mesh
END INTERFACE

INTERFACE REORDER_Faces
 MODULE PROCEDURE REORDER_Faces_Mesh
END INTERFACE

!!### SIMPLE INTERFACES for pointer returns
INTERFACE ptr_Cells        ; MODULE PROCEDURE ptr_Cells_Mesh        ; END INTERFACE
INTERFACE ptr_Faces        ; MODULE PROCEDURE ptr_Faces_Mesh        ; END INTERFACE
INTERFACE ptr_Verts        ; MODULE PROCEDURE ptr_Verts_Mesh        ; END INTERFACE
INTERFACE ptr_Edges        ; MODULE PROCEDURE ptr_Edges_Mesh        ; END INTERFACE
INTERFACE ptr_VertList     ; MODULE PROCEDURE ptr_VertList_Mesh     ; END INTERFACE
INTERFACE ptr_FaceShape    ; MODULE PROCEDURE ptr_FaceShape_Mesh    ; END INTERFACE
INTERFACE ptr_CellShape    ; MODULE PROCEDURE ptr_CellShape_Mesh    ; END INTERFACE
INTERFACE ptr_EdgeShape    ; MODULE PROCEDURE ptr_EdgeShape_Mesh    ; END INTERFACE

INTERFACE ptr_FaceCentroid ; MODULE PROCEDURE ptr_FaceCentroid_Mesh ; END INTERFACE
INTERFACE ptr_FaceArea     ; MODULE PROCEDURE ptr_FaceArea_Mesh     ; END INTERFACE

INTERFACE FaceShape       ; MODULE PROCEDURE FaceShape_Mesh       ; END INTERFACE
INTERFACE CellShape       ; MODULE PROCEDURE CellShape_Mesh       ; END INTERFACE
INTERFACE EdgeShape       ; MODULE PROCEDURE EdgeShape_Mesh       ; END INTERFACE
INTERFACE FaceLabel       ; MODULE PROCEDURE FaceLabel_Mesh       ; END INTERFACE
INTERFACE CellLabel       ; MODULE PROCEDURE CellLabel_Mesh       ; END INTERFACE

INTERFACE NUM_Faces       ; MODULE PROCEDURE NUM_Faces_Mesh      ; END INTERFACE
INTERFACE NUM_Verts       ; MODULE PROCEDURE NUM_Verts_Mesh      ; END INTERFACE
INTERFACE NUM_Cells       ; MODULE PROCEDURE NUM_Cells_Mesh      ; END INTERFACE
INTERFACE NUM_Edges       ; MODULE PROCEDURE NUM_Edges_Mesh      ; END INTERFACE

INTERFACE ExplicitFace    ; MODULE PROCEDURE ExplicitFace_Mesh   ; END INTERFACE
INTERFACE ExplicitCell    ; MODULE PROCEDURE ExplicitCell_Mesh   ; END INTERFACE

INTERFACE COUNT_Cells     ; MODULE PROCEDURE COUNT_Cells_Mesh    ; END INTERFACE
INTERFACE COUNT_Faces     ; MODULE PROCEDURE COUNT_Faces_Mesh    ; END INTERFACE
INTERFACE COUNT_Verts     ; MODULE PROCEDURE COUNT_Verts_Mesh    ; END INTERFACE
INTERFACE NUM_Dimensions  ; MODULE PROCEDURE NUM_Dimensions_Mesh ; END INTERFACE
INTERFACE HasDomain       ; MODULE PROCEDURE HasDomain_Mesh      ; END INTERFACE
INTERFACE IsEqual         ; MODULE PROCEDURE IsEqual_Mesh        ; END INTERFACE


!!### KINDS
PUBLIC :: KIND_MSH

!!### PARAMETERS
PUBLIC :: LEN_MESH_LABEL

!!### DEFINED TYPES
PUBLIC :: TYPE_Mesh
PUBLIC :: TYPE_ExplicitFace
PUBLIC :: TYPE_Cell
PUBLIC :: TYPE_Face
PUBLIC :: TYPE_ExplicitCell
PUBLIC :: TYPE_Domain
PUBLIC :: TYPE_Interfaces
PUBLIC :: TYPE_IntegralRegion

!!### PRINTING
PUBLIC :: PRINT_Mesh
PUBLIC :: PRINT_Mesh_Adap3

!!### COORDINATES FUNCTION
PUBLIC :: coords

!!### UNIFORM PLOTTING GRID
PUBLIC :: UniX,UniY,UniZ
PUBLIC :: ModifyInterpGrid

!!### TESTING
PUBLIC :: TEST_FaceIntegration_WARSA

!!### KEYS
PUBLIC :: Qs_,Qr_,Qg_
PUBLIC :: Straight_
PUBLIC :: KEY_CellShape
PUBLIC :: KEY_EdgeShape
PUBLIC :: KEY_FaceShape
PUBLIC :: KEY_FaceStructure
PUBLIC :: KEY_MeshType

!!### Mesh CREATE/DESTROY
PUBLIC :: CREATE
PUBLIC :: CREATE_Mesh
PUBLIC :: CREATE_Mesh_CellBasedDual
PUBLIC :: DESTROY

!!### Mesh BUILDING
!! * full names
PUBLIC :: ADD_Vert_Mesh
PUBLIC :: ADD_Face_Mesh
PUBLIC :: ADD_Cell_Mesh
PUBLIC :: REMOVE_Vert_Mesh
PUBLIC :: REMOVE_Face_Mesh
PUBLIC :: REMOVE_Cell_Mesh
!! * interface names
PUBLIC :: ADD_Vert
PUBLIC :: ADD_Face
PUBLIC :: ADD_Cell
PUBLIC :: REMOVE_Vert
PUBLIC :: REMOVE_Face
PUBLIC :: REMOVE_Cell
PUBLIC :: INSERT_Cell
PUBLIC :: SPLIT_Cell
PUBLIC :: SPLIT_Cells
PUBLIC :: SPLIT_Face
PUBLIC :: SPLIT_Mesh_Pn
PUBLIC :: SPLIT_Mesh_Pg
PUBLIC :: PERTURB_Verts
PUBLIC :: GRAVITATE_Verts
PUBLIC :: CRACK_Cell
PUBLIC :: TRANSLATE_Mesh
PUBLIC :: SCALE_Mesh
PUBLIC :: ROTATE_Mesh
PUBLIC :: ROTATE_Domain
PUBLIC :: ROTATE_Verts
PUBLIC :: TEST_DualMesh

!!### UNSTRUCTURED FACES/CELLS
PUBLIC :: ADD_Face_Ps
PUBLIC :: ADD_Cell_Jx

!!### DOMAIN BUILDING OPERATIONS
PUBLIC :: CREATE_Domain_Qr
PUBLIC :: ADD_domainVert
PUBLIC :: ADD_domainFace
PUBLIC :: ADD_domainCell

!!### ENTITY QUERY
PUBLIC :: GET_Vert
PUBLIC :: GET_Face
PUBLIC :: GET_Cell
PUBLIC :: GET_VertOnFace
PUBLIC :: GET_Inv_JBdryE
PUBLIC :: GET_CellSpatialMoments
PUBLIC :: MAPTO_CellBased
PUBLIC :: MAPTO_CellBased_Boundary
PUBLIC :: EVAL_CellTraversal

!!### STATUS INQUIRY
PUBLIC :: IsBoundaryFace
PUBLIC :: IsBoundaryVert
PUBLIC :: IsCornerVert
PUBLIC :: IsHangingVert
PUBLIC :: IsPointOnFace
PUBLIC :: IsCellBased
PUBLIC :: IsQg,IsQs,IsTr,IsQr,IsPs,IsJx
PUBLIC :: HasFaceVerts
PUBLIC :: HasCellFaces
PUBLIC :: HasCellVerts
PUBLIC :: HasCellVert

!!### NUMBER FUNCTIONS
!! * full names
PUBLIC :: NUM_Dimensions_Mesh
PUBLIC :: NUM_Verts_Mesh
PUBLIC :: NUM_Faces_Mesh
PUBLIC :: NUM_Cells_Mesh
PUBLIC :: NUM_Edges_Mesh
!! * interface names
PUBLIC :: NUM_Dimensions
PUBLIC :: NUM_Verts
PUBLIC :: NUM_Faces
PUBLIC :: NUM_Cells
PUBLIC :: NUM_Edges
PUBLIC :: NUM_DomainFaces
PUBLIC :: NUM_Faces_CellIndex

!!### COUNTING FUNCTIONS
PUBLIC :: COUNT_BoundaryFaces
PUBLIC :: COUNT_InteriorFaces
PUBLIC :: COUNT_BoundaryVerts

!!### CALCULATION FUNCTIONS
PUBLIC :: FaceAverage
PUBLIC :: CellAverage
PUBLIC :: CellAverage_Lin
PUBLIC :: DimSpan
PUBLIC :: MaxSpan
PUBLIC :: MinSpan
PUBLIC :: AverageSpan
PUBLIC :: MeshScale
PUBLIC :: InteriorCell
PUBLIC :: IntersectFace
PUBLIC :: InteriorCells_Omega
PUBLIC :: IntersectFaces_Omega
PUBLIC :: BoundaryFacesFromVert
PUBLIC :: SymmetryDirection
PUBLIC :: SymmetryPoint
PUBLIC :: SymmetryFace
PUBLIC :: CellTravelSequence
PUBLIC :: LinearCellFunction_Gauss
PUBLIC :: LinearCellFunction_LLS

!!### ODDBALLS
PUBLIC :: GET_Direction
PUBLIC :: CellShape
PUBLIC :: ADD_blockverts,ADD_blockfaces,ADD_blockcells
PUBLIC :: PRINT_IntegralRegions
PUBLIC :: TriangulateCell_2D
PUBLIC :: UPDATE_IntegralRegions
PUBLIC :: SETUP_Grid
PUBLIC :: WRAPUP_Grid
PUBLIC :: GET_CellVertBoundarySet
PUBLIC :: GET_CellVertBoundaryList

!!### ENSURE QUALITY
PUBLIC :: ENSURE_Mesh_CellBased

!!### ORIENTATION
PUBLIC :: OrientBoundary
PUBLIC :: OrientInterior

!!### CONVERSIONS TO EXPLICIT FORMAT
PUBLIC :: ExplicitCell
PUBLIC :: ExplicitFace

!!### EXPLICIT INQUIRIES
PUBLIC :: Interior_XcP
PUBLIC :: Intersect_XfRy

!!### FINALIZE FOR COMPUTATION
!! * full names
PUBLIC :: FINALIZE_Mesh
!! * interface names
PUBLIC :: FINALIZE

!!### RENAMING
PUBLIC :: RENAME_Mesh
PUBLIC :: RENAME_Cells
PUBLIC :: RENAME_Faces
PUBLIC :: RENAME_Verts

!!### INQUIRY FUNCTIONS
PUBLIC :: DAT_Verts
PUBLIC :: DAT_FaceCentroids
PUBLIC :: DAT_FaceNormals
PUBLIC :: DAT_CellCentroids


!!### EVALUATION FUNCTIONS
PRIVATE :: EVAL_FaceArea
PRIVATE :: EVAL_FaceNormal
PRIVATE :: EVAL_DomainFace
PRIVATE :: EVAL_FaceCentroid
PRIVATE :: EVAL_CellCentroid
PRIVATE :: EVAL_CellVolume

!!### SETUP SUBROUTINES
PUBLIC :: SETUP_FaceArea
PUBLIC :: SETUP_FaceNormal
PUBLIC :: SETUP_DomainFace
PUBLIC :: SETUP_FaceCentroid
PUBLIC :: SETUP_CellCentroid
PUBLIC :: SETUP_CellVolume
PUBLIC :: SETUP_Interfaces
PUBLIC :: SETUP_Mesh
PUBLIC :: SETUP_Mesh_Adap3
PUBLIC :: SETUP_VertToFaceLink

!!### WRAP UP SUBROUTINES
PUBLIC :: WRAPUP_FaceArea
PUBLIC :: WRAPUP_FaceNormal
PUBLIC :: WRAPUP_DomainFace
PUBLIC :: WRAPUP_FaceCentroid
PUBLIC :: WRAPUP_CellCentroid
PUBLIC :: WRAPUP_CellVolume
PUBLIC :: WRAPUP_Mesh

!!### UPDATE SUBROUTINES
PUBLIC :: UPDATE_FaceArea
PUBLIC :: UPDATE_FaceNormal
PUBLIC :: UPDATE_FaceCentroid
PUBLIC :: UPDATE_CellCentroid
PUBLIC :: UPDATE_CellVolume
PUBLIC :: UPDATE_DomainFace
PUBLIC :: UPDATE_BoundaryFaces
PUBLIC :: UPDATE_Mesh
PUBLIC :: UPDATE_Cell
PUBLIC :: UPDATE_Face

!!### RETURN POINTERS
!! * full names
PUBLIC :: ptr_CellSet_ALL
PUBLIC :: ptr_CellSet_POLYREGION
PUBLIC :: ptr_CellSet_GENLABEL
PUBLIC :: ptr_VertSet_ALL
PUBLIC :: ptr_VertSet_POLYREGION
PUBLIC :: ptr_VertSet_GENLABEL
PUBLIC :: ptr_FaceSet_ALL
PUBLIC :: ptr_FaceSet_POLYREGION
PUBLIC :: ptr_FaceSet_GENLABEL
PUBLIC :: ptr_FaceSet_PLANEINTERSECT
PUBLIC :: ptr_FaceShape_Mesh
PUBLIC :: ptr_EdgeShape_Mesh
PUBLIC :: ptr_CellShape_Mesh
PUBLIC :: ptr_VertList_Mesh
PUBLIC :: ptr_FaceList_Mesh
PUBLIC :: ptr_Edges_Mesh
!! * interface names
PUBLIC :: ptr_FaceShape
PUBLIC :: ptr_EdgeShape
PUBLIC :: ptr_CellShape
PUBLIC :: ptr_VertList
PUBLIC :: ptr_FaceList
PUBLIC :: ptr_Edges

!!### RETURN VALUES
PUBLIC :: EdgeShapes
PUBLIC :: FaceShape
PUBLIC :: EdgeShape
PUBLIC :: FaceArea
PUBLIC :: FaceNormal
PUBLIC :: DomainFace
PUBLIC :: DomainFaceNormal
PUBLIC :: DomainFaceCentroid
PUBLIC :: FaceCentroid
PUBLIC :: CellCentroid
PUBLIC :: CellVolume
PUBLIC :: Vert
PUBLIC :: Verts
PUBLIC :: ParentFace
PUBLIC :: FirstVert
PUBLIC :: LastVert

!!### DEBUGGING
PUBLIC :: DEBUG_Mesh
PUBLIC :: CHECK_VOL0
PUBLIC :: CHECK_AREA0

!!### PARAMETERS for Mesh types
PUBLIC :: MSH_Unstructured
PUBLIC :: MSH_Structured
PUBLIC :: MSH_Regular
PUBLIC :: MSH_Randomized
PUBLIC :: MSH_Uniform
PUBLIC :: MSH_Saw
PUBLIC :: MSH_Z

!PUBLIC :: EVAL_CellSurfaceArea
!PUBLIC :: EVAL_FaceAreaNormal
PUBLIC :: Reorder_CellBased
PUBLIC :: Reorder_Faces
PUBLIC :: Add_CellCentroidVerts
PUBLIC :: MAX_FacesPerCell,&
 SETUP_meshcelliterator,SliceCell,&
 GET_MemberCells,SETUP_IntegralRegions,&
  FaceAverage_F,CellAverage_F

!!### GLOBALS
INTEGER                :: Nx_=20,Ny_=20,Nz_=20
REAL(KIND_MSH),POINTER :: xout_(:)=>NULL()
REAL(KIND_MSH),POINTER :: yout_(:)=>NULL()
REAL(KIND_MSH),POINTER :: zout_(:)=>NULL()
LOGICAL,SAVE           :: VERBOSE=.TRUE.
CHARACTER(10) :: DEBUG=''

!!## CONTAINED PROCEDURES
CONTAINS


!!### FUNCTION <<CHECK_COPY_Mesh>>
FUNCTION CHECK_COPY_Mesh(Mesh) RESULT(Check)
!!#### PURPOSE
!! Check a copy of a mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### LOCAL VARIABLES
TYPE(TYPE_Mesh) :: MeshA,MeshB
LOGICAL         :: Check,Test

!!--begin--

Check = .FALSE.

!copy
CALL COPY_Mesh(MeshA,Mesh)

!test copy
Test = IsEqual_Mesh(MeshA,Mesh) ; IF( .NOT.Test )RETURN

!test copy of copy
CALL COPY_Mesh(MeshB,MeshA)
Test = IsEqual_Mesh(Mesh,MeshB); IF( .NOT.Test )RETURN

!test permutations
Test = IsEqual_Mesh(MeshB,MeshA); IF( .NOT.Test )RETURN
Test = IsEqual_Mesh(MeshA,MeshB); IF( .NOT.Test )RETURN
Test = IsEqual_Mesh(Mesh ,MeshA); IF( .NOT.Test )RETURN
Test = IsEqual_Mesh(MeshA,Mesh ); IF( .NOT.Test )RETURN
Test = IsEqual_Mesh(Mesh ,MeshB); IF( .NOT.Test )RETURN
Test = IsEqual_Mesh(MeshB,Mesh ); IF( .NOT.Test )RETURN

CHECK = .TRUE.

!!--end--
END FUNCTION



!!### SUBROUTINE <<ADD_FlippedFaces>>
SUBROUTINE ADD_FlippedFaces(Mesh,FaceMapping)

!!#### PURPOSE
!! The first stage of making a Mesh Cell-based, converting
!! all double-duty faces (those who appear in two cell <FaceList>
!! to two actual faces in opposite directions.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
INTEGER,INTENT(INOUT),OPTIONAL :: FaceMapping(:)

!!#### LOCAL VARIABLES
INTEGER :: i,j,j_,jabs,jnew,l,Nl
TYPE(TYPE_Edge),POINTER :: revEdges(:)

!!--begin--
DO i=1,NUM_Cells(Mesh)
 DO j=1,SIZE(Mesh%Cells(i)%FaceList)

  !get global face index
  j_=Mesh%Cells(i)%FaceList(j)

  !WE HAVE A DOUBLE-DUTY FACE
  IF( j_<0 )THEN
   !get absolute value of global index
   jabs = ABS(j_)

   !setup the reversed edges
   Nl = NUM_Edges(Mesh%Faces(jabs))
   NULLIFY(revEdges)
   ALLOCATE( revEdges(1:Nl) )
   DO l=1,Nl
    revEdges(l) = Edge(Mesh%Faces(jabs),Nl-l+1)
   END DO

   !add the new face
   jnew = ADD_Face( Mesh , &
     FaceShape = FaceShape(Mesh,jabs) , &
     FaceLabel = TRIM(FaceLabel(Mesh,jabs))//"r" , &
     FaceNormal = -FaceNormal(Mesh,jabs) , &
     VertList = Reverse(Mesh%Faces(jabs)%VertList) , &
     Edges = revEdges , &
     FaceArea = FaceArea(Mesh,jabs) , &
     FaceCentroid = FaceCentroid(Mesh,jabs) , &
     SubFaces = (/jabs/) )

   IF( PRESENT(FaceMapping) )THEN
    FaceMapping(jnew) = j_
   END IF

   !replace old face index with new face index
   Mesh%Cells(i)%FaceList(j) = jnew

   !give the old face a subface, too
   CALL ALLOCATE( Mesh%Faces(jabs) , SubFaces = (/jnew/) )

   !wrapup the reversed edges
   DEALLOCATE( revEdges )

  ELSE
   IF( PRESENT(FaceMapping) )THEN
    FaceMapping(j_) = j_
   END IF
  END IF
 END DO
END DO

!finalize again
CALL Finalize(Mesh)

!!--end--
END SUBROUTINE


PURE FUNCTION IsCellBased(Mesh) RESULT(TorF)
!!#### PURPOSE
!! Return whether a Mesh is CellBased or not.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
LOGICAL :: TorF

!!--begin--
TorF = Mesh%FaceStructure==MSH_CellBased

!!--end--
END FUNCTION


PURE FUNCTION IsEssential(Mesh) RESULT(TorF)
!!#### PURPOSE
!! Return whether a Mesh is Essential or not.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
LOGICAL :: TorF

!!--begin--
TorF = Mesh%FaceStructure==MSH_Essential

!!--end--
END FUNCTION


!!### SUBROUTINE <<ENSURE_Mesh_CellBased>>
SUBROUTINE ENSURE_Mesh_CellBased(Mesh,Reorder)

!!#### PURPOSE
!! Ensure that a mesh is cell based.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: Reorder

!!#### LOCAL VARIABLES
LOGICAL :: Reorder_

!!--begin--
Reorder_ = Default(.TRUE.,Reorder)

!! 0.a. orient boundary and finalize
CALL FINALIZE( Mesh )
CALL OrientBoundary( Mesh )

!! 0.b.
Mesh%FaceStructure = MSH_CellBased

!! 1. Add the flipped versions of faces.
CALL ADD_FlippedFaces(Mesh)

!! 2. Create the compound faces.
CALL CREATE_CompoundFaces(Mesh)

!! 3. Reorder
IF( Reorder_ )THEN
 CALL Reorder_Cellbased(Mesh)
END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<CREATE_Mesh_CellBasedDual>>
SUBROUTINE CREATE_Mesh_CellBasedDual(&
  EssentialMesh,CellbasedMesh,FaceMapping,Reorder)

!!#### PURPOSE
!! Create a cell-based mesh from an essential
!! mesh and a face mapping to map every face
!! on the cell-based mesh to one of the essential mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: EssentialMesh

!!#### REQUIRED OUTPUT
TYPE(TYPE_Mesh),INTENT(OUT) :: CellBasedMesh

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: FaceMapping(:)

!!#### OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: Reorder

!!#### LOCAL VARIABLES
LOGICAL :: Reorder_
INTEGER :: Ns,js,Nj_CB,s,j
LOGICAL,PARAMETER :: Print_DualMesh=.FALSE.

!!--begin--

Reorder_ = Default(.TRUE.,Reorder)

!! 0.a. orient boundary and finalize
CALL FINALIZE( EssentialMesh )
CALL OrientBoundary( EssentialMesh )

! get a copy of the essential mesh
CALL COPY_Mesh(CellBasedMesh,EssentialMesh)

!! 0.b.
CellBasedMesh%FaceStructure = MSH_CellBased

Nj_CB = NUM_Faces(EssentialMesh)
NULLIFY( FaceMapping )
ALLOCATE( FaceMapping(2*Nj_CB) )
FaceMapping = ERROR(1)

!! 1. Add the flipped versions of faces.
CALL ADD_FlippedFaces(CellBasedMesh,FaceMapping)
if( Print_DualMesh )then
   CALL PRINT_Mesh(EssentialMesh,Unit=NewFile("Mesh.txt",IfOpened="Close"))
end if

!! 2. Create the compound faces.
CALL CREATE_CompoundFaces(CellBasedMesh,FaceMapping)
if( Print_DualMesh )then
   CALL PRINT_Mesh(CellBasedMesh,Unit=NewFile("LOMesh.txt",IfOpened="Close"))
end if

!! Zero out all master faces (leaving only subfaces on the cellbased mesh which
!! map back to faces on the essential mesh).
Nj_CB = NUM_Faces(CellBasedMesh)
DO j=1,Nj_CB
 IF( ASSOCIATED(CellBasedMesh%Faces(j)%SubFaces) )THEN
  Ns = SIZE(CellBasedMesh%Faces(j)%SubFaces)
  IF( Ns>1 )THEN
   FaceMapping(j) = 0
  END IF
 END IF
END DO
CALL REALLOCATE( FaceMapping , SIZEa(FaceMapping)-SIZE(FaceMapping) )

!! 3. Reorder and change face mapping as needed.
IF( Reorder_ )THEN
 CALL Reorder_Cellbased(CellBasedMesh,FaceMapping)
END IF



!!--end--
END SUBROUTINE



SUBROUTINE DAT_Verts( Mesh )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: k,Unit

Unit = NewUnit()

OPEN(Unit,FILE="data/xv.dat")
DO k=1,Mesh%NVerts
 WRITE(Unit,*)Mesh%Verts(1,k)
END DO
CLOSE(Unit)

OPEN(Unit,FILE="data/yv.dat")
DO k=1,Mesh%NVerts
 WRITE(Unit,*)Mesh%Verts(2,k)
END DO
CLOSE(Unit)

END SUBROUTINE


SUBROUTINE DAT_FaceCentroids( Mesh )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: j
INTEGER :: Unit

Unit = NewUnit()

OPEN(Unit,FILE="data/xf.dat")
DO j=1,Mesh%NFaces
 WRITE(Unit,*)Mesh%Faces(j)%FaceCentroid(1)
END DO
CLOSE(Unit)

OPEN(Unit,FILE="data/yf.dat")
DO j=1,Mesh%NFaces
 WRITE(Unit,*)Mesh%Faces(j)%FaceCentroid(2)
END DO
CLOSE(Unit)

END SUBROUTINE



SUBROUTINE DAT_FaceNormals( Mesh )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: j
INTEGER :: Unit

Unit = NewUnit()

OPEN(Unit,FILE="data/nxf.dat")
DO j=1,Mesh%NFaces
 WRITE(Unit,*)Mesh%Faces(j)%FaceNormal(1)
END DO
CLOSE(Unit)

OPEN(Unit,FILE="data/nyf.dat")
DO j=1,Mesh%NFaces
 WRITE(Unit,*)Mesh%Faces(j)%FaceNormal(2)
END DO
CLOSE(Unit)

END SUBROUTINE



SUBROUTINE DAT_CellCentroids( Mesh )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: i
INTEGER :: Unit

Unit = NewUnit()

OPEN(Unit,FILE="data/xc.dat")
DO i=1,Mesh%NCells
 WRITE(Unit,*)Mesh%Cells(i)%CellCentroid(1)
END DO
CLOSE(Unit)

OPEN(Unit,FILE="data/yc.dat")
DO i=1,Mesh%NCells
 WRITE(Unit,*)Mesh%Cells(i)%CellCentroid(2)
END DO
CLOSE(Unit)

END SUBROUTINE


PURE FUNCTION Vert( Mesh , k )
!!#### PURPOSE
!! Return a single vertex.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: k

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Vert(1:Mesh%NDim)

!!--begin--
IF( k<=Mesh%NVerts .AND. k>0 )THEN
 Vert = Mesh%Verts(:,k)
ELSE
 Vert = Error(Vert)
END IF

!!--end--
END FUNCTION


PURE FUNCTION Verts( Mesh , k_ )
!!#### PURPOSE
!! Return a list of verts from a list of vertex
!! indices.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: k_(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Verts(1:Mesh%NDim,1:SIZE(k_))

!!#### LOCAL VARIABLES
INTEGER :: n

!!--begin--
DO n=1,SIZE(k_)
 Verts(:,n) = Vert(Mesh,k_(n))
END DO
!!--end--
END FUNCTION


FUNCTION IsPs_( FaceShape )
INTEGER,INTENT(IN) :: FaceShape
LOGICAL :: IsPs_
IsPs_ = FaceShape==Ps_
END FUNCTION

FUNCTION IsJx_( CellShape )
INTEGER,INTENT(IN) :: CellShape
LOGICAL :: IsJx_
IsJx_ = CellShape==0
END FUNCTION



FUNCTION IsQr_( Shape )
INTEGER,INTENT(IN) :: Shape
LOGICAL :: IsQr_
IsQr_ = ANY(Shape==(/Qr_,Qs_/))
END FUNCTION

FUNCTION IsQs_( Shape )
INTEGER,INTENT(IN) :: Shape
LOGICAL :: IsQs_
IsQs_ = Shape==Qs_
END FUNCTION

FUNCTION IsQg_( Shape )
INTEGER,INTENT(IN) :: Shape
LOGICAL :: IsQg_
IsQg_ = ANY(Shape==(/Qr_,Qs_,Qg_,Tr_/))
END FUNCTION

FUNCTION IsPg_( Shape )
INTEGER,INTENT(IN) :: Shape
LOGICAL :: IsPg_
IsPg_ = ANY(Shape==(/Qr_,Qs_,Qg_,Tr_,Pg_/))
END FUNCTION

FUNCTION IsTr_( Shape )
INTEGER,INTENT(IN) :: Shape
LOGICAL :: IsTr_
IsTr_ = Shape==Tr_
END FUNCTION



SUBROUTINE CREATE_Mesh( Mesh , NDim , MeshType , &
  n , delta , origin , permutations )
!!#### PURPOSE
!! Creates the Mesh according to defaults or present arguments.

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh itself (/Mesh/)
TYPE(TYPE_Mesh)    ,INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * the number of spatial dimensions of the Mesh (/NDim/)
INTEGER           ,INTENT(IN)    :: NDim

!!#### OPTIONAL INPUT
!! * the Mesh type (/MeshType/)
!! * the number of cells in each direction (/n/)
!! * some set of parameters to define the Mesh (/delta/)
!! * the origin of the Mesh, defaults to (0,0,0) (/origin/)
!! * array of position permutations for the structured Mesh (/permutations/)
INTEGER      ,OPTIONAL,INTENT(IN) :: MeshType
INTEGER      ,OPTIONAL,INTENT(IN) :: n(1:NDim)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: delta(:,:)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: origin(:)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: permutations(:,:,:,:)


!!#### LOCAL VARIABLES
INTEGER :: MeshType_

!!--begin--
!get optional Mesh specs
MeshType_ = DEFAULT(MSH_Unstructured,MeshType)

!1. Allocate the Mesh.
CALL ALLOCATE_Mesh( Mesh , NDim , MeshType_ )

!2. Mesh setup proceeds based on MeshType
!select setup routine based on Mesh shape
SELECT CASE( MeshType_ )
 CASE(MSH_Uniform     ) ; CALL SETUP_Uniform     ( Mesh , n , delta , origin )
 CASE(MSH_Regular     ) ; CALL SETUP_Regular     ( Mesh , n , delta , origin )
 CASE(MSH_Structured  ) ; CALL SETUP_Structured  ( Mesh , n , delta , origin , permutations )
 CASE DEFAULT           ; CALL SETUP_Unstructured( Mesh )
END SELECT

!!--end--
END SUBROUTINE



SUBROUTINE SETUP_Unstructured( Mesh )
!!#### PURPOSE
!! Setup the Mesh for MSH_Unstructured Mesh input.

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh object (/Mesh/)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!--begin--
!initialize indices
Mesh%k      = 0
Mesh%Nverts = 0

Mesh%j      = 0
Mesh%Nfaces = 0

Mesh%i      = 0
Mesh%Ncells = 0

!!--end--
ENDSUBROUTINE



SUBROUTINE SETUP_Uniform( Mesh , n , delta , origin )
!!#### PURPOSE
!! Sets up a uniform Mesh.
!
!! @---@---@---@---@---@---@
!! |   |   |   |   |   |   |
!! |   |   |   |   |   |   |
!! @---@---@---@---@---@---@
!! |   |   |   |   |   |   |
!! |   |   |   |   |   |   |
!! @---@---@---@---@---@---@
!

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh object (/Mesh/)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
!! * the number of cells in each dimension (/n/)
!! * the cell widths in each dimension (/delta/)
!! * the origin of the Mesh, defaults to (0,0,0) (/origin/)
INTEGER               ,INTENT(IN) :: n     (1:Mesh%NDim)
REAL(KIND_MSH)         ,INTENT(IN) :: delta (1:Mesh%NDim,1)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: origin(1:Mesh%NDim)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: x,y,z
REAL(KIND_MSH) :: deltax,deltay,deltaz
INTEGER     :: i,j,k,l,m

!!--begin--
SELECT CASE(Mesh%NDim)
 !1-dimensional
 CASE(1)
  deltax = delta(1,1)/REAL(n(1),KIND_MSH)
  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF
  m = 0
  DO i=1,n(1)+1
   m = m + 1
   k = ADD_vert( Mesh , (/x/) , "v"//TRIM(STR(m))//"uni" )
   x = x + deltax
  END DO

 !2-dimensional
 CASE(2)
  deltax = delta(1,1)/REAL(n(1),KIND_MSH)
  deltay = delta(2,1)/REAL(n(2),KIND_MSH)
  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF

  m = 0
  DO i=1,n(1)+1
   IF( PRESENT(origin) )THEN
    y = origin(2)
   ELSE
    y = REAL(0,KIND_MSH)
   ENDIF

   DO j=1,n(2)+1
    m = m + 1
    k = ADD_vert( Mesh , (/x,y/) , "v"//TRIM(STR(m))//"uni" )
    y = y + deltay
   END DO
  x = x + deltax
  END DO

 !3-dimensional
 CASE(3)
  deltax = delta(1,1)/REAL(n(1),KIND_MSH)
  deltay = delta(2,1)/REAL(n(2),KIND_MSH)
  deltaz = delta(3,1)/REAL(n(3),KIND_MSH)
  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF

  m = 0
  DO i=1,n(1)+1
   IF( PRESENT(origin) )THEN
    y = origin(2)
   ELSE
    y = REAL(0,KIND_MSH)
   ENDIF
   DO j=1,n(2)+1
    IF( PRESENT(origin) )THEN
     z = origin(3)
    ELSE
     z = REAL(0,KIND_MSH)
    ENDIF

    DO k=1,n(3)+1
     m = m + 1
     l = ADD_vert( Mesh , (/x,y,z/) , "v"//TRIM(STR(m))//"uni" )
     z = z + deltaz
    END DO
    y = y + deltay
   END DO
   x = x + deltax
  END DO

END SELECT

!!--end--
END SUBROUTINE




SUBROUTINE SETUP_Regular( Mesh , n , delta , origin )
!!#### PURPOSE
!! Sets up a regular Mesh.
!
!! @---@-----@----@---@---@--------@
!! |   |     |    |   |   |        |
!! |   |     |    |   |   |        |
!! @---@-----@----@---@---@--------@
!! |   |     |    |   |   |        |
!! |   |     |    |   |   |        |
!! |   |     |    |   |   |        |
!! @---@-----@----@---@---@--------@
!! |   |     |    |   |   |        |
!! @---@-----@----@---@---@--------@
!

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh itself (/Mesh/)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
!! * the cell widths in each dimension (/delta/)
!! * the number of cells in each dimension (/n/)
!! * the origin of the Mesh, defaults to (0,0,0) (/origin/)
INTEGER             ,INTENT(IN) :: n     (1:Mesh%NDim)
REAL(KIND_MSH)         ,INTENT(IN) :: delta (1:Mesh%NDim,1:MAXVAL(n)+1)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: origin(1:Mesh%NDim)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: x,y,z
INTEGER   :: i,j,k,l,m
!!--begin--
SELECT CASE(Mesh%NDim)
 !1-dimensional
 CASE(1)

  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF

  m = 0
  DO i=1,n(1)+1
   m = m + 1
   k = ADD_vert( Mesh , (/x/) , "v"//TRIM(STR(m))//"reg" )
   x = x + delta(1,i)
  END DO

 !2-dimensional
 CASE(2)
  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF

  m = 0
  DO i=1,n(1)+1
   IF( PRESENT(origin) )THEN
    y = origin(2)
   ELSE
    y = REAL(0,KIND_MSH)
   ENDIF

   DO j=1,n(2)+1
    m = m + 1
    k = ADD_vert( Mesh , (/x,y/) , "v"//TRIM(STR(m))//"reg" )
    y = y + delta(2,j)
   END DO
   x = x + delta(1,i)
  END DO

 !3-dimensional
 CASE(3)
  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF

  m = 0
  DO i=1,n(1)+1
   IF( PRESENT(origin) )THEN
    y = origin(2)
   ELSE
    y = REAL(0,KIND_MSH)
   ENDIF

   DO j=1,n(2)+1
    IF( PRESENT(origin) )THEN
     z = origin(3)
    ELSE
     z = REAL(0,KIND_MSH)
    ENDIF

    DO k=1,n(3)+1
     m = m + 1
     l = ADD_vert( Mesh , (/x,y,z/) , "v"//TRIM(STR(m))//"reg" )
     z = z + delta(3,k)
    END DO
    y = y + delta(2,j)
   END DO
   x = x + delta(1,i)
  END DO
END SELECT

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<CRACK_Cell>>
SUBROUTINE CRACK_Cell( Mesh , CellSet,  VertTol )

!!#### PURPOSE
!! Crack a cell into pieces by connecting face centroids
!! to cell centroids [hack: 2D only].

USE SUB_Sort_quick,ONLY: Sort=>Sort_quick                            !!((03-A-SUB_Sort_quick.f90))
USE FUN_Loop

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,POINTER :: CellSet(:)

!!#### OPTIONAL INPUT
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: VertTol
!!#### OPTIONAL OUTPUT
!INTEGER,POINTER,OPTIONAL :: NewCellMap(:,:)
!INTEGER,POINTER,OPTIONAL :: NewFaceMap(:,:)
!NTEGER,POINTER,OPTIONAL :: NewVerts(:,:) !new verts created in the splitting of 
!usage:
!map values of old cells to values of new cells
! DO is=1,SIZE(NewCellMap,2)
!     iold=CellSet(is)
!     DO it=1,SIZE(NewCellMap,1)
!         inew=NewCellMap(it,is)
!         IF( inew==0 )EXIT
!         ArrayNew(inew)=ArrayOld(iold)
!     END DO
! END DO

!first index is the original cell index in CellSet
!second index is the new cells that replace the old

!!#### LOCAL VARIABLES
INTEGER,ALLOCATABLE :: CellSet_(:)
INTEGER :: Ns
REAL(KIND_MSH) :: Ps_13(2,2,SIZE(CellSet)),Ps_24(2,2,SIZE(CellSet))
REAL(KIND_MSH) :: Pn_13(3,SIZE(CellSet)),Pn_24(3,SIZE(CellSet)),P(2)
INTEGER :: i_,is,i,j1,j2,j3,j4,ks,n,Nj,j_,num,ja,jb,ka,kb,kc,j_next,j_prev
LOGICAL :: Split
INTEGER,POINTER :: CellSetOut2(:),FaceSetOut2(:),VertSetOut2(:)
INTEGER,POINTER :: CellSetOut1(:),FaceSetOut1(:),VertSetOut1(:)
INTEGER,ALLOCATABLE :: SameSurface(:)
!!--begin--

!split first way
IF( .NOT.ASSOCIATED(CellSet) )RETURN

Ns = SIZE(CellSet)
ALLOCATE( CellSet_(Ns) )
CellSet_ = CellSet
CALL Sort(CellSet_)

DO is=1,Ns

    i = CellSet_(is)
    !#DEBUG_CRACK_CELL WRITE(*,*)'getting split planes for cell is=',is,' i=',i

    Nj=NUM_Faces(Mesh%Cells(i))
    ALLOCATE( SameSurface(1:Nj) )

    !assuming 4 faces
    j1=0; j2=0; j3=0; j4=0;
    j_=0
    num=0
    DO WHILE(num<Nj)
        j_ = Loop(j_,Nj,num)
        !#DEBUG_CRACK_CELL WRITE(*,*)'  num=',num
        SameSurface=FLAG_SameSurface(Mesh,i,j_,Allow_Essential=.TRUE.)
        ja=SameSurface(j_)
        
        !#DEBUG_CRACK_CELL WRITE(*,*)'  SameSurface=',SameSurface
        IF( COUNT(SameSurface/=0)==2 )THEN    
            !check previous and next for the surface that matches this one
            IF( j_==Nj )THEN            
                j_next=1
            ELSE
                j_next=j_+1
            END IF
            jb=SameSurface(j_next)
            IF( jb==0 )CYCLE
            !#DEBUG_CRACK_CELL WRITE(*,*)'     Hanging Node Face i=',i,' ja=',ja,' jb=',jb
        ELSE IF( COUNT(SameSurface/=0)==1 )THEN
            jb=0
            !#DEBUG_CRACK_CELL WRITE(*,*)'    Normal Face       i=',i,' j_=',j_
        ELSE
            WRITE(*,*)'    FATAL ERROR: Cannot handle face split into 3 pieces'
            STOP
        END IF

        IF( j1==0 )THEN
            j1 = ABS(ja)
            IF( jb==0 )THEN
                Ps_13(:,1,is) = FaceCentroid(Mesh,j1)
            ELSE
                Ps_13(:,1,is) = Vert(Mesh,LastVert(Mesh,ja))
            END IF
        ELSE IF( j2==0 )THEN
            j2 = ABS(ja)
            IF( jb==0 )THEN
                Ps_24(:,1,is) = FaceCentroid(Mesh,j2)
            ELSE
                Ps_24(:,1,is) = Vert(Mesh,LastVert(Mesh,ja))
            END IF
        ELSE IF ( j3==0 )THEN
            j3 = ABS(ja)        
            IF( jb==0 )THEN
                Ps_13(:,2,is) = FaceCentroid(Mesh,j3)
            ELSE
                Ps_13(:,2,is) = Vert(Mesh,LastVert(Mesh,ja))
            END IF
        ELSE IF( j4==0 )THEN
            j4 = ABS(ja)
            IF( jb==0 )THEN
                Ps_24(:,2,is) = FaceCentroid(Mesh,j4)
            ELSE
                Ps_24(:,2,is) = Vert(Mesh,LastVert(Mesh,ja))
            END IF
        ELSE
            EXIT
        END IF 
    END DO

    DEALLOCATE(SameSurface)

    Ps_13(:,:,is) = xyShrink_Ps( Ps_13(:,:,is) , 0.5_KIND_MSH )
    Ps_24(:,:,is) = xyShrink_Ps( Ps_24(:,:,is) , 0.5_KIND_MSH )

    Pn_13(:,is) = xyPLANE_P2( Ps_13(:,:,is) )
    Pn_24(:,is) = xyPLANE_P2( Ps_24(:,:,is) )

END DO

DO is=1,Ns
    i = CellSet_(is)
    !#DEBUG_CRACK_CELL WRITE(*,*)'testing first split is=',is,' i=',i

    !first split
    NULLIFY(CellSetOut1,FaceSetOut1,VertSetOut1)
    !#DEBUG_CRACK_CELL WRITE(*,*)'before split 1'
    !#DEBUG_CRACK_CELL CALL PRINT_Mesh(Mesh)
    Split = SPLIT_Cell( Mesh , i , Pn_13(:,is) , CellSetOut1 , FaceSetOut1, VertSetOut1, VertTol=VertTol )
    !#DEBUG_CRACK_CELL WRITE(*,*)'after split 1'
    !#DEBUG_CRACK_CELL CALL PRINT_Mesh(Mesh)

    !second split
    IF( Split )THEN
        !#DEBUG_CRACK_CELL WRITE(*,*)'testing second split cellsetout1=',cellsetout1,' original i=',i
        !#DEBUG_CRACK_CELL WRITE(*,*)'point1 (x,y)=',Ps_24(1:2,1,is)
        !#DEBUG_CRACK_CELL WRITE(*,*)'point2 (x,y)=',Ps_24(1:2,2,is)
        DO i_=1,SIZE(CellSetOut1)
            NULLIFY(CellSetOut2,FaceSetOut2,VertSetOut2)
            Split=SPLIT_Cell( Mesh , CellSetOut1(i_) , Pn_24(:,is) , CellSetOut2, FaceSetOut2, VertSetOut2, VertTol=VertTol )
            !#DEBUG_CRACK_CELL WRITE(*,*)'after split 2 cell=',CellSetOut1(i_)
            !#DEBUG_CRACK_CELL CALL PRINT_Mesh(Mesh)
            IF( ASSOCIATED(CellSetOut2) )THEN
                DEALLOCATE(CellSetOut2)
            END IF
            IF( ASSOCIATED(FaceSetOut2) )THEN
                DEALLOCATE(FaceSetOut2)
            END IF
            IF( ASSOCIATED(VertSetOut2) )THEN
                DEALLOCATE(VertSetOut2)
            END IF
        END DO
    END IF
    
    IF( ASSOCIATED(CellSetOut1) )THEN
        DEALLOCATE(CellSetOut1)
    END IF
    IF( ASSOCIATED(FaceSetOut1) )THEN
        DEALLOCATE(FaceSetOut1)
    END IF
    IF( ASSOCIATED(VertSetOut1) )THEN
        DEALLOCATE(VertSetOut1)
    END IF
    
END DO

DEALLOCATE( CellSet_ )
!#DEBUG_CRACK_CELL WRITE(*,*)'leaving CRACK_Cell'

!!--end--
END SUBROUTINE



FUNCTION xyShrink_Ps( Ps , f ) RESULT(xyPs)
REAL(KIND_MSH),INTENT(IN) :: Ps(2,2)
REAL(KIND_MSH) :: xyPs(2,2)
REAL(KIND_MSH),INTENT(IN) :: f
REAL(KIND_MSH) :: c(2),a(2),b(2)

!!--begin--

c = 0.5_KIND_MSH*(Ps(:,1)+Ps(:,2))
a = f*(Ps(:,1)-c)
b = f*(Ps(:,2)-c)
xyPs(:,1) = c+a
xyPs(:,2) = c+b

!--end--
END FUNCTION

FUNCTION SPLIT_Face( Mesh , j , Pn , j_ , k_ , VertTol ) RESULT(Split)
!!#### PURPOSE
!! Split a face <j> into two faces <j_> using a plane.  Always overwrites
!! face j with one of the splits.

!!#### DEPENDENCIES
USE FUN_EQUILOC                                                      !!((03-A-FUN_EQUILOC.f90))

!!#### NOMENCLATURE
!
!!           |
!!       Pn +--->
!!           |
!! +---------+--------+
!! 1         3        2
!

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh object (/Mesh/)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * the face to split <j>
!! * the direction which defines the plane (perpendicular to the plane) <u>
INTEGER       ,INTENT(IN) :: j
REAL(KIND_MSH),INTENT(IN) :: Pn(1:Mesh%NDim+1)

!!#### REQUIRED OUTPUT
!! * whether the face was split or not <Split>
!! * the new faces created <j_>
!! * the new verts created <k_>
LOGICAL         :: Split
INTEGER,POINTER :: j_(:)
INTEGER,POINTER :: k_(:)
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: VertTol

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitFace) :: Face
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S) :: FaceLabel,FaceLabel0
REAL(KIND_MSH) :: v3(1:Mesh%NDim)
INTEGER :: k3,k1,k2,j0,j1,j2,i0,i1,i,key,j3,j4,m,n,i_list(2)
INTEGER,ALLOCATABLE :: FaceList(:)
REAL(KIND_MSH) :: VertTol_

!!--begin--

!#DEBUG_SPLIT_FACE WRITE(*,*)'      SPLIT_Face: beginning for j=',j
!#DEBUG_SPLIT_FACE CALL PRINT_Mesh(Mesh)

VertTol_ = DEFAULT(SQRT(EPSILON(0._KIND_MSH)),VertTol)

!initialization
IF( ASSOCIATED(j_) )THEN
 DEALLOCATE(j_)
END IF
IF( ASSOCIATED(k_) )THEN
 DEALLOCATE(k_)
END IF

NULLIFY( j_,k_ )

!!1. determine face intersection point
Face  = ExplicitFace( Mesh , j )

!!if face is a subset of the plane then return
Split = .FALSE.
IF( INTERIOR_PnXf( Pn , Face ) )RETURN

Split = INTERSECT_XfPn( Face , Pn , P_intersect=v3 , IncludeEdges=.TRUE. , key=key )

!!2. Get out if no split.
IF( .NOT.Split )RETURN

!!3. Try to find the index of the vert
k3 = GET_Vert( Mesh , vert=v3 , tol=VertTol_ )

!!4. If the vert does not exist yet, then we do the splitting.
IF( k3==0 )THEN
    !#DEBUG_SPLIT_FACE WRITE(*,*)'      SPLIT_Face: k3==0 (doing splitting)'

    !a. subtract old face (after getting the verts)
    k1 = Mesh%Faces(j)%VertList(1)
    k2 = Mesh%Faces(j)%VertList(2)
    IF( TRIM(ADJUSTL(DEBUG)) .EQ. 'DEBUG' )WRITE(*,'(a,100(1x,i5))')'** k1,k2=',k1,k2
    !j0 = REMOVE_Face( Mesh , j , FaceLabel ) !no longer remove the face

    !#DEBUG_SPLIT_FACE WRITE(*,*)'      SPLIT_Face: before add faces for j=',j
    FaceLabel0=Mesh%FaceLabels(j)

    !b. add vert
    k3 = ADD_Vert( Mesh , v3 , "v"//TRIM(FaceLabel0))
    
    i_list=Mesh%FaceToCellLink(:,ABS(j))
    !#DEBUG_SPLIT_FACE WRITE(*,*)"      SPLIT_Face: i_list=",i_list
    IF( COUNT(i_list==0)>1 )THEN        
        CALL STOP("FaceToCellLink must be setup with UPDATE_FaceToCellLink")
    END IF
    
    !c. add two new faces
    FaceLabel = MakeSplitFaceLabel( FaceLabel0 )
    j1 = ADD_Face_Ps( Mesh , (/k1,k3/) , (/Straight_/) , TRIM(FaceLabel), overwrite=j )
    !#DEBUG_SPLIT_FACE WRITE(*,*)"      SPLIT_Face: returned from ADD_Face_Ps with j1=",j1
    CALL UPDATE_FaceToCellLink(Mesh,j1,i_list)
    
    FaceLabel = MakeSplitFaceLabel( FaceLabel0 )
    j2 = ADD_Face_Ps( Mesh , (/k3,k2/) , (/Straight_/) , TRIM(FaceLabel) )
    !#DEBUG_SPLIT_FACE WRITE(*,*)"      SPLIT_Face: returned from ADD_Face_Ps with j2=",j2
    CALL UPDATE_FaceToCellLink(Mesh,j2,i_list)
    
    !d. set the new vert pointer
    ALLOCATE( k_(1) )
    k_ = (/k3/)

    !e. set the new face pointer
    ALLOCATE( j_(1:2) )
    j_ = (/j1,j2/)

    !TODO: should really find a way to not iterate through all the cells in the mesh here!
    ![waw] FIXED
    DO n=1,2
        i=i_list(n)
        IF( i==0 )CYCLE
    
        !f. replace j with j1 and j2 in all cells
        !#DEBUG_SPLIT_FACE WRITE(*,'(1x,a,i5,a,i5,a,100(1x,i5))')'      SPLIT_Face: checking face list for i=',i,' j=',j,' face_list=',Mesh%Cells(i)%FaceList
        j3 = EQUILOC( ABS(Mesh%Cells(i)%FaceList) , ABS(j) )

        IF( j3==0 )CYCLE
        !#DEBUG_SPLIT_FACE WRITE(*,'(1x,a,100(1x,i5))')'      SPLIT_Face: i_list=',i_list

        !allocate the face list
        ALLOCATE( FaceList(1:SIZE(Mesh%Cells(i)%FaceList)+1) )

        !insert j_ indices at j3 by:
        !a. doing the first half
        DO j4=1 ,j3-1
            FaceList(j4) = ABS(Mesh%Cells(i)%FaceList(j4))
        END DO

        !b. inserting j_ at location j3 
        FaceList(j3:j3+1) = (/j1,j2/)

        !c. doing the last half
        DO j4=j3+2,SIZE(Mesh%Cells(i)%FaceList)+1
            FaceList(j4) = ABS(Mesh%Cells(i)%FaceList(j4-1))
        END DO
        !#DEBUG_SPLIT_FACE WRITE(*,'(a,i5,a,100(1x,i5))')'      SPLIT_Face: checking final face list for i=',i,' face_list=',FaceList

        !rewrite the cell with the new faces
        !#DEBUG_SPLIT_FACE WRITE(*,*)'      SPLIT_Face: mesh printing before ADD_Cell_Jx for cell i=',i
        !#DEBUG_SPLIT_FACE CALL PRINT_Mesh(Mesh)
        i0  =  ADD_Cell_Jx( Mesh , FaceList , TRIM(Mesh%CellLabels(i)) , overwrite=i )
        !#DEBUG_SPLIT_FACE WRITE(*,*)'      SPLIT_Face: mesh printing after ADD_Cell_Jx for cell i=',i
        !#DEBUG_SPLIT_FACE CALL PRINT_Mesh(Mesh)

        !deallocate the face list
        DEALLOCATE( FaceList )
    END DO

!! We need to determine the face that has already been split
ELSE
    !d. get the vert pointer
    !#DEBUG_SPLIT_FACE WRITE(*,*)'      SPLIT_Face: no split'
    ALLOCATE( k_(1) )
    k_(1) = k3

    !e. get the face pointer
    j1 = j
    DO j0=1,NUM_Faces(Mesh)
        IF( j0==j1 )CYCLE
        IF( ANY(Mesh%Faces(j0)%VertList==k_(1)) )THEN
            j2 = j0
            EXIT
        END IF
    END DO

    ALLOCATE( j_(1:2) )
    j_ = (/j1,j2/)

END IF

!#DEBUG_SPLIT_FACE CALL TEST_FaceToCellLink(Mesh)
        
!!--end--
END FUNCTION


!!### SUBROUTINE <<TEST_FaceToCellLink>>
SUBROUTINE TEST_FaceToCellLink( Mesh )

!!#### PURPOSE
!! Test the FaceToCellLink of the Mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
INTEGER :: i,m,j_
INTEGER,ALLOCATABLE :: TEST(:,:)

!!--begin--

    ALLOCATE( TEST(SIZE(Mesh%FaceToCellLink,1),SIZE(Mesh%FaceToCellLink,2)) )
    TEST=0

    DO i=1,Mesh%NCells
        DO m=1,SIZE(Mesh%Cells(i)%FaceList)
            j_=Mesh%Cells(i)%FaceList(m)
            IF( j_>0 )THEN
                TEST(1,ABS(j_))=i
            ELSE
                TEST(2,ABS(j_))=i
            END IF
        END DO
    END DO

    DO j_=1,Mesh%NFaces
        WRITE(*,*)'TEST_FaceToCellLink: j=',j_,' correct i1,i2= ',TEST(:,j_)
        IF( (TEST(1,j_)/=Mesh%FaceToCellLink(1,j_) .AND. &
             TEST(1,j_)/=Mesh%FaceToCellLink(2,j_)) .OR.  & 
            (TEST(2,j_)/=Mesh%FaceToCellLink(1,j_) .AND. &
             TEST(2,j_)/=Mesh%FaceToCellLink(2,j_)) )THEN
            WRITE(*,*)'TEST_FaceToCellLink: j=',j_,' failed i1,i2= ',Mesh%FaceToCellLink(:,j_)
            STOP
        END IF
    END DO
    DEALLOCATE( TEST )


!!--end--
END SUBROUTINE TEST_FaceToCellLink


FUNCTION SPLIT_Cell( Mesh , i , Pn , i_out , j_out , k_out , map_cells , &
  P_interior , Ps_interior , Pg_region , VertTol ) RESULT(Split)
!!#### PURPOSE
!! Split a cell <i> into two cells <i_> using a plane.
!! Creates at most 2 faces and 2 cells.
!
!!#### NOMENCLATURE
!
!! 3          5        4
!! +----------+--------+
!! |          |        |
!! |      Pn +--->     |
!! |          |        |
!! +----------+--------+
!! 1          6        2
!
!!#### DEPENDENCIES
USE FUN_xySDIST                                                      !!((03-A-FUN_xySDIST.f90))
USE FUN_Loop

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh object (/Mesh/)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * the cell to divide <i>
!! * the direction which defines the plane (perpendicular to the plane) <u>
INTEGER      ,INTENT(IN) :: i
REAL(KIND_MSH),INTENT(IN) :: Pn(1:Mesh%NDim+1)

!!#### REQUIRED OUTPUT
!! * the list of new cells created <i_out>
!! * the list of new faces created <j_out>
!! * the list of new verts created <k_out>
!! * whether the cell could be split <Split>
INTEGER,POINTER :: i_out(:)
INTEGER,POINTER :: j_out(:)
INTEGER,POINTER :: k_out(:)
LOGICAL         :: Split

!!#### OPTIONAL INPUT/OUTPUT
INTEGER,POINTER,OPTIONAL,INTENT(INOUT) :: map_cells(:)
INTEGER,POINTER :: map_verts(:)
INTEGER,POINTER :: map_faces(:,:)

!!#### OPTIONAL OUTPUT
REAL(KIND_MSH),OPTIONAL   :: P_interior(1:Mesh%NDim)
REAL(KIND_MSH),OPTIONAL   :: Ps_interior(:,:)
REAL(KIND_MSH),OPTIONAL   :: Pg_region(:,:),VertTol

!!#### LOCAL VARIABLES
REAL(KIND_MSH)            :: s,FC(2)
REAL(KIND_MSH)            :: P_intersect(1:Mesh%NDim,1:SIZE(Mesh%Cells(i)%facelist))
REAL(KIND_MSH)            :: P_centroid(1:Mesh%NDim),SAREA
TYPE(TYPE_ExplicitCell)   :: Xc
INTEGER,POINTER           :: k_list(:),j_list(:),new_faces(:)
INTEGER,ALLOCATABLE       :: LeftCell(:),RightCell(:),old_faces(:)
INTEGER                   :: j_,j_new,j_old,j__,jl,jr,Nj_new,Nj_old,i1,i2,m,n
CHARACTER(LEN=LEN_MESH_LABEL) :: Label,Label0
LOGICAL                  :: TargetCondition
INTEGER                  :: j_center_face,k_center_face(2)
LOGICAL,ALLOCATABLE      :: Splits(:)

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="SPLIT_Cell"

!!--begin--

!initialize
NULLIFY(k_list,j_list,i_out,j_out,k_out,new_faces)

Split = .FALSE.

!do initial easy-kick out criteria
Xc = ExplicitCell( Mesh , i )
IF( PRESENT(P_interior) )THEN
    IF( .NOT.Interior_XcP( Xc , P_interior , Mesh%Cells(i)%CellCentroid) )RETURN
END IF
IF( PRESENT(Ps_interior) )THEN
    IF( .NOT.Interior_XcPs( Xc , Ps_interior , Mesh%Cells(i)%CellCentroid) )RETURN
END IF
IF( PRESENT(Pg_region) )THEN
    IF( NUM_Dimensions(Mesh)==2 )THEN
        SAREA      = xySAREA_Pg(  SIZE(Pg_region,2) , Pg_region )
        P_centroid = xyCENTROID_Pg( SIZE(Pg_region,2) , Pg_region , SAREA )

        IF( .NOT.xyInterior_PgP( SIZE(Pg_region,2) , &
            Pg_region , Mesh%Cells(i)%CellCentroid , P_centroid ) )RETURN
    ELSE
        CALL Stop(s="The number of dimensions /=2 is not supported.")
    END IF
END IF
CALL CLEAN( Xc )

!get old faces
Nj_old=NUM_Faces(Mesh%Cells(i))

ALLOCATE(old_faces(Nj_old))
old_faces=Mesh%Cells(i)%FaceList

ALLOCATE(Splits(Nj_old)); Splits=.FALSE.

ALLOCATE(map_faces(2,Nj_old)); map_faces=0;
ALLOCATE(map_verts(Nj_old))  ; map_verts=0;
k_center_face=0


!iterate on original faces
DO j_=1,Nj_old
    j_old=old_faces(j_)
    !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: j_old=',j_old

    !find the next original face in the (possibly changing) FaceList
    Nj_new=NUM_Faces(Mesh%Cells(i))
    !#DEBUG_SPLIT_CELL WRITE(*,'(1x,a,100(3x,i5))')'   SPLIT_Cell: faces=',Mesh%Cells(i)%FaceList
    DO j__=1,Nj_new
        !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: testing j__=',j__,'for j_old=',j_old
        IF( ABS(Mesh%Cells(i)%FaceList(j__))==ABS(j_old) )EXIT
    END DO
    !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: found j__=',j__
    IF( j__>Nj_new )THEN
        CALL STOP("did not find the old face...where did it go?")
    END IF

    !attempt to split the face
    !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: before entering SPLIT_Face for face j_old=',j_old
    Splits(j_) = SPLIT_Face( Mesh , ABS(j_old) , Pn , j_list , k_list , VertTol=VertTol )
    !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: after exiting SPLIT_Face with split=',splits(j_)

    !set map faces and map verts
    IF( Splits(j_) )THEN
        IF( SIZE(k_list)/=1 )THEN
            CALL STOP("only 1 vert should be found in the split")
        END IF
        IF( SIZE(j_list)/=2 )THEN
            CALL STOP("only 2 faces should be found in the split")
        END IF
        !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: before map_faces with j_list=',j_list
        map_faces(1:2,j_)=j_list(1:2)
        !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: before map_verts with k_list=',k_list
        map_verts(j_)    =k_list(1)
        if( k_center_face(1)==0 )THEN
            k_center_face(1)=k_list(1)
        else if( k_center_face(1)/=k_list(1) )THEN
            k_center_face(2)=k_list(1)
        end if
        CALL CLEARn(j_list)
        CALL CLEARn(k_list)
    ELSE
        map_faces(1,j_)=ABS(j_old)
    END IF

END DO

Split = ANY(Splits)
!#DEBUG_SPLIT_CELL WRITE(*,'(1x,a,100(g9.1))')'   SPLIT_Cell: splits=',Splits

IF( .NOT. Split )THEN
    RETURN
END IF

!#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: there was a split for cell i=',i

!Add the new face which splits the cell.
CALL CLEAR(Label)
Label = MakeSplitFaceLabel( "f"//TRIM(Mesh%CellLabels(i)) )
!#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: map_verts=',map_verts
!#DEBUG_SPLIT_CELL WRITE(*,'(1x,a,2i5,a,a20)')'   SPLIT_Cell: face added with k_center_face=',k_center_face,'label=',Label

!was j56
j_center_face = ADD_Face_Ps( Mesh , k_center_face , (/Straight_/) , TRIM(Label) )
!#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: after add_face_ps'

!5. Determine which faces are on which cells

!get the new faces
new_faces => ptr_FaceList(Mesh,i)
Nj_new=NUM_Faces(Mesh%Cells(i))
!#DEBUG_SPLIT_CELL WRITE(*,'(1x,a,100(3x,i5))')'   SPLIT_Cell: after 2, new_faces=',new_faces

!add the new splitting face as the first entry
jl = 1
ALLOCATE( LeftCell (Nj_new) )
LeftCell (jl) = j_center_face

jr = 1
ALLOCATE( RightCell(Nj_new) )
RightCell(jr) = j_center_face

!use the face centroids to determine which side
DO j_ = 1,SIZE(new_faces)
    j_new=new_faces(j_)
    !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: j_new=',j_new
    FC=FaceCentroid(Mesh,ABS(j_new))
    s = xySDIST_PnP( Pn , FC )

    IF( s<0._KIND_MSH )THEN
        jl = jl + 1
        LeftCell(jl) = ABS(j_new)
    ELSE
        jr = jr + 1
        RightCell(jr) = ABS(j_new)
    END IF

END DO
NULLIFY(new_faces)
!#DEBUG_SPLIT_CELL WRITE(*,'(1x,a,100(3x,i5))')'   SPLIT_Cell: after new faces, LeftCell=',LeftCell(1:jl)
!#DEBUG_SPLIT_CELL WRITE(*,'(1x,a,100(3x,i5))')'                               RightCell=',RightCell(1:jr)

!7. Remove the old cell and get its label
!i0 = Remove_Cell( Mesh , i , Label )
Label0=Mesh%CellLabels(i)

!!8.a. add left cell (overwriting original cell)
Label = MakeSplitCellLabel( Label0 )
i1 = ADD_Cell_Jx( Mesh , LeftCell(1:jl)  , TRIM(Label), overwrite=i )
!#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: after add cell 1'

!for each face, we possibly renew the FaceToCellLink (start at 2 because we know 1,
!the center face (between two created cells) has already been updated.
!HACK update
DO j__=2,jl    
    j_=LeftCell(j__)
    !for each of two cells
    DO n=1,2
        IF( Mesh%FaceToCellLink(n,j_)==i )THEN
            Mesh%FaceToCellLink(n,j_)=i1
        END IF
    END DO
END DO

!!8.b. add right cell
Label = MakeSplitCellLabel( Label0 )
i2 = ADD_Cell_Jx( Mesh , RightCell(1:jr) , TRIM(Label) )
!#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: after add cell 2'
!HACK update
!for each face, we possibly renew the FaceToCellLink (start at 2 because we know 1,
!the center face (between two created cells) has already been updated.
DO j__=2,jr   
    j_=RightCell(j__)
    !for each of two cells
    DO n=1,2
        IF( Mesh%FaceToCellLink(n,j_)==i )THEN
            Mesh%FaceToCellLink(n,j_)=i2
        END IF
    END DO
END DO

!final facetocelllink update
CALL UPDATE_FaceToCellLink(Mesh,j_center_face,(/i1,i2/))

DEALLOCATE( LeftCell , RightCell )

!WRITE(8,*)"after all cells added..."
!CALL Print_Mesh(Mesh,Unit=8)

!9. return new info
ALLOCATE( j_out(1:4) , k_out(1:2) , i_out(1:2) )
!! return new cells
i_out    = (/i1,i2/)
!! return new verts
k_out    = k_center_face !(/k5,k6/)
!! return new faces
j_=0
DO n=1,SIZE(map_faces,2)
    !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: map_faces(',n,')=',map_faces(:,n)
    DO m=1,SIZE(map_faces,1)
        IF( map_faces(m,n)/=0 )THEN
            !check for duplicates
            IF( j_>0 )THEN
                IF( ANY(j_out(1:j_-1)==ABS(map_faces(m,n))) )CYCLE
            END IF
            j_=j_+1
            !#DEBUG_SPLIT_CELL WRITE(*,*)'   SPLIT_Cell: jout=',ABS(map_faces(m,n))           
            IF( j_>SIZE(j_out) )THEN
                CALL REALLOCATE(j_out,1)
            END IF
            j_out(j_)=ABS(map_faces(m,n))
        END IF
    END DO
END DO
!(/j16,j62,j35,j54/)

IF( PRESENT(map_cells) )THEN
 !10. return the mapping info
 !! reallocate if needed
 IF( Mesh%NCells>SIZE(map_cells) )THEN
  CALL REALLOCATE( map_cells , Mesh%NCells-SIZE(map_cells) )
 END IF
 map_cells(Mesh%NCells)=i2
 !map_cells(:i1) = map_cells(:i1)
 !map_cells(i2:) = map_cells(i2-1:SIZE(map_cells)-1)
END IF

!tapack v2.23 [waw] Nov 2011
CALL UPDATE_Mesh(Mesh,ilist=(/i1,i2/))

!WRITE(8,*)'end of split_cell run with i1=',i1,' i2=',i2
!CALL PRINT_Mesh(Mesh,unit=8)

!!--end--
END FUNCTION


FUNCTION MakeSplitFaceLabel( FaceLabel ) RESULT(NewFaceLabel)
!!#### PURPOSE
!! Transform a <FaceLabel> into split face label <FaceLabel//"_SF">.

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: FaceLabel

!!#### REQUIRED OUTPUT
CHARACTER(LEN=LEN(FaceLabel),KIND=KIND_S) :: NewFaceLabel

!!#### LOCAL VARIABLES
INTEGER,SAVE :: n=0
INTEGER      :: k
!!--begin--
CALL CLEAR(NewFaceLabel)
n = n + 1
k=INDEX(FaceLabel,"_SF",BACK=.TRUE.)
IF( k/=0 )THEN
 NewFaceLabel = TRIM(FaceLabel(1:k+2))//TRIM(STR(n))
ELSE
 NewFaceLabel = TRIM(FaceLabel)//"_SF"//TRIM(STR(n))
END IF

!!--end--
END FUNCTION



FUNCTION MakeSplitCellLabel( CellLabel ) RESULT(NewCellLabel)
!!#### PURPOSE
!! Transform a <CellLabel> into split cell label <CellLabel//"_SC">.

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: CellLabel


!!#### REQUIRED OUTPUT
CHARACTER(LEN=LEN(CellLabel),KIND=KIND_S) :: NewCellLabel

!!#### LOCAL VARIABLES
INTEGER,SAVE :: n=0
INTEGER      :: k
!!--begin--
CALL CLEAR(NewCellLabel)
n = n + 1
k=INDEX(CellLabel,"_SC",BACK=.TRUE.)
IF( k/=0 )THEN
 NewCellLabel = TRIM(CellLabel(1:k+2))//TRIM(STR(n))
ELSE
 NewCellLabel = TRIM(CellLabel)//"_SC"//TRIM(STR(n))
END IF

!!--end--
END FUNCTION



FUNCTION FLAG_SameSurface(Mesh,i,j_,Allow_Essential) RESULT(SameSurface)
!!#### PURPOSE
!! Determine the faces on a cell which effectively describe
!! the same surface and could therefore be combined.

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh object <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER :: SameSurface(1:NUM_Faces(Mesh%Cells(i)))

!!#### REQUIRED INPUT
!! * cell index <i>
!! * local face index in a cell <j_>
INTEGER,INTENT(IN) :: i
INTEGER,INTENT(IN) :: J_
LOGICAL,OPTIONAL,INTENT(IN) :: Allow_Essential

!!#### LOCAL VARIABLES
INTEGER :: j,jj_,jm
TYPE(TYPE_ExplicitFace) :: Xf_1,Xf_2
LOGICAL,PARAMETER :: Interactive=.FALSE.
CHARACTER(*),PARAMETER :: proc_="FLAG_SameSurface"
LOGICAL :: Is,Allow_Essential_

!!--begin--
IF( PRESENT(Allow_Essential) )THEN
    Allow_Essential_=Allow_Essential
ELSE
    Allow_Essential_=.FALSE.
END IF

!check the Mesh to make sure it meets the CellBased requirement
IF( .NOT. Allow_Essential_ )THEN
    INCLUDE "15-B-TBX_Mesh__IsCellBased.f90.sup"
END IF

!initialize
SameSurface = 0

!get the primary face and index
jm = Mesh%Cells(i)%FaceList(j_)
SameSurface(j_) = jm
Xf_1 = ExplicitFace( Mesh , jm )

!loop over other faces
DO jj_=1,SIZE(SameSurface)

 !do not compare primary to itself
 IF( jj_==j_ )CYCLE

 !get comparison index
 j = Mesh%Cells(i)%FaceList(jj_)

 !get comparison face
 Xf_2 = ExplicitFace( Mesh , j )

 !compare faces
 Is = IsSamePlane(Xf_1,Xf_2)
 SameSurface(jj_) = MERGE( j , 0 ,  Is )

 !deallocate
 CALL CLEAN( Xf_2 )

END DO

!deallocate
CALL CLEAN( Xf_1)

!!--end--
END FUNCTION



!!### SUBROUTINE <<CREATE_CompoundFaces>>
SUBROUTINE CREATE_CompoundFaces( Mesh , FaceMapping )
!!#### PURPOSE
!! Stage 2 of the procedure to ensure a Mesh is "Cell-Based".
!! Find any faces that can be combined into new faces and
!! merge them.

!!#### DETAILS
!! All cells <i> by taking into account the actual
!! form of the faces.  A general polygon may become a
!! triangle, rectangle, quadrilateral, etc.  Extra faces
!! are added such that each cell has it's own unique faces.


!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh object <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
!! * the mapping of cell-based faces to faces on the corresponding
!!   essential mesh
INTEGER,OPTIONAL,POINTER :: FaceMapping(:)

!!#### LOCAL VARIABLES
INTEGER :: i,j_,Nfaces
INTEGER,ALLOCATABLE :: SameSurface(:)
CHARACTER(*),PARAMETER :: proc_="CREATE_CompoundFaces"
LOGICAL,PARAMETER :: Interactive=.FALSE.
LOGICAL,PARAMETER :: Noisy=.FALSE.

!!--begin--
INCLUDE "15-B-TBX_Mesh__IsCellBased.f90.sup"

!loop over all cells
DO i=1,NUM_Cells(Mesh)
 IF( Noisy )write(*,*)"cell index i=",i

 j_ = 0
 DO


  !local number of faces
  NFaces = NUM_Faces(Mesh%Cells(i))

  !exit if we are at the last face
  IF( j_==NFaces )EXIT

  !allocate variable to current number of faces
  ALLOCATE( SameSurface( 1:NFaces ) )

  !increment local face index
  j_ = j_ + 1
  IF( Noisy )write(*,*)"local face index j_=",j_

  !determine the uniqueness of faces on a cell
  SameSurface = FLAG_SameSurface( Mesh , i , j_ )
  IF( Noisy )write(*,*)"SameSurface(j)=",SameSurface

  !using the same face data, merge the same faces and make a new
  !cell composed of the compound faces
  CALL CREATE_MasterFace(Mesh,i,j_,SameSurface,FaceMapping)

  !deallocate
  DEALLOCATE( SameSurface )

 END DO

END DO

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<CREATE_MasterFace>>
SUBROUTINE CREATE_MasterFace(Mesh,i,j_,SameSurface,FaceMapping)
!!#### PURPOSE
!! In cell <i>, make local face <j_> a master face that
!! is the union of faces <j=SameSurface(n)> for all <n> except
!! <n=j_>, which specifies the global face index of local
!! face <j>.


!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * the cell index where the face resides
!! * the master face (becomes index of the face described my merging <FaceList> and <j>)
!! * the list of sub-faces <FaceList>
INTEGER,INTENT(IN) :: i
INTEGER,INTENT(IN) :: j_
INTEGER,INTENT(IN) :: SameSurface(:)

!!#### OPTIONAL INPUT
!! * the mapping of cell-based faces to faces on the corresponding
!!   essential mesh
INTEGER,OPTIONAL,POINTER :: FaceMapping(:)

!!#### LOCAL VARIABLES
INTEGER :: j1_,j2_,jm,js,j
INTEGER,POINTER :: FL(:)
INTEGER,SAVE :: i_=0,count=0
LOGICAL :: IsSameSurface(1:SIZE(SameSurface))
CHARACTER(20) :: suffix
LOGICAL,PARAMETER :: Noisy=.FALSE.
!!--begin--
!get global face index
j = SameSurface(j_)


!quick return
IsSameSurface = SameSurface/=0
IsSameSurface(j_) = .FALSE.
IF( .NOT.ANY(IsSameSurface) )THEN
 RETURN
END IF


!add the merged face of the
NULLIFY(FL)
IF( Noisy )write(*,*)"entering MergeFace, FL=",Sentence(STRn(FL),delim=" ")
IF( Noisy )write(*,*)"IsSameSurface=",IsSameSurface
IF( Noisy )write(*,*)"i  =",i
IF( Noisy )write(*,*)"j_ =",j_
CALL MergeFace(Mesh,i,j_,SameSurface,FaceLabel=TRIM(Mesh%FaceLabels(j))//"m",NewFaceList=FL,&
  FaceMapping=FaceMapping)
IF( Noisy )write(*,*)"exiting  MergeFace, FL=",Sentence(STRn(FL),delim=" ")


!prepare the suffix
!a. reset the count
IF( i_/=i )count = 0
!b. form the suffix for the new cell by appending count
CALL CLEAR(suffix)
suffix = "m"//TRIM(STR(count))
!c. increment count
count = count + 1



!add the cell
i_ = ADD_Cell_Jx( Mesh , FL , Mesh%CellLabels(i)//TRIM(suffix) , overwrite=i )
DEALLOCATE( FL )

!make sure all cells receive a new classification
CALL CLASSIFY_Cell(Mesh,i)


!!--end--
END SUBROUTINE



SUBROUTINE MergeFace( Mesh , i , j_ , MergeFaceList , FaceLabel , NewFaceList , FaceMapping )
!!#### PURPOSE
!! Create a face by merging of the faces in the face-list,
!! returning what should be the new face list for cell <i>.

!!#### MODULES
USE FUN_Loop                                                         !!((05-B-FUN_Loop.f90))

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT)       :: Mesh

!!#### REQUIRED INPUT
INTEGER                    ,INTENT(IN) :: i
INTEGER                    ,INTENT(IN) :: j_
INTEGER                    ,INTENT(IN) :: MergeFaceList(:)
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: FaceLabel

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: NewFaceList(:)

!!#### OPTIONAL INPUT
!! * the mapping of cell-based faces to faces on the corresponding
!!   essential mesh
INTEGER,OPTIONAL,POINTER :: FaceMapping(:)

!!#### LOCAL VARIABLES
INTEGER :: l,l_,n,j,Nj,js_,Njs,js,Nl,Nl_
TYPE(TYPE_Edge),POINTER :: Edges(:)
INTEGER :: num,nk,k,k_,jm,jd
INTEGER,POINTER :: VertList(:)
INTEGER,POINTER :: SubFaces(:)
INTEGER,POINTER :: DeleteFaces(:)
CHARACTER(LEN_MESH_LABEL) :: trash
LOGICAL,PARAMETER :: Noisy_=.TRUE.

!!--begin--
SELECT CASE(Mesh%NDim)

 !1D
 CASE(1)
  WRITE(*,*)"No 1D support"
  STOP

 !2D
 CASE(2)

  !initialize stuff
  k_ = 0
  l_ = 0
  js_ = 0

  !initialize
  VertList => NULL()
  SubFaces => NULL()
  DeleteFaces => NULL()

  !1.* iterator: go through each face in MergeFaceList,
  !              starting with the "master face"
  n = j_-1
  num = 0
  Nj = SIZE(MergeFaceList)
  jm = MergeFaceList(j_)
  DO

   !1.a. get the new FaceList entry
   n = Loop(n,Nj,num)
   IF( Noisy_ )write(*,*)"vertlist and subfaces list for n=",n

   !1.b. exit criteria
   IF( num>Nj )EXIT

   !1.c. get the face index of that entry
   j = MergeFaceList(n)
   IF( Noisy_ )write(*,*)"get the face index of that entry: j=",MergeFaceList(n)
   
   IF( j==0 )CYCLE

   !get the verts, starting with the first vert for the master face
   !and the second verts for the remaining faces so we don't duplicate
   !verts---this assumes the master face is first of all faces in the
   !counterclockwise ordering of faces in a cell---dependent on
   Nk = SIZE(Mesh%Faces(j)%VertList)
   DO k=MERGE(1,2,j_==n),Nk
    CALL ADD_TO_LIST( VertList , Mesh%Faces(j)%VertList(k) )
   END DO

   !get the subfaces
   Njs = SIZE(Mesh%Faces(j)%SubFaces)
   DO js=1,Njs
    js_ = js_ + 1
    CALL ADD_TO_LIST( SubFaces , Mesh%Faces(j)%SubFaces(js) )
   END DO

   !flag this face for deletion
   IF( n/=j_ )THEN
    CALL ADD_TO_LIST( DeleteFaces , j )
   END IF

  END DO

  !finalize the lists we have created
  CALL FINALIZE_LIST(SubFaces)
  CALL FINALIZE_LIST(VertList)
  CALL FINALIZE_LIST(DeleteFaces)

  !form the edges list
  Nl_ = SIZE(VertList)-1
  ALLOCATE( Edges(1:Nl_) )

  !go through each subface, starting with the "master face"
  n = j_-1
  num = 0
  DO

   !1. iterator
   !1.a. get the new FaceList entry
   n = Loop(n,Nj,num)

   !1.b. exit criteria
   IF( num>Nj )EXIT

   !1.c. get the face index of that entry
   j = MergeFaceList(n)
   IF( Noisy_ )write(*,*)"get the face index of that entry again: j=",MergeFaceList(n)

   !1.d. cycle if it's zero
   IF( j==0 )CYCLE


   !get the edges (cannot add them incrementally because
   !there is no ADD_TO_SET and FINALIZE for the edges
   !structure
   IF( Noisy_ )write(*,*)"assembling edge list for face j=",j
   Nl = SIZE(Mesh%Faces(j)%Edges)
   IF( Noisy_ )write(*,*)"number of edges=",Nl
   DO l=1,Nl
    l_ = l_ + 1
    IF( ASSOCIATED(Mesh%Faces(j)%Edges(l)%EdgeCoeff) )THEN
        CALL ALLOCATE( Edges(l_) , &
            EdgeShape = Mesh%Faces(j)%Edges(l)%EdgeShape , &
            EdgeCoeff = Mesh%Faces(j)%Edges(l)%EdgeCoeff )
    ELSE 
        CALL ALLOCATE( Edges(l_) , &
            EdgeShape = Mesh%Faces(j)%Edges(l)%EdgeShape )
    END IF
   END DO

  END DO


  !now delete the old faces, remembering to shift the indices each time we
  !remove a face
  DO js=1,SIZE(DeleteFaces)

   jd = DeleteFaces(js)
   IF( Noisy_ )write(*,*)"deleting face jd=",jd

   !remove face
   j = REMOVE_Face( Mesh , jd , trash )

   !bump down indices we still need to delete or change
   WHERE( DeleteFaces(js:)>jd )DeleteFaces(js:)= DeleteFaces(js:)-1
   WHERE( SubFaces(:)>jd )SubFaces(:) = SubFaces(:)-1
   IF( PRESENT(FaceMapping) )THEN
    FaceMapping(jd:SIZE(FaceMapping)-1) = FaceMapping(jd+1:)
    CALL REALLOCATE( FaceMapping , -1 )
   END IF

   !bump down master index
   IF( jm>jd )jm = jm - 1

  END DO

  !get the new face list
  Nj = COUNT( Mesh%Cells(i)%FaceList/=0 )
  ALLOCATE( NewFaceList(1:Nj) )
  NewFaceList = PACK(Mesh%Cells(i)%FaceList,Mesh%Cells(i)%FaceList/=0)

  !add face to the Mesh
  j = ADD_face( Mesh , FaceShape = Ps_       , &
                       FaceLabel = FaceLabel , &
                       VertList  = VertList  , &
                       Edges     = Edges     , &
                       SubFaces  = SubFaces  , &
                       overwrite = jm )

  !deallocate stuff
  CALL DEALLOCATE( Edges )
  IF( ASSOCIATED(Edges) )DEALLOCATE( Edges )
  IF( ASSOCIATED(SubFaces) )DEALLOCATE( SubFaces )
  IF( ASSOCIATED(DeleteFaces) )DEALLOCATE( DeleteFaces )
  IF( ASSOCIATED(VertList) )DEALLOCATE( VertList )
 !3D
 CASE(3)
   WRITE(*,*)"NO method installed!"
   STOP

 CASE DEFAULT
   WRITE(*,*)"number of dimensions>3"
   STOP

END SELECT

CALL SETUP_DomainFace( Mesh , j )
CALL UPDATE_DomainFace( Mesh , j )

CALL SETUP_FaceArea( Mesh , j)
CALL UPDATE_FaceArea( Mesh , j )

!ordering fix-up
IF( FaceArea(Mesh,j)<0._KIND_MSH )THEN
 Mesh%Faces(j)%VertList = Reverse(Mesh%Faces(j)%VertList)
 Mesh%Faces(j)%FaceArea = -Mesh%Faces(j)%FaceArea
END IF

CALL SETUP_FaceCentroid( Mesh , j)
CALL UPDATE_FaceCentroid( Mesh , j )

CALL SETUP_FaceNormal(Mesh,j)
CALL UPDATE_FaceNormal_Mesh( Mesh , j )

!!--end--
END SUBROUTINE



SUBROUTINE CLASSIFY_Cell(Mesh,i)
!!#### PURPOSE
!! (Re)Classify the shape of a cell <i>.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: i

!!--begin--
SELECT CASE( NUM_Dimensions(Mesh) )

 CASE(2)

  SELECT CASE( NUM_Faces(Mesh%Cells(i) ) )
    CASE( 3 )    ; Mesh%Cells(i)%CellShape = Tr_
    CASE( 4 )    ; Mesh%Cells(i)%CellShape = CLASSIFY_Qg(Mesh,i)
    CASE DEFAULT ; Mesh%Cells(i)%CellShape = Pg_
  END SELECT

 CASE(3)
  CALL PRINT_Text( s="Error in CLASSIFY_Cell: 3D is not enabled yet!")
END SELECT
!!--end--
END SUBROUTINE


FUNCTION CLASSIFY_Qg(Mesh,i)
!!#### PURPOSE
!! Classify a general Quadrilateral (Qg) as either
!! a square (Qs) or a rectangle (Qr), or leave
!! as general.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
INTEGER :: CLASSIFY_Qg

!!#### LOCAL VARIABLES
INTEGER :: j1,j2,j3,j4

!!--begin--
j1 = ABS(Mesh%Cells(i)%FaceList(1))
j2 = ABS(Mesh%Cells(i)%FaceList(2))
j3 = ABS(Mesh%Cells(i)%FaceList(3))
j4 = ABS(Mesh%Cells(i)%FaceList(4))

IF( (Mesh%Faces(j1)%FaceArea==&
     Mesh%Faces(j3)%FaceArea  ) .AND. &
    (Mesh%Faces(j2)%FaceArea==&
     Mesh%Faces(j4)%FaceArea  ) )THEN

 IF( Mesh%Faces(j2)%FaceArea==&
    Mesh%Faces(j3)%FaceArea )THEN
  CLASSIFY_Qg = Qs_
 ELSE
  CLASSIFY_Qg = Qr_
 END IF

ELSE
 CLASSIFY_Qg = Qg_
END IF

!!--end--
END FUNCTION


SUBROUTINE SETUP_Structured( Mesh , n , delta , origin , permutations )
!!#### PURPOSE
!! Sets up a structured Mesh.
!
!! *-*----*----*---*---*--------*
!! | |    |    |   |   |        |
!! | |     |   |   |  |        |
!! *-*-----*---*---*--*--------*
!! | |      |  |   |   \       |
!! | |     |   |   |   |      |
!! | *    |    |  _*_  |      |
!! */|----*----*-/ | \-*-----*
!! | |    |    |   |   |     |
!! *-*----*----*---*---*----*
!

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh object (/Mesh/)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
!! * the cell widths in each dimension (/delta/)
!! * the number of cells in each dimension (/n/)
!! * the origin of the Mesh, defaults to (0,0,0) (/origin/)
INTEGER               ,INTENT(IN) :: n     (1:Mesh%NDim)
REAL(KIND_MSH)         ,INTENT(IN) :: delta (1:Mesh%NDim,1:MAXVAL(n)+1)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: origin(1:Mesh%NDim)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: permutations(:,:,:,:)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: x,y,z
INTEGER       :: i,j,k,l,m
!!--begin--
SELECT CASE(Mesh%NDim)
 !1-dimensional
 CASE(1)
  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF
  m = 0
  DO i=1,n(1)+1
   m = m + 1
   IF( PRESENT(permutations) )THEN
    l = ADD_vert( Mesh , (/x/)+permutations(:,1,1,i) , "v"//TRIM(STR(m))//"str")
   ELSE
    l = ADD_vert( Mesh , (/x/) , "v"//TRIM(STR(m))//"str" )
   ENDIF
   x = x + delta(1,i)
  END DO

 !2-dimensional
 CASE(2)
  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF

  m = 0
  DO i=1,n(1)+1
   IF( PRESENT(origin) )THEN
    y = origin(2)
   ELSE
    y = REAL(0,KIND_MSH)
   ENDIF
   DO j=1,n(2)+1
    m = m + 1
    IF( PRESENT(permutations) )THEN
     l = ADD_vert( Mesh , (/x,y/)+permutations(:,1,j,i) ,"v"//TRIM(STR(m))//"str" )
    ELSE
     l = ADD_vert( Mesh , (/x,y/) ,"v"//TRIM(STR(m))//"str" )
    ENDIF
    y = y + delta(2,j)
   END DO
   x = x + delta(1,i)
  END DO

 !3-dimensional
 CASE(3)
  IF( PRESENT(origin) )THEN
   x = origin(1)
  ELSE
   x = REAL(0,KIND_MSH)
  ENDIF

  m = 0
  DO i=1,n(1)+1
   IF( PRESENT(origin) )THEN
    y = origin(2)
   ELSE
    y = REAL(0,KIND_MSH)
   ENDIF
   DO j=1,n(2)+1
    IF( PRESENT(origin) )THEN
     z = origin(3)
    ELSE
     z = REAL(0,KIND_MSH)
    ENDIF
    DO k=1,n(3)+1
     m = m + 1
     IF( PRESENT(permutations) )THEN
      l = ADD_vert( Mesh , (/x,y,z/)+permutations(:,k,j,i) , "v"//TRIM(STR(m))//"str" )
     ELSE
      l = ADD_vert( Mesh , (/x,y,z/) , "v"//TRIM(STR(m))//"str" )
     ENDIF
     z = z + delta(3,k)
    END DO
    y = y + delta(2,j)
   END DO
   x = x + delta(1,i)
  END DO
END SELECT

!!--end--
END SUBROUTINE



!!### FUNCTION <<GET_Vert>>
FUNCTION GET_Vert( Mesh , vertlabel , vert , tol ) RESULT(k)

!!#### PURPOSE
!! Get the index of a vert in Mesh, subject to ONE and ONLY ONE
!! of the optional arguments passed.  They are checked for in
!! the order present in the argument list.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),INTENT(IN),OPTIONAL :: vertlabel
REAL(KIND_MSH)                      ,INTENT(IN),OPTIONAL :: vert(Mesh%NDim)
REAL(KIND_MSH)                      ,INTENT(IN),OPTIONAL :: tol

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: k


!!--begin--

!initialize
k = 0

!1. determine the vert by the label
IF( PRESENT(vertlabel) )THEN

 k = INDEXa( Mesh%VertLabels , vertlabel , CASESEN=.FALSE. )

!2. determine the vert by proximity
ELSE IF( PRESENT(vert) )THEN

 k = GET_Vert_proximity( Mesh , vert , tol )

END IF

!return value
Mesh%k = k

!!--end--
END FUNCTION



!!### FUNCTION <<GET_Vert_proximity>>
FUNCTION GET_Vert_proximity( Mesh , vert , tol ) RESULT(k)

!!#### PURPOSE
!! Get the index of a vert in Mesh, subject to the
!! the distance between the vert and the Mesh vert being
!! less than tol.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
REAL(KIND_MSH),INTENT(IN) :: vert(Mesh%NDim)
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: tol

!!#### REQUIRED OUTPUT
INTEGER :: k

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: proximity(1:Mesh%NVerts)

!!--begin--

!determine the proximities
SELECT CASE( Mesh%NDim )
 CASE(2)
  DO k = 1,Mesh%NVerts
   proximity(k) = xyDIST_PP( Mesh%verts(:,k) , vert )
  END DO
END SELECT

!determine the location
k = MINLOC(proximity,1)

!tolerance check
IF( PRESENT(tol) )THEN
 IF( ABS(proximity(k))>tol )k=0
ELSE
 IF( proximity(k)/=0._KIND_MSH )k=0
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<GET_Face_proximity>>
FUNCTION GET_Face_proximity( Mesh , facecentroid , tol ) RESULT(j)

!!#### PURPOSE
!! Get the index of a face in Mesh, subject to the the distance
!! between the facecentroid and the Mesh facecentroid being
!! less than tol.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
REAL(KIND_MSH),INTENT(IN) :: facecentroid(Mesh%NDim)
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: tol

!!#### REQUIRED OUTPUT
INTEGER :: j

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: proximity(1:Mesh%NFaces)

!!--begin--

!determine the proximities
SELECT CASE( Mesh%NDim )
 CASE(2)
  DO j = 1,Mesh%NFaces
   proximity(j) = xyDIST_PP( Mesh%faces(j)%facecentroid(:) , facecentroid )
  END DO
END SELECT

!determine the location
j = MINLOC(proximity,1)

!tolerance check
IF( PRESENT(tol) )THEN
 IF( ABS(proximity(j))>tol )j=0
ELSE
 IF( proximity(j)/=0._KIND_MSH )j=0
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<GET_Cell>>
FUNCTION GET_Cell( Mesh , celllabel ) RESULT(i)

!!#### PURPOSE
!! Get the index of a cell in Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),INTENT(IN),OPTIONAL :: celllabel

!!#### REQUIRED OUTPUT
INTEGER :: i

!!#### LOCAL VARIABLES
!!--begin--
IF( PRESENT(celllabel) )THEN
 i = INDEXa( Mesh%CellLabels , celllabel , CASESEN=.FALSE. )
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<GET_Face>>
FUNCTION GET_FACE( Mesh , FaceLabel , FaceCentroid , VertList , Faceshape , FaceArea , tol ) RESULT(j)

!!#### PURPOSE
!! Get the face index of the face described by the list of
!! verts, type, and optional parameters.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
CHARACTER(*)  ,INTENT(IN),OPTIONAL :: FaceLabel
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: FaceCentroid(:)
INTEGER       ,INTENT(IN),OPTIONAL :: VertList(:)
INTEGER       ,INTENT(IN),OPTIONAL :: FaceShape
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: FaceArea
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: tol

!!#### RESULT
INTEGER :: j


!!--begin--
!initialize
j = 0

!1. determine the face by the label
IF( PRESENT(facelabel) )THEN

 j = INDEXa( Mesh%FaceLabels(1:NUM_Faces(Mesh)) , facelabel , CASESEN=.FALSE. )

!2. determine the face by proximity
ELSE IF( PRESENT(FaceCentroid) )THEN

 j = GET_Face_proximity( Mesh , facecentroid , tol )

END IF

!return value
Mesh%j = j

!!--end--
END FUNCTION




FUNCTION ADD_Face_Ps( Mesh , VertList , EdgeShape , FaceLabel , overwrite ) RESULT(j)
!!#### PURPOSE
!! Add a PLANAR face to the Mesh.  A planar face is a plane
!! in 3D, a line which extends infinitely into +/- z
!! in 2D, and a point which extends infinitely into +/- xy
!! in 1D.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT)       :: Mesh

!!#### REQUIRED INPUT
INTEGER                     ,INTENT(IN) :: VertList(:)
INTEGER                     ,INTENT(IN) :: EdgeShape(:)
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: FaceLabel
INTEGER      ,OPTIONAL,INTENT(IN) :: overwrite

!!#### RESULT
INTEGER :: j

!!#### LOCAL VARIABLES

INTEGER         :: k,k1,k2,l
!***HACK ALERT***
INTEGER,SAVE :: m = 0
TYPE(TYPE_Edge),POINTER :: Edges(:)

!!--begin--
NULLIFY(Edges)

SELECT CASE(Mesh%NDim)

 !two dimensional
 CASE(2)

  IF( ALL(EdgeShape/=Straight_) )THEN
   j = -3
   WRITE(*,*)"Only can handle line edge shapes"
   STOP
  END IF

  !determine vertices
  k1 = VertList(1)
  k2 = VertList(SIZE(VertList))

  !get pointer edge shape
  ALLOCATE( Edges(1:SIZE(EdgeShape)) )
  DO l=1,SIZE(EdgeShape)
   CALL ALLOCATE( Edges(l) , EdgeShape=Straight_ )
  END DO

  !add face to the Mesh
  m = m + 1
  j = ADD_face( Mesh , FaceShape = Ps_       , &
                       FaceLabel = FaceLabel , &
                       VertList  = VertList  , &
                       Edges     = Edges     , &
                                           overwrite = overwrite   )

![memleak]  CALL DEALLOCATE( Edges(1) )
  DO l=1,SIZE(EdgeShape)
   CALL DEALLOCATE( Edges(l) )
  END DO
  DEALLOCATE( Edges )

END SELECT

CALL SETUP_DomainFace( Mesh , j )
CALL UPDATE_DomainFace( Mesh , j )

CALL SETUP_FaceArea( Mesh , j)
CALL UPDATE_FaceArea( Mesh , j )

IF( FaceArea(Mesh,j)<0._KIND_MSH )THEN
 Mesh%Faces(j)%VertList = Reverse(Mesh%Faces(j)%VertList)
 Mesh%Faces(j)%FaceArea = -Mesh%Faces(j)%FaceArea
END IF

CALL SETUP_FaceCentroid( Mesh , j)
CALL UPDATE_FaceCentroid( Mesh , j )

CALL SETUP_FaceNormal(Mesh,j)
CALL UPDATE_FaceNormal_Mesh( Mesh , j )
!WRITE(*,*)"added face j=",j,"to the Mesh"

!!--end--
END FUNCTION


FUNCTION ADD_Cell_Jx( Mesh , FaceList , CellLabel , overwrite ) RESULT(i)
!!#### PURPOSE
!! Add a cell to the Mesh by means of assembling faces.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: FaceList(:)

!!#### OPTIONAL INPUT
CHARACTER(LEN=*,KIND=KIND_S),OPTIONAL,INTENT(IN) :: CellLabel
INTEGER,INTENT(IN),OPTIONAL :: overwrite

!!#### RESULT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S) :: CellLabel_
INTEGER :: i,k,k1,k2,j,n,j1,j2,nfaces
REAL(KIND_MSH) :: V(1:Mesh%Ndim)
REAL(KIND_MSH) :: Pg(1:Mesh%NDim,1:SIZE(FaceList))
REAL(KIND_MSH) :: P_match(1:Mesh%NDim),Centroid(1:Mesh%NDim)
REAL(KIND_MSH) :: Volume
INTEGER     :: order(1:SIZE(FaceList))
INTEGER     :: ifaces(1:SIZE(FaceList))
LOGICAL     :: placed(1:SIZE(FaceList))
TYPE(TYPE_ExplicitFace) :: Xfx(1:SIZE(FaceList))
REAL(KIND_MSH),POINTER :: Lsx(:,:,:)
INTEGER,SAVE :: m=0
TYPE(varying_string) :: VS

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="ADD_Cell_Jx"
!
!#ADD_CELL_JX WRITE(*,'(a,100(3x,i5))')'   ADD_Cell_Jx: FaceList=',FaceList

SELECT CASE(Mesh%NDim)
 CASE(2)

  nfaces = SIZE(FaceList)
  
  !determine the explicit faces
  NULLIFY(Lsx)
  CALL ALLOCATE_Lsx( CGE_nd2 , CGE_rec , Lsx , nfaces )
  DO j=1,nfaces
   Xfx(j)     = ExplicitFace( Mesh , ABS(FaceList(j)) )
   Lsx(:,:,j) = LINESEGMENT_Xf( Xfx(j) )
  END DO

  order = xyCONTIGUOUS_Lsx( nfaces , Lsx )
  DEALLOCATE(LsX)

  IF( ANY(IsError(order)) )THEN
   VS = MODPROC(mod_,proc_)
   CALL Stop(s=STR(VS)//"a contiguous order of the faces could not be found!"//&
"  If you are performing mesh splitting, try increasing the VertTol"//&
" to 1.d-10, e.g. \MSHsplit{...}{VertTol=1.d-10}")
   VS = ""
  END IF

  !use dummy to reorder
  ifaces = SIGN(facelist,order)
  ifaces = Reorder( ifaces , ABS(order) )

  !finally we can add this cell to the Mesh
  m = m + 1
  IF( PRESENT(CellLabel) )THEN
   CellLabel_ = CellLabel
  ELSE
   CALL CLEAR(CellLabel_)
   CellLabel_ = "c"//TRIM(STR(m))//"fa"
  END IF

  i = ADD_cell( Mesh , CellShape     = Pg_ , &
                       CellLabel     = CellLabel_ , &
                       FaceList      = ifaces , &
                       overwrite     = overwrite )

  !deallocate the explicit faces
  DO j=1,nfaces
   CALL CLEAN( Xfx(j) )
  END DO

  !set the cell number
  i = Mesh%i

END SELECT

CALL SETUP_CellVolume   ( Mesh , i )
CALL UPDATE_CellVolume  ( Mesh , i )

!ordering fix-up
IF( CellVolume(Mesh,i)<0._KIND_MSH )THEN
 Mesh%Cells(i)%FaceList   = -Reverse(Mesh%Cells(i)%FaceList)
 Mesh%Cells(i)%CellVolume = -Mesh%Cells(i)%CellVolume
END IF

CALL SETUP_CellCentroid ( Mesh , i )
CALL UPDATE_CellCentroid( Mesh , i )

!!--end--
END FUNCTION


FUNCTION coords( vert )
!!#### PURPOSE
!! Returns a coordinate format of a vert: (x,y,z).
CHARACTER(*),PARAMETER  :: FMT_coords(1:3) = (/"('(' , 1x , E9.2 ,                   1x,')',2x)",&
                                               "('(' , 1x , E9.2 , 1(1x,',',1x,E9.2),1x,')',2x)",&
                                               "('(' , 1x , E9.2 , 2(1x,',',1x,E9.2),1x,')',2x)" /)
REAL(KIND_MSH),INTENT(IN)  :: vert(:)
CHARACTER(10*SIZE(vert)+3*(SIZE(vert-1))) :: coords
SELECTCASE(SIZE(vert))
 CASE(1) ; WRITE(coords,FMT_coords(1))vert
 CASE(2) ; WRITE(coords,FMT_coords(2))vert
 CASE(3) ; WRITE(coords,FMT_coords(3))vert
ENDSELECT
ENDFUNCTION



SUBROUTINE CREATE_Domain_Qr( Mesh , P0,V1,V2,U3 )
!!#### PURPOSE
!! Create a box domain.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),DIMENSION(1:Mesh%NDim) :: P0,V1,V2,U3

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: AREA
REAL(KIND_MSH),DIMENSION(1:Mesh%NDim)     :: P1,P2,P3,P4,P_centroid
REAL(KIND_MSH),DIMENSION(1:Mesh%NDim,1:4) :: Qr
INTEGER :: k0,k1,k2,k3,j0,j1,j2,j3,i,NDim
TYPE(TYPE_Edge),POINTER :: Edges(:)
!!--begin--

! !set the shape integer corresponding to a box
! ALLOCATE( Mesh%DOMAIN%DomainShape )
! Mesh%DOMAIN%DomainShape = Qr_
NDim = NUM_Dimensions(Mesh)

SELECT CASE(NDim)
 CASE(2)
  CALL ALLOCATE_Domain( Mesh%Domain , NDim , Qr_ )
  !! Get the values.
  P1 = P0 + V1
  P2 = P1 + V2
  P3 = P2 - V1

  !! Add the 4 domain verts.
  k0 = ADD_domainVert( Mesh , P0 )
  k1 = ADD_domainVert( Mesh , P1 )
  k2 = ADD_domainVert( Mesh , P2 )
  k3 = ADD_domainVert( Mesh , P3 )

  !! Add the 4 domain faces.
  ALLOCATE( Edges(1) )
  CALL ALLOCATE( Edges(1) , EdgeShape=Straight_ )

  j0 = ADD_domainFace( Mesh , FaceShape    = Ps_ , &
                              FaceNormal   = xyPERPCW_U( xyDIRECTION_V( P1 - P0 ) ), &
                              FaceArea     = xyDIST_PP ( P1 , P0 ) , &
                              FaceCentroid = 0.5_KIND_MSH*(P1+P0) , &
                              VertList     = (/k0,k1/) , &
                              Edges        = Edges )

  j1 = ADD_domainFace( Mesh , FaceShape    = Ps_ , &
                              FaceNormal   = xyPERPCW_U( xyDIRECTION_V( P2 - P1 ) ), &
                              FaceArea     = xyDIST_PP ( P2 , P1 ) , &
                              FaceCentroid = 0.5_KIND_MSH*(P2+P1) , &
                              VertList     = (/k1,k2/) , &
                              Edges        = Edges )

  j2 = ADD_domainFace( Mesh , FaceShape    = Ps_ , &
                              FaceNormal   = xyPERPCW_U( xyDIRECTION_V( P3 - P2 ) ), &
                              FaceArea     = xyDIST_PP ( P3 , P2 ) , &
                              FaceCentroid = 0.5_KIND_MSH*(P3+P2) , &
                              VertList     = (/k2,k3/) , &
                              Edges        = Edges )

  j3 = ADD_domainFace( Mesh , FaceShape    = Ps_ , &
                              FaceNormal   = xyPERPCW_U( xyDIRECTION_V( P0 - P3 ) ), &
                              FaceArea     = xyDIST_PP ( P0 , P3 ) , &
                              FaceCentroid = 0.5_KIND_MSH*(P0+P3) , &
                              VertList     = (/k3,k0/) , &
                              Edges        = Edges )

  CALL DEALLOCATE( Edges(1) )
  DEALLOCATE( Edges )

  !! Get quadrilateral.
  Qr = RESHAPE((/P1,P2,P3,P0/),(/NDim,4/))

  !! Get area of general quadrilateral.
  AREA = xySAREA_Qg( Qr )

  !! Get centroid
  P_centroid = xyCENTROID_Pg( 4 , Qr , AREA )

  !add the single domain cell
  i  = ADD_domainCell( Mesh , CellShape    = Qr_ , &
                              FaceList     = (/j0,j1,j2,j3/) , &
                              CellVolume   = ABS(AREA) , &
                              CellCentroid = P_centroid )

END SELECT

!!--end--
END SUBROUTINE



PURE FUNCTION NUM_DomainFaces( Mesh ) RESULT(NUM)
!!#### PURPOSE
!! Determine the number of faces used in the domain representation.

!!#### ARGUMENTS
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### RESULT
INTEGER :: NUM
!
IF( ASSOCIATED(Mesh%domain%faces) )THEN
 NUM = SIZE(Mesh%domain%faces)
ELSE
 SELECT CASE( Mesh%domain%DomainShape )
  CASE( Qr_ ) ; NUM = 2*Mesh%NDim
 END SELECT
ENDIF
!
ENDFUNCTION


FUNCTION COUNT_BoundaryFaces( Mesh , jd ) RESULT(COUNT)
!!#### PURPOSE
!! Determine the number of boundary faces (those faces which are
!! entirely part of a domain face).  If optional <jd> is included
!! then search counts only boundary faces on domain face
!! <jd>.

!!#### ARGUMENTS
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: jd

!!#### RESULT
INTEGER :: COUNT

!!#### LOCAL VARIABLES
INTEGER :: j,Njd

!!--begin--
!initialize
COUNT = 0

!simple case
IF( .NOT.PRESENT(jd) )THEN
 DO j=1,NUM_Faces(Mesh)
  IF( IsBoundaryFace(Mesh,j) )THEN
   COUNT = COUNT + 1
  END IF
 END DO

!specific case
ELSE
 Njd = NUM_DomainFaces(Mesh)

 !return error
 IF( jd>Njd .OR. jd<1 )THEN
  COUNT = Error(COUNT)
  RETURN

 !count the boundary faces on domain face <jd>
 ELSE
  DO j=1,NUM_Faces(Mesh)
   IF( IsBoundaryFace(Mesh,j,jd) )THEN
    COUNT = COUNT + 1
   END IF
  END DO
 END IF

END IF

!!--end--
END FUNCTION



FUNCTION COUNT_BoundaryVerts( Mesh , jd ) RESULT(COUNT)
!!#### PURPOSE
!! Determine the number of boundary verts (those verts which are
!! entirely part of a domain face).  If optional <jd> is included
!! then search counts only boundary verts on domain face
!! <jd>.

!!#### ARGUMENTS
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: jd

!!#### RESULT
INTEGER :: COUNT

!!#### LOCAL VARIABLES
INTEGER :: k,Njd

!!--begin--
!initialize
COUNT = 0

!simple case
IF( .NOT.PRESENT(jd) )THEN
 DO k=1,NUM_Verts(Mesh)
  IF( IsBoundaryVert(Mesh,k) )THEN
   COUNT = COUNT + 1
  END IF
 END DO

!specific case
ELSE
 Njd = NUM_DomainFaces(Mesh)

 !return error
 IF( jd>Njd .OR. jd<1 )THEN
  COUNT = Error(COUNT)
  RETURN

 !count the boundary verts on domain face <jd>
 ELSE
  DO k=1,NUM_Verts(Mesh)
   IF( IsBoundaryVert(Mesh,k,jd) )THEN
    COUNT = COUNT + 1
   END IF
  END DO
 END IF

END IF

!!--end--
END FUNCTION



FUNCTION COUNT_InteriorFaces( Mesh ) RESULT(COUNT)
!!#### PURPOSE
!! Determine the number of interior faces (those faces which are
!! not part of the domain faces).

!!#### ARGUMENTS
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### RESULT
INTEGER :: COUNT

!!--begin--
COUNT = NUM_Faces(Mesh) - COUNT_BoundaryFaces(Mesh)

!!--end--
END FUNCTION


PURE FUNCTION NUM_DomainVerts( Mesh ) RESULT(NUM)
!PURPOSE
!! Determine the number of verts used in the domain representation.
!ARGUMENTS
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
!RESULT
INTEGER :: NUM
!
IF( ASSOCIATED(Mesh%domain%verts) )THEN
 NUM = SIZE(Mesh%domain%verts,2)
ELSE
 SELECT CASE( Mesh%domain%DomainShape )
  CASE( Qr_ ) ; NUM = 2**Mesh%NDim
 END SELECT
ENDIF
!
ENDFUNCTION


FUNCTION IsSubFace( Mesh , j , jsub )
!!#### PURPOSE
!! Return whether face <jsub> is a subface of face <j>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsSubFace"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j,jsub

!!#### REQUIRED OUTPUT
LOGICAL :: IsSubFace

!!#### LOCAL VARIABLES
INTEGER :: k
TYPE(varying_string) :: VS

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!just make sure they share verts in common
DO k=1,SIZE(Mesh%Faces(jsub)%VertList)
 IF( .NOT.ANY(Mesh%Faces(j)%VertList==&
              Mesh%Faces(jsub)%VertList(k)) )THEN
  IsSubFace = .FALSE.
  RETURN
 END IF
END DO
IsSubFace = .TRUE.

!!--end--
END FUNCTION


FUNCTION IsHangingVert( Mesh , k , i )
!!#### PURPOSE
!! Return whether vert <k> is a hanging vertex
!! in cell <i>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsHangingVert"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: k,i

!!#### REQUIRED OUTPUT
LOGICAL :: IsHangingVert

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!see if it does have a parent face for this cell
IsHangingVert = (ParentFace(Mesh,k,i)>0)

!!--end--
END FUNCTION



FUNCTION IsPointOnFace( Mesh , j , P )
!!#### PURPOSE
!! Return whether point <P> is on face <j> with a semi-sketchy
!! procedure.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsPointOnFace"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j
REAL(KIND_MSH) ,INTENT(IN) :: P(Mesh%NDim)

!!#### REQUIRED OUTPUT
LOGICAL :: IsPointOnFace

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
REAL(KIND_MSH),DIMENSION(Mesh%NDim) :: fc
INTEGER :: k1
REAL(KIND_MSH) :: l1,lp
!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

k1 = FirstVert(Mesh,j)
!k2 = LastVert(Mesh,j)

fc = FaceCentroid(Mesh,ABS(j))

!only need to check distance against one vert
l1 = xyDIST_PP(fc,Vert(Mesh,k1))
lp = xyDIST_PP(fc,P)

IsPointOnFace = ( lp<=l1+DEFAULT_TOL )

!!--end--
END FUNCTION


FUNCTION FirstVert(Mesh,j) RESULT(k)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: j
INTEGER :: N,k
!!--begin--
IF( j>0 )THEN
 k = Mesh%Faces(j)%VertList(1)
!if j<0 then last vert is first
ELSE
 N = SIZE(Mesh%Faces(ABS(j))%VertList)
 k = Mesh%Faces(ABS(j))%VertList(N)
END IF
!!--end--
END FUNCTION


FUNCTION LastVert(Mesh,j) RESULT(k)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: j
INTEGER :: N,k
!!--begin--
IF( j<0 )THEN
 k = Mesh%Faces(ABS(j))%VertList(1)
!if j<0 then last vert is first
ELSE
 N = SIZE(Mesh%Faces(j)%VertList)
 k = Mesh%Faces(j)%VertList(N)
END IF
!!--end--
END FUNCTION


FUNCTION HasFaceVerts( Mesh , j , VertList )
!!#### PURPOSE
!! Return whether face <j> has all the verts
!! in <VertList>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="HasFaceVerts"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j,VertList(:)

!!#### REQUIRED OUTPUT
LOGICAL :: HasFaceVerts

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
INTEGER :: k_

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!initialize
HasFaceVerts = .TRUE.

!see if it does have a parent face for this cell
DO k_=1,SIZE(VertList)
 !if a vert in the list is not in the face's vert list then false
 IF( .NOT.ANY(VertList(k_)==Mesh%Faces(j)%VertList) )THEN
  HasFaceVerts =.FALSE.
 END IF
 IF( .NOT.HasFaceVerts )EXIT
END DO

!!--end--
END FUNCTION


!!### FUNCTION <<GET_VertOnFace>>
FUNCTION GET_VertOnFace( Mesh , j , n , First , Last ) RESULT(k)

!!#### PURPOSE
!! Return the vertex index on a face.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="GET_VertOnFace"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: n
LOGICAL,OPTIONAL,INTENT(IN) :: First,Last

!!#### REQUIRED OUTPUT
INTEGER :: k

!!#### LOCAL VARIABLES
INTEGER :: Nn

!!--begin--

k = Error( k )
Nn = NUM_Verts_FACEINDEX(Mesh,ABS(j))

!look up by index n
IF( PRESENT(n) )THEN
 IF( n>=1 .AND. n<=Nn )THEN
  IF( j>0 )THEN
   k = Mesh%Faces(j)%VertList(n)
  ELSE
   k = Mesh%Faces(-j)%VertList(Nn+1-n)
  END IF
 END IF

!last or first
ELSE

 IF( PRESENT(First) )THEN
  IF( First )THEN
   IF( j>0 )THEN
    k = Mesh%Faces(j)%VertList(1)
   ELSE
    k = Mesh%Faces(-j)%VertList(Nn)
   END IF
  END IF
 END IF
 IF( PRESENT(Last) )THEN
  IF( Last )THEN
   IF( j>0 )THEN
    k = Mesh%Faces(j)%VertList(Nn)
   ELSE
    k = Mesh%Faces(-j)%VertList(1)
   END IF
  END IF
 END IF

END IF


!!--end--
END FUNCTION




FUNCTION HasCellFaces( Mesh , i , FaceList )
!!#### PURPOSE
!! Return whether cell <i> has all the faces
!! in <FaceList>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="HasCellFaces"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i,FaceList(:)

!!#### REQUIRED OUTPUT
LOGICAL :: HasCellFaces

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
INTEGER :: j_

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!initialize
HasCellFaces = .TRUE.

!iterate through the faces
DO j_=1,SIZE(FaceList)
 !if a face in the list is not in the cell's face list then false
 IF( .NOT.ANY(FaceList(j_)==Mesh%Cells(i)%FaceList) )THEN
  HasCellFaces =.FALSE.
 END IF
 IF( .NOT.HasCellFaces )EXIT
END DO

!!--end--
END FUNCTION


FUNCTION HasCellVert( Mesh , i , k )
!!#### PURPOSE
!! Return whether cell <i> has the vert <k>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="HasCellVert"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i,k

!!#### REQUIRED OUTPUT
LOGICAL :: HasCellVert

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
INTEGER :: j_,j
!REAL :: tin,tout

!!--begin--

!CALL CPU_TIME(tin)
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

HasCellVert = .FALSE.

!check each face of the cell
DO j_=1,SIZE(Mesh%Cells(i)%FaceList)

 j = ABS(Mesh%Cells(i)%FaceList(j_))

 IF( ANY(Mesh%Faces(j)%VertList==k) )THEN
  HasCellVert = .TRUE.
  EXIT
 END IF

END DO

!CALL CPU_TIME(tout)
!DT_HasCellVert = DT_HasCellVert + MAX( 0. , tout-tin )

!!--end--
END FUNCTION


FUNCTION HasCellVerts( Mesh , i , VertList )
!!#### PURPOSE
!! Return whether cell <i> has all the verts
!! in <VertList>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="HasCellVerts"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i,VertList(:)

!!#### REQUIRED OUTPUT
LOGICAL :: HasCellVerts

!!#### LOCAL VARIABLES
INTEGER :: j_,k_,j,k
LOGICAL :: Mask(SIZE(VertList))
TYPE(varying_string) :: VS

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!initialize
Mask = .FALSE.

!iterate through the verts
DO k_=1,SIZE(VertList)

 k = VertList(k_)

 !check each face of the cell
 DO j_=1,SIZE(Mesh%Cells(i)%FaceList)

  j = ABS(Mesh%Cells(i)%FaceList(j_))

  IF( ANY(Mesh%Faces(j)%VertList==k) )THEN
   Mask(k_) = .TRUE.
   EXIT
  END IF

 END DO

END DO

HasCellVerts = ALL(Mask)

!!--end--
END FUNCTION


FUNCTION IsCornerVert( Mesh , k , i )
!!#### PURPOSE
!! Return whether vert <k> is a corner vertex
!! of cell <i>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsCornerVert"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: k,i

!!#### REQUIRED OUTPUT
LOGICAL :: IsCornerVert

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!see if doesn't have a parent face for this cell
IsCornerVert = (ParentFace(Mesh,k,i)==0)

!!--end--
END FUNCTION


FUNCTION ParentFace( Mesh , k , i )
!!#### PURPOSE
!! Return the parent face <j> that vert <k> is a hanging vertex
!! of in cell <i>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="ParentFace"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: k,i

!!#### REQUIRED OUTPUT
INTEGER :: ParentFace

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
INTEGER :: count,Nj_,j_,j
INTEGER,POINTER :: FaceList(:),VertList(:)

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

Nj_ = NUM_Faces(Mesh%Cells(i))
IF( Nj_==0 )THEN
 ParentFace = -1
 RETURN
END IF

!just make sure they share verts in common
FaceList => ptr_FaceList(Mesh,i)
count = 0
DO j_=1,Nj_

 j = FaceList(j_)

 VertList => ptr_VertList(Mesh,j)

 IF( ANY(VertList==k) )THEN
  count = count + 1
  ParentFace = j
 END IF

END DO
FaceList => NULL()
VertList => NULL()

IF( count>1 )THEN
 ParentFace = 0
END IF

!!--end--
END FUNCTION



FUNCTION IsSubFace_Hardway( Mesh , j , jsub ) RESULT(IsSubFace)
!!#### PURPOSE
!! Return whether face <jsub> is a subface of face <j>.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsSubFace"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j,jsub

!!#### REQUIRED OUTPUT
LOGICAL :: IsSubFace

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitFace) :: Xf_1,Xf_2
REAL(KIND_MSH) :: Ls_m(1:2,1:2),Ls_s(1:2,1:2)
REAL(KIND_MSH) :: Pn_1(1:3),Pn_2(1:3),Pn_3(1:3)
REAL(KIND_MSH) :: d_3(1:2),d_12(1:2,1:2)
INTEGER :: n1,n2,n,Nn,NDim
TYPE(varying_string) :: VS

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!select action based on the Faceshape of the boundary-face
Xf_1 = ExplicitFace(Mesh,j)
Xf_2 = ExplicitFace(Mesh,jsub)
IF( IsApprox(Xf_1,Xf_2) )THEN
 IsSubFace = .TRUE.
ELSE
 !get the potential master line segment
 Ls_m = LINESEGMENT_Xf( Xf_1 )
 !get the potential sub line segment
 Ls_s = LINESEGMENT_Xf( Xf_2 )

 !do it the hard way:
 !set up three planes
 ! 1 in the downward direction of the face
 ! 1 in the upward direction of the face
 ! 1 in the direction of the face normal (or negative)
 ! if the distance of both points on the potential subface
 ! is positive from both of the first two planes and zero for
 ! the last plane, then we have a match

 !Plane 1
 Pn_1 = xyPLANE_PV( Ls_m(:,1) , Ls_m(:,2)-Ls_m(:,1) )

 !Plane 2
 Pn_2 = xyPLANE_PV( Ls_m(:,2) , Ls_m(:,1)-Ls_m(:,2) )

 !Plane 3
 Pn_3 = xyPLANE_PV( Ls_m(:,1) , xyPERPCW_V(Ls_m(:,1)-Ls_m(:,2)) )

 !get distance from plane 3 of both points on Ls_s
 d_3(1) = xySDIST_PnP( Pn_3 , Ls_s(:,1) )
 d_3(2) = xySDIST_PnP( Pn_3 , Ls_s(:,2) )

 !get distance from plane 1 of both points on Ls_s
 d_12(1,1) = xySDIST_PnP( Pn_1 , Ls_s(:,1) )
 d_12(2,1) = xySDIST_PnP( Pn_1 , Ls_s(:,2) )
 !get distance from plane 2 of both points on Ls_s
 d_12(1,2) = xySDIST_PnP( Pn_2 , Ls_s(:,1) )
 d_12(2,2) = xySDIST_PnP( Pn_2 , Ls_s(:,2) )

 IF( .NOT.IsApprox( MAXVAL(ABS(d_3)) , 0._KIND_MSH ) )THEN
  IsSubFace=.FALSE.
  RETURN
 END IF

 IF( .NOT.ALL(d_12>=0._KIND_MSH) )THEN
  IsSubFace=.FALSE.
  RETURN
 END IF

 IsSubFace = .TRUE.
END IF

!!--end--
END FUNCTION


FUNCTION IsBoundaryVert1( Mesh , k , jd )
!!#### PURPOSE
!! Return whether vertex k is on the domain face jd.

!!#### USAGE
!
!!  IsBoundaryVert  : if vertex k is on the domain-face jd
!

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsBoundaryVert"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: k,jd

!!#### REQUIRED OUTPUT
LOGICAL :: IsBoundaryVert1
TYPE(varying_string) :: VS

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!select action based on the Faceshape of the boundary-face
SELECT CASE( Mesh%domain%faces(jd)%Faceshape )

 CASE( Ps_ ) ; IsBoundaryVert1 = IsBoundaryVert_Ps( Mesh , k , jd )

END SELECT
!!--end--
END FUNCTION


FUNCTION IsBoundaryVert0( Mesh , k )
!!#### PURPOSE
!! Return whether vertex k is on any domain face.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsBoundaryVert"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: k

!!#### REQUIRED OUTPUT
LOGICAL :: IsBoundaryVert0

!!#### LOCAL VARIABLES
INTEGER :: jd
TYPE(varying_string) :: VS

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"
IsBoundaryVert0 = .FALSE.
DO jd=1,SIZE(Mesh%domain%faces)
 IsBoundaryVert0 = IsBoundaryVert1( Mesh , k , jd )
 IF( IsBoundaryVert0 )EXIT
END DO
!!--end--
END FUNCTION

FUNCTION IsFinalized(Mesh) RESULT(Is)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
LOGICAL :: Is
Is = Mesh%Finalized
END FUNCTION




!!### FUNCTION <<FaceAverage>>
FUNCTION FaceAverage( Mesh , j , y )

!!#### PURPOSE
!! Return the face average value of y on face j, where y is an
!! array of values at all vertices.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="FaceAverage"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j
REAL(KIND_MSH) ,INTENT(IN) :: y(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceAverage
TYPE(varying_string) :: VS

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!select action based on the Faceshape of the face
SELECT CASE( Mesh%faces(j)%Faceshape )

 CASE( Ps_ ) ; FaceAverage = FaceAverage_Ps( Mesh , j , y )

 CASE DEFAULT
   CALL Stop(s="In <FaceAverage> some uknown face shape prevents&
     & execution.")

END SELECT

!!--end--
END FUNCTION


!!!### FUNCTION <<TEST_LinearCellFunction_Gauss>>
!FUNCTION TEST_LinearCellFunction_Gauss(Mesh,i)
!!!#### REQUIRED INPUT
!TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
!INTEGER        ,INTENT(IN) :: i
!REAL(KIND_MSH) :: yc
!REAL(KIND_MSH) :: yf(7)
!REAL(KIND_MSH) :: Lin(Mesh%Ndim+1)
!!!--begin--
!
!N = Random((/3,7/))
!
!a = Random((/-1.d0,1.d0/))
!b = Random((/-1.d0,1.d0/))
!c = Random((/-1.d0,1.d0/))
!
!Lin0 = (/a,b,c/)
!
!DO l=1,N
! yf(l) = FaceAverage_F(
!
! Lin = LinearCellFunction_Gauss(Mesh,i,yc,yf)
!
!!!--end--
!CONTAINS
!
!FUNCTION fLin(x,y)
!!!--begin--
!Lin(1) + Lin(2)*x + Lin(3)
!!!--end--
!END FUNCTION
!
!END FUNCTION

!!### FUNCTION <<LinearCellFunction_Gauss>>
FUNCTION LinearCellFunction_Gauss(Mesh,i,yc,yf) RESULT(Lin)

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: yc
REAL(KIND_MSH) ,INTENT(IN) :: yf(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Lin(Mesh%Ndim+1)

!!#### LOCAL VARIABLES
REAL(KIND_MSH),POINTER,SAVE :: A(:,:)=>NULL(),Fa(:)=>NULL()
INTEGER :: Nd,Na,n,j
REAL(KIND_MSH) :: VC

!!--begin--

Nd = Mesh%NDim
Na = NUM_Faces(Mesh%Cells(i))
IF( .NOT.ASSOCIATED(Fa) )THEN
 ALLOCATE( A(Nd,Na) , Fa(Na) )
ELSE
 IF( Na>SIZE(Fa,1) )THEN
  CALL REALLOCATE( A , (/0,Na/) )
  CALL REALLOCATE( fA , Na )
 END IF
END IF

DO n=1,Na
 j = Mesh%Cells(i)%FaceList(n)
 A(:,n) = FaceNormal(Mesh,j)*FaceArea(Mesh,ABS(j))
 Fa(n)  = yf(ABS(j))
END DO

Vc = Mesh%Cells(i)%CellVolume

!get the gradient and the function (the special thing about the Gauss approximation
!is that the center value and the average are the same.
Lin(1) = yc
Lin(2:Nd+1) = CellGradient_Gauss( Nd , Na , Vc , A(1:Nd,1:Na) , Fa(1:Na) )

!!--end--
END FUNCTION


!!### FUNCTION <<LinearCellFunction_LLS>>
FUNCTION LinearCellFunction_LLS(Mesh,i,yc,yf) RESULT(Lin)

!!#### MODULES
USE INT_LAPACK2                                                      !!((08-B-INT_LAPACK2.f90))

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: yc
REAL(KIND_MSH) ,INTENT(IN) :: yf(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Lin(Mesh%Ndim+1)

!!#### LOCAL VARIABLES
REAL(KIND_MSH),POINTER,SAVE :: A(:,:)=>NULL(),b(:,:)=>NULL()
INTEGER :: Nd,Na,n,j,INFO
REAL(KIND_MSH) :: VC

!!--begin--

Nd = Mesh%NDim
Na = NUM_Faces(Mesh%Cells(i))
IF( .NOT.ASSOCIATED(b) )THEN
 ALLOCATE( A(Na,Nd) , b(Na,1) )
ELSE
 IF( Na>SIZE(A,1) )THEN
  CALL REALLOCATE( A , (/Na,0/) )
  CALL REALLOCATE( b , (/Na,0/) )
 END IF
END IF

DO n=1,Na
 j = Mesh%Cells(i)%FaceList(n)
 A(n,:) = FaceCentroid(Mesh,ABS(j))-CellCentroid(Mesh,i)
 b(n,1) = YF(ABS(j))-YC
END DO

!get least squares solution
CALL LA_GELS( A(1:Na,1:Nd), b(1:Na,1), TRANS="N", INFO=info )

IF( INFO/=0 )THEN
 WRITE(*,*)"Error in LinearCellFunction_LLS: &
   &LA_GELS return error [info="//TRIM(STR(info))//"] using constant&
   & function instead."
 Lin(1) = YC
 Lin(2:) = 0._KIND_MSH
ELSE
 !get the gradient (b is overwritten)
 Lin(1) = 0._KIND_MSH
 Lin(2:Nd+1) = b(1:Nd,1)
 !now get the center value such that the entire linear over the
 !cell equals the average
 Lin(1) = yc - CellAverage_Lin( Mesh , i , Lin )
END IF


!!--end--
END FUNCTION

!!### FUNCTION <<CellAverage_Lin>>
FUNCTION CellAverage_Lin( Mesh , i , Lin ) RESULT(Average)

!!#### PURPOSE
!! Return the cell average value in cell i, over a linear
!! function <Lin>.

!!#### MODULES
USE FUN_xyAVERAGE                                                    !!((09-A-FUN_xyAVERAGE.f90))

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CellAverage_Lin"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: Lin(1:Mesh%NDim)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH)       :: Average

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
REAL(KIND_MSH) :: CC(Mesh%NDim)
REAL(KIND_MSH),ALLOCATABLE :: Pg(:,:)
TYPE(TYPE_ExplicitCell) :: Xc
INTEGER :: N,l

!!--begin--

INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

IF( Mesh%NDim/=2 )THEN
 WRITE(*,*)"Error in TBX_Mesh::CellAverage_Lin : Only 2D allowed."
 STOP
ELSE
 !just do it generally for a polygon
 Xc = ExplicitCell(Mesh,i)
 N  = NUM_Points( Xc )
 ALLOCATE( Pg(2,N) )
 Pg = POLYGON_Xc( Xc )
 CALL CLEAN_Xc( Xc )

 !not sure if we need to shift to zero
 CC = CellCentroid(Mesh,i)
 DO l=1,N
  Pg(:,l) = Pg(:,l) - CC(:)
 END DO
 AVERAGE = xyAVERAGE_Lin2Pg( Lin , N , Pg )

 DEALLOCATE( Pg )

END IF

!!--end--
END FUNCTION



!!### FUNCTION <<CellAverage>>
FUNCTION CellAverage( Mesh , i , yv , yf , Method )

!!#### PURPOSE
!! Return the cell average value in cell i, where <yv> is an
!! array of values at all verts and <yf> is an array at
!! all faces.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CellAverage"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: yv(:)
REAL(KIND_MSH) ,INTENT(IN) :: yf(:)

!!#### OPTIONAL INPUT
CHARACTER(*),OPTIONAL,INTENT(IN) :: Method

!!#### REQUIRED OUTPUT
REAL(KIND_MSH)       :: CellAverage

!!#### LOCAL VARIABLES
TYPE(varying_string)    :: VS
CHARACTER(16)           :: Method_
CHARACTER(16),PARAMETER :: DEFAULT_Method="EQWTSV          "

!!--begin--

INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!default to the equal weights at vertices method (worst)
Method_ = Default( DEFAULT_Method , Method )

!WRITE(*,*)"Using TBX_Mesh::CellAverage with Method="//Method_

SELECT CASE( Upcase(TRIM(Method_)) )
 CASE("EQWTSV") ; CellAverage = CellAverage_EqWtsV( Mesh , i , yv )
 CASE("EQWTSF") ; CellAverage = CellAverage_EqWtsF( Mesh , i , yf )
 CASE("LIN2FC") ; CellAverage = CellAverage_Lin2FC( Mesh , i , yf )
 CASE("LINTRI") ; CellAverage = CellAverage_LinTri( Mesh , i , yv )
 CASE DEFAULT
   CALL Stop(s="ERROR: In <CellAverage> the Method="//TRIM(Method_)//&
     " is not known.")
END SELECT

!!--end--
END FUNCTION




!!### FUNCTION <<CellAverage_LinTri>>
FUNCTION CellAverage_LinTri( Mesh , i , y )  RESULT(CellAverage)

!!#### PURPOSE
!! Return the cell average value from vertex values, <y>
!! by triangulating a cell and assuming a linear function
!! $f(x,y)$ on each triangle.

USE FUN_xyLIN2                                                       !!((09-C-FUN_xyLIN2.f90))
USE FUN_xyEVAL                                                       !!((05-A-FUN_xyEVAL.f90))

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CellAverage_LinTri"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: y(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellAverage

!!#### LOCAL VARIABLES
TYPE(varying_string)   :: VS
REAL(KIND_MSH),POINTER :: Tr(:,:,:)
INTEGER       ,POINTER :: VertList(:,:)
INTEGER                :: t,N,m
INTEGER                :: k1,k2,k3
REAL(KIND_MSH)         :: actualerr,fv(3),Lin2(3)
INTEGER                :: ierr

!!--begin--

INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

NULLIFY( Tr , VertList )

!get triangles
CALL TriangulateCell_2D(Mesh,i,N,Tr,VertIndexTr=VertList)
if( VERBOSE )then
WRITE(*,*)"-------------"
WRITE(*,*)"i=",i
WRITE(*,*)"VertList=",VertList
end if

CellAverage = 0._KIND_MSH
DO t=1,N
 k1 = VertList(1,t)
 k2 = VertList(2,t)
 k3 = VertList(3,t)
 fv = (/y(k1),y(k2),y(k3)/)

 if( VERBOSE )WRITE(*,*)"fv=",fv

 Lin2 = xyLIN2_Tr( Tr(:,:,t) , fv(:) )

 if( VERBOSE )WRITE(*,*)"Lin2=",Lin2

 !integrate them
 CellAverage = CellAverage + &
   xyINTEGRALF_Tr( fLin2 , Tr(:,:,t) , &
     abstol=1.d-12 , reltol=1.d-12 , &
     num_eval=m , err=actualerr , ierr=ierr )/CellVolume(Mesh,i)
 IF( ierr/=0 )THEN
  CALL Stop(s="The xyINTEGRALF_Tr function returned error: "//&
    TRIM(STR(ierr)))
 END IF

END DO

if( VERBOSE )then
WRITE(*,*)"i=",i
WRITE(*,*)"CellAverage=",CellAverage
WRITE(*,*)"-------------"

WRITE(*,*)"number of function evaluations=",m
WRITE(*,*)"actual error=",actualerr

end if

!clear triangles
DEALLOCATE( Tr , VertList )

!!--end--
CONTAINS

FUNCTION fLin2( x , y )
REAL(KIND_MSH),INTENT(IN) :: x,y
REAL(KIND_MSH) :: fLin2
fLin2 = xyEVAL_Lin2P( Lin2 , (/x,y/) )
END FUNCTION

END FUNCTION



!!### SUBROUTINE <<GET_CellSpatialMoments>>
SUBROUTINE GET_CellSpatialMoments( Mesh , i , &
 Moments , Center )

!!#### PURPOSE
!! Return the moments of the cell:
!! 1,x,y,xx,yy,xy.
USE FUN_xyINTEGRAL1                                                  !!((06-A-FUN_xyINTEGRAL1.f90))
USE FUN_xyINTEGRALX                                                  !!((07-A-FUN_xyINTEGRALX.f90))
USE FUN_xyINTEGRALY                                                  !!((07-A-FUN_xyINTEGRALY.f90))
USE FUN_xyINTEGRALXX                                                 !!((07-A-FUN_xyINTEGRALXX.f90))
USE FUN_xyINTEGRALYY                                                 !!((07-A-FUN_xyINTEGRALYY.f90))
USE FUN_xyINTEGRALXY                                                 !!((07-A-FUN_xyINTEGRALXY.f90))

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CellSpatialMoments"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i

!!#### OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: Center

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),INTENT(OUT) :: Moments(6)

!!#### LOCAL VARIABLES
TYPE(varying_string)       :: VS
INTEGER :: j,N,m,d
REAL(KIND_MSH) :: actualerr,CC(Mesh%Ndim)
LOGICAL :: Center_
REAL(KIND_MSH),ALLOCATABLE :: Pg(:,:)
TYPE(TYPE_ExplicitCell) :: Xc
!!--begin--

INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

Center_ = DEFAULT(.FALSE.,Center)

!get the polygon
Xc = ExplicitCell(Mesh,i)
N  = NUM_Points( Xc )
ALLOCATE( Pg(Mesh%NDim,N) )

Pg = POLYGON_Xc( Xc )
CALL CLEAN_Xc( Xc )

IF( Center_ )THEN
 CC = CellCentroid(Mesh,i)
 DO d=1,Mesh%NDim
  Pg(d,:) = Pg(d,:)-CC(d)
 END DO
END IF

Moments(1) = xyINTEGRAL1_Pg(N,Pg)
Moments(2) = xyINTEGRALX_Pg(N,Pg)
Moments(3) = xyINTEGRALY_Pg(N,Pg)
Moments(4) = xyINTEGRALXX_Pg(N,Pg)
Moments(5) = xyINTEGRALYY_Pg(N,Pg)
Moments(6) = xyINTEGRALXY_Pg(N,Pg)

!!--end--
END SUBROUTINE


!!### FUNCTION <<CellAverage_F>>
FUNCTION CellAverage_F( Mesh , i , F , Center )  RESULT(CellAverage)

!!#### PURPOSE
!! Return the cell-average from a function of
!! x and y.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CellAverage_F"

!!#### INTERFACE
INTERFACE
 FUNCTION f(x,y)
  USE KND_Mesh                                                       !!((05-B-KND_Mesh.f90))
  REAL(KIND_MSH),INTENT(IN) :: x,y
  REAL(KIND_MSH) :: f
 END FUNCTION
END INTERFACE

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i

!!#### OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: Center

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellAverage

!!#### LOCAL VARIABLES
TYPE(varying_string)       :: VS
REAL(KIND_MSH),POINTER :: Tr(:,:,:)
INTEGER :: j,N,m,d
REAL(KIND_MSH) :: actualerr,CC(Mesh%Ndim)
LOGICAL :: Center_

!!--begin--
NULLIFY( Tr )

INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

Center_ = DEFAULT(.FALSE.,Center)

!get triangles
!CALL CLEARn( Tr )
CALL TriangulateCell_2D(Mesh,i,N,Tr,IncludeCellCenters=.TRUE.)

IF( Center_ )THEN
 CC = CellCentroid(Mesh,i)
 DO d=1,Mesh%NDim
  Tr(d,:,:) = Tr(d,:,:)-CC(d)
 END DO
 CellAverage = xyINTEGRALF_Trx( flocal , N , Tr , abstol=1.d-12,reltol=1.d-12 , &
   num_eval=m , err=actualerr)/CellVolume(Mesh,i)
ELSE
 !WRITE(*,*)'in_trx'
 CellAverage = xyINTEGRALF_Trx( f , N , Tr , abstol=1.d-12,reltol=1.d-12 , &
   num_eval=m , err=actualerr)/CellVolume(Mesh,i)
END IF

!clear triangles
DEALLOCATE(Tr)

!!--end--
CONTAINS

 FUNCTION flocal(x,y)
  USE KND_Mesh                                                       !!((05-B-KND_Mesh.f90))
  REAL(KIND_MSH),INTENT(IN) :: x,y
  REAL(KIND_MSH) :: flocal
  flocal= f(x+CC(1),y+CC(2))
 END FUNCTION

END FUNCTION



!!### FUNCTION <<FaceAverage_F>>
FUNCTION FaceAverage_F( Mesh , j , F )  RESULT(FaceAverage)

!!#### PURPOSE
!! Return the face-average from a function of
!! x and y.

USE SUB_Swap                                                         !!((04-A-SUB_Swap.f90))

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="FaceAverage_F"

!!#### INTERFACE
INTERFACE
 FUNCTION f(x,y)
  USE KND_IntrinsicTypes                                             !!((01-A-KND_IntrinsicTypes.f90))
  REAL(KIND_Rdp),INTENT(IN) :: x,y
  REAL(KIND_Rdp) :: f
 END FUNCTION
END INTERFACE

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceAverage

!!#### LOCAL VARIABLES
TYPE(varying_string)   :: VS
INTEGER :: k1,k2,Nk
REAL(KIND_MSH) :: Ls(2,2)

!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"
Nk = SIZE(Mesh%Faces(abs(j))%VertList)
k1 = Mesh%Faces(abs(j))%VertList(1)
k2 = Mesh%Faces(abs(j))%VertList(Nk)

IF( j<0 )THEN
 CALL Swap(k1,k2)
END IF

Ls(:,1) = Vert(Mesh,k1)
Ls(:,2) = Vert(Mesh,k2)

!integrate them
FaceAverage = xyINTEGRALF_Ls( f , Ls  ,reltol=1.d-12,abstol=1.d-12)/FaceArea(Mesh,j)

!!--end--
END FUNCTION




FUNCTION IsBoundaryVert_Ps( Mesh , k , jd )
!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsBoundaryVert_Ps"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: k,jd

!!#### REQUIRED OUTPUT
LOGICAL :: IsBoundaryVert_Ps

!!#### LOCALS
REAL(KIND_MSH) :: Pn( 1:Mesh%NDim+1 )
REAL(KIND_MSH) :: SDIST

!!--begin--
Pn = xyPLANE_PV( Mesh%domain%faces(jd)%FaceCentroid , &
                 Mesh%domain%faces(jd)%FaceNormal )

SDIST = xySDIST_PnP( Pn , Mesh%verts(:,k) )

IsBoundaryVert_Ps = (ABS(SDIST)<SQRT(EPSILON(SDIST)))

!!--end--
END FUNCTION



FUNCTION FaceAverage_Ps( Mesh , j , y )
!!#### PURPOSE
!! Determine the face average value of y, where
!! y is some function known at every vertex.

!!#### DEPENDENCIES
USE FUN_Integrate1S_trapezoid,ONLY: Integrate=>Integrate1S_trapezoid !!((04-B-FUN_Integrate1S_trapezoid.f90))

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="FaceAverage_Ps"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j
REAL(KIND_MSH)   ,INTENT(IN) :: y(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceAverage_Ps

!!#### LOCALS
REAL(KIND_MSH),POINTER :: x_(:),y_(:)

!!--begin--
!map the function <y> evaluated at vertices in global
!coordinates to the function <y_> evaluated at local coordinates
!along with the values of those coordinates, <x_>, based
!on the origin at the face center, assuming a planar
!segment face
x_ => NULL()
y_ => NULL()
CALL MAP_LocalFace_Ps( Mesh , j , y , x_ , y_ )

!get face average with trapezoid integration
FaceAverage_Ps = Integrate( y_ , x_ ) / &
  Mesh%faces(j)%FaceArea

!deallocate
DEALLOCATE( x_, y_ )

!!--end--
END FUNCTION




SUBROUTINE MAP_LocalFace_Ps( Mesh , j , y , x_ , y_ )
!!#### PURPOSE
!! Determine the local face coordinates <x_> and
!! local face values of the function <y_>, where
!! <y> is some function known at every vertex.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="MAP_LocalFace_Ps"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j
REAL(KIND_MSH) ,INTENT(IN) :: y(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: x_(:),y_(:)

!!#### LOCALS
REAL(KIND_MSH)         :: Pn( 1:Mesh%NDim+1 )
INTEGER               :: k,k_,Nk

!!#### UPDATED
!! 31.2006

!!#### FUTURE IMPROVEMENTS
!! 2D to 3D needed, treatment of f(r) instead of y(x)
!! <x_> => <r>
!! <y_> => <f>

!!--begin--
!get number of verts and allocate
Nk = SIZE(Mesh%Faces(j)%VertList)
ALLOCATE( x_(1:Nk) , y_(1:Nk) )

!determine plane of the face centroid
Pn = xyPLANE_PV( Mesh%faces(j)%FaceCentroid , &
                 xyPERPCCW_U( Mesh%faces(j)%FaceNormal) )
DO k_=1,Nk
 !get target vert
 k = Mesh%Faces(j)%VertList(k_)

 !get x-values
 x_(k_) = xySDIST_PnP( Pn , Mesh%verts(:,k) )

 !get y-values
 y_(k_) = y(k)
END DO

!reorder the points, possibly
IF( x_(Nk)<x_(1) )THEN
 x_ = Reverse(x_)
 y_ = Reverse(y_)
END IF

!!--end--
END SUBROUTINE



!!### FUNCTION <<CellAverage_EqWtsF>>
FUNCTION CellAverage_EqWtsF( Mesh , i , y )  RESULT(CellAverage)

!!#### PURPOSE
!! Determine the cell average value of y on cell i, using
!! equal weights for a function <y> known at each face.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CellAverage_EqWtsF"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: y(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellAverage

!!#### LOCALS
INTEGER :: n,j,Nj

!!--begin--

Nj = NUM_Faces_CELLINDEX(Mesh,i)

CellAverage = 0._KIND_MSH
DO n=1,Nj
 j = Mesh%Cells(i)%FaceList(n)
 CellAverage = CellAverage  + y(ABS(j))
END DO
CellAverage = CellAverage/REAL(Nj,KIND_MSH)


!!--end--
END FUNCTION




!!### FUNCTION <<CellAverage_EqWtsV>>
FUNCTION CellAverage_EqWtsV( Mesh , i , y )  RESULT(CellAverage)

!!#### PURPOSE
!! Determine the cell average value of y on cell i, using
!! equal weights for a function <y> known at each face.

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CellAverage_EqWtsV"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: y(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellAverage

!!#### LOCALS
INTEGER :: n,k,Nk
INTEGER,POINTER :: VertSet(:)

!!--begin--
!initialize
NULLIFY(VertSet)

CALL GET_CellVertBoundarySet(Mesh,i,VertSet)

Nk = SIZE(VertSet)
CellAverage = 0._KIND_MSH
DO n=1,Nk
 k = VertSet(n)
 CellAverage = CellAverage  + y(k)
END DO
CellAverage = CellAverage/REAL(Nk,KIND_MSH)

DEALLOCATE( VertSet )

!!--end--
END FUNCTION




!!### FUNCTION <<CellAverage_Lin2FC>>
FUNCTION CellAverage_Lin2FC( Mesh , i , y ) RESULT(CellAverage)

!!#### PURPOSE
!! Determine the cell-average value of y on cell i, from
!! y-values on faces, using a linear approximating function $f(x,y)=a+bx+cy$,
!! then evaluating that function at cell center.

USE INT_LAPACK2                                                      !!((08-B-INT_LAPACK2.f90))

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="CellAverage_Lin2FC"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: y(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellAverage

!!#### LOCALS
REAL(KIND_MSH),ALLOCATABLE :: A(:,:),b(:,:)
INTEGER                    :: n,j,Nj,info

!!--begin--

Nj = NUM_Faces_CELLINDEX(Mesh,i)
ALLOCATE( A(Nj,3),b(Nj,1) )

!assemble system to solve for ax+by+c
A(:,1) = 1.d0
DO n=1,Nj
 j = ABS(Mesh%Cells(i)%FaceList(n))
 A(n,2:3) = FaceCentroid(Mesh,j) - CellCentroid(Mesh,i)
 b(n,1)   = y(j)
END DO

!get least squares solution
CALL LA_GELS( A(1:Nj,1:3), b(1:Nj,1), TRANS="N", INFO=info )

!set cell average equal to a
CellAverage = b(1,1)

DEALLOCATE( A,b )

!!--end--
END FUNCTION





SUBROUTINE TriangulateFace_3D( Mesh , j , Tri )
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)  ,INTENT(IN) :: Mesh
INTEGER         ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: Tri(:,:)

END SUBROUTINE

SUBROUTINE TriangulateCell_3D( Mesh , i , Tri )
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)  ,INTENT(IN) :: Mesh
INTEGER         ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: Tri(:,:)

END SUBROUTINE


SUBROUTINE GET_CellVertBoundarySet(Mesh,i,VertSet)
!!#### PURPOSE
!! Return a pointer to a list of vertices that belong
!! to a cell, optionally including face centers in that list.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh) ,INTENT(IN) :: Mesh
INTEGER         ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
INTEGER,POINTER,INTENT(OUT) :: VertSet(:)

!!#### LOCAL VARIABLES
INTEGER :: j_,j,k_,k

!!--begin--
NULLIFY(VertSet)

!initialize
! IF( ASSOCIATED(VertSet) )THEN
!  VertSet = Error(1)
! ELSE
!  ALLOCATE( VertSet(8) )
!  VertSet = Error(1)
! END IF

!loop over faces
DO j_=1,SIZE( Mesh%cells(i)%FaceList )

 !get target
 j = ABS(Mesh%cells(i)%FaceList(j_))

 DO k_=1,SIZE(Mesh%faces(j)%VertList)

  !get target
  k = ABS(Mesh%faces(j)%VertList(k_))

  !add to set
  CALL ADD_TO_SET(VertSet,k)

 END DO

END DO

CALL FINALIZE_SET(VertSet)

!!--end--
END SUBROUTINE




SUBROUTINE GET_CellVertBoundaryList(Mesh,i,VertList)
!!#### PURPOSE
!! Return a pointer to a list of vertices that belong
!! to a cell, optionally including face centers in that list.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh) ,INTENT(IN) :: Mesh
INTEGER         ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: VertList(:)

!!#### LOCAL VARIABLES
INTEGER :: j_,j,k_,k

!!--begin--

!initialize
!should not assume anything about input state
NULLIFY(VertList)
!IF( ASSOCIATED(VertList) )THEN
! VertList = Error(1)
!ELSE
 ALLOCATE( VertList(8) )
 VertList = Error(1)
!END IF

!loop over faces
DO j_=1,SIZE( Mesh%cells(i)%FaceList )

 !get target
 j = Mesh%cells(i)%FaceList(j_)

 IF( j>0 )THEN
  DO k_=1,SIZE(Mesh%faces(j)%VertList)
   k = Mesh%faces(j)%VertList(k_)
   IF( .NOT.ANY(VertList==k) )THEN
    CALL ADD_TO_LIST(VertList,k)
   END IF
  END DO
 ELSE
  DO k_=SIZE(Mesh%faces(abs(j))%VertList),1,-1
   k = Mesh%faces(abs(j))%VertList(k_)
   IF( .NOT.ANY(VertList==k) )THEN
    CALL ADD_TO_LIST(VertList,k)
   END IF
  END DO
 END IF

END DO

CALL FINALIZE_LIST(VertList)

!!--end--
END SUBROUTINE



SUBROUTINE GET_CellSetFromVert(Mesh,k,CellSet)
!!#### PURPOSE
!! Return a pointer to a list of Cells that have
!! a single vertex as a component.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh) ,INTENT(IN) :: Mesh
INTEGER         ,INTENT(IN) :: k

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: CellSet(:)

!!#### LOCAL VARIABLES
INTEGER :: i,j,j_

!!--begin--

!initialize
IF( .NOT.ASSOCIATED(CellSet) )THEN
 ALLOCATE( CellSet(8) )
END IF
CellSet = Error( CellSet(1) )

!loop over cells
DO i=1,NUM_Cells(Mesh)
 DO j_=1,SIZE(Mesh%Cells(i)%FaceList)
  j = ABS(Mesh%Cells(i)%FaceList(j_))
  IF( ANY(Mesh%Faces(j)%VertList==k) )THEN
   CALL ADD_TO_SET( CellSet , i )
  END IF
 END DO
END DO

CALL FINALIZE_SET(CellSet)

!!--end--
END SUBROUTINE



SUBROUTINE RENAME_Cells(Mesh)
!!#### PURPOSE
!! Rename cells according to a simple rule.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh) ,INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--

DO i=1,NUM_Cells(Mesh)
 Mesh%CellLabels(i) = "c"//TRIM(STR(i,"(I)"))
END DO

!!--end--
END SUBROUTINE


SUBROUTINE RENAME_Faces(Mesh)
!!#### PURPOSE
!! Rename faces according to a simple rule.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh) ,INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
INTEGER :: j

!!--begin--

DO j=1,NUM_Faces(Mesh)
 Mesh%FaceLabels(j) = "f"//TRIM(STR(j,"(I)"))
END DO

!!--end--
END SUBROUTINE

SUBROUTINE RENAME_Verts(Mesh)
!!#### PURPOSE
!! Rename verts according to a simple rule.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh) ,INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
INTEGER :: k

!!--begin--

DO k=1,NUM_Verts(Mesh)
 WRITE(Mesh%VertLabels(k),'(a1,i0)')'v',k
 !old slow way
 !Mesh%VertLabels(k) = "v"//TRIM(STR(k,"(I)"))
END DO

!!--end--
END SUBROUTINE


SUBROUTINE RENAME_Mesh(Mesh)
!!#### PURPOSE
!! Rename mesh according to a simple rule.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh) ,INTENT(INOUT) :: Mesh

!!--begin--

CALL RENAME_Cells(Mesh)
CALL RENAME_Faces(Mesh)
CALL RENAME_Verts(Mesh)

!!--end--
END SUBROUTINE




SUBROUTINE TriangulateCell_2D( Mesh , i , N , Tr , &
  IncludeCellCenters , VertIndexTr )

!!#### PURPOSE
!! Return a pointer of triangle verts for the
!! two-dimensional cell.
!! + edge-verts only (by Delaunay)
!! + edge-verts + cell-center-vert (unique).

USE SUB_Swap                                                         !!((04-A-SUB_Swap.f90))

!!#### NOTES
!! Deallocate the pointer when you are done to avoid
!! memory leaks.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh) ,INTENT(IN) :: Mesh
INTEGER         ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
INTEGER       ,INTENT(OUT) :: N
REAL(KIND_MSH),POINTER     :: Tr(:,:,:)

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: IncludeCellCenters
INTEGER,OPTIONAL,POINTER    :: VertIndexTr(:,:)

!!#### LOCAL VARIABLES
INTEGER :: m,k,j,k1,k2,Nv,t
INTEGER,POINTER :: VertList(:)
REAL(KIND_MSH),ALLOCATABLE :: Pg(:,:)
LOGICAL :: IncludeCellCenters_
INTEGER,ALLOCATABLE :: Tr0(:,:)

!!--begin--

!need this to initialize vertlist
NULLIFY(VertList)

!initialize pointers
CALL CLEARn(Tr)
IF( PRESENT(VertIndexTr) )THEN
 CALL CLEARn(VertIndexTr)
END IF


IncludeCellCenters_ = DEFAULT( .FALSE. , IncludeCellCenters )


!make triangles using the centers
IF( IncludeCellCenters_ )THEN

 N = NUM_Faces(Mesh%Cells(i))

 ALLOCATE( Tr(2,3,N) )
 IF( PRESENT(VertIndexTr) )THEN
  ALLOCATE( VertIndexTr(3,N) )
 END IF

 DO m=1,N
  j = Mesh%Cells(i)%FaceList(m)
  Nv = SIZE(Mesh%Faces(abs(j))%VertList)
  k1 = Mesh%Faces(abs(j))%VertList(1)
  k2 = Mesh%Faces(abs(j))%VertList(Nv)

  IF( j<0 )THEN
   CALL Swap(k1,k2)
  END IF

  IF( PRESENT(VertIndexTr) )THEN
   VertIndexTr(1,m) = k1
   VertIndexTr(2,m) = k2
   VertIndexTr(3,m) = 0
  END IF

  Tr(:,1,m) = Vert(Mesh,k1)
  Tr(:,2,m) = Vert(Mesh,k2)
  Tr(:,3,m) = CellCentroid(Mesh,i)
 END DO

!make triangles with triangulation of boundary verts
ELSE

 !new
 CALL GET_CellVertBoundaryList(Mesh,i,VertList)
 Nv = SIZE(VertList)
 ALLOCATE( Pg(2,Nv) )
 DO m=1,Nv
  Pg(:,m) = Vert(Mesh,VertList(m))
 END DO

 !allocate space for the triangularization
 N = Nv-2
 ALLOCATE( Tr0(1:3,1:N) )
 ALLOCATE( Tr(1:2,1:3,1:N) )
 IF( PRESENT(VertIndexTr) )THEN
  ALLOCATE( VertIndexTr(3,N) )
 END IF

 !get the triangulation ordering
 Tr0 = xyTRIANGULATE_Pg(Nv,Pg)
 !get triangles in terms of original verts
 DO t=1,N
  DO m=1,3
   k = VertList( Tr0(m,t) )
   IF( PRESENT(VertIndexTr) )THEN
    VertIndexTr(m,t) = k
   END IF
   Tr(:,m,t) = Vert(Mesh,k)
  END DO
 END DO

 !deallocate
 DEALLOCATE(VertList,Tr0,Pg)

END IF

!!--end--
END SUBROUTINE


FUNCTION IsBoundaryFace( Mesh , j , jd )
!PURPOSE
!! Return whether (according to FINALIZED Mesh <Mesh>) the
!! face j is on the domain, face jd.  If jd is not provided,
!! then a check is performed if j is on ANY domain face.

!!#### USAGE
!
!!  IsBoundaryFace( Mesh , j , jd )   :   face j is on domain-face jd
!!  IsBoundaryFace( Mesh , j )        :   face j is on a domain-face

!!#### UPDATED
!! 31.2006

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="IsBoundaryFace"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: jd

!!#### REQUIRED OUTPUT
LOGICAL :: IsBoundaryFace

!!#### LOCAL VARIABLES
INTEGER :: jd_start,jd_finish,jd_
TYPE(varying_string) :: VS
! INTEGER,SAVE :: counter=0
!!--begin--
! counter=counter+1
! IF( MOD(counter,1000)==0 )THEN
!   WRITE(*,*)'counter=',counter
! END IF

!! Warn if the Mesh isn't finalized.
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"


IF( ASSOCIATED(Mesh%faces(j)%DomainFace) )THEN
 IF( PRESENT(jd) )THEN
  IsBoundaryFace = Mesh%faces(j)%DomainFace==jd
 ELSE
  IsBoundaryFace = Mesh%faces(j)%DomainFace>0
 END IF
ELSE
 DO jd_=1,NUM_DomainFaces(Mesh)
  IsBoundaryFace = EVAL_IsBoundaryFace_Ps( Mesh , j , jd_ )
  IF( IsBoundaryFace )EXIT
 END DO
!  IF( .NOT.ASSOCIATED(Mesh%faces(j)%DomainFace) )THEN
!   ALLOCATE(Mesh%faces(j)%DomainFace)
!  END IF
!  Mesh%faces(j)%DomainFace=0
!  IF( IsBoundaryFace )THEN
!   Mesh%faces(j)%DomainFace=jd_
!  END IF
END IF

!!--end--
END FUNCTION


PURE SUBROUTINE SETUP_DomainFace( Mesh , j )
!!#### PURPOSE
!! Setup the faces used in the domain representation array.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <j>
INTEGER       ,INTENT(IN) :: j

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Mesh%Faces(j)

IF( ASSOCIATED(Face%DomainFace) )THEN
 DEALLOCATE( Face%DomainFace )
END IF

ALLOCATE( Face%DomainFace )
Face%DomainFace = Error(Face%DomainFace)

Face => NULL()

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_DomainFace( Mesh , j )
!!#### PURPOSE
!! Update the domain face for face j.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: j

!!--begin--
Mesh%Faces(j)%DomainFace = EVAL_DomainFace( Mesh , j )

!!--end--
END SUBROUTINE


FUNCTION DomainFace( Mesh , j )
!PURPOSE
!! Return the domain face that j is on.

!!#### USAGE
!
!!  jd = DomainFace( Mesh , j )

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="DomainFace"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
INTEGER :: DomainFace

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
!! Warn if the Mesh isn"t finalized.
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

IF( ASSOCIATED(Mesh%faces(j)%DomainFace) )THEN
 DomainFace = Mesh%faces(j)%DomainFace
ELSE
 DomainFace = EVAL_DomainFace(Mesh,j)
END IF

!!--end--
END FUNCTION


FUNCTION EVAL_DomainFace( Mesh , j ) RESULT(DomainFace)
!PURPOSE
!! Return the domain face that j is on.

!!#### USAGE
!
!  jd = DomainFace( Mesh , j )

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="BoundaryFace"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
INTEGER :: DomainFace

!!#### LOCAL VARIABLES
INTEGER :: jb_
LOGICAL :: IsBoundaryFace

!!--begin--
DomainFace = 0
DO jb_=1,NUM_DomainFaces(Mesh)

 !! Note the OR operation because we may be looping
 !! over multiple faces used in the domain representation, in which case we just want to know if
 !! any of them are true.
 SELECT CASE( Mesh%domain%faces(jb_)%FaceShape )

   CASE( Ps_ )
    IsBoundaryFace = EVAL_IsBoundaryFace_Ps( Mesh , j , jb_ )

   CASE DEFAULT
    !WRITE(*,*)"Error in FaceShape spec in <EVAL_DomainFace>."

 END SELECT

 !don"t need to check any more
 IF( IsBoundaryFace )THEN
  DomainFace = jb_
  EXIT
 END IF
END DO

!!--end--
END FUNCTION


PURE SUBROUTINE WRAPUP_DomainFace( Mesh , j )
!!#### PURPOSE
!! Wrapup the domain face for face j.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to remove domain face component <j>
INTEGER       ,INTENT(IN) :: j

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Mesh%Faces(j)

IF( ASSOCIATED(Face%DomainFace) )THEN
 DEALLOCATE( Face%DomainFace )
 Face%DomainFace => NULL()
END IF

Face => NULL()

!!--end--
END SUBROUTINE


FUNCTION EVAL_IsBoundaryFace_Ps( Mesh , j , jb ) RESULT(IsBoundaryFace)
!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="EVAL_IsBoundaryFace_Ps"

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j
INTEGER       ,INTENT(IN) :: jb

!!#### REQUIRED OUTPUT
LOGICAL :: IsBoundaryFace

!!#### LOCAL VARIABLES
INTEGER      ,POINTER :: VertList(:)
INTEGER               :: k
REAL(KIND_MSH),POINTER :: SDIST(:)
REAL(KIND_MSH)         :: Pn(1:Mesh%NDim+1)

!!--begin--
!! The Faceshape of face j and boundary-face jb must be the same
!! or the face is NOT on the boundary
IF( Mesh%faces(j)%Faceshape==Ps_ )THEN
 Pn = xyPLANE_PV( Mesh%domain%faces(jb)%FaceCentroid , &
                  Mesh%domain%faces(jb)%FaceNormal )

 !get the list of vertices that forms the face
 ALLOCATE( VertList(1:SIZE(Mesh%Faces(j)%VertList)) )
 VertList = Mesh%faces(j)%VertList

 !allocate signed distances array
 ALLOCATE( SDIST(1:SIZE(VertList)) )

 !calculate signed distances from vertices on the face to the plane
 !representation of the boundary face
 DO k=1,SIZE(SDIST)
  SDIST(k) = xySDIST_PnP( Pn , Mesh%verts(:,VertList(k)) )
 END DO

 !if all signed distances are less than some small quantity,
 !then the face j is on the boundary jb
 IsBoundaryFace = ALL(ABS(SDIST)<SQRT(EPSILON(SDIST)))

 !deallocate and nullify
 DEALLOCATE( SDIST )
 DEALLOCATE( VertList )


ELSE
 IsBoundaryFace = .FALSE.
END IF

!!--end--
END FUNCTION


SUBROUTINE OrientInterior( Mesh )
!!#### PURPOSE
!! For CellBased Meshes ONLY!

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
LOGICAL :: reverse_flag(1:NUM_Faces(Mesh))

!!--begin--
IF( IsCellBased(Mesh) )THEN

 reverse_flag = IsNegInteriorFace(Mesh)

 CALL ReverseFaces(Mesh,reverse_flag)

ELSE
 WRITE(*,*)"WARNING: <OrientInterior> may only be used &
   &for CellBased Meshes!  Doing nothing."
END IF

!!--end--
END SUBROUTINE



FUNCTION IsNegInteriorFace(Mesh) RESULT(MASK)
!!#### PURPOSE
!! Return a mask with values of <.TRUE.> for
!! all faces which have negative entries in
!! at least one cell.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
LOGICAL :: MASK(1:NUM_Faces(Mesh))

!!#### LOCAL VARIABLES
INTEGER :: jb,j,j_,i

!!--begin--
!initialize mask
MASK = .FALSE.

!determine the negative interior faces
DO i=1,NUM_Cells(Mesh)
 DO j_=1,NUM_Faces(Mesh%Cells(i))
  j = Mesh%Cells(i)%FaceList(j_)
  IF( .NOT.IsBoundaryFace(Mesh,ABS(j)) )THEN
   IF( j<0 )THEN
    !set mask
    MASK(ABS(j)) = .TRUE.
   END IF
  END IF
 END DO
END DO

!!--end--
END FUNCTION


SUBROUTINE OrientBoundary( Mesh )
!!#### PURPOSE
!! Orient the boundary faces (those on the domain) by
!! flipping their direction to be outward normals.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: dp
INTEGER :: jb,j
LOGICAL :: reverse_flag(1:NUM_Faces(Mesh))

!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: Noisy_Unit_=0

!!--begin--
reverse_flag = .FALSE.
DO jb=1,NUM_DomainFaces(Mesh)
 DO j=1,NUM_Faces(Mesh)

  IF( IsBoundaryFace(Mesh,j,jb) )THEN

   !get dot product
   dp = xyDOT_VV( Mesh%Domain%Faces(jb)%FaceNormal , &
                  Mesh       %Faces(j )%FaceNormal )

   !noisy output
   IF( Noisy_Unit_/=0 )THEN
    write(Noisy_Unit_,*)"jb=",jb
    write(Noisy_Unit_,*)"j =",j
    write(Noisy_Unit_,*)"dp=",dp
   END IF

   IF( dp<0._KIND_MSH )THEN

    !reverse faces in the cell list by changing sign
    reverse_flag(j) = .TRUE.

    !(/waw/) do not do this step because we instead need to recreate
    !the cells themselves and a search over all cells for
    !certain faces in here would be wasteful, thus orient boundary
    !must come before the final facelist creation for cells
   END IF

  END IF

 END DO
END DO

!! Reverse orientation direction of faces.
CALL ReverseFaces(Mesh,reverse_flag)

!!--end--
END SUBROUTINE




SUBROUTINE ReverseFaces(Mesh,reverse_flag)
!!#### PURPOSE
!! Reverse all the flagged faces by reversing their
!! <VertList> and changing the direction of their <FaceNormal>,
!! along with changing their direction (sign)
!! in the cells <FaceList>

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
LOGICAL       ,INTENT(IN)    :: reverse_flag(1:NUM_Faces(Mesh))

!!#### LOCAL VARIABLES
INTEGER :: j,i,j_

!!--begin--
DO j=1,NUM_Faces(Mesh)
 !cycle if we are not supposed to reverse this face
 IF( .NOT.reverse_flag(j) )CYCLE

 !reverse vertlist
 Mesh%Faces(j)%VertList = Reverse(Mesh%Faces(j)%VertList)

 !reverse face surface normal
 Mesh%Faces(j)%FaceNormal = -Mesh%Faces(j)%FaceNormal

 DO i=1,NUM_Cells(Mesh)
  DO j_=1,SIZE(Mesh%Cells(i)%FaceList)
   IF( ABS(Mesh%Cells(i)%FaceList(j_))==j )THEN
    Mesh%Cells(i)%FaceList(j_) = -Mesh%Cells(i)%FaceList(j_)
   END IF
  END DO
 END DO

END DO

!!--end--
END SUBROUTINE



SUBROUTINE SPLIT_Mesh_Pg( Mesh , N , Pg , map_cells , CookieCutter )
!!#### PURPOSE
!! Split a Mesh according to a polygon and also remap an
!! index belonging to each cell.

!!#### DEPENDENCIES
USE TBX_ComputationalGeometry                                        !!((09-A-TBX_ComputationalGeometry.f90))

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
INTEGER       ,INTENT(IN)    :: N
REAL(KIND_MSH) ,INTENT(IN)    :: Pg(:,:)

!!#### OPTIONAL INPUT/OUTPUT
INTEGER,OPTIONAL,INTENT(INOUT),POINTER :: map_cells(:)

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: CookieCutter

!!#### LOCAL VARIABLES
INTEGER :: i,j,k1,k2,a,i2
INTEGER,POINTER :: ip(:),jp(:),kp(:)
REAL(KIND_MSH),DIMENSION(Mesh%NDim)   :: Vp
REAL(KIND_MSH),DIMENSION(Mesh%NDim+1) :: Pn
REAL(KIND_MSH) :: SAREA
LOGICAL :: CookieCutter_
CHARACTER(*),PARAMETER :: proc_= "SPLIT_Mesh_Pg"
LOGICAL,PARAMETER :: Noisy_=.FALSE.
!!--begin--
!initialize
NULLIFY( ip,jp,kp)

IF( PRESENT(CookieCutter) )THEN
 CookieCutter_ = CookieCutter
ELSE
 CookieCutter_ = .FALSE.
END IF

!for each face of the polygon
DO j=1,n

 !get the two vertices
 k1 = j
 k2 = MOD(k1,n) + 1

 !form the perpendicular of the face
 Vp = xyPERPCCW_V( Pg(:,k2) - Pg(:,k1) )

 !the plane to split cells based on one of the points and the perp
 Pn = xyPLANE_PV( Pg(:,k1) , Vp )

 IF( .NOT.CookieCutter_ )THEN

  !split the entire Mesh along the plane
  CALL SPLIT_Mesh_Pn( Mesh , Pn , map_cells )

 ELSE

  IF( Mesh%NDim==2 )THEN
   !split the Mesh along the same plane but only if the edges below intersect faces
   CALL SPLIT_Mesh_Pn( Mesh , Pn , map_cells , &
     Ps_interior=RESHAPE( (/Pg(:,k1),Pg(:,k2)/) , (/2,2/) ) )
  ELSE
   STOP "Error"
  END IF

 END IF

END DO

END SUBROUTINE


SUBROUTINE SPLIT_Mesh_Pn( Mesh , Pn , map_cells , P_interior , Ps_interior )
!!#### PURPOSE
!! Split a Mesh with a plane and also remap an
!! index belonging to each cell.

!!#### DEPENDENCIES
USE TBX_ComputationalGeometry                                        !!((09-A-TBX_ComputationalGeometry.f90))

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_MSH) ,INTENT(IN)    :: Pn(Mesh%NDim+1)

!!#### OPTIONAL INPUT
!! * only split a cell if the point is in the interior
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: P_interior(1:Mesh%NDim)
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: Ps_interior(:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * cells array to remap
INTEGER,OPTIONAL,POINTER,INTENT(INOUT) :: map_cells(:)

!!#### LOCAL VARIABLES
INTEGER :: i
INTEGER,POINTER :: ip(:),jp(:),kp(:)
LOGICAL :: Split
CHARACTER(*),PARAMETER :: proc_= "SPLIT_Mesh_Pn"
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!initialize
NULLIFY(ip,jp,kp)

!split all the appropriate cells
i = 0
IF( Noisy_ )WRITE(*,*)"Enter the splitting loop."
DO i=NUM_Cells(Mesh),1,-1

 !split the cell
 IF( Noisy_ )WRITE(*,*)"trying to split cell i=",i
 Split = SPLIT_Cell( Mesh , i , Pn , ip , jp , kp , map_cells , &
   P_interior=P_interior , Ps_interior=Ps_interior )
 IF( Noisy_ )THEN
  IF( ASSOCIATED(ip) ) WRITE(*,*)" return cell-list, ip=",ip
  IF( ASSOCIATED(jp) ) WRITE(*,*)" return face-list, jp=",jp
  IF( ASSOCIATED(kp) ) WRITE(*,*)" return vert-list, kp=",kp
 END IF

 !refinalize the Mesh
 IF( Noisy_ )WRITE(*,*)"finalizing Mesh"
 CALL Finalize( Mesh )

 !if we have a split cell
 IF( Split )THEN
  IF( Noisy_ )THEN
   WRITE(*,*)"* cell i=",i
   WRITE(*,*)"  * ip=",ip
   WRITE(*,*)"  * jp=",jp
   WRITE(*,*)"  * kp=",kp
  END IF
 END IF

END DO

!!--end--
END SUBROUTINE



SUBROUTINE DESTROY_Mesh(Mesh)
!PURPOSE
!! Destroys the Mesh (same as wrapup---this routine
!! should be removed).
!ARGUMENTS
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!--begin--
CALL DEALLOCATE_Mesh( Mesh )
!!--end--

END SUBROUTINE



PURE FUNCTION IsQg_Mesh( Mesh , i ) RESULT(IsQg)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: i
!!#### REQUIRED OUTPUT
LOGICAL :: IsQg
!!--begin--
IsQg = ANY(Mesh%Cells(i)%CellShape==(/Qs_,Qr_,Qg_/))
!!--end--
END FUNCTION


PURE FUNCTION IsPg_Mesh( Mesh , i ) RESULT(IsPg)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: i
!!#### REQUIRED OUTPUT
LOGICAL :: IsPg
!!--begin--
IsPg = ANY(Mesh%Cells(i)%CellShape==(/Tr_,Qs_,Qr_,Qg_,Pg_/))
!!--end--
END FUNCTION


PURE FUNCTION IsTr_Mesh( Mesh , i ) RESULT(IsTr)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: i
!!#### REQUIRED OUTPUT
LOGICAL :: IsTr
!!--begin--
IsTr = Mesh%Cells(i)%CellShape==Tr_
!!--end--
END FUNCTION


PURE FUNCTION IsQr_Mesh( Mesh , i ) RESULT(IsQr)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: i
!!#### REQUIRED OUTPUT
LOGICAL :: IsQr
!!--begin--
IsQr = ANY(Mesh%Cells(i)%CellShape==(/Qs_,Qr_/))
!!--end--
END FUNCTION


PURE FUNCTION IsQs_Mesh( Mesh , i ) RESULT(IsQs)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: i
!!#### REQUIRED OUTPUT
LOGICAL :: IsQs
!!--begin--
IsQs = Mesh%Cells(i)%CellShape==Qs_
!!--end--
END FUNCTION



!!### PURE FUNCTION <<EVAL_FaceNormal_Mesh>>
PURE FUNCTION EVAL_FaceNormal_Mesh( Mesh , j ) RESULT(FaceNormal)

!!#### PURPOSE
!! Calculate the appropriate normal for a face from
!! library routines based on the shape.

!!#### DETAILS
!! Direction is NOT FLIPPED (out of bounds if j<0).

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * face index to evaluate normal of <j>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceNormal(1:Mesh%NDim)

!!#### LOCAL VARIABLES
INTEGER :: k1,k2

!!--begin--
SELECT CASE( Mesh%Faces(j)%FaceShape )

 CASE( Ps_ )
  SELECT CASE(Mesh%NDim)
   CASE(2)
    k1 = Mesh%Faces(j)%VertList(1)
    k2 = Mesh%Faces(j)%VertList(SIZE(Mesh%Faces(j)%VertList))
    FaceNormal = xyPERPCW_U( xyDIRECTION_PP( Mesh%verts(:,k1) , Mesh%verts(:,k2) )  )
  END SELECT

END SELECT

!!--end--
END FUNCTION


!!### PURE SUBROUTINE <<SETUP_FaceNormal_Mesh>>
PURE SUBROUTINE SETUP_FaceNormal_Mesh( Mesh , j )
!!#### PURPOSE
!! Setup the face normal for face j.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <j>
INTEGER       ,INTENT(IN) :: j

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Mesh%Faces(j)

IF( ASSOCIATED(Face%FaceNormal) )THEN
 DEALLOCATE( Face%FaceNormal )
END IF

ALLOCATE( Face%FaceNormal(1:Mesh%NDim) )
Face%FaceNormal = Error(Face%FaceNormal)

Face => NULL()

!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE <<UPDATE_FaceNormal_Mesh>>
PURE SUBROUTINE UPDATE_FaceNormal_Mesh( Mesh , j )

!!#### PURPOSE
!! Update the face normal for face j.

!!#### DETAILS
!! Direction is NOT FLIPPED (out of bounds if j<0).

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <j>
INTEGER       ,INTENT(IN) :: j

!!--begin--
Mesh%Faces(j)%FaceNormal = EVAL_FaceNormal_Mesh( Mesh , j )

!!--end--
END SUBROUTINE


!!### PURE FUNCTION <<FaceNormal_Mesh>>
PURE FUNCTION FaceNormal_Mesh( Mesh , j )  RESULT(FaceNormal)
!!#### PURPOSE
!! Return the appropriate normal for a face, using
!! the data contained in Mesh if available.

!!#### DETAILS
!! Direction is flipped if j<0.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * face index to evaluate normal of <j>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceNormal(1:Mesh%NDim)

!!--begin--
IF( .NOT.ASSOCIATED(Mesh%Faces(ABS(j))%FaceNormal) )THEN
 FaceNormal = EVAL_FaceNormal_Mesh( Mesh , ABS(j) )
ELSE
 FaceNormal = Mesh%Faces(ABS(j))%FaceNormal
END IF

!flipping
FaceNormal = FaceNormal*j/ABS(j)

!!--end--
END FUNCTION




PURE FUNCTION DomainFaceNormal_Mesh( Mesh , jd )  RESULT(DomainFaceNormal)
!!#### PURPOSE
!! Return the appropriate normal for a domain face, and
!! an error if it doesn't exist.


!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * domain face index to evaluate normal of <j>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: jd

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: DomainFaceNormal(1:Mesh%NDim)

!!--begin--
!initialize
DomainFaceNormal = Error(DomainFaceNormal)

!cascading association conditions
IF( .NOT.ASSOCIATED(Mesh%Domain) )RETURN
IF( .NOT.ASSOCIATED(Mesh%Domain%Faces) )RETURN
IF( .NOT.ASSOCIATED(Mesh%Domain%Faces(jd)%FaceNormal) )RETURN

!calculation
DomainFaceNormal = Mesh%Domain%Faces(jd)%FaceNormal

!!--end--
END FUNCTION

!!### SUBROUTINE <<SCALE_Mesh>>
SUBROUTINE SCALE_Mesh(Mesh,scale)

!!#### PURPOSE
!! Scale each vert in the mesh by a factor.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: scale(Mesh%Ndim)
INTEGER :: Nk
!!--begin--

!CALL PRINT_Mesh(Mesh)
CALL Finalize(Mesh)
Nk=NUM_Verts(Mesh)

SELECT CASE(Mesh%NDim)
 CASE(1)
   Mesh%Verts(1,1:Nk) = Mesh%Verts(1,1:Nk)*scale(1)
   Mesh%Domain%Verts(1,1:2) = Mesh%Domain%Verts(1,1:2)*scale(1)
 CASE(2)
   Mesh%Verts(1,1:Nk) = Mesh%Verts(1,1:Nk)*scale(1)
   Mesh%Verts(2,1:Nk) = Mesh%Verts(2,1:Nk)*scale(2)
   Mesh%Domain%Verts(1,1:4) = Mesh%Domain%Verts(1,1:4)*scale(1)
   Mesh%Domain%Verts(2,1:4) = Mesh%Domain%Verts(2,1:4)*scale(2)
 CASE(3)
   Mesh%Verts(1,1:Nk) = Mesh%Verts(1,1:Nk)*scale(1)
   Mesh%Verts(2,1:Nk) = Mesh%Verts(2,1:Nk)*scale(2)
   Mesh%Verts(3,1:Nk) = Mesh%Verts(3,1:Nk)*scale(3)
   Mesh%Domain%Verts(1,1:8) = Mesh%Domain%Verts(1,1:8)*scale(1)
   Mesh%Domain%Verts(2,1:8) = Mesh%Domain%Verts(2,1:8)*scale(2)
   Mesh%Domain%Verts(3,1:8) = Mesh%Domain%Verts(3,1:8)*scale(3)
 CASE DEFAULT
   WRITE(*,*)"Invalid Case in TBX_Mesh::SCALE_Mesh"
   STOP
END SELECT

!update the domain first or the boundary faces for the mesh
!will be wrong [waw]
CALL UPDATE_Mesh_Domain(Mesh)
CALL UPDATE_Mesh(Mesh,UpdateAll=.TRUE.)

!!--end--
END SUBROUTINE

PURE FUNCTION DomainFaceCentroid_Mesh( Mesh , jd )  RESULT(DomainFaceCentroid)
!!#### PURPOSE
!! Return the appropriate centroid for a domain face, and
!! an error if it doesn't exist.


!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * domain face index to evaluate normal of <j>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: jd

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: DomainFaceCentroid(1:Mesh%NDim)

!!--begin--
!initialize
DomainFaceCentroid = Error(DomainFaceCentroid)

!cascading association conditions
IF( .NOT.ASSOCIATED(Mesh%Domain) )RETURN
IF( .NOT.ASSOCIATED(Mesh%Domain%Faces) )RETURN
IF( .NOT.ASSOCIATED(Mesh%Domain%Faces(jd)%FaceNormal) )RETURN

!calculation
DomainFaceCentroid = Mesh%Domain%Faces(jd)%FaceCentroid

!!--end--
END FUNCTION



PURE SUBROUTINE WRAPUP_FaceNormal_Mesh( Mesh , j )
!!#### PURPOSE
!! Wrapup the face normal for face j.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <j>
INTEGER       ,INTENT(IN) :: j

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Mesh%Faces(j)

IF( ASSOCIATED(Face%FaceNormal) )THEN
 DEALLOCATE( Face%FaceNormal )
 Face%FaceNormal => NULL()
END IF

Face => NULL()

!!--end--
END SUBROUTINE




PURE FUNCTION EVAL_FaceNormal_Domain( Domain , jd ) RESULT(FaceNormal)
!!#### PURPOSE
!! Calculate the appropriate normal for a face from
!! library routines based on the shape.

!!#### DETAILS
!! Direction is NOT FLIPPED (out of bounds if jd<0).

!!#### REQUIRED INPUT
!! * Domain type <Domain>
!! * face index to evaluate normal of <jd>
TYPE(TYPE_Domain),INTENT(IN) :: Domain
INTEGER         ,INTENT(IN) :: jd

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceNormal(1:Domain%NDim)

!!#### LOCAL VARIABLES
INTEGER :: k1,k2

!!--begin--
SELECT CASE( Domain%Faces(jd)%FaceShape )

 CASE( Ps_ )
  SELECT CASE(Domain%NDim)
   CASE(2)
    k1 = Domain%Faces(jd)%VertList(1)
    k2 = Domain%Faces(jd)%VertList(SIZE(Domain%Faces(jd)%VertList))
    FaceNormal = xyPERPCW_U( xyDIRECTION_PP( Domain%verts(:,k1) , Domain%verts(:,k2) )  )
  END SELECT

END SELECT

!!--end--
END FUNCTION


PURE SUBROUTINE SETUP_FaceNormal_Domain( Domain , jd )
!!#### PURPOSE
!! Setup the face normal for face jd.

!!#### REQUIRED INPUT/OUTPUT
!! * Domain type <Domain>
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <jd>
INTEGER       ,INTENT(IN) :: jd

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Domain%Faces(jd)

IF( ASSOCIATED(Face%FaceNormal) )THEN
 DEALLOCATE( Face%FaceNormal )
END IF

ALLOCATE( Face%FaceNormal(1:Domain%NDim) )
Face%FaceNormal = Error(Face%FaceNormal)

Face => NULL()

!!--end--
END SUBROUTINE


PURE SUBROUTINE UPDATE_FaceNormal_Domain( Domain )
!!#### PURPOSE
!! Update the face normal for face jd.

!!#### DETAILS
!! Direction is NOT FLIPPED (out of bounds if jd<0).

!!#### REQUIRED INPUT/OUTPUT
!! * Domain type <Domain>
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### LOCAL VARIABLES
!! * face index to evaluate normal of <jd>
INTEGER :: jd

!!--begin--

DO jd=1,SIZE(Domain%Faces)
 Domain%Faces(jd)%FaceNormal = EVAL_FaceNormal_Domain( Domain , jd )
END DO

!!--end--
END SUBROUTINE


PURE FUNCTION FaceNormal_Domain( Domain , jd ) RESULT(FaceNormal)
!!#### PURPOSE
!! Return the appropriate normal for a face, using
!! the data contained in Domain if available.

!!#### DETAILS
!! Direction is flipped if jd<0.

!!#### REQUIRED INPUT
!! * Domain type <Domain>
!! * face index to evaluate normal of <jd>
TYPE(TYPE_Domain),INTENT(IN) :: Domain
INTEGER       ,INTENT(IN) :: jd

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceNormal(1:Domain%NDim)

!!--begin--
IF( jd==0 )THEN
 RETURN
END IF

IF( .NOT.ASSOCIATED(Domain%Faces(jd)%FaceNormal) )THEN
 FaceNormal = EVAL_FaceNormal_Domain( Domain , ABS(jd) )
ELSE
 FaceNormal = Domain%Faces(jd)%FaceNormal
END IF

!flipping
FaceNormal = FaceNormal*jd/ABS(jd)

!!--end--
END FUNCTION


PURE SUBROUTINE WRAPUP_FaceNormal_Domain( Domain , jd )
!!#### PURPOSE
!! Wrapup the face normal for face jd.

!!#### REQUIRED INPUT/OUTPUT
!! * Domain type <Domain>
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <jd>
INTEGER       ,INTENT(IN) :: jd

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Domain%Faces(jd)

IF( ASSOCIATED(Face%FaceNormal) )THEN
 DEALLOCATE( Face%FaceNormal )
 Face%FaceNormal => NULL()
END IF

Face => NULL()

!!--end--
END SUBROUTINE


PURE FUNCTION FaceArea( Mesh , j )
!!#### PURPOSE
!! Return the appropriate face area for a face, using
!! the data contained in Mesh if available.

!!#### USAGE NOTES
!! If the <FaceArea> will be needed frequently, you should
!! first <SETUP_FaceArea>, then <UPDATE_FaceArea> once
!! initially (or whenever the Mesh changes) and then
!! the calls to <FaceArea> will merely be a lookup of
!! a saved value.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * face index to evaluate normal of <j>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceArea

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
IF( .NOT.ASSOCIATED(Mesh%Faces(ABS(j))%FaceArea) )THEN
 FaceArea = EVAL_FaceArea( Mesh , ABS(j) )
ELSE
 FaceArea = Mesh%Faces(ABS(j))%FaceArea
END IF

!!--end--
END FUNCTION



!!### PURE FUNCTION <<EVAL_FaceArea>>
PURE FUNCTION EVAL_FaceArea( Mesh , j ) RESULT(FaceArea)
!!#### PURPOSE
!! Evaluate the appropriate face area for a face from
!! library routines based on the shape and return the
!! result.  <EVAL_FaceArea> is used internally for
!! the <UPDATE_FaceArea> routine.

!!#### DETAILS
!! Using $j<0$ leads to out of bounds.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * face index to evaluate normal of <j>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceArea

!!#### LOCAL VARIABLES
INTEGER :: k1,k2
!!--begin--
SELECT CASE( Mesh%Faces(j)%FaceShape )

 CASE( Ps_ )
  SELECT CASE(Mesh%NDim)
   CASE(2)
    k1 = Mesh%Faces(j)%VertList(1)
    k2 = Mesh%Faces(j)%VertList(SIZE(Mesh%Faces(j)%VertList))
    FaceArea = xyDIST_PP( Mesh%verts(:,k1) , Mesh%verts(:,k2) )
  END SELECT

END SELECT

!!--end--
END FUNCTION



!!### PURE FUNCTION <<EVAL_FaceArea_Domain>>
PURE FUNCTION EVAL_FaceArea_Domain( Domain , jd ) RESULT(FaceArea)
!!#### PURPOSE
!! Evaluate the appropriate face area for a face from
!! library routines based on the shape and return the
!! result.  <EVAL_FaceArea_Domain> is used internally for
!! the <UPDATE_FaceArea_Domain> routine.

!!#### REQUIRED INPUT
!! * Domain face <jd>
TYPE(TYPE_Domain),INTENT(IN) :: Domain
INTEGER          ,INTENT(IN) :: jd

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceArea

!!#### LOCAL VARIABLES
INTEGER :: k1,k2

!!--begin--

SELECT CASE( Domain%Faces(jd)%FaceShape )

 CASE( Ps_ )
  SELECT CASE(Domain%NDim)
   CASE(2)
    k1 = Domain%Faces(jd)%VertList(1)
    k2 = Domain%Faces(jd)%VertList(SIZE(Domain%Faces(jd)%VertList))
    FaceArea = xyDIST_PP( Domain%verts(:,k1) , Domain%verts(:,k2) )
  END SELECT
END SELECT

!!--end--
END FUNCTION


!!### SUBROUTINE <<UPDATE_Mesh>>
SUBROUTINE UPDATE_Mesh( Mesh , ilist , jlist , klist , UpdateAll )
!!#### PURPOSE
!! Update the cells, faces according to changes in
!! cells, faces, or verts.

!!#### DETAILS
!! Out of bounds if j<0.
!! Out of bounds if the <FaceArea> component has not
!! been setup with <SETUP_FaceArea>.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
!! * vert indices that should be updated <ilist>
!! * face indices that should be updated <jlist>
!! * cell indices that should be updated <klist>
INTEGER,OPTIONAL,INTENT(IN) :: ilist(:)
INTEGER,OPTIONAL,INTENT(IN) :: jlist(:)
INTEGER,OPTIONAL,INTENT(IN) :: klist(:)
LOGICAL,OPTIONAL,INTENT(IN) :: UpdateAll

!!#### LOCAL VARIABLES
INTEGER,POINTER :: iset(:)
INTEGER,POINTER :: jset(:)
INTEGER,POINTER :: kset(:)
INTEGER :: n,i,j
LOGICAL :: UpdateAll_
INTEGER,POINTER :: jbdry(:)

!!--begin--
UpdateAll_ = DEFAULT( .FALSE. , UpdateAll )

!!1. setup sets first
kset => NULL()
jset => NULL()
iset => NULL()
jbdry=> NULL()

IF( UpdateAll_ )THEN

 kset => ptr_Sequence( 1 , 1 , NUM_Verts(Mesh) )
 jset => ptr_Sequence( 1 , 1 , NUM_Faces(Mesh) )
 iset => ptr_Sequence( 1 , 1 , NUM_Cells(Mesh) )

ELSE

 !set of verts to update
 IF( PRESENT(klist) )THEN
  DO n=1,SIZE(klist)
   CALL ADD_TO_SET( kset , klist(n) )
  END DO
 END IF
 !set of faces to update
 IF( PRESENT(jlist) )THEN
  DO n=1,SIZE(jlist)
   CALL ADD_TO_SET( jset , ABS(jlist(n)) )
  END DO
 END IF
 !set of cells to update
 IF( PRESENT(ilist) )THEN
  DO n=1,SIZE(ilist)
   CALL ADD_TO_SET( iset , ilist(n) )
  END DO
 END IF

 CALL FINALIZE_SET(kset)

 !add faces to update based on verts to update
 IF( ASSOCIATED(kset) )THEN
  DO j=1,NUM_Faces(Mesh)
   IF( COUNT_Intersection(Mesh%Faces(j)%VertList,kset)>0 )THEN
    CALL ADD_TO_SET( jset , j )
   END IF
  END DO
 END IF

 CALL FINALIZE_SET(jset)

 !add cells to update based on faces to update
 IF( ASSOCIATED(jset) )THEN
  DO i=1,NUM_Cells(Mesh)
   IF( COUNT_Intersection(ABS(Mesh%Cells(i)%FaceList),jset)>0 )THEN
    CALL ADD_TO_SET( iset , i )
   END IF
  END DO
 END IF

 CALL FINALIZE_SET(iset)

END IF


!!2. now update faces
IF( ASSOCIATED(jset) )THEN
 DO n=1,SIZE(jset)
  CALL UPDATE_Face( Mesh , jset(n) )
  CALL UPDATE_DomainFace(Mesh,jset(n))
 END DO
END IF
CALL UPDATE_BoundaryFaces( Mesh , jbdry )
IF( ASSOCIATED(jbdry) )DEALLOCATE( jbdry )


!!3. now update cells
IF( ASSOCIATED(iset) )THEN
 DO n=1,SIZE(iset)
  CALL UPDATE_Cell( Mesh , iset(n) )
 END DO
END IF

IF( ASSOCIATED(kset) )DEALLOCATE( kset )
IF( ASSOCIATED(jset) )DEALLOCATE( jset )
IF( ASSOCIATED(iset) )DEALLOCATE( iset )

IF( UpdateAll_ )THEN
    CALL UPDATE_FaceToCellLink(Mesh)
END IF

!!--end--
END SUBROUTINE





!!### SUBROUTINE <<UPDATE_Mesh_Domain>>
SUBROUTINE UPDATE_Mesh_Domain( Mesh )

!!#### PURPOSE
!! Update the mesh domain.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!--begin--

!!1. update all faces
CALL UPDATE_Face_Domain( Mesh%Domain )

!!2. update the domain cell
CALL UPDATE_Cell_Domain( Mesh%Domain )


!!--end--
END SUBROUTINE








SUBROUTINE WRAPUP_Mesh( Mesh , FdBk )
!!#### PURPOSE
!! Wrapup the Mesh to recover memory.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback type
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!--begin--
!deallocate the Mesh
CALL DEALLOCATE_Mesh(Mesh)

!!--end--
END SUBROUTINE


SUBROUTINE SETUP_Mesh( Mesh , FdBk )
!!#### PURPOSE
!! Setup the Mesh for use.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback type
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!#### INFORMATION
CHARACTER(*),PARAMETER :: proc_="SETUP_Mesh"

!!#### LOCAL VARIABLES
REAL :: dt,tin,tout

!!--begin--
!begin
CALL CPU_TIME(tin)

!! Finalize the Mesh.
CALL UpdateAndDump(fdbk_comment,FdBk,s="[[MSH]] Finalizing the &
  &memory needed for runtime calculations...")
CALL Finalize(Mesh)

!! The boundary must be oriented before cells are compressed.
CALL UpdateAndDump(fdbk_comment,FdBk,s="[[MSH]] Orienting faces on &
  &the domain boundary as outward normals...")
CALL OrientBoundary(Mesh)

!! [Mesh%VertLabels] warning
IF( .NOT.ASSOCIATED(Mesh%VertLabels) )THEN
 CALL UpdateAndDump(fdbk_warning , FdBk , s="[[MSH]] Component [Mesh%VertLabels] &
   & has not been set (not necessary but helpful).")
END IF

!! [Mesh%FaceLabels] warning
IF( .NOT.ASSOCIATED(Mesh%FaceLabels) )THEN
 CALL UpdateAndDump(fdbk_warning , FdBk , s="[[MSH]] Component [Mesh%FaceLabels] &
   & has not been set (not necessary but helpful).")
END IF

!! <Mesh%CellLabels> warning
IF( .NOT.ASSOCIATED(Mesh%CellLabels) )THEN
 CALL UpdateAndDump(fdbk_warning , FdBk , s="[[MSH]] Component [Mesh%CellLabels] &
   & has not been set (not necessary but helpful).")
END IF

!end time
CALL CPU_TIME(tout)
dt = tout - tin

!timing update
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[MSH]] Setup completed in &
  &[time="//STRTIME(dt)//"].")

!!--end--
END SUBROUTINE


SUBROUTINE DEBUG_Mesh( Mesh , DataOutput , VolumeCheck , AreaNormalCheck , OrthoCheck )
!!#### PURPOSE
!! Debugging facility for the Mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
!! * whether to output the verts, face normals, face centroids, and cell centroids <DataOutput>
!! * whether to check if the total volume is the same as the sum of the cells volume <VolumeCheck>
!! * whether to check if the surface area of the total domain is the same as the sum of the
!!   norm of the sum of surface areas vectors of all cells <AreaNormalCheck>
!! * check whether the line between the centers of two cells is not orthogonal to the face
!!   that comes between the two cells <OrthoCheck>
LOGICAL,OPTIONAL,INTENT(IN) :: DataOutput
LOGICAL,OPTIONAL,INTENT(IN) :: VolumeCheck
LOGICAL,OPTIONAL,INTENT(IN) :: AreaNormalCheck
LOGICAL,OPTIONAL,INTENT(IN) :: OrthoCheck

!!--begin--
!output stuff for debugging
IF( PRESENT(DataOutput) )THEN
 IF( DataOutput )THEN
  CALL DAT_Verts(Mesh)
  CALL DAT_FaceNormals(Mesh)
  CALL DAT_FaceCentroids(Mesh)
  CALL DAT_CellCentroids(Mesh)
 END IF
END IF

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_BoundaryFaces(Mesh,Jbdry,Jinte,Jdomn)
!!#### PURPOSE
!! Determine the faces on the boundary <Jbdry> and possibly
!! the interior faces <Jinte> and the domain faces <Jdomn>.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER,POINTER          :: Jbdry(:)

!!#### OPTIONAL OUTPUT
INTEGER,POINTER,OPTIONAL :: Jinte(:),Jdomn(:)

!!#### LOCAL VARIABLES
INTEGER :: j,j1,j2,jd,Nj,Njd,COUNT_Jinte,COUNT_Jbdry

!!--begin--
!initialize
Nj  = NUM_Faces(Mesh)
Njd = NUM_DomainFaces(Mesh)

!allocate
IF( ASSOCIATED(Jbdry) )DEALLOCATE(Jbdry)
ALLOCATE( Jbdry(1:Nj) ) ; Jbdry = 0
IF( PRESENT(Jinte) )THEN
 IF( ASSOCIATED(Jinte) )DEALLOCATE(Jinte)
 ALLOCATE( Jinte(1:Nj) ) ; Jinte = 0
END IF
IF( PRESENT(Jdomn) )THEN
 IF( ASSOCIATED(Jdomn) )DEALLOCATE( Jdomn )
 ALLOCATE( Jdomn(1:Nj) ) ; Jdomn = 0
END IF

!initialize counters
j1 = 0
j2 = 0

!loop over domain faces
DO jd=1,Njd
 !loop over faces
 DO j=1,Nj
  IF( IsBoundaryFace(Mesh,j,jd) )THEN
   Jbdry(j) = j
   IF( PRESENT(Jdomn) )THEN
    Jdomn(j) = jd
   END IF
  END IF
 END DO
END DO

!! Get interior indices.
IF( PRESENT(Jinte) )THEN
 DO j=1,Nj
  IF( Jbdry(j)==0 )Jinte(j) = j
 END DO
END IF

!! Reallocate <Jbdry> to fit exactly
COUNT_Jbdry = COUNT(Jbdry/=0)
IF( COUNT_Jbdry>0 )THEN

 Jbdry(1:COUNT_Jbdry) = PACK(Jbdry,Jbdry/=0)
 CALL Reallocate( Jbdry , COUNT_Jbdry-Nj)

ELSE
 DEALLOCATE( Jbdry )
 Jbdry => NULL()
END IF


!! Reallocate <Jdomn> to fit exactly.
IF( PRESENT(Jdomn) )THEN

 IF( COUNT_Jbdry>0 )THEN

  Jdomn(1:COUNT_Jbdry) = PACK(Jdomn,Jdomn/=0)
  CALL Reallocate( Jdomn , COUNT_Jbdry-Nj )

 ELSE
  DEALLOCATE( Jdomn )
  Jdomn => NULL()
 END IF

END IF

!! Reallocate <Jinte> to fit exactly.
IF( PRESENT(Jinte) )THEN

 COUNT_Jinte = COUNT(Jinte/=0)
 IF( COUNT_Jinte>0 )THEN

  Jinte(1:COUNT_Jinte) = PACK(Jinte,Jinte/=0)
  CALL Reallocate( Jinte , COUNT_Jinte-Nj)

 ELSE
  DEALLOCATE( Jinte )
  Jinte => NULL()
 END IF

END IF

!!--end--
END SUBROUTINE


SUBROUTINE SETUP_Interfaces(Mesh,Jinte,Interfaces,Unit_noisy)
!!#### PURPOSE
!! Determine the interfaces of the Mesh---i.e. the
!! places where two faces overlap.

!!#### COMMENTS
!! * In a CellBased Mesh, there are <Nja> interior faces and
!!   <Njc<=Nja/2> interfaces.
!! * In a Structured Essential Mesh, there are <Nja> interior faces and
!!   <Njc=Nja> interfaces.
!! * The number of interfaces must be found by inquiring
!!   <IsSubFace(Mesh,j1,j2)>.  If <j1> and <j2> are the same face, then
!!   the returned value is <.TRUE.>.  This forces one requirement,
!!   that to construct the interfaces properly in the structured case,
!!   we require that if <j2> is contained in the interface listing
!!   for <j1> (the interface where component <%master==j1>) then
!!   there is no interface allowed for <j2>!

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,POINTER :: Jinte(:)

!!#### REQUIRED OUTPUT
TYPE(TYPE_Interfaces),POINTER :: Interfaces(:)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit_noisy

!!#### LOCAL VARIABLES
INTEGER :: Nji,Njn,j1,j2,ji1,ji2,jn,n,Unit_noisy_
LOGICAL,ALLOCATABLE :: used_faces(:)
INTEGER,PARAMETER :: DEFAULT_Unit_Noisy = 0

!!--begin--
!initialize
Unit_Noisy_=DEFAULT(DEFAULT_Unit_Noisy,Unit_Noisy)

!get number of interior faces
IF( .NOT.ASSOCIATED(Jinte) )THEN
 Interfaces=>NULL()
 RETURN
ELSE
 Nji = SIZE(Jinte)
END IF

!at *most* the number of interfaces is 1/2 the number
!of interior faces (round up)---we will fix this to the
!exact amount later
Njn = Nji/2 + 1
ALLOCATE( Interfaces(1:Njn) )

!allocate the master_faces list--it contains .true. for used
!faces that cannot become master faces
ALLOCATE( used_faces(1:Nji) )
used_faces = .FALSE.

!initialize number of interfaces found
jn = 0

!loop through masters
DO ji1 = 1,Nji

 !cycle if it has been used already as a master
 IF( used_faces(ji1) )CYCLE

 !get a "master face" to check
 j1 = Jinte(ji1)

 IF( Unit_Noisy_>0 )write(Unit_noisy_,*)"checking master face j1=",j1

 !loop through sub-faces
 DO ji2 = 1,Nji

  !cycle on master and sub the same
  IF( ji1==ji2 )CYCLE

  !cycle if it has been used already as a master (therefore it
  !cannot be a sub)
  IF( used_faces(ji2) )CYCLE

  !get a "sub face" to check
  j2 = Jinte(ji2)

  IF( Unit_Noisy_>0 )write(Unit_noisy_,*)"checking sub-face j2=",j2

  !check if <j2> is a sub of <j1>
  IF( Unit_noisy_>0 )write(Unit_noisy_,*)"IsSubFace(Mesh,j1,j2)=",IsSubFace(Mesh,j1,j2)
  IF( IsSubFace(Mesh,j1,j2) )THEN
   IF( used_faces(ji1) )THEN

    !we found a new subface to an already-established interface
    IF( Unit_noisy_>0 )THEN
     write(Unit_noisy_,*)"addto interface #",jn
     write(Unit_noisy_,*)"  (master face=",j1,")"
     write(Unit_noisy_,*)"  new sub face=",j2
    END IF

   ELSE
    !we found a new interface so  set that <j1> has been used as a master
    used_faces(ji1) = .TRUE.

    !increment interface number
    jn = jn + 1

    IF( Unit_noisy_>0 )THEN
     write(Unit_noisy_,*)"found interface #",jn
     write(Unit_noisy_,*)"  master face=",j1
     write(Unit_noisy_,*)"     sub face=",j2
    END IF

    Interfaces(jn)%master = j1

   END IF

   !add the subface
   CALL REALLOCATE( Interfaces(jn)%subs , 1 )
   n = SIZE(Interfaces(jn)%subs)
   Interfaces(jn)%subs(n) = j2

  END IF

 END DO
END DO

!determine number of actual interfaces
Njn = COUNT(used_faces)
IF( Unit_noisy_>0 )write(Unit_noisy_,*)"TOTAL NUMBER OF INTERFACES FOUND: ",Njn

!reallocate the Interfaces array
CALL REALLOCATE_interfaces(Interfaces,Njn-SIZE(Interfaces))

DEALLOCATE( used_faces )

!!--end--
END SUBROUTINE


SUBROUTINE REALLOCATE_Interfaces(Interfaces,dn)
!!#### PURPOSE
!! Realloate the interfaces structure.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Interfaces),POINTER :: Interfaces(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: dn

!!#### LOCAL VARIABLES
TYPE(TYPE_Interfaces),POINTER :: copy(:)
INTEGER :: n

!!--begin--
IF( .NOT.ASSOCIATED(Interfaces) )THEN
 ALLOCATE( Interfaces(1:dn) )
ELSE
 !allocate
 ALLOCATE( copy(1:SIZE(interfaces)+dn) )
 !point copy
 copy = Interfaces

 !nullify original
 DO n=1,SIZE(interfaces)
  NULLIFY( Interfaces(n)%subs )
 END DO

 !deallocate original
 DEALLOCATE( Interfaces )
 !reallocate
 ALLOCATE( Interfaces(1:SIZE(copy)) )
 !point copy
 Interfaces = copy

 !nullify the copy
 DO n=1,SIZE(copy)
  NULLIFY( copy(n)%subs )
 END DO

END IF

!!--end--
END SUBROUTINE



SUBROUTINE InitializeUniformSpan(univals,N,Mesh,UserN,Init)
!!#### PURPOSE
!! Initialize the uniform values <univals> and determine
!! the number of values <N> that will be used.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_MSH),POINTER     :: univals(:)

!!#### REQUIRED OUTPUT
INTEGER      ,INTENT(OUT) :: N

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)  ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: UserN
LOGICAL,OPTIONAL,INTENT(IN) :: Init

!!--begin--
!! Dump everything if <Init> is present and true.
IF( PRESENT(Init) )THEN
 IF( Init )THEN
  CALL CLEARn ( univals )
  CALL CLEAR  ( N )
 END IF
END IF

!! Determine the number of divisions from user
!! input or from the <Mesh> itself.
IF( PRESENT(UserN) )THEN
 N = UserN
END IF

!! Fix value if less than or equal to 0.
IF( N<=0 )THEN
 N = NUM_Verts(Mesh)
END IF

!!--end--
END SUBROUTINE



SUBROUTINE GetUniformSpan(univals,N,vals)
!!#### PURPOSE
!! Determine the uniform values <univals> that span the array
!! of <vals> with <N> values.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_MSH),POINTER     :: univals(:)

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: vals(:)
INTEGER      ,INTENT(IN) :: N

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: min,max,d

!!--begin--
!! Initialize if associated but not equal to new
!! number of divisions.
IF( ASSOCIATED(univals) )THEN
 IF( N/=SIZE(univals) )THEN
  CALL CLEARn( univals )
 END IF
END IF

!! Setup if anything has changed or if not
!! already associated.
IF( .NOT.ASSOCIATED(univals) )THEN

 !! Get maxes and mins from the domain.
 min = minval(vals)
 max = maxval(vals)

 !! Get the increment in <x>.
 d = (max-min)/REAL(N-1)

 !! Allocate.
 ALLOCATE( univals(N) )

 !! Generate the values of <univals> as
 !! a sequence from <min> to <max>.
 univals = Sequence(min,d,N)
 univals(N) = max

END IF

!!--end--
END SUBROUTINE


FUNCTION UniZ_Mesh(Mesh,NZ,Init) RESULT(UniZ)
!!#### PURPOSE
!! Give a set of z-values that spans the Mesh.

!!#### NOTES
!! Useful for constructing a structured grid
!! that has the same domain as the unstructured
!! grid stored in <Mesh>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN)   :: Mesh

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: UniZ(:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: NZ
LOGICAL,OPTIONAL,INTENT(IN) :: Init

!!--begin--
!! Quick return if this Mesh does not have this many
!! dimensions.
IF( NUM_Dimensions(Mesh)<3 )THEN
 UniZ => NULL()
 RETURN
END IF

!! Initialize <zout_> and <Nz_>.
CALL InitializeUniformSpan(zout_,Nz_,Mesh,Nz,Init)

!! Get <yout_> from <Nx_> and <Mesh> vertices.
CALL GetUniformSpan(Zout_,NZ_,Mesh%Verts(3,:))

!! Set return value.
UniZ => zout_

!!--end--
END FUNCTION



FUNCTION UniY_Mesh(Mesh,NY,Init) RESULT(UniY)
!!#### PURPOSE
!! Give a set of y-values that spans the Mesh.

!!#### NOTES
!! Useful for constructing a structured grid
!! that has the same domain as the unstructured
!! grid stored in <Mesh>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN)   :: Mesh

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: UniY(:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: NY
LOGICAL,OPTIONAL,INTENT(IN) :: Init

!!--begin--
!! Quick return if this Mesh does not have this many
!! dimensions.
IF( NUM_Dimensions(Mesh)<2 )THEN
 UniY => NULL()
 RETURN
END IF

!! Initialize <yout_> and <Ny_>.
CALL InitializeUniformSpan(yout_,Ny_,Mesh,Ny,Init)

!! Get <yout_> from <Nx_> and <Mesh> vertices.
CALL GetUniformSpan(yout_,Ny_,Mesh%Verts(2,:))

!! Set return value.
UniY => yout_

!!--end--
END FUNCTION


FUNCTION UniX_Mesh(Mesh,NX,Init) RESULT(UniX)
!!#### PURPOSE
!! Give a set of x-values that spans the Mesh.

!!#### NOTES
!! Useful for constructing a structured grid
!! that has the same domain as the unstructured
!! grid stored in <Mesh>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN)   :: Mesh

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: UniX(:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Nx
LOGICAL,OPTIONAL,INTENT(IN) :: Init

!!--begin--
!! Quick return if this Mesh does not have this many
!! dimensions.
IF( NUM_Dimensions(Mesh)<1 )THEN
 UniX => NULL()
 RETURN
END IF

!! Initialize <xout_> and <Nx_>.
CALL InitializeUniformSpan(xout_,Nx_,Mesh,Nx,Init)

!! Get <xout_> from <Nx_> and <Mesh> vertices.
CALL GetUniformSpan(xout_,Nx_,Mesh%Verts(1,:))

!! Set return value.
UniX => xout_

!!--end--
END FUNCTION



SUBROUTINE ModifyInterpGrid(Mesh,xout,yout,Nx,Ny,Init)
!!#### PURPOSE
!! Setup a grid for output putting the x and y-values
!! into global <xout> and <yout>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN)   :: Mesh

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),OPTIONAL,POINTER :: xout(:),yout(:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Nx,Ny
LOGICAL,OPTIONAL,INTENT(IN) :: Init

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: xmin,ymin,xmax,ymax,dx,dy

!!--begin--
!! Initialize.
IF( PRESENT(Init) )THEN
 IF( Init )THEN
  IF( ASSOCIATED(xout_) )THEN
   DEALLOCATE( xout_ , yout_ )
  END IF
  Nx_ = 0
  Ny_ = 0
  xout_ => NULL()
  yout_ => NULL()
  RETURN
 END IF
END IF

!! Quick return if aleady setup.
IF( ASSOCIATED(xout_) )THEN
 IF( PRESENT(xout) )xout => xout_
 IF( PRESENT(yout) )yout => yout_
 RETURN
END IF

!! Determine the number of divisions as
!! 5 times the size of the grid.
IF( PRESENT(Nx) )Nx_=Nx
IF( PRESENT(Ny) )Ny_=Ny

!! Get maxes and mins from the domain.
xmin = MINVAL(Mesh%Domain%Verts(1,:))
ymin = MINVAL(Mesh%Domain%Verts(2,:))
xmax = MAXVAL(Mesh%Domain%Verts(1,:))
ymax = MAXVAL(Mesh%Domain%Verts(2,:))

!! Get the increment in <x>, <y>.
dx = (xmax-xmin)/REAL(Nx_-1)
dy = (ymax-ymin)/REAL(Ny_-1)

!! Allocate.
ALLOCATE( xout_(Nx_) )
ALLOCATE( yout_(Ny_) )

!! Generate the values of <xout> and <yout> as
!! a sequence from xmin to xmax.
xout_ = Sequence(xmin,dx,Nx_)
xout_(Nx_) = xmax
yout_ = Sequence(ymin,dy,Ny_)
yout_(Ny_) = ymax

!!--end--
END SUBROUTINE



!!### SUBROUTINE: RemDupsVectorField
SUBROUTINE RemDupsVectorField(v,r,v2,r2)
!!#### PURPOSE
!! Remove duplicate values from a vector field,
!! <v>.

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: v(:,:,:),r(:,:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: v2(:,:,:),r2(:,:)

!!#### LOCAL VARIABLES
INTEGER,ALLOCATABLE :: order(:)
INTEGER :: g,Ng,NUM_Unique,d,Nd

!!--begin--
!! Setup.
ALLOCATE( order(1:SIZE(r,2)) )
Ng = SIZE(v,2)
Nd = SIZE(v,1)

!! Find unique vectors.
CALL FindUniqueVectors(r,NUM_Unique,order,r2,&
  tol=SQRT(EPSILON(1._KIND_MSH)),Unit=6,Interactive=.TRUE.)

!! Allocate <v2>.
ALLOCATE( v2(Nd,Ng,NUM_Unique) )

!! Reorder <v2> similarly.
FORALL( d=1:Nd )
 FORALL( g=1:Ng )
  v2(d,g,:) = Reorder( v(d,g,:) , order , side="R" )
 END FORALL
END FORALL
!! Wrapup.
DEALLOCATE( order )

!!--end--
END SUBROUTINE


!!### SUBROUTINE: RemDups
SUBROUTINE RemDups(f,r,f2,r2)
!!#### PURPOSE
!! Remove duplicate values from <r> AND scalar field <f>,
!! creating <r2> and <f2>.

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: f(:,:)
REAL(KIND_MSH),INTENT(IN) :: r(:,:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),POINTER :: f2(:,:)
REAL(KIND_MSH),POINTER :: r2(:,:)

!!#### LOCAL VARIABLES
INTEGER,ALLOCATABLE :: order(:)
INTEGER :: g,Ng,NUM_Unique

!!--begin--
!! Setup.
ALLOCATE( order(1:SIZE(r,2)) )
Ng = SIZE(f,1)

!! Find unique vectors.
CALL FindUniqueVectors(r,NUM_Unique,order,r2,&
  tol=SQRT(EPSILON(1._KIND_MSH)))

!! Allocate <f2>.
ALLOCATE( f2(Ng,NUM_Unique) )

!! Reorder <f2> similarly.
FORALL( g=1:Ng )
 f2(g,:) = PACK( Reorder( f(g,:) , order , side="R" ) , order>0 )
END FORALL

!! Wrapup.
DEALLOCATE( order )

!!--end--
END SUBROUTINE








SUBROUTINE SETUP_GRID(InterpType_,f,r,f2,r2,x,y,x_,y_,z)
!!#### PURPOSE
!! Set up local variables for grid interpolation.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: InterpType_
REAL(KIND_MSH),INTENT(IN) :: f(:,:)
REAL(KIND_MSH),INTENT(IN) :: r(:,:)

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_MSH),POINTER :: f2(:,:)
REAL(KIND_MSH),POINTER :: r2(:,:)
REAL(KIND_MSH),POINTER :: Z(:,:)
REAL(KIND_MSH),POINTER :: x(:),y(:)
REAL(KIND_MSH),POINTER :: x_(:),y_(:)

!!--begin--
IF( InterpType_==1 )THEN
 !swap x and y
 x_=>x
 y_=>y
 NULLIFY(x,y)
 !! Allocate special cell-centered Z
 ALLOCATE( Z(SIZE(x_)*2-2,SIZE(y_)*2-2) )
ELSE
 !! Remove duplicates.
 CALL RemDups(f ,r ,&  !input
              f2,r2)   !output

 !! Allocate <Z>.
 ALLOCATE( Z(SIZE(x),SIZE(y)) )
END IF

!!--end--
END SUBROUTINE

SUBROUTINE WRAPUP_GRID(InterpType_,f,r,f2,r2,x,y,x_,y_,z)
!!#### PURPOSE
!! Wrapup up local variables for grid interpolation.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: InterpType_
REAL(KIND_MSH),INTENT(IN) :: f(:,:)
REAL(KIND_MSH),INTENT(IN) :: r(:,:)

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_MSH),POINTER :: f2(:,:)
REAL(KIND_MSH),POINTER :: r2(:,:)
REAL(KIND_MSH),POINTER :: Z(:,:)
REAL(KIND_MSH),POINTER :: x(:),y(:)
REAL(KIND_MSH),POINTER :: x_(:),y_(:)

!!--begin--
IF( ASSOCIATED(r2) )DEALLOCATE(r2)
IF( ASSOCIATED(f2) )DEALLOCATE(f2)
NULLIFY( x , y )
NULLIFY( x_ , y_ )
deallocate( z )

!!--end--
END SUBROUTINE


SUBROUTINE SETUP_Mesh_Adap3_(Mesh,vc,s,t)
!!#### PURPOSE
!! Setup the <Adap3> Mesh---a simple 3-cell adaptive Mesh in 2D.

!!#### DETAILS
!! The following details may be printed with <PRINT_Mesh_Adap3>.
!
!
!  VERT MAP
!
!  4---------7---------3
!  |         |         |
!  |         |         |
!  |         |         |
!  |       t |         |
!  8---------5 vc      |
!  |         |         |
!  |         |         |
!  |         |         |
!  |         | s       |
!  1---------6---------2
!
!  static vert coordinates:
!
!    v1 : 0,0
!    v2 : 1,0
!    v3 : 1,1
!    v4 : 0,1
!
!
!  optional specifications:
!
!    vc : location of vert 5 (default is center at (/0.5,0.5/))
!     s : the angle from the x-axis, 2-6-5
!     t : the angle from the center line, 7-5-8
!
!
!  FACE/CELL MAP
!
!  +----5----+ +----4----+
!  |         | |         |
!  |         | |         |
!  6    3   10 |         |
!  |         | |         |
!  |         | |         |
!  +----11---+ |         |
!              9    2    3
!  +----12---+ |         |
!  |         | |         |
!  |         | |         |
!  7    1    8 |         |
!  |         | |         |
!  |         | |         |
!  +----1----+ +----2----+
!

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: vc(NUM_Dimensions(Mesh))
REAL(KIND_MSH),INTENT(IN) :: s
REAL(KIND_MSH),INTENT(IN) :: t


!!#### LOCAL VARIABLES
INTEGER :: k001,k002,k003,k004,k005,k006,k007,k008,k009,k010
INTEGER :: j001,j002,j005,j004,j007,j006,j008,j010,j003,j011,j012,j009
INTEGER :: i001,i002,i003
INTEGER :: j
REAL(KIND_MSH),DIMENSION(NUM_Dimensions(Mesh)) :: U_c,U_l,v006,v007,v008
REAL(KIND_MSH),DIMENSION(NUM_Dimensions(Mesh),NUM_Dimensions(Mesh)) :: Ln_c,Ln_l
REAL(KIND_MSH),DIMENSION(NUM_Dimensions(Mesh),NUM_Dimensions(Mesh)) :: Ls_t,Ls_b
REAL(KIND_MSH),DIMENSION(NUM_Dimensions(Mesh),NUM_Dimensions(Mesh)) :: Ls_l
LOGICAL :: Top,Bot,Lef
REAL(KIND_MSH),POINTER :: xout(:),yout(:)
REAL(KIND_MSH) :: s_,t_

!!--begin--
!! Set face structure.
Mesh%FaceStructure = MSH_CellBased


!! Set static verts.
k001 = ADD_vert(Mesh,(/0.0_KIND_MSH,0.0_KIND_MSH/),"v001")
k002 = ADD_vert(Mesh,(/1.0_KIND_MSH,0.0_KIND_MSH/),"v002")
k003 = ADD_vert(Mesh,(/1.0_KIND_MSH,1.0_KIND_MSH/),"v003")
k004 = ADD_vert(Mesh,(/0.0_KIND_MSH,1.0_KIND_MSH/),"v004")


!! Add center vert.
k005 = ADD_vert(Mesh,vc,"v005")



!! Determine where center line intersects top, bottom, and
!! left faces.
!!
!! * determine angle <s> in radians
s_   = c_PI_by_180 * s
!!
!! * determine direction of center line
U_c  = (/cos(s_),sin(s_)/)
!!
!! * calculate center line
Ln_c = xyLINE_PU( vc , U_c )
!!
!! * determine top and bottom line segments
Ls_b = xyLINESEGMENT_PP( Vert(Mesh,k001) , Vert(Mesh,k002) )
Ls_t = xyLINESEGMENT_PP( Vert(Mesh,k003) , Vert(Mesh,k004) )
!!
!! * calculate intersections with top and bottom
Bot  = xyINTERSECT_LnLs( Ln_c , Ls_b , P_intersect=v006 )
Top  = xyINTERSECT_LnLs( Ln_c , Ls_t , P_intersect=v007 )
!!
!! * now rotate <U_c> an additional <t> degrees
t_   = c_PI_by_180 * t
U_l  = xyROTATE_U( U_c , t_ )
!!
!! * determine the left line
Ln_l = xyLINE_PU( vc , U_l )
!!
!! * determine left line segment
Ls_l = xyLINESEGMENT_PP( Vert(Mesh,k004) , Vert(Mesh,k001) )
!!
!! * calculate intersection with left
Lef  = xyINTERSECT_LnLs( Ln_l , Ls_l , P_intersect=v008 )



!! Add intersection vertices
k006 = ADD_vert(Mesh,v006,"v006")
k007 = ADD_vert(Mesh,v007,"v007")
k008 = ADD_vert(Mesh,v008,"v008")



!! Add the faces.
j001 = ADD_face_Ps(Mesh,(/k001,k006/)     ,(/Straight_/),"f001")
j002 = ADD_face_Ps(Mesh,(/k006,k002/)     ,(/Straight_/),"f002")
j003 = ADD_face_Ps(Mesh,(/k002,k003/)     ,(/Straight_/),"f003")
j004 = ADD_face_Ps(Mesh,(/k003,k007/)     ,(/Straight_/),"f004")
j005 = ADD_face_Ps(Mesh,(/k007,k004/)     ,(/Straight_/),"f005")
j006 = ADD_face_Ps(Mesh,(/k004,k008/)     ,(/Straight_/),"f006")
j007 = ADD_face_Ps(Mesh,(/k008,k001/)     ,(/Straight_/),"f007")
j008 = ADD_face_Ps(Mesh,(/k006,k005/)     ,(/Straight_/),"f008")
j009 = ADD_face_Ps(Mesh,(/k007,k005,k006/),(/Straight_/),"f009")
j010 = ADD_face_Ps(Mesh,(/k005,k007/)     ,(/Straight_/),"f010")
j011 = ADD_face_Ps(Mesh,(/k008,k005/)     ,(/Straight_/),"f011")
j012 = ADD_face_Ps(Mesh,(/k005,k008/)     ,(/Straight_/),"f012")


!! Add the cells.
i001 = ADD_cell_Jx(Mesh,(/j001,j008,j012,j007/),"c001")
i002 = ADD_cell_Jx(Mesh,(/j002,j003,j004,j009/),"c002")
i003 = ADD_cell_Jx(Mesh,(/j011,j010,j005,j006/),"c003")


!! Setup the grid for output.
CALL ModifyInterpGrid(Mesh,xout,yout,2,2)
xout => NULL()
yout => NULL()
CALL Finalize(Mesh)

!! SETUP/UPDATE/ORIENT.
DO j=1,12
 CALL SETUP_DomainFace(Mesh,j)
 CALL UPDATE_DomainFace(Mesh,j)
END DO
CALL OrientBoundary(Mesh)
CALL OrientInterior(Mesh)

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_Mesh_Adap3_( fdbk , Unit )
!!#### PURPOSE
!! Print the unstructured test Mesh: adaptive, 3-cell Mesh.

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_

!!--begin--
!get output unit
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )

WRITE(Unit_,"(a)")"#Mesh KEY: Adap3#"
WRITE(Unit_,"(a)")"  "
WRITE(Unit_,"(a)")"  "
WRITE(Unit_,"(a)")"    VERT MAP"
WRITE(Unit_,"(a)")"  "
WRITE(Unit_,"(a)")"    4---------7---------3"
WRITE(Unit_,"(a)")"    |         |         |"
WRITE(Unit_,"(a)")"    |         |         |"
WRITE(Unit_,"(a)")"    |         |         |"
WRITE(Unit_,"(a)")"    |       t |         |"
WRITE(Unit_,"(a)")"    8---------5 vc      |"
WRITE(Unit_,"(a)")"    |         |         |"
WRITE(Unit_,"(a)")"    |         |         |"
WRITE(Unit_,"(a)")"    |         |         |"
WRITE(Unit_,"(a)")"    |         | s       |"
WRITE(Unit_,"(a)")"    1---------6---------2"
WRITE(Unit_,"(a)")"    "
WRITE(Unit_,"(a)")"    static vert coordinates:"
WRITE(Unit_,"(a)")"  "
WRITE(Unit_,"(a)")"      v1 : 0,0"
WRITE(Unit_,"(a)")"      v2 : 1,0"
WRITE(Unit_,"(a)")"      v3 : 1,1"
WRITE(Unit_,"(a)")"      v4 : 0,1"
WRITE(Unit_,"(a)")"  "
WRITE(Unit_,"(a)")"  "
WRITE(Unit_,"(a)")"    optional specifications: "
WRITE(Unit_,"(a)")"    "
WRITE(Unit_,"(a)")"      vc : location of vert 5 (default is center at (/0.5,0.5/))"
WRITE(Unit_,"(a)")"       s : the angle from the x-axis, 2-6-5"
WRITE(Unit_,"(a)")"       t : the angle from the center line, 7-5-8"
WRITE(Unit_,"(a)")"          "
WRITE(Unit_,"(a)")"  "
WRITE(Unit_,"(a)")"    FACE/CELL MAP"
WRITE(Unit_,"(a)")"  "
WRITE(Unit_,"(a)")"    +----5----+ +----4----+"
WRITE(Unit_,"(a)")"    |         | |         |"
WRITE(Unit_,"(a)")"    |         | |         |"
WRITE(Unit_,"(a)")"    6    3   10 |         |"
WRITE(Unit_,"(a)")"    |         | |         |"
WRITE(Unit_,"(a)")"    |         | |         |"
WRITE(Unit_,"(a)")"    +----11---+ |         |"
WRITE(Unit_,"(a)")"                9    2    3"
WRITE(Unit_,"(a)")"    +----12---+ |         |"
WRITE(Unit_,"(a)")"    |         | |         |"
WRITE(Unit_,"(a)")"    |         | |         |"
WRITE(Unit_,"(a)")"    7    1    8 |         |"
WRITE(Unit_,"(a)")"    |         | |         |"
WRITE(Unit_,"(a)")"    |         | |         |"
WRITE(Unit_,"(a)")"    +----1----+ +----2----+"
WRITE(Unit_,"(a)")"  "

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<PERTURB_Verts>>
SUBROUTINE PERTURB_Verts( Mesh , VertSet , Radius , Polar , Azimuthal , FrozenVertSet )
!!#### PURPOSE
!! Perturb verts in a set by sampling random radii and angles
!! within the bounds set.

USE FUN_Random                                                       !!((03-A-FUN_Random.f90))

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER       ,POINTER    :: VertSet(:)
REAL(KIND_MSH),INTENT(IN) :: Radius(1:2)
REAL(KIND_MSH),INTENT(IN) :: Polar(1:2)
REAL(KIND_MSH),INTENT(IN) :: Azimuthal(1:2)
INTEGER,OPTIONAL,POINTER  :: FrozenVertSet(:)

!!#### LOCAL VARIABLES
REAL(KIND_MSH)       :: R,P,A
INTEGER              :: k,m
INTEGER              :: NDim

!!--begin--

IF( .NOT.ASSOCIATED(VertSet) )RETURN

IF( .NOT.IsFinalized(Mesh) )THEN
 CALL FINALIZE(Mesh)
END IF

NDim = NUM_Dimensions(Mesh)

DO m=1,SIZE(VertSet)

 k = VertSet(m)

 IF( PRESENT(FrozenVertSet) )THEN
  !only move the vert if it is NOT in the frozen vert set
  IF( FIND_IN_SET(FrozenVertSet,k)/=0 )THEN
   CYCLE
  END IF
 END IF

 !get radius
 R = Random( (/Radius(1)   ,Radius(2)   /) )

 !sample angles
 CALL SAMPLE_PA( NDim , Polar,Azimuthal , P,A )

 !perturb
 Mesh%Verts(:,k) = Mesh%Verts(:,k) + SphericalToCartesian(NDim,R,P,A)

END DO

!update the Mesh
CALL UPDATE_Mesh(Mesh,klist=VertSet)

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<GRAVITATE_Verts>>
SUBROUTINE GRAVITATE_Verts( Mesh , VertSet , Center,GravConst , FrozenVertSet, MoveType )
!!#### PURPOSE
!! Gravitate Vertices.

USE FUN_Random                                                       !!((03-A-FUN_Random.f90))

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER       ,POINTER    :: VertSet(:)
REAL(KIND_MSH),INTENT(IN) :: Center(Mesh%NDim)
REAL(KIND_MSH),INTENT(IN) :: GravConst
INTEGER,OPTIONAL,POINTER  :: FrozenVertSet(:)
CHARACTER,OPTIONAL,INTENT(IN) :: MoveType

!!#### LOCAL VARIABLES
REAL(KIND_MSH)       :: eps,MovementVector(Mesh%NDim),AttrForce,RepelForce,r
INTEGER              :: k,m
INTEGER              :: NDim
CHARACTER(32)        :: MoveType_

!!--begin--
 IF( PRESENT(MoveType) )THEN
    MoveType_=MoveType
 ELSE
    MoveType_='InverseSquare'
 END IF

eps = 1.d-12

IF( .NOT.ASSOCIATED(VertSet) )RETURN

IF( .NOT.IsFinalized(Mesh) )THEN
 CALL FINALIZE(Mesh)
END IF

NDim = NUM_Dimensions(Mesh)

DO m=1,SIZE(VertSet)

 k = VertSet(m)

 IF( PRESENT(FrozenVertSet) )THEN
  !only move the vert if it is NOT in the frozen vert set
  IF( FIND_IN_SET(FrozenVertSet,k)/=0 )THEN
   CYCLE
  END IF
 END IF

 MovementVector = Center - Mesh%Verts(:,k)
 r = SQRT(SUM(MovementVector**2))

 !move
 IF( TRIM(MoveType_)=='InverseSquare' )THEN
    Mesh%Verts(:,k) = Mesh%Verts(:,k) + MovementVector/(1.d0+r**2/(ABS(GravConst)+eps))
    !Mesh%Verts(:,k) = Mesh%Verts(:,k) + (MovementVector/r)*MIN(r,GravConst/(r+eps)**2)
 ELSE
    Mesh%Verts(:,k) = Mesh%Verts(:,k) + MovementVector*GravConst
 END IF

END DO

!update the Mesh
CALL UPDATE_Mesh(Mesh,klist=VertSet)

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<ROTATE_Mesh>>
SUBROUTINE ROTATE_Mesh( Mesh , Center , Polar , Azimuthal , spiral )
!!#### PURPOSE
!! Rotate the entire Mesh by sampling random angles
!! within the bounds set.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: Center(1:NUM_Dimensions(Mesh))
REAL(KIND_MSH),INTENT(IN) :: Polar(1:2)
REAL(KIND_MSH),INTENT(IN) :: Azimuthal(1:2)
REAL(KIND_MSH),INTENT(IN) :: spiral

!!#### LOCAL VARIABLES
INTEGER       ,POINTER    :: VertSet(:)
REAL(KIND_MSH) :: R,P,A
INTEGER        :: k,m
REAL(KIND_MSH) :: Pol,Vector(1:NUM_Dimensions(Mesh))

!!--begin--

VertSet => ptr_VertSet_ALL(Mesh)

CALL ROTATE_Verts( Mesh , VertSet , Center , Polar , Azimuthal , spiral )
CALL ROTATE_Domain( Mesh , Center , Polar , Azimuthal , spiral )

CALL UPDATE_Mesh(Mesh,klist=VertSet)

VertSet => NULL()

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<TRANSLATE_Mesh>>
SUBROUTINE TRANSLATE_Mesh( Mesh , V )
!!#### PURPOSE
!! Translate the entire Mesh by a vector.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: V(1:NUM_Dimensions(Mesh))

!!#### LOCAL VARIABLES
INTEGER       ,POINTER    :: VertSet(:)
REAL(KIND_MSH) :: R,P,A
INTEGER        :: k,m
REAL(KIND_MSH) :: Pol,Vector(1:NUM_Dimensions(Mesh))

!!--begin--

VertSet => ptr_VertSet_ALL(Mesh)

CALL TRANSLATE_Verts( Mesh , VertSet , V )
CALL TRANSLATE_Domain( Mesh , V )

CALL UPDATE_Mesh(Mesh,klist=VertSet)

VertSet => NULL()

!!--end--
END SUBROUTINE


!!### <<ROTATE_Domain>>
SUBROUTINE ROTATE_Domain( Mesh , Center , Polar , Azimuthal , spiral )
!!#### PURPOSE
!! Rotate verts in the domain by sampling random angles
!! within the bounds set.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: Center(1:NUM_Dimensions(Mesh))
REAL(KIND_MSH),INTENT(IN) :: Polar(1:2)
REAL(KIND_MSH),INTENT(IN) :: Azimuthal(1:2)
REAL(KIND_MSH),INTENT(IN) :: spiral

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: P,A
INTEGER :: m

!!--begin--

IF( .NOT.HasDomain(Mesh) )RETURN

DO m=1,SIZE(Mesh%Domain%Verts,2)

  !sample rotations
 CALL SAMPLE_PA( NUM_Dimensions(Mesh) , Polar,Azimuthal , P,A )

 Mesh%Domain%Verts(:,m) = ROTATE( NUM_Dimensions(Mesh) , Mesh%Domain%Verts(:,m) , P,A,Center , spiral )

END DO

!update the DOMAIN
CALL UPDATE_Mesh_Domain(Mesh)

!!--end--
END SUBROUTINE




!!### <<TRANSLATE_Domain>>
SUBROUTINE TRANSLATE_Domain( Mesh , V )

!!#### PURPOSE
!! Translate verts in the domain.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: V(1:NUM_Dimensions(Mesh))

!!#### LOCAL VARIABLES
INTEGER :: m

!!--begin--

IF( .NOT.HasDomain(Mesh) )RETURN

DO m=1,SIZE(Mesh%Domain%Verts,2)

 Mesh%Domain%Verts(:,m) = TRANSLATE( NUM_Dimensions(Mesh) , Mesh%Domain%Verts(:,m) , V )

END DO

!update the DOMAIN
CALL UPDATE_Mesh_Domain(Mesh)

!!--end--
END SUBROUTINE




!!### SUBROUTINE <<ROTATE_Verts>>
SUBROUTINE ROTATE_Verts( Mesh , VertSet , Center , Polar , Azimuthal , spiral )
!!#### PURPOSE
!! Rotate verts in a set by sampling random angles
!! within the bounds set.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER       ,POINTER    :: VertSet(:)
REAL(KIND_MSH),INTENT(IN) :: Center(1:NUM_Dimensions(Mesh))
REAL(KIND_MSH),INTENT(IN) :: Polar(1:2)
REAL(KIND_MSH),INTENT(IN) :: Azimuthal(1:2)
REAL(KIND_MSH),INTENT(IN) :: spiral

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: P,A
INTEGER        :: k,m

!!--begin--
IF( .NOT.ASSOCIATED(VertSet) )RETURN

IF( .NOT.IsFinalized(Mesh) )THEN
 CALL FINALIZE(Mesh)
END IF

DO m=1,SIZE(VertSet)

 !sample rotations
 CALL SAMPLE_PA( NUM_Dimensions(Mesh) , Polar,Azimuthal , P,A )

 k = VertSet(m)

 Mesh%Verts(:,k) = ROTATE( NUM_Dimensions(Mesh) , Vert(Mesh,k) , P,A, Center , spiral )

END DO

!update the Mesh
CALL UPDATE_Mesh(Mesh,klist=VertSet)

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<TRANSLATE_Verts>>
SUBROUTINE TRANSLATE_Verts( Mesh , VertSet , V )
!!#### PURPOSE
!! Translate verts in the <VertSet> by a vector <V>

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER       ,POINTER    :: VertSet(:)
REAL(KIND_MSH),INTENT(IN) :: V(1:NUM_Dimensions(Mesh))

!!#### LOCAL VARIABLES
INTEGER        :: k,m

!!--begin--
IF( .NOT.ASSOCIATED(VertSet) )RETURN

IF( .NOT.IsFinalized(Mesh) )THEN
 CALL FINALIZE(Mesh)
END IF

DO m=1,SIZE(VertSet)

 k = VertSet(m)

 Mesh%Verts(:,k) = TRANSLATE( NUM_Dimensions(Mesh) , Vert(Mesh,k) , V )

END DO

!update the Mesh
CALL UPDATE_Mesh(Mesh,klist=VertSet)

!!--end--
END SUBROUTINE


!!### FUNCTION <<ROTATE>>
FUNCTION ROTATE( NDim , Vert , P , A , Center , spiral )  RESULT(Vector)

!!#### PURPOSE
!! Provide a general rotation function for a <Vert>.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: NDim
REAL(KIND_MSH),INTENT(IN) :: Vert(NDim)
REAL(KIND_MSH),INTENT(IN) :: P,A

!!#### OPTIONAL INPUT
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: Center(NDim),spiral

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Vector(Ndim)

!!#### LOCAL VARIABLEs
REAL(KIND_MSH) :: R

!!--begin--

IF( PRESENT(Center) )THEN
 Vector = Vert - Center
ELSE
 Vector = Vert
END IF

!calculate R
R = SQRT( SUM( (Vector)**2 ) )

!add the rotation vector
SELECT CASE( NDim )
 CASE(2) ; !Pol = xyANGLE_VV( (/1._KIND_MSH,0._KIND_MSH/) , Vector )
           Vector = xyROTATE_V( Vector , P/(1._KIND_MSH+R*spiral) )
                   IF( PRESENT(Center) )THEN
                    Vector = Vector + Center
                   END IF
 CASE(3) ; !Mesh%Verts(:,k) = Center + xyzROTATE_V( Vert(Mesh,k)-Center , P , A )
END SELECT

!!--end--
END FUNCTION




!!### FUNCTION <<TRANSLATE>>
FUNCTION TRANSLATE( NDim , Vert , V )  RESULT(Vector)

!!#### PURPOSE
!! Provide a general translation function for a <Vert>.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: NDim
REAL(KIND_MSH),INTENT(IN) :: Vert(NDim)
REAL(KIND_MSH),INTENT(IN) :: V(NDim)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Vector(Ndim)

!!--begin--

Vector = Vert + V

!!--end--
END FUNCTION




!!### SUBROUTINE <<SAMPLE_PA>>
SUBROUTINE SAMPLE_PA( NDim , Polar , Azimuthal , P , A )
!!#### PURPOSE
!! Sample some spherical coordinates.

!!#### REQUIRED INPUT
INTEGER        ,INTENT(IN)  :: NDim
REAL(KIND_MSH) ,INTENT(IN)  :: Polar(2),Azimuthal(2)
REAL(KIND_MSH) ,INTENT(OUT) :: P,A

!!--begin--

!get polar angle
IF( NDim>1 )THEN
 P = c_PI_by_180*Random( (/Polar(1)    ,Polar(2)    /) )
ELSE
 P = c_0
END IF

!get azimuthal angle
IF( NDim>2 )THEN
 A = c_PI_by_180*Random( (/Azimuthal(1),Azimuthal(2)/) )
ELSE
 A = c_PI_by_2
END IF

!!--end--
END SUBROUTINE



!!### FUNCTION <<SphericalToCartesian>>
FUNCTION SphericalToCartesian( NDim , R , P , A ) RESULT(Vector)

!!#### PURPOSE
!! Transform spherical coordinates to cartesian.

!!#### REQUIRED INPUT
INTEGER        ,INTENT(IN) :: NDim
REAL(KIND_MSH) ,INTENT(IN) :: R,P,A

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Vector(NDim)

!!--begin--

SELECT CASE(NDim)
 CASE(1)
  Vector(1) = R*cos(P)*sin(A)
 CASE(2)
  Vector(1) = R*cos(P)*sin(A)
  Vector(2) = R*sin(P)*sin(A)
 CASE(3)
  Vector(1) = R*cos(P)*sin(A)
  Vector(2) = R*sin(P)*sin(A)
  Vector(3) = R*cos(A)
 CASE DEFAULT
  Vector = ERROR(1._KIND_MSH)
END SELECT

!!--end--
END FUNCTION



!!### FUNCTION <<ptr_CellSet_POLYREGION>>
FUNCTION ptr_CellSet_POLYREGION( Mesh , PolyRegion ) RESULT(CellSet)

!!#### PURPOSE
!! Return a set of cells interior (by centroid) to a polyregion.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH) ,INTENT(IN) :: PolyRegion(:,:)

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: CellSet(:)

!!#### LOCAL VARIABLES
INTEGER :: i,N
REAL(KIND_MSH) :: A,C(1:NUM_Dimensions(Mesh))
!!--begin--
NULLIFY( CellSet )

IF( NUM_Dimensions(Mesh)==2 )THEN
 N = SIZE(PolyRegion,2)
 A = xySAREA_Pg( N , PolyRegion )
 C = xyCENTROID_Pg( N , PolyRegion , A )
 DO i=1,NUM_Cells(Mesh)
  IF( xyINTERIOR_PgP( N , PolyRegion , &
    CellCentroid(Mesh,i) , C ) )THEN
   CALL ADD_TO_SET( CellSet , i )
  END IF
 END DO
ELSE
 CALL Stop(s="No 3D yet!")
END IF

CALL FINALIZE_SET( CellSet )

!!--end--
END FUNCTION


FUNCTION ptr_FaceSet_PLANEINTERSECT( Mesh , Plane ) RESULT(FaceSet)
!!#### PURPOSE
!! Return a set of faces intersected by a plane.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH) ,INTENT(IN) :: Plane(:)

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: FaceSet(:)

!!#### LOCAL VARIABLES
INTEGER :: j
REAL(KIND_MSH) :: Ls(2,2)
TYPE(TYPE_ExplicitFace) :: Xf

!!--begin--
NULLIFY( FaceSet )

IF( NUM_Dimensions(Mesh)==2 )THEN

 DO j=1,NUM_Faces(Mesh)
  Xf = ExplicitFace(Mesh,j)
  Ls = LINESEGMENT_Xf( Xf )
  CALL CLEAN(Xf)
  IF( xyINTERSECT_PnLs(Plane ,Ls ) )THEN
   CALL ADD_TO_SET( FaceSet , j )
  END IF
 END DO
ELSE
 CALL Stop(s="No 3D yet!")
END IF

CALL FINALIZE_SET( FaceSet )

!!--end--
END FUNCTION



FUNCTION ptr_FaceSet_POLYREGION( Mesh , PolyRegion ) RESULT(FaceSet)
!!#### PURPOSE
!! Return a set of faces interior (by centroid) to a polyregion.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH) ,INTENT(IN) :: PolyRegion(:,:)

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: FaceSet(:)

!!#### LOCAL VARIABLES
INTEGER :: j,N
REAL(KIND_MSH) :: A,C(1:NUM_Dimensions(Mesh))
!!--begin--
NULLIFY( FaceSet )

IF( NUM_Dimensions(Mesh)==2 )THEN
 N = SIZE(PolyRegion,2)
 A = xySAREA_Pg( N , PolyRegion )
 C = xyCENTROID_Pg( N , PolyRegion , A )
 DO j=1,NUM_Faces(Mesh)
  IF( xyINTERIOR_PgP( N , PolyRegion , &
    FaceCentroid(Mesh,j) , C ) )THEN
   CALL ADD_TO_SET( FaceSet , j )
  END IF
 END DO
ELSE
 CALL Stop(s="No 3D yet!")
END IF

CALL FINALIZE_SET( FaceSet )

!!--end--
END FUNCTION



SUBROUTINE Reorder_Cellbased( Mesh , FaceMapping )
!!#### PURPOSE
!! Reorder the Mesh for cell-based Meshes.

USE SUB_Swap                                                         !!((04-A-SUB_Swap.f90))

!!#### DETAILS
!! The ordering is so that each cell <i> has all its faces in order
!! as <j=N0+1,N0+2,N0+3,...,N0+Nj0> where <Nj0> is the number
!! of faces in cell <i> and <N0> is the total number of
!! faces for cells <1> through <i-1>.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
INTEGER,INTENT(INOUT),OPTIONAL :: FaceMapping(:)

!!#### LOCAL VARIABLES
INTEGER :: i,j,j1,j2,j_,n,Nj_,Ni,Nj
INTEGER,POINTER :: ja(:),jb(:)
INTEGER,ALLOCATABLE :: FaceMappingNew(:)

!!--begin--
!quick return
IF( .NOT.IsCellBased(Mesh) )RETURN

Nj = NUM_Faces(Mesh)
Ni = NUM_Cells(Mesh)
ALLOCATE( ja(Nj) , jb(Nj) )
IF( PRESENT(FaceMapping) )THEN
 ALLOCATE( FaceMappingNew(SIZE(FaceMapping)) )
 FaceMappingNew = 0
END IF
!start assembling the swap list
n = 0
DO i=1,NUM_Cells(Mesh)
 Nj_ = NUM_Faces(Mesh%Cells(i))
 DO j_=1,Nj_
  !increment the swap-to counter
  n  = n + 1

  !get global face index
  j1    = ABS(Mesh%Cells(i)%FaceList(j_))
  j2    = n
  ja(n) = j1
  jb(n) = j2

  IF( PRESENT(FaceMapping) )THEN
   FaceMappingNew(j2) = FaceMapping(j1)
  END IF

 END DO

END DO

!swap the lists ja->jb
CALL Reorder_Faces(Mesh,ja,jb)
IF( PRESENT(FaceMapping) )THEN
 FaceMapping = FaceMappingNew
END IF

!wrapup
DEALLOCATE( ja,jb )
IF( PRESENT(FaceMapping) )THEN
 DEALLOCATE(FaceMappingNew)
END IF

!!--end--
END SUBROUTINE


FUNCTION ptr_VertSet_POLYREGION( Mesh , PolyRegion ) RESULT(VertSet)
!!#### PURPOSE
!! Return a set of Verts interior (by centroid) to a polyregion.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH) ,INTENT(IN) :: PolyRegion(:,:)

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: VertSet(:)

!!#### LOCAL VARIABLES
INTEGER :: k,N
REAL(KIND_MSH) :: A,C(1:NUM_Dimensions(Mesh))
!!--begin--
NULLIFY( VertSet )

IF( NUM_Dimensions(Mesh)==2 )THEN
 N = SIZE(PolyRegion,2)
 A = xySAREA_Pg( N , PolyRegion )
 C = xyCENTROID_Pg( N , PolyRegion , A )
 DO k=1,NUM_Verts(Mesh)
  IF( xyINTERIOR_PgP( N , PolyRegion , &
    Vert(Mesh,k) , C ) )THEN
   CALL ADD_TO_SET( VertSet , k )
  END IF
 END DO
ELSE
 CALL Stop(s="No 3D yet!")
END IF

CALL FINALIZE_SET( VertSet )

!!--end--
END FUNCTION


FUNCTION ptr_CellSet_ALL( Mesh ) RESULT(CellSet)
!!#### PURPOSE
!! Return the set of all cells.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: CellSet(:)

!!--begin--
NULLIFY( CellSet )

CellSet => ptr_Sequence(1,1,NUM_Cells(Mesh))

!!--end--
END FUNCTION


FUNCTION ptr_FaceSet_ALL( Mesh ) RESULT(FaceSet)
!!#### PURPOSE
!! Return the set of all faces.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: FaceSet(:)

!!--begin--
NULLIFY( FaceSet )

FaceSet => ptr_Sequence(1,1,NUM_Faces(Mesh))

!!--end--
END FUNCTION


FUNCTION ptr_VertSet_ALL( Mesh ) RESULT(VertSet)
!!#### PURPOSE
!! Return the set of all Verts.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: VertSet(:)

!!--begin--
NULLIFY( VertSet )

VertSet => ptr_Sequence(1,1,NUM_Verts(Mesh))

!!--end--
END FUNCTION



FUNCTION ptr_SetMatch( LABEL_LIST , GENLABEL ) RESULT(Set)
!!#### PURPOSE
!! Return the set which matches the label.

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: LABEL_LIST(:)
CHARACTER(*),INTENT(IN) :: GENLABEL

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: Set(:)

!!#### LOCAL VARIABLES
INTEGER :: i,n,m,Ni

!!--begin--
NULLIFY( SET )

!init
n=1
Ni=SIZE(LABEL_LIST)

!loop finding matches
DO m=1,Ni
 i = INDEXa(LABEL_LIST(n:Ni),GENLABEL,CASESEN=.FALSE.,WILDCARD="*")
 IF( i==0 )EXIT
 CALL ADD_TO_SET( Set , i )
 n = i+1
 IF( n>=Ni )EXIT
END DO

!set the cells
CALL FINALIZE_SET( Set )

!!--end--
END FUNCTION



FUNCTION ptr_CellSet_GENLABEL( Mesh , GENLABEL ) RESULT(CellSet)
!!#### PURPOSE
!! Return the set of cells which matches
!! the label.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)  ,INTENT(IN) :: GENLABEL

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: CellSet(:)

!!--begin--
NULLIFY( CellSet )
CellSet => ptr_SetMatch( Mesh%CellLabels(1:NUM_Cells(Mesh)) , GENLABEL )

!!--end--
END FUNCTION


FUNCTION ptr_FaceSet_GENLABEL( Mesh , GENLABEL ) RESULT(FaceSet)
!!#### PURPOSE
!! Return the set of Faces which matches
!! the label.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)  ,INTENT(IN) :: GENLABEL

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: FaceSet(:)

!!--begin--
NULLIFY( FaceSet )
FaceSet => ptr_SetMatch( Mesh%FaceLabels(1:NUM_Faces(Mesh)) , GENLABEL )

!!--end--
END FUNCTION


FUNCTION ptr_VertSet_GENLABEL( Mesh , GENLABEL ) RESULT(VertSet)
!!#### PURPOSE
!! Return the set of Verts which matches
!! the label.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
CHARACTER(*)  ,INTENT(IN) :: GENLABEL

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: VertSet(:)

!!#### LOCAL VARIABLES
INTEGER :: i,n,m,NVerts

!!--begin--
NULLIFY( VertSet )
VertSet => ptr_SetMatch( Mesh%VertLabels(1:NUM_Verts(Mesh)) , GENLABEL )

!!--end--
END FUNCTION


FUNCTION ADD_blockverts( Mesh , BlockLabel , P0,U1,U2,U3 , &
                         delx,dely,delz , Div , fdbk , perts ) RESULT(NVerts)
!!#### PURPOSE
!! Add vertices procedure for the Mesh-block routines.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh object <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
!! * number of verts added <NVerts>
INTEGER :: NVerts

!!#### REQUIRED INPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),INTENT(IN) :: BlockLabel
REAL(KIND_MSH)                      ,INTENT(IN) :: P0(:),U1(:),U2(:),U3(:)
REAL(KIND_MSH)                      ,INTENT(IN) :: delx(:),dely(:),delz(:)
INTEGER                            ,INTENT(IN) :: Div(:)

!!#### OPTIONAL INPUT
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: perts(:,:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_Noisy = .FALSE.

!!#### LOCAL VARIABLES
REAL(KIND_MSH),DIMENSION(1:NUM_Dimensions(Mesh)) :: P
REAL(KIND_MSH),ALLOCATABLE :: Verts(:,:)
INTEGER      ,ALLOCATABLE :: nmap(:,:,:)
INTEGER :: n,kz,ky,kx,k,NDim,nx,ny,nz,m
CHARACTER(LEN=100,KIND=KIND_S) :: Label !super size it

!!--begin--
!! Number of dimensions.
NDim=SIZE(Div)

!! Number of verts along each axis.
IF( NDim>=1 )THEN
 nx = Div(1)+1
ELSE
 nx = 1
END IF
IF( NDim>=2 )THEN
 ny = Div(2)+1
ELSE
 ny = Div(2)+1
END IF
IF( NDim>=3 )THEN
 nz = Div(3)+1
ELSE
 nz = 1
END IF

!! Initialize noisy variables.
IF( DEFAULT_Noisy )THEN
 ALLOCATE( nmap(1:nx,1:ny,1:nz) )
 ALLOCATE( Verts(1:NDim,nx*ny*nz) )
END IF

!! Loop.
n = 0
P = P0
DO kz=1,nz

 IF( NDim>=3 )THEN
  IF( kz>1 )THEN
   P = P + delz(kz-1)*U3
  END IF
 END IF

 DO ky=1,ny

  IF( NDim>=2 )THEN
   IF( ky>1 )THEN
    P = P + dely(ky-1)*U2
   END IF
  END IF

  DO kx=1,Div(1)+1

   IF( kx>1 )THEN
    P = P + delx(kx-1)*U1
   END IF

   n = n + 1

   !! Make label.
   !are these functions really slow?
   !CALL Clear(Label)
   !Label = "v"//TRIM(STR(n))//TRIM(BlockLabel)
   !do it the fast way
   WRITE(Label,'(a1,i0,a)')'v',n,BlockLabel

   IF( PRESENT(perts) )THEN
    k = ADD_vert( Mesh , P+perts(:,kx,ky,kz) , Label )
   ELSE
    k = ADD_vert( Mesh , P                   , Label )
   END IF

   IF( DEFAULT_Noisy )THEN
    nmap(kx,ky,kz) = n
    Verts(:,n) = Vert(Mesh,k)
   END IF

  END DO

  P = P - SUM(delx)*U1

 END DO

 P = P - SUM(dely)*U2

END DO


!! Set NVerts.
NVerts = n

!! Output noise.
IF( DEFAULT_Noisy )THEN
 DO kz=1,nz
  DO ky=1,ny
   DO kx=1,nx
    m = nmap(kx,ky,kz)
    WRITE(33,'(a)',ADVANCE="no" )coords(Verts(:,m))
   END DO
   WRITE(33,*)
  END DO
  WRITE(33,*)
 END DO
END IF

!!--end--
END FUNCTION



FUNCTION ADD_blockfaces( Mesh , BlockLabel , &
  kRange , Div , fdbk ) RESULT(NFaces)
!!#### PURPOSE
!! Add faces procedure for the Mesh-block routines.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh object <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
!! * number of faces added <NFaces>
INTEGER :: NFaces


!!#### REQUIRED INPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),INTENT(IN) :: BlockLabel
INTEGER                            ,INTENT(IN) :: kRange(1:2)
INTEGER                            ,INTENT(IN) :: Div(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_Noisy = .FALSE.

!!#### LOCAL VARIABLES
INTEGER :: n,k,kx1,kx2,j,ky1,ky2,NDim,kx,ky,koffset,nx,ny,Unit_noisy
INTEGER,POINTER :: VertList(:)
INTEGER,ALLOCATABLE :: jmapx(:,:),jmapy(:,:),Faces(:,:)
CHARACTER(LEN=100,KIND=KIND_S) :: Label
!!--begin--
NULLIFY(VertList)

!! We need two vertices exactly for each face.
NDim = NUM_Dimensions(Mesh)
ALLOCATE( VertList(1:2) )

!! Initialize nx and ny.
nx = Div(1)
ny = Div(2)

!! Initialize n.
n = 0

!! Set koffset.
koffset = kRange(1)-1

!! If we are requesting noisy output.
IF( DEFAULT_Noisy )THEN
 ALLOCATE( jmapx(nx  ,ny+1) ) ; jmapx = Error(jmapx(1,1))
 ALLOCATE( jmapy(nx+1,ny  ) ) ; jmapy = Error(jmapy(1,1))
 ALLOCATE( Faces(1:2,nx*(ny+1)+ny*(nx+1)) )
END IF

!! Get all the x-direction faces.
IF( NDim==2 )THEN
 DO ky=1,ny+1

  DO kx=1,nx

   !! Two vertices that form an edge in the x-direction
   !! are simply two sequential indices.
   kx1 = kx  + (ky-1)*(nx+1) + koffset
   kx2 = kx1 + 1             + koffset

   !! Set the list of vertices.
   VertList = (/kx1,kx2/)

   !! Add the face.
   n = n + 1
   !too slow
   !Label = "f"//TRIM(STR(n))//TRIM(BlockLabel)
   WRITE(Label,'(a1,i0,a)')'f',n,BlockLabel
   j = ADD_Face_Ps( Mesh , VertList , SPREAD(Straight_,1,1) , Label )

   !! Remember the face.
   IF( DEFAULT_Noisy )THEN
    jmapx(kx,ky) = j
    Faces(:,j)     = VertList
   END IF

  END DO

 END DO

 !! Get all the y-direction faces.
 DO kx=1,nx+1

  DO ky=1,ny

   !! Two vertices that form an edge in the y-direction
   !! are simply two indices separated by.
   ky1 = kx + (ky-1)*(nx+1) + koffset
   ky2 = ky1+ nx+1          + koffset

   !! Set the list of vertices.
   VertList = (/ky1,ky2/)

   !! Add the face.
   n = n + 1
   !too slow
   !Label = "f"//TRIM(STR(n))//TRIM(BlockLabel)
   WRITE(Label,'(a1,i0,a)')'f',n,BlockLabel
   j = ADD_Face_Ps( Mesh , VertList , (/Straight_/) , Label )

   !! Remember the face.
   IF( DEFAULT_Noisy )THEN
    jmapy(kx,ky) = j
    Faces(:,j)   = VertList
   END IF

  END DO

 END DO

END IF

!! Don't need vertex-list anymore.
DEALLOCATE( VertList )

!! Set number of faces added.
NFaces = n

!! Noisy Output.
IF( DEFAULT_Noisy )THEN
 Unit_noisy = NewFile("CCS_Mesh__ADD_blockfaces.noisy",&
   STATUS="Replace",IfOpened="R")
 DO ky=1,ny+1
  DO kx=1,nx
   j = jmapx(kx,ky)
   WRITE(Unit_noisy,"(i7.4,'x',3x,i7.4,'->',i7.4,3x)")j,Faces(:,j)
  END DO
 END DO
 DO kx=1,nx+1
  DO ky=1,ny
   j = jmapy(kx,ky)
   WRITE(Unit_noisy,"(i7.4,'y',3x,i7.4,'->',i7.4,3x)")j,Faces(:,j)
  END DO
 END DO
END IF

!!--end--
END FUNCTION



FUNCTION ADD_blockcells( Mesh , BlockLabel , jRange , Div , fdbk ) RESULT(NCells)
!!#### PURPOSE
!! Add cells procedure for the Mesh-block routines.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh object <Mesh>
!! * cell labels <LABEL_cell>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
!! * number of cells added <NCells>
INTEGER :: NCells

!!#### REQUIRED INPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),INTENT(IN) :: BlockLabel
INTEGER                            ,INTENT(IN) :: jRange(1:2)
INTEGER                            ,INTENT(IN) :: Div(:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback object <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN=100,KIND=KIND_S) :: Label
INTEGER :: n,j,i,nx,ny,jx1,jx2,jy1,jy2,jx,jy
INTEGER,POINTER :: FaceList(:)
INTEGER :: joffset

!!--begin--
NULLIFY(FaceList)

!! Each cell has 4 faces.
ALLOCATE( FaceList(1:4) )

!! Determine offset.
joffset = jRange(1)-1

!! Initialize and start the loop.
n  = 0
nx = Div(1)*(Div(2)+1)
DO jy=1,Div(2)
 DO jx=1,Div(1)

  !! Bottom face.
  jx1 =      (jy-1)*Div(1) + jx

  !! Top face.
  jx2 = jx1 + Div(1)

  !! Left face.
  jy1 = nx + (jx-1)*Div(2) + jy

  !! Right face.
  jy2 = jy1 + Div(2)

  !! Assemble <FaceList> in counter-clockwise order.
  FaceList = (/jx1,jy2,jx2,jy1/)
  FaceList = FaceList + joffset

  n = n + 1

  !! Set the label.
  WRITE(Label,'(a1,i0,a)')'c',n,BlockLabel 

  !! Finally add this cell to the Mesh.
  i = ADD_Cell_Jx( Mesh , FaceList , Label )


 END DO
END DO

!! Deallocate the facelist.
DEALLOCATE( FaceList )

!! Return.
NCells = n

!!--end--
END FUNCTION


FUNCTION SPLIT_Cells( Mesh , CellSet , Pn , Ps_interior , &
  CellSetOut , FaceSetOut , VertSetOut , &
  ShiftToCenter , VertTol ) RESULT(Splits)
!!#### PURPOSE
!! Split a set of cells <CellSet> into
!! using a plane.
USE SUB_Sort_quick,ONLY: Sort=>Sort_quick                            !!((03-A-SUB_Sort_quick.f90))

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh object (/Mesh/)
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * the cells to divide <CellSet>
!! * the plane <Pn>
INTEGER      ,INTENT(IN) :: CellSet(:)
REAL(KIND_MSH),INTENT(IN) :: Pn(1:Mesh%NDim+1)

!!#### REQUIRED OUTPUT
!! * whether the cells could be split <Splits>
LOGICAL :: Splits(SIZE(CellSet))

!!#### OPTIONAL INPUT
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: Ps_interior(:,:),VertTol
LOGICAL       ,INTENT(IN),OPTIONAL :: ShiftToCenter

!!#### OPTIONAL OUTPUT
INTEGER,OPTIONAL,POINTER :: CellSetOut(:)
INTEGER,OPTIONAL,POINTER :: FaceSetOut(:)
INTEGER,OPTIONAL,POINTER :: VertSetOut(:)

!!#### LOCAL VARIABLES
INTEGER,POINTER :: i_(:),j_(:),k_(:)
INTEGER :: m,i,n
CHARACTER(LEN=LEN_MESH_LABEL) :: CellLabels(SIZE(CellSet))
REAL(KIND_MSH) :: Pn_(Mesh%NDim+1)
REAL(KIND_MSH) :: Ps_interior_(Mesh%NDim,2)
LOGICAL :: ShiftToCenter_
INTEGER :: CellSet_(SIZE(CellSet))
REAL(KIND_MSH),DIMENSION(NUM_Dimensions(Mesh)) :: C,U,V

!!--begin--
IF( PRESENT(CellSetOut) )THEN
 NULLIFY( CellSetOut )
END IF
IF( PRESENT(FaceSetOut) )THEN
 NULLIFY( FaceSetOut )
END IF
IF( PRESENT(VertSetOut) )THEN
 NULLIFY( VertSetOut )
END IF
ShiftToCenter_ = DEFAULT(.FALSE.,ShiftToCenter)
Splits         = .FALSE.
NULLIFY( i_,j_,k_ )

![waw]inefficient, but over course of splitting cell numbers
!may change so we must identify cells passed
CellSet_=CellSet
CALL Sort(CellSet_)

DO m=1,SIZE(CellSet_)
 ![waw]over course of splitting, cell numbers may change
 !so we must identify cells by their labels
 i = CellSet_(m)
 WRITE(*,*)'Split Cells entry=',i

 IF( ShiftToCenter_ )THEN

  C   = CellCentroid(Mesh,i)
  U   = xyDIRECTION_Pn( Pn )
  Pn_ = xyPLANE_PV( C , U )

  IF( PRESENT(Ps_interior) )THEN
   V        = Ps_interior(:,2) - Ps_interior(:,1)
   Ps_interior_(:,1) = C
   Ps_interior_(:,2) = C + V
  END IF

 ELSE

  Pn_ = Pn

  IF( PRESENT(Ps_interior) )THEN
   Ps_interior_ = Ps_interior
  END IF

 END IF
 !CALL GET_Inputs(Mesh,i,Pn,ShiftToCenter_,Pn_,Ps_interior_,Ps_interior)
 WRITE(*,*)'entering SPLIT_Cell'
 IF( Size(CellSet_)==2 )THEN
   IF( CellSet_(1)==2 .AND. CellSet(2)==6 )THEN
        DEBUG='DEBUG'
   END IF
 END IF
 IF( PRESENT(Ps_Interior) )THEN
  Splits(m) = SPLIT_Cell( Mesh , i , Pn_ , i_ , j_ , k_ , Ps_interior=Ps_interior_ , VertTol=VertTol )
 ELSE
  Splits(m) = SPLIT_Cell( Mesh , i , Pn_ , i_ , j_ , k_ , VertTol=VertTol )
 END IF
 
 WRITE(*,*)'exiting SPLIT_Cell'

 IF( Splits(m) )THEN
    WRITE(*,*)'H1'

  !altered cell list
  IF( PRESENT(CellSetOut) )THEN
   CALL ADD_TO_SET( CellSetOut , i_(1) )
   CALL ADD_TO_SET( CellSetOut , i_(2) )
   DEALLOCATE(i_)
  END IF
WRITE(*,*)'H2'
  !altered face list
  IF( PRESENT(FaceSetOut) )THEN
   IF( ASSOCIATED(j_) )THEN
    DO n=1,SIZE(j_)
     CALL ADD_TO_SET( FaceSetOut , j_(n) )
    END DO
    DEALLOCATE( j_ )
   END IF
  END IF
WRITE(*,*)'H3'
  !altered vert list
  IF( PRESENT(VertSetOut) )THEN
   IF( ASSOCIATED(k_) )THEN
    DO n=1,SIZE(k_)
     CALL ADD_TO_SET( VertSetOut , k_(n) )
    END DO
    DEALLOCATE(k_ )
   END IF
  END IF

 END IF

END DO
WRITE(*,*)'H5'
IF( PRESENT(CellSetOut) )THEN
 WRITE(*,*)'cellsetout before'
 CALL FINALIZE_SET( CellSetOut )
    WRITE(*,*)'cellsetout after'
END IF
WRITE(*,*)'H6'
IF( PRESENT(FaceSetOut) )THEN
 CALL FINALIZE_SET( FaceSetOut )
END IF
WRITE(*,*)'H7'
IF( PRESENT(VertSetOut) )THEN
 CALL FINALIZE_SET( VertSetOut )
END IF
WRITE(*,*)'at end of FINALIZEING'

!!--end--
END FUNCTION


SUBROUTINE GET_Inputs(Mesh,i,Pn,ShiftToCenter_,Pn_,Ps_interior_,Ps_interior)
!!#### PURPOSE
!! Get the inputs for the single split cell.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: Pn(Mesh%NDim+1)
LOGICAL        ,INTENT(IN) :: ShiftToCenter_

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),INTENT(OUT) :: Pn_(Mesh%NDim+1)
REAL(KIND_MSH),INTENT(OUT) :: Ps_interior_(Mesh%NDim,2)

!!#### OPTIONAL INPUT
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: Ps_interior(2,2)

!!#### LOCAL VARIABLES
REAL(KIND_MSH),DIMENSION(NUM_Dimensions(Mesh)) :: C,U,V

!!--begin--
IF( ShiftToCenter_ )THEN

 C   = CellCentroid(Mesh,i)
 U   = xyDIRECTION_Pn( Pn )
 Pn_ = xyPLANE_PV( C , U )

 IF( PRESENT(Ps_interior) )THEN
  V        = Ps_interior(:,2) - Ps_interior(:,1)
  Ps_interior_(:,1) = C
  Ps_interior_(:,2) = C + V
 END IF

ELSE

 Pn_ = Pn

 IF( PRESENT(Ps_interior) )THEN
  Ps_interior_ = Ps_interior
 END IF

END IF

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_Mesh_( Mesh , Unit )
!!#### PURPOSE
!! Print out the Mesh structure.
USE PRN_Table                                                        !!((11-B-PRN_Table.f90))
USE FUN_Smush                                                        !!((03-A-FUN_Smush.f90))
USE FUN_Sentence                                                     !!((04-B-FUN_Sentence.f90))

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER              :: k,j,i,n,Nk,Nj,Ni,Nd,d
CHARACTER(LEN_MESH_LABEL),ALLOCATABLE :: VertLabels(:)
CHARACTER(LEN_MESH_LABEL),ALLOCATABLE :: FaceLabels(:)
CHARACTER(LEN_MESH_LABEL),ALLOCATABLE :: CellLabels(:)
INTEGER              :: Unit_
LOGICAL              :: Pass_VOL0,Pass_AREA0
REAL(KIND_MSH),POINTER :: FC(:),FA,FN(:),CV,CC(:),V(:)
INTEGER,POINTER :: VertList(:),FaceList(:)
CHARACTER(64),ALLOCATABLE :: Tab(:,:)

!!--begin--
NULLIFY(FC,FA,FN,CV,CC,V,VertList,FaceList)

Unit_ = DEFAULT(window_unit,Unit)

Nk = NUM_Verts(Mesh)
Nj = NUM_Faces(Mesh)
Ni = NUM_Cells(Mesh)
Nd = NUM_Dimensions(Mesh)

ALLOCATE( VertLabels(Mesh%NVerts) )
ALLOCATE( FaceLabels(Mesh%NFaces) )
ALLOCATE( CellLabels(Mesh%NCells) )

!! *CONTROL* variables
WRITE(Unit_,"(a)")" "
WRITE(Unit_,"(2x,a   )")"* CONTROL"
WRITE(Unit_,"(3x,a,i5)")"  Number of Dimensions : ",Nd
WRITE(Unit_,"(3x,a,i5)")"  Number of Verts      : ",Nk
WRITE(Unit_,"(3x,a,i5)")"  Number of Faces      : ",Nj
WRITE(Unit_,"(3x,a,i5)")"  Number of Cells      : ",Ni
IF( ASSOCIATED(Mesh%MeshType) )THEN
 WRITE(Unit_,"(3x,a,i5)")"  Mesh-Type            : ",Mesh%MeshType
END IF
IF( ASSOCIATED(Mesh%FaceStructure) )THEN
 WRITE(Unit_,"(3x,a,i5)")"  Face-Structure       : ",Mesh%FaceStructure
END IF
WRITE(Unit_,"(3x,a,l5)")"  Is Mesh Finalized?   : ",IsFinalized(Mesh)
WRITE(Unit_,"(a)")


!! *Mesh* checks
CALL CHECK_VOL0(Mesh,Pass=Pass_VOL0)
CALL CHECK_AREA0(Mesh,Pass=Pass_AREA0)
WRITE(Unit_,"(2x,a   )")"* DIAGNOSTIC CHECKS"
WRITE(Unit_,"(3x,a,a5)")"  CHECK_VOL0  : ",MERGE("PASS","FAIL",Pass_VOL0)
WRITE(Unit_,"(3x,a,a5)")"  CHECK_AREA0 : ",MERGE("PASS","FAIL",Pass_AREA0)
WRITE(Unit_,"(a)")
WRITE(Unit_,"(2x,a   )")"* Mesh CHARACTERISTICS"
WRITE(Unit_,"(3x,a,3E12.5)")"  MinSpan     : ",MinSpan(Mesh)
WRITE(Unit_,"(3x,a,3E12.5)")"  AverageSpan : ",AverageSpan(Mesh)
WRITE(Unit_,"(3x,a,3E12.5)")"  MaxSpan     : ",MaxSpan(Mesh)
WRITE(Unit_,"(a)")


!! *VERTS* variables

IF( ASSOCIATED(Mesh%VertLabels) )THEN
 VertLabels = Mesh%VertLabels
ELSE
 DO k=1,Nk
  VertLabels(k) = "v"//TRIM(STR(k))
 END DO
END IF

WRITE(Unit_,"(2x,a)")"* VERTS"
Nk = NUM_Verts(Mesh)
Nd = NUM_Dimensions(Mesh)
ALLOCATE( Tab(Nk+1,Nd+2) )
CALL CLEAR(Tab)

Tab(1,1) = "k    "
Tab(1,2) = "Label"
IF( Nd>=1 )Tab(1,3) = "Vx(k)"
IF( Nd>=2 )Tab(1,4) = "Vy(k)"
IF( Nd>=3 )Tab(1,5) = "Vz(k)"
ALLOCATE( V(Nd) )

DO k=1,Nk
 Tab(k+1,1) = STR(k)
 Tab(k+1,2) = STR(VertLabels(k))
 V = Vert(Mesh,k)
 DO d=1,Nd
  Tab(k+1,d+2) = STR(V(d),"(Es21.14)")
 END DO
END DO

CALL PRINT_Table(Tab,Unit=Unit_)
DEALLOCATE( Tab , V )




!! *FACES* variable
IF( ASSOCIATED(Mesh%FaceLabels) )THEN
 FaceLabels = Mesh%FaceLabels
ELSE
 DO j=1,Nj
  FaceLabels(j) = "f"//TRIM(STR(j))
 END DO
END IF

WRITE(Unit_ ,"(2x,a)")"* FACES"

ALLOCATE( Tab(Nj+1,2*Nd+4) )
CALL CLEAR(Tab)

Tab(1,1) = "j    "
Tab(1,2) = "Label"
Tab(1,3) = "FA(j)"
IF( Nd>=1 )Tab(1,3+1) = "FCx(j)"
IF( Nd>=2 )Tab(1,3+2) = "FCy(j)"
IF( Nd>=3 )Tab(1,3+3) = "FCz(j)"
IF( Nd>=1 )Tab(1,3+Nd+1) = "FNx(j)"
IF( Nd>=2 )Tab(1,3+Nd+2) = "FNy(j)"
IF( Nd>=3 )Tab(1,3+Nd+3) = "FNz(j)"
Tab(1,2*Nd+4) = "VertList(j)"

DO j=1,Nj
 Tab(j+1,1) = STR(j)
 Tab(j+1,2) = STR(FaceLabels(j))

 !face area
 FA => ptr_FaceArea(Mesh%faces(j))
 IF( ASSOCIATED(FA) )THEN
  Tab(j+1,3) = STR(FA,"(Es21.14)")
 END IF

 !face centroid
 FC => ptr_FaceCentroid(Mesh%faces(j))
 IF( ASSOCIATED(FC) )THEN
  DO d=1,Nd
   Tab(j+1,3+d) = STR(FC(d),"(Es21.14)")
  END DO
 END IF

 !face normal
 FN => ptr_FaceNormal(Mesh%faces(j))
 IF( ASSOCIATED(FN) )THEN
  DO d=1,Nd
   Tab(j+1,3+Nd+d) = STR(FN(d),"(Es21.14)")
  END DO
 END IF

 !vert list
 VertList => ptr_VertList(Mesh%Faces(j))
 IF( ASSOCIATED(VertList) )THEN
  Tab(j+1,2*Nd+4) = Smush(Sentence(STR(VertList),delim=","))
 END IF

END DO

CALL PRINT_Table(Tab,Unit=Unit_)
DEALLOCATE( Tab )
FA => NULL()
FC => NULL()
FN => NULL()
VertList => NULL()




! *CELLS* variable

IF( ASSOCIATED(Mesh%CellLabels) )THEN
 CellLabels = Mesh%CellLabels
ELSE
 DO i=1,Ni
  CellLabels(i) = "i"//TRIM(STR(i))
 END DO
END IF

WRITE(Unit_ ,"(2x,a)")"* CELLS"

ALLOCATE( Tab(Ni+1,Nd+4) )
CALL CLEAR(Tab)

Tab(1,1) = "i    "
Tab(1,2) = "Label"
Tab(1,3) = "CV(i)"
IF( Nd>=1 )Tab(1,3+1) = "CCx(i)"
IF( Nd>=2 )Tab(1,3+2) = "CCy(i)"
IF( Nd>=3 )Tab(1,3+3) = "CCz(i)"
Tab(1,Nd+4) = "FaceList(i)"

DO i=1,Ni
 Tab(i+1,1) = STR(i)
 Tab(i+1,2) = STR(CellLabels(i))

 !cell volume
 CV => ptr_CellVolume(Mesh%Cells(i))
 IF( ASSOCIATED(CV) )THEN
  Tab(i+1,3) = STR(CV,"(Es21.14)")
 END IF

 !cell centroid
 CC => ptr_CellCentroid(Mesh%cells(i))
 IF( ASSOCIATED(CC) )THEN
  DO d=1,Nd
   Tab(i+1,3+d) = STR(CC(d),"(Es21.14)")
  END DO
 END IF

 !face list
 FaceList => ptr_FaceList(Mesh%Cells(i))
 IF( ASSOCIATED(FaceList) )THEN
  Tab(i+1,4+Nd) = Smush(Sentence(STR(FaceList),delim=","))
 END IF

END DO

CALL PRINT_Table(Tab,Unit=Unit_)
DEALLOCATE( Tab )


WRITE(Unit_ ,"(2x,a)")"* FaceToCellLink"

IF( ASSOCIATED(Mesh%FaceToCellLink) )THEN

    ALLOCATE( Tab(Nj+1,3) )
    CALL CLEAR(Tab)

    Tab(1,1) = "j    "
    Tab(1,2) = "i1"
    Tab(1,3) = "i2"

    DO j=1,Nj
        Tab(j+1,1) = STR(j)
        Tab(j+1,2) = STR(Mesh%FaceToCellLink(1,j))
        Tab(j+1,3) = STR(Mesh%FaceToCellLink(2,j))
    END DO

    CALL PRINT_Table(Tab,Unit=Unit_)
    DEALLOCATE( Tab )

END IF

CV => NULL()
CC => NULL()
FaceList => NULL()

!!--end--
END SUBROUTINE


SUBROUTINE CHECK_VOL0(Mesh,Unit,PASS,ForcePrint)
!!#### PURPOSE
!! Check the volume of the domain versus
!! sum of cell volumes.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit
LOGICAL,INTENT(IN),OPTIONAL :: ForcePrint

!!#### OPTIONAL OUTPUT
LOGICAL,INTENT(OUT),OPTIONAL :: PASS

!!#### LOCAL VARIABLES
INTEGER :: i,Unit_,j_,j
LOGICAL :: PASS_,ForcePrint_
REAL(KIND_MSH) :: CEL_VOL,DOM_VOL,DIFF,LIM
INTEGER,POINTER :: FaceList(:)

!!--begin--
!get unit
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )
ForcePrint_ = DEFAULT( .FALSE. , ForcePrint )

!get cell volume
CEL_VOL = REAL(0,KIND(CEL_VOL))
DO i=1,Mesh%NCells
 CEL_VOL = CEL_VOL + CellVolume(Mesh,i)
END DO

!get domain volume
DOM_VOL = Mesh%Domain%Cell%CellVolume

!get difference
DIFF = DOM_VOL-CEL_VOL

!perform test
LIM = SQRT(EPSILON(CEL_VOL))
PASS_ = ABS(DIFF)<=LIM

!print results
IF( .NOT.PRESENT(PASS) .OR. ForcePrint_ )THEN
 WRITE(Unit_,"(a,a4)"   )"      CHECK_VOLUME(Mesh) : ",MERGE("PASS","FAIL",PASS_)
 WRITE(Unit_,"(a,e12.5)")"total volume of all cells: ",CEL_VOL
 WRITE(Unit_,"(a,e12.5)")"total volume of domain   : ",DOM_VOL
 WRITE(Unit_,"(a,e12.5)")"              difference : ",DIFF
 WRITE(Unit_,"(a,e12.5)")"                   limit : ",LIM
END IF

IF( PRESENT(PASS) )THEN
 PASS = PASS_
END IF

!!--end--
END SUBROUTINE


SUBROUTINE CHECK_AREA0(Mesh,Unit,PASS,ForcePrint)
!!#### PURPOSE
!! Check the area of the domain versus
!! sum of cell areas.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit
LOGICAL,INTENT(IN),OPTIONAL :: ForcePrint

!!#### OPTIONAL OUTPUT
LOGICAL,INTENT(OUT),OPTIONAL :: PASS

!!#### LOCAL VARIABLES
INTEGER        :: i,Unit_,errint_,j_,j
LOGICAL        :: PASS_,ForcePrint_
REAL(KIND_MSH) :: CEL_AREA(1:NUM_Dimensions(Mesh))
REAL(KIND_MSH) :: DOM_AREA(1:NUM_Dimensions(Mesh))
REAL(KIND_MSH) :: DIFF(1:NUM_Dimensions(Mesh))
REAL(KIND_MSH) :: LIM
INTEGER,POINTER :: FaceList(:)

!!--begin--
!get unit
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )
ForcePrint_ = DEFAULT( .FALSE. , ForcePrint )
errint_ = 0

NULLIFY( FaceList )

!get cell areas
CEL_AREA = REAL(0,KIND(CEL_AREA))
DO i=1,Mesh%NCells
 !get face list
 FaceList => ptr_FaceList( Mesh , i )

 !kick out if no face list
 IF( .NOT.ASSOCIATED(FaceList) )THEN
  errint_ = -1
  GOTO 666
 END IF

 !collect areas
 DO j_=1,SIZE(FaceList)
  j = FaceList(j_)
  CEL_AREA = CEL_AREA + FaceArea(Mesh,j)*FaceNormal(Mesh,j)
 END DO

END DO

!get domain area
DOM_AREA = REAL(0,KIND(DOM_AREA))
DO i=1,Mesh%NCells

 !get face list
 FaceList => Mesh%Domain%Cell%FaceList

 !kick out if no face list
 IF( .NOT.ASSOCIATED(FaceList) )THEN
  errint_ = -2
  GOTO 666
 END IF

 !collect areas
 DO j_=1,SIZE(FaceList)
  j = FaceList(j_)
  DOM_AREA = DOM_AREA + Mesh%Domain%Faces(j)%FaceArea*&
    Mesh%Domain%Faces(j)%FaceNormal
 END DO

END DO

NULLIFY( FaceList )

!get difference
DIFF = DOM_AREA-CEL_AREA

!perform test
LIM = SQRT(EPSILON(CEL_AREA))
PASS_ = ALL(ABS(DIFF)<=LIM)

666 CONTINUE
IF( errint_/=0 )THEN
 LIM = 0._KIND_MSH
 DIFF = Error(0._KIND_MSH)
 PASS_ = .FALSE.
END IF

!print results
IF( .NOT.PRESENT(PASS) .OR. ForcePrint_ )THEN
 WRITE(Unit_,"(a,a4)"    )"      CHECK_AREA(Mesh) : ",MERGE("PASS","FAIL",PASS_)
 WRITE(Unit_,"(a,3e12.5)")"total area of all cells: ",CEL_AREA
 WRITE(Unit_,"(a,3e12.5)")"total area of domain   : ",DOM_AREA
 WRITE(Unit_,"(a,3e12.5)")"            difference : ",DIFF
 WRITE(Unit_,"(a,3e12.5)")"                 limit : ",LIM
END IF

IF( PRESENT(PASS) )THEN
 PASS = PASS_
END IF

!!--end--
END SUBROUTINE



!!### FUNCTION <<EVAL_FaceCentroid>>
FUNCTION EVAL_FaceCentroid( Mesh , j ) RESULT(FaceCentroid)

!!#### PURPOSE
!! Calculate the appropriate centroid for a face from
!! library routines based on the shape.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * face index to evaluate normal of <j>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceCentroid(1:Mesh%NDim)

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitFace) :: Xf

!!--begin--
SELECT CASE(Mesh%NDim)
 CASE(2)
  Xf = ExplicitFace( Mesh , j )
  FaceCentroid = CENTROID_Xf( Xf )
  CALL CLEAN( Xf )
END SELECT

!!--end--
END FUNCTION




!!### FUNCTION <<EVAL_FaceCentroid_Domain>>
FUNCTION EVAL_FaceCentroid_Domain( Domain , jd ) RESULT(FaceCentroid)

!!#### PURPOSE
!! Calculate the appropriate centroid for a face from
!! library routines based on the shape.

!!#### REQUIRED INPUT
!! * the domain <Domain>
!! * face index to evaluate normal of <jd>
TYPE(TYPE_Domain),INTENT(IN) :: Domain
INTEGER          ,INTENT(IN) :: jd

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceCentroid(1:Domain%NDim)

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitFace) :: Xf

!!--begin--
SELECT CASE(Domain%NDim)
 CASE(2)
  Xf = ExplicitFace_Domain( Domain , jd )
  FaceCentroid = CENTROID_Xf( Xf )
  CALL CLEAN( Xf )
END SELECT

!!--end--
END FUNCTION



!!### PURE SUBROUTINE <<SETUP_FaceCentroid>>
PURE SUBROUTINE SETUP_FaceCentroid( Mesh , j )
!!#### PURPOSE
!! Setup the face centroid for face j.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate centroid of <j>
INTEGER       ,INTENT(IN) :: j

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Mesh%Faces(j)

IF( ASSOCIATED(Face%FaceCentroid) )THEN
 DEALLOCATE( Face%FaceCentroid )
END IF

ALLOCATE( Face%FaceCentroid(1:Mesh%NDim) )
Face%FaceCentroid = Error(Face%FaceCentroid)

Face => NULL()

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UPDATE_FaceCentroid>>
SUBROUTINE UPDATE_FaceCentroid( Mesh , j )

!!#### PURPOSE
!! Update the face centroid for face j.

!!#### DETAILS
!! Direction is NOT FLIPPED (out of bounds if j<0).

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate centroid of <j>
INTEGER       ,INTENT(IN) :: j

!!--begin--
Mesh%Faces(j)%FaceCentroid = EVAL_FaceCentroid( Mesh , j )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UPDATE_FaceCentroid_Domain>>
SUBROUTINE UPDATE_FaceCentroid_Domain( Domain )

!!#### PURPOSE
!! Update the face centroid for the domain.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### LOCAL VARIABLES
INTEGER :: jd

!!--begin--

DO jd=1,SIZE(Domain%Faces)
 Domain%Faces(jd)%FaceCentroid = EVAL_FaceCentroid_Domain( Domain , jd )
END DO

!!--end--
END SUBROUTINE



FUNCTION FaceCentroid( Mesh , j )
!!#### PURPOSE
!! Return the appropriate centroid for a face, using
!! the data contained in Mesh if available.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * face index to evaluate normal of <j>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: FaceCentroid(1:Mesh%NDim)

!!--begin--
IF( .NOT.ASSOCIATED(Mesh%Faces(j)%FaceCentroid) )THEN
 FaceCentroid = EVAL_FaceCentroid( Mesh , j )
ELSE
 FaceCentroid = Mesh%Faces(j)%FaceCentroid
END IF

!!--end--
END FUNCTION


PURE SUBROUTINE WRAPUP_FaceCentroid( Mesh , j )
!!#### PURPOSE
!! Wrapup the face centroid for face j.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <j>
INTEGER       ,INTENT(IN) :: j

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Mesh%Faces(j)

IF( ASSOCIATED(Face%FaceCentroid) )THEN
 DEALLOCATE( Face%FaceCentroid )
 Face%FaceCentroid => NULL()
END IF

Face => NULL()

!!--end--
END SUBROUTINE


PURE FUNCTION COUNT_Intersection( SET_A , SET_B ) RESULT(COUNT)
!!#### PURPOSE
!! Count the number of intersections of set A and set B.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: SET_A(:),SET_B(:)

!!#### REQUIRED OUTPUT
INTEGER :: COUNT

!!#### LOCAL VARIABLES
INTEGER :: n

!!--begin--
COUNT = 0
DO n=1,SIZE(SET_A)
 IF( ANY(SET_A(n)==SET_B) )THEN
  COUNT = COUNT + 1
 END IF
END DO
!!--end--
END FUNCTION


!!### SUBROUTINE <<UPDATE_Face>>
SUBROUTINE UPDATE_Face( Mesh , j )
!!#### PURPOSE
!! Update the face <j>.

!!#### DETAILS
!! Out of bounds if j<0.
!! Out of bounds if any component of face <j> has not
!! been <SETUP> appropriately.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index update <j>
INTEGER,INTENT(IN) :: j

!!--begin--
CALL UPDATE_FaceArea    ( Mesh , j )
CALL UPDATE_FaceNormal  ( Mesh , j )
CALL UPDATE_FaceCentroid( Mesh , j )
CALL UPDATE_DomainFace  ( Mesh , j )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UPDATE_Face_Domain>>
SUBROUTINE UPDATE_Face_Domain( Domain )

!!#### PURPOSE
!! Update the faces of the domain.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!--begin--

CALL UPDATE_FaceArea_Domain    ( Domain )
CALL UPDATE_FaceNormal_Domain  ( Domain )
CALL UPDATE_FaceCentroid_Domain( Domain )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UPDATE_Cell>>
SUBROUTINE UPDATE_Cell( Mesh , i )
!!#### PURPOSE
!! Update the Cell <i>.

!!#### DETAILS
!! Out of bounds if any component of Cell <i> has not
!! been <SETUP> appropriately.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * Cell index update <i>
INTEGER       ,INTENT(IN) :: i

!!--begin--
CALL UPDATE_CellVolume  ( Mesh , i )
CALL UPDATE_CellCentroid( Mesh , i )
!!--end--
END SUBROUTINE




!!### SUBROUTINE <<UPDATE_Cell_Domain>>
SUBROUTINE UPDATE_Cell_Domain( Domain )
!!#### PURPOSE
!! Update the domain Cell.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!--begin--

CALL UPDATE_CellVolume_Domain  ( Domain )
CALL UPDATE_CellCentroid_Domain( Domain )

!!--end--
END SUBROUTINE



!!### PURE SUBROUTINE <<UPDATE_FaceArea>>
PURE SUBROUTINE UPDATE_FaceArea( Mesh , j )

!!#### PURPOSE
!! Update the internal face area component for face j.

!!#### DETAILS
!! Out of bounds if j<0.
!! Out of bounds if the <FaceArea> component has not
!! been setup with <SETUP_FaceArea>.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <j>
INTEGER       ,INTENT(IN) :: j

!!--begin--
Mesh%Faces(j)%FaceArea = EVAL_FaceArea( Mesh , j )

!!--end--
END SUBROUTINE



!!### PURE SUBROUTINE <<UPDATE_FaceArea_Domain>>
PURE SUBROUTINE UPDATE_FaceArea_Domain( Domain )

!!#### PURPOSE
!! Update the internal face area component for all faces
!! on the domain.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### LOCAL VARIABLES
INTEGER  :: jd

!!--begin--

DO jd=1,SIZE(Domain%Faces)
 Domain%Faces(jd)%FaceArea = EVAL_FaceArea_Domain( Domain , jd )
END DO

!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE <<SETUP_FaceArea>>
PURE SUBROUTINE SETUP_FaceArea( Mesh , j )
!!#### PURPOSE
!! Setup (reallocate) the face area
!! component for face j.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <j>
INTEGER       ,INTENT(IN) :: j

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Mesh%Faces(j)

IF( ASSOCIATED(Face%FaceArea) )THEN
 DEALLOCATE( Face%FaceArea )
END IF

ALLOCATE( Face%FaceArea )
Face%FaceArea = Error(Face%FaceArea)

Face => NULL()

!!--end--
END SUBROUTINE


PURE SUBROUTINE WRAPUP_FaceArea( Mesh , j )
!!#### PURPOSE
!! Wrapup (deallocate and nullify) the
!! face area component for face j.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * face index to evaluate normal of <j>
INTEGER       ,INTENT(IN) :: j

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Face

!!--begin--
Face => Mesh%Faces(j)

IF( ASSOCIATED(Face%FaceArea) )THEN
 DEALLOCATE( Face%FaceArea )
 Face%FaceArea => NULL()
END IF

Face => NULL()

!!--end--
END SUBROUTINE



!!### FUNCTION <<EVAL_CellCentroid>>
FUNCTION EVAL_CellCentroid( Mesh , i ) RESULT(CellCentroid)
!!#### PURPOSE
!! Calculate the appropriate centroid for a cell from
!! library routines based on the shape.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * cell index to centroid of <i>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellCentroid(1:Mesh%NDim)

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitCell) :: Xc

!!--begin--
SELECT CASE(Mesh%NDim)

 !two-dimensional routines
 CASE(2)
  SELECT CASE( Mesh%Cells(i)%CellShape )

   !force quadrilateral
   CASE( Pg_,Qs_,Qr_,Qg_ )
    Xc = ExplicitCell( Mesh , i )
    CellCentroid = CENTROID_Xc( Xc , Mesh%Cells(i)%CellVolume )
    CALL CLEAN( Xc )
  END SELECT

END SELECT

!!--end--
END FUNCTION


!!### FUNCTION <<EVAL_CellCentroid_Domain>>
FUNCTION EVAL_CellCentroid_Domain( Domain ) RESULT(CellCentroid)

!!#### PURPOSE
!! Calculate the appropriate centroid for the domain cell from
!! library routines based on the shape.

!!#### REQUIRED INPUT
TYPE(TYPE_Domain),INTENT(IN) :: Domain

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellCentroid(1:Domain%NDim)

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitCell) :: Xc

!!--begin--
SELECT CASE(Domain%NDim)

 !two-dimensional routines
 CASE(2)
  SELECT CASE( Domain%Cell%CellShape )

   !force quadrilateral
   CASE( Pg_,Qs_,Qr_,Qg_ )
    Xc = ExplicitCell_Domain( Domain )
    CellCentroid = CENTROID_Xc( Xc , Domain%Cell%CellVolume )
    CALL CLEAN( Xc )
  END SELECT

END SELECT

!!--end--
END FUNCTION


PURE SUBROUTINE SETUP_CellCentroid( Mesh , i )
!!#### PURPOSE
!! Setup the cell centroid for cell i.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * cell index to evaluate centroid of <i>
INTEGER       ,INTENT(IN) :: i

!!#### LOCAL VARIABLES
TYPE(TYPE_Cell),POINTER :: Cell

!!--begin--
Cell => Mesh%Cells(i)

IF( ASSOCIATED(Cell%CellCentroid) )THEN
 DEALLOCATE( Cell%CellCentroid )
END IF

ALLOCATE( Cell%CellCentroid(1:Mesh%NDim) )
Cell%CellCentroid = Error(Cell%CellCentroid)

Cell => NULL()

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UPDATE_CellCentroid>>
SUBROUTINE UPDATE_CellCentroid( Mesh , i )

!!#### PURPOSE
!! Update the cell centroid for cell i.

!!#### DETAILS
!! Direction is NOT FLIPPED (out of bounds if i<0).

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * cell index to evaluate centroid of <i>
INTEGER       ,INTENT(IN) :: i

!!--begin--
Mesh%Cells(i)%CellCentroid = EVAL_CellCentroid( Mesh , i )

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<UPDATE_CellCentroid_Domain>>
SUBROUTINE UPDATE_CellCentroid_Domain( Domain )

!!#### PURPOSE
!! Update the cell centroid for the domain.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!--begin--

Domain%Cell%CellCentroid = EVAL_CellCentroid_Domain( Domain )

!!--end--
END SUBROUTINE



!!### FUNCTION <<CellCentroid>>
FUNCTION CellCentroid( Mesh , i )
!!#### PURPOSE
!! Return the appropriate centroid for a cell, using
!! the data contained in Mesh if available.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * cell index to evaluate centroid of <i>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellCentroid(1:Mesh%NDim)

!!--begin--
IF( .NOT.ASSOCIATED(Mesh%Cells(i)%CellCentroid) )THEN
 CellCentroid = EVAL_CellCentroid( Mesh , i )
ELSE
 CellCentroid = Mesh%Cells(i)%CellCentroid
END IF

!!--end--
END FUNCTION


PURE SUBROUTINE WRAPUP_CellCentroid( Mesh , i )
!!#### PURPOSE
!! Wrapup the cell centroid for cell i.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * cell index to evaluate normal of <i>
INTEGER       ,INTENT(IN) :: i

!!#### LOCAL VARIABLES
TYPE(TYPE_Cell),POINTER :: Cell

!!--begin--
Cell => Mesh%Cells(i)

IF( ASSOCIATED(Cell%CellCentroid) )THEN
 DEALLOCATE( Cell%CellCentroid )
 Cell%CellCentroid => NULL()
END IF

Cell => NULL()

!!--end--
END SUBROUTINE





FUNCTION CellVolume( Mesh , i )
!!#### PURPOSE
!! Return the appropriate cell volume for a cell, using
!! the data contained in Mesh if available.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * cell index to evaluate volume of <i>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellVolume

!!#### LOCAL VARIABLES
TYPE(TYPE_Cell),POINTER :: Cell

!!--begin--
IF( .NOT.ASSOCIATED(Mesh%Cells(i)%CellVolume) )THEN
 CellVolume = EVAL_CellVolume( Mesh , i )
ELSE
 CellVolume = Mesh%Cells(i)%CellVolume
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<EVAL_CellVolume>>
FUNCTION EVAL_CellVolume( Mesh , i ) RESULT(CellVolume)

!!#### PURPOSE
!! Calculate the appropriate cell volume for a cell from
!! library routines based on the shape.

!!#### REQUIRED INPUT
!! * Mesh type <Mesh>
!! * cell index to evaluate volume of <i>
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellVolume

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitCell) :: Xc

!!--begin--

Xc = ExplicitCell(Mesh,i)
CellVolume = Volume_Xc( Xc )
CALL CLEAN( Xc )

!!--end--
END FUNCTION



!!### FUNCTION <<EVAL_CellVolume_Domain>>
FUNCTION EVAL_CellVolume_Domain( Domain ) RESULT(CellVolume)

!!#### PURPOSE
!! Calculate the appropriate cell volume for a cell from
!! library routines based on the shape.

!!#### REQUIRED INPUT
!! * Domain type <Domain>
TYPE(TYPE_Domain),INTENT(IN) :: Domain

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: CellVolume

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitCell) :: Xc

!!--begin--

Xc = ExplicitCell_Domain(Domain)
CellVolume = Volume_Xc( Xc )
CALL CLEAN( Xc )

!!--end--
END FUNCTION



!!### SUBROUTINE <<UPDATE_CellVolume>>
SUBROUTINE UPDATE_CellVolume( Mesh , i )

!!#### PURPOSE
!! Update the cell volume for cell i.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * cell index to evaluate volume of <i>
INTEGER        ,INTENT(IN) :: i

!!--begin--

Mesh%Cells(i)%CellVolume = EVAL_CellVolume( Mesh , i )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UPDATE_CellVolume_Domain>>
SUBROUTINE UPDATE_CellVolume_Domain( Domain )

!!#### PURPOSE
!! Update the cell volume for the domain.

!!#### REQUIRED INPUT/OUTPUT
!! * Domain type <Domain>
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!--begin--

Domain%Cell%CellVolume = EVAL_CellVolume_Domain( Domain )

!!--end--
END SUBROUTINE



PURE SUBROUTINE SETUP_CellVolume( Mesh , i )
!!#### PURPOSE
!! Setup the cell volume for cell i.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * cell index to evaluate volume of <i>
INTEGER       ,INTENT(IN) :: i

!!#### LOCAL VARIABLES
TYPE(TYPE_Cell),POINTER :: Cell

!!--begin--
Cell => Mesh%Cells(i)

IF( ASSOCIATED(Cell%CellVolume) )THEN
 DEALLOCATE( Cell%CellVolume )
END IF

ALLOCATE( Cell%CellVolume )
Cell%CellVolume = Error(Cell%CellVolume)

Cell => NULL()

!!--end--
END SUBROUTINE


PURE SUBROUTINE WRAPUP_CellVolume( Mesh , i )
!!#### PURPOSE
!! Wrapup the cell volume for cell i.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * cell index to evaluate volume of <i>
INTEGER       ,INTENT(IN) :: i

!!#### LOCAL VARIABLES
TYPE(TYPE_Cell),POINTER :: Cell

!!--begin--
Cell => Mesh%Cells(i)

IF( ASSOCIATED(Cell%CellVolume) )THEN
 DEALLOCATE( Cell%CellVolume )
 Cell%CellVolume => NULL()
END IF

Cell => NULL()

!!--end--
END SUBROUTINE

SUBROUTINE ADD_CellCentroidVerts( Mesh )
!!#### PURPOSE
!! Add cell centroid vertices to the Mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
INTEGER :: i,k

!!--begin--

DO i=1,Mesh%NCells
 k = ADD_vert( Mesh , CellCentroid(Mesh,i) , "cc"//TRIM(STR(i)) )
END DO

!!--end--
END SUBROUTINE


SUBROUTINE SETUP_IntegralRegions( Mesh , NUM_Integrals , IntegralRegions )
!!#### PURPOSE
!! Setup the <IntegralRegion> structure.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: NUM_Integrals

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_IntegralRegion),POINTER :: IntegralRegions(:)

!!#### LOCAL VARIABLES
INTEGER :: Nd,Nr,Ni,Np,r,i
REAL(KIND_MSH),POINTER :: coeff(:)
REAL(KIND_MSH),ALLOCATABLE :: Pg(:,:)
REAL(KIND_MSH) :: SAREA
REAL(KIND_MSH) :: Pg_centroid(1:NUM_Dimensions(Mesh))
INTEGER,POINTER :: set(:)
LOGICAL :: INTERIOR

!!--begin--

NULLIFY(set,coeff)

IF( .NOT.ASSOCIATED(IntegralRegions) )THEN
 RETURN
END IF

Nd = NUM_Dimensions(Mesh)
Nr = SIZE(IntegralRegions)
Ni = NUM_Cells(Mesh)

DO r=1,Nr
 !do the easy part of allocating the number of integrals and initializing
 ALLOCATE( IntegralRegions(r) % integrals(NUM_integrals) )
 IntegralRegions(r) % integrals = 0._KIND_MSH

 !set coeff
 coeff => IntegralRegions(r) % coeff

 !set polygon
 Np = SIZE(coeff)/Nd
 ALLOCATE( Pg(Nd,Np) )
 Pg          = xyPOLYGON_( coeff )
 SAREA       = xySAREA_Pg( Np , Pg )
 Pg_centroid = xyCENTROID_Pg( Np , Pg , SAREA )

 DO i=1,Ni
  INTERIOR = xyINTERIOR_PgP( Np , Pg , CellCentroid(Mesh,i) , Pg_centroid )
  IF( INTERIOR )THEN
   CALL ADD_TO_SET( set , i )
  END IF
 END DO
 CALL FINALIZE_SET( set )

 !associate set
 IntegralRegions(r) % set => set
 set => NULL()

 !deallocate polygon
 DEALLOCATE( Pg )

END DO
coeff => null()

!!--end--
END SUBROUTINE

FUNCTION DimSpan(Mesh)  RESULT(Span)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Span(Mesh%NDim,NUM_Cells(Mesh))

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: Eval(Mesh%NDim)
INTEGER :: m,i,n,j,o,p,k1,k2,d

!!--begin--

Span = 0._KIND_MSH

DO m=1,NUM_Cells(Mesh)
 i = m
 !for each cell
 DO n=1,NUM_Faces(Mesh%Cells(i))
  j = ABS( Mesh%Cells(i)%FaceList(n) )

  !get span from k1->k2
  DO o=1,NUM_Verts(Mesh%Faces(j))
   k1 = Mesh%Faces(j)%VertList(o)
   DO p=1,NUM_Verts(Mesh%Faces(j))
    k2 = Mesh%Faces(j)%VertList(p)
    Eval = ABS(Vert(Mesh,k2)-Vert(Mesh,k1))
    DO d=1,NUM_Dimensions(Mesh)
     Span(d,i) = MAX( Span(d,i) , Eval(d) )
    END DO
   END DO
  END DO

 END DO
END DO
!!--end--
END FUNCTION



FUNCTION AverageSpan(Mesh)  RESULT(Span)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Span(Mesh%NDim)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: DimSpan_(Mesh%NDim,NUM_Cells(Mesh))
INTEGER :: m,i,n,j,o,p,k1,k2,d

!!--begin--

DimSpan_ = DimSpan(Mesh)

DO d=1,NUM_Dimensions(Mesh)
 Span(d) = SUM( DimSpan_(d,:) )/REAL(NUM_Cells(Mesh),KIND_MSH)
END DO

!!--end--
END FUNCTION


FUNCTION MaxSpan(Mesh)  RESULT(Span)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Span(Mesh%NDim)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: DimSpan_(Mesh%NDim,NUM_Cells(Mesh))
INTEGER :: m,i,n,j,o,p,k1,k2,d

!!--begin--

DimSpan_ = DimSpan(Mesh)

DO d=1,NUM_Dimensions(Mesh)
 Span(d) = MAXVAL( DimSpan_(d,:) )
END DO

!!--end--
END FUNCTION


FUNCTION MeshScale(Mesh)  RESULT(Scale)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Scale

!!--begin--

Scale = (Mesh%Domain%Cell%CellVolume)**(1._KIND_MSH/Mesh%NDim)

!!--end--
END FUNCTION



FUNCTION MinSpan(Mesh)  RESULT(Span)
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Span(Mesh%NDim)

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: DimSpan_(Mesh%NDim,NUM_Cells(Mesh))
INTEGER :: d

!!--begin--

DimSpan_ = DimSpan(Mesh)

DO d=1,NUM_Dimensions(Mesh)
 Span(d) = MINVAL( DimSpan_(d,:) )
END DO

!!--end--
END FUNCTION


SUBROUTINE UPDATE_IntegralRegions( Mesh , IntegralRegions , &
  VALUES )
!!#### PURPOSE
!! Update the <IntegralRegion> structure.

USE KND_IntrinsicTypes,ONLY: KIND_Rdp                                !!((01-A-KND_IntrinsicTypes.f90))
USE PRN_Table                                                        !!((11-B-PRN_Table.f90))

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_Rdp) ,INTENT(IN) :: VALUES(:)

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_IntegralRegion),POINTER :: IntegralRegions(:)

!!#### LOCAL VARIABLES
INTEGER :: Nr,r,i,i_

!!--begin--
IF( .NOT.ASSOCIATED(IntegralRegions) )THEN
 RETURN
END IF

Nr = SIZE(IntegralRegions)

DO r=1,Nr
 IntegralRegions(r) % integrals = 0._KIND_MSH

 IF( .NOT.ASSOCIATED(IntegralRegions(r)%set) )CYCLE

 DO i_=1,SIZE(IntegralRegions(r)%set)
  i = IntegralRegions(r)%set(i_)

  !volume integral
  IntegralRegions(r)%integrals(1) = &
    IntegralRegions(r)%integrals(1) + CellVolume(Mesh,i)

  !weighted integral
  IntegralRegions(r)%integrals(2) = &
    IntegralRegions(r)%integrals(2) + CellVolume(Mesh,i)*Values(i)

 END DO

 !scale integrals
 IntegralRegions(r)%integrals = IntegralRegions(r)%integrals*&
   IntegralRegions(r)%scale
END DO

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_IntegralRegions( IntegralRegions , Unit , MoreIndices , &
  PrintSets )
!!#### PURPOSE
!! Print the <IntegralRegion> structure.

USE FUN_Smush                                                        !!((03-A-FUN_Smush.f90))
USE PRN_Table                                                        !!((11-B-PRN_Table.f90))

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_IntegralRegion),POINTER :: IntegralRegions(:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: MoreIndices(:,:)
LOGICAL,OPTIONAL,INTENT(IN) :: PrintSets

!!#### LOCAL VARIABLES
INTEGER :: Nr,r,Nc,ic
LOGICAL :: PrintSets_
CHARACTER(72),ALLOCATABLE :: Table(:,:)
REAL(KIND_MSH) :: vint,vol,vavg
!!--begin--
IF( .NOT.ASSOCIATED(IntegralRegions) )THEN
 RETURN
END IF

Nr = SIZE(IntegralRegions)
Nc = 7
PrintSets_ = DEFAULT(.TRUE.,PrintSets)

!IF( PRESENT(MoreIndices) )THEN
! Nc = Nc+1
!END IF
IF( PrintSets_ )THEN
 Nc = Nc+1
END IF

ALLOCATE( Table(0:Nr,Nc) )
CALL CLEAR(Table)



Table(0,1) = "r"
Table(0,2) = "Label(r)"
!IF( PRESENT(MoreIndices) )THEN
! ic = SIZE(MoreIndices,2)+3
!ELSE
 ic = 3
!END IF
Table(0,ic) = "V(r)"
Table(0,ic+1) = "scale(r)"
Table(0,ic+2) = "Integ(r)"
Table(0,ic+3) = "Avg(r)"
IF( PrintSets_ )THEN
 Table(0,ic+4) = "set(r)"
END IF

!setup region indices and labels
DO r=1,Nr
 Table(r,1) = STR(r)
 Table(r,2) = IntegralRegions(r)%Label
END DO

!setup MoreIndices
!IF( PRESENT(MoreIndices) )THEN
! DO ic=3,SIZE(MoreIndices,2)+2
!  !set the column
!  Table(:,ic) = MoreIndices(:,ic-2)
!
!  !append the new indices to the column labels
!  Table(0,ic+2) = TRIM(Table(0,ic+2))//TRIM(MoreIndices(1,ic-2))
!  Table(0,ic+3) = TRIM(Table(0,ic+3))//TRIM(MoreIndices(1,ic-2))
!
! END DO
!ELSE
! ic = 3
!END IF

!setup region volume
DO r=1,Nr
 Table(r,ic) = STR(IntegralRegions(r) % integrals(1),"(Es21.14)")
END DO

!setup scaling
DO r=1,Nr
 Table(r,ic+1) = STR(IntegralRegions(r) % scale,"(Es21.14)")
END DO

!setup region total
DO r=1,Nr
 Table(r,ic+2) = STR(IntegralRegions(r) % integrals(2),"(Es21.14)")
END DO

!setup region average
DO r=1,Nr
 vint = IntegralRegions(r) % integrals(2)
 vol  = IntegralRegions(r) % integrals(1)
 vavg = vint/(TINY(vol)+vol) !so no divide by zero
 Table(r,ic+3) = STR(vavg,"(Es21.14)")
END DO

!setup region set
IF( PrintSets_ )THEN
 DO r=1,Nr
  Table(r,ic+4) = Smush(Sentence(STR(IntegralRegions(r)%set),delim=" "))
 END DO
END IF

CALL Print_Table(Table(0:Nr,1:Nc),Unit=Unit,Delim="|")

!!--end--
END SUBROUTINE





FUNCTION IntersectFace(Mesh,r0,Omega0,&
 SDIST,&
 InteriorCell_,&
 IncludeCells,&
 ExcludeFaces,&
 ExcludeCells,&
 tol,&
 PreferBoundaryFace,&
 KnownInteriorCell) RESULT(j)

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH) ,INTENT(IN)    :: r0(Mesh%Ndim)
REAL(KIND_MSH) ,INTENT(IN)    :: Omega0(:)

!!#### OPTIONAL INPUT
REAL(KIND_MSH) ,INTENT(IN),OPTIONAL :: tol
INTEGER        ,INTENT(IN),OPTIONAL :: ExcludeCells(:)
INTEGER        ,INTENT(IN),OPTIONAL :: ExcludeFaces(:)
INTEGER        ,INTENT(IN),OPTIONAL :: IncludeCells(:)
LOGICAL        ,INTENT(IN),OPTIONAL :: PreferBoundaryFace
INTEGER        ,INTENT(IN),OPTIONAL :: KnownInteriorCell

!!#### OPTIONAL OUTPUT
REAL(KIND_MSH) ,INTENT(OUT),OPTIONAL :: SDIST
INTEGER        ,INTENT(OUT),OPTIONAL :: InteriorCell_

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitFace) :: Xf
INTEGER                 :: i,jcheck,j_,j,Nj_,j1,j2
LOGICAL                 :: INTERSECT,PreferBoundaryFace_
REAL(KIND_MSH)          :: tol_,SDIST_,Pn(Mesh%NDim+1)
REAL(KIND_MSH)          :: P(Mesh%NDim),Ry(Mesh%Ndim,2),dotprod,U(Mesh%NDim)
INTEGER,ALLOCATABLE     :: jseq(:)
REAL(KIND_MSH),ALLOCATABLE :: PnDist(:)
INTEGER                 :: n
LOGICAL :: Noisy_=.FALSE.

!!--begin--


!initialize
IF( PRESENT(tol) )THEN
 tol_ = tol
END IF

PreferBoundaryFace_ = DEFAULT( .FALSE. , PreferBoundaryFace )

!initialize
j = 0
IF( PRESENT(SDIST) )THEN
 SDIST = Error(1._KIND_MSH)
END IF

!get cell using special check with Omega
IF( PRESENT(KnownInteriorCell) )THEN
 i = KnownInteriorCell
ELSE
 i = InteriorCell( Mesh , r0 , &
  IncludeCells , &
  ExcludeCells , &
  Omega=Omega0 , &
  tol=tol_ , &
  IncludeEdges=.TRUE.) !changed to .FALSE.
END IF

IF( PRESENT(InteriorCell_) )THEN
 InteriorCell_ = i
END IF

IF( i==0 )RETURN

INCLUDE "15-B-TBX_Mesh__IntersectFace.f90.bdy"

IF( PRESENT(SDIST) )THEN
 SDIST = SDIST_
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<FaceIndex>>
FUNCTION FaceIndex(Mesh,i,n)

!!#### PURPOSE
!! Return the index of the <n>th face
!! of cell <i> or 0 if it doesn't exist.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i,n

!!#### REQUIRED OUTPUT
INTEGER :: FaceIndex


!!--begin--

IF( i>=1 .AND. i<=NUM_Cells(Mesh) )THEN

 IF( n>=1 .AND. n<=NUM_Faces_CELLINDEX(Mesh,i) )THEN
  FaceIndex = ABS( Mesh%Cells(i)%FaceList(n) )
 ELSE
  FaceIndex = 0
 END IF

ELSE
 FaceIndex = 0
END IF

!!--end--
END FUNCTION





!!### FUNCTION <<VertIndex>>
FUNCTION VertIndex(Mesh,j,n)

!!#### PURPOSE
!! Return the <n>th vertex index of face <j>
!! or 0 if it doesn't exist.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j,n

!!#### REQUIRED OUTPUT
INTEGER :: VertIndex

!!--begin--

IF( j>=1 .AND. j<=NUM_Faces(Mesh) )THEN

 IF( n>=1 .AND. n<=NUM_Verts_FACEINDEX(Mesh,j) )THEN
  VertIndex = ABS( Mesh%Faces(j)%VertList(n) )
 ELSE
  VertIndex = 0
 END IF

ELSE
 VertIndex = 0
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<MAX_FacesPerCell>>
FUNCTION MAX_FacesPerCell(Mesh)

!!#### PURPOSE
!! Return the maximum number of faces per
!! cell.

!!#### DETAILS
!! Use to allocate stuff to the right size.


!!#### REQUIRED OUTPUT
INTEGER :: MAX_FacesPerCell

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--

MAX_FacesPerCell = 0
DO i=1,NUM_Cells(Mesh)

 MAX_FacesPerCell = MAX( MAX_FacesPerCell , &
                         NUM_Faces_CELLINDEX(Mesh,i) )

END DO

!!--end--
END FUNCTION




!!### SUBROUTINE <<GET_MemberCells>>
!###REMOVE THIS: DUPLICATE FEATURE WITH FaceToCellLink
SUBROUTINE GET_MemberCells(Mesh,MemberCells)

!!#### PURPOSE
!! Return the member cells.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: MemberCells(:,:)

!!#### LOCAL VARIABLES
INTEGER :: i,m,n,j

!!--begin--

ALLOCATE( MemberCells(2,NUM_Faces(Mesh)) )
MemberCells = 0

!enter the loop
DO i=1,NUM_Cells(Mesh)
 DO n=1,SIZE(Mesh%Cells(i)%FaceList)

  !get face
  j = ABS(Mesh%Cells(i)%FaceList(n))

  !get the next available slot for face j
  m = 1
  DO
   IF( MemberCells(m,j)==0 )EXIT
   m = m + 1
  END DO

  !! set one of the member cells face j belongs to
  MemberCells(m,j) = i

 END DO
END DO

!!--end--
END SUBROUTINE



!!### FUNCTION <<IntersectFaces_Omega>>
FUNCTION IntersectFaces_Omega(Mesh,Omega,InteriorCells,&
 SDIST,&
 ExcludeFaces,&
 PreferBoundaryFace ) RESULT(IntersectFaces)

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: Omega(:,:)
INTEGER       ,INTENT(IN) :: InteriorCells(NUM_Verts(Mesh),SIZE(Omega,2))

!!#### OPTIONAL INPUT
INTEGER       ,INTENT(IN),OPTIONAL :: ExcludeFaces(:)
LOGICAL       ,INTENT(IN),OPTIONAL :: PreferBoundaryFace

!!#### OPTIONAL OUTPUT
REAL(KIND_MSH),INTENT(OUT),OPTIONAL :: SDIST(NUM_Verts(Mesh),SIZE(Omega,2))

!!#### REQUIRED OUTPUT
INTEGER :: IntersectFaces(NUM_Verts(Mesh),SIZE(Omega,2))

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitFace) :: Xf
INTEGER                 :: i,j,j_,m,k,Nj_,j1,j2,jcheck
LOGICAL                 :: INTERSECT,PreferBoundaryFace_
REAL(KIND_MSH)          :: SDIST_,dotprod
REAL(KIND_MSH)          :: Ry(Mesh%Ndim,2),U(Mesh%NDim),P(Mesh%NDim)
REAL(KIND_MSH)          :: Omega0(Mesh%Ndim),r0(Mesh%Ndim),Pn(Mesh%NDim+1)
INTEGER,ALLOCATABLE     :: jseq(:)
REAL(KIND_MSH),ALLOCATABLE :: PnDist(:)
LOGICAL :: Noisy_=.FALSE.
INTEGER :: n
!!--begin--

!initialize
PreferBoundaryFace_ = DEFAULT(.FALSE.,PreferBoundaryFace)

!init face
IntersectFaces = 0
IF( PRESENT(SDIST) )THEN
 SDIST = Error(1._KIND_MSH)
END IF

DO m=1,SIZE(Omega,2)

 Omega0 = Omega(1:Mesh%NDim,m)

 DO k=1,NUM_Verts(Mesh)

  i = InteriorCells(k,m)
  IF( i==0 )CYCLE

  r0 = Vert(Mesh,k)

  j = 0
  !WRITE(*,*)
  !WRITE(*,"(2i5,2x)")i,m
  INCLUDE "15-B-TBX_Mesh__IntersectFace.f90.bdy"
  !WRITE(*,*)

  IntersectFaces(k,m) = j
  IF( j/=0 )THEN
   IF( PRESENT(SDIST) )THEN
    SDIST(k,m) = SDIST_
   END IF
  END IF

 END DO
END DO

!!--end--
END FUNCTION




SUBROUTINE celltravelsequence( Mesh , SA , r0 , Omega , sRange )
USE FUN_SIZEa                                                        !!((06-B-FUN_SIZEa.f90))
USE PAR_Constants_Rdp                                                !!((02-A-PAR_Constants_Rdp.f90))
USE SUB_FlagDuplicateScalars0                                        !!((05-A-SUB_FlagDuplicateScalars0.f90))

TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
!INTEGER        ,INTENT(INOUT) :: N
!INTEGER        ,INTENT(INOUT) :: IA(:)
REAL(KIND_MSH) ,POINTER :: SA(:)

REAL(KIND_MSH) ,INTENT(IN) :: r0(Mesh%NDim)
REAL(KIND_MSH) ,INTENT(IN) :: Omega(:)
REAL(KIND_MSH) ,INTENT(IN) :: sRange(2)

INTEGER :: i,j,N,N2
REAL(KIND_MSH) :: SDIST
REAL(KIND_MSH) :: r(SIZE(r0))
LOGICAL,ALLOCATABLE :: unique(:)

!!--begin--

CALL CLEARn(SA)
CALL REALLOCATE( sA , 10 , fill=Error(1._KIND_MSH) )

!init
SA(1) = sRange(1)
r = r0

!travel through mesh
N = 1
DO

 N = N + 1
 IF( N>SIZE(sA) )THEN
  CALL REALLOCATE( sA , SIZE(sA) , fill=Error(1._KIND_MSH) )
 END IF

 !get face intersected
 j = IntersectFace(Mesh,r,Omega,SDIST,i)

 !update intersection
 SA(N) = SA(N-1) + SDIST

 !got all we need
 IF( j==0 .OR. SA(N)>=SRange(2) )THEN
  SA(N) = SRange(2)
  EXIT

 !move to the intersection point
 ELSE
  r = r + Omega(1:Mesh%NDim)*SDIST
 END IF

END DO

CALL REALLOCATE( sA , SIZEa(sA)-SIZE(sA) )

!**begin** remove duplicates
N = SIZE(sA)
ALLOCATE(unique(N))

CALL FlagDuplicateScalars0(SA,reltol=1.d-12,unique=unique)

N = COUNT( unique )
SA(1:N) = PACK(sA,unique)
CALL REALLOCATE( sA , N - SIZE(sA) )
DEALLOCATE( unique )

IF( SIZE(SA)==1 )THEN
 CALL REALLOCATE( sA , 1 , fill=sA(1) )
END IF
!**end**


CALL split_interior_pts( sA )

!**begin** remove duplicates
N = SIZE(sA)
ALLOCATE(unique(N))

CALL FlagDuplicateScalars0(SA,unique=unique)

N = COUNT( unique )
SA(1:N) = PACK(sA,unique)
CALL REALLOCATE( sA , N - SIZE(sA) )
DEALLOCATE( unique )

IF( SIZE(SA)==1 )THEN
 CALL REALLOCATE( sA , 1 , fill=sA(1) )
END IF
!**end**

!CALL add_boundary_pts( sA )


!!--end--
END SUBROUTINE


!!### SUBROUTINE: <EVAL_celltraversal>
SUBROUTINE EVAL_celltraversal( Mesh , r0 , Omega , s01 , &
   N , iA , sA , tol , MemberCells )

!!#### PURPOSE
!! Starting from location <r0> traverse the mesh in
!! direction <Omega> and report on the cells travelled
!! through <iA> and the <N> locations of the change into
!! another cell <sA>.
!
!   r1 = r0 + Omega * sA(1)  through cell iA( 1 ) from  r0  -> r1
!   r2 = r0 + Omega * sA(2)  through cell iA( 2 ) from  r1  -> r2
!    .    .     .      .
!    .    .     .      .
!    .    .     .      .
!   rN = r0 + Omega * sA(N)  through cell iA(N-1) from rN-1 -> rN



!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER        ,INTENT(INOUT) :: N
INTEGER        ,POINTER       :: iA(:)
REAL(KIND_MSH) ,POINTER       :: sA(:)

!!#### REQUIRED INPUT
REAL(KIND_MSH) ,INTENT(IN) :: r0(Mesh%NDim)
REAL(KIND_MSH) ,INTENT(IN) :: Omega(Mesh%NDim)
REAL(KIND_MSH) ,INTENT(IN) :: s01

!!#### OPTIONAL INPUT
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: tol
INTEGER       ,OPTIONAL,POINTER    :: MemberCells(:,:)

!!#### LOCAL VARIABLES
INTEGER        :: i,j,jlast
INTEGER        :: i1,i2,istar
REAL(KIND_MSH) :: dsN,tol_
REAL(KIND_MSH) :: r(SIZE(r0))

!!--begin--

tol_ = Default( DEFAULT_tol , tol )

!initialize arrays
![speed] do not do so much de/re-allocating
!CALL CLEARn(SA)
!CALL CLEARn(iA)

!start off by allocating some
IF( .NOT.ASSOCIATED(sA) )THEN
 CALL REALLOCATE( sA , 1 , fill=Error(1._KIND_MSH) )
END IF
IF( .NOT.ASSOCIATED(iA) )THEN
 CALL REALLOCATE( iA , 1 , fill=Error(1) )
END IF

!initialize
N  = 1
sA = 0._KIND_MSH
iA = 0

!quick return
IF( s01==0._KIND_MSH )THEN
 RETURN
END IF


!initialize current position
r = r0

!travel through mesh and find the cells we hit and the distances
!traversed through each
jlast = Error(jlast)
DO

 !reallocate if ran out of space
 IF( N>SIZE(sA) )THEN
  CALL REALLOCATE( sA , N-SIZE(sA) , fill=Error(1._KIND_MSH) )
 END IF
 IF( N>SIZE(iA) )THEN
  CALL REALLOCATE( iA , N-SIZE(iA) , fill=Error(1) )
 END IF

 !we know which cell to try first
 IF( PRESENT(MemberCells) .AND. .NOT.IsError(jlast) )THEN

  !determine the cell we must be in
  i1=MemberCells(1,ABS(jlast))
  i2=MemberCells(2,ABS(jlast))
  IF( iA(N-1)==i1 )THEN
   istar = i2
  ELSE
   istar = i1
  END IF

  !only look in cell <istar>
  j = IntersectFace(Mesh,r,Omega,dsN,i,ExcludeCells=iA(1:N-1),&
    ExcludeFaces=(/jlast/),&
    tol=tol_,KnownInteriorCell=istar)

 !don't know what cells try yet
 ELSE

  !get face intersected
  j = IntersectFace(Mesh,r,Omega,dsN,i,ExcludeCells=iA(1:N-1),&
    ExcludeFaces=(/jlast/),&
    tol=tol_ )

 END IF

 !exit conditions
 IF( j==0 )THEN
  N=N-1
  IF( N>0 )THEN
   IF( .NOT.IsApprox(sA(N),s01) )THEN
    WRITE(*,*)"major error: missed some cells"
   END IF
  END IF

  EXIT

 !otherwise move down to the intersection point
 !and update intersection distance and cell
 ELSE

  IF( N==1 )THEN
   SA( N ) = dsN
  ELSE
   sA( N ) = sA(N-1) + dsN
  END IF
  iA( N ) = i

  !if we went past end point
  IF( sA(N)>=s01 )THEN
   sA(N) = s01
   EXIT

  !we are still inside range
  ELSE

   r = r + Omega(1:Mesh%NDim)*dsN
   N = N + 1

  END IF

 END IF

 jlast = j

END DO

!wrapup arrays and reallocate
![speed]
!N = SIZEa(sA)
!CALL REALLOCATE( sA , N-SIZE(sA) )
!CALL REALLOCATE( iA , N-SIZE(iA) )

!!--end--
END SUBROUTINE






SUBROUTINE add_boundary_pts( sA )
USE FUN_SIZEa                                                        !!((06-B-FUN_SIZEa.f90))
REAL(KIND_MSH),POINTER :: SA(:)
REAL(KIND_MSH) :: eps,ol,or,bl,br,fi,fb
INTEGER :: N

!add boundary layers
N = SIZEa( sA )
eps = 1.d-12

!get fractions of boundary and internal
fb = c_1-eps
fi = eps

!get left and right boundary layers
ol = sA(1)
or = sA(N)
bl = ol*fb + sA(2  )*fi
br = or*fb + sA(N-1)*fi

!add more elements to the array
CALL REALLOCATE( sA , 2 , fill=Error(1._KIND_MSH) )
N = N + 2

!bump down values
sA(3:N) = sA(2:N-3)
!and set boundary layers
sA(  1:2) = (/ol,bl/)
SA(N-1:N) = (/br,or/)

END SUBROUTINE



SUBROUTINE split_interior_pts( SA )
REAL(KIND_MSH),POINTER :: SA(:)
REAL(KIND_MSH),POINTER :: SB(:)

REAL(KIND_MSH) :: eps

INTEGER :: N,j1,j2,i

!!--begin--

N = SIZE(SA)

sB =>NULL()
ALLOCATE( sB(2*N) )

SB(1  ) = SA(1)
SB(2*N) = SA(N)

eps = SQRT(EPSILON(1._KIND_MSH))

!interior points
j1 = 1
j2 = 2
DO i=2,N-1

 j1 = j1 + 2
 SB(j1) = SA(i) - eps*( SA(i) - SA(i-1) )

 j2 = j2 + 2
 SB(j2) = SA(i) + eps*( SA(i+1) - SA(i) )

END DO

!end points
SB(2    ) = SA(1) + eps*( SA( 2 ) - SA( 1 ) )
SB(2*N-1) = SA(N) - eps*( SA( N ) - SA(N-1) )

CALL CLEARn( SA )
SA => SB
SB => NULL()

!!--end--
END SUBROUTINE


SUBROUTINE PrintFunctionOnFacesAndCells(Unit,Mesh,f)
!!#### PURPOSE
!! Call the routines to print functions on
!! faces and cells.

!!#### REQUIRED INPUT
!! * unit to write to <Unit>
!! * mesh object <Mesh>
!! * function interface <f>
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTERFACE
 FUNCTION f(x,y)
  USE KND_IntrinsicTypes,ONLY: KIND_Rdp                              !!((01-A-KND_IntrinsicTypes.f90))
  REAL(KIND_Rdp),INTENT(IN) :: x,y
  REAL(KIND_Rdp) :: f
 END FUNCTION
END INTERFACE

!!--begin--

CALL PrintFunctionOnCells(Unit,Mesh,f)
CALL PrintFunctionOnFaces(Unit,Mesh,f)

!!--end--
END SUBROUTINE


SUBROUTINE PrintFunctionOnCells(Unit,Mesh,f,Method)
!!#### PURPOSE
!! Print a function on the cells.

USE KND_IntrinsicTypes,ONLY: KIND_Rdp                                !!((01-A-KND_IntrinsicTypes.f90))

!!#### REQUIRED INPUT
!! * unit to write to <Unit>
!! * mesh object <Mesh>
!! * function interface <f>
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTERFACE
 FUNCTION f(x,y)
  USE KND_IntrinsicTypes,ONLY: KIND_Rdp                              !!((01-A-KND_IntrinsicTypes.f90))
  REAL(KIND_Rdp),INTENT(IN) :: x,y
  REAL(KIND_Rdp) :: f
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
CHARACTER(*),INTENT(IN),OPTIONAL :: Method

!!#### LOCAL VARIABLES
INTEGER              :: i
REAL(KIND_MSH)       :: CC(2)
REAL(KIND_Rdp)       :: fval
TYPE(varying_string) :: Method_

!!--begin--

IF( PRESENT(Method) )THEN
 Method_ = Method
ELSE
 Method_ = "Centroid"
END IF


DO i=1,NUM_Cells(Mesh)

 !select evaluation method
 SELECT CASE(STR(Method))

  CASE("Centroid") ; CC   = CellCentroid(Mesh,i)
                     fval = f(CC(1),CC(2))

  CASE("Average")  ; fval = CellAverage_F(Mesh,i,f)

  CASE DEFAULT     ; CC   = CellCentroid(Mesh,i)
                     fval = f(CC(1),CC(2))
 END SELECT

 !write to file
 WRITE(Unit,"(a)") TRIM(STR(CC(1),"(Es21.14)"))//" "//&
                   TRIM(STR(CC(2),"(Es21.14)"))//" "//&
                   TRIM(STR(fval,"(Es21.14)"))

END DO

!!--end--
END SUBROUTINE



SUBROUTINE PrintFunctionOnFaces(Unit,Mesh,f,Method)
!!#### PURPOSE
!! Print a function on the faces.

USE KND_IntrinsicTypes,ONLY: KIND_Rdp                                !!((01-A-KND_IntrinsicTypes.f90))

!!#### REQUIRED INPUT
!! * unit to write to <Unit>
!! * mesh object <Mesh>
!! * function interface <f>
INTEGER        ,INTENT(IN) :: Unit
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTERFACE
 FUNCTION f(x,y)
  USE KND_IntrinsicTypes,ONLY: KIND_Rdp                              !!((01-A-KND_IntrinsicTypes.f90))
  REAL(KIND_Rdp),INTENT(IN) :: x,y
  REAL(KIND_Rdp) :: f
 END FUNCTION
END INTERFACE

!!#### OPTIONAL INPUT
CHARACTER(*),INTENT(IN),OPTIONAL :: Method

!!#### LOCAL VARIABLES
INTEGER              :: j
REAL(KIND_MSH)       :: FC(2)
REAL(KIND_Rdp)       :: fval
TYPE(varying_string) :: Method_

!!--begin--

IF( PRESENT(Method) )THEN
 Method_ = Method
ELSE
 Method_ = "Centroid"
END IF


DO j=1,NUM_Faces(Mesh)

 !select evaluation method
 SELECT CASE(STR(Method))

  CASE("Centroid") ; FC   = FaceCentroid(Mesh,j)
                     fval = f(FC(1),FC(2))

  CASE("Average")  ; fval = FaceAverage_F(Mesh,j,f)

  CASE DEFAULT     ; FC   = FaceCentroid(Mesh,j)
                     fval = f(FC(1),FC(2))
 END SELECT

 !write to file
 WRITE(Unit,"(a)") TRIM(STR(FC(1),"(Es21.14)"))//" "//&
                   TRIM(STR(FC(2),"(Es21.14)"))//" "//&
                   TRIM(STR(fval,"(Es21.14)"))

END DO

!!--end--
END SUBROUTINE




FUNCTION InteriorCells_Omega(Mesh,Omega,tol,IncludeEdges)
!!#### PURPOSE
!! Return the set of interior cells for all verts registered
!! with the mesh, using directions Omega(:,m), m=1,...,Nm.

!!#### NOTES
!! This version is O(Ni) where Ni is the number of cells, while
!! the InteriorCells(Mesh,r,Omega) for each single vertex
!! is O(Ni*Nk)~O(Ni^2) for quadrilateral/hexahedral meshes.


!!#### REQUIRED INPUT/OUTPUT
!! * mesh structure
!! * point
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_MSH) ,INTENT(IN)    :: Omega(:,:)

!!#### OPTIONAL INPUT
!! * a tolerance region about the boundary to include
!!   if <IncludeEdges=T> or exclude <IncludeEdges=F>, default is
!!   <tol=10*EPSILON>.
!! * whether to include edges or not (default is <IncludeEdges=T>)
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: tol
LOGICAL       ,INTENT(IN),OPTIONAL :: IncludeEdges

INTEGER :: InteriorCells_Omega(NUM_Verts(Mesh),SIZE(Omega,2))

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitCell) :: Xc
INTEGER :: i,j,k,j_,m,Nk_,k_
LOGICAL :: Check
REAL(KIND_MSH) :: tol_,increment
LOGICAL :: IncludeEdges_
REAL(KIND_MSH) :: V(Mesh%Ndim),s,CC(Mesh%Ndim),Pn(Mesh%Ndim+1),r1(2)
INTEGER,POINTER :: VertSet(:)

!!--begin--
!get defaults
tol_ = DEFAULT( DEFAULT_tol , tol )
IncludeEdges_ = DEFAULT( .TRUE. , IncludeEdges )
!WRITE(*,*)"begin of InteriorCells_Omega"
!initialize
InteriorCells_Omega = 0
NULLIFY(VertSet)
!CALL CLEARn( VertSet )

!put cell loop on the outside because getting the explicit
!cells can take a while
DO i=1,NUM_Cells(Mesh)
 !write(*,*)'cell i=',i
 Xc = ExplicitCell( Mesh , i )
 CC = CellCentroid(Mesh,i)
 IF( ASSOCIATED(VertSet) )DEALLOCATE(VertSet)
 CALL GET_CellVertBoundarySet(Mesh,i,VertSet)
 Nk_ = SIZEa(VertSet)

 DO k_=1,Nk_
  k = VertSet(k_)

  !do a preliminary search on if this vert is on the cell
  DO j_=1,SIZE(Mesh%Cells(i)%FaceList)
   j = ABS(Mesh%Cells(i)%FaceList(j_))
   IF( ANY(Mesh%Faces(j)%VertList==k) )THEN
    j = 0
    EXIT
   END IF
  END DO
  IF( j/=0 )CYCLE

  !get vert
  V = Mesh%Verts(:,k)

  !search for each direction
  !if( VERBOSE )write(*,*)'start of direction search'
  DO m=1,SIZE(Omega,2)
   !if( VERBOSE )write(*,*)'m=',m
   increment = SQRT(tol_)*xyNORM_V( V-CC )
   r1 = V + Omega(1:Mesh%Ndim,m)*increment
   check = INTERIOR_XcP( Xc , r1 , CC , tol=0._KIND_MSH , &
             IncludeEdges=IncludeEdges_ )
   IF( check )THEN
    InteriorCells_Omega(k,m) = i
   END IF

  END DO

 END DO
 
 !write(*,*)'end of cell i=',i

 CALL CLEAN( Xc )

END DO

IF( ASSOCIATED(VertSet) )DEALLOCATE(VertSet)

!!--end--
END FUNCTION



FUNCTION InteriorCell(Mesh,r,IncludeCells,&
  ExcludeCells,tol,IncludeEdges,Omega) RESULT(i)
!!#### PURPOSE
!! Return the cell index in which a point <r> resides.

!!#### REQUIRED INPUT/OUTPUT
!! * mesh structure
!! * point
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_MSH) ,INTENT(IN)    :: r(Mesh%Ndim)

!!#### OPTIONAL INPUT
!! * a list of cells to include
!! * a list of cells to exclude
!! * a tolerance region about the boundary to include
!!   if <IncludeEdges=T> or exclude <IncludeEdges=F>, default is
!!   <tol=10*EPSILON>.
!! * whether to include edges or not (default is <IncludeEdges=T>)
!! * directionality requirement (a point r1 = r + Omega*s,
!!   where s is the distance from the cell centroid
!!   to the plane created from r and Omega, must also
!!   be contained within the cell---valid for convex cells
!!   only), tolerance=0 and IncludeEdges=T is fixed for
!!   this check
INTEGER       ,INTENT(IN),OPTIONAL :: IncludeCells(:)
INTEGER       ,INTENT(IN),OPTIONAL :: ExcludeCells(:)
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: tol
LOGICAL       ,INTENT(IN),OPTIONAL :: IncludeEdges
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: Omega(:)

!!#### LOCAL VARIABLES
TYPE(TYPE_ExplicitCell) :: Xc
INTEGER :: i,ip,Nip,t
LOGICAL :: Check
REAL(KIND_MSH) :: tol_,tols_(2),increment
LOGICAL :: IncludeEdges_
REAL(KIND_MSH) :: r1(Mesh%Ndim),s,CC(Mesh%Ndim),Pn(Mesh%Ndim+1),u(Mesh%Ndim)

!!--begin--

!get defaults
tol_ = DEFAULT( DEFAULT_tol , tol )
IncludeEdges_ = DEFAULT( .TRUE. , IncludeEdges )

tols_ = (/0._KIND_MSH,tol_/)

!initialize
CALL CLEAN( Xc )
i = 0

!initialize 2
IF( PRESENT(IncludeCells) )THEN
 Nip = SIZE(IncludeCells)
 Mesh%i = IncludeCells(Nip)
ELSE
 Nip = NUM_Cells(Mesh)
 Mesh%i = Nip
END IF

!start iterating
tol_loop: DO t=1,2

DO ip=1,Nip

 !1. check for exclusion
 IF( PRESENT(ExcludeCells) )THEN
  IF( ANY(Mesh%i==ExcludeCells) )THEN
   GOTO 34
  END IF
 END IF
 IF( Mesh%i==0 )THEN
  GOTO 34
 END IF

 !2. get cell and centroid
 CC = CellCentroid(Mesh,Mesh%i)

 check = .FALSE.

 Xc = ExplicitCell(Mesh,Mesh%i)

 IF( PRESENT(Omega) )THEN
  increment = SQRT(tol_)*xyNORM_V( r-CC )
  r1 = r + Omega(1:Mesh%Ndim)*increment
  check = INTERIOR_XcP( Xc , r1 , CC , tol=tols_(t) , &
    IncludeEdges=IncludeEdges_ )
 ELSE
  check = INTERIOR_XcP( Xc , r , CC , tol=tols_(t) , &
  IncludeEdges=IncludeEdges_ )
 END IF

 !4. clean up
 CALL CLEAN( Xc )

 !5. if we pass check
 IF( check )THEN
  i = Mesh%i
  EXIT tol_loop
 END IF

 34 CONTINUE

 !6. update new cell and continue with check
 IF( PRESENT(IncludeCells) )THEN
  Mesh%i = IncludeCells(ip)
 ELSE
  Mesh%i = MOD(Mesh%i,Nip)+1
 END IF

END DO

END DO tol_loop

!!--end--
END FUNCTION



SUBROUTINE BoundaryFacesFromVert(Mesh,k,jb1,jb2,jd1,jd2)

!!#### REQUIRED INPUT
!! * mesh
!! * vert
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: k

!!#### REQUIRED OUTPUT
!! * boundary faces
!! * domain faces
INTEGER,INTENT(OUT) :: jb1,jb2,jd1,jd2

!!#### LOCAL VARIABLES

!!--begin--

IF( ASSOCIATED(Mesh%VertToFaceLink) )THEN
    !WRITE(*,*)'Using VertToFaceLink'
    !WRITE(*,*)'Nverts=',NUM_Verts(MESH)
    jb1 = Mesh % VertToFaceLink(k) % list(1)
    jb2 = Mesh % VertToFaceLink(k) % list(2)
    !WRITE(*,*)'jb1 jb2=',jb1,' ',jb2
    jd1=0
    jd2=0
    if( jb1>0 )jd1 = DomainFace(Mesh,jb1)
    if( jb2>0 )jd2 = DomainFace(Mesh,jb2)
    !WRITE(*,*)'jd1 jd2=',jd1,' ',jd2
ELSE
    STOP 'the verttofacelink must be setup'
END IF

!!--end--
END SUBROUTINE

SUBROUTINE SETUP_VertToFaceLink(Mesh)

!!#### REQUIRED INPUT
!! * mesh
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED OUTPUT
!! * mesh%verttofacelink

!!#### LOCAL VARIABLES
INTEGER :: j,jd,k,jb1,jb2

!!--begin--

IF( .NOT.ASSOCIATED(Mesh%VertToFaceLink) )THEN

    
    !NEW HERE
    !WRITE(*,*)'Nverts=',NUM_Verts(MESH)
    ALLOCATE(Mesh%VertToFaceLink(NUM_Verts(Mesh)))
    DO k=1,NUM_Verts(Mesh)
        !WRITE(*,*)'k=',k
        ALLOCATE(Mesh%VertToFaceLink(k)%list(1:2))
        Mesh%VertToFaceLink(k)%list(1:2)=0
        jb1=0
        jb2=0
        LOOP_Search: DO j=1,NUM_Faces(Mesh)
            !check only boundary faces
            IF( IsBoundaryFace(Mesh,j) )THEN
                !make sure the vert k is part of the face j
                IF( ANY(ABS(Mesh%faces(j)%vertlist)==k) )THEN
                    !place first boundary face
                    IF( jb1==0 )THEN
                        jb1 = j
                        !WRITE(*,*)'set 1',k,j,jb1
                        Mesh%VertToFaceLink(k)%list(1)=jb1 !NEW HERE
                        !place second boundary face
                    ELSE
                        jb2 = j
                        !WRITE(*,*)'set 2',k,j,jb2
                        Mesh%VertToFaceLink(k)%list(2)=jb2 !NEW HERE
                        !exit when the second boundary face is found
                        EXIT LOOP_Search
                    ENDIF
                ENDIF
            END IF
        END DO LOOP_Search

    END DO

END IF

!!--end--
END SUBROUTINE


!!### FUNCTION: <SymmetryPoint>
FUNCTION SymmetryPoint(Mesh,Pn,P,tol) RESULT(k)

!!#### PURPOSE
!! Reflect a point <P> about a symmetry plane <Pn>
!! and return the closest vertex <k>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_MSH) ,INTENT(IN) :: Pn(3)
REAL(KIND_MSH) ,INTENT(IN) :: P(2)

!!#### OPTIONAL INPUT
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: tol

!!#### REQUIRED OUTPUT
INTEGER        :: k
REAL(KIND_MSH) :: P_ref(2)

!!--begin--

!get reflected point
P_ref = xyREFLECT_PnP( Pn , P )

!get index of point
k = GET_Vert(Mesh,vert=P_ref,tol=tol)

!!--end--
END FUNCTION


!!### FUNCTION: <SymmetryFace>
FUNCTION SymmetryFace(Mesh,Pn,FC,FN,tol) RESULT(j)

!!#### PURPOSE
!! Reflect a face centroid <FC> with direction
!! <FN> about a symmetry plane <Pn>
!! and return the closest face which has the opposite direction
!! <j>.  For now we don't care about the direction part.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
REAL(KIND_MSH) ,INTENT(IN) :: Pn(3)
REAL(KIND_MSH) ,INTENT(IN) :: FC(2),FN(2)

!!#### OPTIONAL INPUT
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: tol

!!#### REQUIRED OUTPUT
INTEGER        :: j
REAL(KIND_MSH) :: P_ref(2)

!!--begin--

!get reflected point
P_ref = xyREFLECT_PnP( Pn , FC )

!get index of point
j = GET_Face(Mesh,FaceCentroid=P_ref,tol=tol)

!!--end--
END FUNCTION


!!### FUNCTION: <SymmetryDirection>
FUNCTION SymmetryDirection(DirCos,Pn,U) RESULT(m)

!!#### PURPOSE
!! Determine which angular direction <m> the reflection of
!! <U> off of plane <Pn> is closest to.

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: U(:)
REAL(KIND_MSH),INTENT(IN) :: DirCos(:,:)
REAL(KIND_MSH),INTENT(IN) :: Pn(SIZE(U)+1)

!!#### RESULT
INTEGER :: m

!!#### LOCALS
REAL(KIND_MSH) :: proximity(1:SIZE(DirCos,2))
REAL(KIND_MSH) :: U_ref(SIZE(U))
INTEGER        :: m_

!!--begin--


!get xy-reflection
U_ref(1:2) = xyREFLECT_PnV( Pn , REAL(U(1:2),KIND_MSH) )

!z-direction is unchanged by this xy refection
IF( SIZE(DirCos,1)>2 )U_ref(3) = U(3)

DO m_=1,SIZE(DirCos,2)
 proximity(m_) = NormEll2( DirCos(:,m_)-U_ref )
END DO
m = MINLOC(proximity,1)


!!--end--
END FUNCTION




!!### FUNCTION: <GET_Direction>
FUNCTION GET_Direction( Directions , U ) RESULT(m)

!!#### PURPOSE
!! Determine which direction <U> is closest to.

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: Directions(:,:)
REAL(KIND_MSH),INTENT(IN) :: U(:)

!!#### RESULT
INTEGER :: m

!!#### LOCALS
REAL(KIND_MSH) :: proximity(1:SIZE(Directions,2))
INTEGER :: m_

!!--begin--

!long way
DO m_=1,SIZE(Directions,2)
 proximity(m_) = NormEll2( Directions(:,m_)-U )
END DO
m = MINLOC(proximity,1)

!short way
!m = NEARLOC( Directions , U )

!!--end--
END FUNCTION



!!### SUBROUTINE <<SliceCell>>
SUBROUTINE SliceCell(Mesh,i,theta,Sli,&
    OutSide,InSide,OutVert,InVert,corig,crot,&
    CHECK_CONVEX_CCW, is_convex )

!!#### PURPOSE
!! Slice a cell of the mesh into triangular prisms (in 3D).
!! In 2D (the only method installed right now), the
!! slices are quadrilaterals with two parallel opposite
!! sides.

!!#### MODULES
USE FUN_xySLICE                                 !!((09-A-FUN_xySLICES.f90))
USE FUN_NewFile
USE LIB_xy,ONLY: xyIS_CONVEX_CCW_Pg

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: theta

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) ,POINTER    :: Sli(:,:)
INTEGER        ,POINTER    :: OutSide(:)
INTEGER        ,POINTER    :: InSide(:)
INTEGER        ,POINTER    :: OutVert(:)
INTEGER        ,POINTER    :: InVert(:)
REAL(KIND_MSH) ,INTENT(OUT) :: corig(3),crot(3)

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: CHECK_CONVEX_CCW
LOGICAL,OPTIONAL,INTENT(OUT) :: is_convex

!!#### LOCAL VARIABLES
INTEGER                    :: N,l
REAL(KIND_MSH),ALLOCATABLE :: Pg(:,:)
TYPE(TYPE_ExplicitCell)    :: Xc
INTEGER    ,ALLOCATABLE    :: OldOutSide(:)
INTEGER    ,ALLOCATABLE    :: OldInSide(:)
INTEGER    ,ALLOCATABLE    :: OldOutVert(:)
INTEGER    ,ALLOCATABLE    :: OldInVert(:)
REAL(KIND_MSH) :: a,c(2),angle(100)
LOGICAL :: each_pass(100)
INTEGER :: ns,nvl,nvr,loop
INTEGER,ALLOCATABLE :: vleft(:),eleft(:),vright(:),eright(:)
LOGICAL :: is_convex_

!!--begin--

NULLIFY( Sli , OutSide, InSide, OutVert, InVert )

SELECT CASE( NUM_Dimensions(Mesh) )
 CASE(2)

    !get the polygon
    Xc = ExplicitCell(Mesh,i)
    N  = NUM_Points( Xc )
    ALLOCATE( Pg(2,N), Sli(3,N) )
    ALLOCATE( OldOutSide(N-1) , OldInSide(N-1) , OldInvert(N) , OldOutVert(N) )
    ALLOCATE( vleft(N), eleft(N-1), vright(N) , eright(N-1) )
    Pg = POLYGON_Xc( Xc )
    CALL CLEAN_Xc( Xc )

    IF( PRESENT(CHECK_CONVEX_CCW) )THEN
        IF( CHECK_CONVEX_CCW )THEN
            is_convex_ = xyIS_CONVEX_CCW_Pg(n,pg,angle=angle(1:n),each_pass=each_pass(1:n))
            IF( .NOT. is_convex_ )THEN
                IF( PRESENT(is_convex) )THEN
                    is_convex=is_convex_
                    RETURN
                ELSE
                    WRITE(*,*)'the polygon representation of cell i=',i,&
                              ' is not convex counter-clockwise, as required by SLICE'
                    WRITE(*,'(4a12)')'x','y','angle','pass'
                    DO loop=1,n
                        WRITE(*,'(3es12.5,g12.0)')pg(1,loop),pg(2,loop),angle(loop),each_pass(loop)
                    END DO
                    STOP
                END IF
            END IF
        END IF
    END IF

    CALL xySLICE_Pgt_Rdp( N , Pg , theta , TOL=1.d-8,&
        ns=ns, sli=sli, a=a, c=c, vin=OldInVert, vout=OldOutVert, ein=OldInSide, eout=OldOutSide, &
        nvl=nvl, vleft=vleft, eleft=eleft, nvr=nvr, vright=vright, eright=eright, &
        corig=corig, crot=crot )
    
    ALLOCATE( OutSide(ns-1) , InSide(ns-1) , InVert(ns) , OutVert(ns) )
    CALL REALLOCATE(Sli,(/0,ns-n/))

    !map local face index (1,2,3,...,etc.) values to global (-131,233,etc.)
    DO l=1,ns-1
     OutSide(l) = Mesh%Cells(i)%FaceList( OldOutSide(l) )
      InSide(l) = Mesh%Cells(i)%FaceList(  OldInSide(l) )
    END DO

    !map local vert index to global
    !  for first N-1 verts can do like this
    OutVert=0
    InVert=0
    DO l=1,ns-1
     IF( OldOutVert(l)/=0 )THEN
      OutVert(l) = GET_VertOnFace(Mesh,OutSide(l),Last=.TRUE.)
     END IF
     IF( OldInVert(l)/=0 )THEN
      InVert(l) = GET_VertOnFace(Mesh,InSide(l),First=.TRUE.)
     END IF
    END DO
    !  then final is different
    IF( OldOutVert(ns)/=0 )THEN
     OutVert(ns) = GET_VertOnFace(Mesh,OutSide(ns-1),First=.TRUE.)
    END IF
    IF( OldInVert(ns)/=0 )THEN
     InVert(ns)  = GET_VertOnFace(Mesh,InSide(ns-1) ,Last=.TRUE.)
    END IF
    DEALLOCATE( Pg,OldOutVert,OldInVert,OldInSide,OldOutSide )


 CASE DEFAULT
    WRITE(*,*)"Non-2D Not installed yet!"
    STOP

END SELECT

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<SETUP_MeshCellIterator>>
SUBROUTINE SETUP_MeshCellIterator(Mesh,theta,CellDeps,order)

!!#### PURPOSE
!! Setup the mesh iterator according to a direction
!! <U>.

!!#### DEPENDENCIES
USE USR_GraphTraversal                                               !!((06-C-USR_GraphTraversal.f90))


!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH) ,INTENT(IN) :: theta

!!#### REQUIRED OUTPUT
INTEGER        ,POINTER    :: CellDeps(:,:)

!!#### LOCAL VARIABLES
INTEGER :: Nmax,Ni,Nout,Nin
INTEGER :: n,i,i1,i2,j,l
INTEGER       ,POINTER :: FaceDeps(:,:),MemberCells(:,:)
INTEGER       ,POINTER :: AllOutGoing(:),AllInComing(:)
INTEGER       ,POINTER :: OutSide(:),InSide(:)
INTEGER       ,POINTER :: OutVert(:),InVert(:)
REAL(KIND_MSH),POINTER :: Sli(:,:)
INTEGER       ,POINTER :: order(:)
REAL(KIND_MSH) :: corig(3),crot(3)

!!--begin--

Nmax = MAX_FacesPerCell(Mesh)
Ni   = NUM_Cells(Mesh)

ALLOCATE( FaceDeps(Nmax,Ni) )
FaceDeps = Error(FaceDeps)


!! get the dependencies of each cell on some faces
DO i=1,Ni

 NULLIFY( AllOutGoing , AllInComing , &
   Inside , Outside , Invert , OutVert , Sli )

 CALL SliceCell(Mesh,i,theta,Sli,OutSide,InSide,OutVert,InVert,corig,crot,CHECK_CONVEX_CCW=.TRUE.)

 DO l=1,SIZE(Sli,2)-1
  CALL ADD_TO_SET( AllOutGoing , OutSide(l) )
  CALL ADD_TO_SET( AllInComing , InSide(l)  )
 END DO
 CALL FINALIZE_SET( AllOutGoing )
 CALL FINALIZE_SET( AllInComing )

 Nout = SIZE(AllOutGoing)
 Nin  = SIZE(AllInComing)

 FaceDeps(1:Nin,i) = AllInComing

 DEALLOCATE( AllOutGoing , AllInComing , &
   Inside , Outside , Invert , OutVert , Sli )

END DO


ALLOCATE( CellDeps(Nmax,Ni) )
CellDeps = 0

!! get the 2 cells which face j has, MemberCells(1:2,j)
CALL GET_MemberCells(Mesh,MemberCells)

DO i=1,Ni

 Nin = SIZEa( FaceDeps(:,i) )

 DO n=1,Nin
  j = FaceDeps(n,i)
  j = ABS( j )
  i1 = MemberCells(1,j)
  i2 = MemberCells(2,j)
  IF( i==i1 )THEN
   CellDeps(n,i) = i2
  ELSE
   CellDeps(n,i) = i1
  END IF
 END DO

END DO

ALLOCATE( order(1:Ni) )
order = GraphOrdering_rec(CellDeps)

DEALLOCATE( FaceDeps , MemberCells )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<Test_DualMesh>>
SUBROUTINE Test_DualMesh(Mesh,fdbk,Noisy)

!!#### PURPOSE
!! Test the dual mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
LOGICAL,INTENT(IN),OPTIONAL :: Noisy
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
TYPE(TYPE_Mesh) :: LOMesh
LOGICAL :: Noisy_
INTEGER :: j,jj
REAL(KIND_MSH) :: CBV,EV,FC(Mesh%NDim)
REAL(KIND_MSH),ALLOCATABLE :: EssentialVar(:),CellBasedVar(:)
INTEGER,POINTER :: FaceMapping(:)
LOGICAL :: Pass
REAL :: tin,tout,dt

!!--begin--

Noisy_ = Default(.FALSE.,Noisy)

!test the mesh dual

NULLIFY(FaceMapping)
CALL UpdateAndDump( fdbk_comment , fdbk , s="[[MSH]] Creating cell-based dual mesh")
CALL CPU_TIME(tin)
CALL CREATE_Mesh_CellBasedDual(Mesh,LOMesh,FaceMapping)
CALL CPU_TIME(tout)
dt=tout-tin
CALL UpdateAndDump( fdbk_comment , fdbk , s="[[MSH]] Finished creating cell-based dual mesh in &
  &[time="//STRTIME(dt)//"].")

IF( Noisy_ )THEN
 CALL UpdateAndDump( fdbk_comment , fdbk , s="[[MSH]] Printing Essential mesh to <EMesh.txt> and&
   &Cell-based mesh to <CBMesh.txt>.")
 CALL PRINT_Mesh(Mesh,Unit=NewFile("EMesh.txt",IfOpened="Close"))
 CALL PRINT_Mesh(LOMesh,Unit=NewFile("CBMesh.txt",IfOpened="Close"))
END IF

!get a distance from origin array for the Essential Mesh
ALLOCATE( EssentialVar(NUM_Faces(Mesh)) )
DO j=1,NUM_Faces(Mesh)
 FC = FaceCentroid(Mesh,j)
 EssentialVar(j) = SQRT( FC(1)**2 + FC(2)**2 )
END DO

!get a distance from the origin array for the CellBased Mesh
ALLOCATE( CellBasedVar(NUM_Faces(LOMesh)) )
DO j=1,NUM_Faces(LOMesh)
 FC = FaceCentroid(LOMesh,j)
 CellBasedVar(j) = SQRT( FC(1)**2 + FC(2)**2 )
END DO

!compare the two
Pass = .TRUE.
DO j=1,NUM_Faces(LOMesh)
 CBV = CellBasedVar(j)
 jj = ABS( FaceMapping(j) )
 IF( jj==0 )CYCLE
 EV = EssentialVar(jj)

 Pass = Pass .AND.IsApprox(CBV,EV,tol=EPSILON(CBV)*10)
 IF( .NOT.Pass )THEN
  CALL UpdateAndDump( fdbk_warning , fdbk , s="[[MSH]] The essential mesh and cell-based mesh&
    & are inconsistent for face [j="//TRIM(STR(j))//"] for the essential mesh.")
  EXIT
 END IF

END DO

IF( Pass )THEN
 CALL UpdateAndDump( fdbk_comment , fdbk , s="[[MSH]] The essential mesh and&
   & cell-based mesh are consistent [test=Pass]")
END IF

!!--end--
END SUBROUTINE



SUBROUTINE MAPTO_CellBased(CBMesh,FaceMapping,Interfaces,CBVar,&
  EVar)
TYPE(TYPE_Mesh)      ,POINTER :: CBMesh
INTEGER              ,POINTER :: FaceMapping(:)
TYPE(TYPE_Interfaces),POINTER :: Interfaces(:)
REAL(KIND_MSH)       ,POINTER :: CBVar(:,:) !output
REAL(KIND_MSH)       ,POINTER :: EVar(:,:)

INTEGER :: n,j,Ns,s,je,m
!!--begin--

CBVar = ERROR(1._KIND_MSH)

!first do the boundary faces
DO j=1,NUM_Faces(CBMesh)
 IF( IsBoundaryFace(CBMesh,j) )THEN
  je = FaceMapping(j)
!  WRITE(*,*)"cell-based boundary face j=",j
!  WRITE(*,*)"essential boundary face je=",je
  CBVar(:,j) = EVar(:,je)
 END IF
END DO


!map all the subfaces
DO m=1,SIZE(Interfaces)
 n=Interfaces(m)%master
 IF( ASSOCIATED(Interfaces(m)%subs) )THEN
  Ns=SIZE(Interfaces(m)%subs)
  DO s=1,Ns
   j = Interfaces(m)%subs(s)
   je = ABS( FaceMapping(j) )
   CBVar(:,j) = EVar(:,je)
  END DO
 END IF
END DO

!map master faces
DO m=1,SIZE(Interfaces)
 n=Interfaces(m)%master
 IF( ASSOCIATED(Interfaces(m)%subs) )THEN
  Ns=SIZE(Interfaces(m)%subs)
  CBVar(:,n) = 0.d0
  DO s=1,Ns
   j = Interfaces(m)%subs(s)
   CBVar(:,n) = CBVar(:,n) + CBVar(:,j)*FaceArea(CBMesh,j)
  END DO
  CBVar(:,n) = CBVar(:,n)/FaceArea(CBMesh,n)
 END IF
END DO

!!--end--
END SUBROUTINE



SUBROUTINE MAPTO_CellBased_Boundary(CBMesh,FaceMapping,&
  JBdryCB,CBVar,EVar,Inv_JBdryE,JBdryE)
TYPE(TYPE_Mesh),INTENT(IN) :: CBMesh
INTEGER       ,POINTER :: FaceMapping(:)
INTEGER       ,POINTER :: JbdryCB(:)
REAL(KIND_MSH),POINTER :: CBVar(:,:) !output
REAL(KIND_MSH),POINTER :: EVar(:,:)
INTEGER       ,POINTER :: Inv_JbdryE(:),JbdryE(:)
INTEGER :: n,jb,j,je,m
!!--begin--
CBVar = ERROR(1._KIND_MSH)
DO n=1,SIZE(JBdryCB)

 jb = JBdryCB(n)
 j = ABS( FaceMapping(jb) )
 IF( j==0 )CYCLE

 m = Inv_JBdryE(n)

 CBVar(:,n) = EVar(:,m)
END DO
!!--end--
END SUBROUTINE



SUBROUTINE GET_Inv_JBdryE(Inv_JBdryE,FaceMapping,JBdryCB,JBdryE)
INTEGER       ,POINTER :: FaceMapping(:)
INTEGER       ,POINTER :: Inv_JBdryE(:)
INTEGER       ,POINTER :: JBdryCB(:)
INTEGER       ,POINTER :: JBdryE(:)
INTEGER :: n,jb,j,m
!!--begin--

Inv_JBdryE = ERROR(1)

DO n=1,SIZE(JBdryCB)
 jb = JBdryCB(n)
 j = ABS( FaceMapping(jb) )
 IF( j==0 )CYCLE
 !find in HI_Bdry where it is equal to j
 DO m=1,SIZE(JBdryE)
  IF( j==JBdryE(m) )THEN
   EXIT
  END IF
 END DO
 Inv_JBdryE(n) = m
END DO
!!--end--
END SUBROUTINE



FUNCTION TEST_FaceIntegration_WARSA(Mesh,cerr,relcerr,&
  Noisy,Noisy_unit) RESULT(Pass)
!!#### PURPOSE
!! Test how accurately we can integrate the function
!! $$(x^2-x^4)*(y^2-y^4)$$
!! over faces of this mesh.
USE VAR_Units                                                        !!((03-A-VAR_Units.f90))

TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(OUT),OPTIONAL :: cerr
REAL(KIND_MSH),INTENT(OUT),OPTIONAL :: relcerr
LOGICAL,INTENT(IN),OPTIONAL :: Noisy
INTEGER,INTENT(IN),OPTIONAL :: Noisy_Unit
LOGICAL :: Pass

!!#### LOCAL VARIABLES
TYPE(varying_string)   :: VS
INTEGER :: k1,k2,Nk,j
REAL(KIND_MSH) :: Ls(2,2),x1,x2,y1,y2
REAL(KIND_MSH) :: cerr_,relcerr_,FA,intref,int
REAL(KIND_MSH) :: b,relb,slop
LOGICAL :: Noisy_
CHARACTER(*),PARAMETER :: Proc_="TEST_FaceIntegration_WARSA"
INTEGER :: Noisy_Unit_
CHARACTER(*),PARAMETER :: FMT2="(i8,1x,f8.3,1x,2(Es23.13,1x),2(Es9.2,1x))"
CHARACTER(*),PARAMETER :: FMT1="(a8,1x,a8,1x,2(a23,1x),2(a9,1x))"
!!--begin--
INCLUDE "08-B-USR_Mesh__IsFinalized.f90.sup"

!!--begin--

Noisy_ = DEFAULT(.FALSE.,Noisy)
Noisy_Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Noisy_Unit)

IF( Noisy_ )THEN
 WRITE(Noisy_unit_,FMT1)"j","FA","tapack","reference","absdiff","reltoarea"
END IF

cerr_=0.d0
relcerr_=0.d0
DO j=1,NUM_Faces(Mesh)

 Nk = SIZE(Mesh%Faces(abs(j))%VertList)
 k1 = Mesh%Faces(abs(j))%VertList(1)
 k2 = Mesh%Faces(abs(j))%VertList(Nk)

 Ls(:,1) = Vert(Mesh,k1)
 Ls(:,2) = Vert(Mesh,k2)

 FA = FaceArea(Mesh,j)

 !integrate them
 int = xyINTEGRALF_Ls( f , Ls  ,reltol=1.d-12,abstol=1.d-12)

 x1 = Ls(1,1)
 x2 = Ls(1,2)
 y1 = Ls(2,1)
 y2 = Ls(2,2)

 IF( ABS(y2-y1)>ABS(x2-x1) )THEN
  slop = ABS(x2-x1)/ABS(y2-y1)
  intref = -((y1 - y2)*(x1**4*(70*y1**4 + 35*y1**3*y2 + &
               15*y1**2*(-6 + y2**2) + 5*y1*y2*(-6 + y2**2) + &
               y2**2*(-6 + y2**2)) + &
            x1**3*x2*(35*y1**4 + 40*y1**3*y2 + &
               16*y1*y2*(-3 + y2**2) + 30*y1**2*(-2 + y2**2) + &
               y2**2*(-18 + 5*y2**2)) + &
            3*x1**2*(5*(-6 + x2**2)*y1**4 + &
               10*(-2 + x2**2)*y1**3*y2 + &
               6*y1**2*(7 - 2*y2**2 + 2*x2**2*(-1 + y2**2)) + &
               y2**2*(7 - 2*y2**2 + x2**2*(-12 + 5*y2**2)) + &
               y1*y2*(21 - 6*y2**2 + 2*x2**2*(-9 + 5*y2**2))) + &
            x1*x2*(5*(-6 + x2**2)*y1**4 + 16*(-3 + x2**2)*y1**3*y2 + &
               4*y1*y2*(21 - 12*y2**2 + 2*x2**2*(-6 + 5*y2**2)) + &
               3*y1**2*(21 - 18*y2**2 + 2*x2**2*(-3 + 5*y2**2)) + &
               y2**2*(63 - 30*y2**2 + 5*x2**2*(-12 + 7*y2**2))) + &
            x2**2*((-6 + x2**2)*y1**4 + (-18 + 5*x2**2)*y1**3*y2 + &
               3*y1**2*(7 - 12*y2**2 + x2**2*(-2 + 5*y2**2)) + &
               2*y2**2*(63 - 45*y2**2 + 5*x2**2*(-9 + 7*y2**2)) + &
               y1*y2*(63 - 60*y2**2 + 5*x2**2*(-6 + 7*y2**2)))))/630.d0
  IF( y2<y1 )THEN
   intref = -intref
  END IF
 ELSE
  slop = ABS(y2-y1)/ABS(x2-x1)
  intref = -((x1 - x2)*(x1**4*(70*y1**4 + 35*y1**3*y2 + &
               15*y1**2*(-6 + y2**2) + 5*y1*y2*(-6 + y2**2) + &
               y2**2*(-6 + y2**2)) + &
            x1**3*x2*(35*y1**4 + 40*y1**3*y2 + &
               16*y1*y2*(-3 + y2**2) + 30*y1**2*(-2 + y2**2) + &
               y2**2*(-18 + 5*y2**2)) + &
            3*x1**2*(5*(-6 + x2**2)*y1**4 + &
               10*(-2 + x2**2)*y1**3*y2 + &
               6*y1**2*(7 - 2*y2**2 + 2*x2**2*(-1 + y2**2)) + &
               y2**2*(7 - 2*y2**2 + x2**2*(-12 + 5*y2**2)) + &
               y1*y2*(21 - 6*y2**2 + 2*x2**2*(-9 + 5*y2**2))) + &
            x1*x2*(5*(-6 + x2**2)*y1**4 + 16*(-3 + x2**2)*y1**3*y2 + &
               4*y1*y2*(21 - 12*y2**2 + 2*x2**2*(-6 + 5*y2**2)) + &
               3*y1**2*(21 - 18*y2**2 + 2*x2**2*(-3 + 5*y2**2)) + &
               y2**2*(63 - 30*y2**2 + 5*x2**2*(-12 + 7*y2**2))) + &
            x2**2*((-6 + x2**2)*y1**4 + (-18 + 5*x2**2)*y1**3*y2 + &
               3*y1**2*(7 - 12*y2**2 + x2**2*(-2 + 5*y2**2)) + &
               2*y2**2*(63 - 45*y2**2 + 5*x2**2*(-9 + 7*y2**2)) + &
               y1*y2*(63 - 60*y2**2 + 5*x2**2*(-6 + 7*y2**2)))))/630.d0
  IF( x2<x1 )THEN
   intref = -intref
  END IF
 END IF
 intref = intref*SQRT(1.d0+slop**2)

 b =ABS(int-intref)
 relb=b/FA
 IF( Noisy_ )THEN
  WRITE(Noisy_unit_,FMT2)j,FA,int,intref,b,relb
 END IF
 cerr_ = MAX(cerr_,b)
 relcerr_ = MAX(relcerr_,relb)
END DO

IF( PRESENT(cerr) )THEN
 cerr = cerr_
END IF
IF( PRESENT(relcerr) )THEN
 relcerr = relcerr_
END IF

Pass = relcerr_<=1.d-12

!!--end--
CONTAINS

FUNCTION f(x,y)
REAL(KIND_MSH),INTENT(IN) :: x,y
REAL(KIND_MSH) :: f
f=(x**2-x**4)*(y**2-y**4)
END FUNCTION

END FUNCTION


END MODULE
