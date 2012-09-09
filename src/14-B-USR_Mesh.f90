!!# MODULE <<USR_Mesh>>
MODULE USR_Mesh

!!## PURPOSE
!! Defines a 1-3 dimensional Mesh type, composed of the
!! DOMAIN, FACE, and CELL types.


!!## DETAILS
!!
!! *CONTROL* components
!
!! 1a. %NDim component
!!   The number of dimensions.
!
!! 1b. %MeshType component
!!   The type of the mesh <STRING>.
!
!! 1c. %FINALIZED
!!   Whether the Mesh type is finalized---checked for
!!   inconsistency and reallocated to fit data exactly.
!
!
!! *VERT* components
!
!! 2a. %Verts
!!   Values of the verts.
!!
!! 2b. %NVerts
!!   The number of verts currently stored (not necessarily
!!   equal to <SIZE(Mesh%Verts,2)> because we preallocate space
!!   for verts when we run out of space---finalizing reallocates
!!   <Mesh%Verts> to fit <Mesh%NVerts>.)
!
!! 2c. %k
!!   The "current" vert.  This allows procedures operating on
!!   Mesh verts in succession to pass the current vert through
!!   the mesh type itself instead of as an argument.
!
!! 2d. %VertLabels
!!   The simple-pointer-list <spList> of labels for the vertices.
!
!
!! *FACE* components
!
!! 3a. %Faces
!!   Values of the faces.
!!
!! 3b. %NFaces
!!   The number of faces currently stored (not necessarily
!!   equal to <SIZE(Mesh%Faces,2)> because we preallocate space
!!   for verts when we run out of space---finalizing reallocates
!!   <Mesh%Faces> to fit <Mesh%NFaces>.)
!
!! 3c. %j
!!   The "current" face.  This allows procedures operating on
!!   Mesh faces in succession to pass the current face through
!!   the mesh type itself instead of as an argument.
!
!! 3d. %FaceLabels
!!   The simple-pointer-list <spList> of labels for the faces.
!
!
!! *CELL* components
!
!! 4a. %Cells
!!   Values of the Cells.
!!
!! 4b. %NCells
!!   The number of Cells currently stored (not necessarily
!!   equal to <SIZE(Mesh%Cells,2)> because we preallocate space
!!   for verts when we run out of space---finalizing reallocates
!!   <Mesh%Cells> to fit <Mesh%NCells>.)
!
!! 4c. %j
!!   The "current" cell.  This allows procedures operating on
!!   Mesh cells in succession to pass the current cell through
!!   the mesh type itself instead of as an argument.
!
!! 4d. %CellLabels
!!   The simple-pointer-list <spList> of labels for the cells.
!
!
!! *DOMAIN* components
!
!! 5. %Domain
!!   The domain specifications for this mesh.
!

!!## FORTRAN STANDARDS
USE ISO_varying_string                     !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S        !!((01-A-KND_IntrinsicTypes.f90))
USE KND_Mesh                               !!((05-B-KND_Mesh.f90))

!!## EXTERNAL PARAMETERS
USE PAR_ComputationalGeometry              !!((02-A-PAR_ComputationalGeometry.f90))
USE PAR_Units     ,ONLY: window_unit       !!((02-A-PAR_Units.f90))
USE PAR_Mesh                               !!((06-B-PAR_Mesh.f90))

!!## GLOBAL USER MODULES
USE USR_Edge                               !!((06-B-USR_Edge.f90))
USE USR_Domain                             !!((13-B-USR_Domain.f90))
USE USR_Face                               !!((11-B-USR_Face.f90))
USE USR_Cell                               !!((12-B-USR_Cell.f90))

USE USR_ExplicitCell                       !!((13-B-USR_ExplicitCell.f90))
USE USR_ExplicitFace                       !!((12-B-USR_ExplicitFace.f90))
USE USR_SimpleList                         !!((09-B-USR_SimpleList.f90))
USE USR_Iptr                               !!((03-C-USR_Iptr.f90))

!!## EXTERNAL PROCEDURES
USE SUB_Reallocate                         !!((04-B-SUB_Reallocate.f90))
USE FUN_STR                                !!((05-B-FUN_STR.f90))
USE FUN_VSTR                               !!((05-B-FUN_VSTR.f90))
USE FUN_Error                              !!((04-A-FUN_Error.f90))
USE SUB_Stop                               !!((04-B-SUB_Stop.f90))
USE FUN_Default                            !!((04-A-FUN_Default.f90))
USE SUB_CLEAR                              !!((04-A-SUB_CLEAR.f90))
USE FUN_IsError                            !!((05-A-FUN_IsError.f90))
USE FUN_NewFile                            !!((05-B-FUN_NewFile.f90))
USE PRN_Table                              !!((11-B-PRN_Table.f90))
USE FUN_ptr_Sequence                       !!((04-A-FUN_ptr_Sequence.f90))
USE SUB_Swap                               !!((04-A-SUB_Swap.f90))

!!## GLOBAL LIBRARIES
USE LIB_GenericPhrases,ONLY: OUT_OF_BOUNDS !!((07-B-LIB_GenericPhrases.f90))

!!## GLOBAL TOOLBOXES
USE TBX_ComputationalGeometry              !!((09-A-TBX_ComputationalGeometry.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PARAMETERS
INTEGER,PARAMETER :: DEFAULT_dNk    = 1
INTEGER,PARAMETER :: DEFAULT_dNj    = 1
INTEGER,PARAMETER :: DEFAULT_dNi    = 1

!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_ ="USR_Mesh"
CHARACTER(*),PARAMETER :: file_="14-B-USR_Mesh.f90"
PRIVATE :: mod_,file_

!!## TYPE DEFINITION for universal mesh type
TYPE TYPE_Mesh

 !CONTROL variables
 INTEGER                             ,POINTER :: NDim          => NULL()
 INTEGER                             ,POINTER :: MeshType      => NULL()
 INTEGER                             ,POINTER :: FaceStructure => NULL()
 LOGICAL                             ,POINTER :: FINALIZED     => NULL()

 !DOMAIN variables
 TYPE(TYPE_Domain)                   ,POINTER :: Domain        => NULL()

 !VERT variables
 INTEGER                             ,POINTER :: NVerts        => NULL()
 REAL(KIND_MSH)                      ,POINTER :: Verts(:,:)    => NULL()
 INTEGER                             ,POINTER :: k             => NULL()
 CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),POINTER :: VertLabels(:) => NULL()

 !FACE variables
 INTEGER                             ,POINTER :: NFaces        => NULL()
 TYPE(TYPE_Face)                     ,POINTER :: Faces(:)      => NULL()
 INTEGER                             ,POINTER :: j             => NULL()
 CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),POINTER :: FaceLabels(:) => NULL()

 !CELL variables
 INTEGER                             ,POINTER :: NCells        => NULL()
 TYPE(TYPE_Cell)                     ,POINTER :: Cells(:)      => NULL()
 INTEGER                             ,POINTER :: i             => NULL()
 CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),POINTER :: CellLabels(:) => NULL()

 !Reverse Link Variables
 TYPE(TYPE_Iptr),POINTER :: VertToFaceLink(:) => NULL()
 INTEGER        ,POINTER :: FaceToCellLink(:,:) => NULL()

END TYPE

!!## PUBLIC ACCESS LIST
!! * kinds
PUBLIC :: KIND_MSH
!! * parameters
PUBLIC :: LEN_MESH_LABEL
!! * types
PUBLIC :: TYPE_Mesh
PUBLIC :: TYPE_Cell
PUBLIC :: TYPE_Face
PUBLIC :: TYPE_Edge
PUBLIC :: TYPE_Domain
PUBLIC :: TYPE_ExplicitFace
PUBLIC :: TYPE_ExplicitCell
!! * type construction
PUBLIC :: ExplicitCell_Mesh
PUBLIC :: ExplicitCell_Domain
PUBLIC :: ExplicitFace_Mesh
PUBLIC :: ExplicitFace_Domain
!! * add/removing verts, faces, cells
PUBLIC :: ADD_Vert_Mesh
PUBLIC :: ADD_Face_Mesh
PUBLIC :: ADD_Cell_Mesh
PUBLIC :: REMOVE_Vert_Mesh
PUBLIC :: REMOVE_Face_Mesh
PUBLIC :: REMOVE_Cell_Mesh
PUBLIC :: ADD_DomainVert_Mesh
PUBLIC :: ADD_DomainFace_Mesh
PUBLIC :: ADD_DomainCell_Mesh
PUBLIC :: INSERT_Cell_Mesh
PUBLIC :: REORDER_Faces_Mesh
!! * finalize for calculations
PUBLIC :: FINALIZE_Mesh
!! * numbers inquiry
PUBLIC :: NUM_Dimensions_Mesh
PUBLIC :: NUM_Verts_Mesh , NUM_Verts_FACEINDEX
PUBLIC :: NUM_Faces_Mesh , NUM_Faces_CELLINDEX
PUBLIC :: NUM_Edges_Mesh
PUBLIC :: NUM_Cells_Mesh
!! * counting
PUBLIC :: COUNT_Cells_Mesh
PUBLIC :: COUNT_Faces_Mesh
PUBLIC :: COUNT_Verts_Mesh
!! * status inquiry
PUBLIC :: HasDomain_Mesh
!! * regular returns of components
PUBLIC :: EdgeShapes_Mesh
PUBLIC :: EdgeShape_Mesh
PUBLIC :: FaceShape_Mesh
PUBLIC :: CellShape_Mesh
PUBLIC :: CellLabel_Mesh
PUBLIC :: FaceLabel_Mesh
!! * pointer returns of components
PUBLIC :: ptr_Verts_Mesh
PUBLIC :: ptr_VertList_Mesh
PUBLIC :: ptr_Faces_Mesh
PUBLIC :: ptr_Cells_Mesh
PUBLIC :: ptr_Edges_Mesh
PUBLIC :: ptr_FaceCentroid_Mesh
PUBLIC :: ptr_FaceList_Mesh
PUBLIC :: ptr_FaceShape_Mesh
PUBLIC :: ptr_DomainShape_Mesh
PUBLIC :: ptr_CellShape_Mesh
PUBLIC :: ptr_EdgeShape_Mesh
PUBLIC :: ptr_FaceArea_Mesh
!! * allocation,deallocation,reallocation
PUBLIC :: ALLOCATE_Mesh
PUBLIC :: DEALLOCATE_Mesh
!PUBLIC :: DEALLOCATE_Faces_Mesh
!PUBLIC :: DEALLOCATE_Cells_Mesh
!PUBLIC :: DEALLOCATE_Control_Mesh
!PUBLIC :: DEALLOCATE_Domain_Mesh
!PUBLIC :: DEALLOCATE_Verts_Mesh
!! * copy mesh subroutine
PUBLIC :: COPY_Mesh
PUBLIC :: IsEqual_Mesh
PUBLIC :: UPDATE_FaceToCellLink


!!## MODULE PROCEDURES
CONTAINS


!!### SUBROUTINE <<UPDATE_FaceToCellLink>>
SUBROUTINE UPDATE_FaceToCellLink( Mesh , j, i_list )

!!#### PURPOSE
!! Update the FaceToCellLink of the Mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * Mesh type <Mesh>
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: j
INTEGER,OPTIONAL,INTENT(IN) :: i_list(2)
INTEGER :: i,m,j_

!!--begin--

IF( .NOT.ASSOCIATED(Mesh%FaceToCellLink) )THEN
    ALLOCATE(Mesh%FaceToCellLink(2,Mesh%NFaces))
    Mesh%FaceToCellLink=0
ELSE IF( Mesh%NFaces>SIZE(Mesh%FaceToCellLink,2) )THEN
    CALL REALLOCATE(Mesh%FaceToCellLink,(/0,Mesh%NFaces-SIZE(Mesh%FaceToCellLink,2)/))
END IF
    
IF( PRESENT(j) .AND. PRESENT(i_list) )THEN
    !#DEBUG_UPDATE_FaceToCellLink WRITE(*,'(1x,a,i5,a,2i5)')'UPDATE_FaceToCellLink (partial update) j=',j,' i_list=',i_list
    Mesh%FaceToCellLink(:,j)=i_list
ELSE
    !#DEBUG_UPDATE_FaceToCellLink WRITE(*,*)'UPDATE_FaceToCellLink (full update)'
    Mesh%FaceToCellLink=0
    DO i=1,Mesh%NCells
        DO m=1,SIZE(Mesh%Cells(i)%FaceList)
            j_=Mesh%Cells(i)%FaceList(m)
            IF( j_>0 )THEN
                Mesh%FaceToCellLink(1,ABS(j_))=i
            ELSE
                Mesh%FaceToCellLink(2,ABS(j_))=i
            END IF
        END DO
    END DO
END IF



!!--end--
END SUBROUTINE UPDATE_FaceToCellLink


FUNCTION ADD_DomainVert_Mesh( Mesh , Vert ) RESULT(kd)
!!#### PURPOSE
!! Adds a Vert to the DOMAIN of Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: Vert(1:Mesh%NDim)

!!#### REQUIRED OUTPUT
INTEGER :: kd

kd = ADD_Vert_Domain(Mesh%Domain,Vert)

END FUNCTION

FUNCTION ADD_DomainFace_Mesh( Mesh , FaceShape , &
  FaceNormal , VertList , Edges , FaceArea , FaceCentroid ) RESULT(jd)
!!#### PURPOSE
!! Adds a Face to the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: FaceShape
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceNormal(:)
INTEGER       ,OPTIONAL,INTENT(IN) :: VertList(:)
TYPE(TYPE_Edge),OPTIONAL,INTENT(IN) :: Edges(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceArea
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceCentroid(:)

!!#### REQUIRED OUTPUT
INTEGER :: jd

jd = ADD_Face_Domain(Mesh%Domain,FaceShape , &
  FaceNormal , VertList , Edges , FaceArea , FaceCentroid )

END FUNCTION


FUNCTION ADD_DomainCell_Mesh( Mesh , CellShape , &
  CellCoeff , FaceList , CellVolume , CellCentroid , &
  CellSurfaceArea )  RESULT(id)
!!#### PURPOSE
!! Adds a Cell to the Domain.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: CellShape
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellCoeff(:)
INTEGER       ,OPTIONAL,INTENT(IN) :: FaceList(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellVolume
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellCentroid(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellSurfaceArea

!!#### REQUIRED OUTPUT
INTEGER :: id

id = ADD_Cell_Domain(Mesh%Domain,CellShape , &
  CellCoeff , FaceList , CellVolume , CellCentroid , &
  CellSurfaceArea )

END FUNCTION


FUNCTION ADD_Vert_Mesh( Mesh , Vert , VertLabel ) RESULT(k)
!!#### PURPOSE
!! Adds a Vert to the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
REAL(KIND_MSH)              ,INTENT(IN) :: Vert(1:Mesh % NDim)
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: VertLabel

!!#### REQUIRED OUTPUT
INTEGER :: k

!!#### LOCAL VARIABLES
INTEGER :: Nk,Nkl

!!--begin--

IF( ASSOCIATED(Mesh%Verts) )THEN
 Nk=SIZE(Mesh%Verts,2)
ELSE
 Nk=0
END IF
IF( ASSOCIATED(Mesh%VertLabels) )THEN
 Nkl=SIZE(Mesh%VertLabels)
ELSE
 Nkl=0
END IF

!! Increment number of Verts.
Mesh % NVerts = Mesh % NVerts + 1
k             = Mesh%NVerts
Mesh % k      = k

!! Reallocate Mesh if needed.
IF( k > Nk )THEN
 CALL REALLOCATE( Mesh%Verts,      dn=(/0,MAX(DEFAULT_dNk*Nk,2)/) )
END IF
!! Reallocate Vert Labels if needed.
IF( k > Nkl )THEN
 IF( .NOT. ASSOCIATED(Mesh%VertLabels) )THEN
  ALLOCATE(Mesh%VertLabels(10))
 ELSE
  CALL REALLOCATE( Mesh%VertLabels, dn=MAX(DEFAULT_dNk*Nkl,2) )
 END IF
END IF

!! Now add new Vert and VertLabel.
Mesh % Verts( : , k ) = Vert
Mesh%VertLabels(k)=VertLabel

!!--end--
END FUNCTION


FUNCTION REMOVE_Vert_Mesh( Mesh , k , VertLabel ) RESULT(Vert)
!!#### PURPOSE
!! Removes a Vert from the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER :: k

!!#### REQUIRED OUTPUT
REAL(KIND_MSH)                                   :: Vert(1:Mesh % NDim)
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),INTENT(OUT) :: VertLabel

!!#### IDENTIFICATION
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: proc_="REMOVE_Vert"

!!#### LOCAL VARIABLES
INTEGER :: NVerts
TYPE(varying_string) :: VS

!!--begin--
!! Get number of verts.
NVerts = Mesh%NVerts

!! Out of bounds error.
IF( k<1 .OR. k>NVerts )THEN
 VS = OUT_OF_BOUNDS(mod_,proc_,"k",STR(k),STR(1),STR(NVerts))
 CALL Stop( s=STR(VS) )
 VS = ""
END IF

!! Return label and vertex removed.
VertLabel = Mesh%VertLabels(k)
Vert      = Mesh%Verts   (:,k)

!! Bump verts down.
Mesh%Verts   (:,k:NVerts-1) = Mesh%Verts   (:,k+1:NVerts)
Mesh%VertLabels(k:NVerts-1) = Mesh%VertLabels(k+1:NVerts)

!! Decrement number of verts and update current vert index.
Mesh % NVerts = NVerts - 1
Mesh % k      = Mesh % NVerts

!!--end--
END FUNCTION


FUNCTION ADD_Face_Mesh( Mesh , FaceShape , FaceLabel , &
  FaceNormal , VertList , Edges , FaceArea , FaceCentroid , SubFaces , overwrite ) RESULT(j)
!!#### PURPOSE
!! Adds a Face to the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER                    ,INTENT(IN) :: FaceShape
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: FaceLabel

!!#### OPTIONAL INPUT
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceNormal(:)
INTEGER       ,OPTIONAL,INTENT(IN) :: VertList(:)
TYPE(TYPE_Edge),OPTIONAL,INTENT(IN) :: Edges(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceArea
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceCentroid(:)
INTEGER       ,OPTIONAL,INTENT(IN) :: SubFaces(:)
INTEGER       ,OPTIONAL,INTENT(IN) :: overwrite

!!#### REQUIRED OUTPUT
INTEGER :: j,Nj,Njl,j_

!!--begin--

IF( PRESENT(overwrite) )THEN
    j=overwrite
    CALL DEALLOCATE_Face( Mesh%Faces(j) )   
ELSE
    Mesh % NFaces = Mesh%NFaces + 1
    j = Mesh%NFaces
    
    IF( ASSOCIATED(Mesh%Faces) )THEN
	Nj=SIZE(Mesh%Faces,1)
    ELSE
	Nj=0
    END IF
    IF( ASSOCIATED(Mesh%FaceLabels) )THEN
	Njl=SIZE(Mesh%FaceLabels)
    ELSE
	Njl=0
    END IF

    !! Reallocate Mesh%Faces if needed.
    IF( j > Nj )THEN
	CALL REALLOCATE_Faces( Mesh%Faces , dn=MAX(DEFAULT_dNj*Nj,2) )
    END IF
    IF( j > Njl )THEN
	IF( .NOT. ASSOCIATED(Mesh%FaceLabels) )THEN
	    ALLOCATE(Mesh%FaceLabels(10))
	ELSE
	    CALL REALLOCATE( Mesh%FaceLabels, dn=MAX(DEFAULT_dNj*Njl,2) )
	END IF
    END IF

END IF

Mesh % FaceLabels(j) = FaceLabel
Mesh % j = j

!! Now add new Face and FaceLabel.
CALL ALLOCATE_Face( Mesh % Faces( j ) , &
  FaceShape=FaceShape       , &
  FaceNormal=FaceNormal     , &
  VertList=VertList         , &
  Edges=Edges               , &
  FaceArea=FaceArea         , &
  FaceCentroid=FaceCentroid , &
  SubFaces=SubFaces )

!! If any of the subfaces of other faces are equal to the new face
!! (only happens with overwrite) then remove them
IF( PRESENT(overwrite) )THEN
 DO j_=1,Mesh%NFaces
  IF( ASSOCIATED(Mesh%Faces(j_)%SubFaces) )THEN
   CALL CLEAN_Subfaces(Mesh%Faces(j_)%Subfaces,j)
  END IF
 END DO
END IF

!this removal operation is O(Nf) but runs every time a face is overwritten,
!so it could make the operation of adding faces O(Nf^2)---unexcusable!

!!--end--
END FUNCTION


SUBROUTINE CLEAN_SubFaces( Subfaces , jremove )
INTEGER,POINTER :: SubFaces(:)
INTEGER,INTENT(IN) :: jremove

INTEGER :: Ns,s,js

!!--begin--

IF( .NOT.ASSOCIATED(SubFaces) )RETURN

Ns = SIZE(SubFaces)
s=0
DO
 s=s+1
 IF( s>Ns )ExIT
 js = SubFaces(s)
 IF( js==jremove )THEN
  SubFaces(s:Ns-1) = SubFaces(s+1:Ns)
  Ns = Ns - 1
  CALL REALLOCATE( SubFaces , -1 )
 END IF
END DO
!!--end--
END SUBROUTINE


SUBROUTINE REALLOCATE_Faces( Faces , dN )
!!#### PURPOSE
!! Reallocate an array of faces, increasing (or decreasing)
!! the number of faces by dN.

!!#### REQUIRED INPUT
!! * array of faces
!! * number of new faces to add
TYPE(TYPE_Face),POINTER    :: Faces(:)
INTEGER       ,INTENT(IN) :: dN

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Faces_(:)
INTEGER :: j,m,n

!!--begin--
!copy data into faces
IF( ASSOCIATED(Faces) )THEN
 n = SIZE(Faces,1)
ELSE
 n = 0
END IF
m = n+dN

!allocate the copy
NULLIFY( Faces_ )
IF( m>0 )THEN
 ALLOCATE( Faces_(1:m) )
END IF

DO j=1,MIN(n,m)
 !point
 Faces_(j) = Faces(j)
 !nullify
 CALL NULLIFY_Face( Faces(j) )
END DO

!deallocate the %Faces pointer
IF( n>0 )THEN
 DEALLOCATE( Faces )
END IF

!associate the real with the copy
Faces => Faces_
!nullify the copy
Faces_ => NULL()

!!--end--
END SUBROUTINE


SUBROUTINE REALLOCATE_Cells( Cells , dN )
!!#### PURPOSE
!! Reallocate an array of cells, increasing (or decreasing)
!! the number of cells by dN.

!!#### REQUIRED INPUT
!! * array of cells
!! * number of new cells to add
TYPE(TYPE_Cell),POINTER    :: Cells(:)
INTEGER       ,INTENT(IN) :: dN

!!#### LOCAL VARIABLES
TYPE(TYPE_Cell),POINTER :: Cells_(:)
INTEGER :: j,m,n

!!--begin--
!copy data into cells
IF( ASSOCIATED(Cells) )THEN
 n = SIZE(Cells,1)
ELSE
 n = 0
END IF
m = n+dN

!allocate the copy
NULLIFY( Cells_ )
IF( m>0 )THEN
 ALLOCATE( Cells_(1:m) )
END IF

DO j=1,MIN(n,m)
 !point
 Cells_(j) = Cells(j)
 !nullify
 CALL NULLIFY_Cell( Cells(j) )
END DO

!deallocate the %Cells pointer
IF( n>0 )THEN
 DEALLOCATE( Cells )
END IF

!associate the real with the copy
Cells => Cells_
!nullify the copy
Cells_ => NULL()

!!--end--
END SUBROUTINE



FUNCTION ADD_Cell_mesh( Mesh , CellShape , CellLabel , CellSurfaceArea , &
  CellCoeff , FaceList , CellVolume , CellCentroid , overwrite ) RESULT(i)
!!#### PURPOSE
!! Adds a Cell to the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER                    ,OPTIONAL,INTENT(IN) :: CellShape
CHARACTER(LEN=*,KIND=KIND_S),OPTIONAL,INTENT(IN) :: CellLabel

!!#### OPTIONAL INPUT
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellCoeff(:)
INTEGER       ,OPTIONAL,INTENT(IN) :: FaceList(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellVolume
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellSurfaceArea
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellCentroid(:)
INTEGER       ,OPTIONAL,INTENT(IN) :: overwrite

!!#### REQUIRED OUTPUT
INTEGER :: i

!!#### LOCAL VARIABLES
INTEGER :: Ni,Nil,i_

!!--begin--
IF( PRESENT(overwrite) )THEN
 i = overwrite
 CALL DEALLOCATE_Cell( Mesh%Cells(i) )
ELSE
 !increment number of Cells
 Mesh % NCells = Mesh % NCells + 1
 i             = Mesh%NCells

 IF( ASSOCIATED(Mesh%Cells) )THEN
   Ni=SIZE(Mesh%Cells,1)
 ELSE
   Ni=0
 END IF
 IF( ASSOCIATED(Mesh%CellLabels) )THEN
   Nil=SIZE(Mesh%CellLabels)
 ELSE 
   Nil=0
 END IF

 IF( i > Ni )THEN
  CALL REALLOCATE_Cells( Mesh%Cells , MAX(DEFAULT_dNi*Ni,2) )
 END IF
 IF( i > Nil )THEN
  IF( .NOT. ASSOCIATED(Mesh%CellLabels) )THEN
   ALLOCATE(Mesh%CellLabels(10))
  ELSE
   CALL REALLOCATE( Mesh%CellLabels, dn=MAX(DEFAULT_dNi*Nil,2) )
  END IF
 END IF
 
END IF

Mesh % CellLabels(i) = CellLabel
Mesh % i = i

!! add cell
CALL ALLOCATE_Cell( Mesh % Cells( i ) , CellShape , &
  CellCoeff , FaceList , CellVolume , CellCentroid , &
  CellSurfaceArea )


!!--end--
END FUNCTION


FUNCTION INSERT_Cell_Mesh( Mesh , i ) RESULT(i_)
!!#### PURPOSE
!! Inserts a Cell into the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
INTEGER :: i_

!!--begin--
!increment number of Cells
Mesh % NCells = Mesh % NCells + 1

!reallocate if necessary
IF( Mesh%NCells>SIZE(Mesh%Cells) )THEN
 CALL REALLOCATE_Cells( Mesh%Cells , MAX(2,Mesh%NCells*DEFAULT_dNi) )
END IF

!bump all the cells up one slot
DO i_=Mesh%NCells,i+1,-1
 Mesh%Cells(i_) = Mesh%Cells(i_-1)
END DO

!nullify the inserted index
CALL NULLIFY_Cell( Mesh%Cells(i_) )

!!--end--
END FUNCTION



FUNCTION REMOVE_Cell_Mesh( Mesh , i , CellLabel ) RESULT(i_)
!!#### PURPOSE
!! Subtracts a Cell from the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
INTEGER :: i_
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),INTENT(OUT) :: CellLabel

!!#### LOCAL VARIABLES
INTEGER :: NCells

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="REMOVE_Cell"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--

NCells = NUM_Cells_Mesh(Mesh)
IF( i<1 .OR. i>NCells )THEN
 VS = OUT_OF_BOUNDS(mod_,proc_,"i",STR(i),STR(1),STR(NCells))
 CALL Stop( s=STR(VS) )
 VS = ""
END IF

!deallocate the current cell
CALL DEALLOCATE_Cell( Mesh%Cells(i) )

!decrement number of Cells
Mesh % NCells = Mesh % NCells - 1

!set cell label
CellLabel = Mesh%CellLabels(i)

!bump all the cells down one slot
DO i_=i,Mesh%NCells
 Mesh%Cells(i_) = Mesh%Cells(i_+1)
 Mesh%CellLabels(i_) = Mesh%CellLabels(i_+1)
END DO

!erase the last label
CALL CLEAR(Mesh%CellLabels(i_))

!nullify the last slot
CALL NULLIFY_Cell( Mesh%Cells(i_) )

!return the last cell
i_ = Mesh%NCells

!!--end--
END FUNCTION



FUNCTION REMOVE_Face_Mesh( Mesh , j , FaceLabel ) RESULT(j_)
!!#### PURPOSE
!! Subtracts a Face from the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
CHARACTER(LEN=LEN_MESH_LABEL,KIND=KIND_S),INTENT(OUT) :: FaceLabel
INTEGER :: j_

!!#### LOCAL VARIABLES
INTEGER,POINTER :: FaceList(:)
INTEGER :: i,j__,NFaces,n

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="REMOVE_Face"

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
NULLIFY(FaceList)

NFaces = NUM_Faces_Mesh(Mesh)
IF( j<1 .OR. j>NFaces )THEN
 VS = OUT_OF_BOUNDS(mod_,proc_,"j",STR(j),STR(1),STR(NFaces))
 CALL Stop( s=STR(VS) )
 VS = ""
END IF

!deallocate
CALL DEALLOCATE_Face( Mesh%Faces(j) )

!decrement number of Faces
Mesh % NFaces = Mesh % NFaces - 1

!set the label
FaceLabel = Mesh%FaceLabels(j)

!bump all the Faces down one slot
DO j_=j,Mesh%NFaces
 Mesh%Faces(j_) = Mesh%Faces(j_+1)
 Mesh%FaceLabels(j_) = Mesh%FaceLabels(j_+1)
END DO

!erase the last label
CALL CLEAR(Mesh%FaceLabels(j_))

!nullify the last slot
CALL NULLIFY_Face( Mesh%Faces(j_) )

!any cells which are dependent on the bumped faces must change as well
DO i=1,Mesh%NCells
 FaceList => ptr_FaceList_Cell(Mesh%Cells(i))
 DO j_ = 1,SIZE(FaceList)
  j__ = FaceList(j_)
  IF( ABS(j__)>j .AND. j__>0 ) FaceList(j_) = FaceList(j_)-1
  IF( ABS(j__)>j .AND. j__<0 ) FaceList(j_) = FaceList(j_)+1
  IF( ABS(j__)==j ) FaceList(j_) = 0
 END DO
 FaceList => NULL()
END DO

!any subfaces which are dependent on the bumped faces must change, too
!(subfaces are for cell-based meshes only)
DO j_=1,Mesh%NFaces
 !clean out deleted face
 CALL CLEAN_SubFaces(Mesh%Faces(j_)%SubFaces,j)

 !bump down
 IF( ASSOCIATED(Mesh%Faces(j_)%SubFaces) )THEN
  DO n=1,SIZE(Mesh%Faces(j_)%SubFaces)
   IF( Mesh%Faces(j_)%SubFaces(n)>j )THEN
    Mesh%Faces(j_)%SubFaces(n) = Mesh%Faces(j_)%SubFaces(n) - 1
   END IF
  END DO
 END IF

END DO

!return the last face
j_ = Mesh%NFaces

!!--end--
END FUNCTION



FUNCTION ptr_VertList_Mesh( Mesh , j )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j
INTEGER,POINTER :: ptr_VertList_Mesh(:)
ptr_VertList_Mesh => ptr_VertList(Mesh%Faces(ABS(j)))
END FUNCTION

FUNCTION ptr_FaceList_Mesh( Mesh , i )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: i
INTEGER,POINTER :: ptr_FaceList_Mesh(:)
ptr_FaceList_Mesh => ptr_FaceList_CELL(Mesh%Cells(i))
END FUNCTION


FUNCTION ptr_DomainShape_Mesh( Mesh )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,POINTER :: ptr_DomainShape_Mesh
ptr_DomainShape_Mesh => ptr_DomainShape_Domain(Mesh%DOMAIN)
END FUNCTION


FUNCTION ptr_FaceShape_Mesh( Mesh , j )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,POINTER :: ptr_FaceShape_Mesh
INTEGER,INTENT(IN) :: j
ptr_FaceShape_Mesh => ptr_FaceShape_Face(Mesh%FACES(j))
END FUNCTION

FUNCTION FaceShape_Mesh( Mesh , j )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: FaceShape_Mesh
INTEGER,INTENT(IN) :: j
FaceShape_Mesh = ptr_FaceShape_Face(Mesh%FACES(j))
END FUNCTION


FUNCTION ptr_CellShape_Mesh( Mesh , i )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,POINTER :: ptr_CellShape_Mesh
INTEGER,INTENT(IN) :: i
ptr_CellShape_Mesh => ptr_CellShape_Cell(Mesh%CELLS(i))
END FUNCTION

FUNCTION CellShape_Mesh( Mesh , i )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: CellShape_Mesh
INTEGER,INTENT(IN) :: i
CellShape_Mesh = ptr_CellShape_Cell(Mesh%CELLS(i))
END FUNCTION

FUNCTION ptr_EdgeShape_Mesh( Mesh , j , l )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,POINTER :: ptr_EdgeShape_Mesh
INTEGER,INTENT(IN) :: j,l
ptr_EdgeShape_Mesh => ptr_EdgeShape_Face(Mesh%FACES(j),l)
END FUNCTION

FUNCTION ptr_Edges_Mesh(Mesh,j)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
TYPE(TYPE_Edge),POINTER :: ptr_Edges_Mesh(:)
INTEGER,INTENT(IN) :: j
ptr_Edges_Mesh => ptr_Edges_Face(Mesh%FACES(j))
END FUNCTION


FUNCTION EdgeShape_Mesh( Mesh , j , l )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: EdgeShape_Mesh
INTEGER,INTENT(IN) :: j,l
EdgeShape_Mesh = ptr_EdgeShape_Face(Mesh%FACES(j),l)
END FUNCTION


PURE FUNCTION NUM_Edges_Mesh( Mesh , j ) RESULT(NUM)
!!#### PURPOSE
!! Return the number of edges used to construct
!! a face in the mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
INTEGER :: NUM

!!--begin--
NUM = NUM_Edges_Face(Mesh%Faces(j))

!!--end--
END FUNCTION


PURE FUNCTION HasDomain_Mesh( Mesh ) RESULT(HasDomain)
!!#### PURPOSE
!! See if the Mesh has a domain.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
LOGICAL :: HasDomain

!!--begin--
HasDomain = ASSOCIATED(Mesh%domain)

!!--end--
END FUNCTION


PURE FUNCTION NUM_Dimensions_Mesh( Mesh ) RESULT(NUM)
!!#### PURPOSE
!! Return the number of dimensions in the Mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER :: NUM

!!--begin--
IF( .NOT.ASSOCIATED(Mesh%NDim) )THEN
 NUM = 0
ELSE
 NUM = Mesh%NDim
END IF

!!--end--
END FUNCTION


PURE FUNCTION EdgeShapes_Mesh( Mesh , j , Nl )
!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: j
INTEGER,INTENT(IN) :: Nl

!!#### REQUIRED OUTPUT
INTEGER :: EdgeShapes_Mesh(1:Nl)

!!--begin--

EdgeShapes_Mesh = EdgeShapes_Face(Nl,Mesh%Faces(j))

!!--end--
END FUNCTION




SUBROUTINE FINALIZE_Mesh( Mesh )
!!#### PURPOSE
!! Finalize the Mesh by removing all the unused Verts, Faces, and Cells.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: Faces(:)
TYPE(TYPE_Cell),POINTER :: Cells(:)
INTEGER :: i,j

!!--begin--

!initialize
NULLIFY(Faces,Cells)

!remove unused Verts with reallocate
CALL REALLOCATE( Mesh%Verts , dn=(/0,Mesh%NVerts-SIZE(Mesh%Verts,2)/) )
Mesh%k = 0
Mesh%NVerts = NUM_Verts_Mesh( Mesh )


!remove unused Faces with copy and point
ALLOCATE( Faces(1:Mesh%NFaces) )
DO j=1,Mesh%NFaces
 Faces(j) = COPY( Mesh%Faces(j) )
 !CALL PRINT_Face(Mesh%Faces(j))
 CALL DEALLOCATE_Face( Mesh%Faces(j) )
END DO
DEALLOCATE( Mesh%Faces )
ALLOCATE( Mesh%Faces(1:Mesh%NFaces) )
Mesh%Faces => Faces
Mesh%j = 0
Mesh%NFaces = NUM_Faces_Mesh( Mesh )
Faces=>NULL()

!remove unused Cells with a copy and point
ALLOCATE( Cells(1:Mesh%NCells) )
DO i=1,Mesh%NCells
 Cells(i) = COPY( Mesh%Cells(i) )
 CALL DEALLOCATE_Cell( Mesh%Cells(i) )
END DO
DEALLOCATE( Mesh%Cells )
ALLOCATE( Mesh%Cells(1:Mesh%NCells) )
Mesh%Cells => Cells
Mesh%i = 0
Mesh%NCells = NUM_Cells_Mesh( Mesh )
Cells=>NULL()

!finalize Mesh
Mesh%finalized = .TRUE.

CALL UPDATE_FaceToCellLink(Mesh)


!!--end--
END SUBROUTINE


SUBROUTINE ALLOCATE_Mesh( Mesh , NDim , MeshType )
!!#### PURPOSE
!! Allocates the mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh itself (/Mesh/)
TYPE(TYPE_Mesh)    ,INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
!! * the number of spatial dimensions of the Mesh (/NDim/)
INTEGER,INTENT(IN)    :: NDim

!!#### OPTIONAL INPUT
!! * mesh type
INTEGER,OPTIONAL,INTENT(IN) :: MeshType


!!--begin--
!! 1. number of dimensions
ALLOCATE( Mesh%NDim )
Mesh % NDim = NDim

!! 2. mesh type
ALLOCATE( Mesh%MeshType )
Mesh%MeshType = DEFAULT(0,MeshType)

!! 2.b. FaceStructure
ALLOCATE( Mesh%FaceStructure )
Mesh%FaceStructure = 0

!! 3. finalized
ALLOCATE( Mesh%finalized )
Mesh%finalized = .FALSE.

!! 4. allocate the *DOMAIN* part
ALLOCATE( Mesh%DOMAIN )

!! 5. allocate the *VERT* part
ALLOCATE( Mesh%k )
ALLOCATE( Mesh%Nverts )
ALLOCATE( Mesh%verts(1:Mesh%NDim,1) )
Mesh%k      = 0
Mesh%Nverts = 0

!! 6. allocate the *FACE* part
ALLOCATE( Mesh%j )
ALLOCATE( Mesh%NFaces )
Mesh%j      = 0
Mesh%NFaces = 0

!! 7. allocate the *CELL* part
ALLOCATE( Mesh%i )
ALLOCATE( Mesh%NCells )
Mesh%i      = 0
Mesh%NCells = 0

!!--end--
END SUBROUTINE



SUBROUTINE DEALLOCATE_Mesh( Mesh )
!!#### PURPOSE
!! Deallocates the mesh.

!!#### REQUIRED INPUT/OUTPUT
!! * the Mesh itself (/Mesh/)
TYPE(TYPE_Mesh)    ,INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--

!! 1. deallocate control
CALL DEALLOCATE_Control_Mesh( Mesh )

!! 2. deallocate the domain
IF( ASSOCIATED(Mesh%Domain) )THEN
 CALL DEALLOCATE_Domain( Mesh%DOMAIN )
 DEALLOCATE( Mesh%DOMAIN , STAT=jerr )
END IF

!! 3. deallocate the verts
DEALLOCATE( Mesh%k , Mesh%nverts , STAT=jerr )
DEALLOCATE( Mesh%verts , STAT=jerr )

!! 4. deallocate the faces
DEALLOCATE( Mesh%j , Mesh%nfaces , STAT=jerr )
IF( ASSOCIATED(Mesh%Faces) )THEN
 CALL DEALLOCATE_Face( Mesh%Faces )
 DEALLOCATE( Mesh%Faces , STAT=jerr )
 NULLIFY( Mesh%Faces )
END IF


!! 5. deallocate the Cells
DEALLOCATE( Mesh%i , Mesh%nCells , STAT=jerr )
IF( ASSOCIATED(Mesh%Cells) )THEN
 CALL DEALLOCATE_Cell( Mesh%Cells )
 DEALLOCATE( Mesh%Cells , STAT=jerr )
 NULLIFY( Mesh%Cells )
END IF

!deallocate the labels
IF( ASSOCIATED(Mesh%VertLabels) )DEALLOCATE( Mesh%VertLabels )
IF( ASSOCIATED(Mesh%FaceLabels) )DEALLOCATE( Mesh%FaceLabels )
IF( ASSOCIATED(Mesh%CellLabels) )DEALLOCATE( Mesh%CellLabels )

!!--end--
END SUBROUTINE



PURE FUNCTION NUM_Cells_Mesh(Mesh) RESULT(NUM)
!!#### PURPOSE
!! Get the number of cells in the mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### RESULT
INTEGER :: NUM

!!--begin--
NUM = Mesh%NCells
!!--end--
END FUNCTION



PURE FUNCTION NUM_Faces_Mesh(Mesh) RESULT(NUM)
!!#### PURPOSE
!! Get the number of faces in the mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### RESULT
INTEGER :: NUM

!!--begin--
NUM = Mesh%NFaces
!!--end--
END FUNCTION


!!### PURE FUNCTION <<NUM_Verts_Mesh>>
PURE FUNCTION NUM_Verts_Mesh(Mesh) RESULT(NUM)

!!#### PURPOSE
!! Get the number of verts in the mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### RESULT
INTEGER :: NUM

!!--begin--
NUM = Mesh%NVerts
!!--end--
END FUNCTION


!!### PURE FUNCTION <<NUM_Faces_CELLINDEX>>
PURE FUNCTION NUM_Faces_CELLINDEX(Mesh,i)  RESULT(NUM)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
INTEGER :: NUM
!!--begin--
IF( i>=1 .AND. i<=NUM_Cells_Mesh(Mesh) )THEN
 NUM = NUM_Faces_Cell(Mesh%Cells(i))
ELSE
 NUM = 0
END IF
!!--end--
END FUNCTION



!!### PURE FUNCTION <<NUM_Verts_FACEINDEX>>
PURE FUNCTION NUM_Verts_FACEINDEX(Mesh,j)  RESULT(NUM)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: j
INTEGER :: NUM
!!--begin--
IF( j>=1 .AND. j<=NUM_Faces_Mesh(Mesh) )THEN
 NUM = NUM_Verts(Mesh%Faces(j))
ELSE
 NUM = 0
END IF
!!--end--
END FUNCTION


!!### FUNCTION <<COUNT_Cells_Mesh>>
FUNCTION COUNT_Cells_Mesh( Mesh ) RESULT(COUNT)
!!#### PURPOSE
!! Count cells in the mesh object by checking
!! to see if a cell has an associated shape.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER :: COUNT

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--
COUNT = 0

IF( ASSOCIATED(Mesh%Cells) )THEN

 DO i=1,SIZE(Mesh%Cells)
  IF( ASSOCIATED(ptr_CellShape(Mesh%Cells(i))) )THEN
   COUNT = COUNT + 1
  END IF
 END DO

END IF

!!--end--
END FUNCTION




FUNCTION COUNT_Faces_Mesh( Mesh ) RESULT(COUNT)
!!#### PURPOSE
!! Count the faces in the mesh object by checking
!! to see if a face has an associated shape.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER :: COUNT

!!#### LOCAL VARIABLES
INTEGER :: j

!!--begin--
COUNT = 0

IF( ASSOCIATED(Mesh%Faces) )THEN

 DO j=1,SIZE(Mesh%Faces)
  IF( ASSOCIATED(ptr_FaceShape(Mesh%Faces(j))) )THEN
   COUNT = COUNT + 1
  END IF
 END DO

END IF

!!--end--
END FUNCTION



ELEMENTAL FUNCTION COUNT_Verts_Mesh( Mesh ) RESULT(COUNT)
!!#### PURPOSE
!! Count the verts in a mesh, taking into account that
!! ones which have the error number are not actually
!! verts.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
INTEGER :: COUNT

!!#### LOCAL VARIABLES
INTEGER :: k

!!--begin--
COUNT = 0

IF( ASSOCIATED(Mesh%Verts) )THEN

 DO k=1,SIZE(Mesh%Verts,2)
  IF( .NOT.ANY(IsError(Mesh%Verts(:,k))) )THEN
   COUNT = COUNT + 1
  END IF
 END DO

END IF

!!--end--
END FUNCTION



!!### FUNCTION <<ExplicitFace_Mesh>>
FUNCTION ExplicitFace_Mesh( Mesh , j ) RESULT(Xf)

!!#### PURPOSE
!! Create an explicit face from the implicit face j in Mesh.

!!#### DETAILS
!! The face ordering is reversed if j<0.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER       ,INTENT(IN) :: j

!!#### REQUIRED OUTPUT
TYPE(TYPE_ExplicitFace) :: Xf

!!--begin--

Xf = ExplicitFace(Mesh%Faces(ABS(j)),Mesh%Verts,Flip=j<0)

!!--end--
END FUNCTION



!!### FUNCTION <<ExplicitCell_Mesh>>
FUNCTION ExplicitCell_Mesh( Mesh , i ) RESULT(Xc)

!!#### PURPOSE
!! Create an explicit Cell from the implicit Cell i in Mesh.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
TYPE(TYPE_ExplicitCell) :: Xc

!!--begin--

Xc = ExplicitCell( Mesh%Cells(i) , Mesh%Faces , Mesh%Verts )

!!--end--
END FUNCTION



!!### FUNCTION <<ExplicitFace_Domain>>
FUNCTION ExplicitFace_Domain( Domain , jd ) RESULT(Xf)

!!#### PURPOSE
!! Create an explicit face from the implicit face j in Mesh.

!!#### DETAILS
!! The face ordering is reversed if jd<0.

!!#### REQUIRED INPUT
TYPE(TYPE_Domain),INTENT(IN) :: Domain
INTEGER          ,INTENT(IN) :: jd

!!#### REQUIRED OUTPUT
TYPE(TYPE_ExplicitFace) :: Xf

!!--begin--

Xf = ExplicitFace(Domain%Faces(ABS(jd)),Domain%Verts,Flip=jd<0)

!!--end--
END FUNCTION


!!### FUNCTION <<ExplicitCell_Domain>>
FUNCTION ExplicitCell_Domain( Domain ) RESULT(Xc)

!!#### PURPOSE
!! Create an explicit Cell from the implicit Cell of a
!! Domain.

!!#### REQUIRED INPUT
TYPE(TYPE_Domain),INTENT(IN) :: Domain

!!#### REQUIRED OUTPUT
TYPE(TYPE_ExplicitCell) :: Xc

!!--begin--

Xc = ExplicitCell( Domain%Cell , Domain%Faces , Domain%Verts )

!!--end--
END FUNCTION



FUNCTION FaceLabel_Mesh(Mesh,j)
CHARACTER(LEN_MESH_LABEL) :: FaceLabel_Mesh
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGEr,INTENT(IN) :: j
FaceLabel_Mesh = Mesh%FaceLabels(j)
END FUNCTION

FUNCTION CellLabel_Mesh(Mesh,i)
CHARACTER(LEN_MESH_LABEL) :: CellLabel_Mesh
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGEr,INTENT(IN) :: i
CellLabel_Mesh = Mesh%CellLabels(i)
END FUNCTION

FUNCTION ptr_FaceArea_Mesh(Mesh,j)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: j
REAL(KIND_MSH),POINTER :: ptr_FaceArea_Mesh
!!--begin--
ptr_FaceArea_Mesh =>ptr_FaceArea_Face(Mesh%Faces(j))
!!--end--
END FUNCTION

FUNCTION ptr_FaceCentroid_Mesh(Mesh,j)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: j
REAL(KIND_MSH),POINTER :: ptr_FaceCentroid_Mesh(:)
!!--begin--
ptr_FaceCentroid_Mesh =>ptr_FaceCentroid_Face(Mesh%faces(j))
!!--end--
END FUNCTION

FUNCTION ptr_Verts_Mesh( Mesh ) RESULT(Verts)
!PURPOSE
!! Returns a pointer to the Mesh Verts.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
REAL(KIND_MSH)  ,POINTER    :: Verts(:,:)
!
Verts => Mesh%Verts
!
END FUNCTION

FUNCTION ptr_Faces_Mesh( Mesh ) RESULT(Faces)
!PURPOSE
!! Returns a pointer to the Mesh Faces.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
TYPE(TYPE_Face)  ,POINTER    :: Faces(:)
!
Faces => Mesh%Faces
!
END FUNCTION

FUNCTION ptr_Cells_Mesh( Mesh ) RESULT(Cells)
!PURPOSE
!! Returns a pointer to the Mesh Cells.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh

!!#### REQUIRED OUTPUT
TYPE(TYPE_Cell)  ,POINTER    :: Cells(:)
!
Cells => Mesh%Cells
!
END FUNCTION

FUNCTION ptr_MeshType_Mesh( Mesh )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,POINTER :: ptr_MeshType_Mesh
ptr_MeshType_Mesh => Mesh%MeshType
END FUNCTION


FUNCTION ptr_NDim_Mesh( Mesh )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,POINTER :: ptr_NDim_Mesh
ptr_NDim_Mesh => Mesh%NDim
END FUNCTION


SUBROUTINE COPY_Mesh( MeshOut , MeshIn )
TYPE(TYPE_Mesh),INTENT(OUT) :: MeshOut
TYPE(TYPE_Mesh),INTENT(IN)  :: MeshIn
INTEGER :: i,j
!!--begin--

!clean out old
CALL DEALLOCATE_Mesh( MeshOut )

!allocate and copy basics
CALL ALLOCATE_Mesh( MeshOut , NDim=MeshIn%NDim , MeshType=MeshIn%MeshType )
MeshOut%Finalized     = MeshIn%Finalized
MeshOut%FaceStructure = MeshIn%FaceStructure

!! 4. copy the domain
MeshOut%Domain = COPY_Domain( MeshIn%DOMAIN )

!! 5. copy the verts
MeshOut%k      = MeshIn%k
MeshOut%Nverts = MeshIn%Nverts
ALLOCATE( MeshOut%Verts(MeshOut%NDim,MeshOut%NVerts) )
MeshOut%Verts  = MeshIn%Verts

!! 6. copy the faces
MeshOut%j      = MeshIn%j
MeshOut%nfaces = MeshIn%nfaces
ALLOCATE( MeshOut%Faces(MeshOut%nfaces) )
DO j=1,MeshOut%nfaces
 MeshOut%Faces(j) = COPY_Face( MeshIn%Faces(j) )
END DO

!! 7. copy the cells
MeshOut%i      = MeshIn%i
MeshOut%ncells = MeshIn%NCells
ALLOCATE( MeshOut%Cells(MeshOut%ncells) )
DO i=1,MeshOut%ncells
 MeshOut%Cells(i)  = COPY_Cell( MeshIn%Cells(i) )
END DO

!! 8. copy the labels
ALLOCATE( MeshOut%VertLabels(MeshOut%NVerts) )
MeshOut%VertLabels = MeshIn%VertLabels

ALLOCATE( MeshOut%FaceLabels(MeshOut%NFaces) )
MeshOut%FaceLabels = MeshIn%FaceLabels

ALLOCATE( MeshOut%CellLabels(MeshOut%NCells) )
MeshOut%CellLabels = MeshIn%CellLabels

!!--end--
END SUBROUTINE



FUNCTION IsEqual_Mesh( Mesh1 , Mesh2 , checklabels ) RESULT(IsEqual)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh1,Mesh2
LOGICAL,INTENT(IN),OPTIONAL :: checklabels
LOGICAL :: IsEqual
LOGICAL :: checklabels_
!!--begin--

checklabels_=Default(.TRUE.,checklabels)

!! check equality of control variables
IsEqual = IsEqual_Mesh_Control( Mesh1 , Mesh2)
IF( .NOT.IsEqual )RETURN

!! check domain equality
IsEqual = IsEqual_Domain( Mesh1%Domain , Mesh2%Domain )
IF( .NOT.IsEqual )RETURN

!! check vert equality
IsEqual = Mesh1%NVerts==Mesh2%NVerts
IF( .NOT.IsEqual )RETURN
IsEqual = ALL(Mesh1%Verts==Mesh2%Verts)
IF( .NOT.IsEqual )RETURN
!don't check iterator Mesh%k

!! check faces equality
IsEqual = Mesh1%NFaces==Mesh2%NFaces
IF( .NOT.IsEqual )RETURN
IsEqual = ALL( IsEqual_Face(Mesh1%Faces,Mesh2%Faces) )
IF( .NOT.IsEqual )RETURN

!! check cells equality
IsEqual = Mesh1%NCells==Mesh2%NCells
IF( .NOT.IsEqual )RETURN
IsEqual = ALL( IsEqual_Cell(Mesh1%Cells,Mesh2%Cells) )
IF( .NOT.IsEqual )RETURN

!! check labels
IF( checklabels_ )THEN
 IsEqual = ALL(Mesh1%VertLabels == Mesh2%VertLabels)
 IF( .NOT.IsEqual )RETURN

 IsEqual = ALL(Mesh1%FaceLabels == Mesh2%FaceLabels)
 IF( .NOT.IsEqual )RETURN

 IsEqual = ALL(Mesh1%CellLabels == Mesh2%CellLabels)
 IF( .NOT.IsEqual )RETURN
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<IsEqual_Mesh_control>>
FUNCTION IsEqual_Mesh_control( Mesh1 , Mesh2 ) RESULT(IsEqual)

!!#### PURPOSE
!! Check equality of control variables.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh1,Mesh2

!!#### REQUIRED OUTPUT
LOGICAL :: IsEqual

!!--begin--

IsEqual = Mesh1 % NDim         == Mesh2 % NDim
IF( .NOT.IsEqual )RETURN

IsEqual = Mesh1 % FINALIZED .AND. Mesh2 % FINALIZED
IF( .NOT.IsEqual )RETURN

IsEqual = Mesh1 % MeshType     == Mesh2 % MeshType
IF( .NOT.IsEqual )RETURN

IsEqual = Mesh1 % FaceStructure== Mesh2 % FaceStructure
IF( .NOT.IsEqual )RETURN

!!--end--
END FUNCTION



SUBROUTINE DEALLOCATE_control_Mesh( Mesh )
!PURPOSE
!! Destroys the control part of Mesh.
!ARGUMENTS
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
!LOCALS
INTEGER :: jerr
!
DEALLOCATE( Mesh % NDim         , STAT=jerr )
DEALLOCATE( Mesh % FINALIZED    , STAT=jerr )
DEALLOCATE( Mesh % MeshType     , STAT=jerr )
DEALLOCATE( Mesh % FaceStructure, STAT=jerr )
!
END SUBROUTINE


SUBROUTINE DEALLOCATE_Domain_Mesh( Mesh )
!PURPOSE
!! Destroys the domain part of Mesh.
!ARGUMENTS
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
!LOCALS
INTEGER :: jerr
!
CALL DEALLOCATE_Domain( Mesh%domain )
DEALLOCATE( Mesh%domain , STAT=jerr )
!
END SUBROUTINE


SUBROUTINE DEALLOCATE_Verts_Mesh( Mesh )
!PURPOSE
!! Deallocates the verts part of Mesh.
!ARGUMENTS
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
!LOCALS
INTEGER :: jerr
!
!deallocate verts
DEALLOCATE( Mesh%verts  , STAT=jerr )
DEALLOCATE( Mesh%Nverts , STAT=jerr )
DEALLOCATE( Mesh%k      , STAT=jerr )
!
END SUBROUTINE


SUBROUTINE DEALLOCATE_Faces_Mesh( Mesh )
!PURPOSE
!! Deallocates the faces part of Mesh.
!ARGUMENTS
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
!LOCALS
INTEGER :: j , jerr
!
DO j=1,Mesh%Nfaces
 CALL DEALLOCATE_Face( Mesh%faces(j) )
END DO
DEALLOCATE( Mesh%faces  , STAT=jerr )
DEALLOCATE( Mesh%Nfaces , STAT=jerr )
DEALLOCATE( Mesh%j      , STAT=jerr )
!
END SUBROUTINE


SUBROUTINE DEALLOCATE_Cells_Mesh( Mesh )
!PURPOSE
!! Dealocates the cells part of Mesh.
!ARGUMENTS
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
!LOCALS
INTEGER :: i,jerr
!
DO i=1,Mesh%Ncells
 CALL DEALLOCATE_Cell( Mesh%cells(i) )
END DO
DEALLOCATE( Mesh%cells  , STAT=jerr )
DEALLOCATE( Mesh%Ncells , STAT=jerr )
DEALLOCATE( Mesh%i      , STAT=jerr )
!
END SUBROUTINE



!!### SUBROUTINE <<REORDER_Faces_Mesh>>
SUBROUTINE REORDER_Faces_Mesh( Mesh , ja , jb )

!!#### PURPOSE
!! Reorder faces <ja> for faces <jb> in the
!! mesh structure.

!!#### NOTE
!! This will be slow.  Improvement desired.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: ja(:),jb(SIZE(ja))

!!#### LOCAL VARIABLES
TYPE(TYPE_Face),POINTER :: F(:)
INTEGER :: n,j1,j2,i,j_,j,jswap
INTEGER :: js,Ns

!!--begin--

!setup new structure
NULLIFY(F)
ALLOCATE( F(NUM_Faces_Mesh(Mesh)) )

!swap the faces in the <Mesh%Faces> structure

!a) incorporate the reordered faces
DO n=1,SIZE(ja)
 j1    = ja(n)
 j2    = jb(n)
 F(j2) = Mesh%Faces(j1)
END DO

!b) incorporate the rest of the faces
DO j=1,NUM_Faces_Mesh(Mesh)
 IF( .NOT.ASSOCIATED(F(j)%FaceShape) )THEN
  F(j) = Mesh%Faces(j)
 END IF
END DO

!c) nullify and reattach the structure
CALL NULLIFY_Face( Mesh%Faces )
Mesh%faces => F
NULLIFY( F )
!now change numbering in cells
DO i=1,NUM_Cells_Mesh(Mesh)
 DO j_=1,NUM_Faces_Cell(Mesh%Cells(i))

  j = Mesh%Cells(i)%FaceList(j_)

  DO n=1,SIZE(ja)
   j1 = ja(n)
   j2 = jb(n)
   IF( ABS(j)==j1 )THEN
    jswap = SIGN(j2,j)
    Mesh%Cells(i)%FaceList(j_) = jswap
    EXIT
   END IF

  END DO

 END DO
END DO

!now change the numbering in Subfaces
DO j_=1,NUM_Faces_Mesh(Mesh)
 IF( ASSOCIATED(Mesh%Faces(j_)%SubFaces) )THEN
  Ns = SIZE(Mesh%Faces(j_)%SubFaces)
  DO js=1,Ns
   j = Mesh%Faces(j_)%SubFaces(js)
   DO n=1,SIZE(ja)

    j1 = ja(n)
    j2 = jb(n)

    IF( ABS(j)==j1 )THEN
     jswap = SIGN(j2,j)
     Mesh%Faces(j_)%SubFaces(js) = jswap
     EXIT
    END IF

   END DO
  END DO
 END IF
END DO

!!--end--
END SUBROUTINE

END MODULE
