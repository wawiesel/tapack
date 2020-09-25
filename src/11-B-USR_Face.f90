!!# USER MODULE <<USR_Face>>
MODULE USR_Face

!!## PURPOSE
!! Defines a 1-3 dimensional face type.


!!## DETAILS
!
!! 1. <%FaceShape>
!!   This is a integer corresponding to the shape of the face.
!
!! 2. <%FaceNormal>
!!   The coefficients that describe the face.
!
!! 3a. <%VertList>
!!   This is a list of vertex indices.
!
!! 3b. <%Edges component>
!!   A face described by <Nk=SIZE(Face%VertList)> vertices may have <Nk-1>
!!   edges specifying how vertices are connected.
!
!! 4. <%FaceArea component>
!!   This is the area of the face.
!
!! 5. <%FaceCentroid>
!!   This is the centroid of the face.
!

!!## EXTERNAL KINDS
USE KND_Mesh                  !!((05-B-KND_Mesh.f90))

!!## FUNCTIONS
USE FUN_Default               !!((04-A-FUN_Default.f90))
USE FUN_STRn                  !!((07-B-FUN_STRn.f90))

!!## GLOBAL USER MODULES
USE USR_Edge                  !!((06-B-USR_Edge.f90))

!!## GEOMETRY TOOLBOX
USE PAR_ComputationalGeometry !!((02-A-PAR_ComputationalGeometry.f90))
USE TBX_ComputationalGeometry !!((09-A-TBX_ComputationalGeometry.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_="USR_Face"
CHARACTER(*),PARAMETER :: file_="11-B-USR_Face.f90"


!!## TYPE DEFINITION for FACE type
TYPE TYPE_Face
 !PRIVATE
 INTEGER        ,POINTER :: FaceShape        => NULL()
 REAL(KIND_MSH) ,POINTER :: FaceNormal(:)    => NULL()
 INTEGER        ,POINTER :: VertList(:)      => NULL()
 TYPE(TYPE_Edge),POINTER :: Edges(:)         => NULL()
 REAL(KIND_MSH) ,POINTER :: FaceArea         => NULL()
 REAL(KIND_MSH) ,POINTER :: FaceCentroid(:)  => NULL()
 REAL(KIND_MSH) ,POINTER :: FaceCoeff(:)     => NULL()
 INTEGER        ,POINTER :: DomainFace       => NULL()
 INTEGER        ,POINTER :: SubFaces(:)      => NULL()
END TYPE

!!## INTERFACES
INTERFACE ALLOCATE         ; MODULE PROCEDURE ALLOCATE_Face        ; END INTERFACE
INTERFACE DEALLOCATE       ; MODULE PROCEDURE DEALLOCATE_Face      ; END INTERFACE
INTERFACE NULLIFY          ; MODULE PROCEDURE NULLIFY_Face         ; END INTERFACE
INTERFACE COPY             ; MODULE PROCEDURE COPY_FACE            ; END INTERFACE
INTERFACE ptr_FaceShape    ; MODULE PROCEDURE ptr_FaceShape_FACE   ; END INTERFACE
INTERFACE ptr_FaceNormal   ; MODULE PROCEDURE ptr_FaceNormal_FACE  ; END INTERFACE
INTERFACE ptr_VertList     ; MODULE PROCEDURE ptr_VertList_FACE    ; END INTERFACE
INTERFACE Edge             ; MODULE PROCEDURE Edge_FACE            ; END INTERFACE
INTERFACE ptr_Edges        ; MODULE PROCEDURE ptr_Edges_FACE       ; END INTERFACE
INTERFACE ptr_FaceArea     ; MODULE PROCEDURE ptr_FaceArea_FACE    ; END INTERFACE
INTERFACE ptr_FaceCentroid ; MODULE PROCEDURE ptr_FaceCentroid_FACE; END INTERFACE
INTERFACE ptr_FaceCoeff    ; MODULE PROCEDURE ptr_FaceNormal_FACE  ; END INTERFACE
INTERFACE ptr_DomainFace   ; MODULE PROCEDURE ptr_DomainFace_FACE  ; END INTERFACE
INTERFACE ptr_EdgeShape    ; MODULE PROCEDURE ptr_EdgeShape_FACE   ; END INTERFACE
INTERFACE EdgeShape        ; MODULE PROCEDURE EdgeShape_FACE       ; END INTERFACE
INTERFACE FaceShape        ; MODULE PROCEDURE FaceShape_FACE       ; END INTERFACE
INTERFACE EdgeShapes       ; MODULE PROCEDURE EdgeShapes_Face      ; END INTERFACE
INTERFACE NUM_Edges        ; MODULE PROCEDURE NUM_Edges_Face       ; END INTERFACE
INTERFACE NUM_Verts        ; MODULE PROCEDURE NUM_Verts_FACE       ; END INTERFACE
INTERFACE VertList         ; MODULE PROCEDURE VertList_FACE        ; END INTERFACE
INTERFACE NUM_SubFaces     ; MODULE PROCEDURE NUM_SubFaces_FACE    ; END INTERFACE
INTERFACE ptr_SubFaces     ; MODULE PROCEDURE ptr_SubFaces_FACE    ; END INTERFACE
INTERFACE IsEqual          ; MODULE PROCEDURE IsEqual_Face         ; END INTERFACE

!!## PUBLIC ACCESS
PUBLIC :: TYPE_Face
PUBLIC :: ALLOCATE         , ALLOCATE_Face
PUBLIC :: DEALLOCATE       , DEALLOCATE_Face
PUBLIC :: NULLIFY          , NULLIFY_Face
PUBLIC :: COPY             , COPY_Face
PUBLIC :: ptr_FaceShape    , ptr_FaceShape_FACE
PUBLIC :: ptr_FaceNormal   , ptr_FaceNormal_FACE
PUBLIC :: ptr_VertList     , ptr_VertList_FACE
PUBLIC :: ptr_Edges        , ptr_Edges_FACE
PUBLIC :: ptr_FaceArea     , ptr_FaceArea_FACE
PUBLIC :: ptr_FaceCentroid , ptr_FaceCentroid_FACE
PUBLIC :: ptr_FaceCoeff    , ptr_FaceCoeff_FACE
PUBLIC :: ptr_DomainFace   , ptr_DomainFace_FACE
PUBLIC :: ptr_EdgeShape    , ptr_EdgeShape_FACE
PUBLIC :: ptr_SubFaces     , ptr_SubFaces_FACE
PUBLIC :: VertList         , VertList_FACE
PUBLIC :: Edge             , Edge_FACE
PUBLIC :: EdgeShape        , EdgeShape_FACE
PUBLIC :: FaceShape        , FaceShape_FACE
PUBLIC :: EdgeShapes       , EdgeShapes_Face
PUBLIC :: NUM_Edges        , NUM_Edges_Face
PUBLIC :: NUM_Verts        , NUM_Verts_FACE
PUBLIC :: NUM_SubFaces     , NUM_SubFaces_FACE
PUBLIC :: IsEqual_Face     , IsEqual
PUBLIC :: PRINT_FACE

!!## MODULE PROCEDURES
CONTAINS

SUBROUTINE PRINT_Face(Face,Unit)
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER,INTENT(IN),OPTIONAL :: Unit
INTEGER :: Unit_

    IF( PRESENT(Unit) )THEN
        Unit_=Unit
    ELSE
        Unit_=6
    END IF
    33 FORMAT (a20,100(3x,a24),/)
    WRITE(Unit_,33)'FaceShape',STRn(Face%FaceShape)
    WRITE(Unit_,33)'Normal',STRn(Face%FaceNormal,FMT='(es24.12)')
    WRITE(Unit_,33)'VertList',STRn(Face%VertList)
    !WRITE(Unit_,33)'Edges',STRn(Face%Edges)
    WRITE(Unit_,33)'FaceArea',STRn(Face%FaceArea,FMT='(es24.12)')
    WRITE(Unit_,33)'FaceCentroid',STRn(Face%FaceCentroid,FMT='(es24.12)')
    WRITE(Unit_,33)'FaceCoeff',STRn(Face%FaceCoeff,FMT='(es24.12)')
    WRITE(Unit_,33)'DomainFace',STRn(Face%DomainFace)
    WRITE(Unit_,33)'SubFaces',STRn(Face%SubFaces)

END SUBROUTINE PRINT_Face

!!### ELEMENTAL FUNCTION <<IsEqual_Face>>
ELEMENTAL FUNCTION IsEqual_Face(Face1,Face2) RESULT(IsEqual)
!!#### PURPOSE
!! Test for equality between faces.

!!#### REQUIRED INPUT
TYPE(TYPE_Face),INTENT(IN) :: Face1,Face2

!!#### REQUIRED OUTPUT
LOGICAL :: IsEqual

!!--begin--

!%FaceShape
IsEqual = (Face1%FaceShape==Face2%FaceShape)
IF( .NOT.IsEqual )RETURN

!%FaceArea
IsEqual = (Face1%FaceArea==Face2%FaceArea)
IF( .NOT.IsEqual )RETURN

!%DomainFace
IsEqual = (Face1%DomainFace==Face2%DomainFace)
IF( .NOT.IsEqual )RETURN


!%FaceNormal
IsEqual = (SIZE(Face1%FaceNormal)==SIZE(Face2%FaceNormal))
IF( .NOT.IsEqual )RETURN
IsEqual = ALL(Face1%FaceNormal(:)==Face2%FaceNormal(:))
IF( .NOT.IsEqual )RETURN


!%VertList
IsEqual = (SIZE(Face1%VertList)==SIZE(Face2%VertList))
IF( .NOT.IsEqual )RETURN
IsEqual = ALL(Face1%VertList(:)==Face2%VertList(:))
IF( .NOT.IsEqual )RETURN


!%FaceCentroid
IsEqual = (SIZE(Face1%FaceCentroid)==SIZE(Face2%FaceCentroid))
IF( .NOT.IsEqual )RETURN
IsEqual = ALL(Face1%FaceCentroid(:)==Face2%FaceCentroid(:))
IF( .NOT.IsEqual )RETURN


!%FaceCoeff
IsEqual = (SIZE(Face1%FaceCoeff)==SIZE(Face2%FaceCoeff))
IF( .NOT.IsEqual )RETURN
IsEqual = ALL(Face1%FaceCoeff(:)==Face2%FaceCoeff(:))
IF( .NOT.IsEqual )RETURN


!%SubFaces
IsEqual = (SIZE(Face1%SubFaces)==SIZE(Face2%SubFaces))
IF( .NOT.IsEqual )RETURN
IsEqual = ALL(Face1%SubFaces(:)==Face2%SubFaces(:))
IF( .NOT.IsEqual )RETURN


!%Edges
IsEqual = ALL(IsEqual_Edge(Face1%Edges,Face2%Edges))
IF( .NOT.IsEqual )RETURN

!!--end--
END FUNCTION



!!### PURE SUBROUTINE <<ALLOCATE_Face>>
PURE SUBROUTINE ALLOCATE_Face( Face , FaceShape , &
  FaceNormal , VertList , Edges , FaceArea , FaceCentroid , &
  FaceCoeff , DomainFace , SubFaces )
!!### PURPOSE
!! Allocate the Face type.

!!### REQUIRED OUTPUT
TYPE(TYPE_Face),INTENT(INOUT) :: Face

!!### OPTIONAL INPUT
INTEGER        ,OPTIONAL,INTENT(IN) :: FaceShape
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceNormal(:)
INTEGER        ,OPTIONAL,INTENT(IN) :: VertList(:)
TYPE(TYPE_Edge),OPTIONAL,INTENT(IN) :: Edges(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceArea
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceCentroid(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceCoeff(:)
INTEGER        ,OPTIONAL,INTENT(IN) :: DomainFace
INTEGER        ,OPTIONAL,INTENT(IN) :: SubFaces(:)

!!--begin--
IF( PRESENT(FaceShape) )THEN
 ALLOCATE( Face%FaceShape )
 Face%FaceShape = FaceShape
END IF

IF( PRESENT(FaceNormal) )THEN
 ALLOCATE( Face%FaceNormal(1:SIZE(FaceNormal)) )
 Face%FaceNormal = FaceNormal
END IF

IF( PRESENT(VertList) )THEN
 ALLOCATE( Face%VertList(1:SIZE(VertList)) )
 Face%VertList = VertList
END IF

IF( PRESENT(Edges) )THEN
 !Face%Edges = Edges
 CALL COPY( Edges, Face%Edges )
END IF

IF( PRESENT(FaceArea) )THEN
 ALLOCATE( Face%FaceArea )
 Face%FaceArea = FaceArea
END IF

IF( PRESENT(FaceCentroid) )THEN
 ALLOCATE( Face%FaceCentroid(1:SIZE(FaceCentroid)) )
 Face%FaceCentroid = FaceCentroid
END IF

IF( PRESENT(FaceCoeff) )THEN
 ALLOCATE( Face%FaceCoeff(1:SIZE(FaceCoeff)) )
 Face%FaceCoeff = FaceCoeff
END IF

IF( PRESENT(DomainFace) )THEN
 ALLOCATE( Face%DomainFace )
 Face%DomainFace = DomainFace
END IF

IF( PRESENT(SubFaces) )THEN
 ALLOCATE( Face%SubFaces(1:SIZE(SubFaces)) )
 Face%SubFaces = SubFaces
END IF

!!--end--
END SUBROUTINE



PURE ELEMENTAL SUBROUTINE COPY_Face( FACE, COPY )
!!### PURPOSE
!! Make a copy of a FACE.
!!### REQUIRED INPUT
TYPE(TYPE_Face),INTENT(IN)  :: FACE
!!### REQUIRED OUTPUT
TYPE(TYPE_Face),INTENT(OUT) :: COPY

!!--begin--
IF( ASSOCIATED(FACE%FaceShape)    )THEN
 CALL ALLOCATE( COPY , FaceShape=FACE%FaceShape )
END IF

IF( ASSOCIATED(FACE%FaceNormal)    )THEN
 CALL ALLOCATE( COPY , FaceNormal=FACE%FaceNormal )
END IF

IF( ASSOCIATED(FACE%VertList)     )THEN
 CALL ALLOCATE( COPY , VertList  =FACE%VertList  )
END IF

IF( ASSOCIATED(FACE%Edges)        )THEN
 CALL ALLOCATE( COPY , Edges     =FACE%Edges     )
END IF

IF( ASSOCIATED(FACE%FaceArea)         )THEN
 CALL ALLOCATE( COPY , FaceArea  =FACE%FaceArea      )
END IF

IF( ASSOCIATED(FACE%FaceCentroid)     )THEN
 CALL ALLOCATE( COPY , FaceCentroid =FACE%FaceCentroid  )
ENDIF

IF( ASSOCIATED(FACE%FaceCoeff)     )THEN
 CALL ALLOCATE( COPY , FaceCoeff =FACE%FaceCoeff  )
ENDIF

IF( ASSOCIATED(FACE%DomainFace)     )THEN
 CALL ALLOCATE( COPY , DomainFace =FACE%DomainFace  )
ENDIF

IF( ASSOCIATED(FACE%SubFaces)     )THEN
 CALL ALLOCATE( COPY , SubFaces  =FACE%SubFaces  )
END IF
!!--end--
END SUBROUTINE


ELEMENTAL SUBROUTINE DEALLOCATE_Face( Face )
!!### PURPOSE
!! Deallocate the Face type.

!!### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Face),INTENT(INOUT) :: Face

!!### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
DEALLOCATE( Face % FaceShape    , STAT=jerr )
DEALLOCATE( Face % FaceNormal   , STAT=jerr )
DEALLOCATE( Face % VertList     , STAT=jerr )
CALL DEALLOCATE( Face % Edges )
DEALLOCATE( Face % Edges        , STAT=jerr )
DEALLOCATE( Face % FaceArea     , STAT=jerr )
DEALLOCATE( Face % FaceCentroid , STAT=jerr )
DEALLOCATE( Face % FaceCoeff    , STAT=jerr )
DEALLOCATE( Face % DomainFace   , STAT=jerr )
DEALLOCATe( Face % SubFaces     , STAT=jerr )

!!--end--
END SUBROUTINE


ELEMENTAL SUBROUTINE NULLIFY_Face( Face )
!!### PURPOSE
!! Nullify the Face type.

!!### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Face),INTENT(INOUT) :: Face

!!--begin--
NULLIFY( Face % FaceShape     )
NULLIFY( Face % FaceNormal    )
NULLIFY( Face % VertList      )
NULLIFY( Face % Edges         )
NULLIFY( Face % FaceArea      )
NULLIFY( Face % FaceCentroid  )
NULLIFY( Face % FaceCoeff     )
NULLIFY( Face % DomainFace    )
NULLIFY( Face % SubFaces      )
!!--end--
END SUBROUTINE


PURE FUNCTION FaceShape_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER :: FaceShape_FACE
FaceShape_FACE = Face % FaceShape
END FUNCTION

FUNCTION ptr_FaceShape_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER,POINTER :: ptr_FaceShape_FACE
ptr_FaceShape_FACE => Face % FaceShape
END FUNCTION

FUNCTION ptr_EdgeShape_FACE( Face , l )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER,INTENT(IN) :: l
INTEGER,POINTER :: ptr_EdgeShape_FACE
ptr_EdgeShape_FACE => Face % Edges(l) % EdgeShape
END FUNCTION

FUNCTION Edge_FACE( Face , l )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER,INTENT(IN) :: l
TYPE(TYPE_Edge) :: Edge_FACE
CALL COPY(Face % Edges(l),Edge_FACE)
END FUNCTION

PURE FUNCTION EdgeShape_FACE( Face , l )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER,INTENT(IN) :: l
INTEGER  :: EdgeShape_FACE
IF( ASSOCIATED(Face%Edges(l)%EdgeShape) )THEN
 EdgeShape_FACE = Face % Edges(l) % EdgeShape
ELSE
 EdgeShape_FACE = Straight_
END IF
END FUNCTION

PURE FUNCTION NUM_Edges_Face( Face ) RESULT(NUM)
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER :: NUM
IF( .NOT.ASSOCIATED(Face%Edges) )THEN
 NUM = 0
ELSE
 NUM = SIZE( Face % Edges )
END IF
END FUNCTION

PURE FUNCTION EdgeShapes_FACE( Nl , Face )
INTEGER,INTENT(IN) :: Nl
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER :: EdgeShapes_FACE(1:Nl)
INTEGER :: l
!!--begin--
DO l=1,Nl
 EdgeShapes_FACE(l) = EdgeShape(Face,l)
END DO
!!--end--
END FUNCTION

FUNCTION ptr_FaceNormal_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
REAL(KIND_MSH),POINTER :: ptr_FaceNormal_FACE(:)
ptr_FaceNormal_FACE => Face % FaceNormal
END FUNCTION

FUNCTION ptr_VertList_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER,POINTER :: ptr_VertList_FACE(:)
ptr_VertList_FACE => Face % VertList
END FUNCTION

FUNCTION ptr_SubFaces_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER,POINTER :: ptr_SubFaces_FACE(:)
ptr_SubFaces_FACE => Face % SubFaces
END FUNCTION

PURE FUNCTION NUM_Verts_FACE(Face) RESULT(NUM)
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER :: NUM
NUM = SIZE(Face%VertList)
END FUNCTION

PURE FUNCTION NUM_SubFaces_FACE(Face) RESULT(NUM)
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER :: NUM
NUM = SIZE(Face%SubFaces)
END FUNCTION

PURE FUNCTION VertList_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER :: VertList_FACE(1:NUM_Verts(Face))
VertList_FACE = Face % VertList
END FUNCTION

FUNCTION ptr_Edges_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
TYPE(TYPE_Edge),POINTER :: ptr_Edges_FACE(:)
ptr_Edges_FACE => Face % Edges
END FUNCTION

FUNCTION ptr_FaceArea_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
REAL(KIND_MSH),POINTER :: ptr_FaceArea_FACE
ptr_FaceArea_FACE => Face % FaceArea
END FUNCTION

FUNCTION ptr_FaceCentroid_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
REAL(KIND_MSH),POINTER :: ptr_FaceCentroid_FACE(:)
ptr_FaceCentroid_FACE => Face % FaceCentroid
END FUNCTION

FUNCTION ptr_DomainFace_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
INTEGER,POINTER :: ptr_DomainFace_FACE
ptr_DomainFace_FACE => Face % DomainFace
END FUNCTION

FUNCTION ptr_FaceCoeff_FACE( Face )
TYPE(TYPE_Face),INTENT(IN) :: Face
REAL(KIND_MSH),POINTER :: ptr_FaceCoeff_FACE(:)
ptr_FaceCoeff_FACE => Face % FaceCoeff
END FUNCTION


END MODULE
