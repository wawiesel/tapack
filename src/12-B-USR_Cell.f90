!!# USER MODULE <<USR_Cell>>
MODULE USR_Cell

!!## PURPOSE
!! Defines a 1-3 dimensional cell type.

!!## DETAILS
!
!! 1. %CellShape
!!   This is a integer corresponding to the shape of the cell.
!
!! 2a. %CellCoeff (excludes the %FaceList component)
!!   The coefficients that describe the cell.
!
!! 2b. %FaceList component (excludes the %CellCoeff component)
!!   This is a list of face indices.
!!    @ FaceList(j)>0 : face normal is the outward normal
!!    @ FaceList(j)<0 : face normal is the inward normal
!
!! 3. %CellVolume component
!!   This is the CellVolume of the cell.
!
!! 4. %CellCentroid
!!   This is the CellCentroid of the cell.
!

!!## EXTERNAL KINDS
USE KND_Mesh                  !!((05-B-KND_Mesh.f90))

!!## FUNCTIONS
USE FUN_Default               !!((04-A-FUN_Default.f90))

!!## GLOBAL USER MODULES
USE USR_Edge                  !!((06-B-USR_Edge.f90))
USE USR_Face                  !!((11-B-USR_Face.f90))

!!## GEOMETRY TOOLBOX
USE TBX_ComputationalGeometry !!((09-A-TBX_ComputationalGeometry.f90))

!!## DEFAULT ACCESS
PRIVATE


!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_="USR_Cell"
CHARACTER(*),PARAMETER :: file_="12-B-USR_Cell.f90"


!!## TYPE DEFINITION for CELL type
TYPE TYPE_Cell
 !PRIVATE
 INTEGER       ,POINTER :: CellShape         => NULL()
 INTEGER       ,POINTER :: FaceList(:)       => NULL()
 REAL(KIND_MSH),POINTER :: CellVolume        => NULL()
 REAL(KIND_MSH),POINTER :: CellCentroid(:)   => NULL()
 REAL(KIND_MSH),POINTER :: CellCoeff(:)      => NULL()
 REAL(KIND_MSH),POINTER :: CellSurfaceArea   => NULL()
END TYPE

!!## PROCEDURE OVERLOADING
INTERFACE ALLOCATE        ; MODULE PROCEDURE ALLOCATE_Cell     ; END INTERFACE
INTERFACE DEALLOCATE      ; MODULE PROCEDURE DEALLOCATE_Cell   ; END INTERFACE
INTERFACE NULLIFY         ; MODULE PROCEDURE NULLIFY_Cell      ; END INTERFACE
INTERFACE COPY            ; MODULE PROCEDURE COPY_Cell         ; END INTERFACE
INTERFACE ptr_CellShape   ; MODULE PROCEDURE ptr_CellShape_CELL ; END INTERFACE
INTERFACE FaceList        ; MODULE PROCEDURE FaceList_CELL     ; END INTERFACE
INTERFACE ptr_FaceList    ; MODULE PROCEDURE ptr_FaceList_CELL  ; END INTERFACE
INTERFACE CellShape       ; MODULE PROCEDURE CellShape_CELL    ; END INTERFACE
INTERFACE NUM_Faces       ; MODULE PROCEDURE NUM_Faces_CELL    ; END INTERFACE
INTERFACE IsEqual         ; MODULE PROCEDURE IsEqual_Cell      ; END INTERFACE

!!## PUBLIC ACCESS
PUBLIC :: TYPE_Cell
PUBLIC :: ALLOCATE        , ALLOCATE_Cell
PUBLIC :: DEALLOCATE      , DEALLOCATE_Cell
PUBLIC :: NULLIFY         , NULLIFY_Cell
PUBLIC :: COPY            , COPY_Cell
PUBLIC :: ptr_CellShape   , ptr_CellShape_CELL
PUBLIC :: FaceList        , FaceList_CELL
PUBLIC :: ptr_FaceList    , ptr_FaceList_CELL
PUBLIC :: CellShape       , CellShape_CELL
PUBLIC :: NUM_Faces       , NUM_Faces_CELL
PUBLIC :: ptr_CellVolume
PUBLIC :: ptr_CellCentroid
PUBLIC :: IsEqual,IsEqual_Cell

!!## CONTAINED PROCEDURES
CONTAINS



!!### ELEMENTAL FUNCTION <<IsEqual_Cell>>
ELEMENTAL FUNCTION IsEqual_Cell( Cell1 , Cell2 ) RESULT(IsEqual)
!!#### PURPOSE
!! Check equality between two Cell types.

!!#### REQUIRED INPUT
TYPE(TYPE_Cell),INTENT(IN) :: Cell1,Cell2

!!#### REQUIRED OUTPUT
LOGICAL :: IsEqual

!!--begin--

IsEqual = (Cell1%CellShape==Cell2%CellShape)
IF( .NOT.IsEqual )RETURN

IsEqual = SIZE(Cell1%CellCoeff)==SIZE(Cell2%CellCoeff)
IF( .NOT.IsEqual )RETURN

IsEqual = ALL(Cell1%CellCoeff==Cell2%CellCoeff)
IF( .NOT.IsEqual )RETURN

IsEqual = SIZE(Cell1%FaceList)==SIZE(Cell2%FaceList)
IF( .NOT.IsEqual )RETURN

IsEqual = ALL(Cell1%FaceList==Cell2%FaceList)
IF( .NOT.IsEqual )RETURN

IsEqual = (Cell1%CellVolume==Cell2%CellVolume)
IF( .NOT.IsEqual )RETURN

IsEqual = SIZE(Cell1%CellCentroid)==SIZE(Cell2%CellCentroid)
IF( .NOT.IsEqual )RETURN

IsEqual = ALL(Cell1%CellCentroid==Cell2%CellCentroid)
IF( .NOT.IsEqual )RETURN

IsEqual = (Cell1%CellSurfaceArea==Cell2%CellSurfaceArea)

!!--end--
END FUNCTION



!!### PURE SUBROUTINE <<ALLOCATE_Cell>>
PURE SUBROUTINE ALLOCATE_Cell( Cell , CellShape , &
  CellCoeff , FaceList , CellVolume , CellCentroid , &
  CellSurfaceArea )

!!#### PURPOSE
!! Allocate the Cell type.

!!#### REQUIRED OUTPUT
TYPE(TYPE_Cell),INTENT(INOUT) :: Cell

!!#### OPTIONAL INPUT
INTEGER       ,OPTIONAL,INTENT(IN) :: CellShape
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: CellCoeff(:)
INTEGER       ,OPTIONAL,INTENT(IN) :: FaceList(:)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: CellVolume
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: CellCentroid(:)
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: CellSurfaceArea

!!--begin--
IF( PRESENT(CellShape) )THEN
 ALLOCATE( Cell%CellShape )
 Cell%CellShape = CellShape
ENDIF

IF( PRESENT(CellCoeff) )THEN
 ALLOCATE( Cell%CellCoeff(1:SIZE(CellCoeff)) )
 Cell%CellCoeff = CellCoeff
ENDIF

IF( PRESENT(FaceList) )THEN
 ALLOCATE( Cell%FaceList(1:SIZE(FaceList)) )
 Cell%FaceList = FaceList
END IF

IF( PRESENT(CellVolume) )THEN
 ALLOCATE( Cell%CellVolume )
 Cell%CellVolume = CellVolume
END IF

IF( PRESENT(CellCentroid) )THEN
 ALLOCATE( Cell%CellCentroid(1:SIZE(CellCentroid)) )
 Cell%CellCentroid = CellCentroid
END IF

IF( PRESENT(CellSurfaceArea) )THEN
 ALLOCATE( Cell%CellSurfaceArea )
 Cell%CellSurfaceArea = CellSurfaceArea
END IF

!!--end--
END SUBROUTINE



PURE ELEMENTAL SUBROUTINE COPY_Cell( CELL, COPY )
!!#### PURPOSE
!! Make a copy of a CELL.
!!#### REQUIRED INPUT
TYPE(TYPE_Cell),INTENT(IN)  :: CELL
!!#### REQUIRED OUTPUT
TYPE(TYPE_Cell),INTENT(OUT) :: COPY

!!--begin--
IF( ASSOCIATED(CELL%CellShape)    )THEN
 CALL ALLOCATE( COPY , CellShape=CELL%CellShape )
END IF

IF( ASSOCIATED(CELL%CellCoeff)    )THEN
 CALL ALLOCATE( COPY , CellCoeff=CELL%CellCoeff )
END IF

IF( ASSOCIATED(CELL%FaceList) )THEN
 CALL ALLOCATE( COPY , FaceList =CELL%FaceList   )
END IF

IF( ASSOCIATED(CELL%CellVolume)       )THEN
 CALL ALLOCATE( COPY , CellVolume   =CELL%CellVolume     )
END IF

IF( ASSOCIATED(CELL%CellCentroid)     )THEN
 CALL ALLOCATE( COPY , CellCentroid =CELL%CellCentroid   )
ENDIF

IF( ASSOCIATED(CELL%CellSurfaceArea)     )THEN
 CALL ALLOCATE( COPY , CellSurfaceArea =CELL%CellSurfaceArea   )
ENDIF

!!--end--
END SUBROUTINE


ELEMENTAL SUBROUTINE DEALLOCATE_Cell( Cell )
!!#### PURPOSE
!! Deallocate the Cell type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Cell),INTENT(INOUT) :: Cell

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
DEALLOCATE( Cell % CellShape , STAT=jerr )
DEALLOCATE( Cell % CellCoeff , STAT=jerr )
DEALLOCATE( Cell % FaceList  , STAT=jerr )
DEALLOCATE( Cell % CellVolume    , STAT=jerr )
DEALLOCATE( Cell % CellCentroid  , STAT=jerr )
DEALLOCATE( Cell % CellSurfaceArea , STAT=jerr )

!!--end--
END SUBROUTINE


ELEMENTAL SUBROUTINE NULLIFY_Cell( Cell )
!!#### PURPOSE
!! Nullify the Cell type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Cell),INTENT(INOUT) :: Cell

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
NULLIFY( Cell % CellShape )
NULLIFY( Cell % CellCoeff )
NULLIFY( Cell % FaceList   )
NULLIFY( Cell % CellVolume     )
NULLIFY( Cell % CellCentroid   )
NULLIFY( Cell % CellSurfaceArea )

!!--end--
END SUBROUTINE

PURE FUNCTION CellShape_CELL( Cell )
TYPE(TYPE_Cell),INTENT(IN) :: Cell
INTEGER :: CellShape_CELL
CellShape_CELL = Cell % CellShape
END FUNCTION

PURE FUNCTION NUM_Faces_CELL(Cell) RESULT(NUM)
TYPE(TYPE_Cell),INTENT(IN) :: Cell
INTEGER :: NUM
NUM = SIZE(Cell % FaceList)
END FUNCTION

FUNCTION ptr_CellShape_CELL( Cell )
TYPE(TYPE_Cell),INTENT(IN) :: Cell
INTEGER,POINTER :: ptr_CellShape_CELL
ptr_CellShape_CELL => Cell % CellShape
END FUNCTION

FUNCTION ptr_CellCoeff( Cell )
TYPE(TYPE_Cell),INTENT(IN) :: Cell
REAL(KIND_MSH),POINTER :: ptr_CellCoeff(:)
ptr_CellCoeff => Cell % CellCoeff
END FUNCTION

FUNCTION ptr_FaceList_CELL( Cell )
TYPE(TYPE_Cell),INTENT(IN) :: Cell
INTEGER,POINTER :: ptr_FaceList_CELL(:)
ptr_FaceList_CELL => Cell % FaceList
END FUNCTION

PURE FUNCTION FaceList_CELL( Cell )
TYPE(TYPE_Cell),INTENT(IN) :: Cell
INTEGER :: FaceList_CELL(1:NUM_Faces(Cell))
FaceList_CELL = Cell % FaceList
END FUNCTION

FUNCTION ptr_CellVolume( Cell )
TYPE(TYPE_Cell),INTENT(IN) :: Cell
REAL(KIND_MSH),POINTER :: ptr_CellVolume
ptr_CellVolume => Cell % CellVolume
END FUNCTION

FUNCTION ptr_CellCentroid( Cell )
TYPE(TYPE_Cell),INTENT(IN) :: Cell
REAL(KIND_MSH),POINTER :: ptr_CellCentroid(:)
ptr_CellCentroid => Cell % CellCentroid
END FUNCTION

FUNCTION ptr_CellSurfaceArea( Cell )
TYPE(TYPE_Cell),INTENT(IN) :: Cell
REAL(KIND_MSH),POINTER :: ptr_CellSurfaceArea
ptr_CellSurfaceArea => Cell % CellSurfaceArea
END FUNCTION


END MODULE
