!!# USER MODULE <<USR_Domain>>
MODULE USR_Domain

!!## PURPOSE
!! Defines a 1-3 dimensional Domain type.

!!## DETAILS
!
!! 1. %DomainShape
!!   This is an integer corresponding to the shape of the Domain.
!
!! 2. %Verts component
!!   This is a list of verts which create the domain.
!
!! 3. %Faces component
!!   This is the list of faces which border the domain.
!
!! 4. %Cell
!!   This is the cell that specify the domain.
!

!!## EXTERNAL KINDS
USE KND_Mesh       !!((05-B-KND_Mesh.f90))

!!## EXTERNAL PROCEDURES
USE SUB_Reallocate !!((04-B-SUB_Reallocate.f90))

!!## GLOBAL USER MODULES
USE USR_Face       !!((11-B-USR_Face.f90))
USE USR_Cell       !!((12-B-USR_Cell.f90))
USE USR_Edge       !!((06-B-USR_Edge.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_="USR_Domain"
CHARACTER(*),PARAMETER :: file_="13-B-USR_Domain.f90"


!!## TYPE DEFINITION for Domain type
TYPE TYPE_Domain
 !PRIVATE
 INTEGER        ,POINTER :: NDim        => NULL()
 INTEGER        ,POINTER :: DomainShape => NULL()
 REAL(KIND_MSH) ,POINTER :: Verts(:,:)  => NULL()
 TYPE(TYPE_Face),POINTER :: Faces(:)    => NULL()
 TYPE(TYPE_Cell),POINTER :: Cell        => NULL()
ENDTYPE


!!## PUBLIC ACCESS
PUBLIC :: TYPE_Domain
PUBLIC :: ALLOCATE_Domain
PUBLIC :: DEALLOCATE_Domain
PUBLIC :: NULLIFY_Domain
PUBLIC :: COPY_Domain
PUBLIC :: ptr_DomainShape_Domain
PUBLIC :: ptr_Cell_Domain
PUBLIC :: ptr_Faces_Domain
PUBLIC :: ptr_Verts_Domain
PUBLIC :: ptr_NDim_Domain
PUBLIC :: ADD_Vert_Domain
PUBLIC :: ADD_Face_Domain
PUBLIC :: ADD_Cell_Domain
PUBLIC :: IsEqual_Domain


!!## MODULE PROCEDURES
CONTAINS


!!### FUNCTION <<IsEqual_Domain>>
FUNCTION IsEqual_Domain(Domain1,Domain2) RESULT(IsEqual)

!!#### PURPOSE
!! Check equality of two domains.

!!#### REQUIRED INPUT
TYPE(TYPE_Domain),INTENT(IN) :: Domain1,Domain2

!!#### REQUIRED OUTPUT
LOGICAL :: IsEqual

!!--begin--

IsEqual = (Domain1%NDim==Domain2%NDim)
IF( .NOT.IsEqual )RETURN

IsEqual = (Domain1%DomainShape==Domain2%DomainShape)
IF( .NOT.IsEqual )RETURN

IsEqual = ALL(Domain1%Verts==Domain2%Verts)
IF( .NOT.IsEqual )RETURN

IsEqual = ALL(IsEqual_Face(Domain1%Faces,Domain2%Faces))
IF( .NOT.IsEqual )RETURN

IsEqual = IsEqual_Cell(Domain1%Cell,Domain2%Cell)
IF( .NOT.IsEqual )RETURN

!!--end--
END FUNCTION


!!### PURE SUBROUTINE <<ALLOCATE_Domain>>
PURE SUBROUTINE ALLOCATE_Domain( Domain , NDim , DomainShape )

!!#### PURPOSE
!! Allocate the Domain type.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: NDim

!!#### REQUIRED OUTPUT
TYPE(TYPE_Domain),INTENT(OUT) :: Domain

!!#### OPTIONAL INPUT
INTEGER    ,OPTIONAL,INTENT(IN) :: DomainShape

!!--begin--
ALLOCATE( Domain%NDim )
Domain % NDim = NDim

IF( PRESENT(DomainShape) )THEN
 ALLOCATE( Domain%DomainShape )
 Domain%DomainShape = DomainShape
ENDIF

!!--end--
END SUBROUTINE



PURE ELEMENTAL FUNCTION COPY_Domain( Domain ) RESULT(D)
!!#### PURPOSE
!! Make a copy of a Domain.

!!#### REQUIRED INPUT
TYPE(TYPE_Domain),INTENT(IN)  :: Domain

!!#### REQUIRED OUTPUT
TYPE(TYPE_Domain) :: D

!!--begin--

IF( ASSOCIATED(Domain%NDim) )THEN
 ALLOCATE( D%NDim )
 D%NDim = Domain%NDim
END IF

IF( ASSOCIATED(Domain%DomainShape) )THEN
 ALLOCATE( D%DomainShape )
 D%DomainShape = Domain%DomainShape
END IF

IF( ASSOCIATED(Domain%Verts)       )THEN
 ALLOCATE( D%Verts(1:SIZE(DOMAIN%Verts,1),1:SIZE(DOMAIN%Verts,2)) )
 D%Verts = DOMAIN%Verts
END IF

IF( ASSOCIATED(Domain%Faces)       )THEN
 ALLOCATE( D%Faces(1:SIZE(DOMAIN%Faces)) )
 CALL COPY( DOMAIN%Faces, D%Faces )
END IF

IF( ASSOCIATED(Domain%Cell)       )THEN
 ALLOCATE( D%Cell )
 CALL COPY( DOMAIN%Cell, D%Cell )
END IF

!!--end--
END FUNCTION


ELEMENTAL SUBROUTINE DEALLOCATE_Domain( Domain )
!!#### PURPOSE
!! Deallocate the Domain type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
IF( ASSOCIATED(Domain%NDim) )THEN
 DEALLOCATE( Domain%NDim )
END IF

IF( ASSOCIATED(Domain%DomainShape) )THEN
 DEALLOCATE( Domain%DomainShape , STAT=jerr )
END IF

IF( ASSOCIATED(Domain%Verts)       )THEN
 DEALLOCATE( Domain%Verts , STAT=jerr )
END IF

IF( ASSOCIATED(Domain%Faces)       )THEN
 CALL DEALLOCATE( Domain%Faces )
 DEALLOCATE( Domain%Faces , STAT=jerr )
END IF

IF( ASSOCIATED(Domain%Cell)       )THEN
 CALL DEALLOCATE( Domain%Cell )
 DEALLOCATE( Domain%Cell , STAT=jerr )
END IF

!!--end--
END SUBROUTINE


ELEMENTAL SUBROUTINE NULLIFY_Domain( Domain )
!!#### PURPOSE
!! Nullify the Domain type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!--begin--
NULLIFY( Domain % NDim )
NULLIFY( Domain % DomainShape )
NULLIFY( Domain % verts )
CALL NULLIFY( Domain%Faces )
NULLIFY( DOMAIN%Faces )
CALL NULLIFY( Domain%Cell )
NULLIFY( DOMAIN%Cell )

!!--end--
END SUBROUTINE


FUNCTION ptr_NDim_Domain( Domain ) RESULT(ptr_NDim_)
TYPE(TYPE_Domain),INTENT(IN) :: Domain
INTEGER,POINTER :: ptr_NDim_
ptr_NDim_ => Domain % NDim
END FUNCTION


FUNCTION ptr_DomainShape_Domain( Domain ) RESULT(ptr_DomainShape_)
TYPE(TYPE_Domain),INTENT(IN) :: Domain
INTEGER,POINTER :: ptr_DomainShape_
ptr_DomainShape_ => Domain % DomainShape
END FUNCTION

FUNCTION ptr_Cell_Domain( Domain ) RESULT(ptr_Cell_)
TYPE(TYPE_Domain),INTENT(IN) :: Domain
TYPE(TYPE_Cell),POINTER :: ptr_Cell_
ptr_Cell_ => Domain % Cell
END FUNCTION

FUNCTION ptr_Faces_Domain( Domain ) RESULT(ptr_Faces_)
TYPE(TYPE_Domain),INTENT(IN) :: Domain
TYPE(TYPE_Face),POINTER :: ptr_Faces_(:)
ptr_Faces_ => Domain % Faces
END FUNCTION

FUNCTION ptr_Verts_Domain( Domain ) RESULT(ptr_Verts_)
TYPE(TYPE_Domain),INTENT(IN) :: Domain
REAL(KIND_MSH),POINTER :: ptr_Verts_(:,:)
ptr_Verts_ => Domain % Verts
END FUNCTION



FUNCTION ADD_Vert_Domain( Domain , Vert ) RESULT(kd)
!!#### PURPOSE
!! Adds a Vert to the DOMAIN.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### REQUIRED INPUT
REAL(KIND_MSH),INTENT(IN) :: Vert(1:Domain%NDim)

!!#### REQUIRED OUTPUT
INTEGER :: kd

!!--begin--
IF( .NOT.ASSOCIATED(Domain%Verts) )THEN
 kd = 1
 ALLOCATE( Domain%Verts(1:Domain%NDim,kd) )
ELSE
 kd = SIZE(Domain%Verts,2) + 1
 CALL REALLOCATE( Domain%Verts , dn=(/0,1/) )
END IF

Domain%Verts(:,kd) = Vert

!!--end--
END FUNCTION



FUNCTION ADD_Face_domain( Domain , FaceShape , &
  FaceNormal , VertList , Edges , FaceArea , FaceCentroid ) RESULT(jd)
!!#### PURPOSE
!! Adds a Face to the Mesh.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### OPTIONAL INPUT
INTEGER        ,OPTIONAL,INTENT(IN) :: FaceShape
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceNormal(:)
INTEGER        ,OPTIONAL,INTENT(IN) :: VertList(:)
TYPE(TYPE_Edge),OPTIONAL,INTENT(IN) :: Edges(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceArea
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: FaceCentroid(:)

!!#### REQUIRED OUTPUT
INTEGER :: jd
TYPE(TYPE_Face),POINTER :: copy(:)

!!#### LOCAL VARIABLES
INTEGER :: j,j_

!!--begin--
!reallocate Mesh%DOMAIN appropriately
IF( .NOT.ASSOCIATED(Domain%Faces) )THEN
 jd = 1
 ALLOCATE( Domain%Faces(1) )
ELSE
 !copy data into copy
 j  = SIZE(Domain%Faces,1)
 jd = j+1
 ALLOCATE( copy(1:jd) )
 FORALL(j_=1:j) copy(j_) = Domain%Faces(j_)
 !reallocate
 CALL NULLIFY( Domain%Faces ) !nullify structure
 NULLIFY( Domain%Faces )      !nullify pointer array
 !reconnect
 Domain%faces => copy
END IF

!now add new Face
CALL ALLOCATE( Domain%Faces(jd) , FaceShape , &
                                  FaceNormal , &
                                  VertList  , &
                                  Edges     , &
                                  FaceArea      , &
                                  FaceCentroid    )

!!--end--
END FUNCTION




FUNCTION ADD_Cell_Domain( Domain , CellShape , &
  CellCoeff , FaceList , CellVolume , CellCentroid , &
  CellSurfaceArea )  RESULT(id)
!!#### PURPOSE
!! Adds a Cell to the Domain.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Domain),INTENT(INOUT) :: Domain

!!#### OPTIONAL INPUT
INTEGER        ,OPTIONAL,INTENT(IN) :: CellShape
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellCoeff(:)
INTEGER        ,OPTIONAL,INTENT(IN) :: FaceList(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellVolume
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellCentroid(:)
REAL(KIND_MSH) ,OPTIONAL,INTENT(IN) :: CellSurfaceArea

!!#### REQUIRED OUTPUT
INTEGER :: id

!!--begin--
!replace for now
IF( ASSOCIATED(Domain%Cell) )DEALLOCATE(Domain%Cell)

!allocate the cell
ALLOCATE( Domain%Cell )

!add the domain cell
CALL ALLOCATE( Domain%Cell , CellShape , &
  CellCoeff , FaceList , CellVolume , CellCentroid , &
  CellSurfaceArea )

!return 0---there is only one domain cell so don"t want
!to give the illusion of an array of cells by returning 1
!or the illusion of an error by returning negative numbers
id = 0

!!--end--
END FUNCTION


ENDMODULE
