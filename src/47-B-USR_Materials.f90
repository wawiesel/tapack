!!# USER MODULE: <USR_Materials>
MODULE USR_Materials

!!## PURPOSE
!! Provide utility routines to associate material id numbers
!! with cells.


!!## USAGE
!! Store a bunch of polygons of various materials.  Routines
!! are provided to associate cells with these materials
!! and split the mesh along material polygon boundaries,
!! such that the resultant mesh has cells that have
!! a single material.


!!## EXTERNAL KINDS
USE KND_Mesh,ONLY: KIND_MSH !!((05-B-KND_Mesh.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## EXTERNAL PARAMETERS
!! * allowed name length for materials
INTEGER,PARAMETER :: LEN_Material = 16

!!## TYPE DEFINITION: Material Fill Region
TYPE TYPE_MaterialFill
 REAL(KIND_MSH),POINTER :: coeff(:) => NULL()
 INTEGER                :: shape    =  0
 INTEGER                :: l        =  0
END TYPE

!!#### REALLOCATE PROCEDURE
INTERFACE Reallocate
 MODULE PROCEDURE Reallocate_MaterialFill
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: LEN_Material
PUBLIC :: TYPE_MaterialFill
PUBLIC :: Reallocate

CONTAINS




SUBROUTINE Reallocate_MaterialFill( MaterialFill , dn )
!!#### PURPOSE
!! Reallocate the MaterialFill type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_MaterialFill),POINTER :: MaterialFill(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: dn

!!#### LOCAL VARIABLES
TYPE(TYPE_MaterialFill),POINTER :: COPY_MaterialFill(:)
INTEGER :: i

!!--begin--
IF( ASSOCIATED(MaterialFill) )THEN
 !point COPY_MaterialFill to old MaterialFill
 COPY_MaterialFill => MaterialFill
ELSE
 COPY_MaterialFill => NULL()
END IF

!point old MaterialFill to null
MaterialFill => NULL()

!if it was associated then reallocate
IF( ASSOCIATED(COPY_MaterialFill) )THEN

 !allocate new MaterialFill
 ALLOCATE( MaterialFill(1:dn+SIZE(COPY_MaterialFill)) )

 !copy loop
 DO i=1,SIZE(COPY_MaterialFill)
  MaterialFill(i) = COPY_MaterialFill(i)
 END DO
 !destroy old MaterialFill
 NULLIFY( COPY_MaterialFill )

ELSE

 !allocate the MaterialFill
 ALLOCATE( MaterialFill(1:dn) )

END IF

!!--end--
END SUBROUTINE

END MODULE
