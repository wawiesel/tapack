!!# USER MODULE >>USR_IntegralRegion<<
MODULE USR_IntegralRegion

!!## PURPOSE
!! Provide the <TYPE_IntegralRegion> type which defines regions
!! in space and functions to be evaluated over them.

!!## MODULES
USE KND_Mesh !!((05-B-KND_Mesh.f90))
USE PAR_Mesh !!((06-B-PAR_Mesh.f90))

IMPLICIT NONE

PRIVATE

!!## TYPE DEFINITION: Integral Region
TYPE TYPE_IntegralRegion
 CHARACTER(LEN_MESH_LABEL)   :: label        =  REPEAT(" ",LEN_MESH_LABEL)
 INTEGER                :: shape        =  0
 REAL(KIND_MSH)         :: scale        =  0._KIND_MSH
 REAL(KIND_MSH),POINTER :: coeff(:)     => NULL()
 REAL(KIND_MSH),POINTER :: integrals(:) => NULL()
 INTEGER       ,POINTER :: set(:)       => NULL()
END TYPE

PUBLIC :: TYPE_IntegralRegion
PUBLIC :: REALLOCATE_IntegralRegion

CONTAINS

!!### SUBROUTINE >>REALLOCATE_IntegralRegion<<
SUBROUTINE REALLOCATE_IntegralRegion( IntegralRegion , dn )
!!#### PURPOSE
!! Reallocate the IntegralRegion type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_IntegralRegion),POINTER :: IntegralRegion(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: dn

!!#### LOCAL VARIABLES
INTEGER :: r
TYPE(TYPE_IntegralRegion),POINTER :: COPY_IntegralRegion(:)

!!--begin--
NULLIFY( COPY_IntegralRegion )

!point COPY_IntegralRegion to old IntegralRegion
IF( ASSOCIATED(IntegralRegion) )THEN
 COPY_IntegralRegion => IntegralRegion
END IF

!if it was associated then reallocate
IF( ASSOCIATED(COPY_IntegralRegion) )THEN

 !allocate new IntegralRegion
 ALLOCATE( IntegralRegion(1:dn+SIZE(COPY_IntegralRegion)) )

 !copy loop
 DO r=1,SIZE(COPY_IntegralRegion)
  IntegralRegion(r) = COPY_IntegralRegion(r)
 END DO
 !destroy old IntegralRegion
 NULLIFY( COPY_IntegralRegion )

ELSE

 !allocate the IntegralRegion
 ALLOCATE( IntegralRegion(1:dn) )

END IF

!!--end--
END SUBROUTINE



END MODULE
