!!# USER MODULE <<USR_Edge>>
MODULE USR_Edge

!!## PURPOSE
!! Defines a 1-3 dimensional edge type.

!!## DETAILS
!
!! 1. %EdgeShape component
!!   This is an integer corresponding to the shape of an edge
!!   connecting two vertices.
!
!! 2. %EdgeCoeff component
!!   These are the coefficients that describe each edge.


!!## EXTERNAL KINDS
USE KND_Mesh !!((05-B-KND_Mesh.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_="USR_Edge"
CHARACTER(*),PARAMETER :: file_="06-B-USR_Edge.f90"


!!## TYPE DEFINITION for EDGE type
TYPE TYPE_Edge

 INTEGER       ,POINTER :: EdgeShape    => NULL()

 REAL(KIND_MSH),POINTER :: EdgeCoeff(:) => NULL()

END TYPE



!!## PROCEDURE OVERLOADING

!!### PROCEDURE OVERLOADING for ALLOCATE
INTERFACE ALLOCATE
 MODULE PROCEDURE ALLOCATE_Edge
END INTERFACE


!!### PROCEDURE OVERLOADING for DEALLOCATE
INTERFACE DEALLOCATE
 MODULE PROCEDURE DEALLOCATE_Edge
 MODULE PROCEDURE DEALLOCATE_Edges
END INTERFACE


!!### PROCEDURE OVERLOADING for NULLIFY
INTERFACE NULLIFY
 MODULE PROCEDURE NULLIFY_Edge
 MODULE PROCEDURE NULLIFY_Edges
END INTERFACE


!!### PROCEDURE OVERLOADING for COPY
INTERFACE COPY
 MODULE PROCEDURE COPY_Edge
 MODULE PROCEDURE COPY_Edges
END INTERFACE

INTERFACE ptr_EdgeShape
 MODULE PROCEDURE ptr_EdgeShape_
END INTERFACE

INTERFACE IsEqual
 MODULE PROCEDURE IsEqual_Edge
END INTERFACE


!!## PUBLIC ACCESS
PUBLIC :: TYPE_Edge
PUBLIC :: ALLOCATE
PUBLIC :: DEALLOCATE
PUBLIC :: NULLIFY
PUBLIC :: COPY
PUBLIC :: ptr_EdgeShape,ptr_EdgeCoeff
PUBLIC :: IsEqual_Edge,IsEqual


!!## CONTAINED PROCEDURES
CONTAINS


!!### ELEMENTAL FUNCTION <<IsEqual_Edge>>
ELEMENTAL FUNCTION IsEqual_Edge(Edge1,Edge2)  RESULT(IsEqual)

!!#### PURPOSE
!! Check for equality of two edges.

!!#### REQUIRED INPUT
TYPE(TYPE_Edge),INTENT(IN) :: Edge1,Edge2

!!#### REQUIRED OUTPUT
LOGICAL :: IsEqual

!!--begin--

IsEqual = (Edge1%EdgeShape==Edge2%EdgeShape)
IF( .NOT.IsEqual )RETURN

IsEqual = (SIZE(Edge1%EdgeCoeff)==SIZE(Edge2%EdgeCoeff))
IF( .NOT.IsEqual )RETURN

IsEqual = ALL(Edge1%EdgeCoeff==Edge2%EdgeCoeff)
IF( .NOT.IsEqual )RETURN

!!--end--
END FUNCTION


!!### PURE SUBROUTINE <<ALLOCATE_Edge>>
PURE SUBROUTINE ALLOCATE_Edge( Edge , EdgeShape , &
  EdgeCoeff )
!!#### PURPOSE
!! Allocate the Edge type.

!!#### REQUIRED OUTPUT
TYPE(TYPE_Edge),INTENT(OUT) :: Edge

!!#### OPTIONAL INPUT
INTEGER        ,OPTIONAL,INTENT(IN) :: EdgeShape
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: EdgeCoeff(:) 
!!--begin--
NULLIFY( Edge%EdgeShape )
NULLIFY( Edge%EdgeCoeff )

IF( PRESENT(EdgeShape) )THEN
    ALLOCATE( Edge%EdgeShape )
    Edge%EdgeShape = EdgeShape
ENDIF

IF( PRESENT(EdgeCoeff) )THEN
    ALLOCATE( Edge%EdgeCoeff(1:SIZE(EdgeCoeff)) )
    Edge%EdgeCoeff = EdgeCoeff
ENDIF

!!--end--
END SUBROUTINE



PURE FUNCTION COPY_Edge( EDGE ) RESULT(COPY)
!!#### PURPOSE
!! Make a copy of an EDGE.
!!#### REQUIRED INPUT
TYPE(TYPE_Edge),INTENT(IN)  :: EDGE
!!#### REQUIRED OUTPUT
TYPE(TYPE_Edge) :: COPY

!!--begin--
IF( ASSOCIATED(EDGE%EdgeShape)    )THEN
 CALL ALLOCATE( COPY , EdgeShape=EDGE%EdgeShape )
END IF

IF( ASSOCIATED(EDGE%EdgeCoeff)    )THEN
 CALL ALLOCATE( COPY , EdgeCoeff=EDGE%EdgeCoeff )
END IF

!!--end--
END FUNCTION


PURE SUBROUTINE COPY_Edges( EDGES, COPY )
!!#### PURPOSE
!! Make a copy of an EDGE.
!!#### REQUIRED INPUT
TYPE(TYPE_Edge),INTENT(IN)  :: EDGES(:)
!!#### REQUIRED OUTPUT
TYPE(TYPE_Edge),INTENT(INOUT),POINTER :: COPY(:)

!!#### LOCAL VARIABLES
INTEGER :: l

!!--begin--
IF( ASSOCIATED(COPY) )THEN
  CALL DEALLOCATE_Edges(COPY)
  DEALLOCATE(COPY)
END IF
NULLIFY(COPY)
ALLOCATE( COPY(SIZE(EDGES)) )
DO l=1,SIZE(EDGES)
 IF( ASSOCIATED(EDGES(l)%EdgeShape)    )THEN
  CALL ALLOCATE( COPY(l) , EdgeShape=EDGES(l)%EdgeShape )
 END IF

 IF( ASSOCIATED(EDGES(l)%EdgeCoeff)    )THEN
  CALL ALLOCATE( COPY(l) , EdgeCoeff=EDGES(l)%EdgeCoeff )
 END IF
END DO

!!--end--
END SUBROUTINE



PURE SUBROUTINE DEALLOCATE_Edge( Edge )
!!#### PURPOSE
!! Deallocate the Edge type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Edge),INTENT(INOUT) :: Edge

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
IF( ASSOCIATED( Edge%EdgeShape ) )DEALLOCATE( Edge % EdgeShape , STAT=jerr )
IF( ASSOCIATED( Edge%EdgeCoeff ) )DEALLOCATE( Edge % EdgeCoeff , STAT=jerr )

!!--end--
END SUBROUTINE


PURE SUBROUTINE DEALLOCATE_Edges( Edges )
!!#### PURPOSE
!! Deallocate a list of edges type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Edge),INTENT(INOUT) :: Edges(:)

!!#### LOCAL VARIABLES
INTEGER :: l,jerr

!!--begin--
DO l=1,SIZE(Edges)
  CALL DEALLOCATE_Edge(Edges(l))
END DO
!!--end--
END SUBROUTINE


PURE SUBROUTINE NULLIFY_Edge( Edge )
!!#### PURPOSE
!! Nullify the Edge type.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Edge),POINTER :: Edge

!!--begin--
NULLIFY( Edge % EdgeShape )
NULLIFY( Edge % EdgeCoeff )
NULLIFY( Edge )

!!--end--
END SUBROUTINE



PURE SUBROUTINE NULLIFY_Edges( Edges )
!!#### PURPOSE
!! Nullify the Edge type (array).

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Edge),POINTER :: Edges(:)

!!#### LOCAL VARIABLES
INTEGER :: m

!!--begin--
DO m=1,SIZE(Edges)
 NULLIFY( Edges(m) % EdgeShape )
 NULLIFY( Edges(m) % EdgeCoeff )
END DO
NULLIFY( Edges )

!!--end--
END SUBROUTINE


FUNCTION ptr_EdgeShape_( Edge )
TYPE(TYPE_Edge),INTENT(IN) :: Edge
INTEGER,POINTER :: ptr_EdgeShape_
ptr_EdgeShape_ => Edge % EdgeShape
END FUNCTION

FUNCTION ptr_EdgeCoeff( Edge )
TYPE(TYPE_Edge),INTENT(IN) :: Edge
REAL(KIND_MSH),POINTER :: ptr_EdgeCoeff(:)
ptr_EdgeCoeff => Edge % EdgeCoeff
END FUNCTION


ENDMODULE
