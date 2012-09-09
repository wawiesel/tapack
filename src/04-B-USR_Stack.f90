MODULE USR_Stack
IMPLICIT NONE
PRIVATE
INTEGER,PARAMETER :: DEFAULT_dN          =  10
LOGICAL,PARAMETER :: REALLOCATE_if_Full  = .TRUE.
LOGICAL,PARAMETER :: DEALLOCATE_if_Empty = .FALSE.

TYPE TYPE_Stack
 INTEGER,POINTER :: stack(:)
 INTEGER         :: top           = -1
 INTEGER         :: num_allocated = -1
ENDTYPE
PUBLIC :: TYPE_Stack

INTERFACE ALLOCATE
 MODULE PROCEDURE ALLOCATE_Stack
ENDINTERFACE
PUBLIC :: ALLOCATE

INTERFACE DEALLOCATE
 MODULE PROCEDURE DEALLOCATE_Stack
ENDINTERFACE
PUBLIC :: DEALLOCATE

INTERFACE REALLOCATE
 MODULE PROCEDURE REALLOCATE_Stack
ENDINTERFACE
PUBLIC :: REALLOCATE

INTERFACE NULLIFY
 MODULE PROCEDURE NULLIFY_Stack
ENDINTERFACE
PUBLIC :: NULLIFY

INTERFACE ASSIGNMENT(=)
 MODULE PROCEDURE Equals_Stack
ENDINTERFACE
PUBLIC :: ASSIGNMENT(=)

INTERFACE OPERATOR(==)
 MODULE PROCEDURE Equality_Stack
ENDINTERFACE
PUBLIC :: OPERATOR(==)

INTERFACE PUSH
 MODULE PROCEDURE PUSH_Stack
ENDINTERFACE
PUBLIC :: PUSH

INTERFACE POP
 MODULE PROCEDURE POP_Stack
ENDINTERFACE
PUBLIC :: POP

INTERFACE TOP
 MODULE PROCEDURE TOP_Stack
ENDINTERFACE
PUBLIC :: TOP

INTERFACE IsEmpty
 MODULE PROCEDURE IsEmpty_Stack
ENDINTERFACE
PUBLIC :: IsEmpty

CONTAINS

PURE SUBROUTINE ALLOCATE_Stack( A , N )
TYPE(TYPE_Stack),INTENT(OUT) :: A
INTEGER,INTENT(IN),OPTIONAL :: N
IF( ASSOCIATED(A%stack) )THEN
 CALL DEALLOCATE_Stack( A )
ENDIF

IF( PRESENT(N) )THEN
 ALLOCATE( A%stack(1:N) )
ELSE
 ALLOCATE( A%stack(1:DEFAULT_dN) )
ENDIF

A % stack = 0
A % top   = 0
A % num_allocated = SIZE(A%stack)

ENDSUBROUTINE

PURE SUBROUTINE REALLOCATE_Stack( A , dN )
TYPE(TYPE_Stack),INTENT(INOUT) :: A
INTEGER,INTENT(IN),OPTIONAL :: dN
TYPE(TYPE_Stack) :: COPY_A
INTEGER :: N,jerr
IF( ASSOCIATED(A%stack) )THEN
 COPY_A = A
 N = A % top + MERGE( dN , DEFAULT_dN , PRESENT(dN) )
 CALL DEALLOCATE_Stack( A )
!! CALL ALLOCATE_Stack( A , N )
 A = COPY_A
ELSE
 CALL NULLIFY_Stack( A )
ENDIF
ENDSUBROUTINE

PURE SUBROUTINE DEALLOCATE_Stack( A )
TYPE(TYPE_Stack),INTENT(OUT) :: A
INTEGER :: jerr
DEALLOCATE(A%stack,STAT=jerr)
A % top           = -1
A % num_allocated = -1
ENDSUBROUTINE

PURE SUBROUTINE NULLIFY_Stack( A )
TYPE(TYPE_Stack),INTENT(OUT) :: A
NULLIFY(A%stack)
A % top           = 0
A % num_allocated = 0
ENDSUBROUTINE

PURE SUBROUTINE Equals_Stack( A , B )
TYPE(TYPE_Stack),INTENT(OUT) :: A
TYPE(TYPE_Stack),INTENT(IN)  :: B
INTEGER :: N
IF( ASSOCIATED(B%stack) )THEN
 N               = B % top
 A%top           = N
 ALLOCATE( A%stack(1:N) )
 A%stack(1:N)    = B % stack(1:N)
 A%num_allocated = B % num_allocated
ELSE
 CALL NULLIFY_Stack( A )
ENDIF
ENDSUBROUTINE

PURE FUNCTION Equality_Stack( A , B ) RESULT(Equality)
TYPE(TYPE_Stack),INTENT(IN) :: A
TYPE(TYPE_Stack),INTENT(IN) :: B
LOGICAL :: Equality
INTEGER :: N
Equality = .TRUE.
IF( .NOT.(ASSOCIATED(B%stack).AND.ASSOCIATED(A%stack)) )THEN
 Equality = .FALSE.
 RETURN
ELSE
 IF( (.NOT.ASSOCIATED(B%stack)) .AND. (.NOT.ASSOCIATED(A%stack)) )THEN
  RETURN
 ELSE
  N = B % top
  IF( N/=A%top )THEN
   Equality = .FALSE.
   RETURN
  ENDIF
  IF( ALL(A%stack(1:N)/=B%stack(1:N)) )THEN
   Equality = .FALSE.
   RETURN
  ENDIF
 ENDIF
ENDIF
ENDFUNCTION


PURE SUBROUTINE PUSH_Stack( I , A , IsFull )
INTEGER,INTENT(IN) :: I
TYPE(TYPE_Stack),INTENT(INOUT) :: A
LOGICAL,OPTIONAL,INTENT(OUT) :: IsFull
IF( A%num_allocated==A%top )THEN
 IF( PRESENT(IsFull) )THEN
  IsFull = .TRUE.
 ENDIF
 IF( REALLOCATE_if_Full )THEN
  CALL REALLOCATE_Stack( A )
 ENDIF
ELSE
 IF( PRESENT(IsFull) )THEN
 IsFull = .FALSE.
 ENDIF
ENDIF
A%top = A%top + 1
A%stack(A%top) = I
ENDSUBROUTINE

PURE SUBROUTINE POP_Stack( I , A , IsEmpty )
INTEGER,INTENT(OUT) :: I
TYPE(TYPE_Stack),INTENT(INOUT) :: A
LOGICAL,OPTIONAL,INTENT(OUT) :: IsEmpty
IF( A%top==0 )THEN
 IF( PRESENT(IsEmpty) )THEN
  IsEmpty = .TRUE.
 ENDIF
 IF( DEALLOCATE_if_Empty )THEN
  CALL DEALLOCATE_Stack( A )
 ENDIF
 I = -1
 RETURN
ELSE
 IF( PRESENT(IsEmpty) )THEN
  IsEmpty = .FALSE.
 ENDIF
ENDIF
I = A%stack(A%top)
A%top = A%top - 1
ENDSUBROUTINE

PURE FUNCTION IsEmpty_Stack( A ) RESULT(IsEmpty)
TYPE(TYPE_Stack),INTENT(IN) :: A
LOGICAL :: IsEmpty
IsEmpty = A%top==0
ENDFUNCTION

PURE FUNCTION IsFull_Stack( A ) RESULT(IsFull)
TYPE(TYPE_Stack),INTENT(IN) :: A
LOGICAL :: IsFull
IsFull = A%top==A%num_allocated
ENDFUNCTION

PURE FUNCTION TOP_Stack( A ) RESULT(I)
TYPE(TYPE_Stack),INTENT(IN) :: A
INTEGER :: I
I = A%stack(A%top)
ENDFUNCTION

ENDMODULE
