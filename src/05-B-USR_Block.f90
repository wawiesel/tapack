!!# USER MODULE  <<USR_Block>>
MODULE USR_Block

!!## PURPOSE
!! Defines the block type (TYPE_Block).  This type is a
!! doubly-linked list with dynamic string, integer, and
!! REAL(KIND_R) components.  It is very versatile and can
!! be used for a variety of jobs.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R,KIND_L,KIND_I,KIND_S !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PROCEDURES
USE SUB_CLEAR                                            !!((04-A-SUB_CLEAR.f90))
USE FUN_Sentence                                         !!((04-B-FUN_Sentence.f90))
USE SUB_Reallocate                                       !!((04-B-SUB_Reallocate.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## EXTERNAL PARAMETERS
INTEGER,PARAMETER :: LEN_S  = 128
PUBLIC :: KIND_R,KIND_L,KIND_I,KIND_S,LEN_S

!!## DERIVED TYPE DECLARATION
TYPE TYPE_Block
 INTEGER        (      KIND_I),POINTER :: I(:)  => NULL()
 CHARACTER      (LEN_S,KIND_S),POINTER :: S(:)  => NULL()
 REAL           (      KIND_R),POINTER :: R(:)  => NULL()
 LOGICAL        (      KIND_L),POINTER :: L(:)  => NULL()
 TYPE(TYPE_Block)             ,POINTER :: PREV  => NULL()
 TYPE(TYPE_Block)             ,POINTER :: NEXT  => NULL()
END TYPE

!!## PROCEDURE OVERLOADING
INTERFACE ALLOCATE
 MODULE PROCEDURE ALLOCATE0_Block
 MODULE PROCEDURE ALLOCATE1_Block
END INTERFACE

INTERFACE NULLIFY
 MODULE PROCEDURE NULLIFY_Block
ENDINTERFACE

! NOTE: This name was changed in triage so that it didn't coincide with
! iso_varying_string's insert which is a function. Apparently you can't
! have an interface that results in something as a function and subroutine
! with the same name.
INTERFACE INSERT_BLOCK
 MODULE PROCEDURE INSERT0_Block
 MODULE PROCEDURE INSERT1_Block
END INTERFACE

INTERFACE OVERWRITE
 MODULE PROCEDURE OVERWRITE0_Block
 MODULE PROCEDURE OVERWRITE1_Block
END INTERFACE

INTERFACE APPEND
 MODULE PROCEDURE APPEND0_Block
 MODULE PROCEDURE APPEND1_Block
END INTERFACE

INTERFACE DEALLOCATE
 MODULE PROCEDURE DEALLOCATE_Block
END INTERFACE

INTERFACE RELEASE
 MODULE PROCEDURE RELEASE0_Block
 MODULE PROCEDURE RELEASE1_Block
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: TYPE_Block
PUBLIC :: ALLOCATE
PUBLIC :: NULLIFY
PUBLIC :: INSERT_BLOCK
PUBLIC :: OVERWRITE
PUBLIC :: APPEND
PUBLIC :: DEALLOCATE
PUBLIC :: RELEASE

!!## MODULE PROCEDURES
CONTAINS


SUBROUTINE ALLOCATE0_Block(Block,I,S,R,L,PREV,NEXT)
!PURPOSE
!! Allocate the Block type with scalar I, S, L, and R.

!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER    :: Block
INTEGER(KIND_I)    ,OPTIONAL,INTENT(IN) :: I
CHARACTER(*,KIND_S),OPTIONAL,INTENT(IN) :: S
REAL(KIND_R)       ,OPTIONAL,INTENT(IN) :: R
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(IN) :: L
TYPE(TYPE_Block)   ,OPTIONAL,POINTER    :: PREV
TYPE(TYPE_Block)   ,OPTIONAL,POINTER    :: NEXT
!

ALLOCATE(Block)

!integer part
IF( PRESENT(I) )THEN
 ALLOCATE(Block%I(1)) ; Block%I = I
ELSE
 Block%I => NULL()
ENDIF

!string part
IF( PRESENT(S) )THEN
 Block%S => ptr_BlockConvert( S )
ELSE
 Block%S => NULL()
ENDIF

!real part
IF( PRESENT(R) )THEN
 ALLOCATE(Block%R(1)) ; Block%R = R
ELSE
 Block%R => NULL()
ENDIF

!logical part
IF( PRESENT(L) )THEN
 ALLOCATE(Block%L(1)) ; Block%L = L
ELSE
 Block%L => NULL()
ENDIF

!previous pointer
IF( PRESENT(PREV) )THEN
 Block%PREV => PREV
ELSE
 Block%PREV => NULL()
ENDIF

!next pointer
IF( PRESENT(NEXT) )THEN
 Block%NEXT => NEXT
ELSE
 Block%NEXT => NULL()
ENDIF

!
END SUBROUTINE


FUNCTION ptr_BlockConvert(S) RESULT(A1S)
CHARACTER(*,KIND=KIND_S),INTENT(IN) :: S
CHARACTER(LEN=LEN_S,KIND=KIND_S),POINTER :: A1S(:)
INTEGER :: i1,i2,is,ns

!!--begin--
ns = LEN_TRIM(S)/LEN_S + 1
ALLOCATE( A1S(1:ns) )
CALL CLEAR( A1S )
i1 = 1
DO is=1,ns
 i2 = i1 + LEN_S-1
 IF( i2>LEN_TRIM(S) )i2 = LEN_TRIM(S)
 A1S(is) = S(i1:i2)
 i1 = i2 + 1
 IF( i1>LEN_TRIM(S) )EXIT
END DO
!!--end--
END FUNCTION


SUBROUTINE ALLOCATE1_Block(Block,I,S,R,L,PREV,NEXT)
!PURPOSE
!! Allocate the Block type with arrays I, R, L, and scalar
!! string S.

!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER    :: Block
INTEGER(KIND_I)    ,         INTENT(IN) :: I(:)
CHARACTER(*,KIND_S),OPTIONAL,INTENT(IN) :: S
REAL(KIND_R)       ,OPTIONAL,INTENT(IN) :: R(:)
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(IN) :: L(:)
TYPE(TYPE_Block)   ,OPTIONAL,POINTER    :: PREV
TYPE(TYPE_Block)   ,OPTIONAL,POINTER    :: NEXT

!
ALLOCATE(Block)

!integer part
IF( SIZE(I)==0 )THEN
 Block%I => NULL()
ELSE
 ALLOCATE(Block%I(1:SIZE(I))) ; Block%I = I
END IF

!string part
IF( PRESENT(S) )THEN
 Block%S => ptr_BlockConvert(S)
ELSE
 Block%S => NULL()
ENDIF

!real part
IF( PRESENT(R) )THEN
 ALLOCATE(Block%R(1:SIZE(R))) ; Block%R = R
ELSE
 Block%R => NULL()
ENDIF

!logical part
IF( PRESENT(L) )THEN
 ALLOCATE(Block%L(1:SIZE(L))) ; Block%L = L
ELSE
 Block%L => NULL()
ENDIF

!previous pointer
IF( PRESENT(PREV) )THEN
 Block%PREV => PREV
ELSE
 Block%PREV => NULL()
ENDIF

!next pointer
IF( PRESENT(NEXT) )THEN
 Block%NEXT => NEXT
ELSE
 Block%NEXT => NULL()
ENDIF

!
END SUBROUTINE



SUBROUTINE INSERT0_Block(Block,I,S,R,L)
!PURPOSE
!! Insert a new block to %PREV of the passed Block, reconnecting
!! the pointers appropriately.
!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER    :: Block
INTEGER(KIND_I)             ,INTENT(IN) :: I
CHARACTER(*,KIND_S),OPTIONAL,INTENT(IN) :: S
REAL(KIND_R)       ,OPTIONAL,INTENT(IN) :: R
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(IN) :: L
!LOCALS
TYPE(TYPE_Block),POINTER :: prevBlock
!
IF( ASSOCIATED(Block) )THEN
 !possible redirection to OVERWRITE
 IF( ASSOCIATED(Block%I) )THEN
  IF( Block%I(1)==-HUGE(I) )THEN
   CALL OVERWRITE0_Block( Block , I , S , R , L )
   RETURN
  ENDIF
 ENDIF

 !INSERT part
 prevBlock    => Block%PREV
 Block % PREV => NULL()
 CALL ALLOCATE0_Block( Block % PREV , I , S , R , L , PREV=prevBlock , NEXT=Block )
 IF( ASSOCIATED(prevBlock) )THEN
  prevBlock % NEXT => Block % PREV
 ENDIF
ELSE
 CALL ALLOCATE0_Block( Block , I , S , R , L , PREV=NULL(Block) , NEXT=NULL(Block) )
ENDIF
!
END SUBROUTINE


SUBROUTINE INSERT1_Block(Block,I,S,R,L)
!PURPOSE
!! Insert a new block to %PREV of the passed Block, reconnecting
!! the pointers appropriately.
!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER    :: Block
INTEGER(KIND_I)    ,OPTIONAL,INTENT(IN) :: I(:)
CHARACTER(*,KIND_S),OPTIONAL,INTENT(IN) :: S
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(IN) :: L(:)
REAL(KIND_R)       ,OPTIONAL,INTENT(IN) :: R(:)
!LOCALS
TYPE(TYPE_Block),POINTER :: prevBlock
!
IF( ASSOCIATED(Block) )THEN

 !possible redirection to OVERWRITE
 IF( ASSOCIATED(Block%I) )THEN
  IF( Block%I(1)==-HUGE(I) )THEN
   CALL OVERWRITE( Block , I , S , R , L )
   RETURN
  ENDIF
 ENDIF

 !INSERT part
 prevBlock    => Block%PREV
 Block % PREV => NULL()
 CALL ALLOCATE1_Block( Block % PREV , I , S , R , L , PREV=prevBlock , NEXT=Block )
 IF( ASSOCIATED(prevBlock) )THEN
  prevBlock % NEXT => Block % PREV
 ENDIF

ELSE
 CALL ALLOCATE1_Block( Block , I , S , R , L , PREV=NULL(Block) , NEXT=NULL(Block) )
ENDIF
!
END SUBROUTINE




SUBROUTINE APPEND0_Block(Block,I,S,R,L)
!PURPOSE
!! Append a new block to %NEXT of the passed Block, reconnecting
!! the pointers appropriately.
!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER    :: Block
INTEGER(KIND_I)             ,INTENT(IN) :: I
CHARACTER(*,KIND_S),OPTIONAL,INTENT(IN) :: S
REAL(KIND_R)       ,OPTIONAL,INTENT(IN) :: R
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(IN) :: L
!LOCALS
TYPE(TYPE_Block),POINTER :: nextBlock
!
IF( ASSOCIATED(Block) )THEN
 !possible redirection to OVERWRITE
 IF( ASSOCIATED(Block%I) )THEN
  IF( Block%I(1)==-HUGE(I) )THEN
   CALL OVERWRITE0_Block( Block , I , S , R , L )
   RETURN
  ENDIF
 ENDIF

 !APPEND part
 nextBlock     => Block%NEXT
 Block % NEXT  => NULL()
 CALL ALLOCATE0_Block( Block % NEXT , I , S , R , L , PREV=Block , NEXT=nextBlock)
 IF( ASSOCIATED(nextBlock) )THEN
  nextBlock % PREV => Block % NEXT
 ENDIF
ELSE
 CALL ALLOCATE0_Block( Block , I , S , R , L , PREV=NULL(Block) , NEXT=NULL(Block) )
ENDIF
!
END SUBROUTINE

SUBROUTINE APPEND1_Block(Block,I,S,R,L)
!PURPOSE
!! Append a new block to %NEXT of the passed Block, reconnecting
!! the pointers appropriately.
!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER    :: Block
INTEGER(KIND_I)             ,INTENT(IN) :: I(:)
CHARACTER(*,KIND_S),OPTIONAL,INTENT(IN) :: S
REAL(KIND_R)       ,OPTIONAL,INTENT(IN) :: R(:)
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(IN) :: L(:)
!LOCALS
TYPE(TYPE_Block),POINTER :: nextBlock
!
!possible redirection to OVERWRITE
IF( ASSOCIATED(Block) )THEN

 IF( ASSOCIATED(Block%I) )THEN
  IF( Block%I(1)==-HUGE(I) )THEN
   CALL OVERWRITE( Block , I , S , R , L )
   RETURN
  ENDIF
 ENDIF

 !APPEND part
 nextBlock     => Block%NEXT
 Block % NEXT  => NULL()
 CALL ALLOCATE1_Block( Block % NEXT , I , S , R , L , PREV=Block , NEXT=nextBlock)
 IF( ASSOCIATED(nextBlock) )THEN
  nextBlock % PREV => Block % NEXT
 ENDIF

ELSE
 CALL ALLOCATE1_Block( Block , I , S , R , L , PREV=NULL(Block) , NEXT=NULL(Block) )
ENDIF
!
END SUBROUTINE




SUBROUTINE OVERWRITE0_Block(Block,I,S,R,L)
!PURPOSE
!! Modify the I, S, R, L components of Block with scalar
!! arguments, overwriting all components with present arguments
!! and deallocating ones without present arguments.
!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER    :: Block
INTEGER(KIND_I)             ,INTENT(IN) :: I
CHARACTER(*,KIND_S),OPTIONAL,INTENT(IN) :: S
REAL(KIND_R)       ,OPTIONAL,INTENT(IN) :: R
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(IN) :: L
!LOCALS
INTEGER :: ns,jerr
!
IF( ASSOCIATED(Block) )THEN

 !integer part
 IF( ASSOCIATED(Block%I) )DEALLOCATE( Block%I , STAT=jerr )
 ALLOCATE(Block%I(1))
 Block%I = I

 !string part
 IF( ASSOCIATED(Block%S) )DEALLOCATE( Block%S , STAT=jerr )
 IF( PRESENT(S) )THEN
  ns = LEN_TRIM(S)/LEN_S + 1
  ALLOCATE( Block%S(1:ns) )
  CALL CLEAR( Block % S )
  Block%S = S
 ENDIF

 !real part
 IF( ASSOCIATED(Block%R) )DEALLOCATE( Block%R , STAT=jerr )
 IF( PRESENT(R) )THEN
  ALLOCATE(Block%R(1))
  Block%R = R
 ENDIF

 !logical part
 IF( ASSOCIATED(Block%L) )DEALLOCATE( Block%L , STAT=jerr )
 IF( PRESENT(L) )THEN
  ALLOCATE(Block%L(1))
  Block%L = L
 ENDIF

ELSE
 CALL ALLOCATE0_Block( Block , I , S , R , L , PREV=NULL(Block) , NEXT=NULL(Block) )
ENDIF
!
END SUBROUTINE

SUBROUTINE OVERWRITE1_Block(Block,I,S,R,L)
!PURPOSE
!! Modify the I, S, R, L components of Block with array
!! arguments, overwriting all components with present arguments
!! and deallocating ones without present arguments.
!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER    :: Block
INTEGER(KIND_I)    ,OPTIONAL,INTENT(IN) :: I(:)
CHARACTER(*,KIND_S),OPTIONAL,INTENT(IN) :: S
REAL(KIND_R)       ,OPTIONAL,INTENT(IN) :: R(:)
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(IN) :: L(:)
!LOCALS
INTEGER :: ns
!
IF( ASSOCIATED(Block) )THEN

 !integer part
 IF( ASSOCIATED(Block%I) )DEALLOCATE( Block%I )
 ALLOCATE(Block%I(1:SIZE(I)))
 Block%I = I

 !string part
 IF( ASSOCIATED(Block%S) )DEALLOCATE( Block%S )
 IF( PRESENT(S) )THEN
  Block%S => ptr_BlockConvert( S )
 ENDIF

 !real part
 IF( ASSOCIATED(Block%R) )DEALLOCATE( Block%R )
 IF( PRESENT(R) )THEN
  ALLOCATE(Block%R(1:SIZE(R)))
  Block%R = R
 ENDIF

 !logical part
 IF( ASSOCIATED(Block%L) )DEALLOCATE( Block%L )
 IF( PRESENT(L) )THEN
  ALLOCATE(Block%L(1:SIZE(L)))
  Block%L = L
 ENDIF

ELSE
 CALL ALLOCATE1_Block( Block , I , S , R , L , PREV=NULL(Block) , NEXT=NULL(Block) )
ENDIF

!
END SUBROUTINE



SUBROUTINE DEALLOCATE_Block(Block)
!PURPOSE
!! Deallocate a Block and connect pointers around it.
!ARGUMENTS
TYPE(TYPE_Block),POINTER :: Block
!LOCALS
TYPE(TYPE_Block),POINTER :: ptr_Block
INTEGER(KIND_I)          :: jerr
!
ptr_Block => Block
IF( ASSOCIATED(Block%PREV) ) Block%PREV%NEXT => ptr_Block%NEXT
IF( ASSOCIATED(Block%NEXT) ) Block%NEXT%PREV => ptr_Block%PREV
Block => Block%PREV
IF( ASSOCIATED(ptr_Block%I) )DEALLOCATE( ptr_Block%I , STAT=jerr )
IF( ASSOCIATED(ptr_Block%S) )DEALLOCATE( ptr_Block%S , STAT=jerr )
IF( ASSOCIATED(ptr_Block%R) )DEALLOCATE( ptr_Block%R , STAT=jerr )
IF( ASSOCIATED(ptr_Block%L) )DEALLOCATE( ptr_Block%L , STAT=jerr )
ptr_Block % PREV => NULL()
ptr_Block % NEXT => NULL()
DEALLOCATE( ptr_Block , STAT=jerr)
!
END SUBROUTINE


SUBROUTINE NULLIFY_Block(Block)
!PURPOSE
!! Nullifies a Block and connect pointers around it.
!ARGUMENTS
TYPE(TYPE_Block),POINTER :: Block
!LOCALS
TYPE(TYPE_Block),POINTER :: ptr_Block
INTEGER(KIND_I)          :: jerr
!
ptr_Block => Block
IF( ASSOCIATED(Block%PREV) ) Block%PREV%NEXT => ptr_Block%NEXT
IF( ASSOCIATED(Block%NEXT) ) Block%NEXT%PREV => ptr_Block%PREV
Block => Block%PREV
NULLIFY( ptr_Block%I )
NULLIFY( ptr_Block%S )
NULLIFY( ptr_Block%R )
NULLIFY( ptr_Block%L )
ptr_Block % PREV => NULL()
ptr_Block % NEXT => NULL()
DEALLOCATE( ptr_Block , STAT=jerr )
!
END SUBROUTINE


SUBROUTINE RELEASE0_Block(Block,I,S,R,L)
!PURPOSE
!! Releases the information in a single Block to the available
!! variables in I, S, R, L, then deallocates the block.
!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER     :: Block
INTEGER(KIND_I)             ,INTENT(OUT) :: I
CHARACTER(*,KIND_S),OPTIONAL,INTENT(OUT) :: S
REAL(KIND_R)       ,OPTIONAL,INTENT(OUT) :: R
LOGICAL(KIND_L)    ,OPTIONAL,INTENT(OUT) :: L
!local
CHARACTER(LEN_S,KIND_S),POINTER :: S_(:)
!
!integer part
IF( ASSOCIATED(Block%I) )THEN
 I = Block%I(1)
ELSE
 I = -HUGE(I)
ENDIF

!string part
IF( PRESENT(S) )THEN
 IF( ASSOCIATED(Block%S) )THEN
  S_ => Block%S
  S = Sentence( S_ , delim="" )
  S_ => NULL()
 ELSE
  CALL CLEAR(S)
 ENDIF
ENDIF

!real part
IF( PRESENT(R) )THEN
 IF( ASSOCIATED(Block%R) )THEN
  R = Block%R(1)
 ELSE
  R = REAL(0,KIND_R)
 ENDIF
ENDIF

!logical part
IF( PRESENT(L) )THEN
 IF( ASSOCIATED(Block%L) )THEN
  L = Block%L(1)
 ELSE
  L = .FALSE._KIND_L
 ENDIF
ENDIF

!deallocate the block
CALL DEALLOCATE( Block )

!
END SUBROUTINE


SUBROUTINE RELEASE1_Block(Block,I,S,R,L)
!!#### PURPOSE
!! Releases the information in a single Block to the available
!! pointer variables in I, S, R, L, then nullify the block.
!ARGUMENTS
TYPE(TYPE_Block)            ,POINTER :: Block
INTEGER(KIND_I)    ,OPTIONAL,POINTER :: I(:)
CHARACTER(*,KIND_S),OPTIONAL,POINTER :: S(:)
REAL(KIND_R)       ,OPTIONAL,POINTER :: R(:)
LOGICAL(KIND_L)    ,OPTIONAL,POINTER :: L(:)
!
!integer part
IF( PRESENT(I) ) I => Block%I

!string part
IF( PRESENT(S) ) S => Block%S

!real part
IF( PRESENT(R) ) R => Block%R

!logical part
IF( PRESENT(L) ) L => Block%L

!deallocate the block
CALL NULLIFY( Block )

!
END SUBROUTINE



END MODULE
