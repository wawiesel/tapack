!!# MODULE: <USR_SimpleSet>
MODULE USR_SimpleSet

!!## PURPOSE
!! The scalar set <SimpleSet> is a simple pointer array of values
!! which are kept sorted so that duplicate values are
!! never added.  A user can <ADD> to the SET,
!! <REMOVE> from the SET and <FINALIZE> the SET.

!!## DETAILS
!! @ Items are added to the set in log_2(n) time,
!! @ removed from the set in log_2(n) time, and
!! @ values are found in log_2(n) time
!! @ the set is finalized in constant time.

!!## MODULES
!! @ function to return the index of a string in an array of strings
!! @ function to return the trimmed length (really trimmed SIZE) of an array of strings
!! @ reallocation routine
!! @ string clearing routine
USE FUN_INDEXa            !!((08-B-FUN_INDEXa.f90))
USE FUN_LEN_TRIMa         !!((03-A-FUN_LEN_TRIMa.f90))
USE SUB_Reallocate        !!((04-B-SUB_Reallocate.f90))
USE SUB_CLEAR             !!((04-A-SUB_CLEAR.f90))
USE FUN_SIZEa             !!((06-B-FUN_SIZEa.f90))
USE FUN_Error             !!((04-A-FUN_Error.f90))
USE FUN_EQUILOC           !!((03-A-FUN_EQUILOC.f90))
USE FUN_IntervalBisection !!((03-A-FUN_IntervalBisection.f90))
USE FUN_Default           !!((04-A-FUN_Default.f90))
USE VAR_Units             !!((03-A-VAR_Units.f90))
USE PRN_Table             !!((11-B-PRN_Table.f90))
USE FUN_STR               !!((05-B-FUN_STR.f90))
USE PAR_IntrinsicLengths  !!((02-A-PAR_IntrinsicLengths.f90))
USE KND_IntrinsicTypes    !!((01-A-KND_IntrinsicTypes.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### INTERFACES
INTERFACE ADD_TO_SET
 MODULE PROCEDURE ADD_TO_SET_S
 MODULE PROCEDURE ADD_TO_SET_I
 MODULE PROCEDURE ADD_TO_SET_Rdp
END INTERFACE

INTERFACE ADDMANY_TO_SET
 MODULE PROCEDURE ADDMANY_TO_SET_I
END INTERFACE

INTERFACE REMOVE_FROM_SET
 MODULE PROCEDURE REMOVE_FROM_SET_S
 MODULE PROCEDURE REMOVE_FROM_SET_I
 MODULE PROCEDURE REMOVE_FROM_SET_rdp
END INTERFACE

INTERFACE FINALIZE_SET
 MODULE PROCEDURE FINALIZE_SET_S
 MODULE PROCEDURE FINALIZE_SET_I
 MODULE PROCEDURE FINALIZE_SET_Rdp
END INTERFACE

INTERFACE PRINT_SET
 MODULE PROCEDURE PRINT_SET_S
 MODULE PROCEDURE PRINT_SET_I
 MODULE PROCEDURE PRINT_SET_Rdp
END INTERFACE

INTERFACE FIND_IN_SET
 MODULE PROCEDURE FIND_IN_SET_S
 MODULE PROCEDURE FIND_IN_SET_I
 MODULE PROCEDURE FIND_IN_SET_Rdp
END INTERFACE

!!### PUBLIC ACCESS SET
PUBLIC :: PRINT_SET
PUBLIC :: ADD_TO_SET
PUBLIC :: ADDMANY_TO_SET
PUBLIC :: REMOVE_FROM_SET
PUBLIC :: FINALIZE_SET
PUBLIC :: FIND_IN_SET

!!### PARAMETERS
INTEGER,PARAMETER :: DEFAULT_dN=5


!!## CONTAINED ROUTINES
CONTAINS

!!### SUBROUTINE: ADD_TO_SET_S
SUBROUTINE ADD_TO_SET_S( SET , Entry )
!!#### PURPOSE
!! Add an Entry to the SET.

!!#### REQUIRED OUTPUT
CHARACTER(*),POINTER :: SET(:)

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: l(1:2),N

!!--begin--
IF( .NOT.ASSOCIATED(SET) )THEN
 ALLOCATE( SET(1:DEFAULT_dN) )
 CALL CLEAR(SET)
 N = 0
 SET(1) = Entry

ELSE
 !get number of elements in set
 N = LEN_TRIMa(SET)

 !easy setup if N=0
 IF( N==0 )THEN
  SET(1) = Entry
  RETURN
 END IF

 !get interval
 l = IntervalBisection(Entry,SET(1:N))

 !first check if we have a duplicate value
 IF( l(1)/=0 )THEN
  IF( SET(l(1))==Entry )RETURN
 END IF

 !increment
 N = N + 1

 !reallocate set if full
 IF( N>=SIZE(SET) )THEN
  CALL Reallocate( SET , DEFAULT_dN , fill=REPEAT(" ",LEN(SET)))
 END IF

 !bump
 SET(l(2)+1:N) = SET(l(2):N-1)

 !set
 SET(l(2)) = Entry

END IF

!!--end--
END SUBROUTINE


!!### FUNCTION: FIND_IN_SET_S
FUNCTION FIND_IN_SET_S(SET,Entry) RESULT(I)
!!#### PURPOSE
!! Find an entry in the set.

!!#### REQUIRED INPUT
CHARACTER(*),POINTER    :: SET(:)
CHARACTER(*),INTENT(IN) :: Entry

!!#### REQUIRED OUTPUT
INTEGER :: I

!!#### LOCAL VARIABLES
INTEGER :: l(1:2),N

!!--begin--
I = 0

IF( ASSOCIATED(SET) )THEN

 N = LEN_TRIMa(SET)

 IF( N==0 )RETURN

 l = IntervalBisection(Entry,SET(1:N))

 IF( l(1)<1 )RETURN

 IF( SET(l(1))==Entry )THEN
  I = l(1)
 END IF

END IF

!!--end--
END FUNCTION


!!### SUBROUTINE: REMOVE_FROM_SET_S
SUBROUTINE REMOVE_FROM_SET_S(SET,Entry)
!!#### PURPOSE
!! Remove an entry from the SET.

!!#### REQUIRED OUTPUT
CHARACTER(*),POINTER    :: SET(:)

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: N,I

!!--begin--
IF( ASSOCIATED(SET) )THEN
 I = FIND_IN_SET(Set,Entry)

 IF( I==0 )RETURN

 N = SIZE(SET)

 SET(I:N-1) = SET(I+1:N)

 CALL Clear(SET(N))

END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE: PRINT_SET_S
SUBROUTINE PRINT_SET_S( SET , Unit )
!!#### PURPOSE
!! Print out the contents of a set.

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*),POINTER :: SET(:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: i,Unit_
CHARACTER(LEN(SET)+LEN_I),POINTER :: D(:,:)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

IF( .NOT.ASSOCIATED(SET) )THEN
 WRITE(Unit_,"(a)")" N U L L   S E T"
 RETURN
ELSE
 ALLOCATE( D(1+SIZE(SET),2) )
 CALL CLEAR(D)
 D(1,:) = (/"index","value"/)
 DO i=1,SIZE(SET)
  D(i+1,1) = TRIM(STR(i))
  D(i+1,2) = TRIM(SET(i))
 END DO
 CALL PRINT_Table(D,Unit=Unit_)
END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE: FINALIZE_SET_S
SUBROUTINE FINALIZE_SET_S( SET )
!!#### PURPOSE
!! Finalize the SET by allocation to its LEN_TRIMa length.

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*),POINTER :: SET(:)

!!#### LOCAL VARIABLES
INTEGER :: dn,N

!!--begin--
IF( ASSOCIATED(SET) )THEN
 N = LEN_TRIMa(SET)
 IF( N==0 )THEN
  DEALLOCATE(SET)
  NULLIFY(SET)
 ELSE
  dn=LEN_TRIMa(SET)-SIZE(SET)
  CALL Reallocate( SET , dn )
 END IF
ENDIF

!!--end--
END SUBROUTINE




!!### SUBROUTINE: ADD_TO_SET_I
SUBROUTINE ADD_TO_SET_I( SET , Entry )
!!#### PURPOSE
!! Add an Entry to the SET.

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: SET(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: l(1:2),N

!!--begin--

IF( .NOT.ASSOCIATED(SET) )THEN
 ALLOCATE( SET(1:DEFAULT_dN) )
 SET = ERROR(1)
 N = 0
 SET(1) = Entry

ELSE
 !get number of elements in set
 N = SIZEa(SET)

 !easy setup if N=0
 IF( N==0 )THEN
  SET(1) = Entry
  RETURN
 END IF

 !get interval
 l = IntervalBisection(Entry,SET(1:N))

 !first check if we have a duplicate value
 IF( l(1)/=0 )THEN
  IF( SET(l(1))==Entry )RETURN
 END IF

 !increment
 N = N + 1

 !reallocate set if full
 IF( N>=SIZE(SET) )THEN
  CALL Reallocate( SET , DEFAULT_dN , fill=ERROR(1) )
 END IF

 !bump
 SET(l(2)+1:N) = SET(l(2):N-1)

 !set
 SET(l(2)) = Entry

END IF

!!--end--
END SUBROUTINE



!!### SUBROUTINE: ADDMANY_TO_SET_I
SUBROUTINE ADDMANY_TO_SET_I( SET , Entries )
!!#### PURPOSE
!! Add many Entries to the SET.

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: SET(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Entries(:)

!!#### LOCAL VARIABLES
INTEGER :: I

!!--begin--

DO I=1,SIZE(Entries)
 CALL ADD_TO_SET( SET , Entries(I) )
END DO

!!--end--
END SUBROUTINE



!!### FUNCTION: FIND_IN_SET_I
FUNCTION FIND_IN_SET_I(SET,Entry) RESULT(I)
!!#### PURPOSE
!! Find an entry in the set.

!!#### REQUIRED INPUT
INTEGER,POINTER    :: SET(:)
INTEGER,INTENT(IN) :: Entry

!!#### REQUIRED OUTPUT
INTEGER :: I

!!#### LOCAL VARIABLES
INTEGER :: l(1:2),N

!!--begin--
I = 0

IF( ASSOCIATED(SET) )THEN

 N = SIZEa(SET)

 IF( N==0 )RETURN

 l = IntervalBisection(Entry,SET(1:N))

 IF( l(1)<1 )RETURN

 IF( SET(l(1))==Entry )THEN
  I = l(1)
 END IF

END IF

!!--end--
END FUNCTION


!!### SUBROUTINE: REMOVE_FROM_SET_I
SUBROUTINE REMOVE_FROM_SET_I(SET,Entry)
!!#### PURPOSE
!! Remove an entry from the SET.

!!#### REQUIRED OUTPUT
INTEGER,POINTER    :: SET(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: N,I

!!--begin--
IF( ASSOCIATED(SET) )THEN
 I = FIND_IN_SET(Set,Entry)

 IF( I==0 )RETURN

 N = SIZE(SET)

 SET(I:N-1) = SET(I+1:N)

 SET(N) = ERROR(1)

END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE: PRINT_SET_I
SUBROUTINE PRINT_SET_I( SET , Unit )
!!#### PURPOSE
!! Print out the contents of a set.

!!#### REQUIRED INPUT/OUTPUT
INTEGER,POINTER :: SET(:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: i,Unit_
CHARACTER(LEN_I),POINTER :: D(:,:)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

IF( .NOT.ASSOCIATED(SET) )THEN
 WRITE(Unit_,"(a)")" N U L L   S E T"
 RETURN
ELSE
 ALLOCATE( D(1+SIZE(SET),2) )
 CALL CLEAR(D)
 D(1,:) = (/"index","value"/)
 DO i=1,SIZE(SET)
  D(i+1,1) = TRIM(STR(i))
  D(i+1,2) = TRIM(STR(SET(i)))
 END DO
 CALL PRINT_Table(D,Unit=Unit_)
END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE: FINALIZE_SET_I
SUBROUTINE FINALIZE_SET_I( SET )
!!#### PURPOSE
!! Finalize the SET by allocation to its SIZEa length.

!!#### REQUIRED INPUT/OUTPUT
INTEGER,POINTER :: SET(:)

!!#### LOCAL VARIABLES
INTEGER :: dn,N

!!--begin--
IF( ASSOCIATED(SET) )THEN
 N = SIZEa(SET)
 IF( N==0 )THEN
  DEALLOCATE(SET)
  NULLIFY(SET)
 ELSE
  dn=SIZEa(SET)-SIZE(SET)
  CALL Reallocate( SET , dn )
 END IF
ENDIF

!!--end--
END SUBROUTINE





!!### SUBROUTINE: ADD_TO_SET_Rdp
SUBROUTINE ADD_TO_SET_Rdp( SET , Entry )
!!#### PURPOSE
!! Add an Entry to the SET.

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp),POINTER :: SET(:)

!!#### REQUIRED INPUT
REAL(KIND_Rdp),INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: l(1:2),N

!!--begin--
IF( .NOT.ASSOCIATED(SET) )THEN
 ALLOCATE( SET(1:DEFAULT_dN) )
 SET = ERROR(1._KIND_Rdp)
 N = 0
 SET(1) = Entry

ELSE
 !get number of elements in set
 N = SIZEa(SET)

 !easy setup if N=0
 IF( N==0 )THEN
  SET(1) = Entry
  RETURN
 END IF

 !get interval
 l = IntervalBisection(Entry,SET(1:N))

 !first check if we have a duplicate value
 IF( l(1)/=0 )THEN
  IF( SET(l(1))==Entry )RETURN
 END IF

 !increment
 N = N + 1

 !reallocate set if full
 IF( N>=SIZE(SET) )THEN
  CALL Reallocate( SET , DEFAULT_dN , fill=ERROR(1._KIND_Rdp) )
 END IF

 !bump
 SET(l(2)+1:N) = SET(l(2):N-1)

 !set
 SET(l(2)) = Entry

END IF

!!--end--
END SUBROUTINE


!!### FUNCTION: FIND_IN_SET_Rdp
FUNCTION FIND_IN_SET_Rdp(SET,Entry) RESULT(I)
!!#### PURPOSE
!! Find an entry in the set.

!!#### REQUIRED INPUT
REAL(KIND_Rdp),POINTER    :: SET(:)
REAL(KIND_Rdp),INTENT(IN) :: Entry

!!#### REQUIRED OUTPUT
INTEGER :: I

!!#### LOCAL VARIABLES
INTEGER :: l(1:2),N

!!--begin--
I = 0

IF( ASSOCIATED(SET) )THEN

 N = SIZEa(SET)

 IF( N==0 )RETURN

 l = IntervalBisection(Entry,SET(1:N))

 IF( l(1)<1 )RETURN

 IF( SET(l(1))==Entry )THEN
  I = l(1)
 END IF

END IF

!!--end--
END FUNCTION


!!### SUBROUTINE: REMOVE_FROM_SET_Rdp
SUBROUTINE REMOVE_FROM_SET_Rdp(SET,Entry)
!!#### PURPOSE
!! Remove an entry from the SET.

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp),POINTER    :: SET(:)

!!#### REQUIRED INPUT
REAL(KIND_Rdp),INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: N,I

!!--begin--
IF( ASSOCIATED(SET) )THEN
 I = FIND_IN_SET(Set,Entry)

 IF( I==0 )RETURN

 N = SIZE(SET)

 SET(I:N-1) = SET(I+1:N)

 SET(N) = ERROR(1._KIND_Rdp)

END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE: PRINT_SET_Rdp
SUBROUTINE PRINT_SET_Rdp( SET , Unit )
!!#### PURPOSE
!! Print out the contents of a set.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_Rdp),POINTER :: SET(:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: i,Unit_
CHARACTER(LEN_Rdp),POINTER :: D(:,:)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

IF( .NOT.ASSOCIATED(SET) )THEN
 WRITE(Unit_,"(a)")" N U L L   S E T"
 RETURN
ELSE
 ALLOCATE( D(1+SIZE(SET),2) )
 CALL CLEAR(D)
 D(1,:) = (/"index","value"/)
 DO i=1,SIZE(SET)
  D(i+1,1) = TRIM(STR(i))
  D(i+1,2) = TRIM(STR(SET(i)))
 END DO
 CALL PRINT_Table(D,Unit=Unit_)
END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE: FINALIZE_SET_Rdp
SUBROUTINE FINALIZE_SET_Rdp( SET )
!!#### PURPOSE
!! Finalize the SET by allocation to its SIZEa length.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_Rdp),POINTER :: SET(:)

!!#### LOCAL VARIABLES
INTEGER :: dn,N

!!--begin--
IF( ASSOCIATED(SET) )THEN
 N = SIZEa(SET)
 IF( N==0 )THEN
  DEALLOCATE(SET)
  NULLIFY(SET)
 ELSE
  dn=SIZEa(SET)-SIZE(SET)
  CALL Reallocate( SET , dn )
 END IF
ENDIF

!!--end--
END SUBROUTINE



END MODULE
