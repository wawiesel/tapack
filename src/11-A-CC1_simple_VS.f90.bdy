!#REQUIRED INPUT/OUTPUT
! @ input/output object [SIO]
! @ integer value [val]
TYPE(TYPE_SIO)       ,POINTER       :: SIO
TYPE(varying_string),INTENT(INOUT) :: val

!#OPTIONAL INPUT/OUTPUT
! @ feedback object [FDBK]
TYPE(TYPE_FDBK),OPTIONAL,INTENT(INOUT) :: FDBK

!#BEGIN
CALL BEGIN_ARGUMENTS(sio,(/'val'/),FdBk)
CALL ARGUMENT(sio,val,FdBk)
CALL END_ARGUMENTS(sio,FdBk)
