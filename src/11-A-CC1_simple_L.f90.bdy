!#REQUIRED INPUT/OUTPUT
! @ input/output object [SIO]
! @ logical ON or OFF option [Switch]
TYPE(TYPE_SIO) ,POINTER       :: SIO
LOGICAL(KIND_L),INTENT(INOUT) :: switch

!#OPTIONAL INPUT/OUTPUT
! @ feedback object [FDBK]
TYPE(TYPE_FDBK),OPTIONAL,INTENT(INOUT) :: FDBK

!#LOCAL VARIABLES
CHARACTER(8) :: switch_

!#BEGIN
!write resolution
IF( TRIM(SIO%ACTION) .sEQ. "Write" )THEN
 IF( switch )THEN
  switch_ = "ON"
 ELSE
  switch_ = "OFF"
 ENDIF
ENDIF

!command setting
CALL BEGIN_ARGUMENTS(SIO,(/"switch"/),FdBk)
CALL ARGUMENT(SIO,switch_,FDBK)
CALL END_ARGUMENTS(SIO,FdBk)

!read resolution
IF( Reading(SIO) )THEN
 IF( TRIM(switch_) .sEQ. "ON" )THEN
  switch = .TRUE._KIND_L
 ELSEIF( TRIM(switch_) .sEQ. "T" )THEN
  switch = .TRUE._KIND_L
 ELSEIF( TRIM(switch_) .sEQ. ".TRUE." )THEN
  switch = .TRUE._KIND_L
 ELSE
  switch = .FALSE._KIND_L
 ENDIF
ENDIF