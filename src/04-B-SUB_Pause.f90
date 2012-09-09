!!# SUBROUTINE MODULE <<SUB_Pause>>
MODULE SUB_Pause

!!## PURPOSE
!! Provide a simple way to ``pause'' execution.



!!## USAGE
!! In it's simplest form,
!
!  CALL Pause()
!
!! we just wait for the user to press <ENTER>.
!!
!!### OPTIONAL INPUTS
!
!  CALL Pause(S,dt,InputUnit,OutputUnit)
!
!! @ print string <S> before pausing <S>
!! @ pause for <dt> seconds
!! @ change the <InputUnit>
!! @ change the <OutputUnit>



!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S                        !!((01-A-KND_IntrinsicTypes.f90))



!!## GLOBAL VARIABLES
USE VAR_Units,ONLY: DEFAULT_INPUT_UNIT,DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## PUBLIC ACCESS LIST
PUBLIC :: Pause



!!## MODULE PROCEDURES
CONTAINS



!!### SUBROUTINE <<Pause>>
SUBROUTINE Pause(S,dt,InputUnit,OutputUnit)

!!#### OPTIONAL INPUT
CHARACTER(LEN=*,KIND=KIND_S),OPTIONAL,INTENT(IN) :: S
REAL                        ,OPTIONAL,INTENT(IN) :: dt
INTEGER                     ,OPTIONAL,INTENT(IN) :: InputUnit,OutputUnit

!!#### LOCAL VARIABLES
REAL    :: tin,tout
INTEGER :: OutputUnit_,InputUnit_

!!--begin--

IF( PRESENT(S) )THEN
 IF( PRESENT(OutputUnit) )THEN
  OutputUnit_ = OutputUnit
 ELSE
  OutputUnit_ = DEFAULT_OUTPUT_UNIT
 END IF
 !output string
 WRITE(OutputUnit_,"(a)")S
END IF

IF( PRESENT(dt) )THEN
 !timing loop
 CALL CPU_TIME(tin)
 DO
  CALL CPU_TIME(tout)
  IF( tout-tin>dt )EXIT
 END DO

ELSE
 IF( PRESENT(InputUnit) )THEN
  InputUnit_ = InputUnit
 ELSE
  InputUnit_ = DEFAULT_INPUT_UNIT
 END IF
 !wait for enter
 READ(InputUnit_,*)
ENDIF

!!--end--
END SUBROUTINE


END MODULE
