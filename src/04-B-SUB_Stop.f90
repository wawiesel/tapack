!!# SUBROUTINE MODULE <<SUB_Stop>>
MODULE SUB_Stop

!!## PURPOSE
!! Halts execution.



!!## USAGE
!! In it's simplest form,
!
!  CALL Stop()
!
!! the execution is stopped.
!!
!!### OPTIONAL INPUTS
!
!  CALL Stop(S,OutputUnit)
!
!! @ print string <S> before stopping
!! @ change the <OutputUnit>



!!## DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: KIND_S     !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_Units,ONLY: DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))



!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ACCESS LIST
PUBLIC :: Stop



!!## MODULE PROCEDURES
CONTAINS



!!### SUBROUTINE <<Stop>>
SUBROUTINE Stop(S,OutputUnit)

!!#### OPTIONAL INPUT
CHARACTER(LEN=*,KIND=KIND_S),OPTIONAL,INTENT(IN) :: S
INTEGER                     ,OPTIONAL,INTENT(IN) :: OutputUnit

!!#### LOCAL VARIABLES
INTEGER :: OutputUnit_

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

!! Actual stop.
STOP

!!--end--
END SUBROUTINE



END MODULE
