!!# KINDS MODULE: <Characteristcs>
MODULE KND_Characteristics
!!## PURPOSE
!! Store kind <USR_Characteristics> module should use.


!!## EXTERNAL
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## LOCAL KINDS
INTEGER,PARAMETER :: KIND_qmc = KIND_Rdp

!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_qmc


END MODULE
