!!# FUNCTION MODULE: <OldFile>
MODULE FUN_OldFile

!!## PURPOSE
!! Return a unit corresponding to a file.



!!## USAGE
!
!  Unit = OldFile( File [, IOSTAT] )
!
!! where <File> is a filename and <IOSTAT> is an optional
!! status variable.



!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_NewFile                     !!((05-B-FUN_NewFile.f90))


!!## HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 99.2007
!         Contact  = william.wieselquist AT gmail.com



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ACCESS LIST
PUBLIC :: OldFile



!!## MODULE PROCEDURES
CONTAINS


!!### FUNCTION: <OldFile>
FUNCTION OldFile(File,IOSTAT) RESULT(Unit)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: File

!!#### REQUIRED OUTPUT
INTEGER :: Unit

!!#### OPTIONAL OUTPUT
INTEGER,INTENT(OUT),OPTIONAL :: IOSTAT

!!#### LOCAL VARIABLES
LOGICAL :: Opened
INTEGER :: IOSTAT_

!!--begin--
INQUIRE( File=File , Opened=Opened , IOSTAT=IOSTAT_ )

IF( .NOT.Opened )THEN
 Unit = NewFile(File)
ELSE
 INQUIRE( File=File , Number=Unit , IOSTAT=IOSTAT_ )
END IF

IF( PRESENT(IOSTAT) )THEN
 IOSTAT = IOSTAT_
END IF

!!--end--
END FUNCTION



END MODULE
