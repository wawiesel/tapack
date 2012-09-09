!!# SUBROUTINE MODULE: <DeleteFile>
MODULE SUB_DeleteFile

!!## PURPOSE
!! Delete a file.



!!## USAGE
!
!  CALL DeleteFile( File [, IOSTAT] )
!
!! where <File> is a filename and <IOSTAT> is an optional
!! status variable.



!!## EXTERNAL
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_OldFile                     !!((06-C-FUN_OldFile.f90))

!!## HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 99.2007
!         Contact  = william.wieselquist AT gmail.com



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ACCESS LIST
PUBLIC :: DeleteFile



!!## MODULE PROCEDURES
CONTAINS


!!### SUBROUTINE: <DeleteFile>
SUBROUTINE DeleteFile(File,IOSTAT)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: File

!!#### OPTIONAL OUTPUT
INTEGER,INTENT(OUT),OPTIONAL :: IOSTAT

!!#### LOCAL VARIABLES
INTEGER :: Unit,IOSTAT_

!!--begin--

Unit = OldFile(File,IOSTAT)

IF( Unit/=0 )THEN

 CLOSE( Unit , Status="Delete" , IOSTAT=IOSTAT_ )

END IF

IF( PRESENT(IOSTAT) )THEN
 IOSTAT = IOSTAT_
END IF

!!--end--
END SUBROUTINE


END MODULE
