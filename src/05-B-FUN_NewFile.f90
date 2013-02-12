!!### MODULE: FUNCTION  NewFile
MODULE FUN_NewFile
!!#### PURPOSE
!! This function opens a <FILE> by name provided and returns the
!! <UNIT> handle connected to that file.


!!#### AUTHOR
!! William Wieselquist | william.wieselquist@gmail.com

!!#### FORTRAN STANDARDS
USE ISO_varying_string              !!((03-A-ISO_varying_string.f90))

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_NewUnit                     !!((04-B-FUN_NewUnit.f90))
USE FUN_Default                     !!((04-A-FUN_Default.f90))

!!#### DEFAULT IMPLICIT
implicit none

!!#### DEFAULT ACCESS
private

INTERFACE NewFile
 MODULE PROCEDURE NewFile_S
 MODULE PROCEDURE NewFile_VS
END INTERFACE

!!#### PUBLIC ACCESS LIST
public :: NewFile


contains


!!### FUNCTION: NewFile_S
function NewFile_S( FILE , &
  STATUS , POSITION , IOSTAT , IfOpened , STATUS2 )  RESULT(Unit)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: FILE

!!#### OPTIONAL INPUT
!! @ Status to open the file with.
!! @ Position to open the file with.
!! @ What to do first if the file is already opened. ("R"=return,
!!   "C"=close) are the only options available right now.
!!   You may include them both (IfOpened="CR" to close the file
!!   and return, or just use "C" or "R" to close, or return
!!   respectively.
!! @ What to do after the first Status attempt fails and
!!   after the <IfOpened> condition.
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: STATUS
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: POSITION
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: IfOpened
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: STATUS2

!!#### OPTIONAL OUTPUT
INTEGER,INTENT(OUT),OPTIONAL :: IOSTAT

!!#### REQUIRED OUTPUT
INTEGER :: Unit

!!#### LOCAL VARIABLES
INTEGER :: jerr
LOGICAL :: Opened,TRY_STATUS2
CHARACTER(10) :: POSITION_,STATUS_

!!--begin--
POSITION_=''
STATUS_=''
POSITION_ = DEFAULT("AsIs      ",POSITION)
STATUS_   = DEFAULT("Unknown   ",STATUS)

!! Handle the special <IfOpened> optional.
!! If the <FILE> passed is found to be open, then
!!  @ a search is performed on the passed string, <IfOpened>.
!!  @ if it contains the character "C" then the unit is closed.
!!  @ if it contains the character "R" then the <RETURN> statement
!!    is executed without continuing.
IF( PRESENT(IfOpened) )THEN
 Unit=0
 INQUIRE(FILE=File,OPENED=Opened,NUMBER=Unit,IOSTAT=jerr)

 IF( Opened )THEN

  !Close case.
  IF( SCAN(IfOpened,"cC")/=0 )THEN
   CLOSE(Unit)
   Unit = 0
  END IF

  !Return case.
  IF( SCAN(IfOpened,"rR")/=0 )THEN
   RETURN
  END IF

 END IF
END IF

!! Get an unused unit.
Unit = NewUnit()


!! Don't try with STATUS2 the first time.
TRY_STATUS2 = .FALSE.

!! Come back here to try with STATUS2.
33 CONTINUE

!! Try to open the file with the passed STATUS.
IF( TRY_STATUS2 )THEN
 OPEN(Unit,FILE=FILE,STATUS=STATUS2,POSITION=POSITION_,IOSTAT=jerr)
ELSE
 OPEN(Unit,FILE=FILE,STATUS=STATUS_,POSITION=POSITION_,IOSTAT=jerr)
END IF

!! If the <IOSTAT> variable is not zero, then
!! we have an error or a warning.
IF( jerr/=0 )THEN

 !! Close the unit.
 CLOSE(Unit)

 !! In the case of an error return the error.
 IF( jerr<0 )THEN
  Unit = jerr

 !! In the case of a warning just return 0.
 ELSE

  !try the second passed status if we haven't already tried it
  IF( .NOT.TRY_STATUS2 .AND. PRESENT(STATUS2) )THEN
   TRY_STATUS2 = .TRUE.
   GOTO 33
  ELSE
   Unit = 0
  END IF

 END IF
END IF

!! Return actual value of <IOSTAT> if needed.
IF( PRESENT(IOSTAT) )THEN
 IOSTAT = jerr
END IF

!!--end--
end function NewFile_S


!!### FUNCTION: NewFile_VS
function NewFile_VS( FILE , &
  STATUS , POSITION , IOSTAT , IfOpened , STATUS2 )  RESULT(Unit)

!!#### REQUIRED INPUT
TYPE(varying_string),INTENT(IN) :: FILE

!!#### OPTIONAL INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: STATUS
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: POSITION
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: IfOpened
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: STATUS2

!!#### OPTIONAL OUTPUT
INTEGER,INTENT(OUT),OPTIONAL :: IOSTAT

!!#### REQUIRED OUTPUT
INTEGER :: Unit

!!--begin--
Unit = NewFile_S( char(FILE) , STATUS , POSITION , IOSTAT , IfOpened , STATUS2 )

!!--end--
end function NewFile_VS


end module
