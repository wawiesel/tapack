!!# SUBROUTINE <<SUB_IncludeFiles>>
MODULE SUB_IncludeFiles

!!## PURPOSE
!! Output to <outunit> the file from <inpunit> with
!! all 'included' files actually included in the output.

!!## DEPENDENCIES
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_NewIncludeFile !!((06-B-FUN_NewIncludeFile.f90))
USE ISO_varying_string !!((03-A-ISO_varying_string.f90))
USE FUN_STR            !!((05-B-FUN_STR.f90))
USE PAR_Units          !!((02-A-PAR_Units.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## PUBLIC ACCESS LIST
PUBLIC :: IncludeFiles


!!## CONTAINED PROCEDURES
CONTAINS


!!### DRIVER SUBROUTINE
SUBROUTINE IncludeFiles(inpunit,outunit,allowed,path)
!!#### REQUIRED INPUT
!! * unit to read from, <inpunit>
!! * unit to write to, <outunit>
INTEGER,INTENT(IN) :: inpunit
INTEGER,INTENT(IN) :: outunit

!!#### OPTIONAL INPUT
!! * allowed character sequences which force include.
TYPE(varying_string)        ,OPTIONAL,INTENT(IN) :: allowed(:)
CHARACTER(LEN=*,KIND=KIND_S),OPTIONAL,INTENT(IN) :: path

!!--begin--

CALL IncludeFiles_(inpunit,outunit)

!!--end--

CONTAINS

!!### RECURSIVE SUBROUTINE  <<IncludeFiles_>>
RECURSIVE SUBROUTINE IncludeFiles_(inpunit,outunit)
!!#### PURPOSE
!! Include files, recursively.

!!#### REQUIRED INPUT
!! * unit to read from, <inpunit>
!! * unit to write to, <outunit>
INTEGER,INTENT(IN) :: inpunit
INTEGER,INTENT(IN) :: outunit

!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
INTEGER :: iostat,inpunit_,count

!!--begin--

!! Initialize:
iostat = 0
count  = 0

!! Loop:
DO
 count = count + 1
 !! Get a brand new line.
 CALL get(inpunit,VS,iostat=iostat)
 IF( iostat==END_OF_FILE )EXIT

 !! Check for an included file and perhaps get the unit
 !! to it is opened as.
 inpunit_ = NewIncludeFile( line=TRIM(STR(VS)) , path=path , allowed=allowed )

 !! Enter the included files.
 IF( inpunit_>0 )THEN
  CALL IncludeFiles_(inpunit_,outunit)

 !! Write out a line.
 ELSE IF( inpunit_<0 )THEN
  WRITE(outunit,*)"--------------------------------------------------------------"
  WRITE(outunit,*)"include statement <"//TRIM(STR(VS))//"> could not be resolved."
  WRITE(outunit,*)"--------------------------------------------------------------"

 ELSE
  CALL put_line(outunit,VS,iostat)
 END IF

END DO

CLOSE(inpunit)

!!--end--
END SUBROUTINE

END SUBROUTINE

END MODULE
