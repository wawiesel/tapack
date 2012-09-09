!!# MODULE <<FUN_COUNT_Lines>>
MODULE FUN_COUNT_Lines

!!## PURPOSE
!! The function defined herein reads a file and returns the
!! number of lines in it.  You may provide the UNIT number of
!! a single connected file or the UNIT numbers of
!! many files in an array.

!!## USAGE
!
!    N = COUNT_Lines( UNIT [, IOSTAT] )
!
!! where <N> is the number of lines in the file attached to <UNIT>, or
!! <N> is a list of the numbers of lines in the files attached to the
!! list of <UNIT>s.

!!## FORTRAN STANDARDS MODULES
USE ISO_varying_string              !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL PARAMETERS
USE PAR_Units                       !!((02-A-PAR_Units.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Default                     !!((04-A-FUN_Default.f90))
USE FUN_NewIncludeFile              !!((06-B-FUN_NewIncludeFile.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## LOCAL PARAMETERS
LOGICAL,PARAMETER :: DEFAULT_CountIncludeFiles=.FALSE.

!!## PROCEDURE OVERLOADING
INTERFACE COUNT_Lines
 MODULE PROCEDURE COUNT_Lines_0
 MODULE PROCEDURE COUNT_Lines_1
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: COUNT_Lines


!!## CONTAINED PROCEDURES
CONTAINS

!!### RECURSIVE FUNCTION: <COUNT_Lines_0>
RECURSIVE FUNCTION COUNT_Lines_0( UNIT , IOSTAT , CountIncludeFiles ) RESULT(N)

!!#### REQUIRED INPUT
!! @ the connected UNIT to check the number of lines remaining to
!!   end-of-file.
INTEGER,INTENT(IN) :: UNIT

!!#### OPTIONAL OUTPUT
!! @ the <IOSTAT> variable
!! @ whether to count "include" files
INTEGER,OPTIONAL,INTENT(OUT) :: IOSTAT
LOGICAL,OPTIONAL,INTENT(IN)  :: CountIncludeFiles

!!#### REQUIRED OUTPUT
!! @ the number of lines
INTEGER :: N

!!#### LOCAL VARIABLES
INTEGER :: UNIT_
LOGICAL :: CountIncludeFiles_
TYPE(varying_string) :: VS
INTEGER :: jerr

!!--begin--
CountIncludeFiles_ = DEFAULT(DEFAULT_CountIncludeFiles,CountIncludeFiles)

!! Reset counter.
N = 0
DO
 !! Get a line.
 CALL GET(UNIT=UNIT,String=VS,IOSTAT=jerr)

 !! Check for end-of-file.
 IF( jerr==END_OF_FILE )EXIT

 !! Check for an include file statement if the optional
 !! argument <CountIncludeFiles==.TRUE.>.
 IF( CountIncludeFiles_ )THEN
  UNIT_ = NewIncludeFile(line=char(VS))
 ELSE
  UNIT_ = 0
 END IF

 !! We have include file!
 IF( UNIT_>0 )THEN
  N = N + COUNT_Lines_0(UNIT=UNIT_,IOSTAT=jerr)
  CLOSE( UNIT_ )
 ELSE
  N = N + 1
 END IF
END DO

!! Return <IOSTAT>.
IF( PRESENT(IOSTAT) )THEN
 IOSTAT = jerr
END IF

!!--end--
END FUNCTION


!!### RECURSIVE FUNCTION: <COUNT_Lines_1>
RECURSIVE FUNCTION COUNT_Lines_1( UNITS , IOSTAT , CountIncludeFiles ) RESULT(N)

!!#### REQUIRED INPUT
!! @ the array of UNITS to check <UNITS>
INTEGER,INTENT(IN) :: UNITS(:)

!!#### OPTIONAL OUTPUT
!! @ the <IOSTAT> variable
INTEGER,OPTIONAL,INTENT(OUT) :: IOSTAT
LOGICAL,OPTIONAL,INTENT(IN)  :: CountIncludeFiles

!!#### REQUIRED OUTPUT
!! @ the number of lines in each file
INTEGER :: N(1:SIZE(UNITS))

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--
DO i=1,SIZE(UNITS)
 N(i) = COUNT_Lines_0( UNITS(i) , IOSTAT , CountIncludeFiles )
END DO

!!--end--
END FUNCTION


END MODULE
