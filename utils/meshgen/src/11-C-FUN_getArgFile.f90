!!# MODULE >>FUN_getArgFile<<
MODULE FUN_getArgFile

!!## PURPOSE
!! Return a unit to a file given by the <I>th argument.

!!# USAGE
!
!   Unit = getArgFile( [I] [,NLINES] [,MSG] )
!
!! where <I> is the optional input argument number to
!! convert to a file name, <NLINES> is optional
!! output of the number of lines of the opened file, and
!! <MSG> is the optional line to prompt the user for a
!! file name if the argument <I> was not present.
!!
!! DEFAULTS for INPUT:
!! <I>   defaults to 1.
!! <MSG> defaults to ``Please enter the file now.''

!!## DEPENDENCIES
USE ISO_varying_string !!((03-A-ISO_varying_string.f90))
USE FUN_STR            !!((05-B-FUN_STR.f90))
USE FUN_NewFile        !!((05-B-FUN_NewFile.f90))
USE FUN_EXIST          !!((04-B-FUN_EXIST.f90))
USE PRN_Text           !!((07-B-PRN_Text.f90))
USE LIB_Prompts        !!((06-B-LIB_Prompts.f90))
USE FUN_COUNT_Lines    !!((07-B-FUN_COUNT_Lines.f90))
USE SUB_CLEAR          !!((04-A-SUB_CLEAR.f90))
USE FUN_Default        !!((04-A-FUN_Default.f90))
USE FUN_INDEX2         !!((04-B-FUN_INDEX2.f90))
USE VAR_FileSystem     !!((03-C-VAR_FileSystem.f90))
USE VAR_Units          !!((03-A-VAR_Units.f90))
USE SUB_TranslatePath  !!((05-C-SUB_TranslatePath.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## PUBLIC ACCESS LIST
PUBLIC :: getArgFile

!!## CONTAINED PROCEDURES
CONTAINS


!!### FUNCTION >>GetArgFile<<
FUNCTION GetArgFile(I,MSG,NLINES,NUMARGS,FILE,PATH,&
  DefaultToDefaultInput , DefaultToDefaultOutput ) RESULT(Unit)

!!#### OPTIONAL INPUT
!! * argument number
!! * message
INTEGER     ,INTENT(IN),OPTIONAL :: I
CHARACTER(*),INTENT(IN),OPTIONAL :: MSG

!!#### REQUIRED OUTPUT
!! * unit of opened file
INTEGER :: Unit

!!#### OPTIONAL OUTPUT
!! * number of lines in file
!! * number of arguments present
INTEGER,OPTIONAL,INTENT(OUT) :: NLINES
INTEGER,OPTIONAL,INTENT(OUT) :: NUMARGS
TYPE(varying_string),OPTIONAL,INTENT(OUT) :: FILE
TYPE(varying_string),OPTIONAL,INTENT(OUT) :: PATH

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: DefaultToDefaultInput
LOGICAL,OPTIONAL,INTENT(IN) :: DefaultToDefaultOutput

!!#### EXTERNAL
!shouldn't be external
!EXTERNAL :: IARGC,GETARG

!!#### LOCAL VARIABLES
INTEGER :: I_,NUMARGS_,k,IARGC
TYPE(varying_string) :: line
CHARACTER(100) :: filename

!!--begin--

I_ = Default( 1 , I )
numargs_ = IARGC()


!first see if we can auto open
IF( numargs_>0 .AND. ( I_<=numargs_ .AND. I_>=1 ) )THEN
 CALL CLEAR(filename)
 CALL GETARG( I_ , filename )
 line = filename
 IF( EXIST(STR(line)) )GOTO 333
END IF

!next see if we have a unit to default to
IF( PRESENT(DefaultToDefaultInput) )THEN
 IF( DefaultToDefaultInput )THEN
  Unit = DEFAULT_INPUT_UNIT
 END IF
 RETURN
END IF
IF( PRESENT(DefaultToDefaultOutput) )THEN
 IF( DefaultToDefaultOutput )THEN
  Unit = DEFAULT_OUTPUT_UNIT
 END IF
 RETURN
END IF

!otherwise enter a loop to get a file name
DO
 IF( PRESENT(MSG) )THEN
  CALL Print_Text(s=InfoPrompt//MSG)
 ELSE
  CALL Print_Text(s=InfoPrompt//"Please enter the file now.")
 END IF
 CALL UserPrompt(s="file=")
 CALL GET(line)
 CALL TranslatePath(line)
 IF( .NOT.EXIST(STR(line)) )THEN
  WRITE(*,"(a)")ErrorPrompt//"The file="//STR(line)//" does not exist."
 ELSE
  EXIT
 END IF
END DO

333 CONTINUE

!return file
IF( PRESENT(FILE) )THEN
 k = INDEX2(STR(Line),fsSeparator,back=.TRUE.)
 FILE = EXTRACT(Line,k+1,LEN(Line))
END IF

!return path
IF( PRESENT(PATH) )THEN
 k = INDEX2(STR(Line),fsSeparator,back=.TRUE.)
 PATH = EXTRACT(Line,1,k)
 IF( LEN_TRIM(path)==0 )THEN
  PATH="."//fsSeparator
 END IF
END IF

!get an unused unit number and open it
Unit = NewFile(STR(Line),STATUS="OLD")

!count lines
IF( PRESENT(NLINES) )THEN
 NLINES = COUNT_Lines(Unit)
 REWIND(Unit)
END IF

!return number of arguments
IF( PRESENT(NUMARGS) )THEN
 NUMARGS = NUMARGS_
END IF

!!--end--
END FUNCTION

END MODULE
