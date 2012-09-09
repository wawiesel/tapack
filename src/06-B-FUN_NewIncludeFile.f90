!!# FUNCTION MODULE <<NewIncludeFile>>
MODULE FUN_NewIncludeFile
!!## PURPOSE
!! Check a string for an include statement and if one
!! is found, return the connected unit for that file contained
!! in the include statement.


!!## USAGE
!
!    Unit = NewIncludeFile( line [, allowed , file ] )
!
!! where
!! * required input <line> is the string of characters to check,
!! * optional input <allowed> is an array of <varying_strings> which specify
!!   the allowed include statements,
!! * optional output <file> is the name of the file found in
!!   the include statement,
!! * and the return value <Unit> is a previously unused unit
!!   you may use to read from the included file.
!!
!! <Unit> is returned as 0 if no include was found or -1 if an
!! include was found but the specified file didn't exist.


!!## DETAILS
!! The default values for <allowed> are for the standard FORTRAN
!! include statement: <INCLUDE '*'> and <INCLUDE "*"> where <*>
!! is the file name, and the include statement for LaTeX: <\include{*}>.
!! Note the form of the command is all the characters required, plus
!! a <*> where the filename would be placed.


!!## AUTHOR
!! William Wieselquist | william.wieselquist@gmail.com


!!## FORTRAN STANDARDS MODULES
USE ISO_varying_string              !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL PROCEDURES
USE FUN_NewFile                     !!((05-B-FUN_NewFile.f90))
USE FUN_INDEX2                      !!((04-B-FUN_INDEX2.f90))
USE FUN_EXIST                       !!((04-B-FUN_EXIST.f90))
USE FUN_EQUILOC                     !!((03-A-FUN_EQUILOC.f90))
USE SUB_TranslatePath               !!((05-C-SUB_TranslatePath.f90))

!!## DEFAULT IMPLICIT
implicit none

!!## DEFAULT ACCESS
private

!!## PUBLIC ACCESS LIST
public :: NewIncludeFile


!!## CONTAINED PROCEDURES
contains


!!### FUNCTION <<NewIncludeFile>>
FUNCTION NewIncludeFile( line , allowed , file , path ) RESULT(Unit)
!!#### PURPOSE
!! Check a line for a file inclusion and return one
!! of three values as <Unit>:
!!  * <Unit>$= 0$ if the line didn't contain an include statement
!!  * <Unit>$=-1$ if the line contained an include statement but the file
!!    doesn't exist
!!  * some free unit number, <Unit>$>0$, of the opened file
!!    if the line contained a valid INCLUDE statement

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN)  :: line

!!#### REQUIRED OUTPUT
INTEGER :: Unit

!!#### OPTIONAL INPUT
!! * allowed character sequences which force include.
TYPE(varying_string),OPTIONAL,INTENT(IN) :: allowed(:)
CHARACTER(LEN=*,KIND=KIND_S),OPTIONAL,INTENT(IN) :: path

!!#### OPTIONAL OUTPUT
TYPE(varying_string),OPTIONAL,INTENT(OUT) :: file

!!#### LOCAL PARAMETERS
!! * default include sequences
CHARACTER(11),PARAMETER :: DEFAULT_allowed(1:3)=&
  (/'INCLUDE "*"' , &
    "INCLUDE '*'" , &
    "\include{*}"  /)

!!#### LOCAL VARIABLES
INTEGER              :: found_index
TYPE(varying_string) :: allowed_
TYPE(varying_string) :: VS,VS_dummy
INTEGER              :: i,j,N,pivot

!!--begin--
!! Determine the number of allowed sequences for the <INCLUDE>
!! command.
IF( PRESENT(allowed) )THEN
 N = SIZE(allowed)
ELSE
 N = 3
END IF


!! Set the line.
VS = ADJUSTL(line)

!! Attempt to find included files using the various possible
!! types of include statements.
DO i=1,N


 !! Get the right value of the command to check.
 IF( PRESENT(allowed) )THEN
  allowed_ = allowed(i)
 ELSE
  allowed_ = TRIM(DEFAULT_allowed(i))
 END IF

 pivot = INDEX( char(allowed_) , "*" )

 VS_dummy = Extract(allowed_,1,pivot-1)

 !! Check for the allowed sequence in the line.
 found_index = INDEX2( char(VS) , char(VS_dummy) , CASESEN=.FALSE.)

 IF( found_index==1 )EXIT

END DO

!! Return value for the unit and possible optional file.
IF( found_index/=1 )THEN

 Unit = 0

 !! Output an empty string as the file name.
 IF( PRESENT(file) )THEN
  file = ""
 END IF

ELSE

 !! Strip all characters but the file name.
 VS = Replace( VS , Extract(allowed_,1      ,pivot-1 ) , "" )
 VS = Replace( VS , Extract(allowed_,pivot+1,LEN(VS)) , "" )

 !! Trim the filename.
 VS = TRIM(ADJUSTL(VS))
 IF( PRESENT(Path) )THEN
  VS = Path//VS
 END IF

 !! At this point we have a fully specified file and path.
 !! We need to make sure we are using the file system
 !! separator: fsSeparator, so we convert one to the other and
 !! vice versa.
 CALL TranslatePath(VS)

 !! Output the file name.
 IF( PRESENT(file) )THEN
  file = VS
 END IF

 !! Inquire if the file exist.
 IF( EXIST(VS) )THEN

  !! Output a unit number attached to the file if it exists.
  Unit = NewFile(char(VS),STATUS="OLD")

 !! Output a negative unit number to indicate that the file
 !! does not exist.
 ELSE

  Unit = -1

 END IF

END IF

!!--end--
END FUNCTION

end module
