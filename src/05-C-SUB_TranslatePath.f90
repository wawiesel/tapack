!!# MODULE <<SUB_TranslatePath>>
MODULE SUB_TranslatePath
!!## PURPOSE
!! Translate a path from the other file system separators
!! to the current file system separator, <fsSeparator> and strips
!! any double quotes from inside.


!!## AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com | 2006


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Sfile,KIND_S !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_FileSystem                             !!((03-C-VAR_FileSystem.f90))
USE ISO_varying_string                         !!((03-A-ISO_varying_string.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## IDENTIFICATION
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: file_ = "07-C-SUB_TranslatePath.f90"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: mod_  = "SUB_TranslatePath"

!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ROUTINES
PUBLIC :: TranslatePath


!!## MODULE PROCEDURES
CONTAINS

SUBROUTINE TranslatePath(VS)
TYPE(varying_string),INTENT(INOUT) :: VS
!!--begin--

!replace wrong separators
IF( fsSeparator=="\" )THEN
 VS = Replace(VS,"/","\",every=.true.)
ELSE IF( fsSeparator=="/" )THEN
 VS = Replace(VS,"\","/",every=.true.)
END IF

!replace quotes
VS = Replace(VS,'"','',every=.true.)

!!--end--
END SUBROUTINE

END MODULE
