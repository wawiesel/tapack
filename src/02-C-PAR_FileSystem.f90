!!# MODULE <<PAR_FileSystem>>
MODULE PAR_FileSystem
!!## PURPOSE
!! Provides file system parameter constants.

!!## DETAILS
!! The following parameter constants are provided by this
!! module:
!!
!! * <DEFAULT_fsSeparator> : the default separator for paths.
!! * <DEFAULT_fsInput    > : the default directory for input files.
!! * <DEFAULT_fsOutput   > : the default directory for output files.
!! * <DEFAULT_fsData     > : the default directory for data.
!! * <DEFAULT_fsTemp     > : the default directory for temp files.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Sfile !!((01-A-KND_IntrinsicTypes.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE


!!## EXTERNAL PARAMETERS
!! * maximum length of file system variables
INTEGER,PARAMETER :: LEN_FileSystem = 128

!! * default separator for paths (/requires a C or fortran preprocessor/)
#ifdef __WINDOWS__
  CHARACTER(LEN=1,KIND=KIND_Sfile),PARAMETER :: DEFAULT_fsSeparator = "\"
#else
  CHARACTER(LEN=1,KIND=KIND_Sfile),PARAMETER :: DEFAULT_fsSeparator = "/"
#endif


!! * default directories
CHARACTER(LEN=5,KIND=KIND_Sfile),PARAMETER :: DEFAULT_fsInput     = "input"
CHARACTER(LEN=6,KIND=KIND_Sfile),PARAMETER :: DEFAULT_fsOutput    = "output"
CHARACTER(LEN=4,KIND=KIND_Sfile),PARAMETER :: DEFAULT_fsData      = "data"
CHARACTER(LEN=4,KIND=KIND_Sfile),PARAMETER :: DEFAULT_fsTemp      = "temp"


!!## PUBLIC ACCESS LIST
!! * kinds
PUBLIC :: KIND_Sfile
!! * parameters
PUBLIC :: LEN_FileSystem
!! * defaults
PUBLIC :: DEFAULT_fsSeparator
PUBLIC :: DEFAULT_fsInput
PUBLIC :: DEFAULT_fsOutput
PUBLIC :: DEFAULT_fsData
PUBLIC :: DEFAULT_fsTemp


END MODULE
