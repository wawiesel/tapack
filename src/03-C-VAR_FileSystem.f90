!!# MODULE <<VAR_FileSystem>>
MODULE VAR_FileSystem

!!## PURPOSE
!! Provides file system values, initialized to <DEFAULT>
!! values in <PAR_FileSystem>.



!!## DETAILS
!! The following values are provided by this
!! module:
!!
!! * <fsSeparator> : the current separator for paths.
!! * <fsInput    > : the current directory for input files.
!! * <fsOutput   > : the current directory for output files.
!! * <fsData     > : the current directory for data.
!! * <fsTemp     > : the current directory for temp files.



!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Sfile !!((01-A-KND_IntrinsicTypes.f90))



!!## EXTERNAL PARAMETER CONSTANTS
USE PAR_FileSystem                      !!((02-C-PAR_FileSystem.f90))



!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE



!!## GLOBAL VARIABLES
!! * directory separator
CHARACTER(LEN=1,KIND=KIND_Sfile) :: fsSeparator = DEFAULT_fsSeparator
!! * default directories
CHARACTER(LEN=LEN_FileSystem,KIND=KIND_Sfile) :: fsInput     = DEFAULT_fsInput
CHARACTER(LEN=LEN_FileSystem,KIND=KIND_Sfile) :: fsOutput    = DEFAULT_fsOutput
CHARACTER(LEN=LEN_FileSystem,KIND=KIND_Sfile) :: fsData      = DEFAULT_fsData
CHARACTER(LEN=LEN_FileSystem,KIND=KIND_Sfile) :: fsTemp      = DEFAULT_fsTemp


!!## PUBLIC ACCESS LIST
PUBLIC :: fsSeparator,fsInput,fsOutput,fsData,fsTemp


END MODULE
