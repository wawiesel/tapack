MODULE USR_SMlib_Misc
  !
  !!  This module is part of SMLIB v. 1.1.  It contains some handy routines.
  !
  !! ------------------------------------------------------------------------
  !
  !!  Copyright (C) 1996 Ernst A. Meese
  !!    Refer to the file copyright.doc for details and important disclaimer.
  !
  !!  Created: January 1996
  !!  Development version
  !
  !! ------------------------------------------------------------------------
  !!  Module contents
  !
  !!   Procedures:
  !!     TimeStamp
  !!     MachineStamp
  !!     PrecisionStamp
  !!     upcase
  !!     downcase
  !
  !! -----------------------------------------------------------------------
  !
  use USR_SMlib_Precision !!((16-B-USR_SMlib_Precision.f90))

  IMPLICIT NONE

CONTAINS
  FUNCTION TimeStamp ()
    !
    !!  Return a string of length 17 with formatted (yy/mm/dd, hh:mm) date and time
    !
    CHARACTER(LEN=17) :: timestamp
    CHARACTER(LEN=10) :: date, time

    CALL DATE_AND_TIME (date, time)
    timestamp = date(1:4) // "/" // date(5:6) // "/" // date(7:8) // ", " &
         & // time(1:2) // ":" // time(3:4)

  END FUNCTION TimeStamp

  !FUNCTION MachineStamp ()
    !
    !!  Return a string identifying the machine and operating system using the UNIX
    !!  command uname.
    !
  !!  CHARACTER (80) :: MachineStamp
  !!  EXTERNAL uname

  !!  CALL eamuname (MachineStamp, 80)

  !END FUNCTION MachineStamp

  FUNCTION PrecisionStamp ()
    !
    !!  Return a string identifying the working real precision of the library
    !
    CHARACTER (80) :: PrecisionStamp

    SELECT CASE (prec)
    CASE (single_prec)
       WRITE (PrecisionStamp,*) "Single precision; Precision=", PRECISION (1.0_prec), ", &
            & Max. exponent=", MAXEXPONENT (1.0_prec), "."
    CASE (double_prec)
       WRITE (PrecisionStamp,*) "Double precision; Precision=", PRECISION (1.0_prec), ", &
            & Max. exponent=", MAXEXPONENT (1.0_prec), "."
    CASE DEFAULT
       WRITE (PrecisionStamp,*) "Auxilary precision; Precision=", PRECISION (1.0_prec), ", &
            & Max. exponent=", MAXEXPONENT (1.0_prec), "."
    END SELECT

  END FUNCTION PrecisionStamp

  SUBROUTINE upcase ( string )
    !
    !! Change lowercase letters to uppercase in string
    !
    CHARACTER(*) :: string
    INTEGER      :: i, upper_to_lower, len_string

    upper_to_lower = IACHAR("a") - IACHAR("A")
    len_string = LEN_TRIM(string)
    DO i = 1, len_string
       SELECT CASE (string(i:i))
       CASE ("a":"z")
          string(i:i) = ACHAR(IACHAR(string(i:i)) - upper_to_lower)
       END SELECT
    END DO

  END SUBROUTINE upcase

  SUBROUTINE downcase ( string )
    !
    !! Change uppercase letters to lowercase in string
    !
    CHARACTER (*) :: string
    INTEGER       :: i, upper_to_lower, len_string

    upper_to_lower = IACHAR("a") - IACHAR("A")
    len_string = LEN_TRIM(string)
    DO i = 1, len_string
       SELECT CASE (string(i:i))
       CASE ("A":"Z")            !! Upper case character found:
          string(i:i) = ACHAR(IACHAR(string(i:i)) + upper_to_lower)
       END SELECT
    END DO

  END SUBROUTINE downcase

END MODULE USR_SMlib_Misc
