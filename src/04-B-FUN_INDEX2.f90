!!## MODULE: FUNCTION  INDEX2
MODULE FUN_INDEX2
!!### PURPOSE
!! Provides an overloading of the intrinsic
!! function <INDEX> to allow for case (in)sensitive
!! indexing.


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!### EXTERNAL PROCEDURES
USE FUN_Upcase                      !!((03-A-FUN_Upcase.f90))


!!### USAGE
!
!     I = INDEX2( STRING , SUBSTRING [, BACK , CASESEN ] )
!
!!  where I is the index of SUBSTRING in the STRING
!!  and BACK is just like the intrinsic INDEX"s BACK,
!!  CASESEN is whether or not the search is case-sensitive.
!


!!### METHOD
!! If CASESEN==.FALSE. then we use INDEX, if CASESEN==.TRUE. then we
!! just convert STRING and SUBSTRING to upper case and then use INDEX.
!! There is almost definitely room for improvement here---the case
!! conversion is somewhat expensive.


!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### LOCAL PARAMETERS
!! @ default case sensitivity
LOGICAL,PARAMETER :: DEFAULT_CASESEN = .TRUE.

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE INDEX2
 MODULE PROCEDURE INDEX2_
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: INDEX2


!!## MODULE PROCEDURES
CONTAINS

!!### PURE FUNCTION: INDEX2_
PURE FUNCTION INDEX2_( STRING , SUBSTRING  , BACK , CASESEN ) RESULT(I)
!!#### REQUIRED INPUT
!! @ the string to be checked <STRING>
!! @ the substring to look for <SUBSTRING>
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: SUBSTRING

!!#### OPTIONAL INPUT
!! @ whether to consider the search case-sensitive
!! @ whether to return the index from the back <BACK>
LOGICAL,OPTIONAL,INTENT(IN) :: BACK
LOGICAL,OPTIONAL,INTENT(IN) :: CASESEN

!!#### REQUIRED OUTPUT
!! @ the index in STRING_ARRAY (0 if not found) <I>
INTEGER :: I

!!#### LOCAL VARIABLES
LOGICAL :: CASESEN_

!!--begin--
!! Determine the local value for case-sensitive search.
IF( PRESENT(CASESEN) )THEN
 CASESEN_ = CASESEN
ELSE
 CASESEN_ = DEFAULT_CASESEN
END IF

!! If not case sensitive, use the upper case to convert.
IF( CASESEN_ )THEN
 I = INDEX( STRING , SUBSTRING , BACK )
ELSE
 I = INDEX( UpCase(STRING) , UpCase(SUBSTRING) , BACK )
END IF

!!--end--
END FUNCTION



END MODULE
