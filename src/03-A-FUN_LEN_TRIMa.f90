!!## MODULE: FUNCTION  LEN_TRIMa
MODULE FUN_LEN_TRIMa

!!### PURPOSE
!! The <LEN_TRIMa> function is somewhat of an extension of
!! the <LEN_TRIM> intrinsic function applied to an array
!! of strings.  It returns the number of elements in the
!! passed array which are not completely blanked out.



!!### USAGE
!
!    I = LEN_TRIMa( STRING_ARRAY )
!
!! where <I> is the number of non-blank elements in the
!! <STRING_ARRAY>.



!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))



!!### HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!### DEFAULT IMPLICIT
IMPLICIT NONE



!!### DEFAULT ACCESS
PRIVATE



!!### INTERFACES
INTERFACE LEN_TRIMa
 MODULE PROCEDURE LEN_TRIMa_A1S
END INTERFACE



!!### PUBLIC ACCESS LIST
PUBLIC :: LEN_TRIMa



!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: LEN_TRIMa_A1S
FUNCTION LEN_TRIMa_A1S( STRING_ARRAY ) RESULT(I)

!!#### REQUIRED INPUT
!! @ the array of strings to be checked <STRING_ARRAY>
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING_ARRAY(:)

!!#### REQUIRED OUTPUT
!! @ the "trimmed" length of STRING_ARRAY (0 if not found) <I>
INTEGER :: I

!!--begin--
DO I=SIZE(STRING_ARRAY),1,-1
 IF( LEN_TRIM(STRING_ARRAY(I))/=0 )EXIT
END DO

!!--end--
END FUNCTION

END MODULE
