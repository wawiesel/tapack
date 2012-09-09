!!# SUBROUTINE MODULE: \<SUB_Find\>
MODULE SUB_Find

!!## PURPOSE
!! Find a target in a string and return a pointer array of the locations
!! that were found.


!!## USAGE
!
!       CALL Find( Target , String , Locs [,CASESEN] )
!
!!  where <Target> is the string to look for, <String> is the
!!  string to look in, <Locs> are the beginning locations of <Target>
!!  in <String>, and <CASESEN> is the optional case-sensitivity flag.
!!  If no instances of <Target> are found, then <Locs> is set to null:
!
!       Locs => NULL()
!

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PROCEDURES
USE FUN_INDEX2                      !!((04-B-FUN_INDEX2.f90))
USE SUB_CLEARn                      !!((04-A-SUB_CLEARn.f90))


!!## AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## INTERFACE
INTERFACE Find
 MODULE PROCEDURE Find_
ENDINTERFACE

!!## ACCESS
PUBLIC :: Find


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE \<Find_\>
PURE SUBROUTINE Find_( TARGET , STRING , LOCS , BACK , CASESEN , WILDCARD)

!!#### REQUIRED INPUT
!! @ the string to search for <TARGET>
!! @ the string to search in <STRING>
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: TARGET
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING

!!#### REQUIRED OUTPUT
!! @ locations (indices) of the beginning of string TARGET in string STRING [LOCS]
!!   is set to null if there are no instances of TARGET in STRING
INTEGER,POINTER :: LOCS(:)

!!#### OPTIONAL INPUT
LOGICAL                     ,OPTIONAL,INTENT(IN) :: BACK
LOGICAL                     ,OPTIONAL,INTENT(IN) :: CASESEN
CHARACTER(LEN=1,KIND=KIND_S),OPTIONAL,INTENT(IN) :: WILDCARD

!!#### LOCAL VARIABLES
INTEGER :: i,k,j
INTEGER :: NLOCS,IWILD,LEN_check
INTEGER :: jerr
INTEGER,ALLOCATABLE :: LOCS_(:)

!!--begin--

!! Initialize (Deallocate input LOCS to avoid memory leaks.)
CALL CLEARn( LOCS )

!! Quick return if STRING or TARGET are empty.
IF( LEN(STRING)==0 .OR. LEN(TARGET)==0 )RETURN

!! Allocate local LOCS_ based on maximum number of times
!! TARGET could be found.
NLOCS = LEN_TRIM(STRING)/LEN(TARGET) + 1
ALLOCATE( LOCS_(1:NLOCS) )
LOCS_ = 0

!! See if we have wildcard.
IF( PRESENT(WILDCARD) )THEN
 IWILD = INDEX(TARGET,WILDCARD)
ELSE
 IWILD = 0
END IF


i = 1
j = 1
!! Start searching for locations.
DO

 !check the current length we are dealing with
 IF( IWILD==0 )THEN
  LEN_check = i+LEN(TARGET)-1
 ELSE
  LEN_check = i+LEN(TARGET)-2
 END IF

 !kick out if we have exhausted chars
 IF( LEN_check>LEN_TRIM(STRING) )EXIT


 !with no wildcard, search for whole target.
 IF( IWILD==0 )THEN
  k = INDEX2(STRING(i:),TARGET,BACK=BACK,CASESEN=CASESEN)

 !with wildcard, search for target in halves
 ELSE
  !back half first
  k = INDEX2( STRING(i:) , TARGET(IWILD+1:LEN(TARGET)) , BACK=BACK , CASESEN=CASESEN )
  !front half so k ends up pointing to beginning of match
  IF( k/=0 )THEN
   k = INDEX2( STRING(i:) , TARGET(1:IWILD-1) , BACK=BACK , CASESEN=CASESEN)
  END IF
 END IF

 IF( k==0 )THEN
  EXIT
 ELSE
  i = k + i - 1
  LOCS_(j) = i
  j = j + 1
  i = i + LEN(TARGET)
 END IF
END DO


!! Get number of times TARGET was found.
NLOCS = COUNT( LOCS_/=0 )

!! Reallocate output LOCS, set equal to local LOCS_.
IF( NLOCS>0 )THEN
 ALLOCATE( LOCS(1:NLOCS) )
 LOCS(:) = LOCS_(1:NLOCS)
END IF

!! Deallocate local LOCS_.
DEALLOCATE( LOCS_ )

!!--end--
END SUBROUTINE


END MODULE
