!!# MODULE: FUNCTION  FUN_INDEXa
MODULE FUN_INDEXa
!!## PURPOSE
!! The <INDEXa> function returns the index in a list
!! of strings which is an exact match for STRING, ignoring
!! all blanks and providing a search from the back of
!! the list and case (in)sensitivity options.


!!## USAGE
!
!    I = INDEXa( STRING_LIST , STRING [, BACK , CASESEN ] )
!
!!  where <I> is the index of <STRING> in the <STRING_LIST>
!!  and <BACK> specifies whether to start at the back of the array,
!!  and <CASESEN> is whether or not the search is case-sensitive.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!## GLOBAL BINARY OPERATORS
USE BOP_sEQ                         !!((03-A-BOP_sEQ.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Default                     !!((04-A-FUN_Default.f90))
USE SUB_Find                        !!((05-B-SUB_Find.f90))
USE SUB_CLEAR                       !!((04-A-SUB_CLEAR.f90))
USE SUB_Replace                     !!((06-B-SUB_Replace.f90))
USE SUB_Reallocate                  !!((04-B-SUB_Reallocate.f90))

!!## METHOD
!! Basically we loop over all indices of STRING_LIST to find STRING.
!! We use .EQ.(==) if CASESEN is .FALSE. and the case-insensitve
!! comparison operator, .sEQ. to compare STRING to entries in STRING_LIST.
!! Because of the necessity for the trimmed length to be the same,
!! we also perform an initial check on each element STRING_LIST(I)
!! to see if it matches LEN_TRIM(STRING), only then do we proceed
!! with character checking.


!!## EXAMPLES
!!  Basic case sensitive/insensitive comparisons:
!!   1. INDEXa( ["real   ","complex"] , "real" ) = 1
!!   2. INDEXa( ["Real   ","complex"] , "real" ) = 0
!!   3. INDEXa( ["Real   ","complex"] , "real" , CASESEN=.FALSE. ) = 1
!!
!!  Using BACK for identical multiple entries:
!!   4. INDEXa( ["real   ","complex","real   "] , "real" , BACK=.TRUE. ) = 3
!!
!!
!!  Demonstration of how blanks are ignored:
!!   5. INDEXa( ["  real ","complex"," real  "] , "real" ) = 1
!!
!!
!!  Showing partial matches are not considered:
!!   6. INDEXa( ["  real ","complex"," real  "] , "rea" ) = 0
!!   7. INDEXa( ["  real ","complex"," real  "] , "comple" ) = 0


!!## POTENTIAL USAGE
!! I use this routine all the time for mapping key-words to key-integers.
!! For example in my code say I have some output options: "Partial","Full","None".
!! I create a parameter array:
!!  CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: OUTPUT_OPTIONS=["Partial",&
!!                                                           "Full   ",&
!!                                                           "None   "]
!! Then when I read the output option I get s="full" or s="FULL" or s="FuLl" but all
!! I want is some integer key for the output option.
!!
!!  OutOpt = INDEXa( OUTPUT_OPTIONS , s , CASESEN=.FALSE. )
!
!! Now inside my code I use a CASE statement to choose the output.
!
!!  SELECT CASE(OutOpt)
!
!!   CASE(1)
!!    <Partial Output Code>
!
!!   CASE(2)
!!    <Full Output Code>
!
!!   CASE(3)
!!    <No Output Code>
!
!!   CASE DEFAULT
!!    <Error Code>
!
!!  END SELECT
!
!! The conversion of strings to integers at input makes for a faster
!! and more robust code.  The only thing to be careful of is making sure that the
!! indices in OUTPUT_OPTIONS always corresponds to the indices in the case
!! statement.

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## LOCAL PARAMETERS
!! @ default case sensitivity
!! @ default backwards search
LOGICAL,PARAMETER :: DEFAULT_CASESEN = .TRUE.
LOGICAL,PARAMETER :: DEFAULT_BACK    = .FALSE.

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE INDEXa
 MODULE PROCEDURE INDEXa_
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: INDEXa


!!## MODULE PROCEDURES
CONTAINS


!!### FUNCTION: INDEXa_
FUNCTION INDEXa_( STRING_LIST , STRING , BACK , &
  CASESEN , START , WILDCARD ) RESULT(I)

!!#### REQUIRED INPUT
!! @ the array of strings to be checked <STRING_LIST>
!! @ the substring to look for <STRING>
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING_LIST(:)
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING

!!#### REQUIRED OUTPUT
!! @ the index in STRING_LIST (0 if not found) <I>
INTEGER :: I

!!#### OPTIONAL INPUT
!! @ whether to return the index from the back <BACK>
!! @ whether to consider the search case-sensitive <CASESEN>
!! @ where to start the search <START>
!! @ the wildcard character <WILDCARD>
LOGICAL                     ,INTENT(IN),OPTIONAL :: BACK
LOGICAL                     ,INTENT(IN),OPTIONAL :: CASESEN
INTEGER                     ,INTENT(IN),OPTIONAL :: START
CHARACTER(LEN=1,KIND=KIND_S),INTENT(IN),OPTIONAL :: WILDCARD

!!#### LOCAL VARIABLES
LOGICAL :: CASESEN_
INTEGER :: J,count,I_,J1,J2,K,IWILD
CHARACTER(LEN=LEN_TRIM(ADJUSTL(STRING)),KIND=KIND_S) :: STRING_
CHARACTER(LEN=LEN(STRING_LIST),KIND=KIND_S) :: S
LOGICAL :: BACK_,Found
INTEGER,POINTER :: WLOCS(:)

!!--begin--
!! Determine the local value for case-sensitive search.
CASESEN_ = Default( DEFAULT_CASESEN , CASESEN )

!! Determine local value for backwards search.
BACK_    = Default( DEFAULT_BACK , BACK )

IF( PRESENT(WILDCARD) )THEN
 IWILD = INDEX( STRING , WILDCARD )
ELSE
 IWILD = 0
END IF

!determine starting index
I_ = Default( 0 , START )

!! Get a local version of STRING
!! with exactly the right number of characters.
STRING_ = TRIM(ADJUSTL(STRING))

!initialize
I     = 0
count = 0

!! Start search.
DO
 !! exit on counting reaching size of list
 IF( count>=SIZE(STRING_LIST) )EXIT

 !! increment element index
 IF( BACK_ )THEN
  IF( I_==1 )THEN
   I_ = SIZE(STRING_LIST)
  ELSE
   I_ = I_ - 1
  END IF
 ELSE
  I_ = MOD(I_,SIZE(STRING_LIST)) + 1
 END IF

 !! increment count
 count = count + 1


 !! get string for this element
 S = ADJUSTL(STRING_LIST(I_))

 !! get length
 J = LEN_TRIM(S)
 IF( J==0 )CYCLE

 !! check equality with wildcard
 IF( IWILD/=0 )THEN

  !first find wildcard locations
  NULLIFY( WLOCS )
  CALL FIND( WILDCARD , STRING_ , WLOCS , CASESEN=CASESEN_ )
  Found = .TRUE.

  !replace characters found with blanks
  J1 = 1
  DO K=1,SIZE(WLOCS)
   J2 = WLOCS(K)
   IF( J2-1>J1 )THEN
    CALL REPLACEString( STRING_(J1:J2-1) , S , "" , S , CaseSensitive=CASESEN_ , NReplaces=J)
    IF( J/=1 )THEN
     Found = .FALSE.
     EXIT
    END IF
   END IF
   J1 = J2 + 1
  END DO

  !if we found it
  IF( Found )THEN
   I = I_
   EXIT
  END IF

  !wrapup
  DEALLOCATE( WLOCS )

 !! simple equality
 ELSE
  !! check lengths first
  IF( J/=LEN(STRING_) )CYCLE

  !! use normal equality operator
  IF( CASESEN_ )THEN
   IF( S(1:J) == STRING_ )THEN
    I = I_
    EXIT
   END IF

  !! use special string equality binary operator
  ELSE
   IF( S(1:J) .sEQ. STRING_ )THEN
    I = I_
    EXIT
   END IF

  END IF

 END IF

END DO


!!--end--
END FUNCTION

END MODULE
