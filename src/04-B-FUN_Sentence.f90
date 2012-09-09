MODULE FUN_Sentence
!!#### PURPOSE
!! Construct a "sentence" (a collection of words) from an array
!! of strings (each element is a word).  Various options are allowed:
!
!! @ Set string S equal to the characters contained in array <Words>,
!! @ with a separator <delim> between each word,
!! @ add a beginning character <beg> or capitalization of the
!!    first N-characters by setting <beg = "1" through "9"> where
!!    the character value is the number of capitalized characters,
!! @ add an ending character <end> which appears at the end of the list.


!!#### AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_Upcase                      !!((03-A-FUN_Upcase.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE Sentence
 MODULE PROCEDURE Sentence0
 MODULE PROCEDURE Sentence1
 MODULE PROCEDURE Sentence3
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Sentence


CONTAINS


PURE FUNCTION Sentence0( Words ) RESULT(S)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: Words(:)

!!#### REQUIRED OUTPUT
CHARACTER(SIZE(Words)*LEN(Words)) :: S

!!#### LOCAL VARIABLES
INTEGER :: i,j

!!--begin--

!! Quick return if <Words> has no elements.
IF( SIZE(Words)==0 )THEN
 S = REPEAT(" ",LEN(S))
 RETURN
END IF

!! Copy <Words> elements into <S>.
i = 1
DO j=1,SIZE(Words)-1
 S(i:i+LEN(Words(j))-1) = TRIM(Words(j))
 i = i + LEN(Words(j))
END DO

S(i:i+LEN(Words(SIZE(Words)))-1) = TRIM(Words(j))

j=i+LEN(Words(SIZE(Words)))

FORALL( i=j:LEN(S) )S(i:i) = " "

!!--end--
END FUNCTION


PURE FUNCTION Sentence1( Words , delim ) RESULT(S)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: Words(:)
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: delim

!!#### RESULT
CHARACTER(SIZE(Words)*LEN(Words)+(SIZE(Words)-1)*LEN(delim)) :: S

!!#### LOCAL VARIABLES
INTEGER :: i,j

!!--begin--

!! Quick return if <Words> has no elements.
IF( SIZE(Words)==0 )THEN
 S = REPEAT(" ",LEN(S))
 RETURN
END IF

!! Copying <Words> elements into <S> with a delim.
i = 1
DO j=1,SIZE(Words)-1
 S(i:i+LEN_TRIM(Words(j))+LEN(delim)) = TRIM(Words(j))//delim
 i = i + LEN_TRIM(Words(j)) + LEN(delim)
END DO

!! Setting the rest.
S(i:i+LEN_TRIM(Words(SIZE(Words)))-1) = TRIM(Words(j))

!! Find the very end.
j=i+LEN_TRIM(Words(SIZE(Words)))

!! Set the rest to spaces.
FORALL( i=j:LEN(S) ) S(i:i) = " "

!!--end--
END FUNCTION


PURE FUNCTION Sentence3( Words , delim , beg , end ) RESULT(S)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: Words(:)
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: delim
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: beg
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: end

!!#### REQUIRED OUTPUT
CHARACTER(SIZE(Words)*LEN(Words)+(SIZE(Words)-1)*LEN(delim)+LEN(beg)+LEN(end)) :: S

!!#### LOCAL VARIABLES
INTEGER :: i,j

!!--begin--

!! Quick return if <Words> has no elements.
IF( SIZE(Words)==0 )THEN
 S = REPEAT(" ",LEN(S))
 RETURN
END IF

!! Add the beg or capitalize some letters of the first word.
i=SCAN("123456789",beg)
IF( i/=0 )THEN
 i = MAX(i,LEN(Words))
 S(1:i) = UpCase(Words(1)(1:i))
 i = i + 1
ELSE
 S(1:LEN(beg)) = beg
 i = LEN(beg) + 1
END IF

!! Copy <Words> elements into S with a delim.
DO j=1,SIZE(Words)-1
 S(i:i+LEN_TRIM(Words(j))+LEN(delim)) = TRIM(Words(j))//delim
 i = i + LEN_TRIM(Words(j)) + LEN(delim)
END DO

!! Add the end.
S(i:i+LEN_TRIM(Words(SIZE(Words)))+LEN(end)-1) = TRIM(Words(j))//end
j=i+LEN_TRIM(Words(SIZE(Words)))+LEN(end)
FORALL( i=j:LEN(S) )S(i:i) = " "

!!--end--
END FUNCTION


END MODULE
