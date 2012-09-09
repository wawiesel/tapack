!!# FUNCTION MODULE: <FUN_VSTROPTION>
MODULE FUN_VSTROPTION

!!## PURPOSE
!! Return a varying string containing a pretty format for
!! an option value.


!!## USAGE
!! The following function call
!
!  VS = VSTROPTION( "OptionName" , "OptionVal" , WhiteSpace="-" , LeadSpace=4 , &
!                 OptionNameLength=10 , OptionValLength=13 )
!!
!! results in the following string being stored in <S>
!
!  VS <- "----OptionName--:----OptionVal"
!

!!### Required Input
!! @ <CHARACTER(*)   :: OptionName> the name of the option
!! @ <CHARACTER(*)   :: OptionVal > the value of the option

!!### Optional Input
!! @ <CHARACTER(1)   :: Delimiter > the delimiter (DEFAULT is the colon, ":")
!! @ <CHARACTER(1)   :: WhiteSpace> the whitespace (DEFAULT is space, " ")
!! @ <INTEGER        :: LeadSpace> the leading number of whitespaces (DEFAULT is 0)
!! @ <INTEGER        :: OptionNameLength> the length of the field for <OptionName>
!! @ <INTEGER        :: OptionValLength> the length of field for <OptionVal>
!! @ <LOGICAL        :: UseQuotes> surround the <OptionVal> by double quotes
!!
!! The option names are left justified (after the leading spaces), while
!! the option values are right justified.

!!## MODULES
USE FUN_Default        !!((04-A-FUN_Default.f90))
USE ISO_varying_string !!((03-A-ISO_varying_string.f90))
USE VAR_Units          !!((03-A-VAR_Units.f90))
USE FUN_STR            !!((05-B-FUN_STR.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## LOCAL VARIABLES
CHARACTER(1),SAVE :: DEFAULT_Delimiter=":"
CHARACTER(1),SAVE :: DEFAULT_WhiteSpace=" "
INTEGER     ,SAVE :: DEFAULT_LeadSpace=0
INTEGER     ,SAVE :: DEFAULT_OptionNameLength=0
INTEGER     ,SAVE :: DEFAULT_OptionValLength=0
LOGICAL     ,SAVE :: DEFAULT_UseQuotes=.FALSE.


!!## ACCESS
PUBLIC :: VSTROPTION
PUBLIC :: SET_VSTROPTION
PUBLIC :: GET_VSTROPTION
PUBLIC :: PRINT_VSTROPTION
PUBLIC :: RESET_VSTROPTION


!!## MODULE PROCEDURES
CONTAINS

!!### SUBROUTINE <SET_VSTROPTION>
SUBROUTINE SET_VSTROPTION(Delimiter,WhiteSpace,&
  LeadSpace,OptionNameLength,OptionValLength,UseQuotes)

!!#### OPTIONAL INPUT
CHARACTER(1),INTENT(IN),OPTIONAL :: Delimiter
CHARACTER(1),INTENT(IN),OPTIONAL :: WhiteSpace
INTEGER     ,INTENT(IN),OPTIONAL :: LeadSpace
INTEGER     ,INTENT(IN),OPTIONAL :: OptionNameLength
INTEGER     ,INTENT(IN),OPTIONAL :: OptionValLength
LOGICAL     ,INTENT(IN),OPTIONAL :: UseQuotes

!!--begin--
IF( PRESENT(Delimiter) )THEN
 DEFAULT_Delimiter=Delimiter
END IF
IF( PRESENT(WhiteSpace) )THEN
 DEFAULT_WhiteSpace=WhiteSpace
END IF
IF( PRESENT(LeadSpace) )THEN
 DEFAULT_LeadSpace=LeadSpace
END IF
IF( PRESENT(OptionNameLength) )THEN
 DEFAULT_OptionNameLength=OptionNameLength
END IF
IF( PRESENT(OptionValLength) )THEN
 DEFAULT_OptionValLength=OptionValLength
END IF
IF( PRESENT(UseQuotes) )THEN
 DEFAULT_UseQuotes=UseQuotes
END IF
!!--end--
END SUBROUTINE



!!### SUBROUTINE <GET_VSTROPTION>
SUBROUTINE GET_VSTROPTION(Delimiter,WhiteSpace,&
  LeadSpace,OptionNameLength,OptionValLength,UseQuotes)

!!#### OPTIONAL OUTPUT
CHARACTER(1),INTENT(OUT),OPTIONAL :: Delimiter
CHARACTER(1),INTENT(OUT),OPTIONAL :: WhiteSpace
INTEGER     ,INTENT(OUT),OPTIONAL :: LeadSpace
INTEGER     ,INTENT(OUT),OPTIONAL :: OptionNameLength
INTEGER     ,INTENT(OUT),OPTIONAL :: OptionValLength
LOGICAL     ,INTENT(OUT),OPTIONAL :: UseQuotes

!!--begin--
IF( PRESENT(Delimiter) )THEN
 Delimiter = DEFAULT_Delimiter
END IF
IF( PRESENT(WhiteSpace) )THEN
 WhiteSpace = DEFAULT_WhiteSpace
END IF
IF( PRESENT(LeadSpace) )THEN
 LeadSpace = DEFAULT_LeadSpace
END IF
IF( PRESENT(OptionNameLength) )THEN
 OptionNameLength = DEFAULT_OptionNameLength
END IF
IF( PRESENT(OptionValLength) )THEN
 OptionValLength = DEFAULT_OptionValLength
END IF
IF( PRESENT(UseQuotes) )THEN
 UseQuotes = DEFAULT_UseQuotes
END IF
!!--end--
END SUBROUTINE


!!### SUBROUTINE <RESET_VSTROPTION>
SUBROUTINE RESET_VSTROPTION()
!!--begin--
CHARACTER(1),SAVE :: DEFAULT_Delimiter=":"
CHARACTER(1),SAVE :: DEFAULT_WhiteSpace=" "
INTEGER     ,SAVE :: DEFAULT_LeadSpace=0
INTEGER     ,SAVE :: DEFAULT_OptionNameLength=0
INTEGER     ,SAVE :: DEFAULT_OptionValLength=0
LOGICAL     ,SAVE :: DEFAULT_UseQuotes=.FALSE.
!!--end--
END SUBROUTINE


!!### SUBROUTINE <PRINT_VSTROPTION>
SUBROUTINE PRINT_VSTROPTION(Unit)
INTEGER,INTENT(IN),OPTIONAL :: Unit
INTEGER :: Unit_
!!--begin--

IF( PRESENT(Unit) )THEN
 Unit_=Unit
ELSE
 Unit_=DEFAULT_OUTPUT_UNIT
END IF

CALL PUT_line(Unit_,VSTROPTION("DEFAULT_Delimiter",DEFAULT_Delimiter))
CALL PUT_line(Unit_,VSTROPTION("DEFAULT_WhiteSpace",DEFAULT_WhiteSpace))
CALL PUT_line(Unit_,VSTROPTION("DEFAULT_LeadSpace",STR(DEFAULT_LeadSpace)))
CALL PUT_line(Unit_,VSTROPTION("DEFAULT_OptionNameLength",STR(DEFAULT_OptionNameLength)))
CALL PUT_line(Unit_,VSTROPTION("DEFAULT_OptionValLength",STR(DEFAULT_OptionValLength)))
CALL PUT_line(Unit_,VSTROPTION("DEFAULT_UseQuotes",STR(DEFAULT_UseQuotes)))

!!--end--
END SUBROUTINE

!!### FUNCTION <VSTROPTION>
FUNCTION VSTROPTION(OptionName,OptionVal,Delimiter,WhiteSpace,&
  LeadSpace,OptionNameLength,OptionValLength,UseQuotes)  RESULT(VS)

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: OptionName
CHARACTER(*),INTENT(IN) :: OptionVal

!!#### REQUIRED OUTPUT
TYPE(varying_string) :: VS

!!#### OPTIONAL INPUT
CHARACTER(1),INTENT(IN),OPTIONAL :: Delimiter
CHARACTER(1),INTENT(IN),OPTIONAL :: WhiteSpace
INTEGER     ,INTENT(IN),OPTIONAL :: LeadSpace
INTEGER     ,INTENT(IN),OPTIONAL :: OptionNameLength
INTEGER     ,INTENT(IN),OPTIONAL :: OptionValLength
LOGICAL     ,INTENT(IN),OPTIONAL :: UseQuotes

!!#### LOCAL VARIABLES
CHARACTER(1) :: Delimiter_
CHARACTER(1) :: WhiteSpace_
INTEGER      :: LeadSpace_
INTEGER      :: OptionNameLength_
INTEGER      :: OptionValLength_
LOGICAL      :: UseQuotes_
INTEGER      :: i,j,n

!!--begin--
Delimiter_ = Default( DEFAULT_Delimiter , Delimiter )
WhiteSpace_ = Default( DEFAULT_WhiteSpace , WhiteSpace )
LeadSpace_ = Default( DEFAULT_LeadSpace , LeadSpace )
OptionNameLength_ = Default( DEFAULT_OptionNameLength , OptionNameLength )
OptionValLength_ = Default( DEFAULT_OptionValLength , OptionValLength )
UseQuotes_ = Default( DEFAULT_UseQuotes , UseQuotes )

IF( OptionNameLength_==0 )THEN
 OptionNameLength_=LEN(OptionName)
END IF

IF( OptionValLength_==0 )THEN
 OptionValLength_=LEN(OptionVal)
END IF


!set leading space
DO i=1,LeadSpace_
 VS = Insert( VS , i , WhiteSpace_ )
END DO

!update the last character index
n=i-1

!set option name
DO i=n+1,n+OptionNameLength_
 j = i-n
 IF( j>LEN(OptionName) )THEN
  VS = Insert( VS , i , WhiteSpace_ )
 ELSE
  VS = Insert( VS , i , OptionName(j:j) )
 END IF
END DO

!set the delimiter
VS = Insert( VS , i , Delimiter_ )
n=i

!set option name
DO i=n+1,n+OptionValLength_
 j = i-n
 IF( j>LEN(OptionVal) )THEN
  VS = Insert( VS , i , WhiteSpace_ )
 ELSE
  VS = Insert( VS , i , OptionVal(j:j) )
 END IF
END DO

!!--end--
END FUNCTION


END MODULE

