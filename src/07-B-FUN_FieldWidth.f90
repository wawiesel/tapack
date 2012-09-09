!!# MODULE <<FUN_FieldWidth>>
MODULE FUN_FieldWidth

!!## PURPOSE
!! Extract the field width as an integer
!! from a single, simple format statement.


!!## EXAMPLES
!! For a simple edit descriptor Es,
!
!!   FieldWidth( "Es12.5" ) = 12
!
!! This will also work if the parentheses are included,
!
!!   FieldWidth( "(Es12.5)" ) = 12
!
!! This will also work if there is a multiplier in front of "Es",
!
!!   FieldWidth( "(4Es12.5)" ) = 12
!
!!     or
!!
!!   FieldWidth( "(4(Es12.5))" ) = 12
!
!! This will also work if no post-decimal number is included,
!
!!   FieldWidth( "I5" ) = 5


!!## MODULES
USE KND_IntrinsicTypes,ONLY: KIND_S  !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_NumericStrings,ONLY: digits_ !!((02-A-PAR_NumericStrings.f90))
USE FUN_INT                          !!((06-B-FUN_INT.f90))


!!## MODULE PROCEDURES
CONTAINS


!!### FUNCTION <<FieldWidth>>
FUNCTION FieldWidth( FMT )
!!#### RESULT
INTEGER :: FieldWidth
!!#### ARGUMENT
CHARACTER(LEN=*,KIND=KIND_S) :: FMT
CHARACTER(LEN=10,KIND=KIND_S) :: fw

!!--begin--
!! Given a format string FMT, first the decimal "." is found.
k2 = INDEX( TRIM(FMT) , "." , BACK=.TRUE. )

!! If there is no decimal then the inner-most closing parenthesis
!! ")" is found.
IF( k2==0 )THEN
 k2 = INDEX( TRIM(FMT) , ")" , BACK=.TRUE. )
ENDIF

!! If this is not present, it is assumed the edit
!! descriptor ends at the trimmed length.
IF( k2==0 )k2=LEN_TRIM(FMT)

!! Then, from k2-1, searching backwards, the first non-numeric
!! character is found at k1.
k1 = VERIFY( FMT(1:k2-1) , SET=digits_ , BACK=.TRUE. )

!! The correct field width is (integer=string conversion used)
fw = REPEAT(" ",LEN(fw))
fw = FMT(k1+1:k2-1)
FieldWidth = INT(fw,KIND(1))

!!--end--
END FUNCTION

END MODULE
