!!# FUNCTION MODULE: <<FUN_BreakLine>>
MODULE FUN_BreakLine

!!## PURPOSE
!! Try to break a line at a specific column.


!!## USAGE
!! Take a string <A> and break it into <A> and <B>
!! at column <Col>.
!
!  CALL BreakLine( A , B , Col )
!


!!## FORTRAN STANDARDS
USE ISO_varying_string              !!((03-A-ISO_varying_string.f90))

!!## GLOBAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!## GLOBAL PARAMETERS
USE PAR_Units,ONLY: window_unit     !!((02-A-PAR_Units.f90))

!!## GLOBAL PROCEDURES
USE SUB_CLEAR                       !!((04-A-SUB_CLEAR.f90))
USE SUB_Reallocate                  !!((04-B-SUB_Reallocate.f90))
USE FUN_Default                     !!((04-A-FUN_Default.f90))
USE FUN_STR                         !!((05-B-FUN_STR.f90))
USE FUN_VSTR                        !!((05-B-FUN_VSTR.f90))

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE BreakLine
 MODULE PROCEDURE BreakLine_VS
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: BreakLine_VS
PUBLIC :: BreakLine

!!## PROCEDURE LISTING
CONTAINS


!!### SUBROUTINE <<BreakLine_VS>>
SUBROUTINE BreakLine_VS( S1 , S2 , Col )

!!#### PURPOSE
!! Break a line into two lines at an appropriate column
!! of the first line.

!!#### REQUIRED INPUT/OUTPUT
TYPE(varying_string),INTENT(INOUT) :: S1,S2
INTEGER,INTENT(IN) :: Col

!!#### LOCAL VARIABLES
CHARACTER :: char
INTEGER :: k,kk,CleanBreak
LOGICAL,PARAMETER :: Noisy_=.FALSE.

!!--begin--

!print info
IF( Noisy_ )THEN
 WRITE(*,*)"--- original ---"
 WRITE(*,*)STR(S1)
END IF

!quick return
IF( LEN(S1)<=Col )GOTO 666


!initialize
CleanBreak = 0
S2 = ""

!get the column we want
char = EXTRACT(S1,Col)

!simple case
IF( char==" " )THEN

 CleanBreak = Col

!difficult case
ELSE

 !go backward from the column (only to 50% of column width)
 !to find a blank
 DO k=Col-1,INT(Col*0.5),-1

  char = EXTRACT(S1,k)

  IF( char==" " )THEN
   CleanBreak=-k
   EXIT
  END IF

 END DO

END IF


!found the easy clean break
IF( CleanBreak>0 )THEN
 S2 = Extract( S1 , CleanBreak+1 , LEN(S1)    )
 S1 = Extract( S1 , 1            , CleanBreak )

!found the backward clean break
ELSE IF( CleanBreak<0 )THEN
 CleanBreak = ABS(CleanBreak)
 S2 = Extract( S1 , CleanBreak+1 , LEN(S1)    )
 S1 = Extract( S1 , 1            , CleanBreak )

!must insert hyphen
ELSE
 S2 = Extract( S1 , Col , LEN(S1) )
 S1 = Extract( S1 , 1   , Col-1 )//"-"
END IF

666 CONTINUE

!print info
IF( Noisy_ )THEN
 WRITE(*,*)"--- split1 ---"
 WRITE(*,*)STR(S1)
 WRITE(*,*)"--- split2 ---"
 WRITE(*,*)STR(S2)
END IF

!!--end--
END SUBROUTINE



END MODULE
