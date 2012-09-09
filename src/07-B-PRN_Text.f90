!!# PRINTING MODULE <<PRN_Text>>
MODULE PRN_Text

!!## PURPOSE
!! Prints text output with good line returns and indentation.


!!## USAGE
!
!  CALL PRINT_Text( STRING )
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
USE FUN_BreakLine                   !!((06-B-FUN_BreakLine.f90))

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE PRINT_Text
 MODULE PROCEDURE PRINT_Text_S
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: PRINT_Text

!!## LOCAL PARAMETERS
INTEGER,PARAMETER :: DEFAULT_Unit             = window_unit
INTEGER,PARAMETER :: DEFAULT_Columns1         = 72
INTEGER,PARAMETER :: DEFAULT_Columns2         = 90
INTEGER,PARAMETER :: DEFAULT_LineIndents(1:2) = (/0,4/)

!!## PROCEDURE LISTING
CONTAINS

!!### PRINTING SUBROUTINE: <PRINT_Text_S>
SUBROUTINE PRINT_Text_S( S , &
  Unit , &
  Columns , &
  LineIndents )
!!####PURPOSE####
!! Print a string in a paragraph type form.

!!####REQUIRED INPUT####
!! @ string [S]
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S

!!####OPTIONAL INPUT####
!! @ unit number
!! @ number of columns of output
!! @ line indentations for the first line and all other lines
INTEGER,INTENT(IN),OPTIONAL :: Unit
INTEGER,INTENT(IN),OPTIONAL :: Columns
INTEGER,INTENT(IN),OPTIONAL :: LineIndents(1:2)

!!####LOCAL VARIABLES####
INTEGER :: Unit_
INTEGER :: Columns_
INTEGER :: LineIndents_(1:2)

TYPE(varying_string),POINTER :: S_(:)
TYPE(varying_string) :: word
INTEGER :: k,ns,kk,Indent
LOGICAL,PARAMETER :: Noisy_=.TRUE.

!!--begin--

!take care of options
Unit_          = DEFAULT( DEFAULT_Unit , Unit )
IF( Unit_==window_unit )THEN
 Columns_ = DEFAULT( DEFAULT_Columns1 , Columns )
ELSE
 Columns_ = DEFAULT( DEFAULT_Columns2 , Columns )
END IF
LineIndents_   = DEFAULT( DEFAULT_LineIndents , LineIndents )

!approximate how many lines we need
ns = 2*LEN(S)/(Columns_) + 1

!allocate the pointer
ALLOCATE( S_(1:ns) )
CALL CLEAR(S_)

!start it off by setting the first entry to input S
k = 1
S_(k) = S

!outer loop
DO k = 1,ns-1


 !determine the real column to break at
 kk = MERGE(1,2,k==1)
 Indent = LineIndents_(kk)

 !break the line near the column number
 CALL BreakLine( S_(k) , S_(k+1) , Columns_-Indent )

 !if we didn't need to split then exit
 IF( LEN(S_(k+1))==0 )EXIT

END DO


!write it out
DO k=1,ns
 !get the line
 word = EXTRACT(S_(k),1,Columns_)

 !if the line is 0
 IF( LEN_TRIM(word)==0 )EXIT

 !determine the indent
 kk = MERGE(1,2,k==1)
 Indent = LineIndents_(kk)

 !write the line
 WRITE(Unit_,"(a)")REPEAT(" ",Indent)//STR(word)
END DO


!!--end--
END SUBROUTINE



END MODULE
