!!### LIBRARY MODULES: Prompts
MODULE LIB_Prompts
!!#### PURPOSE
!! Provide a small library of prompts to use when delivering
!! information to and from the command line.

!!#### DEPENDENCIES
USE KND_IntrinsicTypes     ,ONLY: KIND_S      !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Units,ONLY: window_unit,keyboard_unit !!((02-A-PAR_Units.f90))
USE FUN_STR  ,ONLY: STR                       !!((05-B-FUN_STR.f90))

!!#### PARAMETERS
INTEGER,PARAMETER :: LEN_Prompt = 4
!! @ question prompt [QPrompt]
CHARACTER(LEN=LEN_Prompt,KIND=KIND_S),PARAMETER :: QPrompt       = "(?) "

!! @ blank indentation [Indent]
CHARACTER(LEN=LEN_Prompt,KIND=KIND_S),PARAMETER :: Indent        = "    "

!! @ waiting for USEr input (for appropriate behavior of UserPrompt,
!!   usually need to use subroutine UserPrompt below) [UserPrompt2]
CHARACTER(LEN=LEN_Prompt,KIND=KIND_S),PARAMETER :: UserPrompt2   = ">>> "

!! @ information sent out to human at terminal [InfoPrompt]
!! @ comment information sent out to a human at a terminal [CommentPrompt]
!! @ warning information sent out to human at terminal [WarningPrompt]
!! @ error information sent out to human at terminal [ErrorPrompt]
CHARACTER(LEN=LEN_Prompt,KIND=KIND_S),PARAMETER :: InfoPrompt    = "<<< "
CHARACTER(LEN=LEN_Prompt,KIND=KIND_S),PARAMETER :: CommentPrompt = "... "
CHARACTER(LEN=LEN_Prompt,KIND=KIND_S),PARAMETER :: WarningPrompt = "??? "
CHARACTER(LEN=LEN_Prompt,KIND=KIND_S),PARAMETER :: ErrorPrompt   = "!!! "

CONTAINS

SUBROUTINE UserPrompt(lmargin,unit,s)
!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: lmargin
INTEGER,INTENT(IN),OPTIONAL :: unit
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN),OPTIONAL :: s
!!#### LOCAL VARIABLES
INTEGER :: lmargin_
INTEGER :: unit_
CHARACTER(32) :: fmt_

!!--begin--

!determine unit
IF( PRESENT(unit) )THEN
 unit_ = unit
ELSE
 unit_ = window_unit
ENDIF

!determine left margin
IF( PRESENT(lmargin) )THEN
 fmt_="("//TRIM(STR(lmargin))//"x,a)"
ELSE
 fmt_="(a)"
ENDIF

!! write prompt
WRITE(unit_,fmt_,ADVANCE="no")UserPrompt2

!! add <s>
IF( PRESENT(s) )THEN
 WRITE(unit_,"(a)",ADVANCE="no")s
END IF

!!--end--
ENDSUBROUTINE

ENDMODULE
