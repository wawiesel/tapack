!!# PRINTING MODULE: <<PRN_Table>>

MODULE PRN_Table

!!## PURPOSE
!! Prints a table (rank 2 array) of strings.


!!## USAGE
!! Print a rank 2 array of strings <A2S> via
!! the following subroutine call with optional
!! arguments.
!
!  CALL PRINT_Table( A2S &
!    [,Unit] &
!    [,Delim] &
!    [,PrintSeparator] &
!    [,Separator] &
!    [,Corner] &
!    [,CenterFirstRow] )
!


!!## EXAMPLES
!!
!! 1. Schedule.
!!
! ALLOCATE( A2S(4,6) )
! A2S(1  ,1:6) = (/"Time","M","T","W","H","F"/)
! A2S(2:4,1  ) = (/"Morning","Afternoon","Evening"/)
! A2S(2  ,2:6) = (/"Work","Work","Work","Work","Work"/)
! A2S(3  ,2:6) = (/"Work","Work","Work","Work","Work"/)
! A2S(4  ,2:6) = (/"Work","Play","Play","Play","Play"/)
!
! CALL PRINT_Table( A2S , Delim="|" , PrintSeparator=(/.TRUE.,.TRUE.,.TRUE./) , &
!   Separator="-" , Corner="+" , CenterFirstRow=.TRUE. )
!
!! Output to <DEFAULT_OUTPUT_UNIT>
!
! +---------+----+----+----+----+----+
! |  Time   | M  | T  | W  | H  | F  |
! +---------+----+----+----+----+----+
! |  Morning|Work|Work|Work|Work|Work|
! |Afternoon|Work|Work|Work|Work|Work|
! |  Evening|Work|Play|Play|Play|Play|
! +---------+----+----+----+----+----+
!

!!## GLOBAL STANDARDS
USE ISO_varying_string                  !!((03-A-ISO_varying_string.f90))

!!## GLOBAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S     !!((01-A-KND_IntrinsicTypes.f90))

!!## GLOBAL PARAMETERS
USE VAR_Units,ONLY: DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))

!!## GLOBAL PROCEDURES
USE FUN_JustifyLeft                     !!((03-C-FUN_JustifyLeft.f90))
USE FUN_JustifyRight                    !!((03-C-FUN_JustifyRight.f90))
USE FUN_JustifyCenter                   !!((03-C-FUN_JustifyCenter.f90))
USE FUN_Default                         !!((04-A-FUN_Default.f90))
USE FUN_STR                             !!((05-B-FUN_STR.f90))
USE SUB_CLEAR                           !!((04-A-SUB_CLEAR.f90))
USE USR_TableSeparator                  !!((10-C-USR_TableSeparator.f90))
USE PAR_Units                           !!((02-A-PAR_Units.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## INITIAL DEFAULT SETTINGS
CHARACTER,PARAMETER :: INIT_DEFAULT_Corner            ="+"
CHARACTER,PARAMETER :: INIT_DEFAULT_Separator         ="-"
CHARACTER,PARAMETER :: INIT_DEFAULT_Delim             ="|"
LOGICAL  ,PARAMETER :: INIT_DEFAULT_PrintSeparator(3) = (/.TRUE.,.TRUE.,.TRUE./)
LOGICAL  ,PARAMETER :: INIT_DEFAULT_CenterFirstRow    = .TRUE.
INTEGER  ,PARAMETER :: INIT_DEFAULT_Indent            = 1
INTEGER  ,PARAMETER :: INIT_DEFAULT_FinalBlankLines   = 0
INTEGER  ,PARAMETER :: INIT_DEFAULT_Unit_append       = 0

!!## DEFAULT OPTIONS
CHARACTER :: DEFAULT_Corner            =INIT_DEFAULT_Corner
CHARACTER :: DEFAULT_Separator         =INIT_DEFAULT_Separator
CHARACTER :: DEFAULT_Delim             =INIT_DEFAULT_Delim
LOGICAL   :: DEFAULT_PrintSeparator(3) =INIT_DEFAULT_PrintSeparator
LOGICAL   :: DEFAULT_CenterFirstRow    =INIT_DEFAULT_CenterFirstRow
INTEGER   :: DEFAULT_Indent            =INIT_DEFAULT_Indent
INTEGER   :: DEFAULT_FinalBlankLines   =INIT_DEFAULT_FinalBlankLines
INTEGER   :: DEFAULT_Unit_append       =INIT_DEFAULT_Unit_append


!!## PROCEDURE OVERLOADING
INTERFACE PRINT_Table
 MODULE PROCEDURE PRINT_Table_A2S
END INTERFACE



!!## PUBLIC ACCESS LIST
PUBLIC :: PRINT_Table
PUBLIC :: SET_TableDefaults



!!## PROCEDURE LISTING
CONTAINS




!!### SUBROUTINE: <<RESET_TableDefaults>>
SUBROUTINE RESET_TableDefaults()

!!#### PURPOSE
!! Reset the default values for the table printing.

!!--begin--

DEFAULT_Corner            =INIT_DEFAULT_Corner
DEFAULT_Separator         =INIT_DEFAULT_Separator
DEFAULT_Delim             =INIT_DEFAULT_Delim
DEFAULT_PrintSeparator    =INIT_DEFAULT_PrintSeparator
DEFAULT_CenterFirstRow    =INIT_DEFAULT_CenterFirstRow
DEFAULT_Indent            =INIT_DEFAULT_Indent
DEFAULT_FinalBlankLines   =INIT_DEFAULT_FinalBlankLines
DEFAULT_Unit_append       =INIT_DEFAULT_Unit_append

!!--end--
END SUBROUTINE


!!### SUBROUTINE: <<SET_TableDefaults>>
SUBROUTINE SET_TableDefaults(  Delim , PrintSeparator , &
  Separator , Corner , CenterFirstRow , Indent , &
  FinalBlankLines )

!!#### PURPOSE
!! Sets the default values for the table printing.

!!#### OPTIONAL INPUT
CHARACTER,INTENT(IN),OPTIONAL :: Delim
LOGICAL  ,INTENT(IN),OPTIONAL :: PrintSeparator(3)
CHARACTER,INTENT(IN),OPTIONAL :: Corner
CHARACTER,INTENT(IN),OPTIONAL :: Separator
LOGICAL  ,INTENT(IN),OPTIONAL :: CenterFirstRow
INTEGER  ,INTENT(IN),OPTIONAL :: Indent
INTEGER  ,INTENT(IN),OPTIONAL :: FinalBlankLines

!!--begin--

IF( PRESENT(Delim)          )DEFAULT_Delim=Delim
IF( PRESENT(PrintSeparator) )DEFAULT_PrintSeparator=PrintSeparator
IF( PRESENT(Corner)         )DEFAULT_Corner=Corner
IF( PRESENT(Separator)      )DEFAULT_Separator=Separator
IF( PRESENT(CenterFirstRow) )DEFAULT_CenterFirstRow=CenterFirstRow
IF( PRESENT(Indent)         )DEFAULT_Indent=Indent
IF( PRESENT(FinalBlankLines))DEFAULT_FinalBlankLines=FinalBlankLines

!!--end--

END SUBROUTINE



!!### SUBROUTINE: <<PRINT_Table_A2S>>
SUBROUTINE PRINT_Table_A2S( A2S , Unit , Delim , PrintSeparator , &
  Separator , Corner , CenterFirstRow , Indent , FinalBlankLines , &
  ColumnWidths , UseFixedWidth , Unit_append )

!!#### PURPOSE
!! Prints out a rank 2 array of strings (table).

!!#### REQUIRED INPUT
!! @ the table <A2S(:,:)>
CHARACTER(*),INTENT(IN) :: A2S(:,:)

!!#### OPTIONAL INPUT
!! @ logical unit number to which to print <Unit>
!! @ the column delimiter character <Delim>
!! @ whether to print separators for the table <PrintSeparator(3)>
!! @ the separator character <Separator>
!! @ the corner character <Corner>
!! @ whether to center the first row <CenterFirstRow>
!! @ how many spaces to indent <Indent>
!! @ number of blank lines to add to the end <FinalBlankLines>
INTEGER  ,INTENT(IN),OPTIONAL :: Unit
CHARACTER,INTENT(IN),OPTIONAL :: Delim
LOGICAL  ,INTENT(IN),OPTIONAL :: PrintSeparator(3)
CHARACTER,INTENT(IN),OPTIONAL :: Corner
CHARACTER,INTENT(IN),OPTIONAL :: Separator
LOGICAL  ,INTENT(IN),OPTIONAL :: CenterFirstRow
INTEGER  ,INTENT(IN),OPTIONAL :: Indent
INTEGER  ,INTENT(IN),OPTIONAL :: FinalBlankLines
INTEGER  ,INTENT(IN),OPTIONAL :: ColumnWidths(SIZE(A2S,2))
LOGICAL  ,INTENT(IN),OPTIONAL :: UseFixedWidth(SIZE(A2S,2))
INTEGER  ,INTENT(IN),OPTIONAL :: Unit_append


!!#### LOCAL VARIABLES
!! @ workspace
INTEGER :: i,j,Nrows,Ncols,NField
CHARACTER(LEN(A2S)) :: T(1:SIZE(A2S,2),1:SIZE(A2S,1))
!! @ length of columns
INTEGER :: LEN_Cols(1:SIZE(T,1)),LEN_Unit_append
!! @ formats
TYPE(varying_string) :: FMT_T,FMT_entry,FMT_S
!! @ table separator
TYPE(varying_string) :: TS,line
!! @ local optionals
INTEGER :: Unit_
CHARACTER :: Delim_
CHARACTER :: Corner_
CHARACTER :: Separator_
LOGICAL :: PrintSeparator_(3)
LOGICAL :: CenterFirstRow_
INTEGER :: Unit_append_
INTEGER :: Indent_
INTEGER :: FinalBlankLines_
LOGICAL :: UseFixedWidth_(SIZE(A2S,2))

!!--begin--

!get unit number (optional)
Unit_            = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )

!get other optionals
Delim_           = DEFAULT( DEFAULT_Delim          , Delim )
PrintSeparator_  = DEFAULT( DEFAULT_PrintSeparator , PrintSeparator )
Separator_       = DEFAULT( DEFAULT_Separator      , Separator )
Corner_          = DEFAULT( DEFAULT_Corner         , Corner )
CenterFirstRow_  = DEFAULT( DEFAULT_CenterFirstRow , CenterFirstRow )
Indent_          = DEFAULT( DEFAULT_Indent         , Indent )
FinalBlankLines_ = DEFAULT( DEFAULT_FinalBlankLines, FinalBlankLines )
Unit_append_     = DEFAULT( DEFAULT_Unit_append    , Unit_append )

!get whether to use fixed widths for columns or not
IF( PRESENT(UseFixedWidth) )THEN
 UseFixedWidth_ = UseFixedWidth
ELSE
 IF( PRESENT(ColumnWidths) )THEN
  UseFixedWidth_ = .TRUE.
 ELSE
  UseFixedWidth_ = .FALSE.
 END IF
END IF

!setup
CALL CLEAR(T)
Ncols = SIZE(T,1)
Nrows = SIZE(T,2)

!determine transpose and adjust left
DO i=1,Nrows
 DO j=1,Ncols
  T(j,i) = ADJUSTL(A2S(i,j))
 END DO
END DO

!determine max length of each column
DO j=1,Ncols

 !fixed widths
 IF( UseFixedWidth_(j) )THEN
  !get the width from input
  IF( PRESENT(ColumnWidths) )THEN
   LEN_Cols(j) = ColumnWidths(j)
  !just use the maximum length
  ELSE
   LEN_Cols(j) = LEN(A2S)
  END IF

 !trimmed lengths
 ELSE
  LEN_Cols(j) = MAXVAL( LEN_TRIM(T(j,:)) )
 END IF

END DO
NField = MAXVAL( LEN_Cols )

!get table element formatting string
FMT_T = "("//TRIM(STR(Indent_))//"x,"//"'"//Delim_//"'"
DO j=1,NCols
 FMT_entry = ",a"//TRIM(STR(LEN_Cols(j)))//","//"'"//Delim_//"'"
 FMT_T = FMT_T//FMT_entry
END DO
FMT_T = FMT_T//")"

!get separator formatting string
FMT_S = "("//TRIM(STR(Indent_))//"x,a)"

!get the Separator
IF( ANY(PrintSeparator_) )THEN
 CALL ProduceTS( TS , LEN_Cols , Separator=Separator_ , Corner=Corner_ )
END IF

!output each row at a time
IF( PrintSeparator_(1) )THEN
 IF( Unit_append_/=0 )CALL RewriteRows()
 WRITE(Unit_,STR(FMT_S))STR(TS)
END IF

DO i=1,NRows

 !special first row handling
 IF( i==1 )THEN
  !centering
  IF( CenterFirstRow_ )THEN
   DO j=1,Ncols
    T(j,i) = JustifyCenter(LEN_Cols(j),T(j,i))
   END DO
  !right justification
  ELSE
   DO j=1,Ncols
    T(j,i) = JustifyRight(LEN_Cols(j),T(j,i))
   END DO
  END IF

  !write the row (appending wha was there before)
  IF( Unit_append_/=0 )CALL RewriteRows()
  WRITE(Unit_,STR(FMT_T))T(:,i)

  IF( PrintSeparator_(2) )THEN
   IF( Unit_append_/=0 )CALL RewriteRows()
   WRITE(Unit_,STR(FMT_S))STR(TS)
  END IF

 !other row handling
 ELSE
  DO j=1,Ncols
   T(j,i) = JustifyRight(LEN_Cols(j),T(j,i))
  END DO
  IF( Unit_append_/=0 )CALL RewriteRows()
  WRITE(Unit_,STR(FMT_T))T(:,i)
 END IF

END DO


IF( PrintSeparator_(3) )THEN
 IF( Unit_append_/=0 )CALL RewriteRows()
 WRITE(Unit_,STR(FMT_S))STR(TS)
END IF


DO i=1,FinalBlankLines_
 IF( Unit_append_/=0 )CALL RewriteRows()
 WRITE(Unit_,"(a)")" "
END DO


IF( Unit_append_/=0 )CALL FinishRows()

!clean up
TS = ""
FMT_T = ""
FMT_entry = ""
FMT_S = ""


!!--end--
CONTAINS


SUBROUTINE RewriteRows()
INTEGER :: jerr
!!--begin--
line=""
CALL get(Unit_append_,line,iostat=jerr)

IF( LEN_TRIM(line)==0 .OR. jerr==END_OF_FILE )THEN
 WRITE(Unit_,"(a)",ADVANCE="no")REPEAT(" ",LEN_Unit_append)
ELSE
 LEN_Unit_append = LEN(line)
 WRITE(Unit_,"(a)",ADVANCE="no")STR(line)
END IF
line=""

!!--end--
END SUBROUTINE


SUBROUTINE FinishRows()
INTEGER :: jerr
!!--begin--
DO
 line=""
 CALL get(Unit_append_,line,iostat=jerr)

 IF( LEN_TRIM(line)==0 .OR. jerr==END_OF_FILE )THEN
  EXIT
 ELSE
  WRITE(Unit_,"(a)")STR(line)
 END IF

END DO
line=""
!!--end--
END SUBROUTINE


END SUBROUTINE


END MODULE
