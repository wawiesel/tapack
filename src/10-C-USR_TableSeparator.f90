!!# USER MODULE: <<USR_TableSeparator>>
MODULE USR_TableSeparator

!!## PURPOSE
!! Provide a way to produce and interpret a fixed-width
!! table separator string that gives information about the
!! width of columns in the table without being too ugly.


!!## DETAILS
!! Call subroutine <TestTS()> to verify.


!!## USAGE
!! You provide the widths of each column and
!! the blank and Cornereter characters.  The output is
!! something you could include at the beginning and
!! end of the table.


!!## DEPENDENCIES
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
USE ISO_varying_string !!((03-A-ISO_varying_string.f90))
USE USR_SimpleList     !!((09-B-USR_SimpleList.f90))
USE FUN_Sentence       !!((04-B-FUN_Sentence.f90))
USE FUN_STR            !!((05-B-FUN_STR.f90))
USE FUN_Error          !!((04-A-FUN_Error.f90))


!!## MODULE PROCEDURES
CONTAINS



!!### SUBROUTINE: <<TestTS>>
SUBROUTINE TestTS()

!!#### PURPOSE
!! Test the production and interpretation of
!! TableSeparators.


!!#### LOCAL VARIABLES
TYPE(varying_string) :: TS
INTEGER,POINTER :: TW(:)
CHARACTER :: B,D

!!--begin--
WRITE(*,*)'>>> CALL ProduceTS( TS , (/10,5/) , "," , "_" )'
CALL ProduceTS( TS , (/10,5/) , "," , "_" )
WRITE(*,*)"<<< TS = ",STR(TS)
WRITE(*,*)'>>> CALL InterpretTS( STR(TS) , TW , D , B )'
CALL InterpretTS( STR(TS) , TW , D , B )
WRITE(*,*)"<<< TW = ",Sentence(STR(TW),beg="(/",end="/)",delim=",")
WRITE(*,*)"<<< D  = ",D
WRITE(*,*)"<<< B  = ",B


WRITE(*,*)'>>> CALL ProduceTS( TS , (/10,5/) , "-" , " " )'
CALL ProduceTS( TS , (/10,5/) , "-" , " " )
WRITE(*,*)"<<< TS = ",STR(TS)
WRITE(*,*)'>>> CALL InterpretTS( STR(TS) , TW , D , B )'
CALL InterpretTS( STR(TS) , TW , D , B )
WRITE(*,*)"<<< TW = ",Sentence(STR(TW),beg="(/",end="/)",delim=",")
WRITE(*,*)"<<< D  = ",D
WRITE(*,*)"<<< B  = ",B

WRITE(*,*)'>>> CALL ProduceTS( TS , (/3,3,10/) , "-" , ":" )'
CALL ProduceTS( TS , (/3,3,10/) , "-" , ":" )
WRITE(*,*)"<<< TS = ",STR(TS)
WRITE(*,*)'>>> CALL InterpretTS( STR(TS) , TW , D , B )'
CALL InterpretTS( STR(TS) , TW , D , B )
WRITE(*,*)"<<< TW = ",Sentence(STR(TW),beg="(/",end="/)",delim=",")
WRITE(*,*)"<<< D  = ",D
WRITE(*,*)"<<< B  = ",B

!!--end--
END SUBROUTINE



!!### SUBROUTINE: <<ProduceTS>>
SUBROUTINE ProduceTS( TableSeparator , ColumnWidths , Corner , Separator , Indent )

!!#### PURPOSE
!! Produce a table-separator string that indicate the widths
!! of fixed width tables and doesn't look horribly ugly.


!!#### EXAMPLES
!! 1)
!  >>> CALL ProduceTS( TS , (/10,5/) , "," , "_" )
!  <<< TS = ",__________,_____,"
!
!! 2)
!  >>> CALL ProduceTS( TS , (/10,5/) , "-" , " " )
!  <<< TS = "-          -     -"
!
!! 3)
!  >>> CALL ProduceTS( TS , (/3,3,10/) , "-" , ":" )
!  <<< TS = "-:::-:::-::::::::::-"



!!#### REQUIRED OUTPUT
TYPE(varying_string),INTENT(OUT) :: TableSeparator

!!#### REQUIRED INPUT
INTEGER  ,INTENT(IN) :: ColumnWidths(:)
CHARACTER,INTENT(IN) :: Corner,Separator

!!#### OPTIONAL INPUT
INTEGER  ,INTENT(IN),OPTIONAL :: Indent

!!#### LOCAL VARIABLES
INTEGER :: n,TW
TYPE(varying_string) :: VS


!!--begin--

!initialize
TableSeparator = Corner

DO n=1,SIZE(ColumnWidths)
 !get the current width
 TW = ColumnWidths(n)

 !append the resultant field and Corneriter to the table separator
 TableSeparator=TableSeparator//REPEAT(Separator,TW)//Corner

END DO

IF( PRESENT(Indent) )THEN
 TableSeparator = REPEAT(" ",Indent)//TableSeparator
END IF

!!--end--
END SUBROUTINE


!!#### SUBROUTINE <<InterpretTS>>
SUBROUTINE InterpretTS( TableSeparator , ColumnWidths , Corner , Separator , Indent )

!!#### PURPOSE
!! The inverse operation of <ProduceTS>: look at a table-separator
!! and recover the table widths and the Corneriter and blank characters.

!!#### EXAMPLES
!! 1)
!  >>> CALL InterpretTS( ",__________,_____," , TW , D , B )
!  <<< TW = (/10,5/)
!  <<< B  = ","
!  <<< D  = "_"
!
!! 2)
!  >>> CALL InterpretTS( "-          -     -" , TW , B , D )
!  <<< TW = (/10,5/)
!  <<< B  = "-"
!  <<< D  = " "
!
!! 3)
!  >>> CALL InterpretTS( "-:::-:::-::::::::::-" , TW , B , D )
!  <<< TW = (/3,3,10/)
!  <<< B  = "-"
!  <<< D  = ":"

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: TableSeparator

!!#### REQUIRED OUTPUT
INTEGER  ,POINTER     :: ColumnWidths(:)
CHARACTER,INTENT(OUT) :: Corner,Separator
INTEGER,INTENT(OUT),OPTIONAL :: Indent

!!#### LOCAL VARIABLES
INTEGER :: n,TW,m,Indent_
CHARACTER :: char

!!--begin--
!setup
IF( ASSOCIATED(ColumnWidths) )THEN
 DEALLOCATE(ColumnWidths)
END IF

IF( PRESENT(Indent) )THEN
 DO n=1,LEN(TableSeparator)
  IF( TableSeparator(n:n)/=" " )EXIT
 END DO
 Indent_ = n-1
 !get the Corneriter and blank
 Corner = TableSeparator(n:n)
 Separator = TableSeparator(n+1:n+1)
ELSE
 Indent_ = 0
 Corner = TableSeparator(1:1)
 Separator = TableSeparator(2:2)
END IF


!start extracting characters
TW = 0
m = 0
DO n=Indent_+2,LEN(TableSeparator)

 !get the char
 char = TableSeparator(n:n)

 !found Corneriter
 IF( char==Corner )THEN
  m = m + 1
  CALL ADD_TO_LIST(ColumnWidths,TW)
  TW = 0
  CYCLE

 !found blank
 ELSE IF( char==Separator )THEN
  TW = TW + 1

 !error: character can only be Corner or blank
 ELSE
  ColumnWidths(m:) = ERROR(ColumnWidths)
  RETURN
 END IF

END DO

!reallocate list to exact size
CALL FINALIZE_LIST(ColumnWidths)

IF( PRESENT(Indent) )THEN
 Indent = Indent_
END IF

!!--end--
END SUBROUTINE


END MODULE
