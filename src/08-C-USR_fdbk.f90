!!# USER MODULE <<USR_fdbk>>
MODULE USR_fdbk

!!## PURPOSE
!! Hopefully, as is apparent by the name, the fdbk object facilitates
!! feedback---from inside routines---mostly for warning or error conditions,
!! but can also be used for profiling code.


!!## DETAILS
!! The feedback type <TYPE_fdbk> is built on the block type
!! <TYPE_Block>.

!!## GLOBAL PARAMETERS
USE PAR_Units          !!((02-A-PAR_Units.f90))

USE ISO_varying_string !!((03-A-ISO_varying_string.f90))

!!## GLOBAL PROCEDURES (Flow)
USE SUB_Pause          !!((04-B-SUB_Pause.f90))
USE SUB_Stop           !!((04-B-SUB_Stop.f90))

!!## GLOBAL PROCEDURES (Status)
USE FUN_Error          !!((04-A-FUN_Error.f90))
USE FUN_Warning        !!((04-B-FUN_Warning.f90))
USE FUN_IsError        !!((05-A-FUN_IsError.f90))
USE FUN_IsWarning      !!((05-B-FUN_IsWarning.f90))

!!## GLOBAL PROCEDURES (String Manipulation)#
USE FUN_Sentence       !!((04-B-FUN_Sentence.f90))
USE FUN_Smush          !!((03-A-FUN_Smush.f90))
USE FUN_STR            !!((05-B-FUN_STR.f90))
USE SUB_Replace        !!((06-B-SUB_Replace.f90))

!!## GLOBAL PRINTING
USE PRN_Text           !!((07-B-PRN_Text.f90))

!!## GLOBAL USER MODULES
USE USR_Block          !!((05-B-USR_Block.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts        !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases !!((07-B-LIB_GenericPhrases.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PRIVATE ACCESS LIST
PRIVATE :: KIND_I,KIND_R


!!## GLOBAL PARAMETERS
!! * values for ActiveLevel
INTEGER(KIND_I),PARAMETER :: fdbk_Error   =  100
INTEGER(KIND_I),PARAMETER :: fdbk_Warning =  050
INTEGER(KIND_I),PARAMETER :: fdbk_Comment =  025
INTEGER(KIND_I),PARAMETER :: fdbk_Profile =  010
INTEGER(KIND_I),PARAMETER :: fdbk_None    =  001
INTEGER(KIND_I),PARAMETER :: fdbk_Choose  =  000

!! * values for defaults
LOGICAL(KIND_L),PARAMETER :: CompileDefaults     = .FALSE.
INTEGER(KIND_I),PARAMETER :: DEFAULT_ActiveLevel = fdbk_none
INTEGER(KIND_I),PARAMETER :: DEFAULT_PauseLevel  = fdbk_warning
INTEGER(KIND_I),PARAMETER :: DEFAULT_StopLevel   = fdbk_error
INTEGER(KIND_I),PARAMETER :: DEFAULT_OutputUnit  = window_unit
INTEGER(KIND_I),PARAMETER :: DEFAULT_InputUnit   = keyboard_unit

!!## DEFINED TYPE <<TYPE_fdbk>>
!! * define a type to handle feedback from procedures, <TYPE_fdbk>
TYPE TYPE_fdbk
 INTEGER(KIND_I) ,POINTER :: CurrentLevel => NULL()
 INTEGER(KIND_I) ,POINTER :: ActiveLevel  => NULL()
 INTEGER(KIND_I) ,POINTER :: PauseLevel   => NULL()
 INTEGER(KIND_I) ,POINTER :: StopLevel    => NULL()
 INTEGER         ,POINTER :: OutputUnit   => NULL()
 TYPE(TYPE_Block),POINTER :: RootTrace    => NULL()
 TYPE(TYPE_Block),POINTER :: CurrentTrace => NULL()
END TYPE


!!## MEMORY MANAGEMENT
!! * allocation procedure
INTERFACE ALLOCATE
 MODULE PROCEDURE ALLOCATE_fdbk
END INTERFACE
!! * allocated check function
INTERFACE ALLOCATED
 MODULE PROCEDURE ALLOCATED_fdbk
END INTERFACE
!! * deallocation procedure
INTERFACE DEALLOCATE
 MODULE PROCEDURE DEALLOCATE_fdbk
END INTERFACE


!!## GLOBAL PROCEDURES for <TYPE_fdbk>
!! * update contents of feedback
INTERFACE Update
 MODULE PROCEDURE Update0_fdbk
 MODULE PROCEDURE Update1_fdbk
END INTERFACE
!! * update and dump the contents of feedback
INTERFACE UpdateAndDump
 MODULE PROCEDURE UpdateAndDump0_fdbk
 MODULE PROCEDURE UpdateAndDump1_fdbk
END INTERFACE
!! * dump the contents of feedback
INTERFACE Dump
 MODULE PROCEDURE Dump_fdbk
END INTERFACE
!! * change parameters of the feedback
INTERFACE Modify
 MODULE PROCEDURE Modify_fdbk
END INTERFACE
!! * if the feedback has info
INTERFACE HasInfo
 MODULE PROCEDURE HasInfo_fdbk
END INTERFACE
!! * if the feedback has an error (or worse)
INTERFACE IsError
 MODULE PROCEDURE IsError_fdbk
END INTERFACE
!! * if the feedback has a warning (or worse)
INTERFACE IsWarning
 MODULE PROCEDURE IsWarning_fdbk
END INTERFACE
!! * if the feedback has a comment (or worse)
INTERFACE IsComment
 MODULE PROCEDURE IsComment_fdbk
END INTERFACE
!! * if the feedback is active
INTERFACE IsActive
 MODULE PROCEDURE IsActive_fdbk
END INTERFACE
!! * the unit where information should be dumped
INTERFACE OutputUnit
 MODULE PROCEDURE OutputUnit_fdbk
END INTERFACE
!! * update the current level
INTERFACE Update_Level
 MODULE PROCEDURE Update_level_fdbk
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: TYPE_fdbk
PUBLIC :: fdbk_error
PUBLIC :: fdbk_warning
PUBLIC :: fdbk_comment
PUBLIC :: fdbk_profile
PUBLIC :: fdbk_none
PUBLIC :: fdbk_choose
PUBLIC :: OutputUnit
PUBLIC :: HasInfo
PUBLIC :: IsActive
PUBLIC :: IsWarning
PUBLIC :: IsError
PUBLIC :: IsComment
PUBLIC :: Modify
PUBLIC :: Dump
PUBLIC :: Update,UpdateAndDump
PUBLIC :: ALLOCATE,ALLOCATE_fdbk
PUBLIC :: DEALLOCATE,DEALLOCATE_fdbk
PUBLIC :: ALLOCATED,ALLOCATED_fdbk
PUBLIC :: Print_fdbk

!!## CONTAINED PROCEDURES
CONTAINS


!!### SUBROUTINE <<PRINT_fdbk>>
SUBROUTINE PRINT_fdbk( fdbk )
!!#### PURPOSE
!! Print contents of fdbk.

!!#OPTIONAL INPUT/OUTPUT#
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#LOCAL VARIAABLES#
TYPE(TYPE_Block),POINTER :: temp
INTEGER :: Unit_

!!--begin--

IF( .NOT.PRESENT(fdbk) )THEN
 RETURN
END IF

Unit_ = OutputUnit(fdbk)

IF( ASSOCIATED(fdbk%CurrentLevel) )WRITE(Unit_,"(a)")"fdbk%CurrentLevel="//TRIM(STR(fdbk%CurrentLevel))
IF( ASSOCIATED(fdbk%ActiveLevel ) )WRITE(Unit_,"(a)")"fdbk%ActivelLevel="//TRIM(STR(fdbk%ActiveLevel ))
IF( ASSOCIATED(fdbk%PauseLevel  ) )WRITE(Unit_,"(a)")"fdbk%PauseLevel  ="//TRIM(STR(fdbk%PauseLevel  ))
IF( ASSOCIATED(fdbk%StopLevel   ) )WRITE(Unit_,"(a)")"fdbk%StopLevel   ="//TRIM(STR(fdbk%StopLevel   ))
IF( ASSOCIATED(fdbk%OutputUnit  ) )WRITE(Unit_,"(a)")"fdbk%OutputUnit  ="//TRIM(STR(fdbk%OutputUnit  ))

temp => fdbk%RootTrace

DO

 IF( .NOT.ASSOCIATED(temp) )EXIT

 WRITE(Unit_,'(a)')"fdbk%Prev = "//MERGE("ASSC"          ,"NULL",ASSOCIATED(temp%prev))
 WRITE(Unit_,'(a)')"fdbk%Next = "//MERGE("ASSC"          ,"NULL",ASSOCIATED(temp%next))

 IF( ASSOCIATED(temp%S) )THEN
  WRITE(Unit_,'(a,999a)')"fdbk%S    = ",temp%S
 ELSE
  WRITE(Unit_,'(a     )')"fdbk%S    = N/A"
 END IF

 IF( ASSOCIATED(temp%R) )THEN
  WRITE(Unit_,'(a,999g12.5)')"fdbk%R    = ",temp%R
 ELSE
  WRITE(Unit_,'(a         )')"fdbk%R    = N/A"
 END IF

 IF( ASSOCIATED(temp%I) )THEN
  WRITE(Unit_,'(a,999g12.5)')"fdbk%I    = ",temp%I
 ELSE
  WRITE(Unit_,'(a         )')"fdbk%I    = N/A"
 END IF

 IF( ASSOCIATED(temp%L) )THEN
  WRITE(Unit_,'(a,999g12.5)')"fdbk%L    = ",temp%L
 ELSE
  WRITE(Unit_,'(a         )')"fdbk%L    = N/A"
 END IF

 WRITE(Unit_,*)

 temp => temp%next
END DO

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<Dump_fdbk>>
SUBROUTINE Dump_fdbk( fdbk , UNIT , Columns , LineIndents )
!!#### PURPOSE
!! Dump the contents of fdbk.

!!#OPTIONAL INPUT/OUTPUT#
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN)    :: UNIT
INTEGER,OPTIONAL,INTENT(IN)    :: Columns
INTEGER,OPTIONAL,INTENT(IN)    :: LineIndents(1:2)

!!#LOCAL VARIABLES#
INTEGER                      :: LOCAL_UNIT,j
CHARACTER(LEN_S)    ,POINTER :: S(:)
TYPE(TYPE_Block)    ,POINTER :: old
CHARACTER(LEN_Prompt)        :: S_
TYPE(varying_string)         :: VS
INTEGER                      :: Columns_
INTEGER,SAVE                 :: DEFAULT_Columns=1000
!!--begin--

IF( PRESENT(Columns) )THEN
 Columns_ = Columns
ELSE
 Columns_ = DEFAULT_Columns
END IF

!! Kick out if feedback isn't present
IF( .NOT.PRESENT(fdbk) )RETURN

!! Kick out if feedback isn't allocated
IF( .NOT.ALLOCATED(fdbk) )RETURN

!! Kick out if feedback doesn't have info
IF( .NOT.HasInfo(fdbk) )RETURN

!! Kick out if feedback isn't active
IF( .NOT.IsActive(fdbk) )RETURN

!! Kick out if there is no information to return.
!! Determine the local unit.
IF( CompileDefaults )THEN
 LOCAL_UNIT = DEFAULT_OutputUnit
ELSE
 IF( PRESENT(UNIT) )THEN
  LOCAL_UNIT = UNIT
 ELSE
  LOCAL_UNIT = fdbk % OutputUnit
 ENDIF
ENDIF

![debug]CALL PRINT_fdbk(fdbk)

!! Go to the "root" of the problem.
fdbk%CurrentTrace => fdbk%RootTrace

!! Loop through list and dump everything.
DO

 IF( .NOT.ASSOCIATED(fdbk%CurrentTrace) )EXIT

 IF( ASSOCIATED(fdbk%CurrentTrace%S) )THEN

  !! Add integer tags.
  IF( ASSOCIATED(fdbk%CurrentTrace%I) )THEN
   DO j=1,SIZE(fdbk%CurrentTrace%I)
    CALL REPLACEString( Target        = "%I("//TRIM(STR(j))//")"           , &
                  OldString     = fdbk%CurrentTrace%S               , &
                  ReplaceWith   = TRIM(STR(fdbk%CurrentTrace%I(j))) , &
                  NewString     = fdbk%CurrentTrace%S               , &
                  CaseSensitive = .FALSE.                              )
   ENDDO
  ENDIF

  !! Add real tags.
  IF( ASSOCIATED(fdbk%CurrentTrace%R) )THEN
   DO j=1,SIZE(fdbk%CurrentTrace%R)
    CALL REPLACEString( Target        = "%R("//TRIM(STR(j))//")"           , &
                  OldString     = fdbk%CurrentTrace%S               , &
                  ReplaceWith   = TRIM(STR(fdbk%CurrentTrace%R(j))) , &
                  NewString     = fdbk%CurrentTrace%S               , &
                  CaseSensitive = .FALSE.                              )
   ENDDO
  ENDIF

  !! Add logical tags.
  IF( ASSOCIATED(fdbk%CurrentTrace%L) )THEN
   DO j=1,SIZE(fdbk%CurrentTrace%L)
    CALL REPLACEString( Target        = "%L("//TRIM(STR(j))//")"           , &
                  OldString     = fdbk%CurrentTrace%S               , &
                  ReplaceWith   = TRIM(STR(fdbk%CurrentTrace%L(j))) , &
                  NewString     = fdbk%CurrentTrace%S               , &
                  CaseSensitive = .FALSE.                              )
   ENDDO
  ENDIF

  !! Add error and warning conditions for integer, <%I>
  IF( ASSOCIATED(fdbk%CurrentTrace%I) )THEN
   IF( ANY(IsError(fdbk%CurrentTrace%I)) )THEN
    CALL Update_level( fdbk_error , fdbk )
   ELSE IF( ANY(IsWarning(fdbk%CurrentTrace%I)) )THEN
    CALL Update_level( fdbk_warning , fdbk )
   END IF
  END IF

  !! Add error and warning conditions for real, <%R>
  IF( ASSOCIATED(fdbk%CurrentTrace%R) )THEN
   IF( ANY(IsError(fdbk%CurrentTrace%R)) )THEN
    CALL Update_level( fdbk_error , fdbk )
   ELSE IF( ANY(IsWarning(fdbk%CurrentTrace%R)) )THEN
    CALL Update_level( fdbk_warning , fdbk )
   END IF
  END IF

  !! Now dump string <%S> by printing.
  IF(      fdbk%CurrentLevel>=fdbk_error   )THEN
   S_ = ErrorPrompt
  ELSE IF( fdbk%CurrentLevel>=fdbk_warning )THEN
   S_ = WarningPrompt
  ELSE
   S_ = InfoPrompt
  END IF

  S => fdbk%CurrentTrace%S
  VS = S_//Sentence(S)
  VS = TRIM(Smush(STR(VS)))

  CALL PRINT_Text( STR(VS) , &
     Unit=LOCAL_UNIT , Columns=Columns_ , LineIndents=LineIndents )

  S => NULL()

 END IF

 !! Now deallocate the current trace information (the information
 !! that was just dumped.)
 old => FdBk%CurrentTrace
 FdBk%CurrentTrace => FdBk%CurrentTrace%Next
 CALL DEALLOCATE( old )

END DO

!check if we should stop or Pause
IF( fdbk%CurrentLevel>=fdbk%StopLevel )THEN
 Call Stop(s=ErrorPrompt//"FATAL ERROR!!!",&
   OutputUnit=fdbk%OutputUnit)
ELSE IF( fdbk%CurrentLevel>=fdbk%PauseLevel )THEN
 Call Pause(s=InfoPrompt//"press <ENTER> to continue...",&
   OutputUnit=fdbk%OutputUnit)
END IF

!set old pointer
old => NULL()

!reallocate root trace and set current trace
CALL ALLOCATE(fdbk%RootTrace)
fdbk%CurrentTrace=>fdbk%RootTrace

!reset the feedback level
fdbk%CurrentLevel = fdbk_none

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<Update_Level_fdbk>>
SUBROUTINE Update_level_fdbk( level , fdbk )
!!#### PURPOSE
!! Update the level in feedback.

!!#DETAILS#
!! Always called from APPEND and Modify.

!!#OPTIONAL INPUT/OUTPUT#
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#REQUIRED INPUT#
INTEGER(KIND_I),INTENT(IN) :: level

!!--begin--

IF( PRESENT(fdbk) )THEN
 IF( .NOT.ALLOCATED(fdbk) )THEN
  CALL ALLOCATE( fdbk )
 END IF
 fdbk % CurrentLevel = MAX(level,fdbk % CurrentLevel)
END IF

!!--end--
END SUBROUTINE



!!### FUNCTION <<ALLOCATED_fdbk>>
FUNCTION ALLOCATED_fdbk( fdbk ) RESULT(ALLOCATED)
!!#### PURPOSE
!! Check whether or not fdbk is allocated.

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(IN) :: fdbk

!!#### REQUIRED OUTPUT
LOGICAL :: ALLOCATED

!!--begin--

!just choose one of the components to check if it is assocatiated
IF( PRESENT(fdbk) )THEN
 ALLOCATED = ASSOCIATED( fdbk%ActiveLevel )
ELSE
 ALLOCATED = .FALSE.
END IF

!!--end--
END FUNCTION


!!### SUBROUTINE <<Modify_fdbk>>
SUBROUTINE Modify_fdbk(fdbk,ActiveLevel,PauseLevel,StopLevel,OutputUnit)
!!#### PURPOSE
!! Changes some parameters of the feedback.

!!#OPTIONAL INPUT/OUTPUT#
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
INTEGER       ,INTENT(IN)   ,OPTIONAL :: ActiveLevel
INTEGER       ,INTENT(IN)   ,OPTIONAL :: PauseLevel
INTEGER       ,INTENT(IN)   ,OPTIONAL :: StopLevel
INTEGER       ,INTENT(IN)   ,OPTIONAL :: OutputUnit

!!--begin--

!kick out if the feedback isn"t present
IF( .NOT.PRESENT(fdbk) )RETURN

!automatic allocation
IF( .NOT.ALLOCATED(fdbk) )THEN
 CALL ALLOCATE(fdbk)
END IF

!change the levels
IF( PRESENT(ActiveLevel) ) fdbk%ActiveLevel = ActiveLevel
IF( PRESENT(PauseLevel ) ) fdbk%PauseLevel  = PauseLevel
IF( PRESENT(StopLevel  ) ) fdbk%StopLevel   = StopLevel
IF( PRESENT(OutputUnit ) ) fdbk%OutputUnit  = OutputUnit

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<ALLOCATE_fdbk>>
SUBROUTINE ALLOCATE_fdbk(fdbk,I,S,R,L)
!!#### PURPOSE
!! Allocate the fdbk type.

!!#OPTIONAL INPUT/OUTPUT#
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER     ,INTENT(IN),OPTIONAL :: I
CHARACTER(*),INTENT(IN),OPTIONAL :: S
REAL        ,INTENT(IN),OPTIONAL :: R
LOGICAL     ,INTENT(IN),OPTIONAL :: L

!!#LOCAL VARIABLES#
INTEGER :: jerr

!!--begin--
!! Kick out if <fdbk> is not present
IF( .NOT.PRESENT(fdbk) )RETURN

!allocate stuff
ALLOCATE( fdbk%CurrentLevel,STAT=jerr ) ; fdbk%CurrentLevel = fdbk_none
ALLOCATE( fdbk%ActiveLevel, STAT=jerr ) ; fdbk%ActiveLevel  = DEFAULT_ActiveLevel
ALLOCATE( fdbk%PauseLevel , STAT=jerr ) ; fdbk%PauseLevel   = DEFAULT_PauseLevel
ALLOCATE( fdbk%StopLevel  , STAT=jerr ) ; fdbk%StopLevel    = DEFAULT_StopLevel
ALLOCATE( fdbk%OutputUnit , STAT=jerr ) ; fdbk%OutputUnit   = DEFAULT_OutputUnit

!! Use the components to allocate
CALL ALLOCATE( fdbk%RootTrace , I=I , S=S  , R=R , L=L )

!! Set the current trace to the root trace
fdbk%CurrentTrace => fdbk%RootTrace

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<DEALLOCATE_fdbk>>
SUBROUTINE DEALLOCATE_fdbk(fdbk)
!!#### PURPOSE
!! Deallocate the fdbk type.

!!#OPTIONAL INPUT/OUTPUT#
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT)  :: fdbk

!!#LOCAL VARIABLES#
INTEGER :: jerr
TYPE(TYPE_Block),POINTER :: p

!!--begin--

!! Kick out if not allocated
IF( .NOT.ALLOCATED(fdbk) )RETURN

!deallocate the block linked lists
p => fdbk%RootTrace
DO WHILE( ASSOCIATED(p) )
 p => fdbk%CurrentTrace%next
 CALL DEALLOCATE(fdbk%CurrentTrace)
END DO

!deallocate the rest
DEALLOCATE( fdbk%CurrentLevel   , STAT=jerr )
DEALLOCATE( fdbk%ActiveLevel    , STAT=jerr )
DEALLOCATE( fdbk%PauseLevel     , STAT=jerr )
DEALLOCATE( fdbk%StopLevel      , STAT=jerr )
DEALLOCATE( fdbk%OutputUnit     , STAT=jerr )

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UpdateAndDump0_fdbk>>
SUBROUTINE UpdateAndDump0_fdbk( required_ActiveLevel , fdbk , I , S , R , L , &
  UNIT , Columns , LineIndents )

!!#### PURPOSE
!! Update and dump the contents of fdbk.

!!#REQUIRED INPUT#
!! * required feedback level to apply feedback
INTEGER(KIND_I),INTENT(IN) :: required_ActiveLevel

!!#OPTIONAL INPUT/OUTPUT#
!! * feedback structure
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * integer to add to feedback
!! * string to add to feedback
!! * real to add to feedback
!! * logical to add to feedback
INTEGER(KIND_I)     ,INTENT(IN) ,OPTIONAL :: I
CHARACTER(*,KIND_S) ,INTENT(IN) ,OPTIONAL :: S
REAL(KIND_R)        ,INTENT(IN) ,OPTIONAL :: R
LOGICAL(KIND_L)     ,INTENT(IN) ,OPTIONAL :: L
INTEGER           ,OPTIONAL,INTENT(IN)    :: UNIT
INTEGER           ,OPTIONAL,INTENT(IN)    :: Columns
INTEGER           ,OPTIONAL,INTENT(IN)    :: LineIndents(1:2)

!!--begin--
CALL Update0_fdbk( required_ActiveLevel , fdbk , I , S , R , L )
CALL Dump(fdbk,UNIT,Columns,LineIndents)
!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UpdateAndDump1_fdbk>>
SUBROUTINE UpdateAndDump1_fdbk( required_ActiveLevel , fdbk , I , S , R , L , &
  UNIT , Columns , LineIndents )
!!#### PURPOSE
!! Update feedback with some information.

!!#REQUIRED INPUT#
INTEGER(KIND_I),INTENT(IN)    :: required_ActiveLevel(1)

!!#OPTIONAL INPUT/OUTPUT#
!! * feedback structure
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * integer array to add to feedback
!! * string to add to feedback
!! * real array to add to feedback
!! * logical array to add to feedback
INTEGER(KIND_I)    ,INTENT(IN)   ,OPTIONAL :: I(:)
CHARACTER(*,KIND_S),INTENT(IN)   ,OPTIONAL :: S
REAL(KIND_R)       ,INTENT(IN)   ,OPTIONAL :: R(:)
LOGICAL(KIND_L)    ,INTENT(IN)   ,OPTIONAL :: L(:)
INTEGER            ,OPTIONAL,INTENT(IN)    :: UNIT
INTEGER            ,OPTIONAL,INTENT(IN)    :: Columns
INTEGER            ,OPTIONAL,INTENT(IN)    :: LineIndents(1:2)
!!--begin--
CALL Update1_fdbk( required_ActiveLevel , fdbk , I , S , R , L )
CALL Dump(fdbk,UNIT,Columns,LineIndents)
!!--end--
END SUBROUTINE


!!### SUBROUTINE <<Update0_fdbk>>
SUBROUTINE Update0_fdbk( required_ActiveLevel , fdbk , I , S , R , L )

!!#### PURPOSE
!! Update feedback with some information.

!!#REQUIRED INPUT#
!! * required feedback level to apply feedback
INTEGER(KIND_I),INTENT(IN) :: required_ActiveLevel

!!#OPTIONAL INPUT/OUTPUT#
!! * feedback structure
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * integer to add to feedback
!! * string to add to feedback
!! * real to add to feedback
!! * logical to add to feedback
INTEGER(KIND_I)     ,INTENT(IN) ,OPTIONAL :: I
CHARACTER(*,KIND_S) ,INTENT(IN) ,OPTIONAL :: S
REAL(KIND_R)        ,INTENT(IN) ,OPTIONAL :: R
LOGICAL(KIND_L)     ,INTENT(IN) ,OPTIONAL :: L

!!#LOCAL VARIABLES#
INTEGER(KIND_I) :: required_ActiveLevel_

!!--begin--
IF( required_ActiveLevel==fdbk_Choose )THEN
 IF( PRESENT(I) )THEN
  required_ActiveLevel_ = I
 ELSE
  required_ActiveLevel_ = fdbk_ERROR
 END IF
ELSE
 required_ActiveLevel_ = required_ActiveLevel
END IF

!if feedback isn't present, just print the error to the DEFAULT_OutputUnit
IF( .NOT.PRESENT(fdbk) )THEN
 IF( required_ActiveLevel_>=DEFAULT_ActiveLevel )THEN
  IF( PRESENT(I) ) WRITE(DEFAULT_OutputUnit,*) I
  IF( PRESENT(S) ) WRITE(DEFAULT_OutputUnit,*) S
  IF( PRESENT(R) ) WRITE(DEFAULT_OutputUnit,*) R
  IF( PRESENT(L) ) WRITE(DEFAULT_OutputUnit,*) L
 END IF
 RETURN

ELSE

 !automatic allocation
 IF( .NOT.ALLOCATED(fdbk) )THEN
  CALL ALLOCATE( fdbk )
 END IF

 !set the current level
 IF( required_ActiveLevel_>fdbk%CurrentLevel )THEN
  fdbk%CurrentLevel = required_ActiveLevel
 END IF

 !if feedback is now active
 IF( IsActive(fdbk) )THEN

  !append and move to next
  IF( ASSOCIATED(fdbk%CurrentTrace) )THEN
   CALL APPEND( fdbk%CurrentTrace , I=I , S=S , R=R , L=L )
  ELSE
   CALL OVERWRITE( fdbk%CurrentTrace , I=I , S=S , R=R , L=L )
  END IF
  fdbk%CurrentTrace => fdbk%CurrentTrace % Next

 END IF

END IF

!!--end--
END SUBROUTINE


!!### SUBROUTINE <<Update1_fdbk>>
SUBROUTINE Update1_fdbk( required_ActiveLevel , fdbk , I , S , R , L )
!!#### PURPOSE
!! Update feedback with some information.

!!#REQUIRED INPUT#
INTEGER(KIND_I),INTENT(IN)    :: required_ActiveLevel(1)

!!#OPTIONAL INPUT/OUTPUT#
!! * feedback structure
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * integer array to add to feedback
!! * string to add to feedback
!! * real array to add to feedback
!! * logical array to add to feedback
INTEGER(KIND_I)    ,INTENT(IN)   ,OPTIONAL :: I(:)
CHARACTER(*,KIND_S),INTENT(IN)   ,OPTIONAL :: S
REAL(KIND_R)       ,INTENT(IN)   ,OPTIONAL :: R(:)
LOGICAL(KIND_L)    ,INTENT(IN)   ,OPTIONAL :: L(:)

!!--begin--

!if feedback isn't present, just print the error to the DEFAULT_OutputUnit
IF( .NOT.PRESENT(fdbk) )THEN
 IF( required_ActiveLevel(1)>=DEFAULT_ActiveLevel )THEN
  IF( PRESENT(I) ) WRITE(DEFAULT_OutputUnit,*) I
  IF( PRESENT(S) ) WRITE(DEFAULT_OutputUnit,*) S
  IF( PRESENT(R) ) WRITE(DEFAULT_OutputUnit,*) R
  IF( PRESENT(L) ) WRITE(DEFAULT_OutputUnit,*) L
 END IF
 RETURN

ELSE

 !automatic allocation
 IF( .NOT.ALLOCATED(fdbk) )THEN
  CALL ALLOCATE( fdbk )
 END IF

 !set the current level
 IF( required_ActiveLevel(1)>fdbk%CurrentLevel )THEN
  fdbk%CurrentLevel = required_ActiveLevel(1)
 END IF

 !if feedback is now active
 IF( IsActive(fdbk) )THEN

  !append and move to next
  IF( ASSOCIATED(fdbk%CurrentTrace) )THEN
   CALL APPEND( fdbk%CurrentTrace , I=I , S=S , R=R , L=L )
  ELSE
   CALL OVERWRITE( fdbk%CurrentTrace , I=I , S=S , R=R , L=L )
  END IF
  fdbk%CurrentTrace => fdbk%CurrentTrace % Next

 END IF

END IF

!!--end--
END SUBROUTINE


!!### FUNCTION <<IsError_fdbk>>
FUNCTION IsError_fdbk( fdbk ) RESULT(IsError)
!!#### PURPOSE
!! Tests for fdbk equal to an error condition.

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(IN) :: fdbk

!!#### REQUIRED OUTPUT
LOGICAL :: IsError

!!--begin--

!check if the level is greater than or equal to the error level
IF( PRESENT(fdbk) )THEN
 IsError = fdbk%CurrentLevel >= fdbk_error
ELSE
 IsError = .FALSE.
ENDIF

!!--end--
END FUNCTION


!!### FUNCTION <<IsComment_fdbk>>
FUNCTION IsComment_fdbk( fdbk ) RESULT(IsComment)
!!#### PURPOSE
!! Tests for fdbk equal to a comment condition

!!#### REQUIRED OUTPUT
LOGICAL :: IsComment

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(IN) :: fdbk

!!--begin--

!check if the level is less than the warning level
IF( PRESENT(fdbk) )THEN
 IsComment = fdbk%CurrentLevel < fdbk_warning
ELSE
 IsComment = .FALSE.
ENDIF

!!--end--
END FUNCTION


!!### FUNCTION <<IsWarning_fdbk>>
FUNCTION IsWarning_fdbk( fdbk ) RESULT(IsWarning)
!!#### PURPOSE
!! Tests for fdbk equal to a warning condition.

!!#### REQUIRED OUTPUT
LOGICAL :: IsWarning

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(IN) :: fdbk

!!--begin--

!check if the level is greater than the warning condition
IF( PRESENT(fdbk) )THEN
 IsWarning = fdbk%CurrentLevel >= fdbk_warning
ELSE
 IsWarning = .FALSE.
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<IsActive_fdbk>>
FUNCTION IsActive_fdbk( fdbk ) RESULT(IsActive)
!!#### PURPOSE
!! Tests for fdbk equal to an active condition.

!!#### REQUIRED OUTPUT
LOGICAL :: IsActive

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(IN) :: fdbk

!!--begin--

!check if the level is greater than the active level (whatever it is)
IF( PRESENT(fdbk) )THEN
 IsActive = fdbk%CurrentLevel >= fdbk%ActiveLevel
ELSE
 IsActive = .FALSE.
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<HasInfo_fdbk>>
PURE FUNCTION HasInfo_fdbk( fdbk ) RESULT(HasInfo)
!!#### PURPOSE
!! Tests for fdbk having any info.

!!#### REQUIRED OUTPUT
LOGICAL :: HasInfo

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(IN) :: fdbk

!!--begin--

!check if we have any info
IF( PRESENT(fdbk) )THEN
 HasInfo = fdbk%CurrentLevel>fdbk_none
ELSE
 HasInfo = .FALSE.
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<OutputUnit_fdbk>>
PURE FUNCTION OutputUnit_fdbk( fdbk ) RESULT(Unit)
!!#### PURPOSE
!! Returns the unit that feedback is outputting to.

!!#### REQUIRED OUTPUT
INTEGER :: Unit

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(IN) :: fdbk

!!--begin--

!return the output unit
IF( PRESENT(fdbk) )THEN
 Unit = fdbk%OutputUnit
ELSE
 Unit = DEFAULT_OutputUnit
END IF

!!--end--
END FUNCTION


END MODULE
