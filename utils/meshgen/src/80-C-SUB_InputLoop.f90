!!# SUBROUTINE MODULE >>InputLoop<<
MODULE SUB_InputLoop

!!## PURPOSE
!! With the <TBX_SIO> package, files are read line by line, commands
!! are recognized and deferred to so-called "SWITCHBOARD" routines.  
!! This <InputLoop> subroutine takes a <SWITCHBOARD> subroutine 
!! as an argument.


!!## AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com | 2006


!!## EXTERNAL
USE KND_IntrinsicTypes,ONLY: KIND_Sfile,KIND_S !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_STR                                    !!((05-B-FUN_STR.f90))
USE ISO_varying_string                         !!((03-A-ISO_varying_string.f90))
USE USR_fdbk                                   !!((08-C-USR_fdbk.f90))
USE TBX_SIO                                    !!((10-A-TBX_SIO.f90))
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## IDENTIFICATION
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: file_ = "80-C-SUB_InputLoop.f90"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: mod_  = "SUB_InputLoop"

!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ROUTINES
PUBLIC :: InputLoop


!!## MODULE PROCEDURES
CONTAINS


!!### SUBROUTINE: <InputLoop>
SUBROUTINE InputLoop(SIO,file,SWITCHBOARD,fdbk,keepalive,HEAD)

!!#### REQUIRED INPUT
TYPE(TYPE_sio),POINTER    :: SIO
CHARACTER(*)  ,INTENT(IN) :: FILE

!!#### OPTIONAL INPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
LOGICAL        ,INTENT(IN)   ,OPTIONAL :: keepalive
CHARACTER(*)   ,INTENT(IN)   ,OPTIONAL :: HEAD

!!#### INTERFACES
INTERFACE
 SUBROUTINE SWITCHBOARD(SIO,fdbk)
 USE USR_fdbk                                  !!((08-C-USR_fdbk.f90))
 USE USR_SIO                                   !!((09-A-USR_SIO.f90))
 TYPE(TYPE_sio),POINTER :: SIO
 TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
 END SUBROUTINE
END INTERFACE


!!#### LOCAL  VARIABLES
CHARACTER(72) :: head_

!!--begin--

CALL CLEAR(head_)
IF( PRESENT(HEAD) )THEN
 head_ = HEAD
END IF

!
!! * Connect to the control file.
CALL CONNECT(SIO,ACTION  = "Read"      ,&
                 FORM    = "Formatted" ,&
                 FILE    = TRIM(file)  ,&
                 ACCESS  = "Sequential",&
                 fdbk    = fdbk)
CALL DUMP(fdbk)
!
!! * Enter the input loop.
DO
 !
 !! * Read a command and arguments into .
 CALL READ_COMMAND(SIO,fdbk,keepalive=keepalive)
 !
 !! * Kick out when [SIO] destroys itself (or reaches end of a file) in case the
 !!    SIO is structure is kept alive.
 IF( PRESENT(keepalive) )THEN
  IF( keepalive )THEN
   IF( SIO%end_of_file .AND. .NOT.ASSOCIATED(SIO%prev) )EXIT
  ELSE
   IF( .NOT.ASSOCIATED(SIO) )EXIT
  END IF
 ELSE
  IF( .NOT.ASSOCIATED(SIO) )EXIT
 END IF
 !
 !! * Print a message saying the command
 !!    card being read, line number, etc.
 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(head_)//" Reading &
                & [card="//TRIM(STR(SIO%cmd))//"]"//&
             " on [line="//TRIM(STR(SIO%line_num))//"]"//&
             " of [file="//TRIM(STR(SIO%file))//"]..." )
 !
 !! * Enter in to the database of command cards.
 CALL SWITCHBOARD(SIO,fdbk)

END DO

!!--end--
END SUBROUTINE


END MODULE
