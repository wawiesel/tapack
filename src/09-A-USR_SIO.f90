!!# MODULE <<USR_SIO>>
MODULE USR_SIO

!!## PURPOSE
!! Provide users with a simple yet powerful Serial
!! Input/Output system.

!!## AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.Cmd


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L1,KIND_L2,KIND_L4,& !!((01-A-KND_IntrinsicTypes.f90))
                  KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
                  KIND_Rsp,KIND_Rdp,&
                  KIND_Csp,KIND_Cdp,&
                  KIND_S,KIND_Sen,KIND_Sfile,KIND_Sfmt

!!## EXTERNAL PARAMETERS
USE PAR_Units                                          !!((02-A-PAR_Units.f90))

!!## GLOBAL ASSIGNMENTS
USE ASN_IntrinsicS                                     !!((05-B-ASN_IntrinsicS.f90))

!!## GLOBAL BINARY OPERATORS
USE BOP_sEQ                                            !!((03-A-BOP_sEQ.f90))

!!## FORTRAN STANDARDS
USE ISO_varying_string                                 !!((03-A-ISO_varying_string.f90))

!!## GLOBAL FUNCTIONS
USE FUN_Sentence                                       !!((04-B-FUN_Sentence.f90))
USE FUN_HashP,Hash=>HashP                              !!((05-B-FUN_HashP.f90))
USE FUN_NewUnit                                        !!((04-B-FUN_NewUnit.f90))
USE FUN_INDEXa                                         !!((08-B-FUN_INDEXa.f90))
USE FUN_FMT                                            !!((06-B-FUN_FMT.f90))
USE FUN_Error                                          !!((04-A-FUN_Error.f90))
USE FUN_Warning                                        !!((04-B-FUN_Warning.f90))
USE FUN_INT                                            !!((06-B-FUN_INT.f90))
USE FUN_LOGICAL                                        !!((06-B-FUN_LOGICAL.f90))
USE FUN_REAL                                           !!((06-B-FUN_REAL.f90))
USE FUN_CMPLX                                          !!((06-B-FUN_CMPLX.f90))
USE SUB_Find                                           !!((05-B-SUB_Find.f90))
USE SUB_Reallocate                                     !!((04-B-SUB_Reallocate.f90))
USE SUB_CLEAR                                          !!((04-A-SUB_CLEAR.f90))
USE SUB_CLEARn                                         !!((04-A-SUB_CLEARn.f90))
USE SUB_Words                                          !!((03-A-SUB_Words.f90))
USE FUN_STR                                            !!((05-B-FUN_STR.f90))
USE FUN_Default                                        !!((04-A-FUN_Default.f90))
USE FUN_ptr_STR                                        !!((06-B-FUN_ptr_STR.f90))
USE FUN_STRn                                           !!((07-B-FUN_STRn.f90))
USE FUN_NewFile                                        !!((05-B-FUN_NewFile.f90))

!!## GLOBAL USER MODULES
USE USR_Block                                          !!((05-B-USR_Block.f90))
USE USR_fdbk                                           !!((08-C-USR_fdbk.f90))

!!## GLOBAL LIBRARIES
USE LIB_GenericPhrases                                 !!((07-B-LIB_GenericPhrases.f90))

!!## GLOBAL VARIABLES
USE VAR_Units                                          !!((03-A-VAR_Units.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## FMT PROCEDURE OVERLOADING
INTERFACE FMT
 MODULE PROCEDURE FMT_SIO_A0S
 MODULE PROCEDURE FMT_SIO_A0I1
 MODULE PROCEDURE FMT_SIO_A0I2
 MODULE PROCEDURE FMT_SIO_A0I4
 MODULE PROCEDURE FMT_SIO_A0I8
 MODULE PROCEDURE FMT_SIO_A0Csp
 MODULE PROCEDURE FMT_SIO_A0Cdp
 MODULE PROCEDURE FMT_SIO_A0Rsp
 MODULE PROCEDURE FMT_SIO_A0Rdp
 MODULE PROCEDURE FMT_SIO_A0L1
 MODULE PROCEDURE FMT_SIO_A0L2
 MODULE PROCEDURE FMT_SIO_A0L4
 MODULE PROCEDURE FMT_SIO_A1S
 MODULE PROCEDURE FMT_SIO_A1I1
 MODULE PROCEDURE FMT_SIO_A1I2
 MODULE PROCEDURE FMT_SIO_A1I4
 MODULE PROCEDURE FMT_SIO_A1I8
 MODULE PROCEDURE FMT_SIO_A1Csp
 MODULE PROCEDURE FMT_SIO_A1Cdp
 MODULE PROCEDURE FMT_SIO_A1Rsp
 MODULE PROCEDURE FMT_SIO_A1Rdp
 MODULE PROCEDURE FMT_SIO_A1L1
 MODULE PROCEDURE FMT_SIO_A1L2
 MODULE PROCEDURE FMT_SIO_A1L4
 MODULE PROCEDURE FMT_SIO_A2S
 MODULE PROCEDURE FMT_SIO_A2I1
 MODULE PROCEDURE FMT_SIO_A2I2
 MODULE PROCEDURE FMT_SIO_A2I4
 MODULE PROCEDURE FMT_SIO_A2I8
 MODULE PROCEDURE FMT_SIO_A2Csp
 MODULE PROCEDURE FMT_SIO_A2Cdp
 MODULE PROCEDURE FMT_SIO_A2Rsp
 MODULE PROCEDURE FMT_SIO_A2Rdp
 MODULE PROCEDURE FMT_SIO_A2L1
 MODULE PROCEDURE FMT_SIO_A2L2
 MODULE PROCEDURE FMT_SIO_A2L4
END INTERFACE



INTERFACE ARGUMENT
 MODULE PROCEDURE ARGUMENT_A0S
 MODULE PROCEDURE ARGUMENT_A0VS
 MODULE PROCEDURE ARGUMENT_A0I1
 MODULE PROCEDURE ARGUMENT_A0I2
 MODULE PROCEDURE ARGUMENT_A0I4
 MODULE PROCEDURE ARGUMENT_A0I8
 MODULE PROCEDURE ARGUMENT_A0Csp
 MODULE PROCEDURE ARGUMENT_A0Cdp
 MODULE PROCEDURE ARGUMENT_A0Rsp
 MODULE PROCEDURE ARGUMENT_A0Rdp
 MODULE PROCEDURE ARGUMENT_A0L1
 MODULE PROCEDURE ARGUMENT_A0L2
 MODULE PROCEDURE ARGUMENT_A0L4
 MODULE PROCEDURE ARGUMENT_A1S
 MODULE PROCEDURE ARGUMENT_A1VS
 MODULE PROCEDURE ARGUMENT_A1I1
 MODULE PROCEDURE ARGUMENT_A1I2
 MODULE PROCEDURE ARGUMENT_A1I4
 MODULE PROCEDURE ARGUMENT_A1I8
 MODULE PROCEDURE ARGUMENT_A1Csp
 MODULE PROCEDURE ARGUMENT_A1Cdp
 MODULE PROCEDURE ARGUMENT_A1Rsp
 MODULE PROCEDURE ARGUMENT_A1Rdp
 MODULE PROCEDURE ARGUMENT_A1L1
 MODULE PROCEDURE ARGUMENT_A1L2
 MODULE PROCEDURE ARGUMENT_A1L4
END INTERFACE

INTERFACE READ_ARGUMENT
 MODULE PROCEDURE READ_ARGUMENT_A0S
 MODULE PROCEDURE READ_ARGUMENT_A0VS
 MODULE PROCEDURE READ_ARGUMENT_A0Csp
 MODULE PROCEDURE READ_ARGUMENT_A0Cdp
 MODULE PROCEDURE READ_ARGUMENT_A0Rsp
 MODULE PROCEDURE READ_ARGUMENT_A0Rdp
 MODULE PROCEDURE READ_ARGUMENT_A0L1
 MODULE PROCEDURE READ_ARGUMENT_A0L2
 MODULE PROCEDURE READ_ARGUMENT_A0L4
 MODULE PROCEDURE READ_ARGUMENT_A0I1
 MODULE PROCEDURE READ_ARGUMENT_A0I2
 MODULE PROCEDURE READ_ARGUMENT_A0I4
 MODULE PROCEDURE READ_ARGUMENT_A0I8
 MODULE PROCEDURE READ_ARGUMENT_A1S
 MODULE PROCEDURE READ_ARGUMENT_A1VS
 MODULE PROCEDURE READ_ARGUMENT_A1Csp
 MODULE PROCEDURE READ_ARGUMENT_A1Cdp
 MODULE PROCEDURE READ_ARGUMENT_A1Rsp
 MODULE PROCEDURE READ_ARGUMENT_A1Rdp
 MODULE PROCEDURE READ_ARGUMENT_A1L1
 MODULE PROCEDURE READ_ARGUMENT_A1L2
 MODULE PROCEDURE READ_ARGUMENT_A1L4
 MODULE PROCEDURE READ_ARGUMENT_A1I1
 MODULE PROCEDURE READ_ARGUMENT_A1I2
 MODULE PROCEDURE READ_ARGUMENT_A1I4
 MODULE PROCEDURE READ_ARGUMENT_A1I8
END INTERFACE

INTERFACE WRITE_ARGUMENT
 MODULE PROCEDURE WRITE_ARGUMENT_A0S
 MODULE PROCEDURE WRITE_ARGUMENT_A0VS
 MODULE PROCEDURE WRITE_ARGUMENT_A0Csp
 MODULE PROCEDURE WRITE_ARGUMENT_A0Cdp
 MODULE PROCEDURE WRITE_ARGUMENT_A0Rsp
 MODULE PROCEDURE WRITE_ARGUMENT_A0Rdp
 MODULE PROCEDURE WRITE_ARGUMENT_A0L1
 MODULE PROCEDURE WRITE_ARGUMENT_A0L2
 MODULE PROCEDURE WRITE_ARGUMENT_A0L4
 MODULE PROCEDURE WRITE_ARGUMENT_A0I1
 MODULE PROCEDURE WRITE_ARGUMENT_A0I2
 MODULE PROCEDURE WRITE_ARGUMENT_A0I4
 MODULE PROCEDURE WRITE_ARGUMENT_A0I8
 MODULE PROCEDURE WRITE_ARGUMENT_A1S
 MODULE PROCEDURE WRITE_ARGUMENT_A1VS
 MODULE PROCEDURE WRITE_ARGUMENT_A1Csp
 MODULE PROCEDURE WRITE_ARGUMENT_A1Cdp
 MODULE PROCEDURE WRITE_ARGUMENT_A1Rsp
 MODULE PROCEDURE WRITE_ARGUMENT_A1Rdp
 MODULE PROCEDURE WRITE_ARGUMENT_A1L1
 MODULE PROCEDURE WRITE_ARGUMENT_A1L2
 MODULE PROCEDURE WRITE_ARGUMENT_A1L4
 MODULE PROCEDURE WRITE_ARGUMENT_A1I1
 MODULE PROCEDURE WRITE_ARGUMENT_A1I2
 MODULE PROCEDURE WRITE_ARGUMENT_A1I4
 MODULE PROCEDURE WRITE_ARGUMENT_A1I8
END INTERFACE


!!#### READ_CHARS PROCEDURE OVERLOADING
INTERFACE READ_CHARS
 MODULE PROCEDURE READ_CHARS_
END INTERFACE

!!#### WRITE_CHARS PROCEDURE OVERLOADING
INTERFACE WRITE_CHARS
 MODULE PROCEDURE WRITE_CHARS_
END INTERFACE

!!#### Connect PROCEDURE OVERLOADING
INTERFACE CONNECT
 MODULE PROCEDURE CONNECT__
END INTERFACE

INTERFACE TERMINAL_CONNECT
 MODULE PROCEDURE TERMINAL__
END INTERFACE

!!#### Disconnect PROCEDURE OVERLOADING
INTERFACE DISCONNECT
 MODULE PROCEDURE DISCONNECT__
END INTERFACE

INTERFACE READ_COMMAND
 MODULE PROCEDURE READ_COMMAND_
END INTERFACE

INTERFACE PARSE_COMMAND
 MODULE PROCEDURE PARSE_COMMAND_
END INTERFACE

INTERFACE WRITE_COMMAND
 MODULE PROCEDURE WRITE_COMMAND_
END INTERFACE

INTERFACE BEGIN_ARGUMENTS
 MODULE PROCEDURE BEGIN_ARGUMENTS_
END INTERFACE

INTERFACE END_ARGUMENTS
 MODULE PROCEDURE END_ARGUMENTS_
END INTERFACE



!!## MAIN SEQUENCE
!! * connect
PUBLIC :: CONNECT
PUBLIC :: TERMINAL_CONNECT
!! * read/write a command
PUBLIC :: READ_COMMAND
PUBLIC :: WRITE_COMMAND
!! * read/write Arguments
PUBLIC :: BEGIN_ARGUMENTS
PUBLIC :: ARGUMENT
PUBLIC :: END_ARGUMENTS
!! * disconnect
PUBLIC :: DISCONNECT

!!## SIMPLE INQUIRY
PUBLIC :: Reading
PUBLIC :: Writing
PUBLIC :: Formatted
PUBLIC :: Unformatted
PUBLIC :: Stopped

!!## OTHER PROCEDURES
PUBLIC :: FMT

PUBLIC :: READ_ARGUMENT
PUBLIC :: WRITE_ARGUMENT
PUBLIC :: PARSE_COMMAND
PUBLIC :: WRITE_CHARS
PUBLIC :: READ_CHARS

PUBLIC :: ALLOCATE_CONNECTION
PUBLIC :: DEALLOCATE_CONNECTION
PUBLIC :: NULLIFY_CONNECTION
PUBLIC :: ASSOCIATE_CONNECTION

PUBLIC :: ALLOCATE_SYNTAX
PUBLIC :: DEALLOCATE_SYNTAX
PUBLIC :: NULLIFY_SYNTAX
PUBLIC :: ASSOCIATE_SYNTAX

PUBLIC :: ALLOCATE_FORMAT
PUBLIC :: DEALLOCATE_FORMAT
PUBLIC :: NULLIFY_FORMAT
PUBLIC :: ASSOCIATE_FORMAT

PUBLIC :: ALLOCATE_OPTIONS
PUBLIC :: DEALLOCATE_OPTIONS
PUBLIC :: COPY_OPTIONS

PUBLIC :: ALLOCATE_COMMAND
PUBLIC :: DEALLOCATE_COMMAND

PUBLIC :: TYPE_SIO
PUBLIC :: PRINT_SIO

PUBLIC :: INQUIRE_FILE
PUBLIC :: INQUIRE_ACTION
PUBLIC :: INQUIRE_ACCESS
PUBLIC :: INQUIRE_BLANK
PUBLIC :: INQUIRE_DELIM
PUBLIC :: INQUIRE_EXIST
PUBLIC :: INQUIRE_FORM
PUBLIC :: INQUIRE_PAD
PUBLIC :: INQUIRE_POSITION
PUBLIC :: INQUIRE_RECL

PUBLIC :: LEN_FMT

PUBLIC :: SIO_ADD_BASE

! * ask for next line when reading
PUBLIC :: getNextLine
! * end a line when writing
PUBLIC :: putEndofLine
! * stop a line when writing
PUBLIC :: putStopper

!!## IDENTIFICATION
CHARACTER(LEN=*,KIND=KIND_Sen)  ,PARAMETER :: mod_="USR_SIO"
CHARACTER(LEN=*,KIND=KIND_Sfile),PARAMETER :: file_="09-A-USR_SIO.f90"
PRIVATE :: mod_,file_


!!## LOCAL PARAMETERS
!! *1 maxmimum length of the argument delimiters
INTEGER,PARAMETER :: MAX_LEN_ArgDelim = 2
!! *2 lengths
!!   *1 connection spec
INTEGER,PARAMETER :: LEN_CONNECTION   = 32
!!   *2 format spec
INTEGER,PARAMETER :: LEN_FMT          = 32
!!   *3 length of an argument name
INTEGER,PARAMETER :: LEN_ArgNm        = 32
!! *3 default values for options
LOGICAL,PARAMETER :: DEFAULT_using_ArgDefine = .FALSE.
INTEGER,PARAMETER :: DEFAULT_echo_unit       = window_unit
INTEGER,PARAMETER :: DEFAULT_interpretter    = 0
LOGICAL,PARAMETER :: DEFAULT_auto_memory     = .TRUE.
!! *4 default value for noisy (debugging output)
LOGICAL,PARAMETER :: Noisy_           = .FALSE.
!! *5 command primary: <"SIO">
INTEGER,PARAMETER :: SIO_     = 0000004934
!! *6 command secondaries: <"SYNTAX">, <"FORMAT">, and <"CONNECT">
INTEGER,PARAMETER :: connect_ = 0000064368
INTEGER,PARAMETER :: format_  = 0000040739
INTEGER,PARAMETER :: syntax_  = 0000062691
!! *
CHARACTER(3),SAVE :: SIO_ADD_BASE = "   "


!!## TYPE DEFINTION: SIO (Sequential Input/Output)
TYPE TYPE_SIO

 !!### CONNECTION I (these are initialized or set with CONNECTION)
 TYPE(varying_string)        ,POINTER :: FILE     => NULL()
 CHARACTER(LEN_CONNECTION)   ,POINTER :: FORM     => NULL()
 CHARACTER(LEN_CONNECTION)   ,POINTER :: ACTION   => NULL()
 CHARACTER(LEN_CONNECTION)   ,POINTER :: ACCESS   => NULL()
 CHARACTER(LEN_CONNECTION)   ,POINTER :: STATUS   => NULL()
 CHARACTER(LEN_CONNECTION)   ,POINTER :: POSITION => NULL()
 CHARACTER(LEN_CONNECTION)   ,POINTER :: BLANK    => NULL()
 CHARACTER(LEN_CONNECTION)   ,POINTER :: PAD      => NULL()
 INTEGER                     ,POINTER :: RECL     => NULL()
 CHARACTER(LEN_CONNECTION)   ,POINTER :: DELIM    => NULL()

 !!### CONNECTION II (these are a consequence of current connection)
 INTEGER                     ,POINTER :: UNIT              => NULL()
 INTEGER                     ,POINTER :: IOSTAT            => NULL()
 INTEGER                     ,POINTER :: SIZE              => NULL()
 CHARACTER                   ,POINTER :: Char              => NULL()
 INTEGER                     ,POINTER :: line_num          => NULL()
 INTEGER                     ,POINTER :: column_num        => NULL()
 INTEGER                     ,POINTER :: record_num        => NULL()
 LOGICAL                     ,POINTER :: end_of_line       => NULL()
 LOGICAL                     ,POINTER :: end_of_file       => NULL()
 LOGICAL                     ,POINTER :: shadow_connection => NULL()


 !!### SYNTAX I (initialized or set with SYNTAX)
 CHARACTER                   ,POINTER :: CmdBeg       => NULL()
 CHARACTER                   ,POINTER :: ArgBeg       => NULL()
 CHARACTER                   ,POINTER :: Argdefault   => NULL()
 CHARACTER                   ,POINTER :: ArgDefine    => NULL()
 CHARACTER(MAX_LEN_ArgDelim) ,POINTER :: ArgDelim     => NULL()
 CHARACTER                   ,POINTER :: SubArgDelim  => NULL()
 CHARACTER                   ,POINTER :: ArgEnd       => NULL()
 CHARACTER                   ,POINTER :: Ignore       => NULL()
 CHARACTER                   ,POINTER :: Comment      => NULL()
 CHARACTER                   ,POINTER :: Continue     => NULL()
 CHARACTER                   ,POINTER :: Stop         => NULL()
 CHARACTER                   ,POINTER :: StrTag       => NULL()

 !!### SYNTAX II (consequence of the current connection)
 INTEGER                     ,POINTER :: LEN_ArgDelim  => NULL()
 LOGICAL                     ,POINTER :: shadow_syntax => NULL()


 !!### FORMAT I (initialized or set with FORMAT)
 CHARACTER(LEN_FMT)          ,POINTER :: FMT_L  => NULL()
 CHARACTER(LEN_FMT)          ,POINTER :: FMT_I1 => NULL()
 CHARACTER(LEN_FMT)          ,POINTER :: FMT_I2 => NULL()
 CHARACTER(LEN_FMT)          ,POINTER :: FMT_I4 => NULL()
 CHARACTER(LEN_FMT)          ,POINTER :: FMT_I8 => NULL()
 CHARACTER(LEN_FMT)          ,POINTER :: FMT_sp => NULL()
 CHARACTER(LEN_FMT)          ,POINTER :: FMT_dp => NULL()

 !!### FORMAT II (consequence of current connection)
 LOGICAL                     ,POINTER :: shadow_format =>NULL()


 !!### COMMAND I (initialize or SET with Cmd)
 TYPE(varying_string)        ,POINTER :: Cmd     => NULL()
 INTEGER                     ,POINTER :: CurrArg => NULL()

 !!### COMMAND II (consequence of current connection)
 INTEGER                     ,POINTER :: hash(:)     => NULL()
 TYPE(varying_string)        ,POINTER :: splitCmd(:) => NULL()
 TYPE(varying_string)        ,POINTER :: Arg(:)      => NULL()
 CHARACTER(LEN_ArgNm)        ,POINTER :: ArgNm(:)    => NULL()
 INTEGER                     ,POINTER :: NSubArg(:)  => NULL()
 LOGICAL                     ,POINTER :: Optional(:) => NULL()
 LOGICAL                     ,POINTER :: Present(:)  => NULL()
 LOGICAL                     ,POINTER :: IsMacro     => NULL()

 !!### LINKED-LIST pointer to the previous SIO object
 TYPE(TYPE_SIO)               ,POINTER :: prev => NULL()

 !!### OPTIONS
 !! * If action=write then <ArgDefine-statements> are output.
 !!   If action=read then <ArgDefine-statements> are being used in input.
 LOGICAL                    ,POINTER :: using_ArgDefine => NULL()
 !!
 !! * If action=read and <echo_unit>$>0$ then echo commands to <echo_unit>.
 INTEGER                    ,POINTER :: echo_unit => NULL()
 !!
 !! * If action=read and <interpretter>$>0$ then interpretter commands
 !!   will be read from the terminal.
 INTEGER                    ,POINTER :: interpretter => NULL()
 !!
 !! * Argument/DataBlock variables are allocated/deallocated automatically
 !!   to avoid memory leaks.
 LOGICAL                    ,POINTER :: auto_memory => NULL()

END TYPE


!!## CONTAINED PROCEDURES
CONTAINS


SUBROUTINE PRINT_SIO( SIO , Unit )
!!#### PURPOSE
!! Print out the contents of an <SIO> object.

!!#### REQUIRED INPUT
TYPE(TYPE_SIO),INTENT(IN) :: SIO

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_

!!--begin--
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )


WRITE(Unit_,*)" "
WRITE(Unit_,*)" "
WRITE(Unit_,*)"##TYPE DEFINITION: SIO (Sequential Input/Output)##"
WRITE(Unit_,*)" "
WRITE(Unit_,*)" "
WRITE(Unit_,*)"TYPE TYPE_SIO"
WRITE(Unit_,*)" "
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #CONNECTION I (these are initialized or set with CONNECTION)#"
WRITE(Unit_,*)" TYPE(varying_string)        ,POINTER :: FILE     => "//TRIM(STRn(SIO%FILE))
WRITE(Unit_,*)" CHARACTER(LEN_CONNECTION)   ,POINTER :: FORM     => "//TRIM(STRn(SIO%FORM))
WRITE(Unit_,*)" CHARACTER(LEN_CONNECTION)   ,POINTER :: ACTION   => "//TRIM(STRn(SIO%ACTION))
WRITE(Unit_,*)" CHARACTER(LEN_CONNECTION)   ,POINTER :: ACCESS   => "//TRIM(STRn(SIO%ACCESS))
WRITE(Unit_,*)" CHARACTER(LEN_CONNECTION)   ,POINTER :: STATUS   => "//TRIM(STRn(SIO%STATUS))
WRITE(Unit_,*)" CHARACTER(LEN_CONNECTION)   ,POINTER :: POSITION => "//TRIM(STRn(SIO%POSITION))
WRITE(Unit_,*)" CHARACTER(LEN_CONNECTION)   ,POINTER :: BLANK    => "//TRIM(STRn(SIO%BLANK))
WRITE(Unit_,*)" CHARACTER(LEN_CONNECTION)   ,POINTER :: PAD      => "//TRIM(STRn(SIO%PAD))
WRITE(Unit_,*)" INTEGER                     ,POINTER :: RECL     => "//TRIM(STRn(SIO%RECL))
WRITE(Unit_,*)" CHARACTER(LEN_CONNECTION)   ,POINTER :: DELIM    => "//TRIM(STRn(SIO%DELIM))
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #CONNECTION II (these are a consequence of current connection)#"
WRITE(Unit_,*)" INTEGER                     ,POINTER :: UNIT              => "//TRIM(STRn(SIO%UNIT))
WRITE(Unit_,*)" INTEGER                     ,POINTER :: IOSTAT            => "//TRIM(STRn(SIO%IOSTAT))
WRITE(Unit_,*)" INTEGER                     ,POINTER :: SIZE              => "//TRIM(STRn(SIO%SIZE))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: Char              => "//TRIM(STRn(SIO%CHAR))
WRITE(Unit_,*)" INTEGER                     ,POINTER :: line_num          => "//TRIM(STRn(SIO%line_num))
WRITE(Unit_,*)" INTEGER                     ,POINTER :: column_num        => "//TRIM(STRn(SIO%column_num))
WRITE(Unit_,*)" INTEGER                     ,POINTER :: record_num        => "//TRIM(STRn(SIO%record_num))
WRITE(Unit_,*)" LOGICAL                     ,POINTER :: end_of_line       => "//TRIM(STRn(SIO%end_of_line))
WRITE(Unit_,*)" LOGICAL                     ,POINTER :: end_of_file       => "//TRIM(STRn(SIO%end_of_file))
WRITE(Unit_,*)" LOGICAL                     ,POINTER :: shadow_connection => "//TRIM(STRn(SIO%shadow_connection))
WRITE(Unit_,*)" "
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #SYNTAX I (initialized or set with SYNTAX)#"
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: CmdBeg       => "//TRIM(STRn(SIO%CmdBeg))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: ArgBeg       => "//TRIM(STRn(SIO%ArgBeg))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: Argdefault   => "//TRIM(STRn(SIO%ArgDefault))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: ArgDefine    => "//TRIM(STRn(SIO%ArgDefine))
WRITE(Unit_,*)" CHARACTER(MAX_LEN_ArgDelim) ,POINTER :: ArgDelim     => "//TRIM(STRn(SIO%ArgDelim))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: SubArgDelim  => "//TRIM(STRn(SIO%SubArgDelim))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: ArgEnd       => "//TRIM(STRn(SIO%ArgEnd))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: Ignore       => "//TRIM(STRn(SIO%Ignore))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: Comment      => "//TRIM(STRn(SIO%Comment))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: Continue     => "//TRIM(STRn(SIO%Continue))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: Stop         => "//TRIM(STRn(SIO%Stop))
WRITE(Unit_,*)" CHARACTER                   ,POINTER :: StrTag       => "//TRIM(STRn(SIO%StrTag))
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #SYNTAX II (consequence of the current connection)#"
WRITE(Unit_,*)" INTEGER                     ,POINTER :: LEN_ArgDelim  => "//TRIM(STRn(SIO%LEN_ArgDelim))
WRITE(Unit_,*)" LOGICAL                     ,POINTER :: shadow_syntax => "//TRIM(STRn(SIO%shadow_syntax))
WRITE(Unit_,*)" "
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #FORMAT I (initialized or set with FORMAT)#"
WRITE(Unit_,*)" CHARACTER(LEN_FMT)          ,POINTER :: FMT_L  => "//TRIM(STRn(SIO%FMT_L))
WRITE(Unit_,*)" CHARACTER(LEN_FMT)          ,POINTER :: FMT_I1 => "//TRIM(STRn(SIO%FMT_I1))
WRITE(Unit_,*)" CHARACTER(LEN_FMT)          ,POINTER :: FMT_I2 => "//TRIM(STRn(SIO%FMT_I2))
WRITE(Unit_,*)" CHARACTER(LEN_FMT)          ,POINTER :: FMT_I4 => "//TRIM(STRn(SIO%FMT_I4))
WRITE(Unit_,*)" CHARACTER(LEN_FMT)          ,POINTER :: FMT_I8 => "//TRIM(STRn(SIO%FMT_I8))
WRITE(Unit_,*)" CHARACTER(LEN_FMT)          ,POINTER :: FMT_sp => "//TRIM(STRn(SIO%FMT_sp))
WRITE(Unit_,*)" CHARACTER(LEN_FMT)          ,POINTER :: FMT_dp => "//TRIM(STRn(SIO%FMT_dp))
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #FORMAT II (consequence of current connection)#"
WRITE(Unit_,*)" LOGICAL                     ,POINTER :: shadow_format => "//TRIM(STRn(SIO%shadow_format))
WRITE(Unit_,*)" "
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #COMMAND I (initialize or SET with Cmd)#"
WRITE(Unit_,*)" TYPE(varying_string)        ,POINTER :: Cmd     => "//TRIM(STRn(SIO%Cmd))
WRITE(Unit_,*)" INTEGER                     ,POINTER :: CurrArg => "//TRIM(STRn(SIO%CurrArg))
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #COMMAND II (consequence of current connection)#"
WRITE(Unit_,*)" INTEGER                     ,POINTER :: hash(:)     => "//TRIM(Sentence(STRn(SIO%hash)))
WRITE(Unit_,*)" TYPE(varying_string)        ,POINTER :: SplitCmd(:) => "//TRIM(Sentence(STRn(SIO%SplitCmd)))
WRITE(Unit_,*)" TYPE(varying_string)        ,POINTER :: Arg(:)      => "//TRIM(Sentence(STRn(SIO%Arg)))
WRITE(Unit_,*)" CHARACTER(LEN_ArgNm)        ,POINTER :: ArgNm(:)    => "//TRIM(Sentence(STRn(SIO%ArgNm)))
WRITE(Unit_,*)" INTEGER                     ,POINTER :: NSubArg(:)  => "//TRIM(Sentence(STRn(SIO%NSubArg)))
WRITE(Unit_,*)" LOGICAL                     ,POINTER :: Optional(:) => "//TRIM(Sentence(STRn(SIO%Optional)))
WRITE(Unit_,*)" LOGICAL                     ,POINTER :: Present(:)  => "//TRIM(Sentence(STRn(SIO%Present)))
WRITE(Unit_,*)" LOGICAL                     ,POINTER :: IsMacro     => "//TRIM(STRn(SIO%IsMacro))
WRITE(Unit_,*)" "
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #LINKED-LIST pointer to the previous SIO object#"
WRITE(Unit_,*)" TYPE(TYPE_SIO)               ,POINTER :: prev => ",ASSOCIATED(SIO%Prev)
WRITE(Unit_,*)" "
WRITE(Unit_,*)" "
WRITE(Unit_,*)" #OPTIONS#"
WRITE(Unit_,*)"  * If action=write then <ArgDefine-statements> are output."
WRITE(Unit_,*)"    If action=read then <ArgDefine statements> are being used in input."
WRITE(Unit_,*)"  LOGICAL                    ,POINTER :: using_ArgDefine => "//TRIM(STRn(SIO%using_ArgDefine))
WRITE(Unit_,*)" "
WRITE(Unit_,*)"  * If action=read and echo_unit>0 then echo commands to echo_unit."
WRITE(Unit_,*)"  INTEGER                    ,POINTER :: echo_unit => "//TRIM(STRn(SIO%echo_unit))
WRITE(Unit_,*)"  * If action=read and interpretter>0 then interpretter commands "
WRITE(Unit_,*)"    will be read from the terminal."
WRITE(Unit_,*)"  INTEGER                    ,POINTER :: interpretter => "//TRIM(STRn(SIO%interpretter))
WRITE(Unit_,*)" "
WRITE(Unit_,*)"  * Argument/DataBlock variables are allocated/deallocated automatically"
WRITE(Unit_,*)"    to avoid memory leaks."
WRITE(Unit_,*)"  LOGICAL                    ,POINTER :: auto_memory => "//TRIM(STRn(SIO%auto_memory))
WRITE(Unit_,*)" "
WRITE(Unit_,*)"END TYPE"
WRITE(Unit_,*)" "
WRITE(Unit_,*)" "

!!--end--
END SUBROUTINE

!!**begin**CONNECTION nullify, associate, allocate, deallocate****
SUBROUTINE NULLIFY_CONNECTION(SIO)
!!#### PURPOSE
!! NULLIFY the CONNECTION part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO
!!
!!a. NULLIFY CONNECTION I
NULLIFY( SIO%FILE)
NULLIFY( SIO%FORM)
NULLIFY( SIO%ACTION)
NULLIFY( SIO%ACCESS)
NULLIFY( SIO%STATUS)
NULLIFY( SIO%POSITION)
NULLIFY( SIO%BLANK)
NULLIFY( SIO%PAD)
NULLIFY( SIO%RECL)
NULLIFY( SIO%DELIM)
!!b. NULLIFY CONNECTION II
NULLIFY( SIO%UNIT )
NULLIFY( SIO%IOSTAT )
NULLIFY( SIO%SIZE )
NULLIFY( SIO%Char )
NULLIFY( SIO%line_num )
NULLIFY( SIO%record_num )
NULLIFY( SIO%end_of_line )
NULLIFY( SIO%end_of_file )
DEALLOCATE( SIO%shadow_connection )
!!
END SUBROUTINE

SUBROUTINE ASSOCIATE_CONNECTION(SIO_a,SIO_b)
!!#### PURPOSE
!! Associate the CONNECTION for SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO_a,SIO_b
!!
!!a. associate CONNECTION I
SIO_a%FILE     => SIO_b%FILE
SIO_a%FORM     => SIO_b%FORM
SIO_a%ACTION   => SIO_b%ACTION
SIO_a%ACCESS   => SIO_b%ACCESS
SIO_a%STATUS   => SIO_b%STATUS
SIO_a%POSITION => SIO_b%POSITION
SIO_a%BLANK    => SIO_b%BLANK
SIO_a%PAD      => SIO_b%PAD
SIO_a%RECL     => SIO_b%RECL
SIO_a%DELIM    => SIO_b%DELIM
!!b. associate CONNECTION II
SIO_a%UNIT        => SIO_b%UNIT
SIO_a%IOSTAT      => SIO_b%IOSTAT
SIO_a%SIZE        => SIO_b%SIZE
SIO_a%Char        => SIO_b%Char
SIO_a%line_num    => SIO_b%line_num
SIO_a%record_num  => SIO_b%record_num
SIO_a%end_of_line => SIO_b%end_of_line
SIO_a%end_of_file => SIO_b%end_of_file
ALLOCATE( SIO_a%shadow_CONNECTION ) ; SIO_a%shadow_CONNECTION = .TRUE.
!!
END SUBROUTINE

SUBROUTINE ALLOCATE_CONNECTION(SIO)
!!#### PURPOSE
!! Allocate the CONNECTION part of SIO.

!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO

!!#### LOCALS
INTEGER  :: jerr
!!
!!a. ALLOCATE CONNECTION I
ALLOCATE( SIO%FILE    ,STAT=jerr)
ALLOCATE( SIO%FORM    ,STAT=jerr)
ALLOCATE( SIO%ACTION  ,STAT=jerr)
ALLOCATE( SIO%ACCESS  ,STAT=jerr)
ALLOCATE( SIO%STATUS  ,STAT=jerr)
ALLOCATE( SIO%POSITION,STAT=jerr)
ALLOCATE( SIO%BLANK   ,STAT=jerr)
ALLOCATE( SIO%PAD     ,STAT=jerr)
ALLOCATE( SIO%RECL    ,STAT=jerr)
ALLOCATE( SIO%DELIM   ,STAT=jerr)
!!b. ALLOCATE CONNECTION II
ALLOCATE( SIO%UNIT              ,STAT=jerr) ; SIO%UNIT=0
ALLOCATE( SIO%IOSTAT            ,STAT=jerr) ; SIO%IOSTAT=0
ALLOCATE( SIO%SIZE              ,STAT=jerr) ; SIO%SIZE = 0
ALLOCATE( SIO%Char              ,STAT=jerr) ; SIO%CHAR = " "
ALLOCATE( SIO%line_num          ,STAT=jerr) ; SIO%line_num = ERROR(SIO%line_num)
ALLOCATE( SIO%record_num        ,STAT=jerr) ; SIO%record_num = ERROR(SIO%record_num)
ALLOCATE( SIO%end_of_line       ,STAT=jerr) ; SIO%end_of_line = .FALSE.
ALLOCATE( SIO%end_of_file       ,STAT=jerr) ; SIO%end_of_file = .FALSE.
ALLOCATE( SIO%shadow_connection ,STAT=jerr) ; SIO%shadow_connection = .FALSE.
!!
END SUBROUTINE

SUBROUTINE DEALLOCATE_CONNECTION(SIO)
!!#### PURPOSE
!! DEALLOCATE the CONNECTION part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER                :: SIO
!!#### LOCALS
INTEGER  :: jerr
!!
!!a. DEALLOCATE CONNECTION I
DEALLOCATE( SIO%FILE,STAT=jerr)
DEALLOCATE( SIO%FORM,STAT=jerr)
DEALLOCATE( SIO%ACTION,STAT=jerr)
DEALLOCATE( SIO%ACCESS,STAT=jerr)
DEALLOCATE( SIO%STATUS,STAT=jerr)
DEALLOCATE( SIO%POSITION,STAT=jerr)
DEALLOCATE( SIO%BLANK,STAT=jerr)
DEALLOCATE( SIO%PAD,STAT=jerr)
DEALLOCATE( SIO%RECL,STAT=jerr)
DEALLOCATE( SIO%DELIM,STAT=jerr)
!!b. DEALLOCATE CONNECTION II
DEALLOCATE( SIO%UNIT              ,STAT=jerr)
DEALLOCATE( SIO%IOSTAT            ,STAT=jerr)
DEALLOCATE( SIO%SIZE              ,STAT=jerr)
DEALLOCATE( SIO%Char              ,STAT=jerr)
DEALLOCATE( SIO%line_num          ,STAT=jerr)
DEALLOCATE( SIO%record_num        ,STAT=jerr)
DEALLOCATE( SIO%end_of_line       ,STAT=jerr)
DEALLOCATE( SIO%end_of_file       ,STAT=jerr)
DEALLOCATE( SIO%shadow_connection ,STAT=jerr)
!!
END SUBROUTINE
!!**end**CONNECTION nullify, associate, allocate, deallocate****





!!**begin**SYNTAX nullify, associate, allocate, deallocate****
SUBROUTINE NULLIFY_SYNTAX(SIO)
!!#### PURPOSE
!! NULLIFY the syntax part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO
!!#### LOCALS
!!
!!a. NULLIFY SYNTAX I
NULLIFY( SIO%Comment          )
NULLIFY( SIO%Continue         )
NULLIFY( SIO%Stop             )
NULLIFY( SIO%Ignore           )
NULLIFY( SIO%CmdBeg           )
NULLIFY( SIO%ArgBeg           )
NULLIFY( SIO%ArgEnd           )
NULLIFY( SIO%ArgDelim         )
NULLIFY( SIO%SubArgDelim      )
NULLIFY( SIO%ArgDefine        )
NULLIFY( SIO%Argdefault       )
NULLIFY( SIO%StrTag           )
!!b. NULLIFY SYNTAX II
NULLIFY( SIO%LEN_ArgDelim)
DEALLOCATE( SIO%shadow_syntax )
!!
END SUBROUTINE

SUBROUTINE ASSOCIATE_SYNTAX(SIO_a,SIO_b)
!!#### PURPOSE
!! Associate the syntax for SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO_a,SIO_b
!!
!!a. associate SYNTAX I
SIO_a%Comment             => SIO_b%Comment
SIO_a%Continue            => SIO_b%Continue
SIO_a%Stop                => SIO_b%Stop
SIO_a%Ignore              => SIO_b%Ignore
SIO_a%CmdBeg              => SIO_b%CmdBeg
SIO_a%ArgBeg              => SIO_b%ArgBeg
SIO_a%ArgEnd              => SIO_b%ArgEnd
SIO_a%ArgDelim            => SIO_b%ArgDelim
SIO_a%SubArgDelim         => SIO_b%SubArgDelim
SIO_a%ArgDefine           => SIO_b%ArgDefine
SIO_a%Argdefault          => SIO_b%Argdefault
SIO_a%StrTag              => SIO_b%StrTag
!!b. associate SYNTAX II
SIO_a%LEN_ArgDelim   => SIO_b%LEN_ArgDelim
ALLOCATE( SIO_a%shadow_syntax ) ; SIO_a%shadow_syntax = .TRUE.
!!
END SUBROUTINE

SUBROUTINE ALLOCATE_SYNTAX(SIO)
!!#### PURPOSE
!! Allocate the syntax part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO
!!#### LOCALS
INTEGER  :: jerr
!!
!!a. allocate SYNTAX I
ALLOCATE( SIO%Comment    , STAT=jerr ) ; SIO%Comment     = "!"
ALLOCATE( SIO%Continue   , STAT=jerr ) ; SIO%Continue    = "&"
ALLOCATE( SIO%Stop       , STAT=jerr ) ; SIO%Stop        = "#"
ALLOCATE( SIO%Ignore     , STAT=jerr ) ; SIO%Ignore      = " "
ALLOCATE( SIO%CmdBeg     , STAT=jerr ) ; SIO%CmdBeg      = " "
ALLOCATE( SIO%ArgBeg     , STAT=jerr ) ; SIO%ArgBeg      = " "
ALLOCATE( SIO%ArgEnd     , STAT=jerr ) ; SIO%ArgEnd      = " "
ALLOCATE( SIO%ArgDelim   , STAT=jerr ) ; SIO%ArgDelim    = " "
ALLOCATE( SIO%SubArgDelim, STAT=jerr ) ; SIO%SubArgDelim = " "
ALLOCATE( SIO%ArgDefine  , STAT=jerr ) ; SIO%ArgDefine   = "="
ALLOCATE( SIO%Argdefault , STAT=jerr ) ; SIO%Argdefault  = "*"
!! Note that the string tag Character, <"> is set here and
!! never allowed to be changed.  Originally it was read as input,
!! but <"""> is interpretted as a blank and another string is
!! started.  You need the string Character to define the blank
!! Character, <" ">.  Trust me, it is simpler this way. [waw]
ALLOCATE( SIO%StrTag     , STAT=jerr ) ; SIO%StrTag      = '"'

!!b. allocate SYNTAX II
ALLOCATE( SIO%LEN_ArgDelim, STAT=jerr )
ALLOCATE( SIO%shadow_syntax ) ; SIO%shadow_syntax = .FALSE.
!!
END SUBROUTINE

SUBROUTINE DEALLOCATE_SYNTAX(SIO)
!!#### PURPOSE
!! DEALLOCATE the syntax part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER                :: SIO
!!#### LOCALS
INTEGER  :: jerr
!!
!!a. DEALLOCATE SYNTAX I
DEALLOCATE( SIO%Comment          , STAT=jerr )
DEALLOCATE( SIO%Continue         , STAT=jerr )
DEALLOCATE( SIO%Stop             , STAT=jerr )
DEALLOCATE( SIO%Ignore           , STAT=jerr )
DEALLOCATE( SIO%CmdBeg           , STAT=jerr )
DEALLOCATE( SIO%ArgBeg           , STAT=jerr )
DEALLOCATE( SIO%ArgEnd           , STAT=jerr )
DEALLOCATE( SIO%ArgDelim         , STAT=jerr )
DEALLOCATE( SIO%SubArgDelim      , STAT=jerr )
DEALLOCATE( SIO%ArgDefine        , STAT=jerr )
DEALLOCATE( SIO%Argdefault       , STAT=jerr )
DEALLOCATE( SIO%StrTag           , STAT=jerr )
!!b. DEALLOCATE SYNTAX II
DEALLOCATE( SIO%LEN_ArgDelim, STAT=jerr )
DEALLOCATE( SIO%shadow_syntax )
!!
END SUBROUTINE
!!**end**SYNTAX nullify, associate, allocate, deallocate****





!!**begin**FORMAT nullify, associate, allocate, deallocate****
SUBROUTINE NULLIFY_FORMAT(SIO)
!!#### PURPOSE
!! NULLIFY the FORMAT part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO
!!
!!a. NULLIFY FORMAT I
NULLIFY( SIO%FMT_L )
NULLIFY( SIO%FMT_I1 )
NULLIFY( SIO%FMT_I2 )
NULLIFY( SIO%FMT_I4 )
NULLIFY( SIO%FMT_I8 )
NULLIFY( SIO%FMT_sp )
NULLIFY( SIO%FMT_dp )
!!b. NULLIFY FORMAT II
NULLIFY( SIO%shadow_format )
!!
END SUBROUTINE

SUBROUTINE ASSOCIATE_FORMAT(SIO_a,SIO_b)
!!#### PURPOSE
!! Associate the FORMAT for SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO_a,SIO_b
!!
!!a. ASSOCIATE FORMAT I
SIO_a%FMT_L  => SIO_b%FMT_L
SIO_a%FMT_I1 => SIO_b%FMT_I1
SIO_a%FMT_I2 => SIO_b%FMT_I2
SIO_a%FMT_I4 => SIO_b%FMT_I4
SIO_a%FMT_I8 => SIO_b%FMT_I8
SIO_a%FMT_sp => SIO_b%FMT_sp
SIO_a%FMT_dp => SIO_b%FMT_dp
!!b. ALLOCATE FORMAT II
ALLOCATE( SIO_a%shadow_FORMAT ) ; SIO_a%shadow_FORMAT = .TRUE.
!!
END SUBROUTINE

SUBROUTINE ALLOCATE_FORMAT(SIO)
!!#### PURPOSE
!! Allocate the FORMAT part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO
!!#### LOCALS
INTEGER  :: jerr
!!
!!a. ALLOCATE FORMAT I
ALLOCATE( SIO%FMT_L  , STAT=jerr) ; SIO%FMT_L  = "*"
ALLOCATE( SIO%FMT_I1 , STAT=jerr) ; SIO%FMT_I1 = "*"
ALLOCATE( SIO%FMT_I2 , STAT=jerr) ; SIO%FMT_I2 = "*"
ALLOCATE( SIO%FMT_I4 , STAT=jerr) ; SIO%FMT_I4 = "*"
ALLOCATE( SIO%FMT_I8 , STAT=jerr) ; SIO%FMT_I8 = "*"
ALLOCATE( SIO%FMT_sp , STAT=jerr) ; SIO%FMT_sp = "*"
ALLOCATE( SIO%FMT_dp , STAT=jerr) ; SIO%FMT_dp = "*"
!!b. ALLOCATE FORMAT II
ALLOCATE( SIO%shadow_FORMAT ) ; SIO%shadow_FORMAT = .FALSE.
!!
END SUBROUTINE

SUBROUTINE DEALLOCATE_FORMAT(SIO)
!!#### PURPOSE
!! DEALLOCATE the FORMAT part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER                :: SIO
!!#### LOCALS
INTEGER  :: jerr
!!
!!a. DEALLOCATE FORMAT I
DEALLOCATE( SIO%FMT_L  , STAT=jerr)
DEALLOCATE( SIO%FMT_I1 , STAT=jerr)
DEALLOCATE( SIO%FMT_I2 , STAT=jerr)
DEALLOCATE( SIO%FMT_I4 , STAT=jerr)
DEALLOCATE( SIO%FMT_I8 , STAT=jerr)
DEALLOCATE( SIO%FMT_sp , STAT=jerr)
DEALLOCATE( SIO%FMT_dp , STAT=jerr)
!!b. DEDEALLOCATE FORMAT II
DEALLOCATE( SIO%shadow_FORMAT )
!!
END SUBROUTINE
!!**end**FORMAT nullify, associate, allocate, deallocate****





!!**begin**OPTIONS allocate, deallocate****
SUBROUTINE ALLOCATE_OPTIONS(SIO)
!!#### PURPOSE
!! Allocate the options for SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO
!!#### LOCALS
INTEGER :: jerr
!!
ALLOCATE( SIO%echo_unit , STAT=jerr )
SIO%echo_unit = DEFAULT_echo_unit

ALLOCATE( SIO%auto_memory       , STAT=jerr )
SIO%auto_memory = DEFAULT_auto_memory

ALLOCATE( SIO%interpretter    , STAT=jerr )
SIO%interpretter = DEFAULT_interpretter

ALLOCATE( SIO%using_ArgDefine  , STAT=jerr )
SIO%using_ArgDefine = DEFAULT_using_ArgDefine
!!
END SUBROUTINE

SUBROUTINE COPY_OPTIONS(SIO_a,SIO_b)
!!#### PURPOSE
!! Copy options from another SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO_a,SIO_b
!!#### LOCALS
INTEGER :: jerr
!!
SIO_a%echo_unit       = SIO_b%echo_unit
SIO_a%auto_memory     = SIO_b%auto_memory
SIO_a%interpretter    = SIO_b%interpretter
SIO_a%using_ArgDefine = SIO_b%using_ArgDefine
!!
END SUBROUTINE

SUBROUTINE DEALLOCATE_OPTIONS(SIO)
!!#### PURPOSE
!! Dellocate the options for SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO
!!#### LOCALS
INTEGER :: jerr
!!
!!a. allocate options
DEALLOCATE( SIO%echo_unit      , STAT=jerr )
DEALLOCATE( SIO%auto_memory    , STAT=jerr )
DEALLOCATE( SIO%interpretter   , STAT=jerr )
DEALLOCATE( SIO%using_ArgDefine, STAT=jerr )
!!
END SUBROUTINE
!!**end**OPTIONS allocate, deallocate****






!!**begin**Cmd allocate, deallocate****
SUBROUTINE ALLOCATE_COMMAND(SIO)
!!#### PURPOSE
!! Allocate the Cmd part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER                :: SIO
!!#### LOCALS
INTEGER  :: jerr
!!
!!a. allocate COMMAND I
ALLOCATE( SIO%Cmd      , STAT=jerr )
ALLOCATE( SIO%CurrArg  , STAT=jerr )
!!b. ALLOCATE COMMAND II
ALLOCATE( SIO%hash(1:2) , STAT=jerr )
ALLOCATE( SIO%splitCmd(1:2) , STAT=jerr )
!!
END SUBROUTINE

SUBROUTINE DEALLOCATE_COMMAND(SIO)
!!#### PURPOSE
!! DEALLOCATE the Cmd part of SIO.
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER                :: SIO
!!#### LOCALS
INTEGER  :: jerr
!!
!!a. DEALLOCATE COMMAND I
DEALLOCATE( SIO%Cmd      , STAT=jerr )
DEALLOCATE( SIO%CurrArg  , STAT=jerr )
!!b. DEALLOCATE COMMAND II
DEALLOCATE( SIO%hash , STAT=jerr )
DEALLOCATE( SIO%splitCmd , STAT=jerr )

!!
END SUBROUTINE
!!**end**Cmd allocate, deallocate****


!!**begin** INQUIRE functions
FUNCTION INQUIRE_ACTION(SIO) RESULT(ACTION)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: ACTION
!!
INQUIRE( UNIT=SIO%UNIT , ACTION=ACTION , IOSTAT=SIO%IOSTAT )
!default action
IF( SIO%IOSTAT/=0 )THEN
 ACTION = DEFAULT_ACTION(SIO)
END IF
!!
END FUNCTION

FUNCTION DEFAULT_ACTION(SIO) RESULT(ACTION)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(IN) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: ACTION
!!
ACTION = "READ"
!!
END FUNCTION

FUNCTION INQUIRE_ACCESS(SIO) RESULT(ACCESS)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: ACCESS
!!
INQUIRE( UNIT=SIO%UNIT , ACCESS=ACCESS , IOSTAT=SIO%IOSTAT )
IF( SIO%IOSTAT/=0 )THEN
 ACCESS = DEFAULT_ACCESS(SIO)
END IF
!!
END FUNCTION

FUNCTION DEFAULT_ACCESS(SIO) RESULT(ACCESS)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(IN) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: ACCESS
!!
ACCESS = "SEQUENTIAL"
!!
END FUNCTION


FUNCTION INQUIRE_BLANK(SIO) RESULT(BLANK)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: BLANK
!!
INQUIRE( UNIT=SIO%UNIT , BLANK=BLANK , IOSTAT=SIO%IOSTAT )
IF( SIO%IOSTAT/=0 )THEN
 BLANK = DEFAULT_BLANK(SIO)
END IF
!!
END FUNCTION

FUNCTION DEFAULT_BLANK(SIO) RESULT(BLANK)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(IN) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: BLANK
!!
BLANK = "NULL"
!!
END FUNCTION

FUNCTION INQUIRE_DELIM(SIO) RESULT(DELIM)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: DELIM
!!
INQUIRE( UNIT=SIO%UNIT , DELIM=DELIM , IOSTAT=SIO%IOSTAT )
IF( SIO%IOSTAT/=0 )THEN
 DELIM = DEFAULT_DELIM(SIO)
END IF
!!
END FUNCTION

FUNCTION DEFAULT_DELIM(SIO) RESULT(DELIM)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(IN) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: DELIM
!!
DELIM = "NONE"
!!
END FUNCTION

FUNCTION INQUIRE_EXIST(SIO) RESULT(EXIST)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
LOGICAL :: EXIST
!!
INQUIRE( FILE=STR(SIO%FILE) , EXIST=EXIST , IOSTAT=SIO%IOSTAT )
!!
END FUNCTION

FUNCTION INQUIRE_FORM(SIO) RESULT(FORM)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: FORM
!!
INQUIRE( UNIT=SIO%UNIT , FORM=FORM , IOSTAT=SIO%IOSTAT )
IF( SIO%IOSTAT/=0 )THEN
 FORM = DEFAULT_FORM(SIO)
END IF
!!
END FUNCTION

FUNCTION DEFAULT_FORM(SIO) RESULT(FORM)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(IN) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: FORM
!!
FORM = "FORMATTED"
!!
END FUNCTION

FUNCTION INQUIRE_FILE(SIO) RESULT(FILE)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
TYPE(varying_string) :: FILE
!!#### LOCAL VARIABLES
CHARACTER(1000) :: FILE_
!!
CALL CLEAR(FILE_)
INQUIRE( FILE=FILE_ , IOSTAT=SIO%IOSTAT )
IF( SIO%IOSTAT/=0 )THEN
 CALL CLEAR(FILE)
ELSE
 FILE = TRIM(FILE_)
END IF
!!
END FUNCTION

FUNCTION INQUIRE_PAD(SIO) RESULT(PAD)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: PAD
!!
INQUIRE( UNIT=SIO%UNIT , PAD=PAD , IOSTAT=SIO%IOSTAT )
IF( SIO%IOSTAT/=0 )THEN
 PAD = DEFAULT_PAD(SIO)
END IF
!!
END FUNCTION

FUNCTION DEFAULT_PAD(SIO) RESULT(PAD)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(IN) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: PAD
!!
PAD = "YES"
!!
END FUNCTION

FUNCTION INQUIRE_POSITION(SIO) RESULT(POSITION)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: POSITION
!!
INQUIRE( UNIT=SIO%UNIT , POSITION=POSITION , IOSTAT=SIO%IOSTAT )
IF( SIO%IOSTAT/=0 )THEN
 POSITION = DEFAULT_POSITION(SIO)
END IF
!!
END FUNCTION

FUNCTION DEFAULT_POSITION(SIO) RESULT(POSITION)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(IN) :: SIO
!!#### RESULT
CHARACTER(LEN_CONNECTION) :: POSITION
!!
POSITION = "ASIS"
!!
END FUNCTION

FUNCTION INQUIRE_RECL(SIO) RESULT(RECL)
!!#### ARGUMENTS
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO
!!#### RESULT
INTEGER :: RECL
!!
INQUIRE( UNIT=SIO%UNIT , RECL=RECL , IOSTAT=SIO%IOSTAT )
IF( SIO%IOSTAT/=0 )THEN
 RECL = ERROR(RECL)
END IF
!!
END FUNCTION
!!**end** INQUIRE functions


SUBROUTINE ADD_NEW_CONNECTION(SIO)
!!#### PURPOSE
!! Add a new connection onto an SIO object.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),POINTER    :: SIO

!!#### LOCAL VARIABLES
TYPE(TYPE_SIO),POINTER :: SIO_

!!

!!allocate a new connection, command, options
ALLOCATE( SIO_ )
CALL ALLOCATE_CONNECTION( SIO_ )
CALL ALLOCATE_COMMAND( SIO_ )
CALL ALLOCATE_OPTIONS( SIO_ )

IF( ASSOCIATED(SIO) )THEN
 !!copy options
 CALL COPY_OPTIONS( SIO_ , SIO )
 !!associate the previous syntax, format
 CALL ASSOCIATE_SYNTAX( SIO_ , SIO )
 CALL ASSOCIATE_FORMAT( SIO_ , SIO )
ELSE
 !!allocate because there is no previous syntax, format
 CALL ALLOCATE_SYNTAX( SIO_ )
 CALL ALLOCATE_FORMAT( SIO_ )
END IF

!!set the previous pointer to the last instance
SIO_%prev => SIO
!!set the current pointer to the current instance
SIO       => SIO_

!!
END SUBROUTINE


SUBROUTINE TERMINAL__(SIO,fdbk,ACTION)
!!#### PURPOSE
!! Configure SIO to interact with the terminal.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),POINTER    :: SIO

!!#### OPTIONAL INPUT/OUTPUT
CHARACTER(*)   ,INTENT(IN)   ,OPTIONAL :: ACTION
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(*),PARAMETER :: proc_ = "TERMINAL__"
CHARACTER(LEN_Connection) :: ACTION_
!!

CALL ADD_NEW_CONNECTION( SIO )
ACTION_ = DEFAULT( "READ      " , ACTION )

!!get the right unit
IF( ACTION_ .sEQ. "READ" )THEN
 SIO%ACTION = "READ"
 SIO%UNIT   = keyboard_unit
ELSE
 SIO%ACTION = "WRITE"
 SIO%UNIT   = window_unit
END IF

!!change the connection specifiers of SIO according to what was provided in input
SIO%FILE     = "TERMINAL_WINDOW"
SIO%FORM     = "FORMATTED"
SIO%ACCESS   = "SEQUENTIAL"
SIO%POSITION = "ASIS"
SIO%BLANK    = " "
SIO%PAD      = " "
SIO%DELIM    = "BLANK"


!!set starting line
SIO%line_num = 1

!!
END SUBROUTINE



SUBROUTINE CONNECT__(SIO,fdbk,FILE    ,&
                            FORM    ,&
                            ACTION  ,&
                            ACCESS  ,&
                            STATUS  ,&
                            POSITION,&
                            BLANK   ,&
                            PAD     ,&
                            RECL    ,&
                            DELIM    )
!!#### PURPOSE
!! Establish a new CONNECTION.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),POINTER    :: SIO

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
CHARACTER(*),INTENT(IN),OPTIONAL :: FILE
CHARACTER(*),INTENT(IN),OPTIONAL :: FORM
CHARACTER(*),INTENT(IN),OPTIONAL :: ACTION
CHARACTER(*),INTENT(IN),OPTIONAL :: ACCESS
CHARACTER(*),INTENT(IN),OPTIONAL :: STATUS
CHARACTER(*),INTENT(IN),OPTIONAL :: POSITION
CHARACTER(*),INTENT(IN),OPTIONAL :: BLANK
CHARACTER(*),INTENT(IN),OPTIONAL :: PAD
INTEGER     ,INTENT(IN),OPTIONAL :: RECL
CHARACTER(*),INTENT(IN),OPTIONAL :: DELIM

!!#### LOCAL VARIABLES
CHARACTER(*),PARAMETER :: proc_ = "CONNECT_SIO"
INTEGER                :: IOSTAT_open,Unit_
CHARACTER(3)           :: allow_read
LOGICAL                :: EXIST_file,Opened
TYPE(TYPE_SIO),POINTER :: SIO_
TYPE(varying_string)   :: VS
!!

!!allocate a new connection, command, options
ALLOCATE( SIO_ )
CALL ALLOCATE_CONNECTION( SIO_ )
CALL ALLOCATE_COMMAND( SIO_ )
CALL ALLOCATE_OPTIONS( SIO_ )

IF( ASSOCIATED(SIO) )THEN
 !!copy options
 CALL COPY_OPTIONS( SIO_ , SIO )
 !!associate the previous syntax, format
 CALL ASSOCIATE_SYNTAX( SIO_ , SIO )
 CALL ASSOCIATE_FORMAT( SIO_ , SIO )
ELSE
 !!allocate because there is no previous syntax, format
 CALL ALLOCATE_SYNTAX( SIO_ )
 CALL ALLOCATE_FORMAT( SIO_ )
END IF

!!set the previous pointer to the last instance
SIO_%prev => SIO
!!set the current pointer to the current instance
SIO       => SIO_

!!get a new unit
SIO%UNIT = NewUnit()

!!change the connection specifiers of SIO according to what was provided in input
Opened = .FALSE.
IF( PRESENT(FILE    ) )THEN
 SIO%FILE = FILE
ELSE
 SIO%FILE = ""
END IF

IF( PRESENT(FORM    ) )THEN
 SIO%FORM = FORM
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!       FILE=STR(SIO%FILE),&
!          IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%FORM = DEFAULT_FORM(SIO)
END IF

IF( PRESENT(ACTION  ) )THEN
 SIO%ACTION = ACTION
! IF( Opened )THEN
!  Opened = .FALSE.
!  CLOSE(SIO%unit)
! END IF
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!    FILE=STR(SIO%FILE),&
!    FORM=TRIM(SIO%FORM),&
!       IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%ACTION = DEFAULT_ACTION(SIO)
END IF

IF( PRESENT(ACCESS) )THEN
 SIO%ACCESS   = ACCESS
! IF( Opened )THEN
!  Opened = .FALSE.
!  CLOSE(SIO%unit)
! END IF
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!    FILE=STR(SIO%FILE),&
!    FORM=TRIM(SIO%FORM),&
!    ACTION=TRIM(SIO%ACTION),&
!       IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%ACCESS = DEFAULT_ACCESS(SIO)
END IF

IF( PRESENT(STATUS  ) )THEN
 SIO%STATUS = STATUS
! IF( Opened )THEN
!  Opened = .FALSE.
!  CLOSE(SIO%unit)
! END IF
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!    FILE=STR(SIO%FILE),&
!    FORM=TRIM(SIO%FORM),&
!    ACTION=TRIM(SIO%ACTION),&
!    ACCESS=TRIM(SIO%ACCESS),&
!       IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%STATUS = MERGE("Old    ","Replace",Reading(SIO))
END IF

IF( PRESENT(POSITION) )THEN
 SIO%POSITION = POSITION
! IF( Opened )THEN
!  Opened = .FALSE.
!  CLOSE(SIO%unit)
! END IF
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!    FILE=STR(SIO%FILE),&
!    FORM=TRIM(SIO%FORM),&
!    ACTION=TRIM(SIO%ACTION),&
!    ACCESS=TRIM(SIO%ACCESS),&
!    STATUS=TRIM(SIO%STATUS),&
!       IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%POSITION = DEFAULT_POSITION(SIO)
END IF

IF( PRESENT(BLANK   ) )THEN
 SIO%BLANK = BLANK
! IF( Opened )THEN
!  Opened = .FALSE.
!  CLOSE(SIO%unit)
! END IF
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!    FILE=STR(SIO%FILE),&
!    FORM=TRIM(SIO%FORM),&
!    ACTION=TRIM(SIO%ACTION),&
!    ACCESS=TRIM(SIO%ACCESS),&
!    STATUS=TRIM(SIO%STATUS),&
!    POSITION=TRIM(SIO%POSITION),&
!       IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%BLANK = DEFAULT_BLANK(SIO)
END IF

IF( PRESENT(PAD     ) )THEN
 SIO%PAD = PAD
! IF( Opened )THEN
!  Opened = .FALSE.
!  CLOSE(SIO%unit)
! END IF
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!    FILE=STR(SIO%FILE),&
!    FORM=TRIM(SIO%FORM),&
!    ACTION=TRIM(SIO%ACTION),&
!    ACCESS=TRIM(SIO%ACCESS),&
!    STATUS=TRIM(SIO%STATUS),&
!    POSITION=TRIM(SIO%POSITION),&
!    BLANK=TRIM(SIO%BLANK),&
!       IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%PAD = DEFAULT_PAD(SIO)
END IF

IF( PRESENT(RECL    ) )THEN
 SIO%RECL = RECL
! IF( Opened )THEN
!  Opened = .FALSE.
!  CLOSE(SIO%unit)
! END IF
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!    FILE=STR(SIO%FILE),&
!    FORM=TRIM(SIO%FORM),&
!    ACTION=TRIM(SIO%ACTION),&
!    ACCESS=TRIM(SIO%ACCESS),&
!    STATUS=TRIM(SIO%STATUS),&
!    POSITION=TRIM(SIO%POSITION),&
!    BLANK=TRIM(SIO%BLANK),&
!    PAD=TRIM(SIO%PAD),&
!       IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%RECL = 1
END IF

IF( PRESENT(DELIM   ) )THEN
 SIO%DELIM = DELIM
! IF( Opened )THEN
!  Opened = .FALSE.
!  CLOSE(SIO%unit)
! END IF
ELSE
! IF( .NOT.Opened )THEN
!  OPEN(UNIT=SIO%UNIT,&
!    FILE=STR(SIO%FILE),&
!    FORM=TRIM(SIO%FORM),&
!    ACTION=TRIM(SIO%ACTION),&
!    ACCESS=TRIM(SIO%ACCESS),&
!    STATUS=TRIM(SIO%STATUS),&
!    POSITION=TRIM(SIO%POSITION),&
!    BLANK=TRIM(SIO%BLANK),&
!    PAD=TRIM(SIO%PAD),&
!    RECL=SIO%RECL,&
!       IOSTAT=SIO%IOSTAT)
!  Opened = .TRUE.
! END IF
 SIO%DELIM = DEFAULT_DELIM(SIO)
END IF

!IF( ASSOCIATED(sio%prev) )THEN
! Unit_ = NewFile("old_sio.noisy")
! CALL PRINT_sio(sio%prev,Unit=Unit_)
! CLOSE(Unit_)
!END IF
!Unit_ = NewFile("new_sio.noisy")
!CALL PRINT_sio(sio,Unit=Unit_)
!CLOSE(Unit_)

!!attempt to OPEN with all the passed specifiers
OPEN( UNIT    =SIO%UNIT,&
      FILE    =STR(SIO%FILE),&
      ACTION  =SIO%ACTION,&
      FORM    =SIO%FORM,&
      ACCESS  =SIO%ACCESS,&
      BLANK   =SIO%BLANK,&
      DELIM   =SIO%DELIM,&
      PAD     =SIO%PAD,&
      POSITION=SIO%POSITION,&
      STATUS  =SIO%STATUS,&
      RECL    =SIO%RECL,&
      IOSTAT  =IOSTAT_open)

!!error checking
IF( IOSTAT_open/=0 )THEN

 !!print out the <SIO> structure to the output unit dictated by <fdbk> structure
 CALL PRINT_SIO(SIO,UNIT=OutputUnit(fdbk))

 INQUIRE( UNIT=SIO%UNIT , READ=allow_read , Exist=exist_file )

 !!check existence of file first
 IF( (allow_read .sEQ. "Yes") .AND. (.NOT.exist_file) )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error , fdbk , s=STR(VS)//&
    "The file you specified to open for reading ("//&
    TRIM(STR(SIO%FILE))//") does not exist!!")
  VS = ""
  RETURN
 END IF

 !!then kick out the general error if it wasn't a missing file
 VS = MODPROC(mod_,proc_)
 CALL UPDATE(fdbk_error , fdbk , s=STR(VS)//&
  "Error executing OPEN statement with the connection specifiers provided.")
 VS = ""
 RETURN

END IF

!!set starting line
SIO%line_num = 1

!!
END SUBROUTINE



SUBROUTINE DISCONNECT__(SIO,fdbk)
!!#### PURPOSE
!! Disconnect the current SIO connection.
!!#### ARGUMENTS
TYPE(TYPE_SIO) ,POINTER                :: SIO
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
!!#### LOCALS
TYPE(TYPE_SIO)  ,POINTER :: SIO_
!!

!!quick return
IF( .NOT.ASSOCIATED(SIO) )THEN
 CALL UPDATE(fdbk_warning,fdbk,s="[[SIO]] object already disconnected!")
 RETURN
END IF

!!close current unit
CLOSE(SIO%UNIT)

!!setup pointer to SIO
SIO_ => SIO

!!setup another pointer to the prevSIOus SIO
SIO  => SIO%prev

!!deallocate the current SIO
CALL DEALLOCATE_COMMAND(SIO_)
!!
CALL DEALLOCATE_OPTIONS(SIO_)
!!
IF( SIO_%shadow_syntax )THEN
 CALL NULLIFY_SYNTAX(SIO_)
ELSE
 CALL DEALLOCATE_SYNTAX(SIO_)
END IF
!!
IF( SIO_%shadow_format )THEN
 CALL NULLIFY_FORMAT(SIO_)
ELSE
 CALL DEALLOCATE_FORMAT(SIO_)
END IF
!!
IF( SIO_%shadow_connection )THEN
 CALL NULLIFY_CONNECTION(SIO_)
ELSe
 CALL DEALLOCATE_CONNECTION(SIO_)
END IF
!!
END SUBROUTINE


SUBROUTINE READ_CHARS_(SIO,X,fdbk,Stops,nChars,proper_string,ForceToEOL)
!!#### PURPOSE
!! Read single Characters in until a non-Ignore Character
!! is found.  Then read in Characters into string X until a
!! Character in Stops is found or nChars is reached.  If
!! proper_string=.TRUE. then the string is considered to be
!! "tagged" by the Character SIO%StrTag, just as the word
!! tagged was tagged with the Character ".
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER                :: SIO
TYPE(varying_string),INTENT(OUT)      :: X
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
CHARACTER(1)  ,INTENT(IN)   ,OPTIONAL :: Stops(:)
INTEGER       ,INTENT(IN)   ,OPTIONAL :: nChars
LOGICAL       ,INTENT(IN)   ,OPTIONAL :: proper_string
LOGICAL       ,INTENT(IN)   ,OPTIONAL :: ForceToEOL
!!#### LOCALS
CHARACTER(1),ALLOCATABLE :: Stops_(:)
INTEGER :: iChars,nChars_
LOGICAL :: proper_string_,IN_STRING_READ_MODE,ForceToEOL_
!!
!!initialize
X = ""

!!get Stop
IF( PRESENT(Stops) )THEN
 ALLOCATE( Stops_(1:SIZE(Stops)) )
 Stops_ = Stops
ELSE
 ALLOCATE( Stops_(1:1) )
 Stops_  = SIO%Stop
END IF

!!get nChars
IF( PRESENT(nChars) )THEN
 nChars_ = nChars
ELSE
 nChars_ = HUGE(nChars_)
END IF

!!get proper_string
IF( PRESENT(proper_string) )THEN
 proper_string_ = proper_string
ELSE
 proper_string_ = .FALSE.
END IF

!!get ForceToEOL
IF( PRESENT(ForceToEOL) )THEN
 ForceToEOL_ = ForceToEOL
ELSE
 ForceToEOL_ = .FALSE.
END IF

!!come back here to read more until end of line
333 CONTINUE

!!reset counter
iChars = 0
IN_STRING_READ_MODE = .FALSE.
!!
DO
 !!read a single Character at a time
 READ(SIO%UNIT,"(a1)",ADVANCE="no",SIZE=SIO%SIZE,IOSTAT=SIO%IOSTAT,END=666)SIO%Char

 IF( SIO%IOSTAT/=0 )THEN
  !!found end of line situation
  IF( SIO%IOSTAT==END_OF_RECORD )THEN
   SIO%END_OF_LINE = .TRUE.
   SIO%Char        = SIO%Ignore
  END IF

  !!found end of file situation
  IF( SIO%IOSTAT==END_OF_FILE )THEN
   SIO%END_OF_FILE = .TRUE.
   SIO%Char        = SIO%Ignore
   GOTO 666
  END IF

  !!go to the next line if we are reading a string (strings are assumed to Continue across lines)
  IF( IN_STRING_READ_MODE )THEN
   CALL getNextLine(SIO,fdbk)

  !!get out of the loop and see what to do
  ELSE
   EXIT
  END IF

 END IF


 !!for non-string reading
 IF( .NOT.IN_STRING_READ_MODE )THEN

  !!cycle if we haven't found the first significant (non-Ignore) Character
  IF( SIO%Char==SIO%Ignore .AND. iChars==0 )CYCLE


  !!found continuation Character
  IF( SIO%Char==SIO%Continue )THEN
   CALL getNextLine(SIO,fdbk)
   CYCLE
  END IF


  !!found Stop Character
  IF( ANY(SIO%Char==Stops_) )THEN
   EXIT
  END IF


  !!found Comment Character
  IF( SIO%Char==SIO%Comment )THEN

   !!cycle if it is the first non-ignore character
   IF( iChars==0 )THEN
    CALL getNextLine(SIO,fdbk)
    CYCLE

   !!exit if it is not
   ELSE
    EXIT
   END IF

  END IF


 END IF


 !!enter/exit string read mode (exit only when another StrTag is found)
 IF( SIO%Char==SIO%StrTag )THEN

  !!if already in string read mode then we have found the end of the proper
  !!string
  IF( IN_STRING_READ_MODE )THEN
   SIO%Char = SIO%Ignore
   IN_STRING_READ_MODE = .FALSE.
   IF( proper_string_ )EXIT

  !!otherwise we have just begin to read the "proper string"
  ELSE
   IN_STRING_READ_MODE = .TRUE.
  END IF
  CYCLE
 END IF

 !!make sure the found "Character" is not one of the first 32 ASCII
 !!control Characters
 IF( IACHAR(SIO%Char)>=32 )THEN
  iChars = iChars + 1
  X = X//SIO%Char
 END IF

 !!got all the Chars we need
 IF( iChars>=nChars_ )EXIT

END DO

!!success
IF( ForceToEOL_ )THEN
 X = X//SIO%Ignore
 GOTO 333
ELSE
 RETURN
END IF

!---past here we are resolving special conditions
666 CONTINUE

!!
END SUBROUTINE



SUBROUTINE WRITE_CHARS_(SIO,X,fdbk,end,proper_string)
!!#### PURPOSE
!! Write single Characters out until nChars or the end of X
!! is reached.  Then write the end Character.  If
!! proper_string=.TRUE. then the string is considered to be
!! "tagged" by the Character SIO%StrTag, just as the word
!! tagged was tagged with the Character " so the output
!! would be simply "X" (if SIO%StrTag=").
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER                :: SIO
CHARACTER(*)  ,INTENT(IN)             :: X
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
CHARACTER(1)  ,INTENT(IN)   ,OPTIONAL :: end
LOGICAL       ,INTENT(IN)   ,OPTIONAL :: proper_string
!!#### LOCALS
CHARACTER(*),PARAMETER :: proc_="WRITE_CHARS"
TYPE(varying_string) :: VS
LOGICAL :: proper_string_

!!--begin--

!!get whether or not this string should be outputted as a "proper string"---
!!one that is enclosed in SIO%StrTag tags
IF( PRESENT(proper_string) )THEN
 proper_string_ = proper_string
ELSE
 proper_string_ = .FALSE.
END IF

!!construct the basic string
VS = X

!!add the proper string tags if necessary
IF( proper_string_ )THEN
 VS = SIO%StrTag//VS//SIO%StrTag
END IF

!!add the ending Character if necessary
IF( PRESENT(end) )THEN
 VS = VS//end
END IF

!!string output
WRITE(SIO%UNIT,"(a)",IOSTAT=SIO%IOSTAT,ADVANCE="no")STR(VS)

!!unsucessful writing
IF( SIO%IOSTAT/=0 )THEN
 VS = MODPROC(mod_,proc_)
 CALL UPDATE( fdbk_error , fdbk , s=STR(VS)//" An unknown error&
   & occurred while trying to write Characters.")
END IF

VS=""

!!--end--
END SUBROUTINE


FUNCTION VERIFY_SYNTAX(SIO,sequence,name,fdbk,JustCheck) RESULT(PassedTest)
!!#### ARGUMENTS
TYPE(TYPE_SIO),POINTER :: SIO
CHARACTER(*),INTENT(IN) :: sequence
CHARACTER(*),INTENT(IN) :: name
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
LOGICAL,OPTIONAL :: JustCheck
!!#### RESULT
LOGICAL :: PassedTest
!!#### LOCALS
CHARACTER(LEN(sequence)) :: Chars
TYPE(varying_string) :: VS
LOGICAL :: JustCheck_
INTEGER :: i,start
!!
!!no SYNTAX In unformatted mode
IF( Unformatted(SIO) )RETURN

!!for reading do the verification
IF( Reading(SIO) )THEN

 !!initialize
 IF( LEN_TRIM(sequence)==0 )THEN
  PassedTest = .TRUE.
  RETURN
 ELSE
  PassedTest = .FALSE.
 END IF

 IF( PRESENT(JustCheck) )THEN
  JustCheck_ = JustCheck
 ELSE
  JustCheck_ = .FALSE.
 END IF

 !!possibly already have the first Character of verification in SIO%Char
 IF( SIO%Char==SIO%Ignore )THEN
  !!didn"t have one so we start at 1
  start = 0
 ELSE
  Chars(1:1) = SIO%Char
  !!had one so we start at 2
  start = 1
 END IF

 !!get the extra Characters if more than a single Character sequence
 IF( LEN(sequence)-start>0 )THEN
  CALL READ_CHARS(SIO,VS,fdbk,nChars=LEN(sequence)-start)
  Chars(1+start:) = VS
 END IF

 !!check if the Chars are the exact same as the sequence
 PassedTest = (Chars==sequence)

 !!SIO%Char is set to the last non-Ignorer Character
 DO i=LEN(sequence),1,-1
  SIO%Char = Chars(i:i)
  IF( SIO%Char/=SIO%Ignore )EXIT
 END DO

 IF( .NOT.PassedTest )THEN
  IF( .NOT.JustCheck_ )THEN
   CALL UPDATE(fdbk_error , fdbk , &
     s="Syntax verification failed.  I was expecting the "//&
     TRIM(name)//" Characters ("//sequence//") but found the &
     &sequence "//Chars//".")
  END IF
 END IF

!!for writing you always pass
ELSE
 PassedTest = .TRUE.
 WRITE(SIO%unit,"(a)")sequence
END IF
!!
END FUNCTION


SUBROUTINE putEndOfLine(SIO,fdbk,continue)
!!####  PURPOSE
!! Put and End-of-line with optional continuance.

!!####  REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO)  ,INTENT(INOUT) :: SIO

!!####  OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!####  OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: continue

!!####  LoCAL VARIABLES
LOGICAL :: continue_

!!--begin--

!!kick out if we are in writing mode or unformatted SIO
IF( (TRIM(SIO%ACTION) .sEQ. "Read") .OR. &
    (TRIM(SIO%FORM) .sEQ. "UnFormatted") )RETURN

!!write an advancing blank
continue_ = DEFAULT( .FALSE. , continue )

IF( continue_ )THEN
 WRITE(SIO%UNIT,"(a1)",ADVANCE="yes",IOSTAT=SIO%IOSTAT)SIO%continue
ELSE
 WRITE(SIO%UNIT,"(a1)",ADVANCE="yes",IOSTAT=SIO%IOSTAT)" "
END IF

!!increment line number
SIO%line_num  = SIO%line_num + 1

!!reset IOSTAT variable
SIO%IOSTAT = 0

!--end--
END SUBROUTINE




SUBROUTINE getNextLine(SIO,fdbk)
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,INTENT(INOUT) :: SIO
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
CHARACTER :: char

!!--begin--
!!kick out if we are in writing mode or unformatted SIO
IF( (TRIM(SIO%ACTION) .sEQ. "Write") .OR. &
    (TRIM(SIO%FORM) .sEQ. "Unformatted") )RETURN

!!read advancing input
BACKSPACE(SIO%UNIT)
READ(SIO%UNIT,"(a1)",ADVANCE="yes",IOSTAT=SIO%IOSTAT)Char

!!set end of line to false
SIO%END_OF_LINE = .FALSE.

!!increment line number
SIO%line_num  = SIO%line_num + 1

!!update the end-of-file condition
IF( SIO%IOSTAT/=0 )SIO%end_of_file = .TRUE.

!!reset the IOSTAT variable
SIO%IOSTAT = 0
!!
END SUBROUTINE

FUNCTION Stopped(SIO,fdbk)
TYPE(TYPE_SIO)  ,POINTER :: SIO
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
LOGICAL :: Stopped
!!--begin--
IF( Reading(SIO) )THEN
 Stopped = SIO%Char==SIO%Stop
ELSE
 Stopped = .FALSE.
END IF
!!--end--
END FUNCTION

SUBROUTINE putStopper(SIO,fdbk,Indent,Tag)
TYPE(TYPE_SIO)  ,POINTER :: SIO
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
INTEGER,INTENT(IN),OPTIONAL :: Indent
CHARACTER(*),INTENT(IN),OPTIONAL :: Tag
!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS

!!--begin--
VS = SIO%stop
IF( PRESENT(Indent) )VS = REPEAT(" ",Indent)//VS
IF( PRESENT(Tag) )VS = VS//Tag
CALL WRITE_CHARS(SIO,STR(VS),fdbk,proper_string=.FALSE.)
CALL putEndOfLine(SIO,fdbk)

!!--end--
END SUBROUTINE

FUNCTION FMT_SIO_A0L1(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0L.f90.hdr"
!!
VS = TRIM(SIO%FMT_L)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0L2(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0L.f90.hdr"
!!
VS = TRIM(SIO%FMT_L)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0L4(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0L.f90.hdr"
!!
VS = TRIM(SIO%FMT_L)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0I1(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0I.f90.hdr"
!!
VS = TRIM(SIO%FMT_I1)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0I2(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0I.f90.hdr"
!!
VS = TRIM(SIO%FMT_I2)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0I4(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0I.f90.hdr"
!!
VS = TRIM(SIO%FMT_I4)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0I8(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0I.f90.hdr"
!!
VS = TRIM(SIO%FMT_I8)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0Rsp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0R.f90.hdr"
!!
VS = TRIM(SIO%FMT_sp)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0Rdp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0R.f90.hdr"
!!
VS = TRIM(SIO%FMT_dp)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0Csp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0C.f90.hdr"
!!
VS = TRIM(SIO%FMT_sp)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0Cdp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A0C.f90.hdr"
!!
VS = TRIM(SIO%FMT_dp)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A0S(SIO,X,Pre,Post) RESULT(VS)
!!
INTEGER,PARAMETER :: KIND_S=KIND_Sen
INCLUDE "09-A-USR_SIO__FMT_A0S.f90.hdr"
!!
VS = "a"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION



FUNCTION FMT_SIO_A1L1(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1L.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_L)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1L2(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1L.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_L)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1L4(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1L.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_L)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1I1(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1I.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_I1)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1I2(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1I.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_I2)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1I4(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1I.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_I4)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1I8(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1I.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_I8)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1Rsp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1R.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_sp)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1Rdp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1R.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_dp)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1Csp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1C.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_sp)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1Cdp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A1C.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//TRIM(SIO%FMT_dp)
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A1S(SIO,X,Pre,Post) RESULT(VS)
!!
INTEGER,PARAMETER :: KIND_S=KIND_Sfmt
INCLUDE "09-A-USR_SIO__FMT_A1S.f90.hdr"
!!
VS = TRIM(STR(SIZE(X)))//"a"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION



FUNCTION FMT_SIO_A2L1(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2L.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_L)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2L2(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2L.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_L)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2L4(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2L.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_L)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2I1(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2I.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_I1)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2I2(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2I.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_I2)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2I4(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2I.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_I4)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2I8(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2I.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_I8)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2Rsp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2R.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_sp)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2Rdp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2R.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_dp)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2Csp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2C.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_sp)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2Cdp(SIO,X,Pre,Post) RESULT(VS)
!!
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__FMT_A2C.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//TRIM(SIO%FMT_dp)//"/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION

FUNCTION FMT_SIO_A2S(SIO,X,Pre,Post) RESULT(VS)
!!
INTEGER,PARAMETER :: KIND_S=KIND_Sfmt
INCLUDE "09-A-USR_SIO__FMT_A2S.f90.hdr"
!!
VS = TRIM(STR(SIZE(X,2)))//"("//TRIM(STR(SIZE(X,1)))//"a/)"
IF( PRESENT(Pre)  )VS = Pre//VS
IF( PRESENT(Post) )VS = VS//Post
!!
END FUNCTION


FUNCTION Reading(SIO)
TYPE(TYPE_SIO),POINTER :: SIO
LOGICAL :: Reading
!!
Reading = ( TRIM(SIO%ACTION) .sEQ. "Read" )
!!
END FUNCTION


FUNCTION Writing(SIO)
TYPE(TYPE_SIO),POINTER :: SIO
LOGICAL :: Writing
!!
Writing = ( TRIM(SIO%ACTION) .sEQ. "Write" )
!!
END FUNCTION


FUNCTION Formatted(SIO)
TYPE(TYPE_SIO),POINTER :: SIO
LOGICAL :: Formatted
!!
Formatted = ( TRIM(SIO%FORM) .sEQ. "Formatted" )
!!
END FUNCTION


FUNCTION Unformatted(SIO)
TYPE(TYPE_SIO),POINTER :: SIO
LOGICAL :: Unformatted
!!
Unformatted = ( TRIM(SIO%FORM) .sEQ. "Unformatted" )
!!
END FUNCTION



SUBROUTINE BEGIN_ARGUMENTS_(SIO,ArgNm,fdbk,NSubArg,Optional)
!!#### PURPOSE
!! Begins an SIO Cmd-card setup by setting names.
!!#### ARGUMENTS
TYPE(TYPE_SIO)   ,POINTER       :: SIO
CHARACTER(*)   ,INTENT(IN)    :: ArgNm(:)
TYPE(TYPE_fdbk) ,INTENT(INOUT),OPTIONAL :: fdbk
INTEGER,INTENT(IN),OPTIONAL :: NSubArg(SIZE(ArgNm))
LOGICAL,INTENT(IN),OPTIONAL :: Optional(SIZE(ArgNm))
!!#### LOCALS
CHARACTER(*),PARAMETER :: proc_ = "BEGIN_ARGUMENTS"
TYPE(varying_string) :: VS
CHARACTER(LEN_ArgNm) :: S
INTEGER :: i,j,ReturnFlag
CHARACTER(LEN=1,KIND=KIND_S) :: shape_dummy(1:SIO%LEN_ArgDelim)
LOGICAL :: AutoArgBeg
LOGICAL :: AutoArgEnd
LOGICAL :: ByDefinition
!!

!! initialize
ReturnFlag = 0

!!reset the Argument counter
SIO%CurrArg = 0

!!allocate names array
ALLOCATE( SIO%ArgNm(1:SIZE(ArgNm)) )
!!set names
SIO%ArgNm = ArgNm

!!allocate temporary Argument storage
ALLOCATE( SIO%Arg(1:SIZE(SIO%ArgNm)) )

!!set number of SubArguments
ALLOCATE( SIO%NSubArg(1:SIZE(SIO%ArgNm)) )
IF( PRESENT(NSubArg) )THEN
 SIO%NSubArg = NSubArg
ELSE
 SIO%NSubArg = 0
END IF

!!set whether each Argument is Optional according to the
!!passed array, Optional
ALLOCATE( SIO%Optional(1:SIZE(SIO%ArgNm)) )
IF( PRESENT(Optional) )THEN
 SIO%Optional = Optional
ELSE
 SIO%Optional = .FALSE.
END IF

!!allocate and initialize presence of Optional Arguments
ALLOCATE( SIO%Present(1:SIZE(SIO%ArgNm)) )
SIO%Present = .FALSE.

!!initialize varying string
VS=""

!!set if we are in auto beginning and ending mode
AutoArgBeg    = ALL(TRANSFER(SIO%ArgBeg  ,shape_dummy)==SIO%Ignore)
AutoArgEnd    = ALL(TRANSFER(SIO%ArgEnd  ,shape_dummy)==SIO%Ignore)

!!error if AutoArgEnd with Optionals
IF( AutoArgEnd )THEN
 IF( ANY(SIO%Optional) )THEN
  CALL UPDATE( fdbk_error , fdbk , s="In "//proc_//", you may not have Optional Arguments &
    &if in auto-Argument-end mode.")
 END IF
END IF

IF( Formatted(SIO) )THEN

IF( Reading(SIO) )THEN

 !!1. verify the Argument beginner Character if not in Auto mode
 IF( .NOT.AutoArgBeg )THEN
  IF( .NOT.VERIFY_SYNTAX(SIO,SIO%ArgBeg,"ArgBeg",fdbk) )THEN
   ReturnFlag = 1
   GOTO 666
  END IF
 END IF

 !!WAW [hack]
 SIO%End_OF_LINE = .FALSE.
 !!read in the Arguments
 DO

  !!1.0 read in some Characters ending on a definition, delimeter, or end Character
  CALL READ_CHARS(SIO,VS,fdbk,Stops=&
    (/SIO%ArgDefine,SIO%ArgDelim,SIO%ArgEnd/))

  !!1.2a if we have found the Argument definition Character
  IF( SIO%Char==SIO%ArgDefine )THEN

   !!1.2a.1 get the Argument index of the Argument defined by this name
   S = ADJUSTL(STR(VS))
   SIO%CurrArg = INDEXa( SIO%ArgNm , TRIM(S) , CASESEN=.FALSE. )

   !!1.2a.2 error if the name is not recognized
   IF( SIO%CurrArg==0 )THEN
    SIO%iostat = ERROR(SIO%iostat)
    CALL UPDATE( fdbk_error , fdbk , s="the Argument name found is not in the Argument&
      & names provided to BEGIN_ARGUMENTS.")
    EXIT
   ELSE
    ByDefinition = .TRUE.
   END IF

   !!1.2a.3 Get the actual Argument.
   CALL READ_CHARS(SIO,VS,fdbk,Stops=&
     (/SIO%ArgEnd,SIO%ArgDelim/) )


  !!1.2b otherwise just increment SIO%CurrArg
  ELSE
   ByDefinition = .FALSE.
   SIO%CurrArg = SIO%CurrArg + 1
  END IF

  !!2. set the current Argument to Present
  SIO%Present(SIO%CurrArg) = .TRUE.

  !!3a. if we have an array of data in this Argument, set the Arguments array
  !!    with the first SubArgument already read and
  !!    in and proceed to collect more SubArguments until we have them all.
  IF( sio%NSubArg(sio%CurrArg)>0 )THEN

   SIO%Arg(SIO%CurrArg) = SIO%SubArgDelim//VS//SIO%SubArgDelim

  !!3b.
  ELSE
   IF( ByDefinition )THEN
    SIO%Arg(SIO%CurrArg) = VS
   ELSE
    SIO%Arg(SIO%CurrArg) = VS
   END IF
  END IF

  !!4. verify the Argument delimeter syntax and Continue
  IF( .NOT.VERIFY_SYNTAX(SIO,SIO%ArgDelim,"ArgDelim",fdbk,JustCheck=.TRUE.) )EXIT

  !!exit on end-of-line (after returning the cursor to the next line)
  IF( SIO%END_OF_LINE )THEN
   CALL getNextLine(SIO,fdbk)
   EXIT
  END IF

 END DO

 !!5. verify the Argument ender Character if not in Auto mode
 IF( .NOT.AutoArgEnd )THEN
  IF( .NOT.VERIFY_SYNTAX(SIO,SIO%ArgEnd,"ArgEnd",fdbk) )THEN
   ReturnFlag = 3
   GOTO 666
  END IF
 END IF

ELSE

 WRITE(SIO%Unit,"(a)",ADVANCE="no")SIO%ArgBeg

END IF

 !!6. reset current Argument
 SIO%CurrArg = 0

END IF

666 CONTINUE
IF( ReturnFlag/=0 )THEN
 CALL DUMP(fdbk)
 RETURN
END IF

!!--end--
END SUBROUTINE


SUBROUTINE END_ARGUMENTS_(SIO,fdbk)
!!#### PURPOSE
!! Ends an input/output-stream.
!!#### ARGUMENTS
TYPE(TYPE_SIO)   ,POINTER       :: SIO
TYPE(TYPE_fdbk) ,INTENT(INOUT),OPTIONAL :: fdbk
!!#### LOCALS
CHARACTER(*),PARAMETER :: proc_ = "END_ARGUMENTS"
INTEGER :: j
!!

IF( Writing(SIO) )THEN
 WRITE(SIO%Unit,"(a)",Advance="no")SIO%ArgEnd
 CALL putEndOfLine(SIO,fdbk)
END IF

!!dump the feedback if errors were encountered
CALL DUMP(fdbk)

!!set the Argument counter to warning
SIO%CurrArg = WARNING(SIO%CurrArg)

!!deallocate Argument names
CALL CLEARn( SIO%ArgNm )

!!deallocate Argument values
CALL CLEARn( SIO%Arg )

!!deallocate Optional
CALL CLEARn( SIO%Optional )

!!deallocate NSubArg
CALL CLEARn( SIO%NSubArg )

!!
END SUBROUTINE



SUBROUTINE PARSE_COMMAND_(SIO,fdbk)
!!#### PURPOSE
!! Parses the Cmd (SIO%Cmd) into two parts and hashes each part.

!!#### ARGUMENTS
TYPE(TYPE_SIO) ,INTENT(INOUT)          :: SIO
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCALS
CHARACTER(*),PARAMETER :: proc_ = "PARSE_COMMAND"
INTEGER                :: i,LEN_Cmd,LEN_Cmd1

!!--begin--

!! Must add base.
IF( SIO_ADD_BASE/="   " )THEN
 SIO%Cmd = SIO_ADD_BASE//SIO%Cmd
END IF

!! Get the length of each part of the command.
LEN_Cmd  = LEN_TRIM(SIO%Cmd)
LEN_Cmd1 = MIN(LEN_TRIM(SIO%Cmd),3)

!! Get the length of the command.
SIO%splitCmd(1) = EXTRACT(SIO%Cmd,1         ,LEN_Cmd1)
SIO%splitCmd(2) = EXTRACT(SIO%Cmd,LEN_Cmd1+1,LEN_Cmd )

!! Get the hash.
DO i=1,2
 SIO%hash(i)  = HASH( TRIM(STR(SIO%splitCmd(i))) , CaseSensitive=.FALSE. , FullSpectrum=.FALSE. )
END DO

!!--end--
END SUBROUTINE



SUBROUTINE WRITE_COMMAND_(SIO,fdbk)
!!#### PURPOSE
!! Writes the Cmd (SIO%Cmd) to SIO%FILE.

!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER                :: SIO
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCALS
INTEGER                :: j,m
CHARACTER(*),PARAMETER :: proc_ = "WRITE_COMMAND"
!!

!parse command
CALL PARSE_COMMAND(SIO,fdbk)

!check for internal commands
IF( IsInternalCommand(SIO) )THEN

 !run through database
 CALL DATABASE_SIO(SIO,fdbk)

 RETURN

ELSE

 IF( Formatted(SIO) )THEN

  !!write Cmd word
  WRITE(SIO%UNIT,"(a)",ADVANCE="no")TRIM(SIO%CmdBeg)//TRIM(STR(SIO%Cmd))

 ELSE

  !!write hashes and number of Arguments
  WRITE(SIO%UNIT) SIO%hash(1)
  WRITE(SIO%UNIT) SIO%hash(2)

 END IF

END IF

!!
END SUBROUTINE



SUBROUTINE READ_COMMAND_(SIO,fdbk,keepalive)
!!#### PURPOSE
!! Read the next Cmd in the unit connected to SIO.

!!#### ARGUMENTS
TYPE(TYPE_SIO)          ,POINTER       :: SIO
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk
LOGICAL        ,OPTIONAL,INTENT(IN)    :: keepalive

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: proc_="READ_COMMAND_SIO"
!!#### LOCAL VARIABLES
TYPE(varying_string) :: VS
INTEGER              :: i,LEN_COM,LEN_HASH
LOGICAL              :: AutoCommand,keepalive_

!!--begin--

!get local value for whether to keep SIO allocated
!and alive after it reads the last line of a file
keepalive_ = DEFAULT(.FALSE.,keepalive)

!! Check to make sure we are reading.
IF( .NOT.Reading(SIO)  )THEN
 VS = MODPROC(mod_,proc_)
 CALL UPDATE(fdbk_error , fdbk , s=STR(VS)//&
   "ACTION must be set to 'Read' to use READ_COMMAND" )
 VS = ""
 RETURN
END IF

!! Reset IOSTAT and end-of-file specs.
SIO%IOSTAT    = 0
SIO%end_of_file = .FALSE.

!! Error return clause.
666 IF( SIO%end_of_file )THEN
 IF( keepalive_ )THEN
  RETURN
 ELSE
  CALL DISCONNECT( SIO , fdbk )
  IF( .NOT.ASSOCIATED(SIO) )RETURN
 END IF
END IF

!!A. formatted input
IF( Noisy_ )WRITE(OutputUnit(fdbk),'(4x,a)')"formatted="//STR(Formatted(SIO))
IF( Formatted(SIO) )THEN
 !! Clear the current Cmd.
 SIO%Cmd = ""

 !! Read until some Character is found
 DO

  !! Read in a Character.
  CALL READ_CHARS( SIO , VS , fdbk , nChars=1 )
  IF( Noisy_ )WRITE(OutputUnit(fdbk),'(4x,a)')"Char="//STR(VS)

  !! Goto end-of-file handling claUSE.
  IF( SIO%end_of_file )GOTO 666

  SIO%Char = VS

  !! Update the AutoCommand status.
  AutoCommand = (SIO%CmdBeg==SIO%Ignore)
  IF( Noisy_ )WRITE(OutputUnit(fdbk),'(4x,a)')"autocommand="//STR(AutoCommand)

  !! The beginning of a Cmd is found under these circumstances.
  IF( SIO%Char==SIO%CmdBeg .OR. AutoCommand )THEN

   !! Read in the rest of the command Characters.
   CALL READ_CHARS( SIO , SIO%Cmd , fdbk , Stops=(/SIO%ArgBeg/) )

   !! If we are in automatic command mode then we must do a fix-up
   !! to get the full command.
   IF( AutoCommand )THEN
    SIO%Cmd = ADJUSTL(VS//SIO%Cmd)
   END IF

   !! Output the entire Cmd.
   IF( Noisy_ )WRITE(OutputUnit(fdbk),'(4x,a)')"Cmd="//STR(SIO%Cmd)

   !! If we got here, we found a Cmd, so now we exit.
   IF( LEN_TRIM(SIO%cmd)/=0 )EXIT

  END IF

  !! Go to the next line and Continue searching.
  CALL getNextLine(SIO,fdbk)

 END DO

 !! Parse the Cmd using the <HASH> function.
 CALL PARSE_COMMAND(SIO,fdbk)

!!B. unformatted input
ELSE

 !!B.1. just read the two hashes
 READ(SIO%UNIT,END=666,IOSTAT=SIO%IOSTAT) SIO%hash(1)
 READ(SIO%UNIT,END=666,IOSTAT=SIO%IOSTAT) SIO%hash(2)

END IF

!! Look for internal commands and if we find them, execute and get another.
IF( IsInternalCommand(SIO) )THEN

 !run through database
 CALL DATABASE_SIO(SIO,fdbk)

 IF( IsError(fdbk) )RETURN

 !get another command
 GOTO 666

END IF

!!--end--
END SUBROUTINE


SUBROUTINE SIOformat(SIO,fdbk)
!!#### PURPOSE
!! The format card for the serial-input/output.

!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER       :: SIO
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
!!#### LOCALS
TYPE(varying_string) :: VS
!!
IF( SIO%shadow_format )THEN
 CALL NULLIFY_FORMAT(SIO)
 CALL ALLOCATE_FORMAT(SIO)
END IF

CALL BEGIN_ARGUMENTS(SIO,(/"FMT_L ",&
                           "FMT_I1",&
                           "FMT_I2",&
                           "FMT_I4",&
                           "FMT_I8",&
                           "FMT_sp",&
                           "FMT_dp"/),fdbk)
CALL ARGUMENT(SIO,SIO%FMT_L ,fdbk,Default=SIO%Argdefault )
CALL ARGUMENT(SIO,SIO%FMT_I1,fdbk,Default=SIO%Argdefault )
CALL ARGUMENT(SIO,SIO%FMT_I2,fdbk,Default=SIO%Argdefault )
CALL ARGUMENT(SIO,SIO%FMT_I4,fdbk,Default=SIO%Argdefault )
CALL ARGUMENT(SIO,SIO%FMT_I8,fdbk,Default=SIO%Argdefault )
CALL ARGUMENT(SIO,SIO%FMT_sp,fdbk,Default=SIO%Argdefault )
CALL ARGUMENT(SIO,SIO%FMT_dp,fdbk,Default=SIO%Argdefault )
CALL END_ARGUMENTS(SIO,fdbk)
!!
END SUBROUTINE


SUBROUTINE SIOsyntax(SIO,fdbk)
!!#### PURPOSE
!! The syntax card for the serial-input/output.
!!#### ARGUMENTS
TYPE(TYPE_SIO)  ,POINTER       :: SIO
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
CHARACTER(MAX_LEN_ArgDelim) :: ArgDelim
CHARACTER :: Argdefault,ArgDefine,Continue,StrTag,CmdBeg,ArgBeg,&
  SubArgDelim,ArgEnd,Comment,Stop,Ignore
!!
IF( SIO%shadow_syntax )THEN
 CALL NULLIFY_SYNTAX(SIO)
 CALL ALLOCATE_SYNTAX(SIO)
END IF

CALL BEGIN_ARGUMENTS(SIO,(/"Argdefault ",&
                           "ArgDefine  ",&
                           "Continue   ",&
                           "CmdBeg     ",&
                           "ArgBeg     ",&
                           "ArgDelim   ",&
                           "SubArgDelim",&
                           "ArgEnd     ",&
                           "Comment    ",&
                           "Stop       ",&
                           "Ignore     "/),fdbk)
CALL ARGUMENT(SIO,Argdefault ,fdbk)
CALL ARGUMENT(SIO,ArgDefine  ,fdbk)
CALL ARGUMENT(SIO,Continue   ,fdbk)
CALL ARGUMENT(SIO,CmdBeg     ,fdbk)
CALL ARGUMENT(SIO,ArgBeg     ,fdbk)
CALL ARGUMENT(SIO,ArgDelim   ,fdbk)
CALL ARGUMENT(SIO,SubArgDelim,fdbk)
CALL ARGUMENT(SIO,ArgEnd     ,fdbk)
CALL ARGUMENT(SIO,Comment    ,fdbk)
CALL ARGUMENT(SIO,Stop       ,fdbk)
CALL ARGUMENT(SIO,Ignore     ,fdbk)

CALL END_ARGUMENTS(SIO,fdbk)

!!calculate length of Argument delimeter
SIO%Argdefault =Argdefault
SIO%ArgDefine  =ArgDefine
SIO%Continue   =Continue
SIO%CmdBeg     =CmdBeg
SIO%ArgBeg     =ArgBeg
SIO%ArgDelim   =ArgDelim
SIO%SubArgDelim=SubArgDelim
SIO%ArgEnd     =ArgEnd
SIO%Comment    =Comment
SIO%Stop       =Stop
SIO%Ignore     =Ignore
SIO%LEN_ArgDelim = LEN_TRIM(SIO%ArgDelim)
!!
END SUBROUTINE


SUBROUTINE SIOconnect(SIO,fdbk)
!!#### PURPOSE
!! The connect card for the serial-input/output.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),POINTER :: SIO

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_ = "SIOconnect"

!!#### LOCAL VARIABLES
TYPE(varying_string)      :: FILE
CHARACTER(LEN_CONNECTION) :: FORM
CHARACTER(LEN_CONNECTION) :: ACTION
CHARACTER(LEN_CONNECTION) :: ACCESS
CHARACTER(LEN_CONNECTION) :: STATUS
CHARACTER(LEN_CONNECTION) :: POSITION
CHARACTER(LEN_CONNECTION) :: BLANK
CHARACTER(LEN_CONNECTION) :: PAD
INTEGER                   :: RECL
CHARACTER(LEN_CONNECTION) :: DELIM

!!--begin--
CALL BEGIN_ARGUMENTS(SIO,(/"FILE    ",&
                           "FORM    ",&
                           "STATUS  ",&
                           "ACTION  ",&
                           "ACCESS  ",&
                           "POSITION",&
                           "BLANK   ",&
                           "PAD     ",&
                           "RECL    ",&
                           "DELIM   "/), fdbk  , &
                                        OPTIONAL=(/.TRUE. ,&
                                                   .TRUE. ,&
                                                   .TRUE. ,&
                                                   .TRUE. ,&
                                                   .TRUE. ,&
                                                   .TRUE. ,&
                                                   .TRUE. ,&
                                                   .TRUE. ,&
                                                   .TRUE. ,&
                                                   .TRUE. /) , &
                                        NSubArg=(/0 ,&
                                                  0 ,&
                                                  0 ,&
                                                  0 ,&
                                                  0 ,&
                                                  0 ,&
                                                  0 ,&
                                                  0 ,&
                                                  0 ,&
                                                  0 /)  )

CALL ARGUMENT(SIO,FILE    ,fdbk,DEFAULT=INQUIRE_FILE    (SIO))
CALL ARGUMENT(SIO,FORM    ,fdbk,DEFAULT=INQUIRE_FORM    (SIO))
CALL ARGUMENT(SIO,ACTION  ,fdbk,DEFAULT="Read"               )
CALL ARGUMENT(SIO,ACCESS  ,fdbk,DEFAULT=INQUIRE_ACCESS  (SIO))
CALL ARGUMENT(SIO,STATUS  ,fdbk,DEFAULT=&
  MERGE( "Old    ","Replace", SIO%ACTION .sEQ. "Read" ) )
CALL ARGUMENT(SIO,POSITION,fdbk,DEFAULT="ASIS")
CALL ARGUMENT(SIO,BLANK   ,fdbk,DEFAULT=INQUIRE_BLANK   (SIO))
CALL ARGUMENT(SIO,PAD     ,fdbk,DEFAULT=INQUIRE_PAD     (SIO))
CALL ARGUMENT(SIO,RECL    ,fdbk,DEFAULT=INQUIRE_RECL    (SIO))
CALL ARGUMENT(SIO,DELIM   ,fdbk,DEFAULT=INQUIRE_DELIM   (SIO))

CALL END_ARGUMENTS(SIO,fdbk)

!finally actually perform the connect with the new information
CALL CONNECT(SIO,fdbk,STR(FILE),&
                      FORM    ,&
                      ACTION  ,&
                      ACCESS  ,&
                      STATUS  ,&
                      POSITION,&
                      BLANK   ,&
                      PAD     ,&
                      RECL    ,&
                      DELIM    )
!!--end--
END SUBROUTINE


!!### COMMAND DATABASE SUBROUTINE: SIO (Sequential Input/Output)
SUBROUTINE DATABASE_SIO(SIO,fdbk)
!!#### PURPOSE
!! Carries out <SIO*> commands.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),POINTER :: SIO

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!--begin--
SELECT CASE( SIO%hash(2) )
 CASE(syntax_)  ; CALL SIOsyntax (SIO,fdbk)
 CASE(format_)  ; CALL SIOformat (SIO,fdbk)
 CASE(connect_) ; CALL SIOconnect(SIO,fdbk)
 CASE DEFAULT   ; CALL UPDATE( fdbk_error , fdbk , s="The SIO command "//&
   TRIM(STR(SIO%Cmd))//"does not exist or the hash value has not been set&
   & correctly.")
END SELECT

!!--end--
END SUBROUTINE

!!**begin READ_ARGUMENT_A0 routines
SUBROUTINE READ_ARGUMENT_A0I1(SIO,X,fdbk,Default,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0I.f90.hdr"
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0I1"
!!
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__READ_ARGUMENT_A0I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0I2(SIO,X,fdbk,Default,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0I.f90.hdr"
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0I2"
!!
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__READ_ARGUMENT_A0I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0I4(SIO,X,fdbk,Default,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0I.f90.hdr"
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0I4"
!!
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__READ_ARGUMENT_A0I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0I8(SIO,X,fdbk,Default,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0I.f90.hdr"
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0I8"
!!
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__READ_ARGUMENT_A0I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0Rsp(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0Rsp"
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0R.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0Rdp(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0Rdp"
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0R.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0Csp(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp          !!((01-A-KND_IntrinsicTypes.f90))
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0Csp"
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0C.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0Cdp(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0C.f90.hdr"
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0Cdp"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0L1(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1           !!((01-A-KND_IntrinsicTypes.f90))
!!#### IDENTIFICATSION
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0L1"
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0L2(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0L2"
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0L4(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0L.f90.hdr"
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0L4"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0S(SIO,X,fdbk,Default)
!!#### PURPOSE
INTEGER,PARAMETER :: KIND_S=KIND_Sen
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0S"
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0S.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0S.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A0VS(SIO,X,fdbk,Default)
!!#### PURPOSE
CHARACTER(*),PARAMETER :: proc_ = "READ_ARGUMENT_A0VS"
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0VS.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A0VS.f90.bdy"
!!
END SUBROUTINE
!!**end READ_ARGUMENT_A0


!!**begin READ_ARGUMENT_A1
SUBROUTINE READ_ARGUMENT_A1S(SIO,X,fdbk,Default)
!!#### PURPOSE
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1S.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1S.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1VS(SIO,X,fdbk,Default)
!!#### PURPOSE
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1VS.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1VS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1I1(SIO,X,fdbk,Default,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1I.f90.hdr"
!!
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__READ_ARGUMENT_A1I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1I2(SIO,X,fdbk,Default,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1I.f90.hdr"
!!
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__READ_ARGUMENT_A1I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1I4(SIO,X,fdbk,Default,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1I.f90.hdr"
!!
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__READ_ARGUMENT_A1I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1I8(SIO,X,fdbk,Default,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1I.f90.hdr"
!!
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__READ_ARGUMENT_A1I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1Rsp(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1R.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1Rdp(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1R.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1Csp(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1C.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1Cdp(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1C.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1L1(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1L2(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE READ_ARGUMENT_A1L4(SIO,X,fdbk,Default)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__READ_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE
!!**end READ_ARGUMENT_A1 routines




!!**begin WRITE_ARGUMENT_A0 routines
SUBROUTINE WRITE_ARGUMENT_A0I1(SIO,X,fdbk,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0I1"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0I.f90.hdr"
!!
FMT_ = SIO%FMT_I1
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__WRITE_ARGUMENT_A0I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0I2(SIO,X,fdbk,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0I2"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0I.f90.hdr"
!!
FMT_ = SIO%FMT_I2
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__WRITE_ARGUMENT_A0I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0I4(SIO,X,fdbk,Keys)
!!#### PURPOSE
INTEGER     ,PARAMETER :: KIND_I=KIND_I4
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0I4"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0I.f90.hdr"
!!
FMT_ = SIO%FMT_I4
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__WRITE_ARGUMENT_A0I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0I8(SIO,X,fdbk,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0I8"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0I.f90.hdr"
!!
FMT_ = SIO%FMT_I8
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__WRITE_ARGUMENT_A0I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0Rsp(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0Rsp"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0R.f90.hdr"
!!
FMT_ = SIO%FMT_sp
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0Rdp(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0Rdp"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0R.f90.hdr"
!!
FMT_ = SIO%FMT_dp
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0Csp(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp          !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0Csp"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0C.f90.hdr"
!!
FMT_ = SIO%FMT_sp
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0Cdp(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp          !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0Cdp"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0C.f90.hdr"
!!
FMT_ = SIO%FMT_dp
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0L1(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0L1"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0L.f90.hdr"
!!
FMT_ = SIO%FMT_L
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0L2(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0L2"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0L.f90.hdr"
!!
FMT_ = SIO%FMT_L
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0L4(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0L4"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0L.f90.hdr"
!!
FMT_ = SIO%FMT_L
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0S(SIO,X,fdbk)
!!#### PURPOSE
CHARACTER(*),PARAMETER :: proc_ = "WRITE_ARGUMENT_A0S"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0S.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0S.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A0VS(SIO,X,fdbk)
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A0VS"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0VS.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A0VS.f90.bdy"
!!
END SUBROUTINE
!!**end WRITE_ARGUMENT_A0


!!**begin WRITE_ARGUMENT_A1
SUBROUTINE WRITE_ARGUMENT_A1S(SIO,X,fdbk)
!!#### PURPOSE
INTEGER,PARAMETER :: KIND_S=KIND_Sen
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1S.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1S.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1VS(SIO,X,fdbk)
!!#### PURPOSE
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1VS"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1VS.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1VS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1I1(SIO,X,fdbk,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1I1"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1I.f90.hdr"
!!
FMT_ = SIO%FMT_I1
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__WRITE_ARGUMENT_A1I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1I2(SIO,X,fdbk,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1I2"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1I.f90.hdr"
!!
FMT_ = SIO%FMT_I2
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__WRITE_ARGUMENT_A1I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1I4(SIO,X,fdbk,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1I4"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1I.f90.hdr"
!!
FMT_ = SIO%FMT_I4
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__WRITE_ARGUMENT_A1I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1I8(SIO,X,fdbk,Keys)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1I8"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1I.f90.hdr"
!!
FMT_ = SIO%FMT_I8
IF( PRESENT(Keys) )THEN
 INCLUDE "08-A-USR_SIO__WRITE_ARGUMENT_A1I_KEYS.f90.bdy"
ELSE
 INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
END IF
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1Rsp(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp          !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1Rsp"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1R.f90.hdr"
!!
FMT_ = SIO%FMT_sp
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1Rdp(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp          !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1Rdp"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1R.f90.hdr"
!!
FMT_ = SIO%FMT_dp
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1Csp(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp          !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1Csp"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1C.f90.hdr"
!!
FMT_ = SIO%FMT_sp
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1Cdp(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp          !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1Cdp"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1C.f90.hdr"
!!
FMT_ = SIO%FMT_dp
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1L1(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1L1"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1L.f90.hdr"
!!
FMT_ = SIO%FMT_L
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1L2(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2           !!((01-A-KND_IntrinsicTypes.f90))
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1L2"
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1L.f90.hdr"
!!
FMT_ = SIO%FMT_L
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE WRITE_ARGUMENT_A1L4(SIO,X,fdbk)
!!#### PURPOSE
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1L.f90.hdr"
CHARACTER(*),PARAMETER :: proc_="WRITE_ARGUMENT_A1L4"
!!
FMT_ = SIO%FMT_L
INCLUDE "09-A-USR_SIO__WRITE_ARGUMENT_A1.f90.bdy"
!!
END SUBROUTINE
!!**end WRITE_ARGUMENT_A1 routines



!!**begin** scalar Arguments (ARGUMENT_A0)
SUBROUTINE ARGUMENT_A0I1(SIO,Arg,fdbk,Default,Keys)
!!#### PURPOSE
!! Reads/writes 1-byte integer scalar Cmd-card Argument.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0I1"
INTEGER     ,PARAMETER :: KIND_I = KIND_I1
INCLUDE "09-A-USR_SIO__ARGUMENT_A0I.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT_KEYS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0I2(SIO,Arg,fdbk,Default,Keys)
!!#### PURPOSE
!! Reads/writes 2-byte integer scalar Cmd-card Argument.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0I2"
INTEGER     ,PARAMETER :: KIND_I = KIND_I2
INCLUDE "09-A-USR_SIO__ARGUMENT_A0I.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT_KEYS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0I4(SIO,Arg,fdbk,Default,Keys)
!!#### PURPOSE
!! Reads/writes 4-byte integer scalar Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0I4"
INTEGER     ,PARAMETER :: KIND_I = KIND_I4
INCLUDE "09-A-USR_SIO__ARGUMENT_A0I.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT_KEYS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0I8(SIO,Arg,fdbk,Default,Keys)
!!#### PURPOSE
!! Reads/writes 8-byte integer scalar Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0I8"
INTEGER     ,PARAMETER :: KIND_I = KIND_I8
INCLUDE "09-A-USR_SIO__ARGUMENT_A0I.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT_KEYS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0Rsp(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes single-precision real Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0Rsp"
INTEGER     ,PARAMETER :: KIND_R = KIND_Rsp
INCLUDE "09-A-USR_SIO__ARGUMENT_A0R.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0Rdp(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes double-precision real Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0Rdp"
INTEGER     ,PARAMETER :: KIND_R = KIND_Rdp
INCLUDE "09-A-USR_SIO__ARGUMENT_A0R.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0Csp(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes single-precision complex Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0Csp"
INTEGER     ,PARAMETER :: KIND_C = KIND_Csp
INCLUDE "09-A-USR_SIO__ARGUMENT_A0C.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0Cdp(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes double-precision complex Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0Cdp"
INTEGER     ,PARAMETER :: KIND_C = KIND_Cdp
INCLUDE "09-A-USR_SIO__ARGUMENT_A0C.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0L1(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes level-1 logical scalar Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0L1"
INTEGER     ,PARAMETER :: KIND_L = KIND_L1
INCLUDE "09-A-USR_SIO__ARGUMENT_A0L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0L2(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes level-2 logical scalar Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0L2"
INTEGER     ,PARAMETER :: KIND_L = KIND_L2
INCLUDE "09-A-USR_SIO__ARGUMENT_A0L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0L4(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes level-4 logical scalar Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0L4"
INTEGER     ,PARAMETER :: KIND_L = KIND_L4
INCLUDE "09-A-USR_SIO__ARGUMENT_A0L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0S(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes string scalar Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0S"
INCLUDE "09-A-USR_SIO__ARGUMENT_A0S.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A0VS(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes varying-string scalar Cmd-card ARGUMENT_A0.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A0VS"
INCLUDE "09-A-USR_SIO__ARGUMENT_A0VS.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE
!!**end** scalar Arguments (ARGUMENT_A0)



!!**begin** rank-1 array Arguments (ARGUMENT_A1)
SUBROUTINE ARGUMENT_A1I1(SIO,Arg,fdbk,Default,Keys)
!!#### PURPOSE
!! Reads/writes 1-byte integer rank-1 array Cmd-card Argument.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1I1"
INTEGER     ,PARAMETER :: KIND_I = KIND_I1
INCLUDE "09-A-USR_SIO__ARGUMENT_A1I.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT_KEYS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1I2(SIO,Arg,fdbk,Default,Keys)
!!#### PURPOSE
!! Reads/writes 2-byte integer rank-1 array Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1I2"
INTEGER     ,PARAMETER :: KIND_I = KIND_I2
INCLUDE "09-A-USR_SIO__ARGUMENT_A1I.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT_KEYS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1I4(SIO,Arg,fdbk,Default,Keys)
!!#### PURPOSE
!! Reads/writes 4-byte integer rank-1 array Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1I4"
INTEGER     ,PARAMETER :: KIND_I = KIND_I4
INCLUDE "09-A-USR_SIO__ARGUMENT_A1I.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT_KEYS.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1I8(SIO,Arg,fdbk,Default,Keys)
!!#### PURPOSE
!! Reads/writes 8-byte integer rank-1 array Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1I8"
INTEGER     ,PARAMETER :: KIND_I = KIND_I8
INCLUDE "09-A-USR_SIO__ARGUMENT_A1I.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1Rsp(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes single-precision real Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1Rsp"
INTEGER     ,PARAMETER :: KIND_R = KIND_Rsp
INCLUDE "09-A-USR_SIO__ARGUMENT_A1R.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1Rdp(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes double-precision real Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1Rdp"
INTEGER     ,PARAMETER :: KIND_R = KIND_Rdp
INCLUDE "09-A-USR_SIO__ARGUMENT_A1R.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE


SUBROUTINE ARGUMENT_A1Csp(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes single-precision complex Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1Csp"
INTEGER     ,PARAMETER :: KIND_C = KIND_Csp
INCLUDE "09-A-USR_SIO__ARGUMENT_A1C.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1Cdp(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes double-precision complex Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1Cdp"
INTEGER     ,PARAMETER :: KIND_C = KIND_Cdp
INCLUDE "09-A-USR_SIO__ARGUMENT_A1C.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE


SUBROUTINE ARGUMENT_A1L1(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes level-1 logical rank-1 array Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1L1"
INTEGER     ,PARAMETER :: KIND_L = KIND_L1
INCLUDE "09-A-USR_SIO__ARGUMENT_A1L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1L2(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes level-2 logical rank-1 array Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1L2"
INTEGER     ,PARAMETER :: KIND_L = KIND_L2
INCLUDE "09-A-USR_SIO__ARGUMENT_A1L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1L4(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes level-4 logical rank-1 array Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1L4"
INTEGER     ,PARAMETER :: KIND_L = KIND_L4
INCLUDE "09-A-USR_SIO__ARGUMENT_A1L.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1S(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes string rank-1 array Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1S"
INCLUDE "09-A-USR_SIO__ARGUMENT_A1S.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE

SUBROUTINE ARGUMENT_A1VS(SIO,Arg,fdbk,Default)
!!#### PURPOSE
!! Reads/writes varying-string scalar Cmd-card ARGUMENT_A1.
!!PARAMETERS
CHARACTER(*),PARAMETER :: proc_ = "ARGUMENT_A1VS"
INCLUDE "09-A-USR_SIO__ARGUMENT_A1VS.f90.hdr"
!!
INCLUDE "09-A-USR_SIO__ARGUMENT.f90.bdy"
!!
END SUBROUTINE
!!**end** rank-1 Arguments (ARGUMENT_A1)


!!### PURE LOGICAL FUNCTION: IsInclude
PURE LOGICAL FUNCTION IsInclude( SIO )
!!#### PURPOSE
!! Tests to see if <SIO> currently contains a hashed
!! command equal to <"INCLUDE">, indicating a jump
!! into another file is desired.

!!#### REQUIRED INPUT
TYPE(TYPE_SIO),INTENT(IN) :: SIO

!!#### LOCAL PARAMETERS
!! * special include statement hash <"INC"> plus <"LUDE">
INTEGER,PARAMETER :: INC_       = 3588
INTEGER,PARAMETER :: LUDE_      = 11740

!!--begin--
IsInclude = (SIO%hash(1)==INC_ .AND. SIO%hash(2)==LUDE_ )

!!--end--
END FUNCTION


!!### LOGICAL FUNCTION <<IsInternalCommand>>
LOGICAL FUNCTION IsInternalCommand( SIO )
!!#### PURPOSE
!! Tests to see if <SIO> currently contains a hashed primary
!! part of a command equal to <"SIO">.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO

!!--begin--
IsInternalCommand =  SIO%hash(1)==SIO_

!change <"include"> to <"SIOconnect"> (including is the simplest
!form of connection)
IF( IsInclude(SIO) )THEN
 IsInternalCommand = .TRUE.
 SIO%hash(1) = SIO_
 SIO%hash(2) = connect_
END IF

!!--end--
END FUNCTION


END MODULE
